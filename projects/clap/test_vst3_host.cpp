// Headless VST3 host: loads a midi-langs VST3 (the CLAP through thirdparty/clap-wrapper) and
// checks what the wrapper adds: class info, buses, MIDI output as VST3 events, and state.
// test_clap_host covers the plugin itself.
// Usage: test_vst3_host <plugin .vst3> <cases file> <udp port>
// The plugin's UDP port variable must be set to <udp port>, which turns UDP on at init.
//
// Cases, from the language's clap_test_cases.txt (see test_clap_host.cpp):
//   expr, expr_out, note   as for test_clap_host
//   cc                     optional: control change 7, value 100, channel 1

#include "public.sdk/source/common/memorystream.h"
#include "public.sdk/source/vst/hosting/eventlist.h"
#include "public.sdk/source/vst/hosting/hostclasses.h"
#include "public.sdk/source/vst/hosting/module.h"
#include "pluginterfaces/vst/ivstaudioprocessor.h"
#include "pluginterfaces/vst/ivstcomponent.h"
#include "pluginterfaces/vst/ivstevents.h"
#include "pluginterfaces/vst/ivstprocesscontext.h"

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <map>
#include <string>
#include <vector>

using namespace Steinberg;
using namespace Steinberg::Vst;

namespace {

int gFailures = 0;
int gPort = 0;
const double kRate = 48000.;
const int32 kBlock = 512;
const long long kBeatFrames = 24000; // at 120 BPM

void check(bool ok, const std::string& what)
{
	printf("%s: %s\n", ok ? "PASS" : "FAIL", what.c_str());
	if (!ok) ++gFailures;
}

std::map<std::string, std::string> ReadCases(const char* path)
{
	std::map<std::string, std::string> cases;
	std::ifstream in(path);
	std::string line, name;
	while (std::getline(in, line)) {
		if (line.size() > 2 && line.front() == '[' && line.back() == ']') {
			name = line.substr(1, line.size() - 2);
			cases[name];
		} else if (!name.empty()) {
			cases[name] += line + "\n";
		}
	}
	for (auto& [k, v] : cases) {
		while (!v.empty() && v.back() == '\n') v.pop_back();
	}
	return cases;
}

std::string Eval(const std::string& code)
{
	int fd = socket(AF_INET, SOCK_DGRAM, 0);
	sockaddr_in addr{};
	addr.sin_family = AF_INET;
	addr.sin_port = htons((uint16_t)gPort);
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	sendto(fd, code.data(), code.size(), 0, (sockaddr*)&addr, sizeof(addr));
	timeval timeout{15, 0};
	setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
	char buf[65536];
	ssize_t n = recv(fd, buf, sizeof(buf), 0);
	close(fd);
	return n > 0 ? std::string(buf, n) : std::string();
}

struct Out {
	long long frame; // absolute: block start + offset
	Event e;
};

bool gSilent = true; // every rendered audio sample was 0

// Renders `blocks` blocks with the transport playing from `beat`.
std::vector<Out> Play(IAudioProcessor* proc, double beat, int blocks)
{
	std::vector<Out> out;
	std::vector<float> left(kBlock), right(kBlock);
	float* channels[] = {left.data(), right.data()};
	for (int b = 0; b < blocks; ++b) {
		std::fill(left.begin(), left.end(), 1.f); // the plugin must overwrite these
		std::fill(right.begin(), right.end(), 1.f);
		AudioBusBuffers audio{};
		audio.numChannels = 2;
		audio.channelBuffers32 = channels;
		ProcessContext ctx{};
		ctx.state = ProcessContext::kPlaying | ProcessContext::kTempoValid | ProcessContext::kProjectTimeMusicValid
		            | ProcessContext::kTimeSigValid;
		ctx.sampleRate = kRate;
		ctx.tempo = 120.;
		ctx.timeSigNumerator = 4;
		ctx.timeSigDenominator = 4;
		ctx.projectTimeMusic = beat + b * kBlock / (double)kBeatFrames;
		ctx.projectTimeSamples = (TSamples)std::llround(ctx.projectTimeMusic * kBeatFrames);

		EventList events;
		ProcessData data{};
		data.processMode = kRealtime;
		data.symbolicSampleSize = kSample32;
		data.numSamples = kBlock;
		data.numOutputs = 1;
		data.outputs = &audio;
		data.outputEvents = &events;
		data.processContext = &ctx;
		proc->process(data);
		for (int i = 0; i < kBlock; ++i) gSilent &= left[i] == 0.f && right[i] == 0.f;
		for (int32 i = 0; i < events.getEventCount(); ++i) {
			Event e{};
			events.getEvent(i, e);
			out.push_back({ctx.projectTimeSamples + e.sampleOffset, e});
		}
	}
	return out;
}

// The checks are about notes; a language may add program and control changes.
std::vector<Out> Notes(const std::vector<Out>& events)
{
	std::vector<Out> notes;
	for (const Out& o : events) {
		if (o.e.type == Event::kNoteOnEvent || o.e.type == Event::kNoteOffEvent) notes.push_back(o);
	}
	return notes;
}

bool IsNote(const Out& o, long long frame, bool on, int16 pitch)
{
	if (o.frame != frame) return false;
	if (on) return o.e.type == Event::kNoteOnEvent && o.e.noteOn.pitch == pitch;
	return o.e.type == Event::kNoteOffEvent && o.e.noteOff.pitch == pitch;
}

std::string Describe(const std::vector<Out>& events)
{
	std::string s;
	char buf[80];
	for (const Out& o : events) {
		if (o.e.type == Event::kNoteOnEvent)
			snprintf(buf, sizeof buf, " on %d v%.3f @%lld", o.e.noteOn.pitch, o.e.noteOn.velocity, o.frame);
		else if (o.e.type == Event::kNoteOffEvent)
			snprintf(buf, sizeof buf, " off %d @%lld", o.e.noteOff.pitch, o.frame);
		else if (o.e.type == Event::kLegacyMIDICCOutEvent)
			snprintf(buf, sizeof buf, " cc%d=%d ch%d @%lld", o.e.midiCCOut.controlNumber, o.e.midiCCOut.value,
			         o.e.midiCCOut.channel, o.frame);
		else
			snprintf(buf, sizeof buf, " type%d @%lld", o.e.type, o.frame);
		s += buf;
	}
	return s;
}

std::string SaveState(IComponent* component)
{
	auto* stream = new MemoryStream;
	std::string state;
	if (component->getState(stream) == kResultOk) state.assign(stream->getData(), (size_t)stream->getSize());
	stream->release();
	return state;
}

bool LoadState(IComponent* component, const std::string& state)
{
	auto* stream = new MemoryStream;
	int32 written = 0;
	stream->write((void*)state.data(), (int32)state.size(), &written);
	stream->seek(0, IBStream::kIBSeekSet, nullptr);
	bool ok = component->setState(stream) == kResultOk;
	stream->release();
	return ok;
}

} // namespace

int main(int argc, char** argv)
{
	if (argc != 4) {
		fprintf(stderr, "usage: test_vst3_host <plugin .vst3> <cases file> <udp port>\n");
		return 2;
	}
	auto cases = ReadCases(argv[2]);
	gPort = atoi(argv[3]);

	std::string error;
	auto module = VST3::Hosting::Module::create(argv[1], error);
	if (!module) {
		fprintf(stderr, "cannot load %s: %s\n", argv[1], error.c_str());
		return 1;
	}
	auto factory = module->getFactory();
	const VST3::Hosting::ClassInfo* audio = nullptr;
	auto classes = factory.classInfos();
	for (const auto& c : classes) {
		if (c.category() == kVstAudioEffectClass) audio = &c;
	}
	check(audio != nullptr, "the factory has an audio module class");
	if (!audio) return 1;
	// A note effect has no VST3 category; clap-wrapper files it as an instrument.
	check(audio->subCategoriesString().find("Instrument") != std::string::npos,
	      "listed as an instrument: " + audio->subCategoriesString());

	HostApplication host;
	auto component = factory.createInstance<IComponent>(audio->ID());
	check(component && component->initialize(&host) == kResultOk, "create and initialize");
	if (!component) return 1;
	FUnknownPtr<IAudioProcessor> proc(component);
	check(proc, "the component is an IAudioProcessor");
	if (!proc) return 1;

	// Live refuses an instrument without an event input and an audio output.
	BusInfo audioOut{};
	check(component->getBusCount(kAudio, kInput) == 0 && component->getBusCount(kAudio, kOutput) == 1
	          && component->getBusInfo(kAudio, kOutput, 0, audioOut) == kResultOk && audioOut.channelCount == 2,
	      "one stereo audio output, no audio input");
	check(component->getBusCount(kEvent, kInput) == 1 && component->getBusCount(kEvent, kOutput) == 1,
	      "one event input, one event output");

	ProcessSetup setup{kRealtime, kSample32, kBlock, kRate};
	check(proc->setupProcessing(setup) == kResultOk, "setupProcessing");
	check(component->setActive(true) == kResultOk, "setActive");
	proc->setProcessing(true);

	std::string reply = Eval(cases["expr"]);
	check(reply.find(cases["expr_out"]) != std::string::npos, "expression reply contains its value: " + reply);

	reply = Eval(cases["note"]);
	check(reply.find("clip: ") != std::string::npos, "note becomes a clip: " + reply);
	auto ev = Notes(Play(proc, 0., (int)(8 * kBeatFrames / kBlock)));
	check(ev.size() == 4 && IsNote(ev[0], 0, true, 60) && std::fabs(ev[0].e.noteOn.velocity - 100 / 127.f) < 1e-6f
	          && IsNote(ev[1], kBeatFrames, false, 60) && IsNote(ev[2], 4 * kBeatFrames, true, 60)
	          && IsNote(ev[3], 5 * kBeatFrames, false, 60),
	      "MIDI notes become VST3 note events on frames 0, 24000 and loop at 96000:" + Describe(ev));

	std::string saved = SaveState(component);
	check(!saved.empty() && LoadState(component, saved), "state save and load");
	check(SaveState(component) == saved, "state round trip");
	ev = Notes(Play(proc, 0., 2));
	check(!ev.empty() && IsNote(ev[0], 0, true, 60), "the loaded clip plays:" + Describe(ev));

	if (cases.count("cc")) {
		Eval(cases["cc"]);
		ev = Play(proc, 0., 2);
		bool cc = false;
		for (const Out& o : ev) {
			cc |= o.e.type == Event::kLegacyMIDICCOutEvent && o.e.midiCCOut.controlNumber == 7
			      && o.e.midiCCOut.value == 100 && o.e.midiCCOut.channel == 0;
		}
		check(cc, "a control change becomes a legacy MIDI CC event:" + Describe(ev));
	}

	check(gSilent, "the audio output is silent");

	proc->setProcessing(false);
	component->setActive(false);
	component->terminate();
	printf("%d failure(s)\n", gFailures);
	return gFailures ? 1 : 0;
}
