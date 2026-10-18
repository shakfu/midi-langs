// A midi-lang as a CLAP note effect: code from the editor or UDP runs on an evaluator thread,
// and its MIDI goes to the host on one note output port. Adapted from sapf's examples/clap.
//
// Modes:
//   clip (default) - an evaluation runs at once in script time and becomes a clip: its events in
//                    beats, looped over the host timeline while the transport plays. It follows
//                    tempo, seeks, loops and offline renders, and is saved with the project.
//   live           - code plays as it runs: script time waits for the frames the host renders.
//
// Threads:
//   main      - CLAP lifecycle, editor, state
//   audio     - process() plays the clip or the live queue; it blocks only in live mode while
//               rendering offline, until the evaluator has queued the block's events
//   evaluator - owns the interpreter; runs jobs in order, and between jobs, live voices
//   udp       - receives datagrams, waits for the evaluator, replies with the output
//
// Interpreters keep process-wide state, so only one instance can exist per process.

#include "editor.hpp"
#include "language.hpp"
#include "timeline.hpp"
#include "udp_server.hpp"

#include "midi_clock.h"

#include <clap/clap.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <future>
#include <limits>
#include <memory>
#include <mutex>
#include <string>
#include <thread>

namespace {

const size_t kMaxPendingLog = 1 << 20;
const auto kUdpReplyTimeout = std::chrono::seconds(10);
const double kMaxClipBeats = 1024.;
const size_t kMaxClipEvents = 200000;
const double kLiveLookaheadSeconds = 0.05; // how far live code runs ahead of the host
const auto kOfflineWaitLimit = std::chrono::seconds(10);
const double kNever = std::numeric_limits<double>::infinity();

std::atomic<bool> gInstanceExists{false};

const char* const kFeatures[] = {
	CLAP_PLUGIN_FEATURE_NOTE_EFFECT,
	nullptr,
};

clap_plugin_descriptor_t gDescriptor = {
	CLAP_VERSION_INIT,
	nullptr, // id, name, description: set in entry_init from kLanguage
	nullptr,
	"shakfu",
	"https://github.com/shakfu/midi-langs",
	"",
	"",
	MIDI_LANGS_VERSION,
	nullptr,
	kFeatures,
};

uint16_t ConfiguredUdpPort(bool* fromEnv)
{
	const char* env = getenv(kLanguage.udpPortEnv);
	*fromEnv = env != nullptr;
	if (!env) return kLanguage.udpPort;
	long port = strtol(env, nullptr, 10);
	return (port > 0 && port < 65536) ? (uint16_t)port : kLanguage.udpPort;
}

// State the evaluator and audio threads share.
struct Timeline {
	std::atomic<Clip*> nextClip{nullptr};    // evaluator -> audio
	std::atomic<Clip*> retiredClip{nullptr}; // audio -> evaluator, which frees it
	SpscRing<LiveEvent, 16384> liveQueue;    // evaluator -> audio
	std::atomic<uint32_t> gen{0};            // bumped by stop
	std::atomic<bool> cancel{false};         // stop the running code
	std::atomic<bool> live{false};
	std::atomic<bool> offline{false};
	std::atomic<double> barBeats{4.};
	std::atomic<double> horizon{0.};      // live: beat the evaluator may run ahead to
	std::atomic<double> frontier{kNever}; // live: every event before this beat is queued

	// The clip state saves: the last one published or loaded, none after a stop.
	void setSaved(const Clip& clip)
	{
		std::lock_guard<std::mutex> lock(savedMutex);
		saved = clip;
	}

	std::mutex savedMutex;
	Clip saved;
};

// Runs code on one thread in submission order, with a midi_sink installed there.
class Evaluator {
public:
	using Reply = std::shared_ptr<std::promise<std::string>>;
	using Log = std::function<void(const std::string&)>;

	Evaluator(Timeline& timeline, Log log) : tl_(timeline), log_(std::move(log))
	{
		std::promise<bool> started;
		std::future<bool> ok = started.get_future();
		thread_ = std::thread([this, p = std::move(started)]() mutable { loop(p); });
		ok_ = ok.get();
	}

	~Evaluator()
	{
		{
			std::lock_guard<std::mutex> lock(mutex_);
			quit_ = true;
		}
		tl_.cancel = true; // ends a running evaluation at its next send, sleep or interpreter check
		cv_.notify_one();
		thread_.join();
		for (Job& job : jobs_) {
			if (job.reply) job.reply->set_value("error: evaluator stopped\n");
		}
		delete tl_.nextClip.exchange(nullptr);
		delete tl_.retiredClip.exchange(nullptr);
	}

	bool ok() const { return ok_; }

	void eval(std::string code, Reply reply = nullptr)
	{
		{
			std::lock_guard<std::mutex> lock(mutex_);
			jobs_.push_back({std::move(code), std::move(reply)});
			// Offline rendering waits for a queued job as for a running one.
			tl_.frontier = std::min(tl_.frontier.load(), tl_.horizon.load());
		}
		cv_.notify_one();
	}

	// Stop: queued evaluations are dropped, not run after the stop.
	void dropQueued()
	{
		std::deque<Job> dropped;
		{
			std::lock_guard<std::mutex> lock(mutex_);
			dropped.swap(jobs_);
		}
		for (Job& job : dropped) {
			if (job.reply) job.reply->set_value("error: stopped\n");
		}
	}

	void notify() { cv_.notify_one(); }

private:
	struct Job {
		std::string code;
		Reply reply;
		uint32_t gen = 0; // stop generation when dequeued
	};

	void loop(std::promise<bool>& started)
	{
		midi_sink sink = {this, &Evaluator::Send, &Evaluator::Wait, &Evaluator::Cancelled};
		midi_sink_install(&sink);
		lang_ = kLanguage.create();
		started.set_value(lang_ != nullptr);
		if (!lang_) {
			midi_sink_install(nullptr);
			return;
		}
		for (;;) {
			Job job;
			bool haveJob = false;
			{
				std::unique_lock<std::mutex> lock(mutex_);
				if (!quit_ && jobs_.empty()) {
					// Live voices poll the host clock; otherwise wake to free retired clips.
					double wake;
					const bool voices = tl_.live && lang_->nextWake(&wake);
					tl_.frontier = voices ? wake / kScriptMsPerBeat : kNever;
					cv_.wait_for(lock, voices ? std::chrono::milliseconds(1) : std::chrono::milliseconds(50));
				}
				if (quit_) break;
				if (!jobs_.empty()) {
					job = std::move(jobs_.front());
					jobs_.pop_front();
					job.gen = tl_.gen;
					haveJob = true;
				}
			}
			delete tl_.retiredClip.exchange(nullptr);
			if (tl_.cancel.exchange(false)) lang_->stopVoices();
			if (haveJob && job.gen != tl_.gen) {
				// A stop came after this job was dequeued
				if (job.reply) job.reply->set_value("error: stopped\n");
			} else if (haveJob) {
				run(job);
			}
			else if (tl_.live) resumeDueVoices();
		}
		lang_->stopVoices();
		lang_.reset();
		midi_sink_install(nullptr);
	}

	void run(Job& job)
	{
		std::string out;
		jobGen_ = job.gen;
		capped_ = false;
		if (tl_.live) {
			// New code starts where the host will be when its first events can still play on time.
			midi_clock_set_ms(tl_.horizon * kScriptMsPerBeat);
			tl_.frontier = midi_clock_ms() / kScriptMsPerBeat;
			lang_->eval(job.code, out);
		} else {
			lang_->stopVoices();
			clipEvents_.clear();
			midi_clock_set_ms(0.);
			lang_->eval(job.code, out);
			double t;
			while (!Cancelled(this) && lang_->nextWake(&t)) {
				midi_clock_wait_until(t);
				if (Cancelled(this)) break;
				lang_->resume(t, out);
			}
			lang_->stopVoices();
			if (capped_) {
				out += "stopped: clips are limited to " + std::to_string((int)kMaxClipBeats) + " beats and "
				     + std::to_string(kMaxClipEvents) + " events; use Live for code that does not end\n";
			} else if (!clipEvents_.empty() && tl_.gen == jobGen_) {
				out += publish(MakeClip(std::move(clipEvents_), tl_.barBeats));
			}
			clipEvents_.clear();
		}
		log_(out);
		if (job.reply) job.reply->set_value(std::move(out));
	}

	// Returns a summary for the output.
	std::string publish(Clip clip)
	{
		char summary[96];
		snprintf(summary, sizeof(summary), "clip: %zu events, %g beats\n", clip.events.size(), clip.length);
		tl_.setSaved(clip);
		delete tl_.retiredClip.exchange(nullptr);
		delete tl_.nextClip.exchange(new Clip(std::move(clip)));
		return summary;
	}

	void resumeDueVoices()
	{
		double t;
		if (!lang_->nextWake(&t) || t / kScriptMsPerBeat > tl_.horizon) return;
		jobGen_ = tl_.gen;
		std::string out;
		lang_->resume(t, out);
		if (!out.empty()) log_(out);
	}

	// --- midi_sink ---------------------------------------------------------

	static void Send(void* user, double ms, const unsigned char* msg, size_t len)
	{
		auto* self = static_cast<Evaluator*>(user);
		if (Cancelled(user) || len < 1 || len > 3) return; // sysex is not supported
		MidiEvent e{ms / kScriptMsPerBeat, {0, 0, 0}, (uint8_t)len};
		memcpy(e.data, msg, len);
		if (!self->tl_.live) {
			if (self->clipEvents_.size() >= kMaxClipEvents) self->capped_ = true;
			else self->clipEvents_.push_back(e);
			return;
		}
		while (!self->tl_.liveQueue.push({e, self->jobGen_})) {
			if (Cancelled(user)) return;
			std::this_thread::sleep_for(std::chrono::milliseconds(1));
		}
	}

	static void Wait(void* user, double ms)
	{
		auto* self = static_cast<Evaluator*>(user);
		const double beat = ms / kScriptMsPerBeat;
		if (!self->tl_.live) {
			if (beat > kMaxClipBeats) self->capped_ = true;
			return;
		}
		self->tl_.frontier = beat;
		while (beat > self->tl_.horizon && !Cancelled(user)) {
			std::this_thread::sleep_for(std::chrono::milliseconds(1));
		}
	}

	static int Cancelled(void* user)
	{
		auto* self = static_cast<Evaluator*>(user);
		return self->tl_.cancel || self->capped_;
	}

	Timeline& tl_;
	Log log_;
	std::unique_ptr<Language> lang_;
	std::vector<MidiEvent> clipEvents_;
	uint32_t jobGen_ = 0;
	std::atomic<bool> capped_{false}; // read by language watchers (guile) on their own threads
	bool ok_ = false;
	std::mutex mutex_;
	std::condition_variable cv_;
	std::deque<Job> jobs_;
	bool quit_ = false;
	std::thread thread_;
};

class Plugin {
public:
	explicit Plugin(const clap_host_t* host) : host_(host)
	{
		plugin_.desc = &gDescriptor;
		plugin_.plugin_data = this;
		plugin_.init = [](const clap_plugin_t* p) { return self(p)->init(); };
		plugin_.destroy = [](const clap_plugin_t* p) { delete self(p); };
		plugin_.activate = [](const clap_plugin_t* p, double rate, uint32_t, uint32_t) {
			self(p)->sampleRate_ = rate;
			return true;
		};
		plugin_.deactivate = [](const clap_plugin_t*) {};
		plugin_.start_processing = [](const clap_plugin_t*) { return true; };
		plugin_.stop_processing = [](const clap_plugin_t*) {};
		plugin_.reset = [](const clap_plugin_t* p) { self(p)->stop(); };
		plugin_.process = [](const clap_plugin_t* p, const clap_process_t* process) { return self(p)->process(process); };
		plugin_.get_extension = [](const clap_plugin_t* p, const char* id) { return self(p)->extension(id); };
		plugin_.on_main_thread = [](const clap_plugin_t*) {};
	}

	~Plugin()
	{
		udp_.stop();
		destroyEditor();
		evaluator_.reset();
		delete clip_;
		gInstanceExists = false;
	}

	const clap_plugin_t* clapPlugin() const { return &plugin_; }

private:
	static Plugin* self(const clap_plugin_t* p) { return static_cast<Plugin*>(p->plugin_data); }

	bool init()
	{
		evaluator_ = std::make_unique<Evaluator>(tl_, [this](const std::string& text) { log(text); });
		if (!evaluator_->ok()) {
			fprintf(stderr, "%s: the interpreter failed to start\n", kLanguage.name);
			return false;
		}
		bool portFromEnv = false;
		udpPort_ = ConfiguredUdpPort(&portFromEnv);
		setUdp(portFromEnv);
		return true;
	}

	// Stops the running code and its voices; the audio thread releases every held note.
	void stop()
	{
		tl_.cancel = true;
		tl_.gen++;
		tl_.setSaved(Clip());
		if (evaluator_) {
			evaluator_->dropQueued();
			evaluator_->notify();
		}
	}

	// --- audio --------------------------------------------------------------

	clap_process_status process(const clap_process_t* process)
	{
		const clap_output_events_t* out = process->out_events;
		const uint32_t frames = process->frames_count;
		if (!out || frames == 0 || sampleRate_ <= 0.) return CLAP_PROCESS_CONTINUE;
		auto emit = [out](uint32_t offset, const uint8_t* data, uint8_t size) {
			clap_event_midi_t ev{};
			ev.header.size = sizeof(ev);
			ev.header.time = offset;
			ev.header.space_id = CLAP_CORE_EVENT_SPACE_ID;
			ev.header.type = CLAP_EVENT_MIDI;
			ev.port_index = 0;
			memcpy(ev.data, data, size);
			out->try_push(out, &ev.header);
		};

		const clap_event_transport_t* tr = process->transport;
		const double tempo = (tr && (tr->flags & CLAP_TRANSPORT_HAS_TEMPO) && tr->tempo > 0.) ? tr->tempo : 120.;
		const double beatsPerFrame = tempo / 60. / sampleRate_;
		if (tr && (tr->flags & CLAP_TRANSPORT_HAS_TIME_SIGNATURE) && tr->tsig_num > 0 && tr->tsig_denom > 0) {
			tl_.barBeats = tr->tsig_num * 4. / tr->tsig_denom;
		}

		const uint32_t gen = tl_.gen;
		if (gen != gen_) {
			gen_ = gen;
			clipMuted_ = true;
			clipPlayer_.stop(emit);
			livePlayer_.stop(emit);
		}
		if (!tl_.retiredClip.load()) {
			if (Clip* next = tl_.nextClip.exchange(nullptr)) {
				tl_.retiredClip = clip_;
				clip_ = next;
				clipMuted_ = false;
			}
		}

		// Clip: on the host's timeline; without one, on our own from activation, as if playing.
		const bool hostTimeline = tr && (tr->flags & CLAP_TRANSPORT_HAS_BEATS_TIMELINE);
		const bool playing = !hostTimeline || (tr->flags & CLAP_TRANSPORT_IS_PLAYING);
		const double songBeat = hostTimeline ? (double)tr->song_pos_beats / CLAP_BEATTIME_FACTOR : freeBeat_;
		const bool clipMode = !tl_.live;
		clipPlayer_.render(clipMode && !clipMuted_ ? clip_ : nullptr, playing, {songBeat, beatsPerFrame, frames}, emit);
		freeBeat_ += frames * beatsPerFrame;

		// Live: on our own timeline, which runs whether or not the transport plays.
		const BlockTime live{liveBeat_, beatsPerFrame, frames};
		tl_.horizon = live.end() + kLiveLookaheadSeconds * tempo / 60.;
		if (tl_.live && tl_.offline) {
			const auto deadline = std::chrono::steady_clock::now() + kOfflineWaitLimit;
			evaluator_->notify();
			while (tl_.frontier < live.end() && std::chrono::steady_clock::now() < deadline) {
				std::this_thread::sleep_for(std::chrono::microseconds(200));
			}
		}
		livePlayer_.render(tl_.liveQueue, gen, live, emit);
		liveBeat_ = live.end();
		return CLAP_PROCESS_CONTINUE;
	}

	// --- output -------------------------------------------------------------

	void log(const std::string& text)
	{
		std::lock_guard<std::mutex> lock(logMutex_);
		if (pendingLog_.size() < kMaxPendingLog) pendingLog_ += text;
	}

	std::string drainLog()
	{
		std::lock_guard<std::mutex> lock(logMutex_);
		std::string out;
		out.swap(pendingLog_);
		return out;
	}

	// --- input --------------------------------------------------------------

	static std::string Echo(const char* prompt, const std::string& code)
	{
		return std::string(prompt) + code + (code.empty() || code.back() != '\n' ? "\n" : "");
	}

	void evalFromEditor(const std::string& code)
	{
		log(Echo("> ", code));
		evaluator_->eval(code);
	}

	std::string evalFromUdp(const std::string& code)
	{
		log(Echo("udp> ", code));
		auto reply = std::make_shared<std::promise<std::string>>();
		std::future<std::string> result = reply->get_future();
		evaluator_->eval(code, reply);
		if (result.wait_for(kUdpReplyTimeout) != std::future_status::ready) {
			return "busy: evaluation still running\n";
		}
		std::string out = result.get();
		return out.empty() ? "ok\n" : out;
	}

	void setUdp(bool on)
	{
		udp_.stop();
		if (on) {
			on = udp_.start(udpPort_, [this](const std::string& msg) { return evalFromUdp(msg); });
			log(on ? "listening on UDP 127.0.0.1:" + std::to_string(udpPort_) + "\n"
			       : "could not bind UDP port " + std::to_string(udpPort_) + "\n");
		}
		udpOn_ = on;
		if (editor_) editor_->setUdpState(on);
	}

	// Switching mode releases what the other mode was playing.
	void setLive(bool on)
	{
		if (on == tl_.live) return;
		tl_.live = on;
		stop();
	}

	// --- extensions ---------------------------------------------------------

	const void* extension(const char* id)
	{
		if (!strcmp(id, CLAP_EXT_NOTE_PORTS)) return &kNotePorts;
		if (!strcmp(id, CLAP_EXT_AUDIO_PORTS)) return &kAudioPorts;
		if (!strcmp(id, CLAP_EXT_STATE)) return &kState;
		if (!strcmp(id, CLAP_EXT_RENDER)) return &kRender;
		if (!strcmp(id, CLAP_EXT_GUI)) return &kGui;
		if (!strcmp(id, CLAP_EXT_TIMER_SUPPORT)) return &kTimer;
		return nullptr;
	}

	static const clap_plugin_note_ports_t kNotePorts;
	static const clap_plugin_audio_ports_t kAudioPorts;
	static const clap_plugin_state_t kState;
	static const clap_plugin_render_t kRender;
	static const clap_plugin_gui_t kGui;
	static const clap_plugin_timer_support_t kTimer;

	// State: version byte, flags byte, u64 text length, editor text, clip (timeline.hpp).
	// "No wrap" is a flag so a zero flags byte means the defaults.
	enum : char { kStateUdp = 1, kStateNoWrap = 2, kStateLive = 4 };
	static const char kStateVersion = 1;

	bool saveState(const clap_ostream_t* stream)
	{
		std::string data;
		data += kStateVersion;
		data += (char)((udpOn_ ? kStateUdp : 0) | (wrap_ ? 0 : kStateNoWrap) | (tl_.live ? kStateLive : 0));
		const uint64_t textLen = editorText_.size();
		for (int i = 0; i < 8; ++i) data += (char)(uint8_t)(textLen >> (8 * i));
		data += editorText_;
		{
			std::lock_guard<std::mutex> lock(tl_.savedMutex);
			WriteClip(data, tl_.saved);
		}
		size_t written = 0;
		while (written < data.size()) {
			int64_t n = stream->write(stream, data.data() + written, data.size() - written);
			if (n <= 0) return false;
			written += (size_t)n;
		}
		return true;
	}

	bool loadState(const clap_istream_t* stream)
	{
		std::string data;
		char buf[4096];
		for (;;) {
			int64_t n = stream->read(stream, buf, sizeof(buf));
			if (n < 0) return false;
			if (n == 0) break;
			data.append(buf, (size_t)n);
		}
		if (data.size() < 10 || data[0] != kStateVersion) return false;
		uint64_t textLen = 0;
		for (int i = 0; i < 8; ++i) textLen |= (uint64_t)(uint8_t)data[2 + i] << (8 * i);
		if (textLen > data.size() - 10) return false;
		size_t pos = 10 + textLen;
		Clip clip;
		if (!ReadClip(data, pos, clip)) return false;

		const char flags = data[1];
		editorText_ = data.substr(10, textLen);
		wrap_ = !(flags & kStateNoWrap);
		setLive(flags & kStateLive);
		if (editor_) {
			editor_->setText(editorText_);
			editor_->setWrap(wrap_);
			editor_->setLive(tl_.live);
		}
		setUdp(flags & kStateUdp);
		tl_.setSaved(clip);
		delete tl_.nextClip.exchange(clip.events.empty() ? nullptr : new Clip(std::move(clip)));
		return true;
	}

	bool createEditor()
	{
		EditorCallbacks callbacks;
		callbacks.eval = [this](const std::string& code) { evalFromEditor(code); };
		callbacks.stop = [this] { stop(); };
		callbacks.setUdp = [this](bool on) { setUdp(on); };
		callbacks.setWrap = [this](bool on) { wrap_ = on; };
		callbacks.setLive = [this](bool on) { setLive(on); };
		callbacks.textChanged = [this](const std::string& text) { editorText_ = text; };
		callbacks.drainLog = [this] { return drainLog(); };
		editor_ = CreateEditor(std::move(callbacks), EditorState{editorText_, udpOn_, wrap_, tl_.live, udpPort_}, kLanguage);
		if (!editor_) return false;
		// The host timer drives the editor; on X11 nothing else dispatches its events.
		auto* timers = (const clap_host_timer_support_t*)host_->get_extension(host_, CLAP_EXT_TIMER_SUPPORT);
		if (!timers || !timers->register_timer(host_, 16, &timerId_)) {
			timerId_ = CLAP_INVALID_ID;
			if (!strcmp(EditorWindowApi(), CLAP_WINDOW_API_X11)) log("host has no timer support; the editor will not update\n");
		}
		return true;
	}

	void destroyEditor()
	{
		if (timerId_ != CLAP_INVALID_ID) {
			auto* timers = (const clap_host_timer_support_t*)host_->get_extension(host_, CLAP_EXT_TIMER_SUPPORT);
			if (timers) timers->unregister_timer(host_, timerId_);
			timerId_ = CLAP_INVALID_ID;
		}
		editor_.reset();
	}

	clap_plugin_t plugin_{};
	const clap_host_t* host_;
	Timeline tl_;
	std::unique_ptr<Evaluator> evaluator_;
	UdpServer udp_;
	std::unique_ptr<Editor> editor_;
	clap_id timerId_ = CLAP_INVALID_ID;
	std::string editorText_ = kLanguage.defaultCode;
	uint16_t udpPort_ = 0;
	bool udpOn_ = false;
	bool wrap_ = true;
	unsigned width_ = kEditorWidth, height_ = kEditorHeight;
	double sampleRate_ = 0.;

	// Audio thread only.
	Clip* clip_ = nullptr;
	bool clipMuted_ = false;
	uint32_t gen_ = 0;
	double freeBeat_ = 0.;
	double liveBeat_ = 0.;
	ClipPlayer clipPlayer_;
	LivePlayer livePlayer_;

	std::mutex logMutex_;
	std::string pendingLog_;
};

const clap_plugin_note_ports_t Plugin::kNotePorts = {
	[](const clap_plugin_t*, bool isInput) -> uint32_t { return isInput ? 0 : 1; },
	[](const clap_plugin_t*, uint32_t index, bool isInput, clap_note_port_info_t* info) {
		if (isInput || index != 0) return false;
		info->id = 0;
		info->supported_dialects = CLAP_NOTE_DIALECT_MIDI;
		info->preferred_dialect = CLAP_NOTE_DIALECT_MIDI;
		snprintf(info->name, sizeof(info->name), "%s", "midi out");
		return true;
	},
};

const clap_plugin_audio_ports_t Plugin::kAudioPorts = {
	[](const clap_plugin_t*, bool) -> uint32_t { return 0; },
	[](const clap_plugin_t*, uint32_t, bool, clap_audio_port_info_t*) { return false; },
};

const clap_plugin_state_t Plugin::kState = {
	[](const clap_plugin_t* p, const clap_ostream_t* s) { return self(p)->saveState(s); },
	[](const clap_plugin_t* p, const clap_istream_t* s) { return self(p)->loadState(s); },
};

const clap_plugin_render_t Plugin::kRender = {
	[](const clap_plugin_t*) { return false; },
	[](const clap_plugin_t* p, clap_plugin_render_mode mode) {
		self(p)->tl_.offline = mode == CLAP_RENDER_OFFLINE;
		return true;
	},
};

const clap_plugin_gui_t Plugin::kGui = {
	// is_api_supported
	[](const clap_plugin_t*, const char* api, bool floating) {
		return !floating && !strcmp(api, EditorWindowApi());
	},
	// get_preferred_api
	[](const clap_plugin_t*, const char** api, bool* floating) {
		*api = EditorWindowApi();
		*floating = false;
		return true;
	},
	// create
	[](const clap_plugin_t* p, const char* api, bool floating) {
		return !floating && !strcmp(api, EditorWindowApi()) && self(p)->createEditor();
	},
	// destroy
	[](const clap_plugin_t* p) { self(p)->destroyEditor(); },
	// set_scale: pugl reads the platform scale factor itself
	[](const clap_plugin_t*, double) { return false; },
	// get_size
	[](const clap_plugin_t* p, uint32_t* w, uint32_t* h) {
		*w = self(p)->width_;
		*h = self(p)->height_;
		return true;
	},
	// can_resize
	[](const clap_plugin_t*) { return true; },
	// get_resize_hints
	[](const clap_plugin_t*, clap_gui_resize_hints_t* hints) {
		hints->can_resize_horizontally = true;
		hints->can_resize_vertically = true;
		hints->preserve_aspect_ratio = false;
		hints->aspect_ratio_width = 0;
		hints->aspect_ratio_height = 0;
		return true;
	},
	// adjust_size
	[](const clap_plugin_t*, uint32_t* w, uint32_t* h) {
		*w = std::max<uint32_t>(*w, 480);
		*h = std::max<uint32_t>(*h, 320);
		return true;
	},
	// set_size
	[](const clap_plugin_t* p, uint32_t w, uint32_t h) {
		Plugin* s = self(p);
		s->width_ = w;
		s->height_ = h;
		if (s->editor_) s->editor_->setSize(w, h);
		return true;
	},
	// set_parent
	[](const clap_plugin_t* p, const clap_window_t* window) {
		return self(p)->editor_ && self(p)->editor_->setParent(window);
	},
	// set_transient
	[](const clap_plugin_t*, const clap_window_t*) { return false; },
	// suggest_title
	[](const clap_plugin_t*, const char*) {},
	// show
	[](const clap_plugin_t* p) {
		if (!self(p)->editor_) return false;
		self(p)->editor_->show();
		return true;
	},
	// hide
	[](const clap_plugin_t* p) {
		if (!self(p)->editor_) return false;
		self(p)->editor_->hide();
		return true;
	},
};

const clap_plugin_timer_support_t Plugin::kTimer = {
	[](const clap_plugin_t* p, clap_id id) {
		Plugin* s = self(p);
		if (s->editor_ && id == s->timerId_) s->editor_->idle();
	},
};

// --- factory and entry ------------------------------------------------------

const clap_plugin_factory_t kFactory = {
	[](const clap_plugin_factory_t*) -> uint32_t { return 1; },
	[](const clap_plugin_factory_t*, uint32_t index) -> const clap_plugin_descriptor_t* {
		return index == 0 ? &gDescriptor : nullptr;
	},
	[](const clap_plugin_factory_t*, const clap_host_t* host, const char* id) -> const clap_plugin_t* {
		if (!clap_version_is_compatible(host->clap_version) || strcmp(id, gDescriptor.id)) return nullptr;
		if (gInstanceExists.exchange(true)) {
			fprintf(stderr, "%s: only one instance per process is supported\n", kLanguage.name);
			return nullptr;
		}
		return (new Plugin(host))->clapPlugin();
	},
};

} // namespace

extern "C" CLAP_EXPORT const clap_plugin_entry_t clap_entry = {
	CLAP_VERSION_INIT,
	[](const char*) {
		gDescriptor.id = kLanguage.id;
		gDescriptor.name = kLanguage.name;
		gDescriptor.description = kLanguage.description;
		return true;
	},
	[]() {},
	[](const char* factoryId) -> const void* {
		return strcmp(factoryId, CLAP_PLUGIN_FACTORY_ID) ? nullptr : &kFactory;
	},
};
