#include "timeline.hpp"

#include <cstring>

namespace {

void PutU64(std::string& out, uint64_t v)
{
	for (int i = 0; i < 8; ++i) out += (char)(uint8_t)(v >> (8 * i));
}

bool GetU64(const std::string& in, size_t& pos, uint64_t& v)
{
	if (pos > in.size() || in.size() - pos < 8) return false;
	v = 0;
	for (int i = 0; i < 8; ++i) v |= (uint64_t)(uint8_t)in[pos + i] << (8 * i);
	pos += 8;
	return true;
}

void PutDouble(std::string& out, double d)
{
	uint64_t v;
	memcpy(&v, &d, sizeof(v));
	PutU64(out, v);
}

bool GetDouble(const std::string& in, size_t& pos, double& d)
{
	uint64_t v;
	if (!GetU64(in, pos, v)) return false;
	memcpy(&d, &v, sizeof(d));
	return std::isfinite(d);
}

} // namespace

Clip MakeClip(std::vector<MidiEvent> events, double barBeats)
{
	std::stable_sort(events.begin(), events.end(), [](const MidiEvent& a, const MidiEvent& b) { return a.beat < b.beat; });
	Clip clip;
	const double last = events.empty() ? 0. : std::fmax(events.back().beat, 0.);
	if (!(barBeats > 0.)) barBeats = 4.;
	clip.length = (std::floor(last / barBeats) + 1.) * barBeats;
	clip.events = std::move(events);
	return clip;
}

void WriteClip(std::string& out, const Clip& clip)
{
	PutU64(out, clip.events.size());
	PutDouble(out, clip.length);
	for (const MidiEvent& e : clip.events) {
		PutDouble(out, e.beat);
		out += (char)e.size;
		out.append((const char*)e.data, 3);
	}
}

bool ReadClip(const std::string& in, size_t& pos, Clip& clip)
{
	uint64_t count;
	double length;
	if (!GetU64(in, pos, count) || !GetDouble(in, pos, length) || length < 0.) return false;
	if (count > (in.size() - pos) / 12) return false; // 12 bytes per event
	clip.events.clear();
	clip.length = length;
	for (uint64_t i = 0; i < count; ++i) {
		MidiEvent e;
		if (!GetDouble(in, pos, e.beat)) return false;
		e.size = (uint8_t)in[pos];
		memcpy(e.data, in.data() + pos + 1, 3);
		pos += 4;
		if (e.size < 1 || e.size > 3 || e.beat < 0. || e.beat >= length) return false;
		clip.events.push_back(e);
	}
	return true;
}
