#pragma once

// MIDI events on a beat timeline, played into host blocks at frame offsets.
// No CLAP types, so test_timeline.cpp can drive it directly.

#include <algorithm>
#include <atomic>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

// Script time per beat: scripts are written against 120 BPM, then follow the host tempo.
const double kScriptMsPerBeat = 500.0;

struct MidiEvent {
	double beat;
	uint8_t data[3];
	uint8_t size;
};

struct Clip {
	std::vector<MidiEvent> events; // sorted by beat
	double length = 0.;            // beats, past the last event
};

// Sorts events by beat, keeping send order for equal beats; the length is whole bars past the last event.
Clip MakeClip(std::vector<MidiEvent> events, double barBeats);

// Appends clip to out; ReadClip parses it at pos and advances pos. Little-endian.
void WriteClip(std::string& out, const Clip& clip);
bool ReadClip(const std::string& in, size_t& pos, Clip& clip);

// Where a host block lies on the beat timeline.
struct BlockTime {
	double beat;          // at the block's first frame
	double beatsPerFrame; // tempo / 60 / sample rate
	uint32_t frames;

	// Frame of beat in this block, rounded; outside [0, frames) belongs to another block.
	long long offset(double at) const { return std::llround((at - beat) / beatsPerFrame); }
	double end() const { return beat + frames * beatsPerFrame; }
};

// Notes sounding on the output, so a stop, seek or new clip can release them.
class NoteTracker {
public:
	void track(const uint8_t* data, uint8_t size)
	{
		if (size < 3) return;
		const uint8_t status = data[0] & 0xF0, ch = data[0] & 0x0F;
		if (status == 0x90 && data[2] > 0) held_[data[1] & 0x7F] |= (uint16_t)(1u << ch);
		else if (status == 0x80 || status == 0x90) held_[data[1] & 0x7F] &= (uint16_t)~(1u << ch);
	}

	template <class Emit>
	void releaseAll(uint32_t offset, Emit&& emit)
	{
		for (int key = 0; key < 128; ++key) {
			for (int ch = 0; held_[key] && ch < 16; ++ch) {
				if (!(held_[key] & (1u << ch))) continue;
				const uint8_t off[3] = {(uint8_t)(0x80 | ch), (uint8_t)key, 0};
				emit(offset, off, (uint8_t)3);
				held_[key] &= (uint16_t)~(1u << ch);
			}
		}
	}

private:
	uint16_t held_[128] = {}; // bit per channel
};

// Loops a clip over the host timeline from beat 0. A new clip, a stop, or a jump in position
// releases the held notes first.
class ClipPlayer {
public:
	// clip may be null (silence). playing: the transport is running.
	template <class Emit>
	void render(const Clip* clip, bool playing, const BlockTime& t, Emit&& emit)
	{
		const bool jumped = std::fabs(t.beat - expected_) > t.beatsPerFrame;
		if (clip != last_ || !playing || jumped) notes_.releaseAll(0, emit);
		last_ = clip;
		expected_ = t.end();
		if (!playing || !clip || clip->events.empty() || clip->length <= 0.) {
			expected_ = NAN;
			return;
		}
		const double len = clip->length, end = t.end();
		// One segment per pass of the clip within the block.
		for (double s = t.beat; s < end;) {
			const double cycle = std::floor(s / len), pos = s - cycle * len;
			const double segEnd = std::fmin(end, (cycle + 1.) * len);
			const double posEnd = pos + (segEnd - s);
			for (const MidiEvent& e : clip->events) {
				// Half a frame either side: the rounded offset decides which block plays it.
				if (e.beat < pos - t.beatsPerFrame) continue;
				if (e.beat >= posEnd + t.beatsPerFrame) break;
				const long long at = t.offset(cycle * len + e.beat);
				if (at < 0 || at >= (long long)t.frames) continue;
				emit((uint32_t)at, e.data, e.size);
				notes_.track(e.data, e.size);
			}
			s = segEnd;
		}
	}

	template <class Emit>
	void stop(Emit&& emit)
	{
		notes_.releaseAll(0, emit);
		last_ = nullptr;
		expected_ = NAN;
	}

private:
	const Clip* last_ = nullptr;
	double expected_ = NAN; // beat at which the next block should start
	NoteTracker notes_;
};

// Single-producer, single-consumer ring of fixed capacity.
template <class T, size_t N>
class SpscRing {
public:
	bool push(const T& v)
	{
		const size_t head = head_.load(std::memory_order_relaxed);
		const size_t next = (head + 1) % N;
		if (next == tail_.load(std::memory_order_acquire)) return false;
		items_[head] = v;
		head_.store(next, std::memory_order_release);
		return true;
	}

	const T* front() const
	{
		const size_t tail = tail_.load(std::memory_order_relaxed);
		return tail == head_.load(std::memory_order_acquire) ? nullptr : &items_[tail];
	}

	void pop() { tail_.store((tail_.load(std::memory_order_relaxed) + 1) % N, std::memory_order_release); }

private:
	T items_[N];
	std::atomic<size_t> head_{0}, tail_{0};
};

struct LiveEvent {
	MidiEvent event;
	uint32_t gen; // events from before the last stop are dropped
};

// Plays events queued by the evaluator as the host's frames reach them. Late events play at once.
class LivePlayer {
public:
	template <size_t N, class Emit>
	void render(SpscRing<LiveEvent, N>& queue, uint32_t gen, const BlockTime& t, Emit&& emit)
	{
		long long last = 0;
		while (const LiveEvent* e = queue.front()) {
			if (e->gen != gen) {
				queue.pop();
				continue;
			}
			long long at = t.offset(e->event.beat);
			if (at >= (long long)t.frames) break;
			at = std::max(at, last); // late, or slightly out of order: keep the output sorted
			last = at;
			emit((uint32_t)at, e->event.data, e->event.size);
			notes_.track(e->event.data, e->event.size);
			queue.pop();
		}
	}

	template <class Emit>
	void stop(Emit&& emit)
	{
		notes_.releaseAll(0, emit);
	}

private:
	NoteTracker notes_;
};
