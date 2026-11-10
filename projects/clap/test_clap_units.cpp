// Unit tests for the CLAP framework's timeline, eval-code selection and syntax highlighting.

#include "editor_logic.hpp"
#include "highlight.hpp"
#include "highlight_forth.h"
#include "highlight_haskell.h"
#include "highlight_joy.h"
#include "highlight_lua.h"
#include "highlight_python.h"
#include "highlight_scheme.h"
#include "timeline.hpp"

#include <cstdio>
#include <string>
#include <vector>

namespace {

int gFailures = 0;

void check(bool ok, const std::string& what)
{
	if (!ok) {
		printf("FAIL: %s\n", what.c_str());
		++gFailures;
	}
}

void checkEq(const std::string& got, const std::string& want, const std::string& what)
{
	check(got == want, what + ": got \"" + got + "\", want \"" + want + "\"");
}

// --- SelectEvalCode -----------------------------------------------------

const EvalSyntax kLisp = {EvalSyntax::kBrackets, ";", "\""};
const EvalSyntax kLua = {EvalSyntax::kParagraphs, "--", "\"'"};

std::string At(const std::string& text, int caret, const EvalSyntax& syntax)
{
	return SelectEvalCode(text, caret, caret, caret, syntax);
}

void testSelection()
{
	checkEq(SelectEvalCode("1 2 +\n3 4 *\n", 2, 9, 9, kLisp), "2 +\n3 4", "selection wins");
	checkEq(SelectEvalCode("1 2 +\n3 4 *\n", 9, 2, 2, kLua), "2 +\n3 4", "reversed selection");
	checkEq(SelectEvalCode("1 2 +\n", 5, 6, 6, kLisp), "", "selection of only a newline");
	checkEq(SelectEvalCode("1 2 +", -5, 99, 0, kLisp), "1 2 +", "out-of-range offsets clamp");
}

void testBrackets()
{
	const std::string lines = "1 2 +\n3 4 *\n5 6 -";
	checkEq(At(lines, 0, kLisp), "1 2 +", "caret line");
	checkEq(At(lines, 11, kLisp), "3 4 *", "caret at end of line");
	checkEq(At(lines, 17, kLisp), "5 6 -", "caret at end of text");
	checkEq(At("a\n   \nb", 4, kLisp), "a", "blank line uses the line above");
	checkEq(At("\n\n", 1, kLisp), "", "blank code is empty");

	const std::string block = "(define (f)\n  (note c4))\n";
	for (int caret : {0, 14, (int)block.size()}) {
		checkEq(At(block, caret, kLisp), block.substr(0, block.size() - 1), "multi-line group, caret " + std::to_string(caret));
	}
	checkEq(At("1 (2\n3) +\n4", 11, kLisp), "4", "line after a group");
	checkEq(At("(1 2 +) 3 *\n(4)", 2, kLisp), "(1 2 +) 3 *", "one-line group is the line");
	checkEq(At("1 ; (\n2 \")\" 3\n4", 7, kLisp), "2 \")\" 3", "brackets in comments and strings ignored");
	checkEq(At("(\n1 2 +", 3, kLisp), "1 2 +", "unbalanced group falls back to the line");
}

void testParagraphs()
{
	const std::string text = "a\nb\n\nc\nd\n";
	checkEq(At(text, 2, kLua), "a\nb", "paragraph around the caret");
	checkEq(At(text, 6, kLua), "c\nd", "second paragraph");
	checkEq(At(text, 4, kLua), "a\nb", "blank line uses the paragraph above");
	checkEq(At(text, (int)text.size(), kLua), "c\nd", "caret after the final newline");
	const std::string voice = "spawn(function()\n  play(c4)\nend)\n\nn(e4)";
	checkEq(At(voice, 20, kLua), "spawn(function()\n  play(c4)\nend)", "lua function block");
	checkEq(At(voice, (int)voice.size(), kLua), "n(e4)", "last paragraph without a newline");
}

// --- timeline -----------------------------------------------------------

struct Out {
	long long frame; // absolute
	uint8_t status, key;
};

const double k120At48k = 120. / 60. / 48000.; // beats per frame
const uint32_t kBlock = 512;

MidiEvent Ev(double beat, uint8_t status, uint8_t key, uint8_t vel)
{
	return {beat, {status, key, vel}, 3};
}

// Renders blocks from beat `start`, collecting events at absolute frames from frame `base`.
void RenderClip(ClipPlayer& player, const Clip* clip, bool playing, double start, int blocks, long long base, std::vector<Out>& out)
{
	for (int b = 0; b < blocks; ++b) {
		BlockTime t{start + b * kBlock * k120At48k, k120At48k, kBlock};
		player.render(clip, playing, t, [&](uint32_t offset, const uint8_t* data, uint8_t) {
			out.push_back({base + b * (long long)kBlock + offset, data[0], data[1]});
		});
	}
}

void testMakeClip()
{
	Clip c = MakeClip({Ev(1., 0x80, 60, 0), Ev(0., 0x90, 60, 100), Ev(1., 0x90, 62, 100)}, 4.);
	check(c.length == 4., "clip length rounds up to a bar");
	check(c.events[0].beat == 0. && c.events[1].data[0] == 0x80 && c.events[2].data[1] == 62,
	      "events sorted by beat, send order kept at equal beats");
	check(MakeClip({Ev(4., 0x80, 60, 0)}, 4.).length == 8., "an event on a bar line starts another bar");
	check(MakeClip({Ev(2.9, 0x80, 60, 0)}, 3.).length == 3., "bar length follows the time signature");
}

void testClipRoundTrip()
{
	Clip c = MakeClip({Ev(0., 0x90, 60, 100), Ev(1.5, 0x80, 60, 0), {2., {0xC0, 5, 0}, 2}}, 4.);
	std::string data = "xx";
	WriteClip(data, c);
	size_t pos = 2;
	Clip back;
	check(ReadClip(data, pos, back) && pos == data.size(), "clip reads back");
	check(back.length == c.length && back.events.size() == 3 && back.events[1].beat == 1.5
	          && back.events[2].size == 2 && back.events[2].data[1] == 5,
	      "clip round trip keeps events");
	pos = 2;
	check(!ReadClip(data.substr(0, data.size() - 1), pos, back), "truncated clip is rejected");
	std::string empty;
	WriteClip(empty, Clip());
	pos = 0;
	check(ReadClip(empty, pos, back) && back.events.empty(), "empty clip round trip");
}

void testClipPlayback()
{
	const Clip c = MakeClip({Ev(0., 0x90, 60, 100), Ev(1., 0x80, 60, 0)}, 4.);
	ClipPlayer player;
	std::vector<Out> out;
	// 2 loops of 4 beats: 192000 frames = 375 blocks.
	RenderClip(player, &c, true, 0., 375, 0, out);
	check(out.size() == 4, "two loops give two note pairs, got " + std::to_string(out.size()));
	if (out.size() == 4) {
		check(out[0].frame == 0 && out[0].status == 0x90, "note on at frame 0");
		check(out[1].frame == 24000 && out[1].status == 0x80, "note off one beat later, on a block boundary");
		check(out[2].frame == 96000 && out[3].frame == 120000, "the clip loops after its length");
	}

	// Transport stops with the note held: released at once.
	ClipPlayer stopped;
	out.clear();
	RenderClip(stopped, &c, true, 0., 10, 0, out);
	RenderClip(stopped, &c, false, 10 * kBlock * k120At48k, 1, 10 * kBlock, out);
	check(out.size() == 2 && out[1].status == 0x80 && out[1].frame == 10 * kBlock, "stop releases the held note");

	// The host jumps into the middle of the held note: release, and no note-on until the next loop.
	ClipPlayer jumped;
	out.clear();
	RenderClip(jumped, &c, true, 0., 10, 0, out);
	RenderClip(jumped, &c, true, 0.5, 1, 100000, out);
	check(out.size() == 2 && out[1].status == 0x80 && out[1].frame == 100000, "a jump releases the held note");

	// A new clip releases the old clip's notes.
	const Clip other = MakeClip({Ev(2., 0x90, 64, 100), Ev(3., 0x80, 64, 0)}, 4.);
	ClipPlayer swapped;
	out.clear();
	RenderClip(swapped, &c, true, 0., 10, 0, out);
	RenderClip(swapped, &other, true, 10 * kBlock * k120At48k, 1, 10 * kBlock, out);
	check(out.size() == 2 && out[1].status == 0x80 && out[1].key == 60, "a new clip releases held notes");

	// Pre-roll: negative positions play the end of the clip's previous pass.
	const Clip tail = MakeClip({Ev(3.5, 0x90, 67, 100)}, 4.);
	ClipPlayer preroll;
	out.clear();
	RenderClip(preroll, &tail, true, -1., 47, 0, out);
	check(out.size() == 1 && out[0].frame == 12000, "pre-roll plays the clip's previous pass");
}

void testLivePlayback()
{
	SpscRing<LiveEvent, 16> queue;
	LivePlayer player;
	queue.push({Ev(0.25, 0x90, 60, 100), 1});
	queue.push({Ev(0.0, 0x90, 61, 100), 0}); // from before a stop
	queue.push({Ev(0.0, 0x90, 62, 100), 1}); // late and out of order
	queue.push({Ev(1.0, 0x80, 60, 0), 1});
	std::vector<Out> out;
	auto emit = [&](uint32_t offset, const uint8_t* data, uint8_t) { out.push_back({offset, data[0], data[1]}); };
	// The block at beat 0.24 covers beats [0.24, 0.2613).
	player.render(queue, 1, {0.24, k120At48k, kBlock}, emit);
	check(out.size() == 2, "live block plays due events, got " + std::to_string(out.size()));
	if (out.size() == 2) {
		check(out[0].key == 60 && out[0].frame == 240, "live event at its frame");
		check(out[1].key == 62 && out[1].frame == 240, "late event keeps the output sorted");
	}
	check(queue.front() && queue.front()->event.beat == 1.0, "future events stay queued");
	out.clear();
	player.stop(emit);
	check(out.size() == 2 && out[0].status == 0x80, "stop releases live notes");
}

// --- syntax highlighting -------------------------------------------------

// The highlight of the byte where `word` starts in text.
Highlight KindOf(const std::vector<HighlightRun>& runs, const std::string& text, const std::string& word)
{
	const size_t at = text.find(word);
	for (const HighlightRun& r : runs) {
		if (at >= r.start && at < r.end) return r.kind;
	}
	return Highlight::kNone;
}

void testHighlightNames()
{
	check(HighlightForCapture("keyword.return") == Highlight::kKeyword, "dotted capture names map by their head");
	check(HighlightForCapture("constant.numeric") == Highlight::kNumber, "constant.numeric is a number");
	check(HighlightForCapture("constant") == Highlight::kConstant, "plain constant");
	check(HighlightForCapture("_op") == Highlight::kNone, "helper captures have no colour");
	check(HighlightForCapture("spell") == Highlight::kNone, "unknown captures have no colour");
}

void testHighlightQueries()
{
	// Each query must compile, predicates included, against its own grammar.
	const struct {
		const char* name;
		const TSLanguage* language;
		const char* query;
	} grammars[] = {
		{"lua", tree_sitter_lua(), kHighlightQuery_lua},
		{"python", tree_sitter_python(), kHighlightQuery_python},
		{"scheme", tree_sitter_scheme(), kHighlightQuery_scheme},
		{"haskell", tree_sitter_haskell(), kHighlightQuery_haskell},
		{"forth", tree_sitter_forth(), kHighlightQuery_forth},
		{"joy", tree_sitter_joy(), kHighlightQuery_joy},
	};
	for (const auto& g : grammars) {
		Highlighter h(g.language, g.query);
		check(h.ok(), std::string(g.name) + " query compiles: " + h.error());
	}
	Highlighter none(nullptr, nullptr);
	check(!none.ok() && none.highlight("x = 1").empty(), "no grammar, no highlights");
}

void testHighlightLua()
{
	Highlighter h(tree_sitter_lua(), kHighlightQuery_lua);
	const std::string text = "-- melody\nlocal pitch = \"c4\"\nprint(42)\n";
	const auto runs = h.highlight(text);
	check(KindOf(runs, text, "-- melody") == Highlight::kComment, "lua comment");
	check(KindOf(runs, text, "local") == Highlight::kKeyword, "lua keyword");
	check(KindOf(runs, text, "\"c4\"") == Highlight::kString, "lua string");
	check(KindOf(runs, text, "42") == Highlight::kNumber, "lua number");
	check(KindOf(runs, text, "print") == Highlight::kFunction, "lua function call");
	bool sorted = true;
	for (size_t i = 1; i < runs.size(); ++i) sorted &= runs[i - 1].end <= runs[i].start;
	check(sorted, "runs are sorted and do not overlap");
}

void testHighlightJoy()
{
	// joy-midi's dialect: \ line comments, which the vendored grammar adds to Joy's #.
	Highlighter h(tree_sitter_joy(), kHighlightQuery_joy);
	const std::string text = "\\ melody\n[c e g] play\n60 100 500 midi-note\n# classic Joy comment\n2 dup *\n";
	const auto runs = h.highlight(text);
	check(KindOf(runs, text, "\\ melody") == Highlight::kComment, "joy-midi \\ comment");
	check(KindOf(runs, text, "# classic") == Highlight::kComment, "joy # comment");
	check(KindOf(runs, text, "60") == Highlight::kNumber, "joy number");
	check(KindOf(runs, text, "dup") == Highlight::kFunction, "joy builtin through #any-of?");
	check(KindOf(runs, text, "play") == Highlight::kVariable, "joy word");
}

void testHighlightPredicates()
{
	// python's query colours every identifier a variable, then upper-case ones constants
	// through #match?: the predicate must hold, and the later pattern must win.
	Highlighter h(tree_sitter_python(), kHighlightQuery_python);
	const std::string text = "TEMPO = 120\ntempo = 90\n";
	const auto runs = h.highlight(text);
	check(KindOf(runs, text, "TEMPO") == Highlight::kConstant, "#match? makes TEMPO a constant");
	check(KindOf(runs, text, "tempo") == Highlight::kVariable, "tempo stays a variable");
}

} // namespace

int main()
{
	testSelection();
	testBrackets();
	testParagraphs();
	testMakeClip();
	testClipRoundTrip();
	testClipPlayback();
	testLivePlayback();
	testHighlightNames();
	testHighlightQueries();
	testHighlightLua();
	testHighlightJoy();
	testHighlightPredicates();
	printf("%d failure(s)\n", gFailures);
	return gFailures ? 1 : 0;
}
