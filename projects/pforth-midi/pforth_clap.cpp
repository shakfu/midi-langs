// pforth-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

#include "pf_io_midi.h"
#include "pforth_glue.h"

extern "C" {
#include "midi_clock.h"
}

#include <cstdio>

namespace {

const char* const kDefaultCode =
	"\\ Ctrl+Return evaluates the paragraph around the caret, Ctrl+Shift+Return\n"
	"\\ the whole editor, and Ctrl+. stops. With Live off, an evaluation becomes\n"
	"\\ a clip that loops with the host transport; 500 ms is one beat.\n"
	"\n"
	"250 to dur\n"
	"c4 note e4 note g4 note c5 note\n"
	"a3 note c4 note e4 note a4 note\n"
	"500 to dur\n";

// pForth's terminal output is process-wide; there is one instance per process.
std::string* gOut = nullptr;

class PforthLanguage : public Language {
public:
	PforthLanguage()
	{
		pfm_set_output([](char c, void*) {
			if (gOut) *gOut += c;
		}, nullptr);
		std::string out;
		gOut = &out;
		ok_ = pfm_glue_init() == 0 && pfm_glue_eval_line("midi-open") == 0;
		gOut = nullptr;
		if (!ok_) fprintf(stderr, "plugin setup failed: %s\n", out.c_str());
	}

	~PforthLanguage() override
	{
		pfm_glue_term();
		pfm_set_output(nullptr, nullptr);
	}

	bool ok() const { return ok_; }

	// Line by line, as the terminal feeds pForth: a definition may span lines.
	// The first error ends the evaluation.
	void eval(const std::string& code, std::string& out) override
	{
		std::string text;
		gOut = &text;
		bool failed = false;
		for (size_t start = 0; start < code.size() && !failed;) {
			size_t end = code.find('\n', start);
			if (end == std::string::npos) end = code.size();
			if (midi_clock_cancelled()) {
				text += "stopped\n";
				failed = true;
				break;
			}
			failed = pfm_glue_eval_line(code.substr(start, end - start).c_str()) != 0;
			start = end + 1;
		}
		gOut = nullptr;
		if (failed) {
			out += "error: ";
			text.erase(0, text.find_first_not_of('\n')); // pForth starts messages on a new line
		}
		out += text;
		if (!out.empty() && out.back() != '\n') out += '\n';
	}

	// pForth has no voices.
	bool nextWake(double*) override { return false; }
	void resume(double, std::string&) override {}
	void stopVoices() override {}

private:
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.pforth-midi",
	"pforth-midi",
	"pForth MIDI live coding",
	"MidiLangsPforthMidiEditor",
	"PFORTH_MIDI_CLAP_UDP_PORT",
	57167,
	{EvalSyntax::kParagraphs, "\\", "\""},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<PforthLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
