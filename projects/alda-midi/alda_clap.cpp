// alda-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

extern "C" {
#include "alda/alda.h"
#include "midi_clock.h"
}

#include <cstdio>
#include <cstdlib>

namespace {

const char* const kDefaultCode =
	"# Ctrl+Return evaluates the paragraph around the caret, Ctrl+Shift+Return\n"
	"# the whole editor, and Ctrl+. stops. With Live off, an evaluation becomes\n"
	"# a clip that loops with the host transport. At (tempo 120), a quarter\n"
	"# note is one host beat.\n"
	"\n"
	"piano:\n"
	"  o4 c8 e g > c < a c e a\n"
	"\n"
	"bass:\n"
	"  o2 c2 a\n";

class AldaLanguage : public Language {
public:
	AldaLanguage()
	{
		alda_context_init(&ctx_);
		alda_midi_init_observer(&ctx_);
		ok_ = alda_midi_open_virtual(&ctx_, "AldaMIDI") == 0;
		if (!ok_) fprintf(stderr, "plugin setup failed: no MIDI output\n");
	}

	~AldaLanguage() override
	{
		alda_midi_cleanup(&ctx_);
		alda_context_cleanup(&ctx_);
	}

	bool ok() const { return ok_; }

	// Parts and attributes carry over between evaluations, as in the REPL; each evaluation
	// plays only its own notes. The interpreter writes its errors to stderr, which a plugin
	// cannot capture, so only parse errors are shown in full.
	void eval(const std::string& code, std::string& out) override
	{
		alda_events_clear(&ctx_);
		char* error = nullptr;
		AldaNode* ast = alda_parse(code.c_str(), "<eval>", &error);
		if (error || !ast) {
			out += std::string("error: ") + (error ? error : "cannot parse") + "\n";
			free(error);
			if (ast) alda_ast_free(ast);
			return;
		}
		const int result = alda_interpret_ast(&ctx_, ast);
		alda_ast_free(ast);
		if (result != 0) {
			out += "error: the score has errors; details are on the host's standard error\n";
			return;
		}
		alda_events_play(&ctx_);
		if (midi_clock_cancelled()) out += "error: stopped\n";
	}

	// Alda plays its parts concurrently inside one evaluation.
	bool nextWake(double*) override { return false; }
	void resume(double, std::string&) override {}
	void stopVoices() override {}

private:
	AldaContext ctx_;
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.alda-midi",
	"alda-midi",
	"Alda MIDI live coding",
	"MidiLangsAldaMidiEditor",
	"ALDA_MIDI_CLAP_UDP_PORT",
	57168,
	{EvalSyntax::kParagraphs, "#", "\""},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<AldaLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
