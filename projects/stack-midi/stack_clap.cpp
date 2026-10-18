// stack-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

#include <cstdio>

// The context's field macros (stack, dictionary, ...) would clash with C++ names.
#define STACK_NO_MACROS
extern "C" {
#include "midi_clock.h"
#include "stack_midi.h"
}

namespace {

const char* const kDefaultCode =
	"\\ Ctrl+Return evaluates the paragraph around the caret, Ctrl+Shift+Return\n"
	"\\ the whole editor, and Ctrl+. stops. With Live off, an evaluation becomes\n"
	"\\ a clip that loops with the host transport; 500 ms is one beat.\n"
	"\n"
	"250 dur!\n"
	"c4, e4, g4, c5, a3, c4, e4, a4,\n"
	"500 dur!\n";

// stack-midi's output hook is process-wide; there is one instance per process.
std::string* gOut = nullptr;

class StackLanguage : public Language {
public:
	StackLanguage()
	{
		stack_set_output([](const char* text, size_t len, void*) {
			if (gOut) gOut->append(text, len);
		}, nullptr);
		stack_interrupt_hook = [] { return midi_clock_cancelled(); };
		stack_context_init(&g_ctx);
		init_dictionary();
		interpret("midi-open"); // prints a confirmation, discarded
		ok_ = g_ctx.midi_out != nullptr;
		if (!ok_) fprintf(stderr, "plugin setup failed: no MIDI output\n");
	}

	~StackLanguage() override
	{
		stack_cleanup();
		stack_set_output(nullptr, nullptr);
		stack_interrupt_hook = nullptr;
	}

	bool ok() const { return ok_; }

	// stack-midi reports errors and goes on, so a counted error marks the output.
	void eval(const std::string& code, std::string& out) override
	{
		std::string text;
		gOut = &text;
		const int errors = g_ctx.error_count;
		interpret(code.c_str());
		if (midi_clock_cancelled()) {
			text += "stopped\n";
			// Interrupted code leaves values and modes behind; the dictionary and port stay.
			stack_context_reset(&g_ctx);
		}
		gOut = nullptr;
		if (g_ctx.error_count > errors || midi_clock_cancelled()) out += "error: ";
		out += text;
	}

	bool nextWake(double* ms) override { return async_player_next_wake(ms) != 0; }

	void resume(double ms, std::string& out) override
	{
		gOut = &out;
		async_player_resume_at(ms);
		gOut = nullptr;
	}

	void stopVoices() override
	{
		std::string discarded; // "Stopped all async playback"
		gOut = &discarded;
		async_player_stop_all();
		gOut = nullptr;
	}

private:
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.stack-midi",
	"stack-midi",
	"Forth-like stack MIDI live coding",
	"MidiLangsStackMidiEditor",
	"STACK_MIDI_CLAP_UDP_PORT",
	57166,
	{EvalSyntax::kParagraphs, "\\", "\""},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<StackLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
