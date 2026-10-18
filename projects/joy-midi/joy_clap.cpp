// joy-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

extern "C" {
#include "joy_midi.h"
#include "joy_parser.h"
#include "joy_runtime.h"
#include "midi_clock.h"
#include "midi_primitives.h"
#include "music_notation.h"
}

#include <csetjmp>
#include <cstdio>

extern const char kJoyPrelude[]; // prelude.joy, generated at configure time

namespace {

const char* const kDefaultCode =
	"\\ Ctrl+Return evaluates the paragraph around the caret, Ctrl+Shift+Return\n"
	"\\ the whole editor, and Ctrl+. stops. With Live off, an evaluation becomes\n"
	"\\ a clip that loops with the host transport; 500 ms is one beat.\n"
	"\n"
	"[c e g c5] play\n"
	"[a3 c e a] play\n"
	"[c e g] chord\n";

// Joy's output hook and error recovery are process-wide; there is one instance per process.
std::string* gOut = nullptr;

// Runs code with errors caught. Only trivial locals: joy_error longjmps back here.
bool RunProtected(JoyContext* ctx, const char* code)
{
	jmp_buf recovery;
	volatile bool ok = true;
	ctx->error_jmp = &recovery;
	joy_set_current_context(ctx);
	if (setjmp(recovery) == 0) {
		joy_eval_line(ctx, code);
		if (ctx->post_eval_hook) ctx->post_eval_hook(); // plays SEQ definitions
	} else {
		ok = false;
	}
	ctx->error_jmp = nullptr;
	joy_set_current_context(nullptr);
	return ok;
}

class JoyLanguage : public Language {
public:
	JoyLanguage()
	{
		joy_set_output([](const char* text, size_t len, void*) {
			if (gOut) gOut->append(text, len);
		}, nullptr);
		joy_interrupt_hook = [] { return midi_clock_cancelled(); };
		joy_exit_handler = [](int) { joy_error("quit and abort are disabled in the plugin"); };
		ctx_ = joy_context_new();
		joy_runtime_init(ctx_);
		joy_set_parser_dict(ctx_->dictionary);
		music_notation_init(ctx_);
		joy_midi_register_primitives(ctx_);
		std::string out;
		gOut = &out;
		ok_ = RunProtected(ctx_, kJoyPrelude);
		gOut = nullptr;
		midi_virtual_(ctx_); // prints a confirmation, discarded
		ok_ = ok_ && out.empty();
		if (!ok_) fprintf(stderr, "plugin setup failed: %s", out.c_str());
	}

	~JoyLanguage() override
	{
		music_notation_cleanup(ctx_);
		joy_midi_cleanup();
		joy_context_free(ctx_);
		joy_set_output(nullptr, nullptr);
		joy_interrupt_hook = nullptr;
		joy_exit_handler = nullptr;
	}

	bool ok() const { return ok_; }

	// The stack is printed after each evaluation, as piped input prints it, then cleared:
	// values left over would otherwise show in every later reply.
	void eval(const std::string& code, std::string& out) override
	{
		gOut = &out;
		RunProtected(ctx_, code.c_str());
		if (ctx_->stack->depth > 0) {
			for (size_t i = 0; i < ctx_->stack->depth; i++) {
				if (i > 0) joy_printf(" ");
				joy_value_print(ctx_->stack->items[i]);
			}
			joy_printf("\n");
			joy_stack_clear(ctx_->stack);
		}
		gOut = nullptr;
	}

	// Joy has no voices: SEQ parts play inside the evaluation.
	bool nextWake(double*) override { return false; }
	void resume(double, std::string&) override {}
	void stopVoices() override {}

private:
	JoyContext* ctx_;
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.joy-midi",
	"joy-midi",
	"Joy MIDI live coding",
	"MidiLangsJoyMidiEditor",
	"JOY_MIDI_CLAP_UDP_PORT",
	57165,
	{EvalSyntax::kParagraphs, "\\", "\""},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<JoyLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
