// s7-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

#include <cstdio>

#include "s7.h"

extern "C" {
#include "midi_clock.h"
#include "scheduler.h"

void s7_midi_init(s7_scheme* sc);
void s7_midi_cleanup(void);
}

#include <cstdlib>

namespace {

const char* const kDefaultCode =
	"; Ctrl+Return evaluates the bracket group around the caret, Ctrl+Shift+Return\n"
	"; the whole editor, and Ctrl+. stops. With Live off, an evaluation becomes\n"
	"; a clip that loops with the host transport; 500 ms is one beat.\n"
	"\n"
	"(begin\n"
	"  (arp (list c4 e4 g4 c5) mf eighth)\n"
	"  (arp (list a3 c4 e4 a4) mf eighth))\n"
	"\n"
	"(spawn (make-melody-voice (list c3 c3 c3 c3 c3 c3 c3 c3) f eighth))\n";

// Plugin setup on top of the prelude: a port for n, ch, arp and the voices, the C tempo
// back at the prelude's 120 if an earlier instance in this process changed it, and no
// way to exit the host's process.
const char* const kSetup =
	"(set-tempo! 120)\n"
	"(open)\n"
	"(set! exit (lambda args (error 'exit \"exit is disabled in the plugin\")))\n"
	"(values)\n";

// s7's output functions take no user data; there is one instance per process.
std::string* gOut = nullptr;
std::string gErr;

class S7Language : public Language {
public:
	S7Language()
	{
		sc_ = s7_init();
		s7_set_current_output_port(sc_, Protect(s7_open_output_function(sc_, [](s7_scheme*, uint8_t c, s7_pointer) {
			if (gOut) *gOut += (char)c;
		})));
		s7_set_current_error_port(sc_, Protect(s7_open_output_function(sc_, [](s7_scheme*, uint8_t c, s7_pointer) {
			gErr += (char)c;
		})));
		s7_set_begin_hook(sc_, [](s7_scheme*, bool* stop) {
			if (midi_clock_cancelled()) *stop = true;
		});
		s7_midi_init(sc_);
		std::string out;
		eval(kSetup, out);
		ok_ = out.empty();
		if (!ok_) fprintf(stderr, "plugin setup failed: %s", out.c_str());
	}

	~S7Language() override
	{
		s7_midi_cleanup();
		s7_free(sc_);
	}

	bool ok() const { return ok_; }

	// Every form runs; the last one's value is printed unless it is unspecified, as in the REPL.
	// The setup's last form is a set!, whose value would print, so it ends with (values).
	void eval(const std::string& code, std::string& out) override
	{
		gOut = &out;
		gErr.clear();
		// s7_eval_c_string reads one form; the newline ends a trailing comment.
		const std::string forms = "(begin\n" + code + "\n)";
		s7_pointer result = s7_eval_c_string(sc_, forms.c_str());
		if (midi_clock_cancelled()) {
			out += "error: stopped\n";
		} else if (gErr.empty() && !s7_is_unspecified(sc_, result)) {
			char* text = s7_object_to_c_string(sc_, result);
			out += std::string(text) + "\n";
			free(text);
		}
		flushErrors(out);
		gOut = nullptr;
	}

	bool nextWake(double* ms) override { return s7_scheduler_next_wake(ms) != 0; }

	void resume(double ms, std::string& out) override
	{
		gOut = &out;
		gErr.clear();
		s7_scheduler_resume_at(ms);
		flushErrors(out);
		gOut = nullptr;
	}

	void stopVoices() override { s7_scheduler_stop_all(); }

private:
	s7_pointer Protect(s7_pointer p)
	{
		s7_gc_protect(sc_, p);
		return p;
	}

	// s7 writes errors as ";message" lines.
	static void flushErrors(std::string& out)
	{
		if (gErr.empty()) return;
		size_t start = gErr.find_first_not_of("; \n");
		out += "error: " + (start == std::string::npos ? gErr : gErr.substr(start));
		if (out.back() != '\n') out += '\n';
		gErr.clear();
	}

	s7_scheme* sc_;
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.s7-midi",
	"s7-midi",
	"s7 Scheme MIDI live coding",
	"MidiLangsS7MidiEditor",
	"S7_MIDI_CLAP_UDP_PORT",
	57163,
	{EvalSyntax::kBrackets, ";", "\""},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<S7Language>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
