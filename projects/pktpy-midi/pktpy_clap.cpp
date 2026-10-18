// pktpy-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

#include <cstdio>

#include "pocketpy.h"

extern "C" {
#include "midi_clock.h"
#include "scheduler.h"

void pk_midi_module_init(void);
void pk_midi_module_cleanup(void);
}

namespace {

const char* const kDefaultCode =
	"# Ctrl+Return evaluates the paragraph around the caret, Ctrl+Shift+Return\n"
	"# the whole editor, and Ctrl+. stops. With Live off, an evaluation becomes\n"
	"# a clip that loops with the host transport; 500 ms is one beat.\n"
	"\n"
	"m = midi.open()\n"
	"m.arpeggio(midi.major(midi.c4), midi.mf, midi.eighth)\n"
	"m.arpeggio(midi.minor(midi.a3), midi.mf, midi.eighth)\n"
	"\n"
	"def bass():\n"
	"    for i in range(8):\n"
	"        yield from midi.play(m, midi.c3, midi.f, midi.eighth)\n"
	"midi.spawn(bass)\n";

// Plugin setup: midi imported, time.sleep in script time, and no way to exit the host's
// process or block on its stdin.
const char* const kSetup =
	"import midi\n"
	"import time\n"
	"midi._set_c_tempo(120)\n"
	"time.sleep = lambda s: midi._sleep_ms(s * 1000)\n";

// pocketpy's print and the interrupt hook take no user data; there is one instance per process.
std::string* gOut = nullptr;

class PktpyLanguage : public Language {
public:
	PktpyLanguage()
	{
		// py_finalize is irreversible, so later instances reuse VM 0, reset, on their own thread.
		static bool initialized = false;
		if (!initialized) {
			py_initialize();
			initialized = true;
		} else {
			py_switchvm(0);
			py_resetvm();
		}
		py_Callbacks* callbacks = py_callbacks();
		callbacks->print = [](const char* s) {
			if (gOut) *gOut += s;
		};
		callbacks->getchr = []() { return -1; };
		py_interrupt_hook = [] { return midi_clock_cancelled(); };
		pk_midi_module_init();
		py_GlobalRef builtins = py_getmodule("builtins");
		py_bindfunc(builtins, "exit", [](int, py_StackRef) { return py_exception(tp_RuntimeError, "exit is disabled in the plugin"); });
		pk_scheduler_set_error_handler(&PktpyLanguage::OnVoiceError, nullptr);
		std::string out;
		eval(kSetup, out);
		ok_ = out.empty();
		if (!ok_) fprintf(stderr, "plugin setup failed: %s", out.c_str());
	}

	~PktpyLanguage() override
	{
		pk_midi_module_cleanup();
		pk_scheduler_set_error_handler(nullptr, nullptr);
		py_interrupt_hook = nullptr;
	}

	bool ok() const { return ok_; }

	// An expression's value is printed, as in the REPL; anything else runs as statements.
	void eval(const std::string& code, std::string& out) override
	{
		gOut = &out;
		py_StackRef p0 = py_peek(0);
		bool ok;
		if (py_compile(code.c_str(), "<eval>", EVAL_MODE, false)) {
			ok = py_exec(code.c_str(), "<eval>", EVAL_MODE, nullptr);
			if (ok && !py_isnone(py_retval())) {
				py_push(py_retval()); // py_repr writes its result over the register it reads
				ok = py_repr(py_peek(-1));
				py_pop();
				if (ok) out += std::string(py_tostr(py_retval())) + "\n";
			}
		} else {
			py_clearexc(p0);
			ok = py_exec(code.c_str(), "<eval>", EXEC_MODE, nullptr);
		}
		if (!ok) {
			char* exc = py_formatexc();
			out += std::string("error: ") + (exc ? exc : "unknown") + "\n";
			PK_FREE(exc);
			py_clearexc(p0);
		}
		gOut = nullptr;
	}

	bool nextWake(double* ms) override { return pk_scheduler_next_wake(ms) != 0; }

	void resume(double ms, std::string& out) override
	{
		gOut = &out;
		pk_scheduler_resume_at(ms);
		gOut = nullptr;
	}

	void stopVoices() override { pk_scheduler_stop_all(); }

private:
	static void OnVoiceError(void*, const char* msg)
	{
		if (gOut) *gOut += msg;
	}

	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.pktpy-midi",
	"pktpy-midi",
	"PocketPy MIDI live coding",
	"MidiLangsPktpyMidiEditor",
	"PKTPY_MIDI_CLAP_UDP_PORT",
	57162,
	{EvalSyntax::kParagraphs, "#", "\"'"},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<PktpyLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
