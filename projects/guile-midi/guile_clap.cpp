// guile-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

#include <libguile.h>

extern "C" {
#include "midi_clock.h"
#include "scheduler.h"

void guile_midi_init(void);
void guile_midi_cleanup(void);
}

#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <thread>

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
	"(set! exit (lambda args (error \"exit is disabled in the plugin\")))\n"
	"(set! primitive-exit exit)\n"
	"*unspecified*\n";

const auto kWatchPeriod = std::chrono::milliseconds(5);

struct Call {
	std::string code;
	double ms;
	SCM value = SCM_UNSPECIFIED;
};

// Throws 'stopped while Stop is pending. Run as an async, so it may arrive after the
// evaluation it was meant for; it does nothing then.
SCM ThrowIfStopped()
{
	if (midi_clock_cancelled()) scm_throw(scm_from_utf8_symbol("stopped"), SCM_EOL);
	return SCM_UNSPECIFIED;
}

class GuileLanguage : public Language {
public:
	GuileLanguage()
	{
		scm_init_guile();
		evalThread_ = scm_gc_protect_object(scm_current_thread());
		stopper_ = scm_gc_protect_object(scm_c_make_gsubr("%clap-stop", 0, 0, 0, (scm_t_subr)ThrowIfStopped));
		guile_midi_init();
		watcher_ = std::thread([this] { scm_with_guile(&GuileLanguage::Watch, this); });
		std::string out;
		eval(kSetup, out);
		ok_ = out.empty();
		if (!ok_) fprintf(stderr, "plugin setup failed: %s", out.c_str());
	}

	~GuileLanguage() override
	{
		quit_ = true;
		watcher_.join();
		std::string discarded;
		eval("(close)", discarded); // frees the port now; its smob waits for a GC
		guile_midi_cleanup();
		scm_gc_unprotect_object(stopper_);
		scm_gc_unprotect_object(evalThread_);
	}

	bool ok() const { return ok_; }

	// Every form runs; the last one's value is printed unless it is unspecified, as in the REPL.
	void eval(const std::string& code, std::string& out) override
	{
		Call call{code, 0.};
		run(&GuileLanguage::EvalBody, &call, out);
		if (!scm_is_eq(call.value, SCM_UNSPECIFIED)) {
			char* text = scm_to_utf8_string(scm_object_to_string(call.value, SCM_UNDEFINED));
			out += std::string(text) + "\n";
			free(text);
		}
	}

	bool nextWake(double* ms) override { return guile_scheduler_next_wake(ms) != 0; }

	void resume(double ms, std::string& out) override
	{
		Call call{std::string(), ms};
		run(&GuileLanguage::ResumeBody, &call, out);
	}

	void stopVoices() override { guile_scheduler_stop_all(); }

private:
	static SCM EvalBody(void* data)
	{
		auto* call = static_cast<Call*>(data);
		call->value = scm_c_eval_string(call->code.c_str());
		return SCM_UNSPECIFIED;
	}

	static SCM ResumeBody(void* data)
	{
		guile_scheduler_resume_at(static_cast<Call*>(data)->ms);
		return SCM_UNSPECIFIED;
	}

	static SCM OnError(void* data, SCM key, SCM args)
	{
		auto* port = static_cast<SCM*>(data);
		scm_puts("error: ", *port);
		if (scm_is_eq(key, scm_from_utf8_symbol("stopped"))) scm_puts("stopped\n", *port);
		else scm_print_exception(*port, SCM_BOOL_F, key, args);
		return SCM_UNSPECIFIED;
	}

	// Runs body with output and errors captured into out.
	void run(scm_t_catch_body body, Call* call, std::string& out)
	{
		SCM port = scm_open_output_string();
		SCM oldOut = scm_current_output_port(), oldErr = scm_current_error_port();
		scm_set_current_output_port(port);
		scm_set_current_error_port(port);
		running_ = true;
		scm_c_catch(SCM_BOOL_T, body, call, &GuileLanguage::OnError, &port, nullptr, nullptr);
		running_ = false;
		scm_set_current_output_port(oldOut);
		scm_set_current_error_port(oldErr);
		char* text = scm_to_utf8_string(scm_get_output_string(port));
		out += text;
		free(text);
	}

	// Guile checks for asyncs in every loop, so one Stop reaches any running code.
	static void* Watch(void* data)
	{
		auto* self = static_cast<GuileLanguage*>(data);
		bool marked = false;
		while (!self->quit_) {
			const bool stop = self->running_ && midi_clock_cancelled();
			if (stop && !marked) scm_system_async_mark_for_thread(self->stopper_, self->evalThread_);
			marked = stop;
			std::this_thread::sleep_for(kWatchPeriod);
		}
		return nullptr;
	}

	SCM evalThread_;
	SCM stopper_;
	std::thread watcher_;
	std::atomic<bool> running_{false};
	std::atomic<bool> quit_{false};
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.guile-midi",
	"guile-midi",
	"GNU Guile MIDI live coding",
	"MidiLangsGuileMidiEditor",
	"GUILE_MIDI_CLAP_UDP_PORT",
	57164,
	{EvalSyntax::kBrackets, ";", "\""},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<GuileLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
