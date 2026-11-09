// mhs-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.
// MicroHs runs PluginRepl.hs on a thread of its own (mhs_plugin_runtime.c); this
// adapter turns Language calls into requests to it.

#include "language.hpp"

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

#include "mhs_plugin_runtime.h"

extern "C" {
#include "midi_clock.h"
}

namespace {

const char* const kDefaultCode =
	"-- Ctrl+Return evaluates the paragraph around the caret: a definition or an\n"
	"-- IO action. Ctrl+Shift+Return evaluates each paragraph in turn, and Ctrl+.\n"
	"-- stops. With Live off, an evaluation becomes a clip that loops with the\n"
	"-- host transport; 500 ms is one beat.\n"
	"\n"
	"arpeggioWith 1 mf eighth [c4, e4, g4, c5]\n"
	"\n"
	"spawn \"bass\" (times 8 (noteWith 1 ff eighth c3)) >> return ()\n";

// The REPL outlives plugin instances, so each instance starts from a clean one:
// no voices, no definitions, then the modules and a port for note and friends.
const char* const kSetup[] = {"k", "c", "eimport MidiPerform", "eimport Async", "eopen"};

std::string Request(const std::string& req)
{
	char* text = mhs_rt_request(req.c_str());
	std::string out = text ? text : "";
	free(text);
	return out;
}

// Paragraphs, the REPL's unit: an entry is one definition group or one expression.
std::vector<std::string> Paragraphs(const std::string& code)
{
	std::vector<std::string> out;
	std::string current;
	size_t start = 0;
	while (start <= code.size()) {
		size_t end = code.find('\n', start);
		if (end == std::string::npos) end = code.size();
		std::string line = code.substr(start, end - start);
		if (line.find_first_not_of(" \t\r") == std::string::npos) {
			if (!current.empty()) out.push_back(current);
			current.clear();
		} else {
			current += line + "\n";
		}
		start = end + 1;
	}
	if (!current.empty()) out.push_back(current);
	return out;
}

class MhsLanguage : public Language {
public:
	MhsLanguage()
	{
		char* startup = nullptr;
		ok_ = mhs_rt_start(&startup) == 0;
		if (!ok_) fprintf(stderr, "mhs-midi: the interpreter failed to start: %s", startup ? startup : "");
		free(startup);
		if (!ok_) return;
		std::string out;
		for (const char* req : kSetup) out += Request(req);
		if (out.find("rror") != std::string::npos || out.find("xception") != std::string::npos) {
			fprintf(stderr, "mhs-midi: plugin setup failed: %s", out.c_str());
			ok_ = false;
		}
	}

	~MhsLanguage() override
	{
		if (ok_) Request("k");
	}

	bool ok() const { return ok_; }

	void eval(const std::string& code, std::string& out) override
	{
		for (const std::string& paragraph : Paragraphs(code)) {
			std::string text = Request("e" + paragraph);
			if (midi_clock_cancelled()) {
				// Stop interrupts with UserInterrupt; report it as the other plugins do.
				const std::string interrupt = "*** Exception: user interrupt";
				const size_t at = text.find(interrupt);
				if (at != std::string::npos) text.replace(at, interrupt.size(), "error: stopped");
				out += text;
				return;
			}
			out += text;
		}
	}

	bool nextWake(double* ms) override { return mhs_rt_next_wake(ms) != 0; }

	void resume(double ms, std::string& out) override
	{
		out += Request("r" + std::to_string(std::llround(ms * 1000.0)));
	}

	void stopVoices() override { Request("k"); }

private:
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.mhs-midi",
	"mhs-midi",
	"MicroHs (Haskell) MIDI live coding",
	"MidiLangsMhsMidiEditor",
	"MHS_MIDI_CLAP_UDP_PORT",
	57169,
	{EvalSyntax::kParagraphs, "--", "\""},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<MhsLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
