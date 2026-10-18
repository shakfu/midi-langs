#pragma once

// One interpreter behind a CLAP plugin. Each language links plugin.cpp with an adapter that
// defines kLanguage. All Language calls happen on the plugin's evaluator thread, after a
// midi_sink (midi_clock.h) is installed, so the interpreter's sends and sleeps use script time.

#include "editor_logic.hpp"

#include <cstdint>
#include <memory>
#include <string>

class Language {
public:
	virtual ~Language() = default;
	// Runs code, appending to out what it prints, its result, and any error.
	virtual void eval(const std::string& code, std::string& out) = 0;
	// Earliest wake time, in script ms, of a voice waiting in script time; false if none waits.
	virtual bool nextWake(double* ms) = 0;
	// Resumes the voices due at or before ms, appending their output to out.
	virtual void resume(double ms, std::string& out) = 0;
	virtual void stopVoices() = 0;
};

struct LanguageInfo {
	const char* id;          // CLAP plugin id
	const char* name;        // e.g. "lua-midi"
	const char* description;
	const char* windowClass; // pugl window class, unique per plugin binary
	const char* udpPortEnv;  // turns UDP on at load and sets the port
	uint16_t udpPort;
	EvalSyntax syntax;
	const char* defaultCode;
	std::unique_ptr<Language> (*create)(); // returns nullptr if the interpreter cannot start
};

extern const LanguageInfo kLanguage;
