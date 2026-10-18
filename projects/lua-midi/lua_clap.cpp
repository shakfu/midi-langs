// lua-midi as a CLAP plugin: the interpreter behind projects/clap/plugin.cpp.

#include "language.hpp"

#include <cstdio>

extern "C" {
#include "lauxlib.h"
#include "lua.h"
#include "lualib.h"
#include "midi_clock.h"
#include "scheduler.h"

int luaopen_midi(lua_State* L);
void lua_midi_cleanup(void);
}

namespace {

// Interpreter instructions between checks for Stop.
const int kHookCount = 10000;

const char* const kDefaultCode =
	"-- Ctrl+Return evaluates the paragraph around the caret, Ctrl+Shift+Return\n"
	"-- the whole editor, and Ctrl+. stops. With Live off, an evaluation becomes\n"
	"-- a clip that loops with the host transport; 500 ms is one beat.\n"
	"\n"
	"arp({c4, e4, g4, c5}, mf, eighth)\n"
	"arp({a3, c4, e4, a4}, mf, eighth)\n"
	"\n"
	"spawn(function()\n"
	"  for i = 1, 8 do play(c3, f, eighth) end\n"
	"end)\n";

// Plugin setup on top of the prelude: a port for n(), play() and friends, the C tempo
// back at the prelude's 120 if an earlier instance in this process changed it, and no
// way to exit the host's process or block on its stdin.
const char* const kSetup =
	"midi._set_c_tempo(120)\n"
	"open()\n"
	"os.exit = nil\n"
	"io.read = nil\n";

class LuaLanguage : public Language {
public:
	LuaLanguage()
	{
		L_ = luaL_newstate();
		luaL_openlibs(L_);
		luaL_requiref(L_, "scheduler", luaopen_scheduler, 1);
		lua_pop(L_, 1);
		luaL_requiref(L_, "midi", luaopen_midi, 1);
		lua_pop(L_, 1);
		lua_pushlightuserdata(L_, this);
		lua_pushcclosure(L_, &LuaLanguage::Print, 1);
		lua_setglobal(L_, "print");
		lua_sethook(L_, &LuaLanguage::Hook, LUA_MASKCOUNT, kHookCount);
		scheduler_set_error_handler(&LuaLanguage::OnVoiceError, this);
		std::string out;
		eval(kSetup, out);
		ok_ = out.empty();
		if (!ok_) fprintf(stderr, "plugin setup failed: %s", out.c_str());
	}

	~LuaLanguage() override
	{
		scheduler_cleanup();
		lua_midi_cleanup();
		lua_close(L_);
		scheduler_set_error_handler(nullptr, nullptr);
	}

	bool ok() const { return ok_; }

	// As the REPL does: an expression's values are printed; otherwise the code runs as statements.
	void eval(const std::string& code, std::string& out) override
	{
		out_ = &out;
		lua_settop(L_, 0);
		const std::string expr = "return " + code;
		int status = luaL_loadbuffer(L_, expr.data(), expr.size(), "=eval");
		if (status != LUA_OK) {
			lua_pop(L_, 1);
			status = luaL_loadbuffer(L_, code.data(), code.size(), "=eval");
		}
		if (status == LUA_OK) status = lua_pcall(L_, 0, LUA_MULTRET, 0);
		if (status != LUA_OK) {
			const char* msg = lua_tostring(L_, -1);
			out += std::string("error: ") + (msg ? msg : "unknown") + "\n";
		} else if (int n = lua_gettop(L_); n > 0) {
			lua_getglobal(L_, "print");
			lua_insert(L_, 1);
			lua_pcall(L_, n, 0, 0);
		}
		lua_settop(L_, 0);
		out_ = nullptr;
	}

	bool nextWake(double* ms) override { return scheduler_next_wake(ms) != 0; }

	void resume(double ms, std::string& out) override
	{
		out_ = &out;
		scheduler_resume_at(L_, ms);
		out_ = nullptr;
	}

	void stopVoices() override { scheduler_stop_all(); }

private:
	static int Print(lua_State* L)
	{
		auto* self = static_cast<LuaLanguage*>(lua_touserdata(L, lua_upvalueindex(1)));
		const int n = lua_gettop(L);
		std::string line;
		for (int i = 1; i <= n; ++i) {
			size_t len;
			const char* s = luaL_tolstring(L, i, &len);
			if (i > 1) line += '\t';
			line.append(s, len);
			lua_pop(L, 1);
		}
		if (self->out_) *self->out_ += line + "\n";
		return 0;
	}

	static void Hook(lua_State* L, lua_Debug*)
	{
		if (midi_clock_cancelled()) luaL_error(L, "stopped");
	}

	static void OnVoiceError(void* user, const char* msg)
	{
		auto* self = static_cast<LuaLanguage*>(user);
		if (self->out_) *self->out_ += msg;
	}

	lua_State* L_ = nullptr;
	std::string* out_ = nullptr;
	bool ok_ = false;
};

} // namespace

const LanguageInfo kLanguage = {
	"com.github.shakfu.midi-langs.lua-midi",
	"lua-midi",
	"Lua MIDI live coding",
	"MidiLangsLuaMidiEditor",
	"LUA_MIDI_CLAP_UDP_PORT",
	57161,
	{EvalSyntax::kParagraphs, "--", "\"'"},
	kDefaultCode,
	[]() -> std::unique_ptr<Language> {
		auto lang = std::make_unique<LuaLanguage>();
		if (!lang->ok()) return nullptr;
		return lang;
	},
};
