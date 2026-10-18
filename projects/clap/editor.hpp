#pragma once

#include <functional>
#include <memory>
#include <string>

struct clap_window;
struct LanguageInfo;

// Plugin-side actions the editor triggers. All are called on the host's main thread.
struct EditorCallbacks {
	std::function<void(const std::string& code)> eval;
	std::function<void()> stop;
	std::function<void(bool on)> setUdp;
	std::function<void(bool on)> setWrap;
	std::function<void(bool on)> setLive;
	std::function<void(const std::string& text)> textChanged;
	std::function<std::string()> drainLog; // output posted since the last call
};

struct EditorState {
	std::string text;
	bool udpOn = false;
	bool wrap = true;
	bool live = false;
	unsigned port = 0;
};

// Sizes are in the host's units: points on macOS, pixels elsewhere.
class Editor {
public:
	virtual ~Editor() = default;
	virtual bool setParent(const clap_window* window) = 0;
	virtual void show() = 0;
	virtual void hide() = 0;
	virtual void setSize(unsigned width, unsigned height) = 0;
	virtual void setUdpState(bool on) = 0;
	virtual void setText(const std::string& text) = 0;
	virtual void setWrap(bool on) = 0;
	virtual void setLive(bool on) = 0;
	// Called from the host's timer. Required on X11, where nothing else dispatches the editor's events.
	virtual void idle() = 0;
};

// CLAP window API this platform's editor embeds into.
const char* EditorWindowApi();

// Returns nullptr if the editor cannot be created (e.g. no X11 display).
std::unique_ptr<Editor> CreateEditor(EditorCallbacks callbacks, const EditorState& state, const LanguageInfo& language);

const unsigned kEditorWidth = 720;
const unsigned kEditorHeight = 480;
