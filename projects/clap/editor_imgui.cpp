// Dear ImGui editor in a pugl OpenGL view embedded in the host's window (Cocoa, Win32, X11).
// Adapted from sapf's examples/clap. Ctrl/Cmd+Return evaluates the selection or current block;
// Ctrl/Cmd+Shift+Return evaluates the whole editor. Ctrl+. (or Cmd+. where the host delivers it;
// AppKit usually consumes Cmd+.) stops all notes.

#include "editor.hpp"
#include "editor_logic.hpp"
#include "language.hpp"

#include <clap/ext/gui.h>

#include <pugl/gl.h>
#include <pugl/pugl.h>

#include <imgui.h>
#include <imgui_impl_opengl3.h>
#include <imgui_stdlib.h>

#include <algorithm>
#include <cstring>
#include <string>

namespace {

const double kFramePeriod = 1.0 / 30.0;
const uintptr_t kFrameTimer = 1;
const size_t kMaxLog = 200000;

#ifdef __APPLE__
const PuglMods kPrimaryMod = PUGL_MOD_SUPER;
#else
const PuglMods kPrimaryMod = PUGL_MOD_CTRL;
#endif

ImGuiKey ToImGuiKey(uint32_t key)
{
	if (key >= 'a' && key <= 'z') return (ImGuiKey)(ImGuiKey_A + (key - 'a'));
	if (key >= 'A' && key <= 'Z') return (ImGuiKey)(ImGuiKey_A + (key - 'A'));
	if (key >= '0' && key <= '9') return (ImGuiKey)(ImGuiKey_0 + (key - '0'));
	if (key >= PUGL_KEY_F1 && key <= PUGL_KEY_F12) return (ImGuiKey)(ImGuiKey_F1 + (key - PUGL_KEY_F1));
	switch (key) {
	case PUGL_KEY_BACKSPACE: return ImGuiKey_Backspace;
	case PUGL_KEY_TAB: return ImGuiKey_Tab;
	case PUGL_KEY_ENTER: return ImGuiKey_Enter;
	case PUGL_KEY_PAD_ENTER: return ImGuiKey_KeypadEnter;
	case PUGL_KEY_ESCAPE: return ImGuiKey_Escape;
	case PUGL_KEY_DELETE: return ImGuiKey_Delete;
	case PUGL_KEY_SPACE: return ImGuiKey_Space;
	case PUGL_KEY_LEFT: return ImGuiKey_LeftArrow;
	case PUGL_KEY_RIGHT: return ImGuiKey_RightArrow;
	case PUGL_KEY_UP: return ImGuiKey_UpArrow;
	case PUGL_KEY_DOWN: return ImGuiKey_DownArrow;
	case PUGL_KEY_PAGE_UP: return ImGuiKey_PageUp;
	case PUGL_KEY_PAGE_DOWN: return ImGuiKey_PageDown;
	case PUGL_KEY_HOME: return ImGuiKey_Home;
	case PUGL_KEY_END: return ImGuiKey_End;
	case PUGL_KEY_INSERT: return ImGuiKey_Insert;
	case PUGL_KEY_SHIFT_L: return ImGuiKey_LeftShift;
	case PUGL_KEY_SHIFT_R: return ImGuiKey_RightShift;
	case PUGL_KEY_CTRL_L: return ImGuiKey_LeftCtrl;
	case PUGL_KEY_CTRL_R: return ImGuiKey_RightCtrl;
	case PUGL_KEY_ALT_L: return ImGuiKey_LeftAlt;
	case PUGL_KEY_ALT_R: return ImGuiKey_RightAlt;
	case PUGL_KEY_SUPER_L: return ImGuiKey_LeftSuper;
	case PUGL_KEY_SUPER_R: return ImGuiKey_RightSuper;
	case '\'': return ImGuiKey_Apostrophe;
	case ',': return ImGuiKey_Comma;
	case '-': return ImGuiKey_Minus;
	case '.': return ImGuiKey_Period;
	case '/': return ImGuiKey_Slash;
	case ';': return ImGuiKey_Semicolon;
	case '=': return ImGuiKey_Equal;
	case '[': return ImGuiKey_LeftBracket;
	case '\\': return ImGuiKey_Backslash;
	case ']': return ImGuiKey_RightBracket;
	case '`': return ImGuiKey_GraveAccent;
	default: return ImGuiKey_None;
	}
}

bool IsTextType(const char* type)
{
	return type && (!strcmp(type, "text/plain") || !strcmp(type, "text/plain;charset=utf-8")
	                || !strcmp(type, "UTF8_STRING") || !strcmp(type, "public.utf8-plain-text"));
}

class ImGuiEditor : public Editor {
public:
	ImGuiEditor(EditorCallbacks callbacks, const EditorState& state, const LanguageInfo& language, PuglWorld* world)
		: callbacks_(std::move(callbacks)), language_(language), world_(world), text_(state.text), udpOn_(state.udpOn), wrap_(state.wrap), live_(state.live), port_(state.port)
	{
		context_ = ImGui::CreateContext();
		ImGui::SetCurrentContext(context_);
		ImGuiIO& io = ImGui::GetIO();
		io.IniFilename = nullptr;
		io.BackendPlatformName = "midi_langs_pugl";
		ImGuiPlatformIO& platform = ImGui::GetPlatformIO();
		platform.Platform_ClipboardUserData = this;
		platform.Platform_GetClipboardTextFn = [](ImGuiContext*) -> const char* {
			return static_cast<ImGuiEditor*>(ImGui::GetPlatformIO().Platform_ClipboardUserData)->clipboard_.c_str();
		};
		platform.Platform_SetClipboardTextFn = [](ImGuiContext*, const char* text) {
			auto* self = static_cast<ImGuiEditor*>(ImGui::GetPlatformIO().Platform_ClipboardUserData);
			self->clipboard_ = text;
			puglSetClipboard(self->view_, PUGL_CLIPBOARD_GENERAL, "text/plain", text, strlen(text));
		};
		ImGui::StyleColorsDark();
		ImGui::GetStyle().FontSizeBase = 15.f;

		view_ = puglNewView(world_);
		puglSetHandle(view_, this);
		puglSetBackend(view_, puglGlBackend());
		puglSetEventFunc(view_, [](PuglView* v, const PuglEvent* e) {
			return static_cast<ImGuiEditor*>(puglGetHandle(v))->onEvent(e);
		});
		puglSetViewHint(view_, PUGL_CONTEXT_API, PUGL_OPENGL_API);
		puglSetViewHint(view_, PUGL_CONTEXT_VERSION_MAJOR, 3);
		puglSetViewHint(view_, PUGL_CONTEXT_VERSION_MINOR, 2);
		puglSetViewHint(view_, PUGL_CONTEXT_PROFILE, PUGL_OPENGL_CORE_PROFILE);
		puglSetViewHint(view_, PUGL_DOUBLE_BUFFER, 1);
		puglSetViewHint(view_, PUGL_RESIZABLE, 1);
		puglSetSizeHint(view_, PUGL_DEFAULT_SIZE, kEditorWidth, kEditorHeight);
	}

	~ImGuiEditor() override
	{
		if (realized_) {
			puglStopTimer(view_, kFrameTimer);
			puglUnrealize(view_); // PUGL_UNREALIZE shuts down the renderer with the context current
		}
		puglFreeView(view_);
		puglFreeWorld(world_);
		ImGui::DestroyContext(context_);
	}

	bool setParent(const clap_window* window) override
	{
		PuglNativeView parent = 0;
#if defined(__APPLE__)
		parent = (PuglNativeView)window->cocoa;
#elif defined(_WIN32)
		parent = (PuglNativeView)window->win32;
#else
		parent = (PuglNativeView)window->x11;
#endif
		puglSetParent(view_, parent);
		if (puglRealize(view_) != PUGL_SUCCESS) return false;
		realized_ = true;
		setSize(hostWidth_, hostHeight_); // the scale factor is known only after realizing
		puglStartTimer(view_, kFrameTimer, kFramePeriod);
		return puglShow(view_, PUGL_SHOW_RAISE) == PUGL_SUCCESS;
	}

	void show() override
	{
		if (realized_) puglShow(view_, PUGL_SHOW_PASSIVE);
	}

	void hide() override
	{
		if (realized_) puglHide(view_);
	}

	void setSize(unsigned width, unsigned height) override
	{
		hostWidth_ = width;
		hostHeight_ = height;
		if (!realized_) return;
#ifdef __APPLE__
		const double toPixels = puglGetScaleFactor(view_); // CLAP sizes are points on macOS
#else
		const double toPixels = 1.0;
#endif
		puglSetSizeHint(view_, PUGL_CURRENT_SIZE, (unsigned)(width * toPixels), (unsigned)(height * toPixels));
	}

	void setUdpState(bool on) override { udpOn_ = on; }
	void setText(const std::string& text) override { text_ = text; }
	void setWrap(bool on) override { wrap_ = on; }
	void setLive(bool on) override { live_ = on; }

	void idle() override { puglUpdate(world_, 0.0); }

private:
	PuglStatus onEvent(const PuglEvent* event)
	{
		ImGui::SetCurrentContext(context_);
		ImGuiIO& io = ImGui::GetIO();
		const double scale = puglGetScaleFactor(view_);

		switch (event->type) {
		case PUGL_REALIZE:
			ImGui_ImplOpenGL3_Init("#version 150");
			break;
		case PUGL_UNREALIZE:
			ImGui_ImplOpenGL3_Shutdown();
			break;
		case PUGL_CONFIGURE:
			width_ = event->configure.width;
			height_ = event->configure.height;
			break;
		case PUGL_EXPOSE:
			render(scale);
			break;
		case PUGL_TIMER:
			pullLog();
			puglObscureView(view_);
			break;
		case PUGL_FOCUS_IN:
		case PUGL_FOCUS_OUT:
			io.AddFocusEvent(event->type == PUGL_FOCUS_IN);
			break;
		case PUGL_MOTION:
			setMods(event->motion.state);
			io.AddMousePosEvent((float)(event->motion.x / scale), (float)(event->motion.y / scale));
			break;
		case PUGL_BUTTON_PRESS:
		case PUGL_BUTTON_RELEASE:
			if (event->type == PUGL_BUTTON_PRESS) puglGrabFocus(view_);
			setMods(event->button.state);
			io.AddMousePosEvent((float)(event->button.x / scale), (float)(event->button.y / scale));
			if (event->button.button < ImGuiMouseButton_COUNT) {
				io.AddMouseButtonEvent((int)event->button.button, event->type == PUGL_BUTTON_PRESS);
			}
			break;
		case PUGL_SCROLL:
			setMods(event->scroll.state);
			io.AddMouseWheelEvent((float)event->scroll.dx, (float)event->scroll.dy);
			break;
		case PUGL_KEY_PRESS:
		case PUGL_KEY_RELEASE: {
			const PuglKeyEvent& key = event->key;
			setMods(key.state);
			const bool down = event->type == PUGL_KEY_PRESS;
			if (down && key.key == '.' && (key.state & (PUGL_MOD_CTRL | PUGL_MOD_SUPER))) {
				callbacks_.stop();
				break;
			}
			const bool enter = key.key == PUGL_KEY_ENTER || key.key == PUGL_KEY_PAD_ENTER;
			if (down && enter && (key.state & kPrimaryMod) && (key.state & PUGL_MOD_SHIFT)) {
				evalAll();
				break;
			}
			// Paste needs the clipboard first; pugl delivers it asynchronously, then the paste is replayed.
			if (down && (key.key == 'v' || key.key == 'V') && (key.state & kPrimaryMod)) {
				puglPaste(view_);
				break;
			}
			ImGuiKey imguiKey = ToImGuiKey(key.key);
			if (imguiKey == ImGuiKey_None) break;
			io.AddKeyEvent(imguiKey, down);
#ifdef __APPLE__
			// AppKit never sends key-up while Cmd is held; without this ImGui auto-repeats the key.
			if (down && (key.state & PUGL_MOD_SUPER)) io.AddKeyEvent(imguiKey, false);
#endif
			break;
		}
		case PUGL_TEXT:
			if (event->text.character >= 0x20 && event->text.character != 0x7f) {
				io.AddInputCharactersUTF8(event->text.string);
			}
			break;
		case PUGL_DATA_OFFER:
			acceptTextOffer(&event->offer);
			break;
		case PUGL_DATA: {
			size_t len = 0;
			const void* data = puglGetClipboard(view_, PUGL_CLIPBOARD_GENERAL, event->data.typeIndex, &len);
			if (data) {
				clipboard_.assign((const char*)data, len);
				while (!clipboard_.empty() && clipboard_.back() == '\0') clipboard_.pop_back();
				replayPaste();
			}
			break;
		}
		default:
			break;
		}
		if (event->type != PUGL_EXPOSE && event->type != PUGL_TIMER && event->type != PUGL_CONFIGURE) {
			puglObscureView(view_);
		}
		return PUGL_SUCCESS;
	}

	// text_ is current while the field is active: imgui_stdlib writes each edit through.
	void evalAll()
	{
		if (text_.find_first_not_of(" \t\r\n") != std::string::npos) callbacks_.eval(text_);
	}

	void setMods(PuglMods mods)
	{
		ImGuiIO& io = ImGui::GetIO();
		io.AddKeyEvent(ImGuiMod_Ctrl, (mods & PUGL_MOD_CTRL) != 0);
		io.AddKeyEvent(ImGuiMod_Shift, (mods & PUGL_MOD_SHIFT) != 0);
		io.AddKeyEvent(ImGuiMod_Alt, (mods & PUGL_MOD_ALT) != 0);
		io.AddKeyEvent(ImGuiMod_Super, (mods & PUGL_MOD_SUPER) != 0);
	}

	void acceptTextOffer(const PuglDataOfferEvent* offer)
	{
		const uint32_t n = puglGetNumClipboardTypes(view_, offer->clipboard);
		for (uint32_t i = 0; i < n; ++i) {
			if (IsTextType(puglGetClipboardType(view_, offer->clipboard, i))) {
				puglAcceptOffer(view_, offer, i, PUGL_DATA_ACTION_COPY, 0, 0, width_, height_);
				return;
			}
		}
#if !defined(__APPLE__) && !defined(_WIN32)
		puglRejectOffer(view_, offer, 0, 0, width_, height_); // only X11 implements it in this pugl version
#endif
	}

	// Feeds ImGui the primary+V chord it would have seen; it then reads clipboard_.
	void replayPaste()
	{
		ImGuiIO& io = ImGui::GetIO();
#ifdef __APPLE__
		const ImGuiKey mod = ImGuiMod_Super; // ImGui maps Cmd to Ctrl on macOS
#else
		const ImGuiKey mod = ImGuiMod_Ctrl;
#endif
		io.AddKeyEvent(mod, true);
		io.AddKeyEvent(ImGuiKey_V, true);
		io.AddKeyEvent(ImGuiKey_V, false);
	}

	void pullLog()
	{
		std::string text = callbacks_.drainLog();
		if (text.empty()) return;
		log_ += text;
		if (log_.size() > kMaxLog) log_.erase(0, log_.size() - kMaxLog);
		scrollLog_ = true;
	}

	void render(double scale)
	{
		ImGuiIO& io = ImGui::GetIO();
		const double now = puglGetTime(world_);
		io.DeltaTime = lastTime_ > 0. ? (float)std::max(now - lastTime_, 1e-4) : (float)kFramePeriod;
		lastTime_ = now;
		io.DisplaySize = ImVec2((float)(width_ / scale), (float)(height_ / scale));
		io.DisplayFramebufferScale = ImVec2((float)scale, (float)scale);

		ImGui_ImplOpenGL3_NewFrame();
		ImGui::NewFrame();
		draw();
		ImGui::Render();

		glViewport(0, 0, (GLsizei)width_, (GLsizei)height_);
		glClearColor(0.1f, 0.1f, 0.1f, 1.f);
		glClear(GL_COLOR_BUFFER_BIT);
		ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
	}

	void draw()
	{
		const ImGuiIO& io = ImGui::GetIO();
		ImGui::SetNextWindowPos(ImVec2(0, 0));
		ImGui::SetNextWindowSize(io.DisplaySize);
		ImGui::Begin(language_.name, nullptr,
		             ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoSavedSettings
		                 | ImGuiWindowFlags_NoBringToFrontOnFocus);

		const char* mod = io.ConfigMacOSXBehaviors ? "Cmd" : "Ctrl";
		bool eval = ImGui::Button("Eval Block");
		ImGui::SetItemTooltip("%s+Return: selection, or the block around the caret", mod);
		ImGui::SameLine();
		if (ImGui::Button("Eval All")) evalAll();
		ImGui::SetItemTooltip("%s+Shift+Return: the whole editor as one piece", mod);
		ImGui::SameLine();
		if (ImGui::Button("Stop")) callbacks_.stop();
		ImGui::SetItemTooltip("Ctrl+.: stop the running code and release all notes");
		ImGui::SameLine();
		if (ImGui::Button("Clear")) {
			if (text_.empty()) log_.clear();
			else ImGui::OpenPopup("Clear editor and log?");
		}
		ImGui::SetItemTooltip("Empty the editor and the log");
		drawClearConfirm();
		ImGui::SameLine();
		if (ImGui::Checkbox("Wrap", &wrap_)) callbacks_.setWrap(wrap_);
		ImGui::SetItemTooltip("Wrap long lines in the editor and the log");
		ImGui::SameLine();
		if (ImGui::Checkbox("Live", &live_)) callbacks_.setLive(live_);
		ImGui::SetItemTooltip("Off: an evaluation renders a clip that loops with the host transport. On: code plays as it runs");
		ImGui::SameLine();
		const std::string udpLabel = "UDP :" + std::to_string(port_);
		if (ImGui::Checkbox(udpLabel.c_str(), &udpOn_)) callbacks_.setUdp(udpOn_);
		ImGui::SetItemTooltip("Evaluate datagrams sent to 127.0.0.1:%u; replies with the output", port_);

		const float logHeight = ImGui::GetTextLineHeightWithSpacing() * 8;
		const float codeHeight = ImGui::GetContentRegionAvail().y - logHeight - ImGui::GetStyle().ItemSpacing.y;
		if (refocusCode_) {
			ImGui::SetKeyboardFocusHere();
			refocusCode_ = false;
		}
		// Ctrl/Cmd+Return validates a multiline field (Return inserts a newline); EnterReturnsTrue reports it.
		const ImGuiInputTextFlags flags = ImGuiInputTextFlags_AllowTabInput | ImGuiInputTextFlags_CallbackAlways
		                                  | ImGuiInputTextFlags_EnterReturnsTrue | (wrap_ ? ImGuiInputTextFlags_WordWrap : 0);
		if (ImGui::InputTextMultiline("##code", &text_, ImVec2(-1, codeHeight), flags, &ImGuiEditor::TrackCaret, this)) {
			eval = true;
			refocusCode_ = true;
		}
		if (ImGui::IsItemEdited()) callbacks_.textChanged(text_);
		if (eval) {
			std::string code = SelectEvalCode(text_, selStart_, selEnd_, cursor_, language_.syntax);
			if (!code.empty()) callbacks_.eval(code);
		}

		ImGui::BeginChild("##log", ImVec2(-1, -1), ImGuiChildFlags_Borders, wrap_ ? 0 : ImGuiWindowFlags_HorizontalScrollbar);
		if (wrap_) ImGui::PushTextWrapPos(0.f);
		ImGui::TextUnformatted(log_.data(), log_.data() + log_.size());
		if (wrap_) ImGui::PopTextWrapPos();
		if (scrollLog_) {
			ImGui::SetScrollHereY(1.f);
			scrollLog_ = false;
		}
		ImGui::EndChild();
		ImGui::End();
	}

	// The editor text has no undo after an external change, so clearing it asks first.
	void drawClearConfirm()
	{
		if (!ImGui::BeginPopupModal("Clear editor and log?", nullptr, ImGuiWindowFlags_AlwaysAutoResize)) return;
		ImGui::TextUnformatted("The editor text cannot be restored.");
		if (ImGui::Button("Clear")) {
			text_.clear();
			log_.clear();
			cursor_ = selStart_ = selEnd_ = 0;
			callbacks_.textChanged(text_);
			ImGui::CloseCurrentPopup();
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel") || ImGui::IsKeyPressed(ImGuiKey_Escape)) ImGui::CloseCurrentPopup();
		ImGui::EndPopup();
	}

	static int TrackCaret(ImGuiInputTextCallbackData* data)
	{
		auto* self = static_cast<ImGuiEditor*>(data->UserData);
		self->cursor_ = data->CursorPos;
		self->selStart_ = data->SelectionStart;
		self->selEnd_ = data->SelectionEnd;
		return 0;
	}

	EditorCallbacks callbacks_;
	const LanguageInfo& language_;
	PuglWorld* world_;
	PuglView* view_ = nullptr;
	ImGuiContext* context_ = nullptr;
	bool realized_ = false;
	unsigned hostWidth_ = kEditorWidth, hostHeight_ = kEditorHeight;
	unsigned width_ = kEditorWidth, height_ = kEditorHeight; // pixels
	double lastTime_ = 0.;

	std::string text_;
	std::string log_;
	std::string clipboard_;
	bool udpOn_;
	bool wrap_;
	bool live_;
	unsigned port_;
	bool scrollLog_ = false;
	bool refocusCode_ = false;
	int cursor_ = 0, selStart_ = 0, selEnd_ = 0;
};

} // namespace

const char* EditorWindowApi()
{
#if defined(__APPLE__)
	return CLAP_WINDOW_API_COCOA;
#elif defined(_WIN32)
	return CLAP_WINDOW_API_WIN32;
#else
	return CLAP_WINDOW_API_X11;
#endif
}

std::unique_ptr<Editor> CreateEditor(EditorCallbacks callbacks, const EditorState& state, const LanguageInfo& language)
{
	PuglWorld* world = puglNewWorld(PUGL_MODULE, 0);
	if (!world) return nullptr; // e.g. no X11 display
	// Window class name; unique so other pugl plugins in the same host do not collide.
	puglSetWorldString(world, PUGL_CLASS_NAME, language.windowClass);
	return std::make_unique<ImGuiEditor>(std::move(callbacks), state, language, world);
}
