// Dear ImGui editor in a pugl OpenGL view embedded in the host's window (Cocoa, Win32, X11).
// Adapted from sapf's examples/clap. Ctrl/Cmd+Return evaluates the selection or current block;
// Ctrl/Cmd+Shift+Return evaluates the whole editor. Ctrl+. (or Cmd+. where the host delivers it;
// AppKit usually consumes Cmd+.) stops all notes.

#include "editor.hpp"
#include "editor_logic.hpp"
#include "highlight.hpp"
#include "language.hpp"

#include <TextEditor.h>

#include <clap/ext/gui.h>

#include <pugl/gl.h>
#include <pugl/pugl.h>

#include <imgui.h>
#include <imgui_impl_opengl3.h>

#include <algorithm>
#include <cstring>
#include <string>
#include <vector>

// The plugin's tree-sitter grammar and highlight query (midi_langs_add_clap GRAMMAR), if any.
#ifdef MIDI_LANGS_HIGHLIGHT_HEADER
#include MIDI_LANGS_HIGHLIGHT_HEADER
#define HIGHLIGHT_GRAMMAR MIDI_LANGS_GRAMMAR(), MIDI_LANGS_HIGHLIGHT_QUERY
#else
#define HIGHLIGHT_GRAMMAR nullptr, nullptr
#endif

namespace {

using DocPos = TextEditor::DocPos;
using Color = TextEditor::Color;

// TextEditor positions are a line and a glyph index, one glyph per code point.
// For each byte offset of text, plus its end, the position it falls at.
std::vector<DocPos> Positions(const std::string& text)
{
	std::vector<DocPos> positions;
	positions.reserve(text.size() + 1);
	size_t line = 0, glyph = 0;
	for (size_t i = 0; i < text.size(); ++i) {
		positions.emplace_back(line, glyph);
		if (text[i] == '\n') {
			++line;
			glyph = 0;
		} else if (((unsigned char)text[i] & 0xC0) != 0x80 && (i + 1 == text.size() || ((unsigned char)text[i + 1] & 0xC0) != 0x80)) {
			++glyph; // the last byte of a code point
		}
	}
	positions.emplace_back(line, glyph);
	return positions;
}

// The byte offset in text of a TextEditor position.
size_t ByteOffset(const std::string& text, DocPos pos)
{
	size_t i = 0, line = 0;
	while (i < text.size() && line < pos.line) {
		if (text[i++] == '\n') ++line;
	}
	for (size_t glyph = 0; i < text.size() && text[i] != '\n' && glyph < pos.index; ++glyph) {
		++i;
		while (i < text.size() && ((unsigned char)text[i] & 0xC0) == 0x80) ++i;
	}
	return i;
}

Color EditorColor(Highlight kind)
{
	switch (kind) {
	case Highlight::kKeyword: return Color::keyword;
	case Highlight::kString: return Color::string;
	case Highlight::kNumber: return Color::number;
	case Highlight::kComment: return Color::comment;
	case Highlight::kFunction: return Color::knownIdentifier;
	case Highlight::kType: return Color::declaration;
	case Highlight::kConstant: return Color::preprocessor;
	case Highlight::kOperator: return Color::punctuation;
	case Highlight::kVariable: return Color::identifier;
	default: return Color::text;
	}
}

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
		: callbacks_(std::move(callbacks)), language_(language), world_(world), highlighter_(HIGHLIGHT_GRAMMAR), udpOn_(state.udpOn), wrap_(state.wrap), live_(state.live), port_(state.port)
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

		// After the ImGui context exists.
		editor_.SetText(state.text);
		editor_.SetWordWrapEnabled(wrap_);
		editor_.SetShowMiniMapEnabled(false);
		if (highlighter_.ok()) editor_.SetColorizeCallback([this] { colorize(); });

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
	void setText(const std::string& text) override { editor_.SetText(text); }
	void setWrap(bool on) override
	{
		wrap_ = on;
		editor_.SetWordWrapEnabled(on);
	}
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
			if (down && enter && (key.state & kPrimaryMod)) {
				if (key.state & PUGL_MOD_SHIFT) evalAll();
				else evalBlock_ = true; // drawn next frame; the editor never sees the key
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

	void evalAll()
	{
		const std::string text = editor_.GetText();
		if (text.find_first_not_of(" \t\r\n") != std::string::npos) callbacks_.eval(text);
	}

	// Called by TextEditor while rendering, after an edit: recolours the whole text.
	void colorize()
	{
		const std::string text = editor_.GetText();
		const std::vector<DocPos> at = Positions(text);
		editor_.SetColor(DocPos(0, 0), at.back(), Color::text);
		for (const HighlightRun& run : highlighter_.highlight(text)) {
			editor_.SetColor(at[run.start], at[run.end], EditorColor(run.kind));
		}
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
		if (eval) refocusCode_ = true;
		eval = eval || evalBlock_;
		evalBlock_ = false;
		ImGui::SetItemTooltip("%s+Return: selection, or the block around the caret", mod);
		ImGui::SameLine();
		if (ImGui::Button("Eval All")) evalAll();
		ImGui::SetItemTooltip("%s+Shift+Return: the whole editor as one piece", mod);
		ImGui::SameLine();
		if (ImGui::Button("Stop")) callbacks_.stop();
		ImGui::SetItemTooltip("Ctrl+.: stop the running code and release all notes");
		ImGui::SameLine();
		if (ImGui::Button("Clear")) {
			if (editor_.IsEmpty()) log_.clear();
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
			editor_.SetFocus();
			refocusCode_ = false;
		}
		if (editor_.Render("##code", ImVec2(-1, codeHeight), ImGuiChildFlags_Borders)) callbacks_.textChanged(editor_.GetText());
		if (eval) {
			const std::string text = editor_.GetText();
			const TextEditor::DocSelection selection = editor_.GetMainCursorSelection();
			const size_t caret = ByteOffset(text, editor_.GetMainCursorPosition());
			std::string code = SelectEvalCode(text, (int)ByteOffset(text, selection.start), (int)ByteOffset(text, selection.end),
			                                  (int)caret, language_.syntax);
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
			editor_.SetText("");
			log_.clear();
			callbacks_.textChanged("");
			ImGui::CloseCurrentPopup();
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel") || ImGui::IsKeyPressed(ImGuiKey_Escape)) ImGui::CloseCurrentPopup();
		ImGui::EndPopup();
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

	TextEditor editor_;
	Highlighter highlighter_;
	std::string log_;
	std::string clipboard_;
	bool udpOn_;
	bool wrap_;
	bool live_;
	unsigned port_;
	bool scrollLog_ = false;
	bool refocusCode_ = false;
	bool evalBlock_ = false; // Ctrl/Cmd+Return seen; evaluated in the next draw
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
