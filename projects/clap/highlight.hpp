#pragma once

// Syntax highlighting with tree-sitter, independent of the editor widget.

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

struct TSLanguage;

enum class Highlight : uint8_t {
	kNone,
	kKeyword,
	kString,
	kNumber,
	kComment,
	kFunction,
	kType,
	kConstant,
	kOperator,
	kVariable,
};

// A run of bytes [start, end) with one highlight. Runs are sorted and do not overlap.
struct HighlightRun {
	uint32_t start;
	uint32_t end;
	Highlight kind;
};

// Maps a tree-sitter capture name, such as "keyword.return" or "constant.numeric", to a highlight.
// Names starting with '_' are the query's own helpers: kNone.
Highlight HighlightForCapture(const std::string& name);

class Highlighter {
public:
	// language and query may be null: highlight() then returns nothing.
	Highlighter(const TSLanguage* language, const char* query);
	~Highlighter();
	Highlighter(const Highlighter&) = delete;
	Highlighter& operator=(const Highlighter&) = delete;

	bool ok() const;
	const std::string& error() const;

	// Parses text whole. Where captures overlap, the inner one wins, then the later pattern,
	// the convention of the vendored highlights.scm files.
	std::vector<HighlightRun> highlight(const std::string& text);

private:
	struct Impl;
	std::unique_ptr<Impl> impl_;
};
