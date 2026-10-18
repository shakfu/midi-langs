#pragma once

#include <string>

// How an eval request extends the caret's line, per language.
struct EvalSyntax {
	enum Unit { kParagraphs, kBrackets };
	Unit unit;
	const char* lineComment; // kBrackets: brackets after it are ignored to the end of the line
	const char* quotes;      // kBrackets: characters that open and close strings
};

// Code to evaluate for an eval request. Offsets are byte positions in `text`.
// Returns the selection if non-empty, otherwise the caret's line; a blank caret line
// (e.g. after the final newline) selects the line above. kParagraphs extends that line to the
// blank lines around it. kBrackets extends it to the whole multi-line top-level ( ), [ ] or { }
// group it is part of. Returns "" for blank code.
std::string SelectEvalCode(const std::string& text, int selStart, int selEnd, int cursor, const EvalSyntax& syntax);
