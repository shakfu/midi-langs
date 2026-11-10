#include "highlight.hpp"

#include <tree_sitter/api.h>

#include <algorithm>
#include <cstring>
#include <regex>

namespace {

struct Capture {
	uint32_t start, end;
	uint16_t pattern;
	Highlight kind;
};

// A text predicate. The C API reports predicates but leaves them to the caller.
struct Predicate {
	enum Op { kEq, kMatch, kAnyOf } op;
	bool negate = false;
	uint32_t capture = 0;        // the capture tested
	bool otherIsCapture = false; // kEq against another capture
	uint32_t otherCapture = 0;
	std::vector<std::string> values; // kEq: one string; kAnyOf: the set
	std::regex regex;                // kMatch
};

std::string NodeText(const std::string& text, TSNode node)
{
	const uint32_t start = std::min<uint32_t>(ts_node_start_byte(node), (uint32_t)text.size());
	const uint32_t end = std::min<uint32_t>(ts_node_end_byte(node), (uint32_t)text.size());
	return text.substr(start, end - start);
}

} // namespace

Highlight HighlightForCapture(const std::string& name)
{
	if (name.empty() || name[0] == '_') return Highlight::kNone;
	if (name.rfind("constant.numeric", 0) == 0) return Highlight::kNumber;
	if (name.rfind("constant.character", 0) == 0) return Highlight::kString;
	const std::string head = name.substr(0, name.find('.'));
	static const struct {
		const char* name;
		Highlight kind;
	} kTable[] = {
		{"keyword", Highlight::kKeyword},     {"repeat", Highlight::kKeyword},    {"conditional", Highlight::kKeyword},
		{"include", Highlight::kKeyword},     {"exception", Highlight::kKeyword}, {"preproc", Highlight::kKeyword},
		{"string", Highlight::kString},       {"character", Highlight::kString},  {"escape", Highlight::kString},
		{"number", Highlight::kNumber},       {"float", Highlight::kNumber},      {"boolean", Highlight::kNumber},
		{"comment", Highlight::kComment},     {"function", Highlight::kFunction}, {"method", Highlight::kFunction},
		{"type", Highlight::kType},           {"constructor", Highlight::kType},  {"module", Highlight::kType},
		{"namespace", Highlight::kType},      {"constant", Highlight::kConstant}, {"label", Highlight::kConstant},
		{"attribute", Highlight::kConstant},  {"operator", Highlight::kOperator}, {"punctuation", Highlight::kOperator},
		{"variable", Highlight::kVariable},   {"parameter", Highlight::kVariable}, {"field", Highlight::kVariable},
		{"property", Highlight::kVariable},
	};
	for (const auto& entry : kTable) {
		if (head == entry.name) return entry.kind;
	}
	return Highlight::kNone;
}

struct Highlighter::Impl {
	TSParser* parser = nullptr;
	TSQuery* query = nullptr;
	std::vector<Highlight> captureKinds;          // by capture index
	std::vector<std::vector<Predicate>> predicates; // by pattern index
	std::string error;

	~Impl()
	{
		if (query) ts_query_delete(query);
		if (parser) ts_parser_delete(parser);
	}

	void loadPredicates()
	{
		const uint32_t patterns = ts_query_pattern_count(query);
		predicates.resize(patterns);
		for (uint32_t p = 0; p < patterns; ++p) {
			uint32_t count = 0;
			const TSQueryPredicateStep* steps = ts_query_predicates_for_pattern(query, p, &count);
			for (uint32_t i = 0; i < count;) {
				uint32_t end = i;
				while (end < count && steps[end].type != TSQueryPredicateStepTypeDone) ++end;
				addPredicate(p, steps + i, end - i);
				i = end + 1;
			}
		}
	}

	std::string stringValue(uint32_t id) const
	{
		uint32_t length = 0;
		const char* s = ts_query_string_value_for_id(query, id, &length);
		return std::string(s, length);
	}

	// steps: the predicate name, then its arguments. Directives such as #set! are ignored.
	void addPredicate(uint32_t pattern, const TSQueryPredicateStep* steps, uint32_t count)
	{
		if (count < 2 || steps[0].type != TSQueryPredicateStepTypeString) return;
		std::string name = stringValue(steps[0].value_id);
		Predicate pred;
		if (name.rfind("not-", 0) == 0) {
			pred.negate = true;
			name = name.substr(4);
		}
		if (steps[1].type != TSQueryPredicateStepTypeCapture) return;
		pred.capture = steps[1].value_id;
		if (name == "eq?" && count == 3) {
			pred.op = Predicate::kEq;
			if (steps[2].type == TSQueryPredicateStepTypeCapture) {
				pred.otherIsCapture = true;
				pred.otherCapture = steps[2].value_id;
			} else {
				pred.values.push_back(stringValue(steps[2].value_id));
			}
		} else if ((name == "match?" || name == "lua-match?") && count == 3
		           && steps[2].type == TSQueryPredicateStepTypeString) {
			pred.op = Predicate::kMatch;
			try {
				pred.regex = std::regex(stringValue(steps[2].value_id), std::regex::ECMAScript);
			} catch (const std::regex_error&) {
				return; // a pattern std::regex cannot read: the predicate is dropped
			}
		} else if (name == "any-of?") {
			pred.op = Predicate::kAnyOf;
			for (uint32_t i = 2; i < count; ++i) {
				if (steps[i].type == TSQueryPredicateStepTypeString) pred.values.push_back(stringValue(steps[i].value_id));
			}
		} else {
			return;
		}
		predicates[pattern].push_back(std::move(pred));
	}

	static bool Holds(const Predicate& pred, const TSQueryMatch& match, const std::string& text)
	{
		bool found = false, holds = true;
		for (uint16_t i = 0; i < match.capture_count && holds; ++i) {
			if (match.captures[i].index != pred.capture) continue;
			found = true;
			const std::string value = NodeText(text, match.captures[i].node);
			switch (pred.op) {
			case Predicate::kEq:
				if (pred.otherIsCapture) {
					for (uint16_t j = 0; j < match.capture_count; ++j) {
						if (match.captures[j].index == pred.otherCapture) holds = value == NodeText(text, match.captures[j].node);
					}
				} else {
					holds = value == pred.values[0];
				}
				break;
			case Predicate::kMatch:
				holds = std::regex_search(value, pred.regex);
				break;
			case Predicate::kAnyOf:
				holds = std::find(pred.values.begin(), pred.values.end(), value) != pred.values.end();
				break;
			}
		}
		if (!found) return true; // an optional capture that did not match
		return pred.negate ? !holds : holds;
	}
};

Highlighter::Highlighter(const TSLanguage* language, const char* query) : impl_(std::make_unique<Impl>())
{
	if (!language || !query) {
		impl_->error = "no grammar";
		return;
	}
	impl_->parser = ts_parser_new();
	if (!ts_parser_set_language(impl_->parser, language)) {
		impl_->error = "grammar ABI not supported by this tree-sitter";
		return;
	}
	uint32_t errorOffset = 0;
	TSQueryError errorType = TSQueryErrorNone;
	impl_->query = ts_query_new(language, query, (uint32_t)strlen(query), &errorOffset, &errorType);
	if (!impl_->query) {
		impl_->error = "highlight query error " + std::to_string((int)errorType) + " at byte " + std::to_string(errorOffset);
		return;
	}
	const uint32_t captures = ts_query_capture_count(impl_->query);
	for (uint32_t i = 0; i < captures; ++i) {
		uint32_t length = 0;
		const char* name = ts_query_capture_name_for_id(impl_->query, i, &length);
		impl_->captureKinds.push_back(HighlightForCapture(std::string(name, length)));
	}
	impl_->loadPredicates();
}

Highlighter::~Highlighter() = default;

bool Highlighter::ok() const
{
	return impl_->query != nullptr;
}

const std::string& Highlighter::error() const
{
	return impl_->error;
}

std::vector<HighlightRun> Highlighter::highlight(const std::string& text)
{
	std::vector<HighlightRun> runs;
	if (!ok() || text.empty()) return runs;
	TSTree* tree = ts_parser_parse_string(impl_->parser, nullptr, text.data(), (uint32_t)text.size());
	if (!tree) return runs;

	std::vector<Capture> captures;
	TSQueryCursor* cursor = ts_query_cursor_new();
	ts_query_cursor_exec(cursor, impl_->query, ts_tree_root_node(tree));
	TSQueryMatch match;
	while (ts_query_cursor_next_match(cursor, &match)) {
		bool holds = true;
		for (const Predicate& pred : impl_->predicates[match.pattern_index]) {
			if (!(holds = Impl::Holds(pred, match, text))) break;
		}
		if (!holds) continue;
		for (uint16_t i = 0; i < match.capture_count; ++i) {
			const Highlight kind = impl_->captureKinds[match.captures[i].index];
			if (kind == Highlight::kNone) continue;
			const TSNode node = match.captures[i].node;
			captures.push_back({ts_node_start_byte(node), ts_node_end_byte(node), match.pattern_index, kind});
		}
	}
	ts_query_cursor_delete(cursor);
	ts_tree_delete(tree);

	// Paint outer before inner and earlier patterns before later ones, so the last paint wins.
	std::sort(captures.begin(), captures.end(), [](const Capture& a, const Capture& b) {
		if (a.start != b.start) return a.start < b.start;
		if (a.end != b.end) return a.end > b.end;
		return a.pattern < b.pattern;
	});
	std::vector<Highlight> paint(text.size(), Highlight::kNone);
	for (const Capture& c : captures) {
		const uint32_t end = std::min<uint32_t>(c.end, (uint32_t)text.size());
		std::fill(paint.begin() + std::min(c.start, end), paint.begin() + end, c.kind);
	}
	for (uint32_t i = 0; i < paint.size();) {
		uint32_t j = i + 1;
		while (j < paint.size() && paint[j] == paint[i]) ++j;
		if (paint[i] != Highlight::kNone) runs.push_back({i, j, paint[i]});
		i = j;
	}
	return runs;
}
