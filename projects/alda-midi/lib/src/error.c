/**
 * @file error.c
 * @brief Error handling implementation for the Alda parser.
 */

#include "alda/error.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* alda_extract_line(const char* source, int line) {
    if (!source || line < 1) return NULL;

    const char* p = source;
    int current_line = 1;

    /* Find the start of the requested line */
    while (*p && current_line < line) {
        if (*p == '\n') {
            current_line++;
        }
        p++;
    }

    if (current_line != line) return NULL;

    /* Find the end of the line */
    const char* line_start = p;
    while (*p && *p != '\n') {
        p++;
    }

    size_t len = (size_t)(p - line_start);
    char* result = (char*)malloc(len + 1);
    if (!result) return NULL;

    memcpy(result, line_start, len);
    result[len] = '\0';

    return result;
}

static char* strdup_safe(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s);
    char* copy = (char*)malloc(len + 1);
    if (copy) memcpy(copy, s, len + 1);
    return copy;
}

AldaError* alda_error_new(AldaErrorType type, const char* message,
                          AldaSourcePos pos, const char* source) {
    return alda_error_new_detailed(type, message, pos, source, NULL, NULL, NULL);
}

AldaError* alda_error_new_detailed(AldaErrorType type, const char* message,
                                    AldaSourcePos pos, const char* source,
                                    const char* context, const char* expected,
                                    const char* found) {
    AldaError* err = (AldaError*)malloc(sizeof(AldaError));
    if (!err) return NULL;

    err->type = type;
    err->pos = pos;
    err->source_line = NULL;
    err->context = NULL;
    err->expected = NULL;
    err->found = NULL;

    err->message = strdup_safe(message);
    err->context = strdup_safe(context);
    err->expected = strdup_safe(expected);
    err->found = strdup_safe(found);

    if (source && pos.line > 0) {
        err->source_line = alda_extract_line(source, pos.line);
    }

    return err;
}

void alda_error_free(AldaError* err) {
    if (err) {
        free(err->message);
        free(err->source_line);
        free(err->context);
        free(err->expected);
        free(err->found);
        free(err);
    }
}

char* alda_error_format(AldaError* err) {
    if (!err) return NULL;

    const char* type_str;
    switch (err->type) {
        case ALDA_ERR_SCAN: type_str = "Scan error"; break;
        case ALDA_ERR_SYNTAX: type_str = "Syntax error"; break;
        case ALDA_ERR_MEMORY: type_str = "Memory error"; break;
        default: type_str = "Error"; break;
    }

    /* Calculate required buffer size */
    size_t size = 512;
    if (err->message) size += strlen(err->message);
    if (err->source_line) size += strlen(err->source_line) + 20;
    if (err->pos.filename) size += strlen(err->pos.filename);
    if (err->context) size += strlen(err->context) + 20;
    if (err->expected) size += strlen(err->expected) + 20;
    if (err->found) size += strlen(err->found) + 20;

    char* buf = (char*)malloc(size);
    if (!buf) return NULL;

    int offset = 0;

    /* Format: "filename:line:column: Error: message" */
    if (err->pos.filename) {
        offset += snprintf(buf + offset, size - (size_t)offset, "%s:", err->pos.filename);
    }

    if (err->pos.line > 0) {
        offset += snprintf(buf + offset, size - (size_t)offset, "%d:", err->pos.line);
        if (err->pos.column > 0) {
            offset += snprintf(buf + offset, size - (size_t)offset, "%d:", err->pos.column);
        }
        offset += snprintf(buf + offset, size - (size_t)offset, " ");
    }

    offset += snprintf(buf + offset, size - (size_t)offset, "%s", type_str);

    if (err->message) {
        offset += snprintf(buf + offset, size - (size_t)offset, ": %s", err->message);
    }

    /* Add context if available */
    if (err->context) {
        offset += snprintf(buf + offset, size - (size_t)offset, " (%s)", err->context);
    }

    /* Add expected/found hints if available */
    if (err->expected && err->found) {
        offset += snprintf(buf + offset, size - (size_t)offset,
                          "\n  Expected: %s\n  Found: %s", err->expected, err->found);
    } else if (err->expected) {
        offset += snprintf(buf + offset, size - (size_t)offset,
                          "\n  Expected: %s", err->expected);
    }

    /* Add source line with caret if available */
    if (err->source_line && err->pos.column > 0) {
        offset += snprintf(buf + offset, size - (size_t)offset, "\n  %s\n  ", err->source_line);
        for (int i = 1; i < err->pos.column && (size_t)offset < size - 2; i++) {
            buf[offset++] = ' ';
        }
        buf[offset++] = '^';
        buf[offset] = '\0';
    }

    return buf;
}

/* Error list management */

void alda_error_list_init(AldaErrorList* list, int max_errors) {
    if (!list) return;
    list->errors = NULL;
    list->count = 0;
    list->capacity = 0;
    list->max_errors = max_errors > 0 ? max_errors : 10;
}

void alda_error_list_free(AldaErrorList* list) {
    if (!list) return;
    for (int i = 0; i < list->count; i++) {
        alda_error_free(list->errors[i]);
    }
    free(list->errors);
    list->errors = NULL;
    list->count = 0;
    list->capacity = 0;
}

int alda_error_list_add(AldaErrorList* list, AldaError* error) {
    if (!list || !error) return 0;

    /* Check if at limit */
    if (list->max_errors > 0 && list->count >= list->max_errors) {
        alda_error_free(error);
        return 0;
    }

    /* Grow array if needed */
    if (list->count >= list->capacity) {
        int new_cap = list->capacity == 0 ? 4 : list->capacity * 2;
        AldaError** new_errors = (AldaError**)realloc(list->errors,
                                                       (size_t)new_cap * sizeof(AldaError*));
        if (!new_errors) {
            alda_error_free(error);
            return 0;
        }
        list->errors = new_errors;
        list->capacity = new_cap;
    }

    list->errors[list->count++] = error;
    return 1;
}

int alda_error_list_full(AldaErrorList* list) {
    if (!list) return 1;
    return list->max_errors > 0 && list->count >= list->max_errors;
}

char* alda_error_list_format(AldaErrorList* list) {
    if (!list || list->count == 0) return NULL;

    /* First pass: calculate total size */
    size_t total_size = 64;  /* Header */
    char** formatted = (char**)malloc((size_t)list->count * sizeof(char*));
    if (!formatted) return NULL;

    for (int i = 0; i < list->count; i++) {
        formatted[i] = alda_error_format(list->errors[i]);
        if (formatted[i]) {
            total_size += strlen(formatted[i]) + 2;  /* +2 for newlines */
        }
    }

    /* Allocate result buffer */
    char* result = (char*)malloc(total_size);
    if (!result) {
        for (int i = 0; i < list->count; i++) free(formatted[i]);
        free(formatted);
        return NULL;
    }

    /* Build result string */
    int offset = 0;
    if (list->count == 1) {
        offset += snprintf(result + offset, total_size - (size_t)offset, "1 error:\n");
    } else {
        offset += snprintf(result + offset, total_size - (size_t)offset,
                          "%d errors:\n", list->count);
    }

    for (int i = 0; i < list->count; i++) {
        if (formatted[i]) {
            offset += snprintf(result + offset, total_size - (size_t)offset,
                              "%s\n", formatted[i]);
            free(formatted[i]);
        }
    }
    free(formatted);

    return result;
}
