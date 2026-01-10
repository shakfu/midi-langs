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

AldaError* alda_error_new(AldaErrorType type, const char* message,
                          AldaSourcePos pos, const char* source) {
    AldaError* err = (AldaError*)malloc(sizeof(AldaError));
    if (!err) return NULL;

    err->type = type;
    err->pos = pos;
    err->source_line = NULL;

    if (message) {
        size_t len = strlen(message);
        err->message = (char*)malloc(len + 1);
        if (!err->message) {
            free(err);
            return NULL;
        }
        memcpy(err->message, message, len + 1);
    } else {
        err->message = NULL;
    }

    if (source && pos.line > 0) {
        err->source_line = alda_extract_line(source, pos.line);
    }

    return err;
}

void alda_error_free(AldaError* err) {
    if (err) {
        free(err->message);
        free(err->source_line);
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
    size_t size = 256;
    if (err->message) size += strlen(err->message);
    if (err->source_line) size += strlen(err->source_line) + 20;
    if (err->pos.filename) size += strlen(err->pos.filename);

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
