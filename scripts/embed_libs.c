/*
 * embed_libs.c - Convert library files to C header with embedded strings
 *
 * This is a C implementation of embed_libs.py, suitable for integration
 * into MicroHs or other C-based build systems.
 *
 * Usage: embed_libs <output.h> <libdir1> [libdir2 ...] [options]
 *
 * Options:
 *   --runtime <dir>    Embed runtime C/H files from <dir>
 *   --lib <file>       Embed a library file (.a) in lib/
 *   --header <file>    Embed a header file in src/runtime/
 *
 * Compile: cc -o embed_libs embed_libs.c
 *
 * The generated header contains an array of EmbeddedFile structs:
 *   typedef struct {
 *       const char* path;
 *       const char* content;
 *       size_t length;
 *   } EmbeddedFile;
 *
 *   static const EmbeddedFile embedded_files[] = { ... };
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <errno.h>

#define MAX_FILES 1024
#define MAX_PATH 4096
#define CHUNK_SIZE 4000  /* Max string literal chunk size for C89 compatibility */

typedef struct {
    char* vfs_path;     /* Path in VFS, e.g., "lib/Prelude.hs" */
    char* full_path;    /* Full filesystem path */
    unsigned char* content;
    size_t length;
} FileEntry;

static FileEntry g_files[MAX_FILES];
static int g_file_count = 0;

/* Forward declarations */
static int collect_files_recursive(const char* dir_path, const char* base_name,
                                   const char* pattern, int include_subdirs);
static int write_header(const char* output_path);
static void free_files(void);

/*
 * Check if filename matches pattern (simple suffix match)
 * Patterns: "*.hs", "*.c", "*.h", "*.hs-boot"
 */
static int matches_pattern(const char* filename, const char* pattern) {
    if (pattern[0] != '*') return 0;

    const char* suffix = pattern + 1;  /* Skip '*' */
    size_t suffix_len = strlen(suffix);
    size_t name_len = strlen(filename);

    if (name_len < suffix_len) return 0;
    return strcmp(filename + name_len - suffix_len, suffix) == 0;
}

/*
 * Check if path is a directory
 */
static int is_directory(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    return S_ISDIR(st.st_mode);
}

/*
 * Read entire file into memory
 */
static unsigned char* read_file(const char* path, size_t* out_length) {
    FILE* f = fopen(path, "rb");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        return NULL;
    }
    fseek(f, 0, SEEK_SET);

    unsigned char* content = malloc(size + 1);
    if (!content) {
        fclose(f);
        return NULL;
    }

    size_t read = fread(content, 1, size, f);
    fclose(f);

    if ((long)read != size) {
        free(content);
        return NULL;
    }

    content[size] = '\0';
    *out_length = size;
    return content;
}

/*
 * Add a file to the collection
 */
static int add_file(const char* vfs_path, const char* full_path) {
    if (g_file_count >= MAX_FILES) {
        fprintf(stderr, "Error: Too many files (max %d)\n", MAX_FILES);
        return -1;
    }

    /* Check for duplicates */
    for (int i = 0; i < g_file_count; i++) {
        if (strcmp(g_files[i].vfs_path, vfs_path) == 0) {
            fprintf(stderr, "Note: Skipping duplicate %s\n", vfs_path);
            return 0;
        }
    }

    size_t length;
    unsigned char* content = read_file(full_path, &length);
    if (!content) {
        fprintf(stderr, "Warning: Could not read %s: %s\n", full_path, strerror(errno));
        return 0;  /* Continue with other files */
    }

    FileEntry* entry = &g_files[g_file_count++];
    entry->vfs_path = strdup(vfs_path);
    entry->full_path = strdup(full_path);
    entry->content = content;
    entry->length = length;

    return 0;
}

/*
 * Recursively collect files matching pattern from directory
 */
static int collect_files_recursive(const char* dir_path, const char* base_name,
                                   const char* pattern, int include_subdirs) {
    DIR* dir = opendir(dir_path);
    if (!dir) {
        fprintf(stderr, "Warning: Could not open directory %s: %s\n",
                dir_path, strerror(errno));
        return 0;
    }

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        /* Skip . and .. */
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char full_path[MAX_PATH];
        snprintf(full_path, sizeof(full_path), "%s/%s", dir_path, entry->d_name);

        if (is_directory(full_path)) {
            if (include_subdirs) {
                /* Build new base name for VFS path */
                char new_base[MAX_PATH];
                if (base_name[0])
                    snprintf(new_base, sizeof(new_base), "%s/%s", base_name, entry->d_name);
                else
                    snprintf(new_base, sizeof(new_base), "%s", entry->d_name);

                collect_files_recursive(full_path, new_base, pattern, 1);
            }
        } else if (matches_pattern(entry->d_name, pattern)) {
            /* Build VFS path */
            char vfs_path[MAX_PATH];
            if (base_name[0])
                snprintf(vfs_path, sizeof(vfs_path), "%s/%s", base_name, entry->d_name);
            else
                snprintf(vfs_path, sizeof(vfs_path), "%s", entry->d_name);

            add_file(vfs_path, full_path);
        }
    }

    closedir(dir);
    return 0;
}

/*
 * Collect .hs and .hs-boot files from a library directory
 * VFS paths will be: lib/Module.hs, lib/Data/List.hs, etc.
 */
static int collect_hs_files(const char* lib_dir) {
    /* Get base name (last component of path) */
    const char* base = strrchr(lib_dir, '/');
    base = base ? base + 1 : lib_dir;

    char dir_copy[MAX_PATH];
    strncpy(dir_copy, lib_dir, sizeof(dir_copy) - 1);
    dir_copy[sizeof(dir_copy) - 1] = '\0';

    /* Remove trailing slash if present */
    size_t len = strlen(dir_copy);
    if (len > 0 && dir_copy[len - 1] == '/')
        dir_copy[len - 1] = '\0';

    collect_files_recursive(dir_copy, base, "*.hs", 1);
    collect_files_recursive(dir_copy, base, "*.hs-boot", 1);

    return 0;
}

/*
 * Collect runtime C/H files
 * VFS paths will be: src/runtime/eval.c, src/runtime/unix/config.h, etc.
 */
static int collect_runtime_files(const char* runtime_dir) {
    /* runtime_dir is typically .../src/runtime
     * We want VFS paths like src/runtime/file.c
     */
    char dir_copy[MAX_PATH];
    strncpy(dir_copy, runtime_dir, sizeof(dir_copy) - 1);
    dir_copy[sizeof(dir_copy) - 1] = '\0';

    /* Remove trailing slash */
    size_t len = strlen(dir_copy);
    if (len > 0 && dir_copy[len - 1] == '/')
        dir_copy[len - 1] = '\0';

    /* Find "src/runtime" in path to build correct VFS prefix */
    const char* src_pos = strstr(dir_copy, "src/runtime");
    const char* base = src_pos ? src_pos : "src/runtime";

    collect_files_recursive(dir_copy, base, "*.c", 1);
    collect_files_recursive(dir_copy, base, "*.h", 1);

    return 0;
}

/*
 * Embed a single file with a specified VFS prefix
 * e.g., embed_single_file("/path/to/libfoo.a", "lib") -> "lib/libfoo.a"
 */
static int embed_single_file(const char* file_path, const char* vfs_prefix) {
    /* Get basename */
    const char* basename = strrchr(file_path, '/');
    basename = basename ? basename + 1 : file_path;

    /* Build VFS path */
    char vfs_path[MAX_PATH];
    snprintf(vfs_path, sizeof(vfs_path), "%s/%s", vfs_prefix, basename);

    /* Get file size for output */
    struct stat st;
    if (stat(file_path, &st) == 0) {
        printf("  %s (%ld bytes)\n", basename, (long)st.st_size);
    }

    return add_file(vfs_path, file_path);
}

/*
 * Write escaped byte to output
 * Returns number of characters written to buffer
 */
static int escape_byte(unsigned char byte, char* buf) {
    switch (byte) {
        case '\\': return sprintf(buf, "\\\\");
        case '"':  return sprintf(buf, "\\\"");
        case '\n': return sprintf(buf, "\\n");
        case '\r': return sprintf(buf, "\\r");
        case '\t': return sprintf(buf, "\\t");
        default:
            if (byte < 32 || byte > 126) {
                /* Non-printable: use octal escape */
                return sprintf(buf, "\\%03o", byte);
            } else {
                buf[0] = byte;
                buf[1] = '\0';
                return 1;
            }
    }
}

/*
 * Write file content as C string literal(s)
 * Splits into multiple literals if needed for C89 compatibility
 */
static void write_string_literal(FILE* out, const unsigned char* content, size_t length) {
    char escape_buf[8];
    int chunk_len = 0;
    int first_chunk = 1;

    fprintf(out, "      ");

    for (size_t i = 0; i < length; i++) {
        int esc_len = escape_byte(content[i], escape_buf);

        /* Check if we need to start a new chunk */
        if (chunk_len + esc_len > CHUNK_SIZE) {
            if (first_chunk) {
                fprintf(out, "\"\n      ");
                first_chunk = 0;
            } else {
                fprintf(out, "\"\n      ");
            }
            chunk_len = 0;
        }

        if (chunk_len == 0) {
            fprintf(out, "\"");
        }

        fprintf(out, "%s", escape_buf);
        chunk_len += esc_len;
    }

    if (chunk_len > 0 || length == 0) {
        fprintf(out, "\"");
    }
}

/*
 * Compare function for sorting files by VFS path
 */
static int compare_files(const void* a, const void* b) {
    const FileEntry* fa = (const FileEntry*)a;
    const FileEntry* fb = (const FileEntry*)b;
    return strcmp(fa->vfs_path, fb->vfs_path);
}

/*
 * Write the output header file
 */
static int write_header(const char* output_path) {
    FILE* out = fopen(output_path, "w");
    if (!out) {
        fprintf(stderr, "Error: Could not create %s: %s\n", output_path, strerror(errno));
        return -1;
    }

    /* Sort files by VFS path */
    qsort(g_files, g_file_count, sizeof(FileEntry), compare_files);

    /* Calculate totals */
    size_t total_size = 0;
    int hs_count = 0, runtime_count = 0, lib_count = 0;
    for (int i = 0; i < g_file_count; i++) {
        total_size += g_files[i].length;
        if (matches_pattern(g_files[i].vfs_path, "*.hs") ||
            matches_pattern(g_files[i].vfs_path, "*.hs-boot"))
            hs_count++;
        else if (matches_pattern(g_files[i].vfs_path, "*.a"))
            lib_count++;
        else
            runtime_count++;
    }

    /* Write header */
    fprintf(out, "/* Auto-generated by embed_libs - DO NOT EDIT */\n");
    fprintf(out, "/* %d embedded files (%d .hs, %d runtime, %d libs, %zu bytes total) */\n\n",
            g_file_count, hs_count, runtime_count, lib_count, total_size);
    fprintf(out, "#ifndef MHS_EMBEDDED_LIBS_H\n");
    fprintf(out, "#define MHS_EMBEDDED_LIBS_H\n\n");
    fprintf(out, "#include <stddef.h>\n\n");

    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;      /* VFS path, e.g., \"lib/Prelude.hs\" */\n");
    fprintf(out, "    const char* content;   /* File contents */\n");
    fprintf(out, "    size_t length;         /* Content length in bytes */\n");
    fprintf(out, "} EmbeddedFile;\n\n");

    fprintf(out, "static const EmbeddedFile embedded_files[] = {\n");

    for (int i = 0; i < g_file_count; i++) {
        FileEntry* entry = &g_files[i];
        fprintf(out, "    /* %s */\n", entry->full_path);
        fprintf(out, "    { \"%s\",\n", entry->vfs_path);
        write_string_literal(out, entry->content, entry->length);
        fprintf(out, ",\n");
        fprintf(out, "      %zu },\n\n", entry->length);
    }

    fprintf(out, "    /* Sentinel */\n");
    fprintf(out, "    { NULL, NULL, 0 }\n");
    fprintf(out, "};\n\n");

    fprintf(out, "#define EMBEDDED_FILE_COUNT %d\n\n", g_file_count);
    fprintf(out, "#endif /* MHS_EMBEDDED_LIBS_H */\n");

    fclose(out);

    printf("Generated: %s (%d files, %zu bytes)\n", output_path, g_file_count, total_size);
    return 0;
}

/*
 * Free all allocated memory
 */
static void free_files(void) {
    for (int i = 0; i < g_file_count; i++) {
        free(g_files[i].vfs_path);
        free(g_files[i].full_path);
        free(g_files[i].content);
    }
    g_file_count = 0;
}

static void print_usage(const char* prog) {
    printf("Usage: %s <output.h> <libdir1> [libdir2 ...] [options]\n\n", prog);
    printf("Options:\n");
    printf("  --runtime <dir>    Embed runtime C/H files from <dir>\n");
    printf("  --lib <file>       Embed a library file (.a) in lib/ (repeatable)\n");
    printf("  --header <file>    Embed a header file in src/runtime/ (repeatable)\n");
    printf("  --help             Show this help\n");
    printf("\nExamples:\n");
    printf("  %s mhs_embedded.h lib/ --runtime src/runtime/\n", prog);
    printf("  %s out.h lib/ --lib libfoo.a --lib libbar.a --header foo.h\n", prog);
    printf("\nThis generates a C header with all .hs files from lib/ and\n");
    printf("optionally runtime files, static libraries, and headers.\n");
}

int main(int argc, char** argv) {
    if (argc < 3) {
        print_usage(argv[0]);
        return 1;
    }

    if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
        print_usage(argv[0]);
        return 0;
    }

    const char* output_path = argv[1];
    const char* runtime_dir = NULL;

    /* Collect --lib and --header files in arrays for deferred processing */
    const char* lib_files[64];
    const char* header_files[64];
    int lib_count = 0, header_count = 0;

    /* Parse arguments */
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "--runtime") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --runtime requires an argument\n");
                return 1;
            }
            runtime_dir = argv[++i];
        } else if (strcmp(argv[i], "--lib") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --lib requires an argument\n");
                return 1;
            }
            if (lib_count >= 64) {
                fprintf(stderr, "Error: Too many --lib arguments (max 64)\n");
                return 1;
            }
            lib_files[lib_count++] = argv[++i];
        } else if (strcmp(argv[i], "--header") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --header requires an argument\n");
                return 1;
            }
            if (header_count >= 64) {
                fprintf(stderr, "Error: Too many --header arguments (max 64)\n");
                return 1;
            }
            header_files[header_count++] = argv[++i];
        } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
            return 1;
        } else {
            /* Library directory */
            printf("Collecting .hs files from %s...\n", argv[i]);
            collect_hs_files(argv[i]);
        }
    }

    if (runtime_dir) {
        printf("Collecting runtime files from %s...\n", runtime_dir);
        collect_runtime_files(runtime_dir);
    }

    if (header_count > 0) {
        printf("Embedding headers:\n");
        for (int i = 0; i < header_count; i++) {
            embed_single_file(header_files[i], "src/runtime");
        }
    }

    if (lib_count > 0) {
        printf("Embedding libraries:\n");
        for (int i = 0; i < lib_count; i++) {
            embed_single_file(lib_files[i], "lib");
        }
    }

    if (g_file_count == 0) {
        fprintf(stderr, "Error: No files found\n");
        return 1;
    }

    int result = write_header(output_path);

    free_files();

    return result;
}
