#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <unistd.h>

#include <libremidi/libremidi-c.h>

#define MAX_STACK_SIZE 100
#define MAX_WORD_LENGTH 32
#define MAX_WORDS 100
#define MAX_INPUT_LENGTH 256
#define MAX_PORTS 64

// Stack structure
typedef struct {
    int32_t data[MAX_STACK_SIZE];
    int top;
} Stack;

// Word structure for dictionary
typedef struct {
    char name[MAX_WORD_LENGTH];
    void (*function)(Stack* stack);
    int is_primitive;
} Word;

// Global variables
Stack stack;
Word dictionary[MAX_WORDS];
int dict_count = 0;

// MIDI globals
libremidi_midi_observer_handle* midi_observer = NULL;
libremidi_midi_out_handle* midi_out = NULL;
libremidi_midi_out_port* out_ports[MAX_PORTS];
int out_port_count = 0;

// Stack operations
void push(Stack* s, int32_t value) {
    if (s->top >= MAX_STACK_SIZE - 1) {
        printf("Stack overflow!\n");
        return;
    }
    s->data[++(s->top)] = value;
}

int32_t pop(Stack* s) {
    if (s->top < 0) {
        printf("Stack underflow!\n");
        return 0;
    }
    return s->data[(s->top)--];
}

int32_t peek(Stack* s) {
    if (s->top < 0) {
        printf("Stack underflow!\n");
        return 0;
    }
    return s->data[s->top];
}

// Dictionary operations
int find_word(const char* name) {
    for (int i = 0; i < dict_count; i++) {
        if (strcmp(dictionary[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

void add_word(const char* name, void (*function)(Stack* stack), int is_primitive) {
    if (dict_count >= MAX_WORDS) {
        printf("Dictionary full!\n");
        return;
    }

    strcpy(dictionary[dict_count].name, name);
    dictionary[dict_count].function = function;
    dictionary[dict_count].is_primitive = is_primitive;
    dict_count++;
}

// Primitive operations
void op_plus(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a + b);
}

void op_minus(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a - b);
}

void op_multiply(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a * b);
}

void op_divide(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    if (b == 0) {
        printf("Division by zero!\n");
        return;
    }
    push(stack, a / b);
}

void op_swap(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, b);
    push(stack, a);
}

void op_dup(Stack* stack) {
    int32_t a = peek(stack);
    push(stack, a);
}

void op_drop(Stack* stack) {
    pop(stack);
}

void op_over(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = peek(stack);
    push(stack, b);
    push(stack, a);
}

void op_rot(Stack* stack) {
    int32_t c = pop(stack);
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, b);
    push(stack, c);
    push(stack, a);
}

void op_dot(Stack* stack) {
    int32_t value = pop(stack);
    printf("%d ", value);
}

void op_cr(Stack* stack) {
    printf("\n");
}

void op_space(Stack* stack) {
    printf(" ");
}

void op_dot_s(Stack* stack) {
    printf("<%d> ", stack->top + 1);
    for (int i = 0; i <= stack->top; i++) {
        printf("%d ", stack->data[i]);
    }
}

void op_and(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a & b);
}

void op_or(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a | b);
}

void op_xor(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a ^ b);
}

void op_not(Stack* stack) {
    int32_t a = pop(stack);
    push(stack, ~a);
}

void op_eq(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a == b) ? -1 : 0);
}

void op_lt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a < b) ? -1 : 0);
}

void op_gt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a > b) ? -1 : 0);
}

void op_ms(Stack* stack) {
    int32_t ms = pop(stack);
    if (ms > 0) {
        usleep(ms * 1000);
    }
}

// MIDI operations

void on_output_port_found(void* ctx, const libremidi_midi_out_port* port) {
    if (out_port_count >= MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &out_ports[out_port_count]);
    out_port_count++;
}

int midi_init_observer(void) {
    if (midi_observer != NULL) return 0;  // Already initialized

    int ret = 0;

    libremidi_observer_configuration observer_conf;
    ret = libremidi_midi_observer_configuration_init(&observer_conf);
    if (ret != 0) return ret;

    observer_conf.track_hardware = true;
    observer_conf.track_virtual = true;
    observer_conf.track_any = true;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) return ret;

    api_conf.configuration_type = Observer;
    api_conf.api = COREMIDI;  // macOS

    ret = libremidi_midi_observer_new(&observer_conf, &api_conf, &midi_observer);
    if (ret != 0) {
        printf("Failed to create MIDI observer\n");
        return ret;
    }

    // Enumerate output ports
    out_port_count = 0;
    ret = libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    return ret;
}

void op_midi_list(Stack* stack) {
    if (midi_init_observer() != 0) {
        printf("Failed to initialize MIDI\n");
        return;
    }

    printf("MIDI output ports:\n");
    for (int i = 0; i < out_port_count; i++) {
        const char* name = NULL;
        size_t len = 0;
        if (libremidi_midi_out_port_name(out_ports[i], &name, &len) == 0) {
            printf("  %d: %s\n", i, name);
        }
    }
    if (out_port_count == 0) {
        printf("  (no ports found)\n");
    }
}

void op_midi_open(Stack* stack) {
    int32_t port_idx = pop(stack);

    if (midi_init_observer() != 0) {
        printf("Failed to initialize MIDI\n");
        return;
    }

    if (port_idx < 0 || port_idx >= out_port_count) {
        printf("Invalid port index: %d (have %d ports)\n", port_idx, out_port_count);
        return;
    }

    // Close existing output if open
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    int ret = 0;

    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.out_port = out_ports[port_idx];

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Output;
    api_conf.api = COREMIDI;

    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
    if (ret != 0) {
        printf("Failed to open MIDI output: %d\n", ret);
        return;
    }

    const char* name = NULL;
    size_t len = 0;
    libremidi_midi_out_port_name(out_ports[port_idx], &name, &len);
    printf("Opened MIDI output: %s\n", name);
}

void op_midi_open_virtual(Stack* stack) {
    // Close existing output if open
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    int ret = 0;

    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = "MidiForth";

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Output;
    api_conf.api = COREMIDI;

    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
    if (ret != 0) {
        printf("Failed to create virtual MIDI output: %d\n", ret);
        return;
    }

    printf("Created virtual MIDI output: MidiForth\n");
}

void op_midi_close(Stack* stack) {
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
        printf("MIDI output closed\n");
    }
}

void op_note_on(Stack* stack) {
    int32_t velocity = pop(stack);
    int32_t pitch = pop(stack);
    int32_t channel = pop(stack);

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (pitch < 0 || pitch > 127) {
        printf("Pitch must be 0-127\n");
        return;
    }
    if (velocity < 0 || velocity > 127) {
        printf("Velocity must be 0-127\n");
        return;
    }

    unsigned char msg[3];
    msg[0] = 0x90 | ((channel - 1) & 0x0F);  // Note On, channel 1-16 -> 0-15
    msg[1] = pitch & 0x7F;
    msg[2] = velocity & 0x7F;

    int ret = libremidi_midi_out_send_message(midi_out, msg, 3);
    if (ret != 0) {
        printf("Failed to send note on\n");
    }
}

void op_note_off(Stack* stack) {
    int32_t velocity = pop(stack);
    int32_t pitch = pop(stack);
    int32_t channel = pop(stack);

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (pitch < 0 || pitch > 127) {
        printf("Pitch must be 0-127\n");
        return;
    }

    unsigned char msg[3];
    msg[0] = 0x80 | ((channel - 1) & 0x0F);  // Note Off
    msg[1] = pitch & 0x7F;
    msg[2] = velocity & 0x7F;

    int ret = libremidi_midi_out_send_message(midi_out, msg, 3);
    if (ret != 0) {
        printf("Failed to send note off\n");
    }
}

void op_cc(Stack* stack) {
    int32_t value = pop(stack);
    int32_t cc_num = pop(stack);
    int32_t channel = pop(stack);

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }

    unsigned char msg[3];
    msg[0] = 0xB0 | ((channel - 1) & 0x0F);  // Control Change
    msg[1] = cc_num & 0x7F;
    msg[2] = value & 0x7F;

    int ret = libremidi_midi_out_send_message(midi_out, msg, 3);
    if (ret != 0) {
        printf("Failed to send CC\n");
    }
}

void op_all_notes_off(Stack* stack) {
    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    // Send All Notes Off (CC 123) on all channels
    for (int ch = 0; ch < 16; ch++) {
        unsigned char msg[3];
        msg[0] = 0xB0 | ch;
        msg[1] = 123;  // All Notes Off
        msg[2] = 0;
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }
}

// Initialize the dictionary with primitive words
void init_dictionary(void) {
    // Arithmetic
    add_word("+", op_plus, 1);
    add_word("-", op_minus, 1);
    add_word("*", op_multiply, 1);
    add_word("/", op_divide, 1);

    // Stack manipulation
    add_word("swap", op_swap, 1);
    add_word("dup", op_dup, 1);
    add_word("drop", op_drop, 1);
    add_word("over", op_over, 1);
    add_word("rot", op_rot, 1);
    add_word(".s", op_dot_s, 1);

    // Output
    add_word(".", op_dot, 1);
    add_word("cr", op_cr, 1);
    add_word("space", op_space, 1);

    // Bitwise
    add_word("and", op_and, 1);
    add_word("or", op_or, 1);
    add_word("xor", op_xor, 1);
    add_word("not", op_not, 1);

    // Comparison (use -1 for true, 0 for false - standard Forth)
    add_word("=", op_eq, 1);
    add_word("<", op_lt, 1);
    add_word(">", op_gt, 1);

    // Timing
    add_word("ms", op_ms, 1);

    // MIDI words
    add_word("midi-list", op_midi_list, 1);
    add_word("midi-open", op_midi_open, 1);
    add_word("midi-virtual", op_midi_open_virtual, 1);
    add_word("midi-close", op_midi_close, 1);
    add_word("note-on", op_note_on, 1);
    add_word("note-off", op_note_off, 1);
    add_word("cc", op_cc, 1);
    add_word("panic", op_all_notes_off, 1);
}

// Parse and execute a single word
void execute_word(const char* word) {
    int index = find_word(word);

    if (index == -1) {
        // Try to parse as a number
        char* endptr;
        long num = strtol(word, &endptr, 10);
        if (*endptr == '\0') {
            push(&stack, (int32_t)num);
        } else {
            printf("Unknown word: %s\n", word);
        }
        return;
    }

    // Execute the function for this word
    dictionary[index].function(&stack);
}

// Parse and execute a command line
void execute_line(const char* input) {
    char word[MAX_WORD_LENGTH];
    int i = 0;
    int start = 0;

    while (input[i] != '\0') {
        // Skip whitespace
        while (isspace(input[i])) {
            i++;
        }

        if (input[i] == '\0') break;

        // Extract next word
        start = i;
        while (!isspace(input[i]) && input[i] != '\0') {
            i++;
        }

        int len = i - start;
        if (len >= MAX_WORD_LENGTH) {
            printf("Word too long\n");
            return;
        }

        strncpy(word, input + start, len);
        word[len] = '\0';

        // Execute the word
        execute_word(word);
    }
}

void print_help(void) {
    printf("\nMIDI Forth - A Forth for MIDI sequence generation\n\n");
    printf("MIDI words:\n");
    printf("  midi-list          List available MIDI output ports\n");
    printf("  midi-open ( n -- ) Open MIDI output port by index\n");
    printf("  midi-virtual       Create virtual MIDI output 'MidiForth'\n");
    printf("  midi-close         Close MIDI output\n");
    printf("  note-on  ( ch pitch vel -- )  Send Note On\n");
    printf("  note-off ( ch pitch vel -- )  Send Note Off\n");
    printf("  cc       ( ch cc val -- )     Send Control Change\n");
    printf("  panic              All notes off on all channels\n");
    printf("\nUtility:\n");
    printf("  ms ( n -- )        Sleep for n milliseconds\n");
    printf("  .s                 Show stack contents\n");
    printf("\nExample: Play middle C for 500ms\n");
    printf("  midi-virtual\n");
    printf("  1 60 100 note-on 500 ms 1 60 0 note-off\n\n");
}

// Interactive interpreter loop
void interpreter_loop(void) {
    char input[MAX_INPUT_LENGTH];

    printf("MIDI Forth (type 'help' for commands, 'quit' to exit)\n");

    while (1) {
        printf("> ");
        fflush(stdout);

        if (!fgets(input, sizeof(input), stdin)) {
            break;
        }

        // Remove newline
        input[strcspn(input, "\n")] = 0;

        if (strcmp(input, "quit") == 0) {
            break;
        }

        if (strcmp(input, "help") == 0) {
            print_help();
            continue;
        }

        execute_line(input);
        printf(" ok\n");
    }
}

void cleanup_midi(void) {
    if (midi_out != NULL) {
        op_all_notes_off(NULL);  // Send panic before closing
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;

    if (midi_observer != NULL) {
        libremidi_midi_observer_free(midi_observer);
        midi_observer = NULL;
    }
}

int main(void) {
    // Initialize stack
    stack.top = -1;

    // Initialize dictionary with primitives
    init_dictionary();

    // Start interactive interpreter
    interpreter_loop();

    // Cleanup
    cleanup_midi();

    return 0;
}
