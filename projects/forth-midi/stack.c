/* stack.c - Stack operations for MIDI Forth interpreter */

#include "forth_midi.h"

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
        printf("Stack empty!\n");
        return 0;
    }
    return s->data[s->top];
}

void op_dup(Stack* s) {
    (void)s;
    int32_t a = peek(&stack);
    push(&stack, a);
}

void op_drop(Stack* s) {
    (void)s;
    pop(&stack);
}

void op_swap(Stack* s) {
    (void)s;
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, b);
    push(&stack, a);
}

void op_over(Stack* s) {
    (void)s;
    int32_t b = pop(&stack);
    int32_t a = peek(&stack);
    push(&stack, b);
    push(&stack, a);
}

void op_rot(Stack* s) {
    (void)s;
    int32_t c = pop(&stack);
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, b);
    push(&stack, c);
    push(&stack, a);
}

void op_depth(Stack* s) {
    (void)s;
    push(&stack, stack.top + 1);
}

void op_clear(Stack* s) {
    (void)s;
    stack.top = -1;
}

void op_print_stack(Stack* s) {
    (void)s;
    printf("<%d> ", stack.top + 1);
    for (int i = 0; i <= stack.top; i++) {
        printf("%d ", stack.data[i]);
    }
}
