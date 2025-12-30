/* primitives.c - Arithmetic and basic operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* Arithmetic operations */
void op_add(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a + b);
}

void op_sub(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a - b);
}

void op_mul(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a * b);
}

void op_div(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    if (b == 0) {
        printf("Division by zero!\n");
        return;
    }
    push(&stack, a / b);
}

void op_mod(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    if (b == 0) {
        printf("Modulo by zero!\n");
        return;
    }
    push(&stack, a % b);
}

void op_abs(Stack* s) {
    int32_t a = pop(&stack);
    push(&stack, a < 0 ? -a : a);
}

void op_negate(Stack* s) {
    int32_t a = pop(&stack);
    push(&stack, -a);
}

void op_min(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a < b ? a : b);
}

void op_max(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a > b ? a : b);
}

/* Bitwise operations */
void op_and(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a & b);
}

void op_or(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a | b);
}

void op_xor(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a ^ b);
}

void op_not(Stack* s) {
    int32_t a = pop(&stack);
    push(&stack, ~a);
}

/* Comparison operations - use -1 for true (standard Forth convention) */
void op_eq(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a == b ? -1 : 0);
}

void op_lt(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a < b ? -1 : 0);
}

void op_gt(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a > b ? -1 : 0);
}

void op_le(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a <= b ? -1 : 0);
}

void op_ge(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a >= b ? -1 : 0);
}

void op_ne(Stack* s) {
    int32_t b = pop(&stack);
    int32_t a = pop(&stack);
    push(&stack, a != b ? -1 : 0);
}

/* Output operations */
void op_print(Stack* s) {
    int32_t value = pop(&stack);
    printf("%d ", value);
}

void op_cr(Stack* s) {
    (void)stack;
    printf("\n");
}

void op_space(Stack* s) {
    (void)stack;
    printf(" ");
}

void op_emit(Stack* s) {
    int32_t c = pop(&stack);
    printf("%c", (char)c);
}
