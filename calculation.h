#ifndef FPE_CALCULATION_H
#define FPE_CALCULATION_H

#include <stdint.h>

typedef int64_t int64;
typedef int32_t int32;
typedef int16_t int16;
typedef int8_t int8;

typedef uint64_t uint64;
typedef uint32_t uint32;
typedef uint16_t uint16;
typedef uint8_t uint8;

typedef unsigned int uint;

/* We should ifdef this out for an x86 bitscan when possible. */
int64 ilog(int64 y_signed) {
    if (y_signed <= 0) return 0x8000000000000000; /* -2^63 */
    uint64 y = y_signed;
    /* We could just do this:
    int64 x = 1;
    while (y > 0) {
        x += 1;
        y >>= 1;
    }
    */
    int64 x = 0;
    /* This could potentially be branchless, which may or may not be faster,
       but the main thing is to do 6 tests instead of 64. */
    if (y >= 0x100000000) {
        x += 32;
        y >>= 32;
    }
    if (y >= 0x10000) {
        x += 16;
        y >>= 16;
    }
    if (y >= 0x100) {
        x += 8;
        y >>= 8;
    }
    if (y >= 0x10) {
        x += 4;
        y >>= 4;
    }
    if (y >= 0x4) {
        x += 2;
        y >>= 2;
    }
    /* y is either 1, 2, or 3 at this point. */
    x += y >> 1;

    return x;
}

enum operation {
    OP_MOV,
    OP_ADD,
    OP_SUB,
    OP_NEG,
    OP_LSHIFT,
    OP_RSHIFT,

    OP_MUL,
    OP_DIVMOD,
    OP_DIV,
    OP_MOD,
    OP_EUC_DIVMOD,
    OP_EUC_DIV,
    OP_EUC_MOD,

    OP_ILOG,

    OP_EQ,
    OP_NEQ,
    OP_LESS,
    OP_GREATER,
    OP_LEQ,
    OP_GEQ,
    OP_LOGICAL_AND,
    OP_LOGICAL_OR,

    OP_SELECT,

    /* By having two immediate flags we limit ourselves to 64 instructions, but
       if we somehow need more then we could set 63 = extensions. */
    OP_IMM1 = 128,
    OP_IMM2 = 64,
};

struct instruction {
    enum operation op;
    union {
        struct {
            int64 arg1;
            int64 arg2;
        } binary;
        int64 *args;
    };
};

struct function {
    int arg_count;
    struct instruction *instructions;
    int instruction_count;
    int intermediates_count;
    int result_count;
    int64 *values;
};

int64 *calculate_function(struct function *f) {
    int64 *values = f->values;
    int value_count = f->arg_count;
    for (int i = 0; i < f->instruction_count; i++) {
        struct instruction *instr = &f->instructions[i];
        enum operation op = instr->op;
        bool imm1 = (op & OP_IMM1) != 0;
        bool imm2 = (op & OP_IMM2) != 0;
        op = op & 63; /* 00111111 */

        int arg_count;
        if (op == OP_MOV) arg_count = 1;
        else if (op == OP_NEG) arg_count = 1;
        else if (op == OP_ILOG) arg_count = 1;
        else if (op == OP_SELECT) arg_count = 3;
        else arg_count = 2;

        int64 arg1 = instr->binary.arg1;
        int64 arg2 = instr->binary.arg2;
        int64 arg3 = 0;
        if (arg_count == 3) {
            arg1 = instr->args[0];
            arg2 = instr->args[1];
            /* arg3 can't be immediate, so do the lookup by default. */
            arg3 = values[instr->args[2]];
        }
        if (arg_count >= 1 & !imm1) arg1 = values[arg1];
        if (arg_count >= 2 & !imm2) arg2 = values[arg2];

        int64 result1;
        int64 result2;
        switch (op) {
        case OP_MOV:
            result1 = arg1;
            break;
        case OP_ADD:
            result1 = arg1 + arg2;
            break;
        case OP_SUB:
            result1 = arg1 - arg2;
            break;
        case OP_NEG:
            result1 = -arg1;
            break;
        case OP_LSHIFT:
            result1 = arg1 << arg2;
            break;
        case OP_RSHIFT:
            result1 = arg1 >> arg2;
            break;

        case OP_MUL:
            result1 = arg1 * arg2;
            break;

        /* Division operations. These have dbz guards and euclidean sign
         * cases... Hopefully the compiler does something good with these?
         * Otherwise we could try some inline assembly. */
        case OP_DIVMOD:
            if (arg2 == 0) {
                result1 = 0x7FFFFFFFFFFFFFFF;
                result2 = 0;
            } else {
                result1 = arg1 / arg2;
                result2 = arg1 % arg2;
            }
            break;
        case OP_DIV:
            if (arg2 == 0) result1 = 0x7FFFFFFFFFFFFFFF;
            else result1 = arg1 / arg2;
            break;
        case OP_MOD:
            if (arg2 == 0) result1 = 0;
            else result1 = arg1 % arg2;
            break;
        case OP_EUC_DIVMOD:
            if (arg2 == 0) {
                result1 = 0x7FFFFFFFFFFFFFFF;
                result2 = 0;
            } else if (arg1 >= 0) {
                result1 = arg1 / arg2;
                result2 = arg1 % arg2;
            } else {
                /* e.g. arg1 = -1 -> shifted = -arg2
                                  -> result1 -> -1, result2 = arg2 - 1,
                        arg1 = -n * arg2 -> shifted = -(n+1) * arg2 + 1
                                         -> result1 = n, result2 = 0, */
                arg1 = arg1 - arg2 + 1;
                result1 = arg1 / arg2;
                result2 = arg1 % arg2 + arg2 - 1;
            }
            break;
        case OP_EUC_DIV:
            if (arg2 == 0) result1 = 0x7FFFFFFFFFFFFFFF;
            else result1 = (arg1 - arg2 + 1) / arg2;
            break;
        case OP_EUC_MOD:
            if (arg2 == 0) result1 = 0;
            else result1 = (arg1 - arg2 + 1) % arg2 + arg2 - 1;
            break;

        case OP_ILOG:
            result1 = ilog(arg1);
            break;

        case OP_EQ:
            result1 = arg1 == arg2;
            break;
        case OP_NEQ:
            result1 = arg1 != arg2;
            break;
        case OP_LESS:
            result1 = arg1 < arg2;
            break;
        case OP_GREATER:
            result1 = arg1 > arg2;
            break;
        case OP_LEQ:
            result1 = arg1 <= arg2;
            break;
        case OP_GEQ:
            result1 = arg1 >= arg2;
            break;
        case OP_LOGICAL_AND:
            /* Branchless version of && */
            result1 = (arg1 != 0) & (arg2 != 0);
            break;
        case OP_LOGICAL_OR:
            /* Branchless version of || */
            result1 = (arg1 != 0) | (arg2 != 0);
            break;

        case OP_SELECT:
            /* This will probably be branchless, just a CMOV? */
            result1 = arg2;
            if (arg3) result1 = arg1;
            break;

        default:
            /* Should handle this as a more serious exception. */
            result1 = 0x8000000000000000;
            break;
        }

        values[value_count++] = result1;
        if (op == OP_DIVMOD) values[value_count++] = result2;
    }

    return &values[value_count - f->result_count];
}

struct function_builder {
    int arg_count;
    struct instruction *instructions;
    int instruction_count;
    int instruction_cap;
    int intermediates_count;
};

struct function_builder create_function_builder(int arg_count) {
    return (struct function_builder){arg_count};
}

struct function build_function(
    struct function_builder *builder,
    int result_count
) {
    struct function result;

    result.arg_count = builder->arg_count;
    if (builder->instruction_count > 0) {
        result.instructions = realloc(
            builder->instructions,
            builder->instruction_count * sizeof(struct instruction)
        );
    } else {
        result.instructions = 0;
        if (builder->instruction_cap > 0) free(builder->instructions);
    }
    result.instruction_count = builder->instruction_count;
    result.intermediates_count = builder->intermediates_count;
    int value_total = builder->arg_count + builder->intermediates_count;
    if (value_total > 0) result.values = malloc(value_total * sizeof(int64));
    else result.values = NULL;
    if (result_count <= value_total) result.result_count = result_count;
    else result.result_count = value_total;

    builder->instructions = NULL;
    builder->instruction_cap = 0;
    builder->instruction_count = 0;
    builder->intermediates_count = 0;

    return result;
}

void function_builder_push(
    struct function_builder *builder,
    struct instruction *instr
) {
    if (builder->instruction_cap == 0) {
        int new_cap = 8;
        builder->instructions = malloc(new_cap * sizeof(struct instruction));
        builder->instruction_cap = new_cap;
    } else if (builder->instruction_count >= builder->instruction_cap) {
        int new_cap = builder->instruction_cap + 8;
        builder->instructions = realloc(
            builder->instructions,
            new_cap * sizeof(struct instruction)
        );
        builder->instruction_cap = new_cap;
    }

    builder->instructions[builder->instruction_count++] = *instr;
    if (instr->op == OP_DIVMOD) builder->intermediates_count += 2;
    else builder->intermediates_count += 1;
}

void destroy_function_builder(struct function_builder *builder) {
    if (builder->instruction_cap > 0) free(builder->instructions);
    builder->instructions = NULL;
    builder->instruction_cap = 0;
    builder->instruction_count = 0;
    builder->intermediates_count = 0;
}

void destroy_function(struct function *f) {
    if (f->instruction_count > 0) free(f->instructions);
    if (f->values) free(f->values);
    f->instructions = NULL;
    f->instruction_count = 0;
    f->intermediates_count = 0;
    f->values = NULL;
    f->arg_count = 0;
    f->result_count = 0;
}

#endif
