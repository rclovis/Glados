
#include <stdio.h>

#define OP_END          0
#define OP_INC_DP       1
#define OP_DEC_DP       2
#define OP_INC_VAL      3
#define OP_DEC_VAL      4
#define OP_OUT          5
#define OP_IN           6
#define OP_JMP_FWD      7
#define OP_JMP_BCK      8

#define SUCCESS         0
#define FAILURE         1

#define PROGRAM_SIZE    4096
#define STACK_SIZE      512
#define DATA_SIZE       65535

int main()
{
    unsigned short PROGRAM[PROGRAM_SIZE * 2];
    unsigned short STACK[STACK_SIZE];
    unsigned int SP = 0;

    char *data1 = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.";

    unsigned short pc = 0;
    unsigned short jmp_pc = 0;
    int i = 0;
    while (data1[i] != '\0') {
        if (data1[i] == 62) {
            PROGRAM[pc * 2] = OP_INC_DP;
        }
        if (data1[i] == 60) {
            PROGRAM[pc * 2] = OP_DEC_DP;
        }
        if (data1[i] == 43) {
            PROGRAM[pc * 2] = OP_INC_VAL;
        }
        if (data1[i] == 45) {
            PROGRAM[pc * 2] = OP_DEC_VAL;
        }
        if (data1[i] == 46) {
            PROGRAM[pc * 2] = OP_OUT;
        }
        if (data1[i] == 44) {
            PROGRAM[pc * 2] = OP_IN;
        }
        if (data1[i] == 91) {
            PROGRAM[pc * 2] = OP_JMP_FWD;
            STACK[SP] = pc;
            SP = SP + 1;
        }
        if (data1[i] == 93) {
            SP = SP - 1;
            jmp_pc = STACK[SP];
            PROGRAM[pc * 2] =  OP_JMP_BCK;
            PROGRAM[pc * 2 + 1] = jmp_pc;
            PROGRAM[jmp_pc * 2 + 1] = pc;
        }
        i = i + 1;
        pc = pc + 1;
    }
    PROGRAM[pc * 2] = OP_END;


    pc = 0;
    unsigned short data2[DATA_SIZE];
    unsigned int ptr = DATA_SIZE;
    while (ptr) {
        data2[ptr] = 0;
        ptr = ptr - 1;
    }
    while (PROGRAM[pc * 2] != OP_END && ptr < DATA_SIZE) {
        if (PROGRAM[pc * 2] == OP_INC_DP) {
            ptr = ptr + 1;
        }
        if (PROGRAM[pc * 2] == OP_DEC_DP) {
            ptr = ptr - 1;
        }
        if (PROGRAM[pc * 2] == OP_INC_VAL) {
            data2[ptr] = data2[ptr] + 1;
        }
        if (PROGRAM[pc * 2] == OP_DEC_VAL) {
            data2[ptr] = data2[ptr] - 1;
        }
        if (PROGRAM[pc * 2] == OP_OUT) {
            putchar(data2[ptr]);
        }
        if (PROGRAM[pc * 2] == OP_IN) {
            data2[ptr] = (unsigned int)getchar();
        }
        if (PROGRAM[pc * 2] == OP_JMP_FWD) {
            if(!data2[ptr]) {
                pc = PROGRAM[pc * 2 + 1];
            }
        }
        if (PROGRAM[pc * 2] == OP_JMP_BCK) {
            if(data2[ptr]) {
                pc = PROGRAM[pc * 2 + 1];
            }
        }
        pc = pc + 1;
    }
    return 0;
}