/*******************************************************************************
*                                                                              *
*                        REGISTER SAVE AND RESTORE                             *
*                                                                              *
* psystem_setjmp(buf) saves the callee preserved registers, the stack pointer  *
* as it will be after the return, and the return address, and returns 0;       *
* psystem_longjmp(buf, v) restores them and returns from that same call with   *
* v (1 if v is 0). The exception frames and the non-local goto targets of both *
* code generators use them. The generated code keeps nothing in a register    *
* across a statement, so the callee preserved set is all a handler needs, and *
* no C library jump buffer or unwinder is involved, which keeps the buffer     *
* layout and size (PSYSTEM_JMPWORDS, psystem_exc.h) the same on every target.  *
*                                                                              *
* One implementation per target, selected by the compiler's target macros, so  *
* that one file serves every machine the LLVM target is built for.            *
*                                                                              *
*******************************************************************************/

#include "psystem_exc.h"

#if defined(__x86_64__)

/* rbx rbp r12 r13 r14 r15 rsp rip: 8 words */
__asm__ (
"    .text\n"
"    .globl psystem_setjmp\n"
"    .type  psystem_setjmp, @function\n"
"psystem_setjmp:\n"
"    movq %rbx,0(%rdi)\n"
"    movq %rbp,8(%rdi)\n"
"    movq %r12,16(%rdi)\n"
"    movq %r13,24(%rdi)\n"
"    movq %r14,32(%rdi)\n"
"    movq %r15,40(%rdi)\n"
"    leaq 8(%rsp),%rax\n"
"    movq %rax,48(%rdi)\n"
"    movq (%rsp),%rax\n"
"    movq %rax,56(%rdi)\n"
"    xorl %eax,%eax\n"
"    ret\n"
"    .globl psystem_longjmp\n"
"    .type  psystem_longjmp, @function\n"
"psystem_longjmp:\n"
"    movq %rsi,%rax\n"
"    testq %rax,%rax\n"
"    jnz 1f\n"
"    movl $1,%eax\n"
"1:\n"
"    movq 0(%rdi),%rbx\n"
"    movq 8(%rdi),%rbp\n"
"    movq 16(%rdi),%r12\n"
"    movq 24(%rdi),%r13\n"
"    movq 32(%rdi),%r14\n"
"    movq 40(%rdi),%r15\n"
"    movq 48(%rdi),%rsp\n"
"    jmp *56(%rdi)\n"
);

#elif defined(__i386__)

/* ebx esi edi ebp esp eip: 6 words. The buffer address is the first stack
   argument, the value the second. */
__asm__ (
"    .text\n"
"    .globl psystem_setjmp\n"
"    .type  psystem_setjmp, @function\n"
"psystem_setjmp:\n"
"    movl 4(%esp),%eax\n"
"    movl %ebx,0(%eax)\n"
"    movl %esi,4(%eax)\n"
"    movl %edi,8(%eax)\n"
"    movl %ebp,12(%eax)\n"
"    leal 4(%esp),%ecx\n"
"    movl %ecx,16(%eax)\n"
"    movl (%esp),%ecx\n"
"    movl %ecx,20(%eax)\n"
"    xorl %eax,%eax\n"
"    ret\n"
"    .globl psystem_longjmp\n"
"    .type  psystem_longjmp, @function\n"
"psystem_longjmp:\n"
"    movl 4(%esp),%edx\n"
"    movl 8(%esp),%eax\n"
"    testl %eax,%eax\n"
"    jnz 1f\n"
"    movl $1,%eax\n"
"1:\n"
"    movl 0(%edx),%ebx\n"
"    movl 4(%edx),%esi\n"
"    movl 8(%edx),%edi\n"
"    movl 12(%edx),%ebp\n"
"    movl 16(%edx),%esp\n"
"    jmp *20(%edx)\n"
);

#elif defined(__aarch64__)

/* x19-x28, x29 (fp), x30 (lr), sp, d8-d15: 21 words */
__asm__ (
"    .text\n"
"    .globl psystem_setjmp\n"
"    .type  psystem_setjmp, %function\n"
"psystem_setjmp:\n"
"    stp x19, x20, [x0, #0]\n"
"    stp x21, x22, [x0, #16]\n"
"    stp x23, x24, [x0, #32]\n"
"    stp x25, x26, [x0, #48]\n"
"    stp x27, x28, [x0, #64]\n"
"    stp x29, x30, [x0, #80]\n"
"    mov x1, sp\n"
"    str x1, [x0, #96]\n"
"    stp d8, d9, [x0, #104]\n"
"    stp d10, d11, [x0, #120]\n"
"    stp d12, d13, [x0, #136]\n"
"    stp d14, d15, [x0, #152]\n"
"    mov w0, #0\n"
"    ret\n"
"    .globl psystem_longjmp\n"
"    .type  psystem_longjmp, %function\n"
"psystem_longjmp:\n"
"    ldp x19, x20, [x0, #0]\n"
"    ldp x21, x22, [x0, #16]\n"
"    ldp x23, x24, [x0, #32]\n"
"    ldp x25, x26, [x0, #48]\n"
"    ldp x27, x28, [x0, #64]\n"
"    ldp x29, x30, [x0, #80]\n"
"    ldr x2, [x0, #96]\n"
"    mov sp, x2\n"
"    ldp d8, d9, [x0, #104]\n"
"    ldp d10, d11, [x0, #120]\n"
"    ldp d12, d13, [x0, #136]\n"
"    ldp d14, d15, [x0, #152]\n"
"    cmp x1, #0\n"
"    csinc x0, x1, xzr, ne\n"
"    ret\n"
);

#elif defined(__arm__)

/* r4-r11, sp, lr, d8-d15: 10 words and 8 doubles */
__asm__ (
"    .text\n"
"    .syntax unified\n"
"    .globl psystem_setjmp\n"
"    .type  psystem_setjmp, %function\n"
"psystem_setjmp:\n"
"    stmia r0!, {r4, r5, r6, r7, r8, r9, r10, r11}\n"
"    str sp, [r0], #4\n"
"    str lr, [r0], #4\n"
"    vstmia r0, {d8-d15}\n"
"    mov r0, #0\n"
"    bx lr\n"
"    .globl psystem_longjmp\n"
"    .type  psystem_longjmp, %function\n"
"psystem_longjmp:\n"
"    ldmia r0!, {r4, r5, r6, r7, r8, r9, r10, r11}\n"
"    ldr sp, [r0], #4\n"
"    ldr lr, [r0], #4\n"
"    vldmia r0, {d8-d15}\n"
"    movs r0, r1\n"
"    it eq\n"
"    moveq r0, #1\n"
"    bx lr\n"
);

#elif defined(__riscv)

/* ra, sp, s0-s11, fs0-fs11: 14 registers and 12 doubles. The register store
   and load are word sized (xlen), the floating point ones double. */
#if __riscv_xlen == 64
#define SR "sd"
#define LR "ld"
#define XL "8"
#else
#define SR "sw"
#define LR "lw"
#define XL "4"
#endif
__asm__ (
"    .text\n"
"    .globl psystem_setjmp\n"
"    .type  psystem_setjmp, @function\n"
"psystem_setjmp:\n"
"    " SR " ra, 0*" XL "(a0)\n"
"    " SR " sp, 1*" XL "(a0)\n"
"    " SR " s0, 2*" XL "(a0)\n"
"    " SR " s1, 3*" XL "(a0)\n"
"    " SR " s2, 4*" XL "(a0)\n"
"    " SR " s3, 5*" XL "(a0)\n"
"    " SR " s4, 6*" XL "(a0)\n"
"    " SR " s5, 7*" XL "(a0)\n"
"    " SR " s6, 8*" XL "(a0)\n"
"    " SR " s7, 9*" XL "(a0)\n"
"    " SR " s8, 10*" XL "(a0)\n"
"    " SR " s9, 11*" XL "(a0)\n"
"    " SR " s10, 12*" XL "(a0)\n"
"    " SR " s11, 13*" XL "(a0)\n"
#if defined(__riscv_flen) && __riscv_flen >= 64
"    fsd fs0, 112(a0)\n"
"    fsd fs1, 120(a0)\n"
"    fsd fs2, 128(a0)\n"
"    fsd fs3, 136(a0)\n"
"    fsd fs4, 144(a0)\n"
"    fsd fs5, 152(a0)\n"
"    fsd fs6, 160(a0)\n"
"    fsd fs7, 168(a0)\n"
"    fsd fs8, 176(a0)\n"
"    fsd fs9, 184(a0)\n"
"    fsd fs10, 192(a0)\n"
"    fsd fs11, 200(a0)\n"
#endif
"    li a0, 0\n"
"    ret\n"
"    .globl psystem_longjmp\n"
"    .type  psystem_longjmp, @function\n"
"psystem_longjmp:\n"
"    " LR " ra, 0*" XL "(a0)\n"
"    " LR " sp, 1*" XL "(a0)\n"
"    " LR " s0, 2*" XL "(a0)\n"
"    " LR " s1, 3*" XL "(a0)\n"
"    " LR " s2, 4*" XL "(a0)\n"
"    " LR " s3, 5*" XL "(a0)\n"
"    " LR " s4, 6*" XL "(a0)\n"
"    " LR " s5, 7*" XL "(a0)\n"
"    " LR " s6, 8*" XL "(a0)\n"
"    " LR " s7, 9*" XL "(a0)\n"
"    " LR " s8, 10*" XL "(a0)\n"
"    " LR " s9, 11*" XL "(a0)\n"
"    " LR " s10, 12*" XL "(a0)\n"
"    " LR " s11, 13*" XL "(a0)\n"
#if defined(__riscv_flen) && __riscv_flen >= 64
"    fld fs0, 112(a0)\n"
"    fld fs1, 120(a0)\n"
"    fld fs2, 128(a0)\n"
"    fld fs3, 136(a0)\n"
"    fld fs4, 144(a0)\n"
"    fld fs5, 152(a0)\n"
"    fld fs6, 160(a0)\n"
"    fld fs7, 168(a0)\n"
"    fld fs8, 176(a0)\n"
"    fld fs9, 184(a0)\n"
"    fld fs10, 192(a0)\n"
"    fld fs11, 200(a0)\n"
#endif
"    seqz a0, a1\n"
"    add a0, a0, a1\n"
"    ret\n"
);

#else
#error "psystem_jmp.c: no register save and restore for this target"
#endif
