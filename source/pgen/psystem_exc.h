/*******************************************************************************
*                                                                              *
*                        EXCEPTION SHIM INTERFACE                              *
*                                                                              *
* Shared by the AMD64 and LLVM targets on linux: the exception frame the       *
* generated code reserves on the stack for a try block, and the shim's entry   *
* points. See psystem_exc.c.                                                   *
*                                                                              *
*******************************************************************************/

#ifndef PSYSTEM_EXC_H
#define PSYSTEM_EXC_H

/* A jump buffer as psystem_setjmp fills it (psystem_jmp.c): the callee
   preserved registers, the stack pointer and the return address of the
   target. 256 bytes on every target, whatever its word size (riscv with D
   needs 208, its doubles sitting past the 14 integer registers at a fixed
   112 on both widths), the size the generators reserve. */
#define PSYSTEM_JMPBYTES 256
#define PSYSTEM_JMPWORDS (PSYSTEM_JMPBYTES/sizeof(long))

/* An exception frame: the jump buffer first, so that the frame's address is
   the buffer's; then the enclosing frame and the vector thrown to it. 272
   bytes, which the generators reserve (16 aligned) at bge. */
typedef struct psystem_expframe {
    long jb[PSYSTEM_JMPWORDS];
    struct psystem_expframe* prev;
    long vector;
} psystem_expframe;

/* the register save and restore (psystem_jmp.c) */
int  psystem_setjmp(void* jb) __attribute__((returns_twice));
void psystem_longjmp(void* jb, long v) __attribute__((noreturn));

/* try block entry and exit, the current vector, throw and rethrow */
void psystem_bge(psystem_expframe* f);
void psystem_ede(void);
long psystem_curvec(void);
void psystem_thw(long vec) __attribute__((noreturn));
void psystem_mse(long modnam, long line) __attribute__((noreturn));

/* a catchable system error (from psystem's error handling) */
void psystem_unwind(const char* modnam, int line, int en) __attribute__((noreturn));

/* the master handler: nothing caught the vector */
void psystem_master(long vec) __attribute__((noreturn));

/* drop the frames of activations below (newer than) a frame base: a
   non-local goto leaves their try blocks without an ede */
void psystem_popto(void* frame);

#endif
