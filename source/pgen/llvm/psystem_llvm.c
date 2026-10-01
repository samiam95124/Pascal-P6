/*******************************************************************************
*                                                                              *
*                      LLVM TARGET RUNTIME SUPPORT                             *
*                                                                              *
* The parts of the Pascaline runtime that the AMD64 target keeps in assembly   *
* (psystem.asm, main.asm), for the LLVM IR target, in C:                       *
*                                                                              *
*   - The exception frames. A try block (bge) registers a frame that holds a  *
*     jmp_buf the generated code has set with setjmp; a throw longjmps to the  *
*     innermost frame with the vector, and the handler reads it (sev). The     *
*     frames form a chain; ede pops, mse pops and rethrows to the enclosing    *
*     frame. The outermost frame is the master handler in main.               *
*   - The system exception vectors: system errors that programs may catch are *
*     thrown as the addresses of ExceptionBase[en].                            *
*   - Non-local goto: psystem_goto.c, in psystem.a, shared with the AMD64     *
*     target. A routine that is the target of one carries a table of (label  *
*     number, jmp_buf) in its frame; the goto finds the entry through the     *
*     target frame and longjmps to it.                                        *
*   - main. The module initialization chain it starts (psystem_mods.c, in    *
*     psystem.a) is shared with the AMD64 target: each object places its      *
*     entry in the psystem_llvm_mods section, the linker collects the chain   *
*     in link order, and psystem_llvm_nextmod walks it.                        *
*                                                                              *
*******************************************************************************/

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

#define EXCEPTIONTOP    74   /* last catchable system exception */
#define MASTEREXCEPTION 108  /* unhandled exception */

/* an exception frame: the jmp_buf the generated code fills, the previous
   frame, and the vector thrown to it */
typedef struct expframe {
    jmp_buf jb;
    struct expframe* prev;
    long vector;
} expframe;

static expframe* curexp = NULL; /* innermost frame */

static expframe root;           /* the master frame */

/* the location of the last system error thrown, for the master handler's
   report: the frames carry only the vector */
static const char* errmod = NULL;
static long errline = 0;

/* the vectors of the catchable system exceptions: a throw of system error en
   carries the address of entry en.

   Initialized, so that this is a definition and not a tentative one: a
   compiler that defaults to -fcommon (gcc before 10) makes a tentative
   definition a common symbol, and the linker then searches psystem.a for a definition of ExceptionBase, finds the assembly
   shim's, pulls in psystem_asm.o, and its psystem_thw and psystem_unwind
   collide with the ones here. */
unsigned char ExceptionBase[EXCEPTIONTOP+2] = {0};

extern long psystem_errret;
extern void psystem_errorv(const char* modnam, long line, long en);
/* the initialization chain walker: psystem_mods.c, in psystem.a */
extern void psystem_llvm_nextmod(void);

/* an exception reached the master frame: report and exit */
static void master(long vec)
{
    long en;
    long base = (long)ExceptionBase;

    if (vec >= base && vec <= base+EXCEPTIONTOP) {

        /* a system error nothing caught: report it where it was raised */
        en = vec-base;
        psystem_errorv(errmod ? errmod : "<unknown>", errline, en);

    } else psystem_errorv("<unknown>", 0, MASTEREXCEPTION);
    exit(1);
}

/* begin a try block: register the frame */
void psystem_llvm_bge(expframe* f)
{
    f->prev = curexp;
    f->vector = 0;
    curexp = f;
}

/* end a try block: drop the frame */
void psystem_llvm_ede(void)
{
    if (curexp) curexp = curexp->prev;
}

/* the vector of the current frame */
long psystem_llvm_curvec(void)
{
    return curexp ? curexp->vector : 0;
}

/* throw to the innermost frame */
void psystem_llvm_thw(long vec)
{
    expframe* f = curexp;

    if (!f) master(vec);
    f->vector = vec;
    longjmp(f->jb, 1);
}

/* the throw standard procedure: the vector is the exception variable's
   address */
void psystem_thw(long vec)
{
    psystem_llvm_thw(vec);
}

/* a handler found no match: drop the frame and rethrow to the enclosing one */
void psystem_llvm_mse(long modnam, long line)
{
    long v = curexp ? curexp->vector : 0;

    if (curexp) curexp = curexp->prev;
    psystem_llvm_thw(v);
}

/* a catchable system error, from psystem's error handling */
void psystem_unwind(const char* modnam, int line, int en)
{
    errmod = modnam; errline = line;
    psystem_llvm_thw((long)&ExceptionBase[en]);
}

int main(int argc, char* argv[])
{
    if (setjmp(root.jb)) master(root.vector);
    root.prev = NULL; root.vector = 0;
    curexp = &root;
    psystem_llvm_nextmod();
    return (int)psystem_errret;
}
