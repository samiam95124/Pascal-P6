/*******************************************************************************
*                                                                              *
*                              EXCEPTION SHIM                                  *
*                                                                              *
* Shared by the AMD64 and LLVM targets on linux. A try block (bge) reserves an *
* exception frame on the stack of the routine that owns it, links it as the    *
* innermost frame here, and sets its jump buffer with psystem_setjmp; a throw  *
* longjmps to the innermost frame with the vector, and the handler reads the   *
* vector (sev). ede unlinks the frame; mse, a handler that found no match,     *
* unlinks it and rethrows to the enclosing one. The outermost frame is the     *
* master handler in main.                                                      *
*                                                                              *
* The frames live on the stack, each in the frame of its routine. The one      *
* piece of state outside the stack is the pointer to the innermost frame, and  *
* it is thread local: each thread has its own chain, a throw on a thread finds *
* the try blocks of that thread and no other, and a thread with no try block   *
* reaches the master handler. The generated code never touches this pointer;  *
* it calls here, so this file is the only place that knows the mechanism.     *
*                                                                              *
* The system exception vectors: system errors that programs may catch are     *
* thrown as the addresses of ExceptionBase[en].                                *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "psystem_exc.h"

#define EXCEPTIONTOP    74   /* last catchable system exception */
#define MASTEREXCEPTION 108  /* unhandled exception */

/* the innermost frame of this thread, and where the last system error was
   raised on it, for the master handler's report (the frames carry only the
   vector) */
static _Thread_local psystem_expframe* curexp = NULL;
static _Thread_local const char* errmod = NULL;
static _Thread_local long errline = 0;

/* the vectors of the catchable system exceptions: a throw of system error en
   carries the address of entry en. Initialized, so that this is a definition
   and not a tentative one that -fcommon would merge with another. */
unsigned char ExceptionBase[EXCEPTIONTOP+2] = {0};

extern void psystem_errorv(const char* modnam, long line, long en);

/* an exception reached the master frame: report and exit */
void psystem_master(long vec)
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

/* begin a try block: link the frame as the innermost */
void psystem_bge(psystem_expframe* f)
{
    f->prev = curexp;
    f->vector = 0;
    curexp = f;
}

/* end a try block: unlink the innermost frame */
void psystem_ede(void)
{
    if (curexp) curexp = curexp->prev;
}

/* the vector of the innermost frame */
long psystem_curvec(void)
{
    return curexp ? curexp->vector : 0;
}

/* throw to the innermost frame. The throw standard procedure arrives here
   with the exception variable's address as the vector. */
void psystem_thw(long vec)
{
    psystem_expframe* f = curexp;

    if (!f) psystem_master(vec);
    f->vector = vec;
    psystem_longjmp(f->jb, 1);
}

/* a handler found no match: unlink its frame and rethrow to the enclosing */
void psystem_mse(long modnam, long line)
{
    long v = curexp ? curexp->vector : 0;

    if (curexp) curexp = curexp->prev;
    psystem_thw(v);
}

/* a catchable system error, from psystem's error handling */
void psystem_unwind(const char* modnam, int line, int en)
{
    errmod = modnam; errline = line;
    psystem_thw((long)&ExceptionBase[en]);
}

/* drop the frames below a frame base: the stack grows down, so a frame at a
   lower address belongs to an activation newer than the one at the base, or
   to a try block of that activation that a goto is leaving */
void psystem_popto(void* frame)
{
    while (curexp && (void*)curexp < frame) curexp = curexp->prev;
}
