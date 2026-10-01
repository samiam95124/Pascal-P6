/*******************************************************************************
*                                                                              *
*                              NON-LOCAL GOTO                                  *
*                                                                              *
* Shared by the AMD64 and LLVM targets on linux. A routine that owns a label   *
* a nested routine jumps to keeps a table in its frame, pointed to by the      *
* frame's second word: a count, then (label key, jump buffer address) pairs,   *
* each buffer set by psystem_setjmp in the routine's prologue. The goto finds  *
* the owner's activation through its display, drops the exception frames of   *
* the activations it leaves, and longjmps to the entry for the label.          *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "psystem_exc.h"

/* non-local goto to label key of the routine owning frame */
void psystem_llvm_ipj(unsigned char* frame, long key)
{
    long* t = *(long**)(frame+8);
    long n, k;

    if (t) {
        n = t[0];
        for (k = 0; k < n; k++)
            if (t[1+2*k] == key) {
                psystem_popto(frame);
                psystem_longjmp((void*)t[2+2*k], 1);
            }
    }
    fprintf(stderr, "*** Non-local goto target not found\n");
    exit(1);
}
