/*******************************************************************************
*                                                                              *
*                         MODULE INITIALIZATION CHAIN                          *
*                                                                              *
* Shared by the AMD64 and LLVM targets on linux. Each Pascal object places the *
* address of its entry in the psystem_llvm_mods section; the linker            *
* concatenates the entries in link order, which is the initialization order pc *
* established, and provides the bounds of the section. main calls the first    *
* entry through psystem_llvm_nextmod; each entry runs its initializer, calls   *
* psystem_llvm_nextmod for the modules after it, and runs its finalizer when   *
* that returns, so that the modules nest as they did when each object fell     *
* through the end of its text into the next.                                   *
*                                                                              *
* The section bounds (__start_ and __stop_ of a section named as a C           *
* identifier) are a GNU ld feature, also provided by gold and lld. They are    *
* weak, so a link without any entry resolves to an empty chain.                *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

extern void (*__start_psystem_llvm_mods[])(void) __attribute__((weak));
extern void (*__stop_psystem_llvm_mods[])(void) __attribute__((weak));

static int modidx = 0; /* next entry to call */

/* call the next module in the initialization chain */
void psystem_llvm_nextmod(void)
{
    void (*f)(void);

    if (modidx == 0 && __start_psystem_llvm_mods == __stop_psystem_llvm_mods) {

        /* nothing registered: the program's objects were built by a generator
           that chained the modules by falling through, before the section.
           Without this the program would exit having run nothing. */
        fprintf(stderr, "*** No module in the initialization chain: "
                        "the objects predate it, rebuild them (pc -r)\n");
        exit(1);

    }
    if (&__start_psystem_llvm_mods[modidx] < __stop_psystem_llvm_mods) {
        f = __start_psystem_llvm_mods[modidx];
        modidx++;
        f();
    }
}
