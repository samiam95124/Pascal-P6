/*******************************************************************************
*                                                                              *
*                                 PROGRAM MAIN                                 *
*                                                                              *
* The entry of a Pascal program built by the AMD64 or the LLVM target on       *
* linux. It establishes the master exception frame, runs the module chain      *
* (psystem_mods.c: each module's entry in link order, the program's last), and *
* returns the program's exit status.                                           *
*                                                                              *
*******************************************************************************/

#include "psystem_exc.h"

extern long psystem_errret;
extern void psystem_llvm_nextmod(void);

int main(int argc, char* argv[])
{
    psystem_expframe root;

    if (psystem_setjmp(root.jb)) psystem_master(root.vector);
    psystem_bge(&root);
    psystem_llvm_nextmod();

    return (int)psystem_errret;
}
