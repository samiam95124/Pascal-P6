////////////////////////////////////////////////////////////////////////////////
//
// psystem main shim (aarch64)
//
// Provides the main entry point for the psystem module stack. Should be placed
// before the start of all modules.
//
// The main module creates what is called a "master exception" level. Any
// exception as thrown will "unwind" by going to each exception level in turn,
// checking if that exception is handled, and throwing to the next frame if it
// is not. The master exception catches exceptions if no other exception frame
// catches it.
//
// The exception vector rides in a 16 byte stack slot: psystem_expstk points at
// the slot, a throw replaces its contents and branches to the handler, and the
// handler pops it. The generated code and the psystem.asm companions must
// follow the same protocol.
//
////////////////////////////////////////////////////////////////////////////////

        MasterException = 108

        .text
//
// Code section
//
        .globl  main
        .type   main, %function
main:
        stp     x29, x30, [sp, #-16]!       // save frame and link
        mov     x29, sp
// place master fault handler as exception address
        adrp    x0, main_fault
        add     x0, x0, :lo12:main_fault
        adrp    x1, psystem_expadr
        str     x0, [x1, :lo12:psystem_expadr]
        mov     x0, sp                      // set frame parameters
        adrp    x1, psystem_expstk
        str     x0, [x1, :lo12:psystem_expstk]
        adrp    x1, psystem_expmrk
        str     x29, [x1, :lo12:psystem_expmrk]
        bl      3f                          // execute next module in sequence
        adrp    x1, psystem_errret
        ldr     x0, [x1, :lo12:psystem_errret] // get program error return code
        ldp     x29, x30, [sp], #16         // restore frame and link
        ret                                 // exit to operating system
//
// Exception handler
//
main_fault:
        ldr     x2, [sp], #16               // get vector
        adrp    x3, ExceptionTop
        add     x3, x3, :lo12:ExceptionTop
        cmp     x2, x3                      // check above our vectors
        b.hi    1f
        adrp    x3, ExceptionBase
        add     x3, x3, :lo12:ExceptionBase
        cmp     x2, x3                      // check below our vectors
        b.lo    1f
        sub     x2, x2, x3                  // find vector index
        b       2f
1:
        mov     x2, #MasterException        // load master fault error
        adrp    x0, modnam
        add     x0, x0, :lo12:modnam        // set no module name
        mov     x1, #0                      // set no line number
2:
        mov     x16, sp                     // align stack
        and     x16, x16, #0xfffffffffffffff0
        mov     sp, x16
        bl      psystem_errorv              // go handler
        b       .                           // soft halt
//
// Constants section
//
modnam:
        .string "<unknown>"
//
// Execute next module in sequence
//
        .balign 4
3:
