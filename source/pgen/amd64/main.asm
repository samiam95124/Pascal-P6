################################################################################
#
# psystem main shim
#
# Provides the main entry point for the psystem module stack on windows. The
# linux entry is the C main (source/pgen/main.c), which starts the module
# chain of psystem_mods.c and holds the master exception frame of the
# exception shim (psystem_exc.c); this file keeps the older mechanisms for
# windows: the chain is the fall through, so this object must be placed
# before all the modules and the label at its end runs into the first
# module's entry, and the master exception handler is the one below.
#
# The main module creates what is called a "master exception" level. Any 
# exception as thrown will "unwind" by going to each exception level in turn,
# checking if that exception is handled, and throwing to the next frame if it is
# not. The master exception catches exceptions if no other exception frame
# catches it. 
#
################################################################################

        MasterException = 108

        .text
#
# Code section
#
        .globl  main
# the .type directive is ELF only; the win64 build defines WINDOWS
.ifndef WINDOWS
        .type   main, @function
.endif
main:
# place master fault handler as exception address
        leaq    main_fault(%rip),%rax
        movq    %rax,psystem_expadr(%rip)
        movq    %rsp,psystem_expstk(%rip)   # set frame parameters                                      
        movq    %rbp,psystem_expmrk(%rip) 
.ifdef WINDOWS
        call     3f                          # execute next module in sequence
.else
        subq    $8,%rsp                      # align stack for the C call
        call    psystem_llvm_nextmod         # execute the first module
        addq    $8,%rsp
.endif
        movq    psystem_errret(%rip),%rax    # get program error return code
        ret                                  # exit to operating system
#
# Exception handler
#
main_fault:
        popq    %rdx                     # get vector
        leaq    ExceptionTop(%rip),%rbx
        cmpq    %rbx,%rdx
        ja      1f                       # above
        leaq    ExceptionBase(%rip),%rbx # check in range of our vectors
        cmpq    %rbx,%rdx
        jb      1f                       # below
        subq    %rbx,%rdx
        jmp     2f                       # and go
1:
        movq    $MasterException,%rdx    # load master fault error
        leaq    modnam(%rip),%rdi        # set no module name
        movq    $0,%rsi                  # set no line number
2:
        andq    $0xfffffffffffffff0,%rsp # align stack
        call    psystem_errorv           # go handler
        jmp     .                        # soft halt
#
# Constants section
#
modnam:
    .string "<unknown>"
#
# Execute next module in sequence (windows: the first module follows)
#
3:
        
