################################################################################
#
# psystem main shim
#
# Provides the main entry point for the psystem module stack. Should be placed
# before the start of all modules.
#
# The main module creates what is called a "master exception" level. Any 
# exception as thrown will "unwind" by going to each exception level in turn,
# checking if that exception is handled, and throwing to the next frame if it is
# not. The master exception catches exceptions if no other exception frame
# catches it. 
#
################################################################################

        MasterException = 121

        .text
#
# Code section
#
        .globl  main
        .type   main, @function
main:
# place master fault handler as exception address
        leaq    main_fault(%rip),%rax
        movq    %rax,psystem_expadr(%rip)
        movq    %rsp,psystem_expstk(%rip)   # set frame parameters                                      
        movq    %rbp,psystem_expmrk(%rip) 
        jmp     3f                          # execute next module in sequence
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
2:
        andq    $0xfffffffffffffff0,%rsp # align stack
        leaq    modnam(%rip),%rdi        # set no module name
        movq    $0,%rsi                  # set no line number
        call    psystem_errorv           # go handler
        jmp     .                        # soft halt
#
# Constants section
#
modnam:
    .string "<unknown>"
#
# Execute next module in sequence
#
3:
        
