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
        jmp     1f                          # execute next module in sequence
#
# Exception handler
#
main_fault:
        andq    $0xfffffffffffffff0,%rsp # align stack
        leaq    modnam(%rip),%rdi        # set no module name
        movq    $0,%rsi                  # set no line number
        movq    $MasterException,%rdx    # load case fault error
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
1:
        
