################################################################################
#
# psystem assembly support functions.
#
# This is the assembly language companion to psystem.c. These are functions that
# are difficult or impossible to implement in c. Note we use the filename
# ending "_asm" to prevent this file from being accidently overwritten.
#
# Functions:
#
# psystem_caserror - Processes a case not found error.
#
################################################################################

        CaseValueNotFound = 16

        .text
#
# Code section
#

################################################################################
#
# Throw case not found error
#
# Simpy sends a case not found error back to psystem. The case table jump in the
# code must be a fixed length to make the table math work out, so this serves
# as an intermediate to call the formal error function.
#
# At present, there is no way to know the module and line number of the
# originating module.
#
################################################################################

        .globl  psystem_caseerror
        .type   psystem_caseerror, @function
psystem_caseerror:
        andq    $0xfffffffffffffff0,%rsp # align stack
        leaq    modnam(%rip),%rdi        # set no module name
        movq    $0,%rsi                  # set no line number
        movq    $CaseValueNotFound,%rdx  # load case fault error
        call    psystem_errore           # go handler
        jmp     .                        # soft halt

################################################################################
#
# Throw exception
#
# Expects an exception variable address in rdi. The stack is cut by loading the
# parameters of the current top exception frame, then that frame is executed
# with the exception variable. The result is that the exception works its way
# through the chain of handlers unti the bottom exception, which is the master
# handler. The exception variable is only used for its address.
#
################################################################################

        .globl  psystem_thw
        .type   psystem_thw, @function
psystem_thw:
#
# restore exception frame
#
        movq    psystem_expmrk(%rip),%rbp # frame pointer
        movq    psystem_expstk(%rip),%rsp # stack
        popq    %rax                      # dump exception vector
        pushq   %rdi                      # establish new vector
        jmp     *psystem_expadr(%rip)     # go exception handler

################################################################################
#
# Exception vector table
#
# C cannot directly reference Pascaline dotted names. This set of equates gives
# C code the exception vector addresses. We expose only the base and top of
# exceptions. Each exception address is a one byte allocation, meaning that the
# vector address can be found by base+(exception number-exeption base).
#
################################################################################

        psystem_exceptionbase = exception.ValueOutOfRange
        psystem_privexceptiontop = exception.MasterException
#
# Constants section
#
modnam:
    .string "<unknown>"
