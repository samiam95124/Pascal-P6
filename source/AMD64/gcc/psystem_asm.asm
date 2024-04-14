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

        CaseValueNotFound = 16;

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

#
# Constants section
#
modnam:
    .string "<unknown>"
