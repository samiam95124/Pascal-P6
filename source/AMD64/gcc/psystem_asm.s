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
################################################################################

        .globl  psystem_caseerror
        .type   psystem_caseerror, @function
psystem_caseerror:
        andq    $0xfffffffffffffff0,%rsp # align stack
        movq    $CaseValueNotFound,%rdi  # load case fault error
        call    psystem_errore           # go handler
        jmp     .                        # soft halt
