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
# psystem_base - Finds the given frame by climbing the frame levels.
#
################################################################################

        marksl = -8  # static link
        markdl = -16 # dynamic link
        markep = -24 # (old) maximum frame size
        marksb = -32 # stack bottom
        market = -40 # current ep
        markra = -48 # return address

        .text
#
# Code section
#

################################################################################
#
# Find base address
#
# Climbs the stack frames until the given frame is found. Expects the number of
# frames to climb in rdi. Returns the correct frame address in rax.
#
################################################################################

        .globl  psystem_base
        .type   psystem_base, @function
psystem_base:
        movq    %rbp,%rax           # get current frame pointer
psystem_base01:
        orq     %rdi,%rdi           # check zero
        jz      psystem_base02      # exit if so
        decq    %rdi                # count off frames
        movq    marksl(%rax),%rax   # link to next frame up
        jmp     psystem_base01
psystem_base02:
        ret
