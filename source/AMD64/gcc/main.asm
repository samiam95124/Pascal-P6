################################################################################
#
# psystem main shim
#
# Provides the main entry point for the psystem module stack. Should be placed
# before the start of all modules.
#
################################################################################

        .text
#
# Code section
#
        .globl  main
        .type   main, @function
main:
