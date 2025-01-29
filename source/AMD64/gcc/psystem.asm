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
        leaq    CaseValueNotFound(%rip),%rdx # load case fault error
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
# Unwwind exception
#
# Translate error code to exception vector and throw that. Used to send 
# catchable system exceptions to be caught as user exceptions.
#
# Note we pass the module name and line number, but the exception handlers don't
# do anything with this information. To use this, we would need to have user 
# throws also set the module name and line, which is possible.
#
# C callable as:
#
# void psystem_unwind(const char* modnam[rdi], int line[rsi], int en[rdx]);
#
################################################################################

        .globl  psystem_unwind
        .type   psystem_unwind, @function
psystem_unwind:
        leaq    ExceptionBase(%rip),%rax  # get exception base address
        addq    %rax,%rdx                 # offset
        movq    psystem_expmrk(%rip),%rbp # frame pointer
        movq    psystem_expstk(%rip),%rsp # stack
        pushq   %rdx                      # establish new vector
        jmp     *psystem_expadr(%rip)     # go exception handler

#
# Constants section
#
modnam:
    .string "<unknown>"

################################################################################
#
# Exceptions addresses
#
# Assigns addresses to each system exception. Note that only the address is 
# used, and the contents is not relivant. The names of the exceptions are for
# reference only.
#
################################################################################

    .bss

    .global ExceptionBase
ExceptionBase:

ValueOutOfRange:                    .byte 0
ArrayLengthMatch:                   .byte 0
CaseValueNotFound:                  .byte 0
ZeroDivide:                         .byte 0
InvalidOperand:                     .byte 0
NilPointerDereference:              .byte 0
RealOverflow:                       .byte 0
RealUnderflow:                      .byte 0
RealProcessingFault:                .byte 0
TagValueNotActive:                  .byte 0
TooManyFiles:                       .byte 0
FileIsOpen:                         .byte 0
FileAlreadyNamed:                   .byte 0
FileNotOpen:                        .byte 0
FileModeIncorrect:                  .byte 0
InvalidFieldSpecification:          .byte 0
InvalidRealNumber:                  .byte 0
InvalidFractionSpecification:       .byte 0
InvalidIntegerFormat:               .byte 0
IntegerValueOverflow:               .byte 0
InvalidRealFormat:                  .byte 0
EndOfFile:                          .byte 0
InvalidFilePosition:                .byte 0
FilenameTooLong:                    .byte 0
FileOpenFail:                       .byte 0
FileSIzeFail:                       .byte 0
FileCloseFail:                      .byte 0
FileReadFail:                       .byte 0
FileWriteFail:                      .byte 0
FilePositionFail:                   .byte 0
FileDeleteFail:                     .byte 0
FileNameChangeFail:                 .byte 0
SpaceAllocateFail:                  .byte 0
SpaceReleaseFail:                   .byte 0
SpaceAllocateNegative:              .byte 0
CannotPerformSpecial:               .byte 0
CommandLineTooLong:                 .byte 0
ReadPastEOF:                        .byte 0
FileTransferLengthZero:             .byte 0
FileSizeTooLarge:                   .byte 0
FilenameEmpty:                      .byte 0
CannotOpenStandard:                 .byte 0
TooManyTemporaryFiles:              .byte 0
InputBufferOverflow:                .byte 0
TooManyThreads:                     .byte 0
CannotStartThread:                  .byte 0
InvalidThreadHandle:                .byte 0
CannotStopThread:                   .byte 0
TooManyIntertaskLocks:              .byte 0
InvalidLockHandle:                  .byte 0
LockSequenceFail:                   .byte 0
TooManySignals:                     .byte 0
CannotCreateSignal:                 .byte 0
InvalidSignalHandle:                .byte 0
CannotDeleteSignal:                 .byte 0
CannotSendSignal:                   .byte 0
WaitForSignalFail:                  .byte 0
FieldNotBlank:                      .byte 0
ReadOnWriteOnlyFile:                .byte 0
WriteOnReadOnlyFile:                .byte 0
FileBufferVariableUndefined:        .byte 0
NondecimalRadixOfNegative:          .byte 0
InvalidArgumentToLn:                .byte 0
InvalidArgumentToSqrt:              .byte 0
CannotResetOrRewriteStandardFile:   .byte 0
CannotResetWriteOnlyFile:           .byte 0
CannotRewriteReadOnlyFile:          .byte 0
SetElementOutOfRange:               .byte 0
RealArgumentTooLarge:               .byte 0
BooleanOperatorOfNegative:          .byte 0
InvalidDivisorToMod:                .byte 0
PackElementsOutOfBounds:            .byte 0
UnpackElementsOutOfBounds:          .byte 0
CannotResetClosedTempFile:          .byte 0
ReadCharacterMismatch:              .byte 0

    .global ExceptionTop
ExceptionTop:
