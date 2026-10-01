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
.ifndef WINDOWS
        .type   psystem_caseerror, @function
.endif
psystem_caseerror:
.ifdef WINDOWS
        andq    $0xfffffffffffffff0,%rsp # align stack
        leaq    modnam(%rip),%rcx        # set no module name
        movq    $0,%rdx                  # set no line number
        leaq    CaseValueNotFound(%rip),%r8 # load case fault error
        subq    $32,%rsp                 # allocate shadow space
        call    psystem_errore           # go handler
.else
        andq    $0xfffffffffffffff0,%rsp # align stack
        leaq    modnam(%rip),%rdi        # set no module name
        movq    $0,%rsi                  # set no line number
        movq    $2,%rdx                  # load case fault error (CaseValueNotFound)
        call    psystem_errore           # go handler
.endif
        jmp     .                        # soft halt

################################################################################
#
# Register save and restore for the exception frames and the non-local goto
# targets (linux). psystem_setjmp(buf) saves the callee preserved registers,
# the stack pointer as it will be after the return, and the return address,
# eight words, and returns 0; psystem_longjmp(buf, v) restores them and
# returns from that same call with v. The generated code keeps nothing in a
# register across a statement, so nothing else needs saving, and no C library
# jump buffer or unwinder is involved.
#
# int  psystem_setjmp(void* buf[rdi]);
# void psystem_longjmp(void* buf[rdi], long v[rsi]);
#
################################################################################

.ifndef WINDOWS
        .globl  psystem_setjmp
        .type   psystem_setjmp, @function
psystem_setjmp:
        movq    %rbx,0(%rdi)
        movq    %rbp,8(%rdi)
        movq    %r12,16(%rdi)
        movq    %r13,24(%rdi)
        movq    %r14,32(%rdi)
        movq    %r15,40(%rdi)
        leaq    8(%rsp),%rax             # the stack pointer after our return
        movq    %rax,48(%rdi)
        movq    (%rsp),%rax              # the return address
        movq    %rax,56(%rdi)
        xorl    %eax,%eax
        ret

        .globl  psystem_longjmp
        .type   psystem_longjmp, @function
psystem_longjmp:
        movq    %rsi,%rax                # the value setjmp returns this time
        testq   %rax,%rax
        jnz     1f
        movl    $1,%eax                  # never 0: that was the first return
1:
        movq    0(%rdi),%rbx
        movq    8(%rdi),%rbp
        movq    16(%rdi),%r12
        movq    24(%rdi),%r13
        movq    32(%rdi),%r14
        movq    40(%rdi),%r15
        movq    48(%rdi),%rsp
        jmp     *56(%rdi)
.endif

.ifdef WINDOWS
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
.ifndef WINDOWS
        .type   psystem_thw, @function
.endif
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
.ifndef WINDOWS
        .type   psystem_unwind, @function
.endif
psystem_unwind:
        leaq    ExceptionBase(%rip),%rax  # get exception base address
        addq    %rax,%rdx                 # offset
        movq    psystem_expmrk(%rip),%rbp # frame pointer
        movq    psystem_expstk(%rip),%rsp # stack
        pushq   %rdx                      # establish new vector
        jmp     *psystem_expadr(%rip)     # go exception handler

.endif

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

# On linux the vectors are defined by the exception shim (psystem_exc.c).
.ifdef WINDOWS
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
.endif
