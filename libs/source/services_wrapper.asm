#
# Wrapper series for services module
#
# These wrappers are required for three reasons:
#
# 1. Translate module coining to the final names.
# 2. Perform any parameter translations required.
# 3. Remove input parameters from stack.
# 4. Compensate for differences in function returns.
# 5. Align stack.
#
# The name coining is translation from the format:
#
# <module name>.<routine>$<type signature>
#
# To the C name of the function. The <type signature> is covered in the 
# Pascal-P6 manual under "intermediate language: Symbols".
#
# The #5 requirement is that pgen is cavalier about aligning the stack on calls.
# The AMD64 System V requirement is 16 byte alignment, but this only faults on
# use of vector/128 bit operations. These are used in glibc.
#
# Some or all of these requirements hopefully will be removed in future 
# versions.
#

#
# Macro to call a procedure.
#
# Takes the target function and the number of parameters. Note that the number
# of parameters is in terms of registers occupied, and not source parameters.
#
    .macro  procedure func, numpar
    popq    %r12                # get return address
    .if     \numpar >= 1
    popq    %r11                # dump parameters
    .if     \numpar >= 2
    popq    %r11
    .if     \numpar >= 3
    popq    %r11
    .if     \numpar >= 4
    popq    %r11
    .if     \numpar >= 5
    popq    %r11
    .if     \numpar >= 6
    popq    %r11
    .endif
    .endif
    .endif
    .endif
    .endif
    .endif
    movq    %rsp,%r11           # save original stack
    andq    $0xfffffffffffffff0,%rsp # align stack
    pushq   %r11                # save original stack on stack
    pushq   %r12                # save return address
    call    \func               # call C function
    popq     %r12               # get return address
    popq    %r11                # get original stack
    movq    %r11,%rsp           # restore original stack
    jmp     *%r12               # return
    .endm

    .text

    jmp     1f      # skip stack sequence

    .globl  services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$
    .globl  services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$
    .globl  services.times$p_vc_i
    .globl  services.dates$p_vc_i
    .globl  services.time$f
    .globl  services.local$f_i

# procedure list(view f: string; var l: filptr); external;

services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    popq    %rax                # get return address
    popq    %r11                # dump parameters
    popq    %r11
    popq    %r11
    movq    %rsp,%r11           # save original stack
    andq    $0xfffffffffffffff0,%rsp # align stack
    pushq   %r11                # save original stack on stack
    pushq   %rax                # save return address
    pushq   %rdx                # save **fl
    call    pa_listl            # call C routine
    popq    %rdi                # restore **fl to 1st
    call    cfilelist2pascaline # convert to Pascaline
    popq     %rax               # get return address
    popq    %r11                # get original stack
    movq    %r11,%rsp           # restore original stack
    jmp     *%rax               # return

# overload procedure list(view f: pstring; var  l: filptr); external;

services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    popq    %rax                # get return address
    popq    %r11                # dump parameters
    popq    %r11
    popq    %r11
    movq    %rsp,%r11           # save original stack
    andq    $0xfffffffffffffff0,%rsp # align stack
    pushq   %r11                # save original stack on stack
    pushq   %rax                # save return address
    movq    %rsi,%rdx           # move **fl to 3rd
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    pushq   %rdx                # save **fl
    call    pa_listl            # call C routine
    popq    %rdi                # restore **fl to 1st
    call    cfilelist2pascaline # convert to Pascaline
    popq     %r12               # get return address
    popq    %r11                # get original stack
    movq    %r11,%rsp           # restore original stack
    jmp     *%r12               # return

# procedure times(var s: string; t: integer); external;

services.times$p_vc_i:
    procedure   pa_times, 3

# procedure dates(var s: string; t: integer); external;

services.dates$p_vc_i:
    procedure   pa_dates, 3

# function time: integer; external;

services.time$f:
    popq    %rax                # get return address
    popq    %r11                # dump function return
    movq    %rsp,%r11           # save original stack
    andq    $0xfffffffffffffff0,%rsp # align stack
    pushq   %r11                # save original stack on stack
    pushq   %rax                # save return address
    call    pa_time
    popq     %r12               # get return address
    popq    %r11                # get original stack
    movq    %r11,%rsp           # restore original stack
    jmp     *%r12               # return

# function local(t: integer): integer; external;

services.local$f_i:
    popq    %rax                # get return address
    popq    %r11                # dump function return
    popq    %r11                # dump parameter
    movq    %rsp,%r11           # save original stack
    andq    $0xfffffffffffffff0,%rsp # align stack
    pushq   %r11                # save original stack on stack
    pushq   %rax                # save return address
    call    pa_local
    popq     %r12               # get return address
    popq    %r11                # get original stack
    movq    %r11,%rsp           # restore original stack
    jmp     *%r12               # return

#
# Next module in series
#
1:
