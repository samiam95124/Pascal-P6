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

    .globl  services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$
    .globl  services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$
    .globl  services.times$p_vc_i
    .globl  services.dates$p_vc_i
    .globl  services.time$f
    .globl  services.local$f_i
    .globl  services.writetime$p_fc_i
    .global services.writetime$p_i
    .global services.writedate$p_fc_i
    .global services.writedate$p_i
    .global services.clock$f

#
# Procedure/function preamble
#
# Gives the preamble to procedures and functions. Removes the input parameters,
# and any function result. Aligns the stack, then places the original stack
# pointer and the return address on it.
#
    .macro  preamble func, numpar
    popq    %r12                # get return address
    .if      \func
    popq    %r11                # dump function result
    .endif
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
    .endm

#
# Procedure/function postable
#
# Gives the postamble to procedures and functions. The return address is fetched
# from the stack, and the original stack pointer recovered. Then the
# return address is branched to.
#
    .macro  postamble
    popq     %r12               # get return address
    popq    %r11                # get original stack
    movq    %r11,%rsp           # restore original stack
    jmp     *%r12               # return
    .endm
#
# Macro to call a procedure.
#
# Takes the target function and the number of parameters. Note that the number
# of parameters is in terms of registers occupied, and not source parameters.
#
    .macro  procedure func, numpar
    preamble    0, \numpar
    call    \func               # call C function
    postamble
    .endm

#
# Macro to call a function.
#
# Takes the target function and the number of parameters. Note that the number
# of parameters is in terms of registers occupied, and not source parameters.
#
    .macro  function func, numpar
    preamble    1, \numpar
    call    \func               # call C function
    postamble
    .endm

    .text

    jmp     1f      # skip stack sequence

# procedure list(view f: string; var l: filptr); external;

services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    preamble    0, 3
    pushq   %rdx                # save **fl
    call    pa_listl            # call C routine
    popq    %rdi                # restore **fl to 1st
    call    cfilelist2pascaline # convert to Pascaline
    postamble

# overload procedure list(view f: pstring; var  l: filptr); external;

services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    preamble    0, 3
    movq    %rsi,%rdx           # move **fl to 3rd
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    pushq   %rdx                # save **fl
    call    pa_listl            # call C routine
    popq    %rdi                # restore **fl to 1st
    call    cfilelist2pascaline # convert to Pascaline
    postamble

# procedure times(var s: string; t: integer); external;

services.times$p_vc_i:
    preamble    0, 3
    pushq   %rdi
    pushq   %rsi
    call    pa_times
    popq    %rsi
    pop     %rdi
    call    cstr2pad
    postamble

# overload function times(t: integer): pstring; external;

services.times:
    preamble    1, 1
    movq    %rdi,%rdx           # place t
    subq    $1024,%rsp          # allocate stack buffer
    movq    $1000,%rsi          # set maximum length
    call    times$p_vc_i        # get time string in buffer
    leaq    sbuffer(%rip),%rdi  # index buffer
    movq    $1000,%rsi          # set maximum length
    call    cstr2pstr           # convert to pstring
    addq    $1024,$rsp          # deallocate stack buffer
    postamble

# procedure dates(var s: string; t: integer); external;

services.dates$p_vc_i:
    preamble    0, 3
    pushq   %rdi
    pushq   %rsi
    call    pa_dates
    popq    %rsi
    pop     %rdi
    call    cstr2pad
    postamble

# overload function dates(t: integer): pstring; external;

services.dates:
    preamble    1, 1
    movq    %rdi,%rdx           # place t
    subq    $1024,%rsp          # allocate stack buffer
    movq    $1000,%rsi          # set maximum length
    call    dates$p_vc_i        # get time string in buffer
    leaq    sbuffer(%rip),%rdi  # index buffer
    movq    $1000,%rsi          # set maximum length
    call    cstr2pstr           # convert to pstring
    addq    $1024,$rsp          # deallocate stack buffer
    postamble

# procedure writetime(var f: text; t: integer); external;

services.writetime$p_fc_i:
    preamble    0, 2
    pushq   %rsi                # save t
    call    psystem_libcwrfil   # convert Pascaline file to libc
    movq    %rax,%rdi           # place FILE*
    popq    %rsi                # restore t
    call    pa_writetime        # call C function
    postamble

# overload procedure writetime(t: integer); external;

services.writetime$p_i:
    procedure   pa_writetime, 1

# procedure writedate(var f: text; t: integer); external;

services.writedate$p_fc_i:
    preamble    0, 2
    pushq   %rsi                # save t
    call    psystem_libcwrfil   # convert Pascaline file to libc
    movq    %rax,%rdi           # place FILE*
    popq    %rsi                # restore t
    call    pa_writedate        # call C function
    postamble

# overload procedure writedate(t: integer); external;

services.writedate$p_i:
    procedure   pa_writedate, 1

# function time: integer; external;

services.time$f:
    function    pa_time, 0

# function local(t: integer): integer; external;

services.local$f_i:
    function    pa_local, 1

# function clock: integer; external;

services.clock$f:
   function pa_clock, 0

# function elapsed(r: integer): integer; external;
# function validfile(view s: string): boolean; external;
# overload function validfile(view s: pstring): boolean; external;
# function validpath(view s: string): boolean; external;
# overload function validpath(view s: pstring): boolean; external;
# function wild(view s: string): boolean; external;
# overload function wild(view s: pstring): boolean; external;
# procedure getenv(view ls: string; var ds: string); external;
# overload function getenv(view ls: string): pstring; external;
# procedure setenv(view sn, sd: string); external;
# overload procedure setenv(sn: pstring; view sd: string); external;
# overload procedure setenv(view sn: string; sd: pstring); external;
# overload procedure setenv(sn, sd: pstring); external;
# procedure allenv(var el: envptr); external;
# procedure remenv(view sn: string); external;
# overload procedure remenv(view sn: pstring); external;
# procedure exec(view cmd: string); external;
# overload procedure exec(cmd: pstring); external;
# procedure exece(view cmd: string; el: envptr); external;
# overload procedure exece(cmd: pstring; el: envptr); external;
# procedure execw(view cmd: string; var e: integer); external;
# overload procedure execw(cmd: pstring; var e: integer); external;
# procedure execew(view cmd: string; el: envptr; var e: integer); external;
# overload procedure execew(cmd: pstring; el: envptr; var e: integer); external;
# procedure getcur(var fn: string); external;
# overload function getcur: pstring; external;
# procedure setcur(view fn: string); external;
# overload procedure setcur(fn: pstring); external;
# procedure brknam(view fn: string; var p, n, e: string); external;
# overload procedure brknam(view fn: string; var p, n, e: pstring); external;
# overload procedure brknam(fn: pstring; var p, n, e: pstring); external;
# procedure maknam(var fn: string; view p, n, e: string); external;
# overload function maknam(view p, n, e: string): pstring; external;
# overload function maknam(view p: string; view n: string; e: pstring): pstring; external;
# overload function maknam(view p: string; n: pstring; view e: string): pstring; external;
# overload function maknam(view p: string; n: pstring; e: pstring): pstring; external;
# overload function maknam(p: pstring; view n: string; view e: string): pstring; external;
# overload function maknam(p: pstring; view n: string; e: pstring): pstring; external;
# overload function maknam(p: pstring; n: pstring; view e: string): pstring; external;
# overload function maknam(p: pstring; n: pstring; e: pstring): pstring; external;
# procedure fulnam(var fn: string); external;
# overload function fulnam(view fn: string): pstring; external;
# procedure getpgm(var p: string); external;
# overload function getpgm: pstring; external;
# procedure getusr(var fn: string); external;
# overload function getusr: pstring; external;
# procedure setatr(view fn: string; a: attrset); external;
# overload procedure setatr(fn: pstring; a: attrset); external;
# procedure resatr(view fn: string; a: attrset); external;
# overload procedure resatr(fn: pstring; a: attrset); external;
# procedure bakupd(view fn: string); external;
# overload procedure bakupd(fn: pstring); external;
# procedure setuper(view fn: string; p: permset); external;
# overload procedure setuper(fn: pstring; p: permset); external;
# procedure resuper(view fn: string; p: permset); external;
# overload procedure resuper(fn: pstring; p: permset); external;
# procedure setgper(view fn: string; p: permset); external;
# overload procedure setgper(fn: pstring; p: permset); external;
# procedure resgper(view fn: string; p: permset); external;
# overload procedure resgper(fn: pstring; p: permset); external;
# procedure setoper(view fn: string; p: permset); external;
# overload procedure setoper(fn: pstring; p: permset); external;
# procedure resoper(view fn: string; p: permset); external;
# overload procedure resoper(fn: pstring; p: permset); external;
# procedure makpth(view fn: string); external;
# overload procedure makpth(fn: pstring); external;
# procedure rempth(view fn: string); external;
# overload procedure rempth(fn: pstring); external;
# procedure filchr(var fc: schar); external;
# function optchr: char; external;
# function pthchr: char; external;
# function latitude: integer; external;
# function pa_longitude: integer; external;
# function pa_altitude: integer; external;
# function pa_country: integer; external;
# procedure pa_countrys(view s: string; len: integer; c: integer); external;
# function pa_timezone: integer; external;
# function pa_daysave: integer; external;
# function pa_time24hour: integer; external;
# function pa_language: integer; external;
# procedure pa_languages(view s: string; len: integer; l: integer);  external;
# function pa_decimal: char; external;
# function pa_numbersep: char; external;
# function pa_timeorder: integer; external;
# function pa_dateorder: integer; external;
# function pa_datesep: char; external;
# function pa_timesep: char; external;
# function pa_currchr: char; external;

#
# Next module in series
#
1:
