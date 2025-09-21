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
    .globl  services.times$f_i
    .globl  services.dates$p_vc_i
    .globl  services.dates$f_i
    .globl  services.time$f
    .globl  services.local$f_i
    .globl  services.writetime$p_fc_i
    .global services.writetime$p_i
    .global services.writedate$p_fc_i
    .global services.writedate$p_i
    .global services.clock$f
    .global services.elapsed$f_i
    .global services.validfile$f_vc_i
    .global services.validfile$f_pvc
    .global services.validpath$f_vc_i
    .global services.validpath$f_pvc
    .global services.wild$f_vc_i
    .global services.wild$f_pvc
    .global 
    .global 
    .global 
    .global 
    .global 
    .global 

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
    .if     \numpar >= 7
    popq    %r11
    .if     \numpar >= 8
    popq    %r11
    .endif
    .endif
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

#
# Swap 2 parameters on stack
#
# The System V AMD64 calling convention allows for up to 6 parameters to be in
# registers.Past that, the parameters appear on stack. This wapper uses up to 8
# parameters. For 7 parameters, there is no issue. For 8, the 2 parameters on
# stack will be in the wrong order. This macro swaps those parameters into the
# correct order for the calling convention.
#
    .macro  swaptop
    movq    (%rsp),%r11         # swap the onstack parameters
    movq    8(%rsp),%r12
    movq    16(%rsp),%r13
    movq    24(%rsp),%r14
    movq    %r11,16(%rsp)
    movq    %r12,24(%rsp)
    movq    %r13,(%rsp)
    movq    %r14,8(%rsp)
    .endm

# procedure list(view f: string; var l: filptr); external;

services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    procedure   wrapper_list, 3

# overload procedure list(view f: pstring; var  l: filptr); external;

services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    procedure   wrapper_listp, 3

# procedure times(var s: string; t: integer); external;

services.times$p_vc_i: procedure wrapper_times, 3

# overload function times(t: integer): pstring; external;

services.times$f_i: function wrapper_timesp, 3

# procedure dates(var s: string; t: integer); external;

services.dates$p_vc_i: procedure wrapper_dates, 3

# overload function dates(t: integer): pstring; external;

services.dates$f_i: function wrapper_datesp, 3

# procedure writetime(var f: text; t: integer); external;

services.writetime$p_fc_i: procedure wrapper_writetimef, 2

# overload procedure writetime(t: integer); external;

services.writetime$p_i: procedure writetime, 2

# procedure writedate(var f: text; t: integer); external;

services.writedate$p_fc_i: procedure writedatef, 2

# overload procedure writedate(t: integer); external;

services.writedate$p_i: procedure writedate, 2

# function time: integer; external;

services.time$f: function wrapper_time, 0

# function local(t: integer): integer; external;

services.local$f_i: function wrapper_local, 1

# function clock: integer; external;

services.clock$f: function wrapper_clock, 0

# function elapsed(r: integer): integer; external;

services.elapsed$f_i: function pa_elapsed, 1

# function validfile(view s: string): boolean; external;

services.validfile$f_vc_i: function wrapper_validfile, 2

# overload function validfile(view s: pstring): boolean; external;

services.validfile$f_pvc: procedure wrapper_validfilep, 1

# function validpath(view s: string): boolean; external;

services.validpath$f_vc_i: function wrapper_validpath, 2

# overload function validpath(view s: pstring): boolean; external;

services.validpath$f_pvc: function wrapper_validpathp, 1

??????????????????????????????????????????????????????///

# function wild(view s: string): boolean; external;

services.wild$f_vc_i:
    function pa_wild, 2

# overload function wild(view s: pstring): boolean; external;

services.wild$f_pvc:
    preamble    1, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_wild             # call C routine
    postamble

# procedure getenv(view ls: string; var ds: string); external;

services.getenv$p_vc_vc:
    preamble    0, 4
    pushq   %rdx                # save string
    pushq   %rcx                # save length
    call    pa_getenvl          # call C routine
    pop     %rsi                # restore length
    popq    %rdi                # restore string
    call    cstr2pad            # convert result to padded
    postamble

# overload function getenv(view ls: string): pstring; external;

services.getenv$f_vc:
    preamble    1, 2
    subq    $1024,%rsp          # allocate stack buffer
    movq    %rsp,%rdx           # index that
    movq    $1024,%rcx          # set maximum length
    call    pa_getenvl          # get string in buffer
    movq    %rsp,%rdi           # index buffer
    movq    $1024,%rsi          # set maximum length
    call    cstr2pstr           # convert to pstring
    addq    $1024,%rsp          # deallocate stack buffer
    postamble

# procedure setenv(view sn, sd: string); external;

services.setenv$p_vc_vc:
    procedure pa_setenvl, 4

# overload procedure setenv(sn: pstring; view sd: string); external;

services.setenv$p_pvc_vc:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    setenvl             # call C routine
    postamble

# overload procedure setenv(view sn: string; sd: pstring); external;

services.setenv$p_vc_pvc:
    preamble    0, 3
    movq    (%rdx),%rcx         # move string len to 2nd
    addq    $8,%rdx             # index string data
    call    setenvl             # call C routine
    postamble

# overload procedure setenv(sn, sd: pstring); external;

services.setenv$p_pvc_pvc:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    movq    (%rdx),%rcx         # move string len to 2nd
    addq    $8,%rdx             # index string data
    call    setenvl             # call C routine
    postamble

# procedure allenv(var el: envptr); external;

services.allenv$p_:
    preamble    0, 1
    push    %rdi                # save env*
    call    pa_allenv           # call C function
    call    cenvlist2pascaline  # convert env list
    postamble

# procedure remenv(view sn: string); external;

services.remenv$p_vc:
    procedure   pa_remenvl, 0, 2

# overload procedure remenv(view sn: pstring); external;

services.remenv$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_remenvl             # call C routine
    postamble

# procedure exec(view cmd: string); external;

services.exec$p_vc:
    procedure   pa_execl, 0, 2

# overload procedure exec(cmd: pstring); external;

services.exec$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execl             # call C routine
    postamble

# procedure exece(view cmd: string; el: envptr); external;

services.exece$p_vc:
    preamble    0, 3
    pushq       %rdi            # save cmd string
    pushq       %rsi            # save cmd len
    pushq       %rdx            # save el
    mov         %rdx,%rdi       # set el as 1st par
    call        cenvlist2pascaline # convert env list
    popq        %rdx            # restore el
    popq        %rsi            # restore cmd len
    popq`       %rdi            # restore cmd string
    call        pa_execel       # call C routine
    postamble

# overload procedure exece(cmd: pstring; el: envptr); external;

services.exece$p_pvc:
    preamble    0, 3
    movq    (%rdi),%rsi     # move string len to 2nd
    addq    $8,%rdi         # index string data
    pushq   %rdi            # save cmd string
    pushq   %rsi            # save cmd len
    pushq   %rdx            # save el
    mov     %rdx,%rdi       # set el as 1st par
    call    cenvlist2pascaline # convert env list
    popq    %rdx            # restore el
    popq    %rsi            # restore cmd len
    popq`   %rdi            # restore cmd string
    call    pa_execel       # call C routine
    postamble

# procedure execw(view cmd: string; var e: integer); external;

services.execw$p_vc_i:
    procedure   pa_execwl, 0, 3

# overload procedure execw(cmd: pstring; var e: integer); external;

services.execw$p_pvc_i:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execwl            # call C routine
    postamble

# procedure execew(view cmd: string; el: envptr; var e: integer); external;

services.exece$p_vc_xx_i:
    preamble    0, 4
    pushq       %rdi            # save cmd string
    pushq       %rsi            # save cmd len
    pushq       %rdx            # save el
    pushq       %rcx            # save e
    mov         %rdx,%rdi       # set el as 1st par
    call        cenvlist2pascaline # convert env list
    popq        %rcx            # restore e
    popq        %rdx            # restore el
    popq        %rsi            # restore cmd len
    popq`       %rdi            # restore cmd string
    call        pa_execewl      # call C routine
    postamble

# overload procedure execew(cmd: pstring; el: envptr; var e: integer); external;

services.exece$p_pvc_xx_i:
    preamble    0, 4
    movq        (%rdi),%rsi     # move string len to 2nd
    addq        $8,%rdi         # index string data
    pushq       %rdi            # save cmd string
    pushq       %rsi            # save cmd len
    pushq       %rdx            # save el
    pushq       %rcx            # save e
    mov         %rdx,%rdi       # set el as 1st par
    call        cenvlist2pascaline # convert env list
    popq        %rcx            # restore e
    popq        %rdx            # restore el
    popq        %rsi            # restore cmd len
    popq`       %rdi            # restore cmd string
    call        pa_execewl      # call C routine
    postamble

# procedure getcur(var fn: string); external;

services.getcur$p_vc_i:
    preamble    0, 2
    pushq   %rdi                # save string
    pushq   %rsi                # save length
    call    pa_getcur           # call C function
    popq    %rsi                # restore length
    pop     %rdi                # restore string
    call    cstr2pad            # convert to padded
    postamble

# overload function getcur: pstring; external;

services.getcur$f:
    preamble    1, 1
    movq    %rdi,%rdx           # place t
    subq    $1024,%rsp          # allocate stack buffer
    movq    %rsp,%rdi           # index that
    movq    $1024,%rsi          # set maximum length
    call    pa_getcur           # get time string in buffer
    movq    %rsp,%rdi           # index buffer
    movq    $1024,%rsi          # set maximum length
    call    cstr2pstr           # convert to pstring
    addq    $1024,%rsp          # deallocate stack buffer
    postamble

# procedure setcur(view fn: string); external;

services.setcur$p_vc:
    procedure pa_setcurl, 0, 2

# overload procedure setcur(fn: pstring); external;

services.setcur$f_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_setcurl          # call C routine
    postamble

# procedure brknam(view fn: string; var p, n, e: string); external;

services.brknam$p_vc_vc_vc_vc:
    preamble    0, 6
    swaptop
    call    wrapper_brknam      # execute wrapper
    popq    %r11                # dump 4th parameter
    popq    %r11
    postamble

# overload procedure brknam(view fn: string; var p, n, e: pstring); external;

services.brknam$p_vc_pvc_pvc_pvc:
    procedure wrapper_brknamp, 5

# overload procedure brknam(fn: pstring; var p, n, e: pstring); external;

services.brknam$p_pvc_pvc_pvc_pvc:
    preamble    0, 4
    call    wrapper_brknampp    # execute wrapper
    postamble

# procedure maknam(var fn: string; view p, n, e: string); external;

services.maknam$p_vc_vc_vc_vc:
    preamble    0, 8
    swaptop                     # swap the onstack parameters
    call    pa_maknaml          # call C function
    postamble

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
