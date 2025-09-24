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

# procedure list(view f: pstring; var  l: filptr); external;

services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    procedure   wrapper_listp, 3

# procedure times(var s: string; t: integer); external;

services.times$p_vc_i: procedure wrapper_times, 3

# function times(t: integer): pstring; external;

services.times$f_i: function wrapper_timesp, 3

# procedure dates(var s: string; t: integer); external;

services.dates$p_vc_i: procedure wrapper_dates, 3

# function dates(t: integer): pstring; external;

services.dates$f_i: function wrapper_datesp, 3

# procedure writetime(var f: text; t: integer); external;

services.writetime$p_fc_i: procedure wrapper_writetimef, 2

# procedure writetime(t: integer); external;

services.writetime$p_i: procedure writetime, 2

# procedure writedate(var f: text; t: integer); external;

services.writedate$p_fc_i: procedure writedatef, 2

# procedure writedate(t: integer); external;

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

# function validfile(view s: pstring): boolean; external;

services.validfile$f_pvc: procedure wrapper_validfilep, 1

# function validpath(view s: string): boolean; external;

services.validpath$f_vc_i: function wrapper_validpath, 2

# function validpath(view s: pstring): boolean; external;

services.validpath$f_pvc: function wrapper_validpathp, 1

# function wild(view s: string): boolean; external;

services.wild$_vc: function wrapper_wild, 2

# function wild(view s: pstring): boolean; external;

services.wild$f_pvc: function wrapper_wildp, 1

# procedure getenv(view ls: string; var ds: string); external;

services.getenv$p_vc_vc: procedure wrapper_getenv, 4

# function getenv(view ls: string): pstring; external;

services.getenv$f_vc: function wrapper_getenvp, 2

# procedure setenv(view sn, sd: string); external;

services.setenv$p_vc_vc: procedure wrapper_setenv, 4

# procedure setenv(sn: pstring; view sd: string); external;

services.setenv$p_pvc_vc: procedure wrapper_setenvps, 3

# procedure setenv(view sn: string; sd: pstring); external;

services.setenv$p_vc_pvc: procedure wrapper_setenvsp, 3

# procedure setenv(sn, sd: pstring); external;

services.setenv$p_pvc_pvc: procedure wrapper_setenvpp, 2

# procedure allenv(var el: envptr); external;

services.allenv$p_: procedure wrapper_allenv, 1

# procedure remenv(view sn: string); external;

services.remenv$p_vc: procedure wrapper_remenv, 2

# procedure remenv(view sn: pstring); external;

services.remenv$p_pvc: procedure wrapper_remenvp, 2

# procedure exec(view cmd: string); external;

services.exec$p_vc: procedure wrapper_exec, 2

# procedure exec(cmd: pstring); external;

services.exec$p_pvc: procedure wrapper_execp, 1

# procedure exece(view cmd: string; el: envptr); external;

services.exece$p_vc: procedure wrapper_exece, 3

# procedure exece(cmd: pstring; el: envptr); external;

services.exece$p_pvc: procedure wrapper_execep, 2

# procedure execw(view cmd: string; var e: integer); external;

services.execw$p_vc_i: procedure wrapper_execw, 3

# procedure execw(cmd: pstring; var e: integer); external;

services.execw$p_pvc_i: procedure wrapper_execwp, 2

# procedure execew(view cmd: string; el: envptr; var e: integer); external;

services.execew$p_vc_xx_i: procedure wrapper_execew, 4

# procedure execew(cmd: pstring; el: envptr; var e: integer); external;

services.execew$p_pvc_xx_i: procedure wrapper_execewp, 3

# procedure getcur(var fn: string); external;

services.getcur$p_vc_i: procedure wrapper_getcur, 2

# function getcur: pstring; external;

services.getcur$f: function wrapper_getcurp, 0

# procedure setcur(view fn: string); external;

services.setcur$p_vc: procedure wrapper_setcur, 2

# procedure setcur(fn: pstring); external;

services.setcur$f_pvc: procedure wrapper_setcur, 1

# procedure brknam(view fn: string; var p, n, e: string); external;

services.brknam$p_vc_vc_vc_vc: procedure wrapper_brknam, 8

# procedure brknam(view fn: string; var p, n, e: pstring); external;

services.brknam$p_vc_pvc_pvc_pvc: procedure wrapper_brknamsp, 5

# procedure brknam(fn: pstring; var p, n, e: pstring); external;

services.brknam$p_pvc_pvc_pvc_pvc: procedure wrapper_brknampp, 4

# procedure maknam(var fn: string; view p, n, e: string); external;

services.maknam$p_vc_vc_vc_vc: procedure wrapper_maknam, 8

# function maknam(view p, n, e: string): pstring; external;

services.maknam$p_vc_vc_vc: function wrapper_maknamp, 6

# function maknam(view p: string; view n: string; e: pstring): pstring; external;

services.maknam$p_vc_vc_pvc: function wrapper_maknampsp, 5

# function maknam(view p: string; n: pstring; view e: string): pstring; external;

services.maknam$p_vc_pvc_vc: function wrapper_maknampsp, 5

# function maknam(view p: string; n: pstring; e: pstring): pstring; external;

services.maknam$p_vc_pvc_pvc: function wrapper_maknamspp, 4

# function maknam(p: pstring; view n: string; view e: string): pstring; external;

services.maknam$p_pvc_vc_vc: function wrapper_maknampss, 5

# function maknam(p: pstring; view n: string; e: pstring): pstring; external;

services.maknam$p_pvc_vc_pvc: function wrapper_maknampsp, 4

# function maknam(p: pstring; n: pstring; view e: string): pstring; external;

services.maknam$p_pvc_pvc_vc: function wrapper_maknampps, 4

# function maknam(p: pstring; n: pstring; e: pstring): pstring; external;

services.maknam$p_pvc_pvc_pvc: function wrapper_maknamppp, 3

# procedure fulnam(var fn: string); external;

services.fulnam$p_vc: procedure wrapper_fulnam, 2

# function fulnam(view fn: string): pstring; external;

services.fulnam$p_vc: function wrapper_fulnam, 2

# procedure getpgm(out p: string); external;

services.getpgm$p_vc: function wrapper_getpgm, 2

# function getpgm: pstring; external;

services.getpgm$p_pvc: function wrapper_getpgmp, 1

# procedure getusr(out fn: string); external;

services.getusr$p_vc: function wrapper_getusr, 2

# function getusr: pstring; external;

services.getusr$p_pvc: function wrapper_getusrp, 1

# procedure setatr(view fn: string; a: attrset); external;

services.setatr$p_vc_vi: procedure wrapper_setatr, 3

# procedure setatr(fn: pstring; a: attrset); external;

services.setatr$p_pvc_vi: procedure wrapper_setatrp, 2

# procedure resatr(view fn: string; a: attrset); external;

services.resatr$p_vc_vi: procedure wrapper_resatr, 3

# procedure resatr(fn: pstring; a: attrset); external;

services.resatr$p_pvc_vi: procedure wrapper_resatrp, 2

# procedure bakupd(view fn: string); external;

services.bakupd$p_vc: procedure wrapper_bakupd, 2

# procedure bakupd(fn: pstring); external;

services.bakupd$p_pvc: procedure wrapper_bakupdp, 1

# procedure setuper(view fn: string; p: permset); external;

services.setuper$p_vc_vi: procedure wrapper_setuper, 3

# procedure setuper(fn: pstring; p: permset); external;

services.setuper$p_pvc_vi: procedure wrapper_setuperp, 2

# procedure resuper(view fn: string; p: permset); external;

services.resuper$p_vc_vi: procedure wrapper_resuper, 3

# procedure resuper(fn: pstring; p: permset); external;

services.setuper$p_pvc_vi: procedure wrapper_setuperp, 2

# procedure setgper(view fn: string; p: permset); external;

services.setgper$p_vc_vi: procedure wrapper_setgper, 3

# procedure setgper(fn: pstring; p: permset); external;

services.setgper$p_pvc_vi: procedure wrapper_setgperp, 2

# procedure resgper(view fn: string; p: permset); external;

services.resgper$p_vc_vi: procedure wrapper_resgper, 3

# procedure resgper(fn: pstring; p: permset); external;

services.resgper$p_pvc_vi: procedure wrapper_resgperp, 2

# procedure setoper(view fn: string; p: permset); external;

services.setoper$p_vc_vi: procedure wrapper_setoper, 3

# procedure setoper(fn: pstring; p: permset); external;

services.setoper$p_pvc_vi: procedure wrapper_setoperp, 2

# procedure resoper(view fn: string; p: permset); external;

services.resoper$p_vc_vi: procedure wrapper_resoper, 3

# procedure resoper(fn: pstring; p: permset); external;

services.resoper$p_pvc_vi: procedure wrapper_resoperp, 3

# procedure makpth(view fn: string); external;

services.makpth$p_vc: procedure wrapper_makpth, 2

# procedure makpth(fn: pstring); external;

services.makpth$p_pvc: procedure wrapper_makpthp, 1

# procedure rempth(view fn: string); external;

services.rempth$p_vc: procedure wrapper_rempth, 2

# procedure rempth(fn: pstring); external;

services.rempth$p_pvc: procedure wrapper_rempthp, 1

# procedure filchr(out fc: schar); external;

services.filchr$p_vi: procedure wrapper_filchrp, 1

# function optchr: char; external;

services.optchr$p_c: function wrapper_optchr, 0

# function pthchr: char; external;

services.pthchr$p_c: function wrapper_pthchr, 0

# function latitude: integer; external;

services.latitude$p_i: function wrapper_latitude, 0

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
