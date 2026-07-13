//
// Wrapper series for services module (aarch64)
//
// The aarch64 twin of services_wrapper.asm. With arm64_sysv, the Pascaline
// calling convention matches the AAPCS64 ABI: parameters are passed in
// registers (x0-x7/v0-v7), the stack is aligned, and function results return
// in x0/d0. The only remaining task for this wrapper is name coining:
// translating from the Pascaline mangled symbol format to the C name of the
// function. Each entry is a simple b (tail call).
//
    .globl  services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$
    .globl  services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$
    .globl  services.times$p_vc_i
    .globl  services.times$f_i
    .globl  services.dates$p_vc_i
    .globl  services.dates$f_i
    .globl  services.writetime$p_fc_i
    .global services.writetime$p_i
    .global services.writedate$p_fc_i
    .global services.writedate$p_i
    .global services.validfile$f_vc
    .global services.validfile$f_pvc
    .global services.validpath$f_vc
    .global services.validpath$f_pvc
    .global services.wild$f_vc
    .global services.wild$f_pvc
    .global services.getenv$p_vc_vc
    .global services.getenv$f_vc
    .global services.setenv$p_vc_vc
    .global services.setenv$p_pvc_vc
    .global services.setenv$p_vc_pvc
    .global services.setenv$p_pvc_pvc
    .global services.allenv$p_pr$name$0$pvc$data$8$p12$next$16$p2$
    .global services.remenv$p_vc
    .global services.remenv$p_pvc
    .global services.exec$p_vc
    .global services.exec$p_pvc
    .global services.exece$p_vc_pr$name$0$pvc$data$8$p12$next$16$p2$
    .global services.exece$p_pvc_pr$name$0$pvc$data$8$p12$next$16$p2$
    .global services.execw$p_vc_i
    .global services.execw$p_pvc_i
    .global services.execew$p_vc_pr$name$0$pvc$data$8$p12$next$16$p2$_i
    .global services.execew$p_pvc_pr$name$0$pvc$data$8$p12$next$16$p2$_i
    .global services.getcur$p_vc
    .global services.getcur$f
    .global services.setcur$p_vc
    .global services.setcur$f_pvc
    .global services.brknam$p_vc_vc_vc_vc
    .global services.brknam$p_vc_pvc_pvc_pvc
    .global services.brknam$p_pvc_pvc_pvc_pvc
    .global services.maknam$p_vc_vc_vc_vc
    .global services.maknam$f_vc_vc_vc
    .global services.maknam$f_vc_vc_pvc
    .global services.maknam$f_vc_pvc_vc
    .global services.maknam$f_vc_pvc_pvc
    .global services.maknam$f_pvc_vc_vc
    .global services.maknam$f_pvc_vc_pvc
    .global services.maknam$f_pvc_pvc_vc
    .global services.maknam$f_pvc_pvc_pvc
    .global services.fulnam$p_vc
    .global services.fulnam$f_vc
    .global services.getpgm$p_vc
    .global services.getpgm$f
    .global services.getusr$p_vc
    .global services.getusr$f
    .global services.setatr$p_vc_sx$atexec$atarc$atsys$atdir$atloop$
    .global services.setatr$p_pvc_sx$atexec$atarc$atsys$atdir$atloop$
    .global services.resatr$p_vc_sx$atexec$atarc$atsys$atdir$atloop$
    .global services.resatr$p_pvc_sx$atexec$atarc$atsys$atdir$atloop$
    .global services.bakupd$p_vc
    .global services.bakupd$p_pvc
    .global services.setuper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.setuper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.resuper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.resuper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.setgper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.setgper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.resgper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.resgper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.setoper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.setoper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.resoper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.resoper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$
    .global services.makpth$p_vc
    .global services.makpth$p_pvc
    .global services.rempth$p_vc
    .global services.rempth$p_pvc
    .global services.countrys$p_vc_i
    .global services.languages$p_vc_i
    .global services.time$f
    .global services.local$f_i
    .global services.clock$f
    .global services.elapsed$f_i
    .global services.filchr$p_sc
    .global services.optchr$f
    .global services.pthchr$f
    .global services.latitude$f
    .global services.longitude$f
    .global services.altitude$f
    .global services.country$f
    .global services.timezone$f
    .global services.daysave$f
    .global services.time24hour$f
    .global services.language$f
    .global services.decimal$f
    .global services.numbersep$f
    .global services.timeorder$f
    .global services.dateorder$f
    .global services.datesep$f
    .global services.timesep$f
    .global services.currchr$f

    .text

    b       1f      // skip wrapper sequence

// procedure list(view f: string; var l: filptr); external;

services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    b       wrapper_list

// procedure list(view f: pstring; var  l: filptr); external;

services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    b       wrapper_listp

// procedure times(var s: string; t: integer); external;

services.times$p_vc_i:
    b       wrapper_times

// function times(t: integer): pstring; external;

services.times$f_i:
    b       wrapper_timesp

// procedure dates(var s: string; t: integer); external;

services.dates$p_vc_i:
    b       wrapper_dates

// function dates(t: integer): pstring; external;

services.dates$f_i:
    b       wrapper_datesp

// procedure writetime(var f: text; t: integer); external;

services.writetime$p_fc_i:
    b       wrapper_writetimef

// procedure writetime(t: integer); external;

services.writetime$p_i:
    b       wrapper_writetime

// procedure writedate(var f: text; t: integer); external;

services.writedate$p_fc_i:
    b       wrapper_writedatef

// procedure writedate(t: integer); external;

services.writedate$p_i:
    b       wrapper_writedate

// function validfile(view s: string): boolean; external;

services.validfile$f_vc:
    b       wrapper_validfile

// function validfile(view s: pstring): boolean; external;

services.validfile$f_pvc:
    b       wrapper_validfilep

// function validpath(view s: string): boolean; external;

services.validpath$f_vc:
    b       wrapper_validpath

// function validpath(view s: pstring): boolean; external;

services.validpath$f_pvc:
    b       wrapper_validpathp

// function wild(view s: string): boolean; external;

services.wild$f_vc:
    b       wrapper_wild

// function wild(view s: pstring): boolean; external;

services.wild$f_pvc:
    b       wrapper_wildp

// procedure getenv(view ls: string; var ds: string); external;

services.getenv$p_vc_vc:
    b       wrapper_getenv

// function getenv(view ls: string): pstring; external;

services.getenv$f_vc:
    b       wrapper_getenvp

// procedure setenv(view sn, sd: string); external;

services.setenv$p_vc_vc:
    b       wrapper_setenv

// procedure setenv(sn: pstring; view sd: string); external;

services.setenv$p_pvc_vc:
    b       wrapper_setenvps

// procedure setenv(view sn: string; sd: pstring); external;

services.setenv$p_vc_pvc:
    b       wrapper_setenvsp

// procedure setenv(sn, sd: pstring); external;

services.setenv$p_pvc_pvc:
    b       wrapper_setenvpp

// procedure allenv(var el: envptr); external;

services.allenv$p_pr$name$0$pvc$data$8$p12$next$16$p2$:
    b       wrapper_allenv

// procedure remenv(view sn: string); external;

services.remenv$p_vc:
    b       wrapper_remenv

// procedure remenv(view sn: pstring); external;

services.remenv$p_pvc:
    b       wrapper_remenvp

// procedure exec(view cmd: string); external;

services.exec$p_vc:
    b       wrapper_exec

// procedure exec(cmd: pstring); external;

services.exec$p_pvc:
    b       wrapper_execp

// procedure exece(view cmd: string; el: envptr); external;

services.exece$p_vc_pr$name$0$pvc$data$8$p12$next$16$p2$:
    b       wrapper_exece

// procedure exece(cmd: pstring; el: envptr); external;

services.exece$p_pvc_pr$name$0$pvc$data$8$p12$next$16$p2$:
    b       wrapper_execep

// procedure execw(view cmd: string; var e: integer); external;

services.execw$p_vc_i:
    b       wrapper_execw

// procedure execw(cmd: pstring; var e: integer); external;

services.execw$p_pvc_i:
    b       wrapper_execwp

// procedure execew(view cmd: string; el: envptr; var e: integer); external;

services.execew$p_vc_pr$name$0$pvc$data$8$p12$next$16$p2$_i:
    b       wrapper_execew

// procedure execew(cmd: pstring; el: envptr; var e: integer); external;

services.execew$p_pvc_pr$name$0$pvc$data$8$p12$next$16$p2$_i:
    b       wrapper_execewp

// procedure getcur(var fn: string); external;

services.getcur$p_vc:
    b       wrapper_getcur

// function getcur: pstring; external;

services.getcur$f:
    b       wrapper_getcurp

// procedure setcur(view fn: string); external;

services.setcur$p_vc:
    b       wrapper_setcur

// procedure setcur(fn: pstring); external;

services.setcur$f_pvc:
    b       wrapper_setcurp

// procedure brknam(view fn: string; var p, n, e: string); external;

services.brknam$p_vc_vc_vc_vc:
    b       wrapper_brknam

// procedure brknam(view fn: string; var p, n, e: pstring); external;

services.brknam$p_vc_pvc_pvc_pvc:
    b       wrapper_brknamsp

// procedure brknam(fn: pstring; var p, n, e: pstring); external;

services.brknam$p_pvc_pvc_pvc_pvc:
    b       wrapper_brknampp

// procedure maknam(var fn: string; view p, n, e: string); external;

services.maknam$p_vc_vc_vc_vc:
    b       wrapper_maknam

// function maknam(view p, n, e: string): pstring; external;

services.maknam$f_vc_vc_vc:
    b       wrapper_maknamp

// function maknam(view p: string; view n: string; e: pstring): pstring; external;

services.maknam$f_vc_vc_pvc:
    b       wrapper_maknamppsp

// function maknam(view p: string; n: pstring; view e: string): pstring; external;

services.maknam$f_vc_pvc_vc:
    b       wrapper_maknamppsp

// function maknam(view p: string; n: pstring; e: pstring): pstring; external;

services.maknam$f_vc_pvc_pvc:
    b       wrapper_maknampspp

// function maknam(p: pstring; view n: string; view e: string): pstring; external;

services.maknam$f_pvc_vc_vc:
    b       wrapper_maknamppss

// function maknam(p: pstring; view n: string; e: pstring): pstring; external;

services.maknam$f_pvc_vc_pvc:
    b       wrapper_maknamppsp

// function maknam(p: pstring; n: pstring; view e: string): pstring; external;

services.maknam$f_pvc_pvc_vc:
    b       wrapper_maknamppps

// function maknam(p: pstring; n: pstring; e: pstring): pstring; external;

services.maknam$f_pvc_pvc_pvc:
    b       wrapper_maknampppp

// procedure fulnam(var fn: string); external;

services.fulnam$p_vc:
    b       wrapper_fulnam

// function fulnam(view fn: string): pstring; external;

services.fulnam$f_vc:
    b       wrapper_fulnamp

// procedure getpgm(out p: string); external;

services.getpgm$p_vc:
    b       wrapper_getpgm

// function getpgm: pstring; external;

services.getpgm$f:
    b       wrapper_getpgmp

// procedure getusr(out fn: string); external;

services.getusr$p_vc:
    b       wrapper_getusr

// function getusr: pstring; external;

services.getusr$f:
    b       wrapper_getusrp

// procedure setatr(view fn: string; a: attrset); external;

services.setatr$p_vc_sx$atexec$atarc$atsys$atdir$atloop$:
    b       wrapper_setatr

// procedure setatr(fn: pstring; a: attrset); external;

services.setatr$p_pvc_sx$atexec$atarc$atsys$atdir$atloop$:
    b       wrapper_setatrp

// procedure resatr(view fn: string; a: attrset); external;

services.resatr$p_vc_sx$atexec$atarc$atsys$atdir$atloop$:
    b       wrapper_resatr

// procedure resatr(fn: pstring; a: attrset); external;

services.resatr$p_pvc_sx$atexec$atarc$atsys$atdir$atloop$:
    b       wrapper_resatrp

// procedure bakupd(view fn: string); external;

services.bakupd$p_vc:
    b       wrapper_bakupd

// procedure bakupd(fn: pstring); external;

services.bakupd$p_pvc:
    b       wrapper_bakupdp

// procedure setuper(view fn: string; p: permset); external;

services.setuper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_setuper

// procedure setuper(fn: pstring; p: permset); external;

services.setuper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_setuperp

// procedure resuper(view fn: string; p: permset); external;

services.resuper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_resuper

// procedure resuper(fn: pstring; p: permset); external;

services.resuper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_resuperp

// procedure setgper(view fn: string; p: permset); external;

services.setgper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_setgper

// procedure setgper(fn: pstring; p: permset); external;

services.setgper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_setgperp

// procedure resgper(view fn: string; p: permset); external;

services.resgper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_resgper

// procedure resgper(fn: pstring; p: permset); external;

services.resgper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_resgperp

// procedure setoper(view fn: string; p: permset); external;

services.setoper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_setoper

// procedure setoper(fn: pstring; p: permset); external;

services.setoper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_setoperp

// procedure resoper(view fn: string; p: permset); external;

services.resoper$p_vc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_resoper

// procedure resoper(fn: pstring; p: permset); external;

services.resoper$p_pvc_sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$:
    b       wrapper_resoperp

// procedure makpth(view fn: string); external;

services.makpth$p_vc:
    b       wrapper_makpth

// procedure makpth(fn: pstring); external;

services.makpth$p_pvc:
    b       wrapper_makpthp

// procedure rempth(view fn: string); external;

services.rempth$p_vc:
    b       wrapper_rempth

// procedure rempth(fn: pstring); external;

services.rempth$p_pvc:
    b       wrapper_rempthp

// procedure countrys(view s: string; len: integer; c: integer); external;

services.countrys$p_vc_i:
    b       wrapper_countrys

// procedure languages(view s: string; len: integer; l: integer);  external;

services.languages$p_vc_i:
    b       wrapper_languages

//
// Scalar functions. These require no marshalling, so they are routed directly
// to the Ami entry point.
//

// function time: integer; external;

services.time$f:
    b       ami_time

// function local(t: integer): integer; external;

services.local$f_i:
    b       ami_local

// function clock: integer; external;

services.clock$f:
    b       ami_clock

// function elapsed(r: integer): integer; external;

services.elapsed$f_i:
    b       ami_elapsed

// procedure filchr(out fc: schar); external;

services.filchr$p_sc:
    b       ami_filchr

// function optchr: char; external;

services.optchr$f:
    b       ami_optchr

// function pthchr: char; external;

services.pthchr$f:
    b       ami_pthchr

// function latitude: integer; external;

services.latitude$f:
    b       ami_latitude

// function longitude: integer; external;

services.longitude$f:
    b       ami_longitude

// function altitude: integer; external;

services.altitude$f:
    b       ami_altitude

// function country: integer; external;

services.country$f:
    b       ami_country

// function timezone: integer; external;

services.timezone$f:
    b       ami_timezone

// function daysave: boolean; external;

services.daysave$f:
    b       ami_daysave

// function time24hour: boolean; external;

services.time24hour$f:
    b       ami_time24hour

// function language: integer; external;

services.language$f:
    b       ami_language

// function decimal: char; external;

services.decimal$f:
    b       ami_decimal

// function numbersep: char; external;

services.numbersep$f:
    b       ami_numbersep

// function timeorder: integer; external;

services.timeorder$f:
    b       ami_timeorder

// function dateorder: integer; external;

services.dateorder$f:
    b       ami_dateorder

// function datesep: char; external;

services.datesep$f:
    b       ami_datesep

// function timesep: char; external;

services.timesep$f:
    b       ami_timesep

// function currchr: char; external;

services.currchr$f:
    b       ami_currchr

//
// Next module in series
//
1:
