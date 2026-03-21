{******************************************************************************
*                                                                             *
*                         EXTENDED FUNCTION LIBRARY                           *
*                                                                             *
*                           Copyright (C) 1996                                *
*                                                                             *
*                               S. A. MOORE                                   *
*                                                                             *
* Contains various system oriented library functions, including files,        *
* directories, time, program execution, evironment, and random numbers.       *
* Most or all of these procedures are implemented in a manner specific to     *
* Windows 95/Windows NT 4.0 or later versions.                                *
*                                                                             *
******************************************************************************}

module services;

type 

   { attributes }
   attribute = (atexec,  { is an executable file type }
                atarc,   { has been archived since last modification }
                atsys,   { is a system special file }
                atdir,   { is a directory special file }
                atloop); { contains heriarchy loop }
   attrset = set of attribute; { attributes in a set }
   { permissions }
   permission = (pmread,  { may be read }
                 pmwrite, { may be written }
                 pmexec,  { may be executed }
                 pmdel,   { may be deleted }
                 pmvis,   { may be seen in directory listings }
                 pmcopy,  { may be copied }
                 pmren);  { may be renamed/moved }
   permset = set of permission; { permissions in a set }
   { standard directory format }
   filptr = ^filrec; { pointer to file records }
   filrec = record

      name:   pstring; { name of file }
      size:   integer; { size of file }
      alloc:  integer; { allocation of file }
      attr:   attrset; { attributes }
      create: integer; { time of creation }
      modify: integer; { time of last modification }
      access: integer; { time of last access }
      backup: integer; { time of last backup }
      user:   permset; { user permissions }
      group:  permset; { group permissions }
      other:  permset; { other permissions }
      next:   filptr   { next entry in list }

   end;
   { environment strings }
   envptr = ^envrec; { pointer to environment record }
   envrec = packed record

      name: pstring; { name of string }
      data: pstring; { data in string }
      next: envptr { next entry in list }

   end;

procedure list(view f: string; var l: filptr); mexternal;
overload procedure list(view f: pstring; var  l: filptr); mexternal;
procedure times(out s: string; t: integer); mexternal;
overload function times(t: integer): pstring; mexternal;
procedure dates(out s: string; t: integer); mexternal;
overload function dates(t: integer): pstring; mexternal;
procedure writetime(var f: text; t: integer); mexternal;
overload procedure writetime(t: integer); mexternal;
procedure writedate(var f: text; t: integer); mexternal;
overload procedure writedate(t: integer); mexternal;
function time: integer; mexternal;
function local(t: integer): integer; mexternal;
function clock: integer; mexternal;
function elapsed(r: integer): integer; mexternal;
function validfile(view s: string): boolean; mexternal;
overload function validfile(view s: pstring): boolean; mexternal;
function validpath(view s: string): boolean; mexternal;
overload function validpath(view s: pstring): boolean; mexternal;
function wild(view s: string): boolean; mexternal;
overload function wild(view s: pstring): boolean; mexternal;
procedure getenv(view ls: string; out ds: string); mexternal;
overload function getenv(view ls: string): pstring; mexternal;
procedure setenv(view sn, sd: string); mexternal;
overload procedure setenv(sn: pstring; view sd: string); mexternal;
overload procedure setenv(view sn: string; sd: pstring); mexternal;
overload procedure setenv(sn, sd: pstring); mexternal;
procedure allenv(var el: envptr); mexternal;
procedure remenv(view sn: string); mexternal;
overload procedure remenv(view sn: pstring); mexternal;
procedure exec(view cmd: string); mexternal;
overload procedure exec(cmd: pstring); mexternal;
procedure exece(view cmd: string; el: envptr); mexternal;
overload procedure exece(cmd: pstring; el: envptr); mexternal;
procedure execw(view cmd: string; out e: integer); mexternal;
overload procedure execw(cmd: pstring; out e: integer); mexternal;
procedure execew(view cmd: string; el: envptr; out e: integer); mexternal;
overload procedure execew(cmd: pstring; el: envptr; out e: integer); mexternal;
procedure getcur(out fn: string); mexternal;
overload function getcur: pstring; mexternal;
procedure setcur(view fn: string); mexternal;
overload procedure setcur(fn: pstring); mexternal;
procedure brknam(view fn: string; out p, n, e: string); mexternal;
overload procedure brknam(view fn: string; out p, n, e: pstring); mexternal;
overload procedure brknam(fn: pstring; out p, n, e: pstring); mexternal;
procedure maknam(out fn: string; view p, n, e: string); mexternal;
overload function maknam(view p, n, e: string): pstring; mexternal;
overload function maknam(view p: string; view n: string; e: pstring): pstring; mexternal;
overload function maknam(view p: string; n: pstring; view e: string): pstring; mexternal;
overload function maknam(view p: string; n: pstring; e: pstring): pstring; mexternal;
overload function maknam(p: pstring; view n: string; view e: string): pstring; mexternal;
overload function maknam(p: pstring; view n: string; e: pstring): pstring; mexternal;
overload function maknam(p: pstring; n: pstring; view e: string): pstring; mexternal;
overload function maknam(p: pstring; n: pstring; e: pstring): pstring; mexternal;
procedure fulnam(var fn: string); mexternal;
overload function fulnam(view fn: string): pstring; mexternal;
procedure getpgm(out p: string); mexternal;
overload function getpgm: pstring; mexternal;
procedure getusr(out fn: string); mexternal;
overload function getusr: pstring; mexternal;
procedure setatr(view fn: string; a: attrset); mexternal;
overload procedure setatr(fn: pstring; a: attrset); mexternal;
procedure resatr(view fn: string; a: attrset); mexternal;
overload procedure resatr(fn: pstring; a: attrset); mexternal;
procedure bakupd(view fn: string); mexternal;
overload procedure bakupd(fn: pstring); mexternal;
procedure setuper(view fn: string; p: permset); mexternal;
overload procedure setuper(fn: pstring; p: permset); mexternal;
procedure resuper(view fn: string; p: permset); mexternal;
overload procedure resuper(fn: pstring; p: permset); mexternal;
procedure setgper(view fn: string; p: permset); mexternal;
overload procedure setgper(fn: pstring; p: permset); mexternal;
procedure resgper(view fn: string; p: permset); mexternal;
overload procedure resgper(fn: pstring; p: permset); mexternal;
procedure setoper(view fn: string; p: permset); mexternal;
overload procedure setoper(fn: pstring; p: permset); mexternal;
procedure resoper(view fn: string; p: permset); mexternal;
overload procedure resoper(fn: pstring; p: permset); mexternal;
procedure seterr(e: integer); mexternal;
procedure makpth(view fn: string); mexternal;
overload procedure makpth(fn: pstring); mexternal;
procedure rempth(view fn: string); mexternal;
overload procedure rempth(fn: pstring); mexternal;
procedure filchr(out fc: schar); mexternal;
function optchr: char; mexternal;
function pthchr: char; mexternal;
function latitude: integer; mexternal;
function longitude: integer; mexternal;
function altitude: integer; mexternal;
function country: integer; mexternal;
procedure countrys(out s: string; c: integer); mexternal;
function timezone: integer; mexternal;
function daysave: boolean; mexternal;
function time24hour: boolean; mexternal;
function language: integer; mexternal;
procedure languages(out s: string; l: integer);  external;
function decimal: char; mexternal;
function numbersep: char; mexternal;
function timeorder: integer; mexternal;
function dateorder: integer; mexternal;
function datesep: char; mexternal;
function timesep: char; mexternal;
function currchr: char; mexternal;

begin
end.