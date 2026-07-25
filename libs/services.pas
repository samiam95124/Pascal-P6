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

procedure list(view f: string; var l: filptr); external;
overload procedure list(view f: pstring; var  l: filptr); external;
procedure times(out s: string; t: integer); external;
overload function times(t: integer): pstring; external;
procedure dates(out s: string; t: integer); external;
overload function dates(t: integer): pstring; external;
procedure writetime(var f: text; t: integer); external;
overload procedure writetime(t: integer); external;
procedure writedate(var f: text; t: integer); external;
overload procedure writedate(t: integer); external;
function time: integer; external;
function local(t: integer): integer; external;
function clock: integer; external;
function elapsed(r: integer): integer; external;
function validfile(view s: string): boolean; external;
overload function validfile(view s: pstring): boolean; external;
function validpath(view s: string): boolean; external;
overload function validpath(view s: pstring): boolean; external;
function wild(view s: string): boolean; external;
overload function wild(view s: pstring): boolean; external;
procedure getenv(view ls: string; out ds: string); external;
overload function getenv(view ls: string): pstring; external;
procedure setenv(view sn, sd: string); external;
overload procedure setenv(sn: pstring; view sd: string); external;
overload procedure setenv(view sn: string; sd: pstring); external;
overload procedure setenv(sn, sd: pstring); external;
procedure allenv(var el: envptr); external;
procedure remenv(view sn: string); external;
overload procedure remenv(view sn: pstring); external;
procedure exec(view cmd: string); external;
overload procedure exec(cmd: pstring); external;
procedure exece(view cmd: string; el: envptr); external;
overload procedure exece(cmd: pstring; el: envptr); external;
procedure execw(view cmd: string; out e: integer); external;
overload procedure execw(cmd: pstring; out e: integer); external;
procedure execew(view cmd: string; el: envptr; out e: integer); external;
overload procedure execew(cmd: pstring; el: envptr; out e: integer); external;
procedure getcur(out fn: string); external;
overload function getcur: pstring; external;
procedure setcur(view fn: string); external;
overload procedure setcur(fn: pstring); external;
procedure brknam(view fn: string; out p, n, e: string); external;
overload procedure brknam(view fn: string; out p, n, e: pstring); external;
overload procedure brknam(fn: pstring; out p, n, e: pstring); external;
procedure maknam(out fn: string; view p, n, e: string); external;
overload function maknam(view p, n, e: string): pstring; external;
overload function maknam(view p: string; view n: string; e: pstring): pstring; external;
overload function maknam(view p: string; n: pstring; view e: string): pstring; external;
overload function maknam(view p: string; n: pstring; e: pstring): pstring; external;
overload function maknam(p: pstring; view n: string; view e: string): pstring; external;
overload function maknam(p: pstring; view n: string; e: pstring): pstring; external;
overload function maknam(p: pstring; n: pstring; view e: string): pstring; external;
overload function maknam(p: pstring; n: pstring; e: pstring): pstring; external;
procedure fulnam(var fn: string); external;
overload function fulnam(view fn: string): pstring; external;
procedure getpgm(out p: string); external;
overload function getpgm: pstring; external;
procedure getusr(out fn: string); external;
overload function getusr: pstring; external;
procedure setatr(view fn: string; a: attrset); external;
overload procedure setatr(fn: pstring; a: attrset); external;
procedure resatr(view fn: string; a: attrset); external;
overload procedure resatr(fn: pstring; a: attrset); external;
procedure bakupd(view fn: string); external;
overload procedure bakupd(fn: pstring); external;
procedure setuper(view fn: string; p: permset); external;
overload procedure setuper(fn: pstring; p: permset); external;
procedure resuper(view fn: string; p: permset); external;
overload procedure resuper(fn: pstring; p: permset); external;
procedure setgper(view fn: string; p: permset); external;
overload procedure setgper(fn: pstring; p: permset); external;
procedure resgper(view fn: string; p: permset); external;
overload procedure resgper(fn: pstring; p: permset); external;
procedure setoper(view fn: string; p: permset); external;
overload procedure setoper(fn: pstring; p: permset); external;
procedure resoper(view fn: string; p: permset); external;
overload procedure resoper(fn: pstring; p: permset); external;
procedure makpth(view fn: string); external;
overload procedure makpth(fn: pstring); external;
procedure rempth(view fn: string); external;
overload procedure rempth(fn: pstring); external;
procedure filchr(out fc: schar); external;
function optchr: char; external;
function pthchr: char; external;
function latitude: integer; external;
function longitude: integer; external;
function altitude: integer; external;
function country: integer; external;
procedure countrys(out s: string; c: integer); external;
function timezone: integer; external;
function daysave: boolean; external;
function time24hour: boolean; external;
function language: integer; external;
procedure languages(out s: string; l: integer);  external;
function decimal: char; external;
function numbersep: char; external;
function timeorder: integer; external;
function dateorder: integer; external;
function datesep: char; external;
function timesep: char; external;
function currchr: char; external;

begin
end.