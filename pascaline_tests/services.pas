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

module services(output);

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
procedure times(var s: string; t: integer); external;
procedure dates(var s: string; t: integer); external;
procedure writetime(var f: text; t: integer); external;
procedure writedate(var f: text; t: integer); external;
function time: integer; external;
function local(t: integer): integer; external;
function clock: integer; external;
function elapsed(r: integer): integer; external;
function validfile(view s: string): boolean; external;
function validpath(view s: string): boolean; external;
function wild(view s: string): boolean; external;
procedure getenv(view ls: string; var ds: string); external;
procedure setenv(view sn, sd: string); external;
procedure allenv(var el: envptr); external;
procedure remenv(view sn: string); external;
procedure exec(view cmd: string); external;
procedure exece(view cmd: string; el: envptr); external;
procedure execw(view cmd: string; var e: integer); external;
procedure execew(view cmd: string; el: envptr; var e: integer); external;
procedure getcur(var fn: string); external;
procedure setcur(view fn: string); external;
procedure brknam(view fn: string; var p, n, e: string); external;
procedure maknam(var fn: string; view p, n, e: string); external;
procedure fulnam(var fn: string); external;
procedure getpgm(var p: string); external;
procedure getusr(var fn: string); external;
procedure setatr(view fn: string; a: attrset); external;
procedure resatr(view fn: string; a: attrset); external;
procedure bakupd(view fn: string); external;
procedure setuper(view fn: string; p: permset); external;
procedure resuper(view fn: string; p: permset); external;
procedure setgper(view fn: string; p: permset); external;
procedure resgper(view fn: string; p: permset); external;
procedure setoper(view fn: string; p: permset); external;
procedure resoper(view fn: string; p: permset); external;
procedure seterr(c: integer); external;
procedure makpth(view fn: string); external;
procedure rempth(view fn: string); external;
procedure filchr(var fc: schar); external;
function optchr: char; external;
function pthchr: char; external;

begin
end.