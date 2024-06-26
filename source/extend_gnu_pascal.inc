{ 

Language extension routines. These routines allow specification of semantic
functions beyond the base ISO 7185 specification. 

There are two kinds of files, and both need to be represented here:
text files and binary files. Binary files are files of bytes, and everything
besides text is broken down into bytes and transferred via byte files. 
Note that several functions don't have text equivalents, like length,
location, position and update. 

Routines:

procedure assigntext(var f: text; var fn: filnam); - Assign name to text file.
procedure assignbin(var f: bytfil; var fn: filnam); - Assign name to binary file.
procedure closetext(var f: text); - Close text file.
procedure closebin(var f: bytfil); - Close binary file.
function lengthbin(var f: bytfil): integer; - Find length binary file.
function locationbin(var f: bytfil): integer; - Find location binary file.
procedure positionbin(var f: bytfil; p: integer); - Position binary file.
procedure updatebin(var f: bytfil); - Update binary file.
procedure appendtext(var f: text); - Append text file.
procedure appendbin(var f: bytfil); - Append binary file.
function existsfile(var fn: filnam): boolean; - Find file exists by name.
procedure deletefile(var fn: filnam); - Delete file by name.
procedure changefile(var fnd, fns: filnam); - Change filename.
procedure getcommandline(var cb: cmdbuf; var l: cmdnum); - Get the shell command
                                                           line.
procedure paroptions; - Parse command line options.

This set of extensions is for the GNU Pascal dialect.

Notes:

1. I try to use the native GPC (built-ins) wherever possible. Where there is no
native function, or the native function dosen't work, an external C function was
used.

2. The size of int became an issue between 32 and 64 bit versions of GPC. Ignore
the GPC documentation for that, it is wrong.
  
}
  
{ Allow GNU Pascal extensions }
{$gnu-pascal}

procedure assigntext(var f: text; var fn: filnam);
var s: string(fillen);
    i, l: integer;
begin
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  assign(f, s);
end;

procedure assignbin(var f: bytfil; var fn: filnam);
var s: string(fillen);
    i, l: integer;
begin
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  assign(f, s);
end;

procedure closetext(var f: text);
begin
  close(f)
end;

procedure closebin(var f: bytfil);
begin
  close(f)
end;

function lengthbin(var f: bytfil): integer;
begin
  if empty(f) then
    lengthbin := 0
  else
    lengthbin := LastPosition (f) + 1;
end;

function locationbin(var f: bytfil): integer;
begin
  locationbin := position(f)+1;
end;

procedure positionbin(var f: bytfil; p: integer);
begin
  seek(f, p-1);
end;

procedure updatebin(var f: bytfil);
begin
  append(f);
  seek(f, 0);
end;

procedure appendtext(var f: text);
begin
  append(f);
end;

procedure appendbin(var f: bytfil);
begin
  append(f);
end;

/* there does not appear to be a 32/64 equivalent to long int in GPC */
#ifdef WRDSIZ64
function open(fn: Cstring; f: shortint): shortint; external name 'open';
function c_close(fd: shortint): shortint; external name 'close';
#else /* WRDSIZ32 */
function open(fn: Cstring; f: integer): integer; external name 'open';
function c_close(fd: integer): integer; external name 'close';
#endif

function existsfile(var fn: filnam): boolean;
var s: string(fillen);
    i, l, r, fd: integer;
begin
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  fd := open(s, 0);
  if fd > 0 then r := c_close(fd);
  existsfile := fd >= 0
end;

#ifdef WRDSIZ64
function remove(fn: Cstring): shortint; external name 'remove';
#else /* WRDSIZ32 */
function remove(fn: Cstring): integer; external name 'remove';
#endif

procedure deletefile(var fn: filnam);
var s: string(fillen);
    i, l, r: integer;
begin
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  r := remove(s);
  if r <> 0 then errore(FileDeleteFail);
end;

#ifdef WRDSIZ64
function rename(fns, fnd: Cstring): shortint; external name 'rename';
#else /* WRDSIZ32 */
function rename(fns, fnd: Cstring): integer; external name 'rename';
#endif

procedure changefile(var fnd, fns: filnam);
var ss,sd: string(fillen);
    i, l, r: integer;
begin
  l := fillen;
  while (fns[l] = ' ') and (l > 1) do l := l-1;
  ss := '';
  for i := 1 to l do ss := ss+fns[i];
  l := fillen;
  while (fnd[l] = ' ') and (l > 1) do l := l-1;
  sd := '';
  for i := 1 to l do sd := sd+fnd[i];
  r := rename(ss,sd);
  if r <> 0 then errore(FileNameChangeFail)
end;

procedure getcommandline(var cb: cmdbuf; var l: cmdnum);
var i, j: cmdinx;
    x, p: integer;
procedure putcmd(c: char);
begin
  if i >= maxcmd-1 then errore(CommandLineTooLong);
  cb[i] := c; i := i+1
end;
begin
  i := 1;
  for j := 1 to maxcmd do cb[j] := ' '; j := 1;
  for p := 1 to paramcount do begin
    for x := 1 to length(paramstr(p)) do putcmd(paramstr(p)[x]); 
    if p < paramcount then putcmd(' ')
  end;
  l := i-1
end;

procedure paroptions;
var optst: optstr; oni: optinx; oi: 1..maxopt; ch1: char;
begin
  repeat
    while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
    if bufcommand = '-' then begin
      getcommand;
      if bufcommand = '-' then getcommand;
      if not (bufcommand in ['a'..'z', 'A'..'Z', '_']) then begin
        writeln('*** No valid option found    ');
        goto 99
      end;
      oni := 1; optst := '          ';
      while bufcommand in ['a'..'z', 'A'..'Z', '0'..'9'] do begin
        ch1 := lcase(bufcommand); 
        if optst[oni] = ' ' then optst[oni] := ch1; 
        if oni < optlen then oni := oni+1;
        getcommand
      end;
      oi := 1;
      while (oi < maxopt) and (optst <> opts[oi]) and (optst <> optsl[oi]) do
        oi := oi+1;
      if (optst = opts[oi]) or (optst = optsl[oi]) then begin
        option[oi] := true; if bufcommand = '-' then option[oi] := false;
        options[oi] := true;
        if (bufcommand = '+') or (bufcommand = '-') then getcommand
      end;
    end;
    while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand
  until bufcommand <> '-'
end;

procedure exitprogram(ec: integer);
begin
  halt(ec)
end;

procedure ctlchandler(sig: integer);
begin
  breakflag := true
end;

procedure signal(signum: integer; handler: integer); external name 'signal';

procedure capture;
begin
  signal(2, integer(addr(ctlchandler)));
  signal(6, integer(addr(ctlchandler)))
end;

function chkbrk: boolean;
begin
  chkbrk := breakflag;
  breakflag := false
end;

procedure extendinit;

begin
  { initialize options tables }
  opts[1]  := 'a         ';
  opts[2]  := 'b         ';
  opts[3]  := 'c         ';
  opts[4]  := 'd         ';
  opts[5]  := 'e         ';
  opts[6]  := 'f         ';
  opts[7]  := 'g         ';
  opts[8]  := 'h         ';
  opts[9]  := 'i         ';
  opts[10] := 'ee        ';
  opts[11] := '          ';
  opts[12] := 'l         ';
  opts[13] := 'm         ';
  opts[14] := 'n         ';
  opts[15] := 'o         ';
  opts[16] := 'p         ';
  opts[17] := 'q         ';
  opts[18] := 'r         ';
  opts[19] := 's         ';
  opts[20] := 't         ';
  opts[21] := 'u         ';
  opts[22] := 'v         ';
  opts[23] := 'w         ';
  opts[24] := 'x         ';
  opts[25] := 'y         ';
  opts[26] := 'z         ';

  optsl[1]  := 'debugflt  ';
  optsl[2]  := 'prtlab    ';
  optsl[3]  := 'lstcod    ';
  optsl[4]  := 'chk       ';
  optsl[5]  := 'machdeck  ';
  optsl[6]  := 'debugsrc  ';
  optsl[7]  := 'prtlabdef ';
  optsl[8]  := 'sourceset ';
  optsl[9]  := 'varblk    ';
  optsl[10] := 'experror  ';
  optsl[11] := 'echoline  ';
  optsl[12] := 'list      ';
  optsl[13] := 'breakheap ';
  optsl[14] := 'recycle   ';
  optsl[15] := 'chkoverflo';
  optsl[16] := 'chkreuse  ';
  optsl[17] := 'chkundef  ';
  optsl[18] := 'reference ';
  optsl[19] := 'iso7185   ';
  optsl[20] := 'prttables ';
  optsl[21] := 'undestag  ';
  optsl[22] := 'chkvar    ';
  optsl[23] := 'debug     ';
  optsl[24] := 'prtlex    ';
  optsl[25] := 'prtdisplay';
  optsl[26] := '          ';
end;

{ Restore to ISO 7185 Pascal language }
{$classic-pascal-level-0}
