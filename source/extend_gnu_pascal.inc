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

This set of extensions is for the GNU Pascal dialect.
  
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
  locationbin := position(f);
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

function open(fn: Cstring; f: integer): integer; external name 'open';
function c_close(fd: integer): integer; external name 'close';

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

function remove(fn: Cstring): integer; external name 'remove';

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

function rename(fns, fnd: Cstring): integer; external name 'rename';

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

{ Restore to ISO 7185 Pascal language }
{$classic-pascal-level-0}
