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

This set of extensions is for the Pascaline language.

Note the routines are implementing the Pascaline language, so the contents of
them tend to just be feedthrough implementations.
  
}
  
procedure assigntext(var f: text; var fn: filnam);
var s: string(fillen);
    i, l: integer;
begin
  assign(f, fn);
end;

procedure assignbin(var f: bytfil; var fn: filnam);
var s: string(fillen);
    i, l: integer;
begin
  assign(f, fn);
end;

procedure closetext(var f: text);
begin
  close(f)
end;

procedure closebin(var f: bytfil);
begin
  close(f);
end;

function lengthbin(var f: bytfil): integer;
begin
  lengthbin := length(f);
end;

function locationbin(var f: bytfil): integer;
begin
  locationbin := location(f);
end;

procedure positionbin(var f: bytfil; p: integer);
begin
  position(f, p);
end;

procedure updatebin(var f: bytfil);
begin
  update(f);
end;

procedure appendtext(var f: text);
begin
  append(f);
end;

procedure appendbin(var f: bytfil);
begin
  append(f);
end;

function existsfile(var fn: filnam): boolean;
begin
  existsfile := exists(fn);
end;

procedure deletefile(var fn: filnam);
begin
  delete(fn);
end;

procedure changefile(var fnd, fns: filnam);
begin
  change(fnd, fns);
end;

procedure getcommandline(var cb: cmdbuf; var l: cmdnum);
var i, j: cmdinx;
procedure putcmd(c: char);
begin
  if i >= maxcmd-1 then errore(CommandLineTooLong);
  cb[i] := c; i := i+1
end;
begin
  for j := 1 to maxcmd do cb[j] := ' '; j := 1;
  i := 1; 
  while not eoln(command) do begin read(command, c); putcmd(c) end;
  l := i-1
end;

