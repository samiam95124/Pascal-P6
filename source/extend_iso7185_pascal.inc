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

This set of extensions is for the ISO7185 Pascal language.

Note that since ISO 7185 Pascal has no extensions, these routines are just stubs
that produce errors.
  
}
  
procedure assigntext(var f: text; var fn: filnam);
begin
  fn := fn; reset(f);
  errorv(FunctionNotImplemented)
end;

procedure assignbin(var f: bytfil; var fn: filnam);
begin
  fn := fn; reset(f);
  errorv(FunctionNotImplemented)
end;

procedure closetext(var f: text);
begin
  reset(f);
  errorv(FunctionNotImplemented) 
end;

procedure closebin(var f: bytfil);
begin
  reset(f);
  errorv(FunctionNotImplemented) 
end;

function lengthbin(var f: bytfil): integer;
begin
  reset(f);
  errorv(FunctionNotImplemented);
  lengthbin := 1
end;

function locationbin(var f: bytfil): integer;
begin
  reset(f);
  errorv(FunctionNotImplemented); 
  locationbin := 1
end;

procedure positionbin(var f: bytfil; p: integer);
begin
  reset(f); p := p;
  errorv(FunctionNotImplemented) 
end;

procedure updatebin(var f: bytfil);
begin
  reset(f);
  errorv(FunctionNotImplemented)
end;

procedure appendtext(var f: text);
begin
  reset(f);
  errorv(FunctionNotImplemented)
end;

procedure appendbin(var f: bytfil);
begin
  reset(f);
  errorv(FunctionNotImplemented) 
end;

function existsfile(var fn: filnam): boolean;
begin
  fn := fn;
  errorv(FunctionNotImplemented);
  existsfile := true
end;

procedure deletefile(var fn: filnam);
begin
  fn := fn;
  errorv(FunctionNotImplemented)
end;

procedure changefile(var fnd, fns: filnam);
begin
  fnd := fnd;
  fns := fns;
  errorv(FunctionNotImplemented)
end;

procedure getcommandline(var cb: cmdbuf; var l: cmdnum);
var i, j: cmdinx;
procedure putcmd(c: char);
begin
  if i >= maxcmd-1 then errore(CommandLineTooLong);
  cb[i] := c; i := i+1
end;
begin
  { getcommand is the exception for ISO 7185 Pascal. We don't flag an error, but 
    rather just return a blank command line }
  i := 1; putcmd(' ');
  for j := 1 to maxcmd do cb[j] := ' '; j := 1;
  l := 0
end;

procedure paroptions;
begin
end;

function chkbrk: boolean;
begin
  chkbrk := false
end;

procedure exitprogram(ec: integer);
begin
  ec := ec
end;
