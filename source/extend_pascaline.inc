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

This set of extensions is for the Pascaline language.

Note the routines are implementing the Pascaline language, so the contents of
them tend to just be feedthrough implementations.
  
}
  
procedure assigntext(var f: text; var fn: filnam);
begin
  assign(f, fn);
end;

procedure assignbin(var f: bytfil; var fn: filnam);
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
var i, j: cmdinx; c: char;
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

procedure parhdrfil(var f: text);
const maxfil = 200;
var name: packed array [1..maxfil] of char;
procedure getname;
var i: 1..maxfil;
begin
  for i := 1 to maxfil do name[i] := ' ';
  i := 1;
  while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
  if (bufcommand <> '-') and not eolncommand and not eofcommand then
    while not eolncommand and not eofcommand and (bufcommand <> ' ') do begin
    if i = maxfil then begin
      writeln('*** Filename too long');
      halt
    end;
    name[i] := bufcommand; getcommand; i := i+1
  end
end;
begin
  getname; 
  if name[1] <> ' ' then assign(f, name)
  else begin
    writeln('*** No name provided for file');
    halt
  end
end;

procedure capture;
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
  optsl[26] := 'lineinfo  ';
end;
