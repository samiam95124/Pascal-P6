{ 

Command line processing

Routines for parsing the command line, options and header files.

Routines:

procedure getcommandline(var cb: string; var l: cmdnum); - Get the shell command
                                                           line.
procedure paroptions; - Parse command line options.
procedure parhdrfil(var f: text; var wasproc: boolean; e: filext); - Parse header file

}

module parcmd(command, output);

{ routines in module }
function bufcommand: char; forward;
procedure getcommand; forward;
function eofcommand: boolean; forward;
function eolncommand: boolean; forward;
procedure readlncommand; forward;
procedure getcommandline; forward;
procedure paroptions; forward;
procedure parhdrfil(var f: text; var wasproc: boolean; view e: string); forward;

{ "fileofy" routines for command line processing.

  These routines implement the command header file by reading from a
  buffer where the command line is stored. The assumption here is that
  there is a fairly simple call to retrieve the command line.

  If it is wanted, the command file primitives can be used to implement
  another type of interface, say, reading from an actual file.

  The command buffer is designed to never be completely full.
  The last two locations indicate:

  maxcmd: end of file
  maxcmd-1: end of line

  These locations are always left as space, thus eoln returns space as
  the standard specifies.
}

const
  maxlin = 2000; { size of source line buffer }
  maxcmd = 2000; { size of command line buffer }
  maxopt = 27;  { number of options }
  optlen = 10;  { maximum length of option words }

type
  lininx = 1..maxlin; { index for source line buffer }
  linbuf = packed array [lininx] of char; { buffer for source lines }
  cmdinx = 1..maxcmd; { index for command line buffer }
  cmdnum = 0..maxcmd; { length of command line buffer }
  cmdbuf = packed array [cmdinx] of char; { buffer for command line }
  optinx = 1..optlen;
  optstr = packed array [optinx] of char;

var
  cmdlin: cmdbuf; { command line }
  cmdlen: cmdnum; { length of command line }
  cmdpos: cmdinx; { current position in command line }
  option: array [1..maxopt] of boolean; { option array }
  options: array [1..maxopt] of boolean; { option was set array }
  opts: array [1..maxopt] of optstr;
  optsl: array [1..maxopt] of optstr;
  incbuf: linbuf; { include file buffer }

private

{ find lower case of character }
function lcase(c: char): char;
begin
  if c in ['A'..'Z'] then c := chr(ord(c)-ord('A')+ord('a'));
  lcase := c
end { lcase };

function bufcommand: char;
begin bufcommand := cmdlin[cmdpos] end;

procedure getcommand;
begin if cmdpos <= cmdlen+1 then cmdpos := cmdpos+1 end;

function eofcommand: boolean;
begin eofcommand := cmdpos > cmdlen+1 end;

function eolncommand: boolean;
begin eolncommand := cmdpos >= cmdlen+1 end;

procedure readlncommand;
begin cmdpos := maxcmd end;

procedure getcommandline;
var i, j: cmdinx; c: char;
procedure putcmd(c: char);
begin
  if i >= maxcmd-1 then begin
    writeln('*** Command line too long');
    halt
  end;
  cmdlin[i] := c; i := i+1
end;
begin
  for j := 1 to maxcmd do cmdlin[j] := ' '; j := 1;
  i := 1; 
  while not eoln(command) do begin read(command, c); putcmd(c) end;
  cmdlen := i-1
end;

procedure paroptions;
var optst: optstr; oni: optinx; oi: 1..maxopt; ch1: char; ii: lininx;
begin
  repeat
    while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
    if bufcommand = '-' then begin
      getcommand;
      if bufcommand = '-' then getcommand;
      if not (bufcommand in ['a'..'z', 'A'..'Z', '_']) then begin
        writeln('*** No valid option found    ');
        halt
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
        else if (bufcommand = '=') and (oi = 27) then begin
          { include string }
          getcommand;
          ii := maxlin; while (incbuf[ii] = ' ') and (ii > 1) do ii := ii-1; 
          if incbuf[ii] <> ' ' then begin
            if ii < maxlin then ii := ii+1;
            if (ii < maxlin-1) then begin incbuf[ii] := ':'; ii := ii+1 end
          end;
          while not (bufcommand in [' ',':',',']) do begin
            if ii = maxlin then begin
              writeln('*** Include path too long');
              halt
            end;
            incbuf[ii] := bufcommand; getcommand; ii := ii+1
          end
        end
      end;
    end;
    while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand
  until bufcommand <> '-'
end;

procedure parhdrfil(var f: text; var wasproc: boolean; view e: string);
const maxfil = 200;
var name: packed array [1..maxfil] of char; i: 1..maxfil; el: 0..maxfil;
procedure getname;
var i: 1..maxfil;
begin
  for i := 1 to maxfil do name[i] := ' ';
  i := 1;
  while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
  if (bufcommand <> ' ') and not eolncommand and not eofcommand then
    while not eolncommand and not eofcommand and (bufcommand <> ' ') do begin
    if i = maxfil then begin
      writeln('*** Filename too long');
      halt
    end;
    name[i] := bufcommand; getcommand; i := i+1
  end
end;
begin
  wasproc := false;
  getname; 
  if name[1] <> ' ' then begin 
    el := 0;
    for i := 1 to maxfil do if name[i] = '.' then el := i;
    if el = 0 then begin
      el := maxfil;
      while (name[el] = ' ') and (el > 1) do el := el-1;
      if (name[el] <> ' ') and (el < maxfil) then el := el+1;
      if el+4 > maxfil then begin
        writeln('*** Filename too long');
        halt
      end;
      if el > 0 then for i := 1 to 4 do name[el+i-1] := e[i]
    end;
    assign(f, name); wasproc := true 
  end
end;

begin
  cmdpos := 1;
  cmdlen := 0;

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
  opts[27] := 'md        ';

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
  optsl[27] := 'modules   ';
end.

