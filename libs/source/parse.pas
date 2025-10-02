{******************************************************************************
*                                                                             *
*                        Character Level Parsing Library                      *
*                                                                             *
*                             2002/8 S. A. Moore                              *
*                                                                             *
* Contains functions to parse at the character level. The user opens a        *
* parsing level that includes a file to read input from. Each parsing level   *
* has a line buffer associated with it, and parsing occurs indirectly from    *
* that buffer. The user can choose whether to parse a single line at a time,  *
* or multiple lines, or a mix of those two. It is possible to nest file       *
* levels, and there is a stacking system to hold the current parsing position *
* for backtracking. The backtrack system does not track further than the      *
* current line, but there is nothing to prevent adding that. Right now, the   *
* behavior is to dump all line positions silently when advancing to the next  *
* line.                                                                       *
*                                                                             *
* Parlib can be used in several common modes:                                 *
*                                                                             *
* - Place a string into the parser, and parse that.                           *
*                                                                             *
* - Open the "command" file as the source, read a single line from it, and    *
*   parse that.                                                               *
*                                                                             *
* - Parse full file.                                                          *
*                                                                             *
* - Parse multiple files, with nesting.                                       *
*                                                                             *
* Parlib is generally used for parsing command lines, but also finds use      *
* parsing simple command files, initalization files, etc.                     *
*                                                                             *
* Parlib might be a building block to a supergeneral parser, but note that    *
* parlib is somewhat Pascal syntax oriented at present (for example in the    *
* numeric radix specifiers). It would be nice to have a BNF defined parsing   *
* facillity ontop parlib.                                                     *
*                                                                             *
* Routines in this module:                                                    *
*                                                                             *
* procedure openpar(var ph: parhan);                                          *
*                                                                             *
*      Opens a parser instance, setting the given handle valid.               *
*                                                                             *
* procedure closepar(ph: parhan);                                             *
*                                                                             *
*      Closes a parser instance, and disconnects its handle.                  *
*                                                                             *
* procedure openstr(ph: parhan; view s: string);                              *
*                                                                             *
*      Opens a string as a parser file.                                       *
*                                                                             *
* procedure openfil(ph: parhan; view fn: string; blen: integer);              *
*                                                                             *
*      Opens a file for parsing.                                              *
*                                                                             *
* procedure closefil(ph: parhan);                                             *
*                                                                             *
*      Close a file or string level for parsing.                              *
*                                                                             *
* function endfil(ph: parhan): boolean;                                       *
*                                                                             *
*      Check end of file reached.                                             *
*                                                                             *
* function endlin(ph: parhan): boolean;                                       *
*                                                                             *
*      Check end of line reached.                                             *
*                                                                             *
* function chkchr(ph: parhan): char;                                          *
*                                                                             *
*      Return next character on current line.                                 *
*                                                                             *
* procedure getchr(ph: parhan);                                               *
*                                                                             *
*      Advance to next character on current line.                             *
*                                                                             *
* procedure skpspc(ph: parhan);                                               *
*                                                                             *
*      Skip spaces on current line.                                           *
*                                                                             *
* procedure getchrl(ph: parhan);                                              *
*                                                                             *
*      Advance to next character, including on next line.                     *
*                                                                             *
* procedure skpspcl(ph: parhan);                                              *
*                                                                             *
*      Skip spaces, including any number of blank lines.                      *
*                                                                             *
* procedure getlin(ph: parhan);                                               *
*                                                                             *
*      Dump current line and advance to next.                                 *
*                                                                             *
* procedure pushpos(ph: parhan);                                              *
*                                                                             *
*      Remember and stack current line position.                              *
*                                                                             *
* procedure poppos(ph: parhan);                                               *
*                                                                             *
*      Restore last line position in stack.                                   *
*                                                                             *
* procedure dmppos(ph: parhan);                                               *
*                                                                             *
*      Dispose of last line position in stack.                                *
*                                                                             *
* function chklab(ph: parhan): boolean;                                       *
*                                                                             *
*      Check next character sequence is valid label start.                    *
*                                                                             *
* procedure parlab(ph: parhan; var l: string; var err: boolean);              *
*                                                                             *
*      Parse label.                                                           *
*                                                                             *
* function chknum(ph: parhan; r: integer): boolean;                           *
*                                                                             *
*      Check next character sequence is valid number start.                   *
*                                                                             *
* procedure parnum(ph: parhan; var i: integer; r: integer; var err: boolean); *
*                                                                             *
*      Parse number, including radix.                                         *
*                                                                             *
* function chkfil(ph: parhan): boolean;                                       * 
*                                                                             *
*      Check next character sequence is valid filename start.                 *
*                                                                             *
* procedure parfil(ph: parhan; var n: string; path: boolean;                  *
*                  var err: boolean);                                         *
*                                                                             *
*      Parse filename.                                                        *
*                                                                             *
* procedure parwrd(ph: parhan; var n: string; var err: boolean);              *
*                                                                             *
*      Parse space delimited word.                                            *
*                                                                             *
* procedure setfch(ph: parhan; view vc: schar);                              *
*                                                                             *
*      Set valid file characters.                                             *
*                                                                             *
* function chkstr(ph: parhan): boolean;                                       *
*                                                                             *
*      Check next character sequence is valid string.                         *
*                                                                             *
* procedure parstr(ph: parhan; var s: string; var err: boolean);              *
*                                                                             *
*      Parse string.                                                          *
*                                                                             *
* procedure prterr(ph: parhan; var ef: text; view es: string; pl: boolean);   *
*                                                                             *
*      Print formatted error with diagnostics.                                *
*                                                                             *
* procedure trclin(ph: parhan; trc: boolean);                                 *
*                                                                             *
*      Print each line read from parse file.                                  *
*                                                                             *
* Notes:                                                                      *
*                                                                             *
* 1. Could add \force sequences to parstr.                                    *
*                                                                             *
* 2. Could add floating point parse routine.                                  *
*                                                                             *
******************************************************************************}

module parse(output, command);

uses strings,
     services;

const maxpar = 8{100}; { number of logical parse handles }

type 

   parhan = 1..maxpar; { logical parse handle }
      
{ functions }

procedure openpar(var ph: parhan); forward;
procedure closepar(ph: parhan); forward;
procedure openstr(ph: parhan; view s: string); forward;
procedure openfil(ph: parhan; view fn: string; blen: integer); forward;
procedure opencommand(ph: parhan; blen: integer); forward;
procedure closefil(ph: parhan); forward;
function endfil(ph: parhan): boolean; forward;
function endlin(ph: parhan): boolean; forward;
function chkchr(ph: parhan): char; forward;
procedure getchr(ph: parhan); forward;
procedure skpspc(ph: parhan); forward;
procedure getchrl(ph: parhan); forward;
procedure skpspcl(ph: parhan); forward;
procedure getlin(ph: parhan); forward;
procedure pushpos(ph: parhan); forward;
procedure poppos(ph: parhan); forward;
procedure dmppos(ph: parhan); forward;
function chklab(ph: parhan): boolean; forward;
procedure parlab(ph: parhan; var l: string; var err: boolean); forward;
function chknum(ph: parhan; r: integer): boolean; forward;
procedure parnum(ph: parhan; var i: integer; r: integer; var err: boolean);
   forward;
function chkfil(ph: parhan): boolean; forward;
procedure parfil(ph: parhan; var n: string; path: boolean; var err: boolean);
   forward;
procedure parwrd(ph: parhan; var w: string; var err: boolean); forward;
procedure setfch(ph: parhan; view vc: schar); forward;
function chkstr(ph: parhan): boolean; forward;
procedure parstr(ph: parhan; var s: string; var err: boolean); forward;
procedure prterr(ph: parhan; var ef: text; view es: string; pl: boolean);
   forward;
procedure trclin(ph: parhan; trc: boolean); forward;

private

type 

   posptr = ^posrec; { position save pointer }
   posrec = record { position save record }

      inx: integer; { saved position }
      next: posptr { next entry }

   end;
   iflptr = ^iflrec; { pointer to file record }
   iflrec = record

      buf:  pstring; { buffer for lines }
      inx:  integer; { index for buffer }
      llen: integer; { length of line }
      name: pstring; { name of file }
      line: integer; { line counter }
      endf: boolean; { end of file encountered }
      flin: boolean; { file entry is for line buffer only }
      f:    text;    { file to read }
      cmd:  boolean; { file is command file }
      sav:  posptr;  { position save stack }
      next: iflptr   { next entry in files stack }

   end;
   parptr = ^parrec; { pointer to parse record }
   parrec = record

      fil: iflptr;  { files stack }
      err: integer; { number of errors }
      fch: schar;   { set of valid file characters }
      trc: boolean  { trace input lines }

   end;

var partab: array [parhan] of parptr; { logical parse blocks }
    parinx: parhan; { index for table }
    valfch: schar; { valid file characters }

{*******************************************************************************

Process string library error

Outputs an error message using the special syslib function, then halts.

*******************************************************************************}

procedure error(view s: string);

var i:     integer; { index for string }
    pream: packed array [1..8] of char; { preamble string }
    p:     pstring; { pointer to string }

begin

   pream := 'Parlib: '; { set preamble }
   new(p, max(s)+8); { get string to hold }
   for i := 1 to 8 do p^[i] := pream[i]; { copy preamble }
   for i := 1 to max(s) do p^[i+8] := s[i]; { copy string }
   !ss_wrterr(p^); { output string }
   writeln(error, p^); { output string }
   dispose(p); { release storage }
   halt { end the run }

end;

{*******************************************************************************

Flush position save stack

Flushes all active entries on the given handle's position save stack.

*******************************************************************************}

procedure flushpos(ph: parhan);

var pp: posptr; { pointer to position saves }

begin

   if partab[ph] = nil then error('Invalid parse handle');
   if partab[ph]^.fil = nil then error('File stack is empty');
   with partab[ph]^.fil^ do begin

      while sav <> nil do begin { free position saves }

         pp := sav; { save top entry }
         sav := sav^.next; { gap out entry }
         dispose(pp) { free that }

      end

   end

end;

{*******************************************************************************

Get next line line handle

Loads the next line from the parser file. If the parse is for a single line,
it is simply set to end of file. Otherwise, the next line is loaded. If the
trace mode is on, the line is output.

Getlin is accessable externally because it solves a problem in line oriented
mode. If the user cares about each line, including blank lines, then getchrl
does not work to skip to the next line, because it will skip any blank lines
until a character is found, which is the way it must work.

Getlin serves both as a readln and also to discard the present line.

*******************************************************************************}

procedure getlin(ph: parhan);

var ovf: boolean;

begin

   if partab[ph] = nil then error('Invalid parse handle');
   with partab[ph]^ do begin

      if fil = nil then error('File stack is empty');
      with fil^ do begin

         flushpos(ph); { purge any pending line positions }
         if flin then endf := true { set end of file on single buffer }
         else begin { is a file }

            if eof(f) then endf := true { eof encountered }
            else begin { read the line }

               if cmd then begin { its the command file }

                  reads(command, buf^, ovf); { read the first line in }
                  readln(command)

               end else begin { its a file }

                  reads(f, buf^, ovf); { read the first line in }
                  readln(f)

               end;
               if ovf then error('Line too long for input buffer');
               llen := len(buf^); { set length }
               inx := 1; { set 1st character position }
               line := line+1; { increment line }
               if trc and not flin then begin 

                  { line tracing is on, and not single line buffer }
                  write('[');
                  write(output, name^:0); { print filename }
                  write(':', line:1, '] ');
                  write(output, buf^:0); { print line }
                  writeln

               end

            end

         end

      end

   end

end;

{******************************************************************************

Set up handle

Validates the given handle is open for parsing, contains an open file, and
reads the first line from the file to the buffer if the buffer is not already
loaded.

******************************************************************************}

procedure sethan(ph: parhan);

begin

   if partab[ph] = nil then error('Invalid parse handle');
   with partab[ph]^ do begin

      if fil = nil then error('File stack is empty');
      if fil^.inx = 0 then getlin(ph) { file buffer is empty, load it }

   end

end;

{******************************************************************************

Open parser

Opens a parser for duty by local handle. If there is already a parser open at
that handle, it is automatically disposed of.

******************************************************************************}

procedure openpar(var ph: parhan);

var pi: parhan;    { parser index }
    fi: 0..maxpar; { found parser index }

begin

   { find a parser handle }
   fi := 0; { set none found }
   for pi := 1 to maxpar do if partab[pi] = nil then fi := pi; { found }
   if fi = 0 then error('Parse handle table is full');
   ph := fi; { place parse handle }
   new(partab[ph]); { create parser entry }
   partab[ph]^.fil := nil; { clear files stack }
   partab[ph]^.err := 0; { clear error count }
   partab[ph]^.fch := valfch; { place set of file parse characters }
   partab[ph]^.trc := false { set no trace }

end;

{******************************************************************************

Close parser

Closes the given parser handle. Frees all data and closes all files associated
with the parser entry.

******************************************************************************}

procedure closepar(ph: parhan);

var fp: iflptr; { pointer to file entry }

begin

   if partab[ph] = nil then error('Invalid parse handle');
   while partab[ph]^.fil <> nil do with partab[ph]^ do begin

      { free file entries }
      with fil^ do begin

         if not flin then close(f); { close the file }
         if buf <> nil then dispose(buf); { release buffer }
         if name <> nil then dispose(name) { release name }

      end;
      flushpos(ph); { purge any line positions }
      fp := fil; { save top entry }
      fil := fil^.next; { gap out entry }
      dispose(fp) { free that }

   end;
   dispose(partab[ph]); { release parse entry }
   partab[ph] := nil { and flag free }

end;

{******************************************************************************

Open file by name for parsing

Opens a new file level for processing. A new file entry is stacked and
intialized.

******************************************************************************}

procedure openfil(ph: parhan; view fn: string; blen: integer);

var fp: iflptr; { pointer to file entry }

begin

   if partab[ph] = nil then error('Invalid parse handle');
   new(fp); { get new file entry }
   with fp^ do begin { intalize entry }

      new(buf, blen); { create the line buffer }
      clears(buf^); { clear the buffer }
      inx := 0; { flag no line read }
      name := copy(fn); { create the filename }
      line := 0; { set no line active }
      endf := false; { set no eof }
      flin := false; { set not a buffer only entry }
      sav := nil; { clear position stack }
      cmd := false; { set not command file }
{ problem: the _command file could be opened, it does not exist. fix is
  probally for paslib to recognize that }
{      if not exists(fn) then error('File does not exist');}
      assign(f, fn); { activate file for reading }
      reset(f)

   end;
   fp^.next := partab[ph]^.fil; { push file entry onto stack }
   partab[ph]^.fil := fp

end;

{******************************************************************************

Open command file for parsing

Opens a new file level for processing. A new file entry is stacked and
intialized.

******************************************************************************}

procedure opencommand(ph: parhan; blen: integer);

var fp: iflptr; { pointer to file entry }

begin

   if partab[ph] = nil then error('Invalid parse handle');
   new(fp); { get new file entry }
   with fp^ do begin { intalize entry }

      new(buf, blen); { create the line buffer }
      clears(buf^); { clear the buffer }
      inx := 0; { flag no line read }
      name := copy('<predefined>'); { create the filename }
      line := 0; { set no line active }
      endf := false; { set no eof }
      flin := false; { set not a buffer only entry }
      sav := nil; { clear position stack }
      cmd := true { set command file }

   end;
   fp^.next := partab[ph]^.fil; { push file entry onto stack }
   partab[ph]^.fil := fp

end;

{******************************************************************************

Set line for parsing

Places a string from the caller as a buffer to be parsed.

******************************************************************************}

procedure openstr(ph: parhan; view s: string);

var fp: iflptr; { pointer to file entry }

begin

   if partab[ph] = nil then error('Invalid parse handle');
   new(fp); { get new file entry }
   with fp^ do begin { intalize entry }

      buf := copy(s); { place the string to parse }
      llen := len(buf^);
      inx := 1; { set 1st character }
      name := nil; { set no filename }
      line := 0; { set no line active }
      endf := false; { set no eof }
      flin := true; { set not a buffer only entry }
      sav := nil; { clear position stack }
      cmd := false { set not command file }

   end;
   fp^.next := partab[ph]^.fil; { push file entry onto stack }
   partab[ph]^.fil := fp

end;

{******************************************************************************

Close file level for parsing

Closes the top level file entry and frees it. Also works on buffer only level.

******************************************************************************}

procedure closefil(ph: parhan);

var fp: iflptr; { pointer to file entry }

begin

   if partab[ph] = nil then error('Invalid parse handle');
   fp := partab[ph]^.fil; { index top file }
   if fp = nil then error('File stack is empty');
   if not fp^.flin then close(fp^.f); { close the file }
   flushpos(ph); { purge any pending line positions }
   partab[ph]^.fil := partab[ph]^.fil^.next; { gap out of list }
   dispose(fp^.buf); { dispose of line buffer }
   if fp^.name <> nil then dispose(fp^.name); { dispose of filename }
   dispose(fp); { release entry }
   partab[ph]^.fil := nil { clear entry } 

end;

{******************************************************************************

Push present line position

Saves and stacks the current line position.

******************************************************************************}

procedure pushpos(ph: parhan);

var pp: posptr; { position entry pointer }

begin

   sethan(ph); { make sure handle setup }
   with partab[ph]^.fil^ do begin { create save entry }

      new(pp); { get a new position entry }
      pp^.inx := inx; { place current position }
      pp^.next := sav; { push onto list }
      sav := pp

   end

end;      

{******************************************************************************

Pop old line position

Restores old line position.

******************************************************************************}

procedure poppos(ph: parhan);

var pp: posptr; { position entry pointer }

begin

   sethan(ph); { make sure handle setup }
   with partab[ph]^.fil^ do begin { create save entry }

      if sav = nil then error('No position entry to return to');
      pp := sav; { get top entry }
      sav := sav^.next; { gap out of list }
      inx := pp^.inx; { restore old position }
      dispose(pp) { release entry }

   end

end;      

{******************************************************************************

Dump old line position

Removes the top line position save without restoring it.

******************************************************************************}

procedure dmppos(ph: parhan);

var pp: posptr; { position entry pointer }

begin

   sethan(ph); { make sure handle setup }
   with partab[ph]^.fil^ do begin { create save entry }

      if sav = nil then error('No position entry to return to');
      pp := sav; { get top entry }
      sav := sav^.next; { gap out of list }
      dispose(pp) { release entry }

   end

end;      

{******************************************************************************

Check end of file

Checks if the end of the buffer has been reached.

******************************************************************************}

function endfil(ph: parhan): boolean;

begin

   sethan(ph); { make sure handle setup }
   with partab[ph]^.fil^ do endfil := endf { return eof status }

end;

{******************************************************************************

Check end of line

Checks if the end of the buffer has been reached.

******************************************************************************}

function endlin(ph: parhan): boolean;

begin

   sethan(ph); { make sure handle setup }
   with partab[ph]^.fil^ do
      endlin := inx > llen { input pointer past end of line }

end;

{******************************************************************************

Check next buffer line character

Returns the next character in the buffer line, or a space if past the end.

******************************************************************************}

function chkchr(ph: parhan): char;

begin

   sethan(ph); { make sure handle setup }
   with partab[ph]^.fil^ do
      if not endlin(ph) then chkchr := buf^[inx] { return current character }
      else chkchr := ' ' { else return space }

end;

{******************************************************************************

Get next character

If not at the end of the buffer line, the line position is advanced one 
character.

******************************************************************************}

procedure getchr(ph: parhan);

begin

   sethan(ph); { make sure handle setup }
   with partab[ph]^.fil^ do
      if not endlin(ph) then inx := inx+1 { advance position if not end }

end;

{******************************************************************************

Get next character multiline

If not at the end of file, advances the character position by one. If the
position is at the end of line, the next buffer from the file is read in
and the position advanced.

Because getchrl allows the end of the current line to be surpassed, the net
effect is that end of lines appear as spaces when the file is continuously
read using chkchr/getchrl calls.

******************************************************************************}

procedure getchrl(ph: parhan);

begin

   { perform normal process if not end of file or line }
   if not endfil(ph) and not endlin(ph) then getchr(ph)
   else begin { must read new line or stop }

      sethan(ph); { make sure handle setup }
      with partab[ph]^.fil^ do begin

         flushpos(ph); { purge any pending line positions }
         if flin then endf := true { set end of file on single buffer }
         else begin

            { read lines until the line is not empty }
            while endlin(ph) and not endfil(ph) do { read lines }
               if eof(f) then endf := true else getlin(ph) { not end of file }

         end

      end

   end

end;

{******************************************************************************

Skip spaces

Skips spaces in current buffer line. The "whitespace" option allows any
character at space or below (control characters) to be skipped.

******************************************************************************}

procedure skpspc(ph: parhan);

begin

   { skip spaces, not end }
   while (chkchr(ph) = ' ') and not endlin(ph) do getchr(ph)

end;

{******************************************************************************

Skip spaces multiline

Skips spaces over multiple lines. The "whitespace" option allows any
character at space or below (control characters) to be skipped.

******************************************************************************}

procedure skpspcl(ph: parhan);

begin

   { skip spaces, not end }
   while (chkchr(ph) = ' ') and not endfil(ph) do getchrl(ph)

end;

{******************************************************************************

Check label

Checks a label leader is present. This is a character in the range:

    '_'/'a'..'z'

******************************************************************************}

function chklab(ph: parhan): boolean;

begin

   skpspc(ph); { skip spaces }
   chklab := chkchr(ph) in ['_', 'a'..'z', 'A'..'Z']

end;

{******************************************************************************

Parse label

Parses a label, which is:

    '_'/'a'..'z' ['_', '0'..'9', 'a'..'z']...

The label is returned in the general label buffer labbuf.

******************************************************************************}

procedure parlab(ph: parhan; var l: string; var err: boolean);

var i: integer; { index for label }

begin

   clears(l); { clear label buffer }
   i := 1; { clear index }
   err := false; { clear overflow }
   skpspc(ph); { skip spaces }
   if not (chkchr(ph) in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) then err := true
   else
      while (chkchr(ph) in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) and not err do
         begin

      { parse label characters }
      if i > max(l) then err := true else begin

         l[i] := chkchr(ph); { place character }
         i := i + 1; { next character }
         getchr(ph) { skip }

      end

   end

end;

{******************************************************************************

Check number

Checks a number leader is present. This is a character in the range:

    decimal       '0'..'9'
    hexadecimal   '0'..'9', 'a'..'f'
    octal         '0'..'7'
    binary        '0'..'1'

With '$', '&' and '$' added if the pr (parse radix) flag is set.

******************************************************************************}

function chknum(ph: parhan; r: integer): boolean;

var f: boolean; { found/not found flag }

begin

   if not (r in [2, 8, 10, 16]) then error('Bad radix specification');
   f := true;
   skpspc(ph); { skip spaces }
   if chkchr(ph) = '%' then r := 2 { binary }
   else if chkchr(ph) = '&' then r := 8 { octal }
   else if chkchr(ph) = '$' then r := 16 { hex }
   else if r = 2 then f := chkchr(ph) in ['0'..'1']
   else if r = 8 then f := chkchr(ph) in ['0'..'7']
   else if r = 10 then f := chkchr(ph) in ['0'..'9']
   else f := chkchr(ph) in ['0'..'9', 'a'..'f', 'A'..'F'];

   chknum := f

end;

{******************************************************************************

Parse and convert numeric

Parses and converts the following:

     [radix specification] ['0'..'9', 'a'..'z', 'A'..'Z']...

Where the radix specifier is:

     % - Binary
     & - Octal
     $ - hexadecimal
     none - Decimal

Using the given radix, any digits are processed to yeild an integer unsigned
result. Leading spaces are skipped. Overflow isn't now but should be flagged 
as an error. No spaces are allowed anywhere in the format.
The default radix can be specified.

Returns err set on overflow or bad number format. Use chknum to remove the
latter error.

Does not skip spaces.

******************************************************************************}

procedure parnum(ph: parhan; var i: integer; r: integer; var err: boolean);

var v: 0..36; { integer value holder, enough for 10+(a-z) }

begin

   err := false; { set no error }
   if not (r in [2, 8, 10, 16]) then error('Bad radix specification');
   skpspc(ph); { skip spaces }
   i := 0; { initalize result }
   if chkchr(ph) = '%' then begin r := 2; getchr(ph) end { binary }
   else if chkchr(ph) = '&' then begin r := 8; getchr(ph) end { octal }
   else if chkchr(ph) = '$' then begin r := 16; getchr(ph) end; { hex }
   if not (((chkchr(ph) in ['0'..'9', 'a'..'z', 'A'..'Z']) and (r = 16)) or
           (chkchr(ph) in ['0'..'9'])) then err := true
   else while ((((chkchr(ph) in ['a'..'z', 'A'..'Z']) and (r = 16)) or
               (chkchr(ph) in ['0'..'9']))) and not err do
   begin { parse digits }

      { convert '0'..'9' }
      if (chkchr(ph) in ['0'..'9']) then v := ord(chkchr(ph))-ord('0')
      else v := ord(lcase(chkchr(ph)))-ord('a')+10; { convert 'a'..'z' }
      if v >= r then err := true else begin
       
         { check for overflow }
         if ((i > maxint div r) or 
            ((i = maxint div r) and (v > maxint mod r))) then
            err := true { overflows }
         else begin

            i := i*r+v; { scale and add in }
            getchr(ph) { next }

         end

      end

   end

end;

{******************************************************************************

Check filename

Checks a filename leader is present. This is a character in the system
given file character range.

******************************************************************************}

function chkfil(ph: parhan): boolean;

begin

   sethan(ph); { set handle }
   skpspc(ph); { skip spaces }
   chkfil := chkchr(ph) in partab[ph]^.fch

end;

{******************************************************************************

Parse filename

Gets a filename from the command line and validates it. Sets err if either
no valid filname is found, or if the filename overflows the buffer given.
Use chkfil to remove the latter error.

Note that many systems allow a filename to be quoted to use all characters.
To perform this, use parstr instead.

******************************************************************************}

procedure parfil(ph: parhan; var n: string; path: boolean; var err: boolean);

var fi: integer; { index for filename }

begin

   sethan(ph); { set handle }
   clears(n); { clear filename }
   fi := 1; { set 1st character }
   err := false; { set no error }
   skpspc(ph); { skip spaces }
   if not (chkchr(ph) in partab[ph]^.fch) then err := true
   else while (chkchr(ph) in partab[ph]^.fch) and not err do begin

      if fi > max(n) then err := true { overflow }
      else begin

         n[fi] := chkchr(ph); { place }
         fi := fi+1; { next character }
         getchr(ph) { skip to next }

      end

   end;
   if not err then begin

      if path then err := not validpath(n) { check and error on pathname }
      else err := not validfile(n) { check and error on filename }

   end

end;

{******************************************************************************

Parse word from parser instance

Parses a space or eoln delimited word.

******************************************************************************}

procedure parwrd(ph: parhan; var w: string; var err: boolean);

var wi: integer;

begin

   sethan(ph); { set handle }
   clears(w); { clear word }
   wi := 1; { set 1st character }
   err := false; { set no error }
   skpspc(ph); { skip spaces }
   while (chkchr(ph) <> ' ') and not endlin(ph) and not err do begin

      { place word character }
      if wi > max(w) then err := true else begin

         w[wi] := chkchr(ph); { place }
         wi := wi+1; { next }
         getchr(ph) { skip to next }

      end

   end

end;

{******************************************************************************

Set filename parsing characters

Sets the character set to parse files by.

******************************************************************************}

procedure setfch(ph: parhan; view vc: schar);

begin

   if partab[ph] = nil then error('Invalid parse handle');
   partab[ph]^.fch := vc { place parsing character set }

end;

{******************************************************************************

Check string

Checks if a string exists at the current parsing position. Right now this is
a very simple matter of checking if a quote from the given set exists, but
it might get more complex in the future.

******************************************************************************}

function chkstr(ph: parhan): boolean;

begin

   skpspc(ph); { skip spaces }
   chkstr := chkchr(ph) = '"'

end;

{******************************************************************************

Parse string

Parses a string, starting and stopping with '" quotes, and allowing quote
images in the string (""). Returns error set if no string is found, or if the
string buffer overflows. The first case can be detected with chkstr.

******************************************************************************}

procedure parstr(ph: parhan; var s: string; var err: boolean);

var i: integer; { index for string }
    f: boolean; { string terminator flag }

begin

   clears(s); { clear result }
   err := false; { set no error }
   skpspc(ph); { skip spaces }
   if chkchr(ph) <> '"' then err := true { no string found }
   else begin

      getchr(ph); { skip starting quote }
      i := 1; { set start of string }
      f := false; { set not end }
      repeat { gather string data }

         if chkchr(ph) = '"' then begin

            getchr(ph); { skip (possible) end quote }
            if chkchr(ph) <> '"' then f := true { flag end of string }

         end;
         if not f then begin { string body character }

            { check and flag string overflow }
            if i > max(s) then err := true { buffer overflow }
            else begin

               s[i] := chkchr(ph); { place character }
               getchr(ph); { next }
               i := i+1 { count }

            end

         end

      until f or err { end of string }

   end

end;

{******************************************************************************

Set input line tracing mode

Sets or resets the input line tracing mode. If the mode is on, all lines read
are output to standard output.

******************************************************************************}

procedure trclin(ph: parhan; trc: boolean);

begin

   if partab[ph] = nil then error('Invalid parse handle');
   partab[ph]^.trc := trc { set tracing status }

end;

{******************************************************************************

Print error

Print parse formatted error. Prints an error message of the following format:

"current line"
           ^
[file:line.char,errcnt] "error message"

The first two lines are the line being parsed, and an up arrow to the current
character position. This part of the display can be removed or inserted by
the pl (print line) flag.

The last line, which allways appears, is the filename, line number and
character position that is current. If the current line comes from a buffer,
the filename and line will not appear. The reasons for not printing this
information include personal taste, and the fact that the line may have already
been printed, perhaps as a source listing or command line entry.

Note that no line return preceeds the sequence. The caller is responsible for
making sure the print position is on a new line.

******************************************************************************}

procedure prterr(ph: parhan; var ef: text; view es: string; pl: boolean);

begin

   sethan(ph); { set up handle }
   partab[ph]^.err := partab[ph]^.err+1; { count errors }
   with partab[ph]^.fil^ do begin { create save entry }

      if pl then begin { print source line display }

         write(ef, buf^:0); { print source line }
         writeln(ef);
         writeln(ef, '^': inx) { print indicator }

      end;
      { print error line }
      write(ef, '[');
      if not flin then begin { print filename and line }

         write(ef, name^:0); { print filename }
         write(ef, ':', line:1, '.');

      end;
      writeln(ef, inx:1, ',', partab[ph]^.err:1, '] ', es)

   end

end;

{ start block }

begin

   filchr(valfch); { get the filename valid characters }
   for parinx := 1 to maxpar do partab[parinx] := nil { clear parse block list }

end;

{ end block }

begin

   { dispose of all parse blocks to clean up }
   for parinx := 1 to maxpar do if partab[parinx] <> nil then closepar(parinx)

end.
