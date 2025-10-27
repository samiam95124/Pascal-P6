{******************************************************************************
*                                                                             *
* SCANNER MODULE                                                              *
*                                                                             *
* Scans IP Pascal tolkens from a file and returns them. Does nothing with     *
* "uses" files, and has no error recovery.                                    *
* The scanner module is a general purpose version of the scanner built in to  *
* the general compiler. It is usefull for program analisis tasks, such as     *
* symbols/crossreference generators, and convertion tasks, such as a          *
* prettyprinter.                                                              *
* The scanner buffers up its input in terms of lines. This is required        *
* because some of the tolkens require backtracking to recognise.              *
* No ANSI status is recognized. The scanner allways works in full IP mode.    *
*                                                                             *
* There are two calls for the scanner module:                                 *
*                                                                             *
* iniscn(var f: text);                                                        *
*                                                                             *
* Initalizes the scanner and reads the first line from the input file. The    *
* caller may check if the file is null (eof) before the call.                 *
*                                                                             *
* gettlk(var f: text);                                                        *
*                                                                             *
* Gets the next tolken from the input, and places that in the next tolken     *
* buffers, as follows:                                                        *
*                                                                             *
* nxttlk                                                                      *
*                                                                             *
* Returns the next tolken code. This is one of the tolkens from the "tolken"  *
* type.                                                                       *
*                                                                             *
* nxtlab                                                                      *
*                                                                             *
* Returns the next label, if the tolken is a symbol, or the next string,      *
* if the next tolken is a string. Strings have their enclosing quotes         *
* removed.                                                                    *
* Other tolkens will also have their equivalent forms placed in the nxtlab    *
* buffer, including integers and reals.                                       *
*                                                                             *
* nxtlen                                                                      *
*                                                                             *
* Returns the length of a string for a string tolken.                         *
*                                                                             *
* nxtint                                                                      *
*                                                                             *
* Returns an integer if the next tolken is an integer.                        *
*                                                                             *
* nxtflt                                                                      *
*                                                                             *
* Returns a floating point if the next tolken is a real.                      *
*                                                                             *
******************************************************************************}

module scanner(output);

uses strings, { string functions }
     restbl,  { reserved word hash table }
     spctbl,  { symbols hash table }
     tolkens; { scanner tolkens }

const

maxstr = 200; { maximum length of string/symbol }
maxlin = 1000; { maximum size of input line }

type

lininx = 1..maxlin;  { index for text line }
linbuf = packed array [lininx] of char; { a text line }
fcbptr = ^fcbrec; { file control block pointer }
fcbrec = record { file control block }

   name:   pstring;  { name of file being processed }
   inpfil: text;    { input file }
   lininp: linbuf;  { input line buffer }
   linptr: integer; { index for line }
   linend: integer; { end of line without blanks }
   lincnt: integer; { line count in source file }
   nxttlk: tolken;  { next tolken }
   nxtlab: packed array [1..maxstr] of char; { next label/string }
   nxtlen: integer; { next length of string }
   nxtint: integer; { next integer }
   nxtflt: real     { next real }

end;

var 

   fansi: boolean; { standard Pascal mode }

procedure opnscn(var f: fcbptr; view n: string); forward;
procedure clsscn(f: fcbptr); forward;
procedure gettlk(f: fcbptr); forward;

private

const

maxexp  = 308; { maximum exponent of real }

type

labinx = 1..maxstr; { index for label }
errcod = (einpltl,  { Input line too large }
          einvdig,  { Invalid digit }
          edigbrx,  { Digit beyond radix }
          enumovf,  { Numeric overflow }
          einvrft,  { Invalid real format }
          eexptl,   { Exponent too large }
          enoldqt,  { No Leading quote }
          estrftl,  { String force number greater than 255 }
          euntstr,  { Unterminated string }
          enulstr,  { Null string }
          einvsch,  { Invalid symbol/character }
          euntcmt); { Unterminated comment }

var

deftbl: array [tolken] of pstring; { tolken definition strings }
ci:     chrinx;
ri:     resinx;
li:     labinx;

{******************************************************************************

Process scanner error

Prints an error by code, then halts the program.

******************************************************************************}

procedure error(f: fcbptr; e: errcod);

begin

   writeln(f^.lininp:*); { output source line }
   writeln('^': f^.linptr); { output marker }
   write('*** ', f^.name^, ' [', f^.lincnt:1, ':', f^.linptr:1, '] ');
   case e of { error }

      einpltl: writeln('Input line too large');
      einvdig: writeln('Invalid digit');
      edigbrx: writeln('Digit beyond radix');
      enumovf: writeln('Numeric overflow');
      einvrft: writeln('Invalid real format');
      eexptl:  writeln('Exponent too large');
      enoldqt: writeln('No Leading quote');
      estrftl: writeln('String force number greater than 255');
      euntstr: writeln('Unterminated string');
      enulstr: writeln('Null string');
      einvsch: writeln('Invalid symbol/character');
      euntcmt: writeln('Unterminated comment');

   end;
   halt

end;

{******************************************************************************

Find label hash function

Finds a hash function for the given label. The maximum specifies the maximum
value desired from the hash generator. The return value will be between
1 and the max. The "add" parameter is a "stirring" parameter that just changes
the hash value to a different set of values. This is used to optimize fixed
tables, done using an external generator program. See the program for details,
but the basic idea is that we will find an add that gives the optimum set of
hash values for a fixed set of labels.
Note that for dynamic tables, the add parameter can be left to 0.

******************************************************************************}

function hash(view s:    string;  { label to find hash for }
                   add:  integer; { stirring parameter }
                   maxv: integer) { maximum value returned }
             : integer;          { return hash }

var i, r : integer;

begin

   r := 0;
   for i := 1 to max(s) do
      if s[i] <> ' ' then begin

      r := r + ord(lcase(s[i])) + add;

   end;
   hash := r mod maxv + 1

end;

{******************************************************************************

Get next input line

Gets a single line from the given file.

******************************************************************************}

procedure getlin(var f: fcbptr); { file control block }

var ovf: boolean; { overflow flag }

begin

   if not eof(f^.inpfil) then begin { not at file end }

      reads(f^.inpfil, f^.lininp, ovf); { get next line }
      readln(f^.inpfil);
      if ovf then error(f, einpltl); { overflow }
      f^.linptr := 1; { reset line pointer }
      f^.linend := len(f^.lininp); { set end of line }
      f^.lincnt := f^.lincnt+1 { count lines }

   end;
{;if not eof(f^.inpfil) then begin
;write(output, f^.linbuf:* );
;writeln;
;end;}

end;

{******************************************************************************

Check end of line

Simply checks if the input position is beyond the current end of line.

******************************************************************************}

function endlin(f: fcbptr) { file control block }
                : boolean; { end of line status }

begin

   endlin := f^.linptr > f^.linend

end;

{******************************************************************************

Check eof

Checks if the end of the input buffer and the source file has been reached.

******************************************************************************}

function seof(var f: fcbptr) { file control block }
              : boolean;

begin

  { true eof is the end of line, and end of file } 
  seof := endlin(f) and eof(f^.inpfil)

end;

{******************************************************************************

Check next input character

The next character in the input buffer is returned. No advance is made from the
current position (succesive calls to this procedure will yeild the same
character).

******************************************************************************}

function chkchr(f: fcbptr) { file control block }
                : char;    { current input character }

var c: char; { result }

begin

   if endlin(f) then c := ' ' { just return endless spaces }
   { else return the next character at the input pointer }
   else c := f^.lininp[f^.linptr];
   chkchr := c { return result }

end;

{******************************************************************************

Skip input character

Causes the current input character to be skipped, so that the next chkchr call
will return the next character. If we are at the end of the line, no action
will take place (will not advance beyond end of line).

******************************************************************************}

procedure getchr(f: fcbptr); { file control block }

begin

   if f^.linptr <= f^.linend then { process advance }
      f^.linptr := f^.linptr+1 { advance one character }

end;

{******************************************************************************

Skip input spaces or controls

Skips the input position past any spaces or controls. Will skip the end of
line, loading the next line from the input. The view of the input is for each
line to be terminated by an infinite series of blanks, which only this routine
will cross.

******************************************************************************}

procedure skpspc(var f: fcbptr); { file control block }

begin

  repeat

     { skip any spaces }
     while (chkchr(f) <= ' ') and not endlin(f) do getchr(f);
     if endlin(f) then getlin(f) { get a new line }

   until seof(f) or (chkchr(f) > ' ') { eof or non-space }

end;                  

{******************************************************************************

Recognize control memnonic

Attempts to recognize a control memnonic at the present position. If found, the
equivalent control character is returned, else just returns space. The input
position is left past the sequence.

******************************************************************************}

procedure conchr(    f: fcbptr; { file control block }
                 var c: char);  { control character }

var s:   packed array [1..4] of char;  { holding cell }
    r:   0..1020;                      { hash calculator holding (4*c) }
    chn: 0..35;                        { index of control characters }
    i:   1..4;                         { index for cell }
    ips: array [1..4] of integer;   { line pointer saves }

procedure lookup; { attempt lookup of memnonic }

var i: 1..4; { index for cell }

begin

   { find hash }
   r := 0;
   for i := 1 to 4 do r := r + ord(s[i]);
   chn := r mod 35 + 1;
   c := ' '; { set none found }
   while (c = ' ') and (chn <> 0) do case chn of { hash index }

      21: begin if s = 'nul\00'   then c := chr(0);   chn := 0  end;
      16: begin if s = 'soh\00'   then c := chr(1);   chn := 15 end;
      2:  begin if s = 'stx\00'   then c := chr(2);   chn := 0  end;
      23: begin if s = 'etx\00'   then c := chr(3);   chn := 33 end;
      14: begin if s = 'eot\00'   then c := chr(4);   chn := 0  end;
      10: begin if s = 'enq\00'   then c := chr(5);   chn := 0  end;
      24: begin if s = 'ack\00'   then c := chr(6);   chn := 0  end;
      28: begin if s = 'bel\00'   then c := chr(7);   chn := 0  end;
      4:  begin if s = 'bs\00\00' then c := chr(8);   chn := 3  end;
      11: begin if s = 'ht\00\00' then c := chr(9);   chn := 12 end;
      1:  begin if s = 'lf\00\00' then c := chr(10);  chn := 18 end;
      25: begin if s = 'vt\00\00' then c := chr(11);  chn := 0  end;
      30: begin if s = 'ff\00\00' then c := chr(12);  chn := 29 end;
      3:  begin if s = 'cr\00\00' then c := chr(13);  chn := 13 end;
      17: begin if s = 'so\00\00' then c := chr(14);  chn := 0  end;
      12: begin if s = 'si\00\00' then c := chr(15);  chn := 0  end;
      29: begin if s = 'dle\00'   then c := chr(16);  chn := 34 end;
      13: begin if s = 'dc1\00'   then c := chr(17);  chn := 0  end;
      27: begin if s = 'xon\00'   then c := chr(17);  chn := 26 end;
      5:  begin if s = 'dc2\00'   then c := chr(18);  chn := 0  end;
      6:  begin if s = 'dc3\00'   then c := chr(19);  chn := 0  end;
      15: begin if s = 'xoff'     then c := chr(19);  chn := 31 end;
      7:  begin if s = 'dc4\00'   then c := chr(20);  chn := 0  end;
      35: begin if s = 'nak\00'   then c := chr(21);  chn := 0  end;
      32: begin if s = 'syn\00'   then c := chr(22);  chn := 0  end;
      18: begin if s = 'etb\00'   then c := chr(23);  chn := 19 end;
      26: begin if s = 'can\00'   then c := chr(24);  chn := 0  end;
      22: begin if s = 'em\00\00' then c := chr(25);  chn := 0  end;
      31: begin if s = 'sub\00'   then c := chr(26);  chn := 0  end;
      19: begin if s = 'esc\00'   then c := chr(27);  chn := 22 end;
      8:  begin if s = 'fs\00\00' then c := chr(28);  chn := 0  end;
      9:  begin if s = 'gs\00\00' then c := chr(29);  chn := 0  end;
      20: begin if s = 'rs\00\00' then c := chr(30);  chn := 0  end;
      33: begin if s = 'us\00\00' then c := chr(31);  chn := 0  end;
      34: begin if s = 'del\00'   then c := chr(127); chn := 0  end

   end

end;

begin

   { load cell }
   for i := 1 to 4 do begin

      ips[i] := f^.linptr; { save input at that }
      { insure we aren't fooled by \00 }
      if chkchr(f) = '\00' then s[i] := '?'
      else s[i] := chkchr(f);
      getchr(f)

   end;
   lookup; { try 4 characters }
   if c = ' ' then begin

      s[4] := '\00'; { knock out character }
      lookup; { try 3 characters }
      if c = ' ' then begin

         s[3] := '\00'; { knock out character }
         lookup; { try 2 characters }
         if c <> ' ' then f^.linptr := ips[3] { restore to that }

      end else f^.linptr := ips[4] { restore to that }

   end;
   if c = ' ' then f^.linptr := ips[1] { if not found, restore position }

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

Using the given radix, any digits are processed to yeild a word unsigned
result. Leading spaces are skipped. Overflow is checked and flagged as an
error. Overflowing exponents is also checked. No spaces are allowed anywhere in
the format. If the real flag is set, when a '.' or 'e' is encountered, the
integer will be promoted to a floating point value. Whether or not any further
indicator characters are to be looked for is required since this routine can be
used inside a string.
Note that in case of a parsing error, the rest of the number is skipped (as
possible), and a zero is returned.
Note that the overflow check can be bypassed by the 'nooverflow' option. The
purpose of this is to allow bootstrapping to a larger word size. For instance,
if we design a back end coder that generates a larger word size for integer,
then we may recode all compiler modules to use that new size. We must then
compile a parser without overflow checks, so that a new parser can be compiled
with larger overflow checking.

******************************************************************************}

procedure parnum(var f:          fcbptr;   { file to read from }
                     searchreal: boolean); { real number search flag }

var cs:   integer;  { save for command line pointer }
    c:    char;
    r:    1..16;   { radix }
    v:    0..36;   { integer value holder, enough for 10+(a-z) }
    rm:   boolean; { radix mark encountered flag }
    exp:  integer; { exponent of real }
    sgn:  integer; { sign holder }
    zero: boolean; { number consists of zeros }
    p:    real;    { power }
    outb: packed array [1..maxstr] of char; { output buffer }
    outi: integer; { output buffer index }

{ find power of ten effciently }

function pwrten(e: integer): real;

var t: real; { accumulator }
    p: real; { current power }

begin

   p := 1.0e+1; { set 1st power }
   t := 1.0; { initalize result }
   repeat 

      if odd(e) then t := t*p; { if bit set, add this power }
      e := e div 2; { index next bit }
      p := sqr(p) { find next power }

   until e = 0;
   pwrten := t

end;

{ move character to output buffer }

procedure getchrb;

begin

   outb[outi] := chkchr(f); { place character in buffer }
   outi := outi+1; { next character }
   getchr(f) { and get that }

end;

begin

   outi := 1; { index 1st output character }
   clears(outb); { clear buffer }
   skpspc(f); { skip spaces }
   rm := false; { set no radix mark }
   r := 10; { set default radix decimal}
   exp := 0; { clear real exponent }
   f^.nxtint := 0; { initalize result }
   { check binary }
   if chkchr(f) = '%' then { binary }
      begin r := 2; rm := true; getchrb end
   { check octal }
   else if chkchr(f) = '&' then { octal }
      begin r := 8; rm := true; getchrb end
   { check hexadecimal }
   else if chkchr(f) = '$' then { hexadecimal }
      begin r := 16; rm := true; getchrb end;
   if not (((chkchr(f) in ['0'..'9', 'a'..'z', 'A'..'Z']) and (r = 16)) or
           (chkchr(f) in ['0'..'9'])) then error(f, einvdig);
   while (((chkchr(f) in ['a'..'z', 'A'..'Z']) and (r = 16)) or
      (chkchr(f) in ['0'..'9'])) do begin { parse digits }

         { count significant digits to exponent (used on real only) }
         if (chkchr(f) <> '0') or (exp <> 0) then exp := exp+1;
         { convert '0'..'9' }
         if (chkchr(f) in ['0'..'9']) then v := ord(chkchr(f)) - ord('0')
         else v := ord(lcase(chkchr(f))) - ord('a') + 10; { convert 'a'..'z' }
         if v >= r then error(f, edigbrx) { does not fit radix }
         else begin { ok }

            { check for overflow }
            if ((f^.nxtint > maxint div r) or 
               ((f^.nxtint = maxint div r) and (v > maxint mod r))) then 
               error(f, enumovf)
            else f^.nxtint := f^.nxtint * r + v { scale and add in }

         end;
         getchrb { next }

   end;
   if exp <> 0 then exp := exp - 1; { adjust exponent }
   f^.nxttlk := cinteger; { place type }
   if searchreal then begin { search for real specification }

      f^.nxtflt := f^.nxtint; { move integer to real }
      if chkchr(f) = '.' then begin { decimal point }

         { the '.' could be a '..' or '.)', in which case it's really
           <integer>.. or a alternate ']'. We must back up in this case }
         cs := f^.linptr; { save command pointer }
         getchrb; { skip '.' }
         if (chkchr(f) = '.') or (chkchr(f) = ')') then 
            f^.linptr := cs { back up }
         else begin { is a decimal point }

            zero := f^.nxtint = 0; { check number is zero (so far) }
            if rm then error(f, einvrft);
            if not (chkchr(f) in ['0'..'9']) then error(f, einvrft);
            p := 1.0; { initalize power }
            while chkchr(f) in ['0'..'9']  do begin { parse digits }

               if zero then exp := exp-1; { adjust the 'virtual exponent' }
               if chkchr(f) <> '0' then zero := false; { set leading digit found }
               p := p / 10.0; { find next scale }
               { add and scale new digit }
               f^.nxtflt := f^.nxtflt + (p * (ord(chkchr(f)) - ord('0')));
               getchrb { next }

            end;
            f^.nxttlk := creal { place tolken }

         end

      end;
      if lcase(chkchr(f)) = 'e' then begin { exponent }

         getchrb; { skip 'e' }
         sgn := 1; { set sign of exponent }
         c := chkchr(f); { check next }
         if c = '-' then sgn := -sgn; { set negative }
         if (c = '+') or (c = '-') then getchrb; { skip sign }
         if not (chkchr(f) in ['0'..'9']) then error(f, einvrft)
         else begin

            parnum(f, false); { parse integer only }
            if (f^.nxtint > maxexp) or (abs(sgn*f^.nxtint+exp) > maxexp) then 
               error(f, eexptl); { exponent too large }
            { find with exponent }
            if c = '-' then f^.nxtflt := f^.nxtflt / pwrten(f^.nxtint)
            else f^.nxtflt := f^.nxtflt * pwrten(f^.nxtint)

         end;
         f^.nxttlk := creal { place tolken }

      end

   end;
   copy(f^.nxtlab, outb) { place number as parsed }

end;

{******************************************************************************

Parse string

Parses and returns an input string. A string is any characters between single
quotes on a line. A double quote sequence within a string denotes a single
quote character. A '\' character introduces a force sequence as:

   \<memnonic>  - an ascii memonic denoting the control
                  character desired (as '\cr', etc.).

   \<number>    - the ascii value of the character
                  desired (with prefixes '$', '&' and
                  '%' possible).

   \<character> - all others just force the given character,
                  including '\'' for quote.

Since a string is as big as the input line, no overflow errors are required.
The one error consists of a missing quote.

******************************************************************************}

procedure parstr(var f: fcbptr); { file to read from }

label 1; { exit label }

var c: char;

begin

   f^.nxtlen := 0; { null string }
   skpspc(f); { skip leading spaces }
   { the following will never happen }
   if chkchr(f) <> '''' then error(f, enoldqt);
   getchr(f); { skip }
   while not endlin(f) do begin { process string }

      c := chkchr(f); { check next }
      if (c = '\\') and not fansi then begin { control sequence }

         getchr(f); { skip }
         c := chkchr(f); { next }
         if c in ['$', '&', '%', '0'..'9'] then begin

            { process numeric force }
            parnum(f, false); { parse numeric (rejecting reals) }
            if f^.nxtint > 255 then error(f, estrftl)
            else c := chr(f^.nxtint) { place character }

         end else begin

            conchr(f, c); { get possible control character }
            if c = ' ' then begin

               c := chkchr(f); { not found, is a force }
               getchr(f) { skip }

            end

         end

      end else if c = '''' then begin { found a quote }

         getchr(f); { skip }
         c := chkchr(f);
         if c <> '''' then goto 1; { was an exit quote }
         getchr(f) { skip }

      end else getchr(f); { skip }
      f^.nxtlen := f^.nxtlen + 1; { add to length }
      f^.nxtlab[f^.nxtlen] := c { place character }

   end;
   error(f, euntstr); { unterminated string }
   1: { terminate }
   f^.nxttlk := cstring; { place tolken }
   if (f^.nxtlen = 0) and fansi then error(f, enulstr) { null string }

end;

{******************************************************************************

Parse label/reserved word

Parses a label, which is:

    '_'/'a'..'z' ['_', '0'..'9', 'a'..'z']...

After parsing, the label is checked against the reserved list, and returned as
a reserved tolken if so.

******************************************************************************}

procedure parlabr(f: fcbptr);

var i:  0..maxstr; { index for label }
    ri: 0..resmax; { index for reserved table }

begin

   for i := 1 to maxstr do f^.nxtlab[i] := ' '; { clear label buffer }
   i := 0; { clear index }
   if fansi then while chkchr(f) in ['0'..'9', 'a'..'z', 'A'..'Z'] do begin

      { parse label characters }
      if i <> maxstr then begin { label not full }

         i := i + 1; { next character }
         f^.nxtlab[i] := chkchr(f) { place character }

      end;
      getchr(f) { skip }

   end else while chkchr(f) in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do begin

      { parse label characters }
      if i <> maxstr then begin { label not full }

         i := i + 1; { next character }
         f^.nxtlab[i] := chkchr(f) { place character }

      end;
      getchr(f) { skip }

   end;
   ri := hash(f^.nxtlab, hashoff, resmax); { find initial hash function }
   if restbl[ri].lab = nil then f^.nxttlk := cidentifier { not found}
   else if compp(f^.nxtlab, restbl[ri].lab^) then { found 1st try }
      f^.nxttlk := restbl[ri].tolk
   else begin { search chained }

      { traverse chains searching }
      while (restbl[ri].chn <> 0) and 
            not compp(f^.nxtlab, restbl[ri].lab^) do ri := restbl[ri].chn;
      if compp(f^.nxtlab, restbl[ri].lab^) then { found }
         f^.nxttlk := restbl[ri].tolk
      else f^.nxttlk := cidentifier { place tolken is identifier }

   end;
   { if ansi mode, serveral tolkens are invalid }
   if fansi and (f^.nxttlk in [cxor, cforward, cmodule, cuses, cjoins, cprivate,
                            cexternal, cview, cfixed, cprocess, cmonitor,
                            cshare, cclass, cis, catom, coverload, coverride,
                            creference, cthread, cjoins, cstatic, cinherited,
                            cself, cvirtual, ctry, cexcept, cextends, con,
                            cresult, coperator, ctask, cproperty, cchannel,
                            cstream, cout]) then
      f^.nxttlk := cidentifier { not tolkens }

end;

{******************************************************************************

Parse special character sequence

Parses a 1 or 2 character special sequence. These are arbitrary characters,
with any kind of termination.

******************************************************************************}

procedure parchr(f: fcbptr);

var hold: chrstr; { holding cell }
    i:    labinx; { index for label }

{ Find hash function }

function hash(s: chrstr; add: integer; max: integer): integer;

var i, r : integer;

begin

   r := 0;
   for i := 1 to spcmax do
      if s[i] <> ' ' then begin

      r := r + ord(lcase(s[i])) + add;

   end;
   hash := r mod max + 1

end;

{ search label }

procedure search;

var ci: chrinx; { index for special character table }

begin

   ci := hash(hold, chroff, chrmax); { find initial hash function }
   if hold = spctbl[ci].lab then { found 1st try }
      f^.nxttlk := spctbl[ci].tolk
   else begin { search chained }

      { traverse chains searching }
      while (spctbl[ci].chn <> 0) and 
            not (hold = spctbl[ci].lab) do ci := spctbl[ci].chn;
      if hold = spctbl[ci].lab then { found }
         f^.nxttlk := spctbl[ci].tolk
      else f^.nxttlk := cundefined { set not found }

   end

end;

begin

   hold[1] := chkchr(f); { place 1st character }
   getchr(f); { skip }
   hold[2] := chkchr(f); { place 2nd character }
   for i := 1 to maxstr do f^.nxtlab[i] := ' '; { set up label for errors }
   f^.nxtlab[1] := hold[1];
   f^.nxtlab[2] := hold[2];
   search; { try that }
   if f^.nxttlk <> cundefined then begin { found }

      { found 2 character sequence, or perhaps single with
        a trailing space. It really makes no difference,
        but we don't skip the space to give more accurate
        error pointers. }
      if hold[2] <> ' ' then getchr(f)

   end else begin { search single characters }

      hold[2] := ' '; { knock out 2nd character }
      f^.nxtlab[2] := ' ';
      search; { try that }
      if f^.nxttlk = cundefined then error(f, einvsch) { Invalid symbol/char }

   end

end;

{******************************************************************************

Parse tolken

Parses the following tolken types:

   1. String constants.
   2. Numeric constants.
   3. Reserved words.
   4. Specical character sequences.
   5. Indentifiers.
   6. Option lines.

******************************************************************************}

procedure partlk(var f: fcbptr); { file to read from }

var ls: integer; { line position save }
    c:  char;    { char save }

begin

   skpspc(f); { skip spaces }
   if seof(f) then f^.nxttlk := ceof { eof }
   else begin

      if chkchr(f) = '''' then
         parstr(f) { string }
      else if chkchr(f) in ['_', 'A'..'Z', 'a'..'z'] then
         parlabr(f) { label }
      else if chkchr(f) in ['0'..'9'] then
         parnum(f, true) { numeric }
      else if chkchr(f) in ['&', '%', '$'] then begin

         { either number or plain radix }
         c := chkchr(f); { save leader }
         ls := f^.linptr; { save line position }
         getchr(f); { go next }
         { if regular digit follows, is a number otherwise radix symbol }
         if chkchr(f) in ['0'..'9'] then begin

            f^.linptr := ls; { back up }
            parnum(f, false) { parse number without real }

         end else case c of

           '$': f^.nxttlk := chex;
           '&': f^.nxttlk := coct;
           '%': f^.nxttlk := cbin

         end

      end else
         parchr(f) { special character sequence }

   end

end;

{******************************************************************************

Get next tolken

Parses the next tolken in line, and places the tolken code and any data in the
next buffers. Removes the error suppression on the parser.

******************************************************************************}

procedure gettlk(f: fcbptr); { file to read from }

var ts: tolken; { tolken save }
    c:  char;   { holding }
    i:  labinx; { index for string }

begin

   repeat

      partlk(f); { parse tolken }
      ts := f^.nxttlk; { save tolken }
      if f^.nxttlk = clct then begin { comment }

         repeat

            c := chkchr(f); { check next }
            while (c <> '}') and (c <> '*') and not seof(f) do begin

               { if the next is a space, we use a space skip and
                 option check, as that is the only way to get from
                 line to line }
               if c = ' ' then skpspc(f)
               else getchr(f); { skip characters }
               c := chkchr(f) { check next }

            end;
            getchr(f) { skip comment char }

         until (c = '}') or ((c = '*') and (chkchr(f) = ')')) or seof(f);
         if c = '*' then getchr(f); { skip ')' }
         { if eof was hit without seeing a comment terminate, issue error }
         if (c <> '}') and (c <> '*') and seof(f) then 
            error(f, euntcmt) { unterminated comment }

      end

   until ts <> clct; { not comment }

   { the following is a diagnostic to print the next tolken }

   if false then begin

      write('*');	
      if f^.nxttlk in [clct, crct, cinteger, cidentifier, cstring, 
                       creal, cundefined, ceof] then
         case f^.nxttlk of { special tolken }
   
         clct:        write('left comment');
         crct:        write('right comment');
         cinteger:    write('unsigned integer constant: ', f^.nxtint);
         cidentifier: begin write('identifier: '); 
                            write(output, f^.nxtlab:*) end;
         cstring:     begin write('string constant: '); 
                            for i := 1 to f^.nxtlen do write(f^.nxtlab[i]) end;
         creal:       write('real constant: ', f^.nxtflt);
         cundefined:  write('undefined');
         ceof:        write('end of file');
   
      end else write(output, deftbl[f^.nxttlk]^);
      writeln('*')

   end

end;

{******************************************************************************

Open scanner file

Allocates the file control block, opens the file, then loads the first tolken.

******************************************************************************}

procedure opnscn(var f: fcbptr; view n: string);

begin

   new(f); { get a new fcb }
   copy(f^.name, n); { place name for errors }
   f^.lincnt := 0; { reset line counter }
   assign(f^.inpfil, n); { place filename }
   reset(f^.inpfil); { open file }
   getlin(f); { get 1st line in file }
   gettlk(f) { load 1st tolken in file }

end;

{******************************************************************************

Close scanner file

Closes the file, and frees the fcb.

******************************************************************************}

procedure clsscn(f: fcbptr);

begin

   close(f^.inpfil); { close file }
   dispose(f) { free its structure }

end;

{******************************************************************************

Initalize scanner

Initalizes the special character and reserved word tables, then loads the first
line from the input file.

******************************************************************************}

begin

   { definitions table.
     This table is used to translate tolkens back to 
     ASCII. It is used for diagnostics and spelling correction }

   deftbl[cplus]       := copy('+');
   deftbl[cminus]      := copy('-');
   deftbl[ctimes]      := copy('*');
   deftbl[crdiv]       := copy('/');
   deftbl[cequ]        := copy('=');
   deftbl[cnequ]       := copy('<>');
   deftbl[cnequa]      := copy('><');
   deftbl[cltn]        := copy('<');
   deftbl[cgtn]        := copy('>');
   deftbl[clequ]       := copy('<=');
   deftbl[clequa]      := copy('=<');
   deftbl[cgequ]       := copy('>=');
   deftbl[cgequa]      := copy('=>');
   deftbl[clparen]     := copy('(');
   deftbl[crparen]     := copy(')');
   deftbl[clbrkt]      := copy('[');
   deftbl[crbrkt]      := copy(']');
   deftbl[clct]        := copy('{');
   deftbl[crct]        := copy('}');
   deftbl[cbcms]       := copy(':=');
   deftbl[cperiod]     := copy('.');
   deftbl[ccma]        := copy(',');
   deftbl[cscn]        := copy(';');
   deftbl[ccln]        := copy(':');
   deftbl[ccmf]        := copy('^');
   deftbl[crange]      := copy('..');
   deftbl[cdiv]        := copy('div');
   deftbl[cmod]        := copy('mod');
   deftbl[cnil]        := copy('nil');
   deftbl[cin]         := copy('in');
   deftbl[cor]         := copy('or');
   deftbl[cand]        := copy('and');
   deftbl[cxor]        := copy('xor');
   deftbl[cnot]        := copy('not');
   deftbl[cif]         := copy('if');
   deftbl[cthen]       := copy('then');
   deftbl[celse]       := copy('else');
   deftbl[ccase]       := copy('case');
   deftbl[cof]         := copy('of');
   deftbl[crepeat]     := copy('repeat');
   deftbl[cuntil]      := copy('until');
   deftbl[cwhile]      := copy('while');
   deftbl[cdo]         := copy('do');
   deftbl[cfor]        := copy('for');
   deftbl[cto]         := copy('to');
   deftbl[cdownto]     := copy('downto');
   deftbl[cbegin]      := copy('begin');
   deftbl[cend]        := copy('end');
   deftbl[cwith]       := copy('with');
   deftbl[cgoto]       := copy('goto');
   deftbl[cconst]      := copy('const');
   deftbl[cvar]        := copy('var');
   deftbl[ctype]       := copy('type');
   deftbl[carray]      := copy('array');
   deftbl[crecord]     := copy('record');
   deftbl[cset]        := copy('set');
   deftbl[cfile]       := copy('file');
   deftbl[cfunction]   := copy('function');
   deftbl[cprocedure]  := copy('procedure');
   deftbl[clabel]      := copy('label');
   deftbl[cpacked]     := copy('packed');
   deftbl[cprogram]    := copy('program');
   deftbl[cforward]    := copy('forward');
   deftbl[cmodule]     := copy('module');
   deftbl[cuses]       := copy('uses');
   deftbl[cprivate]    := copy('private');
   deftbl[cexternal]   := copy('external');
   deftbl[cview]       := copy('view');
   deftbl[cfixed]      := copy('fixed');
   deftbl[cprocess]    := copy('process');
   deftbl[cmonitor]    := copy('monitor');
   deftbl[cshare]      := copy('share');
   deftbl[cclass]      := copy('class');
   deftbl[cis]         := copy('is');
   deftbl[catom]       := copy('atom');
   deftbl[catom]       := copy('overload');
   deftbl[catom]       := copy('override');
   deftbl[catom]       := copy('reference');
   deftbl[catom]       := copy('thread');
   deftbl[catom]       := copy('joins');
   deftbl[catom]       := copy('static');
   deftbl[catom]       := copy('inherited');
   deftbl[catom]       := copy('self');
   deftbl[catom]       := copy('virtual');
   deftbl[catom]       := copy('try');
   deftbl[catom]       := copy('except');
   deftbl[catom]       := copy('extends');
   deftbl[catom]       := copy('on');
   deftbl[catom]       := copy('result');
   deftbl[catom]       := copy('operator');
   deftbl[catom]       := copy('task');
   deftbl[catom]       := copy('property');
   deftbl[catom]       := copy('channel');
   deftbl[catom]       := copy('stream');
   deftbl[catom]       := copy('out');
   deftbl[cinteger]    := copy('');
   deftbl[cidentifier] := copy('');
   deftbl[cstring]     := copy('');
   deftbl[creal]       := copy('');
   deftbl[cundefined]  := copy('');
   deftbl[ceof]        := copy('');

   { flags }
  
   fansi := false { set not standard mode }

end.
