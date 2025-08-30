{******************************************************************************
*                                                                             *
*                         STRING HANDLING LIBRARY                             *
*                                                                             *
* LICENSING:                                                                  *
*                                                                             *
* Copyright (c) 1996, 2018, Scott A. Franco                                   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
* 1. Redistributions of source code must retain the above copyright notice,   *
*    this list of conditions and the following disclaimer.                    *
* 2. Redistributions in binary form must reproduce the above copyright        *
*    notice, this list of conditions and the following disclaimer in the      *
*    documentation and/or other materials provided with the distribution.     *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
* The views and conclusions contained in the software and documentation are   *
* those of the authors and should not be interpreted as representing official *
* policies, either expressed or implied, of the Pascal-P6 project.            *
*                                                                             *
* FUNCTION:                                                                   *
*                                                                             *
* Declares and handles space padded and normal strings. Space padded strings  *
* are strings that have spaces at the right, so that the length of the string *
* is the rightmost character of the string. Most functions are accounted for. *
* The advantages of using space padded strings is that this package           *
* integrates well with existing Pascal string types, and padded strings are   *
* efficient and use fixed storage. The disadvantage is that no strings        *
* with spaces at the right, or strings consisting only of spaces can exist.   *
*                                                                             *
* Also operates on "pointer" or literal strings, where every character is     *
* significant. These strings are pointer based, and can have any length. They *
* work by essentially placing the work of space accounting back to the OS.    *
* Some routines accept either type of string. For example, copy can use       *
* either type of string as a source, and can thus translate a pointer string  *
* to a padded string. But the pointer version, copy, can't be used to         *
* translate a padded string to a pointer, because it treats the spaces as     *
* significant. copys performs this translation.                               *
*                                                                             *
* Pointer routines are the most primitive about string handling as possible.  *
* copy overwrites any destination string without returning any disposing      *
* of a previous string.                                                       *
*                                                                             *
* NAMING CONVENTIONS:                                                         *
*                                                                             *
* There are three basic types of strings, and any of the parameters of a      *
* routine could be any type. These are fixed, pointer and padded. Fixed       *
* strings are never specifically named. Whenever possible, an operand         *
* to fixed mode, because that is universal. Pointer based strings are         *
* considered the next most common, and there is no special naming convention  *
* for this. The last type, padded, is allways indicated with a trailing "p"   *
* on the name.                                                                *
*                                                                             *
* When strings are compared, it is normally caseless. If case does matter,    *
* the routine has "c" appended. If a routine for case significant padded      *
* strings, then "cp" is appended.                                             *
*                                                                             *
* When a number is converted to a string, it has a "s" appended (which        *
* prevents naming conflicts). If a string is converted to a number, "v" (for  *
* "value" is appended. Routines that place numbers in strings or output them  *
* can appear with formatting field lengths, indicated by an appended "f".     *
*                                                                             *
* HISTORY:                                                                    *
*                                                                             *
* Strlib was created in 1996 as one of the first modules after the I80386     *
* implementations. It was overhauled and greatly expanded in 2004 with the    *
* addition of procedure and function overloads. The procedures and functions  *
* were overloaded to cover many more cases, and the number and types of       *
* functions were expanded. In addition, a string temp management system was   *
* incorporated, which allowed dynamic strings to appear as function results.  *
*                                                                             *
* STANDARD:                                                                   *
*                                                                             *
* This source complies with the Pascaline standard language.                  *
*                                                                             *
* FORMATTING:                                                                 *
*                                                                             *
* The formatting here is compliant with prefix based automatic formatting and *
* documenting tools. It maintains the source within 80 characters.            *
*                                                                             *
* TO DO:                                                                      *
*                                                                             *
* 1. The Pascaline documented object interface needs to be implemented.       *
*                                                                             *
* 2. It has been proposed to add regular expressions to the search.           *
*                                                                             *
******************************************************************************}

module strings(input, output, error);

var

{ No string block is active }            NoStringBlock,
{ No room in outermost block }           OuterBlockFull,
{ Current string block is full }         CurrentBlockFull,
{ String passed is nil }                 StringNil,
{ String was too large for destination } StringDestinationOverflow,
{ Sring index out of range }             IndexOutOfRange,
{ Repeat cont was negative }             NegativeRepeatCount,
{ Word array index was out of range }    WordIndexOutOfRange,
{ String was too large for destination } StringReadOverflow,
{ Format too large for destination }     FormatTooLarge,
{ field specified is invalid }           InvalidFieldSpecification,
{ Radix was negative }                   NegativeValueNondecimal,
{ Number overflows space provided in 
  format string }                        NumberOverflowsFormat,
{ Negative sign not placed in format }   NegativeNotPlaced,
{ Invalid real number }                  InvalidRealNumber,
{ Invalid fraction specification }       InvalidFractionSpecification,
{ Invalid radix }                        InvalidRadix,
{ Invalid integer format }               InvalidIntegerFormat,
{ Number too large }                     NumberTooLarge,
{ Integer too large }                    IntegerTooLarge,
{ Invalid real format }                  InvalidRealFormat: exception;

function lcase(c: char): char; forward;
overload function lcase(view s: pstring): pstring; forward;
function lcases(view s: string): pstring; forward;
overload procedure lcases(var s: string); forward;
function ucase(c: char): char; forward;
overload function ucase(view s: pstring): pstring; forward;
function ucases(view s: string): pstring; forward;
overload procedure ucases(var s: string); forward;
procedure clears(var s: string); forward;
function len(view s: string): integer; forward;
overload function len(view s: pstring): integer; forward;
procedure copy(var d: string; view s: string); forward;
overload procedure copy(var  d: string; s: pstring); forward;
overload function copy(view s: string): pstring; forward;
overload function copy(view s: pstring): pstring; forward;
overload procedure copy(var  d: pstring; view s: string); forward;
procedure cat(var d: string; view s: string); forward;
overload function cat(view sa, sb: string): pstring; forward;
overload function cat(s1, s2: pstring): pstring; forward;
overload function cat(view s1: string; s2: pstring): pstring; forward;
overload function cat(s1: pstring; view s2: string): pstring; forward;
function compc(view d, s: string): boolean; forward;
overload function compc(d, s: pstring): boolean; forward;
overload function compc(view d: string; s: pstring): boolean; forward;
overload function compc(d: pstring; view s: string): boolean; forward;
function compcp(view d, s: string): boolean; forward;
function comp(view d, s: string): boolean; forward;
overload function comp(d, s: pstring): boolean; forward;
overload function comp(view d: string; s: pstring): boolean; forward;
overload function comp(d: pstring; view s: string): boolean; forward;
function compp(view d, s: string): boolean; forward;
function gtrc(view d, s: string): boolean; forward;
overload function gtrc(d, s: pstring): boolean; forward;
overload function gtrc(view d: string; s: pstring): boolean; forward;
overload function gtrc(d: pstring; view s: string): boolean; forward;
function gtrcp(view d, s: string): boolean; forward;
function gtr(view d, s: string): boolean; forward;
overload function gtr(d, s: pstring): boolean; forward;
overload function gtr(view d: string; s: pstring): boolean; forward;
overload function gtr(d: pstring; view s: string): boolean; forward;
function gtrp(view d, s: string): boolean; forward;
function indexc(view d, s: string): integer; forward;
overload function indexc(d, s: pstring): integer; forward;
overload function indexc(view d: string; s: pstring): integer; forward;
overload function indexc(d: pstring; view s: string): integer; forward;
function indexcp(view d, s: string): integer; forward;
function index(view d, s: string): integer; forward;
overload function index(d, s: pstring): integer; forward;
overload function index(view d: string; s: pstring): integer; forward;
overload function index(d: pstring; view s: string): integer; forward;
function indexp(view d, s: string): integer; forward;
procedure extract(var d: string; view s: string; l, r: integer);
   forward;
overload function extract(view s: pstring; l, r: integer): pstring; forward;
overload function extract(view s: string; l, r: integer): pstring; forward;

procedure insert(var d: string; view s: string; p: integer); forward;
overload function insert(view sa, sb: string; p: integer): pstring; forward;
overload function insert(sa, sb: pstring; p: integer): pstring; forward;
overload function insert(view sa: string; sb: pstring; p: integer): pstring; 
   forward;
overload function insert(sa: pstring; view sb: string; p: integer): pstring; 
   forward;
function rep(view s: string; r: integer): pstring; forward;
overload function rep(s: pstring; r: integer): pstring; forward;
overload procedure rep(var d: string; view s: string; r: integer); forward;
function trim(view s: string): pstring; forward;
overload function trim(s: pstring): pstring; forward;
overload procedure trim(var d: string; view s: string); forward;
function words(view s: string): integer; forward;
overload function words(s: pstring): integer; forward;
function extwords(view s: string; l, r: integer): pstring; forward;
overload function extwords(s: pstring; l, r: integer): pstring; forward;
overload procedure extwords(var d: string; view s: string; l, r: integer); 
   forward;
procedure reads(var f: text; var s: pstring; var ovf: boolean); forward;
overload procedure reads(var f: text; var s: string; var ovf: boolean); forward;
overload procedure reads(var f: text; var s: pstring); forward;
overload procedure reads(var f: text; var s: string); forward;
overload procedure reads(var s: pstring; var ovf: boolean); forward;
overload procedure reads(var s: string; var ovf: boolean); forward;
overload procedure reads(var s: pstring); forward;
overload procedure reads(var s: string); forward;
procedure ints(var s: string; i: integer; f: integer); forward;
overload function ints(i: integer; f: integer): pstring; forward;
overload procedure ints(var s: string; i: integer); forward;
overload function ints(i: integer): pstring; forward;
overload procedure ints(var s: string; i: integer; view fmt: string); forward;
overload function ints(i: integer; view fmt: string): pstring; forward;
procedure reals(var s: string; r: real; f: integer); forward;
overload function reals(r: real; f: integer): pstring; forward;
overload procedure reals(var s: string; r: real); forward;
overload function reals(r: real): pstring; forward;
overload procedure reals(var s: string; r: real; fl: integer; fr: integer);
   forward;
overload function reals(r: real; fl: integer; fr: integer): pstring; forward;
overload procedure reals(var s: string; r: real; view fmt: string); forward;
overload function reals(r: real; view fmt: string): pstring; forward;
procedure reales(var s: string; r: real; f: integer); forward;
overload function reales(r: real; f: integer): pstring; forward;
overload procedure reales(var s: string; r: real); forward;
overload function reales(r: real): pstring; forward;
procedure hexs(var s: string; w: integer; f: integer); forward;
overload function hexs(w: integer; f: integer): pstring; forward;
overload procedure hexs(var s: string; w: integer); forward;
overload function hexs(w: integer): pstring; forward;
overload procedure hexs(var s: string; i: integer; view fmt: string); forward;
overload function hexs(i: integer; view fmt: string): pstring; forward;
procedure octs(var s: string; w: integer; f: integer); forward;
overload function octs(w: integer; f: integer): pstring; forward;
overload procedure octs(var s: string; w: integer); forward;
overload function octs(w: integer): pstring; forward;
overload procedure octs(var s: string; i: integer; view fmt: string); forward;
overload function octs(i: integer; view fmt: string): pstring; forward;
procedure bins(var s: string; w: integer; f: integer); forward;
overload function bins(w: integer; f: integer): pstring; forward;
overload procedure bins(var s: string; w: integer); forward;
overload function bins(w: integer): pstring; forward;
overload procedure bins(var s: string; i: integer; view fmt: string); forward;
overload function bins(i: integer; view fmt: string): pstring; forward;
procedure writed(var f: text; i: integer; view fmt: string); forward;
overload procedure writed(i: integer; view fmt: string); forward;
procedure writer(var f: text; r: real; view fmt: string); forward;
overload procedure writer(r: real; view fmt: string); forward;
procedure writere(var f: text; r: real; fl: integer); forward;
overload procedure writere(r: real; fl: integer); forward;
overload procedure writere(var f: text; r: real); forward;
overload procedure writere(r: real); forward;
procedure writeh(var f: text; i: integer; fl: integer); forward;
overload procedure writeh(i: integer; fl: integer); forward;
overload procedure writeh(var f: text; i: integer); forward;
overload procedure writeh(i: integer); forward;
overload procedure writeh(var f: text; i: integer; view fmt: string); forward;
overload procedure writeh(i: integer; view fmt: string); forward;
procedure writeo(var f: text; i: integer; fl: integer); forward;
overload procedure writeo(i: integer; fl: integer); forward;
overload procedure writeo(var f: text; i: integer); forward;
overload procedure writeo(i: integer); forward;
overload procedure writeo(var f: text; i: integer; view fmt: string); forward;
overload procedure writeo(i: integer; view fmt: string); forward;
procedure writeb(var f: text; i: integer; fl: integer); forward;
overload procedure writeb(i: integer; fl: integer); forward;
overload procedure writeb(var f: text; i: integer); forward;
overload procedure writeb(i: integer); forward;
overload procedure writeb(var f: text; i: integer; view fmt: string); forward;
overload procedure writeb(i: integer; view fmt: string); forward;
function intv(view s: string): integer; forward;
overload function intv(s: pstring): integer; forward;
overload function intv(view s: string; var ovf: boolean): integer; forward;
overload function intv(s: pstring; var ovf: boolean): integer; forward;
function hexv(view s: string): integer; forward;
overload function hexv(s: pstring): integer; forward;
overload function hexv(view s: string; var ovf: boolean): integer; forward;
overload function hexv(s: pstring; var ovf: boolean): integer; forward;
function octv(view s: string): integer; forward;
overload function octv(s: pstring): integer; forward;
overload function octv(view s: string; var ovf: boolean): integer; forward;
overload function octv(s: pstring; var ovf: boolean): integer; forward;
function binv(view s: string): integer; forward;
overload function binv(s: pstring): integer; forward;
overload function binv(view s: string; var ovf: boolean): integer; forward;
overload function binv(s: pstring; var ovf: boolean): integer; forward;
function realv(view s: string): real; forward;
overload function realv(s: pstring): real; forward;
procedure openstring; forward;
procedure closestring; forward;
procedure exportstring(p: pstring); forward;
procedure upstring(p: pstring); forward;
procedure subst(var  s: string; view m: string; view r: string); forward;
overload function subst(view s: string; view m: string; view r: string)
                        : pstring; forward;
overload function subst(view s: string; view m: string; r: pstring): pstring;
   forward;
overload function subst(view s: string; m: pstring; view r: string): pstring;
   forward;
overload function subst(view s: string; m: pstring; r: pstring): pstring;
   forward;
overload function subst(s: pstring; view m: string; view r: string): pstring;
   forward;
overload function subst(s: pstring; view m: string; r: pstring): pstring;
   forward;
overload function subst(s: pstring; m: pstring; view r: string): pstring;
   forward;
overload function subst(s: pstring; m: pstring; r: pstring): pstring; forward;
procedure substc(var  s: string; view m: string; view r: string); forward;
overload function substc(view s: string; view m: string; view r: string)
                        : pstring; forward;
overload function substc(view s: string; view m: string; r: pstring): pstring;
   forward;
overload function substc(view s: string; m: pstring; view r: string): pstring;
   forward;
overload function substc(view s: string; m: pstring; r: pstring): pstring;
   forward;
overload function substc(s: pstring; view m: string; view r: string): pstring;
   forward;
overload function substc(s: pstring; view m: string; r: pstring): pstring;
   forward;
overload function substc(s: pstring; m: pstring; view r: string): pstring;
   forward;
overload function substc(s: pstring; m: pstring; r: pstring): pstring; forward;
procedure substall(var s: string; view m: string; view r: string); forward;
overload function substall(view s: string; view m: string; view r: string)
                           : pstring; forward;
overload function substall(view  s: string; view m: string; r: pstring)
                           : pstring; forward;
overload function substall(view  s: string; m: pstring; view r: string)
                           : pstring; forward;
overload function substall(view  s: string; m: pstring; r: pstring): pstring; 
   forward;
overload function substall(s: pstring; view m: string; view r: string): pstring;
   forward;
overload function substall(s: pstring; view m: string; r: pstring): pstring;
   forward;
overload function substall(s: pstring; m: pstring; view r: string): pstring;
   forward;
overload function substall(s: pstring; m: pstring; r: pstring): pstring;
   forward;
procedure substcall(var s: string; view m: string; view r: string); forward;
overload function substcall(view s: string; view m: string; view r: string)
                           : pstring; forward;
overload function substcall(view  s: string; view m: string; r: pstring)
                           : pstring; forward;
overload function substcall(view  s: string; m: pstring; view r: string)
                           : pstring; forward;
overload function substcall(view  s: string; m: pstring; r: pstring): pstring; 
   forward;
overload function substcall(s: pstring; view m: string; view r: string): pstring;
   forward;
overload function substcall(s: pstring; view m: string; r: pstring): pstring;
   forward;
overload function substcall(s: pstring; m: pstring; view r: string): pstring;
   forward;
overload function substcall(s: pstring; m: pstring; r: pstring): pstring;
   forward;

private

{ *** This set of caluculations, should be automated as done in pint.pas *** }
const maxpwr = 1000000000; { maximum power of 10 that fits into integer }
      maxhdg = 8;          { maximum number of hex digits in integer }
      maxdig = 11;         { maximum number of digits to hold integer (with 
                             sign) }
      maxlin = 200;        { maximum number of characters in input line }
      rlfld  = 22;         { maximum precision field for real }
      rlfrc  = 20;         { maximum digits in fraction }
      maxstr = 100;        { number of strings tracked on each level }

type stkptr = ^strtrk; { string tracking pointer }
     strtrk = record { string allocation tracking entry }

        strings: array [1..maxstr] of pstring; { string tracking table }
        next: stkptr { next block }

     end;

var strstk: stkptr; { string blocks tracking stack }
    strfre: stkptr; { string blocks free list }


{******************************************************************************

Process string library error

Outputs an error message to the error output file, and then halts. This may
need to be either directed towards a custom error file or handler, since we
make two assumptions here:

1. That the error is output as a single line before program termination.

2. That the system may add its own preamble.

******************************************************************************}

procedure perror(view s: string);

begin

   { write error string to error output with preamble }
   writeln(error, 'Strlib: ', s);

end;

procedure throw(ev: exception);

begin

    writeln(error, 'Strlib: exception')

end;

{******************************************************************************

Open new string tracking level

Creates or recycles a string tracking block, and places that atop the tracking
stack.

******************************************************************************}

procedure openstring;

var sp: stkptr;    { stack block pointer }
    i:  1..maxstr; { index for strings }

begin

   if strfre <> nil then begin { get free entry }

      sp := strfre; { index top entry }
      strfre := strfre^.next { gap out }

   end else new(sp); { get a new entry }
   sp^.next := strstk; { push onto stack }
   strstk := sp;
   for i := 1 to maxstr do sp^.strings[i] := nil { clear string holders }

end;

{******************************************************************************

Export string from tracking level

Removes the indicated string completely from the tracking system.

******************************************************************************}

procedure exportstring(p: pstring);

var i: 1..maxstr; { index for strings }

begin

   if strstk = nil then throw(NoStringBlock);
   { search for string in block }
   for i := 1 to maxstr do 
      { remove completely if found }
      if strstk^.strings[i] = p then strstk^.strings[i] := nil

end;

{******************************************************************************

Move string to outter tracking block

Moves the string in the current level, up to the next surrounding level. If the
outter level is nil, then the effect is the same as exportstring.

******************************************************************************}

procedure upstring(p: pstring);

var i:  1..maxstr; { index for strings }
    fs: 0..maxstr; { free string entry }

begin

   exportstring(p); { start by removing it completely }
   if strstk <> nil then { should not happen }
      if strstk^.next <> nil then begin { there is an outter block }

      fs := 0; { clear free string index }
      { find lowest number free string entry }
      for i := maxstr downto 1 do 
         if strstk^.next^.strings[i] = nil then fs := i;
      if fs = 0 then throw(OuterBlockFull);
      strstk^.next^.strings[fs] := p { record new string }

   end

end;

{******************************************************************************

Close string tracking level

Frees all strings from the top string tracking level, and frees the block.

******************************************************************************}

procedure closestring;

var sp: stkptr;    { stack block pointer }
    i:  1..maxstr; { index for strings }

begin

   if strstk = nil then throw(NoStringBlock);
   { dispose of all active strings in block }
   for i := 1 to maxstr do 
      if strstk^.strings[i] <> nil then dispose(strstk^.strings[i]);
   { remove stack top entry }
   sp := strstk; { index top entry }
   strstk := strstk^.next; { gap out }
   { push onto free list }
   strstk := sp

end;

{******************************************************************************

Allocate new string

Allocates a dynamic string. If the string tracking system is active, then the
string will be recorded in the current tracking block.

******************************************************************************}

procedure newstr(var p: pstring; s: integer);

var i:  1..maxstr; { index for strings }
    fs: 0..maxstr; { free string entry }

begin

   new(p, s); { allocate new string }
   if strstk <> nil then begin { tracking block is active, record it }

      fs := 0; { clear free string index }
      { find lowest number free string entry }
      for i := maxstr downto 1 do if strstk^.strings[i] = nil then fs := i;
      if fs = 0 then throw(CurrentBlockFull);
      strstk^.strings[fs] := p { record new string }

   end

end;

{******************************************************************************

Find lower case

Finds the lower case equivalent of 'A'..'Z'.

Forms:

function lcase(char): char - Returns the lower case of the argument.
procedure lcase(var char) - Changes the argument to lower case.
procedure lcases(var string) - Changes the string to all lower case.
function lcases(string): pstring - Returns lower case dynamic copy of argument.

******************************************************************************}

function lcase(c: char): char;

begin

   { convert lower case }
   if c in ['A'..'Z'] then c := chr(ord(c)-ord('A')+ord('a'));
   lcase := c { return result }

end;

overload function lcase(view s: pstring): pstring;

var p: pstring;

begin

   if s = nil then throw(StringNil);
   p := copy(s^); { copy source to temp }
   lcases(p^); { convert that to lcase }
   
   lcase := p

end;

function lcases(view s: string): pstring;

var p: pstring;

begin

   p := copy(s); { copy source to temp }
   lcases(p^); { convert that to lcase }
   
   lcases := p

end;

overload procedure lcases(var s: string);

var i: integer;

begin

   for i := 1 to max(s) do s[i] := lcase(s[i])

end;

{******************************************************************************

Find upper case

Finds the upper case equivalent of 'a'..'z'.

Forms:

function ucase(char): char - Returns the upper case of the argument.
procedure ucase(var char) - Changes the argument to upper case.
procedure ucases(var string) - Changes the string to all upper case.
function ucases(string): pstring - Returns upper case dynamic copy of argument.

******************************************************************************}

function ucase(c: char): char;

begin

   { convert upper case }
   if c in ['a'..'z'] then c := chr(ord(c)-ord('a')+ord('A'));
   ucase := c { return result }

end;

overload function ucase(view s: pstring): pstring;

var p: pstring;

begin

   if s = nil then throw(StringNil);
   p := copy(s^); { copy source to temp }
   ucases(p^); { convert that to ucase }
   
   ucase := p

end;

function ucases(view s: string): pstring;

var p: pstring;

begin

   p := copy(s); { copy source to temp }
   ucases(p^); { convert that to ucase }
   
   ucases := p

end;

overload procedure ucases(var s: string);

var i: integer;

begin

   for i := 1 to max(s) do s[i] := ucase(s[i])

end;

{******************************************************************************

Clear string

Clears the given string to all blanks.

Forms:

procedure clears(var string) - Clear string to blanks.

******************************************************************************}

procedure clears(var s: string);

var i: integer; { index for string }

begin

   for i := 1 to max(s) do s[i] := ' ' { clear string }

end;

{******************************************************************************

Find string length padded

Finds the length of the given string, which is the index of the last character
in the string that is non-blank, or 0 if the string is all blanks.

Forms:

function len(string) - Returns the padded length of string.

******************************************************************************}

function len(view s: string): integer;

var i: integer; { index for string }

begin

   i := max(s); { index last of string }
   if i <> 0 then begin { string has length }

      while (i > 1) and (s[i] = ' ') do i := i-1; { find last character }
      if s[i] = ' ' then i := 0 { handle single blank case }

   end;
   len := i { return length of string }

end;

overload function len(view s: pstring): integer;

begin

   if s = nil then throw(StringNil);
   len := max(s^)

end;

{******************************************************************************

Copy string

Creates a new string at the destination, and copies the source into it.

Forms:

procedure copy(string, string) - Copy to string result, padding right as 
                                 required.
procedure copy(pstring, string) - Copy string to new dynamic.
procedure copy(pstring, string) - Copy padded string to new dynamic.

******************************************************************************}

procedure copy(var  d: string;  { destination }
               view s: string); { source }

var i:  integer; { index for string }
    ls: integer; { length of source save }

begin

   ls := len(s); { calculate this only once }
   if ls > max(d) then { string too large }
      throw(StringDestinationOverflow);
   for i := 1 to ls do d[i] := s[i]; { copy string into place }
   for i := ls+1 to max(d) do d[i] := ' ' { fill remainder }

end;

overload procedure copy(var  d: string;   { destination }
                             s: pstring); { source }

begin

   copy(d, s^) { copy }

end;

overload function copy(view s: string) { source }
                       : pstring; { result }

var d: pstring;

begin

   newstr(d, max(s)); { create destination }
   d^ := s; { copy string }

   copy := d { return result }

end;

overload function copy(view s: pstring) { source }
                       : pstring; { result }

begin

   if s = nil then throw(StringNil);

   copy := copy(s^) { return result }

end;

overload procedure copy(var  d: pstring; { destination }
                        view s: string); { source }

begin

   newstr(d, len(s)); { create destination }
   copy(d^, s) { copy string padded }

end;

{******************************************************************************

Concatenate strings

Creates a new string as long as the combination of the two source strings,
then places the source string b to the right of the source string a in the
destination. The source strings are view.

Forms:

procedure cat(string, string) - Concatenates the padded arguments to a padded
                                left result (two operand version).
procedure cat(pstring, string, string) - Concatentates the strings to a left
                                         dynamic result (three operand version).
function cat(string, string) - Concatenates the strings to a dynamic result.

******************************************************************************}

procedure cat(var  d: string;  { destination }
              view s: string); { source }

var i:      integer; { index for string }
    ls, ld: integer; { length saves }

begin

   ls := len(s); { calculate this only once }
   ld := len(d);
   if ls+ls > max(d) then { strings too long }
      throw(StringDestinationOverflow);
   for i := 1 to ls do d[i+ld] := s[i] { copy string into place }

end;

overload function cat(view sa, sb: string) { sources }
                     : pstring;

var i: integer; { index for string }
    d: pstring; { temp string }

begin

   newstr(d, max(sa)+max(sb)); { create destination }
   for i := 1 to max(sa) do d^[i] := sa[i]; { copy left string }
   for i := 1 to max(sb) do d^[max(sa)+i] := sb[i]; { copy right string }

   cat := d { return result }

end;

overload function cat(s1, s2: pstring): pstring;

begin

   if (s1 = nil) or (s2 = nil) then throw(StringNil);
   cat := cat(s1^, s2^); { concatenate to result }

end;

overload function cat(view s1: string; s2: pstring): pstring;

begin

   if s2 = nil then throw(StringNil);
   cat := cat(s1, s2^); { concatenate to result }

end;

overload function cat(s1: pstring; view s2: string): pstring;

begin

   if s1 = nil then throw(StringNil);
   cat := cat(s1^, s2); { concatenate to result }

end;

{******************************************************************************

Compare strings

Compares two strings, and returns the equality of the strings. Case is
observed.

******************************************************************************}

function compc(view d: string;  { destination }
               view s: string)  { source }
               : boolean; { result }

var r: boolean; { result holder }

begin

   r := false; { set not equal }
   if max(d) = max(s) then r := d = s; { compare }
   compc := r

end;

overload function compc(d: pstring; { destination }
                        s: pstring) { source }
                        : boolean; { result }

begin

   if (d = nil) or (s = nil) then throw(StringNil);

   compc := compc(d^, s^) { return result }

end;

overload function compc(view d: string; { destination }
                             s: pstring) { source }
                        : boolean; { result }

begin

   if s = nil then throw(StringNil);

   compc := compc(d, s^) { return result }

end;

overload function compc(     d: pstring; { destination }
                        view s: string) { source }
                        : boolean; { result }

begin

   if d = nil then throw(StringNil);

   compc := compc(d^, s) { return result }

end;

{******************************************************************************

Compare strings padded

Compares two strings, and returns the equality of the strings. Case is
observed.

******************************************************************************}

function compcp(view d, s: string): boolean;

var i:      integer; { index for string }
    ls, ld: integer; { length saves }
    r:      boolean; { result save }

begin

   ls := len(s); { calculate this only once }
   ld := len(d);
   if ls <> ld then r := false { strings don't match in length }
   else begin { check each character }

      r := true; { set strings match }
      for i := 1 to ls do if d[i] <> s[i] then r := false { mismatch }

   end;
   compcp := r { return match status }

end;

{******************************************************************************

Compare strings caseless

Compares two strings, and returns the equality of the strings. Case is
NOT observed.

******************************************************************************}

function comp(view d, s: string): boolean;

var i: integer; { index for string }
    r: boolean; { result save }

begin

   if max(d) <> max(s) then r := false { strings don't match in length }
   else begin { check each character }

      r := true; { set strings match }
      for i := 1 to max(d) do if lcase(d[i]) <> lcase(s[i]) then
         r := false { mismatch }

   end;
   comp := r { return match status }

end;

overload function comp(d, s: pstring) { destination }
                       : boolean; { result }

begin

   if (d = nil) or (s = nil) then throw(StringNil);

   comp := comp(d^, s^) { return result }

end;

overload function comp(view d: string; { destination }
                            s: pstring) { source }
                       : boolean; { result }

begin

   if s = nil then throw(StringNil);

   comp := comp(d, s^) { return result }

end;

overload function comp(     d: pstring; { destination }
                       view s: string) { source }
                       : boolean; { result }

begin

   if d = nil then throw(StringNil);

   comp := comp(d^, s) { return result }

end;

{******************************************************************************

Compare strings caseless padded

Compares two strings, and returns the equality of the strings. Case is
NOT observed.

******************************************************************************}

function compp(view d, s: string): boolean;

var i:      integer; { index for string }
    ls, ld: integer; { length saves }
    r:      boolean; { result save }

begin

   ls := len(s); { calculate this only once }
   ld := len(d);
   if ls <> ld then r := false { strings don't match in length }
   else begin { check each character }

      r := true; { set strings match }
      for i := 1 to ls do if lcase(d[i]) <> lcase(s[i]) then
         r := false { mismatch }

   end;
   compp := r { return match status }

end;

{******************************************************************************

Compare string greater case sensitive

Compares two strings, and returns if the source string is greater than the
destination. Case is observed.

******************************************************************************}

function gtrc(view d, s: string): boolean;

var i:      integer; { index for string }
    l:      integer; { common region length }
    r:      boolean; { result save }

begin

   { check case where one string is null, and compare lengths if so }
   if (max(d) = 0) or (max(s) = 0) then r := max(d) < max(s)
   else begin

      { find the shorter of the strings }
      if max(s) > max(d) then l := max(d) else l := max(s);
      { compare the overlapping region }
      i := 1; { set 1st character }
      { find first non-matching character }
      while (i < l) and (s[i] = d[i]) do i := i+1;
      { check not equal, and return status based on that character }
      if s[i] <> d[i] then r := d[i] < s[i]
      { if the entire common region matched, then we base the result on
        length }
      else r := max(d) < max(s)

   end;
   gtrc := r { return match status }

end;

overload function gtrc(d, s: pstring) { destination }
                       : boolean; { result }

begin

   if (d = nil) or (s = nil) then throw(StringNil);

   gtrc := gtrc(d^, s^) { return result }

end;

overload function gtrc(view d: string; { destination }
                            s: pstring) { source }
                       : boolean; { result }

begin

   if s = nil then throw(StringNil);

   gtrc := gtrc(d, s^) { return result }

end;

overload function gtrc(     d: pstring; { destination }
                       view s: string) { source }
                       : boolean; { result }

begin

   if d = nil then throw(StringNil);

   gtrc := gtrc(d^, s) { return result }

end;

{******************************************************************************

Compare string greater case sensitive padded

Compares two strings, and returns if the source string is greater than the
destination. Case is observed.

******************************************************************************}

function gtrcp(view d, s: string): boolean;

var i:      integer; { index for string }
    ls, ld: integer; { length saves }
    l:      integer; { common region length }
    r:      boolean; { result save }

begin

   ls := len(s); { calculate this only once }
   ld := len(d);
   { check case where one string is null, and compare lengths if so }
   if (ls = 0) or (ld = 0) then r := ld < ls
   else begin

      { find the shorter of the strings }
      if ls > ld then l := ld else l := ls;
      { compare the overlapping region }
      i := 1; { set 1st character }
      { find first non-matching character }
      while (i < l) and (s[i] = d[i]) do i := i+1;
      { check not equal, and return status based on that character }
      if s[i] <> d[i] then r := d[i] < s[i]
      { if the entire common region matched, then we base the result on
        length }
      else r := ld < ls

   end;
   gtrcp := r { return match status }

end;

{******************************************************************************

Compare string greater caseless

Compares two strings, and returns if the source string is greater than the
destination.

******************************************************************************}

function gtr(view d, s: string): boolean;

var i:      integer; { index for string }
    l:      integer; { common region length }
    r:      boolean; { result save }

begin

   { check case where one string is null, and compare lengths if so }
   if (max(s) = 0) or (max(d) = 0) then r := max(d) < max(s)
   else begin

      { find the shorter of the strings }
      if max(s) > max(d) then l := max(d) else l := max(s);
      { compare the overlapping region }
      i := 1; { set 1st character }
      { find first non-matching character }
      while (i < l) and (lcase(s[i]) = lcase(d[i])) do i := i+1;
      { check not equal, and return status based on that character }
      if lcase(s[i]) <> lcase(d[i]) then r := lcase(d[i]) < lcase(s[i])
      { if the entire common region matched, then we base the result on
        length }
      else r := max(d) < max(s)

   end;
   gtr := r { return match status }

end;

overload function gtr(d, s: pstring) { destination }
                      : boolean; { result }

begin

   if (d = nil) or (s = nil) then throw(StringNil);

   gtr := gtr(d^, s^) { return result }

end;

overload function gtr(view d: string; { destination }
                           s: pstring) { source }
                       : boolean; { result }

begin

   if s = nil then throw(StringNil);

   gtr := gtr(d, s^) { return result }

end;

overload function gtr(     d: pstring; { destination }
                      view s: string) { source }
                       : boolean; { result }

begin

   if d = nil then throw(StringNil);

   gtr := gtr(d^, s) { return result }

end;

{******************************************************************************

Compare string greater caseless padded

Compares two strings, and returns if the source string is greater than the
destination.

******************************************************************************}

function gtrp(view d, s: string): boolean;

var i:      integer; { index for string }
    ls, ld: integer; { length saves }
    l:      integer; { common region length }
    r:      boolean; { result save }

begin

   ls := len(s); { calculate this only once }
   ld := len(d);
   { check case where one string is null, and compare lengths if so }
   if (ls = 0) or (ld = 0) then r := ld < ls
   else begin

      { find the shorter of the strings }
      if ls > ld then l := ld else l := ls;
      { compare the overlapping region }
      i := 1; { set 1st character }
      { find first non-matching character }
      while (i < l) and (lcase(s[i]) = lcase(d[i])) do i := i+1;
      { check not equal, and return status based on that character }
      if lcase(s[i]) <> lcase(d[i]) then r := lcase(d[i]) < lcase(s[i])
      { if the entire common region matched, then we base the result on
        length }
      else r := ld < ls

   end;
   gtrp := r { return match status }

end;

{******************************************************************************

Find string index case sensitive

Finds the index of the source string in the destination. Case is observed.
If the source string does not exist in the destination, 0 is returned. Only
the first occurance in the destination is found.

******************************************************************************}

function indexc(view d, s: string): integer;

var i:      integer; { index for string }
    r:      boolean; { result save }

{ compare substring }

function subcmp(b: integer): boolean;

var r: boolean; { result }
    i: integer; { index for string }

begin

   r := true; { set string matches }
   { compare characters }
   for i := 1 to max(s) do if d[b+i-1] <> s[i] then r := false; { no compare }
   subcmp := r { return result }

end;

begin

   r := false;
   i := 1; { set 1st character }
   while not r and (i+max(s)-1 <= max(d)) do begin { search }

      if subcmp(i) then r := true { set found }
      else i := i+1 { next }

   end;
   if not r then i := 0; { set not found }
   indexc := i { return result }

end;

overload function indexc(d, s: pstring): integer;

begin

   if (d = nil) or (s = nil) then throw(StringNil);

   indexc := indexc(d^, s^) { return result }

end;

overload function indexc(view d: string; s: pstring): integer;

begin

   if s = nil then throw(StringNil);

   indexc := indexc(d, s^) { return result }

end;

overload function indexc(d: pstring; view s: string): integer;

begin

   if d = nil then throw(StringNil);

   indexc := indexc(d^, s) { return result }

end;

{******************************************************************************

Find string index padded case sensitive

Finds the index of the source string in the destination. Case is observed.
If the source string does not exist in the destination, 0 is returned. Only
the first occurance in the destination is found.

******************************************************************************}

function indexcp(view d, s: string): integer;

var i:      integer; { index for string }
    ls, ld: integer; { length saves }
    r:      boolean; { result save }

{ compare substring }

function subcmp(b: integer): boolean;

var r: boolean; { result }
    i: integer; { index for string }

begin

   r := true; { set string matches }
   { compare characters }
   for i := 1 to ls do if d[b+i-1] <> s[i] then r := false; { no compare }
   subcmp := r { return result }

end;

begin

   ls := len(s); { calculate this only once }
   ld := len(d);
   r := false;
   i := 1; { set 1st character }
   while not r and (i+ls-1 <= ld) do begin { search }

      if subcmp(i) then r := true { set found }
      else i := i+1 { next }

   end;
   if not r then i := 0; { set not found }
   indexcp := i { return result }

end;

{******************************************************************************

Find string index caseless

Finds the index of the source string in the destination. Case is not observed.
If the source string does not exist in the destination, 0 is returned. Only
the first occurance in the destination is found.

******************************************************************************}

function index(view d, s: string): integer;

var i:      integer; { index for string }
    r:      boolean; { result save }

{ compare substring }

function subcmp(b: integer): boolean;

var r: boolean; { result }
    i: integer; { index for string }

begin

   r := true; { set string matches }
   { compare characters }
   for i := 1 to max(s) do if lcase(d[b+i-1]) <> lcase(s[i]) then
      r := false; { no compare }
   subcmp := r { return result }

end;

begin

   r := false;
   i := 1; { set 1st character }
   while not r and (i+max(s)-1 <= max(d)) do begin { search }

      if subcmp(i) then r := true { set found }
      else i := i+1 { next }

   end;
   if not r then i := 0; { set not found }
   index := i { return result }

end;

overload function index(d, s: pstring): integer;

begin

   if (d = nil) or (s = nil) then throw(StringNil);

   index := index(d^, s^) { return result }

end;

overload function index(view d: string; s: pstring): integer;

begin

   if s = nil then throw(StringNil);

   index := index(d, s^) { return result }

end;

overload function index(d: pstring; view s: string): integer;

begin

   if d = nil then throw(StringNil);

   index := index(d^, s) { return result }

end;

{******************************************************************************

Find string index caseless padded

Finds the index of the source string in the destination. Case is not observed.
If the source string does not exist in the destination, 0 is returned. Only
the first occurance in the destination is found.

******************************************************************************}

function indexp(view d, s: string): integer;

var i:      integer; { index for string }
    ls, ld: integer; { length saves }
    r:      boolean; { result save }

{ compare substring }

function subcmp(b: integer): boolean;

var r: boolean; { result }
    i: integer; { index for string }

begin

   r := true; { set string matches }
   { compare characters }
   for i := 1 to ls do if lcase(d[b+i-1]) <> lcase(s[i]) then
      r := false; { no compare }
   subcmp := r { return result }

end;

begin

   ls := len(s); { calculate this only once }
   ld := len(d);
   r := false;
   i := 1; { set 1st character }
   while not r and (i+ls-1 <= ld) do begin { search }

      if subcmp(i) then r := true { set found }
      else i := i+1 { next }

   end;
   if not r then i := 0; { set not found }
   indexp := i { return result }

end;

{******************************************************************************

Extract substring

Extracts the indicated range of characters from the source, and places that
into the destination.

Forms:

procedure extract(pstring, string, l, r) - Extract to dynamic result.
procedure extract(string, string, l, r) - Extract to padded result.

******************************************************************************}

procedure extract(var  d:    string;   { destination }
                  view s:    string;   { source }
                       l, r: integer); { range of characters }

var ln: integer; { length of string }
    i:  integer; { index for string }

begin

   if l > r then clears(d) { result is null, clear }
   else begin

      ln := len(s); { find length of source }
      if (l > ln) or (r > ln) then { index out of range }
         throw(IndexOutOfRange);
      if r-l+1 > max(d) then { too large for result }
         throw(StringDestinationOverflow);
      for i := l to r do d[i-l+1] := s[i]; { transfer string contents }
      for i := r-l+1+1 to max(d) do d[i] := ' ' { clear remainder }

   end

end;

overload function extract(view s:    string;  { source }
                               l, r: integer) { range of characters }
                          : pstring; { result }

var i: integer; { index for string }
    d: pstring; { result string }

begin

   if l > r then newstr(d, 0) { null, allocate null result }
   else begin

      newstr(d, r-l+1); { allocate destination }
      if (l > max(s)) or (r > max(s)) then { index out of range }
         throw(IndexOutOfRange);
      for i := l to r do d^[i-l+1] := s[i] { transfer string contents }

   end;

   extract := d { return result }

end;

overload function extract(view s:    pstring; { source }
                               l, r: integer) { range of characters }
                          : pstring; { result }

begin

   if s = nil then throw(StringNil);
   extract := extract(s^, l, r); { perform extract to temp }

end;

{******************************************************************************

Insert string

Inserts the source string into the second string at the indicated position,
then places that in the destination.

Forms:

procedure insert(pstring, string, string, integer) - Insert string to find
                                                     dynamic result.
procedure insert(string, string, integer) - Insert padded string to find
                                            padded result.

******************************************************************************}

procedure insert(var d: string; view s: string; p: integer);

var i:      integer; { index for strings }
    ls, ld: integer; { length of strings }

begin

   ld := len(d); { find lengths }
   ls := len(s);
   { check combined string larger than destination }
   if ld+ls > max(d) then throw(StringDestinationOverflow);
   { check any part of destination off end of destination }
   if p+ls > max(d) then throw(IndexOutOfRange);
   { move destination string up }
   for i := ld+ls downto p+ls do d[i] := d[i-ls];
   { move source into place }
   for i := 1 to ls do begin

      d[p] := s[i]; { move character }
      p := p+1 { next }

   end

end;

overload function insert(view sa, sb: string; p: integer): pstring;

var i: integer; { index for strings }
    t: pstring; { temp to build destination }

begin

   newstr(t, max(sa)+max(sb)); { get the destination string }
   for i := 1 to p-1 do t^[i] := sa[i]; { place 1st part of first }
   for i := p to p+max(sb)-1 do t^[i] := sb[i-p+1]; { place second }
   { place second part of first }
   for i := p to max(sa) do t^[i+max(sb)] := sa[i];

   insert := t { return result }

end;
   
overload function insert(sa, sb: pstring; p: integer): pstring;

begin

   if (sa = nil) or (sb = nil) then throw(StringNil);
   insert := insert(sa^, sb^, p); { return insert }

end; 

overload function insert(view sa: string; sb: pstring; p: integer): pstring;

begin

   if sb = nil then throw(StringNil);
   insert := insert(sa, sb^, p); { insert to temp }

end; 

overload function insert(sa: pstring; view sb: string; p: integer): pstring;

begin

   if sa = nil then throw(StringNil);
   insert := insert(sa^, sb, p); { insert to temp }

end; 

{******************************************************************************

Substitute string

Replaces one string with another.

******************************************************************************}

procedure subst(var  s: string; { string to substitute in }
                view m: string; { match string }
                view r: string); { replacement string }

var i: integer; { string index }
    d, t: packed array [1..maxlin] of char; { temp strings }

begin

   i := index(s, m); { find substring }
   if i > 0 then begin { a substring was found, advance to replace }

      if i > 1 then begin { there is a left }

         extract(d, s, 1, i-1); { get left side }
         insert(d, r, i) { insert replacement string }

      end;
      if i+len(m) <= len(s) then begin { there is a right }

         extract(t, s, i+len(m), len(s)); { get right side }
         insert(d, t, i+len(r)) { place }

      end;
      copy(s, d) { copy to result }

   end

end;

overload function subst(view s: string; { string to substitute in }
                        view m: string; { match string }
                        view r: string) { replacement string }
                        : pstring;      { result string }

var i: integer; { string index }
    p: pstring; { result holder }

begin

   openstring; { start new level }
   i := index(s, m); { find substring }
   if i > 0 then { a substring was found, advance to replace }
      { construct new string from left, replacement and right }
      p := cat(cat(extract(s, 1, i-1), r), extract(s, i+max(m), max(s)))
   else p := copy(s); { just copy existing string }
   upstring(p); { move result to surrounding block }
   closestring; { close out level }

   subst := p { return result }

end;

overload function subst(view s: string; view m: string; r: pstring): pstring;

begin 

   if r = nil then throw(StringNil); 

   subst := subst(s, m, r^)

end;

overload function subst(view s: string; m: pstring; view r: string): pstring;

begin 

   if m = nil then throw(StringNil); 

   subst := subst(s, m^, r)

end;

overload function subst(view s: string; m: pstring; r: pstring): pstring;

begin 

   if (m = nil) or (r = nil) then throw(StringNil); 

   subst := subst(s, m^, r^)

end;

overload function subst(s: pstring; view m: string; view r: string): pstring;

begin 

   if s = nil then throw(StringNil); 

   subst := subst(s^, m, r)

end;

overload function subst(s: pstring; view m: string; r: pstring): pstring;

begin 

   if (s = nil) or (r = nil) then throw(StringNil); 

   subst := subst(s^, m, r^)

end;

overload function subst(s: pstring; m: pstring; view r: string): pstring;

begin 

   if (s = nil) or (m = nil) then throw(StringNil); 

   subst := subst(s^, m^, r)

end;

overload function subst(s: pstring; m: pstring; r: pstring): pstring;

begin 

   if (s = nil) or (m = nil) or (r = nil) then throw(StringNil); 

   subst := subst(s^, m^, r^)

end;

procedure substc(var  s: string; { string to substitute in }
                 view m: string; { match string }
                 view r: string); { replacement string }

var i: integer; { string index }
    d, t: packed array [1..maxlin] of char; { temp strings }

begin

   i := indexc(s, m); { find substring }
   if i > 0 then begin { a substring was found, advance to replace }

      if i > 1 then begin { there is a left }

         extract(d, s, 1, i-1); { get left side }
         insert(d, r, i) { insert replacement string }

      end;
      if i+len(m) <= len(s) then begin { there is a right }

         extract(t, s, i+len(m), len(s)); { get right side }
         insert(d, t, i+len(r)) { place }

      end;
      copy(s, d) { copy to result }

   end

end;

overload function substc(view s: string; { string to substitute in }
                         view m: string; { match string }
                         view r: string) { replacement string }
                         : pstring;      { result string }

var i: integer; { string index }
    p: pstring; { result holder }

begin

   openstring; { start new level }
   i := indexc(s, m); { find substring }
   if i > 0 then { a substring was found, advance to replace }
      { construct new string from left, replacement and right }
      p := cat(cat(extract(s, 1, i-1), r), extract(s, i+max(m), max(s)))
   else p := copy(s); { just copy existing string }
   upstring(p); { export to surrounding block }
   closestring; { close out level }

   substc := p { return result }

end;

overload function substc(view s: string; view m: string; r: pstring): pstring;

begin 

   if r = nil then throw(StringNil); 

   substc := substc(s, m, r^)

end;

overload function substc(view s: string; m: pstring; view r: string): pstring;

begin 

   if m = nil then throw(StringNil); 

   substc := substc(s, m^, r)

end;

overload function substc(view s: string; m: pstring; r: pstring): pstring;

begin 

   if (m = nil) or (r = nil) then throw(StringNil); 

   substc := substc(s, m^, r^)

end;

overload function substc(s: pstring; view m: string; view r: string): pstring;

begin 

   if s = nil then throw(StringNil); 

   substc := substc(s^, m, r)

end;

overload function substc(s: pstring; view m: string; r: pstring): pstring;

begin 

   if (s = nil) or (r = nil) then throw(StringNil); 

   substc := substc(s^, m, r^)

end;

overload function substc(s: pstring; m: pstring; view r: string): pstring;

begin 

   if (s = nil) or (m = nil) then throw(StringNil); 

   substc := substc(s^, m^, r)

end;

overload function substc(s: pstring; m: pstring; r: pstring): pstring;

begin 

   if (s = nil) or (m = nil) or (r = nil) then throw(StringNil); 

   substc := substc(s^, m^, r^)

end;

procedure substall(var s: string;   { string to substitute in }
                   view m: string;  { match string }
                   view r: string); { replacement string }

var i: integer; { string index }
    d, t: packed array [1..maxlin] of char; { temp strings }

{ match substring at index }

function match(view s: string; p: integer; view m: string): boolean;

var f: boolean; { match flag }
    i: integer; { string index }

begin

   if p+max(m)-1 > max(s) then f := false { does not fit }
   else begin

      f := true; { assume match }
      { match all characters of string }
      for i := 1 to max(m) do if lcase(s[p+i-1]) <> lcase(m[i]) then f := false

   end;

   match := f { return match status }

end;

begin

   i := 1; { set 1st character of string }
   while i < max(s) do begin { match and replace }

      if match(s, i, m) then begin

         if i > 1 then begin { there is a left }

            extract(d, s, 1, i-1); { get left side }
            insert(d, r, i); { insert replacement string }

         end;
         if i+len(m) <= len(s) then begin { there is a right }

            extract(t, s, i+len(m), len(s)); { get right side }
            insert(d, t, i+len(r)); { place }

         end;
         i := i+len(r); { move past replacement point }
         copy(s, d) { copy to result }

      end else i := i+1 { next character }

   end

end;

overload function substall(view s: string; { string to substitute in }
                           view m: string; { match string }
                           view r: string) { replacement string }
                           : pstring;      { result }

var i: integer; { string index }
    d: pstring; { destination holder }

{ match substring at index }

function match(view s: string; p: integer; view m: string): boolean;

var f: boolean; { match flag }
    i: integer; { string index }

begin

   if p+max(m)-1 > max(s) then f := false { does not fit }
   else begin

      f := true; { assume match }
      { match all characters of string }
      for i := 1 to max(m) do if lcase(s[p+i-1]) <> lcase(m[i]) then f := false

   end;

   match := f { return match status }

end;

begin

   openstring; { start string level }
   i := 1; { set 1st character of string }
   copy(d, s); { copy to result in case of no match }
   while i < max(d^) do begin { match and replace }

      if match(d^, i, m) then begin

         { construct new string from left, replacement and right }
         d := cat(cat(extract(d, 1, i-1), r), extract(d, i+max(m), max(d^)));
         i := i+max(r) { move past replacement point }

      end else i := i+1 { next character }

   end;
   upstring(d); { move result to surrounding block }
   closestring; { end string level }

   substall := d { return result }

end;

overload function substall(view s: string; view m: string; r: pstring): pstring;

begin

   if r = nil then throw(StringNil);

   substall := substall(s, m, r^) { return result }

end;

overload function substall(view s: string; m: pstring; view r: string): pstring;

begin

   if m = nil then throw(StringNil);

   substall := substall(s, m^, r) { return result }

end;

overload function substall(view s: string; m: pstring; r: pstring): pstring;

begin

   if (m = nil) or (r = nil) then throw(StringNil);

   substall := substall(s, m^, r^) { return result }

end;

overload function substall(s: pstring; view m: string; view r: string): pstring;

begin

   if s = nil then throw(StringNil);

   substall := substall(s^, m, r) { return result }

end;

overload function substall(s: pstring; view m: string; r: pstring): pstring;

begin

   if (s = nil) or (r = nil) then throw(StringNil);

   substall := substall(s^, m, r^) { return result }

end;

overload function substall(s: pstring; m: pstring; view r: string): pstring;

begin

   if (s = nil) or (m = nil) then throw(StringNil);

   substall := substall(s^, m^, r) { return result }

end;

overload function substall(s: pstring; m: pstring; r: pstring): pstring;

begin

   if (s = nil) or (m = nil) or (r = nil) then throw(StringNil);

   substall := substall(s^, m^, r^) { return result }

end;

procedure substcall(var s: string;   { string to substitute in }
                    view m: string;  { match string }
                    view r: string); { replacement string }

var i: integer; { string index }
    d, t: packed array [1..maxlin] of char; { temp strings }

{ match substring at index }

function match(view s: string; p: integer; view m: string): boolean;

var f: boolean; { match flag }
    i: integer; { string index }

begin

   if p+max(m)-1 > max(s) then f := false { does not fit }
   else begin

      f := true; { assume match }
      { match all characters of string }
      for i := 1 to max(m) do if s[p+i-1] <> m[i] then f := false

   end;

   match := f { return match status }

end;

begin

   copy(d, s); { default to string as is }
   i := 1; { set 1st character of string }
   while i < max(s) do begin { match and replace }

      if match(s, i, m) then begin

         if i > 1 then begin { there is a left }

            extract(d, s, 1, i-1); { get left side }
            insert(d, r, i) { insert replacement string }

         end;
         if i+len(m) <= len(s) then begin { there is a right }

            extract(t, s, i+len(m), len(s)); { get right side }
            insert(d, t, i+len(r)) { place }

         end;
         i := i+len(r); { move past replacement point }
         copy(s, d) { copy to result }

      end else i := i+1 { next character }

   end

end;

overload function substcall(view s: string; { string to substitute in }
                            view m: string; { match string }
                            view r: string) { replacement string }
                            : pstring;      { result }

var i: integer; { string index }
    d: pstring; { destination holder }

{ match substring at index }

function match(view s: string; p: integer; view m: string): boolean;

var f: boolean; { match flag }
    i: integer; { string index }

begin

   if p+max(m)-1 > max(s) then f := false { does not fit }
   else begin

      f := true; { assume match }
      { match all characters of string }
      for i := 1 to max(m) do if s[p+i-1] <> m[i] then f := false

   end;

   match := f { return match status }

end;

begin

   openstring; { start string level }
   i := 1; { set 1st character of string }
   copy(d, s); { copy to result in case of no match }
   while i < max(s) do begin { match and replace }

      if match(s, i, m) then begin

         { construct new string from left, replacement and right }
         d := cat(cat(extract(s, 1, i-1), r), extract(s, i+max(m), max(s)));
         i := i+max(r) { move past replacement point }

      end else i := i+1 { next character }

   end;
   upstring(d); { move result to surrounding block }
   closestring; { end string level }

   substcall := d { return result }

end;

overload function substcall(view s: string; view m: string; r: pstring): pstring;

begin

   if r = nil then throw(StringNil);

   substcall := substcall(s, m, r^) { return result }

end;

overload function substcall(view s: string; m: pstring; view r: string): pstring;

begin

   if m = nil then throw(StringNil);

   substcall := substcall(s, m^, r) { return result }

end;

overload function substcall(view s: string; m: pstring; r: pstring): pstring;

begin

   if (m = nil) or (r = nil) then throw(StringNil);

   substcall := substcall(s, m^, r^) { return result }

end;

overload function substcall(s: pstring; view m: string; view r: string): pstring;

begin

   if s = nil then throw(StringNil);

   substcall := substall(s^, m, r) { return result }

end;

overload function substcall(s: pstring; view m: string; r: pstring): pstring;

begin

   if (s = nil) or (r = nil) then throw(StringNil);

   substcall := substcall(s^, m, r^) { return result }

end;

overload function substcall(s: pstring; m: pstring; view r: string): pstring;

begin

   if (s = nil) or (m = nil) then throw(StringNil);

   substcall := substcall(s^, m^, r) { return result }

end;

overload function substcall(s: pstring; m: pstring; r: pstring): pstring;

begin

   if (s = nil) or (m = nil) or (r = nil) then throw(StringNil);

   substcall := substcall(s^, m^, r^) { return result }

end;

{******************************************************************************

Repeat string

Finds the source string, repeated the specified number of times, into the
destination.

Forms: 

procedure rep(pstring, string, integer) - repeat source string into dynamic
                                          result.
procedure rep(string, string, integer) - repeat source string to padded string.

******************************************************************************}

function rep(view s: string;  { source string }
                  r: integer) { repeat count }
             : pstring; { result }

var x, i: integer; { string indexes }
    d:    pstring; { result holder }

begin

   if r < 0 then throw(NegativeRepeatCount);
   newstr(d, max(s)*r); { allocate destiantion }
   if max(s) > 0 then begin { output has length }

      x := 1; { set 1st character of destination }
      while r <> 0 do begin { repeat to destination }

         for i := 1 to max(s) do begin { copy }

            d^[x] := s[i]; { copy character }
            x := x+1 { next character }

         end;
         r := r-1 { count }

      end

   end;

   rep := d { return result }

end;

overload function rep(s: pstring;  { source string }
                      r: integer) { repeat count }
                      : pstring; { result }

begin

   if s = nil then throw(StringNil);

   rep := rep(s^, r)

end;

overload procedure rep(var  d: string;   { destination string }
                       view s: string;   { source string }
                            r: integer); { repeat count }

begin

   clears(d); { clear destination }
   while r <> 0 do begin { repeat to destination }

      cat(d, s); { concatenate to accumulator }
      r := r-1 { count }

   end

end;

{******************************************************************************

Trim leading and trailing spaces

Removes leading and trailing spaces from string. Note that "remove trailing
spaces" is redundant to the idea of padded strings.

Forms:

procedure trim(pstring, string) - trim to dynamic result.
procedure trim(string, string) - trim to padded result.

******************************************************************************}

function trim(view s: string): pstring;

var si, ei: integer; { leading and trailing spaces }
    d:      pstring; { result holder }

begin

   if max(s) = 0 then newstr(d, 0) { string empty, create empty result }
   else begin { process substring }

      si :=1; { bracket the string }
      ei := max(s);
      { find leading non-space }
      while (s[si] = ' ') and (si < max(s)) do si := si+1;
      { find trailing non-space }
      while (s[ei] = ' ') and (ei > 1) do ei := ei-1;
      { check empty case }
      if (si = ei) and (s[si] = ' ') then newstr(d, 0) { yes, empty string }
      else d := extract(s, si, ei) { extract substring }

   end;

   trim := d { return result }

end;

overload function trim(s: pstring): pstring;

begin

   if s = nil then throw(StringNil);

   trim := trim(s^)  { return result }

end;

overload procedure trim(var d: string; view s: string);

var l:      integer; { length }
    si, di: integer; { indexes for string }

begin

   l := len(s); { find length }
   if l <> 0 then begin { not null }

      si := 1; { find leading space }
      while (s[si] = ' ') and (si < l) do si := si+1;
      di := 1; { index string start }
      while si <= l do begin { move string characters }

         { check destination overrun }
         if di > max(d) then throw(StringDestinationOverflow);
         d[di] := s[si]; { move single }
         si := si+1; { next }
         di := di+1

      end;
      { pad the rest }
      while di <= max(d) do begin

         d[di] := ' '; { blank out }
         di := di+1 { next }

      end

   end else clears(d) { input empty, clear result as well }

end;

{******************************************************************************

Count number of words

Counts the number of words in the string, and returns that. Words are non-space
character sequences divided by spaces.

******************************************************************************}

function words(view s: string): integer;

var si: integer; { string index }
    wc: integer; { word count }
    l:  integer; { length of string }

begin

   wc := 0; { clear word count }
   l := len(s); { find length }
   si := 1; { set 1st character in string }
   if l <> 0 then begin { find words }

      repeat { count words }

         { skip leading spaces }
         while (s[si] = ' ') and (si < l) do si := si+1; 
         { skip characters }
         while (s[si] <> ' ') and (si < l) do si := si+1;
         wc := wc+1; { count words }
         { skip trailing spaces }
         while (s[si] = ' ') and (si < l) do si := si+1; 

      until si = l { until end of string }

   end;
   words := wc { return word count }

end;

overload function words(s: pstring): integer;

begin

   if s = nil then throw(StringNil);

   words := words(s^)

end;

{******************************************************************************

Extract words

Extracts the starting word to the indicated word from the source string to
the target string. The number of spaces between words are reduced to one, and
leading and trailing spaces removed.

For the padded version, we do the fill into the same string of the substring
without using a buffer by using the principle: the result will either be at the
same character position as its place in the source, or the source will be after
the same position in the result.

Forms:

procedure extwords(pstring, string, integer, integer) - Extracts a string to
                                                        dynamic result.
procedure extwords(string, string, integer, integer) - Extracts a string to
                                                       padded result.

******************************************************************************}

function extwords(view s: string; l, r: integer): pstring;

var si: integer; { index for string }
    tc: integer; { total characters in result }
    tw: integer; { total words in result }
    di: integer; { index for destination }
    wo: boolean; { word output }
    d:  pstring; { result holder }
    i:  integer;

{ check next character }

function chkchr: char;

var c: char;

begin

   if si <= max(s) then c := s[si] { return character from string }
   else c := ' '; { return space }
   chkchr := c { return result }

end;

{ skip spaces }

procedure skpspc;

begin

   while (chkchr = ' ') and (si <= max(s)) do si := si+1

end;

{ skip word }

procedure skpwrd;

begin

   skpspc;
   while (chkchr <> ' ') and (si <= max(s)) do begin

      si := si+1; { next character }
      { if within word range, count word characters }
      if (tw >= l) and (tw <= r) then begin

         tc := tc+1; { count }
         wo := true { set word output }

      end

   end

end;

{ place word }

procedure plcwrd;

begin

   skpspc;
   while (chkchr <> ' ') and (si <= max(s)) do begin

      d^[di] := chkchr; { place destination character }
      di := di+1; { next }
      si := si+1 { next character }

   end

end;

begin

   tc := 0; { set number of result characters }
   tw := 0; { set number of words }
   si := 1; { set 1st index source }
   { run first pass to tally words and characters }
   skpspc; { skip any spaces }
   wo := false; { set no word output }
   while si <= max(s) do begin { count }

      tw := tw+1; { count words }
      { if another word already output, and this word will be output, count
        a separator space }
      if wo and (tw >= l) and (tw <= r) then tc := tc+1;
      skpwrd; { skip a word }
      skpspc { skip spaces }

   end;
   { check valid words are specified }
   if (l < 1) or (l > tw) or (r < 1) or (r > tw) or (l > r) then
      throw(WordIndexOutOfRange);
   newstr(d, tc); { allocate a string for result }
   si := 1; { set 1st character source }
   di := 1; { set 1st character destination }
   for i := 1 to l-1 do skpwrd; { skip words to left of l }
   for i := l to r do begin { process words to result }

      plcwrd; { place word to result }
      if i < r then begin { not last word, place separator space }

         d^[di] := ' '; { place separator space }
         di := di+1 { next }
 
      end

   end;

   extwords := d { return result }

end;

overload function extwords(s: pstring; l, r: integer): pstring;

begin

   if s = nil then throw(StringNil);

   extwords := extwords(s^, l, r)

end;

overload procedure extwords(var d: string; view s: string; l, r: integer);

var si: integer; { index for string }
    tw: integer; { total words in result }
    di: integer; { index for destination }
    i:  integer;

{ check next character }

function chkchr: char;

var c: char;

begin

   if si <= max(s) then c := s[si] { return character from string }
   else c := ' '; { return space }
   chkchr := c { return result }

end;

{ skip spaces }

procedure skpspc;

begin

   while (chkchr = ' ') and (si <= max(s)) do si := si+1

end;

{ skip word }

procedure skpwrd;

begin

   skpspc;
   while (chkchr <> ' ') and (si <= max(s)) do si := si+1; { next character }

end;

{ place word }

procedure plcwrd;

begin

   skpspc;
   while (chkchr <> ' ') and (si <= max(s)) do begin

      d[di] := chkchr; { place destination character }
      di := di+1; { next }
      si := si+1 { next character }

   end

end;

begin

   tw := 0; { set number of words }
   si := 1; { set 1st index source }
   { run first pass to tally words and characters }
   skpspc; { skip any spaces }
   while si <= max(s) do begin { count }

      tw := tw+1; { count words }
      skpwrd; { skip a word }
      skpspc { skip spaces }

   end;
   { check valid words are specified }
   if (l < 1) or (l > tw) or (r < 1) or (r > tw)  or (l > r) then
      throw(WordIndexOutOfRange);
   si := 1; { set 1st character source }
   di := 1; { set 1st character destination }
   for i := 1 to l-1 do skpwrd; { skip words to left of l }
   for i := l to r do begin { process words to result }

      plcwrd; { place word to result }
      if i < r then begin { not last word, place separator space }

         d[di] := ' '; { place separator space }
         di := di+1 { next }
 
      end

   end;
   while di <= max(d) do begin { fill remaining string with blanks }

      d[di] := ' ';
      di := di+1

   end

end;

{******************************************************************************

Read string from file

Reads a string from the file to the destination. For the dynamic version, 
because we cant know how long the string is before we read it in, we have to
use a fixed length buffer, which is 250 characters long. 

If the string read is too large for the destination, it is trucated, and the
overflow error flag set. The rest of the input string up to eoln is then
discarded. Overflow will apply even if the end of the read string is blank.

Forms:

procedure reads(text, pstring, boolean) - Read string to dynamic result, with
                                          overflow indication.
procedure reads(text, string, boolean) - Read string to padded result. With
                                         overflow indication.
procedure reads(text, pstring) - Read string to dynamic result.
procedure reads(text, string) - Read string to padded result.
procedure reads(pstring, boolean) - Read string to dynamic result, with
                                    overflow indication.
procedure reads(string, boolean) - Read string to padded result. With overflow
                                   indication.
procedure reads(pstring) - Read string to dynamic result.
procedure reads(string) - Read string to padded result.

******************************************************************************}

procedure reads(var f:   text;     { file to read from }
                var s:   pstring;  { string to read to }
                var ovf: boolean); { overflow flag }

var i:    integer; { index for string }
    c:    char;    { character discard holder }
    buff: packed array [1..maxlin] of char; { line buffer }

begin

   i := 1; { set 1st string character }
   ovf := false; { set no overflow }
   while not eoln(f) do begin { read string characters }

      { check past string end }
      if i > maxlin then begin { string overflows }

         ovf := true; { flag overflow }
         read(f, c) { read and discard next character }

      end else begin { place string character }

         read(f, buff[i]); { read next character }
         i := i+1 { next }

      end

   end;
   newstr(s, i-1); { allocate destination string }
   for i := 1 to i-1 do s^[i] := buff[i] { copy string }

end;

overload procedure reads(var f:   text;     { file to read from }
                         var s:   string;   { string to read to }
                         var ovf: boolean); { overflow flag }

var i: integer; { index for string }
    l: integer; { length of string }
    c: char;    { character discard holder }

begin

   l := max(s); { find length of string }
   for i := 1 to l do s[i] := ' '; { clear destination }
   i := 1; { set 1st string character }
   ovf := false; { set no overflow }
   while not eoln(f) do begin { read string characters }

      { check past string end }
      if i > l then begin { string overflows }

         ovf := true; { flag overflow }
         read(f, c) { read and discard next character }

      end else begin { place string character }

         read(f, s[i]); { read next character }
         i := i+1 { next }

      end

   end

end;

overload procedure reads(var f: text;     { file to read from }
                         var s: pstring); { string to read to }

var ovf: boolean;

begin

   reads(f, s, ovf); { read string }
   if ovf then throw(StringReadOverflow)

end;

overload procedure reads(var f: text;    { file to read from }
                         var s: string); { string to read to }

var ovf: boolean;

begin

   reads(f, s, ovf); { read string }
   if ovf then throw(StringReadOverflow)

end;

overload procedure reads(var s:   pstring;  { string to read to }
                         var ovf: boolean); { overflow flag }

begin

   reads(input, s, ovf)

end;

overload procedure reads(var s:   string;   { string to read to }
                         var ovf: boolean); { overflow flag }

begin

   reads(input, s, ovf)

end;

overload procedure reads(var s: pstring); { string to read to }

begin

   reads(input, s)

end;

overload procedure reads(var s: string); { string to read to }

begin

   reads(input, s)

end;

{******************************************************************************

Place number in string

Places an arbitrary radix number in a string. Used to generate numbers for all
of the numeric convertion routines.

******************************************************************************}

procedure nums(var s: string;   { result string }
                   n: integer;  { number to convert }
                   r: integer); { radix }
 
var dc:   integer; { digit counter }
    sign: boolean; { sign of number }
    t:    integer; { temp }

{ place single digit }

procedure plcdig;

var v: integer; { digit value }
    c: char;    { digit character }

begin

   v := n mod r; { extract digit }
   { convert ascii }
   if v >= 10 then c := chr(v+ord('A')-10)
   else c := chr(v+ord('0'));
   n := n div r; { reduce number }
   s[dc] := c { place digit }
 
end;

begin

   clears(s); { clear result }
   sign := false; { set not negative }
   if n < 0 then begin
 
      n := abs(n); { remove sign }
      sign := true { set signed }

   end;
   { count the digits in the number }
   dc := 1; { set default count }
   t := n; { save number }
   t := t div r; { remove first digit }
   while t > 0 do begin { reduce }

      t := t div r; { shift out lsd }
      dc := dc+1 { count }

   end;
   if sign then dc := dc+1; { add in sign }
   { now walk backwards to place digits and sign }
   plcdig; { place lsd }
   dc := dc-1; { count }
   while n > 0 do begin { reduce }

      plcdig; { place digits }
      dc := dc-1 { count }

   end;
   if sign then s[dc] := '-' { place sign }

end;

overload procedure nums(var s: string;   { result string }
                            n: integer;  { integer to print }
                            f: integer;  { field }
                            r: integer); { radix }
 
var si:   integer; { index for output string }
    os:   packed array [1..maxlin] of char; { output store array }
    lhs:  integer; { length of hex string }
    i:    integer; { index }

{ place character in output string }

procedure plcchr(c: char);

begin

   if si > max(s) then throw(FormatTooLarge);
   s[si] := c; { place character }
   si := si+1 { next }

end;

begin

   clears(s); { clear result }
   if f < 1 then { bad field }
      throw(InvalidFieldSpecification);
   if (r <> 10) and (n < 0) then throw(NegativeValueNondecimal);
   nums(os, n, r); { find number in ASCII }
   lhs := len(os); { set length of result }
   if lhs > f then f := lhs; { print minimum digits }
   si := 1; { clear output index }
   for i := 1 to f-lhs do plcchr(' '); { process left justify }
   for i := 1 to lhs do plcchr(os[i]); { place digits }   

end;

overload function nums(n: integer; { number to place }
                       f: integer; { field }
                       r: integer) { radix }
                       : pstring;  { result }


var buff: packed array [1..maxlin] of char; { string buffer }
    s:    pstring; { result holder }

begin

   if f > 0 then begin { right justify }

      nums(buff, n, f, r); { place in buffer }
      copy(s, buff) { place to result string }

   end else begin { left justified }

      nums(buff, n, 1, r); { place in buffer }
      f := abs(f); { find net field }
      if len(buff) > f then f := len(buff); { set minimum format }
      newstr(s, f); { create result }
      copy(s^, buff) { copy to result }

   end;

   nums := s { return that }
   
end;

overload procedure nums(var s: string; i: integer; view fmt: string; 
                        r: integer);

{ valid digits in output }

const digit = ['0'..'9', 'a'..'z', 'A'..'Z'];

var buff:   packed array [1..maxlin] of char;
    pi, pf: integer; { indexes for integer and format }
    dlrout: boolean; { dollar sign already output }

begin

   nums(buff, i, 1, r); { first, get number into standard form }
   copy(s, fmt); { copy format string to result string }
   pi := len(buff); { index last character of converted integer }
   pf := len(s); { index last character of format }
   dlrout := false; { set no dollar sign output }
   while pf > 0 do begin { interpret format }

      if s[pf] in ['9', '0', '-', '+', ',', '$', '&', '%'] then 
         case s[pf] of { control character }

         '9': if pi < 1 then s[pf] := ' '
                 else if not (buff[pi] in digit) then s[pf] := ' '
                    else begin { valid digit exists }

            s[pf] := buff[pi]; { transfer digit }
            pi := pi-1 { back up one in number }

         end;
         '0': if pi > 0 then
                 if buff[pi] in digit then begin { valid digit exists }

            s[pf] := buff[pi]; { transfer digit }
            pi := pi-1 { back up one in number }

         end;
         '-': if pi < 1 then s[pf] := ' ' { knock out sign }
              else if buff[pi] in digit then 
                 throw(NumberOverflowsFormat)
              else pi := pi-1; { back up over sign }
         '+': if pi > 0 then begin

            if buff[pi] in digit then 
               throw(NumberOverflowsFormat);
            { if sign is negative, change, otherwise leave '+' }
            if buff[pi] = '-' then begin

               s[pf] := '-';
               pi := pi-1 { back up over sign }

            end

         end;
         ',': begin

            if pi < 1 then begin

               s[pf] := ' '; { out of digits, kill with space }
               if pf > 1 then { check left is '$' }
                  if s[pf-1] = '$' then { yes, treat as '$' }
                     if not dlrout then begin

                  s[pf] := '$'; { set '$' }
                  dlrout := true { set was output }

               end

            end else if not (buff[pi] in digit) then begin

               s[pf] := ' '; { out of digits, kill with space }
               if pf > 1 then { check left is '$' }
                  if s[pf-1] = '$' then { yes, treat as '$' }
                     if not dlrout then begin

                  s[pf] := '$'; { set '$' }
                  dlrout := true { set was output }

               end

            end

         end;
         '$', '&', '%': begin

            if pi < 1 then begin { no data left in number }

               if not dlrout then dlrout := true { first occurance, output }
               else s[pf] := ' ' { else knock it out }

            end else if (buff[pi] in digit) then begin

               s[pf] := buff[pi]; { transfer digit }
               pi := pi-1 { back up one in number }

            end else begin { no digits left in number }

               if not dlrout then dlrout := true { first occurance, output }
               else s[pf] := ' ' { else knock it out }

            end

         end;

      end;
      pf := pf-1 { next format character }

   end;
   if pi > 0 then begin

      if buff[pi] in ['0'..'9'] then throw(NumberOverflowsFormat);
      if buff[pi] = '-' then throw(NegativeNotPlaced)

   end

end;

{******************************************************************************

Place integer in string

Places the given integer in a string, with the given field.

Forms:

procedure ints(string, integer, integer) - Place integer in padded string by
                                           field.
function ints(integer, integer): pstring - Place integer in dynamic string by
                                           field, and return that.
procedure ints(string, integer) - Place integer in padded string with field of
                                  1.
function ints(pstring, integer) - Place integer in dynamic string with field
                                  of 1.
procedure ints(string, integer, string) - Place integer in padded string by
                                          using format string.
function ints(integer, string): pstring - Place integer in dynamic string by
                                          using format string, and return.

The field can be positive, or negative. If negative, left alignment is
specified.

In the format string forms, the contents of the format string is copied to
the target string, then the control characters within the string are
interpreted as follows:

9 - A digit, is replaced with the integer power in 10's, as counted from the
rightmost digit. If there is no digit > 0 above this position, a space is
output.

0 - Same as digit, but a leading zero is used, instead of a space.

- - Sign, if the number is negative, a sign appears here, otherwise a space.
    Note number must be completely to the right of this.

+ - As sign, but "+" appears instead of space.
    Note number must be completely to the right of this.

, - Outputs ",", but only if digits exist to the left of this position,
    otherwise space. If the format character to the left of the ',' is '$',
    then that is output instead of space.

$ - If there is a significant digit left, this is placed at the position,
    otherwise, if a '$' has not already been output, that is placed. If it
    has, it is replaced with space.

Example:

"-999,990.00"

For values:

1       "       0.01"
1000    "      10.00"
-100000 "-  1,000.00"

******************************************************************************}

procedure ints(var s: string;   { string to place into }
                   i: integer;  { integer to place }
                   f: integer); { field }

begin

   nums(s, i, f, 10) { convert }

end;

overload function ints(i: integer; { integer to place }
                       f: integer) { field }
                       : pstring;  { result }

begin

   ints := nums(i, f, 10) { return result }
   
end;

overload procedure ints(var s: string;   { string to place into }
                            i: integer); { integer to place }

begin

   ints(s, i, 1) { place integer with 1 field }

end;

overload function ints(i: integer) { integer to place }
                       : pstring;      { result }

begin

   ints := ints(i, 1) { return integer with 1 field }

end;

overload procedure ints(var s: string; i: integer; view fmt: string);

begin

   nums(s, i, fmt, 10) { place result }

end;

overload function ints(i: integer; view fmt: string): pstring;

var s: pstring;

begin

   newstr(s, len(fmt)); { get a result string }
   ints(s^, i, fmt); { place the number as formatted }

   ints := s { return result }

end;

{******************************************************************************

Place real in string

Places a real into a target string in a number of formats;

procedure reals(string, real)

   Places the real into a padded string in floating point notation, with default
   field.

Procedure reals(pstring, real)

   Places the real into a dynamic string in floating point notation, with
   default field.

Procedure reals(string, integer, real)

   Places the real into a padded string, in floating point notation with field.

Procedure reals(pstring, integer, real)

   Places the real into a dynamic string, in floating point notation with field.

Procedure reals(string, integer, integer, real)

   Places the real into a padded string, in fixed point notation with field.

Procedure reals(pstring, integer, integer, real)

   Places the real into a dynamic string, in fixed point notation with field.

The routines are built on two basic forms, one for floating point format, and
another for fixed.

If the format is larger than the destination will hold, an error results. This
applies only to the padded output versions.

******************************************************************************}

procedure reals(var s: string;   { result string }
                    r: real;     { real to write }
                    f: integer); { field }

var e:   integer; { exponent holder }
    i:   integer; { mantissa buffer }
    p:   integer; { power holder }
    si:  integer; { index for string }

{ place character in output string }

procedure plcchr(c: char);

begin

   if si > max(s) then throw(FormatTooLarge);
   s[si] := c; { place character }
   si := si+1 { next }

end;

{ place string in output string }

procedure plcstr(view s: string);

var i: integer; { index for string }

begin

   for i := 1 to max(s) do plcchr(s[i]) { place all characters }

end;

begin

   clears(s); { clear result }
   if f < 1 then { bad field }
      throw(InvalidFieldSpecification);
   si := 1; { set 1st output character }
   f := f-8; { find field remaining after essential places }
   if r = 0.0 then begin

      plcstr(' 0.'); { it's zero, output that }
      { pad with trailing zeros as required }
      while f > 0 do begin plcchr('0'); f := f-1 end;
      plcstr('e+000') { output exponent }

   end else begin { nonzero }

      e := 0; { clear exponent }
      { place number so that it is just < 10 }
      while (abs(r) < 10.0) and (e >= -308) do begin r := r*10.0; e := e-1 end;
      while (abs(r) >= 10.0) and (e <= 308) do begin r := r/10.0; e := e+1 end;
      { check invalid exponent }
      if abs(e) > 308 then { invalid }
         throw(InvalidRealNumber);
      if r < 0 then plcchr('-') else plcchr(' '); { output sign }
      { move to maximum representable digit in integer }
      p := maxpwr div 10; { set maximum power (that fits in integer) }
      i := round(abs(r)*p); { find number as scaled integer }
      { the first digit, followed by the decimal point, is allways output }
      plcchr(chr(i div p+ord('0'))); { output digit }
      i := i mod p; { remove from value }
      p := p div 10; { find next power }
      plcchr('.'); { output decimal point }
      while (p <> 0) and (f > 0) do begin { fit powers }

         plcchr(chr(i div p+ord('0'))); { output digit }
         i := i mod p; { remove from value }
         p := p div 10; { find next power }
         f := f-1 { count spaces }

      end;
      { pad with trailing zeros as required }
      while f > 0 do begin plcchr('0'); f := f-1 end;
      plcchr('e'); { output exponent mark }
      if e < 0 then plcchr('-') else plcchr('+'); { output sign }
      e := abs(e); { remove exponent sign }
      p := 100; { set maximum exponent power }
      while p <> 0 do begin { fit powers }

         plcchr(chr(e div p+ord('0'))); { output digit }
         e := e mod p; { remove from value }
         p := p div 10 { find next power }

      end
     
   end

end;

overload function reals(r: real;    { real to write }
                        f: integer) { field }
                        : pstring;  { result }

var buff: packed array [1..maxlin] of char; { string buffer }
    s:    pstring; { result holder }

begin

   reals(buff, r, f); { place in buffer }
   copy(s, buff); { create result }

   reals := s { return result }
   
end;

overload procedure reals(var s: string;  { result string }
                             r: real);   { real to write }

begin

   reals(s, r, rlfld) { place real with default field }

end;

overload function reals(r: real)   { real to write }
                        : pstring; { result }

var buff: packed array [1..maxlin] of char; { string buffer }
    s:    pstring; { result holder }

begin

   reals(buff, r); { place in buffer }
   copy(s, buff); { create result }

   reals := s { return result }
   
end;

overload procedure reals(var s: string;    { destination string }
                             r:  real;     { real to write }
                             fl: integer;  { field }
                             fr: integer); { fraction }

var i:      integer; { integer portion of number }
    p:      integer; { power holder }
    ps:     integer; { save for that }
    digits: integer; { count of representable digits }
    sp:     integer; { space counter }
    fc:     integer; { fraction counter }
    si:     integer; { index for string }
    ib:     packed array [1..maxdig] of char; { buffer for integer section }

{ place character in output string }

procedure plcchr(c: char);

begin

   if si > max(s) then throw(FormatTooLarge);
   s[si] := c; { place character }
   si := si+1 { next }

end;

{ place string in output string }

procedure plcstr(view s: string);

var i: integer; { index for string }

begin

   for i := 1 to len(s) do plcchr(s[i]) { place all characters }

end;

begin

   clears(s); { clear result }
   if fl < 1 then { bad field }
      throw(InvalidFieldSpecification);
   if fr < 1 then { bad field }
      throw(InvalidFractionSpecification);
   si := 1; { set 1st output character }
   i := trunc(r); { find "whole" part of real }
   { find number of digits in whole part }
   p := maxpwr; { set maximium power in integer }
   while p <> 0 do { find the top digit }
      if i div p <> 0 then begin ps := p; p := 0 end { found digit, terminate }
      else p := p div 10; { set next digit }
   digits := 0; { clear digit count }
   { count significant digits }
   while ps <> 0 do begin ps := ps div 10; digits := digits+1 end;
   if digits = 0 then 
      digits := 1; { must be at least one digit (zero case) }
   if r < 0 then digits := digits+1; { count sign place }
   { now we know the minimum length of the number, 
     digits+decimal+fraction. output leading spaces based on that }
   for sp := 1 to fl-(digits+1+fr) do plcchr(' '); { output padding } 
   { and output that, followed by decimal point }
   ints(ib, i); { place integer in buffer }
   plcstr(ib); { place in final }
   plcchr('.'); { place decimal point }
   r := abs(r-i); { find fraction without sign }
   i := round(r*maxpwr); { move to maximum representable digit in integer }
   p := maxpwr div 10; { set maximum power (that fits in integer) }
   fc := fr; { set fraction counter }
   while (p <> 0) and (fc > 0) do begin { fit powers }

      plcchr(chr(i div p+ord('0'))); { output digit }
      i := i mod p; { remove from value }
      p := p div 10; { find next power }
      fc := fc-1 { count fractional digits }

   end;
   { now pad out the rest of the specified fraction with zeros }
   while fc <> 0 do begin plcchr('0'); fc := fc-1 end

end;

overload function reals(r:  real;    { real to write }
                        fl: integer; { field }
                        fr: integer) { fraction }
                        : pstring;   { result }

var buff: packed array [1..maxlin] of char; { string buffer }
    s:    pstring; { result holder }

begin

   if fl > 0 then begin { right justify }

      reals(buff, r, fl, fr); { place in buffer }
      copy(s, buff) { place to result string }

   end else begin { left justified }

      reals(buff, r, 1, fr); { place in buffer }
      fl := abs(fl); { find net field }
      if len(buff) > fl then fl := len(buff); { set minimum format }
      newstr(s, fl); { create result }
      copy(s^, buff) { copy to result }

   end;

   reals := s { return result }
   
end;

overload procedure reals(var s: string; r: real; view fmt: string);

{ valid digits in output }

const digit = ['0'..'9'];

var int, frc:   packed array [1..maxlin] of char;
    intp, frcp: integer; { pointers for integer and fraction data }
    fmtp:       integer; { format string pointer }
    sign:       boolean; { sign of number }
    sgnout:     boolean; { sign was placed }
    dlrout:     boolean; { dollar sign already output }

{ unpack real into constituent fields }

procedure unpack(    r:        real;     { real to unpack }
                 var int, frc: string;   { integer and fraction sections }
                 var sign:     boolean); { sign }

var buff: packed array [1..maxlin] of char; { buffer for ASCII number }
    dp:   integer; { decimal point }
    e:    integer; { end of zero truncated fraction }

begin

   reals(buff, r, 1, rlfrc); { first, get number into standard form }
   sign := index(buff, '-') > 0; { find sign }
   dp := index(buff, '.'); { find decimal position }
   e := len(buff); { index end }
   while buff[e] = '0' do e := e-1; { back up to non-zero or decimal point }
   if buff[e] = '.' then e := e+1; { if all zeros, move back to .0 }
   extract(int, buff, 1+ord(sign), dp-1); { get integer section }
   extract(frc, buff, dp+1, e) { get fraction section }

end;

begin

   unpack(r, int, frc, sign); { unpack number }
   copy(s, fmt); { copy format string to result string }
   intp := len(int); { index last character of converted integer }
   fmtp := index(s, '.'); { index last character of format }
   if fmtp = 0 then fmtp := len(s) { no decimal, set last }
   else fmtp := fmtp-1; { set before decimal }
   dlrout := false; { set no dollar sign output }
   sgnout := false; { set no sign output }
   while fmtp > 0 do begin { interpret format }

      if s[fmtp] in ['9', '0', '-', '+', ',', '$'] then 
         case s[fmtp] of { control character }

         '9': if intp < 1 then s[fmtp] := ' '
                 else if not (int[intp] in digit) then s[fmtp] := ' '
                    else begin { valid digit exists }

            s[fmtp] := int[intp]; { transfer digit }
            intp := intp-1 { back up one in number }

         end;
         '0': if intp > 0 then
                 if int[intp] in digit then begin { valid digit exists }
             
            s[fmtp] := int[intp]; { transfer digit }
            intp := intp-1 { back up one in number }

         end;
         '-': begin

            if not sign then s[fmtp] := ' '; { knock out sign }
            sgnout := true { set sign output }

         end;
         '+': begin

            if sign then s[fmtp] := '-'; { place negative sign }
            sgnout := true { set sign output }

         end;
         ',': begin

            if intp < 1 then begin

               s[fmtp] := ' '; { out of digits, kill with space }
               if fmtp > 1 then { check left is '$' }
                  if s[fmtp-1] = '$' then { yes, treat as '$' }
                     if not dlrout then begin

                  s[fmtp] := '$'; { set '$' }
                  dlrout := true { set was output }

               end

            end else if not (int[intp] in digit) then begin

               s[fmtp] := ' '; { out of digits, kill with space }
               if fmtp > 1 then { check left is '$' }
                  if s[fmtp-1] = '$' then { yes, treat as '$' }
                     if not dlrout then begin

                  s[fmtp] := '$'; { set '$' }
                  dlrout := true { set was output }

               end

            end

         end;
         '$': begin

            if intp < 1 then begin { no data left in number }

               if not dlrout then dlrout := true { first occurance, output }
               else s[fmtp] := ' ' { else knock it out }

            end else if (int[intp] in digit) then begin

               s[fmtp] := int[intp]; { transfer digit }
               intp := intp-1 { back up one in number }

            end else begin { no digits left in number }

               if not dlrout then dlrout := true { first occurance, output }
               else s[fmtp] := ' ' { else knock it out }

            end

         end;

      end;
      fmtp := fmtp-1 { next format character }

   end;
   { now place optional fraction section }
   fmtp := index(s, '.'); { index decimal point }
   if fmtp <> 0 then begin { decimal point exists }

      fmtp := fmtp+1; { set after decimal }
      frcp := 1; { set 1st fraction digit }
      while fmtp <= max(s) do begin { interpret format }

         if s[fmtp] in ['9', '0'] then 
            case s[fmtp] of { control character }

            '9': if frcp > maxlin then s[fmtp] := ' '
                    else if not (frc[frcp] in digit) then s[fmtp] := ' '
                       else begin { valid digit exists }

               s[fmtp] := frc[frcp]; { transfer digit }
               frcp := frcp+1 { back up one in number }

            end;
            '0': if frcp < maxlin then
                    if frc[frcp] in digit then begin { valid digit exists }
                
               s[fmtp] := frc[frcp]; { transfer digit }
               frcp := frcp+1 { back up one in number }

            end;
            
         end;
         fmtp := fmtp+1 { next format character }

      end

   end;
   { check didn't use all digits of integer (fraction digits left out is ok) }
   if intp > 0 then throw(NumberOverflowsFormat);
   { check sign was negative, and not placed }
   if sign and not sgnout then throw(NegativeNotPlaced)

end;

overload function reals(r: real; view fmt: string): pstring;

var s: pstring;

begin

   newstr(s, len(fmt)); { get a result string }
   reals(s^, r, fmt); { place the number as formatted }

   reals := s { return result }

end;

{******************************************************************************

Output real "economy" format

Outputs a real in minium space, preferably without an exponent. The economy
format is designed both to take minimum space, but still give the maximum
number of significant digits, and to remove the exponent if possible, by
placing it within the number.

To do this, the number is converted, in ascii, to the floating point buffer
in a large buffer. Then, the exponent is read from the number, and blanked
out. The decial point is removed, and the number normalized to decimal point
left. If the decimal point can be placed within the number, it is. Also, we
allow a certain number of zeros before and after the number, as long as they
don't exceed the size of an exponent in ASCII. If the decimal still cannot be
placed, then the number reverts to floating point format, abet with the
trailing zeros removed.

******************************************************************************}

procedure reales(var s: string;   { string to place result in }
                     r: real;     { real to convert }
                     f: integer); { field }

var p1, p2: pstring; { string buffer }
    ex:     integer; { exponent }
    ld:     integer; { last digit }
    ep:     integer; { 'e' point in string }
    sign:   boolean; { sign }
    si:     integer; { output index }
    i:      integer; { string index }
    el:     integer; { exponent length }

{ place character in output string }

procedure plcchr(c: char);

begin

   if si > max(s) then throw(FormatTooLarge);
   s[si] := c; { place character }
   si := si+1 { next }

end;

{ find number of decimals in integer }

function numlen(i: integer): integer;

var dc: integer;

begin

   dc := 1; { set default length }
   if i < 0 then begin { signed }
  
      dc := dc+1; { count sign }
      i := abs(i) { remove sign }

   end;
   i := i div 10; { reduce default digit }
   while i > 0 do begin { count off digits }

      i := i div 10; { reduce digit }
      dc := dc+1 { count }

   end;

   numlen := dc { return result }

end;

begin

   if f < 1 then { bad field }
      throw(InvalidFieldSpecification);
   openstring; { start string temp level }
   p1 := reals(r); { place real in buffer }
   { find sign }
   sign := p1^[1] = '-';
   { find exponent ('e') position }
   ep := index(p1, 'e');
   { find last non-zero digit }
   ld := ep-1;
   while p1^[ld] = '0' do ld := ld-1;
   { get exponent, adjusted for decimal point far left }
   ex := intv(extract(p1, ep+1, len(p1)))+1; { get exponent }
   { get mantissa, without decimal point }
   p1 := cat(extract(p1, 2, 2), extract(p1, 4, ld)); { add the rest }
   { find exponent length }
   el := numlen(ex-1)+1;
   { check exponent is within number, or at ends, or within the exponent length
     that is required }
   if (ex >= 0-el) and (ex <= len(p1)+el) then begin

      if ex < 0 then begin

         { decimal position is left of digits, make up leading zeros }
         p1 := cat(rep('0', abs(ex)), p1);
         ex := 0 { we zeroed out the exponent }

      end else if ex > len(p1) then
         { decimal position is right of digits, make up trailing zeros }
         p1 := cat(p1, rep('0', ex-len(p1)));
      { Check decimal at extreme right. If not we can place it. Otherwise, the
        number can be left without a decimal point. }
      if ex < len(p1) then begin 

         { place decimal point within number }
         p2 := cat(extract(p1, 1, ex), '.');
         p1 := cat(p2, extract(p1, ex+1, len(p1)))

      end

   end else begin

      { put decimal point back, exponent back, and place number without
        trailing zeros }
      p2 := cat(extract(p1, 1, 1), '.');
      p1 := cat(p2, extract(p1, 2, len(p1)));
      p1 := cat(p1, 'e'); { add exponent mark }
      p1 := cat(p1, ints(ex-1)) { replace exponent, adjusted after first digit }

   end;
   if sign then p1 := cat('-', p1); { replace sign if exists }
   { output with padding }
   clears(s); { clear result }
   if max(p1^) > f then f := len(p1); { set minimum length }
   si := 1; { clear output index }
   for i := 1 to f-max(p1^) do plcchr(' '); { process left justify }
   for i := 1 to max(p1^) do plcchr(p1^[i]); { place digits }   
   closestring { close out string level }

end;

overload function reales(r: real;    { real to write }
                         f: integer) { field }
                         : pstring;  { result }

var buff: packed array [1..maxlin] of char; { string buffer }
    s:    pstring; { result holder }

begin

   if f > 0 then begin { right justify }

      reales(buff, r, f); { place in buffer }
      copy(s, buff) { create result }

   end else begin { left justified }

      reales(buff, r, 1); { place in buffer }
      f := abs(f); { find net field }
      if len(buff) > f then f := len(buff); { set minimum format }
      newstr(s, f); { create result }
      copy(s^, buff) { copy to result }

   end;

   reales := s { return result }
   
end;

overload procedure reales(var s: string;  { result string }
                              r: real);   { real to write }

begin

   reales(s, r, 1) { place real with default field }

end;

overload function reales(r: real)   { real to write }
                         : pstring; { result }

var buff: packed array [1..maxlin] of char; { string buffer }
    s:    pstring; { result holder }

begin

   reales(buff, r); { place in buffer }
   copy(s, buff); { create result }

   reales := s { return result }
   
end;

{******************************************************************************

Place hex value in string

Places the given integer in hex into the destination string. Has the following
forms:

procedure hexs(string, integer)

   Places the value into the padded string in hexadecimal format, with field of
   1.

procedure hexs(pstring, integer)

   Places the value into the dynamic string in hexadecimal format, with field
   of 1.

procedure hexs(string, integer, integer)

   Places the value into the padded string in hexadecimal format, with field.

procedure hexs(pstring, integer, integer)

   Places the value into the dynamic string in hexadecimal format, with field.

Note that sign is not processed in this routine.

******************************************************************************}

procedure hexs(var s: string;   { result string }
                   w: integer;  { integer to print }
                   f: integer); { field }
 
begin

   nums(s, w, f, 16) { place result }

end;

overload function hexs(w: integer; { integer to print }
                       f: integer) { field }
                       : pstring;  { result }

begin

   hexs := nums(w, f, 16) { return result }
   
end;

overload procedure hexs(var s: string;   { result string }
                            w: integer); { integer to print }

begin

   hexs(s, w, 1) { place hex value }

end;

overload function hexs(w: integer) { integer to print }
                        : pstring;     { result }

begin

   hexs := hexs(w, 1) { return result }
   
end;

overload procedure hexs(var s: string; i: integer; view fmt: string);

begin

   nums(s, i, fmt, 16) { place result }

end;

overload function hexs(i: integer; view fmt: string): pstring;

var s: pstring;

begin

   newstr(s, len(fmt)); { get a result string }
   hexs(s^, i, fmt); { place the number as formatted }

   hexs := s { return result }

end;

{******************************************************************************

Place octal value in string

Places the given integer in octal into the destination string. Has the following
forms:

procedure octs(string, integer)

   Places the value into the padded string in hexadecimal format, with field of
   1.

procedure octs(pstring, integer)

   Places the value into the dynamic string in hexadecimal format, with field
   of 1.

procedure octs(string, integer, integer)

   Places the value into the padded string in hexadecimal format, with field.

procedure octs(pstring, integer, integer)

   Places the value into the dynamic string in hexadecimal format, with field.

Note that sign is not processed in this routine.

******************************************************************************}

procedure octs(var s: string;   { result string }
                   w: integer;  { integer to print }
                   f: integer); { field }
 
begin

   nums(s, w, f, 8) { place result }

end;

overload function octs(w: integer; { integer to print }
                       f: integer) { field }
                       : pstring;  { result }

begin

   octs := nums(w, f, 8) { return result }
   
end;

overload procedure octs(var s: string;   { result string }
                            w: integer); { integer to print }

begin

   octs(s, w, 1) { place hex value }

end;

overload function octs(w: integer) { integer to print }
                        : pstring;     { result }

begin

   octs := octs(w, 1) { return result }
   
end;

overload procedure octs(var s: string; i: integer; view fmt: string);

begin

   nums(s, i, fmt, 8) { place result }

end;

overload function octs(i: integer; view fmt: string): pstring;

var s: pstring;

begin

   newstr(s, len(fmt)); { get a result string }
   octs(s^, i, fmt); { place the number as formatted }

   octs := s { return result }

end;

{******************************************************************************

Place binary value in string

Places the given integer in binary into the destination string. Has the following
forms:

procedure bins(string, integer)

   Places the value into the padded string in hexadecimal format, with field of
   1.

procedure bins(pstring, integer)

   Places the value into the dynamic string in hexadecimal format, with field
   of 1.

procedure bins(string, integer, integer)

   Places the value into the padded string in hexadecimal format, with field.

procedure bins(pstring, integer, integer)

   Places the value into the dynamic string in hexadecimal format, with field.

Note that sign is not processed in this routine.

******************************************************************************}

procedure bins(var s: string;   { result string }
                   w: integer;  { integer to print }
                   f: integer); { field }
 
begin

   nums(s, w, f, 2) { place result }

end;

overload function bins(w: integer; { integer to print }
                       f: integer) { field }
                       : pstring;  { result }

begin

   bins := nums(w, f, 2) { return result }
   
end;

overload procedure bins(var s: string;   { result string }
                            w: integer); { integer to print }

begin

   bins(s, w, 1) { place hex value }

end;

overload function bins(w: integer) { integer to print }
                        : pstring;     { result }

begin

   bins := bins(w, 1) { return result }
   
end;

overload procedure bins(var s: string; i: integer; view fmt: string);

begin

   nums(s, i, fmt, 2) { place result }

end;

overload function bins(i: integer; view fmt: string): pstring;

var s: pstring;

begin

   newstr(s, len(fmt)); { get a result string }
   bins(s^, i, fmt); { place the number as formatted }

   bins := s { return result }

end;

{******************************************************************************

Write decimal value

Decimal writes are covered well by the standard write system. However, there
are a few forms that are not, namely the economy format for reals, and image
formatted versions of real and integer.

******************************************************************************}

procedure writed(var  f:   text;    { file to print }
                      i:   integer; { integer to print }
                 view fmt: string); { format string }

var buff: packed array [1..maxlin] of char; { output buffer }

begin

   ints(buff, i, fmt); { convert to buffer }
   write(f, buff: len(fmt)) { output with same length as format }

end;

overload procedure writed(     i:   integer; { integer to print }
                          view fmt: string); { format string }

begin

   writed(output, i, fmt) { output }

end;

procedure writer(var  f:   text;    { file to print }
                      r:   real;    { real to print }
                 view fmt: string); { format string }

var buff: packed array [1..maxlin] of char; { output buffer }

begin

   reals(buff, r, fmt); { convert to buffer }
   write(f, buff: len(fmt)) { output with same length as format }

end;

overload procedure writer(     r:   real;    { real to print }
                          view fmt: string); { format string }

begin

   writer(output, r, fmt) { output }
   
end;

procedure writere(var  f:   text;     { file to print }
                       r:   real;     { real to print }
                       fl:  integer); { field }

var buff: packed array [1..maxlin] of char; { output buffer }
    i:    integer; { index for string }

begin

   if fl > 0 then begin { right justify }
   
      reales(buff, r, fl); { convert to buffer }
      write(f, buff:0) { output }

   end else begin { left justified }

      reales(buff, r, 1); { place in buffer }
      fl := abs(fl); { find net field }
      if len(buff) > fl then fl := len(buff); { set minimum format }
      write(f, buff:0); { output }
      for i := 1 to fl-len(buff) do write(' ') { space right }

   end

end;

overload procedure writere(r:  real;     { real to print }
                           fl: integer); { field }

begin

   writere(output, r, fl) { output }

end;

overload procedure writere(var f: text;  { file to print }
                               r: real); { real to print }

begin

   writere(f, r, 1) { output }

end;

overload procedure writere(r: real); { real to print }

begin

   writere(output, r, 1)

end;

{******************************************************************************

Write hex value

Writes the hex value to the given text file. Has the following forms:

procedure writeh(text, integer)

   Writes the value in hex, with field of 1.

procedure writeh(text, integer, integer)

   Writes the value in hex, with the given field.

******************************************************************************}

procedure writeh(var f:    text;     { file to print }
                     i:    integer;  { integer to print }
                     fl:   integer); { field }

var os: packed array [1..maxhdg] of char; { output save }
    l:  integer; { length of output buffer }
    fi: integer; { fill index }

begin

   hexs(os, i); { place output in buffer }
   l := len(os); { find length }
   if fl > 0 then for fi := 1 to fl-l do write(f, ' '); { fill left }
   write(f, os:0); { output number }
   if fl < 0 then for fi := 1 to abs(fl)-l do write(f, ' ') { fill right }

end;

overload procedure writeh(i:    integer;  { integer to print }
                          fl:   integer); { field }

begin

   writeh(output, i, fl)

end;

overload procedure writeh(var f: text;     { file to print }
                              i: integer); { integer to print }

var os: packed array [1..maxhdg] of char; { output save }

begin

   hexs(os, i); { place output in buffer }
   write(f, os:0) { output }

end;

overload procedure writeh(i: integer); { integer to print }

begin

   writeh(i, 1)

end;

overload procedure writeh(var  f:   text;    { file to print }
                               i:   integer; { integer to print }
                          view fmt: string); { format string }

var buff: packed array [1..maxlin] of char; { output buffer }

begin

   hexs(buff, i, fmt); { convert to buffer }
   write(f, buff: len(fmt)) { output with same length as format }

end;

overload procedure writeh(     i:   integer; { integer to print }
                          view fmt: string); { format string }

begin

   writeh(output, i, fmt) { output }

end;

{******************************************************************************

Write octal value

Writes the octal value to the given text file. Has the following forms:

procedure writeo(text, integer)

   Writes the value in octal, with field of 1.

procedure writeo(text, integer, integer)

   Writes the value in octal, with the given field.

******************************************************************************}

procedure writeo(var f:    text;     { result string }
                     i:    integer;  { integer to print }
                     fl:   integer); { field }

var os: packed array [1..maxlin] of char; { output save }
    l:  integer; { length of output buffer }
    fi: integer; { fill index }

begin

   octs(os, i); { place output in buffer }
   l := len(os); { find length }
   if fl > 0 then for fi := 1 to fl-l do write(f, ' '); { fill left }
   write(f, os:0); { output number }
   if fl < 0 then for fi := 1 to abs(fl)-l do write(f, ' ') { fill right }

end;

overload procedure writeo(i:    integer;  { integer to print }
                          fl:   integer); { field }

begin

   writeo(output, i, fl)

end;

overload procedure writeo(var f: text;     { result string }
                              i: integer); { integer to print }

var os: packed array [1..maxlin] of char; { output save }

begin

   bins(os, i); { place output in buffer }
   write(f, os:0) { output }

end;

overload procedure writeo(i: integer); { integer to print }

begin

   writeo(i, 1)

end;

overload procedure writeo(var  f:   text;    { file to print }
                               i:   integer; { integer to print }
                          view fmt: string); { format string }

var buff: packed array [1..maxlin] of char; { output buffer }

begin

   octs(buff, i, fmt); { convert to buffer }
   write(f, buff: len(fmt)) { output with same length as format }

end;

overload procedure writeo(     i:   integer; { integer to print }
                          view fmt: string); { format string }

begin

   writeo(output, i, fmt) { output }

end;

{******************************************************************************

Write binary value

Writes the binary value to the given text file. Has the following forms:

procedure writeb(text, integer)

   Writes the value in binary, with field of 1.

procedure writeb(text, integer, integer)

   Writes the value in binary, with the given field.

******************************************************************************}

procedure writeb(var f:    text;     { result string }
                     i:    integer;  { integer to print }
                     fl:   integer); { field }

var os: packed array [1..maxlin] of char; { output save }
    l:  integer; { length of output buffer }
    fi: integer; { fill index }

begin

   bins(os, i); { place output in buffer }
   l := len(os); { find length }
   if fl > 0 then for fi := 1 to fl-l do write(f, ' '); { fill left }
   write(f, os:0); { output number }
   if fl < 0 then for fi := 1 to abs(fl)-l do write(f, ' ') { fill right }

end;

overload procedure writeb(i:    integer;  { integer to print }
                          fl:   integer); { field }

begin

   writeb(output, i, fl)

end;

overload procedure writeb(var f: text;     { result string }
                              i: integer); { integer to print }

var os: packed array [1..maxlin] of char; { output save }

begin

   bins(os, i); { place output in buffer }
   write(f, os:0) { output }

end;

overload procedure writeb(i: integer); { integer to print }

begin

   writeb(i, 1)

end;

overload procedure writeb(var  f:   text;    { file to print }
                               i:   integer; { integer to print }
                          view fmt: string); { format string }

var buff: packed array [1..maxlin] of char; { output buffer }

begin

   bins(buff, i, fmt); { convert to buffer }
   write(f, buff: len(fmt)) { output with same length as format }

end;

overload procedure writeb(     i:   integer; { integer to print }
                          view fmt: string); { format string }

begin

   writeb(output, i, fmt) { output }

end;

{******************************************************************************

Get number from string

Parses a number from the given string. Takes a radix for the number, but also
allows the number to override this with a radix marker.

******************************************************************************}

function numv(view s: string;   { string containing integer }
                   r: integer;  { radix }
              var ovf: boolean) { overflow occurred }
              : integer;        { result }

var sgn:   integer; { sign of result }
    v:     integer; { incoming digit value }
    si:    integer; { index for string }
    i:     integer; { integer holder }
    digit: set of char; { digit set }

{ check end of string }

function endstr: boolean; begin endstr := si > max(s) end;

{ check next character in string }

function chkchr: char; 

begin if endstr then chkchr := ' ' else chkchr := s[si] end;

{ get next character in string }

procedure getchr; begin if not endstr then si := si+1 end;

{ skip spaces }

procedure skpspc; begin while (chkchr = ' ') and not endstr do getchr end;

begin

   si := 1; { set 1st string character }
   skpspc; { skip spaces in string }
   sgn := 1; { set sign is positive }
   { check if radix mark exists }
   if chkchr = '$' then begin { hexadecimal }

      getchr; { skip '$' }
      r := 16 { set radix }

   end else if chkchr = '&' then begin { octal }

      getchr; { skip '&' }
      r := 8 { set radix }

   end else if chkchr = '%' then begin { binary }

      getchr; { skip '%' }
      r := 2 { set radix }

   end;
   if r = 10 then begin { decimal, can contain sign }

      { check sign appears }
      if chkchr = '+' then getchr { skip '+' }
      else if chkchr = '-' then begin { negative }
   
         getchr; { skip '-' }
         sgn := -1 { set sign negative }
   
      end

   end;
   { set valid characters according to radix, and check radix }
   if r = 2 then digit := ['0'..'1'] { binary }
   else if r = 8 then digit := ['0'..'7'] { octal }
   else if r = 10 then digit := ['0'..'9'] { hexadecimal }
   else if r = 16 then digit := ['0'..'9', 'a'..'f', 'A'..'F'] { hexadecimal }
   else throw(InvalidRadix); { bad radix }
   if not (chkchr in digit) then { invalid integer }
      throw(InvalidIntegerFormat);
   i := 0; { clear result }
   ovf := false; { set no overflow }
   while chkchr in digit do begin { read digits }

      v := ord(chkchr)-ord('0'); { find the value of the new digit }
      { check for overflow }
      if (i > maxint div r) or 
         ((i = maxint div r) and (v > maxint mod r)) then ovf := true
      else i := i*r+v; { add in next value }
      getchr { next digit }

   end;
   i := i*sgn; { set sign of result }
   skpspc; { skip any trailing spaces }
   if not endstr then { trailing garbage }
      throw(InvalidIntegerFormat);

   numv := i { return result }

end;

{******************************************************************************

Get decimal integer from string

Parses an integer from the given string.

******************************************************************************}

function intv(view s: string) { string containing integer }
              : integer;      { result }

var ovf: boolean; { overflow flag }

begin

   intv := numv(s, 10, ovf); { return result }
   if ovf then throw(NumberTooLarge)

end;

overload function intv(view s: string;    { string containing integer }
                       var  ovf: boolean) { overflow flag }
                       : integer;         { result }

begin

   intv := numv(s, 10, ovf) { return result }

end;

overload function intv(s: pstring) { string containing integer }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   intv := intv(s^) { return result }

end;

overload function intv(    s:   pstring; { string containing integer }
                       var ovf: boolean) { overflow flag }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   intv := intv(s^, ovf) { return result }

end;

{******************************************************************************

Get hexadecimal from string

Parses an hexadecimal from the given string.

******************************************************************************}

function hexv(view s: string) { string containing integer }
              : integer;      { result }

var ovf: boolean; { overflow flag }

begin

   hexv := numv(s, 16, ovf); { return result }
   if ovf then throw(NumberTooLarge)

end;

overload function hexv(view s: string;    { string containing integer }
                       var  ovf: boolean) { overflow flag }
                       : integer;         { result }

begin

   hexv := numv(s, 16, ovf) { return result }

end;

overload function hexv(s: pstring) { string containing integer }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   hexv := hexv(s^) { return result }

end;

overload function hexv(    s:   pstring; { string containing integer }
                       var ovf: boolean) { overflow flag }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   hexv := hexv(s^, ovf) { return result }

end;

{******************************************************************************

Get octal from string

Parses an octal from the given string.

******************************************************************************}

function octv(view s: string) { string containing integer }
              : integer;      { result }

var ovf: boolean; { overflow flag }

begin

   octv := numv(s, 8, ovf); { return result }
   if ovf then throw(NumberTooLarge)

end;

overload function octv(view s: string;    { string containing integer }
                       var  ovf: boolean) { overflow flag }
                       : integer;         { result }

begin

   octv := numv(s, 8, ovf) { return result }

end;

overload function octv(s: pstring) { string containing integer }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   octv := octv(s^) { return result }

end;

overload function octv(    s:   pstring; { string containing integer }
                       var ovf: boolean) { overflow flag }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   octv := octv(s^, ovf) { return result }

end;

{******************************************************************************

Get binary from string

Parses a binary number from the given string.

******************************************************************************}

function binv(view s: string) { string containing integer }
              : integer;      { result }

var ovf: boolean; { overflow flag }

begin

   binv := numv(s, 2, ovf); { return result }
   if ovf then throw(NumberTooLarge)

end;

overload function binv(view s: string;    { string containing integer }
                       var  ovf: boolean) { overflow flag }
                       : integer;         { result }

begin

   binv := numv(s, 2, ovf) { return result }

end;

overload function binv(s: pstring) { string containing integer }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   binv := binv(s^) { return result }

end;

overload function binv(    s:   pstring; { string containing integer }
                       var ovf: boolean) { overflow flag }
                       : integer;  { result }

begin

   if s = nil then throw(StringNil);

   binv := binv(s^, ovf) { return result }

end;

{******************************************************************************

Get real from string

Parses a real from the given string.

******************************************************************************}

function realv(view s: string) { string to parse from }
               : real;         { return real }

var i:   integer; { integer portion }
    p:   real;    { power }
    si:  integer; { index for string }
    r:   real;    { real to read }

{ check end of string }

function endstr: boolean; begin endstr := si > max(s) end;

{ check next character in string }

function chkchr: char; 

begin if endstr then chkchr := ' ' else chkchr := s[si] end;

{ get next character in string }

procedure getchr; begin if not endstr then si := si+1 end;

{ skip spaces }

procedure skpspc; begin while (chkchr = ' ') and not endstr do getchr end;


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

{ parse integer }

procedure getint(var i: integer);

var sgn: integer; { sign of result }
    v:   integer; { incoming digit value }

begin

   sgn := 1; { set sign is positive }
   { check sign appears }
   if chkchr = '+' then getchr { skip '+' }
   else if chkchr = '-' then begin { negative }

      getchr; { skip '-' }
      sgn := -1 { set sign negative }

   end;
   if not (chkchr in ['0'..'9']) then { invalid integer }
      throw(InvalidIntegerFormat);
   i := 0; { clear result }
   while chkchr in ['0'..'9'] do begin { read digits }

      v := ord(chkchr)-ord('0'); { find the value of the new digit }
      { check for overflow }
      if (i > maxint div 10) or 
         ((i = maxint div 10) and (v > maxint mod 10)) then
         throw(IntegerTooLarge);
      i := i*10+v; { add in next value }
      getchr { next digit }

   end;
   i := i*sgn { set sign of result }

end;

begin

   si := 1; { set 1st string character }
   skpspc; { skip spaces in string }
   getint(i); { read integer section }
   r := i; { convert integer to real }
   if chkchr in ['.', 'e', 'E'] then begin { it's a real }

      if chkchr = '.' then begin { decimal point }

         getchr; { skip '.' }
         if not (chkchr in ['0'..'9']) then
            throw(InvalidRealFormat);
         p := 1.0; { initalize power }
         while chkchr in ['0'..'9'] do begin { parse digits }

            p := p/10.0; { find next scale }
            { add and scale new digit }
            r := r+(p * (ord(chkchr) - ord('0')));
            getchr { next }

         end

      end;
      if chkchr in ['e', 'E'] then begin { exponent }

         getchr; { skip 'e' }
         if not (chkchr in ['0'..'9', '+', '-']) then
            throw(InvalidRealFormat);
         getint(i); { get exponent }
         { find with exponent }
         if i < 0 then r := r/pwrten(i) else r := r*pwrten(i)

      end

   end;
   skpspc; { skip any trailing spaces }
   if not endstr then { trailing garbage }
      throw(InvalidRealFormat);

   realv := r { return result }

end;

overload function realv(s: pstring) { string containing integer }
                       : real;  { result }

begin

   if s = nil then throw(StringNil);

   realv := realv(s^) { return result }

end;

begin { init }

   strstk := nil; { clear string block tracking stack }
   strfre := nil { clear string block free stack }

end;

begin { deinit }

   { handle exceptions in modules }
{
   on NoStringBlock except                perror('No string block is active')          
   on OuterBlockFull except               perror('No room in outermost block')         
   on CurrentBlockFull except             perror('Current string block is full')         
   on StringNil except                    perror('String passed is nil')                 
   on StringDestinationOverflow except    
      perror('String was too large for destination') 
   on IndexOutOfRange except              perror('String index out of range')             
   on NegativeRepeatCount except          perror('Repeat cont was negative')             
   on WordIndexOutOfRange except          
      perror('Word array index was out of range')    
   on StringReadOverflow except           
      perror('String was too large for destination') 
   on FormatTooLarge except               
      perror('Format too large for destination')     
   on InvalidFieldSpecification except    perror('field specified is invalid')           
   on NegativeValueNondecimal except      perror('Radix was negative')                   
   on NumberOverflowsFormat except        
      perror('Number overflows space provided in format string');                        
   on NegativeNotPlaced except            
      perror('Negative sign not placed in format')   
   on InvalidRealNumber except            perror('Invalid real number')                  
   on InvalidFractionSpecification except 
      perror('Invalid fraction specification')       
   on InvalidRadix except                 perror('Invalid radix')                        
   on InvalidIntegerFormat except         perror('Invalid integer format')               
   on NumberTooLarge except               perror('Number too large')                     
   on IntegerTooLarge except              perror('Integer too large')                    
   on InvalidRealFormat except            perror('Invalid real format')                  
}

end.
