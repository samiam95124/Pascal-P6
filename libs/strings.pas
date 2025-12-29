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
{ Radix was negative }                   NegativeValueNondecimal,
{ Number overflows space provided in 
  format string }                        NumberOverflowsFormat,
{ Negative sign not placed in format }   NegativeNotPlaced,
{ Invalid radix }                        InvalidRadix,
{ Number too large }                     NumberTooLarge,
{ Integer too large }                    IntegerTooLarge: exception;

function lcase(c: char): char; external;
overload function lcase(view s: pstring): pstring; external;
function lcases(view s: string): pstring; external;
overload procedure lcases(var s: string); external;
function ucase(c: char): char; external;
overload function ucase(view s: pstring): pstring; external;
function ucases(view s: string): pstring; external;
overload procedure ucases(var s: string); external;
procedure clears(var s: string); external;
function len(view s: string): integer; external;
overload function len(view s: pstring): integer; external;
procedure copy(var d: string; view s: string); external;
overload procedure copy(var  d: string; s: pstring); external;
overload function copy(view s: string): pstring; external;
overload function copy(view s: pstring): pstring; external;
overload procedure copy(var  d: pstring; view s: string); external;
procedure cat(var d: string; view s: string); external;
overload function cat(view sa, sb: string): pstring; external;
overload function cat(s1, s2: pstring): pstring; external;
overload function cat(view s1: string; s2: pstring): pstring; external;
overload function cat(s1: pstring; view s2: string): pstring; external;
function compc(view d, s: string): boolean; external;
overload function compc(d, s: pstring): boolean; external;
overload function compc(view d: string; s: pstring): boolean; external;
overload function compc(d: pstring; view s: string): boolean; external;
function compcp(view d, s: string): boolean; external;
function comp(view d, s: string): boolean; external;
overload function comp(d, s: pstring): boolean; external;
overload function comp(view d: string; s: pstring): boolean; external;
overload function comp(d: pstring; view s: string): boolean; external;
function compp(view d, s: string): boolean; external;
function gtrc(view d, s: string): boolean; external;
overload function gtrc(d, s: pstring): boolean; external;
overload function gtrc(view d: string; s: pstring): boolean; external;
overload function gtrc(d: pstring; view s: string): boolean; external;
function gtrcp(view d, s: string): boolean; external;
function gtr(view d, s: string): boolean; external;
overload function gtr(d, s: pstring): boolean; external;
overload function gtr(view d: string; s: pstring): boolean; external;
overload function gtr(d: pstring; view s: string): boolean; external;
function gtrp(view d, s: string): boolean; external;
function indexc(view d, s: string): integer; external;
overload function indexc(d, s: pstring): integer; external;
overload function indexc(view d: string; s: pstring): integer; external;
overload function indexc(d: pstring; view s: string): integer; external;
function indexcp(view d, s: string): integer; external;
function index(view d, s: string): integer; external;
overload function index(d, s: pstring): integer; external;
overload function index(view d: string; s: pstring): integer; external;
overload function index(d: pstring; view s: string): integer; external;
function indexp(view d, s: string): integer; external;
procedure extract(var d: string; view s: string; l, r: integer);
   external;
overload function extract(view s: pstring; l, r: integer): pstring; external;
overload function extract(view s: string; l, r: integer): pstring; external;

procedure insert(var d: string; view s: string; p: integer); external;
overload function insert(view sa, sb: string; p: integer): pstring; external;
overload function insert(sa, sb: pstring; p: integer): pstring; external;
overload function insert(view sa: string; sb: pstring; p: integer): pstring; 
   external;
overload function insert(sa: pstring; view sb: string; p: integer): pstring; 
   external;
function rep(view s: string; r: integer): pstring; external;
overload function rep(s: pstring; r: integer): pstring; external;
overload procedure rep(var d: string; view s: string; r: integer); external;
function trim(view s: string): pstring; external;
overload function trim(s: pstring): pstring; external;
overload procedure trim(var d: string; view s: string); external;
function words(view s: string): integer; external;
overload function words(s: pstring): integer; external;
function extwords(view s: string; l, r: integer): pstring; external;
overload function extwords(s: pstring; l, r: integer): pstring; external;
overload procedure extwords(var d: string; view s: string; l, r: integer); 
   external;
procedure reads(var f: text; var s: pstring; var ovf: boolean); external;
overload procedure reads(var f: text; var s: string; var ovf: boolean); external;
overload procedure reads(var f: text; var s: pstring); external;
overload procedure reads(var f: text; var s: string); external;
overload procedure reads(var s: pstring; var ovf: boolean); external;
overload procedure reads(var s: string; var ovf: boolean); external;
overload procedure reads(var s: pstring); external;
overload procedure reads(var s: string); external;
procedure ints(var s: string; i: integer; f: integer); external;
overload function ints(i: integer; f: integer): pstring; external;
overload procedure ints(var s: string; i: integer); external;
overload function ints(i: integer): pstring; external;
overload procedure ints(var s: string; i: integer; view fmt: string); external;
overload function ints(i: integer; view fmt: string): pstring; external;
procedure reals(var s: string; r: real; f: integer); external;
overload function reals(r: real; f: integer): pstring; external;
overload procedure reals(var s: string; r: real); external;
overload function reals(r: real): pstring; external;
overload procedure reals(var s: string; r: real; fl: integer; fr: integer);
   external;
overload function reals(r: real; fl: integer; fr: integer): pstring; external;
overload procedure reals(var s: string; r: real; view fmt: string); external;
overload function reals(r: real; view fmt: string): pstring; external;
procedure reales(var s: string; r: real; f: integer); external;
overload function reales(r: real; f: integer): pstring; external;
overload procedure reales(var s: string; r: real); external;
overload function reales(r: real): pstring; external;
procedure hexs(var s: string; w: integer; f: integer); external;
overload function hexs(w: integer; f: integer): pstring; external;
overload procedure hexs(var s: string; w: integer); external;
overload function hexs(w: integer): pstring; external;
overload procedure hexs(var s: string; i: integer; view fmt: string); external;
overload function hexs(i: integer; view fmt: string): pstring; external;
procedure octs(var s: string; w: integer; f: integer); external;
overload function octs(w: integer; f: integer): pstring; external;
overload procedure octs(var s: string; w: integer); external;
overload function octs(w: integer): pstring; external;
overload procedure octs(var s: string; i: integer; view fmt: string); external;
overload function octs(i: integer; view fmt: string): pstring; external;
procedure bins(var s: string; w: integer; f: integer); external;
overload function bins(w: integer; f: integer): pstring; external;
overload procedure bins(var s: string; w: integer); external;
overload function bins(w: integer): pstring; external;
overload procedure bins(var s: string; i: integer; view fmt: string); external;
overload function bins(i: integer; view fmt: string): pstring; external;
procedure writed(var f: text; i: integer; view fmt: string); external;
overload procedure writed(i: integer; view fmt: string); external;
procedure writer(var f: text; r: real; view fmt: string); external;
overload procedure writer(r: real; view fmt: string); external;
procedure writere(var f: text; r: real; fl: integer); external;
overload procedure writere(r: real; fl: integer); external;
overload procedure writere(var f: text; r: real); external;
overload procedure writere(r: real); external;
procedure writeh(var f: text; i: integer; fl: integer); external;
overload procedure writeh(i: integer; fl: integer); external;
overload procedure writeh(var f: text; i: integer); external;
overload procedure writeh(i: integer); external;
overload procedure writeh(var f: text; i: integer; view fmt: string); external;
overload procedure writeh(i: integer; view fmt: string); external;
procedure writeo(var f: text; i: integer; fl: integer); external;
overload procedure writeo(i: integer; fl: integer); external;
overload procedure writeo(var f: text; i: integer); external;
overload procedure writeo(i: integer); external;
overload procedure writeo(var f: text; i: integer; view fmt: string); external;
overload procedure writeo(i: integer; view fmt: string); external;
procedure writeb(var f: text; i: integer; fl: integer); external;
overload procedure writeb(i: integer; fl: integer); external;
overload procedure writeb(var f: text; i: integer); external;
overload procedure writeb(i: integer); external;
overload procedure writeb(var f: text; i: integer; view fmt: string); external;
overload procedure writeb(i: integer; view fmt: string); external;
function intv(view s: string): integer; external;
overload function intv(s: pstring): integer; external;
overload function intv(view s: string; var ovf: boolean): integer; external;
overload function intv(s: pstring; var ovf: boolean): integer; external;
function hexv(view s: string): integer; external;
overload function hexv(s: pstring): integer; external;
overload function hexv(view s: string; var ovf: boolean): integer; external;
overload function hexv(s: pstring; var ovf: boolean): integer; external;
function octv(view s: string): integer; external;
overload function octv(s: pstring): integer; external;
overload function octv(view s: string; var ovf: boolean): integer; external;
overload function octv(s: pstring; var ovf: boolean): integer; external;
function binv(view s: string): integer; external;
overload function binv(s: pstring): integer; external;
overload function binv(view s: string; var ovf: boolean): integer; external;
overload function binv(s: pstring; var ovf: boolean): integer; external;
function realv(view s: string): real; external;
overload function realv(s: pstring): real; external;
procedure openstring; external;
procedure closestring; external;
procedure exportstring(p: pstring); external;
procedure upstring(p: pstring); external;
procedure subst(var  s: string; view m: string; view r: string); external;
overload function subst(view s: string; view m: string; view r: string)
                        : pstring; external;
overload function subst(view s: string; view m: string; r: pstring): pstring;
   external;
overload function subst(view s: string; m: pstring; view r: string): pstring;
   external;
overload function subst(view s: string; m: pstring; r: pstring): pstring;
   external;
overload function subst(s: pstring; view m: string; view r: string): pstring;
   external;
overload function subst(s: pstring; view m: string; r: pstring): pstring;
   external;
overload function subst(s: pstring; m: pstring; view r: string): pstring;
   external;
overload function subst(s: pstring; m: pstring; r: pstring): pstring; external;
procedure substc(var  s: string; view m: string; view r: string); external;
overload function substc(view s: string; view m: string; view r: string)
                        : pstring; external;
overload function substc(view s: string; view m: string; r: pstring): pstring;
   external;
overload function substc(view s: string; m: pstring; view r: string): pstring;
   external;
overload function substc(view s: string; m: pstring; r: pstring): pstring;
   external;
overload function substc(s: pstring; view m: string; view r: string): pstring;
   external;
overload function substc(s: pstring; view m: string; r: pstring): pstring;
   external;
overload function substc(s: pstring; m: pstring; view r: string): pstring;
   external;
overload function substc(s: pstring; m: pstring; r: pstring): pstring; external;
procedure substall(var s: string; view m: string; view r: string); external;
overload function substall(view s: string; view m: string; view r: string)
                           : pstring; external;
overload function substall(view  s: string; view m: string; r: pstring)
                           : pstring; external;
overload function substall(view  s: string; m: pstring; view r: string)
                           : pstring; external;
overload function substall(view  s: string; m: pstring; r: pstring): pstring; 
   external;
overload function substall(s: pstring; view m: string; view r: string): pstring;
   external;
overload function substall(s: pstring; view m: string; r: pstring): pstring;
   external;
overload function substall(s: pstring; m: pstring; view r: string): pstring;
   external;
overload function substall(s: pstring; m: pstring; r: pstring): pstring;
   external;
procedure substcall(var s: string; view m: string; view r: string); external;
overload function substcall(view s: string; view m: string; view r: string)
                           : pstring; external;
overload function substcall(view  s: string; view m: string; r: pstring)
                           : pstring; external;
overload function substcall(view  s: string; m: pstring; view r: string)
                           : pstring; external;
overload function substcall(view  s: string; m: pstring; r: pstring): pstring; 
   external;
overload function substcall(s: pstring; view m: string; view r: string): pstring;
   external;
overload function substcall(s: pstring; view m: string; r: pstring): pstring;
   external;
overload function substcall(s: pstring; m: pstring; view r: string): pstring;
   external;
overload function substcall(s: pstring; m: pstring; r: pstring): pstring;
   external;


begin
end.
