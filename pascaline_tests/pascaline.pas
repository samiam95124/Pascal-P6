{$list+}
{*******************************************************************************
*                                                                              *
*                         TEST SUITE FOR PASCALINE                             *
*                                                                              *
* This program tests the extentions to ISO 7185 standard pascal that make up   *
* Pascaline. This program attempts to use and display the results of each      *
* feature of Pascaline. It is a "positive" test in that it should compile and  *
* run error free, and thus does not check error conditions/detection.          *
*                                                                              *
* Each test is labeled and numbered, and the expected result also output, so   *
* that the output can be self evidently hand checked.                          *
*                                                                              *
* The test cases are roughly arranged in the same order as the section         *
* headings of "The language Pascaline".                                        *
*                                                                              *
* The output can be redirected to a printer or a file to facillitate such      *
* checking.                                                                    *
*                                                                              *
* The output can also be automatically checked by comparing a known good file  *
* to the generated file. To this end, we have regularized the output,          *
* specifying all output field widths that are normally compiler dependent.     *
*                                                                              *
*                                                                              *
* LICENSING:                                                                   *
*                                                                              *
* Copyright (c) 2022, Scott A. Franco                                          *
* All rights reserved.                                                         *
*                                                                              *
* Redistribution and use in source and binary forms, with or without           *
* modification, are permitted provided that the following conditions are met:  *
*                                                                              *
* 1. Redistributions of source code must retain the above copyright notice,    *
*    this list of conditions and the following disclaimer.                     *
* 2. Redistributions in binary form must reproduce the above copyright         *
*    notice, this list of conditions and the following disclaimer in the       *
*    documentation and/or other materials provided with the distribution.      *
*                                                                              *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE     *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   *
* POSSIBILITY OF SUCH DAMAGE.                                                  *
*                                                                              *
* The views and conclusions contained in the software and documentation are    *
* those of the authors and should not be interpreted as representing official  *
* policies, either expressed or implied, of the Pascal-P6 project.             *
*                                                                              *
*******************************************************************************}

program pascaline(output, error, list, file1, file2, number1, number2, command);

joins pascaline2; { test joined module }
uses pascaline1; { test used module }

{ this will test for extended labeling }

var a_1: integer;
    c:   char;
    i, x, y:   integer;
    lx, ly, lz: linteger;
    cx, cy, cz: cardinal;
    lcx, lcy, lcz: lcardinal;
    file1: text;
    file2: file of integer;
    number1: integer;
    number2: real;

{ this tests relaxed declarations }

const c_one = 1;

{ this tests alphabetical goto labels }

label skip_over;

{ test constant expressions }

const not_exp = not 42;
      mlt_exp = 13*10;
      div_exp = 124 div 5;
      rdiv_exp = 1.234/543.22;
      mod_exp = 54 mod 13;
      and_exp = 76 and 12;
      neg_exp = -76;
      pos_exp = +54;
      add_exp = 74+23;
      sub_exp = 87-34;
      or_exp = 34 or 72;
      xor_exp = 31 xor 53;
      set_exp = ['a'..'d', 'z'];
      set_exp1 = [1..5, 10+2];
      mmaxlint = -maxlint;

type iarr   = array of integer;
     miarr  = array of array of integer;

fixed f_i: integer = 432;
      f_c: char = 'Q';
      f_r: real = 1.23456;
      f_s: packed array 8 of char = 'hi there';
      f_ai: packed array 5 of integer = array 1, 5, 3, 10, 92 end;
      f_ac: packed array 5 of char = array 'a', 'h', 'u', 'o', 'z' end;
      f_ar: packed array 5 of real = array 1.1, 1.2, 1.3, 1.4, 1.5 end;
      f_ma: packed array 3, 4 of integer = array

         array 1, 3, 64, 2 end,
         array 12, 31, 647, 21 end,
         array 190, 32, 641, 243 end

      end;
      f_rc: record i: integer; c: char; r: real end = record 42, 'a', 1.234 end;

type enum_a   = (one, two, three);
     enum_b   = (red, green, blue, cyan, purple, black, white);
     string10 = packed array 10 of char;
     pinteger = ^integer;
     rec1     = record i: integer; c: char end;
     

var ps:         pstring;
    s10:        packed array 10 of char;
    ia:        ^iarr;
    miap:      ^miarr;
    a, b:      integer;
    ft:        text;
    fb:        file of byte;
    ba, bb:    byte;
    mia:       array 10, 10 of integer;
    cd:        cardinal;
    li:        linteger;
    lc:        lcardinal;
    excpt:     exception;
    ps10(10):    string;
    v(10):     vector;
    m(10, 10): matrix;
    vr:        record case e: enum_a of
                 one..two: (i: integer);
                 three:    (c: char)
               end;
    pi1, pi2, pi3: pinteger;
    r1:        rec1;
    f1:        lreal;
    f2:        sreal;
    fl1:       text;
    f3, f4:    real;
    eb:        enum_b;
    las, lbs, lcs, lds, les, lgs, lhs: linteger;
    lvnum: -maxint..maxint;
    sra, srb, src, srd, sre: real;
    lra, lrb, lrc, lrd, lre: real;

{ allocate variable length string }
function str(s: string): pstring;

var sp: pstring;

begin

   new(sp, max(s));
   sp^ := s;
   
   result sp

end;
   
{ this tests duplication of parameter lists }

procedure prtstr(view s: string); forward;

procedure prtstr(view s: string);

begin

   write(s)

end;

{ test pointer result }

function getstr: pstring;

var p: pstring;

begin

   new(p, 8);
   p^ := 'hi there';
   getstr := p

end;

{ test var parameter container array }

static procedure prtstr1(var s: string);

begin

   write(s)

end;

{ test value parameter container array }

procedure prtstr2(s: string);

begin

   write(s)

end;

{ test value parameter 2d container array }

procedure mconarr1(ca: miarr);

var x, y: integer;

begin

   ca[5, 5] := 123;
   for x := max(ca, 2) downto 1 do
      begin for y := max(ca, 1) downto 1 do write(ca[x, y]:4); writeln end

end;

{ test var parameter 2d container array }

procedure mconarr2(var ca: miarr);

var x, y: integer;

begin

   ca[5, 5] := 123;
   for x := max(ca, 2) downto 1 do
      begin for y := max(ca, 1) downto 1 do write(ca[x, y]:4); writeln end

end;

{ test view parameter 2d container array }

procedure mconarr3(view ca: miarr);

var x, y: integer;

begin

   for x := max(ca, 2) downto 1 do
      begin for y := max(ca, 1) downto 1 do write(ca[x, y]:4); writeln end

end;

{ overloads }

procedure bark;

begin

   writeln('This is bark')

end;

overload procedure bark(i: integer);

begin

   writeln('bark: the integer is: ', i:1)

end;

overload procedure bark(s: string);

begin

   writeln('bark: the string is: ', s)

end;

overload function bark: integer;

begin

   bark := 42

end;

{ overrides }

override procedure abstract;

begin

   writeln('This is the overriding procedure');

   inherited abstract

end;

{
override procedure pascaline2.abstract;

begin

   writeln('This is the overriding procedure');

   inherited abstract

end;
}

{ parameterized variables }

procedure parvar(l: integer);

var v(l): vector;
    i:    integer;

begin

   for i := 1 to l do v[i] := i+20;
   for i := l downto 1 do write(v[i]:3)

end;

procedure parvar2(x, y: integer);

var m(x, y): matrix;
    i:       integer;

begin

   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin m[x, y] := i; i := i+1 end;
   for x := 10 downto 1 do
      begin for y := 10 downto 1 do write(m[x, y]:4); writeln end

end;

{ can create two parvars, can copy one to the other }

procedure parvar3(l: integer);

var v1(l), v2(l): vector;
    i:    integer;

begin

   for i := 1 to l do v1[i] := i+20;
   v2 := v1;
   for i := l downto 1 do write(v2[i]:3)

end;

procedure parvar4(x, y: integer);

var m1(x, y), m2(x, y): matrix;
    i:       integer;

begin

   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin m1[x, y] := i; i := i+1 end;
   m2 := m1;
   for x := 10 downto 1 do
      begin for y := 10 downto 1 do write(m2[x, y]:4); writeln end

end;

procedure outpar(out i: integer);

begin

   i := 42

end;

function anonyret: integer;

begin

   result 42

end;

function strret: string10;

begin

    result 'hi there ?'

end;

function strret2: string10;

begin

    result 'abcd      '

end;

procedure prtstr10(s: string10);

begin

   write(s)

end;

function recret: rec1;

var r: rec1;

begin

    r.i := 42;
    r.c := 'a';

    result r

end;

operator +(a: enum_b): enum_b;

begin

    result succ(a)

end;

operator -(a: enum_b): enum_b;

begin

   result pred(a)

end;

operator not(a: enum_b): enum_b;

begin

   result succ(succ(a))

end;

operator +(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator -(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)-ord(b))

end;

operator *(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)*ord(b))

end;

operator /(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a) div ord(b))

end;

operator div(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a) div ord(b)+1)

end;

operator mod(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a) mod ord(b))

end;

operator and(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator or(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator xor(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator <(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator >(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator =(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator <=(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator >=(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator in(a, b: enum_b): enum_b;

begin

   result enum_b(ord(a)+ord(b))

end;

operator :=(out a: enum_b; b: enum_b);

begin

   a := succ(b)

end;

operator +(a: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := +a^;   
   result p

end;

operator -(a: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := -a^;   
   result p

end;

operator +(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^+b^;   
   result p

end;

operator -(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^-b^;   
   result p

end;

operator *(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^*b^;   
   result p

end;

operator /(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^ div b^;   
   result p

end;

operator div(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^ div b^;   
   result p

end;

operator mod(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^ mod b^;   
   result p

end;

operator and(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^ and b^;   
   result p

end;

operator or(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^ or b^;   
   result p

end;

operator xor(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^ xor b^;   
   result p

end;

operator <(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^+b^;   
   result p

end;

operator >(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^+b^;   
   result p

end;

operator =(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^+b^;   
   result p

end;

operator <=(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^+b^;   
   result p

end;

operator >=(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^+b^;   
   result p

end;

operator in(a, b: pinteger): pinteger;

var p: pinteger;

begin

   new(p);
   p^ := a^+b^;   
   result p

end;

begin

   write('****************************************************************');
   writeln('***************');
   writeln;
   writeln('                       TEST SUITE FOR PASCALINE');
   writeln;
   write('                 Copyright (C) 2022 S. A. Franco - All rights ');
   writeln('reserved');
   writeln;
   write('****************************************************************');
   writeln('***************');
   writeln;

{*******************************************************************************

                                 Metering

*******************************************************************************}

   writeln('The following are implementation defined characteristics');
   writeln;
   writeln('Maxcrd: ',  maxcrd:1);
   writeln('Maxlint: ', maxlint:1);
   writeln('Maxlcrd: ', maxlcrd:1);
   writeln('Maxchr: ',  ord(maxchr):1);
   writeln('Maxreal: ', maxreal);
   writeln('Maxsreal: ', maxsreal);
   writeln('Maxlreal: ', maxlreal);

   cd := maxcrd;
   x := 0;
   while cd > 0 do begin cd := cd div 2;  x := x+1 end;
   writeln('Bit length of cardinal appears to be: ', x:1);

   li := maxlint;
   x := 0;
   while li > 0 do begin li := li div 2;  x := x+1 end;
   writeln('Bit length of long integer without sign appears to be: ', x:1);

   lc := maxlcrd;
   x := 0;
   while lc > 0 do begin lc := lc div 2;  x := x+1 end;
   writeln('Bit length of long cardinal appears to be: ', x:1);

   writeln('Cardinal default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   cd := 1;
   writeln(cd);

   writeln('Long integer default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   li := 1;
   writeln(li);

   writeln('Long cardinal default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   lc := 1;
   writeln(lc);

{*******************************************************************************

                                   6.3 Comments

*******************************************************************************}

   { standard comment }

   { multiline
     comment }

   ! Pascaline comment

   writeln('cmt1: <nothing>'); ! in line Pascaline comment

{*******************************************************************************

                               6.4 Identifiers

*******************************************************************************}

   a_1 := 42;
   writeln('id1: ', a_1:1, ' s/b 42');

{*******************************************************************************

                                  6.5 labels

*******************************************************************************}

   write('glb1: start ');
   goto skip_over;
   write('!!! BAD !!!');
   ! Note this also contains an extended Pascaline symbol character, '_'
   skip_over: writeln(' stop s/b start stop');

{*******************************************************************************

                             6.6 Numeric constants

*******************************************************************************}

! Pascaline style comment
   writeln('nc1: ', $a5:1, ' s/b 165'); ! pascaline trailing comment
   writeln('nc2: ', &72:1, ' s/b 58');
   writeln('nc3: ', %011001:1, ' s/b ', 25:1);
   writeln('nc3: ', 123_456_:1, ' s/b ', 123456);
   writeln('nc4: ', $342_834_:1, ' s/b ', 3418164);
   writeln('nc5: ', &321_732_:1, ' s/b ', 107482);
   writeln('nc6: ', %0101_0111_:1, ' s/b ', 87);

{*******************************************************************************

                               6.7 Constant expressions

*******************************************************************************}

   writeln('ext15: ', not_exp:1,  ' s/b 9223372036854775765');
   writeln('ext16: ', mlt_exp:1,  ' s/b 130');
   writeln('ext17: ', div_exp:1,  ' s/b 24');
   writeln('ext18: ', rdiv_exp:1:4, ' s/b 0.0022');
   writeln('ext19: ', mod_exp:1,  ' s/b 2');
   writeln('ext20: ', and_exp:1,  ' s/b 12');
   writeln('ext21: ', neg_exp:1,  ' s/b -76');
   writeln('ext22: ', pos_exp:1,  ' s/b 54');
   writeln('ext23: ', add_exp:1,  ' s/b 97');
   writeln('ext24: ', sub_exp:1,  ' s/b 53');
   writeln('ext25: ', or_exp:1,   ' s/b 106');
   writeln('ext26: ', xor_exp:1,  ' s/b 42');
   write('ext27: ');
   for c := 'a' to 'z' do if c in set_exp then write(c);
   writeln(' s/b abcdz');
   write('ext28: ');
   for i := 1 to 20 do if i in set_exp1 then write(i:1, ' ');
   writeln(' s/b 1 2 3 4 5 12');
   writeln('ext29: ', f_i:1, ' s/b 432');
   writeln('ext30: ', f_c, ' s/b Q');
   writeln('ext31: ', f_r, ' s/b 1.23456000e+000');
   writeln('ext32: ', f_s, ' s/b hi there');

{*******************************************************************************

                        6.8 Boolean integer operations

*******************************************************************************}

   a := 56;
   b := 13;
   writeln('vb1: ', not a:1, ' s/b 9223372036854775751');
   writeln('vb2: ', a and b:1, ' s/b 8');
   writeln('vb3: ', a or b:1, ' s/b 61');
   writeln('vb4: ', a xor b:1, ' s/b 53');

{*******************************************************************************

                             6.9 View and out parameters

*******************************************************************************}

   write('vop1: ');
   prtstr('this is a test');
   writeln(' s/b this is a test');
   write('vop2: ');
   outpar(i);
   writeln(i:1, ' s/b 42');

{*******************************************************************************

                            6.10 Extended case statements

*******************************************************************************}

   writeln('ecs1:');
   for i := 1 to 10 do
      case i of

      1: writeln(i:1, ': one');
      2..4: writeln(i:1, ': from 2 to 4');
      else writeln(i:1, ': from 5 to 10')

   end;
   writeln('s/b');
   writeln('1: one');
   writeln('2: from 2 to 4');
   writeln('3: from 2 to 4');
   writeln('4: from 2 to 4');
   writeln('5: from 5 to 10');
   writeln('6: from 5 to 10');
   writeln('7: from 5 to 10');
   writeln('8: from 5 to 10');
   writeln('9: from 5 to 10');
   writeln('10: from 5 to 10');

{*******************************************************************************

                          6.11 variant record case ranges

*******************************************************************************}

   write('vcr1: ');
   vr.e := one;
   vr.i := 42;
   write(vr.i:1, ' ');
   vr.e := two;
   vr.i := 43;
   write(vr.i:1, ' ');
   vr.e := three;
   vr.c := 'a';
   write(vr.c, ' ');
   writeln(' s/b 42 43 a');

{*******************************************************************************

                          6.12 Array type shorthand

*******************************************************************************}

   s10 := 'hi there ?';
   writeln('ats1: ', s10, ' s/b hi there ?');
   writeln('ats2: ');
   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin mia[x, y] := i; i := i+1 end;
   for x := 10 downto 1 do
      begin for y := 10 downto 1 do write(mia[x, y]:4); writeln end;
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46  45  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');

{*******************************************************************************

                        6.13 Single demension container arrays

*******************************************************************************}

   write('sdc1: ');
   new(ps, 10);
   ps^ := 'hi there ?';
   write(ps^);
   writeln(' s/b hi there ?');
   write('sdc2: ');
   new(ps, 10);
   ps^ := 'hi there ?';
   s10 := ps^;
   write(s10);
   writeln(' s/b hi there ?');
   write('sdc3: ');
   new(ps, 10);
   ps^ := 'hi there ?';
   for i := 1 to 10 do write(ps^[i]);
   writeln(' s/b hi there ?');
   write('sdc4: ');
   new(ia, 10);
   ia^[1] := 143;
   ia^[2] := 276;
   ia^[3] := 388;
   ia^[4] := 412;
   ia^[5] := 574;
   ia^[6] := 622;
   ia^[7] := 74;
   ia^[8] := 83;
   ia^[9] := 99;
   ia^[10] := 1;
   for i := 1 to 10 do write(ia^[i]:1, ' ');
   writeln(' s/b 143 276 388 412 574 622 74 83 99 1');
   write('sdc5: ');
   ps10 := 'hi there ?';
   write(ps10);
   writeln(' s/b hi there ?');
   write('sdc6: ');
   for i := 1 to 10 do v[i] := i+10;
   for i := 10 downto 1 do write(v[i]:3);
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('sdc7: ');
   parvar(10);
   writeln(' s/b 30 29 28 27 26 25 24 23 22 21');
   write('sdc8: ');
   parvar3(10);
   writeln(' s/b 30 29 28 27 26 25 24 23 22 21');

   { compares using pstring }
   write('sdc9: ');
   ps := str('hi there bob');
   writeln(ps^ = 'hi there bob', ' s/b true');
   write('sdc10: ');
   ps := str('lo there bob');
   writeln(ps^ = 'hi there bob', ' s/b false');
   write('sdc11: ');
   ps := str('hi there bob');
   writeln(ps^ <> 'hi there bob', ' s/b false');
   write('sdc12: ');
   ps := str('lo there bob');
   writeln(ps^ <> 'hi there bob', ' s/b true');
   write('sdc13: ');
   ps := str('abcd');
   writeln(ps^ < 'abce', ' s/b true');
   write('sdc14: ');
   writeln('abce' < ps^, ' s/b false');
   write('sdc15: ');
   ps := str('abcd');
   writeln(ps^ > 'abce', ' s/b false');
   write('sdc16: ');
   writeln('abce' > ps^, ' s/b true');
   write('sdc17: ');
   ps := str('abcd');
   writeln(ps^ <= 'abce', ' s/b true');
   write('sdc18: ');
   writeln('abce' <= ps^, ' s/b false');
   write('sdc19: ');
   ps := str('abcd');
   writeln(ps^ <= 'abcd', ' s/b true');
   write('sdc20: ');
   writeln('abcd' <= ps^, ' s/b true');
   write('sdc21: ');
   ps := str('abcd');
   writeln(ps^ >= 'abce', ' s/b false');
   write('sdc22: ');
   writeln('abce' >= ps^, ' s/b true');
   write('sdc23: ');
   ps := str('abcd');
   writeln(ps^ >= 'abcd', ' s/b true');
   write('sdc24: ');
   writeln('abcd' >= ps^, ' s/b true');

   { compares using parameterized string }
   write('sdc25: ');
   ps10 := 'hi there ?';
   writeln(ps10 = 'hi there ?', ' s/b true');
   write('sdc26: ');
   ps10 := 'lo there ?';
   writeln(ps10 = 'hi there ?', ' s/b false');
   write('sdc27: ');
   ps10 := 'hi there ?';
   writeln(ps10 <> 'hi there ?', ' s/b false');
   write('sdc28: ');
   ps10 := 'lo there ?';
   writeln(ps10 <> 'hi there ?', ' s/b true');
   write('sdc29: ');
   ps10 := 'abcd      ';
   writeln(ps10 < 'abce      ', ' s/b true');
   write('sdc30: ');
   writeln('abce      ' < ps10, ' s/b false');
   write('sdc31: ');
   ps10 := 'abcd      ';
   writeln(ps10 > 'abce      ', ' s/b false');
   write('sdc32: ');
   writeln('abce      ' > ps10, ' s/b true');
   write('sdc33: ');
   ps10 := 'abcd      ';
   writeln(ps10 <= 'abce      ', ' s/b true');
   write('sdc34: ');
   writeln('abce      ' <= ps10, ' s/b false');
   write('sdc35: ');
   ps10 := 'abcd      ';
   writeln(ps10 <= 'abcd      ', ' s/b true');
   write('sdc36: ');
   writeln('abcd      ' <= ps10, ' s/b true');
   write('sdc37: ');
   ps10 := 'abcd      ';
   writeln(ps10 >= 'abce      ', ' s/b false');
   write('sdc38: ');
   writeln('abce      ' >= ps10, ' s/b true');
   write('sdc39: ');
   ps10 := 'abcd      ';
   writeln(ps10 >= 'abcd      ', ' s/b true');
   write('sdc40: ');
   writeln('abcd      ' >= ps10, ' s/b true');
   
{*******************************************************************************

                    6.13 Multiple dimension container arrays

*******************************************************************************}

   writeln('mdc1: ');
   new(miap, 10, 10);
   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin miap^[x, y] := i; i := i+1 end;
   for x := 10 downto 1 do
      begin for y := 10 downto 1 do write(miap^[x, y]:4); writeln end;
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46  45  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');
   dispose(miap);

   writeln('mdc2: ');
   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin m[x, y] := i; i := i+1 end;
   for x := 10 downto 1 do
      begin for y := 10 downto 1 do write(m[x, y]:4); writeln end;
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46  45  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');

   writeln('mdc3: ');
   parvar2(10, 10);
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46  45  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');

   writeln('mdc4: ');
   parvar4(10, 10);
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46  45  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');

{*******************************************************************************

                       6.13 Container arrays as parameters

*******************************************************************************}

   write('ext47: ');
   prtstr('hi george');
   writeln(' s/b hi george');
   write('ext48: ');
   s10 := 'hi george?';
   prtstr1(s10);
   writeln(' s/b hi george?');

   write('ext49: ');
   prtstr2('hi george');
   writeln(' s/b hi george');
   writeln('ext50:');
   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin mia[x, y] := i; i := i+1 end;
   mconarr1(mia);
   for x := 10 downto 1 do
      begin for y := 10 downto 1 do write(mia[x, y]:4); writeln end;
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46 123  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46  45  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');
   writeln('ext51:');
   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin mia[x, y] := i; i := i+1 end;
   mconarr2(mia);
   for x := 10 downto 1 do
      begin for y := 10 downto 1 do write(mia[x, y]:4); writeln end;
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46 123  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46 123  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');
   writeln('ext52:');
   i := 1;
   for x := 1 to 10 do
      for y := 1 to 10 do begin mia[x, y] := i; i := i+1 end;
   mconarr3(mia);
   writeln('s/b');
   writeln(' 100  99  98  97  96  95  94  93  92  91');
   writeln('  90  89  88  87  86  85  84  83  82  81');
   writeln('  80  79  78  77  76  75  74  73  72  71');
   writeln('  70  69  68  67  66  65  64  63  62  61');
   writeln('  60  59  58  57  56  55  54  53  52  51');
   writeln('  50  49  48  47  46  45  44  43  42  41');
   writeln('  40  39  38  37  36  35  34  33  32  31');
   writeln('  30  29  28  27  26  25  24  23  22  21');
   writeln('  20  19  18  17  16  15  14  13  12  11');
   writeln('  10   9   8   7   6   5   4   3   2   1');

{*******************************************************************************

                      6.13 Container pointer function results

*******************************************************************************}

   write('cp1: ');
   ps := str('hi there');
   writeln(ps^, ' s/b hi there');

{*******************************************************************************

                       6.15 Extended write/writeln statements

*******************************************************************************}

   writeln('ew1:');
   for i := 0 to 10 do writeln('*', 10:-i, '*');
   writeln('s/b');
   writeln('*10*');
   writeln('*10*');
   writeln('*10*');
   writeln('*10 *');
   writeln('*10  *');
   writeln('*10   *');
   writeln('*10    *');
   writeln('*10     *');
   writeln('*10      *');
   writeln('*10       *');
   writeln('*10        *');

   writeln('ew2:');
   for i := 0 to 10 do writeln('*', 'hi there':i, '*');
   writeln('s/b');
   writeln('**');
   writeln('*h*');
   writeln('*hi*');
   writeln('*hi *');
   writeln('*hi t*');
   writeln('*hi th*');
   writeln('*hi the*');
   writeln('*hi ther*');
   writeln('*hi there*');
   writeln('* hi there*');
   writeln('*  hi there*');

   writeln('ew3:');
   for i := 0 to 10 do writeln('*', 'hi there':-i, '*');
   writeln('s/b');
   writeln('**');
   writeln('*h*');
   writeln('*hi*');
   writeln('*hi *');
   writeln('*hi t*');
   writeln('*hi th*');
   writeln('*hi the*');
   writeln('*hi ther*');
   writeln('*hi there*');
   writeln('*hi there *');
   writeln('*hi there  *');

   writeln('ew4:');
   writeln('hi there         ':*, '<');
   writeln('s/b');
   writeln('hi there<');

   writeln('ew5:');
   for i := 0 to 10 do writeln('*', 10:#i, '*');
   writeln('s/b');
   writeln('*10*');
   writeln('*10*');
   writeln('*10*');
   writeln('*010*');
   writeln('*0010*');
   writeln('*00010*');
   writeln('*000010*');
   writeln('*0000010*');
   writeln('*00000010*');
   writeln('*000000010*');
   writeln('*0000000010*');

   writeln('ew6:');
   for i := 0 to 10 do writeln('*', 10:#-i, '*');
   writeln('s/b');
   writeln('*10*');
   writeln('*10*');
   writeln('*10*');
   writeln('*10 *');
   writeln('*10  *');
   writeln('*10   *');
   writeln('*10    *');
   writeln('*10     *');
   writeln('*10      *');
   writeln('*10       *');
   writeln('*10        *');

   writeln('ew7: ', $abcdef$:1, ' s/b abcdef');
   writeln('ew8: ', &76543&:1, ' s/b 76543');
   writeln('ew9: ', %1100101%:1, ' s/b 1100101');

   writeln('ew10:');
   for i := 0 to 10 do writeln('*', $123abc$:i, '*');
   writeln('s/b');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('* 123abc*');
   writeln('*  123abc*');
   writeln('*   123abc*');
   writeln('*    123abc*');

   writeln('ew11:');
   for i := 0 to 10 do writeln('*', $123abc$:-i, '*');
   writeln('s/b');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc *');
   writeln('*123abc  *');
   writeln('*123abc   *');
   writeln('*123abc    *');

   writeln('ew12:');
   for i := 0 to 10 do writeln('*', $123abc$:#i, '*');
   writeln('s/b');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*0123abc*');
   writeln('*00123abc*');
   writeln('*000123abc*');
   writeln('*0000123abc*');

   writeln('ew13:');
   for i := 0 to 10 do writeln('*', $123abc$:#-i, '*');
   writeln('s/b');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc*');
   writeln('*123abc *');
   writeln('*123abc  *');
   writeln('*123abc   *');
   writeln('*123abc    *');

   writeln('ew14:');
   for i := 0 to 10 do writeln('*', &123765&:i, '*');
   writeln('s/b');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('* 123765*');
   writeln('*  123765*');
   writeln('*   123765*');
   writeln('*    123765*');

   writeln('ew15:');
   for i := 0 to 10 do writeln('*', &123765&:-i, '*');
   writeln('s/b');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765 *');
   writeln('*123765  *');
   writeln('*123765   *');
   writeln('*123765    *');

   writeln('ew16:');
   for i := 0 to 10 do writeln('*', &123765&:#i, '*');
   writeln('s/b');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*0123765*');
   writeln('*00123765*');
   writeln('*000123765*');
   writeln('*0000123765*');

   writeln('ew17:');
   for i := 0 to 10 do writeln('*', &123765&:#-i, '*');
   writeln('s/b');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765*');
   writeln('*123765 *');
   writeln('*123765  *');
   writeln('*123765   *');
   writeln('*123765    *');

{*******************************************************************************

                       6.16 Extended read/readln statements

*******************************************************************************}

   assign(fl1, 'readtest.txt');
   reset(fl1);
   readln(fl1, s10);
   writeln('er1: ', s10, ' s/b hi there ?');
   readln(fl1, ps10);
   writeln('er2: ', ps10, ' s/b lo there ?');
   readln(fl1, i:10, x:10);
   writeln('er3: ', i:1, ' ', x:1, ' s/b 42 92');
   readln(fl1, i:2, x:3);
   writeln('er4: ', i:1, ' ', x:1, ' s/b 92 421');
   readln(fl1, i:3);
   writeln('er5: ', i:1, ' s/b 456');
   readln(fl1, c:3);
   writeln('er6: ', c, ' s/b a');
   readln(fl1, s10:13);
   writeln('er7: ', s10, ' s/b hi there ?');
   readln(fl1, s10:-13);
   writeln('er8: ', s10, ' s/b lo there ?');
   read(fl1, s10:13); read(fl1, i:2); readln(fl1);
   writeln('er9: ', s10, i:1, ' s/b bite me  ?42');
   s10 := '1234567890';
   read(fl1, s10:0);
   writeln('er10: ', s10, ' s/b 1234567890');
   readln(fl1, f3:9, f4:9);
   writeln('er11: ', f3:9:4, ' ', f4:9:4, ' s/b 1234.5678 9876.5432');

   {
   string constants to match input
   unimplemented
   read(fl1, 'The answer is: ', i);
   writeln('er12: ', i:1, ' s/b 42');
   }
   readln(fl1);

   {
   radix specifiers in the input file
   unimplemented
   read(fl1, i, x, y);
   writeln('er13: ', i:1, x:1, y:1);
   }
   readln(fl1);

   {
   radix specifiers on variables
   unimplemented
   read(fl1, i$, x&, y%);
   writeln('er13: ', i:1, x:1, y:1);
   }
   readln(fl1);

   s10 := '1234567890';
   readln(fl1, s10:*);
   writeln('er14: ', s10, '<', ' s/b hi!       <');

{*******************************************************************************

                           6.17 Type converters/restrictors

*******************************************************************************}

   write('tcr1: ');
   for i := 0 to 2 do case enum_a(i) of

      one: write('one ');
      two: write('two ');
      three: write('three ')

   end;
   writeln(' s/b one two three');

   ! type restrictor
   ba := 42;
   bb := 11;
   ba := byte(ba+bb);
   writeln('tcr2: ', ba:1, ' s/b 53 ');

{*******************************************************************************

                                    6.18 Fixed types

*******************************************************************************}

   write('f1: ');
   for i := 1 to 5 do write(f_ai[i]:1, ' ');
   writeln(' s/b 1 5 3 10 92');
   write('f2: ');
   for i := 1 to 5 do write(f_ac[i]);
   writeln(' s/b ahuoz');
   write('f3: ');
   for i := 1 to 5 do write(f_ar[i]:1:1, ' ');
   writeln(' s/b 1.1 1.2 1.3 1.4 1.5');
   write('f4: ');
   for x := 1 to 3 do
      for y := 1 to 4 do write(f_ma[x, y]:1, ' ');
   writeln(' s/b 1 3 64 2 12 31 647 21 190 32 641 243');
   writeln('ext37: ', f_rc.i:1, f_rc.c, f_rc.r:1:4, ' s/b 42a1.2340');

{*******************************************************************************

                  6.19 Extended file procedures and functions

*******************************************************************************}

   write('fh1: ');
   if exists('ext0001.txt') then delete('ext0001.txt');
   if exists('ext0002.txt') then delete('ext0002.txt');
   assign(ft, 'ext0001.txt');
   rewrite(ft);
   writeln(ft, 'hi there, bob');
   reset(ft);
   while not eoln(ft) do begin

      read(ft, c);
      write(c);

   end;
   close(ft);
   writeln(' s/b hi there, bob');

   write('fh2: ');
   change('ext0002.txt', 'ext0001.txt');
   assign(ft, 'ext0002.txt');
   reset(ft);
   while not eoln(ft) do begin

      read(ft, c);
      write(c);

   end;
   close(ft);
   writeln(' s/b hi there, bob');

   writeln('fh3: ', exists('ext0001.txt'), ' s/b false');

   writeln('fh4: ', exists('ext0002.txt'), ' s/b true');
   delete('ext0002.txt');

   writeln('fh5: ', exists('ext0002.txt'), ' s/b false');

   write('fh6: ');
   assign(fb, 'ext0001.txt');
   rewrite(fb);
   for i := 1 to 10 do write(fb, i);
   reset(fb);
   for i := 10 downto 1 do begin read(fb, ba); write(ba:1, ' ') end;
   writeln(' s/b 1 2 3 4 5 6 7 8 9 10');

   writeln('fh7: ', length(fb):1, ' s/b 10');

   writeln('fh8: ', location(fb):1, ' s/b 11');

   write('fh9: ');
   position(fb, 5);
   read(fb, ba);
   writeln(ba:1, ' s/b 5');

   writeln('fh10: ', location(fb):1, ' s/b 6');

   write('fh11: ');
   rewrite(fb);
   for i := 1 to 10 do write(fb, i);
   update(fb);
   for i := 1 to 5 do write(fb, 99);
   reset(fb);
   for i := 1 to 10 do begin read(fb, ba); write(ba:3) end;
   writeln(' s/b  99  99  99  99  99   6   7   8   9  10');

   writeln('fh12: ');
   rewrite(ft);
   writeln(ft, 'The rain in spain');
   writeln(ft, 'falls mainly on the plain');
   reset(ft);
   while not eof(ft) do begin

      while not eoln(ft) do begin

         read(ft, c);
         write(c)

      end;
      readln(ft);
      writeln

   end;
   append(ft);
   writeln(ft, 'But only on tuesdays and thursdays');
   reset(ft);
   while not eof(ft) do begin

      while not eoln(ft) do begin

         read(ft, c);
         write(c)

      end;
      readln(ft);
      writeln

   end;
   writeln('s/b');
   writeln('The rain in spain');
   writeln('falls mainly on the plain');
   writeln('The rain in spain');
   writeln('falls mainly on the plain');
   writeln('But only on tuesdays and thursdays');

   ! change procedure

{*******************************************************************************

                      6.20 Added program header standard bindings

*******************************************************************************}

   ! header binding is optional, and is covered in annex C. That is included
   ! here because it must be done in order.

   ! expects headertest1 headertest2 42 123.456 hi there as the command tail

   writeln('hbt1:');
   reset(file1);
   while not eof(file1) do 
      if not eoln(file1) then begin read(file1, c); write(c) end
      else begin get(file1); writeln end;
   writeln('s/b');
   writeln('The rain in spain falls mainly on the plain');

   writeln('hbt2:');
   rewrite(file2);
   for i := 1 to 10 do write(file2, i);
   reset(file2);
   for i := 1 to 10 do begin read(file2, x); write(x:1, ' ') end;
   writeln(' s/b 1 2 3 4 5 6 7 8 9 10');

   writeln('hbt3: ', number1:1, ' s/b 42');

   writeln('hbt4: ', number2:1:3, ' s/b 123.456');

   writeln('hbt5: ');
   while not eoln(command) do begin write(command^); get(command) end;
   writeln;
   writeln('s/b');
   writeln(' hi there');

   writeln('hbt6:');
   writeln(error, 'This is a test of the error file');
   writeln('s/b');
   writeln('This is a test of the error file');

   writeln('hbt7:');
   writeln(list, 'This is a test of the list file');
   writeln('s/b');
   writeln('This is a test of the list file');

{*******************************************************************************

             6.21 Redeclaration of forwarded procedures and functions

*******************************************************************************}

   ! This was tested in prtstr()

{*******************************************************************************

                           6.22 Anonymous function result

*******************************************************************************}

   writeln('afr1: ', anonyret:1, ' s/b 42');

{*******************************************************************************

                            6.23 Extended function results

*******************************************************************************}

   { assignments }
   write('sfr1: ');
   s10 := strret;
   writeln(s10, ' s/b hi there ?');
   write('sfr2: ');
   ps10 := strret;
   writeln(ps10, ' s/b hi there ?');

   { writes }
   writeln('sfr3: ', strret, ' s/b hi there ?');

   { compares }
   writeln('sfr4: ', strret = 'hi there ?', ' s/b true');
   writeln('sfr5: ', strret = 'lo there ?', ' s/b false');
   writeln('sfr6: ', strret <> 'hi there ?', ' s/b false');
   writeln('sfr7: ', strret <> 'lo there ?', ' s/b true');
   writeln('sfr8: ', strret2 < 'abce      ', ' s/b true');
   writeln('sfr9: ', 'abce      ' < strret2, ' s/b false');
   writeln('sfr10: ', strret2 > 'abce      ', ' s/b false');
   writeln('sfr11: ', 'abce      ' > strret2, ' s/b true');
   writeln('sfr12: ', strret2 <= 'abce      ', ' s/b true');
   writeln('sfr13: ', 'abce      ' <= strret2, ' s/b false');
   writeln('sfr14: ', strret2 <= 'abcd      ', ' s/b true');
   writeln('sfr15: ', 'abcd      ' <= strret2, ' s/b true');
   writeln('sfr16: ', strret2 >= 'abce      ', ' s/b false');
   writeln('sfr17: ', 'abce      ' >= strret2, ' s/b true');
   writeln('sfr18: ', strret2 >= 'abcd      ', ' s/b true');
   writeln('sfr19: ', 'abcd      ' >= strret2, ' s/b true');

   { as a parameter }
   write('sfr20: ');
   prtstr10(strret);
   writeln(' s/b hi there ?');
   write('sfr21: ');
   prtstr(strret);
   writeln(' s/b hi there ?');
   
   { records }
   r1 := recret;
   writeln('sfr22: ', r1.i:1, ' ', r1.c, ' s/b 42 a');
   
{*******************************************************************************

                    6.25 Overloading of procedures and functions

*******************************************************************************}

   writeln('ol1:');
   bark;
   bark(52);
   bark('hi there');
   writeln(bark:1);
   writeln('s/b');
   writeln('This is bark');
   writeln('bark: the integer is: 52');
   writeln('bark: the string is: hi there');
   writeln('42');

{*******************************************************************************

                            6.26 Operator overloads

*******************************************************************************}

   ! note there is no requirement for the result to be the same type as the
   ! system equivalent.

   writeln('opo1: ', ord(+red):1, ' s/b 1');
   writeln('opo2: ', ord(-cyan):1, ' s/b 2');
   writeln('opo3: ', ord(not purple):1, ' s/b 6');   
   writeln('opo4: ', ord(green+blue):1, ' s/b 3');
   writeln('opo5: ', ord(blue-green)$:1, ' s/b 1');
   writeln('opo6: ', ord(blue*cyan):1, ' s/b 6');
   writeln('opo7: ', ord(white/blue):1, ' s/b 3');
   writeln('opo8: ', ord(white div blue):1, ' s/b 4');
   writeln('opo9: ', ord(black mod cyan):1, ' s/b 2');
   writeln('opo10: ', ord(red and green):1, ' s/b 1');
   writeln('opo11: ', ord(green or green):1, ' s/b 2');
   writeln('opo12: ', ord(blue xor green):1, ' s/b 3');
   writeln('opo13: ', ord(cyan < green):1, ' s/b 4');
   writeln('opo14: ', ord(purple > green):1, ' s/b 5');
   writeln('opo15: ', ord(black = green):1, ' s/b 6');
   writeln('opo16: ', ord(red <= blue):1, ' s/b 2');
   writeln('opo17: ', ord(green >= cyan):1, ' s/b 4');
   writeln('opo18: ', ord(cyan in blue):1, ' s/b 5');

   {
   ! assigns not implemented correctly
   eb := blue;
   writeln('opo19: ', ord(eb):1, ' s/b 3');
   }

   new(pi1);
   new(pi2);
   pi1^ := 42;
   pi2^ := 12;
   write('opo19: ');
   pi3 := +pi1;
   writeln(pi3^:1, ' s/b 42');
   write('opo20: ');
   pi3 := -pi1;
   writeln(pi3^:1, ' s/b -42');
   write('opo21: ');
   pi3 := pi1+pi2;
   writeln(pi3^:1, ' s/b 54');
   write('opo22: ');
   pi3 := pi1-pi2;
   writeln(pi3^:1, ' s/b 30');
   write('opo23: ');
   pi3 := pi1*pi2;
   writeln(pi3^:1, ' s/b 504');
   write('opo24: ');
   pi3 := pi1/pi2;
   writeln(pi3^:1, ' s/b 3');
   write('opo25: ');
   pi3 := pi1 div pi2;
   writeln(pi3^:1, ' s/b 3');
   write('opo26: ');
   pi3 := pi1 mod pi2;
   writeln(pi3^:1, ' s/b 6');
   write('opo27: ');
   pi3 := pi1 and pi2;
   writeln(pi3^:1, ' s/b 8');
   write('opo28: ');
   pi3 := pi1 or pi2;
   writeln(pi3^:1, ' s/b 46');
   write('opo29: ');
   pi3 := pi1 xor pi2;
   writeln(pi3^:1, ' s/b 38');
   write('opo30: ');
   pi3 := pi1 < pi2;
   writeln(pi3^:1, ' s/b 54');
   write('opo31: ');
   pi3 := pi1 > pi2;
   writeln(pi3^:1, ' s/b 54');
   write('opo32: ');
   pi3 := pi1 = pi2;
   writeln(pi3^:1, ' s/b 54');
   write('opo33: ');
   pi3 := pi1 <= pi2;
   writeln(pi3^:1, ' s/b 54');
   write('opo34: ');
   pi3 := pi1 >= pi2;
   writeln(pi3^:1, ' s/b 54');
   write('opo35: ');
   pi3 := pi1 in pi2;
   writeln(pi3^:1, ' s/b 54');

   { overload from used module }

   { overload from joined module }

   { inherited operator overload }

{*******************************************************************************

                          6.27 Static procedures and functions

*******************************************************************************}

   ! Tested in prtstr1

{*******************************************************************************

                       6.28 Relaxation of declaration order

*******************************************************************************}

   ! Tested in header

{*******************************************************************************

                             6.29 Exception handling

*******************************************************************************}

   writeln('ex1:');
   try

      writeln('before exception');
      throw(excpt);
      writeln('after exception')

   except writeln('exception taken')
   else writeln('exception not taken');
   writeln('s/b');
   writeln('before exception');
   writeln('exception taken');

   writeln('ex2:');
   try

      writeln('In try block')

   except writeln('exception taken')
   else writeln('exception not taken');
   writeln('s/b');
   writeln('In try block');
   writeln('exception not taken');

   { nested in procedure }

   { in module destructor }

{*******************************************************************************

                   6.30 Assert procedure

*******************************************************************************}

   ! all we can do here is test false asserts.
   ! Asserts can also be a negative test.

   writeln('as1:');
   writeln('Testing negative assertion, this should not stop here');
   i := 4;
   assert(i = 4);

{*******************************************************************************

                            6.31 Extended range types

*******************************************************************************}

   { linteger variables }
   lx := 43; ly := 78; lz := ly;
   writeln('linteger1:   ', lx + ly:1, ' s/b 121');
   writeln('linteger2:   ', ly - lx:1, ' s/b 35');
   writeln('linteger3:   ', lx * ly:1, ' s/b 3354');
   writeln('linteger4:   ', ly div lx:1, ' s/b 1');
   writeln('linteger5:   ', ly mod lx:1, ' s/b 35');
   writeln('linteger6:   ', succ(lx):1, ' s/b 44');
   writeln('linteger7:   ', pred(lx):1, ' s/b 42');
   writeln('linteger8:   ', sqr(lx):1, ' s/b 1849');
   writeln('linteger9:   ', chr(ly), ' s/b N');
   writeln('linteger10:  ', ord(chr(lx)):1, ' s/b 43');
   writeln('linteger11:  ', odd(lx):5, ' s/b true');
   writeln('linteger12:  ', odd(ly):5, ' s/b false');
   writeln('linteger13:  ', lz = ly:5, ' s/b true');
   writeln('linteger14:  ', lx = ly:5, ' s/b false');
   writeln('linteger15:  ', lx < ly:5, ' s/b true');
   writeln('linteger16:  ', ly < lx:5, ' s/b false');
   writeln('linteger17:  ', ly > lx:5, ' s/b true');
   writeln('linteger18:  ', lx > ly:5, ' s/b false');
   writeln('linteger19:  ', lx <> ly:5, ' s/b true');
   writeln('linteger20:  ', ly <> lz:5, ' s/b false');
   writeln('linteger21:  ', lx <= ly:5, ' s/b true');
   writeln('linteger22:  ', lz <= ly:5, ' s/b true');
   writeln('linteger23:  ', ly <= lx:5, ' s/b false');
   writeln('linteger24:  ', ly >= lx:5, ' s/b true');
   writeln('linteger25:  ', ly >= lz:5, ' s/b true');
   writeln('linteger26:  ', lx >= ly:5, ' s/b false');

   { signed linteger variables }
   las := -14;
   lbs := -32;
   lcs := -14;
   lds := 20;
   les := -15;
   lgs := maxlint;
   lhs := mmaxlint;
   lvnum := -maxlint;
   writeln('linteger27:  ', las + lds:1, ' s/b 6');
   writeln('linteger28:  ', lds + las:1, ' s/b 6');
   writeln('linteger29:  ', lbs + lds:1, ' s/b -12');
   writeln('linteger30:  ', las + lbs:1, ' s/b -46');
   writeln('linteger31:  ', lds - las:1, ' s/b 34');
   writeln('linteger32:  ', lbs - lds:1, ' s/b -52');
   writeln('linteger33:  ', lbs - las:1, ' s/b -18');
   writeln('linteger34:  ', lds * las:1, ' s/b -280');
   writeln('linteger35:  ', las * lds:1, ' s/b -280');
   writeln('linteger36:  ', las * lbs:1, ' s/b 448');
   writeln('linteger37:  ', lds div las:1, ' s/b -1');
   writeln('linteger38:  ', lbs div lds:1, ' s/b -1');
   writeln('linteger39:  ', lbs div las:1, ' s/b 2');
   writeln('linteger40:  ', succ(las):1, ' s/b -13');
   writeln('linteger41:  ', pred(lbs):1, ' s/b -33');
   writeln('linteger42: ', sqr(las):1, ' s/b 196');
   writeln('linteger43:  ', odd(las):5, ' s/b false');
   writeln('linteger44:  ', odd(les):5, ' s/b true');
   writeln('linteger45:  ', las = lcs:5, ' s/b true');
   writeln('linteger46:  ', las = lbs:5, ' s/b false');
   writeln('linteger47:  ', las <> lbs:5, ' s/b true');
   writeln('linteger48:  ', las <> lcs:5, ' s/b false');
   writeln('linteger49:  ', las < lds:5, ' s/b true');
   writeln('linteger50:  ', lbs < las:5, ' s/b true');
   writeln('linteger51:  ', lds < las:5, ' s/b false');
   writeln('linteger52:  ', las < lbs:5, ' s/b false');
   writeln('linteger53:  ', lds > las:5, ' s/b true');
   writeln('linteger54:  ', las > lbs:5, ' s/b true');
   writeln('linteger55:  ', las > lds:5, ' s/b false');
   writeln('linteger56:  ', lbs > las:5, ' s/b false');
   writeln('linteger57:  ', las <= lds:5, ' s/b true');
   writeln('linteger58:  ', lbs <= las:5, ' s/b true');
   writeln('linteger59:  ', las <= lcs:5, ' s/b true');
   writeln('linteger60:  ', lds <= las:5, ' s/b false');
   writeln('linteger61:  ', las <= lbs:5, ' s/b false');
   writeln('linteger62:  ', lds >= las:5, ' s/b true');
   writeln('linteger63:  ', las >= lbs:5, ' s/b true');
   writeln('linteger64:  ', las >= lcs:5, ' s/b true');
   writeln('linteger65:  ', las >= lds:5, ' s/b false');
   writeln('linteger66:  ', lbs >= las:5, ' s/b false');
   writeln('linteger67:  ', abs(las):1, ' s/b 14');
   writeln('linteger68:  ', lgs+lhs:1, ' s/b 0');
   writeln('linteger69:  ', lgs-maxlint:1, ' s/b 0');
   writeln('linteger70:  ', lgs+lvnum:1, ' s/b 0');

   { cardinal variables }
   cx := 43; cy := 78; cz := cy;
   writeln('cardinal1:   ', cx + cy:1, ' s/b 121');
   writeln('cardinal2:   ', cy - cx:1, ' s/b 35');
   writeln('cardinal3:   ', cx * cy:1, ' s/b 3354');
   writeln('cardinal4:   ', cy div cx:1, ' s/b 1');
   writeln('cardinal5:   ', cy mod cx:1, ' s/b 35');
   writeln('cardinal6:   ', succ(cx):1, ' s/b 44');
   writeln('cardinal7:   ', pred(cx):1, ' s/b 42');
   writeln('cardinal8:   ', sqr(cx):1, ' s/b 1849');
   writeln('cardinal9:   ', chr(cy), ' s/b N');
   writeln('cardinal10:  ', ord(chr(cx)):1, ' s/b 43');
   writeln('cardinal11:  ', odd(cx):5, ' s/b true');
   writeln('cardinal12:  ', odd(cy):5, ' s/b false');
   writeln('cardinal13:  ', cz = cy:5, ' s/b true');
   writeln('cardinal14:  ', cx = cy:5, ' s/b false');
   writeln('cardinal15:  ', cx < cy:5, ' s/b true');
   writeln('cardinal16:  ', cy < cx:5, ' s/b false');
   writeln('cardinal17:  ', cy > cx:5, ' s/b true');
   writeln('cardinal18:  ', cx > cy:5, ' s/b false');
   writeln('cardinal19:  ', cx <> cy:5, ' s/b true');
   writeln('cardinal20:  ', cy <> cz:5, ' s/b false');
   writeln('cardinal21:  ', cx <= cy:5, ' s/b true');
   writeln('cardinal22:  ', cz <= cy:5, ' s/b true');
   writeln('cardinal23:  ', cy <= cx:5, ' s/b false');
   writeln('cardinal24:  ', cy >= cx:5, ' s/b true');
   writeln('cardinal25:  ', cy >= cz:5, ' s/b true');
   writeln('cardinal26:  ', cx >= cy:5, ' s/b false');

   { lcardinal variables }
   lcx := 43; lcy := 78; lcz := lcy;
   writeln('lcardinal1:   ', lcx + lcy:1, ' s/b 121');
   writeln('lcardinal2:   ', lcy - lcx:1, ' s/b 35');
   writeln('lcardinal3:   ', lcx * lcy:1, ' s/b 3354');
   writeln('lcardinal4:   ', lcy div lcx:1, ' s/b 1');
   writeln('lcardinal5:   ', lcy mod lcx:1, ' s/b 35');
   writeln('lcardinal6:   ', succ(lcx):1, ' s/b 44');
   writeln('lcardinal7:   ', pred(lcx):1, ' s/b 42');
   writeln('lcardinal8:   ', sqr(lcx):1, ' s/b 1849');
   writeln('lcardinal9:   ', chr(lcy), ' s/b N');
   writeln('lcardinal10:  ', ord(chr(lcx)):1, ' s/b 43');
   writeln('lcardinal11:  ', odd(lcx):5, ' s/b true');
   writeln('lcardinal12:  ', odd(lcy):5, ' s/b false');
   writeln('lcardinal13:  ', lcz = lcy:5, ' s/b true');
   writeln('lcardinal14:  ', lcx = lcy:5, ' s/b false');
   writeln('lcardinal15:  ', lcx < lcy:5, ' s/b true');
   writeln('lcardinal16:  ', lcy < lcx:5, ' s/b false');
   writeln('lcardinal17:  ', lcy > lcx:5, ' s/b true');
   writeln('lcardinal18:  ', lcx > lcy:5, ' s/b false');
   writeln('lcardinal19:  ', lcx <> lcy:5, ' s/b true');
   writeln('lcardinal20:  ', lcy <> lcz:5, ' s/b false');
   writeln('lcardinal21:  ', lcx <= lcy:5, ' s/b true');
   writeln('lcardinal22:  ', lcz <= lcy:5, ' s/b true');
   writeln('lcardinal23:  ', lcy <= lcx:5, ' s/b false');
   writeln('lcardinal24:  ', lcy >= lcx:5, ' s/b true');
   writeln('lcardinal25:  ', lcy >= lcz:5, ' s/b true');
   writeln('lcardinal26:  ', lcx >= lcy:5, ' s/b false');

{*******************************************************************************

                              6.32 Extended real types

*******************************************************************************}

   { sreal }

   { unsigned variables }
   sra := 435.23;
   srb := 983.67;
   src := srb;
   srd := 0.3443;
   writeln('sreal1:  ', sra + srb:15, ' s/b  1.418900e+03');
   writeln('sreal2:  ', srb - sra:15, ' s/b  5.484399e+02');
   writeln('sreal3:  ', sra * srb:15, ' s/b  4.281227e+05');
   writeln('sreal4:  ', srb / sra:15, ' s/b  2.260115e+00');
   writeln('sreal5:  ', src = srb:5, ' s/b true');
   writeln('sreal6:  ', sra = srb:5, ' s/b false');
   writeln('sreal7:  ', sra < srb:5, ' s/b true');
   writeln('sreal8:  ', srb < sra:5, ' s/b false');
   writeln('sreal9:  ', srb > sra:5, ' s/b true');
   writeln('sreal10:  ', sra > srb:5, ' s/b false');
   writeln('sreal11:  ', sra <> srb:5, ' s/b true');
   writeln('sreal12:  ', srb <> src:5, ' s/b false');
   writeln('sreal13:  ', sra <= srb:5, ' s/b true');
   writeln('sreal14:  ', src <= srb:5, ' s/b true');
   writeln('sreal15:  ', srb <= sra:5, ' s/b false');
   writeln('sreal16:  ', srb >= sra:5, ' s/b true');
   writeln('sreal17:  ', srb >= src:5, ' s/b true');
   writeln('sreal18:  ', sra >= srb:5, ' s/b false');
   writeln('sreal19:  ', abs(sra):15, ' s/b  4.35230e+02');
   writeln('sreal20:  ', sqr(sra):15, ' s/b  1.89425e+05');
   writeln('sreal21:  ', sqrt(srb):15, ' s/b  3.13635e+01');
   writeln('sreal22:  ', sin(srb):15, ' s/b -3.44290e-01');
   writeln('sreal23:  ', arctan(sra):15, ' s/b  1.56850e+00');
   writeln('sreal24:  ', exp(srd):15, ' s/b  1.41100e+00');
   writeln('sreal25:  ', ln(sra):15, ' s/b  6.07587e+00');
   writeln('sreal26:  ', trunc(sra):1, ' s/b 435');
   writeln('sreal27:  ', round(srb):1, ' s/b 984');
   writeln('sreal28:  ', round(sra):1, ' s/b 435');

   { signed variables }
   sra := -734.2;
   srb := -7634.52;
   src := sra;
   srd := 1034.54;
   sre := -0.38483;
   writeln('sreal29:  ', sra + srd:15, ' s/b  3.003400e+02');
   writeln('sreal30:  ', srd + sra:15, ' s/b  3.003400e+02');
   writeln('sreal31:  ', srb + srd:15, ' s/b -6.599980e+03');
   writeln('sreal32:  ', sra + srb:15, ' s/b -8.368720e+03');
   writeln('sreal33:  ', srd - sra:15, ' s/b  1.768740e+03');
   writeln('sreal34:  ', srb - srd:15, ' s/b -8.669061e+03');
   writeln('sreal35:  ', srb - sra:15, ' s/b -6.900320e+03');
   writeln('sreal36:  ', srd * sra:15, ' s/b -7.595593e+05');
   writeln('sreal37:  ', sra * srd:15, ' s/b -7.595593e+05');
   writeln('sreal38:  ', sra * srb:15, ' s/b  5.605265e+06');
   writeln('sreal39:  ', srd / sra:15, ' s/b -1.409071e+00');
   writeln('sreal40:  ', srb / srd:15, ' s/b -7.379627e+00');
   writeln('sreal41:  ', srb / sra:15, ' s/b  1.039842e+01');
   writeln('sreal42:  ', sra = src:5, ' s/b true');
   writeln('sreal43:  ', sra = srb:5, ' s/b false');
   writeln('sreal44:  ', sra <> srb:5, ' s/b true');
   writeln('sreal45:  ', sra <> src:5, ' s/b false');
   writeln('sreal46:  ', sra < srd:5, ' s/b true');
   writeln('sreal47:  ', srb < sra:5, ' s/b true');
   writeln('sreal48:  ', srd < sra:5, ' s/b false');
   writeln('sreal49:  ', sra < srb:5, ' s/b false');
   writeln('sreal50:  ', srd > sra:5, ' s/b true');
   writeln('sreal51:  ', sra > srb:5, ' s/b true');
   writeln('sreal52:  ', sra > srd:5, ' s/b false');
   writeln('sreal53:  ', srb > sra:5, ' s/b false');
   writeln('sreal54:  ', sra <= srd:5, ' s/b true');
   writeln('sreal55:  ', srb <= sra:5, ' s/b true');
   writeln('sreal56:  ', sra <= src:5, ' s/b true');
   writeln('sreal57:  ', srd <= sra:5, ' s/b false');
   writeln('sreal58:  ', sra <= srb:5, ' s/b false');
   writeln('sreal59:  ', srd >= sra:5, ' s/b true');
   writeln('sreal60:  ', sra >= srb:5, ' s/b true');
   writeln('sreal61: ', sra >= src:5, ' s/b true');
   writeln('sreal62: ', sra >= srd:5, ' s/b false');
   writeln('sreal63: ', srb >= sra:5, ' s/b false');
   writeln('sreal64: ', abs(sra):15, ' s/b  7.34200e+02');
   writeln('sreal65: ', sqr(sra):15, ' s/b  5.39050e+05');
   writeln('sreal66: ', sin(srb):15, ' s/b -4.34850e-01');
   writeln('sreal67: ', arctan(sra):15, ' s/b -1.56943e+00');
   writeln('sreal68: ', exp(sre):15, ' s/b  6.80566e-01');
   writeln('sreal69: ', trunc(sra):15, ' s/b -734');
   writeln('sreal70: ', round(srb):15, ' s/b -7635');
   writeln('sreal71: ', round(sra):15, ' s/b -734');

   { lreal }

   { unsigned variables }
   lra := 435.23;
   lrb := 983.67;
   lrc := lrb;
   lrd := 0.3443;
   writeln('lreal1:  ', lra + lrb:15, ' s/b  1.418900e+03');
   writeln('lreal2:  ', lrb - lra:15, ' s/b  5.484399e+02');
   writeln('lreal3:  ', lra * lrb:15, ' s/b  4.281227e+05');
   writeln('lreal4:  ', lrb / lra:15, ' s/b  2.260115e+00');
   writeln('lreal5:  ', lrc = lrb:5, ' s/b true');
   writeln('lreal6:  ', lra = lrb:5, ' s/b false');
   writeln('lreal7:  ', lra < lrb:5, ' s/b true');
   writeln('lreal8:  ', lrb < lra:5, ' s/b false');
   writeln('lreal9:  ', lrb > lra:5, ' s/b true');
   writeln('lreal10:  ', lra > lrb:5, ' s/b false');
   writeln('lreal11:  ', lra <> lrb:5, ' s/b true');
   writeln('lreal12:  ', lrb <> lrc:5, ' s/b false');
   writeln('lreal13:  ', lra <= lrb:5, ' s/b true');
   writeln('lreal14:  ', lrc <= lrb:5, ' s/b true');
   writeln('lreal15:  ', lrb <= lra:5, ' s/b false');
   writeln('lreal16:  ', lrb >= lra:5, ' s/b true');
   writeln('lreal17:  ', lrb >= lrc:5, ' s/b true');
   writeln('lreal18:  ', lra >= lrb:5, ' s/b false');
   writeln('lreal19:  ', abs(lra):15, ' s/b  4.35230e+02');
   writeln('lreal20:  ', sqr(lra):15, ' s/b  1.89425e+05');
   writeln('lreal21:  ', sqrt(lrb):15, ' s/b  3.13635e+01');
   writeln('lreal22:  ', sin(lrb):15, ' s/b -3.44290e-01');
   writeln('lreal23:  ', arctan(lra):15, ' s/b  1.56850e+00');
   writeln('lreal24:  ', exp(lrd):15, ' s/b  1.41100e+00');
   writeln('lreal25:  ', ln(lra):15, ' s/b  6.07587e+00');
   writeln('lreal26:  ', trunc(lra):1, ' s/b 435');
   writeln('lreal27:  ', round(lrb):1, ' s/b 984');
   writeln('lreal28:  ', round(lra):1, ' s/b 435');

   { signed variables }
   lra := -734.2;
   lrb := -7634.52;
   lrc := lra;
   lrd := 1034.54;
   lre := -0.38483;
   writeln('lreal29:  ', lra + lrd:15, ' s/b  3.003400e+02');
   writeln('lreal30:  ', lrd + lra:15, ' s/b  3.003400e+02');
   writeln('lreal31:  ', lrb + lrd:15, ' s/b -6.599980e+03');
   writeln('lreal32:  ', lra + lrb:15, ' s/b -8.368720e+03');
   writeln('lreal33:  ', lrd - lra:15, ' s/b  1.768740e+03');
   writeln('lreal34:  ', lrb - lrd:15, ' s/b -8.669061e+03');
   writeln('lreal35:  ', lrb - lra:15, ' s/b -6.900320e+03');
   writeln('lreal36:  ', lrd * lra:15, ' s/b -7.595593e+05');
   writeln('lreal37:  ', lra * lrd:15, ' s/b -7.595593e+05');
   writeln('lreal38:  ', lra * lrb:15, ' s/b  5.605265e+06');
   writeln('lreal39:  ', lrd / lra:15, ' s/b -1.409071e+00');
   writeln('lreal40:  ', lrb / lrd:15, ' s/b -7.379627e+00');
   writeln('lreal41:  ', lrb / lra:15, ' s/b  1.039842e+01');
   writeln('lreal42:  ', lra = lrc:5, ' s/b true');
   writeln('lreal43:  ', lra = lrb:5, ' s/b false');
   writeln('lreal44:  ', lra <> lrb:5, ' s/b true');
   writeln('lreal45:  ', lra <> lrc:5, ' s/b false');
   writeln('lreal46:  ', lra < lrd:5, ' s/b true');
   writeln('lreal47:  ', lrb < lra:5, ' s/b true');
   writeln('lreal48:  ', lrd < lra:5, ' s/b false');
   writeln('lreal49:  ', lra < lrb:5, ' s/b false');
   writeln('lreal50:  ', lrd > lra:5, ' s/b true');
   writeln('lreal51:  ', lra > lrb:5, ' s/b true');
   writeln('lreal52:  ', lra > lrd:5, ' s/b false');
   writeln('lreal53:  ', lrb > lra:5, ' s/b false');
   writeln('lreal54:  ', lra <= lrd:5, ' s/b true');
   writeln('lreal55:  ', lrb <= lra:5, ' s/b true');
   writeln('lreal56:  ', lra <= lrc:5, ' s/b true');
   writeln('lreal57:  ', lrd <= lra:5, ' s/b false');
   writeln('lreal58:  ', lra <= lrb:5, ' s/b false');
   writeln('lreal59:  ', lrd >= lra:5, ' s/b true');
   writeln('lreal60:  ', lra >= lrb:5, ' s/b true');
   writeln('lreal61: ', lra >= lrc:5, ' s/b true');
   writeln('lreal62: ', lra >= lrd:5, ' s/b false');
   writeln('lreal63: ', lrb >= lra:5, ' s/b false');
   writeln('lreal64: ', abs(lra):15, ' s/b  7.34200e+02');
   writeln('lreal65: ', sqr(lra):15, ' s/b  5.39050e+05');
   writeln('lreal66: ', sin(lrb):15, ' s/b -4.34850e-01');
   writeln('lreal67: ', arctan(lra):15, ' s/b -1.56943e+00');
   writeln('lreal68: ', exp(lre):15, ' s/b  6.80566e-01');
   writeln('lreal69: ', trunc(lra):15, ' s/b -734');
   writeln('lreal70: ', round(lrb):15, ' s/b -7635');
   writeln('lreal71: ', round(lra):15, ' s/b -734');

{*******************************************************************************

                                6.35 Matrix mathmatics

*******************************************************************************}

   ! not implemented

{*******************************************************************************

                          6.36 Saturated math operators

*******************************************************************************}

   ! not implemented

{*******************************************************************************

                               6.37 Properties

*******************************************************************************}

   ! not implemented

{*******************************************************************************

                               6.39.2 Used module

*******************************************************************************}

   writeln('um1:');
   mod1_i := 87;
   mod1_p;
   writeln('s/b');
   writeln('this is the used module: 87');

{*******************************************************************************

                                 6.39.4 Overrides

*******************************************************************************}

   writeln('ov1:');
   abstract;
   writeln('s/b');
   writeln('This is the overriding procedure');
   writeln('This is the abstract procedure');

{*******************************************************************************

                               6.39.2 Joined module

*******************************************************************************}

   writeln('lm1:');
   pascaline2.mod1_i := 87;
   pascaline2.mod1_p;
   writeln('s/b');
   writeln('this is the joined module: 87');

   {
   override of joined module
   does not work at present.
   writeln('lm2:');
   pascaline2.zork;
   pascaline2.zork(52);
   pascaline2.zork('hi there');
   writeln(bark:1);
   writeln('s/b');
   writeln('This is bark');
   writeln('bark: the integer is: 52');
   writeln('bark: the string is: hi there');
   writeln('42');
   }

{*******************************************************************************

                               6.41 Classes

*******************************************************************************}

   ! Not implemented

{*******************************************************************************

                          Annex E: Character escapes

*******************************************************************************}

   ! note that these are Annex E character escapes and are optional
   writeln('ce1: my\'self s/b my''self');
   writeln('ce2: this\\one s/b this<slash>one');
   writeln('ce3: \101 s/b e');
   writeln('ce4: \$6f s/b o');
   writeln('ce5: \&132 s/b Z');
   writeln('ce6: \%1001001 s/b I');
   writeln('ce7: ');
   write(ord('\nul'):1, ' ');
   write(ord('\soh'):1, ' ');
   write(ord('\stx'):1, ' ');
   write(ord('\etx'):1, ' ');
   write(ord('\eot'):1, ' ');
   write(ord('\enq'):1, ' ');
   write(ord('\ack'):1, ' ');
   write(ord('\bel'):1, ' ');
   write(ord('\bs'):1, ' ');
   write(ord('\ht'):1, ' ');
   write(ord('\lf'):1, ' ');
   write(ord('\vt'):1, ' ');
   write(ord('\ff'):1, ' ');
   write(ord('\cr'):1, ' ');
   write(ord('\so'):1, ' ');
   write(ord('\si'):1, ' ');
   write(ord('\dle'):1, ' ');
   write(ord('\dc1'):1, ' ');
   write(ord('\xon'):1, ' ');
   write(ord('\dc2'):1, ' ');
   write(ord('\dc3'):1, ' ');
   write(ord('\xoff'):1, ' ');
   write(ord('\dc4'):1, ' ');
   writeln;
   write(ord('\nak'):1, ' ');
   write(ord('\syn'):1, ' ');
   write(ord('\etb'):1, ' ');
   write(ord('\can'):1, ' ');
   write(ord('\em'):1, ' ');
   write(ord('\sub'):1, ' ');
   write(ord('\esc'):1, ' ');
   write(ord('\fs'):1, ' ');
   write(ord('\gs'):1, ' ');
   write(ord('\rs'):1, ' ');
   write(ord('\us'):1, ' ');
   write(ord('\del'):1, ' ');
   writeln(ord('\\'):1, ' ');
   writeln('s/b 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 17 18 19 19 20');
   writeln('21 22 23 24 25 26 27 28 29 30 31 127');
   write('ce8: \\'); writeln(' s/b \\');
   write('ce9: \a'); writeln(' s/b a');

{*******************************************************************************

                             6.24 Halt procedure

*******************************************************************************}

   writeln('ht:');
   writeln('The test should now halt');
   halt;
   writeln('!!! Bad !!! halt did not take effect')

end.
