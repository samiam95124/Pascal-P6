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

program pascaline(output);

uses pascaline1; { test used module }

{ this will test for extended labeling }

var a_1: integer;
    c:   char;
    i, x, y:   integer;

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

type enum_a  = (one, two, three);
     string  = packed array of char;
     pstring = ^string;
     byte    = 0..255;

var s:         ^string;
    st:        packed array 10 of char;
    ia:        ^iarr;
    miap:      ^miarr;
    a, b:      integer;
    ft:        text;
    fb:        file of byte;
    ba:        byte;
    sp:        pstring;
    mia:       array 10, 10 of integer;
    cd:        cardinal;
    li:        linteger;
    lc:        lcardinal;
    excpt:     exception;
    s2(10):    string;
    v(10):     vector;
    m(10, 10): matrix;
    vr:        record case e: enum_a of
                 one..two: (i: integer);
                 three:    (c: char)
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

                                   Constants

*******************************************************************************}

! Pascaline style comment
   writeln('ext1: ', $a5:1, ' s/b 165'); ! pascaline trailing comment
   writeln('ext2: ', &72:1, ' s/b 58');
   writeln('ext3: ', %011001:1, ' s/b ', 25:1);
   writeln('ext3: ', 123_456_:1, ' s/b ', 123456);
   writeln('ext4: ', $342_834_:1, ' s/b ', 3418164);
   writeln('ext5: ', &321_732_:1, ' s/b ', 107482);
   writeln('ext6: ', %0101_0111_:1, ' s/b ', 87);

   ! note that these are Annex E character escapes and are optional
   writeln('ext7: my\'self s/b my''self');
   writeln('ext8: this\\one s/b this<slash>one');
   writeln('ext9: \101 s/b e');
   writeln('ext10: \$6f s/b o');
   writeln('ext11: \&132 s/b Z');
   writeln('ext12: \%1001001 s/b I');
   writeln('ext13: ');
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
   writeln(ord('\del'):1, ' ');
   writeln('s/b 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 17 18 19 19 20');
   writeln('21 22 23 24 25 26 27 28 29 30 31 127');

{*******************************************************************************

                                Extended goto labels

*******************************************************************************}

   write('ext14: start ');
   goto skip_over;
   write('!!! BAD !!!');
   ! Note this also contains an extended Pascaline symbol character, '_'
   skip_over: writeln(' stop s/b start stop');

{*******************************************************************************

                               Constant expressions

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

                                    Fixed

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

                                Type convertion

*******************************************************************************}

   write('tc1: ');
   for i := 0 to 2 do case enum_a(i) of

      one: write('one ');
      two: write('two ');
      three: write('three ')

   end;
   writeln(' s/b one two three');

{*******************************************************************************

                        Single demension container arrays

*******************************************************************************}

   write('sdc1: ');
   new(s, 10);
   s^ := 'hi there ?';
   write(s^);
   writeln(' s/b hi there ?');
   write('sdc2: ');
   new(s, 10);
   s^ := 'hi there ?';
   st := s^;
   write(st);
   writeln(' s/b hi there ?');
   write('sdc3: ');
   new(s, 10);
   s^ := 'hi there ?';
   for i := 1 to 10 do write(s^[i]);
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
   s2 := 'hi there ?';
   write(s2);
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

{*******************************************************************************

                    Multiple dimension container arrays

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

                 Variable boolean integer expressions

*******************************************************************************}

   a := 56;
   b := 13;
   writeln('ext43: ', not a:1, ' s/b 9223372036854775751');
   writeln('ext44: ', a and b:1, ' s/b 8');
   writeln('ext45: ', a or b:1, ' s/b 61');
   writeln('ext46: ', a xor b:1, ' s/b 53');

{*******************************************************************************

                               Out parameters

*******************************************************************************}

   write('op1: ');
   outpar(i);
   writeln(i:1, ' s/b 42');

{*******************************************************************************

                       Container arrays as parameters

*******************************************************************************}

   write('ext47: ');
   prtstr('hi george');
   writeln(' s/b hi george');
   write('ext48: ');
   st := 'hi george ';
   prtstr1(st);
   writeln(' s/b hi george');

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

                    File handling procedures and functions

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

{*******************************************************************************

                      Container pointer function results

*******************************************************************************}

   write('cp1: ');
   sp := getstr;
   writeln(sp^, ' s/b hi there');

{*******************************************************************************

                            Extended case statements

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

                          variant record case ranges

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

                       Extended write/writeln statements

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

                       Extended read/readln statements

*******************************************************************************}

{tbd}

{*******************************************************************************

                                       Overloads

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

                                       Overrides

*******************************************************************************}

   writeln('ov1:');
   abstract;
   writeln('s/b');
   writeln('This is the overriding procedure');
   writeln('This is the abstract procedure');

{*******************************************************************************

                                   Exception handling

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

{*******************************************************************************

                   Assert: all we can do here is test false asserts

*******************************************************************************}

   writeln('as1:');
   writeln('Testing negative assertion, this should not stop here');
   i := 4;
   assert(i = 4);

{*******************************************************************************

                               Used module

*******************************************************************************}

   writeln('ex2:');
   mod1_i := 87;
   mod1_p;
   writeln('s/b');
   writeln('this is the used module: 87');

{*******************************************************************************

                             Halt procedure

*******************************************************************************}

   writeln('ht:');
   writeln('The test should now halt');
   halt;
   writeln('!!! Bad !!! halt did not take effect')

end.
