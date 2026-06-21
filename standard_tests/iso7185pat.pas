(*$l-*)
{******************************************************************************
*                                                                             *
*                      TEST SUITE FOR ISO 7185 PASCAL                         *
*                                                                             *
*                       The "PASCAL ACCEPTANCE TEST"                          *
*                                                                             *
*                              Version 1.1                                    *
*                                                                             *
*            Copyright (C) 2010 S. A. Moore - All rights reserved             *
*                                                                             *
* This program attempts to use and display the results of each feature of     *
* standard pascal. It is a "positive" test in that it should compile and run  *
* error free, and thus does not check error conditions/detection.             *
*                                                                             *
* Each test is labeled and numbered, and the expected result also output, so  *
* that the output can be self evidently hand checked.                         *
*                                                                             *
* The output can be redirected to a printer or a file to facillitate such     *
* checking.                                                                   *
*                                                                             *
* The output can also be automatically checked by comparing a known good file *
* to the generated file. To this end, we have regularized the output,         *
* specifying all output field widths that are normally compiler dependent.    *
*                                                                             *
* Only the following factors exist that are allowed to be compiler dependent, *
* but could cause a miscompare of the output:                                 *
*                                                                             *
*    1. The case of output booleans. We have choosen a standard format of     *
*       LOWER case for such output. Note that compilers can choose any case,  *
*       or mixture of cases.                                                  *
*                                                                             *
* Because of this, it may be required to return to hand checking when         *
* encountering a differing compiler system.                                   *
*                                                                             *
* Notes:                                                                      *
*                                                                             *
* 1. This test will not run or compile unless "set of char" is possible.      *
* This does not mean that compilers lacking in "set of char" capability are   *
* not standard. However, in the authors opinion, this is a crippling          *
* limitation for any Pascal compiler.                                         *
*                                                                             *
* 2. Because there is no "close" function in ISO 7185 Pascal, the file        *
* handling contained with is likely to generate a large number of open        *
* temporary files. This may cause some implementations to trip a limit on the *
* number of total open files. If this occurs, turn the constant "testfile"    *
* below to "false". This will cause the temporary files test to be skipped.   *
*                                                                             *
* 3. The test assumes that both upper and lower case characters are           *
* available, both in source and in the text files that are processed. The     *
* ISO 7185 standard does not technically require this.                        *
*                                                                             *
* The following sections track "context testing": the same operation battery  *
* applied to each type across every access path. Status as of version 1.1:     *
*                                                                             *
* 1. Buffer variables. DONE. The per-type "xxxcontexts" procedures exercise    *
* each type's operations through a file buffer variable (f^) alongside the     *
* other access paths.                                                         *
*                                                                             *
* 2. Arrays, records and pointers containing files. DONE. "filecontainers"     *
* exercises a file held in a record field, an array element, a record-of-      *
* array-of-files, an array-of-records-with-files, and a pointer.              *
*                                                                             *
* 3. Pointer variables, array variables, and other complex accesses subjected  *
* to the same extensive tests as base variables. DONE. Each "xxxcontexts"      *
* procedure sweeps the battery through local, array element, record field,     *
* nested field, pointer, file buffer, value param, var param, and surrounding  *
* local.                                                                      *
*                                                                             *
* 4. Access to locals of a surrounding procedure. DONE. Each "xxxcontexts"     *
* procedure includes a "surround" nested procedure that reaches its enclosing  *
* procedure's local.                                                          *
*                                                                             *
* 5. Dynamic storage that allocates various sizes, not just integers. DONE.    *
* The pointer context in each "xxxcontexts" allocates that type (char through  *
* record/array), and the record area tests variant new/dispose sizes.         *
*                                                                             *
* 6. Tests for reads from the "input" file, as well as explicitly specifying   *
* the output file. DEFERRED. Binding a program parameter to an external entity *
* is implementation-defined in ISO 7185; it is exercised in the Pascaline      *
* tests where the binding is defined.                                         *
*                                                                             *
* 7. Test for page. DEFERRED. Its effect is undefined by ISO 7185, so there is *
* nothing to self-check against.                                              *
*                                                                             *
******************************************************************************}

program iso7185pat(output);

label
      0, 3, 9999, 0004;

const

      { flags to control run }

      { the pointer torture test takes time and isn't run for interpreted
        systems }
      doptrtortst = false;
      
      tcnst = 768;
      scst = 'this is a string';
      ccst = 'v';
      tsncst = -52;
      rcnst = 43.33;
      rscst = -84.22;
      tsncst2 = -tcnst;
      tsncst3 = -tsncst;
      rscst2 = -rcnst;
      rscst3 = -rscst;
      testfile = true;
      mmaxint = -maxint;
      cone = 1;

type
     string10 = packed array [1..10] of char;
     enum  = (one, two, three, four, five, six, seven, eight, nine, ten);
     esub  = three..six;
     subr  = 10..20;
     (* Note use of alternatives for '[' and ']'. The availablity of these
        alternates is implementation defined. *)
     arri  = array (.1..10.) of integer;
     arrim = array [1..2, 1..2] of array [1..2, 1..2, 1..2, 1..2] of integer;
     cset  = set of char;
     { Note that the availability of the alternate '@' is implementation
       defined }
     iptr  = @integer;
     { recursive pointer type for the linked-list test (ISO 6.4.4) }
     lnp   = ^lnode;
     lnode = record vl: integer; nxt: lnp end;
     recs  = record

               a: integer;
               b: char

            end;
     rec = record

              i:   integer;
              b:   boolean;
              c:   char;
              e:   enum;
              es:  esub;
              s:   subr;
              r:   real;
              st:  string10;
              a:   arri;
              rc:  recs;
              stc: cset;
              p:   iptr

           end;
     prec = packed record

              i:   integer;
              b:   boolean;
              c:   char;
              e:   enum;
              es:  esub;
              s:   subr;
              r:   real;
              st:  string10;
              a:   arri;
              rc:  recs;
              stc: cset;
              p:   iptr

           end;
     recv = record

               a: integer;
               b: char;
               case c: boolean of

                  false: (d: string10);
                  true:  (e: enum)

               { end }

            end;
     recvb = record
     
                i: integer;
                case b: boolean of
              
                   true: (c: char);
                   false: (
                 
                      case q: boolean of
                    
                         true: (r: real);
                         false: (n: boolean)
                       
                   )
                       
             end;
     recvc = record

              case vt: subr of

                 10, 11, 12, 13, 14, 15: (vi: integer);
                 16, 17, 18, 19, 20: (vb: boolean)

              { end }

           end;
     arrr = array [1..10] of recs;
     vart = (vti, vtb, vtc, vte, vtes, vts, vtr, vtst, vta, vtrc, vtstc, vtp);
     intalias = integer;

var
    i, x, y, z, q, n, t : integer;
    srx, sry, srz: 0..100;
    sras, srbs, srcs, srds, sres: -100..100;
    a : array [1..10] of integer;
    r : record

           rx: integer;
           rc: char;
           ry: integer;
           rb: boolean;
           rs: packed array [1..10] of char;

        end;
    da:    array [1..10, 1..10] of integer;
    sa, sb, sc : packed array [1..10] of char;
    ca, cb, cc : char;
    car :  array ['a'..'z'] of integer;
    sar:   array [1..10] of packed array [1..10] of char;
    ba, bb, bc : boolean;
    sva, svb, svc : (mon, tue, wed, thur, fri, sat, sun);
    s:     string10;
    as, bs, cs, ds, es, gs, hs : integer;
    vnum: -maxint..maxint;
    ra, rb, rc, rd, re: real;
    sta,   stb, stc, std: set of 1..100;
    ste:   set of 1..10;
    stf:   packed set of 1..10;
    stg:   packed set of 1..20;
    csta,  cstb, cstc, cstd: set of char;
    cste:  set of 'a'..'z';
    cstf:  packed set of 'a'..'f';
    cstg:  packed set of char;
    ci:    char;
    sena,  senb, senc, send: set of enum;
    sene:  set of one..five;
    senf:  packed set of enum;
    seng:  packed set of one..seven;
    ei, ea: enum;
    sba,   sbb, sbc, sbd: set of boolean;
    sbe:   set of false..true;
    sbf:   packed set of boolean;
    sbg:   packed set of false..true;
    ai:    arri;
    arec:  rec;
    parec: prec;
    vrec:  recv;
    ip:    iptr;
    rcs1,  rcs2: recs; { for whole-record assignment + file-of-record tests }
    lh,    lp:   lnp;  { linked-list head and cursor }
    avi:   arri;
    avi2:  arri;
    pavi:  packed array [1..10] of integer;
    avis:  array [1..10] of 10..20;
    pavis: packed array [1..10] of 10..20;
    avb:   array [1..10] of boolean;
    pavb:  packed array [1..10] of boolean;
    avr:   array [1..10] of real;
    pavr:  packed array [1..10] of real;
    avc:   array [1..10] of char;
    pavc:  packed array [1..10] of char;
    avcs:  array [1..10] of 'g'..'p';
    pavcs: packed array [1..10] of 'g'..'p';
    ave:   array [1..10] of enum;
    pave:  packed array [1..10] of enum;
    aves:  array [1..10] of esub;
    paves: packed array [1..10] of esub;
    avs:   array [1..10] of cset;
    pavs:  packed array [1..10] of cset;
    avrc:  array [1..10] of recs;
    pavrc: packed array [1..10] of recs;
    avf:   array [1..10] of text;
    pavf:  packed array [1..10] of text;
    avp:   array [1..10] of iptr;
    pavp:  packed array [1..10] of iptr;
    bia:   array [boolean] of integer;
    pbia:  packed array [boolean] of integer;
    cia:   array [char] of integer;
    pcia:  packed array [char] of integer;
    csia:  array ['a'..'z'] of integer;
    pcsia: packed array ['a'..'z'] of integer;
    eia:   array [enum] of integer;
    peia:  packed array [enum] of integer;
    esia:  array [two..six] of integer;
    pesia: packed array [two..six] of integer;
    mdar:  arrim;
    mdar2: arrim;
    vra:   record

              i: integer;
              case vt: vart of

                 vti:   (vdi:   integer;  a: integer);
                 vtb:   (vdb:   boolean;  b: integer);
                 vtc:   (vdc:   char;     c: integer);
                 vte:   (vde:   enum;     d: integer);
                 vtes:  (vdes:  esub;     e: integer);
                 vts:   (vds:   subr;     f: integer);
                 vtr:   (vdr:   real;     g: integer);
                 vtst:  (vdst:  string10; h: integer);
                 vta:   (vda:   arri;     j: integer);
                 vtrc:  (vdrc:  recs;     k: integer);
                 vtstc: (vdstc: cset;     l: integer);
                 vtp:   (vdp:   iptr;     m: integer)

              { end }

           end;
    vvrs:  record

              case vt: subr of

                 10, 11, 12, 13, 14, 15: (vi: integer);
                 16, 17, 18, 19, 20: (vb: boolean)

              { end }

           end;
    vvrb:  record

              case vt:boolean of

                 true: (vi: integer);
                 false: (vb: boolean)

              { end }

           end;
    vvre:  record

              case vt: enum of

                 one, two, three, four, five: (vi: integer);
                 six, seven, eight, nine, ten: (vb: boolean)

              { end }

           end;
    vvres: record

              case vt: esub of

                 three, four: (vi: integer);
                 five, six: (vb: boolean)

              { end }

           end;
    nvr:   record

              i: integer;
              r: record

                 i: integer;
                 r: record

                    i: integer;
                    r: record

                       i: integer;
                       r: record

                          i: integer;
                          r: record

                             i: integer;
                             r: record

                                i: integer;
                                r: record

                                   i: integer;
                                   r: record

                                      i: integer;
                                      r: record

                                         i: integer

                                      end

                                   end

                                end

                             end

                          end

                       end

                    end

                 end

              end

           end;
    rpa:   ^rec;
    rpb:   ^recvb;
    rpc:   ^recvc;
    ara:   arrr;
    fi:    file of integer;
    pfi:   packed file of integer;
    fb:    file of boolean;
    pfb:   packed file of boolean;
    fc:    file of char;
    pfc:   packed file of char;
    fe:    file of enum;
    pfe:   packed file of enum;
    fes:   file of esub;
    pfes:  packed file of esub;
    fs:    file of subr;
    pfs:   packed file of subr;
    fr:    file of real;
    pfr:   packed file of real;
    fst:   file of string10;
    pfst:  packed file of string10;
    fa:    file of arri;
    pfa:   packed file of arri;
    frc:   file of recs;
    pfrc:  packed file of recs;
    fstc:  file of cset;
    pfstc: packed file of cset;
    fp:    file of iptr;
    pfp:   packed file of iptr;
    ft:    text;
    pti, pti1: ^integer;
    pti2:  iptr;
    ptb:   ^boolean;
    ptc:   ^char;
    pte:   ^enum;
    ptes:  ^esub;
    pts:   ^subr;
    ptr:   ^real;
    ptst:  ^string10;
    pta:   ^arri;
    ptrc:  ^recs;
    ptstc: ^cset;
    ptp:   ^iptr;
    ipa,       ipb, ipc, ipd, ipe: ^integer;
    iap:       array [1..100] of ^integer;
    rndseq:    integer;
    cnt, cnt2: integer;
    rn:        integer;
    rcastt: integer;
    rcast: record case rcastt: boolean of true: (); false: () end;
    pi1, pi2: ^integer;
    intaliasv: intalias;
    iso7185pat: integer;
    MyOwnInteger: integer;
    myvar: integer;
    myvarmyvar: integer;
    myvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar: integer;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar: integer;

procedure junk1(z, q : integer);

begin

   write(z:1, ' ', q:1);

end;

procedure junk2(var z : integer);

begin

   z := z + 1

end;

procedure junk3(var p : string10);

begin

   write(p)

end;

procedure junk4(p : string10);

begin

   p[5] := '?';
   write(p)

end;

function junk5(x : integer) : integer;

begin

   junk5 := x + 1

end;

procedure junk6;

begin

   goto 09999

end;

function junk7(a, b, c: integer): integer; forward;

function junk7;

var x, y, z: integer;

begin

   x := 1;
   y := 2;
   z := 3;
   write(a:1, ' ', b:1, ' ', c:1, ' ');
   a := 4;
   b := 5;
   c := 6;
   write(c:1, ' ', b:1, ' ', a:1, ' ', z:1, ' ', y:1, ' ', x:1);
   junk7 := 78

end;

procedure junk8(a: integer; b: boolean; c: char; e: enum; es: esub; s: subr;
                r: real; st: string10; ar: arri; rc: rec; rv: recv; stc: cset;
                p: iptr);

var i:  integer;
    ci: char;

begin

   writeln(a:1, ' ', b:5, ' ', c:1, ' ', ord(e):1, ' ', ord(es):1, ' ', s:1, ' ',
           r:15, ' ', st);
   for i := 1 to 10 do write(ar[i]:1, ' '); writeln;
   writeln(rc.i:1, ' ', rc.b:5, ' ', rc.c:1, ' ', ord(rc.e):1, ' ', ord(rc.es):1,
           ' ', rc.s:1, ' ', rc.r:15, ' ', rc.st);
   for i := 1 to 10 do write(rc.a[i]:1, ' '); writeln;
   writeln(rc.rc.a:1, ' ', rc.rc.b:1);
   for ci := 'a' to 'j' do if ci in rc.stc then write(ci) else write('_');
   writeln;
   writeln(rc.p^:1);
   writeln(rv.a:1, ' ', rv.b:1, ' ', rv.c:5);
   if rv.c then writeln(ord(rv.e):1) else writeln(rv.d);
   for ci := 'a' to 'j' do if ci in stc then write(ci) else write('_');
   writeln;
   writeln(p^:1)

end;

procedure junk9(procedure junk9(junk9, b: integer; c: char);
                function y(a: integer): integer);

begin

   junk9(9834, 8383, 'j');
   write(' ', y(743):1);

end;

procedure junk10(x, y: integer; junk10: char);

begin

   write(x:1, ' ', y:1, ' ', junk10:1)

end;

function junk11(x: integer): integer;

begin

   junk11 := succ(x)

end;

procedure junk12(procedure xq(function yq(z: integer): integer);
                 function q(n: integer): integer);

begin

   xq(q)

end;

procedure junk13(function xz(z: integer): integer);

begin

   write(xz(941):1)

end;

procedure junk14;

var i, x: integer;

procedure junk15;

begin

   write(i:1, ' ', x:1)

end;

begin

   i := 62;
   x := 76;
   junk15

end;

procedure junk16; begin end;

procedure junk17(procedure x; i: integer);

procedure junk18;

begin

 write(i:1)

end;

begin

   x;
   if i=52 then junk17(junk18, 83)

end;

{ test preference of pointer bonding to current scope }

procedure junk19;

type pt = ^intalias;
     intalias = char;

var p: pt;

begin

   new(p);
   p^ := 'a';
   write(p^);
   dispose(p)

end;

{ test ability to assign function result to nested function }

function junk20: integer;

var i: integer;

function inner: integer;

begin

   inner := 12;
   junk20 := 37

end;

begin

   i := inner

end;

function frp: iptr;

begin

   frp := pti2
   
end;
 
function random (low, hi : integer) : integer;

const a = 16807;
      m = 2147483647;

var gamma: integer;

begin
  gamma := a*(rndseq mod (m div a))-(m mod a)*(rndseq div (m div a));
  if gamma > 0 then rndseq := gamma else rndseq := gamma+m;
  random := rndseq div (maxint div (hi-low+1))+low
end {of random};

function junk21: integer;

var
  true:    1..10;
  false:   1..10;
  real:    1..10;
  boolean: 1..10;
  text:    1..10;
  abs:     1..10;
  sqr:     1..10;
  sqrt:    1..10;
  sin:     1..10;
  cos:     1..10;
  arctan:  1..10;
  ln:      1..10;
  exp:     1..10;
  trunc:   1..10;
  round:   1..10;
  ord:     1..10;
  chr:     1..10;
  succ:    1..10;
  pred:    1..10;
  odd:     1..10;
  eoln:    1..10;
  eof:     1..10;
  read:    1..10;
  readln:  1..10;
  write:   1..10;
  writeln: 1..10;
  rewrite: 1..10;
  reset:   1..10;
  put:     1..10;
  get:     1..10;
  page:    1..10;
  new:     1..10;
  dispose: 1..10;
  pack:    1..10;
  unpack:  1..10;

begin

  true    := 1;    
  false   := 1;   
  real    := 1;  
  boolean := 1; 
  text    := 1;    
  abs     := 1;     
  sqr     := 1;     
  sqrt    := 1;    
  sin     := 1;     
  cos     := 1;     
  arctan  := 1;  
  ln      := 1;      
  exp     := 1;     
  trunc   := 1;   
  round   := 1;   
  ord     := 1;     
  chr     := 1;     
  succ    := 1;    
  pred    := 1;    
  odd     := 1;     
  eoln    := 1;    
  eof     := 1;     
  read    := 1;    
  readln  := 1;  
  write   := 1;   
  writeln := 1; 
  rewrite := 1; 
  reset   := 1;   
  put     := 1;     
  get     := 1;     
  page    := 1;    
  new     := 1;     
  dispose := 1; 
  pack    := 1;    
  unpack  := 1;  
  
  junk21 := true+false+real+boolean+text+abs+sqr+sqrt+sin+cos+arctan+ln+
            exp+trunc+round+ord+chr+succ+pred+odd+eoln+eof+read+readln+write+
            writeln+rewrite+reset+put+get+page+new+dispose+pack+unpack

end;

{ Context tests: the integer operation battery (assign, +, -, *, div, mod, the
  ISO non-negative mod on a negative dividend, abs/succ/sqr, relational, odd)
  run through every access-path context -- local, array element, record field,
  nested record-array field, pointer, file buffer, value and var parameters,
  and a surrounding procedure's local. Each line states its expected result
  inline. This is the prototype shape for sweeping every type through every
  context. }
procedure integercontexts;

var lt: integer;

   procedure intvalpar(t: integer); { value-parameter context }
   begin
      t := 5;
      writeln('Int valpar 1:  ', t:1, ' s/b 5');
      t := t+7; t := t*3; t := t-1;
      writeln('Int valpar 2:  ', t:1, ' s/b 35');
      t := t div 4; t := t mod 6;
      writeln('Int valpar 3:  ', t:1, ' s/b 2');
      t := t-10; t := t mod 3;
      writeln('Int valpar 4:  ', t:1, ' s/b 1');
      t := sqr(succ(abs(t)));
      writeln('Int valpar 5:  ', t:1, ' s/b 4');
      writeln('Int valpar 6:  ', (t = 4):5, ' ', odd(t):5, ' s/b  true false')
   end;

   procedure intvarpar(var t: integer); { var-parameter context }
   begin
      t := 5;
      writeln('Int varpar 1:  ', t:1, ' s/b 5');
      t := t+7; t := t*3; t := t-1;
      writeln('Int varpar 2:  ', t:1, ' s/b 35');
      t := t div 4; t := t mod 6;
      writeln('Int varpar 3:  ', t:1, ' s/b 2');
      t := t-10; t := t mod 3;
      writeln('Int varpar 4:  ', t:1, ' s/b 1');
      t := sqr(succ(abs(t)));
      writeln('Int varpar 5:  ', t:1, ' s/b 4');
      writeln('Int varpar 6:  ', (t = 4):5, ' ', odd(t):5, ' s/b  true false')
   end;

   procedure intsurround; { surrounding-procedure-local context: works on lt }
   begin
      lt := 5;
      writeln('Int surround 1:  ', lt:1, ' s/b 5');
      lt := lt+7; lt := lt*3; lt := lt-1;
      writeln('Int surround 2:  ', lt:1, ' s/b 35');
      lt := lt div 4; lt := lt mod 6;
      writeln('Int surround 3:  ', lt:1, ' s/b 2');
      lt := lt-10; lt := lt mod 3;
      writeln('Int surround 4:  ', lt:1, ' s/b 1');
      lt := sqr(succ(abs(lt)));
      writeln('Int surround 5:  ', lt:1, ' s/b 4');
      writeln('Int surround 6:  ', (lt = 4):5, ' ', odd(lt):5, ' s/b  true false')
   end;

begin

   { local variable }
   lt := 5;
   writeln('Int local 1:  ', lt:1, ' s/b 5');
   lt := lt+7; lt := lt*3; lt := lt-1;
   writeln('Int local 2:  ', lt:1, ' s/b 35');
   lt := lt div 4; lt := lt mod 6;
   writeln('Int local 3:  ', lt:1, ' s/b 2');
   lt := lt-10; lt := lt mod 3;
   writeln('Int local 4:  ', lt:1, ' s/b 1');
   lt := sqr(succ(abs(lt)));
   writeln('Int local 5:  ', lt:1, ' s/b 4');
   writeln('Int local 6:  ', (lt = 4):5, ' ', odd(lt):5, ' s/b  true false');

   { array element a[i] }
   a[3] := 5;
   writeln('Int array 1:  ', a[3]:1, ' s/b 5');
   a[3] := a[3]+7; a[3] := a[3]*3; a[3] := a[3]-1;
   writeln('Int array 2:  ', a[3]:1, ' s/b 35');
   a[3] := a[3] div 4; a[3] := a[3] mod 6;
   writeln('Int array 3:  ', a[3]:1, ' s/b 2');
   a[3] := a[3]-10; a[3] := a[3] mod 3;
   writeln('Int array 4:  ', a[3]:1, ' s/b 1');
   a[3] := sqr(succ(abs(a[3])));
   writeln('Int array 5:  ', a[3]:1, ' s/b 4');
   writeln('Int array 6:  ', (a[3] = 4):5, ' ', odd(a[3]):5, ' s/b  true false');

   { record field arec.i }
   arec.i := 5;
   writeln('Int recfld 1:  ', arec.i:1, ' s/b 5');
   arec.i := arec.i+7; arec.i := arec.i*3; arec.i := arec.i-1;
   writeln('Int recfld 2:  ', arec.i:1, ' s/b 35');
   arec.i := arec.i div 4; arec.i := arec.i mod 6;
   writeln('Int recfld 3:  ', arec.i:1, ' s/b 2');
   arec.i := arec.i-10; arec.i := arec.i mod 3;
   writeln('Int recfld 4:  ', arec.i:1, ' s/b 1');
   arec.i := sqr(succ(abs(arec.i)));
   writeln('Int recfld 5:  ', arec.i:1, ' s/b 4');
   writeln('Int recfld 6:  ', (arec.i = 4):5, ' ', odd(arec.i):5, ' s/b  true false');

   { nested: record's array field arec.a[i] }
   arec.a[3] := 5;
   writeln('Int recarr 1:  ', arec.a[3]:1, ' s/b 5');
   arec.a[3] := arec.a[3]+7; arec.a[3] := arec.a[3]*3; arec.a[3] := arec.a[3]-1;
   writeln('Int recarr 2:  ', arec.a[3]:1, ' s/b 35');
   arec.a[3] := arec.a[3] div 4; arec.a[3] := arec.a[3] mod 6;
   writeln('Int recarr 3:  ', arec.a[3]:1, ' s/b 2');
   arec.a[3] := arec.a[3]-10; arec.a[3] := arec.a[3] mod 3;
   writeln('Int recarr 4:  ', arec.a[3]:1, ' s/b 1');
   arec.a[3] := sqr(succ(abs(arec.a[3])));
   writeln('Int recarr 5:  ', arec.a[3]:1, ' s/b 4');
   writeln('Int recarr 6:  ', (arec.a[3] = 4):5, ' ', odd(arec.a[3]):5, ' s/b  true false');

   { pointer ip^ }
   new(ip);
   ip^ := 5;
   writeln('Int ptr 1:  ', ip^:1, ' s/b 5');
   ip^ := ip^+7; ip^ := ip^*3; ip^ := ip^-1;
   writeln('Int ptr 2:  ', ip^:1, ' s/b 35');
   ip^ := ip^ div 4; ip^ := ip^ mod 6;
   writeln('Int ptr 3:  ', ip^:1, ' s/b 2');
   ip^ := ip^-10; ip^ := ip^ mod 3;
   writeln('Int ptr 4:  ', ip^:1, ' s/b 1');
   ip^ := sqr(succ(abs(ip^)));
   writeln('Int ptr 5:  ', ip^:1, ' s/b 4');
   writeln('Int ptr 6:  ', (ip^ = 4):5, ' ', odd(ip^):5, ' s/b  true false');
   dispose(ip);

   { file buffer fi^: swept as an ordinary variable, then the file round-trip
     and buffer/eof status checks specific to its file nature }
   rewrite(fi);
   fi^ := 5;
   writeln('Int buf 1:  ', fi^:1, ' s/b 5');
   fi^ := fi^+7; fi^ := fi^*3; fi^ := fi^-1;
   writeln('Int buf 2:  ', fi^:1, ' s/b 35');
   fi^ := fi^ div 4; fi^ := fi^ mod 6;
   writeln('Int buf 3:  ', fi^:1, ' s/b 2');
   fi^ := fi^-10; fi^ := fi^ mod 3;
   writeln('Int buf 4:  ', fi^:1, ' s/b 1');
   fi^ := sqr(succ(abs(fi^)));
   writeln('Int buf 5:  ', fi^:1, ' s/b 4');
   fi^ := 42; put(fi);
   reset(fi);
   writeln('Int buf eof1:  ', eof(fi):5, ' s/b false');
   writeln('Int buf rt:  ', fi^:1, ' s/b 42');
   get(fi);
   writeln('Int buf eof2:  ', eof(fi):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   lt := 99;
   intvalpar(lt);
   writeln('Int valpar rt:  ', lt:1, ' s/b 99');

   { var parameter (battery mutates the caller's lt in place) }
   intvarpar(lt);
   writeln('Int varpar rt:  ', lt:1, ' s/b 4');

   { surrounding-procedure local (inner routine works on this proc's lt) }
   intsurround;
   writeln('Int surround rt:  ', lt:1, ' s/b 4')

end;

procedure charcontexts;

type charr = array[1..5] of char;
     chrec = record
                f: char;
                a: charr
             end;
     chptr = ^char;
     chfil = file of char;

var lt:   char;
    la:   charr;
    lr:   chrec;
    lp:   chptr;
    lf:   chfil;

   procedure charbat_local;
   begin
      lt := 'm';
      writeln('Char local 1:  ', lt, ' s/b m');
      lt := succ(lt);
      writeln('Char local 2:  ', lt, ' s/b n');
      lt := pred(pred(lt));
      writeln('Char local 3:  ', lt, ' s/b l');
      writeln('Char local 4:  ', ord(lt):1, ' s/b 108');
      writeln('Char local 5:  ', chr(ord(lt)), ' ', (chr(ord(lt)) = lt):5, ' s/b l  true');
      writeln('Char local 6:  ', (lt = 'l'):5, ' ', (lt <> 'm'):5, ' s/b  true  true');
      writeln('Char local 7:  ', (lt < 'm'):5, ' ', (lt <= 'l'):5, ' s/b  true  true');
      writeln('Char local 8:  ', (lt > 'a'):5, ' ', (lt >= 'l'):5, ' s/b  true  true')
   end;

   procedure charvalpar(t: char); { value-parameter context }
   begin
      t := 'm';
      writeln('Char valpar 1:  ', t, ' s/b m');
      t := succ(t);
      writeln('Char valpar 2:  ', t, ' s/b n');
      t := pred(pred(t));
      writeln('Char valpar 3:  ', t, ' s/b l');
      writeln('Char valpar 4:  ', ord(t):1, ' s/b 108');
      writeln('Char valpar 5:  ', chr(ord(t)), ' ', (chr(ord(t)) = t):5, ' s/b l  true');
      writeln('Char valpar 6:  ', (t = 'l'):5, ' ', (t <> 'm'):5, ' s/b  true  true');
      writeln('Char valpar 7:  ', (t < 'm'):5, ' ', (t <= 'l'):5, ' s/b  true  true');
      writeln('Char valpar 8:  ', (t > 'a'):5, ' ', (t >= 'l'):5, ' s/b  true  true')
   end;

   procedure charvarpar(var t: char); { var-parameter context }
   begin
      t := 'm';
      writeln('Char varpar 1:  ', t, ' s/b m');
      t := succ(t);
      writeln('Char varpar 2:  ', t, ' s/b n');
      t := pred(pred(t));
      writeln('Char varpar 3:  ', t, ' s/b l');
      writeln('Char varpar 4:  ', ord(t):1, ' s/b 108');
      writeln('Char varpar 5:  ', chr(ord(t)), ' ', (chr(ord(t)) = t):5, ' s/b l  true');
      writeln('Char varpar 6:  ', (t = 'l'):5, ' ', (t <> 'm'):5, ' s/b  true  true');
      writeln('Char varpar 7:  ', (t < 'm'):5, ' ', (t <= 'l'):5, ' s/b  true  true');
      writeln('Char varpar 8:  ', (t > 'a'):5, ' ', (t >= 'l'):5, ' s/b  true  true')
   end;

   procedure charsurround; { surrounding-procedure-local context: works on lt }
   begin
      lt := 'm';
      writeln('Char surround 1:  ', lt, ' s/b m');
      lt := succ(lt);
      writeln('Char surround 2:  ', lt, ' s/b n');
      lt := pred(pred(lt));
      writeln('Char surround 3:  ', lt, ' s/b l');
      writeln('Char surround 4:  ', ord(lt):1, ' s/b 108');
      writeln('Char surround 5:  ', chr(ord(lt)), ' ', (chr(ord(lt)) = lt):5, ' s/b l  true');
      writeln('Char surround 6:  ', (lt = 'l'):5, ' ', (lt <> 'm'):5, ' s/b  true  true');
      writeln('Char surround 7:  ', (lt < 'm'):5, ' ', (lt <= 'l'):5, ' s/b  true  true');
      writeln('Char surround 8:  ', (lt > 'a'):5, ' ', (lt >= 'l'):5, ' s/b  true  true')
   end;

begin

   { local variable }
   charbat_local;

   { array element a[i] }
   la[3] := 'm';
   writeln('Char array 1:  ', la[3], ' s/b m');
   la[3] := succ(la[3]);
   writeln('Char array 2:  ', la[3], ' s/b n');
   la[3] := pred(pred(la[3]));
   writeln('Char array 3:  ', la[3], ' s/b l');
   writeln('Char array 4:  ', ord(la[3]):1, ' s/b 108');
   writeln('Char array 5:  ', chr(ord(la[3])), ' ', (chr(ord(la[3])) = la[3]):5, ' s/b l  true');
   writeln('Char array 6:  ', (la[3] = 'l'):5, ' ', (la[3] <> 'm'):5, ' s/b  true  true');
   writeln('Char array 7:  ', (la[3] < 'm'):5, ' ', (la[3] <= 'l'):5, ' s/b  true  true');
   writeln('Char array 8:  ', (la[3] > 'a'):5, ' ', (la[3] >= 'l'):5, ' s/b  true  true');

   { record field lr.f }
   lr.f := 'm';
   writeln('Char recfld 1:  ', lr.f, ' s/b m');
   lr.f := succ(lr.f);
   writeln('Char recfld 2:  ', lr.f, ' s/b n');
   lr.f := pred(pred(lr.f));
   writeln('Char recfld 3:  ', lr.f, ' s/b l');
   writeln('Char recfld 4:  ', ord(lr.f):1, ' s/b 108');
   writeln('Char recfld 5:  ', chr(ord(lr.f)), ' ', (chr(ord(lr.f)) = lr.f):5, ' s/b l  true');
   writeln('Char recfld 6:  ', (lr.f = 'l'):5, ' ', (lr.f <> 'm'):5, ' s/b  true  true');
   writeln('Char recfld 7:  ', (lr.f < 'm'):5, ' ', (lr.f <= 'l'):5, ' s/b  true  true');
   writeln('Char recfld 8:  ', (lr.f > 'a'):5, ' ', (lr.f >= 'l'):5, ' s/b  true  true');

   { nested: record's array field lr.a[i] }
   lr.a[3] := 'm';
   writeln('Char recarr 1:  ', lr.a[3], ' s/b m');
   lr.a[3] := succ(lr.a[3]);
   writeln('Char recarr 2:  ', lr.a[3], ' s/b n');
   lr.a[3] := pred(pred(lr.a[3]));
   writeln('Char recarr 3:  ', lr.a[3], ' s/b l');
   writeln('Char recarr 4:  ', ord(lr.a[3]):1, ' s/b 108');
   writeln('Char recarr 5:  ', chr(ord(lr.a[3])), ' ', (chr(ord(lr.a[3])) = lr.a[3]):5, ' s/b l  true');
   writeln('Char recarr 6:  ', (lr.a[3] = 'l'):5, ' ', (lr.a[3] <> 'm'):5, ' s/b  true  true');
   writeln('Char recarr 7:  ', (lr.a[3] < 'm'):5, ' ', (lr.a[3] <= 'l'):5, ' s/b  true  true');
   writeln('Char recarr 8:  ', (lr.a[3] > 'a'):5, ' ', (lr.a[3] >= 'l'):5, ' s/b  true  true');

   { pointer lp^ }
   new(lp);
   lp^ := 'm';
   writeln('Char ptr 1:  ', lp^, ' s/b m');
   lp^ := succ(lp^);
   writeln('Char ptr 2:  ', lp^, ' s/b n');
   lp^ := pred(pred(lp^));
   writeln('Char ptr 3:  ', lp^, ' s/b l');
   writeln('Char ptr 4:  ', ord(lp^):1, ' s/b 108');
   writeln('Char ptr 5:  ', chr(ord(lp^)), ' ', (chr(ord(lp^)) = lp^):5, ' s/b l  true');
   writeln('Char ptr 6:  ', (lp^ = 'l'):5, ' ', (lp^ <> 'm'):5, ' s/b  true  true');
   writeln('Char ptr 7:  ', (lp^ < 'm'):5, ' ', (lp^ <= 'l'):5, ' s/b  true  true');
   writeln('Char ptr 8:  ', (lp^ > 'a'):5, ' ', (lp^ >= 'l'):5, ' s/b  true  true');
   dispose(lp);

   { file buffer lf^: swept as an ordinary variable, then the file round-trip
     and buffer/eof status checks specific to its file nature }
   rewrite(lf);
   lf^ := 'm';
   writeln('Char buf 1:  ', lf^, ' s/b m');
   lf^ := succ(lf^);
   writeln('Char buf 2:  ', lf^, ' s/b n');
   lf^ := pred(pred(lf^));
   writeln('Char buf 3:  ', lf^, ' s/b l');
   writeln('Char buf 4:  ', ord(lf^):1, ' s/b 108');
   writeln('Char buf 5:  ', chr(ord(lf^)), ' ', (chr(ord(lf^)) = lf^):5, ' s/b l  true');
   writeln('Char buf 6:  ', (lf^ = 'l'):5, ' ', (lf^ <> 'm'):5, ' s/b  true  true');
   writeln('Char buf 7:  ', (lf^ < 'm'):5, ' ', (lf^ <= 'l'):5, ' s/b  true  true');
   writeln('Char buf 8:  ', (lf^ > 'a'):5, ' ', (lf^ >= 'l'):5, ' s/b  true  true');
   lf^ := 'z'; put(lf);
   reset(lf);
   writeln('Char buf eof1:  ', eof(lf):5, ' s/b false');
   writeln('Char buf rt:  ', lf^, ' s/b z');
   get(lf);
   writeln('Char buf eof2:  ', eof(lf):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   lt := 'q';
   charvalpar(lt);
   writeln('Char valpar rt:  ', lt, ' s/b q');

   { var parameter (battery mutates the caller's lt in place) }
   charvarpar(lt);
   writeln('Char varpar rt:  ', lt, ' s/b l');

   { surrounding-procedure local (inner routine works on this proc's lt) }
   lt := 'q';
   charsurround;
   writeln('Char surround rt:  ', lt, ' s/b l')

end;

procedure booleancontexts;

type btyp = boolean;
     barr = array[1..5] of btyp;
     brec = record
               f: btyp;
               a: barr
            end;
     bptr = ^btyp;
     bfil = file of btyp;

var lt:   btyp;
    a:    barr;
    r:    brec;
    p:    bptr;
    fi:   bfil;

   procedure boolbat_valpar(t: btyp); { value-parameter context }
   begin
      t := true;
      writeln('Boolean valpar 1:  ', t:5, ' s/b  true');
      t := t and false;
      writeln('Boolean valpar 2:  ', t:5, ' s/b false');
      t := t or true;
      writeln('Boolean valpar 3:  ', t:5, ' s/b  true');
      t := not t;
      writeln('Boolean valpar 4:  ', t:5, ' s/b false');
      writeln('Boolean valpar 5:  ', (false < true):5, ' ', (false <= true):5,
              ' ', (true > false):5, ' ', (true >= false):5,
              ' s/b  true  true  true  true');
      writeln('Boolean valpar 6:  ', (false = false):5, ' ', (true <> false):5,
              ' s/b  true  true');
      writeln('Boolean valpar 7:  ', succ(false):5, ' ', pred(true):5,
              ' s/b  true false');
      writeln('Boolean valpar 8:  ', ord(false):1, ' ', ord(true):1,
              ' s/b 0 1')
   end;

   procedure boolbat_varpar(var t: btyp); { var-parameter context }
   begin
      t := true;
      writeln('Boolean varpar 1:  ', t:5, ' s/b  true');
      t := t and false;
      writeln('Boolean varpar 2:  ', t:5, ' s/b false');
      t := t or true;
      writeln('Boolean varpar 3:  ', t:5, ' s/b  true');
      t := not t;
      writeln('Boolean varpar 4:  ', t:5, ' s/b false');
      writeln('Boolean varpar 5:  ', (false < true):5, ' ', (false <= true):5,
              ' ', (true > false):5, ' ', (true >= false):5,
              ' s/b  true  true  true  true');
      writeln('Boolean varpar 6:  ', (false = false):5, ' ', (true <> false):5,
              ' s/b  true  true');
      writeln('Boolean varpar 7:  ', succ(false):5, ' ', pred(true):5,
              ' s/b  true false');
      writeln('Boolean varpar 8:  ', ord(false):1, ' ', ord(true):1,
              ' s/b 0 1')
   end;

   procedure boolsurround; { surrounding-procedure-local context: works on lt }
   begin
      lt := true;
      writeln('Boolean surround 1:  ', lt:5, ' s/b  true');
      lt := lt and false;
      writeln('Boolean surround 2:  ', lt:5, ' s/b false');
      lt := lt or true;
      writeln('Boolean surround 3:  ', lt:5, ' s/b  true');
      lt := not lt;
      writeln('Boolean surround 4:  ', lt:5, ' s/b false');
      writeln('Boolean surround 5:  ', (false < true):5, ' ', (false <= true):5,
              ' ', (true > false):5, ' ', (true >= false):5,
              ' s/b  true  true  true  true');
      writeln('Boolean surround 6:  ', (false = false):5, ' ', (true <> false):5,
              ' s/b  true  true');
      writeln('Boolean surround 7:  ', succ(false):5, ' ', pred(true):5,
              ' s/b  true false');
      writeln('Boolean surround 8:  ', ord(false):1, ' ', ord(true):1,
              ' s/b 0 1')
   end;

begin

   { local variable }
   lt := true;
   writeln('Boolean local 1:  ', lt:5, ' s/b  true');
   lt := lt and false;
   writeln('Boolean local 2:  ', lt:5, ' s/b false');
   lt := lt or true;
   writeln('Boolean local 3:  ', lt:5, ' s/b  true');
   lt := not lt;
   writeln('Boolean local 4:  ', lt:5, ' s/b false');
   writeln('Boolean local 5:  ', (false < true):5, ' ', (false <= true):5,
           ' ', (true > false):5, ' ', (true >= false):5,
           ' s/b  true  true  true  true');
   writeln('Boolean local 6:  ', (false = false):5, ' ', (true <> false):5,
           ' s/b  true  true');
   writeln('Boolean local 7:  ', succ(false):5, ' ', pred(true):5,
           ' s/b  true false');
   writeln('Boolean local 8:  ', ord(false):1, ' ', ord(true):1, ' s/b 0 1');

   { array element a[i] }
   a[3] := true;
   writeln('Boolean array 1:  ', a[3]:5, ' s/b  true');
   a[3] := a[3] and false;
   writeln('Boolean array 2:  ', a[3]:5, ' s/b false');
   a[3] := a[3] or true;
   writeln('Boolean array 3:  ', a[3]:5, ' s/b  true');
   a[3] := not a[3];
   writeln('Boolean array 4:  ', a[3]:5, ' s/b false');
   writeln('Boolean array 5:  ', (false < true):5, ' ', (false <= true):5,
           ' ', (true > false):5, ' ', (true >= false):5,
           ' s/b  true  true  true  true');
   writeln('Boolean array 6:  ', (false = false):5, ' ', (true <> false):5,
           ' s/b  true  true');
   writeln('Boolean array 7:  ', succ(false):5, ' ', pred(true):5,
           ' s/b  true false');
   writeln('Boolean array 8:  ', ord(false):1, ' ', ord(true):1, ' s/b 0 1');

   { record field r.f }
   r.f := true;
   writeln('Boolean recfld 1:  ', r.f:5, ' s/b  true');
   r.f := r.f and false;
   writeln('Boolean recfld 2:  ', r.f:5, ' s/b false');
   r.f := r.f or true;
   writeln('Boolean recfld 3:  ', r.f:5, ' s/b  true');
   r.f := not r.f;
   writeln('Boolean recfld 4:  ', r.f:5, ' s/b false');
   writeln('Boolean recfld 5:  ', (false < true):5, ' ', (false <= true):5,
           ' ', (true > false):5, ' ', (true >= false):5,
           ' s/b  true  true  true  true');
   writeln('Boolean recfld 6:  ', (false = false):5, ' ', (true <> false):5,
           ' s/b  true  true');
   writeln('Boolean recfld 7:  ', succ(false):5, ' ', pred(true):5,
           ' s/b  true false');
   writeln('Boolean recfld 8:  ', ord(false):1, ' ', ord(true):1, ' s/b 0 1');

   { nested: record's array field r.a[i] }
   r.a[3] := true;
   writeln('Boolean recarr 1:  ', r.a[3]:5, ' s/b  true');
   r.a[3] := r.a[3] and false;
   writeln('Boolean recarr 2:  ', r.a[3]:5, ' s/b false');
   r.a[3] := r.a[3] or true;
   writeln('Boolean recarr 3:  ', r.a[3]:5, ' s/b  true');
   r.a[3] := not r.a[3];
   writeln('Boolean recarr 4:  ', r.a[3]:5, ' s/b false');
   writeln('Boolean recarr 5:  ', (false < true):5, ' ', (false <= true):5,
           ' ', (true > false):5, ' ', (true >= false):5,
           ' s/b  true  true  true  true');
   writeln('Boolean recarr 6:  ', (false = false):5, ' ', (true <> false):5,
           ' s/b  true  true');
   writeln('Boolean recarr 7:  ', succ(false):5, ' ', pred(true):5,
           ' s/b  true false');
   writeln('Boolean recarr 8:  ', ord(false):1, ' ', ord(true):1, ' s/b 0 1');

   { pointer p^ }
   new(p);
   p^ := true;
   writeln('Boolean ptr 1:  ', p^:5, ' s/b  true');
   p^ := p^ and false;
   writeln('Boolean ptr 2:  ', p^:5, ' s/b false');
   p^ := p^ or true;
   writeln('Boolean ptr 3:  ', p^:5, ' s/b  true');
   p^ := not p^;
   writeln('Boolean ptr 4:  ', p^:5, ' s/b false');
   writeln('Boolean ptr 5:  ', (false < true):5, ' ', (false <= true):5,
           ' ', (true > false):5, ' ', (true >= false):5,
           ' s/b  true  true  true  true');
   writeln('Boolean ptr 6:  ', (false = false):5, ' ', (true <> false):5,
           ' s/b  true  true');
   writeln('Boolean ptr 7:  ', succ(false):5, ' ', pred(true):5,
           ' s/b  true false');
   writeln('Boolean ptr 8:  ', ord(false):1, ' ', ord(true):1, ' s/b 0 1');
   dispose(p);

   { file buffer fi^: swept as an ordinary variable, then the file round-trip
     and buffer/eof status checks specific to its file nature }
   rewrite(fi);
   fi^ := true;
   writeln('Boolean buf 1:  ', fi^:5, ' s/b  true');
   fi^ := fi^ and false;
   writeln('Boolean buf 2:  ', fi^:5, ' s/b false');
   fi^ := fi^ or true;
   writeln('Boolean buf 3:  ', fi^:5, ' s/b  true');
   fi^ := not fi^;
   writeln('Boolean buf 4:  ', fi^:5, ' s/b false');
   fi^ := true; put(fi);
   reset(fi);
   writeln('Boolean buf eof1:  ', eof(fi):5, ' s/b false');
   writeln('Boolean buf rt:  ', fi^:5, ' s/b  true');
   get(fi);
   writeln('Boolean buf eof2:  ', eof(fi):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   lt := false;
   boolbat_valpar(lt);
   writeln('Boolean valpar rt:  ', lt:5, ' s/b false');

   { var parameter (battery mutates the caller's lt in place) }
   boolbat_varpar(lt);
   writeln('Boolean varpar rt:  ', lt:5, ' s/b false');

   { surrounding-procedure local (inner routine works on this proc's lt) }
   boolsurround;
   writeln('Boolean surround rt:  ', lt:5, ' s/b false')

end;

procedure enumcontexts;

type en = (one, two, three, four, five);
     enarr = array[1..5] of en;
     enrec = record f: en; a: enarr end;
     enptr = ^en;

var lt: en;
    la: enarr;
    lr: enrec;
    lp: enptr;
    lf: file of en;

   procedure enumvalpar(t: en);
   begin
   t := three;
   writeln('Enum valpar 1:  ', ord(t):1, ' s/b 2');
   t := succ(t);
   writeln('Enum valpar 2:  ', ord(t):1, ' s/b 3');
   t := pred(pred(t));
   writeln('Enum valpar 3:  ', ord(t):1, ' s/b 1');
   writeln('Enum valpar 4:  ', (t = two):5, ' ', (t <> three):5, ' s/b  true  true');
   writeln('Enum valpar 5:  ', (t < three):5, ' ', (t <= two):5, ' s/b  true  true');
   writeln('Enum valpar 6:  ', (t > one):5, ' ', (t >= two):5, ' s/b  true  true')
   end;

   procedure enumvarpar(var t: en);
   begin
   t := three;
   writeln('Enum varpar 1:  ', ord(t):1, ' s/b 2');
   t := succ(t);
   writeln('Enum varpar 2:  ', ord(t):1, ' s/b 3');
   t := pred(pred(t));
   writeln('Enum varpar 3:  ', ord(t):1, ' s/b 1');
   writeln('Enum varpar 4:  ', (t = two):5, ' ', (t <> three):5, ' s/b  true  true');
   writeln('Enum varpar 5:  ', (t < three):5, ' ', (t <= two):5, ' s/b  true  true');
   writeln('Enum varpar 6:  ', (t > one):5, ' ', (t >= two):5, ' s/b  true  true')
   end;

   procedure enumsurround;
   begin
   lt := three;
   writeln('Enum surround 1:  ', ord(lt):1, ' s/b 2');
   lt := succ(lt);
   writeln('Enum surround 2:  ', ord(lt):1, ' s/b 3');
   lt := pred(pred(lt));
   writeln('Enum surround 3:  ', ord(lt):1, ' s/b 1');
   writeln('Enum surround 4:  ', (lt = two):5, ' ', (lt <> three):5, ' s/b  true  true');
   writeln('Enum surround 5:  ', (lt < three):5, ' ', (lt <= two):5, ' s/b  true  true');
   writeln('Enum surround 6:  ', (lt > one):5, ' ', (lt >= two):5, ' s/b  true  true')
   end;

begin

   lt := three;
   writeln('Enum local 1:  ', ord(lt):1, ' s/b 2');
   lt := succ(lt);
   writeln('Enum local 2:  ', ord(lt):1, ' s/b 3');
   lt := pred(pred(lt));
   writeln('Enum local 3:  ', ord(lt):1, ' s/b 1');
   writeln('Enum local 4:  ', (lt = two):5, ' ', (lt <> three):5, ' s/b  true  true');
   writeln('Enum local 5:  ', (lt < three):5, ' ', (lt <= two):5, ' s/b  true  true');
   writeln('Enum local 6:  ', (lt > one):5, ' ', (lt >= two):5, ' s/b  true  true');

   la[3] := three;
   writeln('Enum array 1:  ', ord(la[3]):1, ' s/b 2');
   la[3] := succ(la[3]);
   writeln('Enum array 2:  ', ord(la[3]):1, ' s/b 3');
   la[3] := pred(pred(la[3]));
   writeln('Enum array 3:  ', ord(la[3]):1, ' s/b 1');
   writeln('Enum array 4:  ', (la[3] = two):5, ' ', (la[3] <> three):5, ' s/b  true  true');
   writeln('Enum array 5:  ', (la[3] < three):5, ' ', (la[3] <= two):5, ' s/b  true  true');
   writeln('Enum array 6:  ', (la[3] > one):5, ' ', (la[3] >= two):5, ' s/b  true  true');

   lr.f := three;
   writeln('Enum recfld 1:  ', ord(lr.f):1, ' s/b 2');
   lr.f := succ(lr.f);
   writeln('Enum recfld 2:  ', ord(lr.f):1, ' s/b 3');
   lr.f := pred(pred(lr.f));
   writeln('Enum recfld 3:  ', ord(lr.f):1, ' s/b 1');
   writeln('Enum recfld 4:  ', (lr.f = two):5, ' ', (lr.f <> three):5, ' s/b  true  true');
   writeln('Enum recfld 5:  ', (lr.f < three):5, ' ', (lr.f <= two):5, ' s/b  true  true');
   writeln('Enum recfld 6:  ', (lr.f > one):5, ' ', (lr.f >= two):5, ' s/b  true  true');

   lr.a[3] := three;
   writeln('Enum recarr 1:  ', ord(lr.a[3]):1, ' s/b 2');
   lr.a[3] := succ(lr.a[3]);
   writeln('Enum recarr 2:  ', ord(lr.a[3]):1, ' s/b 3');
   lr.a[3] := pred(pred(lr.a[3]));
   writeln('Enum recarr 3:  ', ord(lr.a[3]):1, ' s/b 1');
   writeln('Enum recarr 4:  ', (lr.a[3] = two):5, ' ', (lr.a[3] <> three):5, ' s/b  true  true');
   writeln('Enum recarr 5:  ', (lr.a[3] < three):5, ' ', (lr.a[3] <= two):5, ' s/b  true  true');
   writeln('Enum recarr 6:  ', (lr.a[3] > one):5, ' ', (lr.a[3] >= two):5, ' s/b  true  true');

   new(lp);
   lp^ := three;
   writeln('Enum ptr 1:  ', ord(lp^):1, ' s/b 2');
   lp^ := succ(lp^);
   writeln('Enum ptr 2:  ', ord(lp^):1, ' s/b 3');
   lp^ := pred(pred(lp^));
   writeln('Enum ptr 3:  ', ord(lp^):1, ' s/b 1');
   writeln('Enum ptr 4:  ', (lp^ = two):5, ' ', (lp^ <> three):5, ' s/b  true  true');
   writeln('Enum ptr 5:  ', (lp^ < three):5, ' ', (lp^ <= two):5, ' s/b  true  true');
   writeln('Enum ptr 6:  ', (lp^ > one):5, ' ', (lp^ >= two):5, ' s/b  true  true');
   dispose(lp);

   rewrite(lf);
   lf^ := three;
   writeln('Enum buf 1:  ', ord(lf^):1, ' s/b 2');
   lf^ := succ(lf^);
   writeln('Enum buf 2:  ', ord(lf^):1, ' s/b 3');
   lf^ := pred(pred(lf^));
   writeln('Enum buf 3:  ', ord(lf^):1, ' s/b 1');
   writeln('Enum buf 4:  ', (lf^ = two):5, ' ', (lf^ <> three):5, ' s/b  true  true');
   writeln('Enum buf 5:  ', (lf^ < three):5, ' ', (lf^ <= two):5, ' s/b  true  true');
   writeln('Enum buf 6:  ', (lf^ > one):5, ' ', (lf^ >= two):5, ' s/b  true  true');
   lf^ := five; put(lf);
   reset(lf);
   writeln('Enum buf eof1:  ', eof(lf):5, ' s/b false');
   writeln('Enum buf rt:  ', ord(lf^):1, ' ', (lf^ > four):5, ' s/b 4  true');
   get(lf);
   writeln('Enum buf eof2:  ', eof(lf):5, ' s/b  true');

   lt := one;
   enumvalpar(lt);
   writeln('Enum valpar rt:  ', ord(lt):1, ' s/b 0');

   enumvarpar(lt);
   writeln('Enum varpar rt:  ', ord(lt):1, ' s/b 1');

   lt := one;
   enumsurround;
   writeln('Enum surround rt:  ', ord(lt):1, ' s/b 1')

end;

procedure subrangecontexts;

type sr = 1..100;

var lt: sr;
    a: array[1..5] of sr;
    arec: record
            f: sr;
            a: array[1..5] of sr
          end;
    p: ^sr;
    fi: file of sr;

   procedure srvalpar(t: sr); { value-parameter context }
   begin
      t := 5;
      writeln('Subrange valpar 1:  ', t:1, ' s/b 5');
      t := t+7; t := t*3; t := t-1;
      writeln('Subrange valpar 2:  ', t:1, ' s/b 35');
      t := t div 4; t := t mod 6;
      writeln('Subrange valpar 3:  ', t:1, ' s/b 2');
      t := succ(t); t := succ(t); t := pred(t);
      writeln('Subrange valpar 4:  ', t:1, ' s/b 3');
      writeln('Subrange valpar 5:  ', (t = 3):5, ' ', (t > 2):5, ' ', (t < 3):5,
              ' s/b  true  true false')
   end;

   procedure srvarpar(var t: sr); { var-parameter context }
   begin
      t := 5;
      writeln('Subrange varpar 1:  ', t:1, ' s/b 5');
      t := t+7; t := t*3; t := t-1;
      writeln('Subrange varpar 2:  ', t:1, ' s/b 35');
      t := t div 4; t := t mod 6;
      writeln('Subrange varpar 3:  ', t:1, ' s/b 2');
      t := succ(t); t := succ(t); t := pred(t);
      writeln('Subrange varpar 4:  ', t:1, ' s/b 3');
      writeln('Subrange varpar 5:  ', (t = 3):5, ' ', (t > 2):5, ' ', (t < 3):5,
              ' s/b  true  true false')
   end;

   procedure srsurround; { surrounding-procedure-local context: works on lt }
   begin
      lt := 5;
      writeln('Subrange surround 1:  ', lt:1, ' s/b 5');
      lt := lt+7; lt := lt*3; lt := lt-1;
      writeln('Subrange surround 2:  ', lt:1, ' s/b 35');
      lt := lt div 4; lt := lt mod 6;
      writeln('Subrange surround 3:  ', lt:1, ' s/b 2');
      lt := succ(lt); lt := succ(lt); lt := pred(lt);
      writeln('Subrange surround 4:  ', lt:1, ' s/b 3');
      writeln('Subrange surround 5:  ', (lt = 3):5, ' ', (lt > 2):5, ' ', (lt < 3):5,
              ' s/b  true  true false')
   end;

begin

   { local variable }
   lt := 5;
   writeln('Subrange local 1:  ', lt:1, ' s/b 5');
   lt := lt+7; lt := lt*3; lt := lt-1;
   writeln('Subrange local 2:  ', lt:1, ' s/b 35');
   lt := lt div 4; lt := lt mod 6;
   writeln('Subrange local 3:  ', lt:1, ' s/b 2');
   lt := succ(lt); lt := succ(lt); lt := pred(lt);
   writeln('Subrange local 4:  ', lt:1, ' s/b 3');
   writeln('Subrange local 5:  ', (lt = 3):5, ' ', (lt > 2):5, ' ', (lt < 3):5,
           ' s/b  true  true false');

   { array element a[i] }
   a[3] := 5;
   writeln('Subrange array 1:  ', a[3]:1, ' s/b 5');
   a[3] := a[3]+7; a[3] := a[3]*3; a[3] := a[3]-1;
   writeln('Subrange array 2:  ', a[3]:1, ' s/b 35');
   a[3] := a[3] div 4; a[3] := a[3] mod 6;
   writeln('Subrange array 3:  ', a[3]:1, ' s/b 2');
   a[3] := succ(a[3]); a[3] := succ(a[3]); a[3] := pred(a[3]);
   writeln('Subrange array 4:  ', a[3]:1, ' s/b 3');
   writeln('Subrange array 5:  ', (a[3] = 3):5, ' ', (a[3] > 2):5, ' ', (a[3] < 3):5,
           ' s/b  true  true false');

   { record field arec.f }
   arec.f := 5;
   writeln('Subrange recfld 1:  ', arec.f:1, ' s/b 5');
   arec.f := arec.f+7; arec.f := arec.f*3; arec.f := arec.f-1;
   writeln('Subrange recfld 2:  ', arec.f:1, ' s/b 35');
   arec.f := arec.f div 4; arec.f := arec.f mod 6;
   writeln('Subrange recfld 3:  ', arec.f:1, ' s/b 2');
   arec.f := succ(arec.f); arec.f := succ(arec.f); arec.f := pred(arec.f);
   writeln('Subrange recfld 4:  ', arec.f:1, ' s/b 3');
   writeln('Subrange recfld 5:  ', (arec.f = 3):5, ' ', (arec.f > 2):5, ' ', (arec.f < 3):5,
           ' s/b  true  true false');

   { nested: record's array field arec.a[i] }
   arec.a[3] := 5;
   writeln('Subrange recarr 1:  ', arec.a[3]:1, ' s/b 5');
   arec.a[3] := arec.a[3]+7; arec.a[3] := arec.a[3]*3; arec.a[3] := arec.a[3]-1;
   writeln('Subrange recarr 2:  ', arec.a[3]:1, ' s/b 35');
   arec.a[3] := arec.a[3] div 4; arec.a[3] := arec.a[3] mod 6;
   writeln('Subrange recarr 3:  ', arec.a[3]:1, ' s/b 2');
   arec.a[3] := succ(arec.a[3]); arec.a[3] := succ(arec.a[3]); arec.a[3] := pred(arec.a[3]);
   writeln('Subrange recarr 4:  ', arec.a[3]:1, ' s/b 3');
   writeln('Subrange recarr 5:  ', (arec.a[3] = 3):5, ' ', (arec.a[3] > 2):5, ' ', (arec.a[3] < 3):5,
           ' s/b  true  true false');

   { pointer p^ }
   new(p);
   p^ := 5;
   writeln('Subrange ptr 1:  ', p^:1, ' s/b 5');
   p^ := p^+7; p^ := p^*3; p^ := p^-1;
   writeln('Subrange ptr 2:  ', p^:1, ' s/b 35');
   p^ := p^ div 4; p^ := p^ mod 6;
   writeln('Subrange ptr 3:  ', p^:1, ' s/b 2');
   p^ := succ(p^); p^ := succ(p^); p^ := pred(p^);
   writeln('Subrange ptr 4:  ', p^:1, ' s/b 3');
   writeln('Subrange ptr 5:  ', (p^ = 3):5, ' ', (p^ > 2):5, ' ', (p^ < 3):5,
           ' s/b  true  true false');
   dispose(p);

   { file buffer fi^: swept as an ordinary variable, then the file round-trip
     and buffer/eof status checks specific to its file nature }
   rewrite(fi);
   fi^ := 5;
   writeln('Subrange buf 1:  ', fi^:1, ' s/b 5');
   fi^ := fi^+7; fi^ := fi^*3; fi^ := fi^-1;
   writeln('Subrange buf 2:  ', fi^:1, ' s/b 35');
   fi^ := fi^ div 4; fi^ := fi^ mod 6;
   writeln('Subrange buf 3:  ', fi^:1, ' s/b 2');
   fi^ := succ(fi^); fi^ := succ(fi^); fi^ := pred(fi^);
   writeln('Subrange buf 4:  ', fi^:1, ' s/b 3');
   fi^ := 42; put(fi);
   reset(fi);
   writeln('Subrange buf eof1:  ', eof(fi):5, ' s/b false');
   writeln('Subrange buf rt:  ', fi^:1, ' s/b 42');
   get(fi);
   writeln('Subrange buf eof2:  ', eof(fi):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   lt := 99;
   srvalpar(lt);
   writeln('Subrange valpar rt:  ', lt:1, ' s/b 99');

   { var parameter (battery mutates the caller's lt in place) }
   srvarpar(lt);
   writeln('Subrange varpar rt:  ', lt:1, ' s/b 3');

   { surrounding-procedure local (inner routine works on this proc's lt) }
   srsurround;
   writeln('Subrange surround rt:  ', lt:1, ' s/b 3')

end;

procedure realcontexts;

type rec  = record f: real; a: array [1..5] of real end;
     rp   = ^real;
     rfil = file of real;

var lt:   real;
    a:    array [1..5] of real;
    arec: rec;
    p:    rp;
    f:    rfil;

   procedure realvalpar(t: real); { value-parameter context }
   begin
      t := 16.0;
      writeln('Real valpar 1:  ', t:15, ' s/b  1.600000e+01');
      t := sqrt(t);
      writeln('Real valpar 2:  ', t:15, ' s/b  4.000000e+00');
      t := sqr(t);
      writeln('Real valpar 3:  ', t:15, ' s/b  1.600000e+01');
      t := t-12.0; t := t*2.0; t := t/4.0; t := t+1.0;
      writeln('Real valpar 4:  ', t:15, ' s/b  3.000000e+00');
      writeln('Real valpar 5:  ', abs(-t):15, ' s/b  3.000000e+00');
      writeln('Real valpar 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
      writeln('Real valpar 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
      writeln('Real valpar 8:  ', (t = 3.0):5, ' ', (t < 4.0):5, ' ', (t > 4.0):5, ' ', (t <> 4.0):5, ' s/b  true  true false  true');
      writeln('Real valpar 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4')
   end;

   procedure realvarpar(var t: real); { var-parameter context }
   begin
      t := 16.0;
      writeln('Real varpar 1:  ', t:15, ' s/b  1.600000e+01');
      t := sqrt(t);
      writeln('Real varpar 2:  ', t:15, ' s/b  4.000000e+00');
      t := sqr(t);
      writeln('Real varpar 3:  ', t:15, ' s/b  1.600000e+01');
      t := t-12.0; t := t*2.0; t := t/4.0; t := t+1.0;
      writeln('Real varpar 4:  ', t:15, ' s/b  3.000000e+00');
      writeln('Real varpar 5:  ', abs(-t):15, ' s/b  3.000000e+00');
      writeln('Real varpar 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
      writeln('Real varpar 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
      writeln('Real varpar 8:  ', (t = 3.0):5, ' ', (t < 4.0):5, ' ', (t > 4.0):5, ' ', (t <> 4.0):5, ' s/b  true  true false  true');
      writeln('Real varpar 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4')
   end;

   procedure realsurround; { surrounding-procedure-local context: works on lt }
   begin
      lt := 16.0;
      writeln('Real surround 1:  ', lt:15, ' s/b  1.600000e+01');
      lt := sqrt(lt);
      writeln('Real surround 2:  ', lt:15, ' s/b  4.000000e+00');
      lt := sqr(lt);
      writeln('Real surround 3:  ', lt:15, ' s/b  1.600000e+01');
      lt := lt-12.0; lt := lt*2.0; lt := lt/4.0; lt := lt+1.0;
      writeln('Real surround 4:  ', lt:15, ' s/b  3.000000e+00');
      writeln('Real surround 5:  ', abs(-lt):15, ' s/b  3.000000e+00');
      writeln('Real surround 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
      writeln('Real surround 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
      writeln('Real surround 8:  ', (lt = 3.0):5, ' ', (lt < 4.0):5, ' ', (lt > 4.0):5, ' ', (lt <> 4.0):5, ' s/b  true  true false  true');
      writeln('Real surround 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4')
   end;

begin

   { local variable }
   lt := 16.0;
   writeln('Real local 1:  ', lt:15, ' s/b  1.600000e+01');
   lt := sqrt(lt);
   writeln('Real local 2:  ', lt:15, ' s/b  4.000000e+00');
   lt := sqr(lt);
   writeln('Real local 3:  ', lt:15, ' s/b  1.600000e+01');
   lt := lt-12.0; lt := lt*2.0; lt := lt/4.0; lt := lt+1.0;
   writeln('Real local 4:  ', lt:15, ' s/b  3.000000e+00');
   writeln('Real local 5:  ', abs(-lt):15, ' s/b  3.000000e+00');
   writeln('Real local 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
   writeln('Real local 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
   writeln('Real local 8:  ', (lt = 3.0):5, ' ', (lt < 4.0):5, ' ', (lt > 4.0):5, ' ', (lt <> 4.0):5, ' s/b  true  true false  true');
   writeln('Real local 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4');

   { array element a[i] }
   a[3] := 16.0;
   writeln('Real array 1:  ', a[3]:15, ' s/b  1.600000e+01');
   a[3] := sqrt(a[3]);
   writeln('Real array 2:  ', a[3]:15, ' s/b  4.000000e+00');
   a[3] := sqr(a[3]);
   writeln('Real array 3:  ', a[3]:15, ' s/b  1.600000e+01');
   a[3] := a[3]-12.0; a[3] := a[3]*2.0; a[3] := a[3]/4.0; a[3] := a[3]+1.0;
   writeln('Real array 4:  ', a[3]:15, ' s/b  3.000000e+00');
   writeln('Real array 5:  ', abs(-a[3]):15, ' s/b  3.000000e+00');
   writeln('Real array 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
   writeln('Real array 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
   writeln('Real array 8:  ', (a[3] = 3.0):5, ' ', (a[3] < 4.0):5, ' ', (a[3] > 4.0):5, ' ', (a[3] <> 4.0):5, ' s/b  true  true false  true');
   writeln('Real array 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4');

   { record field arec.f }
   arec.f := 16.0;
   writeln('Real recfld 1:  ', arec.f:15, ' s/b  1.600000e+01');
   arec.f := sqrt(arec.f);
   writeln('Real recfld 2:  ', arec.f:15, ' s/b  4.000000e+00');
   arec.f := sqr(arec.f);
   writeln('Real recfld 3:  ', arec.f:15, ' s/b  1.600000e+01');
   arec.f := arec.f-12.0; arec.f := arec.f*2.0; arec.f := arec.f/4.0; arec.f := arec.f+1.0;
   writeln('Real recfld 4:  ', arec.f:15, ' s/b  3.000000e+00');
   writeln('Real recfld 5:  ', abs(-arec.f):15, ' s/b  3.000000e+00');
   writeln('Real recfld 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
   writeln('Real recfld 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
   writeln('Real recfld 8:  ', (arec.f = 3.0):5, ' ', (arec.f < 4.0):5, ' ', (arec.f > 4.0):5, ' ', (arec.f <> 4.0):5, ' s/b  true  true false  true');
   writeln('Real recfld 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4');

   { nested: record's array field arec.a[i] }
   arec.a[3] := 16.0;
   writeln('Real recarr 1:  ', arec.a[3]:15, ' s/b  1.600000e+01');
   arec.a[3] := sqrt(arec.a[3]);
   writeln('Real recarr 2:  ', arec.a[3]:15, ' s/b  4.000000e+00');
   arec.a[3] := sqr(arec.a[3]);
   writeln('Real recarr 3:  ', arec.a[3]:15, ' s/b  1.600000e+01');
   arec.a[3] := arec.a[3]-12.0; arec.a[3] := arec.a[3]*2.0; arec.a[3] := arec.a[3]/4.0; arec.a[3] := arec.a[3]+1.0;
   writeln('Real recarr 4:  ', arec.a[3]:15, ' s/b  3.000000e+00');
   writeln('Real recarr 5:  ', abs(-arec.a[3]):15, ' s/b  3.000000e+00');
   writeln('Real recarr 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
   writeln('Real recarr 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
   writeln('Real recarr 8:  ', (arec.a[3] = 3.0):5, ' ', (arec.a[3] < 4.0):5, ' ', (arec.a[3] > 4.0):5, ' ', (arec.a[3] <> 4.0):5, ' s/b  true  true false  true');
   writeln('Real recarr 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4');

   { pointer p^ }
   new(p);
   p^ := 16.0;
   writeln('Real ptr 1:  ', p^:15, ' s/b  1.600000e+01');
   p^ := sqrt(p^);
   writeln('Real ptr 2:  ', p^:15, ' s/b  4.000000e+00');
   p^ := sqr(p^);
   writeln('Real ptr 3:  ', p^:15, ' s/b  1.600000e+01');
   p^ := p^-12.0; p^ := p^*2.0; p^ := p^/4.0; p^ := p^+1.0;
   writeln('Real ptr 4:  ', p^:15, ' s/b  3.000000e+00');
   writeln('Real ptr 5:  ', abs(-p^):15, ' s/b  3.000000e+00');
   writeln('Real ptr 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
   writeln('Real ptr 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
   writeln('Real ptr 8:  ', (p^ = 3.0):5, ' ', (p^ < 4.0):5, ' ', (p^ > 4.0):5, ' ', (p^ <> 4.0):5, ' s/b  true  true false  true');
   writeln('Real ptr 9:  ', trunc(3.7):1, ' ', round(3.5):1, ' s/b 3 4');
   dispose(p);

   { file buffer f^: swept as an ordinary variable, then the file round-trip }
   rewrite(f);
   f^ := 16.0;
   writeln('Real buf 1:  ', f^:15, ' s/b  1.600000e+01');
   f^ := sqrt(f^);
   writeln('Real buf 2:  ', f^:15, ' s/b  4.000000e+00');
   f^ := sqr(f^);
   writeln('Real buf 3:  ', f^:15, ' s/b  1.600000e+01');
   f^ := f^-12.0; f^ := f^*2.0; f^ := f^/4.0; f^ := f^+1.0;
   writeln('Real buf 4:  ', f^:15, ' s/b  3.000000e+00');
   writeln('Real buf 5:  ', abs(-f^):15, ' s/b  3.000000e+00');
   writeln('Real buf 6:  ', exp(0.0):15, ' ', ln(1.0):15, ' s/b  1.000000e+00  0.000000e+00');
   writeln('Real buf 7:  ', sin(0.0):15, ' ', cos(0.0):15, ' ', arctan(0.0):15, ' s/b  0.000000e+00  1.000000e+00  0.000000e+00');
   f^ := 42.0; put(f);
   reset(f);
   writeln('Real buf eof1:  ', eof(f):5, ' s/b false');
   writeln('Real buf rt:  ', f^:15, ' s/b  4.200000e+01');
   get(f);
   writeln('Real buf eof2:  ', eof(f):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   lt := 99.0;
   realvalpar(lt);
   writeln('Real valpar rt:  ', lt:15, ' s/b  9.900000e+01');

   { var parameter (battery mutates the caller's lt in place) }
   realvarpar(lt);
   writeln('Real varpar rt:  ', lt:15, ' s/b  3.000000e+00');

   { surrounding-procedure local (inner routine works on this proc's lt) }
   realsurround;
   writeln('Real surround rt:  ', lt:15, ' s/b  3.000000e+00')

end;

procedure setcontexts;

type cs = set of 'a'..'z';

var lt: cs;
    a: array[1..5] of cs;
    arec: record
             f: cs;
             a: array[1..5] of cs
          end;
    sp: ^cs;
    fi: file of cs;
    b1, b2, b3: boolean;

   procedure setvalpar(t: cs); { value-parameter context }
   var b1, b2: boolean;
   begin
      t := ['a'..'e'];
      b1 := 'c' in t; b2 := 'g' in t;
      writeln('Set valpar 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
      b1 := 'g' in (t + ['f'..'h']); b2 := 'b' in (t + ['f'..'h']);
      writeln('Set valpar 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
      b1 := 'a' in (t - ['a'..'b']); b2 := 'c' in (t - ['a'..'b']);
      writeln('Set valpar 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
      b1 := 'c' in (t * ['d'..'z']); b2 := 'e' in (t * ['d'..'z']);
      writeln('Set valpar 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
      b1 := ['a'..'c'] <= t; b2 := t >= ['a'..'c'];
      writeln('Set valpar 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
      b1 := t = ['a'..'e']; b2 := t <> ['a'..'e'];
      writeln('Set valpar 6:  ', b1:5, ' ', b2:5, ' s/b  true false')
   end;

   procedure setvarpar(var t: cs); { var-parameter context }
   var b1, b2: boolean;
   begin
      t := ['a'..'e'];
      b1 := 'c' in t; b2 := 'g' in t;
      writeln('Set varpar 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
      b1 := 'g' in (t + ['f'..'h']); b2 := 'b' in (t + ['f'..'h']);
      writeln('Set varpar 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
      b1 := 'a' in (t - ['a'..'b']); b2 := 'c' in (t - ['a'..'b']);
      writeln('Set varpar 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
      b1 := 'c' in (t * ['d'..'z']); b2 := 'e' in (t * ['d'..'z']);
      writeln('Set varpar 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
      b1 := ['a'..'c'] <= t; b2 := t >= ['a'..'c'];
      writeln('Set varpar 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
      b1 := t = ['a'..'e']; b2 := t <> ['a'..'e'];
      writeln('Set varpar 6:  ', b1:5, ' ', b2:5, ' s/b  true false');
      { mutate caller's variable so var-propagation is observable }
      t := ['x'..'z']
   end;

   procedure setsurround; { surrounding-procedure-local context: works on lt }
   var b1, b2: boolean;
   begin
      lt := ['a'..'e'];
      b1 := 'c' in lt; b2 := 'g' in lt;
      writeln('Set surround 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
      b1 := 'g' in (lt + ['f'..'h']); b2 := 'b' in (lt + ['f'..'h']);
      writeln('Set surround 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
      b1 := 'a' in (lt - ['a'..'b']); b2 := 'c' in (lt - ['a'..'b']);
      writeln('Set surround 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
      b1 := 'c' in (lt * ['d'..'z']); b2 := 'e' in (lt * ['d'..'z']);
      writeln('Set surround 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
      b1 := ['a'..'c'] <= lt; b2 := lt >= ['a'..'c'];
      writeln('Set surround 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
      b1 := lt = ['a'..'e']; b2 := lt <> ['a'..'e'];
      writeln('Set surround 6:  ', b1:5, ' ', b2:5, ' s/b  true false');
      lt := ['x'..'z']
   end;

begin

   { local variable }
   lt := ['a'..'e'];
   b1 := 'c' in lt; b2 := 'g' in lt;
   writeln('Set local 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
   b1 := 'g' in (lt + ['f'..'h']); b2 := 'b' in (lt + ['f'..'h']);
   writeln('Set local 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := 'a' in (lt - ['a'..'b']); b2 := 'c' in (lt - ['a'..'b']);
   writeln('Set local 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := 'c' in (lt * ['d'..'z']); b2 := 'e' in (lt * ['d'..'z']);
   writeln('Set local 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := ['a'..'c'] <= lt; b2 := lt >= ['a'..'c'];
   writeln('Set local 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := lt = ['a'..'e']; b2 := lt <> ['a'..'e'];
   writeln('Set local 6:  ', b1:5, ' ', b2:5, ' s/b  true false');

   { array element a[i] }
   a[3] := ['a'..'e'];
   b1 := 'c' in a[3]; b2 := 'g' in a[3];
   writeln('Set array 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
   b1 := 'g' in (a[3] + ['f'..'h']); b2 := 'b' in (a[3] + ['f'..'h']);
   writeln('Set array 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := 'a' in (a[3] - ['a'..'b']); b2 := 'c' in (a[3] - ['a'..'b']);
   writeln('Set array 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := 'c' in (a[3] * ['d'..'z']); b2 := 'e' in (a[3] * ['d'..'z']);
   writeln('Set array 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := ['a'..'c'] <= a[3]; b2 := a[3] >= ['a'..'c'];
   writeln('Set array 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := a[3] = ['a'..'e']; b2 := a[3] <> ['a'..'e'];
   writeln('Set array 6:  ', b1:5, ' ', b2:5, ' s/b  true false');

   { record field arec.f }
   arec.f := ['a'..'e'];
   b1 := 'c' in arec.f; b2 := 'g' in arec.f;
   writeln('Set recfld 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
   b1 := 'g' in (arec.f + ['f'..'h']); b2 := 'b' in (arec.f + ['f'..'h']);
   writeln('Set recfld 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := 'a' in (arec.f - ['a'..'b']); b2 := 'c' in (arec.f - ['a'..'b']);
   writeln('Set recfld 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := 'c' in (arec.f * ['d'..'z']); b2 := 'e' in (arec.f * ['d'..'z']);
   writeln('Set recfld 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := ['a'..'c'] <= arec.f; b2 := arec.f >= ['a'..'c'];
   writeln('Set recfld 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := arec.f = ['a'..'e']; b2 := arec.f <> ['a'..'e'];
   writeln('Set recfld 6:  ', b1:5, ' ', b2:5, ' s/b  true false');

   { nested: record's array field arec.a[i] }
   arec.a[3] := ['a'..'e'];
   b1 := 'c' in arec.a[3]; b2 := 'g' in arec.a[3];
   writeln('Set recarr 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
   b1 := 'g' in (arec.a[3] + ['f'..'h']); b2 := 'b' in (arec.a[3] + ['f'..'h']);
   writeln('Set recarr 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := 'a' in (arec.a[3] - ['a'..'b']); b2 := 'c' in (arec.a[3] - ['a'..'b']);
   writeln('Set recarr 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := 'c' in (arec.a[3] * ['d'..'z']); b2 := 'e' in (arec.a[3] * ['d'..'z']);
   writeln('Set recarr 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := ['a'..'c'] <= arec.a[3]; b2 := arec.a[3] >= ['a'..'c'];
   writeln('Set recarr 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := arec.a[3] = ['a'..'e']; b2 := arec.a[3] <> ['a'..'e'];
   writeln('Set recarr 6:  ', b1:5, ' ', b2:5, ' s/b  true false');

   { pointer sp^ }
   new(sp);
   sp^ := ['a'..'e'];
   b1 := 'c' in sp^; b2 := 'g' in sp^;
   writeln('Set ptr 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
   b1 := 'g' in (sp^ + ['f'..'h']); b2 := 'b' in (sp^ + ['f'..'h']);
   writeln('Set ptr 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := 'a' in (sp^ - ['a'..'b']); b2 := 'c' in (sp^ - ['a'..'b']);
   writeln('Set ptr 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := 'c' in (sp^ * ['d'..'z']); b2 := 'e' in (sp^ * ['d'..'z']);
   writeln('Set ptr 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := ['a'..'c'] <= sp^; b2 := sp^ >= ['a'..'c'];
   writeln('Set ptr 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := sp^ = ['a'..'e']; b2 := sp^ <> ['a'..'e'];
   writeln('Set ptr 6:  ', b1:5, ' ', b2:5, ' s/b  true false');
   dispose(sp);

   { file buffer fi^: swept as an ordinary variable, then the file round-trip
     and buffer/eof status checks specific to its file nature }
   rewrite(fi);
   fi^ := ['a'..'e'];
   b1 := 'c' in fi^; b2 := 'g' in fi^;
   writeln('Set buf 1:  ', b1:5, ' ', b2:5, ' s/b  true false');
   b1 := 'g' in (fi^ + ['f'..'h']); b2 := 'b' in (fi^ + ['f'..'h']);
   writeln('Set buf 2:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := 'a' in (fi^ - ['a'..'b']); b2 := 'c' in (fi^ - ['a'..'b']);
   writeln('Set buf 3:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := 'c' in (fi^ * ['d'..'z']); b2 := 'e' in (fi^ * ['d'..'z']);
   writeln('Set buf 4:  ', b1:5, ' ', b2:5, ' s/b false  true');
   b1 := ['a'..'c'] <= fi^; b2 := fi^ >= ['a'..'c'];
   writeln('Set buf 5:  ', b1:5, ' ', b2:5, ' s/b  true  true');
   b1 := fi^ = ['a'..'e']; b2 := fi^ <> ['a'..'e'];
   writeln('Set buf 6:  ', b1:5, ' ', b2:5, ' s/b  true false');
   fi^ := ['p'..'t']; put(fi);
   reset(fi);
   writeln('Set buf eof1:  ', eof(fi):5, ' s/b false');
   b1 := 'r' in fi^; b2 := 'a' in fi^; b3 := (fi^ = ['p'..'t']);
   writeln('Set buf rt:  ', b1:5, ' ', b2:5, ' ', b3:5, ' s/b  true false  true');
   get(fi);
   writeln('Set buf eof2:  ', eof(fi):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   lt := ['m'..'p'];
   setvalpar(lt);
   b1 := 'n' in lt; b2 := 'a' in lt; b3 := (lt = ['m'..'p']);
   writeln('Set valpar rt:  ', b1:5, ' ', b2:5, ' ', b3:5, ' s/b  true false  true');

   { var parameter (battery mutates the caller's lt in place -> ['x'..'z']) }
   setvarpar(lt);
   b1 := 'y' in lt; b2 := 'a' in lt; b3 := (lt = ['x'..'z']);
   writeln('Set varpar rt:  ', b1:5, ' ', b2:5, ' ', b3:5, ' s/b  true false  true');

   { surrounding-procedure local (inner routine works on this proc's lt -> ['x'..'z']) }
   setsurround;
   b1 := 'y' in lt; b2 := 'a' in lt; b3 := (lt = ['x'..'z']);
   writeln('Set surround rt:  ', b1:5, ' ', b2:5, ' ', b3:5, ' s/b  true false  true')

end;

procedure stringcontexts;

type s10 = packed array [1..10] of char;
     sptr = ^s10;
     srec = record
               f: s10;
               a: array [1..5] of s10
            end;

var lt:   s10;
    a:    array [1..5] of s10;
    arec: srec;
    sp:   sptr;
    fs:   file of s10;
    i:    integer;

   procedure strvalpar(t: s10); { value-parameter context }
   begin
      t := 'abcdefghij';
      writeln('String valpar 1:  ', t, ' s/b abcdefghij');
      writeln('String valpar 2:  ', (t = 'abcdefghij'):5, ' ',
              (t <> 'abcdefghij'):5, ' s/b  true false');
      writeln('String valpar 3:  ', (t < 'abcdefghix'):5, ' ',
              (t <= 'abcdefghij'):5, ' s/b  true  true');
      writeln('String valpar 4:  ', (t > 'abcdefghij'):5, ' ',
              (t >= 'abcdefghij'):5, ' s/b false  true');
      writeln('String valpar 5:  ', t[3], ' s/b c');
      t[3] := 'C';
      writeln('String valpar 6:  ', t, ' s/b abCdefghij')
   end;

   procedure strvarpar(var t: s10); { var-parameter context }
   begin
      t := 'abcdefghij';
      writeln('String varpar 1:  ', t, ' s/b abcdefghij');
      writeln('String varpar 2:  ', (t = 'abcdefghij'):5, ' ',
              (t <> 'abcdefghij'):5, ' s/b  true false');
      writeln('String varpar 3:  ', (t < 'abcdefghix'):5, ' ',
              (t <= 'abcdefghij'):5, ' s/b  true  true');
      writeln('String varpar 4:  ', (t > 'abcdefghij'):5, ' ',
              (t >= 'abcdefghij'):5, ' s/b false  true');
      writeln('String varpar 5:  ', t[3], ' s/b c');
      t[3] := 'C';
      writeln('String varpar 6:  ', t, ' s/b abCdefghij')
   end;

   procedure strsurround; { surrounding-procedure-local context: works on lt }
   begin
      lt := 'abcdefghij';
      writeln('String surround 1:  ', lt, ' s/b abcdefghij');
      writeln('String surround 2:  ', (lt = 'abcdefghij'):5, ' ',
              (lt <> 'abcdefghij'):5, ' s/b  true false');
      writeln('String surround 3:  ', (lt < 'abcdefghix'):5, ' ',
              (lt <= 'abcdefghij'):5, ' s/b  true  true');
      writeln('String surround 4:  ', (lt > 'abcdefghij'):5, ' ',
              (lt >= 'abcdefghij'):5, ' s/b false  true');
      writeln('String surround 5:  ', lt[3], ' s/b c');
      lt[3] := 'C';
      writeln('String surround 6:  ', lt, ' s/b abCdefghij')
   end;

begin

   { local variable }
   lt := 'abcdefghij';
   writeln('String local 1:  ', lt, ' s/b abcdefghij');
   writeln('String local 2:  ', (lt = 'abcdefghij'):5, ' ',
           (lt <> 'abcdefghij'):5, ' s/b  true false');
   writeln('String local 3:  ', (lt < 'abcdefghix'):5, ' ',
           (lt <= 'abcdefghij'):5, ' s/b  true  true');
   writeln('String local 4:  ', (lt > 'abcdefghij'):5, ' ',
           (lt >= 'abcdefghij'):5, ' s/b false  true');
   writeln('String local 5:  ', lt[3], ' s/b c');
   lt[3] := 'C';
   writeln('String local 6:  ', lt, ' s/b abCdefghij');

   { array element a[i] }
   a[3] := 'abcdefghij';
   writeln('String array 1:  ', a[3], ' s/b abcdefghij');
   writeln('String array 2:  ', (a[3] = 'abcdefghij'):5, ' ',
           (a[3] <> 'abcdefghij'):5, ' s/b  true false');
   writeln('String array 3:  ', (a[3] < 'abcdefghix'):5, ' ',
           (a[3] <= 'abcdefghij'):5, ' s/b  true  true');
   writeln('String array 4:  ', (a[3] > 'abcdefghij'):5, ' ',
           (a[3] >= 'abcdefghij'):5, ' s/b false  true');
   writeln('String array 5:  ', a[3][3], ' s/b c');
   a[3][3] := 'C';
   writeln('String array 6:  ', a[3], ' s/b abCdefghij');

   { record field arec.f }
   arec.f := 'abcdefghij';
   writeln('String recfld 1:  ', arec.f, ' s/b abcdefghij');
   writeln('String recfld 2:  ', (arec.f = 'abcdefghij'):5, ' ',
           (arec.f <> 'abcdefghij'):5, ' s/b  true false');
   writeln('String recfld 3:  ', (arec.f < 'abcdefghix'):5, ' ',
           (arec.f <= 'abcdefghij'):5, ' s/b  true  true');
   writeln('String recfld 4:  ', (arec.f > 'abcdefghij'):5, ' ',
           (arec.f >= 'abcdefghij'):5, ' s/b false  true');
   writeln('String recfld 5:  ', arec.f[3], ' s/b c');
   arec.f[3] := 'C';
   writeln('String recfld 6:  ', arec.f, ' s/b abCdefghij');

   { nested: record's array field arec.a[i] }
   arec.a[3] := 'abcdefghij';
   writeln('String recarr 1:  ', arec.a[3], ' s/b abcdefghij');
   writeln('String recarr 2:  ', (arec.a[3] = 'abcdefghij'):5, ' ',
           (arec.a[3] <> 'abcdefghij'):5, ' s/b  true false');
   writeln('String recarr 3:  ', (arec.a[3] < 'abcdefghix'):5, ' ',
           (arec.a[3] <= 'abcdefghij'):5, ' s/b  true  true');
   writeln('String recarr 4:  ', (arec.a[3] > 'abcdefghij'):5, ' ',
           (arec.a[3] >= 'abcdefghij'):5, ' s/b false  true');
   writeln('String recarr 5:  ', arec.a[3][3], ' s/b c');
   arec.a[3][3] := 'C';
   writeln('String recarr 6:  ', arec.a[3], ' s/b abCdefghij');

   { pointer sp^ }
   new(sp);
   sp^ := 'abcdefghij';
   writeln('String ptr 1:  ', sp^, ' s/b abcdefghij');
   writeln('String ptr 2:  ', (sp^ = 'abcdefghij'):5, ' ',
           (sp^ <> 'abcdefghij'):5, ' s/b  true false');
   writeln('String ptr 3:  ', (sp^ < 'abcdefghix'):5, ' ',
           (sp^ <= 'abcdefghij'):5, ' s/b  true  true');
   writeln('String ptr 4:  ', (sp^ > 'abcdefghij'):5, ' ',
           (sp^ >= 'abcdefghij'):5, ' s/b false  true');
   writeln('String ptr 5:  ', sp^[3], ' s/b c');
   sp^[3] := 'C';
   writeln('String ptr 6:  ', sp^, ' s/b abCdefghij');
   dispose(sp);

   { file buffer fs^: swept as an ordinary variable, then the file round-trip
     and buffer/eof status checks specific to its file nature }
   rewrite(fs);
   fs^ := 'abcdefghij';
   writeln('String buf 1:  ', fs^, ' s/b abcdefghij');
   writeln('String buf 2:  ', (fs^ = 'abcdefghij'):5, ' ',
           (fs^ <> 'abcdefghij'):5, ' s/b  true false');
   writeln('String buf 3:  ', (fs^ < 'abcdefghix'):5, ' ',
           (fs^ <= 'abcdefghij'):5, ' s/b  true  true');
   writeln('String buf 4:  ', (fs^ > 'abcdefghij'):5, ' ',
           (fs^ >= 'abcdefghij'):5, ' s/b false  true');
   writeln('String buf 5:  ', fs^[3], ' s/b c');
   fs^[3] := 'C';
   writeln('String buf 6:  ', fs^, ' s/b abCdefghij');
   fs^ := 'klmnopqrst'; put(fs);
   reset(fs);
   writeln('String buf eof1:  ', eof(fs):5, ' s/b false');
   writeln('String buf rt:  ', fs^, ' s/b klmnopqrst');
   get(fs);
   writeln('String buf eof2:  ', eof(fs):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   lt := 'zzzzzzzzzz';
   strvalpar(lt);
   writeln('String valpar rt:  ', lt, ' s/b zzzzzzzzzz');

   { var parameter (battery mutates the caller's lt in place) }
   strvarpar(lt);
   writeln('String varpar rt:  ', lt, ' s/b abCdefghij');

   { surrounding-procedure local (inner routine works on this proc's lt) }
   strsurround;
   writeln('String surround rt:  ', lt, ' s/b abCdefghij');

   i := 1; { reference i to avoid unused-variable concern }
   if i = 0 then writeln(lt)

end;

procedure pointercontexts;

type pint = ^integer;
     parr = array [1..5] of pint;
     prec = record
               f: pint;
               a: parr
            end;
     ppoint = ^pint;
     pfile = file of pint;

var lt: pint;             { local variable }
    ar: parr;             { array element }
    rec: prec;            { record field and nested array field }
    pp: ppoint;           { pointer to the type }
    fp: pfile;            { file of the type }
    q: pint;              { alias / equality companion }

   procedure ptrvalpar(t: pint); { value-parameter context }
   var q: pint;
   begin
      new(t);
      t^ := 5;
      writeln('Pointer valpar 1:  ', t^:1, ' s/b 5');
      t^ := t^+7; t^ := t^*3; t^ := t^-1;
      writeln('Pointer valpar 2:  ', t^:1, ' s/b 35');
      t^ := t^ div 4; t^ := t^ mod 6;
      writeln('Pointer valpar 3:  ', t^:1, ' s/b 2');
      q := t;
      t^ := t^*5;
      writeln('Pointer valpar 4:  ', q^:1, ' s/b 10');
      writeln('Pointer valpar 5:  ', (q = t):5, ' ', (q <> t):5, ' s/b  true false');
      q := nil;
      writeln('Pointer valpar 6:  ', (q = nil):5, ' ', (t = nil):5, ' ', (t <> q):5,
              ' s/b  true false  true');
      dispose(t)
   end;

   procedure ptrvarpar(var t: pint); { var-parameter context }
   var q: pint;
   begin
      new(t);
      t^ := 5;
      writeln('Pointer varpar 1:  ', t^:1, ' s/b 5');
      t^ := t^+7; t^ := t^*3; t^ := t^-1;
      writeln('Pointer varpar 2:  ', t^:1, ' s/b 35');
      t^ := t^ div 4; t^ := t^ mod 6;
      writeln('Pointer varpar 3:  ', t^:1, ' s/b 2');
      q := t;
      t^ := t^*5;
      writeln('Pointer varpar 4:  ', q^:1, ' s/b 10');
      writeln('Pointer varpar 5:  ', (q = t):5, ' ', (q <> t):5, ' s/b  true false');
      q := nil;
      writeln('Pointer varpar 6:  ', (q = nil):5, ' ', (t = nil):5, ' ', (t <> q):5,
              ' s/b  true false  true')
   end;

   procedure ptrsurround; { surrounding-procedure-local context: works on lt }
   var q: pint;
   begin
      new(lt);
      lt^ := 5;
      writeln('Pointer surround 1:  ', lt^:1, ' s/b 5');
      lt^ := lt^+7; lt^ := lt^*3; lt^ := lt^-1;
      writeln('Pointer surround 2:  ', lt^:1, ' s/b 35');
      lt^ := lt^ div 4; lt^ := lt^ mod 6;
      writeln('Pointer surround 3:  ', lt^:1, ' s/b 2');
      q := lt;
      lt^ := lt^*5;
      writeln('Pointer surround 4:  ', q^:1, ' s/b 10');
      writeln('Pointer surround 5:  ', (q = lt):5, ' ', (q <> lt):5, ' s/b  true false');
      q := nil;
      writeln('Pointer surround 6:  ', (q = nil):5, ' ', (lt = nil):5, ' ', (lt <> q):5,
              ' s/b  true false  true')
   end;

begin

   { local variable }
   new(lt);
   lt^ := 5;
   writeln('Pointer local 1:  ', lt^:1, ' s/b 5');
   lt^ := lt^+7; lt^ := lt^*3; lt^ := lt^-1;
   writeln('Pointer local 2:  ', lt^:1, ' s/b 35');
   lt^ := lt^ div 4; lt^ := lt^ mod 6;
   writeln('Pointer local 3:  ', lt^:1, ' s/b 2');
   q := lt;
   lt^ := lt^*5;
   writeln('Pointer local 4:  ', q^:1, ' s/b 10');
   writeln('Pointer local 5:  ', (q = lt):5, ' ', (q <> lt):5, ' s/b  true false');
   q := nil;
   writeln('Pointer local 6:  ', (q = nil):5, ' ', (lt = nil):5, ' ', (lt <> q):5,
           ' s/b  true false  true');
   dispose(lt);

   { array element ar[i] }
   new(ar[3]);
   ar[3]^ := 5;
   writeln('Pointer array 1:  ', ar[3]^:1, ' s/b 5');
   ar[3]^ := ar[3]^+7; ar[3]^ := ar[3]^*3; ar[3]^ := ar[3]^-1;
   writeln('Pointer array 2:  ', ar[3]^:1, ' s/b 35');
   ar[3]^ := ar[3]^ div 4; ar[3]^ := ar[3]^ mod 6;
   writeln('Pointer array 3:  ', ar[3]^:1, ' s/b 2');
   q := ar[3];
   ar[3]^ := ar[3]^*5;
   writeln('Pointer array 4:  ', q^:1, ' s/b 10');
   writeln('Pointer array 5:  ', (q = ar[3]):5, ' ', (q <> ar[3]):5, ' s/b  true false');
   q := nil;
   writeln('Pointer array 6:  ', (q = nil):5, ' ', (ar[3] = nil):5, ' ', (ar[3] <> q):5,
           ' s/b  true false  true');
   dispose(ar[3]);

   { record field rec.f }
   new(rec.f);
   rec.f^ := 5;
   writeln('Pointer recfld 1:  ', rec.f^:1, ' s/b 5');
   rec.f^ := rec.f^+7; rec.f^ := rec.f^*3; rec.f^ := rec.f^-1;
   writeln('Pointer recfld 2:  ', rec.f^:1, ' s/b 35');
   rec.f^ := rec.f^ div 4; rec.f^ := rec.f^ mod 6;
   writeln('Pointer recfld 3:  ', rec.f^:1, ' s/b 2');
   q := rec.f;
   rec.f^ := rec.f^*5;
   writeln('Pointer recfld 4:  ', q^:1, ' s/b 10');
   writeln('Pointer recfld 5:  ', (q = rec.f):5, ' ', (q <> rec.f):5, ' s/b  true false');
   q := nil;
   writeln('Pointer recfld 6:  ', (q = nil):5, ' ', (rec.f = nil):5, ' ', (rec.f <> q):5,
           ' s/b  true false  true');
   dispose(rec.f);

   { nested: record's array field rec.a[i] }
   new(rec.a[3]);
   rec.a[3]^ := 5;
   writeln('Pointer recarr 1:  ', rec.a[3]^:1, ' s/b 5');
   rec.a[3]^ := rec.a[3]^+7; rec.a[3]^ := rec.a[3]^*3; rec.a[3]^ := rec.a[3]^-1;
   writeln('Pointer recarr 2:  ', rec.a[3]^:1, ' s/b 35');
   rec.a[3]^ := rec.a[3]^ div 4; rec.a[3]^ := rec.a[3]^ mod 6;
   writeln('Pointer recarr 3:  ', rec.a[3]^:1, ' s/b 2');
   q := rec.a[3];
   rec.a[3]^ := rec.a[3]^*5;
   writeln('Pointer recarr 4:  ', q^:1, ' s/b 10');
   writeln('Pointer recarr 5:  ', (q = rec.a[3]):5, ' ', (q <> rec.a[3]):5, ' s/b  true false');
   q := nil;
   writeln('Pointer recarr 6:  ', (q = nil):5, ' ', (rec.a[3] = nil):5, ' ', (rec.a[3] <> q):5,
           ' s/b  true false  true');
   dispose(rec.a[3]);

   { pointer pp^ (a pointer-to-pointer; the type lives at pp^) }
   new(pp);
   new(pp^);
   pp^^ := 5;
   writeln('Pointer ptr 1:  ', pp^^:1, ' s/b 5');
   pp^^ := pp^^+7; pp^^ := pp^^*3; pp^^ := pp^^-1;
   writeln('Pointer ptr 2:  ', pp^^:1, ' s/b 35');
   pp^^ := pp^^ div 4; pp^^ := pp^^ mod 6;
   writeln('Pointer ptr 3:  ', pp^^:1, ' s/b 2');
   q := pp^;
   pp^^ := pp^^*5;
   writeln('Pointer ptr 4:  ', q^:1, ' s/b 10');
   writeln('Pointer ptr 5:  ', (q = pp^):5, ' ', (q <> pp^):5, ' s/b  true false');
   q := nil;
   writeln('Pointer ptr 6:  ', (q = nil):5, ' ', (pp^ = nil):5, ' ', (pp^ <> q):5,
           ' s/b  true false  true');
   dispose(pp^);
   dispose(pp);

   { file buffer fp^: swept as an ordinary variable, then a file round-trip.
     fp^ is itself a pointer (pint); the integers live at fp^^. }
   rewrite(fp);
   new(fp^);
   fp^^ := 5;
   writeln('Pointer buf 1:  ', fp^^:1, ' s/b 5');
   fp^^ := fp^^+7; fp^^ := fp^^*3; fp^^ := fp^^-1;
   writeln('Pointer buf 2:  ', fp^^:1, ' s/b 35');
   fp^^ := fp^^ div 4; fp^^ := fp^^ mod 6;
   writeln('Pointer buf 3:  ', fp^^:1, ' s/b 2');
   q := fp^;
   fp^^ := fp^^*5;
   writeln('Pointer buf 4:  ', q^:1, ' s/b 10');
   writeln('Pointer buf 5:  ', (q = fp^):5, ' ', (q <> fp^):5, ' s/b  true false');
   { file round-trip: write a pointer value out, read it back, follow it }
   new(fp^);
   fp^^ := 42; put(fp);
   reset(fp);
   writeln('Pointer buf eof1:  ', eof(fp):5, ' s/b false');
   writeln('Pointer buf rt:  ', fp^^:1, ' s/b 42');
   get(fp);
   writeln('Pointer buf eof2:  ', eof(fp):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected).
     pass a pointer with a known target; the callee reassigns its copy via new,
     so the caller's pointer still addresses the original 77. }
   new(lt); lt^ := 77;
   ptrvalpar(lt);
   writeln('Pointer valpar rt:  ', lt^:1, ' s/b 77');
   dispose(lt);

   { var parameter (battery mutates the caller's lt in place) }
   ptrvarpar(lt);
   writeln('Pointer varpar rt:  ', lt^:1, ' s/b 10');
   dispose(lt);

   { surrounding-procedure local (inner routine works on this proc's lt) }
   ptrsurround;
   writeln('Pointer surround rt:  ', lt^:1, ' s/b 10');
   dispose(lt)

end;

procedure arraycontexts;

type a5 = array[1..5] of integer;

var lt:   a5;                          { local variable of the type }
    la:   array[1..5] of a5;           { array of the type }
    lr:   record f: a5; a: array[1..5] of a5 end; { record: field + array-of-type field }
    lp:   ^a5;                         { pointer to the type }
    lf:   file of a5;                  { file of the type }
    b:    a5;                          { whole-array assignment target }
    i:    integer;

   procedure arrvalpar(t: a5); { value-parameter context }
   var b: a5; i: integer;
   begin
      t[3] := 5;
      writeln('Array valpar 1:  ', t[3]:1, ' s/b 5');
      t[3] := t[3]+7; t[3] := t[3]*3; t[3] := t[3]-1;
      writeln('Array valpar 2:  ', t[3]:1, ' s/b 35');
      t[3] := t[3] div 4; t[3] := t[3] mod 6;
      writeln('Array valpar 3:  ', t[3]:1, ' s/b 2');
      t[3] := t[3]-10; t[3] := t[3] mod 3;
      writeln('Array valpar 4:  ', t[3]:1, ' s/b 1');
      t[3] := sqr(succ(abs(t[3])));
      writeln('Array valpar 5:  ', t[3]:1, ' s/b 4');
      writeln('Array valpar 6:  ', (t[3] = 4):5, ' ', odd(t[3]):5, ' s/b  true false');
      for i := 1 to 5 do t[i] := i*10;
      b := t;
      writeln('Array valpar 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
              b[5]:1, ' s/b 10 20 30 40 50')
   end;

   procedure arrvarpar(var t: a5); { var-parameter context }
   var b: a5; i: integer;
   begin
      t[3] := 5;
      writeln('Array varpar 1:  ', t[3]:1, ' s/b 5');
      t[3] := t[3]+7; t[3] := t[3]*3; t[3] := t[3]-1;
      writeln('Array varpar 2:  ', t[3]:1, ' s/b 35');
      t[3] := t[3] div 4; t[3] := t[3] mod 6;
      writeln('Array varpar 3:  ', t[3]:1, ' s/b 2');
      t[3] := t[3]-10; t[3] := t[3] mod 3;
      writeln('Array varpar 4:  ', t[3]:1, ' s/b 1');
      t[3] := sqr(succ(abs(t[3])));
      writeln('Array varpar 5:  ', t[3]:1, ' s/b 4');
      writeln('Array varpar 6:  ', (t[3] = 4):5, ' ', odd(t[3]):5, ' s/b  true false');
      for i := 1 to 5 do t[i] := i*10;
      b := t;
      writeln('Array varpar 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
              b[5]:1, ' s/b 10 20 30 40 50')
   end;

   procedure arrsurround; { surrounding-procedure-local context: works on lt }
   var b: a5; i: integer;
   begin
      lt[3] := 5;
      writeln('Array surround 1:  ', lt[3]:1, ' s/b 5');
      lt[3] := lt[3]+7; lt[3] := lt[3]*3; lt[3] := lt[3]-1;
      writeln('Array surround 2:  ', lt[3]:1, ' s/b 35');
      lt[3] := lt[3] div 4; lt[3] := lt[3] mod 6;
      writeln('Array surround 3:  ', lt[3]:1, ' s/b 2');
      lt[3] := lt[3]-10; lt[3] := lt[3] mod 3;
      writeln('Array surround 4:  ', lt[3]:1, ' s/b 1');
      lt[3] := sqr(succ(abs(lt[3])));
      writeln('Array surround 5:  ', lt[3]:1, ' s/b 4');
      writeln('Array surround 6:  ', (lt[3] = 4):5, ' ', odd(lt[3]):5, ' s/b  true false');
      for i := 1 to 5 do lt[i] := i*10;
      b := lt;
      writeln('Array surround 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
              b[5]:1, ' s/b 10 20 30 40 50')
   end;

begin

   { local variable: lt of type a5 }
   lt[3] := 5;
   writeln('Array local 1:  ', lt[3]:1, ' s/b 5');
   lt[3] := lt[3]+7; lt[3] := lt[3]*3; lt[3] := lt[3]-1;
   writeln('Array local 2:  ', lt[3]:1, ' s/b 35');
   lt[3] := lt[3] div 4; lt[3] := lt[3] mod 6;
   writeln('Array local 3:  ', lt[3]:1, ' s/b 2');
   lt[3] := lt[3]-10; lt[3] := lt[3] mod 3;
   writeln('Array local 4:  ', lt[3]:1, ' s/b 1');
   lt[3] := sqr(succ(abs(lt[3])));
   writeln('Array local 5:  ', lt[3]:1, ' s/b 4');
   writeln('Array local 6:  ', (lt[3] = 4):5, ' ', odd(lt[3]):5, ' s/b  true false');
   for i := 1 to 5 do lt[i] := i*10;
   b := lt;
   writeln('Array local 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
           b[5]:1, ' s/b 10 20 30 40 50');

   { array element la[i] (array-of-array): la[2] is the a5 we operate on }
   la[2][3] := 5;
   writeln('Array arr 1:  ', la[2][3]:1, ' s/b 5');
   la[2][3] := la[2][3]+7; la[2][3] := la[2][3]*3; la[2][3] := la[2][3]-1;
   writeln('Array arr 2:  ', la[2][3]:1, ' s/b 35');
   la[2][3] := la[2][3] div 4; la[2][3] := la[2][3] mod 6;
   writeln('Array arr 3:  ', la[2][3]:1, ' s/b 2');
   la[2][3] := la[2][3]-10; la[2][3] := la[2][3] mod 3;
   writeln('Array arr 4:  ', la[2][3]:1, ' s/b 1');
   la[2][3] := sqr(succ(abs(la[2][3])));
   writeln('Array arr 5:  ', la[2][3]:1, ' s/b 4');
   writeln('Array arr 6:  ', (la[2][3] = 4):5, ' ', odd(la[2][3]):5, ' s/b  true false');
   for i := 1 to 5 do la[2][i] := i*10;
   b := la[2];
   writeln('Array arr 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
           b[5]:1, ' s/b 10 20 30 40 50');

   { record field lr.f: the a5 we operate on }
   lr.f[3] := 5;
   writeln('Array recfld 1:  ', lr.f[3]:1, ' s/b 5');
   lr.f[3] := lr.f[3]+7; lr.f[3] := lr.f[3]*3; lr.f[3] := lr.f[3]-1;
   writeln('Array recfld 2:  ', lr.f[3]:1, ' s/b 35');
   lr.f[3] := lr.f[3] div 4; lr.f[3] := lr.f[3] mod 6;
   writeln('Array recfld 3:  ', lr.f[3]:1, ' s/b 2');
   lr.f[3] := lr.f[3]-10; lr.f[3] := lr.f[3] mod 3;
   writeln('Array recfld 4:  ', lr.f[3]:1, ' s/b 1');
   lr.f[3] := sqr(succ(abs(lr.f[3])));
   writeln('Array recfld 5:  ', lr.f[3]:1, ' s/b 4');
   writeln('Array recfld 6:  ', (lr.f[3] = 4):5, ' ', odd(lr.f[3]):5, ' s/b  true false');
   for i := 1 to 5 do lr.f[i] := i*10;
   b := lr.f;
   writeln('Array recfld 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
           b[5]:1, ' s/b 10 20 30 40 50');

   { nested: record's array field lr.a[i] (array of a5): lr.a[2] is the a5 }
   lr.a[2][3] := 5;
   writeln('Array recarr 1:  ', lr.a[2][3]:1, ' s/b 5');
   lr.a[2][3] := lr.a[2][3]+7; lr.a[2][3] := lr.a[2][3]*3; lr.a[2][3] := lr.a[2][3]-1;
   writeln('Array recarr 2:  ', lr.a[2][3]:1, ' s/b 35');
   lr.a[2][3] := lr.a[2][3] div 4; lr.a[2][3] := lr.a[2][3] mod 6;
   writeln('Array recarr 3:  ', lr.a[2][3]:1, ' s/b 2');
   lr.a[2][3] := lr.a[2][3]-10; lr.a[2][3] := lr.a[2][3] mod 3;
   writeln('Array recarr 4:  ', lr.a[2][3]:1, ' s/b 1');
   lr.a[2][3] := sqr(succ(abs(lr.a[2][3])));
   writeln('Array recarr 5:  ', lr.a[2][3]:1, ' s/b 4');
   writeln('Array recarr 6:  ', (lr.a[2][3] = 4):5, ' ', odd(lr.a[2][3]):5, ' s/b  true false');
   for i := 1 to 5 do lr.a[2][i] := i*10;
   b := lr.a[2];
   writeln('Array recarr 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
           b[5]:1, ' s/b 10 20 30 40 50');

   { pointer lp^: the a5 we operate on }
   new(lp);
   lp^[3] := 5;
   writeln('Array ptr 1:  ', lp^[3]:1, ' s/b 5');
   lp^[3] := lp^[3]+7; lp^[3] := lp^[3]*3; lp^[3] := lp^[3]-1;
   writeln('Array ptr 2:  ', lp^[3]:1, ' s/b 35');
   lp^[3] := lp^[3] div 4; lp^[3] := lp^[3] mod 6;
   writeln('Array ptr 3:  ', lp^[3]:1, ' s/b 2');
   lp^[3] := lp^[3]-10; lp^[3] := lp^[3] mod 3;
   writeln('Array ptr 4:  ', lp^[3]:1, ' s/b 1');
   lp^[3] := sqr(succ(abs(lp^[3])));
   writeln('Array ptr 5:  ', lp^[3]:1, ' s/b 4');
   writeln('Array ptr 6:  ', (lp^[3] = 4):5, ' ', odd(lp^[3]):5, ' s/b  true false');
   for i := 1 to 5 do lp^[i] := i*10;
   b := lp^;
   writeln('Array ptr 7:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
           b[5]:1, ' s/b 10 20 30 40 50');
   dispose(lp);

   { file buffer lf^: swept as an ordinary variable, then a file round-trip }
   rewrite(lf);
   lf^[3] := 5;
   writeln('Array buf 1:  ', lf^[3]:1, ' s/b 5');
   lf^[3] := lf^[3]+7; lf^[3] := lf^[3]*3; lf^[3] := lf^[3]-1;
   writeln('Array buf 2:  ', lf^[3]:1, ' s/b 35');
   lf^[3] := lf^[3] div 4; lf^[3] := lf^[3] mod 6;
   writeln('Array buf 3:  ', lf^[3]:1, ' s/b 2');
   lf^[3] := lf^[3]-10; lf^[3] := lf^[3] mod 3;
   writeln('Array buf 4:  ', lf^[3]:1, ' s/b 1');
   lf^[3] := sqr(succ(abs(lf^[3])));
   writeln('Array buf 5:  ', lf^[3]:1, ' s/b 4');
   for i := 1 to 5 do lf^[i] := i*10;
   put(lf);
   reset(lf);
   writeln('Array buf eof1:  ', eof(lf):5, ' s/b false');
   b := lf^;
   writeln('Array buf rt:  ', b[1]:1, ' ', b[2]:1, ' ', b[3]:1, ' ', b[4]:1, ' ',
           b[5]:1, ' s/b 10 20 30 40 50');
   get(lf);
   writeln('Array buf eof2:  ', eof(lf):5, ' s/b  true');

   { value parameter (battery runs on the copy; caller's lt is unaffected) }
   for i := 1 to 5 do lt[i] := 99;
   arrvalpar(lt);
   writeln('Array valpar rt:  ', lt[1]:1, ' ', lt[2]:1, ' ', lt[3]:1, ' ', lt[4]:1, ' ',
           lt[5]:1, ' s/b 99 99 99 99 99');

   { var parameter (battery mutates the caller's lt in place) }
   arrvarpar(lt);
   writeln('Array varpar rt:  ', lt[1]:1, ' ', lt[2]:1, ' ', lt[3]:1, ' ', lt[4]:1, ' ',
           lt[5]:1, ' s/b 10 20 30 40 50');

   { surrounding-procedure local (inner routine works on this proc's lt) }
   arrsurround;
   writeln('Array surround rt:  ', lt[1]:1, ' ', lt[2]:1, ' ', lt[3]:1, ' ', lt[4]:1, ' ',
           lt[5]:1, ' s/b 10 20 30 40 50')

end;

procedure recordcontexts;

type rr = record i: integer; c: char end;
     rarr = array[1..5] of rr;
     rrec = record f: rr; a: rarr end;
     rptr = ^rr;

var lt2: rr;
    lt: rr;
    la: rarr;
    lr: rrec;
    lp: rptr;
    lf: file of rr;

   procedure recvalpar(t: rr);
   var lt2: rr;
   begin
   t.i := 42; t.c := 'x';
   writeln('Rec valpar 1:  ', t.i:1, ' ', t.c, ' s/b 42 x');
   t.i := t.i + 8;
   writeln('Rec valpar 2:  ', t.i:1, ' s/b 50');
   lt2 := t;
   writeln('Rec valpar 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; t := lt2;
   writeln('Rec valpar 4:  ', t.i:1, ' ', t.c, ' s/b 7 y')
   end;

   procedure recvarpar(var t: rr);
   var lt2: rr;
   begin
   t.i := 42; t.c := 'x';
   writeln('Rec varpar 1:  ', t.i:1, ' ', t.c, ' s/b 42 x');
   t.i := t.i + 8;
   writeln('Rec varpar 2:  ', t.i:1, ' s/b 50');
   lt2 := t;
   writeln('Rec varpar 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; t := lt2;
   writeln('Rec varpar 4:  ', t.i:1, ' ', t.c, ' s/b 7 y')
   end;

   procedure recsurround;
   var lt2: rr;
   begin
   lr.f.i := 42; lr.f.c := 'x';
   writeln('Rec surround 1:  ', lr.f.i:1, ' ', lr.f.c, ' s/b 42 x');
   lr.f.i := lr.f.i + 8;
   writeln('Rec surround 2:  ', lr.f.i:1, ' s/b 50');
   lt2 := lr.f;
   writeln('Rec surround 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; lr.f := lt2;
   writeln('Rec surround 4:  ', lr.f.i:1, ' ', lr.f.c, ' s/b 7 y')
   end;

begin

   lt.i := 42; lt.c := 'x';
   writeln('Rec local 1:  ', lt.i:1, ' ', lt.c, ' s/b 42 x');
   lt.i := lt.i + 8;
   writeln('Rec local 2:  ', lt.i:1, ' s/b 50');
   lt2 := lt;
   writeln('Rec local 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; lt := lt2;
   writeln('Rec local 4:  ', lt.i:1, ' ', lt.c, ' s/b 7 y');

   la[3].i := 42; la[3].c := 'x';
   writeln('Rec array 1:  ', la[3].i:1, ' ', la[3].c, ' s/b 42 x');
   la[3].i := la[3].i + 8;
   writeln('Rec array 2:  ', la[3].i:1, ' s/b 50');
   lt2 := la[3];
   writeln('Rec array 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; la[3] := lt2;
   writeln('Rec array 4:  ', la[3].i:1, ' ', la[3].c, ' s/b 7 y');

   lr.f.i := 42; lr.f.c := 'x';
   writeln('Rec recfld 1:  ', lr.f.i:1, ' ', lr.f.c, ' s/b 42 x');
   lr.f.i := lr.f.i + 8;
   writeln('Rec recfld 2:  ', lr.f.i:1, ' s/b 50');
   lt2 := lr.f;
   writeln('Rec recfld 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; lr.f := lt2;
   writeln('Rec recfld 4:  ', lr.f.i:1, ' ', lr.f.c, ' s/b 7 y');

   lr.a[3].i := 42; lr.a[3].c := 'x';
   writeln('Rec recarr 1:  ', lr.a[3].i:1, ' ', lr.a[3].c, ' s/b 42 x');
   lr.a[3].i := lr.a[3].i + 8;
   writeln('Rec recarr 2:  ', lr.a[3].i:1, ' s/b 50');
   lt2 := lr.a[3];
   writeln('Rec recarr 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; lr.a[3] := lt2;
   writeln('Rec recarr 4:  ', lr.a[3].i:1, ' ', lr.a[3].c, ' s/b 7 y');

   new(lp);
   lp^.i := 42; lp^.c := 'x';
   writeln('Rec ptr 1:  ', lp^.i:1, ' ', lp^.c, ' s/b 42 x');
   lp^.i := lp^.i + 8;
   writeln('Rec ptr 2:  ', lp^.i:1, ' s/b 50');
   lt2 := lp^;
   writeln('Rec ptr 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; lp^ := lt2;
   writeln('Rec ptr 4:  ', lp^.i:1, ' ', lp^.c, ' s/b 7 y');
   dispose(lp);

   rewrite(lf);
   lf^.i := 42; lf^.c := 'x';
   writeln('Rec buf 1:  ', lf^.i:1, ' ', lf^.c, ' s/b 42 x');
   lf^.i := lf^.i + 8;
   writeln('Rec buf 2:  ', lf^.i:1, ' s/b 50');
   lt2 := lf^;
   writeln('Rec buf 3:  ', lt2.i:1, ' ', lt2.c, ' s/b 50 x');
   lt2.i := 7; lt2.c := 'y'; lf^ := lt2;
   writeln('Rec buf 4:  ', lf^.i:1, ' ', lf^.c, ' s/b 7 y');
   lf^.i := 77; lf^.c := 'z'; put(lf);
   reset(lf);
   writeln('Rec buf eof1:  ', eof(lf):5, ' s/b false');
   writeln('Rec buf rt:  ', lf^.i:1, ' ', lf^.c, ' s/b 77 z');
   get(lf);
   writeln('Rec buf eof2:  ', eof(lf):5, ' s/b  true');

   lt.i := 1; lt.c := 'a';
   recvalpar(lt);
   writeln('Rec valpar rt:  ', lt.i:1, ' ', lt.c, ' s/b 1 a');

   recvarpar(lt);
   writeln('Rec varpar rt:  ', lt.i:1, ' ', lt.c, ' s/b 7 y');

   recsurround;
   writeln('Rec surround rt:  ', lr.f.i:1, ' ', lr.f.c, ' s/b 7 y')

end;


procedure filecontainers;

{ Item 2: arrays, records and pointers CONTAINING files (ISO 6.4.3.5 permits
  a structure to contain a file). The file is exercised through each container
  access path. }
type fir   = record fi: file of integer; tag: integer end;
     fiarr = array[1..3] of file of integer;
     frec  = record fa: fiarr end;
     arf   = array[1..3] of fir;
     firp  = ^fir;

var lr: fir; la: fiarr; ln: frec; lar: arf; lp: firp; x: integer;

begin

   rewrite(lr.fi);
   lr.fi^ := 10; put(lr.fi); lr.fi^ := 20; put(lr.fi); lr.fi^ := 30; put(lr.fi);
   reset(lr.fi);
   read(lr.fi, x);
   writeln('File-inrec 1:  ', x:1, ' s/b 10');
   read(lr.fi, x);
   writeln('File-inrec 2:  ', x:1, ' s/b 20');
   writeln('File-inrec 3:  ', eof(lr.fi):5, ' s/b false');
   read(lr.fi, x);
   writeln('File-inrec 4:  ', x:1, ' s/b 30');
   writeln('File-inrec 5:  ', eof(lr.fi):5, ' s/b  true');

   rewrite(la[2]);
   la[2]^ := 10; put(la[2]); la[2]^ := 20; put(la[2]); la[2]^ := 30; put(la[2]);
   reset(la[2]);
   read(la[2], x);
   writeln('File-inarr 1:  ', x:1, ' s/b 10');
   read(la[2], x);
   writeln('File-inarr 2:  ', x:1, ' s/b 20');
   writeln('File-inarr 3:  ', eof(la[2]):5, ' s/b false');
   read(la[2], x);
   writeln('File-inarr 4:  ', x:1, ' s/b 30');
   writeln('File-inarr 5:  ', eof(la[2]):5, ' s/b  true');

   rewrite(ln.fa[2]);
   ln.fa[2]^ := 10; put(ln.fa[2]); ln.fa[2]^ := 20; put(ln.fa[2]); ln.fa[2]^ := 30; put(ln.fa[2]);
   reset(ln.fa[2]);
   read(ln.fa[2], x);
   writeln('File-recarr 1:  ', x:1, ' s/b 10');
   read(ln.fa[2], x);
   writeln('File-recarr 2:  ', x:1, ' s/b 20');
   writeln('File-recarr 3:  ', eof(ln.fa[2]):5, ' s/b false');
   read(ln.fa[2], x);
   writeln('File-recarr 4:  ', x:1, ' s/b 30');
   writeln('File-recarr 5:  ', eof(ln.fa[2]):5, ' s/b  true');

   rewrite(lar[2].fi);
   lar[2].fi^ := 10; put(lar[2].fi); lar[2].fi^ := 20; put(lar[2].fi); lar[2].fi^ := 30; put(lar[2].fi);
   reset(lar[2].fi);
   read(lar[2].fi, x);
   writeln('File-arrrec 1:  ', x:1, ' s/b 10');
   read(lar[2].fi, x);
   writeln('File-arrrec 2:  ', x:1, ' s/b 20');
   writeln('File-arrrec 3:  ', eof(lar[2].fi):5, ' s/b false');
   read(lar[2].fi, x);
   writeln('File-arrrec 4:  ', x:1, ' s/b 30');
   writeln('File-arrrec 5:  ', eof(lar[2].fi):5, ' s/b  true');

   new(lp);
   rewrite(lp^.fi);
   lp^.fi^ := 10; put(lp^.fi); lp^.fi^ := 20; put(lp^.fi); lp^.fi^ := 30; put(lp^.fi);
   reset(lp^.fi);
   read(lp^.fi, x);
   writeln('File-inptr 1:  ', x:1, ' s/b 10');
   read(lp^.fi, x);
   writeln('File-inptr 2:  ', x:1, ' s/b 20');
   writeln('File-inptr 3:  ', eof(lp^.fi):5, ' s/b false');
   read(lp^.fi, x);
   writeln('File-inptr 4:  ', x:1, ' s/b 30');
   writeln('File-inptr 5:  ', eof(lp^.fi):5, ' s/b  true');
   dispose(lp)

end;

begin

   write('****************************************************************');
   writeln('***************');
   writeln;
   writeln('                 TEST SUITE FOR ISO 7185 PASCAL');
   writeln;
   write('                 Copyright (C) 1995 S. A. Moore - All rights ');
   writeln('reserved');
   writeln;
   write('****************************************************************');
   writeln('***************');
   writeln;

{******************************************************************************

                          Reference dangling defines

******************************************************************************}

{ unused declarations are always a problem, because it is always concievable
  that there is a compiler test that will reveal they are not used. We use
  assign to references here because a simple read of a variable could fault
  on an undefined reference. Its also possible that a never used fault could
  occur (written, but never used), in which case the code would have to be
  more complex. The best solution, of course, is to write a real test that
  uses the variables. }

   a[1] :=  1;
   esia[two] := 1;
   pesia[two] := 1;
   rewrite(fes);
   rewrite(pfes);
   rewrite(fs);
   rewrite(pfs);
   rewrite(fr);
   rewrite(pfr);
   rewrite(fst);
   rewrite(pfst);
   rewrite(fa);
   rewrite(pfa);
   rewrite(frc);
   rewrite(pfrc);
   rewrite(fstc);
   rewrite(pfstc);
   rewrite(fp);
   rewrite(pfp);
   rcastt := 1;
   rcast.rcastt := true;
   intaliasv := 1;

{******************************************************************************

                                 Metering

******************************************************************************}

   writeln('The following are implementation defined characteristics');
   writeln;
   writeln('Maxint: ', maxint:1);
   i := maxint;
   x := 0;
   while i > 0 do begin i := i div 2;  x := x+1 end;
   writeln('Bit length of integer without sign bit appears to be: ', x:1);
   writeln('Integer default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln(1);
   writeln('Real default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln(1.2);
   writeln('Note that the exponent character ''e'' or ''E'' is implementation');
   writeln('defined as well as the number of exponent digits');
   writeln('Boolean default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln(false);
   writeln(true);
   writeln('Note that the upper or lower case state of the characters in');
   writeln('''true'' and ''false'' are implementation defined');
   writeln('Char default output field');
   writeln('         1111111111222222222233333333334');
   writeln('1234567890123456789012345678901234567890');
   writeln('a');
   if (ord('a') = 97) and (ord('(') = 40) and (ord('^') = 94) then
      writeln('Appears to be ASCII')
   else
      writeln('Appears to not be ASCII');

{******************************************************************************

                           Control structures

******************************************************************************}

   writeln;
   writeln('******************* Control structures tests *******************');
   writeln;
   write('Control1: ');
   for i := 1 to 10 do write(i:1, ' ');
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control2: ');
   for i := 10 downto 1 do write(i:1, ' ');
   writeln('s/b 10 9 8 7 6 5 4 3 2 1');
   write('Control3: ');
   i := 1;
   while i <=10{comment}do begin write(i:1, ' '); i := i + 1 end;
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control4: ');
   i := 1; repeat write(i:1, ' '); i := i + 1 until i > 10;
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control5: ');
   i := 1;{comment*)
   0: write(i:1, ' '); i := i + 1; if i <= 10 then goto 0;
   writeln('s/b 1 2 3 4 5 6 7 8 9 10');
   write('Control6: ');(*comment}
   if true then write('yes') else{comment}write('no');
   writeln(' s/b yes');
   write('Control7: ');
   if false then write('no') else write('yes');
   writeln(' s/b yes');
   write('Control8: ');
   if true then write('yes '); write('stop');
   writeln(' s/b yes stop');
   write('Control9: ');
   if false then write('no '); write('stop');
   writeln(' s/b stop');(*)comment*)
   write('Control10: ');
   for i := 1 to 10 do
      case i of
         1:     write('one ');
         2:     write('two ');
         3:     write('three ');
         4:     write('four ');
         5:     write('five ');
         6:     write('six ');
         7:     write('seven ');
         8:     write('eight ');
         9, 10: write('nine-ten ')

      end;
   writeln;
   write('Control10: s/b ');
   write('one two three four five ');
   writeln('six seven eight nine-ten nine-ten');
   write('Control11: start ');
   junk6;
   write('!! BAD !!');
   9999: writeln('stop s/b start stop');
   write('Control12: start ');
   goto 003;
   write('!! BAD !!');
   3: writeln('stop s/b start stop');
   write('Control13: start ');
   { self defined fors }
   i := 10;
   for i := 1 to i do write(i:3);
   writeln(' s/b start  1  2  3  4  5  6  7  8  9 10');
   write('Control14: start ');
   { self defined fors }
   i := 10;
   for i := i downto 1 do write(i:3);
   writeln(' s/b start 10  9  8  7  6  5  4  3  2  1');
   write('Control15: start ');
   { for against 0 }
   for i := 0 to 9 do write(i:2);
   writeln(' s/b start 0 1 2 3 4 5 6 7 8 9');
   write('Control16: start ');
   { for against 0 }
   for i := 9 downto 0 do write(i:2);
   writeln(' s/b start 9 8 7 6 5 4 3 2 1 0');
   { wide spread of case statements }
   write('Control17: start ');
   i := 10000;
   case i of{comment{comment}
      1: write('*** bad ***');
      10000: write('good')
   end;
   writeln(' s/b start good');
   write('Control18: start ');
   repeat(*comment(*comment*)
      goto 004;
      write('!! BAD !!');
      4: writeln('stop s/b start stop');
      i := 0;
      if i <> 0 then goto 04;
   until true;

{******************************************************************************

                            Integers

******************************************************************************}

   writeln;
   writeln('******************* Integers *******************');
   writeln;

   { integer variables }
   x := 43; y := 78; z := y;
   writeln('Integer1:   ', x + y:1, ' s/b 121');
   writeln('Integer2:   ', y - x:1, ' s/b 35');
   writeln('Integer3:   ', x * y:1, ' s/b 3354');
   writeln('Integer4:   ', y div x:1, ' s/b 1');
   writeln('Integer5:   ', y mod x:1, ' s/b 35');
   writeln('Integer6:   ', succ(x):1, ' s/b 44');
   writeln('Integer7:   ', pred(x):1, ' s/b 42');
   writeln('Integer8:   ', sqr(x):1, ' s/b 1849');
   writeln('Integer9:   ', chr(y), ' s/b N');
   writeln('Integer10:  ', ord(chr(x)):1, ' s/b 43');
   writeln('Integer11:  ', odd(x):5, ' s/b true');
   writeln('Integer12:  ', odd(y):5, ' s/b false');
   writeln('Integer13:  ', z = y:5, ' s/b true');
   writeln('Integer14:  ', x = y:5, ' s/b false');
   writeln('Integer15:  ', x < y:5, ' s/b true');
   writeln('Integer16:  ', y < x:5, ' s/b false');
   writeln('Integer17:  ', y > x:5, ' s/b true');
   writeln('Integer18:  ', x > y:5, ' s/b false');
   writeln('Integer19:  ', x <> y:5, ' s/b true');
   writeln('Integer20:  ', y <> z:5, ' s/b false');
   writeln('Integer21:  ', x <= y:5, ' s/b true');
   writeln('Integer22:  ', z <= y:5, ' s/b true');
   writeln('Integer23:  ', y <= x:5, ' s/b false');
   writeln('Integer24:  ', y >= x:5, ' s/b true');
   writeln('Integer25:  ', y >= z:5, ' s/b true');
   writeln('Integer26:  ', x >= y:5, ' s/b false');

   { unsigned integer constants }
   write('Integer27:  '); i := 546; writeln(i:1, ' s/b 546');
   writeln('Integer28:  ', 56 + 34:1, ' s/b 90');
   writeln('Integer29:  ', 56 - 34:1, ' s/b 22');
   writeln('Integer30:  ', 056 * 34:1, ' s/b 1904');
   writeln('Integer31:  ', 56 div 34:1, ' s/b 1');
   writeln('Integer32:  ', 00000056 mod 34:1, ' s/b 22');
   writeln('Integer33:  ', succ(5):1, ' s/b 6');
   writeln('Integer34:  ', pred(5):1, ' s/b 4');
   writeln('Integer35:  ', sqr(7):1, ' s/b 49');
   writeln('Integer36:  ', chr(65), ' s/b A');
   writeln('Integer37:  ', ord(chr(65)):1, ' s/b 65');
   writeln('Integer38:  ', tcnst:1, ' s/b 768');
   writeln('Integer39:  ', odd(5):5, ' s/b true');
   writeln('Integer40:  ', odd(8):5, ' s/b false');
   writeln('Integer41:  ', 56 = 56:5, ' s/b true');
   writeln('Integer42:  ', 56 = 57:5, ' s/b false');
   writeln('Integer43:  ', 56 < 57:5, ' s/b true');
   writeln('Integer44:  ', 57 < 56:5, ' s/b false');
   writeln('Integer45:  ', 57 > 56:5, ' s/b true');
   writeln('Integer46:  ', 56 > 57:5, ' s/b false');
   writeln('Integer47:  ', 56 <> 57:5, ' s/b true');
   writeln('Integer48:  ', 56 <> 56:5, ' s/b false');
   writeln('Integer49:  ', 55 <= 500:5, ' s/b true');
   writeln('Integer50:  ', 67 <= 67:5, ' s/b true');
   writeln('Integer51:  ', 56 <= 33:5, ' s/b false');
   writeln('Integer52:  ', 645 >= 4:5, ' s/b true');
   writeln('Integer53:  ', 23 >= 23:5, ' s/b true');
   writeln('Integer54:  ', 45 >= 123:5, ' s/b false');

   { signed integer variables }
   as := -14;
   bs := -32;
   cs := -14;
   ds := 20;
   es := -15;
   gs := maxint;
   hs := mmaxint;
   vnum := -maxint;
   writeln('Integer55:  ', as + ds:1, ' s/b 6');
   writeln('Integer56:  ', ds + as:1, ' s/b 6');
   writeln('Integer57:  ', bs + ds:1, ' s/b -12');
   writeln('Integer58:  ', as + bs:1, ' s/b -46');
   writeln('Integer59:  ', ds - as:1, ' s/b 34');
   writeln('Integer60:  ', bs - ds:1, ' s/b -52');
   writeln('Integer61:  ', bs - as:1, ' s/b -18');
   writeln('Integer62:  ', ds * as:1, ' s/b -280');
   writeln('Integer63:  ', as * ds:1, ' s/b -280');
   writeln('Integer64:  ', as * bs:1, ' s/b 448');
   writeln('Integer65:  ', ds div as:1, ' s/b -1');
   writeln('Integer66:  ', bs div ds:1, ' s/b -1');
   writeln('Integer67:  ', bs div as:1, ' s/b 2');
   writeln('Integer68:  ', succ(as):1, ' s/b -13');
   writeln('Integer69:  ', pred(bs):1, ' s/b -33');
   writeln('Integer70: ', sqr(as):1, ' s/b 196');
   writeln('Integer71:  ', odd(as):5, ' s/b false');
   writeln('Integer72:  ', odd(es):5, ' s/b true');
   writeln('Integer73:  ', as = cs:5, ' s/b true');
   writeln('Integer74:  ', as = bs:5, ' s/b false');
   writeln('Integer75:  ', as <> bs:5, ' s/b true');
   writeln('Integer76:  ', as <> cs:5, ' s/b false');
   writeln('Integer77:  ', as < ds:5, ' s/b true');
   writeln('Integer78:  ', bs < as:5, ' s/b true');
   writeln('Integer79:  ', ds < as:5, ' s/b false');
   writeln('Integer80:  ', as < bs:5, ' s/b false');
   writeln('Integer81:  ', ds > as:5, ' s/b true');
   writeln('Integer82:  ', as > bs:5, ' s/b true');
   writeln('Integer83:  ', as > ds:5, ' s/b false');
   writeln('Integer84:  ', bs > as:5, ' s/b false');
   writeln('Integer85:  ', as <= ds:5, ' s/b true');
   writeln('Integer86:  ', bs <= as:5, ' s/b true');
   writeln('Integer87:  ', as <= cs:5, ' s/b true');
   writeln('Integer88:  ', ds <= as:5, ' s/b false');
   writeln('Integer89:  ', as <= bs:5, ' s/b false');
   writeln('Integer90:  ', ds >= as:5, ' s/b true');
   writeln('Integer91:  ', as >= bs:5, ' s/b true');
   writeln('Integer92:  ', as >= cs:5, ' s/b true');
   writeln('Integer93:  ', as >= ds:5, ' s/b false');
   writeln('Integer94:  ', bs >= as:5, ' s/b false');
   writeln('Integer95:  ', abs(as):1, ' s/b 14');
   writeln('Integer96:  ', gs+hs:1, ' s/b 0');
   writeln('Integer97:  ', gs-maxint:1, ' s/b 0');
   writeln('Integer98:  ', gs+vnum:1, ' s/b 0');

   { signed integer constants }
   writeln('Integer99:  ', 45 + (-30):1, ' s/b 15');
   writeln('Integer100:  ', -25 + 70:1, ' s/b 45');
   writeln('Integer101: ', -62 + 23:1, ' s/b -39');
   writeln('Integer102: ', -20 + (-15):1, ' s/b -35');
   writeln('Integer103: ', 20 - (-14):1, ' s/b 34');
   writeln('Integer104: ', -34 - 14:1, ' s/b -48');
   writeln('Integer105: ', -56 - (-12):1, ' s/b -44');
   writeln('Integer106: ', 5 * (-4):1, ' s/b -20');
   writeln('Integer107: ', (-18) * 7:1, ' s/b -126');
   writeln('Integer108: ', (-40) * (-13):1, ' s/b 520');
   writeln('Integer109: ', 30 div (-5):1, ' s/b -6');
   writeln('Integer110: ', (-50) div 2:1, ' s/b -25');
   writeln('Integer111: ', (-20) div (-4):1, ' s/b 5');
   writeln('Integer112: ', succ(-10):1, ' s/b -9');
   writeln('Integer113: ', succ(-1):1, ' s/b 0');
   writeln('Integer114: ', pred(-1):1, ' s/b -2');
   writeln('Integer115: ', sqr(-8):1, ' s/b 64');
   writeln('Integer116: ', pred(-54):1, ' s/b -55');
   writeln('Integer117: ', odd(-20):5, ' s/b false');
   writeln('Integer118: ', odd(-15):5, ' s/b true');
   writeln('Integer119: ', -5 = -5:5, ' s/b true');
   writeln('Integer120: ', -5 = 5:5, ' s/b false');
   writeln('Integer121: ', -21 <> -40:5, ' s/b true');
   writeln('Integer122: ', -21 <> -21:5, ' s/b false');
   writeln('Integer123: ', -3 < 5:5, ' s/b true');
   writeln('Integer124: ', -32 < -20:5, ' s/b true');
   writeln('Integer125: ', 20 < -20:5, ' s/b false');
   writeln('Integer126: ', -15 < -40:5, ' s/b false');
   writeln('Integer127: ', 70 > -4:5, ' s/b true');
   writeln('Integer128: ', -23 > -34:5, ' s/b true');
   writeln('Integer129: ', -5 > 5:5, ' s/b false');
   writeln('Integer130: ', -60 > -59:5, ' s/b false');
   writeln('Integer131: ', -12 <= 4:5, ' s/b true');
   writeln('Integer132: ', -14 <= -5:5, ' s/b true');
   writeln('Integer133: ', -7 <= -7:5, ' s/b true');
   writeln('Integer134: ', 5 <= -5:5, ' s/b false');
   writeln('Integer135: ', -10 <= -20:5, ' s/b false');
   writeln('Integer136: ', 9 >= -3:5, ' s/b true');
   writeln('Integer137: ', -4 >= -10:5, ' s/b true');
   writeln('Integer138: ', -13 >= -13:5, ' s/b true');
   writeln('Integer139: ', -6 >= 6:5, ' s/b false');
   writeln('Integer140: ', -20 >= -10:5, ' s/b false');
   writeln('Integer141: ', abs(-6):1, ' s/b 6');
   writeln('Integer142: ', tsncst:1, ' s/b -52');
   writeln('Integer143: ', -tsncst:1, ' s/b 52');
   writeln('Integer144: ', tsncst2:1, ' s/b -768');
   writeln('Integer145: ', tsncst3:1, ' s/b 52');
   writeln('Integer146: ', maxint+mmaxint:1, ' s/b 0');
   
   { other integer }
   myowninteger := 42;
   writeln('Integer147: ', mYowNintegeR:1, ' s/b 42');
   myvar := 1;
   myvarmyvar := 2;
   myvarmyvarmyvar := 3;
   myvarmyvarmyvarmyvar := 4;
   myvarmyvarmyvarmyvarmyvar := 5;
   myvarmyvarmyvarmyvarmyvarmyvar := 6;
   myvarmyvarmyvarmyvarmyvarmyvarmyvar := 7;
   myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar := 8;
   myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar := 9;
   myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar := 10;
   myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar := 11;
   myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar := 12;
   myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar := 13;
   writeln('Integer148: ', 
      myvar:1, ' ',
      myvarmyvar:1, ' ',
      myvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar:1, ' ',
      myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar:1,
      ' s/b 1 2 3 4 5 6 7 8 9 10 11 12 13');
   { ISO 7185 6.7.2.2: i mod j yields a non-negative result, 0 <= r < j }
   writeln('Integer149: ', (-10) mod 3:1, ' s/b 2');
   writeln('Integer150: ', (-1) mod 3:1, ' s/b 2');
   writeln('Integer151: ', (-6) mod 3:1, ' s/b 0');
   writeln('Integer152: ', (-15) mod 4:1, ' s/b 1');
   writeln('Integer153: ', 10 mod 3:1, ' s/b 1');

   { integer operation battery through every access-path context }
   integercontexts;

{******************************************************************************

                            Subranges

******************************************************************************}

   writeln;
   writeln('******************* Subranges *******************');
   writeln;

   { subrange unsigned variables }
   srx := 43; sry := 78; srz := sry;
   writeln('Subrange1:   ', srx + sry:1, ' s/b 121');
   writeln('Subrange2:   ', sry - srx:1, ' s/b 35');
   writeln('Subrange3:   ', srx * sry:1, ' s/b 3354');
   writeln('Subrange4:   ', sry div srx:1, ' s/b 1');
   writeln('Subrange5:   ', sry mod srx:1, ' s/b 35');
   writeln('Subrange6:   ', succ(srx):1, ' s/b 44');
   writeln('Subrange7:   ', pred(srx):1, ' s/b 42');
   writeln('Subrange8:   ', chr(sry), ' s/b N');
   writeln('Subrange9:   ', ord(chr(srx)):1, ' s/b 43');
   writeln('Subrange10:  ', odd(srx):5, ' s/b true');
   writeln('Subrange11:  ', odd(sry):5, ' s/b false');
   writeln('Subrange12:  ', srz = sry:5, ' s/b true');
   writeln('Subrange13:  ', srx = sry:5, ' s/b false');
   writeln('Subrange14:  ', srx < sry:5, ' s/b true');
   writeln('Subrange15:  ', sry < srx:5, ' s/b false');
   writeln('Subrange16:  ', sry > srx:5, ' s/b true');
   writeln('Subrange17:  ', srx > sry:5, ' s/b false');
   writeln('Subrange18:  ', srx <> sry:5, ' s/b true');
   writeln('Subrange19:  ', sry <> srz:5, ' s/b false');
   writeln('Subrange20:  ', srx <= sry:5, ' s/b true');
   writeln('Subrange21:  ', srz <= sry:5, ' s/b true');
   writeln('Subrange22:  ', sry <= srx:5, ' s/b false');
   writeln('Subrange23:  ', sry >= srx:5, ' s/b true');
   writeln('Subrange24:  ', sry >= srz:5, ' s/b true');
   writeln('Subrange25:  ', srx >= sry:5, ' s/b false');

   { signed subrange variables }
   sras := -14;
   srbs := -32;
   srcs := -14;
   srds := 20;
   sres := -15;
   writeln('Subrange26:  ', sras + srds:1, ' s/b 6');
   writeln('Subrange27:  ', srds + sras:1, ' s/b 6');
   writeln('Subrange28:  ', srbs + srds:1, ' s/b -12');
   writeln('Subrange29:  ', sras + srbs:1, ' s/b -46');
   writeln('Subrange30:  ', srds - sras:1, ' s/b 34');
   writeln('Subrange31:  ', srbs - srds:1, ' s/b -52');
   writeln('Subrange32:  ', srbs - sras:1, ' s/b -18');
   writeln('Subrange33:  ', srds * sras:1, ' s/b -280');
   writeln('Subrange34:  ', sras * srds:1, ' s/b -280');
   writeln('Subrange35:  ', sras * srbs:1, ' s/b 448');
   writeln('Subrange36:  ', srds div sras:1, ' s/b -1');
   writeln('Subrange37:  ', srbs div srds:1, ' s/b -1');
   writeln('Subrange38:  ', srbs div sras:1, ' s/b 2');
   writeln('Subrange39:  ', succ(sras):1, ' s/b -13');
   writeln('Subrange40:  ', pred(srbs):1, ' s/b -33');
   writeln('Subrange41:  ', odd(sras):5, ' s/b false');
   writeln('Subrange42:  ', odd(sres):5, ' s/b true');
   writeln('Subrange43:  ', sras = srcs:5, ' s/b true');
   writeln('Subrange44:  ', sras = srbs:5, ' s/b false');
   writeln('Subrange45:  ', sras <> srbs:5, ' s/b true');
   writeln('Subrange46:  ', sras <> srcs:5, ' s/b false');
   writeln('Subrange47:  ', sras < srds:5, ' s/b true');
   writeln('Subrange48:  ', srbs < sras:5, ' s/b true');
   writeln('Subrange49:  ', srds < sras:5, ' s/b false');
   writeln('Subrange50:  ', sras < srbs:5, ' s/b false');
   writeln('Subrange51:  ', srds > sras:5, ' s/b true');
   writeln('Subrange52:  ', sras > srbs:5, ' s/b true');
   writeln('Subrange53:  ', sras > srds:5, ' s/b false');
   writeln('Subrange54:  ', srbs > sras:5, ' s/b false');
   writeln('Subrange55:  ', sras <= srds:5, ' s/b true');
   writeln('Subrange56:  ', srbs <= sras:5, ' s/b true');
   writeln('Subrange57:  ', sras <= srcs:5, ' s/b true');
   writeln('Subrange58:  ', srds <= sras:5, ' s/b false');
   writeln('Subrange59:  ', sras <= srbs:5, ' s/b false');
   writeln('Subrange60:  ', srds >= sras:5, ' s/b true');
   writeln('Subrange61:  ', sras >= srbs:5, ' s/b true');
   writeln('Subrange62:  ', sras >= srcs:5, ' s/b true');
   writeln('Subrange63:  ', sras >= srds:5, ' s/b false');
   writeln('Subrange64:  ', srbs >= sras:5, ' s/b false');
   writeln('Subrange65:  ', abs(sras):1, ' s/b 14');


   { Subranges types: operation battery through every access-path context }
   subrangecontexts;

{******************************************************************************

                         Characters

******************************************************************************}

   writeln;
   writeln('******************* Characters*******************');
   writeln;

   { character variables }
   ca := 'g'; cb := 'g'; cc := 'u';
   writeln('Character1:   ', ca, ' ', cb, ' ', cc, ' s/b g g u');
   writeln('Character2:   ', succ(ca), ' s/b h');
   writeln('Character3:   ', pred(cb), ' s/b f');
   writeln('Character4:   ', ord(ca):1, ' s/b 103');
   writeln('Character5:   ', chr(ord(cc)), ' s/b u');
   writeln('Character6:   ', ca = cb:5, ' s/b true');
   writeln('Character7:   ', ca = cc:5, ' s/b false');
   writeln('Character8:   ', ca < cc:5, ' s/b true');
   writeln('Character9:   ', cc < ca:5, ' s/b false');
   writeln('Character10:  ', cc > ca:5, ' s/b true');
   writeln('Character11:  ', ca > cc:5, ' s/b false');
   writeln('Character12:  ', ca <> cc:5, ' s/b true');
   writeln('Character13:  ', ca <> cb:5, ' s/b false');
   writeln('Character14:  ', ca <= cc:5, ' s/b true');
   writeln('Character15:  ', ca <= cb:5, ' s/b true');
   writeln('Character16:  ', cc <= ca:5, ' s/b false');
   writeln('Character17:  ', cc >= cb:5, ' s/b true');
   writeln('Character18:  ', cb >= ca:5, ' s/b true');
   writeln('Character19:  ', cb >= cc:5, ' s/b false');
   sa := 'porker    '; sb := 'porker    '; sc := 'parker    ';
   writeln('Character20:  ', sa, sb, sc,
      ' s/b porker    porker    parker');
   writeln('Character21:  ', sa = sb:5, ' s/b true');
   writeln('Character22:  ', sa = sc:5, ' s/b false');
   writeln('Character23:  ', sc < sa:5, ' s/b true');
   writeln('Character24:  ', sa < sc:5, ' s/b false');
   writeln('Character25:  ', sa > sc:5, ' s/b true');
   writeln('Character26:  ', sc > sa:5, ' s/b false');
   writeln('Character27:  ', sa <> sc:5, ' s/b true');
   writeln('Character28:  ', sa <> sb:5, ' s/b false');
   writeln('Character29:  ', sc <= sa:5, ' s/b true');
   writeln('Character30:  ', sa <= sb:5, ' s/b true');
   writeln('Character40:  ', sa <= sc:5, ' s/b false');
   writeln('Character41:  ', sa >= sc:5, ' s/b true');
   writeln('Character42:  ', sa >= sb:5, ' s/b true');
   writeln('Character43:  ', sc >= sa:5, ' s/b false');
   write('Character44:  ');
   for ca := 'a' to 'z' do write(ca);
   writeln(' s/b abcdefghijklmnopqrstuvwxyz');
   write('Character45:  ');
   for ca := 'z' downto 'a' do write(ca);
   writeln(' s/b zyxwvutsrqponmlkjihgfedcba');
   write('Character46:  ');
   x := 0;
   for ca := 'a' to 'z' do begin car[ca] := x; x := x + 1 end;
   for ca := 'z' downto 'a' do write(car[ca]:1, ' ');
   writeln;
   writeln('Character46: s/b 25 24 23 22 21 20 19 18 17 16 15',
      ' 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0');
   r.rc := 'n'; writeln('Character47: ', r.rc, ' s/b n');
   r.rs := 'junky01234'; writeln('Character48: ', r.rs,
                           ' s/b junky01234');
   for i := 1 to 10 do sar[i] := '0123456789';
   sar[1] := 'trash     ';
   sar[2] := 'finnork   ';
   sar[10] := 'crapola   ';
   writeln('Character49:  ');
   for i := 10 downto 1 do writeln(sar[i]);
   writeln('Character49: s/b');
   writeln('crapola');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('0123456789');
   writeln('finnork');
   writeln('trash');
   writeln('Character50:  ');
   for ca := '0' to '9' do
   begin
     case ca of
       '5': write('five ');
       '3': write('three ');
       '6': write('six ');
       '8': write('eight ');
       '0': write('zero ');
       '9': write('nine ');
       '7': write('seven ');
       '4': write('four ');
       '1': write('one ');
       '2': write('two ');
     end
   end;
   writeln;
   writeln(' s/b zero one two three four five six ',
           'seven eight nine');

   { character constants }
   writeln('Character51:  ', 'a', ' s/b a');
   writeln('Character52:  ', succ('a'), ' s/b b');
   writeln('Character53:  ', pred('z'), ' s/b y');
   writeln('Character54:  ', ord('c'):1, ' s/b 99');
   writeln('Character55:  ', chr(ord('g')), ' s/b g');
   writeln('Character56:  ', 'q' = 'q':5, ' s/b true');
   writeln('Character57:  ', 'r' = 'q':5, ' s/b false');
   writeln('Character58:  ', 'b' < 't':5, ' s/b true');
   writeln('Character59:  ', 'g' < 'c':5, ' s/b false');
   writeln('Character60:  ', 'f' > 'e':5, ' s/b true');
   writeln('Character61:  ', 'f' > 'g':5, ' s/b false');
   writeln('Character62:  ', 'h' <> 'l':5, ' s/b true');
   writeln('Character63:  ', 'i' <> 'i':5, ' s/b false');
   writeln('Character64:  ', 'v' <= 'y':5, ' s/b true');
   writeln('Character65:  ', 'y' <= 'y':5, ' s/b true');
   writeln('Character66:  ', 'z' <= 'y':5, ' s/b false');
   writeln('Character67:  ', 'l' >= 'b':5, ' s/b true');
   writeln('Character68:  ', 'l' >= 'l':5, ' s/b true');
   writeln('Character69:  ', 'l' >= 'm':5, ' s/b false');
   writeln('Character70:  ', 'finnork' = 'finnork':5, ' s/b true');
   writeln('Character71:  ',
      'finoork' = 'finnork':5, ' s/b false');
   writeln('Character72:  ', 'oliab' < 'olibb':5, ' s/b true');
   writeln('Character73:  ', 'olibb' < 'oliab':5, ' s/b false');
   writeln('Character74:  ', 'olibb' > 'oliab':5, ' s/b true');
   writeln('Character75:  ', 'oliab' > 'olibb':5, ' s/b false');
   writeln('Character76:  ', 'fark ' <> 'farks':5, ' s/b true');
   writeln('Character77:  ', 'farks' <> 'farks':5, ' s/b false');
   writeln('Character78:  ', 'farka' <= 'farkz':5, ' s/b true');
   writeln('Character79:  ', 'farks' <= 'farks':5, ' s/b true');
   writeln('Character80:  ', 'farkz' <= 'farks':5, ' s/b false');
   writeln('Character81:  ', 'topnat' >= 'topcat':5, ' s/b true');
   writeln('Character82:  ', 'topcat' >= 'topcat':5, ' s/b true');
   writeln('Character83:  ', 'topcat' >= 'topzat':5, ' s/b false');
   writeln('Character84:  ', scst, ' s/b this is a string');
   writeln('Character85:  ', ccst, ' s/b v');
   writeln('Character86:  ');
   for i := 15 downto 1 do writeln('hello, world': i);
   writeln('Character87:  s/b:');
   writeln('   hello, world');
   writeln('  hello, world');
   writeln(' hello, world ');
   writeln('hello, world');
   writeln('hello, worl');
   writeln('hello, wor');
   writeln('hello, wo');
   writeln('hello, w');
   writeln('hello, ');
   writeln('hello,');
   writeln('hello');
   writeln('hell');
   writeln('hel');
   writeln('he');
   writeln('h');
   
   { ordering }
   writeln('Character88: ');
   write(succ('0') = '1', ' ');
   write(succ('1') = '2', ' ');
   write(succ('2') = '3', ' ');
   write(succ('3') = '4', ' ');
   write(succ('4') = '5', ' ');
   write(succ('5') = '6', ' ');
   write(succ('6') = '7', ' ');
   write(succ('7') = '8', ' ');
   writeln(succ('8') = '9', ' ');
   writeln('s/b');
   writeln(' true  true  true  true  true  true  true  true  true');
   
   { Note it is possible for only one case to be present, but likely this whole
     test would fail if that were true }
   writeln('Character89:');
   write('a' < 'b', ' ');
   write('b' < 'c', ' ');
   write('c' < 'd', ' ');
   write('d' < 'e', ' ');
   write('e' < 'f', ' ');
   write('f' < 'g', ' ');
   write('g' < 'h', ' ');
   write('h' < 'i', ' ');
   write('i' < 'j', ' ');
   writeln('j' < 'k', ' ');
   write('k' < 'l', ' ');
   write('l' < 'm', ' ');
   write('m' < 'n', ' ');
   write('n' < 'o', ' ');
   write('o' < 'p', ' ');
   write('p' < 'q', ' ');
   write('q' < 'r', ' ');
   write('r' < 's', ' ');
   write('s' < 't', ' ');
   writeln('t' < 'u', ' ');
   write('u' < 'v', ' ');
   write('v' < 'w', ' ');
   write('w' < 'x', ' ');
   write('x' < 'y', ' ');
   writeln('y' < 'z', ' ');
   writeln('s/b');
   writeln(' true  true  true  true  true  true  true  true  true  true');
   writeln(' true  true  true  true  true  true  true  true  true  true');
   writeln(' true  true  true  true  true');
   writeln('Character90:');
   write('A' < 'B', ' ');
   write('B' < 'C', ' ');
   write('C' < 'D', ' ');
   write('D' < 'E', ' ');
   write('E' < 'F', ' ');
   write('F' < 'G', ' ');
   write('G' < 'H', ' ');
   write('H' < 'I', ' ');
   write('I' < 'J', ' ');
   writeln('J' < 'K', ' ');
   write('K' < 'L', ' ');
   write('L' < 'M', ' ');
   write('M' < 'N', ' ');
   write('N' < 'O', ' ');
   write('O' < 'P', ' ');
   write('P' < 'Q', ' ');
   write('Q' < 'R', ' ');
   write('R' < 'S', ' ');
   write('S' < 'T', ' ');
   writeln('T' < 'U', ' ');
   write('U' < 'V', ' ');
   write('V' < 'W', ' ');
   write('W' < 'X', ' ');
   write('X' < 'Y', ' ');
   writeln('Y' < 'Z', ' ');
   writeln('s/b');
   writeln(' true  true  true  true  true  true  true  true  true  true');
   writeln(' true  true  true  true  true  true  true  true  true  true');
   writeln(' true  true  true  true  true');


   { Characters types: operation battery through every access-path context }
   charcontexts;

{******************************************************************************

                            Booleans

******************************************************************************}

   writeln;
   writeln('******************* Booleans *******************');
   writeln;

   { boolean variables }
   ba := true; bb := false; bc := true;
   writeln('Boolean1:   ', ba:5, ' ', bb:5, ' s/b true false');
   writeln('Boolean2:   ', succ(bb):5, ' s/b true');
   writeln('Boolean3:   ', pred(ba):5, ' s/b false');
   writeln('Boolean4:   ', ord(bb):1, ' s/b 0');
   writeln('Boolean5:   ', ord(ba):1, ' s/b 1');
   writeln('Boolean6:   ', ba = bc:5, ' s/b true');
   writeln('Boolean7:   ', bb = bb:5, ' s/b true');
   writeln('Boolean8:   ', ba = bb:5, ' s/b false');
   writeln('Boolean9:   ', bb < ba:5, ' s/b true');
   writeln('Boolean10:  ', ba < bb:5, ' s/b false');
   writeln('Boolean11:  ', ba > bb:5, ' s/b true');
   writeln('Boolean12:  ', bb > ba:5, ' s/b false');
   writeln('Boolean13:  ', ba <> bb:5, ' s/b true');
   writeln('Boolean14:  ', ba <> bc:5, ' s/b false');
   writeln('Boolean15:  ', bb <= ba:5, ' s/b true');
   writeln('Boolean16:  ', ba <= bc:5, ' s/b true');
   writeln('Boolean17:  ', ba <= bb:5, ' s/b false');
   writeln('Boolean18:  ', ba >= bb:5, ' s/b true');
   writeln('Boolean19:  ', bb >= bb:5, ' s/b true');
   writeln('Boolean20:  ', bb >= ba:5, ' s/b false');
   write('Boolean21:  ');
   for ba := false to true do write(ba:5, ' ');
   writeln('s/b false true');
   write('Boolean22:  ');
   for bb := true downto false do write(bb:5, ' ');
   writeln('s/b true false');
   write('Boolean23:  ');
   ba := 1 > 0; writeln(ba:5, ' s/b true');
   write('Boolean24:  ');
   ba := 1 < 0; writeln(ba:5, ' s/b false');

   { boolean constants }
   writeln('Boolean25:  ', true:5, ' ', false:5, ' s/b true false');
   writeln('Boolean26:  ', succ(false):5, ' s/b true');
   writeln('Boolean27:  ', pred(true):5, ' s/b false');
   writeln('Boolean28:  ', ord(false):1, ' s/b 0');
   writeln('Boolean29:  ', ord(true):1, ' s/b 1');
   writeln('Boolean30:  ', true = true:5, ' s/b true');
   writeln('Boolean31:  ', false = false:5, ' s/b true');
   writeln('Boolean32:  ', true = false:5, ' s/b false');
   writeln('Boolean33:  ', false < true:5, ' s/b true');
   writeln('Boolean34:  ', true < false:5, ' s/b false');
   writeln('Boolean35:  ', true > false:5, ' s/b true');
   writeln('Boolean36:  ', false > true:5, ' s/b false');
   writeln('Boolean37:  ', true <> false:5, ' s/b true');
   writeln('Boolean38:  ', true <> true:5, ' s/b false');
   writeln('Boolean39:  ', false <= true:5, ' s/b true');
   writeln('Boolean40:  ', true <= true:5, ' s/b true');
   writeln('Boolean41:  ', true <= false:5, ' s/b false');
   writeln('Boolean42:  ', true >= false:5, ' s/b true');
   writeln('Boolean43:  ', false >= false:5, ' s/b true');
   writeln('Boolean44:  ', false >= true:5, ' s/b false');
   writeln('Boolean45:');
   for i := 10 downto 1 do writeln(false:i);
   writeln('Boolean45: s/b:');
   writeln('     false');
   writeln('    false');
   writeln('   false');
   writeln('  false');
   writeln(' false');
   writeln('false');
   writeln('fals');
   writeln('fal');
   writeln('fa');
   writeln('f');
   writeln('Boolean46:');
   for i := 10 downto 1 do writeln(true:i);
   writeln('Boolean46: s/b:');
   writeln('      true');
   writeln('     true');
   writeln('    true');
   writeln('   true');
   writeln('  true');
   writeln(' true');
   writeln('true');
   writeln('tru');
   writeln('tr');
   writeln('t');
   { Boolean operators or/and/not (ISO 7185 6.7.2.3) }
   writeln('Boolean47:  ', (true or false):5, ' s/b  true');
   writeln('Boolean48:  ', (false or false):5, ' s/b false');
   writeln('Boolean49:  ', (true and true):5, ' s/b  true');
   writeln('Boolean50:  ', (true and false):5, ' s/b false');
   writeln('Boolean51:  ', (not false):5, ' s/b  true');
   writeln('Boolean52:  ', (not true):5, ' s/b false');



   { Booleans types: operation battery through every access-path context }
   booleancontexts;

{******************************************************************************

                            Scalar variables

******************************************************************************}

   writeln;
   writeln('******************* Scalar *******************');
   writeln;

   { scalar variables }
   sva := wed; svb := mon; svc := wed;
   writeln('Scalar1:   ', succ(svb) = tue:5, ' s/b true');
   writeln('Scalar2:   ', pred(sva) = tue:5, ' s/b true');
   writeln('Scalar3:   ', ord(svb):1, ' s/b 0');
   writeln('Scalar4:   ', ord(sva):1, ' s/b 2');
   writeln('Scalar5:   ', sva = svc:5, ' s/b true');
   writeln('Scalar6:   ', svb = svb:5, ' s/b true');
   writeln('Scalar7:   ', sva = svb:5, ' s/b false');
   writeln('Scalar8:   ', svb < sva:5, ' s/b true');
   writeln('Scalar9:   ', sva < svb:5, ' s/b false');
   writeln('Scalar10:  ', sva > svb:5, ' s/b true');
   writeln('Scalar11:  ', svb > sva:5, ' s/b false');
   writeln('Scalar12:  ', sva <> svb:5, ' s/b true');
   writeln('Scalar13:  ', sva <> svc:5, ' s/b false');
   writeln('Scalar14:  ', svb <= sva:5, ' s/b true');
   writeln('Scalar15:  ', sva <= svc:5, ' s/b true');
   writeln('Scalar16:  ', sva <= svb:5, ' s/b false');
   writeln('Scalar17:  ', sva >= svb:5, ' s/b true');
   writeln('Scalar18:  ', svb >= svb:5, ' s/b true');
   writeln('Scalar19:  ', svb >= sva:5, ' s/b false');
   write('Scalar20:  ');
   for sva := mon to sun do write(ord(sva):1, ' ');
   writeln('s/b 0 1 2 3 4 5 6');
   write('Scalar21:  ');
   for svb := sun downto mon do write(ord(svb):1, ' ');
   writeln('s/b 6 5 4 3 2 1 0');

   { scalar constants }
   writeln('Scalar20:   ', succ(mon) = tue:5, ' s/b true');
   writeln('Scalar21:   ', pred(fri) = thur:5, ' s/b true');
   writeln('Scalar22:   ', ord(wed):1, ' s/b 2');
   writeln('Scalar23:   ', ord(sun):1, ' s/b 6');
   writeln('Scalar24:   ', thur = thur:5, ' s/b true');
   writeln('Scalar25:   ', fri = fri:5, ' s/b true');
   writeln('Scalar26:   ', tue = wed:5, ' s/b false');
   writeln('Scalar27:   ', mon < wed:5, ' s/b true');
   writeln('Scalar28:   ', fri < fri:5, ' s/b false');
   writeln('Scalar29:  ', sun > sat:5, ' s/b true');
   writeln('Scalar30:  ', fri > sun:5, ' s/b false');
   writeln('Scalar31:  ', thur <> tue:5, ' s/b true');
   writeln('Scalar32:  ', wed <> wed:5, ' s/b false');
   writeln('Scalar33:  ', mon <= fri:5, ' s/b true');
   writeln('Scalar34:  ', fri <= fri:5, ' s/b true');
   writeln('Scalar35:  ', sat <= fri:5, ' s/b false');
   writeln('Scalar36:  ', fri >= tue:5, ' s/b true');
   writeln('Scalar37:  ', tue >= tue:5, ' s/b true');
   writeln('Scalar38:  ', tue >= sat:5, ' s/b false');


   { Scalar variables types: operation battery through every access-path context }
   enumcontexts;

{******************************************************************************

                            Reals

******************************************************************************}

   writeln;
   writeln('******************* Reals ******************************');
   writeln;

   { formats, input (compiler) and output }
   writeln('Real1:   ', 1.554:15, ' s/b  1.554000e+00');
   writeln('Real2:   ', 0.00334:15, ' s/b  3.340000e-03');
   writeln('Real3:   ', 0.00334e-21:15, ' s/b  3.340000e-24');
   writeln('Real4:   ', 4e-45:15, ' s/b  4.000000e-45');
   writeln('Real5:   ', -5.565:15, ' s/b -5.565000e+00');
   writeln('Real6:   ', -0.00944:15, ' s/b -9.440000e-03');
   writeln('Real7:   ', -0.006364E32:15, ' s/b -6.364000e+29');
   writeln('Real8:   ', -2e-14:15, ' s/b -2.000000e-14');
   writeln('Real9:');
   writeln('         11111111112222222222333333333344444444445');
   writeln('12345678901234567890123456789012345678901234567890');
   for i := 1 to 14 do writeln(1.23456789012345678901234567890:i);
   writeln('s/b (note precision dropoff at right):');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.2e+000');
   writeln(' 1.23e+000');
   writeln(' 1.234e+000');
   writeln(' 1.2345e+000');
   writeln(' 1.23456e+000');
   writeln('Real10:');
   writeln('         11111111112222222222333333333344444444445');
   writeln('12345678901234567890123456789012345678901234567890');
   for i := 1 to 14 do writeln(i+0.23456789012345678901234567890:1:i);
   writeln('s/b (note precision dropoff at right):');
   writeln('1.2');
   writeln('2.23');
   writeln('3.234');
   writeln('4.2345');
   writeln('5.23456');
   writeln('6.234567');
   writeln('7.2345678');
   writeln('8.23456789');
   writeln('9.234567890');
   writeln('10.2345678901');
   writeln('11.23456789012');
   writeln('12.234567890123');
   writeln('13.2345678901234');
   writeln('14.23456789012345');

   { unsigned variables }
   ra := 435.23;
   rb := 983.67;
   rc := rb;
   rd := 0.3443;
   writeln('Real11:  ', ra + rb:15, ' s/b  1.418900e+03');
   writeln('Rea112:  ', rb - ra:15, ' s/b  5.484399e+02');
   writeln('Real13:  ', ra * rb:15, ' s/b  4.281227e+05');
   writeln('Real14:  ', rb / ra:15, ' s/b  2.260115e+00');
   writeln('Real15:  ', rc = rb:5, ' s/b true');
   writeln('Real16:  ', ra = rb:5, ' s/b false');
   writeln('Real17:  ', ra < rb:5, ' s/b true');
   writeln('Real18:  ', rb < ra:5, ' s/b false');
   writeln('Real19:  ', rb > ra:5, ' s/b true');
   writeln('Real20:  ', ra > rb:5, ' s/b false');
   writeln('Real21:  ', ra <> rb:5, ' s/b true');
   writeln('Real22:  ', rb <> rc:5, ' s/b false');
   writeln('Real23:  ', ra <= rb:5, ' s/b true');
   writeln('Real24:  ', rc <= rb:5, ' s/b true');
   writeln('Real25:  ', rb <= ra:5, ' s/b false');
   writeln('Real26:  ', rb >= ra:5, ' s/b true');
   writeln('Real27:  ', rb >= rc:5, ' s/b true');
   writeln('Real28:  ', ra >= rb:5, ' s/b false');
   writeln('Real29:  ', abs(ra):15, ' s/b  4.35230e+02');
   writeln('Real30:  ', sqr(ra):15, ' s/b  1.89425e+05');
   writeln('Real31:  ', sqrt(rb):15, ' s/b  3.13635e+01');
   writeln('Real32:  ', sin(rb):15, ' s/b -3.44290e-01');
   writeln('Real33:  ', arctan(ra):15, ' s/b  1.56850e+00');
   writeln('Real34:  ', exp(rd):15, ' s/b  1.41100e+00');
   writeln('Real35:  ', ln(ra):15, ' s/b  6.07587e+00');
   writeln('Real36:  ', trunc(ra):1, ' s/b 435');
   writeln('Real37:  ', round(rb):1, ' s/b 984');
   writeln('Real38:  ', round(ra):1, ' s/b 435');

   { unsigned constants }
   writeln('Real39:  ', 344.939 + 933.113:15, ' s/b  1.278052e+03');
   writeln('Real40:  ', 883.885 - 644.939:15, ' s/b  2.389460e+02');
   writeln('Real41:  ', 754.74 * 138.75:15, ' s/b  1.047202e+05');
   writeln('Real42:  ', 634.3 / 87373.99:15, ' s/b  7.259598e-03');
   writeln('Real43:  ', 77.44 = 77.44:5, ' s/b true');
   writeln('Real44:  ', 733.9 = 959.2:5, ' s/b false');
   writeln('Real45:  ', 883.22 < 8383.33:5, ' s/b true');
   writeln('Real46:  ', 475.322 < 234.93:5, ' s/b false');
   writeln('Real47:  ', 7374.3 > 6442.34:5, ' s/b true');
   writeln('Real48:  ', 985.562 > 1001.95:5, ' s/b false');
   writeln('Real49:  ', 030.11 <> 0938.44:5, ' s/b true');
   writeln('Real50:  ', 1.233 <> 1.233:5, ' s/b false');
   writeln('Real51:  ', 8484.002 <= 9344.003:5, ' s/b true');
   writeln('Real52:  ', 9.11 <= 9.11:5, ' s/b true');
   writeln('Real53:  ', 93.323 <= 90.323:5, ' s/b false');
   writeln('Real54:  ', 6543.44 >= 5883.33:5, ' s/b true');
   writeln('Real55:  ', 3247.03 >= 3247.03:5, ' s/b true');
   writeln('Real56:  ', 28343.22 >= 30044.45:5, ' s/b false');
   writeln('Real57:  ', abs(34.93):15, ' s/b  3.493000e+01');
   writeln('Real58:  ', sqr(2.34):15, ' s/b  5.475600e+00');
   writeln('Real59:  ', sqrt(9454.32):15, ' s/b  9.723333e+01');
   writeln('Real60:  ', sin(34.22):15, ' s/b  3.311461e-01');
   writeln('Real61:  ', arctan(343.2):15, ' s/b  1.567883e+00');
   writeln('Real62:  ', exp(0.332):15, ' s/b  1.393753e+00');
   writeln('Real63:  ', ln(83.22):15, ' s/b  4.421488e+00');
   writeln('Real64:  ', trunc(24.344):1, ' s/b 24');
   writeln('Real65:  ', round(74.56):1, ' s/b 75');
   writeln('Real66:  ', round(83.24):1, ' s/b 83');
   writeln('Real67:  ', rcnst:15, ' s/b  4.333000e+01');

   { signed variables }
   ra := -734.2;
   rb := -7634.52;
   rc := ra;
   rd := 1034.54;
   re := -0.38483;
   writeln('Real68:  ', ra + rd:15, ' s/b  3.003400e+02');
   writeln('Real69:  ', rd + ra:15, ' s/b  3.003400e+02');
   writeln('Real70:  ', rb + rd:15, ' s/b -6.599980e+03');
   writeln('Real71:  ', ra + rb:15, ' s/b -8.368720e+03');
   writeln('Real72:  ', rd - ra:15, ' s/b  1.768740e+03');
   writeln('Real73:  ', rb - rd:15, ' s/b -8.669061e+03');
   writeln('Real74:  ', rb - ra:15, ' s/b -6.900320e+03');
   writeln('Real75:  ', rd * ra:15, ' s/b -7.595593e+05');
   writeln('Real76:  ', ra * rd:15, ' s/b -7.595593e+05');
   writeln('Real77:  ', ra * rb:15, ' s/b  5.605265e+06');
   writeln('Real78:  ', rd / ra:15, ' s/b -1.409071e+00');
   writeln('Real79:  ', rb / rd:15, ' s/b -7.379627e+00');
   writeln('Real80:  ', rb / ra:15, ' s/b  1.039842e+01');
   writeln('Real81:  ', ra = rc:5, ' s/b true');
   writeln('Real82:  ', ra = rb:5, ' s/b false');
   writeln('Real83:  ', ra <> rb:5, ' s/b true');
   writeln('Real84:  ', ra <> rc:5, ' s/b false');
   writeln('Real85:  ', ra < rd:5, ' s/b true');
   writeln('Real86:  ', rb < ra:5, ' s/b true');
   writeln('Real87:  ', rd < ra:5, ' s/b false');
   writeln('Real88:  ', ra < rb:5, ' s/b false');
   writeln('Real89:  ', rd > ra:5, ' s/b true');
   writeln('Real90:  ', ra > rb:5, ' s/b true');
   writeln('Real91:  ', ra > rd:5, ' s/b false');
   writeln('Real92:  ', rb > ra:5, ' s/b false');
   writeln('Real93:  ', ra <= rd:5, ' s/b true');
   writeln('Real94:  ', rb <= ra:5, ' s/b true');
   writeln('Real95:  ', ra <= rc:5, ' s/b true');
   writeln('Real96:  ', rd <= ra:5, ' s/b false');
   writeln('Real97:  ', ra <= rb:5, ' s/b false');
   writeln('Real98:  ', rd >= ra:5, ' s/b true');
   writeln('Real99:  ', ra >= rb:5, ' s/b true');
   writeln('Real100: ', ra >= rc:5, ' s/b true');
   writeln('Real101: ', ra >= rd:5, ' s/b false');
   writeln('Real102: ', rb >= ra:5, ' s/b false');
   writeln('Real103: ', abs(ra):15, ' s/b  7.34200e+02');
   writeln('Real104: ', sqr(ra):15, ' s/b  5.39050e+05');
   writeln('Real105: ', sin(rb):15, ' s/b -4.34850e-01');
   writeln('Real106: ', arctan(ra):15, ' s/b -1.56943e+00');
   writeln('Real107: ', exp(re):15, ' s/b  6.80566e-01');
   writeln('Real108: ', trunc(ra):15, ' s/b -734');
   writeln('Real109: ', round(rb):15, ' s/b -7635');
   writeln('Real110: ', round(ra):15, ' s/b -734');

   { signed constants }
   writeln('Real111: ', 45.934 + (-30.834):15, ' s/b  1.510000e+01');
   writeln('Real112: ', -25.737 + 70.87:15, ' s/b  4.513300e+01');
   writeln('Real113: ', -62.63 + 23.99:15, ' s/b -3.864000e+01');
   writeln('Real114: ', -20.733 + (-15.848):15, ' s/b -3.658100e+01');
   writeln('Real115: ', 20.774 - (-14.774):15, ' s/b  3.554800e+01');
   writeln('Real116: ', -34.523 - 14.8754:15, ' s/b -4.939840e+01');
   writeln('Real117: ', -56.664 - (-12.663):15, ' s/b -4.400100e+01');
   writeln('Real118: ', 5.663 * (-4.664):15, ' s/b -2.641223e+01');
   writeln('Real119: ', (-18.62) * 7.997:15, ' s/b -1.489041e+02');
   writeln('Real120: ', (-40.552) * (-13.774):15, ' s/b  5.585632e+02');
   writeln('Real121: ', 30.6632 / (-5.874):15, ' s/b -5.220157e+00');
   writeln('Real122: ', (-50.636) / 2.8573:15, ' s/b -1.772163e+01');
   writeln('Real123: ', (-20.7631) / (-4.85734):15, ' s/b  4.274582e+00');
   writeln('Real124: ', -5.775 = -5.775:5, ' s/b true');
   writeln('Real125: ', -5.6364 = 5.8575:5, ' s/b false');
   writeln('Real126: ', -21.6385 <> -40.764:5, ' s/b true');
   writeln('Real127: ', -21.772 <> -21.772:5, ' s/b false');
   writeln('Real128: ', -3.512 < 5.8467:5, ' s/b true');
   writeln('Real129: ', -32.644 < -20.9074:5, ' s/b true');
   writeln('Real130: ', 20.763 < -20.743:5, ' s/b false');
   writeln('Real131: ', -15.663 < -40.784:5, ' s/b false');
   writeln('Real132: ', 70.766 > -4.974:5, ' s/b true');
   writeln('Real133: ', -23.6532 > -34.774:5, ' s/b true');
   writeln('Real134: ', -5.773 > 5.9874:5, ' s/b false');
   writeln('Real135: ', -60.663 > -59.78:5, ' s/b false');
   writeln('Real136: ', -12.542 <= 4.0848:5, ' s/b true');
   writeln('Real137: ', -14.8763 <= -5.0847:5, ' s/b true');
   writeln('Real138: ', -7.8373 <= -7.8373:5, ' s/b true');
   writeln('Real139: ', 5.4564 <= -5.4564:5, ' s/b false');
   writeln('Real140: ', -10.72633 <= -20.984:5, ' s/b false');
   writeln('Real141: ', 9.834 >= -3.9383:5, ' s/b true');
   writeln('Real142: ', -4.562 >= -10.74:5, ' s/b true');
   writeln('Real143: ', -13.63 >= -13.63:5, ' s/b true');
   writeln('Real144: ', -6.74 >= 6.74:5, ' s/b false');
   writeln('Real145: ', -20.7623 >= -10.574:5, ' s/b false');
   writeln('Real146: ', abs(-6.823):15, ' s/b  6.823000e+00');
   writeln('Real147  ', sqr(-348.22):15, ' s/b  1.212572e+05');
   writeln('Real148: ', sin(-733.22):15, ' s/b  9.421146e-01');
   writeln('Real149: ', arctan(-8387.22):15, ' s/b -1.570677e+00');
   writeln('Real150: ', exp(-0.8743):15, ' s/b  4.171539e-01');
   writeln('Real151: ', trunc(-33.422):1, ' s/b -33');
   writeln('Real152: ', round(-843.22):1, ' s/b -843');
   writeln('Real153: ', round(-6243.76):1, ' s/b -6244');
   writeln('Real154: ', rscst:15, ' s/b -8.422000e+01');
   writeln('Real155: ', -rscst:15, ' s/b  8.422000e+01');
   writeln('Real156:  ', rscst2:15, ' s/b -4.333000e+01');
   writeln('Real157: ', rscst3:15, ' s/b  8.422000e+01');
   writeln('Real158: ', cos(0.0):15, ' s/b  1.000000e+00');
   writeln('Real159: ', cos(1.0471975512):15, ' s/b  5.000000e-01');
   writeln('Real160: ', cos(-3.1415926536):15, ' s/b -1.000000e+00');


   { Reals types: operation battery through every access-path context }
   realcontexts;

{******************************************************************************

                            Sets

******************************************************************************}

   writeln;
   writeln('******************* sets ******************************');
   writeln;

   { sets of integers }
   write('Set1:  ');
   sta := [];
   for i := 1 to 10 do if odd(i) then sta := sta+[i, i+10];
   for i := 1 to 20 do if i in sta then write('1') else write('0');
   write(' s/b ');
   writeln('10101010101010101010');
   write('Set2:  ');
   sta := [1, 4, 5];
   stb := [2, 6, 10];
   for i := 1 to 10 do if i in sta+stb then write('1') else write('0');
   write(' s/b ');
   writeln('1101110001');
   write('Set3:  ');
   sta := [1, 2, 6, 5, 7];
   stb := [2, 6, 10];
   for i := 1 to 10 do if i in sta*stb then write('1') else write('0');
   write(' s/b ');
   writeln('0100010000');
   write('Set4:  ');
   sta := [2, 4, 7, 8];
   stb := [1, 3, 4, 8, 10];
   for i := 1 to 10 do if i in sta-stb then write('1') else write('0');
   write(' s/b ');
   writeln('0100001000');
   sta := [4, 6, 8, 9];
   stb := [1, 4, 5, 9];
   stc := [4, 6, 8, 9];
   writeln('Set5:  ', sta = stb:5, ' s/b false');
   writeln('Set6:  ', sta = stc:5, ' s/b true');
   writeln('Set7:  ', sta <> stb:5, ' s/b true');
   writeln('Set8:  ', sta <> stc:5, ' s/b false');
   sta := [1, 2, 5, 7, 10];
   stb := [1, 5, 10];
   stc := [1, 5, 10, 6];
   std := [1, 2, 5, 7, 10];
   writeln('Set9:  ', stb <= sta:5, ' s/b true');
   writeln('Set10: ', stb <= std:5, ' s/b true');
   writeln('Set11: ', stc <= sta:5, ' s/b false');
   writeln('Set12: ', sta >= stb:5, ' s/b true');
   writeln('Set13: ', std >= stb:5, ' s/b true');
   writeln('Set14: ', sta >= stc:5, ' s/b false');
   write('Set15: ');
   i := 2;
   x := 4;
   sta := [i, x, i+x];
   for i := 1 to 10 do if i in sta then write('1') else write('0');
   write(' s/b ');
   writeln('0101010000');
   { these are just compile time tests }
   ste := std;
   stf := [1, 2, 5, 7];
   stg := stf;
   i := 10;
   writeln('Set16: ', 5 in [cone..i], ' s/b true');

   { sets of characters }
   write('Set17: ');
   csta := [];
   for ci := 'a' to 'j' do
      if odd(ord(ci)) then csta := csta+[ci, chr(ord(ci)+10)];
   for ci := 'a' to 't' do if ci in csta then write(ci) else write('_');
   write(' s/b ');
   writeln('a_c_e_g_i_k_m_o_q_s_');
   write('Set18: ');
   csta := ['a', 'c', 'f'];
   cstb := ['c', 'd', 'g'];
   for ci := 'a' to 'j' do if ci in csta+cstb then write(ci) else write('_');
   write(' s/b ');
   writeln('a_cd_fg___');
   write('Set19: ');
   csta := ['d', 'f', 'h', 'a'];
   cstb := ['a', 'b', 'i', 'h'];
   for ci := 'a' to 'j' do if ci in csta*cstb then write(ci) else write('_');
   write(' s/b ');
   writeln('a______h__');
   write('Set20: ');
   csta := ['b', 'd', 'i', 'j'];
   cstb := ['i', 'h', 'd', 'e'];
   for ci := 'a' to 'j' do if ci in csta-cstb then write(ci) else write('_');
   write(' s/b ');
   writeln('_b_______j');
   csta := ['b', 'd', 'h', 'j'];
   cstb := ['a', 'd', 'h', 'c'];
   cstc := ['b', 'd', 'h', 'j'];
   writeln('Set21: ', csta = cstb:5, ' s/b false');
   writeln('Set22: ', csta = cstc:5, ' s/b true');
   writeln('Set23: ', csta <> cstb:5, ' s/b true');
   writeln('Set24: ', csta <> cstc:5, ' s/b false');
   csta := ['a', 'b', 'f', 'g', 'j'];
   cstb := ['a', 'f', 'g'];
   cstc := ['a', 'f', 'g', 'h'];
   cstd := ['a', 'b', 'f', 'g', 'j'];
   writeln('Set25: ', cstb <= csta:5, ' s/b true');
   writeln('Set26: ', cstb <= cstd:5, ' s/b true');
   writeln('Set27: ', cstc <= csta:5, ' s/b false');
   writeln('Set28: ', csta >= cstb:5, ' s/b true');
   writeln('Set29: ', cstd >= cstb:5, ' s/b true');
   writeln('Set30: ', csta >= cstc:5, ' s/b false');
   write('Set31: ');
   ci := 'a';
   i := 4;
   csta := [ci, chr(ord(ci)+i)];
   for ci := 'a' to 'j' do if ci in csta then write(ci) else write('_');
   write(' s/b ');
   writeln('a___e_____');
   { these are just compile time tests }
   cste := cstd;
   cstf := ['a', 'b', 'e', 'f'];
   cstg := cstf;

   { sets of enumerated }
   write('Set32: ');
   sena := [];
   for ei := one to ten do if odd(ord(ei)) then sena := sena+[ei];
   for ei := one to ten do if ei in sena then write('1') else write('0');
   write(' s/b ');
   writeln('0101010101');
   write('Set33: ');
   sena := [one, four, five];
   senb := [two, six, ten];
   for ei := one to ten do if ei in sena+senb then write('1') else write('0');
   write(' s/b ');
   writeln('1101110001');
   write('Set34: ');
   sena := [one, two, six, five, seven];
   senb := [two, six, ten];
   for ei := one to ten do if ei in sena*senb then write('1') else write('0');
   write(' s/b ');
   writeln('0100010000');
   write('Set35: ');
   sena := [two, four, seven, eight];
   senb := [one, three, four, eight, ten];
   for ei := one to ten do if ei in sena-senb then write('1') else write('0');
   write(' s/b ');
   writeln('0100001000');
   sena := [four, six, eight, nine];
   senb := [one, four, five, nine];
   senc := [four, six, eight, nine];
   writeln('Set36: ', sena = senb:5, ' s/b false');
   writeln('Set37: ', sena = senc:5, ' s/b true');
   writeln('Set38: ', sena <> senb:5, ' s/b true');
   writeln('Set39: ', sena <> senc:5, ' s/b false');
   sena := [one, two, five, seven, ten];
   senb := [one, five, ten];
   senc := [one, five, ten, six];
   send := [one, two, five, seven, ten];
   writeln('Set40: ', senb <= sena:5, ' s/b true');
   writeln('Set41: ', senb <= send:5, ' s/b true');
   writeln('Set42: ', senc <= sena:5, ' s/b false');
   writeln('Set43: ', sena >= senb:5, ' s/b true');
   writeln('Set44: ', send >= senb:5, ' s/b true');
   writeln('Set45: ', sena >= senc:5, ' s/b false');
   write('Set46: ');
   ei := two;
   sena := [ei, succ(ei)];
   for ei := one to ten do if ei in sena then write('1') else write('0');
   write(' s/b ');
   writeln('0110000000');
   { these are just compile time tests }
   send := [one, two, five];
   sene := send;
   senf := [one, two, five, seven];
   seng := senf;

   { sets of boolean }
   write('Set47: ');
   sba := [];
   for ba := false to true do if odd(ord(ba)) then sba := sba+[ba];
   for ba := false to true do if ba in sba then write('1') else write('0');
   write(' s/b ');
   writeln('01');
   write('Set48: ');
   sba := [false];
   sbb := [true];
   for ba := false to true do if ba in sba+sbb then write('1') else write('0');
   write(' s/b ');
   writeln('11');
   write('Set49: ');
   sba := [false, true];
   sbb := [false];
   for ba := false to true do if ba in sba*sbb then write('1') else write('0');
   write(' s/b ');
   writeln('10');
   write('Set50: ');
   sba := [true, false];
   sbb := [true];
   for ba := false to true do if ba in sba-sbb then write('1') else write('0');
   write(' s/b ');
   writeln('10');
   sba := [true];
   sbb := [false];
   sbc := [true];
   writeln('Set51: ', sba = sbb:5, ' s/b false');
   writeln('Set52: ', sba = sbc:5, ' s/b true');
   writeln('Set53: ', sba <> sbb:5, ' s/b true');
   writeln('Set54: ', sba <> sbc:5, ' s/b false');
   sba := [true, false];
   sbb := [false];
   sbc := [true];
   sbd := [false];
   writeln('Set55: ', sbb <= sba:5, ' s/b true');
   writeln('Set56: ', sbb <= sbd:5, ' s/b true');
   writeln('Set57: ', sbc <= sbb:5, ' s/b false');
   writeln('Set58: ', sba >= sbb:5, ' s/b true');
   writeln('Set59: ', sbd >= sbb:5, ' s/b true');
   writeln('Set60: ', sbb >= sbc:5, ' s/b false');
   write('Set61: ');
   ba := false;
   sba := [ba, succ(ba)];
   for ba := false to true do if ba in sba then write('1') else write('0');
   write(' s/b ');
   writeln('11');
   { these are just compile time tests }
   sbe := sbd;
   sbf := [true];
   sbg := sbf;
   write('set62: ');
   new(pi1);
   new(pi2);
   pi1^ := 3;
   pi2^ := 5;
   write([pi1^..pi2^] = [3..5]:5);
   writeln(' s/b true');
   write('set63: ');
   srx := 1;
   sry := 10;
   for i := 1 to 10 do if i in [srx,sry] then write('1') else write('0');
   writeln(' s/b 1000000001');


   { Sets types: operation battery through every access-path context }
   setcontexts;

{******************************************************************************

                            Pointers

******************************************************************************}

   writeln;
   writeln('******************* Pointers ******************************');
   writeln;

   { pointers to types }
   write('Pointer1:   ');
   new(pti);
   pti^ := 4594;
   writeln(pti^:1, ' s/b 4594');
   write('Pointer2:   ');
   new(ptb);
   ptb^ := true;
   writeln(ptb^:5, ' s/b  true');
   write('Pointer3:   ');
   new(ptb);
   ptb^ := false;
   writeln(ptb^:5, ' s/b false');
   write('Pointer4:   ');
   new(ptc);
   ptc^ := 'p';
   writeln(ptc^, ' s/b p');
   write('Pointer5:   ');
   new(pte);
   pte^ := six;
   writeln(ord(pte^):1, ' s/b 5');
   write('Pointer6:   ');
   new(ptes);
   ptes^ := four;
   writeln(ord(ptes^):1, ' s/b 3');
   write('Pointer7:   ');
   new(pts);
   pts^ := 17;
   writeln(pts^:1, ' s/b 17');
   write('Pointer8:   ');
   new(ptr);
   ptr^ := 1234.5678;
   writeln(ptr^:1:4, ' s/b 1234.5678');
   write('Pointer9:   ');
   new(ptst);
   ptst^ := 'my word is';
   writeln(ptst^, ' s/b my word is');
   write('Pointer10:  ');
   new(pta);
   for i := 1 to 10 do pta^[i] := i+10;
   for i := 10 downto 1 do write(pta^[i]:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   write('Pointer11:   ');
   new(ptrc);
   ptrc^.a := 7234;
   ptrc^.b := 'y';
   writeln(ptrc^.a:1, ' ', ptrc^.b, ' s/b 7234 y');
   write('Pointer12:   ');
   new(ptstc);
   ptstc^ := ['b', 'd', 'i'..'j'];
   for ci := 'a' to 'j' do if ci in ptstc^ then write(ci) else write('_');
   writeln(' s/b _b_d____ij');
   write('Pointer13:  ');
   new(ptp);
   new(ptp^);
   ptp^^ := 3732;
   writeln(ptp^^:1, ' s/b 3732');

   { equality/inequality, nil }
   write('Pointer14:  ');
   pti := nil;
   writeln(pti = nil:5, ' s/b  true');
   write('Pointer15:  ');
   new(pti);
   writeln(pti = nil:5, ' s/b false');
   write('Pointer16:  ');
   pti1 := pti;
   writeln(pti = pti1:5, ' s/b true');
   write('Pointer17:  ');
   pti1 := pti;
   writeln(pti <> pti1:5, ' s/b false');
   write('Pointer18:  ');
   new(pti1);
   writeln(pti = pti1:5, ' s/b false');
   write('Pointer19:  ');
   writeln(pti <> pti1:5, ' s/b  true');
   
   { test dispose takes expression (this one does not print) }
   new(pti2);
   dispose(frp);

   { dynamic allocation stress tests }

   { allocate top to bottom, then free from top to bottom }
   write('Pointer20:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   dispose(ipa);
   dispose(ipb);
   dispose(ipc);
   writeln('done s/b done');

   { allocate top to bottom, then free from bottom to top }

   write('Pointer21:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   dispose(ipc);
   dispose(ipb);
   dispose(ipa);

   { free 2 middle blocks to test coalesce }

   write('Pointer22:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   new(ipd);
   dispose(ipb);
   dispose(ipc);
   dispose(ipa);
   dispose(ipd);
   writeln('done s/b done');

   { free 3 middle blocks to test coalesce }
   write('Pointer23:  ');
   new(ipa);
   new(ipb);
   new(ipc);
   new(ipd);
   new(ipe);
   dispose(ipb);
   dispose(ipd);
   dispose(ipc);
   dispose(ipa);
   dispose(ipe);
   writeln('done s/b done');

   if doptrtortst then begin
   
      { linear torture test }
      writeln('Pointer24:  ');
      for cnt := 1 to 100 do begin

         write(cnt:3, ' '); if (cnt mod 10) = 0 then writeln;
         for i := 1 to 100 do iap[i] := nil;
         for i := 1 to 100 do begin new(iap[i]); iap[i]^ := i end;
         for i := 1 to 100 do if iap[i] = nil then
            writeln('*** bad allocation of block');
         for i := 100 downto 1 do if iap[i]^ <> i then
            writeln('*** bad block content');
         for i := 1 to 100 do begin

            dispose(iap[i]);
            iap[i] := nil;
            for x := 1 to 100 do if iap[x] <> nil then
               if iap[x]^ <> x then
                  writeln('*** bad block content')

         end;

         for i := 1 to 100 do iap[i] := nil;
         for i := 1 to 100 do begin new(iap[i]); iap[i]^ := i end;
         for i := 1 to 100 do if iap[i] = nil then
            writeln('*** bad allocation of block');
         for i := 100 downto 1 do if iap[i]^ <> i then
            writeln('*** bad block content');
         for i := 100 downto 1 do begin

            dispose(iap[i]);
            iap[i] := nil;
            for x := 1 to 100 do if iap[x] <> nil then
               if iap[x]^ <> x then
                  writeln('*** bad block content')

         end

      end;
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');
   
   end else begin
   
      { keep listing equal for compare }
      writeln('Pointer24:  ');
      writeln('  1   2   3   4   5   6   7   8   9  10 ');
      writeln(' 11  12  13  14  15  16  17  18  19  20 ');
      writeln(' 21  22  23  24  25  26  27  28  29  30 ');
      writeln(' 31  32  33  34  35  36  37  38  39  40 ');
      writeln(' 41  42  43  44  45  46  47  48  49  50 ');
      writeln(' 51  52  53  54  55  56  57  58  59  60 ');
      writeln(' 61  62  63  64  65  66  67  68  69  70 ');
      writeln(' 71  72  73  74  75  76  77  78  79  80 ');
      writeln(' 81  82  83  84  85  86  87  88  89  90 ');
      writeln(' 91  92  93  94  95  96  97  98  99 100 ');
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');
   
   end;

   if doptrtortst then begin
   
      rndseq := 1;

      { random block torture test }
      writeln('Pointer25:  ');
      for i := 1 to 100 do iap[i] := nil;
      for cnt2 := 1 to 100 do begin

         write(cnt2:3, ' '); if (cnt2 mod 10) = 0 then writeln;
         for cnt := 1 to 100 do begin

            { allocate random }
            rn := random(1, 100); { choose random pointer }
            new(iap[rn]); { allocate }
            iap[rn]^ := rn; { set number }
            for i := 1 to 100 do if iap[i] <> nil then
               if iap[i]^ <> i then
                  writeln('*** bad block content');

            { deallocate random }
            rn := random(1, 100); { choose random pointer }
            if iap[rn] <> nil then dispose(iap[rn]); { deallocate }
            iap[rn] := nil;
            for i := 1 to 100 do if iap[i] <> nil then
               if iap[i]^ <> i then
                  writeln('*** bad block content');

         end

      end;
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');
      
   end else begin
   
      { keep listing equal for comparision }
      writeln('Pointer25:  ');
      writeln('  1   2   3   4   5   6   7   8   9  10 ');
      writeln(' 11  12  13  14  15  16  17  18  19  20 ');
      writeln(' 21  22  23  24  25  26  27  28  29  30 ');
      writeln(' 31  32  33  34  35  36  37  38  39  40 ');
      writeln(' 41  42  43  44  45  46  47  48  49  50 ');
      writeln(' 51  52  53  54  55  56  57  58  59  60 ');
      writeln(' 61  62  63  64  65  66  67  68  69  70 ');
      writeln(' 71  72  73  74  75  76  77  78  79  80 ');
      writeln(' 81  82  83  84  85  86  87  88  89  90 ');
      writeln(' 91  92  93  94  95  96  97  98  99 100 ');      
      writeln;
      writeln('s/b');
      writeln;
      writeln('  1   2   3   4   5   6   7   8   9  10');
      writeln(' 11  12  13  14  15  16  17  18  19  20');
      writeln(' 21  22  23  24  25  26  27  28  29  30');
      writeln(' 31  32  33  34  35  36  37  38  39  40');
      writeln(' 41  42  43  44  45  46  47  48  49  50');
      writeln(' 51  52  53  54  55  56  57  58  59  60');
      writeln(' 61  62  63  64  65  66  67  68  69  70');
      writeln(' 71  72  73  74  75  76  77  78  79  80');
      writeln(' 81  82  83  84  85  86  87  88  89  90');
      writeln(' 91  92  93  94  95  96  97  98  99  100');      
   
   end;

   { recursive-pointer linked structure: build 1->2->3 and traverse (ISO 6.4.4) }
   write('Pointer26:  ');
   lh := nil;
   for i := 3 downto 1 do begin new(lp); lp^.vl := i; lp^.nxt := lh; lh := lp end;
   lp := lh;
   while lp <> nil do begin write(lp^.vl:1, ' '); lp := lp^.nxt end;
   writeln('s/b 1 2 3');


   { Pointers types: operation battery through every access-path context }
   pointercontexts;

{******************************************************************************

                            Arrays

******************************************************************************}

   writeln;
   writeln('******************* arrays ******************************');
   writeln;

   { single demension, integer index }
   write('Array1:   ');
   for i := 1 to 10 do avi[i] := i+10;
   for i := 10 downto 1 do write(avi[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array2:   ');
   for i := 1 to 10 do pavi[i] := i+10;
   for i := 10 downto 1 do write(pavi[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array3:   ');
   for i := 1 to 10 do avis[i] := i+10;
   for i := 10 downto 1 do write(avis[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array4:   ');
   for i := 1 to 10 do pavis[i] := i+10;
   for i := 10 downto 1 do write(pavis[i]:1, ' ');
   writeln(' s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array5:   ');
   for i := 1 to 10 do avb[i] := odd(i);
   for i := 10 downto 1 do write(avb[i]:5, ' ');
   writeln;
   writeln('    s/b:   false  true false  true false  true false  true false',
           '  true');
   write('Array6:   ');
   for i := 1 to 10 do pavb[i] := odd(i);
   for i := 10 downto 1 do write(pavb[i]:5, ' ');
   writeln;
   writeln('    s/b:   false  true false  true false  true false  true false',
           '  true');
   write('Array7:   ');
   for i := 1 to 10 do avr[i] := i+10+0.12;
   for i := 10 downto 1 do write(avr[i]:1:2, ' ');
   writeln;
   writeln('    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 ',
           '13.12 12.12 11.12');
   write('Array8:   ');
   for i := 1 to 10 do pavr[i] := i+10+0.12;
   for i := 10 downto 1 do write(pavr[i]:1:2, ' ');
   writeln;
   writeln('    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 ',
           '13.12 12.12 11.12');
   write('Array9:   ');
   for i := 1 to 10 do avc[i] := chr(i+ord('a'));
   for i := 10 downto 1 do write(avc[i]:1, ' ');
   writeln('s/b k j i h g f e d c b');
   write('Array10:  ');
   for i := 1 to 10 do pavc[i] := chr(i+ord('a'));
   for i := 10 downto 1 do write(pavc[i]:1, ' ');
   writeln('s/b k j i h g f e d c b');
   write('Array11:  ');
   for i := 1 to 10 do avcs[i] := chr(i+ord('f'));
   for i := 10 downto 1 do write(avcs[i]:1, ' ');
   writeln('s/b p o n m l k j i h g');
   write('Array12:  ');
   for i := 1 to 10 do pavcs[i] := chr(i+ord('f'));
   for i := 10 downto 1 do write(pavcs[i]:1, ' ');
   writeln('s/b p o n m l k j i h g');
   write('Array13:  ');
   for ei := one to ten do ave[ord(ei)+1] := ei;
   for ei := ten downto one do write(ord(ave[ord(ei)+1]):1, ' ');
   writeln('s/b 9 8 7 6 5 4 3 2 1 0');
   write('Array14:  ');
   for ei := one to ten do pave[ord(ei)+1] := ei;
   for ei := ten downto one do write(ord(ave[ord(ei)+1]):1, ' ');
   writeln('s/b 9 8 7 6 5 4 3 2 1 0');
   write('Array15:  ');
   for ei := three to six do aves[ord(ei)+1] := ei;
   for ei := six downto three do write(ord(aves[ord(ei)+1]):1, ' ');
   writeln('s/b 5 4 3 2');
   write('Array16:  ');
   for ei := three to six do paves[ord(ei)+1] := ei;
   for ei := six downto three do write(ord(paves[ord(ei)+1]):1, ' ');
   writeln('s/b 5 4 3 2');
   write('Array17:  ');
   for i := 1 to 10 do avs[i] := [chr(i+ord('a'))];
   for i := 10 downto 1 do
      for ci := 'a' to 'z' do if ci in avs[i] then write(ci, ' ');
   writeln('s/b k j i h g f e d c b');
   write('Array18:  ');
   for i := 1 to 10 do pavs[i] := [chr(i+ord('a'))];
   for i := 10 downto 1 do
      for ci := 'a' to 'z' do if ci in pavs[i] then write(ci, ' ');
   writeln('s/b k j i h g f e d c b');
   write('Array19:  ');
   for i := 1 to 10 do
      begin avrc[i].a := i+10; avrc[i].b := chr(i+ord('a')) end;
   for i := 10 downto 1 do write(avrc[i].a:1, ' ', avrc[i].b, ' ');
   writeln;
   writeln('     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b');
   write('Array20:  ');
   for i := 1 to 10 do
      begin pavrc[i].a := i+10; pavrc[i].b := chr(i+ord('a')) end;
   for i := 10 downto 1 do write(pavrc[i].a:1, ' ', pavrc[i].b, ' ');
   writeln;
   writeln('     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b');
   write('Array21:  ');
   for i := 1 to 10 do begin rewrite(avf[i]); writeln(avf[i], i+10) end;
   for i := 10 downto 1 do
      begin reset(avf[i]); readln(avf[i], x); write(x:1, ' ') end;
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array22:  ');
   for i := 1 to 10 do begin rewrite(pavf[i]); writeln(pavf[i], i+10) end;
   for i := 10 downto 1 do
      begin reset(pavf[i]); readln(pavf[i], x); write(x:1, ' ') end;
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array23:  ');
   for i := 1 to 10 do begin new(avp[i]); avp[i]^ := i+10 end;
   for i := 10 downto 1 do write(avp[i]^:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   write('Array24:  ');
   for i := 1 to 10 do begin new(pavp[i]); pavp[i]^ := i+10 end;
   for i := 10 downto 1 do write(pavp[i]^:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');

   { indexing tests }
   write('Array25:  ');
   for ba := false to true do bia[ba] := ord(ba)+10;
   for ba := true downto false do write(bia[ba]:1, ' ');
   writeln(' s/b 11 10');
   write('Array26:  ');
   for ba := false to true do pbia[ba] := ord(ba)+10;
   for ba := true downto false do write(pbia[ba]:1, ' ');
   writeln(' s/b 11 10');
   write('Array27:  ');
   for ci := 'a' to 'j' do cia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(cia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array28:  ');
   for ci := 'a' to 'j' do pcia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(pcia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array29:  ');
   for ci := 'a' to 'j' do csia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(csia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array30:  ');
   for ci := 'a' to 'j' do pcsia[ci] := ord(ci);
   for ci := 'j' downto 'a' do write(chr(pcsia[ci]), ' ');
   writeln(' s/b  j i h g f e d c b a');
   write('Array31:  ');
   for ei := one to ten do eia[ei] := ord(ei);
   for ei := ten downto one do write(eia[ei]:1, ' ');
   writeln(' s/b  9 8 7 6 5 4 3 2 1 0');
   write('Array32:  ');
   for ei := one to ten do peia[ei] := ord(ei);
   for ei := ten downto one do write(peia[ei]:1, ' ');
   writeln(' s/b  9 8 7 6 5 4 3 2 1 0');
   write('Array33:  ');
   for ei := two to six do eia[ei] := ord(ei);
   for ei := six downto two do write(eia[ei]:1, ' ');
   writeln(' s/b  5 4 3 2 1');
   write('Array34:  ');
   for ei := two to six do peia[ei] := ord(ei);
   for ei := six downto two do write(peia[ei]:1, ' ');
   writeln(' s/b  5 4 3 2 1');

   { multidementional arrays }
   writeln('Array35:');
   z := 0;
   for x := 1 to 10 do
      for y := 1 to 10 do begin da[y, x] := z; z := z + 1 end;
   for x := 1 to 10 do
   begin
      for y := 1 to 10 do write(da[x][y]:2, ' ');
      writeln;
   end;
   writeln('s/b');
   writeln('0 10 20 30 40 50 60 70 80 90');
   writeln('1 11 21 31 41 51 61 71 81 91');
   writeln('2 12 22 32 42 52 62 72 82 92');
   writeln('3 13 23 33 43 53 63 73 83 93');
   writeln('4 14 24 34 44 54 64 74 84 94');
   writeln('5 15 25 35 45 55 65 75 85 95');
   writeln('6 16 26 36 46 56 66 76 86 96');
   writeln('7 17 27 37 47 57 67 77 87 97');
   writeln('8 18 28 38 48 58 68 78 88 98');
   writeln('9 19 29 39 49 59 69 79 89 99');
   writeln('Array36: ');
   t := 0;
   for i := 1 to 2 do
      for x := 1 to 2 do
         for y := 1 to 2 do
            for z := 1 to 2 do
               for q := 1 to 2 do
                  for n := 1 to 2 do
                     begin mdar[i][x, y, z][q][n] := t; t := t+1 end;
   for i := 2 downto 1 do
      for x := 2 downto 1 do
         for y := 2 downto 1 do begin

            for z := 2 downto 1 do
               for q := 2 downto 1 do
                  for n := 2 downto 1 do write(mdar[i, x][y, z][q][n]:2, ' ');
            writeln;

         end;
   writeln('s/b:');
   writeln('63 62 61 60 59 58 57 56');
   writeln('55 54 53 52 51 50 49 48');
   writeln('47 46 45 44 43 42 41 40');
   writeln('39 38 37 36 35 34 33 32');
   writeln('31 30 29 28 27 26 25 24');
   writeln('23 22 21 20 19 18 17 16');
   writeln('15 14 13 12 11 10  9  8');
   writeln(' 7  6  5  4  3  2  1  0');

   { assignments }
   writeln('Array37: ');
   pavc := 'hello, guy';
   writeln(pavc, ' s/b hello, guy');
   writeln('Array38: ');
   for i := 1 to 10 do avi[i] := i+10;
   avi2 := avi;
   for i := 10 downto 1 do write(avi2[i]:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   writeln('Array39: ');
   t := 0;
   for i := 1 to 2 do
      for x := 1 to 2 do
         for y := 1 to 2 do
            for z := 1 to 2 do
               for q := 1 to 2 do
                  for n := 1 to 2 do
                     begin mdar[i][x, y, z][q][n] := t; t := t+1 end;
   mdar2 := mdar;
   for i := 2 downto 1 do
      for x := 2 downto 1 do
         for y := 2 downto 1 do begin

            for z := 2 downto 1 do
               for q := 2 downto 1 do
                  for n := 2 downto 1 do write(mdar2[i, x][y, z][q][n]:2, ' ');
            writeln;

         end;
   writeln('s/b:');
   writeln('63 62 61 60 59 58 57 56');
   writeln('55 54 53 52 51 50 49 48');
   writeln('47 46 45 44 43 42 41 40');
   writeln('39 38 37 36 35 34 33 32');
   writeln('31 30 29 28 27 26 25 24');
   writeln('23 22 21 20 19 18 17 16');
   writeln('15 14 13 12 11 10  9  8');
   writeln(' 7  6  5  4  3  2  1  0');

   { transfer procedures }
   writeln('Array40: ');
   for i := 1 to 10 do pavi[i] := i+10;
   unpack(pavi, avi, 1);
   for i := 10 downto 1 do write(avi[i]:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   writeln('Array41: ');
   for i := 1 to 10 do avi[i] := i+20;
   pack(avi, 1, pavi);
   for i := 10 downto 1 do write(pavi[i]:1, ' ');
   writeln('s/b 30 29 28 27 26 25 24 23 22 21');
   writeln('Array42: ');
   for i := 1 to 10 do pavi[i] := i+30;
   unpack(pavi, cia, 'g');
   for ci := 'p' downto 'g' do write(cia[ci]:1, ' ');
   writeln('s/b 40 39 38 37 36 35 34 33 32 31');
   writeln('Array43: ');
   x := 1;
   for ci := 'a' to 'z' do begin cia[ci] := x; x := x+1 end;
   pack(cia, 'm', pavi);
   for i := 10 downto 1 do write(pavi[i]:1, ' ');
   writeln('s/b 22 21 20 19 18 17 16 15 14 13');


   { Arrays types: operation battery through every access-path context }
   stringcontexts;
   arraycontexts;

{******************************************************************************

                            Records

******************************************************************************}

   writeln;
   writeln('******************* records ******************************');
   writeln;

   { types in records }
   writeln('Record1:   ');
   arec.i := 64;
   arec.b := false;
   arec.c := 'j';
   arec.e := two;
   arec.es := four;
   arec.s := 12;
   arec.r := 4545.12e-32;
   arec.st := 'what ? who';
   for i := 1 to 10 do arec.a[i] := i+20;
   arec.rc.a := 2324;
   arec.rc.b := 'y';
   arec.stc := ['b'..'e', 'i'];
   new(arec.p);
   arec.p^ := 8454;
   writeln(arec.i:1, ' ', arec.b:5, ' ', arec.c:1, ' ', ord(arec.e):1, ' ',
           ord(arec.es):1,
           ' ', arec.s:1, ' ', arec.r:15, ' ', arec.st);
   for i := 1 to 10 do write(arec.a[i]:1, ' '); writeln;
   writeln(arec.rc.a:1, ' ', arec.rc.b:1);
   for ci := 'a' to 'j' do if ci in arec.stc then write(ci) else write('_');
   writeln;
   writeln(arec.p^:1);
   writeln('s/b:');
   writeln('64 false j 1 3 12  4.54512000e-29 what ? who');
   writeln('21 22 23 24 25 26 27 28 29 30');
   writeln('2324 y');
   writeln('_bcde___i_');
   writeln('8454');
   writeln('Record2:   ');
   parec.i := 64;
   parec.b := false;
   parec.c := 'j';
   parec.e := two;
   parec.es := four;
   parec.s := 12;
   parec.r := 4545.12e-32;
   parec.st := 'what ? who';
   for i := 1 to 10 do parec.a[i] := i+20;
   parec.rc.a := 2324;
   parec.rc.b := 'y';
   parec.stc := ['b'..'e', 'i'];
   new(parec.p);
   parec.p^ := 8454;
   writeln(parec.i:1, ' ', parec.b:5, ' ', parec.c:1, ' ', ord(parec.e):1, ' ',
           ord(parec.es):1,
           ' ', parec.s:1, ' ', parec.r:15, ' ', parec.st);
   for i := 1 to 10 do write(parec.a[i]:1, ' '); writeln;
   writeln(parec.rc.a:1, ' ', parec.rc.b:1);
   for ci := 'a' to 'j' do if ci in parec.stc then write(ci) else write('_');
   writeln;
   writeln(parec.p^:1);
   writeln('s/b:');
   writeln('64 false j 1 3 12  4.54512000e-29 what ? who');
   writeln('21 22 23 24 25 26 27 28 29 30');
   writeln('2324 y');
   writeln('_bcde___i_');
   writeln('8454');

   { types in variants, and border clipping }
   write('Record3:   ');
   vra.i := 873;
   vra.vt := vti;
   vra.a := 427;
   vra.vdi := 235;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdi:1, ' ', vra.a:1);
   writeln(' s/b 873 0 235 427');
   write('Record4:   ');
   vra.i := 873;
   vra.vt := vtb;
   vra.b := 427;
   vra.vdb := true;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdb:5, ' ', vra.b:1);
   writeln(' s/b 873 1  true 427');
   write('Record5:   ');
   vra.i := 873;
   vra.vt := vtc;
   vra.c := 427;
   vra.vdc := 'f';
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdc, ' ', vra.c:1);
   writeln(' s/b 873 2 f 427');
   write('Record6:   ');
   vra.i := 873;
   vra.vt := vte;
   vra.d := 427;
   vra.vde := nine;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', ord(vra.vde):1, ' ', vra.d:1);
   writeln(' s/b 873 3 8 427');
   write('Record7:   ');
   vra.i := 873;
   vra.vt := vtes;
   vra.e := 427;
   vra.vdes := four;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', ord(vra.vdes):1, ' ', vra.e:1);
   writeln(' s/b 873 4 3 427');
   write('Record8:   ');
   vra.i := 873;
   vra.vt := vts;
   vra.f := 427;
   vra.vds := 12;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vds:1, ' ', vra.f:1);
   writeln(' s/b 873 5 12 427');
   write('Record9:   ');
   vra.i := 873;
   vra.vt := vtr;
   vra.g := 427;
   vra.vdr := 8734.8389;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdr:1:4, ' ', vra.g:1);
   writeln(' s/b 873 6 8734.8389 427');
   write('Record10:  ');
   vra.i := 873;
   vra.vt := vtst;
   vra.h := 427;
   vra.vdst := 'this one ?';
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdst, ' ', vra.h:1);
   writeln(' s/b 873 7 this one ? 427');
   write('Record11:  ');
   vra.i := 873;
   vra.vt := vta;
   vra.j := 427;
   for i := 1 to 10 do vra.vda[i] := i+10;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ');
   for i := 10 downto 1 do write(vra.vda[i]:1, ' ');
   writeln(vra.j:1);
   writeln('      s/b:  873 8 20 19 18 17 16 15 14 13 12 11 427');
   write('Record12:  ');
   vra.i := 873;
   vra.vt := vtrc;
   vra.k := 427;
   vra.vdrc.a := 2387;
   vra.vdrc.b := 't';
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdrc.a:1, ' ', vra.vdrc.b, ' ',
         vra.k:1);
   writeln(' s/b:  873 9 2387 t 427');
   write('Record13:  ');
   vra.i := 873;
   vra.vt := vtstc;
   vra.l := 427;
   vra.vdstc := ['b'..'g', 'i'];
   write(vra.i:1, ' ', ord(vra.vt):1, ' ');
   for ci := 'j' downto 'a' do if ci in vra.vdstc then write(ci) else write('_');
   writeln(' ', vra.l:1);
   writeln('      s/b:  873 10 _i_gfedcb_ 427');
   write('Record14:  ');
   vra.i := 873;
   vra.vt := vtp;
   vra.m := 427;
   new(vra.vdp);
   vra.vdp^ := 2394;
   write(vra.i:1, ' ', ord(vra.vt):1, ' ', vra.vdp^:1, ' ', vra.m:1);
   writeln(' s/b 873 11 2394 427');

   { types of variant tags }
   write('Record15:  ');
   vvrs.vt := 10;
   vvrs.vi := 2343;
   write(vvrs.vt:1, ' ', vvrs.vi:1);
   writeln(' s/b 10 2343');
   write('Record16:  ');
   vvrs.vt := 19;
   vvrs.vb := true;
   write(vvrs.vt:1, ' ', vvrs.vb:5);
   writeln(' s/b 19  true');
   write('Record17:  ');
   vvrb.vt := true;
   vvrb.vi := 2343;
   write(vvrb.vt:5, ' ', vvrb.vi:1);
   writeln(' s/b  true 2343');
   write('Record18:  ');
   vvrb.vt := false;
   vvrb.vb := true;
   write(vvrb.vt:5, ' ', vvrb.vb:5);
   writeln(' s/b false  true');
   write('Record19:  ');
   vvre.vt := three;
   vvre.vi := 2343;
   write(ord(vvre.vt):1, ' ', vvre.vi:1);
   writeln(' s/b 2 2343');
   write('Record20:  ');
   vvre.vt := eight;
   vvre.vb := true;
   write(ord(vvre.vt):1, ' ', vvre.vb:5);
   writeln(' s/b 7  true');
   write('Record21:  ');
   vvres.vt := four;
   vvres.vi := 2343;
   write(ord(vvres.vt):1, ' ', vvres.vi:1);
   writeln(' s/b 3 2343');
   write('Record22:  ');
   vvres.vt := five;
   vvres.vb := true;
   write(ord(vvres.vt):1, ' ', vvres.vb:5);
   writeln(' s/b 4  true');
   { change to another tag constant in same variant }
   write('Record23:  ');
   vvrs.vt := 10;
   vvrs.vi := 42;
   i := vvrs.vi;
   vvrs.vt := 11;
   i := vvrs.vi;
   writeln(i:1, ' s/b 42');

   { nested records }
   write('Record24:  ');
   nvr.i := 1;
   nvr.r.i := 2;
   nvr.r.r.i := 3;
   nvr.r.r.r.i := 4;
   nvr.r.r.r.r.i := 5;
   nvr.r.r.r.r.r.i := 6;
   nvr.r.r.r.r.r.r.i := 7;
   nvr.r.r.r.r.r.r.r.i := 8;
   nvr.r.r.r.r.r.r.r.r.i := 9;
   nvr.r.r.r.r.r.r.r.r.r.i := 10;
   writeln(nvr.i:1, ' ',
           nvr.r.i:1, ' ',
           nvr.r.r.i:1, ' ',
           nvr.r.r.r.i:1, ' ',
           nvr.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.r.i:1, ' ',
           's/b 1 2 3 4 5 6 7 8 9 10');

   { 'with' statements }
   write('Record25:  ');
   with nvr do begin

      i := 10;
      with r do begin

         i := 9;
         with r do begin

            i := 8;
            with r do begin

               i := 7;
               with r do begin

                  i := 6;
                  with r do begin

                     i := 5;
                     with r do begin

                        i := 4;
                        with r do begin

                           i := 3;
                           with r do begin

                              i := 2;
                              with r do begin

                                 i := 2;
                                 with r do begin

                                    i := 1

                                 end

                              end

                           end

                        end

                     end

                  end

               end

            end

         end

      end

   end;
   writeln(nvr.i:1, ' ',
           nvr.r.i:1, ' ',
           nvr.r.r.i:1, ' ',
           nvr.r.r.r.i:1, ' ',
           nvr.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.r.i:1, ' ',
           's/b 10 9 8 7 6 5 4 3 2 1');
   write('Record26:  ');
   with nvr, r, r, r, r, r, r, r, r, r do i := 76;
   writeln(nvr.i:1, ' ',
           nvr.r.i:1, ' ',
           nvr.r.r.i:1, ' ',
           nvr.r.r.r.i:1, ' ',
           nvr.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.i:1, ' ',
           nvr.r.r.r.r.r.r.r.r.r.i:1, ' ',
           's/b 10 9 8 7 6 5 4 3 2 76');
   write('Record27:  ');
   new(rpa);
   with rpa^ do begin

      i := 1;
      with rc do b := 'g'

   end;
   writeln(rpa^.i:1, ' ', rpa^.rc.b, ' s/b 1 g');
   write('Record28:  ');
   for i := 1 to 10 do with ara[i] do a := i+10;
   for i := 10 downto 1 do with ara[i] do write(a:1, ' ');
   writeln('s/b 20 19 18 17 16 15 14 13 12 11');
   write('Record29: ');
   new(rpb, false, true);
   rpb^.i := 42;
   rpb^.b := false;
   rpb^.q := true;
   rpb^.r := 12.34;
   write(rpb^.i:1, ' ', rpb^.b, ' ', rpb^.q, ' ', rpb^.r);
   writeln(' s/b 42 False True 1.234000000000000e+01');
   dispose(rpb, false, true);
   write('Record30: ');
   new(rpc, 10);
   rpc^.vt := 10;
   rpc^.vi := 185;
   rpc^.vt := 14;
   write(rpc^.vi:1);
   writeln(' s/b 185');
   dispose(rpc, 15);

   { whole-record (aggregate) assignment (ISO 6.4.3.3) }
   write('Record31:  ');
   rcs1.a := 42; rcs1.b := 'x'; rcs2 := rcs1;
   writeln(rcs2.a:1, ' ', rcs2.b, ' s/b 42 x');


   { Records types: operation battery through every access-path context }
   recordcontexts;

{******************************************************************************

                            Files

******************************************************************************}

if testfile then begin

   writeln;
   writeln('******************* files ******************************');
   writeln;

   { file base types }
   write('File1:   ');
   rewrite(fi);
   for i := 1 to 10 do write(fi, i+10);
   reset(fi);
   for i := 1 to 10 do begin read(fi, x); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File2:   ');
   rewrite(pfi);
   for i := 1 to 10 do write(pfi, i+10);
   reset(pfi);
   for i := 1 to 10 do begin read(pfi, x); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File3:   ');
   rewrite(fb);
   for i := 1 to 10 do write(fb, odd(i));
   reset(fb);
   for i := 1 to 10 do begin read(fb, ba); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File4:   ');
   rewrite(pfb);
   for i := 1 to 10 do write(pfb, odd(i));
   reset(pfb);
   for i := 1 to 10 do begin read(pfb, ba); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File5:   ');
   rewrite(fc);
   for ci := 'a' to 'j' do write(fc, ci);
   reset(fc);
   for ci := 'a' to 'j' do begin read(fc, ca); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File6:   ');
   rewrite(pfc);
   for ci := 'a' to 'j' do write(pfc, ci);
   reset(pfc);
   for ci := 'a' to 'j' do begin read(pfc, ca); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File7:   ');
   rewrite(fe);
   for ei := one to ten do write(fe, ei);
   reset(fe);
   for ei := one to ten do begin read(fe, ea); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');
   write('File8:   ');
   rewrite(pfe);
   for ei := one to ten do write(pfe, ei);
   reset(pfe);
   for ei := one to ten do begin read(pfe, ea); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');

   { types written to text }
   writeln('File9:');
   rewrite(ft);
   x := 7384;
   writeln(ft, x:1);
   writeln(ft, 8342:1);
   ba := true;
   writeln(ft, ba:5);
   writeln(ft, false:5);
   ca := 'm';
   writeln(ft, ca);
   writeln(ft, 'q');
   ra := 1234.5678e-3;
   writeln(ft, ra:15);
   writeln(ft, ra:1:7);
   writeln(ft, 5689.4321e-2:15);
   writeln(ft, 9383.7632e-4:1:8);
   s := 'hi there !';
   writeln(ft, s);
   writeln(ft, s:5);
   writeln(ft, s:15);
   reset(ft); get(ft); cc := ft^; reset(ft);
   while not eof(ft) do begin

      if eoln(ft) then begin

         readln(ft);
         writeln

      end else begin

         read(ft, ci);
         write(ci)

      end

   end;
   writeln('s/b:');
   writeln('7384');
   writeln('8342');
   writeln(' true');
   writeln('false');
   writeln('m');
   writeln('q');
   writeln(' 1.2345678000e+00');
   writeln('1.2345678');
   writeln(' 5.6894321000e+01');
   writeln('0.93837632');
   writeln('hi there !');
   writeln('hi th');
   writeln('     hi there !');

   { types read from text }
   writeln('file10:');
   reset(ft);
   readln(ft, y);
   writeln(y:1);
   readln(ft, y);
   writeln(y:1);
   readln(ft);
   readln(ft);
   readln(ft, ci);
   writeln(ci);
   readln(ft, ci);
   writeln(ci);
   readln(ft, rb);
   writeln(rb:15);
   readln(ft, rb);
   writeln(rb:15);
   readln(ft, rb);
   writeln(rb:15);
   readln(ft, rb);
   writeln(rb:15);
   writeln('s/b:');
   writeln('7384');
   writeln('8342');
   writeln('m');
   writeln('q');
   writeln(' 1.2345678000e+00');
   writeln(' 1.2345678000e+00');
   writeln(' 5.6894321000e+01');
   writeln(' 9.3837632000e-01');

   { line and file endings in text }
   writeln('file11:');
   rewrite(ft);
   writeln(ft, 'how now');
   writeln(ft, 'brown cow');
   reset(ft);
   write('''');
   while not eof(ft) do begin

      if eoln(ft) then write('<eoln>');
      read(ft, ca);
      write(ca)

   end;
   write('''');
   writeln(' s/b ''how now<eoln> brown cow<eoln> ''');
   writeln('file12:');
   rewrite(ft);
   writeln(ft, 'too much');
   write(ft, 'too soon');
   reset(ft);
   write('''');
   while not eof(ft) do begin

      if eoln(ft) then write('<eoln>');
      read(ft, ca);
      write(ca)

   end;
   write('''');
   writeln(' s/b ''too much<eoln> too soon<eoln> ''');

   { get/put and buffer variables }
   write('File13:   ');
   rewrite(fi);
   for i := 1 to 10 do begin fi^ := i+10; put(fi) end;
   reset(fi);
   for i := 1 to 10 do begin x := fi^; get(fi); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File14:   ');
   rewrite(pfi);
   for i := 1 to 10 do begin pfi^ := i+10; put(pfi) end;
   reset(pfi);
   for i := 1 to 10 do begin x := pfi^; get(pfi); write(x:1, ' ') end;
   writeln('s/b 11 12 13 14 15 16 17 18 19 20');
   write('File15:   ');
   rewrite(fb);
   for i := 1 to 10 do begin fb^ := odd(i); put(fb) end;
   reset(fb);
   for i := 1 to 10 do begin ba := fb^; get(fb); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File16:   ');
   rewrite(pfb);
   for i := 1 to 10 do begin pfb^ := odd(i); put(pfb) end;
   reset(pfb);
   for i := 1 to 10 do begin ba := pfb^; get(pfb); write(ba:5, ' ') end;
   writeln;
   writeln('   s/b:    true false  true false  true false  true false  true ',
           'false');
   write('File17:   ');
   rewrite(fc);
   for ci := 'a' to 'j' do begin fc^ := ci; put(fc) end;
   reset(fc);
   for ci := 'a' to 'j' do begin ca := fc^; get(fc); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File18:   ');
   rewrite(pfc);
   for ci := 'a' to 'j' do begin pfc^ := ci; put(pfc) end;
   reset(pfc);
   for ci := 'a' to 'j' do begin ca := pfc^; get(pfc); write(ca, ' ') end;
   writeln('s/b a b c d e f g h i j');
   write('File19:   ');
   rewrite(fe);
   for ei := one to ten do begin fe^ := ei; put(fe) end;
   reset(fe);
   for ei := one to ten do begin ea := fe^; get(fe); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');
   write('File20:   ');
   rewrite(pfe);
   for ei := one to ten do begin pfe^ := ei; put(pfe) end;
   reset(pfe);
   for ei := one to ten do begin ea := pfe^; get(pfe); write(ord(ea):1, ' ') end;
   writeln('s/b 0 1 2 3 4 5 6 7 8 9');
   write('File21:   ');
   rewrite(ft);
   writeln(ft, '50');
   reset(ft);
   read(ft, srx);
   write(srx:1);
   writeln(' s/b ', 50:1);
   write('File22:   ');
   rewrite(ft);
   writeln(eof(ft), ' s/b true');
   { transfer a record value through a file of records (ISO 6.4.3.5/6.6.5.2) }
   write('File23:   ');
   rewrite(frc); rcs1.a := 17; rcs1.b := 'q'; write(frc, rcs1);
   reset(frc); read(frc, rcs2);
   writeln(rcs2.a:1, ' ', rcs2.b, ' s/b 17 q');

end;


   { Item 2: files contained in arrays, records and pointers }
   filecontainers;

{******************************************************************************

                         Procedures and functions

******************************************************************************}

   writeln;
   writeln('************ Procedures and functions ******************');
   writeln;
   write('ProcedureFunction1:   ');
   x := 45; y := 89;
   junk1(x, y);
   writeln(' s/b 45 89');
   write('ProcedureFunction2:   ');
   x := 45; junk2(x);
   writeln(x:1, ' s/b 46');
   write('ProcedureFunction3:   ');
   s := 'total junk';
   junk3(s);
   writeln(' s/b total junk');
   write('ProcedureFunction4:   ');
   s := 'total junk';
   junk4(s);
   writeln(' s/b tota? junk');
   writeln('                      ', s, ' s/b total junk');
   write('ProcedureFunction5:   ');
   writeln(junk5(34):1, ' s/b 35');
   write('ProcedureFunction6:   ');
   i := junk7(10, 9, 8);
   writeln(' ', i:1);
   writeln('s/b:   10 9 8 6 5 4 3 2 1 78');
   writeln('ProcedureFunction7:');
   for i := 1 to 10 do ai[i] := i+10;
   arec.i := 64;
   arec.b := false;
   arec.c := 'j';
   arec.e := two;
   arec.es := four;
   arec.s := 12;
   arec.r := 4545.12e-32;
   arec.st := 'what ? who';
   for i := 1 to 10 do arec.a[i] := i+20;
   arec.rc.a := 2324;
   arec.rc.b := 'y';
   arec.stc := ['b'..'e', 'i'];
   new(arec.p);
   arec.p^ := 8454;
   vrec.a := 23487;
   vrec.b := 'n';
   vrec.c := false;
   vrec.d := 'help me123';
   new(ip);
   ip^ := 734;
   junk8(93, true, 'k', eight, five, 10, 3.1414, 'hello, guy', ai, arec, vrec,
         ['a'..'d', 'h'], ip);
   writeln('s/b:');
   writeln('93  true k 7 4 10  3.14140000e+00 hello, guy');
   writeln('11 12 13 14 15 16 17 18 19 20');
   writeln('64 false j 1 3 12  4.54512000e-29 what ? who');
   writeln('21 22 23 24 25 26 27 28 29 30');
   writeln('2324 y');
   writeln('_bcde___i_');
   writeln('8454');
   writeln('23487 n false');
   writeln('help me123');
   writeln('abcd___h__');
   writeln('734');
   write('ProcedureFunction8:   ');
   junk9(junk10, junk11);
   writeln(' s/b 9834 8383 j 744');
   write('ProcedureFunction9:   ');
   junk12(junk13, junk11);
   writeln(' s/b 942');
   write('ProcedureFunction10:   ');
   junk14;
   writeln(' s/b 62 76');
   write('ProcedureFunction11:   ');
   junk17(junk16, 52);
   writeln(' s/b 52');
   write('ProcedureFunction12:   ');
   junk19;
   writeln(' s/b a');
   write('ProcedureFunction13:   ');
   writeln(junk20:1, ' s/b 37');
   write('ProcedureFunction14:   ');
   writeln(junk21:1, ' s/b 35');

end.
