{*******************************************************************************
*                                                                              *
*                           GENERATE C OBJECT FILE                             *
*                                                                              *
* LICENSING:                                                                   *
*                                                                              *
* Copyright (c) 1996, 2018, Scott A. Franco                                    *
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
* FUNCTION:                                                                    *
*                                                                              *
* Generates a C constant data array with embedded code from a pmach generated  *
* object file.                                                                 *
*                                                                              *
*******************************************************************************}

program pmach(input,output,prd,prr);

label 99;

var prd,prr: text;

procedure wrtnum(var tf: text; v: integer; r: integer; f: integer; lz: boolean);
const digmax = 32;
var p,i,x,d,t,n: integer; sgn: boolean;
    digits: packed array [1..digmax] of char;
function digit(d: integer): char;
var c: char;
begin
  if d < 10 then c := chr(d+ord('0'))
  else c := chr(d-10+ord('A'));
  digit := c
end;
begin sgn := false;
   if (r = 10) and (v < 0) then begin sgn := true; v := -v; lz := false end;
   if r = 16 then n := 8
   else if r = 10 then n := 10
   else if r = 8 then n := 11
   else n := 32;
   for i := 1 to digmax do digits[i] := '0';
   { adjust signed radix }
   if (r = 16) and (v < 0) then begin
     v := v+1+maxint; { convert number to 31 bit unsigned }
     t := v div 268435456{ $1000_0000}+8; { extract high digit }
     digits[8] := digit(t); { place high digit }
     v := v mod 268435456{ $1000_0000}; { remove digit }
     n := 7 { set number of digits-1 }
   end else if (r = 8) and (v < 0) then begin
     v := v+1+maxint; { convert number to 31 bit unsigned }
     t := v div 1073741824{ &10_000_000_000}+2; { extract high digit }
     digits[11] := digit(t); { place high digit }
     v := v mod 1073741824{ $10_000_000_000}; { remove digit }
     n := 10 { set number of digits-1 }
   end else if (r = 2) and (v < 0) then begin
     v := v+1+maxint; { convert number to 31 bit unsigned }
     t := v div 268435456{ $1000_0000}+8; { extract high digit }
     digits[32] := '1'; { place high digit }
     n := 31 { set number of digits-1 }
   end;
   p := 1;
   for i := 1 to n do begin
      d := v div p mod r; { extract digit }
      digits[i] := digit(d); { place }
      p := p*r
   end;
   i := digmax; 
   while (digits[i] = '0') and (i > 1) do i := i-1; { find sig digits }
   if sgn then begin digits[i+1] := '-'; i := i+1 end;
   if not lz then for x := digmax downto i+1 do digits[x] := ' '; 
   if i > f then f := i;
   if f > digmax then begin 
     for i := 1 to f-8 do if lz then write(tf, '0') else write(tf, ' '); 
     f := digmax 
   end;
   { print result }
   for i := f downto 1 do write(tf, digits[i])
end;

{ load code file }

procedure xlate;

   var  ad, ad2: integer;
        i, l, cs, csc, b: integer;
        c: char;

   procedure errorl; (*error in loading*)
   begin writeln;
      writeln('*** Invalid code deck');
      goto 99
   end; (*errorl*)

   procedure readhex(var v: integer; d: integer);
   var i: integer; c: char;
   begin v:= 0;
     for i := 1 to d do begin
       if eof(prd) or eoln(prd) then errorl;
       read(prd, c); if not (c in ['0'..'9', 'A'..'F']) then errorl;
       if c in ['0'..'9'] then v:= v*16+ord(c)-ord('0')
       else v := v*16+ord(c)-ord('A')+10
     end
   end;
       
begin (*xlate*)
  writeln(prr, '= {');
  ad := 0; l := 1;
  while not eof(prd) and (l > 0) do begin
    read(prd, c); if c <> ':' then errorl;
    readhex(l, 2); readhex(i, 16); ad2 := i; 
    if (ad <> ad2) and (l > 0) then errorl;
    cs := 0; 
    for i := 1 to l do 
      begin 
        readhex(b, 2); write(prr, '0x'); wrtnum(prr, b, 16, 2, true); 
        write(prr, ', '); cs := (cs+b) mod 256; ad := ad+1 
      end;
    readhex(csc, 2); if cs <> csc then errorl;
    writeln(prr);
    readln(prd)
  end;
  writeln(prr, '};');
  write(prr, '#define PCTOP 0x'); wrtnum(prr, ad, 16, 8, true); writeln(prr);
end;

begin { main }

  writeln('Mach program object deck to C constant table converter');
  reset(prd);
  rewrite(prr);
  xlate;
  writeln('Program complete');
  99:;

end.
