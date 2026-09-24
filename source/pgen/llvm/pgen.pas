{*******************************************************************************
*                                                                              *
*                           Portable Pascal compiler                           *
*                           ************************                           *
*                                                                              *
*                                 Pascal P6                                    *
*                                                                              *
*                            LLVM IR code generator                            *
*                                                                              *
* Translates the P6 intermediate code to textual LLVM IR (.ll), for clang or   *
* llc to compile. It consumes the same intermediate as the AMD64 generator     *
* (the amd64_sysv calling convention deck): pcom lays out each routine's frame *
* for that convention (display, register pad, locals), and this generator     *
* builds an identical frame in one alloca, so every frame offset in the deck   *
* is honored as is. The differences from the AMD64 generator are all in what   *
* replaces the machine:                                                        *
*                                                                              *
*   - Registers: LLVM allocates them. Expression trees are walked into SSA    *
*     values, one per node result (two for fat pointer results).              *
*   - The frame pointer chain: the x86 ENTER display is reproduced from the   *
*     caller's frame base, which the caller leaves in the runtime global      *
*     psystem_llvm_sl just before the call; the callee copies the display     *
*     entries it needs from it, exactly as ENTER does.                        *
*   - Set and structure function results: the caller's result frame (SFR) is *
*     an alloca whose address it leaves in psystem_llvm_sfr; the callee maps  *
*     the positive frame offsets pcom uses for it onto that pointer. Globals  *
*     rather than hidden arguments, so that the C thunks of the library      *
*     modules and the generated routines share one signature.                 *
*   - Exceptions and non-local goto: setjmp/longjmp in the C runtime          *
*     (psystem_llvm.c), replacing the assembly throw/unwind and the frame     *
*     pointer restores.                                                       *
*   - Module initialization chain: each object registers its entry in the    *
*     psystem_llvm_mods section; the runtime calls the entries in link       *
*     order, replacing the fall-through into the next object.                 *
*                                                                              *
* Functions are buffered and written when complete, because the return type   *
* of a routine is only known from its return instruction, and the frame size  *
* from labels defined after the body.                                          *
*                                                                              *
*******************************************************************************}

program pgen(output);

joins services;

uses endian,      { endian mode }
     mpb,         { machine parameter block }
     version,     { current version number }
     parcmd,      { command line parsing }
     registers,   { register definitions (none for this target) }
     independent; { cpu independent module }

label 99;

const

   maxll = 40000;  { line buffer length: a routine name carries its type digest,
                     which runs to thousands of characters for a record-heavy
                     signature (the P2/P4 compilers), and an alias or declare
                     line holds the whole name }
   maxpar = 128;   { maximum parameters of a routine }
   maxcase = 4000; { maximum case table entries }
   maxlvl = 32;    { maximum nesting level }

   { psystem error codes (psystem.c numbering) }
   ecValueOutOfRange          = 0;
   ecCaseValueNotFound        = 2;
   ecZeroDivide               = 3;
   ecNilPointerDereference    = 5;
   ecIntegerValueOverflow     = 19;
   ecRealArgumentTooLarge     = 68;
   ecBooleanOperatorOfNegative = 69;
   ecInvalidDivisorToMod      = 70;
   ecVariantNotActive         = 93;
   ecContainerMismatch        = 103;
   ecInvalidContainerLevel    = 104;

   { frame layout (see the AMD64 generator's mst): below the display come the
     integer register pad (7 quads: 6 parameter registers and the function
     result) and the real register pad (6 doubles); overflow parameters sit
     above the saved frame pointer, mark and return address }
   padsize = 104;  { 7*8 + 6*8 }
   ovfbase = 40;   { first overflow parameter frame offset }
   sfrslot = 0;    { frame offset holding the result frame pointer }
   ipjslot = 8;    { frame offset holding the non-local goto table pointer }

   { pseudo value numbers }
   vfb   = -1; { the frame base pointer %fb }
   vsfr  = -2; { the result frame pointer %sfr }
   vnull = -3; { the null pointer }

type

   lineptr = ^lineety;
   lineety = record s: pstring; next: lineptr end;
   linelst = record first, last: lineptr end;
   nameptr = ^nameety;
   nameety = record name: pstring; sig: pstring; par: pstring; next: nameptr end;
   ipjptr = ^ipjety;
   ipjety = record name: pstring; key: integer; next: ipjptr end;
   parclass = (pcint, pcreal, pcpair);
   calptr = ^calety;
   calety = record k: integer; next: calptr end;
   stripptr = ^stripety;
   stripety = record
      blk: pblock;        { the block the strip belongs to }
      pro, body: linelst; { its prologue and code }
      calsites: calptr;   { the local calls it makes }
      next: stripptr
   end;
   parsymtab = array [1..maxpar] of psymbol;

var

   lbuf: packed array [1..maxll] of char; { line buffer }
   ll: integer; { line length }
   inpro: boolean; { emitting to the prologue }
   prolst, bodylst: linelst; { function prologue and body }
   fnopen: boolean;  { a function is being generated }
   fndone: boolean;  { its return has been seen }
   blkopen: boolean; { the current basic block has no terminator }
   fnname: pstring;  { function entry name }
   fnalias: nameptr; { extra entry labels, become aliases }
   pendlab: nameptr; { code labels defined between functions }
   fnlvl: integer;   { function nesting level }
   fnretk: integer;  { return kind: 0 void, 1 i64, 2 double }
   fnlcl: pstring;   { locals space label }
   fnstrip: boolean; { the function is a module entry strip (no frame) }
   fnparn: integer;  { number of parameters }
   fnparc: array [1..maxpar] of parclass; { parameter classes }
   fnparoff: array [1..maxpar] of integer; { parameter spill offsets }
   fnovf: integer;   { overflow parameter bytes }
   fnblk: pblock;    { the block of the function }
   vn: integer;      { SSA value counter }
   labn: integer;    { generated label counter }
   declst: nameptr;  { declared external functions }
   defsyms: nameptr; { global symbols defined here }
   refrtn: nameptr;  { external routines referenced by address }
   refsyms: nameptr; { global symbols referenced }
   ipjpend: ipjptr;  { non-local goto targets not yet defined }
   ipjlst: ipjptr;   { non-local goto targets defined in this function }
   xjptbl: pstring;  { case table label awaited }
   xjpidx: integer;  { case index value }
   intbl: boolean;   { collecting a case table }
   tblent: array [1..maxcase] of pstring; { case table entries }
   tblcnt: integer;
   modsym: pstring;  { the module name as a symbol }
   instrip: boolean; { generating an initializer strip region }
   stripn: integer;  { strip ordinal, prefixes its value names }
   curstrip: stripptr; { the strip being generated }
   striplst: stripptr; { the strips generated, by block }
   calcnt: integer;  { local call site counter }
   fncals: calptr;   { the local call sites of this function }
   fnstrips: linelst; { the strip code spliced into this function }
   donelst: calptr;  { shared call results produced in this function }
   tmps: pstring;    { scratch string }

{******************************************************************************

Abort

******************************************************************************}

override procedure abort;

begin

   goto 99

end;

{******************************************************************************

Name lists

******************************************************************************}

function innames(np: nameptr; view s: string): boolean;

var f: boolean;

begin

   f := false;
   while np <> nil do begin

      if compcp(np^.name^, s) then f := true;
      np := np^.next

   end;
   innames := f

end;

procedure addname(var np: nameptr; view s: string);

var p: nameptr;

begin

   if not innames(np, s) then begin

      new(p); p^.name := extract(s, 1, len(s)); p^.sig := nil;
      p^.next := np; np := p

   end

end;

{******************************************************************************

Line buffer and emission

The line is built in lb, then ol appends it to the function's prologue or body
list, or writes it directly when no function is open (module level).

******************************************************************************}

procedure oc(c: char);

begin

   if ll >= maxll then error('Output line too long');
   ll := ll+1; lbuf[ll] := c

end;

procedure os(view s: string);

var i: integer;

begin

   for i := 1 to max(s) do oc(s[i])

end;

{ integer, decimal }
procedure oi(i: integer);

var d: packed array [1..24] of char; n: integer;

begin

   if i = -maxint-1 then os('-9223372036854775808')
   else begin

      if i < 0 then begin oc('-'); i := -i end;
      n := 0;
      repeat n := n+1; d[n] := chr(ord('0')+i mod 10); i := i div 10 until i = 0;
      while n > 0 do begin oc(d[n]); n := n-1 end

   end

end;

{ value operand }
procedure ov(v: integer);

begin

   if v = vfb then os('%fb')
   else if v = vsfr then os('%sfr')
   else if v = vnull then os('null')
   else if instrip then begin os('%s'); oi(stripn); oc('_'); oi(v) end
   else begin os('%v'); oi(v) end

end;

{ quoted name }
procedure oq(view s: string);

var i: integer;

begin

   oc('"');
   for i := 1 to max(s) do oc(s[i]);
   oc('"')

end;

{ label reference }
procedure olab(view s: string);

begin

   os('label %'); oq(s)

end;

procedure addline(var l: linelst);

var p: lineptr;

begin

   new(p); p^.s := extract(lbuf, 1, ll); p^.next := nil;
   if l.first = nil then l.first := p else l.last^.next := p;
   l.last := p

end;

{ end of line: place it }
procedure ol;

var i: integer;

begin

   if instrip then begin

      if inpro then addline(curstrip^.pro) else addline(curstrip^.body)

   end else if fnopen then begin

      if inpro then addline(prolst) else addline(bodylst)

   end else begin

      for i := 1 to ll do write(prr, lbuf[i]);
      writeln(prr)

   end;
   ll := 0

end;

{ append a copy of one list to another }
procedure catlines(var d: linelst; var s: linelst);

var p, q: lineptr;

begin

   p := s.first;
   while p <> nil do begin
      new(q); q^.s := p^.s; q^.next := nil;
      if d.first = nil then d.first := q else d.last^.next := q;
      d.last := q;
      p := p^.next
   end

end;

procedure wrtlines(var l: linelst);

var p: lineptr;

begin

   p := l.first;
   while p <> nil do begin writeln(prr, p^.s^); p := p^.next end;
   l.first := nil; l.last := nil

end;

{ the line buffer as a string }
function lbstr: pstring;

begin

   lbstr := extract(lbuf, 1, ll)

end;

function newv: integer;

begin

   vn := vn+1; newv := vn

end;

{ new generated label, in the given buffer }
procedure genlab(view pfx: string; var s: pstring);

var b: packed array [1..64] of char; d: packed array [1..20] of char;
    i, n, k, x: integer;

begin

   labn := labn+1;
   n := 0;
   for i := 1 to len(pfx) do begin n := n+1; b[n] := pfx[i] end;
   n := n+1; b[n] := '.';
   x := labn; k := 0;
   repeat k := k+1; d[k] := chr(ord('0')+x mod 10); x := x div 10 until x = 0;
   while k > 0 do begin n := n+1; b[n] := d[k]; k := k-1 end;
   s := extract(b, 1, n)

end;

{ mark the block terminated }
procedure term;

begin

   blkopen := false

end;

{ start an instruction line: the block must be open }
procedure oins;

var s: pstring;

begin

   if not blkopen then begin

      { code after a terminator with no label: give it a block }
      genlab('dead', s);
      oq(s^); oc(':'); ol;
      blkopen := true

   end;
   os('  ')

end;

{ define a basic block label }
procedure defblk(view s: string);

begin

   if blkopen then begin oins; os('br '); olab(s); ol; term end;
   oq(s); oc(':'); ol;
   blkopen := true

end;

{******************************************************************************

Constant expression operands

******************************************************************************}

{ hexadecimal of the bit pattern of a real }
procedure ohexreal(r: real);

var ro: record case boolean of
          true:  (rv: real);
          false: (iv: integer)
        end;
    v, d, i: integer;
    h: packed array [1..16] of char;
    neg: boolean;

begin

   ro.rv := r; v := ro.iv;
   neg := v < 0;
   if neg then v := v-(-maxint-1); { v+2^63, now positive }
   for i := 16 downto 1 do begin
      d := v mod 16; v := v div 16;
      if (i = 1) and neg then d := d+8;
      if d < 10 then h[i] := chr(ord('0')+d) else h[i] := chr(ord('A')+d-10)
   end;
   os('0x');
   for i := 1 to 16 do oc(h[i])

end;

{ symbol address as an i64 constant expression }
procedure osymadr(view s: string);

begin

   os('ptrtoint (ptr @'); oq(s); os(' to i64)');
   addname(refsyms, s)

end;

{******************************************************************************

Declarations

External functions are declared once, with the signature of the first call.

******************************************************************************}

procedure declfn(view name: string; view ret: string; view par: string);

var p: nameptr;

begin

   if not innames(declst, name) then begin

      new(p); p^.name := extract(name, 1, len(name));
      p^.sig := extract(ret, 1, len(ret));
      p^.par := extract(par, 1, len(par));
      p^.next := declst; declst := p

   end

end;

{ value of a defined value label }
function labelvalof(s: pstring): integer;

var x: labelrg; f: boolean;

begin

   { search the label table by reference name }
   f := false; x := 1; labelvalof := 0;
   while (x <= maxlabel) and not f do begin
      if labeltab[x].ref <> nil then
         if compcp(labeltab[x].ref^, s^) then begin
            f := true;
            if labeltab[x].st = defined then labelvalof := labeltab[x].val
            else error('Label value not defined')
         end;
      x := x+1
   end;
   if not f then error('Label not found')

end;

{******************************************************************************

Frame addressing

The frame base %fb points at the top of the negative area, where the AMD64
frame pointer would. Display entry k (the frame base of level k) is at
%fb-8k, the current level's own entry included. Positive offsets from ovfbase
are the overflow parameters (copied into the frame) and, past them, the
caller's result frame, reached through the pointer saved at sfrslot.

******************************************************************************}

{ the class of a parameter from its type digest: reals, fat pointer
  pairs (procedure parameters and containers), everything else an integer
  word (values, addresses) }
function parclassof(sp: psymbol): parclass;

var c: char;

begin

   c := ' ';
   if sp^.digest <> nil then if max(sp^.digest^) > 0 then c := sp^.digest^[1];
   if c = 'n' then parclassof := pcreal
   else if (c = 'q') or (c = 'v') then parclassof := pcpair
   else parclassof := pcint

end;

{ the parameter symbols of a block in declaration order: the block's list
  was built by prepending, so it is walked backwards through an index }
procedure parsyms(bp: pblock; var n: integer; var tab: parsymtab);

var sp: psymbol; i, j: integer; t: psymbol;

begin

   n := 0;
   if bp <> nil then begin
      sp := bp^.symbols;
      while sp <> nil do begin
         if sp^.styp = stparam then begin
            if n >= maxpar then error('Too many parameters');
            n := n+1; tab[n] := sp
         end;
         sp := sp^.next
      end;
      { reverse }
      i := 1; j := n;
      while i < j do begin t := tab[i]; tab[i] := tab[j]; tab[j] := t; i := i+1; j := j-1 end
   end

end;

{ overflow parameter bytes of a block: the parameters past the six integer
  and six real register slots, assigned in declaration order as pgen and
  pcom do }
function blkovf(bp: pblock): integer;

var n, k, ipc, fpc, ovf: integer; tab: parsymtab;

begin

   ovf := 0; ipc := 0; fpc := 0;
   parsyms(bp, n, tab);
   for k := 1 to n do case parclassof(tab[k]) of
      pcint:  begin ipc := ipc+1; if ipc > 6 then ovf := ovf+8 end;
      pcreal: begin fpc := fpc+1; if fpc > 6 then ovf := ovf+8 end;
      pcpair: begin ipc := ipc+2; if ipc > 6 then ovf := ovf+16 end
   end;
   blkovf := ovf

end;

{ find the block at a given level in the current chain }
function blkatlvl(p: integer): pblock;

var bp, fp: pblock;

begin

   fp := nil; bp := blkstk;
   while bp <> nil do begin
      if bp^.lvl = p then fp := bp;
      bp := bp^.next
   end;
   blkatlvl := fp

end;

{ gep on a pointer value }
function gep(b: integer; off: integer): integer;

var v: integer;

begin

   if off = 0 then v := b
   else begin

      v := newv;
      oins; ov(v); os(' = getelementptr i8, ptr '); ov(b); os(', i64 '); oi(off); ol

   end;
   gep := v

end;

{ frame base of level p as a pointer value }
function frameof(p: integer): integer;

var v, a: integer;

begin

   if fnstrip then frameof := vnull
   else if p = fnlvl then frameof := vfb
   else begin

      a := gep(vfb, -8*p);
      v := newv;
      oins; ov(v); os(' = load ptr, ptr '); ov(a); ol;
      frameof := v

   end

end;

{ address of a frame location as a pointer value }
function locadr(p, q: integer): integer;

var b, v, a, ovf: integer;

begin

   b := frameof(p);
   if p = fnlvl then ovf := fnovf else ovf := blkovf(blkatlvl(p));
   if q >= ovfbase+ovf then begin

      { the result frame: through the pointer saved in the frame }
      a := gep(b, sfrslot);
      v := newv;
      oins; ov(v); os(' = load ptr, ptr '); ov(a); ol;
      locadr := gep(v, q-ovfbase-ovf)

   end else locadr := gep(b, q)

end;

{ address of a global as a pointer value }
function gbladr(q: integer): integer;

var v: integer;

begin

   v := newv;
   oins; ov(v); os(' = getelementptr i8, ptr @globals_start, i64 '); oi(q); ol;
   gbladr := v

end;

{ address of a symbol as a pointer value }
function symadr(view s: string): integer;

var v: integer;

begin

   addname(refsyms, s);
   v := newv;
   oins; ov(v); os(' = getelementptr i8, ptr @'); oq(s); os(', i64 0'); ol;
   symadr := v

end;

{ pointer from an i64 value }
function i2p(x: integer): integer;

var v: integer;

begin

   v := newv;
   oins; ov(v); os(' = inttoptr i64 '); ov(x); os(' to ptr'); ol;
   i2p := v

end;

{ i64 from a pointer value }
function p2i(x: integer): integer;

var v: integer;

begin

   v := newv;
   oins; ov(v); os(' = ptrtoint ptr '); ov(x); os(' to i64'); ol;
   p2i := v

end;

{ loads and stores by kind: 'i' quad, 'b' byte zero extended, 'r' double }
function ld(k: char; a: integer): integer;

var v, t: integer;

begin

   v := newv;
   case k of
      'i': begin oins; ov(v); os(' = load i64, ptr '); ov(a); ol end;
      'r': begin oins; ov(v); os(' = load double, ptr '); ov(a); ol end;
      'b': begin
         t := newv;
         oins; ov(t); os(' = load i8, ptr '); ov(a); ol;
         oins; ov(v); os(' = zext i8 '); ov(t); os(' to i64'); ol
      end
   end;
   ld := v

end;

procedure st(k: char; x, a: integer);

var t: integer;

begin

   case k of
      'i': begin oins; os('store i64 '); ov(x); os(', ptr '); ov(a); ol end;
      'r': begin oins; os('store double '); ov(x); os(', ptr '); ov(a); ol end;
      'b': begin
         t := newv;
         oins; ov(t); os(' = trunc i64 '); ov(x); os(' to i8'); ol;
         oins; os('store i8 '); ov(t); os(', ptr '); ov(a); ol
      end
   end

end;

{ memory copy of n bytes, addresses as i64 values }
procedure memcpy(d, s: integer; n: integer);

var pd, ps: integer;

begin

   pd := i2p(d); ps := i2p(s);
   oins; os('call void @llvm.memmove.p0.p0.i64(ptr '); ov(pd); os(', ptr ');
   ov(ps); os(', i64 '); oi(n); os(', i1 false)'); ol

end;

{ memory copy with a computed length }
procedure memcpyv(d, s: integer; n: integer);

var pd, ps: integer;

begin

   pd := i2p(d); ps := i2p(s);
   oins; os('call void @llvm.memmove.p0.p0.i64(ptr '); ov(pd); os(', ptr ');
   ov(ps); os(', i64 '); ov(n); os(', i1 false)'); ol

end;

{ a prologue alloca of n bytes, returning the pointer value }
function alloca(n: integer; view tag: string): integer;

var v: integer;

begin

   v := newv;
   inpro := true;
   os('  '); ov(v); os(' = alloca [ '); oi(n); os(' x i8 ], align 16 ; '); os(tag); ol;
   os('  call void @llvm.memset.p0.i64(ptr '); ov(v); os(', i8 0, i64 '); oi(n); os(', i1 false)'); ol;
   inpro := false;
   alloca := v

end;

{ current line number as an operand }
procedure oline;

begin

   oi(sline)

end;

{ runtime error call }
procedure emiterr(code: integer);

begin

   oins; os('call void @psystem_errore(i64 ptrtoint (ptr @modnam to i64), i64 ');
   oline; os(', i64 '); oi(code); oc(')'); ol

end;

{ conditional error: if the i1 value c is true, raise the error }
procedure errif(c: integer; code: integer);

var lf, lc: pstring;

begin

   genlab('err', lf); genlab('cont', lc);
   oins; os('br i1 '); ov(c); os(', '); olab(lf^); os(', '); olab(lc^); ol; term;
   defblk(lf^);
   emiterr(code);
   oins; os('br '); olab(lc^); ol; term;
   defblk(lc^)

end;

{******************************************************************************

Function open and close

******************************************************************************}

{ the parameters of a block, in declaration order, from its parameter
  symbols, with the frame slot each arrives in.

  The type digest cannot tell a var parameter from a value one (a var real
  is an address, an integer class word), but the frame offset pcom assigned
  can: an offset in the integer register pad is an integer word, one in the
  real register pad a real, a positive one an overflow slot (typed as a word
  of bits, whatever its class), and one below the pads is the local copy of
  a structured value parameter, whose address arrives in the next integer
  slot in declaration order. }
procedure parparms(bp: pblock);

var n, k, ii, oi2, off, ilo, ihi, rlo, rhi: integer; tab: parsymtab; c: parclass;

begin

   parsyms(bp, n, tab);
   fnparn := n;
   ilo := -(8*fnlvl+56); ihi := -(8*fnlvl+8);
   rlo := -(8*fnlvl+104); rhi := -(8*fnlvl+64);
   ii := 0; oi2 := 0;
   for k := 1 to n do begin

      off := tab[k]^.off; c := parclassof(tab[k]);
      if c = pcpair then begin
         fnparc[k] := pcpair; fnparoff[k] := off;
         if off >= ovfbase then oi2 := oi2+2 else ii := ii+2
      end else if (off >= rlo) and (off <= rhi) then begin
         fnparc[k] := pcreal; fnparoff[k] := off
      end else if (off >= ilo) and (off <= ihi) then begin
         fnparc[k] := pcint; fnparoff[k] := off; ii := ii+1
      end else if off >= ovfbase then begin
         fnparc[k] := pcint; fnparoff[k] := off; oi2 := oi2+1
      end else begin
         { the local copy of a structured value: the address is in the next
           integer slot }
         fnparc[k] := pcint;
         ii := ii+1;
         if ii <= 6 then fnparoff[k] := -(8*fnlvl+8*(7-ii))
         else begin fnparoff[k] := ovfbase+8*oi2; oi2 := oi2+1 end
      end

   end

end;

{ the qualified name of a block: the chain of enclosing block names }
procedure oblklong(bp: pblock);

begin

   if bp <> nil then begin
      if bp^.parent <> nil then begin oblklong(bp^.parent); oc('.') end;
      os(bp^.name^)
   end

end;

{ open a function }
procedure openfn(view name: string; strip: boolean);

begin

   if fnopen then error('System error: function already open');
   fnopen := true; fndone := false; blkopen := true;
   fnname := extract(name, 1, len(name));
   fnalias := nil;
   prolst.first := nil; prolst.last := nil;
   bodylst.first := nil; bodylst.last := nil;
   fnstrip := strip; fnlvl := 0; fnretk := 0; fnlcl := nil;
   fnparn := 0; fnovf := 0; fnblk := blkstk;
   vn := 0;
   ipjlst := nil;
   fncals := nil; fnstrips.first := nil; fnstrips.last := nil;
   donelst := nil

end;

{ write the parameter list of a function definition, with names, or its
  type list for an alias }
procedure oparlist(named: boolean);

var k: integer;

begin

   oc('(');
   for k := 1 to fnparn do begin

      if k > 1 then os(', ');
      case fnparc[k] of
         pcint:  begin os('i64'); if named then begin os(' %p'); oi(k) end end;
         pcreal: begin os('double'); if named then begin os(' %p'); oi(k) end end;
         pcpair: begin
            os('i64');
            if named then begin os(' %p'); oi(k); os('a') end;
            os(', i64');
            if named then begin os(' %p'); oi(k); os('b') end
         end
      end

   end;
   oc(')')

end;

{ write the prologue of a framed function: the frame alloca, the display,
  the result frame pointer, the parameter spills and the non-local goto table }
procedure wrtprologue;

var neg, pos, k, n: integer; ip: ipjptr;

begin

   if not fnstrip then begin

      { the caller's frame and result frame arrive through the runtime, so
        the signature stays plain: C thunks and Pascal routines alike }
      writeln(prr, '  %sl = load ptr, ptr @psystem_llvm_sl');
      writeln(prr, '  %sfr = load ptr, ptr @psystem_llvm_sfr');
      neg := 8*fnlvl+padsize+128;
      if fnlcl <> nil then neg := neg+labelvalof(fnlcl);
      { round to 16 }
      neg := ((neg+15) div 16)*16;
      pos := ovfbase+fnovf+16;
      writeln(prr, '  %frame = alloca i8, i64 ', neg+pos:1, ', align 16');
      { the frame is cleared, as the AMD64 generator clears the locals: file
        variables, among others, rely on it }
      writeln(prr, '  call void @llvm.memset.p0.i64(ptr %frame, i8 0, i64 ', neg+pos:1, ', i1 false)');
      writeln(prr, '  %fb = getelementptr i8, ptr %frame, i64 ', neg:1);
      { display: copy the caller's entries below ours, then ours }
      for k := 1 to fnlvl-1 do begin
         writeln(prr, '  %dl', k:1, ' = getelementptr i8, ptr %sl, i64 ', -8*k:1);
         writeln(prr, '  %dv', k:1, ' = load ptr, ptr %dl', k:1);
         writeln(prr, '  %dd', k:1, ' = getelementptr i8, ptr %fb, i64 ', -8*k:1);
         writeln(prr, '  store ptr %dv', k:1, ', ptr %dd', k:1)
      end;
      writeln(prr, '  %dd', fnlvl:1, ' = getelementptr i8, ptr %fb, i64 ', -8*fnlvl:1);
      writeln(prr, '  store ptr %fb, ptr %dd', fnlvl:1);
      { result frame pointer }
      writeln(prr, '  %sfs = getelementptr i8, ptr %fb, i64 ', sfrslot:1);
      writeln(prr, '  store ptr %sfr, ptr %sfs');
      { parameters into the frame slots they belong in }
      for k := 1 to fnparn do case fnparc[k] of

         pcint: begin
            writeln(prr, '  %ps', k:1, ' = getelementptr i8, ptr %fb, i64 ', fnparoff[k]:1);
            writeln(prr, '  store i64 %p', k:1, ', ptr %ps', k:1)
         end;

         pcreal: begin
            writeln(prr, '  %ps', k:1, ' = getelementptr i8, ptr %fb, i64 ', fnparoff[k]:1);
            writeln(prr, '  store double %p', k:1, ', ptr %ps', k:1)
         end;

         pcpair: begin
            writeln(prr, '  %ps', k:1, 'a = getelementptr i8, ptr %fb, i64 ', fnparoff[k]:1);
            writeln(prr, '  store i64 %p', k:1, 'a, ptr %ps', k:1, 'a');
            writeln(prr, '  %ps', k:1, 'b = getelementptr i8, ptr %fb, i64 ', fnparoff[k]+8:1);
            writeln(prr, '  store i64 %p', k:1, 'b, ptr %ps', k:1, 'b')
         end

      end;
      { non-local goto targets: a table of (label, jmp_buf) in the frame, and
        a setjmp per target landing on its label }
      writeln(prr, '  %ips = getelementptr i8, ptr %fb, i64 ', ipjslot:1);
      if ipjlst = nil then writeln(prr, '  store ptr null, ptr %ips')
      else begin
         n := 0; ip := ipjlst;
         while ip <> nil do begin n := n+1; ip := ip^.next end;
         writeln(prr, '  %ipt = alloca [ ', n*16+8:1, ' x i8 ], align 16');
         writeln(prr, '  store ptr %ipt, ptr %ips');
         writeln(prr, '  store i64 ', n:1, ', ptr %ipt');
         n := 0; ip := ipjlst;
         while ip <> nil do begin
            writeln(prr, '  %ipj', n:1, ' = alloca [ 208 x i8 ], align 16');
            writeln(prr, '  %ipe', n:1, ' = getelementptr i8, ptr %ipt, i64 ', 8+n*16:1);
            writeln(prr, '  store i64 ', ip^.key:1, ', ptr %ipe', n:1);
            writeln(prr, '  %ipf', n:1, ' = getelementptr i8, ptr %ipt, i64 ', 16+n*16:1);
            writeln(prr, '  store ptr %ipj', n:1, ', ptr %ipf', n:1);
            n := n+1; ip := ip^.next
         end
      end

   end;
   if fncals <> nil then begin
      writeln(prr, '  %calra = alloca [ 16 x i64 ], align 16');
      writeln(prr, '  %calsp = alloca i64, align 8');
      writeln(prr, '  store i64 0, ptr %calsp')
   end;
   wrtlines(prolst);
   { the setjmps come after every alloca }
   if not fnstrip and (ipjlst <> nil) then begin
      n := 0; ip := ipjlst;
      while ip <> nil do begin
         writeln(prr, '  %ipr', n:1, ' = call i32 @_setjmp(ptr %ipj', n:1, ')');
         writeln(prr, '  %ipc', n:1, ' = icmp ne i32 %ipr', n:1, ', 0');
         writeln(prr, '  br i1 %ipc', n:1, ', label %"', ip^.name^, '", label %"ipj.next', n:1, '"');
         writeln(prr, '"ipj.next', n:1, '":');
         n := n+1; ip := ip^.next
      end
   end;
   writeln(prr, '  br label %body')

end;

{ close the open function: write it out }
procedure closefn;

var np: nameptr; cp: calptr;

begin

   if not fnopen then error('System error: no function open');
   if not fndone then begin
      { no return seen: end it }
      if blkopen then begin oins; os('unreachable'); ol; term end
   end;
   write(prr, 'define ');
   case fnretk of 0: write(prr, 'void'); 1: write(prr, 'i64'); 2: write(prr, 'double') end;
   write(prr, ' @'); write(prr, '"', fnname^, '"');
   if fnstrip then writeln(prr, '() {')
   else begin ll := 0; oparlist(true); write(prr, lbuf:ll); writeln(prr, ' {'); ll := 0 end;
   writeln(prr, 'prologue:');
   wrtprologue;
   writeln(prr, 'body:');
   wrtlines(bodylst);
   if fncals <> nil then begin
      { the return of a local call: pop the site and branch to it }
      writeln(prr, '"calsw":');
      writeln(prr, '  %cs0 = load i64, ptr %calsp');
      writeln(prr, '  %cs1 = sub i64 %cs0, 1');
      writeln(prr, '  store i64 %cs1, ptr %calsp');
      writeln(prr, '  %cs2 = getelementptr i64, ptr %calra, i64 %cs1');
      writeln(prr, '  %cs3 = load i64, ptr %cs2');
      writeln(prr, '  switch i64 %cs3, label %"calsw.bad" [');
      cp := fncals;
      while cp <> nil do begin
         writeln(prr, '    i64 ', cp^.k:1, ', label %"calret.', cp^.k:1, '"');
         cp := cp^.next
      end;
      writeln(prr, '  ]');
      writeln(prr, '"calsw.bad":');
      writeln(prr, '  unreachable')
   end;
   wrtlines(fnstrips);
   writeln(prr, '}');
   addname(defsyms, fnname^);
   { aliases for the other names of this routine }
   np := fnalias;
   while np <> nil do begin
      write(prr, '@"', np^.name^, '" = alias ');
      case fnretk of 0: write(prr, 'void'); 1: write(prr, 'i64'); 2: write(prr, 'double') end;
      if fnstrip then write(prr, ' ()')
      else begin ll := 0; oparlist(false); write(prr, ' '); write(prr, lbuf:ll); ll := 0 end;
      writeln(prr, ', ptr @"', fnname^, '"');
      addname(defsyms, np^.name^);
      np := np^.next
   end;
   writeln(prr);
   fnopen := false; fndone := false; blkopen := false

end;

{ splice the initializer strips of the current block into the open
  function: their allocas into its prologue, their code after its body, and
  their local call sites into its dispatch }
procedure splicestrips;

var sp: stripptr; cp, nc: calptr;

begin

   sp := striplst;
   while sp <> nil do begin

      if sp^.blk = blkstk then begin
         catlines(prolst, sp^.pro);
         catlines(fnstrips, sp^.body);
         cp := sp^.calsites;
         while cp <> nil do begin
            new(nc); nc^.k := cp^.k; nc^.next := fncals; fncals := nc;
            cp := cp^.next
         end
      end;
      sp := sp^.next

   end

end;

{ finalize a completed function if one is pending }
procedure finfn;

begin

   if fnopen and fndone then closefn

end;

{ at a module's end: labels defined after the last routine name the module
  end, which the initialization chain calls to reach the next module; each
  becomes a stub that continues the chain }
procedure endstubs;

var np: nameptr;

begin

   finfn;
   np := pendlab;
   while np <> nil do begin
      writeln(prr, 'define void @"', np^.name^, '"() {');
      writeln(prr, '  call void @psystem_llvm_nextmod()');
      writeln(prr, '  ret void');
      writeln(prr, '}');
      addname(defsyms, np^.name^);
      np := np^.next
   end;
   pendlab := nil

end;

{******************************************************************************

Labels

******************************************************************************}

{ the number of a label modnam.N }
function labelkey(s: pstring): integer;

var i, v: integer;

begin

   v := 0; i := 1;
   while i <= max(s^) do begin
      if s^[i] = '.' then v := 0
      else if s^[i] in ['0'..'9'] then v := v*10+ord(s^[i])-ord('0');
      i := i+1
   end;
   labelkey := v

end;

procedure flushtbl; forward;

{ a label defined in this function that is a non-local goto target moves
  from the pending list to the function's list }
procedure claimipj(view s: string);

var ip, lp, np: ipjptr;

begin

   ip := ipjpend; lp := nil;
   while ip <> nil do begin

      np := ip^.next;
      if compcp(ip^.name^, s) then begin
         if lp = nil then ipjpend := np else lp^.next := np;
         ip^.next := ipjlst; ipjlst := ip
      end else lp := ip;
      ip := np

   end

end;

override procedure deflabel(x: labelrg; pc: boolean);

var np: nameptr;

begin

   if pc then begin

      if intbl then flushtbl;
      if instrip or (fnopen and not fndone) then begin

         defblk(labeltab[x].ref^);
         if not instrip then claimipj(labeltab[x].ref^);
         { a case table label: the entries follow }
         if xjptbl <> nil then
            if compcp(xjptbl^, labeltab[x].ref^) then begin
               intbl := true; tblcnt := 0; xjptbl := nil
            end

      end else begin

         { between functions: an entry label of the next routine }
         new(np); np^.name := labeltab[x].ref; np^.sig := nil;
         np^.next := pendlab; pendlab := np

      end

   end
   { value labels are read from the label table when needed }

end;

{******************************************************************************

Case tables

The xjp branches to the table label; the ujp/ujc lines that follow it are
collected and become one switch in that block.

******************************************************************************}

procedure flushtbl;

var i: integer; lf: pstring;

begin

   intbl := false;
   genlab('casefail', lf);
   oins; os('switch i64 '); ov(xjpidx); os(', '); olab(lf^); os(' ['); ol;
   for i := 1 to tblcnt do begin
      os('    i64 '); oi(i-1); os(', ');
      if tblent[i] = nil then olab(lf^) else olab(tblent[i]^);
      ol
   end;
   os('  ]'); ol; term;
   defblk(lf^);
   emiterr(ecCaseValueNotFound);
   oins; os('unreachable'); ol; term

end;

{******************************************************************************

Hooks: header, symbols, lines, files, variant tables, constants, globals

******************************************************************************}

{ the intermediate line echo, as an IR comment }
override procedure emitecho;

var i: integer;

begin

   if fnopen or instrip then os('  ');
   os('; '); oi(sline); os(': '); oi(iline); os(': ');
   for i := 1 to inplen do oc(inplin[i]);
   ol

end;

override procedure emithdr;

begin

   writeln(prr, '; Pascal-P6 LLVM IR');
   writeln(prr)

end;

override procedure emitline(x: integer; pend: boolean);

begin

   if fnopen or instrip then begin os('  ; line '); oi(x); ol end

end;

override procedure emitfile(view srcfil, n, e: string);

begin

   writeln(prr, 'source_filename = "', srcfil:*, '"');
   writeln(prr)

end;

override procedure emitsym(bp: pblock; sp: psymbol; k: char);

var fl: integer;

begin

   { global symbols are exported as aliases into the globals area, under the
     long block-qualified name that other modules reference }
   if k = 'g' then begin

      ll := 0;
      os('@'); oc('"');
      fl := 0;
      { the long name: module.symbol }
      if bp <> nil then begin os(bp^.name^); oc('.') end;
      os(sp^.name^); oc('"');
      os(' = alias i8, ptr getelementptr (i8, ptr @globals_start, i64 ');
      oi(sp^.off); oc(')');
      { module level text, whatever is being generated }
      writeln(prr, lbuf:ll); ll := 0;
      { record the name as defined }
      os(bp^.name^); oc('.'); os(sp^.name^);
      tmps := lbstr; addname(defsyms, tmps^); ll := 0

   end

end;

override procedure emitvartab(x: labelrg; view vt: vartabty; vl: integer);

var i: integer;

begin

   { defined as a global constant array, not as a code label }
   labeltab[x].st := defined; labeltab[x].val := labelvalue;
   putlabel(x); labeltab[x].blk := blkstk;
   write(prr, '@"', labeltab[x].ref^, '" = private constant [ ', vl+1:1, ' x i64 ] [ i64 ', vl:1);
   for i := 1 to vl do write(prr, ', i64 ', vt[i]:1);
   writeln(prr, ' ]');
   addname(defsyms, labeltab[x].ref^)

end;

function hexd(d: integer): char;

begin

   if d < 10 then hexd := chr(ord('0')+d) else hexd := chr(ord('A')+d-10)

end;

{ write a byte as an LLVM string escape }
procedure obyte(b: integer);

begin

   if (b >= 32) and (b < 127) and (b <> ord('"')) and (b <> 92) then
      write(prr, chr(b))
   else write(prr, chr(92), hexd(b div 16), hexd(b mod 16))

end;

override procedure emitcst;

var cp: cstptr; i: integer; sb: pstring;
    r: record case boolean of
         true:  (s: settype);
         false: (b: packed array [1..setsize] of byte)
       end;
    ro: record case boolean of
          true:  (rv: real);
          false: (iv: integer)
        end;

{ byte size of a constant table, counting exactly what wrttab writes: the
  alignment position restarts at a reset entry, the total does not }
function tabsize(cp: cstptr): integer;

var ad, tot: integer;

procedure align(a: integer);
begin while (ad mod a) <> 0 do begin ad := ad+1; tot := tot+1 end end;

procedure count(n: integer);
begin ad := ad+n; tot := tot+n end;

begin

   ad := 0; tot := 0;
   while cp <> nil do begin
      case cp^.ct of
         cstr: count(cp^.strl);
         creal: begin align(realal); count(realsize) end;
         cset: begin align(setal); count(setsize) end;
         ctmp: count((cp^.tsize+1)*intsize);
         ctab: begin count(tabsize(cp^.tb)); end;
         cint: begin align(intal); count(intsize) end;
         cchr: begin align(charal); count(charsize) end;
         cbol: begin align(boolal); count(boolsize) end;
         cvalx: count(1);
         crst: ad := 0
      end;
      cp := cp^.next
   end;
   tabsize := tot

end;

{ serialize a constant table into bytes }
procedure wrttab(cp: cstptr);

var ad, i, v, k: integer;

procedure align(a: integer);
begin
   while (ad mod a) <> 0 do begin write(prr, chr(92), '00'); ad := ad+1 end
end;

procedure quad(v: integer);
var k, b: integer;
begin
   for k := 1 to 8 do begin
      b := v mod 256; if b < 0 then b := b+256;
      obyte(b);
      v := (v-b) div 256 { exact, so a negative value carries its sign down }
   end
end;

begin

   ad := 0;
   while cp <> nil do begin

      case cp^.ct of
         cstr: begin
            for i := 1 to cp^.strl do
               if i <= max(cp^.str^) then obyte(ord(cp^.str^[i])) else obyte(32);
            ad := ad+cp^.strl
         end;
         creal: begin align(realal); ro.rv := cp^.r; quad(ro.iv); ad := ad+realsize end;
         cset: begin
            align(setal); r.s := cp^.s;
            for i := 1 to setsize do obyte(r.b[i]);
            ad := ad+setsize
         end;
         ctmp: begin
            for k := 1 to cp^.tsize do quad(cp^.ta[k]);
            ad := ad+(cp^.tsize+1)*intsize
         end;
         ctab: begin wrttab(cp^.tb); ad := ad+tabsize(cp^.tb) end;
         cint: begin align(intal); quad(cp^.i); ad := ad+intsize end;
         cchr: begin align(charal); obyte(cp^.c); ad := ad+charsize end;
         cbol: begin align(boolal); obyte(cp^.b); ad := ad+boolsize end;
         cvalx: begin obyte(cp^.x); ad := ad+1 end;
         crst: ad := 0
      end;
      cp := cp^.next

   end

end;

begin

   endstubs;
   writeln(prr);
   writeln(prr, '; constants');
   write(prr, '@modnam = private constant [ ', max(modnam^)+1:1, ' x i8 ] c"');
   for i := 1 to max(modnam^) do obyte(ord(modnam^[i]));
   writeln(prr, chr(92), '00"');
   { the module's entry in the initialization chain: the runtime walks the
     psystem_llvm_mods section in link order, which is the initialization
     order pc established, calling each entry in turn }
   writeln(prr, '@"psystem_llvm_mod.', modnam^, '" = global ptr @"', modnam^,
                '", section "psystem_llvm_mods", align 8');
   cp := csttbl;
   while cp <> nil do begin

      case cp^.ct of

         cstr: begin
            write(prr, '@"string', cp^.strn:1, '" = private constant [ ', cp^.strl:1, ' x i8 ] c"');
            for i := 1 to cp^.strl do
               if i <= max(cp^.str^) then obyte(ord(cp^.str^[i])) else obyte(32);
            writeln(prr, '"')
         end;

         creal: ; { reals are inline constants }

         cset: begin
            write(prr, '@"set', cp^.setn:1, '" = private constant [ ', setsize:1, ' x i8 ] c"');
            r.s := cp^.s;
            for i := 1 to setsize do obyte(r.b[i]);
            writeln(prr, '"')
         end;

         ctmp: begin
            write(prr, '@"', modnam^, '.', cp^.tn:1, '" = private constant [ ', cp^.tsize:1, ' x i64 ] [');
            for i := 1 to cp^.tsize do begin
               if i > 1 then write(prr, ',');
               write(prr, ' i64 ', cp^.ta[i]:1)
            end;
            writeln(prr, ' ]');
            ll := 0; os(modnam^); oc('.'); oi(cp^.tn);
            tmps := lbstr; addname(defsyms, tmps^); ll := 0
         end;

         ctab: begin
            i := tabsize(cp^.tb);
            if cp^.cs <> nil then begin
               write(prr, '@"', cp^.cs^, '" = constant [ ', i:1, ' x i8 ] c"');
               addname(defsyms, cp^.cs^)
            end else begin
               write(prr, '@"', modnam^, '.', cp^.cn:1, '" = private constant [ ', i:1, ' x i8 ] c"');
               ll := 0; os(modnam^); oc('.'); oi(cp^.cn);
               tmps := lbstr; addname(defsyms, tmps^); ll := 0
            end;
            wrttab(cp^.tb);
            writeln(prr, '"')
         end;

         cint, cchr, cbol, cvalx, crst: ;

      end;
      cp := cp^.next

   end

end;

override procedure emitgbl;

var n: integer;

begin

   n := gblsiz; if n < 16 then n := 16;
   writeln(prr);
   writeln(prr, '@globals_start = internal global [ ', n:1, ' x i8 ] zeroinitializer, align 16')

end;

override procedure emitdbg;

var np: nameptr;

begin

   writeln(prr);
   writeln(prr, '; declarations');
   writeln(prr, 'declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)');
   writeln(prr, 'declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)');
   writeln(prr, 'declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64)');
   writeln(prr, 'declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64)');
   writeln(prr, 'declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64)');
   writeln(prr, 'declare double @llvm.fabs.f64(double)');
   writeln(prr, 'declare i64 @llvm.lrint.i64.f64(double)');
   writeln(prr, 'declare i32 @_setjmp(ptr) returns_twice');
   writeln(prr, 'declare void @psystem_errore(i64, i64, i64)');
   writeln(prr, 'declare void @psystem_llvm_nextmod()');
   writeln(prr, 'declare void @psystem_llvm_bge(ptr)');
   writeln(prr, 'declare void @psystem_llvm_ede()');
   writeln(prr, 'declare void @psystem_llvm_mse(i64, i64)');
   writeln(prr, 'declare i64 @psystem_llvm_curvec()');
   writeln(prr, 'declare void @psystem_llvm_ipj(ptr, i64)');
   writeln(prr, '@psystem_iso7185 = external global i64');
   writeln(prr, '@psystem_llvm_sl = external global ptr');
   writeln(prr, '@psystem_llvm_sfr = external global ptr');
   np := declst;
   while np <> nil do begin
      if not innames(defsyms, np^.name^) then
         writeln(prr, 'declare ', np^.sig^, ' @"', np^.name^, '"', np^.par^);
      np := np^.next
   end;
   { external routines referenced by address and never called: declared
     without a signature }
   np := refrtn;
   while np <> nil do begin
      if not innames(declst, np^.name^) then
         if not innames(defsyms, np^.name^) then
            writeln(prr, 'declare void @"', np^.name^, '"()');
      np := np^.next
   end;
   { referenced global symbols not defined here }
   np := refsyms;
   while np <> nil do begin
      if not innames(defsyms, np^.name^) then
         if not innames(declst, np^.name^) then
            writeln(prr, '@"', np^.name^, '" = external global i8');
      np := np^.next
   end

end;

{******************************************************************************

Module preamble and postamble

The preamble opens the module entry function; the strip code that follows in
the intermediate (the calls to the initializer body and to the next module)
becomes its body, and its ret closes it.

******************************************************************************}

override procedure preamble;

var i: integer;

begin

   modsym := modnam;
   openfn(modnam^, true);
   { the standard file numbers in the globals header }
   for i := 0 to 6 do begin
      oins; os('store i8 '); oi(i+1); os(', ptr getelementptr (i8, ptr @globals_start, i64 '); oi(i*2); oc(')'); ol
   end;
   if iso7185 then begin
      oins; os('store i64 1, ptr @psystem_iso7185'); ol
   end

end;

override procedure postamble;

begin

   endstubs

end;

{******************************************************************************

Assemble

Translates one instruction. Expression instructions build trees on the
expression stack; statement instructions pop them and generate.

******************************************************************************}

override procedure assemble;

   var name :alfa; r :real; s :settype;
       i,s1,lb,ub,l,v:integer; c,dc: char;
       str: strbuf; { buffer for string constants }
       cstp: cstptr;
       ep, ep2, ep3, ep4, ep5: expptr;
       sp, sp2: pstring; def, def2: boolean; val, val2: integer;
       blk: pblock; { block reference }
       ip: ipjptr; v2, v3: integer; cp: calptr;
       ro: record case boolean of
             true:  (rv: real);
             false: (iv: integer)
           end;

   procedure getlvl(var p: lvltyp);
   var i: integer;
   begin getint(i); p := i end;

   procedure getadr(var a: address);
   var i: integer;
   begin getint(i); a := i end;

   procedure parp;
   begin getlvl(p) end;

   procedure parq;
   begin getadr(q) end;

   procedure parpq;
   begin getlvl(p); getadr(q) end;

   procedure parqq;
   begin getadr(q); getadr(q1) end;

   function isfltres(ep: expptr): boolean;
   var isf: boolean;
   begin
      isf := false;
      if instab[ep^.op].insf then isf := true
      else if (ep^.op in [247{cif}, 246{cuf}, 249{cvf}]) and (ep^.rc = 1) then isf := true
      else if ep^.op = 15{csp} then
         if ep^.q in [19{atn},15{cos},16{exp},17{log},14{sin},18{sqt}] then isf := true;
      isfltres := isf
   end;

   { the LLVM type of a node result }
   procedure oty(ep: expptr);
   begin if isfltres(ep) then os('double') else os('i64') end;

   { attach tag check sequence to leaf }
   procedure attach(ep: expptr);
   begin
      if estack <> nil then
         if estack^.op = 188{cke} then popstk(ep^.al)
   end;

   { duplicate subtree. A call in the tree is made once: the copies share
     a result value assigned here (t2a flags it), and the first copy
     generated makes the call. The copy keeps the result frame size label
     in lb, since it has no sfr link of its own. }
   procedure duptre(s: expptr; var d: expptr);
   begin
      if s = nil then d := nil else begin
         if s^.op in [246{cuf}, 247{cif}, 249{cvf}, 15{csp}] then begin
            if s^.t2a <> 1 then begin s^.t2a := 1; s^.r1a := newv; s^.r2a := 0 end;
            if s^.sl <> nil then s^.lb := s^.sl^.lb
         end;
         getexp(d); d^ := s^; d^.next := nil; d^.sl := nil; d^.al := nil; d^.pl := nil;
         duptre(s^.l, d^.l); duptre(s^.r, d^.r); duptre(s^.x1, d^.x1); duptre(s^.cl, d^.cl)
      end
   end;

   { has a shared call result already been produced? }
   function calldone(v: integer): boolean;
   var cp: calptr; f: boolean;
   begin
      f := false; cp := donelst;
      while cp <> nil do begin if cp^.k = v then f := true; cp := cp^.next end;
      calldone := f
   end;

   procedure setdone(v: integer);
   var cp: calptr;
   begin
      new(cp); cp^.k := v; cp^.next := donelst; donelst := cp
   end;

   { get n parameters into the list, in reverse }
   procedure getparn(ep: expptr; pn: integer);
   var pp: expptr;
   begin
      while (pn > 0) and (estack <> nil) do
         begin popstk(pp); pp^.next := ep^.pl; ep^.pl := pp; pn := pn-1 end
   end;

   { get system call parameters, counted in stack words }
   procedure getparc(ep: expptr; pn: integer);
   var pp: expptr;
   begin
      while (pn > 0) and (estack <> nil) do
         begin popstk(pp); pp^.next := ep^.pl; ep^.pl := pp;
               if instab[pp^.op].insr = 2 then pn := pn-2 else pn := pn-1 end
   end;

   { get parameters of a user call }
   procedure getpar(ep: expptr);
   begin
      ep^.sl := nil;
      if estack^.op = 245{sfr} then popstk(ep^.sl);
      getparn(ep, ep^.pn);
      if ep^.sl = nil then popstk(ep^.sl);
      if ep^.sl^.op <> 245{sfr} then error('system error')
   end;

   procedure revpar(ep: expptr);
   var pl, pp: expptr;
   begin
      pl := nil;
      while ep^.pl <> nil do begin
         pp := ep^.pl; ep^.pl := ep^.pl^.next; pp^.next := pl; pl := pp
      end;
      ep^.pl := pl
   end;

   procedure ordpar(ep: expptr);
   var lp, pp: expptr;
   begin
      pp := ep^.pl; lp := nil;
      if pp <> nil then while pp^.next <> nil do begin lp := pp; pp := pp^.next end;
      if (pp = nil) or (lp = nil) then error('system error');
      lp^.next := nil; pp^.next := ep^.pl; ep^.pl := pp
   end;

   procedure genexp(ep: expptr); forward;

   { generate a list of values into an alloca array, top of stack first,
     returning the array pointer }
   function genlist(n: integer): integer;
   var a, k, e: integer; ep: expptr;
   begin
      a := alloca(n*8+8, 'list');
      for k := 0 to n-1 do begin
         popstk(ep); genexp(ep);
         e := gep(a, k*8);
         st('i', ep^.r1a, e);
         deltre(ep)
      end;
      genlist := a
   end;

   { i1 result of an integer compare }
   function icmp(view cc: string; a, b: integer): integer;
   var v: integer;
   begin
      v := newv;
      oins; ov(v); os(' = icmp '); os(cc); os(' i64 '); ov(a); os(', '); ov(b); ol;
      icmp := v
   end;

   function icmpi(view cc: string; a: integer; b: integer): integer;
   var v: integer;
   begin
      v := newv;
      oins; ov(v); os(' = icmp '); os(cc); os(' i64 '); ov(a); os(', '); oi(b); ol;
      icmpi := v
   end;

   { i64 from an i1 }
   function zext1(c: integer): integer;
   var v: integer;
   begin
      v := newv;
      oins; ov(v); os(' = zext i1 '); ov(c); os(' to i64'); ol;
      zext1 := v
   end;

   { binary i64 operation }
   function bini(view op: string; a, b: integer): integer;
   var v: integer;
   begin
      v := newv;
      oins; ov(v); os(' = '); os(op); os(' i64 '); ov(a); os(', '); ov(b); ol;
      bini := v
   end;

   function binii(view op: string; a: integer; b: integer): integer;
   var v: integer;
   begin
      v := newv;
      oins; ov(v); os(' = '); os(op); os(' i64 '); ov(a); os(', '); oi(b); ol;
      binii := v
   end;

   function binr(view op: string; a, b: integer): integer;
   var v: integer;
   begin
      v := newv;
      oins; ov(v); os(' = '); os(op); os(' double '); ov(a); os(', '); ov(b); ol;
      binr := v
   end;

   { checked arithmetic: the intrinsic, then the overflow test }
   function chkarith(view intr: string; a, b: integer): integer;
   var t, v, f: integer;
   begin
      t := newv;
      oins; ov(t); os(' = call { i64, i1 } @llvm.'); os(intr); os('.with.overflow.i64(i64 ');
      ov(a); os(', i64 '); ov(b); oc(')'); ol;
      v := newv;
      oins; ov(v); os(' = extractvalue { i64, i1 } '); ov(t); os(', 0'); ol;
      f := newv;
      oins; ov(f); os(' = extractvalue { i64, i1 } '); ov(t); os(', 1'); ol;
      errif(f, ecIntegerValueOverflow);
      chkarith := v
   end;

   function chkarithi(view intr: string; a: integer; b: integer): integer;
   var t, v, f: integer;
   begin
      t := newv;
      oins; ov(t); os(' = call { i64, i1 } @llvm.'); os(intr); os('.with.overflow.i64(i64 ');
      ov(a); os(', i64 '); oi(b); oc(')'); ol;
      v := newv;
      oins; ov(v); os(' = extractvalue { i64, i1 } '); ov(t); os(', 0'); ol;
      f := newv;
      oins; ov(f); os(' = extractvalue { i64, i1 } '); ov(t); os(', 1'); ol;
      errif(f, ecIntegerValueOverflow);
      chkarithi := v
   end;

   { real compare to i64 boolean }
   function fcmp(view cc: string; a, b: integer): integer;
   var v: integer;
   begin
      v := newv;
      oins; ov(v); os(' = fcmp '); os(cc); os(' double '); ov(a); os(', '); ov(b); ol;
      fcmp := zext1(v)
   end;

   { a set temporary as an i64 address }
   function settmp: integer;
   begin settmp := p2i(alloca(setsize, 'set')) end;

   { Pascal callees receive their overflow parameters (past the six integer
     or six real register slots) as words of bits, since the callee cannot
     tell a real from an address there. Mark the real arguments that overflow
     and convert them ahead of the call; t1a holds the converted value. }
   procedure castovf(pp: expptr; pascal: boolean);
   var ipc, fpc: integer;
   begin
      ipc := 0; fpc := 0;
      while pp <> nil do begin
         pp^.t1a := 0;
         if instab[pp^.op].insr = 2 then ipc := ipc+2
         else if isfltres(pp) then begin
            fpc := fpc+1;
            if pascal and (fpc > 6) then begin
               pp^.t1a := newv;
               oins; ov(pp^.t1a); os(' = bitcast double '); ov(pp^.r1a); os(' to i64'); ol
            end
         end else ipc := ipc+1;
         pp := pp^.next
      end
   end;

   { write one argument }
   procedure oarg(pp: expptr; var first: boolean);
   begin
      if not first then os(', ');
      first := false;
      if instab[pp^.op].insr = 2 then begin
         os('i64 '); ov(pp^.r1a); os(', i64 '); ov(pp^.r2a)
      end else if pp^.t1a <> 0 then begin
         os('i64 '); ov(pp^.t1a)
      end else begin
         oty(pp); oc(' '); ov(pp^.r1a)
      end
   end;

   { write the argument list of a call from a parameter list, in declaration
     order (the definitions assign slots in the same order) }
   procedure oargs(pp: expptr; first: boolean; pascal: boolean);
   begin
      while pp <> nil do begin oarg(pp, first); pp := pp^.next end
   end;

   procedure otype(pp: expptr; var first: boolean);
   begin
      if not first then os(', ');
      first := false;
      if instab[pp^.op].insr = 2 then os('i64, i64')
      else if pp^.t1a <> 0 then os('i64')
      else oty(pp)
   end;

   { the type list of a parameter list, for a declaration }
   procedure otypes(pp: expptr; first: boolean; pascal: boolean);
   begin
      while pp <> nil do begin otype(pp, first); pp := pp^.next end
   end;

   { generate the parameters of a call, left to right }
   procedure genpars(pp: expptr);
   begin
      while pp <> nil do begin genexp(pp); pp := pp^.next end
   end;

   { the result frame of a call: an alloca of the sfr size for stacked
     results, null otherwise }
   function callsfr(ep: expptr): integer;
   var n: integer; lb: pstring;
   begin
      callsfr := vnull;
      lb := nil;
      if ep^.sl <> nil then lb := ep^.sl^.lb else lb := ep^.lb;
      if lb <> nil then if ep^.rc in [2, 3] then begin
         n := labelvalof(lb);
         if n < 16 then n := 16;
         callsfr := alloca(n, 'sfr')
      end
   end;

   { return type text of a call by result code }
   procedure orettyp(rc: integer; proc: boolean);
   begin
      if proc then os('void')
      else case rc of 0: os('i64'); 1: os('double'); 2, 3: os('void') end
   end;

   { the name of an external routine as linked }
   procedure extname(s: pstring; var n: pstring);
   var ep: pextdsc; b: packed array [1..400] of char; i, k: integer; c: char;
   begin
      ep := fndext(s); k := 0;
      if ep = nil then n := s
      else case ep^.etyp of
         0: n := s;
         1: begin { C external: strip the module prefix and type digest }
            i := 1;
            while (i <= max(s^)) and (s^[i] <> '.') do i := i+1;
            i := i+1;
            while (i <= max(s^)) and (s^[i] <> '$') do begin
               k := k+1; b[k] := s^[i]; i := i+1
            end;
            n := extract(b, 1, k)
         end;
         2: begin { module external: dot to underscore, strip the digest }
            i := 1;
            while (i <= max(s^)) and (s^[i] <> '$') do begin
               c := s^[i]; if c = '.' then c := '_';
               k := k+1; b[k] := c; i := i+1
            end;
            n := extract(b, 1, k)
         end
      end
   end;

   { is a call target a C routine (no hidden arguments)? }
   function isc(s: pstring): boolean;
   var ep: pextdsc;
   begin
      ep := fndext(s);
      isc := false;
      if ep <> nil then isc := ep^.etyp in [1, 2]
   end;

   { user call: cup/cuf }
   procedure gencall(ep: expptr);
   label 1;
   var sfr, v, fr: integer; nm, rt: pstring; cfn: boolean;
   begin
      if ep^.t2a = 1 then if calldone(ep^.r1a) then goto 1;
      genexp(ep^.sl); { the sfr: nothing to do here }
      sfr := callsfr(ep);
      genpars(ep^.pl);
      extname(ep^.fn, nm); cfn := isc(ep^.fn);
      castovf(ep^.pl, not cfn);
      fr := frameof(fnlvl);
      { record the signature of every call target: the ones not defined in
        this module are declared at the end }
      begin
         ll := 0;
         orettyp(ep^.rc, ep^.op = 12);
         rt := lbstr; ll := 0;
         oc('(');
         otypes(ep^.pl, true, not cfn);
         oc(')');
         tmps := lbstr; declfn(nm^, rt^, tmps^); ll := 0
      end;
      oins; os('store ptr '); ov(fr); os(', ptr @psystem_llvm_sl'); ol;
      oins; os('store ptr '); ov(sfr); os(', ptr @psystem_llvm_sfr'); ol;
      if (ep^.op = 246{cuf}) and (ep^.rc in [0, 1]) then begin
         if ep^.t2a <> 1 then ep^.r1a := newv;
         v := ep^.r1a;
         oins; ov(v); os(' = call ')
      end else begin oins; os('call ') end;
      orettyp(ep^.rc, ep^.op = 12);
      os(' @'); oq(nm^); oc('(');
      oargs(ep^.pl, true, not cfn); oc(')'); ol;
      if ep^.op = 246{cuf} then if ep^.rc in [2, 3] then begin
         if ep^.t2a <> 1 then ep^.r1a := newv;
         oins; ov(ep^.r1a); os(' = ptrtoint ptr '); ov(sfr); os(' to i64'); ol
      end;
      if ep^.t2a = 1 then setdone(ep^.r1a);
      1:
   end;

   { indirect call: cip/cif, the fat pointer in l }
   procedure genicall(ep: expptr);
   label 2;
   var sfr, v, f, fr, a, b: integer;
   begin
      if ep^.t2a = 1 then if calldone(ep^.r1a) then goto 2;
      genexp(ep^.sl);
      sfr := callsfr(ep);
      genpars(ep^.pl);
      castovf(ep^.pl, true);
      genexp(ep^.l);
      { l is the address of the procedure value: the routine address, then
        its frame }
      a := i2p(ep^.l^.r1a);
      f := newv;
      oins; ov(f); os(' = load ptr, ptr '); ov(a); ol;
      b := gep(a, ptrsize);
      fr := newv;
      oins; ov(fr); os(' = load ptr, ptr '); ov(b); ol;
      oins; os('store ptr '); ov(fr); os(', ptr @psystem_llvm_sl'); ol;
      oins; os('store ptr '); ov(sfr); os(', ptr @psystem_llvm_sfr'); ol;
      if (ep^.op = 247{cif}) and (ep^.rc in [0, 1]) then begin
         if ep^.t2a <> 1 then ep^.r1a := newv;
         v := ep^.r1a;
         oins; ov(v); os(' = call ')
      end else begin oins; os('call ') end;
      orettyp(ep^.rc, ep^.op = 113);
      oc(' '); ov(f); oc('(');
      oargs(ep^.pl, true, true); oc(')'); ol;
      if ep^.op = 247{cif} then if ep^.rc in [2, 3] then begin
         if ep^.t2a <> 1 then ep^.r1a := newv;
         oins; ov(ep^.r1a); os(' = ptrtoint ptr '); ov(sfr); os(' to i64'); ol
      end;
      if ep^.t2a = 1 then setdone(ep^.r1a);
      2:
   end;

   { vectored call: cuv/cvf, through a global holding the routine address }
   procedure genvcall(ep: expptr);
   label 3;
   var sfr, v, a, f, fr: integer;
   begin
      if ep^.t2a = 1 then if calldone(ep^.r1a) then goto 3;
      genexp(ep^.sl);
      sfr := callsfr(ep);
      genpars(ep^.pl);
      castovf(ep^.pl, true);
      if ep^.qs <> nil then a := symadr(ep^.qs^) else a := gbladr(ep^.q);
      f := newv;
      oins; ov(f); os(' = load ptr, ptr '); ov(a); ol;
      fr := frameof(fnlvl);
      oins; os('store ptr '); ov(fr); os(', ptr @psystem_llvm_sl'); ol;
      oins; os('store ptr '); ov(sfr); os(', ptr @psystem_llvm_sfr'); ol;
      if (ep^.op = 249{cvf}) and (ep^.rc in [0, 1]) then begin
         if ep^.t2a <> 1 then ep^.r1a := newv;
         v := ep^.r1a;
         oins; ov(v); os(' = call ')
      end else begin oins; os('call ') end;
      orettyp(ep^.rc, ep^.op = 27);
      oc(' '); ov(f); oc('(');
      oargs(ep^.pl, true, true); oc(')'); ol;
      if ep^.op = 249{cvf} then if ep^.rc in [2, 3] then begin
         if ep^.t2a <> 1 then ep^.r1a := newv;
         oins; ov(ep^.r1a); os(' = ptrtoint ptr '); ov(sfr); os(' to i64'); ol
      end;
      if ep^.t2a = 1 then setdone(ep^.r1a);
      3:
   end;

   { system call }
   procedure callsp(ep: expptr; var sc: alfa; r: boolean);
   label 4;
   var nm: packed array [1..20] of char; i, n, v: integer; isr: boolean; rt, nms: pstring;
   begin
      if ep^.t2a = 1 then if calldone(ep^.r1a) then goto 4;
      genpars(ep^.pl);
      castovf(ep^.pl, false);
      for i := 1 to 20 do nm[i] := ' ';
      nm[1] := 'p'; nm[2] := 's'; nm[3] := 'y'; nm[4] := 's'; nm[5] := 't';
      nm[6] := 'e'; nm[7] := 'm'; nm[8] := '_'; n := 8;
      for i := 1 to maxalfa do if sc[i] <> ' ' then begin n := n+1; nm[n] := sc[i] end;
      isr := ep^.q in [19{atn},15{cos},16{exp},17{log},14{sin},18{sqt}];
      { declare }
      ll := 0;
      if not r then os('void') else if isr then os('double') else os('i64');
      rt := lbstr; ll := 0;
      oc('('); otypes(ep^.pl, true, false); oc(')');
      tmps := lbstr; nms := extract(nm, 1, n); declfn(nms^, rt^, tmps^); ll := 0;
      if r then begin
         if ep^.t2a <> 1 then ep^.r1a := newv;
         v := ep^.r1a;
         oins; ov(v); os(' = call ');
         if isr then os('double') else os('i64')
      end else begin oins; os('call void') end;
      os(' @'); oq(nms^); oc('(');
      oargs(ep^.pl, true, false); oc(')'); ol;
      if ep^.t2a = 1 then setdone(ep^.r1a);
      4:
   end;

   { new/dispose with tag list: the parameters are (addr, size, tagcount,
     tags...); the tags go to a list whose address is the fourth argument }
   procedure callnwldsl(ep: expptr);
   var pp, tp: expptr; n, a, k, e: integer;
   begin
      pp := ep^.pl; genexp(pp); { address }
      pp := pp^.next; genexp(pp); { size }
      pp := pp^.next; genexp(pp); { tag count }
      { the tags }
      n := 0; tp := pp^.next;
      while tp <> nil do begin n := n+1; tp := tp^.next end;
      { the list order is the order the AMD64 generator pushes them, so the
        runtime sees the last of the list first }
      a := alloca(n*8+8, 'tags');
      k := n-1; tp := pp^.next;
      while tp <> nil do begin
         genexp(tp); e := gep(a, k*8); st('i', tp^.r1a, e);
         k := k-1; tp := tp^.next
      end;
      k := p2i(a);
      if ep^.q = 39 then begin
         declfn('psystem_nwl', 'void', '(i64, i64, i64, i64)');
         oins; os('call void @psystem_nwl(i64 ')
      end else begin
         declfn('psystem_dsl', 'void', '(i64, i64, i64, i64)');
         oins; os('call void @psystem_dsl(i64 ')
      end;
      pp := ep^.pl; ov(pp^.r1a); os(', i64 ');
      pp := pp^.next; ov(pp^.r1a); os(', i64 ');
      pp := pp^.next; ov(pp^.r1a); os(', i64 ');
      ov(k); oc(')'); ol
   end;

   procedure genexp(ep: expptr);
   var ep2: expptr; v, t, a, b, c, d, n: integer; lf, lc: pstring;
   begin
      if ep <> nil then begin
         genexp(ep^.al);
         if not (ep^.op in [113{cip}, 247{cif}, 12{cup}, 246{cuf}, 27{cuv}, 249{cvf}, 15{csp},
                            188{cke}, 248{mpc}]) then
            genexp(ep^.l);
         if not (ep^.op in [12, 246, 113, 247, 27, 249, 15, 248]) then begin
            genexp(ep^.r); genexp(ep^.x1)
         end;
         case ep^.op of

            {lodi,loda}
            0,105: ep^.r1a := ld('i', locadr(ep^.p, ep^.q));
            {lodx,lodb,lodc}
            193,108,109: ep^.r1a := ld('b', locadr(ep^.p, ep^.q));
            {lodr}
            106: ep^.r1a := ld('r', locadr(ep^.p, ep^.q));
            {lods}
            107: ep^.r1a := p2i(locadr(ep^.p, ep^.q));
            {lda}
            4: ep^.r1a := p2i(locadr(ep^.p, ep^.q));

            {adi}
            28: if dochkovf then ep^.r1a := chkarith('sadd', ep^.l^.r1a, ep^.r^.r1a)
                else ep^.r1a := bini('add', ep^.l^.r1a, ep^.r^.r1a);
            {adr}
            29: ep^.r1a := binr('fadd', ep^.l^.r1a, ep^.r^.r1a);
            {sbi}
            30: if dochkovf then ep^.r1a := chkarith('ssub', ep^.l^.r1a, ep^.r^.r1a)
                else ep^.r1a := bini('sub', ep^.l^.r1a, ep^.r^.r1a);
            {sbr}
            31: ep^.r1a := binr('fsub', ep^.l^.r1a, ep^.r^.r1a);

            {equr,neqr,geqr,grtr,leqr,lesr}
            138: ep^.r1a := fcmp('oeq', ep^.l^.r1a, ep^.r^.r1a);
            144: ep^.r1a := fcmp('une', ep^.l^.r1a, ep^.r^.r1a);
            150: ep^.r1a := fcmp('oge', ep^.l^.r1a, ep^.r^.r1a);
            156: ep^.r1a := fcmp('ogt', ep^.l^.r1a, ep^.r^.r1a);
            162: ep^.r1a := fcmp('ole', ep^.l^.r1a, ep^.r^.r1a);
            168: ep^.r1a := fcmp('olt', ep^.l^.r1a, ep^.r^.r1a);

            {lip}
            120: begin
               a := locadr(ep^.p, ep^.q);
               ep^.r1a := ld('i', a);
               ep^.r2a := ld('i', gep(a, ptrsize))
            end;

            {equm,neqm,geqm,grtm,leqm,lesm}
            142,148,154,160,166,172: begin
               declfn('psystem_strcmp', 'i64', '(i64, i64, i64)');
               v := newv;
               oins; ov(v); os(' = call i64 @psystem_strcmp(i64 '); ov(ep^.l^.r1a);
               os(', i64 '); ov(ep^.r^.r1a); os(', i64 '); oi(ep^.q); oc(')'); ol;
               case ep^.op of
                  142: ep^.r1a := zext1(icmpi('eq', v, 0));
                  148: ep^.r1a := zext1(icmpi('ne', v, 0));
                  154: ep^.r1a := zext1(icmpi('sge', v, 0));
                  160: ep^.r1a := zext1(icmpi('sgt', v, 0));
                  166: ep^.r1a := zext1(icmpi('sle', v, 0));
                  172: ep^.r1a := zext1(icmpi('slt', v, 0))
               end
            end;

            {lao,lto}
            5,234: if ep^.fl <> nil then ep^.r1a := p2i(symadr(ep^.fl^))
                   else ep^.r1a := p2i(gbladr(ep^.q));

            {ixa}
            16: begin
               t := binii('mul', ep^.r^.r1a, ep^.q);
               ep^.r1a := bini('add', ep^.l^.r1a, t)
            end;

            {swp}
            118: ;

            {ldoi,ldoa,ltci}
            1,65,228: if ep^.fl <> nil then ep^.r1a := ld('i', symadr(ep^.fl^))
                      else ep^.r1a := ld('i', gbladr(ep^.q));
            {ldob,ldoc,ldox,ltcb,ltcc,ltcx}
            68,69,194,231,232,233: if ep^.fl <> nil then ep^.r1a := ld('b', symadr(ep^.fl^))
                                   else ep^.r1a := ld('b', gbladr(ep^.q));
            {ldor,ltcr}
            66,229: if ep^.fl <> nil then ep^.r1a := ld('r', symadr(ep^.fl^))
                    else ep^.r1a := ld('r', gbladr(ep^.q));
            {ldos,ltcs}
            67,230: if ep^.fl <> nil then ep^.r1a := p2i(symadr(ep^.fl^))
                    else ep^.r1a := p2i(gbladr(ep^.q));

            {indi,inda}
            9,85: ep^.r1a := ld('i', gep(i2p(ep^.l^.r1a), ep^.q));
            {indr}
            86: ep^.r1a := ld('r', gep(i2p(ep^.l^.r1a), ep^.q));
            {indb,indc,indx}
            88,89,198: ep^.r1a := ld('b', gep(i2p(ep^.l^.r1a), ep^.q));
            {inds}
            87: if ep^.q <> 0 then ep^.r1a := binii('add', ep^.l^.r1a, ep^.q)
                else ep^.r1a := ep^.l^.r1a;

            {inci,incb,incc,incx}
            10,93,94,201: if dochkovf then ep^.r1a := chkarithi('sadd', ep^.l^.r1a, ep^.q)
                          else ep^.r1a := binii('add', ep^.l^.r1a, ep^.q);
            {inca}
            90: ep^.r1a := binii('add', ep^.l^.r1a, ep^.q);
            {deci,decb,decc,decx}
            57,103,104,202: if dochkovf then ep^.r1a := chkarithi('ssub', ep^.l^.r1a, ep^.q)
                            else ep^.r1a := binii('sub', ep^.l^.r1a, ep^.q);

            {s2c}
            173: begin
               declfn('psystem_s2c', 'i64', '(i64, i64)');
               v := newv; ep^.r1a := v;
               oins; ov(v); os(' = call i64 @psystem_s2c(i64 '); ov(ep^.l^.r1a);
               os(', i64 '); oi(ep^.q); oc(')'); ol
            end;

            {mdc}
            254: begin
               ep^.r2a := ep^.l^.r1a;
               ep^.r1a := binii('add', ep^.l^.r1a, ep^.q)
            end;

            {ckvi,ckvb,ckvc,ckvx}
            175,179,180,203: begin
               { compare the tag (r1a of the cke, passed in) with q, or into
                 the running boolean r2a }
               c := zext1(icmpi('eq', ep^.r1a, ep^.q));
               ep^.r2a := bini('or', ep^.r2a, c)
            end;

            {cvbi,cvbx,cvbb,cvbc}
            100,115,116,121: begin
               declfn('psystem_tagchgvar', 'void', '(i64, i64, i64, i64, i64, i64)');
               if ep^.op = 100 then t := ld('i', i2p(ep^.l^.r1a))
               else t := ld('b', i2p(ep^.l^.r1a));
               oins; os('call void @psystem_tagchgvar(i64 '); oi(ep^.q); os(', i64 '); oi(ep^.q1);
               os(', i64 '); osymadr(ep^.lt^); os(', i64 '); ov(ep^.r^.r1a); os(', i64 '); ov(t);
               os(', i64 '); ov(ep^.l^.r1a); oc(')'); ol
            end;

            {ivti,ivtx,ivtb,ivtc}
            192,101,102,111: begin
               declfn('psystem_tagchginv', 'void', '(i64, i64, i64, i64, i64, i64)');
               if ep^.op = 192 then t := ld('i', i2p(ep^.l^.r1a))
               else t := ld('b', i2p(ep^.l^.r1a));
               oins; os('call void @psystem_tagchginv(i64 '); oi(ep^.q); os(', i64 '); oi(ep^.q1);
               os(', i64 '); osymadr(ep^.lt^); os(', i64 '); ov(ep^.r^.r1a); os(', i64 '); ov(t);
               os(', i64 '); ov(ep^.l^.r1a); oc(')'); ol
            end;

            {cps}
            176: begin
               c := icmp('ne', ep^.l^.r2a, ep^.r^.r2a);
               errif(c, ecContainerMismatch)
            end;

            {cpc}
            177: begin
               declfn('psystem_cmptmp', 'void', '(i64, i64, i64)');
               oins; os('call void @psystem_cmptmp(i64 '); oi(ep^.q); os(', i64 ');
               ov(ep^.l^.r2a); os(', i64 '); ov(ep^.r^.r2a); oc(')'); ol
            end;

            {cta}
            191: begin
               declfn('psystem_tagchkass', 'void', '(i64, i64, i64, i64, i64)');
               oins; os('call void @psystem_tagchkass(i64 '); oi(ep^.q); os(', i64 '); oi(ep^.q1);
               os(', i64 '); osymadr(ep^.lt^); os(', i64 '); ov(ep^.r^.r1a); os(', i64 ');
               ov(ep^.l^.r1a); oc(')'); ol
            end;

            {lpa}
            114: begin
               { the address of a routine: an external one needs a declaration
                 even when it is never called here }
               addname(refrtn, ep^.fn^);
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = ptrtoint ptr @'); oq(ep^.fn^); os(' to i64'); ol;
               ep^.r2a := p2i(frameof(ep^.p))
            end;

            {ldci,ldcc,ldcb}
            123,127,126: begin
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = add i64 '); oi(ep^.vi); os(', 0'); ol
            end;
            {ldcn}
            125: begin
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = add i64 0, 0'); ol
            end;
            {ldcr}
            124: begin
               ep^.r1a := newv;
               ro.iv := ep^.vi;
               oins; ov(ep^.r1a); os(' = fadd double '); ohexreal(ro.rv); os(', 0.0'); ol
            end;
            {ldcs}
            7: begin
               ep^.r1a := newv;
               ll := ll; oins; ov(ep^.r1a); os(' = add i64 ptrtoint (ptr @"set'); oi(ep^.setn);
               os('" to i64), 0'); ol
            end;

            {chki,chkb,chkc,chkx}
            26,98,99,199: begin
               errif(icmpi('slt', ep^.l^.r1a, ep^.vi), ecValueOutOfRange);
               errif(icmpi('sgt', ep^.l^.r1a, ep^.vi2), ecValueOutOfRange);
               ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.l^.r2a
            end;
            {chka}
            95: begin
               errif(icmpi('eq', ep^.l^.r1a, 0), ecNilPointerDereference);
               ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.l^.r2a
            end;
            {chks}
            97: begin
               declfn('psystem_chksetbnd', 'void', '(i64, i64, i64)');
               oins; os('call void @psystem_chksetbnd(i64 '); oi(ep^.vi); os(', i64 '); oi(ep^.vi2);
               os(', i64 '); ov(ep^.l^.r1a); oc(')'); ol;
               ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.l^.r2a
            end;
            {ckla}
            190: begin
               if ep^.q <> 0 then errif(icmpi('eq', ep^.l^.r1a, 0), ecNilPointerDereference);
               ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.l^.r2a
            end;

            {lca}
            56: begin
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = add i64 ptrtoint (ptr @"string'); oi(ep^.strn);
               os('" to i64), 0'); ol
            end;

            {equs,neqs,geqs,leqs}
            140,146,152,164: begin
               if ep^.op in [140, 146] then begin
                  declfn('psystem_setequ', 'i64', '(i64, i64)');
                  v := newv;
                  oins; ov(v); os(' = call i64 @psystem_setequ(i64 '); ov(ep^.l^.r1a);
                  os(', i64 '); ov(ep^.r^.r1a); oc(')'); ol;
                  if ep^.op = 146 then ep^.r1a := binii('xor', v, 1) else ep^.r1a := v
               end else begin
                  declfn('psystem_setinc', 'i64', '(i64, i64)');
                  v := newv;
                  oins; ov(v); os(' = call i64 @psystem_setinc(i64 '); ov(ep^.l^.r1a);
                  os(', i64 '); ov(ep^.r^.r1a); oc(')'); ol;
                  ep^.r1a := v
               end
            end;

            {equa,equi,equb,equc}
            17,137,139,141: ep^.r1a := zext1(icmp('eq', ep^.l^.r1a, ep^.r^.r1a));
            {neqa,neqi,neqb,neqc}
            18,143,145,147: ep^.r1a := zext1(icmp('ne', ep^.l^.r1a, ep^.r^.r1a));
            {geqi,geqb,geqc}
            149,151,153: ep^.r1a := zext1(icmp('sge', ep^.l^.r1a, ep^.r^.r1a));
            {grti,grtb,grtc}
            155,157,159: ep^.r1a := zext1(icmp('sgt', ep^.l^.r1a, ep^.r^.r1a));
            {leqi,leqb,leqc}
            161,163,165: ep^.r1a := zext1(icmp('sle', ep^.l^.r1a, ep^.r^.r1a));
            {lesi,lesb,lesc}
            167,169,171: ep^.r1a := zext1(icmp('slt', ep^.l^.r1a, ep^.r^.r1a));

            {ordi,ordb,ordc,ordx}
            59,134,136,200: begin ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.l^.r2a end;

            {lcp}
            135: begin
               ep^.r2a := binii('add', ep^.l^.r1a, ptrsize);
               ep^.r1a := ld('i', i2p(ep^.l^.r1a))
            end;

            {sgs}
            32: begin
               declfn('psystem_setsgl', 'void', '(i64, i64)');
               oins; os('call void @psystem_setsgl(i64 '); ov(ep^.l^.r1a); os(', i64 ');
               ov(ep^.r^.r1a); oc(')'); ol;
               ep^.r1a := ep^.r^.r1a
            end;

            {flt,flo}
            33,34: begin
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = sitofp i64 '); ov(ep^.l^.r1a); os(' to double'); ol
            end;

            {trc}
            35: begin
               if dochkovf then begin
                  c := newv;
                  oins; ov(c); os(' = fcmp ogt double '); ov(ep^.l^.r1a); os(', 9223372036854775807.0'); ol;
                  errif(c, ecRealArgumentTooLarge);
                  c := newv;
                  oins; ov(c); os(' = fcmp olt double '); ov(ep^.l^.r1a); os(', -9223372036854775807.0'); ol;
                  errif(c, ecRealArgumentTooLarge)
               end;
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = fptosi double '); ov(ep^.l^.r1a); os(' to i64'); ol
            end;

            {ngi}
            36: begin
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = sub i64 0, '); ov(ep^.l^.r1a); ol
            end;
            {ngr}
            37: begin
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = fneg double '); ov(ep^.l^.r1a); ol
            end;
            {sqi}
            38: if dochkovf then ep^.r1a := chkarith('smul', ep^.l^.r1a, ep^.l^.r1a)
                else ep^.r1a := bini('mul', ep^.l^.r1a, ep^.l^.r1a);
            {sqr}
            39: ep^.r1a := binr('fmul', ep^.l^.r1a, ep^.l^.r1a);
            {abi}
            40: begin
               c := icmpi('slt', ep^.l^.r1a, 0);
               t := newv;
               oins; ov(t); os(' = sub i64 0, '); ov(ep^.l^.r1a); ol;
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = select i1 '); ov(c); os(', i64 '); ov(t); os(', i64 ');
               ov(ep^.l^.r1a); ol
            end;
            {abr}
            41: begin
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = call double @llvm.fabs.f64(double '); ov(ep^.l^.r1a); oc(')'); ol
            end;
            {notb}
            42: ep^.r1a := zext1(icmpi('eq', ep^.l^.r1a, 0));
            {noti}
            205: begin
               if dodbgchk then errif(icmpi('slt', ep^.l^.r1a, 0), ecBooleanOperatorOfNegative);
               t := binii('xor', ep^.l^.r1a, -1);
               ep^.r1a := binii('and', t, maxint)
            end;
            {odd}
            50: ep^.r1a := binii('and', ep^.l^.r1a, 1);
            {rnd}
            62: begin
               if dochkovf then begin
                  c := newv;
                  oins; ov(c); os(' = fcmp ogt double '); ov(ep^.l^.r1a); os(', 9223372036854775807.0'); ol;
                  errif(c, ecRealArgumentTooLarge);
                  c := newv;
                  oins; ov(c); os(' = fcmp olt double '); ov(ep^.l^.r1a); os(', -9223372036854775807.0'); ol;
                  errif(c, ecRealArgumentTooLarge)
               end;
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = call i64 @llvm.lrint.i64.f64(double '); ov(ep^.l^.r1a); oc(')'); ol
            end;
            {chr}
            60: begin ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.l^.r2a end;

            {and,ior,xor}
            43,44,206: begin
               if dodbgchk then begin
                  errif(icmpi('slt', ep^.l^.r1a, 0), ecBooleanOperatorOfNegative);
                  errif(icmpi('slt', ep^.r^.r1a, 0), ecBooleanOperatorOfNegative)
               end;
               case ep^.op of
                  43: ep^.r1a := bini('and', ep^.l^.r1a, ep^.r^.r1a);
                  44: ep^.r1a := bini('or', ep^.l^.r1a, ep^.r^.r1a);
                  206: ep^.r1a := bini('xor', ep^.l^.r1a, ep^.r^.r1a)
               end
            end;

            {dif,int,uni}
            45,46,47: begin
               { copy the first operand to the destination temp, then the
                 psystem operation applies the second to it }
               memcpy(ep^.x1^.r1a, ep^.l^.r1a, setsize);
               case ep^.op of
                  45: begin declfn('psystem_setdif', 'void', '(i64, i64)');
                      oins; os('call void @psystem_setdif(i64 ') end;
                  46: begin declfn('psystem_setint', 'void', '(i64, i64)');
                      oins; os('call void @psystem_setint(i64 ') end;
                  47: begin declfn('psystem_setuni', 'void', '(i64, i64)');
                      oins; os('call void @psystem_setuni(i64 ') end
               end;
               ov(ep^.x1^.r1a); os(', i64 '); ov(ep^.r^.r1a); oc(')'); ol;
               ep^.r1a := ep^.x1^.r1a
            end;

            {inn}
            48: begin
               declfn('psystem_setsin', 'i64', '(i64, i64)');
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = call i64 @psystem_setsin(i64 '); ov(ep^.l^.r1a);
               os(', i64 '); ov(ep^.r^.r1a); oc(')'); ol
            end;

            {mod}
            49: begin
               errif(icmpi('sle', ep^.r^.r1a, 0), ecInvalidDivisorToMod);
               t := bini('srem', ep^.l^.r1a, ep^.r^.r1a);
               { ISO 7185: 0 <= result < divisor }
               c := icmpi('slt', t, 0);
               a := bini('add', t, ep^.r^.r1a);
               ep^.r1a := newv;
               oins; ov(ep^.r1a); os(' = select i1 '); ov(c); os(', i64 '); ov(a); os(', i64 '); ov(t); ol
            end;
            {dvi}
            53: begin
               errif(icmpi('eq', ep^.r^.r1a, 0), ecZeroDivide);
               ep^.r1a := bini('sdiv', ep^.l^.r1a, ep^.r^.r1a)
            end;
            {mpi}
            51: if dochkovf then ep^.r1a := chkarith('smul', ep^.l^.r1a, ep^.r^.r1a)
                else ep^.r1a := bini('mul', ep^.l^.r1a, ep^.r^.r1a);
            {mpr}
            52: ep^.r1a := binr('fmul', ep^.l^.r1a, ep^.r^.r1a);
            {dvr}
            54: begin
               if dodbgchk then begin
                  c := newv;
                  oins; ov(c); os(' = fcmp oeq double '); ov(ep^.r^.r1a); os(', 0.0'); ol;
                  errif(c, ecZeroDivide)
               end;
               ep^.r1a := binr('fdiv', ep^.l^.r1a, ep^.r^.r1a)
            end;

            {rgs}
            110: begin
               declfn('psystem_setrgs', 'void', '(i64, i64, i64)');
               oins; os('call void @psystem_setrgs(i64 '); ov(ep^.l^.r1a); os(', i64 ');
               ov(ep^.r^.r1a); os(', i64 '); ov(ep^.x1^.r1a); oc(')'); ol;
               ep^.r1a := ep^.x1^.r1a
            end;

            { 181-186: reserved }
            181,182,183,184,185,186: ;
            {cks}
            187: ;

            {csp}
            15: if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then callnwldsl(ep)
                else callsp(ep, sfptab[ep^.q].sptable, sfptab[ep^.q].spfunc);

            {sfr}
            245: ; { the result frame is allocated by the call }

            {cup,cuf}
            12,246: gencall(ep);
            {cip,cif}
            113,247: genicall(ep);
            {cuv,cvf}
            27,249: genvcall(ep);

            {cke}
            188: begin
               genexp(ep^.l);
               ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.l^.r2a;
               { the running boolean, or'ed by each tag check }
               t := newv;
               oins; ov(t); os(' = add i64 0, 0'); ol;
               ep2 := ep^.cl;
               while ep2 <> nil do begin
                  ep2^.r1a := ep^.r1a; ep2^.r2a := t;
                  genexp(ep2); t := ep2^.r2a;
                  ep2 := ep2^.next
               end;
               errif(icmpi('eq', t, 0), ecVariantNotActive)
            end;

            {wbs}
            243: begin
               declfn('psystem_withenter', 'void', '(i64)');
               oins; os('call void @psystem_withenter(i64 '); ov(ep^.l^.r1a); oc(')'); ol;
               ep^.r1a := ep^.l^.r1a
            end;

            {cxs}
            211: begin
               t := binii('sub', ep^.r^.r1a, 1);
               if dodbgchk then errif(icmp('uge', t, ep^.l^.r2a), ecValueOutOfRange);
               a := binii('mul', t, ep^.q);
               ep^.r1a := bini('add', ep^.l^.r1a, a)
            end;

            {cxc}
            212: begin
               { index-1, check against the template length, then scale by
                 the base size times the inner template lengths }
               t := binii('sub', ep^.r^.r1a, 1);
               if dodbgchk then begin
                  d := ld('i', i2p(ep^.l^.r2a));
                  errif(icmp('uge', t, d), ecValueOutOfRange)
               end;
               a := newv;
               oins; ov(a); os(' = add i64 '); oi(ep^.q1); os(', 0'); ol;
               b := ep^.l^.r2a;
               for n := 1 to ep^.q-1 do begin
                  b := binii('add', b, intsize);
                  d := ld('i', i2p(b));
                  a := bini('mul', a, d)
               end;
               ep^.r2a := binii('add', ep^.l^.r2a, intsize);
               c := bini('mul', a, t);
               ep^.r1a := bini('add', ep^.l^.r1a, c)
            end;

            {lft}
            213: begin
               ep^.r1a := ep^.l^.r1a;
               ep^.r2a := newv;
               oins; ov(ep^.r2a); os(' = add i64 '); osymadr(ep^.lt^); os(', 0'); ol
            end;

            {max}
            214: begin
               if dodbgchk then begin
                  errif(icmpi('ult', ep^.r^.r1a, 1), ecInvalidContainerLevel);
                  errif(icmpi('ugt', ep^.r^.r1a, ep^.q), ecInvalidContainerLevel)
               end;
               if ep^.q <> 1 then begin
                  t := newv;
                  oins; ov(t); os(' = sub i64 '); oi(ep^.q); os(', '); ov(ep^.r^.r1a); ol;
                  a := binii('shl', t, 4);
                  b := bini('add', ep^.l^.r2a, a);
                  ep^.r1a := ld('i', i2p(b))
               end else ep^.r1a := ep^.l^.r2a
            end;

            {equv,neqv,lesv,grtv,leqv,geqv}
            215,216,217,218,219,220: begin
               declfn('psystem_strcmp', 'i64', '(i64, i64, i64)');
               v := newv;
               oins; ov(v); os(' = call i64 @psystem_strcmp(i64 '); ov(ep^.l^.r1a);
               os(', i64 '); ov(ep^.r^.r1a); os(', i64 '); ov(ep^.l^.r2a); oc(')'); ol;
               case ep^.op of
                  215: ep^.r1a := zext1(icmpi('eq', v, 0));
                  216: ep^.r1a := zext1(icmpi('ne', v, 0));
                  220: ep^.r1a := zext1(icmpi('sge', v, 0));
                  218: ep^.r1a := zext1(icmpi('sgt', v, 0));
                  219: ep^.r1a := zext1(icmpi('sle', v, 0));
                  217: ep^.r1a := zext1(icmpi('slt', v, 0))
               end
            end;

            {spc}
            222: begin
               ep^.r1a := ep^.l^.r1a;
               ep^.r2a := ld('i', i2p(ep^.l^.r2a))
            end;

            {ccs}
            223: begin
               { total size: the base size times the length, held in the
                 second word for a one level container, or times the lengths
                 read from the template it points to for more levels; then a
                 stack copy }
               a := newv;
               oins; ov(a); os(' = add i64 '); oi(ep^.q1); os(', 0'); ol;
               if ep^.q = 1 then a := bini('mul', a, ep^.l^.r2a)
               else begin
                  b := ep^.l^.r2a;
                  for n := 1 to ep^.q do begin
                     d := ld('i', i2p(b));
                     a := bini('mul', a, d);
                     b := binii('add', b, intsize)
                  end
               end;
               t := newv;
               oins; ov(t); os(' = alloca i8, i64 '); ov(a); os(', align 16'); ol;
               ep^.r1a := p2i(t);
               memcpyv(ep^.r1a, ep^.l^.r1a, a);
               ep^.r2a := ep^.l^.r2a
            end;

            {ldp}
            225: begin
               ep^.r2a := ld('i', gep(i2p(ep^.l^.r1a), intsize));
               ep^.r1a := ld('i', i2p(ep^.l^.r1a))
            end;

            {mpc}
            248: begin
               genexp(ep^.l); genexp(ep^.r);
               ep^.r1a := ep^.l^.r1a; ep^.r2a := ep^.r^.r1a
            end;

            {cpl}
            251: ep^.r1a := ep^.l^.r2a;

            else error('Unsupported instruction in expression')

         end
      end
   end;

   { copy a set to a frame location }
   procedure setlocal(p, q: integer; s: integer);
   begin
      memcpy(p2i(locadr(p, q)), s, setsize)
   end;

   { start an initializer strip region at the pending labels }
   procedure openstrip;
   var np: nameptr; sp: stripptr; lp: nameptr;
   begin
      finfn;
      if pendlab = nil then error('Code outside of a routine');
      new(sp); sp^.blk := blkstk; sp^.pro.first := nil; sp^.pro.last := nil;
      sp^.body.first := nil; sp^.body.last := nil; sp^.calsites := nil;
      sp^.next := striplst; striplst := sp;
      curstrip := sp; stripn := stripn+1; instrip := true; vn := 0;
      blkopen := false;
      { the strip runs in the frame of the block's routine }
      fnstrip := false;
      if blkstk <> nil then fnlvl := blkstk^.lvl else fnlvl := 1;
      fnovf := blkovf(blkstk);
      { the labels, in definition order (the list is reversed) }
      lp := nil; np := pendlab;
      while np <> nil do begin lp := np; np := np^.next end;
      { emit from the last (first defined) back }
      while lp <> nil do begin
         defblk(lp^.name^);
         np := pendlab; if np = lp then np := nil
         else while np^.next <> lp do np := np^.next;
         lp := np
      end;
      pendlab := nil
   end;

   { start a function at the pending entry labels: a routine at its mst, or
     a code strip (a global initializer) at its first instruction }
   procedure openroutine(strip: boolean);
   var np: nameptr;
   begin
      finfn;
      if pendlab = nil then error('Code outside of a routine');
      { the first defined label names the function, the rest alias it; the
        list was built in reverse }
      np := pendlab;
      while np^.next <> nil do np := np^.next;
      openfn(np^.name^, strip);
      np := pendlab;
      while np <> nil do begin
         if not compcp(np^.name^, fnname^) then addname(fnalias, np^.name^);
         np := np^.next
      end;
      pendlab := nil
   end;

begin { assemble }

   p := 0;  q := 0;  q1 := 0; q2 := 0; q3 := 0; q4 := 0; op := 0;
   getname(name);
   while (instab[op].instr<>name) and (op < maxins) do op := op+1;
   if op = maxins then error('illegal instruction');
   { a case table ends at the first instruction that is not one of its
     entries }
   if intbl and not (op in [23{ujp}, 61{ujc}]) then flushtbl;
   { an instruction outside any routine, in a routine or program block,
     starts an initializer strip at the labels just defined: code the
     routine that follows runs in its own frame through cal (the mst of a
     routine opens its own function) }
   if (op <> 11) and not instrip and ((not fnopen) or fndone) then openstrip;
   case op of

      { *** non-terminals *** }

      {lodi,lodx,loda,lodr,lods,lodb,lodc,lda}
      0,193,105,106,107,108,109,4: begin parpq;
         q1 := -p*ptrsize; getexp(ep); attach(ep); pshstk(ep)
      end;

      {adi,adr,sbi,sbr}
      28, 29, 30, 31: begin
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {lip}
      120: begin parpq;
         q1 := -p*ptrsize; getexp(ep); pshstk(ep)
      end;

      {equm,neqm,geqm,grtm,leqm,lesm}
      142, 148, 154, 160, 166, 172: begin parq;
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {lao}
      5: begin skpspc;
         sp := nil;
         if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
         getexp(ep); ep^.fl := sp; attach(ep); pshstk(ep)
      end;

      {lto}
      234: begin labelsearch(def, val, sp, blk);
         getexp(ep); ep^.qs := sp; ep^.fl := sp; attach(ep); pshstk(ep)
      end;

      {ixa}
      16: begin parq;
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {swp}
      118: begin parq;
         popstk(ep); popstk(ep2); pshstk(ep); pshstk(ep2)
      end;

      {ldoi,ldoa,ldor,ldos,ldob,ldoc,ldox,ltci,ltcr,ltcs,ltcb,ltcc,ltcx}
      1, 65, 66, 67, 68, 69, 194,228,229,230,231,232,233: begin
         skpspc;
         sp := nil;
         if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
         getexp(ep); ep^.fl := sp; attach(ep); pshstk(ep)
      end;

      {indi,inda,indr,inds,indb,indc,indx}
      9, 85, 86, 87, 88, 89, 198: begin parq;
         getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep)
      end;

      {inci,inca,incb,incc,incx,deci,deca,decb,decc,decx}
      10, 90, 93, 94, 201, 57, 103, 104, 202: begin parq;
         getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep)
      end;

      {mdc}
      254: begin parq;
         getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep)
      end;

      {s2c}
      173: begin parq;
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {ckvi,ckvb,ckvc,ckvx}
      175, 179, 180, 203: begin parq;
         getexp(ep); pshstk(ep)
      end;

      {cpc}
      177: begin
         getexp(ep); popstk(ep2); popstk(ep3);
         duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
         genexp(ep); deltre(ep)
      end;

      {lpa}
      114: begin getlvl(p); labelsearch(def, val, sp, blk);
         q1 := -p*ptrsize; getexp(ep); ep^.fn := sp; pshstk(ep)
      end;

      {ldcs,ldci,ldcr,ldcn,ldcb,ldcc}
      7, 123, 124, 125, 126, 127: begin case op of
         123: begin getint(i);
            getexp(ep); attach(ep); ep^.vi := i; pshstk(ep)
         end;
         124: begin getreal(r);
            getexp(ep); attach(ep); pshstk(ep);
            ro.rv := r; ep^.vi := ro.iv
         end;
         125: begin
            getexp(ep); pshstk(ep)
         end;
         126: begin getint(i);
            getexp(ep); attach(ep); ep^.vi := i; pshstk(ep)
         end;
         127: begin
            skpspc;
            if ch in ['0'..'9'] then begin i := 0;
               while ch in ['0'..'9'] do
                  begin i := i*10+ord(ch)-ord('0'); getnxt end;
               c := chr(i)
            end else begin
               if ch <> '''' then error('illegal character');
               getnxt;  c := ch;
               getnxt;
               if ch <> '''' then error('illegal character')
            end;
            getexp(ep); attach(ep); ep^.vi := ord(c); pshstk(ep)
         end;
         7: begin skpspc;
            if ch <> '(' then error('ldcs() expected');
            s := [ ];  getnxt;
            while ch<>')' do
               begin getint(s1); skpspc; s := s + [s1] end;
            getexp(ep); attach(ep); pshstk(ep);
            new(cstp); cstp^.ct := cset; cstp^.s := s;
            setnum := setnum+1; cstp^.setn := setnum;
            cstp^.next := csttbl; csttbl := cstp; ep^.setn := setnum
         end
         end
      end;

      {chki,chks,chkb,chkc,ckla,chkx}
      26, 97, 98, 99, 190, 199: begin getint(lb); getint(ub);
         getexp(ep); popstk(ep^.l);
         pshstk(ep); ep^.vi := lb; ep^.vi2 := ub
      end;

      {chka}
      95: begin getint(lb); getint(ub);
         if lb <> 0 then begin getexp(ep); popstk(ep^.l); pshstk(ep) end
      end;

      {lca}
      56: begin getint(l); skpspc;
         for i := 1 to strlen do str[i] := ' ';
         if ch <> '''' then error('bad string format');
         i := 0;
         repeat
            if eolinp then error('unterminated string');
            getnxt;
            c := ch; dc := ch;
            if (ch = '''') and (chla = '''') then begin getnxt; c := ' '; dc := '''' end
            else if ch = chr(92) then begin
               getnxt;
               if ch = chr(92) then dc := chr(92)
               else begin
                  getnxt;
                  if ch <= '9' then v := ord(ch)-ord('0') else v := ord(ch)-ord('a')+10;
                  getnxt;
                  if ch <= '9' then v := v*16+ord(ch)-ord('0')
                               else v := v*16+ord(ch)-ord('a')+10;
                  dc := chr(v)
               end;
               c := ' '
            end;
            if c <> '''' then begin
               if i >= strlen then error('string overflow');
               str[i+1] := dc;
               i := i+1
            end
         until c = '''';
         getexp(ep); attach(ep); pshstk(ep);
         new(cstp); cstp^.ct := cstr; cstp^.str := extract(str, 1, len(str));
         cstp^.strl := l; strnum := strnum+1; cstp^.strn := strnum;
         cstp^.next := csttbl; csttbl := cstp; ep^.strn := strnum
      end;

      {grts,less}
      158,170: error('Invalid operand');

      {equa,equi,equr,equb,equs,equc}
      17, 137, 138, 139, 140, 141,
      {neqa,neqi,neqr,neqb,neqs,neqc}
      18, 143, 144, 145, 146, 147,
      {geqi,geqr,geqb,geqs,geqc}
      149, 150, 151, 152, 153,
      {grti,grtr,grtb,grtc}
      20, 155, 156, 157, 159,
      {leqi,leqr,leqb,leqs,leqc}
      161, 162, 163, 164, 165,
      {lesi,lesr,lesb,lesc}
      167, 168, 169, 171: begin
         getexp(ep);
         if op = 164 then begin popstk(ep^.l); popstk(ep^.r) end
         else begin popstk(ep^.r); popstk(ep^.l) end;
         pshstk(ep)
      end;

      {brk}
      19: ;

      {ord}
      59, 134, 136, 200: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {lcp}
      135: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {sgs}
      32: begin
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {flt}
      33: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {flo}
      34: begin
         getexp(ep); popstk(ep2); popstk(ep^.l);
         pshstk(ep); pshstk(ep2)
      end;

      {trc}
      35: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {ngi,ngr}
      36,37: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {sqi,sqr}
      38,39: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {abi,abr}
      40,41: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {notb,odd,chr,rnd,noti}
      42,50,60,62,205: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {and,ior,xor,inn,mod,mpi,mpr,dvi,dvr}
      43,44,48,49,51,52,53,54,206: begin
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {dif,int,uni,rgs}
      45,46,47,110: begin
         getexp(ep); popstk(ep^.x1); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      { 181-186: reserved }
      181, 182, 183, 184, 185, 186: ;

      {cks}
      187: begin
         getexp(ep); pshstk(ep)
      end;

      {sfr}
      245: begin labelsearch(def, val, sp, blk);
         getexp(ep); pshstk(ep);
         ep^.lb := nil;
         if (def and (val <> 0)) or not def then ep^.lb := sp
      end;

      {cuf}
      246: begin labelsearch(def, val, sp, blk);
         getadr(q); getadr(q1); getadr(q2); getadr(q3);
         getexp(ep); ep^.fn := sp; ep^.pn := q; ep^.rc := q1; ep^.blk := blk;
         getpar(ep); pshstk(ep)
      end;

      {cif}
      247: begin
         getadr(q); getadr(q1); getadr(q2); getadr(q3);
         getexp(ep); ep^.pn := q; ep^.rc := q1; popstk(ep^.l); getpar(ep);
         pshstk(ep)
      end;

      {cvf}
      249: begin
         skpspc;
         sp := nil;
         if ch = 'l' then begin
            labelsearch(def, val, sp, blk);
            getadr(q1); getadr(q2); getadr(q3); getadr(q4)
         end else begin getadr(q); getadr(q1); getadr(q2); getadr(q3); getadr(q4) end;
         getexp(ep); ep^.qs := sp; ep^.pn := q1; ep^.rc := q2;
         getpar(ep); pshstk(ep)
      end;

      {cke}
      188: begin
         getexp(ep);
         ep4 := estack;
         while ep4 <> nil do begin
            if estack^.op in [187,179,180,175,203] then begin
               popstk(ep5);
               if ep5^.op <> 187 then begin ep5^.next := ep^.cl; ep^.cl := ep5 end
               else putexp(ep5);
               ep4 := estack
            end else ep4 := nil
         end;
         popstk(ep^.l); pshstk(ep)
      end;

      {wbs}
      243: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {cxs}
      211: begin parq;
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {cxc}
      212: begin parqq;
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {lft}
      213: begin labelsearch(def, val, sp, blk);
         getexp(ep); popstk(ep^.l); ep^.lt := sp; pshstk(ep)
      end;

      {max}
      214: begin parq;
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {equv,neqv,lesv,grtv,leqv,geqv}
      215,216,217,218,219,220: begin
         getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
      end;

      {spc}
      222: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {ccs}
      223: begin parqq;
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {ldp}
      225: begin
         getexp(ep); popstk(ep^.l); pshstk(ep)
      end;

      {mpc}
      248: begin parqq;
         ep4 := nil; popstk(ep2); i := q;
         while i > 0 do begin
            ep2^.next := ep4; ep4 := ep2; popstk(ep2); i := i-1
         end;
         getexp(ep); popstk(ep3);
         if q1 = 0 then begin ep^.l := ep2; ep^.r := ep3 end
         else begin ep^.l := ep3; ep^.r := ep2 end;
         pshstk(ep);
         while ep4 <> nil do begin ep := ep4; ep4 := ep4^.next; pshstk(ep) end
      end;

      {cpl}
      251: begin
         getexp(ep);
         popstk(ep2); duptre(ep2, ep3); pshstk(ep2);
         ep^.l := ep3; pshstk(ep)
      end;

      { *** calls can be terminal or non-terminal *** }

      {csp}
      15: begin skpspc; getname(name);
         while name<>sfptab[q].sptable do begin
            q := q+1; if q > maxsp then error('std proc/func not found')
         end;
         getexp(ep);
         if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then
            begin getparn(ep, maxint); revpar(ep); ordpar(ep) end
         else getparc(ep, sfptab[q].sppar);
         if sfptab[q].spfunc then pshstk(ep)
         else begin genexp(ep); deltre(ep) end
      end;

      {cuv}
      27: begin
         skpspc;
         sp := nil;
         if ch = 'l' then begin
            labelsearch(def, val, sp, blk);
            getadr(q1)
         end else begin getadr(q); getadr(q1) end;
         getexp(ep); ep^.qs := sp; ep^.pn := q1; getpar(ep);
         genexp(ep); deltre(ep)
      end;

      { *** terminals *** }

      {cvbi,cvbx,cvbb,cvbc}
      100, 115, 116, 121,
      {ivti,ivtx,ivtb,ivtc,cta}
      192,101,102,111,191: begin getadr(q); getadr(q1);
         labelsearch(def, val, sp, blk);
         getexp(ep); ep^.lt := sp; popstk(ep2); popstk(ep3);
         duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
         genexp(ep); deltre(ep)
      end;

      {cup}
      12: begin labelsearch(def, val, sp, blk); getadr(q1);
         getexp(ep); ep^.fn := sp; ep^.pn := q1; ep^.blk := blk; getpar(ep);
         genexp(ep); deltre(ep)
      end;

      {cip}
      113: begin parq;
         getexp(ep); ep^.pn := q; popstk(ep^.l); getpar(ep);
         genexp(ep); deltre(ep)
      end;

      {rip}
      13: begin skpspc;
         if ch = 'l' then labelsearch(def, val, sp, blk)
         else parq
      end;

      {stri,stra}
      2,70: begin parpq;
         popstk(ep); attach(ep); genexp(ep);
         st('i', ep^.r1a, locadr(p, q));
         deltre(ep)
      end;

      {strx,strb,strc}
      195,73,74: begin parpq;
         popstk(ep); attach(ep); genexp(ep);
         st('b', ep^.r1a, locadr(p, q));
         deltre(ep)
      end;

      {strr}
      71: begin parpq;
         popstk(ep); attach(ep); genexp(ep);
         st('r', ep^.r1a, locadr(p, q));
         deltre(ep)
      end;

      {strs}
      72: begin parpq;
         popstk(ep); attach(ep); genexp(ep);
         setlocal(p, q, ep^.r1a);
         deltre(ep)
      end;

      {sev}
      253: begin parpq;
         { the vector of the current exception frame }
         v := newv;
         oins; ov(v); os(' = call i64 @psystem_llvm_curvec()'); ol;
         st('i', v, locadr(p, q))
      end;

      {mst}
      11: begin getlvl(p); labelsearch(def, val, sp, blk);
         labelsearch(def2, val2, sp2, blk);
         if p >= maxlvl then error('Too many nested levels');
         openroutine(false);
         fnlvl := p+1;
         fnlcl := sp;
         parparms(blkstk);
         fnovf := blkovf(blkstk);
         splicestrips;
         if blkstk <> nil then if blkstk^.btyp in [btproc, btfunc] then begin
            { the block's qualified name is an alias for the entry }
            ll := 0; oblklong(blkstk); tmps := lbstr; addname(fnalias, tmps^); ll := 0
         end;
         botstk;
         prologue_pending := true
      end;

      {mov}
      55: begin parq;
         popstk(ep); popstk(ep2); genexp(ep2); genexp(ep);
         memcpy(ep2^.r1a, ep^.r1a, q);
         deltre(ep); deltre(ep2);
         botstk
      end;

      {dmp}
      117: begin parq;
         if estack <> nil then begin popstk(ep); deltre(ep) end
      end;

      {sroi,sroa,sror,srob,sroc,srox}
      3, 75, 76, 78, 79, 196: begin
         skpspc;
         sp := nil;
         if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
         popstk(ep); attach(ep); genexp(ep);
         if sp <> nil then v := symadr(sp^) else v := gbladr(q);
         if op in [78, 79, 196] then st('b', ep^.r1a, v)
         else if op = 76 then st('r', ep^.r1a, v)
         else st('i', ep^.r1a, v);
         deltre(ep)
      end;

      {sros}
      77: begin
         skpspc;
         sp := nil;
         if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
         popstk(ep); attach(ep); genexp(ep);
         if sp <> nil then v := symadr(sp^) else v := gbladr(q);
         memcpy(p2i(v), ep^.r1a, setsize);
         deltre(ep)
      end;

      {aps}
      178: begin parq;
         popstk(ep2); popstk(ep); genexp(ep); genexp(ep2);
         v := binii('mul', ep^.r2a, q);
         memcpyv(ep^.r1a, ep2^.r1a, v);
         deltre(ep2); deltre(ep);
         botstk
      end;

      {pck}
      63: begin parqq;
         popstk(ep); popstk(ep2); popstk(ep3);
         genexp(ep); genexp(ep2); genexp(ep3);
         declfn('psystem_pack', 'void', '(i64, i64, i64, i64, i64)');
         oins; os('call void @psystem_pack(i64 '); oi(q); os(', i64 '); oi(q1); os(', i64 ');
         ov(ep^.r1a); os(', i64 '); ov(ep2^.r1a); os(', i64 '); ov(ep3^.r1a); oc(')'); ol;
         deltre(ep); deltre(ep2); deltre(ep3);
         botstk
      end;

      {upk}
      64: begin parqq;
         popstk(ep); popstk(ep2); popstk(ep3);
         genexp(ep); genexp(ep2); genexp(ep3);
         declfn('psystem_unpack', 'void', '(i64, i64, i64, i64, i64)');
         oins; os('call void @psystem_unpack(i64 '); oi(q); os(', i64 '); oi(q1); os(', i64 ');
         ov(ep^.r1a); os(', i64 '); ov(ep2^.r1a); os(', i64 '); ov(ep3^.r1a); oc(')'); ol;
         deltre(ep); deltre(ep2); deltre(ep3);
         botstk
      end;

      {ujp}
      23: begin labelsearch(def, val, sp, blk);
         if intbl then begin
            { a case table entry }
            if tblcnt >= maxcase then error('Case table too large');
            tblcnt := tblcnt+1; tblent[tblcnt] := sp
         end else begin
            oins; os('br '); olab(sp^); ol; term;
            if estack <> nil then begin
               getexp(ep); ep^.qs := sp;
               ep^.l := estack; estack := nil; ep^.next := jmpstr; jmpstr := ep
            end
         end
      end;

      {fjp,tjp}
      24,119: begin labelsearch(def, val, sp, blk);
         popstk(ep); genexp(ep);
         genlab('fall', sp2);
         v := icmpi('ne', ep^.r1a, 0);
         oins; os('br i1 '); ov(v); os(', ');
         if op = 24 then begin olab(sp2^); os(', '); olab(sp^) end
         else begin olab(sp^); os(', '); olab(sp2^) end;
         ol; term;
         defblk(sp2^);
         deltre(ep)
      end;

      {xjp}
      25: begin labelsearch(def, val, sp, blk);
         popstk(ep); genexp(ep);
         xjpidx := ep^.r1a; xjptbl := sp;
         oins; os('br '); olab(sp^); ol; term;
         deltre(ep);
         botstk
      end;

      {ipj}
      112: begin getlvl(p); labelsearch(def, val, sp, blk);
         { the target label belongs to the routine at level p, generated
           later: record it pending, keyed by its number, and jump through
           that routine's table }
         v := labelkey(sp);
         new(ip); ip^.name := sp; ip^.key := v; ip^.next := ipjpend; ipjpend := ip;
         v2 := frameof(p);
         oins; os('call void @psystem_llvm_ipj(ptr '); ov(v2); os(', i64 '); oi(v); oc(')'); ol;
         oins; os('unreachable'); ol; term;
         botstk
      end;

      {vbs}
      92: begin parq;
         popstk(ep); genexp(ep);
         declfn('psystem_varenter', 'void', '(i64, i64)');
         v := binii('add', ep^.r1a, q-1);
         oins; os('call void @psystem_varenter(i64 '); ov(ep^.r1a); os(', i64 '); ov(v); oc(')'); ol;
         deltre(ep);
         botstk
      end;

      {vbe}
      96: begin
         declfn('psystem_varexit', 'void', '()');
         oins; os('call void @psystem_varexit()'); ol;
         botstk
      end;

      {ret}
      22: begin
         if instrip then begin
            { the strip returns to its caller through the dispatch }
            oins; os('br label %"calsw"'); ol; term;
            instrip := false; curstrip := nil
         end else begin
            oins; os('ret void'); ol; term;
            fndone := true; fnretk := 0
         end;
         botstk
      end;

      {retp,retm}
      14,237: begin parq;
         oins; os('ret void'); ol; term;
         fndone := true; fnretk := 0;
         botstk; deltmp
      end;

      {reti,reta,retx,retc,retb}
      128,132,204,130,131: begin parq;
         v := ld('i', gep(vfb, -(fnlvl*ptrsize+7*ptrsize)));
         if op in [204, 130, 131] then v := binii('and', v, 255);
         oins; os('ret i64 '); ov(v); ol; term;
         fndone := true; fnretk := 1;
         botstk; deltmp
      end;

      {retr}
      129: begin parq;
         v := ld('r', gep(vfb, -(fnlvl*ptrsize+7*ptrsize)));
         oins; os('ret double '); ov(v); ol; term;
         fndone := true; fnretk := 2;
         botstk; deltmp
      end;

      {rets}
      236: begin parq;
         oins; os('ret void'); ol; term;
         fndone := true; fnretk := 0;
         botstk; deltmp
      end;

      {stoi,stoa,stor,stob,stoc,stox}
      6, 80, 81, 83, 84, 197: begin
         popstk(ep2); popstk(ep); attach(ep);
         genexp(ep); genexp(ep2);
         v := gep(i2p(ep^.r1a), q);
         case op of
            6, 80: st('i', ep2^.r1a, v);
            81: st('r', ep2^.r1a, v);
            83, 84, 197: st('b', ep2^.r1a, v)
         end;
         deltre(ep); deltre(ep2)
      end;

      {stos}
      82: begin
         popstk(ep2); popstk(ep); attach(ep);
         genexp(ep); genexp(ep2);
         memcpy(ep^.r1a, ep2^.r1a, setsize);
         deltre(ep); deltre(ep2)
      end;

      {stom}
      235: begin parqq;
         popstk(ep2); popstk(ep); attach(ep);
         genexp(ep); genexp(ep2);
         memcpy(ep^.r1a, ep2^.r1a, q);
         deltre(ep); deltre(ep2)
      end;

      {stp}
      58: ;

      {inv}
      189: begin
         popstk(ep); deltre(ep);
         botstk
      end;

      {ujc}
      61: begin
         if intbl then begin
            if tblcnt >= maxcase then error('Case table too large');
            tblcnt := tblcnt+1; tblent[tblcnt] := nil
         end else begin
            emiterr(ecCaseValueNotFound);
            oins; os('unreachable'); ol; term
         end;
         botstk
      end;

      {cjp}
      8: begin getadr(q); getadr(q1); labelsearch(def, val, sp, blk);
         popstk(ep); genexp(ep);
         genlab('cjp', sp2);
         v := icmpi('sge', ep^.r1a, q);
         v2 := icmpi('sle', ep^.r1a, q1);
         v3 := newv;
         oins; ov(v3); os(' = and i1 '); ov(v); os(', '); ov(v2); ol;
         oins; os('br i1 '); ov(v3); os(', '); olab(sp^); os(', '); olab(sp2^); ol; term;
         defblk(sp2^);
         pshstk(ep)
      end;

      {wbe}
      244: begin
         declfn('psystem_withexit', 'void', '()');
         oins; os('call void @psystem_withexit()'); ol;
         botstk
      end;

      {vip}
      133: begin parqq;
         popstk(ep);
         v := genlist(q);
         genexp(ep);
         v2 := p2i(v);
         declfn('psystem_vip', 'void', '(i64, i64, i64, i64)');
         oins; os('call void @psystem_vip(i64 '); oi(q); os(', i64 '); oi(q1); os(', i64 ');
         ov(ep^.r1a); os(', i64 '); ov(v2); oc(')'); ol;
         deltre(ep);
         botstk
      end;

      {vis}
      122: begin parqq;
         popstk(ep);
         v := genlist(q);
         genexp(ep);
         declfn('psystem_vis', 'i64', '(i64, i64, i64, i64)');
         v3 := p2i(v);
         v2 := newv;
         oins; ov(v2); os(' = call i64 @psystem_vis(i64 '); oi(q); os(', i64 '); oi(q1); os(', i64 ');
         ov(ep^.r1a); os(', i64 '); ov(v3); oc(')'); ol;
         v3 := newv;
         oins; ov(v3); os(' = alloca i8, i64 '); ov(v2); os(', align 16'); ol;
         st('i', p2i(v3), i2p(ep^.r1a));
         deltre(ep);
         botstk
      end;

      {vin}
      226: begin parqq;
         popstk(ep);
         v := genlist(q);
         genexp(ep);
         v2 := p2i(v);
         declfn('psystem_vin', 'void', '(i64, i64, i64, i64)');
         oins; os('call void @psystem_vin(i64 '); oi(q); os(', i64 '); oi(q1); os(', i64 ');
         ov(ep^.r1a); os(', i64 '); ov(v2); oc(')'); ol;
         deltre(ep);
         botstk
      end;

      {suv}
      91: begin labelsearch(def, val, sp, blk);
         skpspc;
         sp2 := nil;
         if ch = 'l' then labelsearch(def, val, sp2, blk) else getadr(q1);
         v := newv;
         oins; ov(v); os(' = add i64 '); osymadr(sp^); os(', 0'); ol;
         if sp2 <> nil then st('i', v, symadr(sp2^)) else st('i', v, gbladr(q1))
      end;

      {cal}
      21: begin labelsearch(def, val, sp, blk);
         if fnstrip and not instrip then begin
            { the module strip calling the module end label: the stub that
              continues the module chain }
            oins; os('call void @'); oq(sp^); os('()'); ol
         end else begin
            { a local call to an initializer strip in this frame: push the
              site, branch, and continue at the site's return label }
            calcnt := calcnt+1;
            new(cp); cp^.k := calcnt;
            if instrip then begin cp^.next := curstrip^.calsites; curstrip^.calsites := cp end
            else begin cp^.next := fncals; fncals := cp end;
            v := newv;
            oins; ov(v); os(' = load i64, ptr %calsp'); ol;
            v2 := newv;
            oins; ov(v2); os(' = getelementptr i64, ptr %calra, i64 '); ov(v); ol;
            oins; os('store i64 '); oi(calcnt); os(', ptr '); ov(v2); ol;
            v3 := newv;
            oins; ov(v3); os(' = add i64 '); ov(v); os(', 1'); ol;
            oins; os('store i64 '); ov(v3); os(', ptr %calsp'); ol;
            oins; os('br '); olab(sp^); ol; term;
            genlab('calret', sp2);
            { the return label is the site number }
            ll := 0; os('calret.'); oi(calcnt); sp2 := lbstr; ll := 0;
            defblk(sp2^)
         end
      end;

      {bge}
      207: begin labelsearch(def, val, sp, blk);
         { an exception frame in the prologue, registered, then the setjmp
           whose second return lands on the handler }
         v := alloca(256, 'exception frame');
         oins; os('call void @psystem_llvm_bge(ptr '); ov(v); oc(')'); ol;
         v2 := newv;
         oins; ov(v2); os(' = call i32 @_setjmp(ptr '); ov(v); oc(')'); ol;
         v3 := newv;
         oins; ov(v3); os(' = icmp ne i32 '); ov(v2); os(', 0'); ol;
         genlab('try', sp2);
         oins; os('br i1 '); ov(v3); os(', '); olab(sp^); os(', '); olab(sp2^); ol; term;
         defblk(sp2^);
         botstk
      end;

      {ede}
      208: begin
         oins; os('call void @psystem_llvm_ede()'); ol;
         botstk
      end;

      {mse}
      209: begin
         oins; os('call void @psystem_llvm_mse(i64 ptrtoint (ptr @modnam to i64), i64 '); oline; oc(')'); ol;
         oins; os('unreachable'); ol; term;
         botstk
      end;

      {apc}
      210: begin parqq;
         popstk(ep); popstk(ep2);
         genexp(ep2); genexp(ep);
         declfn('psystem_apc', 'void', '(i64, i64, i64, i64, i64)');
         oins; os('call void @psystem_apc(i64 '); oi(q); os(', i64 '); oi(q1); os(', i64 ');
         ov(ep^.r1a); os(', i64 '); ov(ep2^.r1a); os(', i64 '); ov(ep2^.r2a); oc(')'); ol;
         deltre(ep); deltre(ep2);
         botstk
      end;

      {vdp,vdd}
      221,227: begin
         popstk(ep); genexp(ep);
         declfn('psystem_dsp', 'void', '(i64, i64)');
         oins; os('call void @psystem_dsp(i64 '); ov(ep^.r1a); os(', i64 1)'); ol;
         deltre(ep);
         botstk
      end;

      {scp}
      224: begin
         popstk(ep2); popstk(ep);
         genexp(ep); genexp(ep2);
         v := i2p(ep^.r1a);
         st('i', ep2^.r1a, v);
         st('i', ep2^.r2a, gep(v, intsize));
         deltre(ep); deltre(ep2)
      end;

      {ctb}
      238: error('ctb is not supported by the LLVM target');

      {cps}
      176: begin
         getexp(ep); popstk(ep2); popstk(ep3);
         duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
         genexp(ep); deltre(ep)
      end;

      {cpp}
      239: parqq;
      {cpr}
      240: parqq;

      {sfs}
      252: begin parqq;
         popstk(ep2); popstk(ep); attach(ep);
         genexp(ep); genexp(ep2);
         memcpy(ep2^.r1a, ep^.r1a, q);
         deltre(ep); deltre(ep2)
      end;

      {lsa}
      241: parq;
      {lsp}
      250: ;

      else error('Unsupported instruction')

   end

end; (*assemble*)

begin (* main *)

   proginit; { perform independent init }
   convreq := true; { require the amd64_sysv deck }
   fnopen := false; fndone := false; blkopen := false;
   pendlab := nil; declst := nil; defsyms := nil; refsyms := nil; ipjlst := nil;
   refrtn := nil;
   ipjpend := nil;
   xjptbl := nil; intbl := false; tblcnt := 0; vn := 0; labn := 0; ll := 0;
   inpro := false; instrip := false; stripn := 0; curstrip := nil; striplst := nil;
   calcnt := 0; fncals := nil; fnstrips.first := nil; fnstrips.last := nil;
   donelst := nil;
   write('P6 Pascal LLVM IR code generator vs. ', majorver:1, '.', minorver:1);
   if experiment then write('.x');
   writeln;
   writeln;
   parcmdlin; { parse command line }
   rewrite(prr);
   writeln('Generating program');
   write(prr, '; File generated by P6 Pascal LLVM IR code generator vs. ', majorver:1, '.', minorver:1);
   if experiment then write(prr, '.x');
   writeln(prr);
   writeln(prr);
   xlate; (* assembles and stores code *)
   if not amd64_sysv then error('Calling convention mismatch');
   if windows or arm64_sysv then error('Calling convention mismatch');
   99 : { abort run }
   writeln;
   if errret then writeln('Program generation aborted')
   else writeln('Program generation complete');
   seterr(ord(errret));

end.
