#ifndef FPC_PASCAL
(*$c+,t-,d-,l-,s+*)
#endif
{*******************************************************************************
*                                                                              *
*                           Portable Pascal compiler                           *
*                           ************************                           *
*                                                                              *
*                                 Pascal P6                                    *
*                                                                              *
*                                 ETH May 76                                   *
*                                                                              *
* Authors:                                                                     *
*                                                                              *
*    Urs Ammann                                                                *
*    Kesav Nori                                                                *
*    Christian Jacobi                                                          *
*    K. Jensen                                                                 *
*    N. Wirth                                                                  *
*                                                                              *
* Address:                                                                     *
*                                                                              *
*    Institut Fuer Informatik                                                  *
*    Eidg. Technische Hochschule                                               *
*    CH-8096 Zuerich                                                           *
*                                                                              *
*  This code is fully documented in the book                                   *
*        "Pascal Implementation"                                               *
*   by Steven Pemberton and Martin Daniels                                     *
* published by Ellis Horwood, Chichester, UK                                   *
*         ISBN: 0-13-653-0311                                                  *
*       (also available in Japanese)                                           *
*                                                                              *
* Steven Pemberton, CWI/AA,                                                    *
* Kruislaan 413, 1098 SJ Amsterdam, NL                                         *
* Steven.Pemberton@cwi.nl                                                      *
*                                                                              *
* Adaption from Pascal-P5 to Pascal-P6 by:                                     *
*                                                                              *
*    Scott A. Franco                                                           *
*    samiam@moorecad.com                                                       *
*                                                                              *
* AMD64 code generator for GCC                                                 *
*                                                                              *
* This is the code generator backend for 64 bit AMD64 processor model running  *
* with a gcc code base. It generates GAS (GNU Assembler) source according to   *
* the System V Application Binary Interface, AMD64 Architecture Processor      *
* supplement.                                                                  *
*                                                                              *
* ---------------------------------------------------------------------------- *
*                                                                              *
*                                   LICENSE                                    *
*                                                                              *
* ---------------------------------------------------------------------------- *
*                                                                              *
* This software is unlicensed and exists in the public domain. It has:         *
*                                                                              *
* 1. Been acknowledged as public domain by the author, Niklaus Wirth at ETH    *
*    Zurich.                                                                   *
*                                                                              *
* 2. Has been freely distributed since 1976 with only charges for printing and *
*    shipping costs.                                                           *
*                                                                              *
* 3. Has been used as the basis for many projects, both paid and free, by      *
*    other authors.                                                            *
*                                                                              *
* I, Scott Franco, have extensively expanded the original software. I certify  *
* that all my changes and additions to it are also public domain.              *
*                                                                              *
* I respectfully request that this notice accompany the software even if it is *
* further modified.                                                            *
*                                                                              *
* If you receive a copy of this software without this notice, I suggest you    *
* obtain the original. It has been modified.                                   *
*                                                                              *
*******************************************************************************}

program pcode(input,output,prd,prr);

label 1;

const

      { ************************************************************************

      Program object sizes and characteristics, sync with pint. These define
      the machine specific characteristics of the target.

      The configurations are as follows:

      type                  #bits 32  #bits 64
      ===========================================================
      integer               32        64
      real                  64        64
      char                  8         8
      boolean               8         8
      set                   256       256
      pointers              32        64
      marks                 32        64
      File logical number   8         8

      Both endian types are supported. There is no alignment needed, but you
      may wish to use alignment to tune the runtime speed.

      The machine characteristics dependent on byte accessable machines. This
      table is all you should need to adapt to any byte addressable machine.

      }

#include <mpb64.inc>

      { internal constants }

      { !!! Need to use the small size memory to self compile, otherwise, by
        definition, pint cannot fit into its own memory. }
      {elide}maxstr      = 16777215;{noelide}  { maximum size of addressing for program/var }
      {remove maxstr     =  2000000; remove}  { maximum size of addressing for program/var }
      maxdef      = 2097152; { maxstr / 8 for defined bits }
      maxdigh     = 6;       { number of digits in hex representation of maxstr }
      maxdigd     = 8;       { number of digits in decimal representation of maxstr }

      codemax     = maxstr;  { set size of code store to maximum possible }

      maxlabel = 5000;       { total possible labels in intermediate }
      resspc   = 0;          { reserve space in heap (if you want) }

      { locations of header files after program block mark, each header
        file is two values, a file number and a single character buffer }
      inputoff  = 0;         { 'input' file address }
      outputoff = 2;         { 'output' file address }
      prdoff    = 4;         { 'prd' file address }
      prroff    = 6;         { 'prr' file address }

      { assigned logical channels for header files }
      inputfn    = 1;        { 'input' file no. }
      outputfn   = 2;        { 'output' file no. }
      prdfn      = 3;        { 'prd' file no. }
      prrfn      = 4;        { 'prr' file no. }

      { locations of standard exceptions }
      exceptionbase                      = 14;
      ValueOutOfRange                    = 14;
      ArrayLengthMatch                   = 15;
      CaseValueNotFound                  = 16;
      ZeroDivide                         = 17;
      InvalidOperand                     = 18;
      NilPointerDereference              = 19;
      RealOverflow                       = 20;
      RealUnderflow                      = 21;
      RealProcessingFault                = 22;
      TagValueNotActive                  = 23;
      TooManyFiles                       = 24;
      FileIsOpen                         = 25;
      FileAlreadyNamed                   = 26;
      FileNotOpen                        = 27;
      FileModeIncorrect                  = 28;
      InvalidFieldSpecification          = 29;
      InvalidRealNumber                  = 30;
      InvalidFractionSpecification       = 31;
      InvalidIntegerFormat               = 32;
      IntegerValueOverflow               = 33;
      InvalidRealFormat                  = 34;
      EndOfFile                          = 35;
      InvalidFilePosition                = 36;
      FilenameTooLong                    = 37;
      FileOpenFail                       = 38;
      FileSIzeFail                       = 39;
      FileCloseFail                      = 40;
      FileReadFail                       = 41;
      FileWriteFail                      = 42;
      FilePositionFail                   = 43;
      FileDeleteFail                     = 44;
      FileNameChangeFail                 = 45;
      SpaceAllocateFail                  = 46;
      SpaceReleaseFail                   = 47;
      SpaceAllocateNegative              = 48;
      CannotPerformSpecial               = 49;
      CommandLineTooLong                 = 50;
      ReadPastEOF                        = 51;
      FileTransferLengthZero             = 52;
      FileSizeTooLarge                   = 53;
      FilenameEmpty                      = 54;
      CannotOpenStandard                 = 55;
      TooManyTemporaryFiles              = 56;
      InputBufferOverflow                = 57;
      TooManyThreads                     = 58;
      CannotStartThread                  = 59;
      InvalidThreadHandle                = 60;
      CannotStopThread                   = 61;
      TooManyIntertaskLocks              = 62;
      InvalidLockHandle                  = 63;
      LockSequenceFail                   = 64;
      TooManySignals                     = 65;
      CannotCreateSignal                 = 66;
      InvalidSignalHandle                = 67;
      CannotDeleteSignal                 = 68;
      CannotSendSignal                   = 69;
      WaitForSignalFail                  = 70;
      FieldNotBlank                      = 71;
      ReadOnWriteOnlyFile                = 72;
      WriteOnReadOnlyFile                = 73;
      FileBufferVariableUndefined        = 74;
      NondecimalRadixOfNegative          = 75;
      InvalidArgumentToLn                = 76;
      InvalidArgumentToSqrt              = 77;
      CannotResetOrRewriteStandardFile   = 78;
      CannotResetWriteOnlyFile           = 79;
      CannotRewriteReadOnlyFile          = 80;
      SetElementOutOfRange               = 81;
      RealArgumentTooLarge               = 82;
      BooleanOperatorOfNegative          = 83;
      InvalidDivisorToMod                = 84;
      PackElementsOutOfBounds            = 85;
      UnpackElementsOutOfBounds          = 86;
      CannotResetClosedTempFile          = 87;
      exceptiontop                       = 87;

      { Exceptions that can't be caught.
        Note that these don't have associated exception variables. }

      UndefinedLocationAccess            = 88;
      FunctionNotImplemented             = 89;
      InvalidInISO7185Mode               = 90;
      HeapFormatInvalid                  = 91;
      DisposeOfUninitalizedPointer       = 92;
      DisposeOfNilPointer                = 93;
      BadPointerValue                    = 94;
      BlockAlreadyFreed                  = 95;
      InvalidStandardProcedureOrFunction = 96;
      InvalidInstruction                 = 97;
      NewDisposeTagsMismatch             = 98;
      PCOutOfRange                       = 99;
      StoreOverflow                      = 100;
      StackBalance                       = 101;
      SetInclusion                       = 102;
      UninitializedPointer               = 103;
      DereferenceOfNilPointer            = 104;
      PointerUsedAfterDispose            = 105;
      VariantNotActive                   = 106;
      InvalidCase                        = 107;
      SystemError                        = 108;
      ChangeToAllocatedTagfield          = 109;
      UnhandledException                 = 110;
      ProgramCodeAssertion               = 111;
      VarListEmpty                       = 112;
      ChangeToVarReferencedVariant       = 113;
      DisposeOfVarReferencedBlock        = 114;
      VarReferencedFileBufferModified    = 115;
      ContainerMismatch                  = 116;
      InvalidContainerLevel              = 117;
      privexceptiontop                   = 117;

      strlen      = 1000;    { longest string length we can buffer }
      maxsp       = 81;      { number of predefined procedures/functions }
      maxins      = 255;     { maximum instruction code, 0-255 or byte }
      maxfil      = 100;     { maximum number of general (temp) files }
      maxalfa     = 10;      { maximum number of characters in alfa type }
      lablen      = 4000;    { label maximum length }
      varsqt      = 10;      { variable string quanta }

      { coder parameters }
      maxreg      = 1000;    { maximum virtual registers to allocate }
      maxphy      = 6;       { maximum physical registers to allocate }
      tabspc      = 8;       { tab spacing on assembly code }

      { version numbers }

      majorver   = 0; { major version number }
      minorver   = 2; { minor version number }
      experiment = true; { is version experimental? }

type
      { These equates define the instruction layout. I have choosen a 32 bit
        layout for the instructions defined by (4 bit) digit:

           byte 0:   Instruction code
           byte 1:   P parameter
           byte 2-5: Q parameter

        This means that there are 256 instructions, 256 procedure levels,
        and 2gb of total addressing. This could be 4gb if we get rid of the
        need for negatives. }
      lvltyp      = 0..255;     { procedure/function level }
      instyp      = 0..maxins;  { instruction }
      sctyp       = 0..maxsp;   { system call }
      address     = -maxstr..maxstr; { address }

      beta        = packed array[1..25] of char; (*error message*)
      settype     = set of setlow..sethigh;
      alfainx     = 1..maxalfa; { index for alfa type }
      alfa        = packed array[alfainx] of char;
      byte        = 0..255; { 8-bit byte }
      bytfil      = packed file of byte; { untyped file of bytes }
      fileno      = 0..maxfil; { logical file number }
      labbuf      = packed array [1..lablen] of char; { label buffer }
      strbuf      = packed array [1..strlen] of char;
      { Here is the variable length string containment to save on space. strings
        are only stored in their length rounded to the nearest 10th. }
      strvsp = ^strvs; { pointer to variable length id string }
      strvs = record { id string variable length }
                str:   packed array [1..varsqt] of char; { data contained }
                next:  strvsp { next }
              end;
      ctype = (cstr, creal, cset);
      cstptr = ^cstrec; { pointer to string constant entry table }
      cstrec = record 
        next: cstptr; 
        case ct: ctype of
            cstr:  (str: strvsp; strl: integer; strn: integer);
            creal: (r:   real; realn: integer);
            cset:  (s:   settype; setn: integer);
      end;
      pblock       = ^block;
      block        = record
                       next:    pblock; { next list block }
                       incnxt:  pblock; { included blocks list }
                       name:    strvsp; { name of block, including type }
                       bname:   strvsp; { name of block, not including type }
                       tmpnam:  strvsp; { name of temp allocation space }
                       short:   boolean; { there is a short name }
                       { block type }
                       btyp:    (btprog, btmod, btproc, btfunc);
                       en:      integer { encounter number }
                     end;

var   op : instyp; p : lvltyp; q : address;  (*instruction register*)
      q1 : address; { extra parameter }
      gblsiz: address; { size of globals }
      hexdig      : integer; { digits in unsigned hex }
      decdig      : integer; { digits in unsigned decimal }
      octdig      : integer; { digits in unsigned octal }
      bindig      : integer; { digits in unsigned binary }
      maxpow16    : integer; { maximum power of 16 }
      maxpow10    : integer; { maximum power of 10 }
      maxpow8     : integer; { maximum power of 8 }
      maxpow2     : integer; { maximum power of 2 }

      { check flags: these turn on runtime checks }
      dochkovf: boolean; { check arithmetic overflow }

      { debug flags: turn these on for various dumps and traces }
      dodmplab: boolean; { dump label definitions }
      dotrcrot: boolean; { trace routine executions }
      dotrcins: boolean; { trace instruction executions }
      dosrclin: boolean; { add source line sets to code }
      dotrcsrc: boolean; { trace source line executions (requires dosrclin) }
      dorecycl: boolean; { obey heap space recycle requests }
      dodebug:  boolean; { start up debug on entry }
      dodbgflt: boolean; { enter debug on fault }
      { Don't set this option unless you have file language extensions!
        It will just cause the run to fail }
      dodbgsrc: boolean; { do source file debugging }
      { invoke a special recycle mode that creates single word entries on
        recycle of any object, breaking off and recycling the rest. Once
        allocated, each entry exists forever, and accesses to it can be
        checked. }
      dochkrpt: boolean; { check reuse of freed entry (automatically
                           invokes dorecycl = false }
      donorecpar: boolean; { companion flag to dochkrpt: break returned blocks
                             as occupied, not free. This essentially converts
                             disposed blocks to flagged but dead entries that
                             generate errors on use. }
      dochkdef: boolean; { check undefined accesses }
      dosrcprf: boolean; { do source level profiling }
      dochkcov: boolean; { do code coverage }
      doanalys: boolean; { do analyze }
      dodckout: boolean; { do output code deck }
      dochkvbk: boolean; { do check VAR blocks }

      { other flags }
      iso7185: boolean; { iso7185 standard flag }

      { !!! remove this next statement for self compile }
      {elide}prd,prr     : text;{noelide}(*prd for read only, prr for write only *)

      instr       : array[instyp] of alfa; (* mnemonic instruction codes *)
      insr        : array[instyp] of integer; { number of stack words in result }
      insf        : array[instyp] of boolean; { result is real }
      sptable     : array[sctyp] of alfa; (*standard functions and procedures*)
      spfunc      : array[sctyp] of boolean; (*standard function or procedure
                                                  is function*)
      sppar       : array[sctyp] of integer; (*standard functions and procedures
                                                  number of parameters*)
      spkeep      : array[sctyp] of boolean; { keep the file parameter }
      option      : array ['a'..'z'] of boolean; { option array }
      csttbl      : cstptr; { constants table }
      strnum      : integer; { string constant label count }
      realnum     : integer; { real constants label count }
      setnum      : integer; { set constants label count }
      parlvl      : integer; { parameter level }
      blkstk      : pblock; { stack of symbol blocks }
      blklst      : pblock; { discard list of symbols blocks }

      (*locally used for interpreting one instruction*)
      ad          : address;
      c1          : char;

procedure wrtnum(var tf: text; v: integer; r: integer; f: integer; lz: boolean);
const digmax = 64; { maximum total digits }
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
   if r = 16 then n := hexdig
   else if r = 10 then n := decdig
   else if r = 8 then n := octdig
   else n := bindig;
   for i := 1 to digmax do digits[i] := '0';
   { adjust signed radix }
   if (r = 16) and (v < 0) then begin
     v := v+1+maxint; { convert number to 31 bit unsigned }
     t := v div maxpow16+8; { extract high digit w/sign }
     digits[hexdig] := digit(t); { place high digit }
     v := v mod maxpow16; { remove digit }
     n := hexdig-1 { set number of digits-1 }
   end else if (r = 8) and (v < 0) then begin
     v := v+1+maxint; { convert number to 31 bit unsigned }
     if (bindig mod 3) = 2 then { top is either 2 bits or 1 }
       t := v div maxpow8+4 { extract high digit w/sign }
     else t := 1; { it is sign }
     digits[octdig] := digit(t); { place high digit }
     v := v mod maxpow8; { remove digit }
     n := octdig-1 { set number of digits-1 }
   end else if (r = 2) and (v < 0) then begin
     v := v+1+maxint; { convert number to 31 bit unsigned }
     digits[bindig] := '1'; { place high digit (sign) }
     n := bindig-1 { set number of digits-1 }
   end;
   p := 1;
   for i := 1 to n do begin
      d := v div p mod r; { extract digit }
      digits[i] := digit(d); { place }
      if i < n then p := p*r
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

procedure wrthex(var tf: text; v: integer; f: integer; lz: boolean);
begin
  wrtnum(tf, v, 16, f, lz)
end;

{ find lower case of character }
function lcase(c: char): char;
begin
  if c in ['A'..'Z'] then c := chr(ord(c)-ord('A')+ord('a'));
  lcase := c
end { lcase };

{ get string quanta }
procedure getstr(var p: strvsp);
begin
  new(p); { get new entry }
end;

{ find padded length of variable length id string }
function lenpv(s: strvsp): integer;
var l, tl: integer;
begin tl := 0;
  while s <> nil do begin
    l := varsqt; 
    if s^.next = nil then begin
      while (s^.str[l] = ' ') and (l > 1) do l := l-1;
      if s^.str[l] = ' ' then l := 0;
    end;
    tl := tl+l;
    s := s^.next
  end;
  lenpv := tl
end;

{ get character from variable length string }
function strchr(a: strvsp; x: integer): char;
var c: char; i: integer; q: integer;
begin
   c := ' '; i := 1; q := 1;
   while i < x do begin
      if q >= varsqt then begin q := 1; if a <> nil then a := a^.next end
      else q := q+1;
      i := i+1
   end;
   if a <> nil then c := a^.str[q];
   strchr := c
 end;

{ put character to variable length string }
procedure strchrass(var a: strvsp; x: integer; c: char);
var i: integer; q: integer; p, l: strvsp;
procedure getsqt;
var y: integer;
begin
   if p = nil then begin getstr(p); for y := 1 to varsqt do p^.str[y] := ' ';
      p^.next := nil; if a = nil then a := p else l^.next := p
   end
end;
begin
   i := 1; q := 1; p := a; l := nil;
   getsqt;
   while i < x do begin
      if q >= varsqt then begin q := 1; l := p; p := p^.next; getsqt end
      else q := q+1;
      i := i+1
   end;
   p^.str[q] := c
end;

{ copy variable strings }
procedure strassvv(var d: strvsp; s: strvsp);
var i: integer;
begin
  d := nil;
  for i := 1 to lenpv(s) do strchrass(d, i, strchr(s, i))
end;

{ assign symbol identifier fixed to variable length string, including
  allocation, with length specified }
procedure strassvfl(var a: strvsp; var b: labbuf; l: integer);
var i, j: integer; p, lp: strvsp;
begin p := nil; a := nil; j := 1; lp := nil;
  for i := 1 to l do begin
    if j > varsqt then p := nil;
    if p = nil then begin
      getstr(p); p^.next := nil; j := 1;
      if a = nil then a := p else lp^.next := p; lp := p
    end;
    p^.str[j] := b[i]; j := j+1
  end;
  if p <> nil then for j := j to varsqt do p^.str[j] := ' '
end;

{ assign symbol identifier fixed to variable length string, including
  allocation }
procedure strassvf(var a: strvsp; var b: labbuf);
var i, j, l: integer; p, lp: strvsp;
begin l := lablen; p := nil; a := nil; j := 1; lp := nil;
  while (l > 1) and (b[l] = ' ') do l := l-1; { find length of fixed string }
  if b[l] = ' ' then l := 0;
  for i := 1 to l do begin
    if j > varsqt then p := nil;
    if p = nil then begin
      getstr(p); p^.next := nil; j := 1;
      if a = nil then a := p else lp^.next := p; lp := p
    end;
    p^.str[j] := b[i]; j := j+1
  end;
  if p <> nil then for j := j to varsqt do p^.str[j] := ' '
end;

{ assign fixed string to variable length string, including allocation }
procedure strassvsb(var a: strvsp; var b: strbuf);
var i, j, l: integer; p, lp: strvsp;
begin l := strlen; p := nil; a := nil; j := 1; lp := nil;
  while (l > 1) and (b[l] = ' ') do l := l-1; { find length of fixed string }
  if b[l] = ' ' then l := 0;
  for i := 1 to l do begin
    if j > varsqt then p := nil;
    if p = nil then begin
      getstr(p); p^.next := nil; j := 1;
      if a = nil then a := p else lp^.next := p; lp := p
    end;
    p^.str[j] := b[i]; j := j+1
  end;
  if p <> nil then for j := j to varsqt do p^.str[j] := ' '
end;

{ compare variable length id strings }
function strequvv(a, b: strvsp): boolean;
var m: boolean; i: integer;
begin
  m := true;
  while (a <> nil) and (b <> nil) do begin
    for i := 1 to varsqt do if lcase(a^.str[i]) <> lcase(b^.str[i]) then m := false;
    a := a^.next; b := b^.next
  end;
  if a <> b then m := false;
  strequvv := m
end;

{ write variable length string to file }
procedure writev(var f: text; s: strvsp; fl: integer);
var i: integer; c: char;
begin i := 1;
  while fl > 0 do begin
    c := ' '; if s <> nil then begin c := s^.str[i]; i := i+1 end;
    write(f, c); fl := fl-1;
    if i > varsqt then begin s := s^.next; i := 1 end
  end
end;

{ write padded string to file }
procedure writevp(var f: text; s: strvsp);
var l: integer;
begin
  while s <> nil do begin
    l := varsqt; 
    if s^.next = nil then begin
      while (s^.str[l] = ' ') and (l > 1) do l := l-1;
      if s^.str[l] = ' ' then l := 0;
    end;
    if l > 0 then write(f, s^.str:l);
    s := s^.next
  end;
end;

{ align address, upwards }
procedure alignu(algn: address; var flc: address);
  var l: integer;
begin
  l := flc-1;
  flc := l + algn  -  (algn+l) mod algn
end (*align*);

{ align address, downwards }
procedure alignd(algn: address; var flc: address);
  var l: integer;
begin
  l := flc+1;
  flc := l - algn  +  (algn-l) mod algn
end (*align*);

(*--------------------------------------------------------------------*)

{ translate intermediate file }

procedure xlate;
    const
      insmax10 = 10;
      insmax20 = 20;
      insmax30 = 30;
      insmax40 = 40;
      maxintreg = 14; { maximum number of assignable integer reg }
      maxfltreg = 16; { maximum number of assignable float reg }
      maxintparreg = 6; { maximum number of integer/pointer parameter regs }
      maxfltparreg = 8; { maximum number of floating parameter regs }

   type  labelst  = (entered,defined); (*label situation*)
         labelrg  = 0..maxlabel;       (*label range*)
         labelrec = record
                          val: address;
                          st: labelst;
                          ref: strvsp
                    end;
         flabelp = ^flabel;
         flabel = record
                       next: flabelp;
                       val: address;
                       ref: strvsp
                     end;
         { registers in target }
         reg = (rgnull, rgrax, rgrbx, rgrcx, rgrdx, rgrsi, rgrdi, rgrbp, rgrsp, 
                rgr8, rgr9, rgr10, rgr11, rgr12, rgr13, rgr14, rgr15, 
                rgxmm0, rgxmm1, rgxmm2, rgxmm3, rgxmm4, rgxmm5, rgxmm6, rgxmm7,
                rgxmm8, rgxmm9, rgxmm10, rgxmm11, rgxmm12, rgxmm13, rgxmm14, rgxmm15);
         regset = set of reg;
         { stack and expression tree entries }
         expptr = ^expstk;
         expstk = record
                    sn: integer; { serial number for entry }
                    free: boolean; { allocated/free }
                    next: expptr; { next entry link }
                    op:   instyp; { operator type }
                    p:   lvltyp; q, q1, q2: address; { p and q parameters }
                    r1, r2, r3: reg; { result registers }
                    t1, t2: reg; { temporary registers }
                    r1a, r2a, r3a: address; { set temp tracking addresses }
                    t1a, t2a: address;
                    l, r: expptr; { right and left links }
                    x1: expptr; { extra link }
                    pl: expptr; { parameter link for functions }
                    sl: expptr; { sfr start link }
                    cl: expptr; { cke chain }
                    al: expptr; { attachment link }
                    strn: integer; { string number }
                    realn: integer; { real number }
                    setn: integer; { set number }
                    vi, vi2: integer; { integer value }
                    rs: regset; { push/pop mask }
                    fn: strvsp; { function call name }
                    lb: strvsp; { label for sfr }
                    lt: strvsp; { label for table }
                  end;
         { temp entries for sets }
         setptr = ^setety;
         setety = record
           next: setptr; { next set temp in line }
           occu: boolean; { occupied status }
           off: address { stack offset }
         end;
         insstr10 = packed array [1..insmax10] of char;
         insstr20 = packed array [1..insmax20] of char;
         insstr30 = packed array [1..insmax30] of char;
         insstr40 = packed array [1..insmax40] of char;

   var  word : array[alfainx] of char; ch  : char;
        labeltab: array[labelrg] of labelrec;
        labelvalue: address;
        sline: integer; { line number of Pascal source file }
        iline: integer; { line number of intermediate file }
        sn: labbuf;
        snl: 1..lablen;
        flablst: flabelp; { list of far labels }
        estack, efree: expptr;
        frereg, allreg: regset;
        intassord: array [1..maxintreg] of reg; { integer register assignment order }
        fltassord: array [1..maxfltreg] of reg; { floating point register assignment order }
        stacklvl: integer;
        parreg: array [1..7] of reg; { parameter registers }
        parregf: array [1..8] of reg; { floating point parameter registers }
        expsn: integer; { expression entries sn }
        tmpoff: address; { starting address of set temps offset in stack }
        tmpspc: address; { size of temps area }
        tmplst: setptr; { list of active temps }
        tmpfre: setptr; { free temp entries }
        lclspc: strvsp; { label for locals space }

   procedure init;
      var i: integer;
   begin for i := 0 to maxins do instr[i] := '          ';
         {

           Notes:

           1. Instructions marked with "*" are for internal use only.
              The "*" mark both shows in the listing, and also prevents
              their use in the intermediate file, since only alpha
              characters are allowed as opcode labels.

           2. "---" entries are no longer used, but left here to keep the
              original instruction numbers from P4. They could be safely
              assigned to other instructions if the space is needed.

         }
         instr[  0]:='lodi      '; insr[  0] := 1; insf[  0] := false; 
         instr[  1]:='ldoi      '; insr[  1] := 1; insf[  1] := false;
         instr[  2]:='stri      '; insr[  2] := 0; insf[  2] := false; 
         instr[  3]:='sroi      '; insr[  3] := 0; insf[  3] := false;
         instr[  4]:='lda       '; insr[  4] := 1; insf[  4] := false; 
         instr[  5]:='lao       '; insr[  5] := 1; insf[  5] := false;
         instr[  6]:='stoi      '; insr[  6] := 0; insf[  6] := false;
         instr[  7]:='ldcs      '; insr[  7] := 1; insf[  7] := false;
         instr[  8]:='cjp       '; insr[  8] := 0; insf[  8] := false;
         instr[  9]:='indi      '; insr[  9] := 1; insf[  9] := false;
         instr[ 10]:='inci      '; insr[ 10] := 1; insf[ 10] := false;
         instr[ 11]:='mst       '; insr[ 11] := 0; insf[ 11] := false; 
         instr[ 12]:='cup       '; insr[ 12] := 0; insf[ 12] := false;
         instr[ 13]:='rip       '; insr[ 13] := 0; insf[ 13] := false;
         instr[ 14]:='retp      '; insr[ 14] := 0; insf[ 14] := false;
         instr[ 15]:='csp       '; insr[ 15] := 0; insf[ 15] := false;
         instr[ 16]:='ixa       '; insr[ 16] := 1; insf[ 16] := false;
         instr[ 17]:='equa      '; insr[ 17] := 1; insf[ 17] := false;
         instr[ 18]:='neqa      '; insr[ 18] := 1; insf[ 18] := false;
         instr[ 19]:='brk*      '; insr[ 19] := 0; insf[ 19] := false;
         instr[ 20]:='lnp*      '; insr[ 20] := 0; insf[ 20] := false;
         instr[ 21]:='cal       '; insr[ 21] := 1; insf[ 21] := false;
         instr[ 22]:='ret       '; insr[ 22] := 0; insf[ 22] := false;
         instr[ 23]:='ujp       '; insr[ 23] := 0; insf[ 23] := false;
         instr[ 24]:='fjp       '; insr[ 24] := 0; insf[ 24] := false;
         instr[ 25]:='xjp       '; insr[ 25] := 0; insf[ 25] := false;
         instr[ 26]:='chki      '; insr[ 26] := 1; insf[ 26] := false;
         instr[ 27]:='cuv       '; insr[ 27] := 0; insf[ 27] := false;
         instr[ 28]:='adi       '; insr[ 28] := 1; insf[ 28] := false;
         instr[ 29]:='adr       '; insr[ 29] := 1; insf[ 29] := true;
         instr[ 30]:='sbi       '; insr[ 30] := 1; insf[ 30] := false;
         instr[ 31]:='sbr       '; insr[ 31] := 1; insf[ 31] := true;
         instr[ 32]:='sgs       '; insr[ 32] := 1; insf[ 32] := false;
         instr[ 33]:='flt       '; insr[ 33] := 1; insf[ 33] := false;
         instr[ 34]:='flo       '; insr[ 34] := 2; insf[ 34] := false;
         instr[ 35]:='trc       '; insr[ 35] := 1; insf[ 35] := false;
         instr[ 36]:='ngi       '; insr[ 36] := 1; insf[ 36] := false;
         instr[ 37]:='ngr       '; insr[ 37] := 1; insf[ 37] := true;
         instr[ 38]:='sqi       '; insr[ 38] := 1; insf[ 38] := false;
         instr[ 39]:='sqr       '; insr[ 39] := 1; insf[ 39] := true;
         instr[ 40]:='abi       '; insr[ 40] := 1; insf[ 40] := false;
         instr[ 41]:='abr       '; insr[ 41] := 1; insf[ 41] := true;
         instr[ 42]:='notb      '; insr[ 42] := 1; insf[ 42] := false;
         instr[ 43]:='and       '; insr[ 43] := 1; insf[ 43] := false;
         instr[ 44]:='ior       '; insr[ 44] := 1; insf[ 44] := false;
         instr[ 45]:='dif       '; insr[ 45] := 1; insf[ 45] := false;
         instr[ 46]:='int       '; insr[ 46] := 1; insf[ 46] := false;
         instr[ 47]:='uni       '; insr[ 47] := 1; insf[ 47] := false;
         instr[ 48]:='inn       '; insr[ 48] := 1; insf[ 48] := false;
         instr[ 49]:='mod       '; insr[ 49] := 1; insf[ 49] := false;
         instr[ 50]:='odd       '; insr[ 50] := 1; insf[ 50] := false;
         instr[ 51]:='mpi       '; insr[ 51] := 1; insf[ 51] := false;
         instr[ 52]:='mpr       '; insr[ 52] := 1; insf[ 52] := true;
         instr[ 53]:='dvi       '; insr[ 53] := 1; insf[ 53] := false;
         instr[ 54]:='dvr       '; insr[ 54] := 1; insf[ 54] := true;
         instr[ 55]:='mov       '; insr[ 55] := 0; insf[ 55] := false;
         instr[ 56]:='lca       '; insr[ 56] := 1; insf[ 56] := false;
         instr[ 57]:='deci      '; insr[ 57] := 1; insf[ 57] := false;
         instr[ 58]:='stp*      '; insr[ 58] := 0; insf[ 58] := false;
         instr[ 59]:='ordi      '; insr[ 59] := 1; insf[ 59] := false;
         instr[ 60]:='chr       '; insr[ 60] := 1; insf[ 60] := false;
         instr[ 61]:='ujc       '; insr[ 61] := 0; insf[ 61] := false;
         instr[ 62]:='rnd       '; insr[ 62] := 1; insf[ 62] := false;
         instr[ 63]:='pck       '; insr[ 63] := 0; insf[ 63] := false;
         instr[ 64]:='upk       '; insr[ 64] := 0; insf[ 64] := false;
         instr[ 65]:='ldoa      '; insr[ 65] := 1; insf[ 65] := false;
         instr[ 66]:='ldor      '; insr[ 66] := 1; insf[ 66] := true;
         instr[ 67]:='ldos      '; insr[ 67] := 1; insf[ 67] := false;
         instr[ 68]:='ldob      '; insr[ 68] := 1; insf[ 68] := false;
         instr[ 69]:='ldoc      '; insr[ 69] := 1; insf[ 69] := false;
         instr[ 70]:='stra      '; insr[ 70] := 0; insf[ 70] := false;
         instr[ 71]:='strr      '; insr[ 71] := 0; insf[ 71] := false;
         instr[ 72]:='strs      '; insr[ 72] := 0; insf[ 72] := false;
         instr[ 73]:='strb      '; insr[ 73] := 0; insf[ 73] := false;
         instr[ 74]:='strc      '; insr[ 74] := 0; insf[ 74] := false;
         instr[ 75]:='sroa      '; insr[ 75] := 0; insf[ 75] := false;
         instr[ 76]:='sror      '; insr[ 76] := 1; insf[ 76] := true;
         instr[ 77]:='sros      '; insr[ 77] := 1; insf[ 77] := false;
         instr[ 78]:='srob      '; insr[ 78] := 0; insf[ 78] := false;
         instr[ 79]:='sroc      '; insr[ 79] := 0; insf[ 79] := false;
         instr[ 80]:='stoa      '; insr[ 80] := 0; insf[ 80] := false;
         instr[ 81]:='stor      '; insr[ 81] := 0; insf[ 81] := false;
         instr[ 82]:='stos      '; insr[ 82] := 0; insf[ 82] := false;
         instr[ 83]:='stob      '; insr[ 83] := 0; insf[ 83] := false;
         instr[ 84]:='stoc      '; insr[ 84] := 0; insf[ 84] := false;
         instr[ 85]:='inda      '; insr[ 85] := 1; insf[ 85] := false;
         instr[ 86]:='indr      '; insr[ 86] := 1; insf[ 86] := true;
         instr[ 87]:='inds      '; insr[ 87] := 1; insf[ 87] := false;
         instr[ 88]:='indb      '; insr[ 88] := 1; insf[ 88] := false;
         instr[ 89]:='indc      '; insr[ 89] := 1; insf[ 89] := false;
         instr[ 90]:='inca      '; insr[ 90] := 1; insf[ 90] := false;
         instr[ 91]:='suv       '; insr[ 91] := 0; insf[ 91] := false;
         instr[ 92]:='vbs       '; insr[ 92] := 0; insf[ 92] := false;
         instr[ 93]:='incb      '; insr[ 93] := 1; insf[ 93] := false;
         instr[ 94]:='incc      '; insr[ 94] := 1; insf[ 94] := false;
         instr[ 95]:='chka      '; insr[ 95] := 1; insf[ 95] := false;
         instr[ 96]:='vbe       '; insr[ 96] := 0; insf[ 96] := false;
         instr[ 97]:='chks      '; insr[ 97] := 0; insf[ 97] := false;
         instr[ 98]:='chkb      '; insr[ 98] := 1; insf[ 98] := false;
         instr[ 99]:='chkc      '; insr[ 99] := 1; insf[ 99] := false;
         instr[100]:='cvbi      '; insr[100] := 2; insf[100] := false;
         instr[101]:='ivtx      '; insr[101] := 2; insf[101] := false;
         instr[102]:='ivtb      '; insr[102] := 2; insf[102] := false;
         instr[103]:='decb      '; insr[103] := 1; insf[103] := false;
         instr[104]:='decc      '; insr[104] := 1; insf[104] := false;
         instr[105]:='loda      '; insr[105] := 1; insf[105] := false;
         instr[106]:='lodr      '; insr[106] := 1; insf[106] := true;
         instr[107]:='lods      '; insr[107] := 1; insf[107] := false;
         instr[108]:='lodb      '; insr[108] := 1; insf[108] := false;
         instr[109]:='lodc      '; insr[109] := 1; insf[109] := false;
         instr[110]:='rgs       '; insr[110] := 1; insf[110] := false;
         instr[111]:='ivtc      '; insr[111] := 2; insf[111] := false;
         instr[112]:='ipj       '; insr[112] := 0; insf[112] := false;
         instr[113]:='cip       '; insr[113] := 0; insf[113] := false;
         instr[114]:='lpa       '; insr[114] := 2; insf[114] := false;
         instr[115]:='cvbx      '; insr[115] := 2; insf[115] := false;
         instr[116]:='cvbb      '; insr[116] := 2; insf[116] := false;
         instr[117]:='dmp       '; insr[117] := 0; insf[117] := false;
         instr[118]:='swp       '; insr[118] := 2; insf[118] := false;
         instr[119]:='tjp       '; insr[119] := 0; insf[119] := false;
         instr[120]:='lip       '; insr[120] := 2; insf[120] := false;
         instr[121]:='cvbc      '; insr[121] := 2; insf[121] := false;
         instr[122]:='vis       '; insr[122] := 1; insf[122] := false;
         instr[123]:='ldci      '; insr[123] := 1; insf[123] := false;
         instr[124]:='ldcr      '; insr[124] := 1; insf[124] := true;
         instr[125]:='ldcn      '; insr[125] := 1; insf[125] := false;
         instr[126]:='ldcb      '; insr[126] := 1; insf[126] := false;
         instr[127]:='ldcc      '; insr[127] := 1; insf[127] := false;
         instr[128]:='reti      '; insr[128] := 1; insf[128] := false;
         instr[129]:='retr      '; insr[129] := 1; insf[129] := false;
         instr[130]:='retc      '; insr[130] := 1; insf[130] := false;
         instr[131]:='retb      '; insr[131] := 1; insf[131] := false;
         instr[132]:='reta      '; insr[132] := 1; insf[132] := false;
         instr[133]:='vip       '; insr[133] := 0; insf[133] := false;
         instr[134]:='ordb      '; insr[134] := 1; insf[134] := false;
         instr[135]:='lcp       '; insr[135] := 2; insf[135] := false;
         instr[136]:='ordc      '; insr[136] := 1; insf[136] := false;
         instr[137]:='equi      '; insr[137] := 1; insf[137] := false;
         instr[138]:='equr      '; insr[138] := 1; insf[138] := false;
         instr[139]:='equb      '; insr[139] := 1; insf[139] := false;
         instr[140]:='equs      '; insr[140] := 1; insf[140] := false;
         instr[141]:='equc      '; insr[141] := 1; insf[141] := false;
         instr[142]:='equm      '; insr[142] := 1; insf[142] := false;
         instr[143]:='neqi      '; insr[143] := 1; insf[143] := false;
         instr[144]:='neqr      '; insr[144] := 1; insf[144] := false;
         instr[145]:='neqb      '; insr[145] := 1; insf[145] := false;
         instr[146]:='neqs      '; insr[146] := 1; insf[146] := false;
         instr[147]:='neqc      '; insr[147] := 1; insf[147] := false;
         instr[148]:='neqm      '; insr[148] := 1; insf[148] := false;
         instr[149]:='geqi      '; insr[149] := 1; insf[149] := false;
         instr[150]:='geqr      '; insr[150] := 1; insf[150] := false;
         instr[151]:='geqb      '; insr[151] := 1; insf[151] := false;
         instr[152]:='geqs      '; insr[152] := 1; insf[152] := false;
         instr[153]:='geqc      '; insr[153] := 1; insf[153] := false;
         instr[154]:='geqm      '; insr[154] := 1; insf[154] := false;
         instr[155]:='grti      '; insr[155] := 1; insf[155] := false;
         instr[156]:='grtr      '; insr[156] := 1; insf[156] := false;
         instr[157]:='grtb      '; insr[157] := 1; insf[157] := false;
         instr[158]:='grts      '; insr[158] := 1; insf[158] := false;
         instr[159]:='grtc      '; insr[159] := 1; insf[159] := false;
         instr[160]:='grtm      '; insr[160] := 1; insf[160] := false;
         instr[161]:='leqi      '; insr[161] := 1; insf[161] := false;
         instr[162]:='leqr      '; insr[162] := 1; insf[162] := false;
         instr[163]:='leqb      '; insr[163] := 1; insf[163] := false;
         instr[164]:='leqs      '; insr[164] := 1; insf[164] := false;
         instr[165]:='leqc      '; insr[165] := 1; insf[165] := false;
         instr[166]:='leqm      '; insr[166] := 1; insf[166] := false;
         instr[167]:='lesi      '; insr[167] := 1; insf[167] := false;
         instr[168]:='lesr      '; insr[168] := 1; insf[168] := false;
         instr[169]:='lesb      '; insr[169] := 1; insf[169] := false;
         instr[170]:='less      '; insr[170] := 1; insf[170] := false;
         instr[171]:='lesc      '; insr[171] := 1; insf[171] := false;
         instr[172]:='lesm      '; insr[172] := 1; insf[172] := false;
         instr[173]:='---       '; insr[173] := 0; insf[173] := false;
         instr[174]:='mrkl*     '; insr[174] := 0; insf[174] := false;
         instr[175]:='ckvi      '; insr[175] := 2; insf[175] := false;
         instr[176]:='cps       '; insr[176] := 3; insf[176] := false;
         instr[177]:='cpc       '; insr[177] := 3; insf[177] := false;
         instr[178]:='aps       '; insr[178] := 0; insf[178] := false;
         instr[179]:='ckvb      '; insr[179] := 2; insf[179] := false;
         instr[180]:='ckvc      '; insr[180] := 2; insf[180] := false;
         instr[181]:='dupi      '; insr[181] := 1; insf[181] := false;
         instr[182]:='dupa      '; insr[182] := 2; insf[182] := false;
         instr[183]:='dupr      '; insr[183] := 1; insf[183] := true;
         instr[184]:='dups      '; insr[184] := 1; insf[184] := false;
         instr[185]:='dupb      '; insr[185] := 1; insf[185] := false;
         instr[186]:='dupc      '; insr[186] := 1; insf[186] := false;
         instr[187]:='cks       '; insr[187] := 2; insf[187] := false;
         instr[188]:='cke       '; insr[188] := 0; insf[188] := false;
         instr[189]:='inv       '; insr[189] := 0; insf[189] := false;
         instr[190]:='ckla      '; insr[190] := 1; insf[190] := false;
         instr[191]:='cta       '; insr[191] := 2; insf[191] := false;
         instr[192]:='ivti      '; insr[192] := 2; insf[192] := false;
         instr[193]:='lodx      '; insr[193] := 1; insf[193] := false;
         instr[194]:='ldox      '; insr[194] := 1; insf[194] := false;
         instr[195]:='strx      '; insr[195] := 0; insf[195] := false;
         instr[196]:='srox      '; insr[196] := 1; insf[196] := false;
         instr[197]:='stox      '; insr[197] := 0; insf[197] := false;
         instr[198]:='indx      '; insr[198] := 1; insf[198] := false;
         instr[199]:='chkx      '; insr[199] := 1; insf[199] := false;
         instr[200]:='ordx      '; insr[200] := 1; insf[200] := false;
         instr[201]:='incx      '; insr[201] := 1; insf[201] := false;
         instr[202]:='decx      '; insr[202] := 1; insf[202] := false;
         instr[203]:='ckvx      '; insr[203] := 2; insf[203] := false;
         instr[204]:='retx      '; insr[204] := 1; insf[204] := false;
         instr[205]:='noti      '; insr[205] := 1; insf[205] := false;
         instr[206]:='xor       '; insr[206] := 1; insf[206] := false;
         instr[207]:='bge       '; insr[207] := 4; insf[207] := false;
         instr[208]:='ede       '; insr[208] := 0; insf[208] := false;
         instr[209]:='mse       '; insr[209] := 0; insf[209] := false;
         instr[210]:='apc       '; insr[210] := 0; insf[210] := false;
         instr[211]:='cxs       '; insr[211] := 1; insf[211] := false;
         instr[212]:='cxc       '; insr[212] := 2; insf[212] := false;
         instr[213]:='lft       '; insr[213] := 2; insf[213] := false;
         instr[214]:='max       '; insr[214] := 1; insf[214] := false;
         instr[215]:='equv      '; insr[215] := 1; insf[215] := false;
         instr[216]:='neqv      '; insr[216] := 1; insf[216] := false;
         instr[217]:='lesv      '; insr[217] := 1; insf[217] := false;
         instr[218]:='grtv      '; insr[218] := 1; insf[218] := false;
         instr[219]:='leqv      '; insr[219] := 1; insf[219] := false;
         instr[220]:='geqv      '; insr[220] := 1; insf[220] := false;
         instr[221]:='vdp       '; insr[221] := 0; insf[221] := false;
         instr[222]:='spc       '; insr[222] := 2; insf[222] := false;
         instr[223]:='ccs       '; insr[223] := 2; insf[223] := false;
         instr[224]:='scp       '; insr[224] := 0; insf[224] := false;
         instr[225]:='ldp       '; insr[225] := 2; insf[225] := false;
         instr[226]:='vin       '; insr[226] := 0; insf[226] := false;
         instr[227]:='vdd       '; insr[227] := 0; insf[227] := false;
         { ltc and lto are aliases to ldo and lao instructions }
         instr[228]:='ltci      '; insr[228] := 1; insf[228] := false;
         instr[229]:='ltcr      '; insr[229] := 1; insf[229] := true;
         instr[230]:='ltcs      '; insr[230] := 1; insf[230] := false;
         instr[231]:='ltcb      '; insr[231] := 1; insf[231] := false;
         instr[232]:='ltcc      '; insr[232] := 1; insf[232] := false;
         instr[233]:='ltcx      '; insr[233] := 1; insf[233] := false;
         instr[234]:='lto       '; insr[234] := 1; insf[234] := false;
         instr[235]:='stom      '; insr[235] := 0; insf[235] := false;
         instr[236]:='rets      '; insr[236] := 1; insf[236] := false;
         instr[237]:='retm      '; insr[237] := 1; insf[237] := false;
         instr[238]:='ctb       '; insr[238] := 0; insf[238] := false;
         instr[239]:='cpp       '; insr[239] := 1; insf[239] := false;
         instr[240]:='cpr       '; insr[240] := 1; insf[240] := false;
         instr[241]:='lsa       '; insr[241] := 1; insf[241] := false;
         instr[242]:='eext*     '; insr[242] := 0; insf[242] := false;
         instr[243]:='wbs       '; insr[243] := 1; insf[243] := false;
         instr[244]:='wbe       '; insr[244] := 0; insf[244] := false;
         instr[245]:='sfr       '; insr[245] := 0; insf[245] := false;
         instr[246]:='cuf       '; insr[246] := 0; insf[246] := false;
         instr[247]:='cif       '; insr[247] := 0; insf[247] := false;

         sptable[ 0]:='get       '; spfunc[ 0]:=false; sppar[ 0]:=1; spkeep[ 0]:=false;
         sptable[ 1]:='put       '; spfunc[ 1]:=false; sppar[ 1]:=1; spkeep[ 1]:=false;
         sptable[ 2]:='thw       '; spfunc[ 2]:=false; sppar[ 2]:=1; spkeep[ 2]:=false;   
         sptable[ 3]:='rln       '; spfunc[ 3]:=false; sppar[ 3]:=1; spkeep[ 3]:=true;
         sptable[ 4]:='new       '; spfunc[ 4]:=false; sppar[ 4]:=2; spkeep[ 4]:=false;   
         sptable[ 5]:='wln       '; spfunc[ 5]:=false; sppar[ 5]:=1; spkeep[ 5]:=true;
         sptable[ 6]:='wrs       '; spfunc[ 6]:=false; sppar[ 6]:=4; spkeep[ 6]:=true;   
         sptable[ 7]:='eln       '; spfunc[ 7]:=true;  sppar[ 7]:=1; spkeep[ 7]:=false;
         sptable[ 8]:='wri       '; spfunc[ 8]:=false; sppar[ 8]:=3; spkeep[ 8]:=true;   
         sptable[ 9]:='wrr       '; spfunc[ 9]:=false; sppar[ 9]:=3; spkeep[ 9]:=true;
         sptable[10]:='wrc       '; spfunc[10]:=false; sppar[10]:=3; spkeep[10]:=true;   
         sptable[11]:='rdi       '; spfunc[11]:=false; sppar[11]:=2; spkeep[11]:=true;
         sptable[12]:='rdr       '; spfunc[12]:=false; sppar[12]:=2; spkeep[12]:=true;   
         sptable[13]:='rdc       '; spfunc[13]:=false; sppar[13]:=2; spkeep[13]:=true;
         sptable[14]:='sin       '; spfunc[14]:=true;  sppar[14]:=1; spkeep[14]:=false;   
         sptable[15]:='cos       '; spfunc[15]:=true;  sppar[15]:=1; spkeep[15]:=false;
         sptable[16]:='exp       '; spfunc[16]:=true;  sppar[16]:=1; spkeep[16]:=false;
         sptable[17]:='log       '; spfunc[17]:=true;  sppar[17]:=1; spkeep[17]:=false;
         sptable[18]:='sqt       '; spfunc[18]:=true;  sppar[18]:=1; spkeep[18]:=false;   
         sptable[19]:='atn       '; spfunc[19]:=true;  sppar[19]:=1; spkeep[19]:=false;
         sptable[20]:='---       '; spfunc[20]:=false; sppar[20]:=1; spkeep[20]:=false;   
         sptable[21]:='pag       '; spfunc[21]:=false; sppar[21]:=1; spkeep[21]:=false;
         sptable[22]:='rsf       '; spfunc[22]:=false; sppar[22]:=1; spkeep[22]:=false;   
         sptable[23]:='rwf       '; spfunc[23]:=false; sppar[23]:=1; spkeep[23]:=false;
         sptable[24]:='wrb       '; spfunc[24]:=false; sppar[24]:=3; spkeep[24]:=true;   
         sptable[25]:='wrf       '; spfunc[25]:=false; sppar[25]:=4; spkeep[25]:=true;
         sptable[26]:='dsp       '; spfunc[26]:=false; sppar[26]:=2; spkeep[26]:=false;   
         sptable[27]:='wbf       '; spfunc[27]:=false; sppar[27]:=3; spkeep[27]:=true;
         sptable[28]:='wbi       '; spfunc[28]:=false; sppar[28]:=2; spkeep[28]:=true;   
         sptable[29]:='wbr       '; spfunc[29]:=false; sppar[29]:=2; spkeep[29]:=true;
         sptable[30]:='wbc       '; spfunc[30]:=false; sppar[30]:=2; spkeep[30]:=true;   
         sptable[31]:='wbb       '; spfunc[31]:=false; sppar[31]:=2; spkeep[31]:=true;
         sptable[32]:='rbf       '; spfunc[32]:=false; sppar[32]:=3; spkeep[32]:=true;   
         sptable[33]:='rsb       '; spfunc[33]:=false; sppar[33]:=1; spkeep[33]:=false;
         sptable[34]:='rwb       '; spfunc[34]:=false; sppar[34]:=1; spkeep[34]:=false;   
         sptable[35]:='gbf       '; spfunc[35]:=false; sppar[35]:=2; spkeep[35]:=false;
         sptable[36]:='pbf       '; spfunc[36]:=false; sppar[36]:=2; spkeep[36]:=false;   
         sptable[37]:='rib       '; spfunc[37]:=false; sppar[37]:=4; spkeep[37]:=true;
         sptable[38]:='rcb       '; spfunc[38]:=false; sppar[38]:=4; spkeep[38]:=true;   
         sptable[39]:='nwl       '; spfunc[39]:=false; sppar[39]:=3; spkeep[39]:=false; { special }
         sptable[40]:='dsl       '; spfunc[40]:=false; sppar[40]:=3; spkeep[40]:=false; { special }
         sptable[41]:='eof       '; spfunc[41]:=true;  sppar[41]:=1; spkeep[41]:=false;
         sptable[42]:='efb       '; spfunc[42]:=true;  sppar[42]:=1; spkeep[42]:=false;   
         sptable[43]:='fbv       '; spfunc[43]:=false; sppar[43]:=1; spkeep[43]:=true;
         sptable[44]:='fvb       '; spfunc[44]:=false; sppar[44]:=2; spkeep[44]:=true;
         sptable[45]:='wbx       '; spfunc[45]:=false; sppar[45]:=2; spkeep[45]:=true;
         sptable[46]:='asst      '; spfunc[46]:=false; sppar[46]:=3; spkeep[46]:=false;
         sptable[47]:='clst      '; spfunc[47]:=false; sppar[47]:=1; spkeep[47]:=false;
         sptable[48]:='pos       '; spfunc[48]:=false; sppar[48]:=2; spkeep[48]:=false;
         sptable[49]:='upd       '; spfunc[49]:=false; sppar[49]:=1; spkeep[49]:=false;
         sptable[50]:='appt      '; spfunc[50]:=false; sppar[50]:=1; spkeep[50]:=false;
         sptable[51]:='del       '; spfunc[51]:=false; sppar[51]:=2; spkeep[51]:=false;
         sptable[52]:='chg       '; spfunc[52]:=false; sppar[52]:=4; spkeep[52]:=false;
         sptable[53]:='len       '; spfunc[53]:=true;  sppar[53]:=1; spkeep[53]:=false;
         sptable[54]:='loc       '; spfunc[54]:=true;  sppar[54]:=1; spkeep[54]:=false;
         sptable[55]:='exs       '; spfunc[55]:=true;  sppar[55]:=2; spkeep[55]:=false;
         sptable[56]:='assb      '; spfunc[56]:=false; sppar[56]:=3; spkeep[56]:=false;
         sptable[57]:='clsb      '; spfunc[57]:=false; sppar[57]:=1; spkeep[57]:=false;
         sptable[58]:='appb      '; spfunc[58]:=false; sppar[58]:=1; spkeep[58]:=false;
         sptable[59]:='hlt       '; spfunc[59]:=false; sppar[59]:=0; spkeep[59]:=false;
         sptable[60]:='ast       '; spfunc[60]:=false; sppar[60]:=1; spkeep[60]:=false;
         sptable[61]:='asts      '; spfunc[61]:=false; sppar[61]:=3; spkeep[61]:=false;
         sptable[62]:='wrih      '; spfunc[62]:=false; sppar[62]:=3; spkeep[62]:=true;
         sptable[63]:='wrio      '; spfunc[63]:=false; sppar[63]:=3; spkeep[63]:=true;
         sptable[64]:='wrib      '; spfunc[64]:=false; sppar[64]:=3; spkeep[64]:=true;
         sptable[65]:='wrsp      '; spfunc[65]:=false; sppar[65]:=3; spkeep[65]:=true;
         sptable[66]:='wiz       '; spfunc[66]:=false; sppar[66]:=3; spkeep[66]:=true;
         sptable[67]:='wizh      '; spfunc[67]:=false; sppar[67]:=3; spkeep[67]:=true;
         sptable[68]:='wizo      '; spfunc[68]:=false; sppar[68]:=3; spkeep[68]:=true;
         sptable[69]:='wizb      '; spfunc[69]:=false; sppar[69]:=3; spkeep[69]:=true;
         sptable[70]:='rds       '; spfunc[70]:=false; sppar[70]:=3; spkeep[70]:=true;
         sptable[71]:='ribf      '; spfunc[71]:=false; sppar[71]:=5; spkeep[71]:=true;
         sptable[72]:='rdif      '; spfunc[72]:=false; sppar[72]:=3; spkeep[72]:=true;
         sptable[73]:='rdrf      '; spfunc[73]:=false; sppar[73]:=3; spkeep[73]:=true;
         sptable[74]:='rcbf      '; spfunc[74]:=false; sppar[74]:=5; spkeep[74]:=true;
         sptable[75]:='rdcf      '; spfunc[75]:=false; sppar[75]:=3; spkeep[75]:=true;
         sptable[76]:='rdsf      '; spfunc[76]:=false; sppar[76]:=4; spkeep[76]:=true;
         sptable[77]:='rdsp      '; spfunc[77]:=false; sppar[77]:=3; spkeep[77]:=true;
         sptable[78]:='aeft      '; spfunc[78]:=false; sppar[78]:=3; spkeep[78]:=false;
         sptable[79]:='aefb      '; spfunc[79]:=false; sppar[79]:=3; spkeep[79]:=false;
         sptable[80]:='rdie      '; spfunc[80]:=false; sppar[80]:=3; spkeep[80]:=false;
         sptable[81]:='rdre      '; spfunc[81]:=false; sppar[81]:=3; spkeep[81]:=false;

         for i:= 1 to 10 do word[i]:= ' ';
         for i:= 0 to maxlabel do
             with labeltab[i] do begin val:=-1; st:= entered end;

         { !!! remove this next statement for self compile }
         {elide}reset(prd);{noelide}

         sline := 0; { set no line of source }
         iline := 1; { set 1st line of intermediate }
         flablst := nil; { clear far label list }
         expsn := 0; { expression entries serial number start }
         tmpoff := 0; { set temps stack offset }
         tmpspc := 0; { set temps total size }
         tmplst := nil; { clear temps list }
         tmpfre := nil; { clear temps free list }
         estack := nil; stacklvl := 0; efree := nil;
         allreg := [rgrax, rgrbx, rgrcx, rgrdx, rgrsi, rgrdi, 
                    rgr8, rgr9, rgr10, rgr11, rgr12, rgr13, rgr14, rgr15,
                    rgxmm0, rgxmm1, rgxmm2, rgxmm3, rgxmm4, rgxmm6, rgxmm7,
                    rgxmm8, rgxmm9, rgxmm10, rgxmm11, rgxmm12, rgxmm13, rgxmm14,
                    rgxmm15];
         frereg := allreg;
         { set parameter registers }
         parreg[1] :=rgrdi;
         parreg[2] :=rgrsi;
         parreg[3] :=rgrdx;
         parreg[4] :=rgrcx;
         parreg[5] :=rgr8;
         parreg[6] :=rgr9;
         parreg[7] :=rgnull;

         parregf[1] :=rgxmm0;
         parregf[2] :=rgxmm1;
         parregf[3] :=rgxmm2;
         parregf[4] :=rgxmm3;
         parregf[5] :=rgxmm4;
         parregf[6] :=rgxmm5;
         parregf[7] :=rgxmm6;
         parregf[8] :=rgxmm7;
         { set assignment preference registers }
         intassord[1] := rgrbx;
         intassord[2] := rgrcx; 
         intassord[3] := rgrsi; 
         intassord[4] := rgrdi; 
         intassord[5] := rgr8;
         intassord[6] := rgr9; 
         intassord[7] := rgr10; 
         intassord[8] := rgr11; 
         intassord[9] := rgr12; 
         intassord[10] := rgr13; 
         intassord[11] := rgr14; 
         intassord[12] := rgr15;
         intassord[13] := rgrdx;
         intassord[14] := rgrax;

         fltassord[1] := rgxmm8;
         fltassord[2] := rgxmm9; 
         fltassord[3] := rgxmm10; 
         fltassord[4] := rgxmm11; 
         fltassord[5] := rgxmm12;
         fltassord[6] := rgxmm13; 
         fltassord[7] := rgxmm14; 
         fltassord[8] := rgxmm15; 
         fltassord[9] := rgxmm0; 
         fltassord[10] := rgxmm1; 
         fltassord[11] := rgxmm2; 
         fltassord[12] := rgxmm3;
         fltassord[13] := rgxmm4;
         fltassord[14] := rgxmm5;
         fltassord[15] := rgxmm6;
         fltassord[16] := rgxmm7;
   end;(*init*)

   procedure errorl(string: beta); (*error in loading*)
   begin writeln;
      writeln('*** Program translation error: [', sline:1, ',', iline:1, '] ', string);
      goto 1
   end; (*errorl*)

   procedure dmplabs; { dump label table }

   var i: labelrg;

   begin

      writeln;
      writeln('Label table');
      writeln;
      for i := 1 to maxlabel do if labeltab[i].val <> -1 then begin

         write('Label: ', i:5, ' value: ', labeltab[i].val, ' ');
         if labeltab[i].st = entered then writeln('Entered')
         else writeln('Defined')

      end;
      writeln

   end;

   procedure putlabel(x: labelrg);
   var i, p: integer; digit: boolean;
   begin
     strassvf(labeltab[x].ref, sn); strchrass(labeltab[x].ref, snl+1, '.'); i := snl+2;
     p := maxpow10;
     digit := false;
     while p > 0 do begin
       if ((x div p) mod 10 <> 0) or (p = 1) or digit then begin
         strchrass(labeltab[x].ref, i, chr((x div p) mod 10+ord('0'))); 
         i := i+1; digit := true
       end;
       p := p div 10
     end
   end;

   procedure update(x: labelrg; pc: boolean); (*when a label definition lx is found*)
   begin
      if labeltab[x].st=defined then errorl('duplicated label         ')
      else begin
        labeltab[x].st := defined;
        labeltab[x].val:= labelvalue;
        putlabel(x);
        writevp(prr, labeltab[x].ref);
        if pc then  writeln(prr, ':') 
        else writeln(prr, ' = ', labeltab[x].val:1)
      end
   end;(*update*)

   procedure getnxt; { get next character }
   begin
      ch := ' ';
      if not eoln(prd) then read(prd,ch)
   end;

   procedure skpspc; { skip spaces }
   begin
     while (ch = ' ') and not eoln(prd) do getnxt
   end;

   procedure getlin; { get next line }
   begin
     readln(prd);
     iline := iline+1 { next intermediate line }
   end;

   procedure getlab;
   var i: 1..lablen;
   begin skpspc; for i := 1 to lablen do sn[i] := ' '; snl := 1;
     if not (ch in ['a'..'z','A'..'Z','_']) then
       errorl('Symbols format error     ');
     while ch in ['a'..'z','A'..'Z','0'..'9','_'] do begin
       if snl >= lablen then errorl('Symbols format error     ');
       sn[snl] := ch; getnxt; snl := snl+1
     end;
     snl := snl-1
   end;

   procedure getsds;
   var i: 1..lablen;
   begin skpspc; for i := 1 to lablen do sn[i] := ' '; snl := 1;
     if ch = ' ' then errorl('Symbols format error     ');
     while ch <> ' ' do begin
       if snl >= lablen then errorl('Symbols format error     ');
       sn[snl] := ch; getnxt; snl := snl+1
     end;
     snl := snl-1
   end;

   procedure parlab(var x: integer; var fl: strvsp);
   var i,j: integer;
   begin fl := nil;
     getlab; if ch <> '.' then errorl('Symbols format error     ');
     if prd^ in ['0'..'9'] then begin read(prd, x); getnxt end { near label }
     else begin { far label }
       getnxt; strassvf(fl, sn); strchrass(fl, snl+1, '.'); i := snl+2; getlab;
       for j := 1 to snl do begin strchrass(fl, i, sn[j]); i := i+1 end
     end
   end;

   procedure preamble;
   begin
     { see how much of this is really required }
     writeln(prr, '        .globl  main');
     writeln(prr, '        .type   main, @function');
     writeln(prr, 'main:');
     writeln(prr, '# Set up default files');
     writeln(prr, '        movb    $inputfn,globals_start+inputoff(%rip)');
     writeln(prr, '        movb    $outputfn,globals_start+outputoff(%rip)');
     writeln(prr, '        movb    $errorfn,globals_start+erroroff(%rip)');
     writeln(prr, '        movb    $listfn,globals_start+listoff(%rip)');
     writeln(prr, '        movb    $commandfn,globals_start+commandoff(%rip)');
   end;

   procedure postamble;
   begin
   end;

   procedure errorcode;
   begin

     writeln(prr, '# Error codes');
     writeln(prr, 'ValueOutOfRange                    = 14');
     writeln(prr, 'ArrayLengthMatch                   = 15');
     writeln(prr, 'CaseValueNotFound                  = 16');
     writeln(prr, 'ZeroDivide                         = 17');
     writeln(prr, 'InvalidOperand                     = 18');
     writeln(prr, 'NilPointerDereference              = 19');
     writeln(prr, 'RealOverflow                       = 20');
     writeln(prr, 'RealUnderflow                      = 21');
     writeln(prr, 'RealProcessingFault                = 22');
     writeln(prr, 'TagValueNotActive                  = 23');
     writeln(prr, 'TooManyFiles                       = 24');
     writeln(prr, 'FileIsOpen                         = 25');
     writeln(prr, 'FileAlreadyNamed                   = 26');
     writeln(prr, 'FileNotOpen                        = 27');
     writeln(prr, 'FileModeIncorrect                  = 28');
     writeln(prr, 'InvalidFieldSpecification          = 29');
     writeln(prr, 'InvalidRealNumber                  = 30');
     writeln(prr, 'InvalidFractionSpecification       = 31');
     writeln(prr, 'InvalidIntegerFormat               = 32');
     writeln(prr, 'IntegerValueOverflow               = 33');
     writeln(prr, 'InvalidRealFormat                  = 34');
     writeln(prr, 'EndOfFile                          = 35');
     writeln(prr, 'InvalidFilePosition                = 36');
     writeln(prr, 'FilenameTooLong                    = 37');
     writeln(prr, 'FileOpenFail                       = 38');
     writeln(prr, 'FileSIzeFail                       = 39');
     writeln(prr, 'FileCloseFail                      = 40');
     writeln(prr, 'FileReadFail                       = 41');
     writeln(prr, 'FileWriteFail                      = 42');
     writeln(prr, 'FilePositionFail                   = 43');
     writeln(prr, 'FileDeleteFail                     = 44');
     writeln(prr, 'FileNameChangeFail                 = 45');
     writeln(prr, 'SpaceAllocateFail                  = 46');
     writeln(prr, 'SpaceReleaseFail                   = 47');
     writeln(prr, 'SpaceAllocateNegative              = 48');
     writeln(prr, 'CannotPerformSpecial               = 49');
     writeln(prr, 'CommandLineTooLong                 = 50');
     writeln(prr, 'ReadPastEOF                        = 51');
     writeln(prr, 'FileTransferLengthZero             = 52');
     writeln(prr, 'FileSizeTooLarge                   = 53');
     writeln(prr, 'FilenameEmpty                      = 54');
     writeln(prr, 'CannotOpenStandard                 = 55');
     writeln(prr, 'TooManyTemporaryFiles              = 56');
     writeln(prr, 'InputBufferOverflow                = 57');
     writeln(prr, 'TooManyThreads                     = 58');
     writeln(prr, 'CannotStartThread                  = 59');
     writeln(prr, 'InvalidThreadHandle                = 60');
     writeln(prr, 'CannotStopThread                   = 61');
     writeln(prr, 'TooManyIntertaskLocks              = 62');
     writeln(prr, 'InvalidLockHandle                  = 63');
     writeln(prr, 'LockSequenceFail                   = 64');
     writeln(prr, 'TooManySignals                     = 65');
     writeln(prr, 'CannotCreateSignal                 = 66');
     writeln(prr, 'InvalidSignalHandle                = 67');
     writeln(prr, 'CannotDeleteSignal                 = 68');
     writeln(prr, 'CannotSendSignal                   = 69');
     writeln(prr, 'WaitForSignalFail                  = 70');
     writeln(prr, 'FieldNotBlank                      = 71');
     writeln(prr, 'ReadOnWriteOnlyFile                = 72');
     writeln(prr, 'WriteOnReadOnlyFile                = 73');
     writeln(prr, 'FileBufferVariableUndefined        = 74');
     writeln(prr, 'NondecimalRadixOfNegative          = 75');
     writeln(prr, 'InvalidArgumentToLn                = 76');
     writeln(prr, 'InvalidArgumentToSqrt              = 77');
     writeln(prr, 'CannotResetOrRewriteStandardFile   = 78');
     writeln(prr, 'CannotResetWriteOnlyFile           = 79');
     writeln(prr, 'CannotRewriteReadOnlyFile          = 80');
     writeln(prr, 'SetElementOutOfRange               = 81');
     writeln(prr, 'RealArgumentTooLarge               = 82');
     writeln(prr, 'BooleanOperatorOfNegative          = 83');
     writeln(prr, 'InvalidDivisorToMod                = 84');
     writeln(prr, 'PackElementsOutOfBounds            = 85');
     writeln(prr, 'UnpackElementsOutOfBounds          = 86');
     writeln(prr, 'CannotResetClosedTempFile          = 87');
     writeln(prr, 'UndefinedLocationAccess            = 88');
     writeln(prr, 'FunctionNotImplemented             = 89');
     writeln(prr, 'InvalidInISO7185Mode               = 90');
     writeln(prr, 'HeapFormatInvalid                  = 91');
     writeln(prr, 'DisposeOfUninitalizedPointer       = 92');
     writeln(prr, 'DisposeOfNilPointer                = 93');
     writeln(prr, 'BadPointerValue                    = 94');
     writeln(prr, 'BlockAlreadyFreed                  = 95');
     writeln(prr, 'InvalidStandardProcedureOrFunction = 96');
     writeln(prr, 'InvalidInstruction                 = 97');
     writeln(prr, 'NewDisposeTagsMismatch             = 98');
     writeln(prr, 'PCOutOfRange                       = 99');
     writeln(prr, 'StoreOverflow                      = 100');
     writeln(prr, 'StackBalance                       = 101');
     writeln(prr, 'SetInclusion                       = 102');
     writeln(prr, 'UninitializedPointer               = 103');
     writeln(prr, 'DereferenceOfNilPointer            = 104');
     writeln(prr, 'PointerUsedAfterDispose            = 105');
     writeln(prr, 'VariantNotActive                   = 106');
     writeln(prr, 'InvalidCase                        = 107');
     writeln(prr, 'SystemError                        = 108');
     writeln(prr, 'ChangeToAllocatedTagfield          = 109');
     writeln(prr, 'UnhandledException                 = 110');
     writeln(prr, 'ProgramCodeAssertion               = 111');
     writeln(prr, 'VarListEmpty                       = 112');
     writeln(prr, 'ChangeToVarReferencedVariant       = 113');
     writeln(prr, 'DisposeOfVarReferencedBlock        = 114');
     writeln(prr, 'VarReferencedFileBufferModified    = 115');
     writeln(prr, 'ContainerMismatch                  = 116');
     writeln(prr, 'InvalidContainerLevel              = 117');
     writeln(prr, 'DisposeOfWithReferencedBlock       = 118');
     writeln(prr, 'WithBaseListEmpty                  = 119');
     writeln(prr, 'ExternalsNotEnabled                = 120');
     writeln(prr);

   end;

   procedure assemble; forward;

   procedure prtline;
   begin
     write(prr, '# ', sline:6, ': ', iline:6, ': ')
   end;

   procedure wrtmods(bp: pblock; s: boolean);
   begin
     if bp <> nil then begin
       wrtmods(bp^.next, s);
       if s then begin
         writevp(prr, bp^.bname);
         if bp^.en > 1 then write(prr, '$', bp^.en:1)
       end else writevp(prr, bp^.name);
       write(prr, '.')
     end
   end;

   function anyshort(bp: pblock): boolean;
   var short: boolean;
   begin
     short := false;
     while bp <> nil do begin
       if bp^.short then short := true;
       bp := bp^.next
     end;
     anyshort := short
   end;

   function fndovrmax(bn: strvsp; bp: pblock): integer;
   var ovrmax: integer;
   begin
     ovrmax := 0;
     while bp <> nil do begin
       if strequvv(bn, bp^.bname) and (bp^.en > ovrmax) then ovrmax := bp^.en;
       bp := bp^.incnxt
     end;
     fndovrmax := ovrmax 
   end;

   procedure wrtblklab(bp: pblock);
   begin
     if bp <> nil then begin
       if anyshort(bp) then begin
         wrtmods(bp^.next, true);
         writevp(prr, bp^.bname); 
         if bp^.en > 1 then write(prr, '$', bp^.en:1);
         writeln(prr, ':')
       end;
       wrtmods(bp^.next, false);
       writevp(prr, bp^.name); writeln(prr, ':')
     end
   end;

   procedure generate;(*generate segment of code*)
      var x: integer; (* label number *)
          again: boolean;
          c,ch1: char;
          ls: strvsp;    
          ispc: boolean;  
          i, l: 1..lablen;   
          bp: pblock;
          sgn: boolean;
          sn2: labbuf;
          snl2: 1..lablen;
          vt: array [1..100] of integer;
          vi, vl: integer;
          ts: alfa;
   begin
      again := true;
      while again do begin
        if eof(prd) then errorl('unexpected eof on input  ');
        getnxt;(* first character of line*)
        if not (ch in ['!', 'l', 'q', ' ', ':', 'o', 'g', 'b', 'e', 's', 'f',
                       'v', 't', 'n', 'x', 'c']) then
          errorl('unexpected line start    ');
        case ch of
          '!': begin prtline; write(prr, ' ', '!'); while not eoln(prd) do
                 begin read(prd, ch); write(prr, ch) end;
                 writeln(prr);
               end;
          'l': begin getnxt; parlab(x,ls); 
                     prtline; write(prr, ' ', 'l ', sn:snl, '.', x:1);
                     if ls <> nil then
                       errorl('Invalid intermediate     ');
                     if ch='=' then 
                       begin read(prd,labelvalue); 
                         write(prr, '=', labelvalue:1);
                         ispc := false
                       end else ispc := true;
                     getlin; writeln(prr); update(x, ispc)
               end;
          'q': begin again := false; getlin end;
          ' ': begin getnxt; 
                     while not eoln(prd) and (ch = ' ') do getnxt;
                     if not eoln(prd) and (ch <> ' ') then assemble
                     else getlin 
               end;
          ':': begin { source line }
                  read(prd,x); { get source line number }
                  sline := x; prtline; writeln(prr, ' ', ':', x:1);
                  { skip the rest of the line, which would be the
                    contents of the source line if included }
                  while not eoln(prd) do
                     read(prd, c); { get next character }
                  getlin { source line }
               end;
          'o': begin { option }
                 prtline; write(prr, ' ', 'o ');
                 getnxt;
                 while not eoln(prd) and (ch = ' ') do getnxt;
                 repeat
                   if not (ch in ['a'..'z']) then
                     errorl('No valid option found    ');
                   ch1 := ch; write(prr, ch); getnxt; write(prr, ch);
                   option[ch1] := ch = '+'; getnxt;
                   case ch1 of
                     'g': dodmplab := option[ch1];
                     'h': dosrclin := option[ch1];
                     'n': dorecycl := option[ch1];
                     'o': dochkovf := option[ch1];
                     'p': dochkrpt := option[ch1];
                     'm': donorecpar := option[ch1];
                     'q': dochkdef := option[ch1];
                     's': iso7185  := option[ch1];
                     'w': dodebug  := option[ch1];
                     'a': dodbgflt := option[ch1];
                     'f': dodbgsrc := option[ch1];
                     'e': dodckout := option[ch1];
                     'i': dochkvbk := option[ch1];
                     'b':; 'c':; 'd':; 'l':; 't':; 'u':; 'v':;
                     'x':; 'y':; 'z':; 'k':; 'j':; 'r':;
                   end
                 until not (ch in ['a'..'z']);
                 getlin; writeln(prr);
               end;
          'g': begin read(prd, gblsiz); getlin end; { set globals space }
          'b': begin { block start }
                 getnxt; skpspc;
                 if not (ch in ['p', 'm', 'r', 'f']) then
                   errorl('Block type is invalid    ');
                 ch1 := ch; { save block type }
                 if ch in  ['p','m'] then preamble;
                 getnxt; skpspc; getsds;
                 for i := 1 to snl do begin
                   { translate '@' to '$' for type spagetti demarcate, and '(' 
                     or ')' to '_' because they are invalid }
                   if sn[i] = '@' then sn[i] := '$'
                   else 
                     if sn[i] in ['(',')',',',':'] then sn[i] := '_'
                 end;
                 new(bp); strassvf(bp^.name, sn);
                 { get basename, without type }
                 l := 1; bp^.short := true;
                 while (l < lablen) and (sn[l] <> '$') do l := l+1;
                 if sn[l] = '$' then strassvfl(bp^.bname, sn, l-1)
                 else begin
                   strassvf(bp^.bname, sn); { just use whole name }
                   bp^.short := false
                 end;
                 strassvv(bp^.tmpnam, bp^.bname); { copy to temp id }
                 ts := '$_tmpspc  '; l := lenpv(bp^.tmpnam);
                 for i := 1 to 8 do strchrass(bp^.tmpnam, l+i, ts[i]); 
                 bp^.incnxt := nil;
                 case ch1 of { block type }
                   'p': bp^.btyp := btprog;
                   'm': bp^.btyp := btmod;
                   'r': bp^.btyp := btproc;
                   'f': bp^.btyp := btfunc
                 end;
                 bp^.en := 1; { set default encounter number }
                 if blkstk <> nil then begin
                   { process block inclusions }
                   bp^.en := fndovrmax(bp^.bname, blkstk^.incnxt)+1;
                   bp^.incnxt := blkstk^.incnxt; { insert to list }
                   blkstk^.incnxt := bp
                 end;
                 { put onto block stack }
                 bp^.next := blkstk; blkstk := bp;
                 prtline; write(prr, ' b ', ch1, ' '); writevp(prr, bp^.name);
                 writeln(prr);
                 if ch1 in ['p', 'm'] then wrtblklab(bp);
                 getlin
               end;
          'e': begin 
                 getnxt; skpspc;
                 if not (ch in ['p', 'm', 'r', 'f']) then
                   errorl('Block type is invalid    ');
                 prtline; writeln(prr, ' e ', ch);
                 if ch = 'p' then postamble;
                 if blkstk = nil then errorl('System error             ');
                 blkstk := blkstk^.next;
                 getlin
               end;
         's': begin { symbol }
                prtline; write(prr, ' s ');
                getnxt; getlab;
                write(prr, sn:snl); 
                sn2 := sn; snl2 := snl;
                skpspc;
                if not (ch in ['g', 'l','p']) then
                  errorl('Symbol type is invalid   ');
                ch1 := ch;
                write(prr, ' ', ch, ' ');
                getnxt;
                skpspc;
                if not (ch in ['0'..'9','-']) then
                  errorl('No offset found          ');
                sgn := ch = '-'; if ch = '-' then getnxt;
                ad := 0; while ch in ['0'..'9'] do
                  begin
                    if ad <= maxstr div 10 then
                      ad := ad*10+ord(ch)-ord('0')
                    else errorl('Symbol offset > max      ');
                    getnxt
                  end;
                if sgn then ad := -ad;
                write(prr, ad:1, ' ');
                getsds; writeln(prr, sn:snl);
                if anyshort(blkstk) then begin
                  wrtmods(blkstk, true); 
                  if ch1 = 'g' then 
                    writeln(prr, sn2:snl2, ' = globals_start+', ad:1)
                  else
                    writeln(prr, sn2:snl2, ' = ', ad:1);
                end;
                wrtmods(blkstk, false); 
                if ch1 = 'g' then
                  writeln(prr, sn2:snl2, ' = globals_start+', ad:1)
                else
                  writeln(prr, sn2:snl2, ' = ', ad:1);
                getlin
              end;
          'f': getlin; { source error count }
          'v': begin { variant logical table }
                getnxt; skpspc;
                if ch <> 'l' then errorl('Label format error       ');
                getnxt; parlab(x,ls); 
                prtline; write(prr, ' ', 'v l ', sn:snl, '.', x:1, ' ');
                read(prd, vl); write(prr, vl:1, ' '); vi := 1; 
                for vi := 1 to vl do 
                  begin read(prd, vt[vi]); write(prr, vt[vi]:1, ' ') end;
                writeln(prr); update(x, true); getlin;
                write(prr, '        .quad   ', vl:1);
                for vi := 1 to vl do
                  write(prr, ',', vt[vi]:1);
                writeln(prr)
               end;
          't': getlin; { template }
          'n': getlin; { start constant table }
          'x': getlin;
          'c': getlin;
       end
     end
   end; (*generate*)

    procedure assemble; (*translate symbolic code into machine code and store*)

      var name :alfa; r :real; s :settype;
          i,s1,lb,ub,l:integer; c: char;
          str: strbuf; { buffer for string constants }
          cstp: cstptr;
          ep, ep2, ep3, ep4, ep5, pp: expptr;
          r1: reg; sp, sp2: strvsp; def, def2: boolean; val, val2: integer;
          stkadr: integer; { stack address tracking }

      procedure labelsearch(var def: boolean; var val: integer; var sp: strvsp);
         var x: integer; flp: flabelp;
      begin def := false; val := 0; flp := nil; skpspc; 
        if ch <> 'l' then errorl('Label format error       ');
        getnxt; parlab(x,sp);
        if sp <> nil then begin { far label }
          new(flp); flp^.next := flablst; flablst := flp; flp^.ref := sp
        end else begin { near label }
          if labeltab[x].ref = nil then putlabel(x);
          sp := labeltab[x].ref; def := labeltab[x].st = defined; 
          val := labeltab[x].val
        end
      end;(*labelsearch*)

      procedure getname;
      var i: alfainx;
      begin
        if eof(prd) then errorl('Unexpected eof on input  ');
        for i := 1 to maxalfa do word[i] := ' ';
        i := 1; { set 1st character of word }
        if not (ch in ['a'..'z']) then errorl('No operation label       ');
        while ch in ['a'..'z'] do begin
          if i = maxalfa then errorl('Opcode label is too long ');
          word[i] := ch;
          i := i+1; ch := ' ';
          if not eoln(prd) then read(prd,ch); { next character }
        end;
        pack(word,1,name)
      end; (*getname*)

      procedure wrtreg(var f: text; r: reg);
      begin
        case r of
          rgnull:  write(f, '<null>');
          rgrax:   write(f, 'rax');
          rgrbx:   write(f, 'rbx');
          rgrcx:   write(f, 'rcx');
          rgrdx:   write(f, 'rdx');
          rgrsi:   write(f, 'rsi');
          rgrdi:   write(f, 'rdi');
          rgrbp:   write(f, 'rbp');
          rgrsp:   write(f, 'rsp');
          rgr8:    write(f, 'r8');
          rgr9:    write(f, 'r9');
          rgr10:   write(f, 'r10');
          rgr11:   write(f, 'r11');
          rgr12:   write(f, 'r12');
          rgr13:   write(f, 'r13');
          rgr14:   write(f, 'r14');
          rgr15:   write(f, 'r15');
          rgxmm0:  write(f, 'xmm0'); 
          rgxmm1:  write(f, 'xmm1'); 
          rgxmm2:  write(f, 'xmm2'); 
          rgxmm3:  write(f, 'xmm3'); 
          rgxmm4:  write(f, 'xmm4'); 
          rgxmm5:  write(f, 'xmm5'); 
          rgxmm6:  write(f, 'xmm6'); 
          rgxmm7:  write(f, 'xmm7');
          rgxmm8:  write(f, 'xmm8');
          rgxmm9:  write(f, 'xmm9'); 
          rgxmm10: write(f, 'xmm10'); 
          rgxmm11: write(f, 'xmm11'); 
          rgxmm12: write(f, 'xmm12'); 
          rgxmm13: write(f, 'xmm13'); 
          rgxmm14: write(f, 'xmm14'); 
          rgxmm15: write(f, 'xmm15');
        end
      end;

      procedure wrtbreg(var f: text; r: reg);
      begin
        case r of
          rgnull:  write(f, '<null>');
          rgrax:   write(f, 'al');
          rgrbx:   write(f, 'bl');
          rgrcx:   write(f, 'cl');
          rgrdx:   write(f, 'dl');
          rgrsi:   write(f, 'sil');
          rgrdi:   write(f, 'dil');
          rgrbp:   write(f, 'bpl');
          rgrsp:   write(f, 'spl');
          rgr8:    write(f, 'r8b');
          rgr9:    write(f, 'r9b');
          rgr10:   write(f, 'r10b');
          rgr11:   write(f, 'r11b');
          rgr12:   write(f, 'r12b');
          rgr13:   write(f, 'r13b');
          rgr14:   write(f, 'r14b');
          rgr15:   write(f, 'r15b');
          rgxmm0:  write(f, 'xmm0'); 
          rgxmm1:  write(f, 'xmm1'); 
          rgxmm2:  write(f, 'xmm2'); 
          rgxmm3:  write(f, 'xmm3'); 
          rgxmm4:  write(f, 'xmm4'); 
          rgxmm5:  write(f, 'xmm5'); 
          rgxmm6:  write(f, 'xmm6'); 
          rgxmm7:  write(f, 'xmm7');
          rgxmm8:  write(f, 'xmm8');
          rgxmm9:  write(f, 'xmm9'); 
          rgxmm10: write(f, 'xmm10'); 
          rgxmm11: write(f, 'xmm11'); 
          rgxmm12: write(f, 'xmm12'); 
          rgxmm13: write(f, 'xmm13'); 
          rgxmm14: write(f, 'xmm14'); 
          rgxmm15: write(f, 'xmm15');
        end
      end;

      procedure wrtregs(var f: text; rs: regset);
      var first: boolean; r: reg;
      begin
        first := true; write(prr, '['); 
        for r := rgrax to rgxmm15 do if r in rs then begin 
          if not first then write(f, ', ');
          wrtreg(f, r); first := false
        end;
        write(prr, ']')
      end;

      procedure getexp(var ep: expptr);
      begin
        if efree <> nil then begin ep := efree; efree := ep^.next end
        else begin new(ep); expsn := expsn+1; ep^.sn := expsn end;
        ep^.next := nil; ep^.op := op; ep^.p := p; ep^.q := q; ep^.q1 := q1;
        ep^.l := nil; ep^.r := nil; ep^.x1 := nil; ep^.sl := nil;
        ep^.cl := nil; ep^.al := nil; ep^.pl := nil; 
        ep^.r1 := rgnull; ep^.r2 := rgnull; ep^.r3 := rgnull; 
        ep^.t1 := rgnull; ep^.t2 := rgnull; ep^.rs := []; 
        ep^.fn := nil; ep^.lb := nil; ep^.lt := nil; ep^.free := false;
        ep^.r1a := 0; ep^.r2a := 0; ep^.r3a := 0; 
        ep^.t1a := 0; ep^.t2a := 0;
      end;
      
      procedure putexp(ep: expptr);
      begin
        if ep^.free then errorl('System fault: dbl free   ');
        ep^.next := efree; efree := ep; ep^.free := true
      end;

      procedure gettmp(var a: address);
      var p, fp: setptr;
      begin
        fp := nil; p := tmplst;
        while p <> nil do begin if not p^.occu then fp := p; p := p^.next end;
        if fp = nil then begin
          if tmpfre <> nil then begin fp := tmpfre; tmpfre := tmpfre^.next end
          else new(fp); 
          fp^.next := tmplst; tmplst := fp;
          tmpspc := tmpspc+setsize; tmpoff := tmpoff-setsize; fp^.off := tmpoff
        end;
        fp^.occu := true; 
        a := fp^.off
      end;

      procedure puttmp(a: address);
      var p, fp: setptr;
      begin
        fp := nil; p := tmplst;
        while p <> nil do begin if p^.off = a then fp := p; p := p^.next end;
        if fp = nil then errorl('System error: tmp addr   ');
        fp^.occu := false
      end;

      procedure deltmp;
      var p: setptr;
      begin
        if tmplst <> nil then begin
          p := tmplst;
          while p^.next <> nil do p := p^.next;
          p^.next := tmpfre
        end else tmpfre := tmplst;
        tmplst := nil
      end;
      
      procedure dmpstk(var f: text);
      var ep: expptr;
      begin
        ep := estack;
        while ep <> nil do begin
          writeln(f, 'Stack: ', ep^.op:3, ': ', instr[ep^.op]);
          ep := ep^.next
        end
      end;

      procedure pshstk(ep: expptr);
      begin
        ep^.next := estack; estack := ep; stacklvl := stacklvl+1
      end;
      
      procedure popstk(var ep: expptr);
      begin
        if estack = nil then errorl('Expression underflow     ');
        ep := estack; estack := estack^.next; ep^.next := nil; 
        stacklvl := stacklvl-1
      end;

      procedure botstk;
      begin
        if estack <> nil then begin
          writeln;
          writeln('*** Program translation error: [', sline:1, ',', iline:1, '] Stack balance');
          writeln;
          writeln('Contents of stack:');
          dmpstk(output);
          goto 1
        end
      end;

      function depth: integer;
      var ep: expptr; c: integer;
      begin
        c := 0; ep := estack;
        while ep <> nil do begin c := c+1; ep := ep^.next end;
        depth := c
      end;
      
      procedure deltre(ep: expptr);
      begin
        if ep^.l <> nil then deltre(ep^.l); 
        if ep^.r <> nil then deltre(ep^.r);
        if ep^.x1 <> nil then deltre(ep^.x1);
        if ep^.sl <> nil then deltre(ep^.sl);
        if ep^.cl <> nil then deltre(ep^.cl);
        if ep^.al <> nil then deltre(ep^.al);
        if ep^.pl <> nil then deltre(ep^.pl);
        if ep^.next <> nil then deltre(ep^.next);
        putexp(ep)
      end;

      procedure dmpety(var f: text; ep: expptr);
      begin
        write(f, ep^.op:3, ': ', instr[ep^.op]:4, ' ');
        if ep^.op = 15{csp} then write(f, ep^.q:1, ': ', sptable[ep^.q]:4) 
        else write(f, ep^.q:1);
        if ep^.r1 <> rgnull then begin write(f, ' r1: '); wrtreg(f, ep^.r1) end;
        if ep^.r2 <> rgnull then begin write(f, ' r2: '); wrtreg(f, ep^.r2) end;
        if ep^.r3 <> rgnull then begin write(f, ' r3: '); wrtreg(f, ep^.r3) end;
        if ep^.t1 <> rgnull then begin write(f, ' t1: '); wrtreg(f, ep^.t1) end;
        if ep^.t2 <> rgnull then begin write(f, ' t2: '); wrtreg(f, ep^.t2) end;
      end;
      
      procedure dmptrel(ep: expptr; lvl: integer);
      var l: expptr;
      begin
        write(prr, '# ', ' '); dmpety(prr, ep); writeln(prr);
        if ep^.l <> nil then begin
          writeln(prr, '# ', ' ': lvl, 'Left:');
          dmptrel(ep^.l, lvl+3);
        end;
        if ep^.r <> nil then begin
          writeln(prr, '# ', ' ': lvl, 'right:');
          dmptrel(ep^.r, lvl+3);
        end;
        if ep^.x1 <> nil then begin
          writeln(prr, '# ', ' ': lvl, 'xtra1:');
          dmptrel(ep^.x1, lvl+3);
        end;
        if ep^.sl <> nil then begin
          writeln(prr, '# ', ' ': lvl, 'call start:');
          dmptrel(ep^.sl, lvl+3);
        end;
        if ep^.cl <> nil then begin
          writeln(prr, '# ', ' ': lvl, 'tag checks:');
          l := ep^.cl;
          while l <> nil do begin
            dmptrel(l, lvl+3);
            l := l^.next
          end
        end;
        if ep^.al <> nil then begin
          writeln(prr, '# ', ' ': lvl, 'attached:');
          dmptrel(ep^.al, lvl+3);
        end;
        if ep^.pl <> nil then begin
          l := ep^.pl;
          while l <> nil do begin
            writeln(prr, '# ', ' ': lvl, 'Parameter:');
            dmptrel(l, lvl+3);
            l := l^.next
          end
        end
      end;

      procedure dmptre(ep: expptr);
      begin
        writeln(prr, '#    expr:');
        dmptrel(ep, 1)
      end;

      procedure dmplst(ep: expptr);
      begin
        while ep <> nil do begin
          dmptre(ep);
          ep := ep^.next
        end
      end;

      procedure getreg(var r: reg; var rf: regset);
      var i: 1..maxintreg;
      begin
        i := 1;
        r := intassord[i];
        while not (r in rf) and (i < maxintreg) do 
          begin i := i+1; r := intassord[i] end;
        if not (r in rf) then errorl('Out of registers         ');
        rf := rf-[r];
      end;

      procedure getfreg(var r: reg; var rf: regset);
      var i: 1..maxfltreg;
      begin
        i := 1;
        r := fltassord[i];
        while not (r in rf) and (i < maxfltreg) do 
          begin i := i+1; r := fltassord[i] end;
        if not (r in rf) then errorl('Out of registers         ');
        rf := rf-[r];
      end;

      function isfltres(ep: expptr): boolean;
      var isf: boolean;
      begin
        isf := false; if insf[ep^.op] then isf := true
        else if ep^.op = 15{csp} then
          if ep^.q in [19{atn},15{cos},16{exp},17{log},14{sin},18{sqt}] then 
            isf := true;
        isfltres := isf
      end;

      procedure assreg(ep: expptr; rf: regset; r1, r2: reg);
      var rfs: regset;

      procedure resreg(r: reg);
      begin
        if not (r in rf) and (r <> r1) and (r <> r2) then ep^.rs := ep^.rs+[r];
        rf := rf-[r]
      end;

      procedure dstreg(r: reg);
      begin
        if not (r in rfs) and (r <> r1) and (r <> r2) then ep^.rs := ep^.rs+[r];
        rfs := rfs-[r]
      end;

      { assign registers to parameters in call }
      procedure asspar(ep: expptr; mi: integer);
      var pp: expptr; pc: integer; fpc: integer; r1, r2: reg; fr: reg;
      begin
        pp := ep^.pl; pc := 1; fpc := 1;
        while pp <> nil do begin
          if isfltres(pp) then begin { floating result }
            if fpc <= maxfltparreg then begin
              resreg(parregf[fpc]); assreg(pp, rf, parregf[fpc], rgnull)
            end else begin
              getfreg(fr, rf); assreg(pp, rf, rgnull, rgnull)
            end;
            fpc := fpc+1
          end else if insr[pp^.op] = 2 then begin { double register }
            if (pc <= mi) and (pc <= maxintparreg-1) then begin
              resreg(parreg[pc]); resreg(parreg[pc+1]); 
              assreg(pp, rf, parreg[pc], parreg[pc+1])
            end else begin
              getreg(r1, rf); getreg(r2, rf); assreg(pp, rf, r1, r2)
            end;
            pc := pc+2
          end else begin { single register }
            if (pc <= mi) and (pc <= maxintparreg) then begin
              resreg(parreg[pc]); assreg(pp, rf, parreg[pc], rgnull)
            end else begin 
              getreg(r1, rf); assreg(pp, rf, r1, rgnull) 
            end;
            pc := pc+1
          end;            
          pp := pp^.next
        end
      end;

      procedure asscall;
      begin
        { calling convention says can trash these }
        dstreg(rgrax); dstreg(rgrcx); dstreg(rgrdx); dstreg(rgrsi); 
        dstreg(rgrdi); dstreg(rgr8); dstreg(rgr9); dstreg(rgr10); 
        dstreg(rgr11)
      end;

      begin
        { This diag is a bit verbose for me, but can be enabled for debugging }
        {
        write(prr, '# assigning: '); dmpety(prr, ep); write(prr, ' rf: ');
        wrtregs(prr, rf); writeln(prr);
        }
        rfs := rf;
        if ep^.al <> nil then assreg(ep^.al, rf, rgnull, rgnull);
        if r1 <> rgnull then rf := rf-[r1];
        if r2 <> rgnull then rf := rf-[r2];
        case ep^.op of

          {lodi,lodx,loda,lodb,lodc,lda}
          0,193,105,108,109,4: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
          end;

          {lodr}
          106: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            getreg(ep^.t1, rf)
          end;

          {lods}
          107: begin resreg(rgrsi); resreg(rgrdi); ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a)
          end;

          {adr,sbr}
          29, 31: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) end;

          {equr,neqr,geqr,grtr,leqr,lesr}
          138,144,150,156,162,168: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgnull, rgnull); resreg(ep^.l^.r1);
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {grts,less}
          158,170: ; { are invalid }

          {equs,neqs,geqs,leqs}
          140,146,152,164: begin 
            asscall;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdi, rgnull); 
            assreg(ep^.r, rf, rgrsi, rgnull)
          end;

          {adi,sbi,equ,neq,geq,grt,leq,les}
          28, 30, 17, 137, 139, 141, 18, 143, 145, 
          147, 19, 149, 151, 153, 20, 155, 157, 159, 21, 
          161, 163, 165, 167, 169, 171: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) 
          end;

          120{lip}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            ep^.r2 := r2; if ep^.r2 = rgnull then getreg(ep^.r2, rf);
            getreg(ep^.t1, rf) 
          end; 

          {equm,neqm,geqm,grtm,leqm,lesm}
          142, 148, 154, 160, 166, 172: begin 
            asscall;
            ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdi, rgnull); 
            assreg(ep^.r, rf, rgrsi, rgnull)
          end;

          5{lao}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          16{ixa}: begin 
            dstreg(rgrax); dstreg(rgrdx);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); ep^.t1 := ep^.r1;
            if (ep^.r1 = rgrax) or (ep^.r1 = rgrdx) then getreg(ep^.t1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull); assreg(ep^.r, rf, rgnull, rgnull)
          end;

          118{swp}: ; { done at top level }

          {ldoi,ldoa,ldob,ldoc,ldox}
          1,65,68,69,194:begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          {ldor}
          66:begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) 
          end;

          {ldos}
          67: begin resreg(rgrsi); resreg(rgrdi); ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a)
          end;

          {ind,inda,indb,indc,indx}
          9, 85,88,89,198: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
            assreg(ep^.l, rf, ep^.r1, rgnull) 
          end;

          {indr}
          86: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf); getreg(ep^.r2, rf);
            assreg(ep^.l, rf, ep^.r2, rgnull) 
          end;

          {inds}
          87: begin 
            dstreg(rgrsi); dstreg(rgrdi);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull);
            gettmp(ep^.r1a)
          end;

          {inc,dec}
          10, 90, 93, 94, 57, 103, 104, 201, 202: begin 
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {suv}
          91: ;

          {ckvi,ckvb,ckvc,ckvx}
          175, 179, 180, 203: ;

          {cvbi,cvbx,cvbb,cvbc}
          100, 115, 116, 121: begin 
            asscall;
            assreg(ep^.l, rf, rgr9, rgnull); assreg(ep^.r, rf, rgrcx, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgrax
          end;

          {ivti,ivtx,ivtb,ivtc}
          192,101,102,111: begin 
            asscall;
            assreg(ep^.l, rf, rgr9, rgnull); assreg(ep^.r, rf, rgrcx, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgrax
          end;

          {cta}
          191: begin
            asscall;
            assreg(ep^.l, rf, rgnull, rgrcx); assreg(ep^.r, rf, rgnull, rgr8)
          end;

          {cps}
          176: begin 
            asscall;
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
            if ep^.r2 = rgnull then getreg(ep^.r2, rf)
          end;

          {cpc}
          177: begin
            asscall; 
            assreg(ep^.l, rf, rgnull, rgrsi); assreg(ep^.r, rf, rgnull, rgrdx);
          end;

          {lpa}
          114: begin ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
            if ep^.r2 = rgnull then getreg(ep^.r2, rf)
          end;

          {ldci,ldcc,ldcb}
          123,127,126: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
          end;

          {ldcn}
          125: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          {ldcr}
          124: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) end;

          {ldcs}
          7: begin 
            dstreg(rgrsi); dstreg(rgrdi);
            ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a) 
          end;

          {chki,chka,chkb,chkc,chkx}
          26, 95, 98, 99, 199: begin 
            dstreg(rgrax);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); getreg(ep^.t1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {chks}
          97: begin 
            asscall;
            if (r1 = rgnull) and (rgrdx in rf) then ep^.r1 := rgrdx
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdx, rgnull);
            resreg(rgrdi); resreg(rgrsi);
            ep^.r1a := ep^.l^.r1a
          end;

          {ckla}
          190: begin 
            dstreg(rgrax);
            ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          56 {lca}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
          end;

          {ordi,ordb,ordc,ordx}
          59, 134, 136, 200: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2)
          end;

          {lcp}
          135: begin ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf)
          end;

          {sgs}
          32: begin 
            asscall; assreg(ep^.l, rf, rgrdi, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a)
          end;
     
          {flt,flo}
          33,34: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgnull, rgnull)  
          end;

          {trc}
          35: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgnull, rgnull)  
          end;

          {ngi}
          36: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)  
          end;

          {ngr}
          37: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgnull, rgnull)  
          end;

          {abi,sqi}
          40,38: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2) 
          end;

          {sqr}
          39: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2) 
          end;

          {abr}
          41: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2);
            getreg(ep^.t1, rf); getfreg(ep^.t2, rf) 
          end;

          {noti}
          205: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2)
          end;

          {notb,odd,chr}
          42,50,60: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2) 
          end;

          {rnd}
          62: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgnull, rgnull) 
          end;

          {and,ior,xor}
          43,44,206: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull); assreg(ep^.r, rf, rgnull, rgnull);
          end;

          {dif,int,uni}
          45,46,47: begin 
            asscall; ep^.r1 := r1;
            if ep^.r1 = rgnull then ep^.r1 := rgrdi;
            assreg(ep^.l, rf, rgrdi, rgnull); assreg(ep^.r, rf, rgrsi, rgnull);
            ep^.r1a := ep^.l^.r1a
          end;

          {inn}
          48: begin 
            asscall;
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax else 
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdi, rgnull); assreg(ep^.r, rf, rgrsi, rgnull);
            if ep^.r1 <> rgrax then dstreg(rgrax)
          end;

          {mod}
          49: begin 
            if (r1 = rgnull) and (rgrdx in rf) then ep^.r1 := rgrdx
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrax, rgnull); assreg(ep^.r, rf, rgnull, rgnull);
            if ep^.r1 <> rgrax then dstreg(rgrax)
          end;

          {dvi}
          53: begin 
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrax, rgnull); assreg(ep^.r, rf, rgnull, rgnull);
            if ep^.r1 <> rgrax then dstreg(rgrax)
          end;

          {mpi}
          51: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull);
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {mpr,dvr}
          52,54: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull); 
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {rgs}
          110: begin 
            asscall; 
            assreg(ep^.l, rf, rgrdi, rgnull); assreg(ep^.r, rf, rgrsi, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a)
          end;

          { dupi, dupa, dupr, dups, dupb, dupc }
          181, 182, 183, 184, 185, 186: ;

          {cks}
          187:;

          {csp}
          15: begin
            asscall; asspar(ep, sppar[ep^.q]);
            if spfunc[ep^.q] then begin { function }
              if isfltres(ep) then begin
                if (r1 = rgnull) and (rgxmm0 in rf) then ep^.r1 := rgxmm0
                else ep^.r1 := r1;
                if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
                if ep^.r1 <> rgxmm0 then dstreg(rgxmm0)
              end else begin
                if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
                else ep^.r1 := r1;
                if ep^.r1 = rgnull then getreg(ep^.r1, rf);
                if ep^.r1 <> rgrax then dstreg(rgrax)
              end
            end
          end;
 
          {cuf}
          246: begin 
            asscall;
            if r1 = rgnull then begin
              if rgrax in rf then ep^.r1 := rgrax else getreg(ep^.r1, rf)
            end else ep^.r1 := r1;
            asspar(ep, ep^.q)
          end;

          {cup}
          12: begin
            asscall; asspar(ep, ep^.q)
          end;

          {cip}
          113: begin
            asscall; asspar(ep, ep^.q); assreg(ep^.l, rf, rgnull, rgnull)
          end;

          {cif}
          247: begin
            asscall;
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else ep^.r1 := r1;
            asspar(ep, ep^.q); assreg(ep^.l, rf, rgnull, rgnull)
          end;

          {cke}
          188: begin
            getreg(ep^.r1, frereg); getreg(ep^.r2, frereg); 
            getreg(ep^.t1, frereg); assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

        end
      end;

      { Interpret instruction macro string.
        The macros are:
        $0 - Immediate integer 1
        $1 - Immediate integer 2
        $s - Immediate symbol
        %1 - Register 1, l postfix means lower
        %2 - register 2, l postfix means lower
        +0 - Immediate integer 1
        +1 - Immediate integer 2
        -0 - Immediate integer 1
        -1 - Immediate integer 2
        ^0 - Immediate integer 1 without leader
        ^1 - Immediate integer 2 without leader
        @  - Symbol
      }
      procedure wrtins40(si: insstr40; i1, i2: integer; r1, r2: reg; sn: strvsp);
      var i,j: 1..insmax40;
      procedure next;
      begin if i = insmax40 then errorl('Error in instruction     '); i := i+1
      end;
        
      begin
        write(prr, ' ':tabspc);
        i := 1; while si[i] <> ' ' do begin write(prr, si[i]); next end;
        j := i;
        if j >= tabspc then write(prr, ' ');
        while j <= tabspc do begin write(prr, ' '); j := j+1 end;
        while (i < insmax40) and (si[i] = ' ') do next;
        while i < insmax40 do begin
          if si[i] = '$' then begin next; write(prr, '$');
            if si[i] = '0' then write(prr, i1:1) 
            else if si[i] = '1' then write(prr, i2:1)
            else if si[i] = 's' then writevp(prr, sn)
            else write(prr, si[i])
          end else if si[i] = '%' then begin next; write(prr, '%');
            if si[i] = '1' then begin
              if si[i+1] = 'l' then begin wrtbreg(prr, r1); i := i+1 end
              else wrtreg(prr, r1) 
            end else if si[i] = '2' then begin
              if si[i+1] = 'l' then begin wrtbreg(prr, r2); i := i+1 end
              else wrtreg(prr, r2)
            end else write(prr, si[i])
          end else if si[i] = '+' then begin next; write(prr, '+');
            if si[i] = '0' then write(prr, i1:1) 
            else if si[i] = '1' then write(prr, i2:1)
            else write(prr, si[i])
          end else if si[i] = '-' then begin next; write(prr, '-');
            if si[i] = '0' then write(prr, i1:1) 
            else if si[i] = '1' then write(prr, i2:1)
            else write(prr, si[i])
          end else if si[i] = '^' then begin next;
            if si[i] = '0' then write(prr, i1:1) 
            else if si[i] = '1' then write(prr, i2:1)
            else write(prr, si[i])
          end else if si[i] = '@' then writevp(prr, sn)
          else write(prr, si[i]);
          i := i+1
        end;
        writeln(prr)
      end;

      procedure wrtins10(si: insstr10; i1, i2: integer; r1, r2: reg; sn: strvsp);
      var s: insstr40;
          i: 1..insmax40;
      begin
        for i := 1 to insmax40 do s[i] := ' ';
        for i := 1 to insmax10 do s[i] := si[i];
        wrtins40(s, i1, i2, r1, r2, sn)
      end;

      procedure wrtins20(si: insstr20; i1, i2: integer; r1, r2: reg; sn: strvsp);
      var s: insstr40;
          i: 1..insmax40;
      begin
        for i := 1 to insmax40 do s[i] := ' ';
        for i := 1 to insmax20 do s[i] := si[i];
        wrtins40(s, i1, i2, r1, r2, sn)
      end;

      procedure wrtins30(si: insstr30; i1, i2: integer; r1, r2: reg; sn: strvsp);
      var s: insstr40;
          i: 1..insmax40;
      begin
        for i := 1 to insmax40 do s[i] := ' ';
        for i := 1 to insmax30 do s[i] := si[i];
        wrtins40(s, i1, i2, r1, r2, sn)
      end;

      procedure genexp(ep: expptr);
      var r: reg; ep2: expptr; i: integer;

      { push parameters to call depth first }
      procedure pshpar(ep: expptr);
      begin
        if ep <> nil then begin
          pshpar(ep^.next);
          dmptrel(ep, 1); genexp(ep);
          if ep^.r2 <> rgnull then
            wrtins20('pushq %1  ', 0, 0, ep^.r2, rgnull, nil); stkadr := stkadr-intsize;
          if ep^.r1 in [rgrax..rgr15] then
            wrtins20('pushq %1  ', 0, 0, ep^.r1, rgnull, nil); stkadr := stkadr-intsize;
          if ep^.r1 in [rgxmm0..rgxmm15] then begin
            wrtins20('subq -0,%rsp        ', realsize, 0, rgnull, rgnull, nil); 
            stkadr := stkadr-realsize;
            wrtins20('movsd %1,(%rsp)     ', 0, 0, ep^.r1, rgnull, nil)
          end
        end
      end;

      procedure callsp(ep: expptr; var sc: alfa; r: boolean);
      var si: insstr20; i: integer; pp: expptr;
      begin
        { evaluate all parameters }
        pp := ep^.pl;
        while pp <> nil do begin genexp(pp); pp := pp^.next end;
        if stkadr mod 16 <> 0 then
          wrtins10('pushq %rbx', 0, 0, rgnull, rgnull, nil);
        si := 'call psystem_       ';
        for i := 1 to maxalfa do if sc[i] <> ' ' then si[14+i-1] := sc[i];
        wrtins20(si, 0, 0, rgnull, rgnull, nil);
        if stkadr mod 16 <> 0 then
          wrtins10('popq %rbx ', 0, 0, rgnull, rgnull, nil);
        if r then begin
          if isfltres(ep) then begin
            if ep^.r1 <> rgxmm0 then 
              wrtins20('movsd %xmm0,%1      ', ep^.p, 0, ep^.r1, rgnull, nil)
          end else begin 
            if ep^.r1 <> rgrax then 
              wrtins20('movq %rax,%1        ', ep^.p, 0, ep^.r1, rgnull, nil);
            if (ep^.r2 <> rgnull) and (ep^.r2 <> rgrdx) then
              wrtins20('movq %rdx,%1        ', ep^.p, 0, ep^.r2, rgnull, nil)
          end
        end
      end;

      begin { genexp }
        if ep <> nil then begin
          for r := rgrax to rgr15 do if r in ep^.rs then begin
              wrtins10('pushq %1  ', 0, 0, r, rgnull, nil); 
              stkadr := stkadr-intsize
          end;
          genexp(ep^.al);
          if (ep^.op <> 113{cip}) and (ep^.op <> 247{cif}) then genexp(ep^.l);
          genexp(ep^.r); genexp(ep^.x1);
          write(prr, '# generating: '); dmpety(prr, ep); writeln(prr);
          case ep^.op of

            {lodi,loda}
            0,105: begin
              wrtins20('movq ^0(%rbp),%1    ', ep^.q1, 0, ep^.r1, rgnull, nil);
              wrtins20('movq ^0(%1),%1      ', ep^.q, 0, ep^.r1, rgnull, nil);
            end;

            {lodx,lodb,lodc}
            193,108,109: begin
              wrtins20('movq ^0(%rbp),%1    ', ep^.q1, 0, ep^.r1, rgnull, nil);
              wrtins20('movzx ^0(%1),%1      ', ep^.q, 0, ep^.r1, rgnull, nil);
            end;

            {lodr}
            106: begin
              wrtins20('movq ^0(%rbp),%1    ', ep^.q1, 0, ep^.t1, rgnull, nil);
              wrtins20('movsd (%1),%2     ', 0, 0, ep^.t1, ep^.r1, nil)
            end;

            {lods}
            107: begin
              wrtins20('movq ^0(%rbp),%rsi  ', ep^.p, 0, rgnull, rgnull, nil);
              wrtins20('lea ^0(%rsi),%rsi   ', ep^.q, 0, ep^.r1, rgnull, nil);
              wrtins30('leaq ^-@^0(%rbp),%rdi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins30('leaq ^-@^0(%rbp),%1 ', ep^.r1a, 0, ep^.r1, rgnull, lclspc);
            end;

            {lda}
            4: begin
              wrtins20('movq ^0(%rbp),%1    ', ep^.q1, 0, ep^.r1, rgnull, nil);
              wrtins20('lea ^0(%1),%1       ', ep^.q, 0, ep^.r1, rgnull, nil);
            end;

            {adi}
            28: 
              wrtins20('add %1,%2           ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);

            {adr}
            29: 
              wrtins20('addsd %1,%2         ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);

            {sbi}
            30: 
              wrtins20('sub %1,%2           ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);

            {sbr}
            31: 
              wrtins20('subsd %1,%2         ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);

            {equr,neqr,geqr,grtr,leqr,lesr}
            138,144,150,156,162,168: begin 
              case ep^.op of
                138{equr}: wrtins20('cmpeqsd %1,%2       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                144{neqr}: wrtins20('cmpneqsd %1,%2      ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                150{geqr}: wrtins20('cmplesd %2,%1       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                156{grtr}: wrtins20('cmpltsd %2,%1       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                162{leqr}: wrtins20('cmplesd %1,%2       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                168{lesr}: wrtins20('cmpltsd %1,%2       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
              end;
              if ep^.op in [150{geqr},156{grtr}] then 
                wrtins20('movq %1,%2          ', 0, 0, ep^.r^.r1, ep^.r1, nil)
              else
                wrtins20('movq %1,%2          ', 0, 0, ep^.l^.r1, ep^.r1, nil);
              wrtins20('andq $0,%1          ', 1, 0, ep^.r1, rgnull, nil) 
            end;

            120{lip}: begin 
              wrtins20('movq ^0(%rbp),%1    ', ep^.p, 0, ep^.t1, rgnull, nil);
              wrtins20('movq ^0(%1),%1    ', ep^.q+ptrsize, 0, ep^.r2, rgnull, nil);
              wrtins20('movq ^0(%1),%1    ', ep^.q, 0, ep^.r1, rgnull, nil)
            end;  

            {equm,neqm,geqm,gtrm,leqm,lesm}
            142,148,154,160,166,172: begin 
              wrtins20('movq $0,%rdx        ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_strcmp ', 0, 0, rgnull, rgnull, nil); 
              wrtins20('cmpq $0,%rax        ', 0, 0, rgnull, rgnull, nil);
              case ep^.op of
                142{equm}: wrtins10('sete %1l  ', 0, 0, ep^.r1, rgnull, nil);
                148{neqm}: wrtins10('setne %1l ', 0, 0, ep^.r1, rgnull, nil);
                154{geqm}: wrtins10('setge %1l ', 0, 0, ep^.r1, rgnull, nil);
                160{grtm}: wrtins10('setg %1l  ', 0, 0, ep^.r1, rgnull, nil);
                166{leqm}: wrtins10('setle %1l ', 0, 0, ep^.r1, rgnull, nil);
                172{lesm}: wrtins10('setl %1l  ', 0, 0, ep^.r1, rgnull, nil);
              end;
              wrtins20('movsx %1l,%1        ', 0, 0, ep^.r1, rgnull, nil)
            end;

            5{lao}:
              wrtins40('leaq globals_start+0(%rip),%1          ', ep^.q, 0, ep^.r1, rgnull, nil);

            16{ixa}: begin 
              if ep^.r1 <> ep^.t1 then
                wrtins10('movq %1,%2', 0, 0, ep^.r1, ep^.t1, nil);
              wrtins20('movq $0,%rax        ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins10('mul %1    ', 0, 0, ep^.r^.r1, rgnull, nil);
              wrtins20('add %rax,%1         ', 0, 0, ep^.t1, rgnull, nil);
              if ep^.r1 <> ep^.t1 then
                wrtins10('movq %1,%2', 0, 0, ep^.t1, ep^.r1, nil)
            end;

            118{swp}: ; { done at top level }

            {ldoi,loda}
            1,65:
              wrtins40('movq globals_start+0(%rip),%1          ', ep^.q, 0, ep^.r1, rgnull, nil);

            {ldob,ldoc,ldox}
            68,69,194:
              wrtins40('movzx globals_start+0(%rip),%1          ', ep^.q, 0, ep^.r1, rgnull, nil);

            {ldor}
            66: 
              wrtins40('movsd globals_start+0(%rip),%1          ', ep^.q, 0, ep^.r1, rgnull, nil);

            {ldos}
            67: begin
              wrtins40('leaq globals_start+0(%rip),%rsi         ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins30('leaq ^-@^0(%rbp),%rdi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins30('leaq ^-@^0(%rbp),%1 ', ep^.r1a, 0, ep^.r1, rgnull, lclspc)
            end;

            {indi,inda}
            9,85: 
              wrtins20('movq ^0(%1),%1  ', ep^.q, 0, ep^.l^.r1, rgnull, nil);

            {indr}
            86: 
              wrtins20('movsd ^0(%2),%1  ', ep^.q, 0, ep^.r1, ep^.r2, nil);

            {indb,indc,indx}
            88,89,198: wrtins20('movzx ^0(%1),%1  ', ep^.q, 0, ep^.l^.r1, rgnull, nil);

            {inds}
            87: begin 
              wrtins30('leaq ^-@^0(%rbp),%rdi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins30('leaq ^-@^0(%rbp),%1 ', ep^.r1a, 0, ep^.r1, rgnull, lclspc)
            end;

            {inci,inca,incb,incc,incx}
            10, 90, 93, 94, 201: 
              wrtins20('addq $0,%1          ', ep^.q, 0, ep^.r1, rgnull, nil);

            {deci,decb,decc,decx}
            57, 103, 104, 202: 
              wrtins20('subq $0,%1          ', ep^.q, 0, ep^.r1, rgnull, nil);

            {ckvi,ckvb,ckvc,ckvx}
            175, 179, 180, 203: begin 
              wrtins20('cmpq $0,%1          ', ep^.q, 0, ep^.r1, rgnull, nil);
              wrtins20('sete %1l            ', 0, 0, ep^.t1, rgnull, nil);
              wrtins20('orq %1,%2           ', 0, 0, ep^.t1, ep^.r2, nil);
            end;

            {cvbi,cvbx,cvbb,cvbc}
            100, 115, 116, 121: begin
              wrtins20('movq $0,%rdi        ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rsi        ', ep^.q1, 0, rgnull, rgnull, nil);
              wrtins20('leaq @(%rip),%rdx   ', 0, 0, rgnull, rgnull, ep^.lt);
              if ep^.op = 100 then
                wrtins20('movq (%1),%r8       ', 0, 0, ep^.l^.r1, rgnull, nil)
              else
                wrtins20('movzx (%1),%r8      ', ep^.q, 0, ep^.l^.r1, rgnull, nil);
              wrtins30('call psystem_tagchgvar        ', 0, 0, rgnull, rgnull, nil)
            end;

            {ivti,ivtx,ivtb,ivtc}
            192,101,102,111: begin
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rsi         ', ep^.q1, 0, rgnull, rgnull, nil);
              wrtins20('leaq @(%rip),%rdx    ', 0, 0, rgnull, rgnull, ep^.lt);
              if ep^.op = 100 then
                wrtins20('movq (%1),%r8        ', ep^.q, 0, ep^.l^.r1, rgnull, nil)
              else
                wrtins20('movzx (%1),%r8       ', ep^.q, 0, ep^.l^.r1, rgnull, nil);
              wrtins30('call psystem_tagchginv         ', 0, 0, rgnull, rgnull, nil)
            end;

            {cps}
            176: begin 
              wrtins10('cmpq %1,%2', 0, 0, ep^.r^.r2, ep^.l^.r2, nil);
              wrtins10('je 1f     ', ep^.q, 0, ep^.r^.r1, rgnull, nil);
              wrtins30('movq $ContainerMismatch,%rdi  ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);
            end;

            {cpc}
            177: begin 
              wrtins20('movq $0,%rdi        ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_cmptmp ', 0, 0, rgnull, rgnull, nil)
            end;

            {cta}
            191: begin
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rsi         ', ep^.q1, 0, rgnull, rgnull, nil);
              wrtins20('leaq @(%rip),%rdx    ', 0, 0, rgnull, rgnull, ep^.lt);
              wrtins30('call psystem_tagchkass           ', 0, 0, rgnull, rgnull, nil)
            end;

            {lpa}
            114: begin 
              wrtins20('leaq @(%rip),%1    ', 0, 0, ep^.r1, rgnull, ep^.fn);
              wrtins20('movq ^0(%rbp),%1    ', ep^.q1, 0, ep^.r2, rgnull, nil)
            end;

            {ldci,ldcc,ldcb}
            123,127,126:
              wrtins20('movq $0,%1          ', ep^.vi, 0, ep^.r1, rgnull, nil); 

            {ldcn}
            125:
              wrtins20('movq $0,%1          ', 0, 0, ep^.r1, rgnull, nil);

            {ldcr}
            124:
              wrtins30('movsd real^0(%rip),%1         ', ep^.realn, 0, ep^.r1, rgnull, nil);

            {ldcs}
            7: begin
              wrtins30('leaq set^0(%rip),%rsi         ', ep^.setn, 0, rgnull, rgnull, nil);
              wrtins30('leaq ^-@^0(%rbp),%rdi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins20('leaq ^-@^0(%rbp),%1  ', ep^.r1a, 0, ep^.r1, rgnull, lclspc);
            end;

            {chki,chka,chkb,chkc,chkx}
            26, 95, 98, 99, 199: begin 
              wrtins20('movq $0,%1          ', ep^.vi, 0, ep^.t1, rgnull, nil);
              wrtins20('cmpq %1,%2          ', 0, 0, ep^.t1, ep^.r1, nil);
              wrtins20('jge 1f              ', 0, 0, rgnull, rgnull, nil);
              wrtins30('movq $ValueOutOfRange,%rdi    ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);
              wrtins20('movq $0,%1          ', ep^.vi2, 0, ep^.t1, rgnull, nil);
              wrtins20('cmpq %1,%2          ', 0, 0, ep^.t1, ep^.r1, nil);
              wrtins20('jle 1f              ', 0, 0, rgnull, rgnull, nil);
              wrtins30('movq $ValueOutOfRange,%rdi    ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp)
            end;

            {chks}
            97: begin
              wrtins20('movq $0,%rdi         ', ep^.vi, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rsi         ', ep^.vi2, 0, rgnull, rgnull, nil);
              wrtins30('call psystem_chksetbnd         ', 0, 0, rgnull, rgnull, nil);
              if ep^.r1 <> rgrdx then
                wrtins10('movq %1,%2', ep^.vi, 0, ep^.l^.r1, ep^.r1, nil);

            end;

            {ckla}
            190: begin
              if ep^.q <> 0 then begin
                wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
                wrtins20('jge 1f              ', 0, 0, ep^.r2, rgnull, nil);
                wrtins40('movq $DereferenceOfNilPointer,%rdi      ', 0, 0, rgnull, rgnull, nil);
                wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
                wrtins10('1:        ', 0, 0, rgnull, rgnull, sp)
              end
            end;

            56 {lca}: wrtins30('leaq string^0(%rip),%1        ', ep^.strn, 0, ep^.r1, rgnull, nil); 

            {grts,less}
            158,170: ; { are invalid }

            {equs,neqs,geqs,leqs}
            140,146,152,164: begin 
              case ep^.op of
                140: wrtins20('call psystem_setequ ', 0, 0, rgnull, rgnull, nil);
                146: begin
                  wrtins20('call psystem_setequ ', 0, 0, rgnull, rgnull, nil);
                  wrtins20('xor $0,%rax         ', 1, 0, rgnull, rgnull, nil);
                end;
                152,164: wrtins20('call psystem_setinc ', 0, 0, rgnull, rgnull, nil);
              end;
              wrtins20('movq %rax,%1        ', 0, 0, ep^.r1, rgnull, nil);
              puttmp(ep^.l^.r1a); puttmp(ep^.r^.r1a)
            end;

            {equa,equi,equb,equc}
            17, 137, 139, 141,
            {neqa,neqi,neqb,neqc}
            18, 143, 145, 147,
            {geqi,geqb,geqc}
            149, 151, 153,
            {grti,grtb,grtc}
            155, 157, 159,
            {leqi,leqb,leqc}
            161, 163, 165,
            {lesi,lesb,lesc}
            167, 169, 171: begin 
              wrtins10('cmp %1,%2 ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
              case ep^.op of
                17,137,138,139,141: wrtins10('sete %1l  ', 0, 0, ep^.r1, rgnull, nil);
                18,143,144,145,147: wrtins10('setne %1l ', 0, 0, ep^.l^.r1, rgnull, nil);
                149,150,151,153: wrtins10('setge %1l ', 0, 0, ep^.l^.r1, rgnull, nil);
                155,156,157,159: wrtins10('setg %1l  ', 0, 0, ep^.l^.r1, rgnull, nil);
                161,162,163,165: wrtins10('setle %1l ', 0, 0, ep^.l^.r1, rgnull, nil);
                167,168,169,171: wrtins10('setl %1l  ', 0, 0, ep^.l^.r1, rgnull, nil)
              end;
              wrtins20('movsx %1l,%1        ', 0, 0, ep^.l^.r1, rgnull, nil)
            end;

            {ordi,ordb,ordc,ordx}
            59, 134, 136, 200: ; { ord is a no-op }

            {lcp}
            135: begin 
              wrtins20('movq (%1),%2        ', 0, 0, ep^.l^.r1, ep^.r1, nil);
              wrtins20('movq ^0(%1),%2      ', ptrsize, 0, ep^.l^.r1, ep^.r2, nil)
            end;

            {sgs}
            32: begin
              wrtins30('leaq ^-@^0(%rbp),%rdi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
              wrtins20('call psystem_setsgl ', 0, 0, rgnull, rgnull, nil);
              wrtins20('leaq ^-@^0(%rbp),%1 ', ep^.r1a, 0, ep^.r1, rgnull, lclspc);
            end;

            {flt,flo}
            33,34: wrtins20('cvtsi2sd %1,%2      ', 0, 0, ep^.l^.r1, ep^.r1, nil);

            {trc}
            35: wrtins20('cvttsd2si %1,%2     ', 0, 0, ep^.l^.r1, ep^.r1, nil);

            {ngi}
            36: wrtins20('negq %1     ', 0, 0, ep^.r1, rgnull, nil);

            {ngr}
            37: begin 
              wrtins20('subsd %1,%1         ', 0, 0, ep^.r1, ep^.r1, nil);
              wrtins20('subsd %1,%2         ', 0, 0, ep^.l^.r1, ep^.r1, nil)
            end;

            {sqi}
            38: begin 
              wrtins20('imulq %1,%1          ', 0, 0, ep^.r1, rgnull, nil);
            end;

            {sqr}
            39: begin 
              wrtins20('mulsd %1,%1         ', 0, 0, ep^.r1, rgnull, nil);
            end;

            {abi}
            40: begin
              wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('jns 1f              ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('negq %1             ', 0, 0, ep^.r1, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);

            end;

            {abr}
            41: begin 
              wrtins30('movq $0x7fffffffffffffff,%1  ', 0, 0, ep^.t1, rgnull, nil);
              wrtins20('movq %1,%2          ', 0, 0, ep^.t1, ep^.t2, nil);
              wrtins20('andpd %1,%2         ', 0, 0, ep^.t2, ep^.r1, nil)
            end;

            {notb}
            42: begin 
              wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('movq $1,%1          ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('jz 1f               ', 0, 0, ep^.r2, rgnull, nil);
              wrtins20('movq $0,%1          ', 0, 0, ep^.r1, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp)
            end;

            {noti}
            205: begin 
              wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('jns 1f           ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq $BooleanOperatorOfNegative,%rdi      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins20('not %1              ', 0, 0, ep^.r1, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp)
            end;

            {odd}
            50: begin 
              wrtins20('andq $0,%1          ', 1, 0, ep^.r1, rgnull, nil);
            end;

            {rnd}
            62: wrtins20('cvtsd2si %1,%2      ', 0, 0, ep^.l^.r1, ep^.r1, nil);

            {chr}
            60: ; { chr is no-op }

            {and,ior,xor}
            43,44,206: begin 
              wrtins20('orq %1,%1           ', 0, 0, ep^.l^.r1, rgnull, nil);
              wrtins20('jns 1f              ', 0, 0, rgnull, rgnull, nil);
              wrtins40('movq $BooleanOperatorOfNegative,%rdi    ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);
              wrtins20('orq %1,%1           ', 0, 0, ep^.r^.r1, rgnull, nil);
              wrtins20('jns 1f              ', 0, 0, rgnull, rgnull, nil);
              wrtins40('movq $BooleanOperatorOfNegative,%rdi    ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);
              case ep^.op of
                43: wrtins20('andq %1,%2           ', 0, 0, ep^.l^.r1, ep^.r^.r1, nil);
                44: wrtins20('orq %1,%2           ', 0, 0, ep^.l^.r1, ep^.r^.r1, nil);
                206: wrtins20('xorq %1,%2          ', 0, 0, ep^.l^.r1, ep^.r^.r1, nil)
              end
            end;

            {dif,int,uni}
            45,46,47: begin
              case ep^.op of             
                45: wrtins20('call psystem_setdif ', 0, 0, rgnull, rgnull, nil);
                46: wrtins20('call psystem_setint ', 0, 0, rgnull, rgnull, nil);
                47: wrtins20('call psystem_setuni ', 0, 0, rgnull, rgnull, nil);
              end;
              wrtins30('leaq ^-@^0(%rbp),%1         ', ep^.r1a, 0, ep^.r1, rgnull, lclspc);
              puttmp(ep^.r^.r1a)
            end;

            {inn}
            48: begin
              wrtins20('call psystem_setsin ', 0, 0, rgnull, rgnull, nil);
              if ep^.r1 <> rgrax then
                wrtins20('movq %rax,%1          ', 0, 0, ep^.r1, rgnull, nil)
            end;

            {mod}
            49: begin 
              wrtins20('xorq %rdx,%rdx      ', 0, 0, rgnull, rgnull, nil);
              wrtins10('idivq %1  ', 0, 0, ep^.r^.r1, rgnull, nil);
              if ep^.r1 <> rgrdx then
                wrtins20('movq %rdx,%1        ', 0, 0, ep^.r1, rgnull, nil)
            end;

            {dvi}
            53: begin 
              wrtins20('xorq %rdx,%rdx      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('subq $0,%rax        ', 0, 0, ep^.r^.r1, rgnull, nil);
              wrtins10('jns 1f    ', 0, 0, ep^.r^.r1, rgnull, nil);
              wrtins10('decq %rdx ', 0, 0, rgnull, rgnull, sp);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);
              wrtins10('idivq %1  ', 0, 0, ep^.r^.r1, rgnull, nil);
              if ep^.r1 <> rgrax then
                wrtins20('movq %rax,%1        ', 0, 0, ep^.r1, rgnull, nil)
            end;

            {mpi}
            51: wrtins20('imulq %1,%2         ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);

            {mpr}
            52: wrtins20('mulsd %1,%2         ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);

            {dvr}
            54: wrtins20('divsd %1,%2         ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);

            {rgs}
            110: begin 
              wrtins30('leaq ^-@^0(%rbp),%rdx         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
              wrtins20('call psystem_setrgs ', 0, 0, rgnull, rgnull, nil);
              wrtins20('leaq ^-@^0(%rbp),%1 ', ep^.r1a, 0, ep^.r1, rgnull, lclspc);
            end;

            { dupi, dupa, dupr, dups, dupb, dupc }
            181, 182, 183, 184, 185, 186: ;

            {cks}
            187: ;

            {csp}
            15: begin
              if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then begin
                { need irregular handling for nwl and dsl }
                ep2 := ep^.pl;
                for i := 1 to 3 do begin
                  if ep2 = nil then errorl('system error             ');
                  ep2 := ep2^.next
                end;
                pshpar(ep2);
                pp := ep^.pl; genexp(pp); { addr rdi }
                pp := pp^.next; genexp(pp); { size rsi }
                pp := pp^.next; genexp(pp); { tagcnt rdx }
                wrtins20('movq %rsp,%rcx      ', 0, 0, rgnull, rgnull, nil);
                wrtins10('pushq %rcx', 0, 0, rgnull, rgnull, nil); 
                stkadr := stkadr-intsize;
                if stkadr mod 16 <> 0 then
                  wrtins10('pushq %rbx', 0, 0, rgnull, rgnull, nil);
                if ep^.q = 39 then
                  wrtins20('call psystem_nwl    ', 0, 0, rgnull, rgnull, nil)
                else
                  wrtins20('call psystem_dsl    ', 0, 0, rgnull, rgnull, nil);
                if stkadr mod 16 <> 0 then
                  wrtins10('popq %rbx ', 0, 0, rgnull, rgnull, nil);
                wrtins10('popq %rcx ', 0, 0, rgnull, rgnull, nil); 
                stkadr := stkadr+intsize;
                wrtins20('movq $0,%rax        ', intsize, 0, rgnull, rgnull, nil);
                wrtins10('mulq %rcx ', 0, 0, rgnull, rgnull, nil);
                wrtins20('addq %rcx,%rsp      ', 0, 0, rgnull, rgnull, nil);
              end else callsp(ep, sptable[ep^.q], spfunc[ep^.q])
            end;

            {sfr}
            245:
              if ep^.lb <> nil then
                wrtins20('sub $s,%rsp         ', 0, 0, rgnull, rgnull, ep^.lb);

            {cup,cuf}
            12, 246: begin
              genexp(ep^.sl); { process sfr start link }
              pshpar(ep^.pl); { process parameters first }
              wrtins10('call @    ', 0, 0, rgnull, rgnull, ep^.fn);
              if (ep^.op = 246{cuf}) and (ep^.r1 <> rgrax) then
                wrtins20('movq %rax,%1        ', 0, 0, ep^.r1, rgnull, nil);
            end;

            {cip,cif}
            113,247: begin
              genexp(ep^.sl); { process sfr start link }
              pshpar(ep^.pl); { process parameters first }
              genexp(ep^.l); { load procedure address }
              wrtins20('movq ^0(%1),%rbp   ', 1*ptrsize, 0, ep^.l^.r1, rgnull, nil);
              wrtins10('call *(%1)', 0, 0, ep^.l^.r1, rgnull, nil);
              if (ep^.op = 247{cif}) and (ep^.r1 <> rgrax) then
                wrtins20('movq %rax,%1        ', 0, 0, ep^.r1, rgnull, nil);
            end;

            {cke}
            188: begin
              wrtins10('movq $0,%1', 0, 0, ep^.r2, rgnull, nil);
              ep2 := ep^.cl; 
              while ep2 <> nil do begin 
                ep2^.r1 := ep^.r1; ep2^.r2 := ep^.r2; ep2^.t1 := ep^.t1; 
                genexp(ep2); ep2 := ep2^.next 
              end;   
              wrtins10('jnz 1f    ', 0, 0, rgnull, rgnull, nil);
              wrtins30('movq $VariantNotActive,%rdi   ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);
            end;

          end;
          for r := rgr15 downto rgrax do if r in ep^.rs then begin
            wrtins20('popq %1             ', 0, 0, r, rgnull, nil);
            stkadr := stkadr-intsize
          end
        end
      end;

      { evaluate and push a parameter list, right to left }
      procedure pshpar(ep: expptr; pn, pc: integer);
      begin
        if (ep <> nil) and (pn < 6) and (pc > 1) then 
          pshpar(ep^.next, pn+1, pc-1);
        if pn > 6 then assreg(ep, frereg, rgnull, rgnull)
        else case pn of
          1: assreg(ep, frereg, rgrdi, rgnull);
          2: assreg(ep, frereg, rgrsi, rgnull);
          3: assreg(ep, frereg, rgrdx, rgnull);
          4: assreg(ep, frereg, rgrcx, rgnull);
          5: assreg(ep, frereg, rgr8, rgnull);
          6: assreg(ep, frereg, rgr9, rgnull)
        end;
        dmptrel(ep, 1); genexp(ep);
        if ep^.r2 <> rgnull then
          wrtins20('pushq %1  ', 0, 0, ep^.r2, rgnull, nil); stkadr := stkadr-intsize;
        if ep^.r1 in [rgrax..rgr15] then
          wrtins20('pushq %1  ', 0, 0, ep^.r1, rgnull, nil); stkadr := stkadr-intsize;
        if ep^.r1 in [rgxmm0..rgxmm15] then begin
          wrtins20('subq -0,%rsp        ', realsize, 0, rgnull, rgnull, nil); 
          stkadr := stkadr-realsize;
          wrtins20('movsd %1,(%rsp)     ', 0, 0, ep^.r1, rgnull, nil)
        end
      end;

      { remove parameter list }
      procedure poppar(pc: integer);
      var ep: expptr;
      begin
        while (estack <> nil) and (pc >= 1) do begin popstk(ep); deltre(ep) end
      end;

      { get number of parameters of procedure/function/system call to parameters
        list }
      procedure getparn(ep: expptr; pn: integer);
      var pp: expptr;
      begin
          { pull parameters into list }
          while (pn > 0) and (estack <> nil) do 
            begin popstk(pp); pp^.next := ep^.pl; ep^.pl := pp; pn := pn-1 end
      end;

      { get parameters of procedure/function/system call to parameters list }
      procedure getpar(ep: expptr);
      begin
          { sfr starts the list }
          ep^.q := 0;
          if stacklvl > parlvl then ep^.q := stacklvl-parlvl;
          getparn(ep, ep^.q); { get those parameters into list }
          popstk(ep^.sl); { get sfr start }
          if ep^.sl^.op <> 245{sfr} then errorl('system error             ');
          parlvl := maxint { set parameter level inactive }
      end;

      { attach tag chk sequence to leaf }
      procedure attach(ep: expptr);
      begin
        if estack <> nil then
          if estack^.op = 188{cke} then popstk(ep^.al)
      end;

      { duplicate subtree }
      procedure duptre(s: expptr; var d: expptr);
      begin
        if s = nil then d := nil else begin { only copy tree components }
          getexp(d); d^ := s^; d^.next := nil; d^.sl := nil; d^.cl := nil; 
          d^.al := nil; d^.pl := nil;
          duptre(s^.l, d^.l); duptre(s^.r, d^.r); duptre(s^.x1, d^.x1)
        end
      end;

    begin { assemble } 
      p := 0;  q := 0;  op := 0; stkadr := 0;
      getname;
      { note this search removes the top instruction from use }
      while (instr[op]<>name) and (op < maxins) do op := op+1;
      if op = maxins then errorl('illegal instruction      ');
      prtline; write(prr, op:3, ': ', name:8);
      case op of

        { *** non-terminals *** }

        {lodi,lodx,loda,lodr,lods,lodb,lodc,loda,lda}
        0,193,105,106,107,108,109,4: begin 
          read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          q1 := -p*ptrsize; getexp(ep); attach(ep); pshstk(ep) 
        end;

        {adi,adr,sbi,sbr}
        28, 29, 30, 31: begin writeln(prr); getexp(ep); popstk(ep^.r);
          popstk(ep^.l); pshstk(ep)
        end;

        {lip} 
        120: begin read(prd,p,q); writeln(prr,p:1,' ', q:1); getexp(ep);
          pshstk(ep) 
        end;

        { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
        142, 148, 154, 160, 166, 172: begin read(prd,q); writeln(prr,q:1); 
          getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
        end;

        {lao} 
        5: begin read(prd,q); writeln(prr,q:1); getexp(ep); attach(ep); 
          pshstk(ep)
        end;

        {ixa}
        16: begin read(prd,q); writeln(prr,q:1); getexp(ep); 
          popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
        end;

        {swp}
        118: begin read(prd,q); writeln(prr,q:1); popstk(ep); 
          popstk(ep2); pshstk(ep); pshstk(ep2) 
        end;

        {ldoi,ldoa,ldor,ldos,ldob,ldoc,ldox}
        1, 65, 66, 67, 68, 69, 194: begin read(prd,q); writeln(prr,q:1);
          getexp(ep); attach(ep); pshstk(ep) 
        end;

        {indi,inda,indr,inds,indb,indc,indx}
        9, 85, 86, 87, 88, 89, 198: begin read(prd,q); writeln(prr,q:1);
          getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep)
        end;

        {inci,inca,incb,incc,incx,deci,deca,decb,decc,decx}
        10, 90, 93, 94, 201, 57, 103, 104, 202: begin read(prd,q); 
          writeln(prr,q:1); getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {ckvi,ckvb,ckvc,ckvx}
        175, 179, 180, 203: begin read(prd,q); writeln(prr,q:1); 
          getexp(ep); pshstk(ep) 
        end;

        {cps}
        176: begin writeln(prr); getexp(ep); popstk(ep^.r); popstk(ep^.l); 
          pshstk(ep) 
        end;

        {cpc}
        177: begin writeln(prr); getexp(ep); popstk(ep^.r); popstk(ep^.l);
          pshstk(ep) 
        end;


        {lpa}
        114: begin read(prd,p); labelsearch(def, val, sp); writeln(prr); 
          q1 := -p*ptrsize; getexp(ep); ep^.fn := sp; pshstk(ep);
        end;

        {ldcs,ldci,ldcr,ldcn,ldcb,ldcc}
        7, 123, 124, 125, 126, 127: begin case op of

          123: begin read(prd,i); writeln(prr, i:1); 
            getexp(ep); attach(ep); ep^.vi := i; pshstk(ep) 
          end;

          124: begin read(prd,r); writeln(prr, r); getexp(ep); attach(ep);
            pshstk(ep); new(cstp); cstp^.ct := creal; 
            cstp^.r := r; realnum := realnum+1; 
            cstp^.realn := realnum; cstp^.next := csttbl; 
            csttbl := cstp; ep^.realn := realnum 
          end;

          125: begin writeln(prr); getexp(ep); pshstk(ep) 
          end;

          126: begin read(prd,i); writeln(prr, i:1); 
            getexp(ep); attach(ep); ep^.vi := i; pshstk(ep) 
          end;

          127: begin
            skpspc;
            if ch in ['0'..'9'] then begin i := 0;
              while ch in ['0'..'9'] do
                begin i := i*10+ord(ch)-ord('0'); getnxt end;
              c := chr(i);
              writeln(prr, i)
            end else begin
              if ch <> '''' then errorl('illegal character        ');
              getnxt;  c := ch;
              getnxt;
              if ch <> '''' then errorl('illegal character        ');
              writeln(prr, '''', c, '''')
            end;
            getexp(ep); attach(ep); ep^.vi := ord(c); pshstk(ep)
          end;

          7: begin skpspc;
            if ch <> '(' then errorl('ldcs() expected          ');
            s := [ ];  getnxt;
            while ch<>')' do
              begin read(prd,s1); getnxt; s := s + [s1] end;
            getexp(ep); attach(ep); pshstk(ep);
            writeln(prr);
            new(cstp); cstp^.ct := cset; cstp^.s := s;
            setnum := setnum+1; cstp^.setn := setnum;
            cstp^.next := csttbl; csttbl := cstp; ep^.setn := setnum
          end

          end (*case*)
        end;

        {chki,chka,chks,chkb,chkc,ckla,chkx}
        26, 95, 97, 98, 99, 190, 199: begin read(prd,lb,ub); 
          writeln(prr, lb:1, ' ', ub:1); getexp(ep); popstk(ep^.l); 
          pshstk(ep); ep^.vi := lb; ep^.vi2 := ub
        end;

        {lca}
        56: begin read(prd,l); write(prr, l:1, ' '); skpspc;
          for i := 1 to strlen do str[i] := ' ';
          if ch <> '''' then errorl('bad string format        ');
          i := 0;
          repeat
            if eoln(prd) then errorl('unterminated string      ');
            getnxt;
            c := ch; if (ch = '''') and (prd^ = '''') then 
              begin getnxt; c := ' ' end;
            if c <> '''' then begin
              if i >= strlen then errorl('string overflow          ');
              str[i+1] := ch; { accumulate string }
              i := i+1
            end
          until c = '''';
          getexp(ep); attach(ep); pshstk(ep);
          writeln(prr, '"', str:l,'"');
          new(cstp); cstp^.ct := cstr; strassvsb(cstp^.str, str); 
          cstp^.strl := l; strnum := strnum+1; cstp^.strn := strnum;
          cstp^.next := csttbl; csttbl := cstp; ep^.strn := strnum
        end;

        {grts,less}
        158,170: errorl('Invalid operand          ');

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
        167, 168, 169, 171: begin writeln(prr); getexp(ep); 
          { reverse order for leqs }
          if op = 164 then begin popstk(ep^.l); popstk(ep^.r) end
          else begin popstk(ep^.r); popstk(ep^.l) end;
          pshstk(ep)
        end;

        {brk}
        19: ; { unused }

        {ord}
        59, 134, 136, 200: begin writeln(prr); getexp(ep); popstk(ep^.l);
          pshstk(ep);
        end;

        {lcp}
        135: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {sgs}
        32: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;
 
        {flt}
        33: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {flo}
        34: begin writeln(prr); getexp(ep); popstk(ep2); popstk(ep^.l);
          pshstk(ep); pshstk(ep2)
        end;

        {trc}
        35: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep); 
        end;

        {ngi,ngr}
        36,37: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {sqi,sqr}
        38,39: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep)
        end;

        {abi,abr}
        40,41: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {notb,odd,chr,rnd,noti}
        42,50,60,62,205: begin writeln(prr); getexp(ep); popstk(ep^.l); 
          pshstk(ep)
        end;

        {and,ior,xor,dif,int,uni,inn,mod,mpi,mpr,dvi,dvr,rgs}
        43,44,45,46,47,48,49,51,52,53,54,110,206: begin writeln(prr); 
          getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
        end;

        { At this level we just duplicate the tree. At lower levels we can
          optimize this. }

        { dupi, dupa, dupr, dups, dupb, dupc }
        181, 182, 183, 184, 185, 186: begin writeln(prr); 
          duptre(estack, ep); pshstk(ep);
        end;

        {cks}
        187: begin writeln(prr); 
          getexp(ep); pshstk(ep)
        end;

        {sfr}
        245: begin labelsearch(def, val, sp); write(prr, 'l '); writevp(prr, sp);
          writeln(prr);
          getexp(ep); pshstk(ep); 
          ep^.lb := nil;
          if (def and (val <> 0)) or not def then ep^.lb := sp;
          parlvl := stacklvl { set parameter level active }
        end;

        {cuf}
        246: begin labelsearch(def, val, sp); write(prr, 'l '); writevp(prr, sp); 
          writeln(prr);
          getexp(ep); ep^.fn := sp; getpar(ep); pshstk(ep);
        end;

        {cif}
        247: begin writeln(prr);
          getexp(ep); popstk(ep^.l); getpar(ep); pshstk(ep);
        end;

        {cke}
        188: begin writeln(prr); 
          getexp(ep);
          ep4 := estack;
          while ep4 <> nil do begin
            if estack^.op in [{cks}187,{ckvb}179,{ckvc}180,{ckvi}175,{ckvx}203] then begin
              popstk(ep5); 
              if ep5^.op <> 187 then begin ep5^.next := ep^.cl; ep^.cl := ep5 end
              else putexp(ep5);
              ep4 := estack
            end else ep4 := nil
          end;
          popstk(ep^.l); pshstk(ep)
        end;

        { *** calls can be terminal or non-terminal *** }

        {csp} 
        15: begin skpspc; getname;
          while name<>sptable[q] do begin 
            q := q+1; if q > maxsp then errorl('std proc/func not found  ')
          end; 
          writeln(prr, sptable[q]);
          getexp(ep); 
          if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then getparn(ep, maxint)
          else getparn(ep, sppar[q]);
          if spfunc[q] then pshstk(ep) { non-terminal, stack it }
          else begin { terminal, execute here }
            if spkeep[ep^.q] then begin
              if ep^.pl = nil then errorl('System error             ');
              duptre(ep^.pl, ep2); pshstk(ep2)
            end;
            frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
            dmptre(ep); genexp(ep);
            deltre(ep)
          end
        end;

        {cuv}
        27: begin labelsearch(def, val, sp); write(prr, 'l '); writevp(prr, sp); 
          writeln(prr);
          frereg := allreg;
          wrtins10('call *@   ', 0, 0, rgnull, rgnull, sp)
        end;

        { *** terminals *** }

        {cvbi,cvbx,cvbb,cvbc}
        100, 115, 116, 121,
        {ivti,ivtx,ivtb,ivtc,cta}
        192,101,102,111,191: begin read(prd,q, q1); labelsearch(def, val, sp); 
          write(prr,q:1, ' ', q1:1, ' l '); writevp(prr, sp); writeln(prr);
          getexp(ep); ep^.lt := sp; popstk(ep2); popstk(ep3); 
          duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); deltre(ep)
        end;

        {cup}
        12: begin labelsearch(def, val, sp); write(prr, 'l '); writevp(prr, sp); 
          writeln(prr);
          getexp(ep); ep^.fn := sp; getpar(ep);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
          genexp(ep); deltre(ep);
        end;

        {cip}
        113: begin writeln(prr);
          getexp(ep); popstk(ep^.l); getpar(ep);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
          genexp(ep); deltre(ep);
        end;

        {rip}
        13: begin read(prd,q); writeln(prr);
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('movq ^0(%rsp),%rbp    ', q, 0, rgnull, rgnull, nil)
        end;

        {stri,stra}
        2,70: begin read(prd,p,q); writeln(prr,p:1,' ', q:1);
          frereg := allreg;
          popstk(ep); getreg(r1, frereg); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('movq ^0(%rbp),%1    ', -p*ptrsize, 0, r1, rgnull, nil);
          wrtins20('movq %1,^0(%2)       ', q, 0, ep^.r1, r1, nil);
          deltre(ep); 
          botstk 
        end;

        {strx,strb,strc} 
        195,73,74: begin
          read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          frereg := allreg; getreg(r1, frereg);
          popstk(ep); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('movq ^0(%rbp),%1    ', -p*ptrsize, 0, r1, rgnull, nil);
          wrtins20('movb %1l,^0(%2)      ', q, 0, ep^.r1, r1, nil);
          deltre(ep); 
          botstk 
        end;

        {strr}
        71: begin read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          frereg := allreg; getreg(r1, frereg);
          popstk(ep); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('movq ^0(%rbp),%1    ', -p*ptrsize, 0, r1, rgnull, nil);
          wrtins20('movsd %1,^0(%2)     ', q, 0, ep^.r1, r1, nil);
          deltre(ep); 
          botstk 
        end;

        {strs} 
        72:begin read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          frereg := allreg; popstk(ep); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('movq ^0(%rbp),%rdi  ', -p*ptrsize, 0, ep^.t1, rgnull, nil);
          wrtins20('lea %1,^0(%2)       ', q, 0, ep^.r1, ep^.t1, nil);
          wrtins30('leaq ^-@^0(%rbp),%rsi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          puttmp(ep^.r1a); deltre(ep); 
          botstk 
        end;

        {mst}
        11: begin read(prd,p); labelsearch(def, val, lclspc); labelsearch(def2, val2, sp2);
          write(prr,p:1, ' l '); writevp(prr, lclspc); write(prr, ' l '); 
          writevp(prr, sp2); writeln(prr);
          if blkstk <> nil then
            if blkstk^.btyp in [btproc, btfunc] then wrtblklab(blkstk);
          frereg := allreg;
          { We limit to the enter instruction }
          if p >= 32 then errorl('Too many nested levels   ');
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { place current ep }
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { place bottom of stack }
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { previous ep }
          wrtins20('enter $1,$0        ', p+1, 0, rgnull, rgnull, nil); { enter frame }
          wrtins20('movq %rsp,%rax     ', 0, 0, rgnull, rgnull, nil); { copy sp }
          { find sp-locals }
          write(prr, '        subq    $'); writevp(prr, lclspc); write(prr, '+'); 
          writevp(prr, blkstk^.tmpnam); writeln(prr, ',%rax');
          wrtins10('1:        ', 0, 0, rgnull, rgnull, lclspc);
          wrtins20('cmpq %rax,%rsp     ', 0, 0, rgnull, rgnull, nil); { check stack is there }
          wrtins10('je 2f     ', 0, 0, rgnull, rgnull, nil); { skip if so }
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { push 0 word }
          wrtins10('jmp 1b    ', 7, 0, rgnull, rgnull, nil); { loop }
          wrtins10('2:        ', 0, 0, rgnull, rgnull, lclspc);
          wrtins20('movq %rsp,^0(%rbp)  ', marksb, 0, rgnull, rgnull, nil);
          wrtins30('andq $0xfffffffffffffff0,%rsp ', 0, 0, rgnull, rgnull, nil); { align stack }
          tmpoff := -(p+1)*ptrsize;
          { note ep is unused at this time }
          botstk
        end;

        {mov}
        55: begin read(prd,q); writeln(prr,q:1); 
          frereg := allreg; popstk(ep); popstk(ep2); dmptre(ep); dmptre(ep2);
          assreg(ep2, frereg, rgrdi, rgnull); assreg(ep, frereg, rgrsi, rgnull);
          genexp(ep2); genexp(ep);
          wrtins20('movq $0,%rcx        ', q, 0, rgnull, rgnull, nil);
          wrtins10('repnz     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsb     ', 0, 0, rgnull, rgnull, nil);
          deltre(ep); deltre(ep2);
          botstk
        end;

        {dmp}
        117: begin read(prd,q); writeln(prr,q:1); 
          popstk(ep); deltre(ep)
        end;

        {sroi,sroa,sror,srob,sroc,srox}
        3, 75, 76, 78, 79, 196: begin read(prd,q); writeln(prr,q:1);
          frereg := allreg;
          popstk(ep); assreg(ep, frereg, rgnull, rgnull); dmptre(ep); 
          genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          if (op = 78{srob}) or (op = 79){sroc} or (op = 196){srox} then
            wrtins40('movb %1l,globals_start+0(%rip) ', q, 0, ep^.r1, rgnull, nil)
          else if op = 76{sror} then
            wrtins40('movsd %1l,globals_start+0(%rip) ', q, 0, ep^.r1, rgnull, nil)
          else wrtins40('movq %1,globals_start+0(%rip) ', q, 0, ep^.r1, rgnull, nil);
          deltre(ep); 
          botstk 
        end;

        {sros}
        77: begin read(prd,q); writeln(prr,q:1);
          frereg := allreg;
          popstk(ep); assreg(ep, frereg, rgnull, rgnull); dmptre(ep); 
          genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins30('leaq ^-@^0(%rbp),%rsi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
          wrtins40('leaq globals_start+0(%rip),%rdi         ', q, 0, ep^.r1, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          puttmp(ep^.r1a); deltre(ep); 
          botstk 
        end;

        {apc}
        178: begin writeln(prr); 
          frereg := allreg; popstk(ep); popstk(ep2); dmptre(ep2); 
          dmptre(ep); deltre(ep2); deltre(ep); botstk  
        end; 

        {pck, upk}
        63, 64: begin read(prd,q,q1); writeln(prr,q:1, ' ', q1:1); 
          frereg := allreg; popstk(ep);
          popstk(ep2); popstk(ep3); dmptre(ep3); dmptre(ep2); dmptre(ep); 
          deltre(ep); deltre(ep2); deltre(ep3); 
          botstk 
        end;

        {ujp}
        23: begin labelsearch(def, val, sp); write(prr, 'l '); 
          writevp(prr, sp); writeln(prr);
          wrtins10('jmp @     ', 0, 0, rgnull, rgnull, sp);
          botstk
        end;

        {fjp,tjp}
        24,119: begin labelsearch(def, val, sp); write(prr, 'l '); 
          writevp(prr, sp); writeln(prr);
          frereg := allreg; popstk(ep); 
          assreg(ep, frereg, rgnull, rgnull); dmptre(ep); genexp(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('orb %1l,%1l         ', 0, 0, ep^.r1, rgnull, nil);
          if op = 24{fjp} then wrtins10('jz @      ', 0, 0, rgnull, rgnull, sp)
          else {tjp} wrtins10('jnz @     ', 0, 0, rgnull, rgnull, sp);
          deltre(ep); 
          botstk 
        end;

        {xjp}
        25: begin labelsearch(def, val, sp); write(prr, 'l '); 
          writevp(prr, sp); writeln(prr);
          frereg := allreg; popstk(ep); getreg(r1, frereg);
          assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins10('movq %1,%2', 0, 0, ep^.r1, r1, sp);
          wrtins10('salq $2,%1', 0, 0, ep^.r1, rgnull, sp);
          wrtins10('addq %2,%1', 0, 0, ep^.r1, r1, sp);
          wrtins20('leaq @(%rip),%1     ', 0, 0, r1, rgnull, sp);
          wrtins10('addq %2,%1', 0, 0, ep^.r1, r1, sp);
          wrtins10('jmp *%1   ', 0, 0, ep^.r1, rgnull, sp);
          deltre(ep); 
          botstk 
        end;

        {ipj}
        112: begin read(prd,p); labelsearch(def, val, sp); writeln(prr, p:1);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('movq ^0(%rbp),%rbp    ', -p*ptrsize, 0, rgnull, rgnull, nil);
          wrtins20('movq ^0(%rbp),%rsp    ', marksb, 0, rgnull, rgnull, nil);
          wrtins30('andq $0xfffffffffffffff0,%rsp ', 0, 0, rgnull, rgnull, nil); { align stack }
          wrtins10('jmp @     ', 0, 0, rgnull, rgnull, sp);
          botstk 
        end;

        {vbs}
        92: begin read(prd,q); writeln(prr, q:1); 
          frereg := allreg; popstk(ep); 
          assreg(ep, frereg, rgrdi, rgnull); dmptrel(ep, 19); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('movq %rdi,%rsi         ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsi           ', ep^.q-1, 0, rgnull, rgnull, nil);
          wrtins20('call varenter          ', 0, 0, rgnull, rgnull, nil);
          deltre(ep);
          botstk
        end;

        {vbe}
        96: begin 
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins20('call varexit        ', 0, 0, rgnull, rgnull, nil);
          botstk
        end;

        {ret}
        22: begin writeln(prr);
          frereg := allreg;
          wrtins20('ret       ', 0, 0, rgnull, rgnull, nil);
          botstk
        end;

        {retp}
        14: begin read(prd,q); writeln(prr, q:1);
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins10('leave     ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', marksize, 0, rgnull, rgnull, nil);
          wrtins10('popq %rax  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', q, 0, rgnull, rgnull, nil);
          wrtins10('pushq %rax ', 0, 0, rgnull, rgnull, nil);
          wrtins10('ret        ', 0, 0, rgnull, rgnull, nil);
          writevp(prr, blkstk^.tmpnam); writeln(prr, ' = ', tmpspc:1);
          botstk; deltmp
        end;

        {reti,reta,retx,retc,retb}
        128,132,204,130,131: begin read(prd,q); writeln(prr, q:1);
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins10('leave     ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', marksize, 0, rgnull, rgnull, nil);
          wrtins10('popq %rbx  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', q, 0, rgnull, rgnull, nil);
          wrtins10('popq %rax  ', 0, 0, rgnull, rgnull, nil);
          if (op = 130{retc}) or (op = 131{retb}) then
            wrtins20('andq $0,%rax        ', 255, 0, rgnull, rgnull, nil);
          wrtins10('pushq %rbx ', 0, 0, rgnull, rgnull, nil);
          wrtins10('ret        ', 0, 0, rgnull, rgnull, nil);
          writevp(prr, blkstk^.tmpnam); writeln(prr, ' = ', tmpspc:1);
          botstk; deltmp
        end;

        {retr}
        129: begin read(prd,q); writeln(prr, q:1);
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins10('leave     ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', marksize, 0, rgnull, rgnull, nil);
          wrtins10('popq %rbx  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', q, 0, rgnull, rgnull, nil);
          wrtins20('movsd (%rsp),%xmm0  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', realsize, 0, rgnull, rgnull, nil);
          wrtins10('pushq %rbx ', 0, 0, rgnull, rgnull, nil);
          wrtins10('ret        ', 0, 0, rgnull, rgnull, nil);
          writevp(prr, blkstk^.tmpnam); writeln(prr, ' = ', tmpspc:1);
          botstk; deltmp
        end;

        {vip,vis}
        133, 122: begin writeln(prr); { ??? fill me in }
          frereg := allreg;
          botstk
        end;

        {vin}
        226: begin writeln(prr); { ??? fill me in }
          frereg := allreg;
          botstk
        end;

        {stoi,stoa,stor,stos,stob,stoc,stox}
        6, 80, 81, 82, 83, 84, 197: begin writeln(prr); 
          frereg := allreg; popstk(ep2); popstk(ep);
          if op = 81{stor} then getfreg(ep2^.r1, frereg) else getreg(ep2^.r1, frereg);
          assreg(ep2, frereg, ep2^.r1,  rgnull);
          if op = 82{stos} then assreg(ep, frereg, rgrdi, rgnull)
          else assreg(ep, frereg, rgnull, rgnull);
          dmptre(ep); dmptre(ep2);
          genexp(ep); genexp(ep2);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          case op of
            6{stoi},80{stoa}: wrtins20('movq %1,(%2)        ', q, 0, ep2^.r1, ep^.r1, nil);
            81{stor}: wrtins20('movsd %1,(%2)       ', q, 0, ep2^.r1, ep^.r1, nil);
            82{stos}: begin
              wrtins30('leaq ^-@^0(%rbp),%rdi         ', ep^.r1a, 0, rgnull, rgnull, lclspc);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              puttmp(ep^.r1a)
            end;
            83{stob},84{stoc},197{stox}:
              wrtins20('movb %1l,(%2)        ', q, 0, ep2^.r1, ep^.r1, nil)
          end;
          deltre(ep); deltre(ep2);
          botstk
        end;

        {stp}
        58:; { unused }

        {inv} { ??? fill me in }
        189: begin writeln(prr); 
          frereg := allreg; popstk(ep); dmptre(ep); deltre(ep); 
          botstk
        end;

        61 {ujc}: begin writeln(prr);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins30('movq $CaseValueNotFound,%rdi  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
          botstk
        end;
     
        {cjp}
        8: begin read(prd,q,q1); labelsearch(def, val, sp); 
          write(prr,q:1, ' ', q1:1, ' l '); writevp(prr, sp); writeln(prr);
          frereg := allreg; popstk(ep); 
          assreg(ep, frereg, rgnull, rgnull);
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins10('cmpq $0,%1', q, 0, ep^.r1, rgnull, nil);
          wrtins10('jl 1f     ', 0, 0, rgnull, rgnull, sp);
          wrtins10('cmpq $0,%1', q1, 0, ep^.r1, rgnull, nil);
          wrtins10('jle @     ', 0, 0, rgnull, rgnull, sp);
          wrtins10('1:        ', 0, 0, rgnull, rgnull, sp);
          pshstk(ep)
        end;

        {wbs}
        243: ;
        
        {wbe}
        244: ;

        { these are all Pascaline unimplemented }

        {suv}
        91,
        {cal}
        21,
        {bge}
        207,
        {ede}
        208,
        {mse}
        209,
        {apc}
        210,
        {cxs}
        211,
        {cxc}
        212,
        {lft} 
        213,
        {max} 
        214,
        {equv} 
        215,
        {neqv} 
        216,
        {lesv} 
        217, 
        {grtv} 
        218, 
        {leqv} 
        219, 
        {geqv} 
        220, 
        {vdp} 
        221, 
        {spc} 
        222, 
        {ccs} 
        223, 
        {scp} 
        224, 
        {ldp} 
        225, 
        {vdd} 
        227, 
        {ltci} 
        228, 
        {ltcr} 
        229,
        {ltcs} 
        230, 
        {ltcb} 
        231, 
        {ltcc} 
        232, 
        {ltcx} 
        233, 
        {lto} 
        234, 
        {stom} 
        235, 
        {rets} 
        236, 
        {retm} 
        237, 
        {ctb} 
        238, 
        {cpp} 
        239, 
        {cpr} 
        240, 
        {lsa} 
        241: errorl('Intermediate unimplement ');

      end; (*case*)

      getlin; { next intermediate line }

   end; (*assemble*)

   procedure gencst;
   var r: record case boolean of

          true:  (s: settype);
          false: (b: packed array [1..setsize] of byte);

       end;
       i: 1..setsize;
   begin
     while csttbl <> nil do begin
       case csttbl^.ct of
         cstr: begin writeln(prr, 'string', csttbl^.strn:1, ':');
           write(prr, '        .string "');
           writev(prr, csttbl^.str, csttbl^.strl);
           writeln(prr, '"') end;
         creal: begin writeln(prr, 'real', csttbl^.realn:1, ':');
           writeln(prr, '        .double ', csttbl^.r) end;
         cset: begin writeln(prr, 'set', csttbl^.setn:1, ':');
           write(prr, '        .byte   ');
           r.s := csttbl^.s;
           for i := 1 to setsize do begin
             write(prr, r.b[i]:1); if i < setsize then write(prr, ',') 
           end;
           writeln(prr)
         end;
       end;
       csttbl := csttbl^.next
     end
   end;

begin (*xlate*)

   init;
   writeln(prr, '# Header file locations');
   writeln(prr, 'inputoff = 0');
   writeln(prr, 'outputoff = 2');
   writeln(prr, 'erroroff = 4');
   writeln(prr, 'listoff = 6');
   writeln(prr, 'commandoff = 8');
   writeln(prr);
   writeln(prr, '# Logical file numbers for header files');
   writeln(prr, 'inputfn = 1');
   writeln(prr, 'outputfn = 2');
   writeln(prr, 'errorfn = 3');
   writeln(prr, 'listfn = 4');
   writeln(prr, 'commandfn = 5');
   writeln(prr);
   errorcode;
   writeln(prr, '        .text');
   writeln(prr, '#');
   writeln(prr, '# Code section');
   writeln(prr, '#');
   generate;
   writeln(prr, '#');
   writeln(prr, '# Constants section');
   writeln(prr, '#');
   gencst;

   writeln(prr, '        .bss');
   writeln(prr, '#');
   writeln(prr, '# Globals section');
   writeln(prr, '#');
   writeln(prr, 'globals_start:');
   writeln(prr, '        .zero ', gblsiz:1);

   if dodmplab then dmplabs { Debug: dump label definitions }

end; (*load*)

procedure fndpow(var m: integer; p: integer; var d: integer);
begin
  m := 1; d := 1;
  while m < maxint div p do begin m := m*p; d := d+1 end
end;

begin (* main *)

  { Suppress unreferenced errors. }
  if adral = 0 then;
  if adral = 0 then;     
  if boolal = 0 then;    
  if charmax = 0 then;   
  if charal = 0 then;     
  if codemax = 0 then;    
  if filesize = 0 then;   
  if intdig = 0 then;     
  if ordminchar = 0 then; 
  if ordmaxchar = 0 then; 
  if stackelsize = 0 then; 

  csttbl := nil; strnum := 0; realnum := 0; setnum := 0; gblsiz := 0; 
  parlvl := maxint;

  for c1 := 'a' to 'z' do option[c1] := false;

  { preset options }
  dochkovf := true;  { check arithmetic overflow }
  dodmplab := false; { dump label definitions }
  dotrcrot := false; { trace routine executions }
  dotrcins := false; { trace instruction executions }
  dosrclin := true;  { add source line sets to code }
  dotrcsrc := false; { trace source line executions (requires dosrclin) }
  dorecycl := true;  { obey heap space recycle requests }
  dochkrpt := false;  { check reuse of freed entry (automatically) }
  donorecpar := false; { check reuse, but leave whole block unused }
  dochkdef := true;  { check undefined accesses }
  iso7185 := false;  { iso7185 standard mode }
  dodebug := false;  { no debug }
  dodbgflt := false; { no debug on fault }
  dodbgsrc := false; { no source level debug }
  dosrcprf := true;  { do source level profiling }
  dochkcov := false; { don't do code coverage }
  doanalys := false; { don't do analyze mode }
  dodckout := false; { don't output code deck }
  dochkvbk := false; { don't check variable blocks }

  { supress warnings }
  if dochkovf then;
  if dodmplab then;
  if dotrcrot then;
  if dotrcins then;
  if dosrclin then;
  if dotrcsrc then;
  if dorecycl then;
  if dochkrpt then;
  if donorecpar then;
  if dochkdef then;
  if iso7185 then;
  if dodebug then;
  if dodbgflt then;
  if dodbgsrc then;
  if dosrcprf then;
  if dochkcov then;
  if doanalys then;
  if dodckout then;
  if dochkvbk then;

  blkstk := nil; { clear symbols block stack }
  blklst := nil; { clear symbols block discard list }

  { supress warning }
  if blklst = nil then;

  fndpow(maxpow10, 10, decdig);
  fndpow(maxpow16, 16, hexdig);
  fndpow(maxpow8, 8, octdig);
  fndpow(maxpow2, 2, bindig); bindig := bindig+1; { add sign bit }

  write('P6 Pascal AMD64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  writeln;

  rewrite(prr);

  writeln('Generating program');

  writeln(prr, '#');
  write(prr, '# File generated by P6 Pascal AMD64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write(prr, '.x');
  writeln(prr);
  writeln(prr, '#');
  writeln(prr);
  
  xlate; (* assembles and stores code *)

  1 : { abort run }

  writeln;
  writeln('Program generation complete');

end.
