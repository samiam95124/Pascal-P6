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

program pgen(input,output,command);

uses endian,  { endian mode }
     mpb,     { machine parameter block }
     version, { current version number }
     parcmd;  { command line parsing }

label 99;

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

      { internal constants }

      maxstr      = maxint;  { maximum size of addressing for program/var }
      maxdigh     = 6;       { number of digits in hex representation of maxstr }
      maxdigd     = 8;       { number of digits in decimal representation of maxstr }

      codemax     = maxstr;  { set size of code store to maximum possible }

      maxlabel = 30000;      { total possible labels in intermediate }
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
      ReadCharacterMismatch              = 88;
      exceptiontop                       = 88;

      { Exceptions that can't be caught.
        Note that these don't have associated exception variables. }

      UndefinedLocationAccess            = 89;
      FunctionNotImplemented             = 90;
      InvalidInISO7185Mode               = 91;
      HeapFormatInvalid                  = 92;
      DisposeOfUninitalizedPointer       = 93;
      DisposeOfNilPointer                = 94;
      BadPointerValue                    = 95;
      BlockAlreadyFreed                  = 96;
      InvalidStandardProcedureOrFunction = 97;
      InvalidInstruction                 = 98;
      NewDisposeTagsMismatch             = 99;
      PCOutOfRange                       = 100;
      StoreOverflow                      = 101;
      StackBalance                       = 102;
      SetInclusion                       = 103;
      UninitializedPointer               = 104;
      DereferenceOfNilPointer            = 105;
      PointerUsedAfterDispose            = 106;
      VariantNotActive                   = 107;
      InvalidCase                        = 108;
      SystemError                        = 109;
      ChangeToAllocatedTagfield          = 110;
      UnhandledException                 = 111;
      ProgramCodeAssertion               = 112;
      VarListEmpty                       = 113;
      ChangeToVarReferencedVariant       = 114;
      DisposeOfVarReferencedBlock        = 115;
      VarReferencedFileBufferModified    = 116;
      ContainerMismatch                  = 117;
      InvalidContainerLevel              = 118;
      DisposeOfWithReferencedBlock       = 119;
      WithBaseListEmpty                  = 120;
      ExternalsNotEnabled                = 121;
      privexceptiontop                   = 121;

      strlen      = 1000;    { longest string length we can buffer }
      maxsp       = 110;     { number of predefined procedures/functions }
      maxins      = 255;     { maximum instruction code, 0-255 or byte }
      maxfil      = 100;     { maximum number of general (temp) files }
      maxalfa     = 10;      { maximum number of characters in alfa type }
      fillen      = 20000;   { maximum length of filenames }
      lablen      = 20000;   { label maximum length }
      varsqt      = 10;      { variable string quanta }
      parfld      = 24;      { field length for intermediate parameters }
      maxtmp      = 20;      { maximum number of template dimensions }
      maxflen     = 200;     { maximum filename length }

      { coder parameters }
      maxreg      = 1000;    { maximum virtual registers to allocate }
      maxphy      = 6;       { maximum physical registers to allocate }
      opcspc      = 8;       { tab spacing on assembly code }
      parspc      = 16;      { tab spacing to parameters }
      cmtspc      = 40;      { tab spacing to comments }

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
      filnam      = packed array [1..fillen] of char; { filename strings }
      labbuf      = packed array [1..lablen] of char; { label buffer }
      strbuf      = packed array [1..strlen] of char;
      filext      = packed array [1..4] of char; { filename extension }
      ctype = (cstr, creal, cset, ctmp, ctab, cint, cchr, cbol, cvalx);
      cstptr = ^cstrec; { pointer to string constant entry table }
      cstrec = record 
        next: cstptr; 
        case ct: ctype of
            cstr:  (str: pstring; strl: integer; strn: integer);
            creal: (r:   real; realn: integer);
            cset:  (s:   settype; setn: integer);
            ctmp:  (ta:  array [1..maxtmp] of integer; tsize: integer; tn: integer);
            ctab:  (tb:  cstptr; csize: integer; cn: integer);
            cint:  (i:   integer; intn: integer);
            cchr:  (c:   integer; chrn: integer);
            cbol:  (b:   integer; boln: integer);
            cvalx: (x:   integer; valxn: integer);
      end;
      psymbol     = ^symbol;
      symbol      = record
                      next:   psymbol; { next list symbol }
                      name:   pstring; { name }
                      styp:   (stglobal, stlocal, stparam); { area type }
                      off:    address; { offset address }
                      digest: pstring; { type digest }
                    end;
      pblock       = ^block;
      block        = record
                       next:    pblock; { next list block }
                       incnxt:  pblock; { included blocks list }
                       parent:  pblock; { the parent of this block }
                       name:    pstring; { name of block, including type }
                       bname:   pstring; { name of block, not including type }
                       tmpnam:  pstring; { name of temp allocation space }
                       short:   boolean; { there is a short name }
                       symbols: psymbol; { symbol list for block }
                       { block type }
                       btyp:    (btprog, btmod, btproc, btfunc);
                       en:      integer; { encounter number }
                       lvl:     integer; { level of block }
                     end;

var   op : instyp; p : lvltyp; q : address;  (*instruction register*)
      q1, q2, q3, q4 : address; { extra parameters }
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
      dodbgchk: boolean; { do debug checks }
      doechlin: boolean; { echo input command line }

      { other flags }
      iso7185: boolean; { iso7185 standard flag }

      prd,prr     : text; (*prd for read only, prr for write only *)

      instr       : array[instyp] of alfa; (* mnemonic instruction codes *)
      insr        : array[instyp] of integer; { number of stack words in result }
      insf        : array[instyp] of boolean; { result is real }
      inss        : array[instyp] of boolean; { result is set }
      sptable     : array[sctyp] of alfa; (*standard functions and procedures*)
      spfunc      : array[sctyp] of boolean; (*standard function or procedure
                                                  is function*)
      sppar       : array[sctyp] of integer; (*standard functions and procedures
                                                  number of parameters*)
      spkeep      : array[sctyp] of boolean; { keep the file parameter }
      csttbl      : cstptr; { constants table }
      strnum      : integer; { string constant label count }
      realnum     : integer; { real constants label count }
      setnum      : integer; { set constants label count }
      blkstk      : pblock; { stack of symbol blocks }
      blklst      : pblock; { discard list of symbols blocks }
      level       : integer; { level count of active blocks }
      modnam      : pstring; { block name }
      prdval      : boolean; { input source file parsed }
      prrval      : boolean; { output source file parsed }
      srcfil(maxflen): string; { name of input source file }

      (*locally used for interpreting one instruction*)
      ad          : address;
      c1          : char;
      oi          : 1..maxopt;

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

function digits(i: integer): integer;
var dc: integer;
begin
  dc := 0; if i < 0 then begin i := -i; dc := dc+1 end;
  if i = 0 then dc := 1
  else while i > 0 do begin i := i div 10; dc := dc+1 end;
  digits := dc
end;

procedure lftjst(fl: integer);
begin
  if fl > 0 then write(prr, ' ':fl)
end;

{*******************************************************************************

Pascaline string handling

*******************************************************************************}

{ find length of right padded string }
function lenp(view s: string): integer;
var i: integer;
begin
  if max(s) = 0 then lenp := 0 else begin
    i := max(s);
    while (s[i] = ' ') and (i > 1) do i := i-1;
    if s[i] <> ' ' then i := i+1;
    lenp := i-1
  end
end;

{ make string from string substring }
function strl(view s: string; l: integer): pstring;
var p: pstring; i: integer;
begin
  new(p, l);
  for i := 1 to l do p^[i] := s[i];
  strl := p
end;

{ make string from string }
function str(view s: string): pstring;
var p: pstring;
begin
  str := strl(s, max(s))
end;

{ make string from padded string }
function strp(view s: string): pstring;
var p: pstring; l: integer;
begin
  strp := strl(s, lenp(s))
end;

{ write string with escaped quotes to file }
procedure writeq(var f: text; view s: string; fl: integer);
var i: integer; c: char;
begin i := 1;
  while fl > 0 do begin
    c := ' '; if i <= max(s) then begin c := s[i]; i := i+1 end;
    if (c = '"') or (c = '\\') then write(f, '\\', c)
    else if c < ' ' then write(f, '\\x', ord(c)$:#2)
    else write(f, c);
    fl := fl-1
  end
end;

{ concatenate strings }
procedure cat(var d: pstring; view s: string);
var p: pstring; i,x: integer; ld, ls: integer;
begin
  ld := max(d^);
  ls := max(s);
  new(p, ld+ls);
  for i := 1 to ld do p^[i] := d^[i];
  x := ld+1;
  for i := 1 to ls do begin p^[x] := s[i]; x := x+1 end;
  dispose(d);
  d := p
end;

{ match padded strings }
function matchp(view a, b: string): boolean;
var la, lb, i: integer;
begin
  la := lenp(a); lb := lenp(b); matchp := true;
  if la <> lb then matchp := false
  else for i := 1 to la do if a[i] <> b[i] then matchp := false
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

{ Language extension routines }

{ support I/O errors from extension library }

procedure errore(e: integer);
begin writeln; write('*** I/O error: ');
  case e of
    FileDeleteFail:                     writeln('File delete fail');
    FileNameChangeFail:                 writeln('File name change fail');
    CommandLineTooLong:                 writeln('Command line too long');
    FunctionNotImplemented:             writeln('Function not implemented');
  end;
  goto 99
end;

(*--------------------------------------------------------------------*)

{ translate intermediate file }

procedure xlate;
    const
      maxintreg = 14; { maximum number of assignable integer reg }
      maxfltreg = 16; { maximum number of assignable float reg }
      maxintparreg = 6; { maximum number of integer/pointer parameter regs }
      maxfltparreg = 8; { maximum number of floating parameter regs }

   type  labelst  = (entered,defined); (*label situation*)
         labelrg  = 0..maxlabel;       (*label range*)
         labelrec = record
                          val: address;
                          st: labelst;
                          ref: pstring;
                          blk: pblock;
                    end;
         flabelp = ^flabel;
         flabel = record
                       next: flabelp;
                       val: address;
                       ref: pstring
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
                    p:   lvltyp; { p parameter } 
                    q, q1, q2, q3, q4: address; { q parameters values }
                    qs, qs1, qs2: pstring; { q parameters symbols }
                    r1, r2, r3: reg; { result registers }
                    t1, t2, t3: reg; { temporary registers }
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
                    fn: pstring; { function call name }
                    lb: pstring; { label for sfr }
                    lt: pstring; { label for table }
                    fl: pstring; { far label (where needed) }
                    pn: integer; { number of parameters for procedure/function }
                    rc: integer; { return type code 0=integer, 1=real, 2=set }
                    blk: pblock; { block called for cup }
                    sline: integer; { source line }
                    iline: integer; { intermediate line }
                  end;
         { temp entries for sets }
         tmpptr = ^tmpety;
         tmpety = record
           next: tmpptr; { next set temp in line }
           occu: boolean; { occupied status }
           off: address; { stack offset }
           len: address { length }
         end;

   var  word : array[alfainx] of char; ch  : char;
        labeltab: array[labelrg] of labelrec;
        labelvalue: address;
        sline: integer; { line number of Pascal source file }
        iline: integer; { line number of intermediate file }
        sn: labbuf;
        snl: 1..lablen;
        flablst: flabelp; { list of far labels }
        estack, efree: expptr; { expression stack }
        jmpstr: expptr; { unresolved jump cache }
        frereg, allreg: regset;
        intassord: array [1..maxintreg] of reg; { integer register assignment order }
        fltassord: array [1..maxfltreg] of reg; { floating point register assignment order }
        stacklvl: integer;
        parreg: array [1..7] of reg; { parameter registers }
        parregf: array [1..8] of reg; { floating point parameter registers }
        expsn: integer; { expression entries sn }
        tmpoff: address; { starting address of set temps offset in stack }
        tmpspc: address; { size of temps area }
        tmplst: tmpptr; { list of active temps }
        tmpfre: tmpptr; { free temp entries }
        lclspc: pstring; { label for locals space }

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
         instr[  0]:='lodi      '; insr[  0] := 1; insf[  0] := false; inss[  0] := false;
         instr[  1]:='ldoi      '; insr[  1] := 1; insf[  1] := false; inss[  1] := false;
         instr[  2]:='stri      '; insr[  2] := 0; insf[  2] := false; inss[  2] := false;
         instr[  3]:='sroi      '; insr[  3] := 0; insf[  3] := false; inss[  3] := false;
         instr[  4]:='lda       '; insr[  4] := 1; insf[  4] := false; inss[  4] := false;
         instr[  5]:='lao       '; insr[  5] := 1; insf[  5] := false; inss[  5] := false;
         instr[  6]:='stoi      '; insr[  6] := 0; insf[  6] := false; inss[  6] := false;
         instr[  7]:='ldcs      '; insr[  7] := 1; insf[  7] := false; inss[  7] := true;
         instr[  8]:='cjp       '; insr[  8] := 0; insf[  8] := false; inss[  8] := false;
         instr[  9]:='indi      '; insr[  9] := 1; insf[  9] := false; inss[  9] := false;
         instr[ 10]:='inci      '; insr[ 10] := 1; insf[ 10] := false; inss[ 10] := false;
         instr[ 11]:='mst       '; insr[ 11] := 0; insf[ 11] := false; inss[ 11] := false;
         instr[ 12]:='cup       '; insr[ 12] := 0; insf[ 12] := false; inss[ 12] := false;
         instr[ 13]:='rip       '; insr[ 13] := 0; insf[ 13] := false; inss[ 13] := false;
         instr[ 14]:='retp      '; insr[ 14] := 0; insf[ 14] := false; inss[ 14] := false;
         instr[ 15]:='csp       '; insr[ 15] := 0; insf[ 15] := false; inss[ 15] := false;
         instr[ 16]:='ixa       '; insr[ 16] := 1; insf[ 16] := false; inss[ 16] := false;
         instr[ 17]:='equa      '; insr[ 17] := 1; insf[ 17] := false; inss[ 17] := false;
         instr[ 18]:='neqa      '; insr[ 18] := 1; insf[ 18] := false; inss[ 18] := false;
         instr[ 19]:='brk*      '; insr[ 19] := 0; insf[ 19] := false; inss[ 19] := false;
         instr[ 20]:='lnp*      '; insr[ 20] := 0; insf[ 20] := false; inss[ 20] := false;
         instr[ 21]:='cal       '; insr[ 21] := 1; insf[ 21] := false; inss[ 21] := false;
         instr[ 22]:='ret       '; insr[ 22] := 0; insf[ 22] := false; inss[ 22] := false;
         instr[ 23]:='ujp       '; insr[ 23] := 0; insf[ 23] := false; inss[ 23] := false;
         instr[ 24]:='fjp       '; insr[ 24] := 0; insf[ 24] := false; inss[ 24] := false;
         instr[ 25]:='xjp       '; insr[ 25] := 0; insf[ 25] := false; inss[ 25] := false;
         instr[ 26]:='chki      '; insr[ 26] := 1; insf[ 26] := false; inss[ 26] := false;
         instr[ 27]:='cuv       '; insr[ 27] := 0; insf[ 27] := false; inss[ 27] := false;
         instr[ 28]:='adi       '; insr[ 28] := 1; insf[ 28] := false; inss[ 28] := false;
         instr[ 29]:='adr       '; insr[ 29] := 1; insf[ 29] := true;  inss[ 29] := false; 
         instr[ 30]:='sbi       '; insr[ 30] := 1; insf[ 30] := false; inss[ 30] := false;
         instr[ 31]:='sbr       '; insr[ 31] := 1; insf[ 31] := true;  inss[ 31] := false; 
         instr[ 32]:='sgs       '; insr[ 32] := 1; insf[ 32] := false; inss[ 32] := true;
         instr[ 33]:='flt       '; insr[ 33] := 1; insf[ 33] := true; inss[ 33] := false;
         instr[ 34]:='flo       '; insr[ 34] := 2; insf[ 34] := true; inss[ 34] := false;
         instr[ 35]:='trc       '; insr[ 35] := 1; insf[ 35] := false; inss[ 35] := false;
         instr[ 36]:='ngi       '; insr[ 36] := 1; insf[ 36] := false; inss[ 36] := false;
         instr[ 37]:='ngr       '; insr[ 37] := 1; insf[ 37] := true;  inss[ 37] := false; 
         instr[ 38]:='sqi       '; insr[ 38] := 1; insf[ 38] := false; inss[ 38] := false;
         instr[ 39]:='sqr       '; insr[ 39] := 1; insf[ 39] := true;  inss[ 39] := false; 
         instr[ 40]:='abi       '; insr[ 40] := 1; insf[ 40] := false; inss[ 40] := false;
         instr[ 41]:='abr       '; insr[ 41] := 1; insf[ 41] := true;  inss[ 41] := false; 
         instr[ 42]:='notb      '; insr[ 42] := 1; insf[ 42] := false; inss[ 42] := false;
         instr[ 43]:='and       '; insr[ 43] := 1; insf[ 43] := false; inss[ 43] := false;
         instr[ 44]:='ior       '; insr[ 44] := 1; insf[ 44] := false; inss[ 44] := false;
         instr[ 45]:='dif       '; insr[ 45] := 1; insf[ 45] := false; inss[ 45] := true;
         instr[ 46]:='int       '; insr[ 46] := 1; insf[ 46] := false; inss[ 46] := true;
         instr[ 47]:='uni       '; insr[ 47] := 1; insf[ 47] := false; inss[ 47] := true;
         instr[ 48]:='inn       '; insr[ 48] := 1; insf[ 48] := false; inss[ 48] := false;
         instr[ 49]:='mod       '; insr[ 49] := 1; insf[ 49] := false; inss[ 49] := false;
         instr[ 50]:='odd       '; insr[ 50] := 1; insf[ 50] := false; inss[ 50] := false;
         instr[ 51]:='mpi       '; insr[ 51] := 1; insf[ 51] := false; inss[ 51] := false;
         instr[ 52]:='mpr       '; insr[ 52] := 1; insf[ 52] := true;  inss[ 52] := false; 
         instr[ 53]:='dvi       '; insr[ 53] := 1; insf[ 53] := false; inss[ 53] := false;
         instr[ 54]:='dvr       '; insr[ 54] := 1; insf[ 54] := true;  inss[ 54] := false; 
         instr[ 55]:='mov       '; insr[ 55] := 0; insf[ 55] := false; inss[ 55] := false;
         instr[ 56]:='lca       '; insr[ 56] := 1; insf[ 56] := false; inss[ 56] := false;
         instr[ 57]:='deci      '; insr[ 57] := 1; insf[ 57] := false; inss[ 57] := false;
         instr[ 58]:='stp*      '; insr[ 58] := 0; insf[ 58] := false; inss[ 58] := false;
         instr[ 59]:='ordi      '; insr[ 59] := 1; insf[ 59] := false; inss[ 59] := false;
         instr[ 60]:='chr       '; insr[ 60] := 1; insf[ 60] := false; inss[ 60] := false;
         instr[ 61]:='ujc       '; insr[ 61] := 0; insf[ 61] := false; inss[ 61] := false;
         instr[ 62]:='rnd       '; insr[ 62] := 1; insf[ 62] := false; inss[ 62] := false;
         instr[ 63]:='pck       '; insr[ 63] := 0; insf[ 63] := false; inss[ 63] := false;
         instr[ 64]:='upk       '; insr[ 64] := 0; insf[ 64] := false; inss[ 64] := false;
         instr[ 65]:='ldoa      '; insr[ 65] := 1; insf[ 65] := false; inss[ 65] := false;
         instr[ 66]:='ldor      '; insr[ 66] := 1; insf[ 66] := true;  inss[ 66] := false; 
         instr[ 67]:='ldos      '; insr[ 67] := 1; insf[ 67] := false; inss[ 67] := true;
         instr[ 68]:='ldob      '; insr[ 68] := 1; insf[ 68] := false; inss[ 68] := false;
         instr[ 69]:='ldoc      '; insr[ 69] := 1; insf[ 69] := false; inss[ 69] := false;
         instr[ 70]:='stra      '; insr[ 70] := 0; insf[ 70] := false; inss[ 70] := false;
         instr[ 71]:='strr      '; insr[ 71] := 0; insf[ 71] := false; inss[ 71] := false;
         instr[ 72]:='strs      '; insr[ 72] := 0; insf[ 72] := false; inss[ 72] := false;
         instr[ 73]:='strb      '; insr[ 73] := 0; insf[ 73] := false; inss[ 73] := false;
         instr[ 74]:='strc      '; insr[ 74] := 0; insf[ 74] := false; inss[ 74] := false;
         instr[ 75]:='sroa      '; insr[ 75] := 0; insf[ 75] := false; inss[ 75] := false;
         instr[ 76]:='sror      '; insr[ 76] := 0; insf[ 76] := true;  inss[ 76] := false; 
         instr[ 77]:='sros      '; insr[ 77] := 0; insf[ 77] := false; inss[ 77] := false;
         instr[ 78]:='srob      '; insr[ 78] := 0; insf[ 78] := false; inss[ 78] := false;
         instr[ 79]:='sroc      '; insr[ 79] := 0; insf[ 79] := false; inss[ 79] := false;
         instr[ 80]:='stoa      '; insr[ 80] := 0; insf[ 80] := false; inss[ 80] := false;
         instr[ 81]:='stor      '; insr[ 81] := 0; insf[ 81] := false; inss[ 81] := false;
         instr[ 82]:='stos      '; insr[ 82] := 0; insf[ 82] := false; inss[ 82] := false;
         instr[ 83]:='stob      '; insr[ 83] := 0; insf[ 83] := false; inss[ 83] := false;
         instr[ 84]:='stoc      '; insr[ 84] := 0; insf[ 84] := false; inss[ 84] := false;
         instr[ 85]:='inda      '; insr[ 85] := 1; insf[ 85] := false; inss[ 85] := false;
         instr[ 86]:='indr      '; insr[ 86] := 1; insf[ 86] := true;  inss[ 86] := false; 
         instr[ 87]:='inds      '; insr[ 87] := 1; insf[ 87] := false; inss[ 87] := true;
         instr[ 88]:='indb      '; insr[ 88] := 1; insf[ 88] := false; inss[ 88] := false;
         instr[ 89]:='indc      '; insr[ 89] := 1; insf[ 89] := false; inss[ 89] := false;
         instr[ 90]:='inca      '; insr[ 90] := 1; insf[ 90] := false; inss[ 90] := false;
         instr[ 91]:='suv       '; insr[ 91] := 0; insf[ 91] := false; inss[ 91] := false;
         instr[ 92]:='vbs       '; insr[ 92] := 0; insf[ 92] := false; inss[ 92] := false;
         instr[ 93]:='incb      '; insr[ 93] := 1; insf[ 93] := false; inss[ 93] := false;
         instr[ 94]:='incc      '; insr[ 94] := 1; insf[ 94] := false; inss[ 94] := false;
         instr[ 95]:='chka      '; insr[ 95] := 1; insf[ 95] := false; inss[ 95] := false;
         instr[ 96]:='vbe       '; insr[ 96] := 0; insf[ 96] := false; inss[ 96] := false;
         instr[ 97]:='chks      '; insr[ 97] := 0; insf[ 97] := false; inss[ 97] := true;
         instr[ 98]:='chkb      '; insr[ 98] := 1; insf[ 98] := false; inss[ 98] := false;
         instr[ 99]:='chkc      '; insr[ 99] := 1; insf[ 99] := false; inss[ 99] := false;
         instr[100]:='cvbi      '; insr[100] := 2; insf[100] := false; inss[100] := false;
         instr[101]:='ivtx      '; insr[101] := 2; insf[101] := false; inss[101] := false;
         instr[102]:='ivtb      '; insr[102] := 2; insf[102] := false; inss[102] := false;
         instr[103]:='decb      '; insr[103] := 1; insf[103] := false; inss[103] := false;
         instr[104]:='decc      '; insr[104] := 1; insf[104] := false; inss[104] := false;
         instr[105]:='loda      '; insr[105] := 1; insf[105] := false; inss[105] := false;
         instr[106]:='lodr      '; insr[106] := 1; insf[106] := true;  inss[106] := false; 
         instr[107]:='lods      '; insr[107] := 1; insf[107] := false; inss[107] := true;
         instr[108]:='lodb      '; insr[108] := 1; insf[108] := false; inss[108] := false;
         instr[109]:='lodc      '; insr[109] := 1; insf[109] := false; inss[109] := false;
         instr[110]:='rgs       '; insr[110] := 1; insf[110] := false; inss[110] := true;
         instr[111]:='ivtc      '; insr[111] := 2; insf[111] := false; inss[111] := false;
         instr[112]:='ipj       '; insr[112] := 0; insf[112] := false; inss[112] := false;
         instr[113]:='cip       '; insr[113] := 0; insf[113] := false; inss[113] := false;
         instr[114]:='lpa       '; insr[114] := 2; insf[114] := false; inss[114] := false;
         instr[115]:='cvbx      '; insr[115] := 2; insf[115] := false; inss[115] := false;
         instr[116]:='cvbb      '; insr[116] := 2; insf[116] := false; inss[116] := false;
         instr[117]:='dmp       '; insr[117] := 0; insf[117] := false; inss[117] := false;
         instr[118]:='swp       '; insr[118] := 2; insf[118] := false; inss[118] := false;
         instr[119]:='tjp       '; insr[119] := 0; insf[119] := false; inss[119] := false;
         instr[120]:='lip       '; insr[120] := 2; insf[120] := false; inss[120] := false;
         instr[121]:='cvbc      '; insr[121] := 2; insf[121] := false; inss[121] := false;
         instr[122]:='vis       '; insr[122] := 1; insf[122] := false; inss[122] := false;
         instr[123]:='ldci      '; insr[123] := 1; insf[123] := false; inss[123] := false;
         instr[124]:='ldcr      '; insr[124] := 1; insf[124] := true;  inss[124] := false; 
         instr[125]:='ldcn      '; insr[125] := 1; insf[125] := false; inss[125] := false;
         instr[126]:='ldcb      '; insr[126] := 1; insf[126] := false; inss[126] := false;
         instr[127]:='ldcc      '; insr[127] := 1; insf[127] := false; inss[127] := false;
         instr[128]:='reti      '; insr[128] := 1; insf[128] := false; inss[128] := false;
         instr[129]:='retr      '; insr[129] := 1; insf[129] := false; inss[129] := false;
         instr[130]:='retc      '; insr[130] := 1; insf[130] := false; inss[130] := false;
         instr[131]:='retb      '; insr[131] := 1; insf[131] := false; inss[131] := false;
         instr[132]:='reta      '; insr[132] := 1; insf[132] := false; inss[132] := false;
         instr[133]:='vip       '; insr[133] := 0; insf[133] := false; inss[133] := false;
         instr[134]:='ordb      '; insr[134] := 1; insf[134] := false; inss[134] := false;
         instr[135]:='lcp       '; insr[135] := 2; insf[135] := false; inss[135] := false;
         instr[136]:='ordc      '; insr[136] := 1; insf[136] := false; inss[136] := false;
         instr[137]:='equi      '; insr[137] := 1; insf[137] := false; inss[137] := false;
         instr[138]:='equr      '; insr[138] := 1; insf[138] := false; inss[138] := false;
         instr[139]:='equb      '; insr[139] := 1; insf[139] := false; inss[139] := false;
         instr[140]:='equs      '; insr[140] := 1; insf[140] := false; inss[140] := false;
         instr[141]:='equc      '; insr[141] := 1; insf[141] := false; inss[141] := false;
         instr[142]:='equm      '; insr[142] := 1; insf[142] := false; inss[142] := false;
         instr[143]:='neqi      '; insr[143] := 1; insf[143] := false; inss[143] := false;
         instr[144]:='neqr      '; insr[144] := 1; insf[144] := false; inss[144] := false;
         instr[145]:='neqb      '; insr[145] := 1; insf[145] := false; inss[145] := false;
         instr[146]:='neqs      '; insr[146] := 1; insf[146] := false; inss[146] := false;
         instr[147]:='neqc      '; insr[147] := 1; insf[147] := false; inss[147] := false;
         instr[148]:='neqm      '; insr[148] := 1; insf[148] := false; inss[148] := false;
         instr[149]:='geqi      '; insr[149] := 1; insf[149] := false; inss[149] := false;
         instr[150]:='geqr      '; insr[150] := 1; insf[150] := false; inss[150] := false;
         instr[151]:='geqb      '; insr[151] := 1; insf[151] := false; inss[151] := false;
         instr[152]:='geqs      '; insr[152] := 1; insf[152] := false; inss[152] := false;
         instr[153]:='geqc      '; insr[153] := 1; insf[153] := false; inss[153] := false;
         instr[154]:='geqm      '; insr[154] := 1; insf[154] := false; inss[154] := false;
         instr[155]:='grti      '; insr[155] := 1; insf[155] := false; inss[155] := false;
         instr[156]:='grtr      '; insr[156] := 1; insf[156] := false; inss[156] := false;
         instr[157]:='grtb      '; insr[157] := 1; insf[157] := false; inss[157] := false;
         instr[158]:='grts      '; insr[158] := 1; insf[158] := false; inss[158] := false;
         instr[159]:='grtc      '; insr[159] := 1; insf[159] := false; inss[159] := false;
         instr[160]:='grtm      '; insr[160] := 1; insf[160] := false; inss[160] := false;
         instr[161]:='leqi      '; insr[161] := 1; insf[161] := false; inss[161] := false;
         instr[162]:='leqr      '; insr[162] := 1; insf[162] := false; inss[162] := false;
         instr[163]:='leqb      '; insr[163] := 1; insf[163] := false; inss[163] := false;
         instr[164]:='leqs      '; insr[164] := 1; insf[164] := false; inss[164] := false;
         instr[165]:='leqc      '; insr[165] := 1; insf[165] := false; inss[165] := false;
         instr[166]:='leqm      '; insr[166] := 1; insf[166] := false; inss[166] := false;
         instr[167]:='lesi      '; insr[167] := 1; insf[167] := false; inss[167] := false;
         instr[168]:='lesr      '; insr[168] := 1; insf[168] := false; inss[168] := false;
         instr[169]:='lesb      '; insr[169] := 1; insf[169] := false; inss[169] := false;
         instr[170]:='less      '; insr[170] := 1; insf[170] := false; inss[170] := false;
         instr[171]:='lesc      '; insr[171] := 1; insf[171] := false; inss[171] := false;
         instr[172]:='lesm      '; insr[172] := 1; insf[172] := false; inss[172] := false;
         instr[173]:='---       '; insr[173] := 0; insf[173] := false; inss[173] := false;
         instr[174]:='mrkl*     '; insr[174] := 0; insf[174] := false; inss[174] := false;
         instr[175]:='ckvi      '; insr[175] := 2; insf[175] := false; inss[175] := false;
         instr[176]:='cps       '; insr[176] := 3; insf[176] := false; inss[176] := false;
         instr[177]:='cpc       '; insr[177] := 3; insf[177] := false; inss[177] := false;
         instr[178]:='aps       '; insr[178] := 0; insf[178] := false; inss[178] := false;
         instr[179]:='ckvb      '; insr[179] := 2; insf[179] := false; inss[179] := false;
         instr[180]:='ckvc      '; insr[180] := 2; insf[180] := false; inss[180] := false;
         instr[181]:='dupi      '; insr[181] := 1; insf[181] := false; inss[181] := false;
         instr[182]:='dupa      '; insr[182] := 2; insf[182] := false; inss[182] := false;
         instr[183]:='dupr      '; insr[183] := 1; insf[183] := true;  inss[183] := false; 
         instr[184]:='dups      '; insr[184] := 1; insf[184] := false; inss[184] := true;
         instr[185]:='dupb      '; insr[185] := 1; insf[185] := false; inss[185] := false;
         instr[186]:='dupc      '; insr[186] := 1; insf[186] := false; inss[186] := false;
         instr[187]:='cks       '; insr[187] := 2; insf[187] := false; inss[187] := false;
         instr[188]:='cke       '; insr[188] := 0; insf[188] := false; inss[188] := false;
         instr[189]:='inv       '; insr[189] := 0; insf[189] := false; inss[189] := false;
         instr[190]:='ckla      '; insr[190] := 1; insf[190] := false; inss[190] := false;
         instr[191]:='cta       '; insr[191] := 2; insf[191] := false; inss[191] := false;
         instr[192]:='ivti      '; insr[192] := 2; insf[192] := false; inss[192] := false;
         instr[193]:='lodx      '; insr[193] := 1; insf[193] := false; inss[193] := false;
         instr[194]:='ldox      '; insr[194] := 1; insf[194] := false; inss[194] := false;
         instr[195]:='strx      '; insr[195] := 0; insf[195] := false; inss[195] := false;
         instr[196]:='srox      '; insr[196] := 1; insf[196] := false; inss[196] := false;
         instr[197]:='stox      '; insr[197] := 0; insf[197] := false; inss[197] := false;
         instr[198]:='indx      '; insr[198] := 1; insf[198] := false; inss[198] := false;
         instr[199]:='chkx      '; insr[199] := 1; insf[199] := false; inss[199] := false;
         instr[200]:='ordx      '; insr[200] := 1; insf[200] := false; inss[200] := false;
         instr[201]:='incx      '; insr[201] := 1; insf[201] := false; inss[201] := false;
         instr[202]:='decx      '; insr[202] := 1; insf[202] := false; inss[202] := false;
         instr[203]:='ckvx      '; insr[203] := 2; insf[203] := false; inss[203] := false;
         instr[204]:='retx      '; insr[204] := 1; insf[204] := false; inss[204] := false;
         instr[205]:='noti      '; insr[205] := 1; insf[205] := false; inss[205] := false;
         instr[206]:='xor       '; insr[206] := 1; insf[206] := false; inss[206] := false;
         instr[207]:='bge       '; insr[207] := 4; insf[207] := false; inss[207] := false;
         instr[208]:='ede       '; insr[208] := 0; insf[208] := false; inss[208] := false;
         instr[209]:='mse       '; insr[209] := 0; insf[209] := false; inss[209] := false;
         instr[210]:='apc       '; insr[210] := 0; insf[210] := false; inss[210] := false;
         instr[211]:='cxs       '; insr[211] := 1; insf[211] := false; inss[211] := false;
         instr[212]:='cxc       '; insr[212] := 2; insf[212] := false; inss[212] := false;
         instr[213]:='lft       '; insr[213] := 2; insf[213] := false; inss[213] := false;
         instr[214]:='max       '; insr[214] := 1; insf[214] := false; inss[214] := false;
         instr[215]:='equv      '; insr[215] := 1; insf[215] := false; inss[215] := false;
         instr[216]:='neqv      '; insr[216] := 1; insf[216] := false; inss[216] := false;
         instr[217]:='lesv      '; insr[217] := 1; insf[217] := false; inss[217] := false;
         instr[218]:='grtv      '; insr[218] := 1; insf[218] := false; inss[218] := false;
         instr[219]:='leqv      '; insr[219] := 1; insf[219] := false; inss[219] := false;
         instr[220]:='geqv      '; insr[220] := 1; insf[220] := false; inss[220] := false;
         instr[221]:='vdp       '; insr[221] := 0; insf[221] := false; inss[221] := false;
         instr[222]:='spc       '; insr[222] := 2; insf[222] := false; inss[222] := false;
         instr[223]:='ccs       '; insr[223] := 2; insf[223] := false; inss[223] := false;
         instr[224]:='scp       '; insr[224] := 0; insf[224] := false; inss[224] := false;
         instr[225]:='ldp       '; insr[225] := 2; insf[225] := false; inss[225] := false;
         instr[226]:='vin       '; insr[226] := 0; insf[226] := false; inss[226] := false;
         instr[227]:='vdd       '; insr[227] := 0; insf[227] := false; inss[227] := false;
         { ltc and lto are aliases to ldo and lao instructions }
         instr[228]:='ltci      '; insr[228] := 1; insf[228] := false; inss[228] := false;
         instr[229]:='ltcr      '; insr[229] := 1; insf[229] := true;  inss[229] := false; 
         instr[230]:='ltcs      '; insr[230] := 1; insf[230] := false; inss[230] := false;
         instr[231]:='ltcb      '; insr[231] := 1; insf[231] := false; inss[231] := false;
         instr[232]:='ltcc      '; insr[232] := 1; insf[232] := false; inss[232] := false;
         instr[233]:='ltcx      '; insr[233] := 1; insf[233] := false; inss[233] := false;
         instr[234]:='lto       '; insr[234] := 1; insf[234] := false; inss[234] := false;
         instr[235]:='stom      '; insr[235] := 0; insf[235] := false; inss[235] := false;
         instr[236]:='rets      '; insr[236] := 1; insf[236] := false; inss[236] := true;
         instr[237]:='retm      '; insr[237] := 1; insf[237] := false; inss[237] := false;
         instr[238]:='ctb       '; insr[238] := 0; insf[238] := false; inss[238] := false;
         instr[239]:='cpp       '; insr[239] := 1; insf[239] := false; inss[239] := false;
         instr[240]:='cpr       '; insr[240] := 1; insf[240] := false; inss[240] := false;
         instr[241]:='lsa       '; insr[241] := 1; insf[241] := false; inss[241] := false;
         instr[242]:='eext*     '; insr[242] := 0; insf[242] := false; inss[242] := false;
         instr[243]:='wbs       '; insr[243] := 1; insf[243] := false; inss[243] := false;
         instr[244]:='wbe       '; insr[244] := 0; insf[244] := false; inss[244] := false;
         instr[245]:='sfr       '; insr[245] := 0; insf[245] := false; inss[245] := false;
         instr[246]:='cuf       '; insr[246] := 0; insf[246] := false; inss[246] := false;
         instr[247]:='cif       '; insr[247] := 0; insf[247] := false; inss[247] := false;
         instr[248]:='mpc       '; insr[248] := 2; insf[248] := false; inss[248] := false;
         instr[249]:='cvf       '; insr[249] := 0; insf[249] := false; inss[249] := false;
         instr[250]:='lsp       '; insr[250] := 2; insf[250] := false; inss[250] := false;
         instr[251]:='cpl       '; insr[251] := 1; insf[251] := false; inss[251] := false;
         instr[252]:='sfs       '; insr[252] := 0; insf[252] := false; inss[252] := false;
         { sev is an alias for stra in pint. It has meaning to pgen. }
         instr[253]:='sev       '; insr[253] := 0; insf[253] := false; inss[253] := false;
         instr[254]:='mdc       '; insr[254] := 1; insf[254] := false; inss[254] := false;

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
         sptable[43]:='fbv       '; spfunc[43]:=false;  sppar[43]:=1; spkeep[43]:=true;
         sptable[44]:='fvb       '; spfunc[44]:=false;  sppar[44]:=2; spkeep[44]:=true;
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
         sptable[82]:='rdx       '; spfunc[82]:=false; sppar[82]:=2; spkeep[82]:=true;
         sptable[83]:='rdxf      '; spfunc[83]:=false; sppar[83]:=3; spkeep[83]:=true;
         sptable[84]:='rxb       '; spfunc[84]:=false; sppar[84]:=4; spkeep[84]:=true;
         sptable[85]:='rxbf      '; spfunc[85]:=false; sppar[85]:=5; spkeep[85]:=true;
         sptable[86]:='rdsc      '; spfunc[86]:=false; sppar[86]:=3; spkeep[86]:=true;
         sptable[87]:='rdih      '; spfunc[87]:=false; sppar[87]:=2; spkeep[87]:=true;
         sptable[88]:='rdio      '; spfunc[88]:=false; sppar[88]:=2; spkeep[88]:=true;    
         sptable[89]:='rdib      '; spfunc[89]:=false; sppar[89]:=2; spkeep[89]:=true;
         sptable[90]:='rifh      '; spfunc[90]:=false; sppar[90]:=3; spkeep[90]:=true;    
         sptable[91]:='rifo      '; spfunc[91]:=false; sppar[91]:=3; spkeep[91]:=true;
         sptable[92]:='rifb      '; spfunc[92]:=false; sppar[92]:=3; spkeep[92]:=true;   
         sptable[93]:='ribh      '; spfunc[93]:=false; sppar[93]:=4; spkeep[93]:=true;
         sptable[94]:='ribo      '; spfunc[94]:=false; sppar[94]:=4; spkeep[94]:=true;    
         sptable[95]:='ribb      '; spfunc[95]:=false; sppar[95]:=4; spkeep[95]:=true;
         sptable[96]:='rbfh      '; spfunc[96]:=false; sppar[96]:=5; spkeep[96]:=true;
         sptable[97]:='rbfo      '; spfunc[97]:=false; sppar[97]:=5; spkeep[97]:=true;
         sptable[98]:='rbfb      '; spfunc[98]:=false; sppar[98]:=5; spkeep[98]:=true;
         sptable[99]:='rdxh      '; spfunc[99]:=false; sppar[99]:=2; spkeep[99]:=true;
         sptable[100]:='rdxo      '; spfunc[100]:=false; sppar[100]:=2; spkeep[100]:=true;
         sptable[101]:='rdxb      '; spfunc[101]:=false; sppar[101]:=2; spkeep[101]:=true;
         sptable[102]:='rxfh      '; spfunc[102]:=false; sppar[102]:=3; spkeep[102]:=true;    
         sptable[103]:='rxfo      '; spfunc[103]:=false; sppar[103]:=3; spkeep[103]:=true;
         sptable[104]:='rxfb      '; spfunc[104]:=false; sppar[104]:=3; spkeep[104]:=true;
         sptable[105] :='rxbh      '; spfunc[105]:=false; sppar[105]:=4; spkeep[105]:=true;
         sptable[106] :='rxbo      '; spfunc[106]:=false; sppar[106]:=4; spkeep[106]:=true;
         sptable[107] :='rxbb      '; spfunc[107]:=false; sppar[107]:=4; spkeep[107]:=true;
         sptable[108] :='rbxh      '; spfunc[108]:=false; sppar[108]:=5; spkeep[108]:=true;    
         sptable[109] :='rbxo      '; spfunc[109]:=false; sppar[109]:=5; spkeep[109]:=true;
         sptable[110] :='rbxb      '; spfunc[110]:=false; sppar[110]:=5; spkeep[110]:=true;
         for i:= 1 to 10 do word[i]:= ' ';
         for i:= 0 to maxlabel do
             with labeltab[i] do begin val:=-1; st:= entered; ref := nil; blk := nil end;

         { !!! remove this next statement for self compile }
         reset(prd);

         sline := 0; { set no line of source }
         iline := 1; { set 1st line of intermediate }
         flablst := nil; { clear far label list }
         expsn := 0; { expression entries serial number start }
         tmpoff := 0; { set temps stack offset }
         tmpspc := 0; { set temps total size }
         tmplst := nil; { clear temps list }
         tmpfre := nil; { clear temps free list }
         estack := nil; stacklvl := 0; efree := nil; jmpstr := nil;
         allreg := [rgrax, rgrbx, rgrcx, rgrdx, rgrsi, rgrdi, 
                    rgr8, rgr9, rgr10, rgr11, rgr12, rgr13, rgr14, rgr15,
                    rgxmm0, rgxmm1, rgxmm2, rgxmm3, rgxmm4, rgxmm5, rgxmm6, 
                    rgxmm7, rgxmm8, rgxmm9, rgxmm10, rgxmm11, rgxmm12, rgxmm13,
                    rgxmm14,
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
         intassord[2] := rgr12; 
         intassord[3] := rgr13; 
         intassord[4] := rgr14; 
         intassord[5] := rgr15;
         intassord[6] := rgrcx; 
         intassord[7] := rgr8;
         intassord[8] := rgr9; 
         intassord[9] := rgr10; 
         intassord[10] := rgr11;
         intassord[11] := rgrsi; 
         intassord[12] := rgrdi; 
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

   procedure errorl(view es: string); (*error in loading*)
   begin writeln;
      writeln('*** Program translation error: [', sline:1, ',', iline:1, '] ', es);
      goto 99
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
   var i, p: integer; digit: boolean; sc: packed array [1..1] of char;
   begin
     labeltab[x].ref :=  strp(sn); cat(labeltab[x].ref, '.');
     p := maxpow10;
     digit := false;
     while p > 0 do begin
       if ((x div p) mod 10 <> 0) or (p = 1) or digit then begin
         sc[1] := chr((x div p) mod 10+ord('0'));
         cat(labeltab[x].ref, sc); 
         digit := true
       end;
       p := p div 10
     end
   end;

   procedure getexp(var ep: expptr);
   begin
     if efree <> nil then begin ep := efree; efree := ep^.next end
     else begin new(ep); expsn := expsn+1; ep^.sn := expsn end;
     ep^.next := nil; ep^.op := op; ep^.p := p; ep^.q := q; ep^.q1 := q1;
     ep^.q2 := q2; ep^.q3 := q3; ep^.q4 := q4;
     ep^.l := nil; ep^.r := nil; ep^.x1 := nil; ep^.sl := nil;
     ep^.cl := nil; ep^.al := nil; ep^.pl := nil; 
     ep^.r1 := rgnull; ep^.r2 := rgnull; ep^.r3 := rgnull; 
     ep^.t1 := rgnull; ep^.t2 := rgnull; ep^.rs := []; 
     ep^.fn := nil; ep^.blk := nil; ep^.lb := nil; ep^.lt := nil; 
     ep^.fl := nil; ep^.pn := 0; ep^.rc := 0; ep^.free := false; 
     ep^.r1a := 0; ep^.r2a := 0; ep^.r3a := 0; ep^.t1a := 0; ep^.t2a := 0;
     ep^.sline := sline; ep^.iline := iline;
   end;
      
   procedure putexp(ep: expptr);
   begin
     if ep^.free then errorl('System fault: dbl free');
     ep^.next := efree; efree := ep; ep^.free := true
   end;

   { search and attach expression stack to jump cache }
   procedure schjmp(view s: string);
   var ep, lp, fp: expptr;
   begin
     ep := jmpstr; lp := nil; fp := nil;
     while ep <> nil do begin
       if ep^.qs^ = s then begin fp := ep; ep := nil end
       else begin lp := ep; ep := ep^.next end
     end;
     if fp <> nil then if fp^.l <> nil then begin
       estack := fp^.l;
       if lp = nil then jmpstr := fp^.next else lp^.next := fp^.next;
       putexp(fp)
     end
   end;

   procedure update(x: labelrg; pc: boolean); (*when a label definition lx is found*)
   begin
      if labeltab[x].st=defined then errorl('duplicated label')
      else begin
        labeltab[x].st := defined;
        labeltab[x].val:= labelvalue;
        putlabel(x);
        labeltab[x].blk := blkstk;
        write(prr, labeltab[x].ref^);
        if pc then writeln(prr, ':') 
        else writeln(prr, ' = ', labeltab[x].val:1);
        schjmp(labeltab[x].ref^);
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
       errorl('Symbols format error');
     while ch in ['a'..'z','A'..'Z','0'..'9','_'] do begin
       if snl >= lablen then errorl('Symbols format error');
       sn[snl] := ch; getnxt; snl := snl+1
     end;
     snl := snl-1
   end;

   procedure getsds;
   var i: 1..lablen;
   begin skpspc; for i := 1 to lablen do sn[i] := ' '; snl := 1;
     if ch = ' ' then errorl('Symbols format error');
     while ch <> ' ' do begin
       if snl >= lablen then errorl('Symbols format error');
       sn[snl] := ch; getnxt; snl := snl+1
     end;
     snl := snl-1
   end;

   procedure coinfunc;
   type inslab = packed array [1..6] of char;
     procedure insert(s: inslab);
     var cl, fl, i, d: integer;
     begin
       cl := 1; while (cl < 6) and (s[cl] <> ' ') do cl := cl+1;
       if (cl > 1) and (s[cl] = ' ') then cl := cl-1;
       fl := 1; while (fl < lablen) and (sn[fl] <> '@') do fl := fl+1;
       if (fl > 1) and (sn[fl] = '@') then fl := fl-1;
       d := abs(cl-fl);
       if cl-fl < 0 then for i := cl+1 to lablen-1 do sn[i] := sn[i+d]
       else if cl-fl > 0 then for i := lablen downto cl+1 do sn[i] := sn[i-d]; 
       for i := 1 to cl do sn[i] := s[i];
       snl := snl+(cl-fl)
     end;
   begin
     case sn[1] of
       '<':
         if sn[2] = '>' then insert('$neq  ')
         else if sn[2] = '=' then insert('$leq  ')
         else insert('$ltn  ');
       '>':
          if sn[2] = '=' then insert('$geq  ')
          else insert('$gtr  ');
       '=':
         if sn[2] = '>' then insert('$geq  ')
         else if sn[2] = '<' then insert('$leq  ')
         else insert('$equ  ');
       '+': insert('$plus ');;
       '-': insert('$minus');
       ':': if sn[2] = '=' then insert('$bcms ');
       '/': insert('$rdiv ');
       '*': insert('$times');
     end
   end;

   procedure cvtsds;
   var i: 1..lablen;
   begin
     i := 1;
     if sn[i] in ['<','>','=','+','-',':','*','/'] then coinfunc;
     { skip to type signature }
     while (i <= snl) and (sn[i] <> '@') do i := i+1;
     while i <= snl do begin
       { translate '@' to '$' for type spagetti demarcate, and 
         characters in the set ['(',')',',',':','-','+'] to '$' 
         because they are invalid }
       if sn[i] = '@' then sn[i] := '$'
       else 
         if sn[i] in ['(',')',',',':','-','+'] then sn[i] := '$';
       i := i+1
     end;
   end;

   procedure parlab(var x: integer; var fl: pstring);
   var i,j: integer; cs: packed array [1..1] of char;
   begin fl := nil;
     getlab; if ch <> '.' then errorl('Symbols format error');
     if prd^ in ['0'..'9'] then begin read(prd, x); getnxt end { near label }
     else begin { far label }
       getnxt; fl := strp(sn); cat(fl, '.');
       getsds; cvtsds;
       for j := 1 to snl do begin cs[1] := sn[j]; cat(fl, cs) end
     end
   end;

   procedure preamble;
   begin
     { see how much of this is really required }
     write(prr, '        .globl  '); write(prr, modnam^); writeln(prr);
     write(prr, '        .type   '); write(prr, modnam^); writeln(prr, ', @function');
     writeln(prr, modnam^, ':');
     writeln(prr, '# Align stack');
     writeln(prr, '        pushq   %rax');
     writeln(prr, '# Set up default files');
     writeln(prr, '        movb    $inputfn,globals_start+inputoff(%rip)');
     writeln(prr, '        movb    $outputfn,globals_start+outputoff(%rip)');
     writeln(prr, '        movb    $prdfn,globals_start+prdoff(%rip)');
     writeln(prr, '        movb    $prrfn,globals_start+prroff(%rip)');
     writeln(prr, '        movb    $errorfn,globals_start+erroroff(%rip)');
     writeln(prr, '        movb    $listfn,globals_start+listoff(%rip)');
     writeln(prr, '        movb    $commandfn,globals_start+commandoff(%rip)');
     writeln(prr, '# Call startup code');
     writeln(prr, '        call    1f');
     writeln(prr, '        popq    %rax');
     writeln(prr, '        sub     %rax,%rax');
     writeln(prr, '        ret');
     writeln(prr, '1:');
   end;

   procedure postamble;
   begin

   end;

   procedure errorcode;
   begin

     writeln(prr, '# Error codes');
     writeln(prr, 'ValueOutOfRange                    = 0');
     writeln(prr, 'ArrayLengthMatch                   = 1');
     writeln(prr, 'CaseValueNotFound                  = 2');
     writeln(prr, 'ZeroDivide                         = 3');
     writeln(prr, 'InvalidOperand                     = 4');
     writeln(prr, 'NilPointerDereference              = 5');
     writeln(prr, 'RealOverflow                       = 6');
     writeln(prr, 'RealUnderflow                      = 7');
     writeln(prr, 'RealProcessingFault                = 8');
     writeln(prr, 'TagValueNotActive                  = 9');
     writeln(prr, 'TooManyFiles                       = 10');
     writeln(prr, 'FileIsOpen                         = 11');
     writeln(prr, 'FileAlreadyNamed                   = 12');
     writeln(prr, 'FileNotOpen                        = 13');
     writeln(prr, 'FileModeIncorrect                  = 14');
     writeln(prr, 'InvalidFieldSpecification          = 15');
     writeln(prr, 'InvalidRealNumber                  = 16');
     writeln(prr, 'InvalidFractionSpecification       = 17');
     writeln(prr, 'InvalidIntegerFormat               = 18');
     writeln(prr, 'IntegerValueOverflow               = 19');
     writeln(prr, 'InvalidRealFormat                  = 20');
     writeln(prr, 'EndOfFile                          = 21');
     writeln(prr, 'InvalidFilePosition                = 22');
     writeln(prr, 'FilenameTooLong                    = 23');
     writeln(prr, 'FileOpenFail                       = 24');
     writeln(prr, 'FileSIzeFail                       = 25');
     writeln(prr, 'FileCloseFail                      = 26');
     writeln(prr, 'FileReadFail                       = 27');
     writeln(prr, 'FileWriteFail                      = 28');
     writeln(prr, 'FilePositionFail                   = 29');
     writeln(prr, 'FileDeleteFail                     = 30');
     writeln(prr, 'FileNameChangeFail                 = 31');
     writeln(prr, 'SpaceAllocateFail                  = 32');
     writeln(prr, 'SpaceReleaseFail                   = 33');
     writeln(prr, 'SpaceAllocateNegative              = 34');
     writeln(prr, 'CannotPerformSpecial               = 35');
     writeln(prr, 'CommandLineTooLong                 = 36');
     writeln(prr, 'ReadPastEOF                        = 37');
     writeln(prr, 'FileTransferLengthZero             = 38');
     writeln(prr, 'FileSizeTooLarge                   = 39');
     writeln(prr, 'FilenameEmpty                      = 40');
     writeln(prr, 'CannotOpenStandard                 = 41');
     writeln(prr, 'TooManyTemporaryFiles              = 42');
     writeln(prr, 'InputBufferOverflow                = 43');
     writeln(prr, 'TooManyThreads                     = 44');
     writeln(prr, 'CannotStartThread                  = 45');
     writeln(prr, 'InvalidThreadHandle                = 46');
     writeln(prr, 'CannotStopThread                   = 47');
     writeln(prr, 'TooManyIntertaskLocks              = 48');
     writeln(prr, 'InvalidLockHandle                  = 49');
     writeln(prr, 'LockSequenceFail                   = 50');
     writeln(prr, 'TooManySignals                     = 51');
     writeln(prr, 'CannotCreateSignal                 = 52');
     writeln(prr, 'InvalidSignalHandle                = 53');
     writeln(prr, 'CannotDeleteSignal                 = 54');
     writeln(prr, 'CannotSendSignal                   = 55');
     writeln(prr, 'WaitForSignalFail                  = 56');
     writeln(prr, 'FieldNotBlank                      = 57');
     writeln(prr, 'ReadOnWriteOnlyFile                = 58');
     writeln(prr, 'WriteOnReadOnlyFile                = 59');
     writeln(prr, 'FileBufferVariableUndefined        = 60');
     writeln(prr, 'NondecimalRadixOfNegative          = 61');
     writeln(prr, 'InvalidArgumentToLn                = 62');
     writeln(prr, 'InvalidArgumentToSqrt              = 63');
     writeln(prr, 'CannotResetOrRewriteStandardFile   = 64');
     writeln(prr, 'CannotResetWriteOnlyFile           = 65');
     writeln(prr, 'CannotRewriteReadOnlyFile          = 66');
     writeln(prr, 'SetElementOutOfRange               = 67');
     writeln(prr, 'RealArgumentTooLarge               = 68');
     writeln(prr, 'BooleanOperatorOfNegative          = 69');
     writeln(prr, 'InvalidDivisorToMod                = 70');
     writeln(prr, 'PackElementsOutOfBounds            = 71');
     writeln(prr, 'UnpackElementsOutOfBounds          = 72');
     writeln(prr, 'CannotResetClosedTempFile          = 73');
     writeln(prr, 'ReadCharacterMismatch              = 74');
     
     writeln(prr, 'UndefinedLocationAccess            = 75');
     writeln(prr, 'FunctionNotImplemented             = 76');
     writeln(prr, 'InvalidInISO7185Mode               = 77');
     writeln(prr, 'HeapFormatInvalid                  = 78');
     writeln(prr, 'DisposeOfUninitalizedPointer       = 79');
     writeln(prr, 'DisposeOfNilPointer                = 80');
     writeln(prr, 'BadPointerValue                    = 81');
     writeln(prr, 'BlockAlreadyFreed                  = 82');
     writeln(prr, 'InvalidStandardProcedureOrFunction = 83');
     writeln(prr, 'InvalidInstruction                 = 84');
     writeln(prr, 'NewDisposeTagsMismatch             = 85');
     writeln(prr, 'PCOutOfRange                       = 86');
     writeln(prr, 'StoreOverflow                      = 87');
     writeln(prr, 'StackBalance                       = 88');
     writeln(prr, 'SetInclusion                       = 89');
     writeln(prr, 'UninitializedPointer               = 90');
     writeln(prr, 'DereferenceOfNilPointer            = 91');
     writeln(prr, 'PointerUsedAfterDispose            = 92');
     writeln(prr, 'VariantNotActive                   = 93');
     writeln(prr, 'InvalidCase                        = 94');
     writeln(prr, 'SystemError                        = 95');
     writeln(prr, 'ChangeToAllocatedTagfield          = 96');
     writeln(prr, 'UnhandledException                 = 97');
     writeln(prr, 'ProgramCodeAssertion               = 98');
     writeln(prr, 'VarListEmpty                       = 99');
     writeln(prr, 'ChangeToVarReferencedVariant       = 100');
     writeln(prr, 'DisposeOfVarReferencedBlock        = 101');
     writeln(prr, 'VarReferencedFileBufferModified    = 102');
     writeln(prr, 'ContainerMismatch                  = 103');
     writeln(prr, 'InvalidContainerLevel              = 104');
     writeln(prr, 'DisposeOfWithReferencedBlock       = 105');
     writeln(prr, 'WithBaseListEmpty                  = 106');
     writeln(prr, 'ExternalsNotEnabled                = 107');
     writeln(prr, 'MasterException                    = 108');
     writeln(prr);

   end;

   procedure mpb;
   begin
     writeln(prr, '# machine parameters');
     writeln(prr, 'lendian     = ', lendian:10,     ' # endian mode');
     writeln(prr, 'intsize     = ', intsize:10,     ' # size of integer');
     writeln(prr, 'intal       = ', intal:10,       ' # alignment of integer');
     writeln(prr, 'intdig      = ', intdig:10,      ' # number of decimal digits in integer');
     writeln(prr, 'inthex      = ', inthex:10,      ' # number of hex digits of integer');
     writeln(prr, 'realsize    = ', realsize:10,    ' # size of real');
     writeln(prr, 'realal      = ', realal:10,      ' # alignment of real');
     writeln(prr, 'charsize    = ', charsize:10,    ' # size of char');
     writeln(prr, 'charal      = ', charal:10,      ' # alignment of char');
     writeln(prr, 'boolsize    = ', boolsize:10,    ' # size of boolean');
     writeln(prr, 'boolal      = ', boolal:10,      ' # alignment of boolean');
     writeln(prr, 'ptrsize     = ', ptrsize:10,     ' # size of pointer');
     writeln(prr, 'adrsize     = ', adrsize:10,     ' # size of address');
     writeln(prr, 'adral       = ', adral:10,       ' # alignment of address');
     writeln(prr, 'setsize     = ', setsize:10,     ' # size of set');
     writeln(prr, 'setal       = ', setal:10,       ' # alignment of set');
     writeln(prr, 'filesize    = ', filesize:10,    ' # required runtime space for file (lfn)');
     writeln(prr, 'fileidsize  = ', fileidsize:10,  ' # size of the lfn only');
     writeln(prr, 'exceptsize  = ', exceptsize:10,  ' # size of exception variable');
     writeln(prr, 'exceptal    = ', exceptal:10,    ' # alignment of exception');
     writeln(prr, 'stackal     = ', stackal:10,     ' # alignment of stack');
     writeln(prr, 'stackelsize = ', stackelsize:10, ' # stack element size');
     writeln(prr, 'maxsize     = ', maxsize:10,     ' # this is the largest type that can be on the stack');
     writeln(prr, 'heapal      = ', heapal:10,      ' # alignment for each heap arena');
     writeln(prr, 'gbsal       = ', gbsal:10,       ' # globals area alignment');
     writeln(prr, 'sethigh     = ', sethigh:10,     ' # set highest value');
     writeln(prr, 'setlow      = ', setlow:10,      ' # set lowest value');
     writeln(prr, 'ordmaxchar  = ', ordmaxchar:10,  ' # character highest value');
     writeln(prr, 'ordminchar  = ', ordminchar:10,  ' # character lowest value');
     writeln(prr, 'marksize    = ', marksize:10,    ' # size of mark');
     writeln(prr, 'maxexp      = ', maxexp:10,      ' # maximum exponent of real');
     writeln(prr, 'nilval      = ', nilval:10,      ' # value of nil');
     writeln(prr, 'maxint      = ', maxint:10,      ' # value of maxint on target machine');
     writeln(prr, 'markep      = ', markep:10,      ' # (old) maximum frame size');
     writeln(prr, 'marksb      = ', marksb:10,      ' # stack bottom');
     writeln(prr, 'market      = ', market:10,      ' # current ep');
   end;

   procedure assemble; forward;

   procedure prtline;
   begin
     write(prr, '# ', sline:6, ': ', iline:6, ': ')
   end;

   { write short block name with field }
   procedure wrtblksht(bp: pblock; var fl: integer);
   begin
     write(prr, bp^.bname^); fl := fl+max(bp^.bname^);
     if bp^.en > 1 then begin write(prr, '$', bp^.en:1); fl := fl+1+digits(bp^.en) end
   end;

   { write module/block scope reference with optional short and field}
   procedure wrtblks(bp: pblock; s: boolean; var fl: integer);
   begin
     if bp <> nil then begin
       wrtblks(bp^.parent, s, fl);
       if s then wrtblksht(bp, fl)
       else begin write(prr, bp^.name^); fl := fl+lenp(bp^.name^) end;
       write(prr, '.'); fl := fl+1
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

   { search overloads for max number }
   function fndovrmax(view bn: string; bp: pblock): integer;
   var ovrmax: integer;
   begin
     ovrmax := 0;
     while bp <> nil do begin
       if matchp(bn, bp^.bname^) and (bp^.en > ovrmax) then ovrmax := bp^.en;
       bp := bp^.incnxt
     end;
     fndovrmax := ovrmax 
   end;

   procedure wrtblklng(bp: pblock);
   var fl: integer;
   begin
     if bp <> nil then begin
       wrtblks(bp^.next, false, fl);
       write(prr, bp^.name^)
     end
   end;

   { write both short and long form labels }
   procedure wrtblklabs(bp: pblock);
   var fl: integer;
   begin
     if bp <> nil then begin
       if anyshort(bp) then begin
         wrtblks(bp^.parent, true, fl);
         write(prr, bp^.bname^); 
         if bp^.en > 1 then write(prr, '$', bp^.en:1);
         writeln(prr, ':')
       end;
       wrtblklng(bp);
       writeln(prr, ':')
     end
   end;

   procedure wrtgbl(var f: text; a: address; var fl: integer);
   var bp, fbp: pblock; sp, fsp: psymbol;
   begin
     bp := blkstk; fbp := nil; fsp := nil;
     while bp <> nil do begin
       if bp^.btyp in [btprog, btmod] then fbp := bp;
       bp := bp^.next
     end;
     if fbp <> nil then begin
       sp := fbp^.symbols;
       while sp <> nil do begin
         if (sp^.off = a) and (sp^.styp = stglobal) then fsp := sp;
         sp := sp^.next
       end
     end;
     if fsp <> nil then begin
       write(f, fbp^.bname^); write(f, '.'); write(f, fsp^.name^); 
       fl := fl+lenp(fbp^.bname^)+1+lenp(fsp^.name^)
     end else begin 
       write(f, 'globals_start+', a:1); fl := fl+14+digits(a) 
     end
   end;

   procedure wrtlcl(var f: text; p: lvltyp; a: address; var fl: integer);
   var bp, fbp: pblock; sp, fsp: psymbol;
   begin
     bp := blkstk; fbp := nil; fsp := nil;
     while bp <> nil do begin
       if (bp^.btyp in [btproc, btfunc]) and (bp^.lvl = p) then fbp := bp;
       bp := bp^.next
     end;
     if fbp <> nil then begin
       sp := fbp^.symbols;
       while sp <> nil do begin
         if (sp^.off = a) and (sp^.styp in [stlocal, stparam]) then fsp := sp;
         sp := sp^.next
       end
     end;
     if fsp <> nil then begin
       wrtblks(fbp, true, fl); write(f, fsp^.name^); 
       fl := fl+max(fsp^.name^)
     end else begin 
       write(f, a:1); fl := fl+digits(a) 
     end
   end;

   procedure generate; (*generate segment of code*)
      var x: integer; (* label number *)
          again: boolean;
          c,ch1: char;
          ls: pstring;    
          ispc: boolean;  
          i, l: integer;   
          bp: pblock;
          sp: psymbol;
          sgn: boolean;
          sn2: labbuf;
          snl2: 1..lablen;
          vt: array [1..100] of integer;
          vi, vl: integer;
          ts: alfa;
          fl: integer;
          os: optstr;
          oi: 1..maxopt;
          ti: 1..maxtmp;
          tv: integer;
          cstp, cstp2, cstp3: cstptr;
          csttab: boolean;
          v: integer;
          r: real;
          s: settype;
          cs: packed array [1..1] of char;
   begin
      again := true; csttab := false;
      while again and not eof(prd) do begin
        getnxt;(* first character of line*)
        if not (ch in ['!', 'l', 'q', ' ', ':', 'o', 'g', 'b', 'e', 's', 'f',
                       'v', 't', 'n', 'x', 'c']) then
          errorl('unexpected line start');
        case ch of
          '!': begin prtline; write(prr, ' ', '!'); while not eoln(prd) do
                 begin read(prd, ch); write(prr, ch) end;
                 writeln(prr);
               end;
          'l': begin getnxt; parlab(x,ls); 
                     prtline; write(prr, ' ', 'l ', sn:snl, '.', x:1);
                     if ls <> nil then
                       errorl('Invalid intermediate');
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
                  writeln(prr, '        .loc 1 ', x:1, ' 1'); { write debug line }
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
                   if not (ch in ['a'..'z', 'A'..'Z', '_']) then
                     errorl('No valid option found');
                   getlab; if snl > optlen then errorl('Option is too long');
                   write(prr, sn:snl);
                   for i := 1 to optlen do os[i] := sn[i];
                   oi := 1;
                   while (oi < maxopt) and (os <> opts[oi]) and (os <> optsl[oi]) do
                     oi := oi+1;
                   if (os = opts[oi]) or (os = optsl[oi]) then begin
                     ch1 := chr(oi+ord('a')-1);
                     option[oi] := true; 
                     if ch = '-' then option[oi] := false;
                     if (ch = '-') or (ch = '+') then begin write(prr, ch); getnxt end;
                     write(prr, ' ');
                     case oi of
                       7:  dodmplab   := option[oi];
                       8:  dosrclin   := option[oi];
                       11: doechlin   := option[oi];
                       14: dorecycl   := option[oi];
                       15: dochkovf   := option[oi];
                       16: dochkrpt   := option[oi];
                       13: donorecpar := option[oi];
                       17: dochkdef   := option[oi];
                       19: iso7185    := option[oi];
                       23: dodebug    := option[oi];
                       1:  dodbgflt   := option[oi];
                       6:  dodbgsrc   := option[oi];
                       5:  dodckout   := option[oi];
                       9:  dochkvbk   := option[oi];
                       2:; 3:; 4:; 12:; 20:; 21:; 22:;
                       24:; 25:; 26:; 10:; 18:;
                     end
                   end else errorl('No valid option found');
                   while not eoln(prd) and (ch = ' ') do getnxt
                 until not (ch in ['a'..'z']);
                 getlin; writeln(prr);
               end;
          'g': begin read(prd, gblsiz); getlin end; { set globals space }
          'b': begin { block start }
                 getnxt; skpspc;
                 if not (ch in ['p', 'm', 'r', 'f']) then
                   errorl('Block type is invalid');
                 ch1 := ch; { save block type }
                 getnxt; skpspc; getsds; sn2 := sn; snl2 := snl; cvtsds;
                 new(bp); bp^.name := strp(sn);
                 { get basename, without type }
                 l := 2; bp^.short := true;
                 while (l < lablen) and (sn[l] <> '$') do l := l+1;
                 if sn[l] = '$' then bp^.bname := strl(sn, l-1)
                 else begin
                   bp^.bname := strp(sn); { just use whole name }
                   bp^.short := false
                 end;
                 bp^.tmpnam := str(bp^.bname^); { copy to temp id }
                 ts := '$_tmpspc  '; l := max(bp^.tmpnam^);
                 for i := 1 to 8 do begin cs[1] := ts[i]; cat(bp^.tmpnam, cs) end; 
                 bp^.symbols := nil;
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
                   bp^.en := fndovrmax(bp^.bname^, blkstk^.incnxt)+1;
                   bp^.incnxt := blkstk^.incnxt; { insert to list }
                   blkstk^.incnxt := bp
                 end;
                 bp^.parent := blkstk; { set parent entry }
                 { put onto block stack }
                 bp^.next := blkstk; blkstk := bp;
                 prtline; writeln(prr, ' b ', ch1, ' ', sn2:snl2);
                 if ch1 in ['p', 'm'] then begin
                   modnam := bp^.bname;
                   preamble
                 end;
                 level := level+1; { count block levels }
                 bp^.lvl := level; { set }
                 getlin
               end;
          'e': begin 
                 getnxt; skpspc;
                 if not (ch in ['p', 'm', 'r', 'f']) then
                   errorl('Block type is invalid');
                 prtline; writeln(prr, ' e ', ch);
                 if ch = 'p' then postamble;
                 if blkstk = nil then errorl('System error');
                 bp := blkstk;
                 blkstk := blkstk^.next;
                 bp^.next := blklst;
                 blklst := bp;
                 level := level-1; { count block levels }
                 getlin
               end;
         's': begin { symbol }
                prtline; write(prr, ' s ');
                getnxt; getlab;
                new(sp); sp^.name := strp(sn);
                write(prr, sn:snl); 
                sn2 := sn; snl2 := snl;
                skpspc;
                if not (ch in ['g', 'l','p']) then
                  errorl('Symbol type is invalid');
                if ch = 'g' then sp^.styp := stglobal
                else if ch = 'p' then sp^.styp := stparam
                else sp^.styp := stlocal;
                ch1 := ch;
                write(prr, ' ', ch, ' ');
                getnxt;
                skpspc;
                if not (ch in ['0'..'9','-']) then
                  errorl('No offset found');
                sgn := ch = '-'; if ch = '-' then getnxt;
                ad := 0; while ch in ['0'..'9'] do
                  begin
                    if ad <= maxstr div 10 then
                      ad := ad*10+ord(ch)-ord('0')
                    else errorl('Symbol offset > max');
                    getnxt
                  end;
                if sgn then ad := -ad;
                write(prr, ad:1, ' ');
                sp^.off := ad; getsds; writeln(prr, sn:snl);
                sp^.digest := strp(sn);
                if anyshort(blkstk) then begin
                  wrtblks(blkstk, true, fl); 
                  if ch1 = 'g' then 
                    writeln(prr, sn2:snl2, ' = globals_start+', ad:1)
                  else
                    writeln(prr, sn2:snl2, ' = ', ad:1);
                end;
                if ch1 = 'g' then begin
                  write(prr, '        .globl   ');
                  wrtblks(blkstk, false, fl);
                  writeln(prr, sn2:snl2);
                  wrtblks(blkstk, false, fl); 
                  writeln(prr, sn2:snl2, ' = globals_start+', ad:1)
                end else begin
                  wrtblks(blkstk, false, fl); 
                  writeln(prr, sn2:snl2, ' = ', ad:1)
                end;
                { place in block symbol list }
                sp^.next := blkstk^.symbols;
                blkstk^.symbols := sp;
                getlin
              end;
          'f': getlin; { source error count }
          'v': begin { variant logical table }
                getnxt; skpspc;
                if ch <> 'l' then errorl('Label format error');
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
          't': begin { template }
                getnxt; skpspc;
                if ch <> 'l' then
                  errorl('Label format error');
                getnxt; parlab(x,ls);
                if ls <> nil then
                  errorl('Invalid intermediate');
                read(prd,l); 
                new(cstp2); cstp2^.ct := ctmp; cstp2^.next := csttbl; 
                csttbl := cstp2;
                cstp2^.tsize := l; cstp2^.tn := x; ti := 1;
                while not eoln(prd) do begin
                  if ti = maxtmp then errorl('Too many template indexes');
                  read(prd,tv); cstp2^.ta[ti] := tv; ti := ti+1;
                  { this is a gpc compiler bug, \cr is passing the eoln filter }
                  while not eoln(prd) and (prd^ <= ' ') do get(prd)
                end;
                getlin
              end;
         'n': begin { start constant table }
                if csttab then
                  errorl('Already in constant table');
                csttab := true; { flag in table }
                getnxt; skpspc;
                if ch <> 'l' then
                  errorl('Label format error');
                getnxt; parlab(x,ls);
                if ls <> nil then
                  errorl('Invalid intermediate');
                read(prd,l); { note the size is unused }
                new(cstp); cstp^.ct := ctab; cstp^.tb := nil; 
                cstp^.next := csttbl; csttbl := cstp;
                cstp^.csize := l; cstp^.cn := x;
                getlin
                { note mixed constants with other operands is
                  neither encouraged nor forbidden }
              end;
         'x': begin
                if not csttab then
                  errorl('No constant table active');
                cstp2 := cstp^.tb; cstp^.tb := nil;
                while cstp2 <> nil do begin
                  cstp3 := cstp2; cstp2 := cstp2^.next;
                  cstp3^.next := cstp^.tb; cstp^.tb := cstp3
                end;
                csttab := false;
                getlin
              end;
         'c': begin
                getnxt; skpspc;
                if not (ch in ['i','r','p','s','c','b','x'])
                  then errorl('Invalid const table type');
                case ch of { constant type }
                  'i': begin
                         getnxt; read(prd,v); new(cstp2); cstp2^.ct := cint; 
                         cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                         cstp2^.i := v; cstp2^.intn := 0
                       end;
                  'r': begin
                         getnxt; read(prd,r); new(cstp2); cstp2^.ct := creal;
                         cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                         cstp2^.r := r; cstp2^.realn := 0
                       end;
                  'p': begin
                         getnxt; skpspc;
                         if ch <> '(' then errorl('''('' expected for set');
                         s := [ ]; getnxt;
                         while ch<>')' do
                           begin read(prd,i); getnxt; s := s + [i] end;
                         new(cstp2); cstp2^.ct := cset;
                         cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                         cstp2^.s := s; cstp2^.setn := 0
                       end;
                  's': begin
                         getnxt; skpspc;
                         if ch <> '''' then errorl('quote expected for string');
                         getnxt; i := 1;
                         while ch<>'''' do
                           begin sn[i] := ch; i := i+1; getnxt end;
                         new(cstp2); cstp2^.ct := cstr;
                         cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                         cstp2^.str := strp(sn); cstp2^.strl := i; 
                         cstp2^.strn := 0
                       end;
                  'c': begin
                         getnxt;
                         { chars are output as values }
                         read(prd,i); new(cstp2); cstp2^.ct := cchr;
                         cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                         cstp2^.c := i; cstp2^.chrn := 0
                       end;
                  'b': begin
                         getnxt;
                         { booleans are output as values }
                         read(prd,i); new(cstp2); cstp2^.ct := cbol;
                         cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                         cstp2^.b := i; cstp2^.boln := 0
                       end;
                  'x': begin
                         getnxt; read(prd,i); new(cstp2); cstp2^.ct := cvalx;
                         cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                         cstp2^.x := v; cstp2^.valxn := 0
                       end;
                end;
                getlin
              end;
       end
     end
   end; (*generate*)

    procedure assemble; (*translate symbolic code into machine code and store*)

      var name :alfa; r :real; s :settype;
          i,s1,lb,ub,l:integer; c: char;
          str: strbuf; { buffer for string constants }
          cstp: cstptr;
          ep, ep2, ep3, ep4, ep5, pp: expptr;
          r1: reg; sp, sp2: pstring; def, def2: boolean; val, val2: integer;
          stkadr: integer; { stack address tracking }
          fl: integer; { field width }
          blk: pblock; { block reference }

      procedure labelsearch(var def: boolean; var val: integer; var sp: pstring; 
                            var blk: pblock);
      var x: integer; flp: flabelp;
      begin def := false; val := 0; flp := nil; blk := nil; skpspc; 
        if ch <> 'l' then errorl('Label format error');
        getnxt; parlab(x,sp);
        if sp <> nil then begin { far label }
          new(flp); flp^.next := flablst; flablst := flp; flp^.ref := sp
        end else begin { near label }
          if labeltab[x].ref = nil then putlabel(x);
          sp := labeltab[x].ref; def := labeltab[x].st = defined; 
          val := labeltab[x].val; blk := labeltab[x].blk
        end
      end;(*labelsearch*)

      procedure getname;
      var i: alfainx;
      begin
        if eof(prd) then errorl('Unexpected eof on input');
        for i := 1 to maxalfa do word[i] := ' ';
        i := 1; { set 1st character of word }
        if not (ch in ['a'..'z']) then errorl('No operation label');
        while ch in ['a'..'z'] do begin
          if i = maxalfa then errorl('Opcode label is too long');
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

      function regl(r: reg): integer;
      begin
        if r = rgnull then regl := 6
        else if r in [rgrax, rgrbx, rgrcx, rgrdx, rgrsi, rgrdi, rgrbp, rgrsp,
                      rgr10, rgr11, rgr12, rgr13, rgr14, rgr15] then regl := 3
        else if r in [rgr8, rgr9] then regl := 2
        else if r in [rgxmm0..rgxmm9] then regl := 4
        else regl := 5
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

      function bregl(r: reg): integer;
      begin
        if r = rgnull then bregl := 6
        else if r in [rgrax, rgrbx, rgrcx, rgrdx] then bregl := 2
        else if r in [rgrsi, rgrdi, rgrbp, rgrsp, rgr8, rgr9] then bregl := 3
        else if r in [rgr10..rgr15, rgxmm0..rgxmm9] then bregl := 4
        else bregl := 5
      end;

      procedure wrtregs(var f: text; rs: regset; n: boolean);
      var first: boolean; r: reg;
      begin
        first := true; write(f, '['); 
        for r := rgrax to rgxmm15 do 
          if ((r in rs) = n) and not (r in [rgrbp, rgrsp]) then begin 
          if not first then write(f, ', ');
          wrtreg(f, r); first := false
        end;
        write(f, ']')
      end;

      procedure dmptmp(var f: text);
      var p: tmpptr;
      begin 
        write(f, 'Temps list: ');
        p := tmplst;
        while p <> nil do begin write(f, p^.off); p := p^.next end;
        writeln(f)
      end;

      procedure gettmp(var a: address; len: address);
      var p, fp: tmpptr;
      begin
        fp := nil; p := tmplst; alignu(stackal, len);
        while p <> nil do begin if not p^.occu and (p^.len = len) then fp := p; p := p^.next end;
        if fp = nil then begin
          if tmpfre <> nil then begin fp := tmpfre; tmpfre := tmpfre^.next end
          else new(fp); 
          fp^.next := tmplst; tmplst := fp;
          tmpspc := tmpspc+len; tmpoff := tmpoff-len; fp^.off := tmpoff;
          fp^.len := len
        end;
        fp^.occu := true; 
        a := fp^.off;
        { uncomment for diagnostic }
        {
        writeln(prr, '# gettmp: address: ', a:1)
        }
      end;

      procedure puttmp(a: address);
      var p, fp: tmpptr;
      begin
        { uncomment for diagnostic }
        {
        writeln(prr, '# puttmp: address: ', a:1);
        }
        fp := nil; p := tmplst;
        while p <> nil do begin if p^.off = a then fp := p; p := p^.next end;
        if fp = nil then errorl('System error: tmp addr');
        fp^.occu := false
      end;

      procedure deltmp;
      var p: tmpptr;
      begin
        if tmplst <> nil then begin
          p := tmplst;
          while p^.next <> nil do p := p^.next;
          p^.next := tmpfre
        end else tmpfre := tmplst;
        tmplst := nil
      end;

      procedure pshstk(ep: expptr);
      begin
        ep^.next := estack; estack := ep; stacklvl := stacklvl+1
      end;
      
      procedure popstk(var ep: expptr);
      begin
        if estack = nil then errorl('Expression underflow');
        ep := estack; estack := estack^.next; ep^.next := nil; 
        stacklvl := stacklvl-1
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

      procedure dmpety(var f: text; ep: expptr; r1, r2: reg);
      begin
        write(prr, ep^.sline:6, ': ', ep^.iline:6, ': ');
        write(f, ep^.op:3, ': ', instr[ep^.op]:4, ' ');
        if ep^.op = 15{csp} then write(f, ep^.q:1, ': ', sptable[ep^.q]:4) 
        else if ep^.op in [123{ldci},126{ldcb},127{lccc}] then write(f, ep^.vi:1)
        else write(f, ep^.q:1);
        if r1 <> rgnull then begin write(prr, ' dr1: '); wrtreg(prr, r1) end;
        if r2 <> rgnull then begin write(prr, ' dr2: '); wrtreg(prr, r2) end;
        if ep^.r1 <> rgnull then begin write(f, ' r1: '); wrtreg(f, ep^.r1) end;
        if ep^.r1a <> 0 then write(f, ' r1a: ', ep^.r1a:1); 
        if ep^.r2 <> rgnull then begin write(f, ' r2: '); wrtreg(f, ep^.r2) end;
        if ep^.r2a <> 0 then write(f, ' r2a: ', ep^.r2a:1);
        if ep^.r3 <> rgnull then begin write(f, ' r3: '); wrtreg(f, ep^.r3) end;
        if ep^.r3a <> 0 then write(f, ' r3a: ', ep^.r3a:1);
        if ep^.t1 <> rgnull then begin write(f, ' t1: '); wrtreg(f, ep^.t1) end;
        if ep^.t1a <> 0 then write(f, ' t1a: ', ep^.t1a:1);
        if ep^.t2 <> rgnull then begin write(f, ' t2: '); wrtreg(f, ep^.t2) end;
        if ep^.t2a <> 0 then write(f, ' t2a: ', ep^.t2a:1);
        write(f, ' rs: '); wrtregs(f, ep^.rs, true) 
      end;

      overload procedure dmpety(var f: text; ep: expptr);
      begin dmpety(f, ep, rgnull, rgnull) end;

      overload procedure dmpety(var f: text; ep: expptr; r1: reg);
      begin dmpety(f, ep, r1, rgnull) end;
      
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

      procedure dmpstk(var f: text);
      var ep: expptr; sl: integer;
      begin
        ep := estack; sl := -1;
        while ep <> nil do begin
          write(f, 'Stack ', sl:3); dmpety(f, ep); writeln(f);
          ep := ep^.next; sl := sl-1
        end
      end;

      procedure botstk;
      begin
        if estack <> nil then begin
          writeln;
          writeln('*** Program translation error: [', sline:1, ',', iline:1, '] Stack balance');
          writeln;
          writeln('Contents of stack:');
          dmpstk(output);
          goto 99
        end
      end;

      procedure getreg(var r: reg; var rf: regset);
      var i: 1..maxintreg;
      begin
        i := 1;
        r := intassord[i];
        while not (r in rf) and (i < maxintreg) do 
          begin i := i+1; r := intassord[i] end;
        if not (r in rf) then errorl('Out of registers');
        rf := rf-[r];
      end;

      procedure getfreg(var r: reg; var rf: regset);
      var i: 1..maxfltreg;
      begin
        i := 1;
        r := fltassord[i];
        while not (r in rf) and (i < maxfltreg) do 
          begin i := i+1; r := fltassord[i] end;
        if not (r in rf) then errorl('Out of registers');
        rf := rf-[r];
      end;

      function isfltres(ep: expptr): boolean;
      var isf: boolean;
      begin
        isf := false; 
        if insf[ep^.op] then isf := true
        else if (ep^.op in [247{cif}, 246{cuf}]) and (ep^.rc = 1) then isf := true
        else if ep^.op = 15{csp} then
          if ep^.q in [19{atn},15{cos},16{exp},17{log},14{sin},18{sqt}] then 
            isf := true;
        isfltres := isf
      end;

      procedure assreg(ep: expptr; rf: regset; r1, r2: reg);
      var rfs: regset;

      procedure resreg(r: reg);
      begin
        if r <> rgnull then begin
          if not (r in rf) and (r <> r1) and (r <> r2) then ep^.rs := ep^.rs+[r];
          rf := rf-[r]
        end
      end;

      procedure dstreg(r: reg);
      begin
        if r <> rgnull then begin
          if not (r in rfs) and (r <> r1) and (r <> r2) then ep^.rs := ep^.rs+[r];
          rfs := rfs-[r]
        end
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
            if pc <= maxintparreg-1 then begin
              resreg(parreg[pc]); resreg(parreg[pc+1]); 
              assreg(pp, rf, parreg[pc], parreg[pc+1])
            end else begin
              getreg(r1, rf); getreg(r2, rf); assreg(pp, rf, r1, r2)
            end;
            pc := pc+2
          end else begin { single register }
            if pc <= maxintparreg then begin
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
        dstreg(rgr11); dstreg(rgxmm0); dstreg(rgxmm1); dstreg(rgxmm2); 
        dstreg(rgxmm3); dstreg(rgxmm4); dstreg(rgxmm5); dstreg(rgxmm6); 
        dstreg(rgxmm7); dstreg(rgxmm8); dstreg(rgxmm9); dstreg(rgxmm10);
        dstreg(rgxmm11); dstreg(rgxmm12); dstreg(rgxmm13); dstreg(rgxmm14); 
        dstreg(rgxmm15)
      end;

      begin { assreg }
        write(prr, '# assigning:  '); dmpety(prr, ep, r1, r2); 
        write(prr, ' ~rf: '); wrtregs(prr, rf, false); writeln(prr);
        rfs := rf;
        if ep^.al <> nil then assreg(ep^.al, rf, rgnull, rgnull);
        case ep^.op of

          {lodi,lodx,loda,lodb,lodc,lda}
          0,193,105,108,109,4: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
          end;

          {lodr}
          106: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            if ep^.p <> blkstk^.lvl then getreg(ep^.t1, rf)
          end;

          {lods}
          107: begin 
            resreg(rgrsi); resreg(rgrdi); 
            ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a, setsize)
          end;

          {adr,sbr}
          29, 31: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) 
          end;

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
            assreg(ep^.l, rf, rgrdi, rgnull); 
            assreg(ep^.r, rf, rgrsi, rgnull);
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          {adi,sbi,equ,neq,geq,grt,leq,les}
          28, 30, 17, 137, 139, 141, 18, 143, 145, 
          147, 19, 149, 151, 153, 20, 155, 157, 159, 21, 
          161, 163, 165, 167, 169, 171: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
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

          5{lao},234{lto}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          16{ixa}: begin 
            dstreg(rgrax); dstreg(rgrdx); resreg(rgrax);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            ep^.t1 := ep^.r1;
            if (ep^.r1 = rgrax) or (ep^.r1 = rgrdx) then getreg(ep^.t1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull); assreg(ep^.r, rf, rgnull, rgnull)
          end;

          118{swp}: ; { done at top level }

          {ldoi,ldoa,ldob,ldoc,ldox,ltci,ltcb,ltcc,ltcx}
          1,65,68,69,194,228,231,232,233:begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          {ldor,ltcr}
          66,229:begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) 
          end;

          {ldos,ltcs}
          67,230: begin 
            resreg(rgrsi); resreg(rgrdi); 
            ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a, setsize)
          end;

          {ind,inda,indb,indc,indx}
          9, 85,88,89,198: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
            assreg(ep^.l, rf, ep^.r1, rgnull) 
          end;

          {indr}
          86: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf); getreg(ep^.t1, rf);
            assreg(ep^.l, rf, ep^.t1, rgnull) 
          end;

          {inds}
          87: begin 
            dstreg(rgrsi); dstreg(rgrdi);
            assreg(ep^.l, rf, rgrsi, rgnull);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a, setsize)
          end;

          {inc,dec}
          10, 90, 93, 94, 57, 103, 104, 201, 202: begin 
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {mdc}
          254: begin 
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
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
            assreg(ep^.l, rf, rgr8, rgnull); assreg(ep^.r, rf, rgrcx, rgnull)
          end;

          {cps}
          176: begin 
            assreg(ep^.l, rf, rgnull, rgnull); 
            resreg(ep^.l^.r1); resreg(ep^.l^.r2);
            assreg(ep^.r, rf, rgnull, rgnull);
          end;

          {cpc}
          177: begin
            asscall; 
            assreg(ep^.l, rf, rgnull, rgrsi); assreg(ep^.r, rf, rgnull, rgrdx);
          end;

          {lpa}
          114: begin ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1); 
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
            gettmp(ep^.r1a, setsize) 
          end;

          {chki,chkb,chkc,chkx}
          26, 98, 99, 199: begin 
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1); 
            getreg(ep^.t1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {chka}
          95: begin 
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1); 
            assreg(ep^.l, rf, ep^.r1, rgnull);
            { this gets copied back because sometimes chka is applied to fat 
              pointer }
            ep^.r2 := ep^.l^.r2
          end;

          {chks}
          97: begin 
            asscall;
            assreg(ep^.l, rf, rgrdx, rgnull);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
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
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.r2 = rgnull then getreg(ep^.r2, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {sgs}
          32: begin 
            asscall; assreg(ep^.l, rf, rgrdi, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a, setsize)
          end;
     
          {flt,flo}
          33,34: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, rgnull, rgnull)  
          end;

          {trc}
          35: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if dochkovf then begin getfreg(ep^.t1, rf); getreg(ep^.t2, rf) end;
            assreg(ep^.l, rf, rgnull, rgnull)  
          end;

          {ngi}
          36: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull)  
          end;

          {ngr}
          37: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
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
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, ep^.r1, r2);
            getreg(ep^.t1, rf); getfreg(ep^.t2, rf) 
          end;

          {noti}
          205: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            getreg(ep^.t1, rf);
            assreg(ep^.l, rf, ep^.r1, r2)
          end;

          {notb,odd,chr}
          42,50,60: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2) 
          end;

          {rnd}
          62: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if dochkovf then begin getfreg(ep^.t1, rf); getreg(ep^.t2, rf) end;
            assreg(ep^.l, rf, rgnull, rgnull) 
          end;

          {and,ior,xor}
          43,44,206: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, ep^.r1, rgnull); 
            assreg(ep^.r, rf, rgnull, rgnull);
          end;

          {dif,int,uni}
          45,46,47: begin 
            asscall; 
            assreg(ep^.l, rf, rgrdi, rgnull); resreg(rgrdi);
            assreg(ep^.r, rf, rgrsi, rgnull);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            ep^.r1a := ep^.l^.r1a
          end;

          {inn}
          48: begin 
            asscall;
            assreg(ep^.l, rf, rgrdi, rgnull); resreg(rgrdi);
            assreg(ep^.r, rf, rgrsi, rgnull);
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax else 
            ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          {mod}
          49: begin 
            dstreg(rgrax); dstreg(rgrdx);
            if (r1 = rgnull) and (rgrdx in rf) then ep^.r1 := rgrdx
            else ep^.r1 := r1;
            assreg(ep^.l, rf, rgrax, rgnull); resreg(rgrax); 
            assreg(ep^.r, rf, rgnull, rgnull);
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          {dvi}
          53: begin 
            dstreg(rgrax); dstreg(rgrdx);
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else ep^.r1 := r1;
            assreg(ep^.l, rf, rgrax, rgnull); resreg(rgrax); 
            assreg(ep^.r, rf, rgnull, rgnull);
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          {mpi}
          51: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, ep^.r1, rgnull);
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {mpr}
          52: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, ep^.r1, rgnull); 
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {dvr}
          54: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
            if dodbgchk then begin getfreg(ep^.t1, rf); getreg(ep^.t2, rf) end;
            assreg(ep^.l, rf, ep^.r1, rgnull); 
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {rgs}
          110: begin 
            asscall; 
            assreg(ep^.l, rf, rgrdi, rgnull); assreg(ep^.r, rf, rgrsi, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            gettmp(ep^.r1a, setsize)
          end;

          { dupi, dupa, dupr, dups, dupb, dupc }
          181, 182, 183, 184, 185, 186: ;

          {cks}
          187:;

          {csp}
          15: begin
            asscall; asspar(ep, sppar[ep^.q]);
            if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then resreg(rgrcx);
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
            if ep^.rc = 1 then begin
              if r1 = rgnull then begin
                if rgxmm0 in rf then ep^.r1 := rgxmm0 else getfreg(ep^.r1, rf)
              end else ep^.r1 := r1
            end else if (ep^.rc = 2) or (ep^.rc = 3) then begin 
              ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
              gettmp(ep^.r1a, setsize)
            end else begin
              if r1 = rgnull then begin
                if rgrax in rf then ep^.r1 := rgrax else getreg(ep^.r1, rf)
              end else ep^.r1 := r1
            end;
            asspar(ep, ep^.pn)
          end;

          {cup}
          12: begin
            asscall; asspar(ep, ep^.pn)
          end;

          {cip}
          113: begin
            asscall; asspar(ep, ep^.pn); assreg(ep^.l, rf, rgnull, rgnull)
          end;

          {cif}
          247: begin
            asscall; resreg(rgr15);
            if ep^.rc = 1 then begin
              if r1 = rgnull then begin
                if rgxmm0 in rf then ep^.r1 := rgxmm0 else getfreg(ep^.r1, rf)
              end else ep^.r1 := r1
            end else if (ep^.rc = 2) or (ep^.rc = 3) then begin 
              ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
              gettmp(ep^.r1a, setsize)
            end else begin
              if r1 = rgnull then begin 
                if rgrax in rf then ep^.r1 := rgrax else getreg(ep^.r1, rf)
              end else ep^.r1 := r1
            end;
            asspar(ep, ep^.pn); assreg(ep^.l, rf, rgnull, rgnull)
          end;

          {cuv}
          27: begin
            asscall
          end;

          {cvf}
          249: begin 
            asscall;
            if ep^.rc = 1 then begin
              if r1 = rgnull then begin
                if rgxmm0 in rf then ep^.r1 := rgxmm0 else getfreg(ep^.r1, rf)
              end else ep^.r1 := r1
            end else if (ep^.rc = 2) or (ep^.rc = 3) then begin 
              ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
              gettmp(ep^.r1a, setsize)
            end else begin
              if r1 = rgnull then begin
                if rgrax in rf then ep^.r1 := rgrax else getreg(ep^.r1, rf)
              end else ep^.r1 := r1
            end;
            asspar(ep, ep^.pn)
          end;

          {cke}
          188: begin
            getreg(ep^.r1, rf); getreg(ep^.r2, rf); 
            getreg(ep^.t1, rf); assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {wbs}
          243: begin 
            asscall;
            ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {cxs}
          211: begin
            dstreg(rgrax); dstreg(rgrdx); resreg(rgrax); resreg(rgrdx);
            ep^.r1 := r1;
            if ep^.r1 = rgnull then ep^.r1 := rgrax;
            resreg(ep^.r1);
            assreg(ep^.l, rf, ep^.r1, rgnull); resreg(ep^.l^.r2);
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {cxc}
          212: begin
            dstreg(rgrax); dstreg(rgrdx); resreg(rgrax); resreg(rgrdx);
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then ep^.r1 := rgrax;
            resreg(ep^.r1); 
            if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2); 
            getreg(ep^.t1, rf);
            assreg(ep^.l, rf, ep^.r1, ep^.r2);
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {lft} 
          213: begin
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {max} 
          214: begin
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.q <> 1 then getreg(ep^.t1, rf);
            { left is complex pointer, right is lvl, result is left }
            assreg(ep^.l, rf, rgnull, ep^.r1);
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {equv,neqv,lesv,grtv,leqv,geqv} 
          215,216,217,218,219,220: begin
            asscall;
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdi, rgrdx); 
            assreg(ep^.r, rf, rgrsi, rgnull)
          end;

          {spc} 
          222: begin
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
            assreg(ep^.l, rf, ep^.r1, ep^.r2)
          end;

          {ccs} 
          223: begin
            dstreg(rgrsi); dstreg(rgrdi); dstreg(rgrcx); dstreg(rgrax); dstreg(rgrdx);
            resreg(rgrsi); resreg(rgrdi); resreg(rgrcx); resreg(rgrax); resreg(rgrdx);
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
            assreg(ep^.l, rf, ep^.r1, ep^.r2);
            getreg(ep^.t1, rf); getreg(ep^.t2, rf); getreg(ep^.t3, rf)
          end;

          {ldp} 
          225: begin
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
            assreg(ep^.l, rf, ep^.r1, rgnull)
          end;

          {mpc} 
          248: begin
            ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
            assreg(ep^.l, rf, ep^.r1, rgnull);
            assreg(ep^.r, rf, ep^.r2, rgnull)
          end;

          {cpl} 
          251: begin
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
            assreg(ep^.l, rf, rgnull, ep^.r1)
          end;

        end;
        write(prr, '# assigning~: '); dmpety(prr, ep); write(prr, ' ~rf: ');
        wrtregs(prr, rf, false); writeln(prr)
      end;

      { Interpret instruction macro string and write to output.
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
        @s - Symbol
        @g - Global symbol from integer 1 (by offset)
        @l - Local symbol from integer 1 (by offset)
      }
      procedure wrtins(view si: string; i1, i2: integer; r1, r2: reg; view sn: string);
      var i, j: integer;

      function cur: char;
      begin if i > max(si) then cur := ' ' else cur := si[i] end;

      function looka: char;
      begin if i+1 > max(si) then looka := ' ' else looka := si[i+1] end;

      procedure next;
      begin if i > max(si) then errorl('Error in instruction     '); i := i+1 end;
        
      begin
        i := 1; j := 1;
        { write any label }
        while cur <> ' ' do begin write(prr, cur); next; j := j+1 end;
        write(prr, ' '); j := j+1;
        while j <= opcspc do begin write(prr, ' '); j := j+1 end;
        while (cur = ' ') and (i < max(si)) do next; { skip spaces }
        { write opcode }
        while cur <> ' ' do begin write(prr, cur); next; j := j+1 end;
        write(prr, ' '); j := j+1;
        while j <= parspc do begin write(prr, ' '); j := j+1 end;
        while (cur = ' ') and (i < max(si)) do next; { skip spaces }
        { parse parameters and macros }
        while i <= max(si) do begin
          if cur = '#' then begin
            while j <= cmtspc do begin write(prr, ' '); j := j+1 end;
            j := max(si);
            while (si[j] = ' ') and (j > 1) do j := j-1;
            while i <= j do begin write(prr, cur); next end;
            i := max(si)+1;
          end else begin
            if cur = '$' then begin next; write(prr, '$'); j := j+1;
              if cur = '0' then begin write(prr, i1:1); j := j+digits(i1) end
              else if cur = '1' then begin write(prr, i2:1); j := j+digits(i2) end
              else if cur = 's' then begin write(prr, sn); j := j+lenp(sn) end
              else begin write(prr, cur); j := j+1 end
            end else if cur = '%' then begin next; write(prr, '%'); j := j+1;
              if cur = '1' then begin
                if looka = 'l' then begin wrtbreg(prr, r1); next; j := j+bregl(r1) end
                else begin wrtreg(prr, r1); j := j+regl(r1) end
              end else if cur = '2' then begin
                if looka = 'l' then begin wrtbreg(prr, r2); next; j := j+bregl(r2) end
                else begin wrtreg(prr, r2); j := j+regl(r2) end
              end else begin write(prr, cur); j := j+1 end
            end else if cur = '+' then begin next; write(prr, '+'); j := j+1;
              if cur = '0' then begin write(prr, i1:1); j := j+digits(i1) end
              else if cur = '1' then begin write(prr, i2:1); j := j+digits(i2) end
              else begin write(prr, cur); j := j+1 end
            end else if cur = '-' then begin next; write(prr, '-');j := j+1;
              if cur = '0' then begin write(prr, i1:1); j := j+digits(i1) end 
              else if cur = '1' then begin write(prr, i2:1); j := j+digits(i2) end
              else begin write(prr, cur); j := j+1 end
            end else if cur = '^' then begin next;
              if cur = '0' then begin write(prr, i1:1); j := j+digits(i1) end
              else if cur = '1' then begin write(prr, i2:1); j := j+digits(i1) end
              else begin write(prr, cur);  j := j+1 end
            end else if cur = '@' then begin next;
              if cur = 's' then begin write(prr, sn); j := j+lenp(sn) end
              else if cur = 'g' then wrtgbl(prr, i1, j)
              else if cur = 'l' then wrtlcl(prr, i2, i1, j)
              else begin write(prr, cur); j := j+1 end
            end else begin write(prr, cur);  j := j+1 end;
            next
          end
        end;
        writeln(prr)
      end;

      overload procedure wrtins(view si: string);
      begin wrtins(si, 0, 0, rgnull, rgnull, '') end;

      overload procedure wrtins(view si: string; i1, i2: integer);
      begin wrtins(si, i1, i2, rgnull, rgnull, '') end;

      overload procedure wrtins(view si: string; i1, i2: integer; r1, r2: reg);
      begin wrtins(si, i1, i2, r1, r2, '') end;

      overload procedure wrtins(view si: string; r1, r2: reg; view sn: string);
      begin wrtins(si, 0, 0, r1, r2, sn) end;

      overload procedure wrtins(view si: string; r1, r2: reg);
      begin wrtins(si, 0, 0, r1, r2, '') end;

      overload procedure wrtins(view si: string; r1: reg);
      begin wrtins(si, 0, 0, r1, rgnull, '') end;

      overload procedure wrtins(view si: string; i1, i2: integer; r1: reg);
      begin wrtins(si, i1, i2, r1, rgnull, '') end;

      overload procedure wrtins(view si: string; r1: reg; view sn: string);
      begin wrtins(si, 0, 0, r1, rgnull, sn) end;

      overload procedure wrtins(view si: string; i1: integer);
      begin wrtins(si, i1, 0, rgnull, rgnull, '') end;

      overload procedure wrtins(view si: string; i1: integer; r1, r2: reg);
      begin wrtins(si, i1, 0, r1, r2, '') end;

      overload procedure wrtins(view si: string; i1: integer; r1: reg);
      begin wrtins(si, i1, 0, r1, rgnull, '') end;

      overload procedure wrtins(view si: string; i1: integer; r1, r2: reg; view sn: string);
      begin wrtins(si, i1, 0, r1, r2, sn) end;

      overload procedure wrtins(view si: string; i1: integer; r1: reg; view sn: string);
      begin wrtins(si, i1, 0, r1, rgnull, sn) end;

      overload procedure wrtins(view si: string; view sn: string);
      begin wrtins(si, 0, 0, rgnull, rgnull, sn) end;

      overload procedure wrtins(view si: string; i1, i2: integer; view sn: string);
      begin wrtins(si, i1, i2, rgnull, rgnull, sn) end;

      overload procedure wrtins(view si: string; i1: integer; view sn: string);
      begin wrtins(si, i1, 0, rgnull, rgnull, sn) end;

      procedure genexp(ep: expptr);
      var r: reg; ep2: expptr; i: integer; stkadrs: integer; fl: integer;

      { push parameters in order }
      procedure pshpar(pp: expptr);
      begin
        while pp <> nil do begin
          genexp(pp);
          if pp^.r2 <> rgnull then begin
            wrtins(' pushq %1 # place 2nd register on stack', pp^.r2); 
            stkadr := stkadr-intsize
          end;
          if inss[pp^.op] then begin
            wrtins(' subq $0,%rsp # allocate set', setsize);
            wrtins(' pushq %rsi # save source');
            wrtins(' pushq %rdi # save destination');
            if pp^.r1 <> rgrsi then
              wrtins(' movq %1,%rsi # place source', pp^.r1);
            wrtins(' movq %rsp,%rdi # destination is stack');
            wrtins(' addq $0,%rdi # index over saved', ptrsize*2);
            wrtins(' movsq # move set');
            wrtins(' movsq');
            wrtins(' movsq');
            wrtins(' movsq');  
            wrtins(' popq %rdi # restore');          
            wrtins(' popq %rsi');
            stkadr := stkadr-setsize
          end else if pp^.r1 in [rgrax..rgr15] then begin
            wrtins(' pushq %1 # save parameter', pp^.r1); 
            stkadr := stkadr-intsize
          end else if pp^.r1 in [rgxmm0..rgxmm15] then begin
            wrtins(' subq $0,%rsp # allocate real on stack', realsize); 
            stkadr := stkadr-realsize;
            wrtins(' movsd %1,(%rsp) # place real on stack', pp^.r1)
          end;
          pp := pp^.next
        end
      end;

      { call system procedure/function }
      procedure callsp(ep: expptr; var sc: alfa; r: boolean);
      var si: packed array [1..60] of char; i: integer; pp: expptr; aln: boolean;
      begin
        { evaluate all parameters }
        pp := ep^.pl;
        while pp <> nil do begin genexp(pp); pp := pp^.next end;
        aln := false;
        if stkadr mod 16 <> 0 then begin
          wrtins(' pushq %rbx # align');
          aln := true
        end;
        si := ' call psystem_     # call system procedure/function         ';
        for i := 1 to maxalfa do if sc[i] <> ' ' then si[15+i-1] := sc[i];
        wrtins(si);
        if aln then
          wrtins(' popq %rbx # drop alignment');
        if r then begin
          if isfltres(ep) then begin
            if ep^.r1 <> rgxmm0 then 
              wrtins(' movsd %xmm0,%1 # place result', ep^.p, ep^.r1)
          end else begin 
            if ep^.r1 <> rgrax then 
              wrtins(' movq %rax,%1 # place result', ep^.p, ep^.r1);
            if (ep^.r2 <> rgnull) and (ep^.r2 <> rgrdx) then
              wrtins(' movq %rdx,%1', ep^.p, ep^.r2)
          end
        end
      end;

      { call nwl/dsl }
      procedure callnwldsl(ep: expptr);
      var aln: boolean; pp, ep2: expptr; i: integer; stkadrs: integer;
      begin
        stkadrs := stkadr; { save because we don't know tag count }
        ep2 := ep^.pl;
        for i := 1 to 3 do begin
          if ep2 = nil then errorl('system error');
          ep2 := ep2^.next
        end;
        pshpar(ep2);
        pp := ep^.pl; genexp(pp); { addr rdi }
        pp := pp^.next; genexp(pp); { size rsi }
        pp := pp^.next; genexp(pp); { tagcnt rdx }
        wrtins(' pushq %1 # save tag count', pp^.r1);
        wrtins(' movq %rsp,%rcx # index tag list');
        wrtins(' addq $0,%rcx', intsize);
        stkadr := stkadr-adrsize;
        aln := false;
        if stkadr mod 16 <> 0 then begin
          wrtins(' pushq %rbx # align'); 
          stkadr := stkadr-adrsize; aln := true
        end;
        if ep^.q = 39 then
          wrtins(' call psystem_nwl # call new')
        else
          wrtins(' call psystem_dsl # call dispose');
        if aln then begin
          wrtins(' popq %rbx # dump align');
          stkadr := stkadr+adrsize
        end;
        wrtins(' popq %rcx # restore tag count');
        stkadr := stkadr+adrsize;
        wrtins(' movq $0,%rax # find *integer', intsize);
        wrtins(' mulq %rcx');
        wrtins(' addq %rax,%rsp # dump taglist from stack');
        stkadr := stkadrs { restore to entry }
      end;

      begin { genexp }
        if ep <> nil then begin
          for r := rgrax to rgxmm15 do if r in ep^.rs then begin
              if r in [rgrax..rgr15] then begin
                wrtins(' pushq %1 # save used register', r); 
                stkadr := stkadr-intsize
              end else begin
                wrtins(' subq $0,%rsp # allocate real on stack', realsize);
                wrtins(' movsd %1,(%rsp) # save used register', r);
                stkadr := stkadr-realsize
              end
          end;
          genexp(ep^.al);
          if (ep^.op <> 113{cip}) and (ep^.op <> 247{cif}) then genexp(ep^.l);
          genexp(ep^.r); genexp(ep^.x1);
          write(prr, '# generating:  '); dmpety(prr, ep); writeln(prr);
          case ep^.op of

            {lodi,loda}
            0,105: begin
              if ep^.p <> blkstk^.lvl then begin
                wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.r1);
                wrtins(' movq @l(%1),%1 # fetch local qword', ep^.q, ep^.p, ep^.r1)
              end else
                wrtins(' movq @l(%rbp),%1 # fetch local qword', ep^.q, ep^.p, ep^.r1)
            end;

            {lodx,lodb,lodc}
            193,108,109: begin
              if ep^.p <> blkstk^.lvl then begin
                wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.r1);
                wrtins(' movzx @l(%1),%1 # fetch local byte', ep^.q, ep^.p, ep^.r1)
              end else
                wrtins(' movzx @l(%rbp),%1 # fetch local byte', ep^.q, ep^.p, ep^.r1)
            end;

            {lodr}
            106: begin
              if ep^.p <> blkstk^.lvl then begin
                wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.t1);
                wrtins(' movsd @l(%1),%2 # fetch local real', ep^.q, ep^.p, ep^.t1, ep^.r1)
              end else
                wrtins(' movsd @l(%rbp),%1 # fetch local real', ep^.q, ep^.p, ep^.r1)
            end;

            {lods}
            107: begin
              if ep^.p <> blkstk^.lvl then begin
                wrtins(' movq ^0(%rbp),%rsi # get display pointer', ep^.q1);
                wrtins(' lea @l(%rsi),%rsi # index local set', ep^.q, ep^.p)
              end else
                wrtins(' lea @l(%rbp),%rsi # index local set', ep^.q, ep^.p);
              wrtins(' leaq ^-@s^0(%rbp),%rdi # index destination temp', ep^.r1a, lclspc^);
              wrtins(' movsq # move');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' leaq ^-@s^0(%rbp),%1 # index temp again', ep^.r1a, ep^.r1, lclspc^);
            end;

            {lda}
            4: begin
              if ep^.p <> blkstk^.lvl then begin
                wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.r1);
                wrtins(' lea @l(%1),%1 # index local', ep^.q, ep^.r1)
              end else
                wrtins(' lea @l(%rbp),%1 # index local', ep^.q, ep^.r1)
            end;

            {adi}
            28: begin
              wrtins(' add %1,%2 # add integers', ep^.r^.r1, ep^.l^.r1);
              if dochkovf then begin
                wrtins(' jno 1f # skip no overflow');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # set line number', sline);
                wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end
            end;

            {adr}
            29: 
              wrtins(' addsd %1,%2 # add reals', ep^.r^.r1, ep^.l^.r1);

            {sbi}
            30: begin
              wrtins(' sub %1,%2 # subtract integers', ep^.r^.r1, ep^.l^.r1);
              if dochkovf then begin
                wrtins(' jno 1f # skip no overflow');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # set line number', sline);
                wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end
            end;

            {sbr}
            31: 
              wrtins(' subsd %1,%2 # subtract reals', ep^.r^.r1, ep^.l^.r1);

            {equr,neqr,geqr,grtr,leqr,lesr}
            138,144,150,156,162,168: begin 
              case ep^.op of
                138{equr}: wrtins(' cmpeqsd %1,%2 # compare real equal', ep^.r^.r1, ep^.l^.r1);
                144{neqr}: wrtins(' cmpneqsd %1,%2 # compare real not equal', ep^.r^.r1, ep^.l^.r1);
                150{geqr}: wrtins(' cmplesd %2,%1 # compare real greater or equal', ep^.r^.r1, ep^.l^.r1);
                156{grtr}: wrtins(' cmpltsd %2,%1 # compare real greater', ep^.r^.r1, ep^.l^.r1);
                162{leqr}: wrtins(' cmplesd %1,%2 # compare real less or equal', ep^.r^.r1, ep^.l^.r1);
                168{lesr}: wrtins(' cmpltsd %1,%2 # compare real less', ep^.r^.r1, ep^.l^.r1);
              end;
              if ep^.op in [150{geqr},156{grtr}] then 
                wrtins(' movq %1,%2 # move to result', ep^.r^.r1, ep^.r1)
              else
                wrtins(' movq %1,%2 # move to result', ep^.l^.r1, ep^.r1);
              wrtins(' andq $0,%1 # mask boolean', 1, ep^.r1) 
            end;

            120{lip}: begin 
              if ep^.p <> blkstk^.lvl then begin
                wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.t1);
                wrtins(' movq @l+ptrsize(%2),%1 # load frame pointer', ep^.q, ep^.p, ep^.r2, ep^.t1);
                wrtins(' movq @l(%2),%1 # load procedure address', ep^.q, ep^.p, ep^.r1, ep^.t1)
              end else begin
                wrtins(' movq @l+ptrsize(%rbp),%1 # load frame pointer', ep^.q, ep^.p, ep^.r2);
                wrtins(' movq @l(%rbp),%1 # load procedure address', ep^.q, ep^.p, ep^.r1)
              end
            end;  

            {equm,neqm,geqm,gtrm,leqm,lesm}
            142,148,154,160,166,172: begin 
              wrtins(' movq $0,%rdx # get string length', ep^.q);
              wrtins(' call psystem_strcmp # compare strings'); 
              wrtins(' cmpq $0,%rax # compare -0+ result');
              case ep^.op of
                142{equm}: wrtins(' sete %1l # set equal', ep^.r1);
                148{neqm}: wrtins(' setne %1l # set not equal', ep^.r1);
                154{geqm}: wrtins(' setge %1l # set greater or equal', ep^.r1);
                160{grtm}: wrtins(' setg %1l # set greater', ep^.r1);
                166{leqm}: wrtins(' setle %1l # set less or equal', ep^.r1);
                172{lesm}: wrtins(' setl %1l # set less', ep^.r1);
              end;
              wrtins(' movsx %1l,%1 # sign extend boolean', ep^.r1)
            end;

            5{lao},234{lto}:
              if ep^.fl <> nil then 
                wrtins(' leaq @s(%rip),%1 # load address of global', ep^.r1, ep^.fl^)
              else
                wrtins(' leaq @g(%rip),%1 # load address of global', ep^.q, ep^.r1);

            16{ixa}: begin 
              { left is address right is index, size is q }
              if ep^.r1 <> ep^.t1 then
                wrtins(' movq %1,%2 # save index', ep^.r1, ep^.t1);
              wrtins(' movq $0,%rax # get element size', ep^.q);
              wrtins(' mul %1 # find index*size', ep^.r^.r1);
              wrtins(' add %rax,%1 # add to base', ep^.t1);
              if ep^.r1 <> ep^.t1 then
                wrtins(' movq %1,%2 # move to final register', ep^.t1, ep^.r1)
            end;

            118{swp}: ; { done at top level }

            {ldoi,ltci}
            1,65,228:
              if ep^.fl <> nil then
                wrtins(' movq @s(%rip),%1 # load global quad', ep^.r1, ep^.fl^)
              else
                wrtins(' movq @g(%rip),%1 # load global quad', ep^.q, ep^.r1);

            {ldob,ldoc,ldox,ltcb,ltcc,ltcx}
            68,69,194,231,232,233:
              if ep^.fl <> nil then
                wrtins(' movzx @s(%rip),%1 # load and zero extend global byte', ep^.r1, ep^.fl^)
              else
                wrtins(' movzx @g(%rip),%1 # load and zero extend global byte', ep^.q, ep^.r1);

            {ldor,ltcr}
            66,229: 
              if ep^.fl <> nil then
                wrtins(' movsd @s(%rip),%1 # load global real', ep^.r1, ep^.fl^)
              else
                wrtins(' movsd @g(%rip),%1 # load global real', ep^.q, ep^.r1);

            {ldos,ltcs}
            67,230: begin
              if ep^.fl <> nil then
                wrtins(' leaq @s(%rip),%rsi # load address of global set', ep^.fl^)
              else
                wrtins(' leaq @g(%rip),%rsi # load address of global set', ep^.q);
              wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
              wrtins(' movsq # move');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end;

            {indi,inda}
            9,85: 
              wrtins(' movq ^0(%1),%1 # load qword from address', ep^.q, ep^.l^.r1);

            {indr}
            86: 
              wrtins(' movsd ^0(%2),%1 # load real from address', ep^.q, ep^.r1, ep^.t1);

            {indb,indc,indx}
            88,89,198: wrtins(' movzx ^0(%1),%1 # load byte from address', ep^.q, ep^.l^.r1);

            {inds}
            87: begin 
              wrtins(' addq $0,%rsi # offset', ep^.q);
              wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
              wrtins(' movsq # move');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end;

            {inci,incb,incc,incx}
            10, 93, 94, 201: begin
              wrtins(' addq $0,%1 # increment by n', ep^.q, ep^.r1);
              if dochkovf then begin
                wrtins(' jno 1f # skip no overflow');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # set line number', sline);
                wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end
            end;

            {inca}
            90: 
              wrtins(' addq $0,%1 # increment by n', ep^.q, ep^.r1);

            {deci,decb,decc,decx}
            57, 103, 104, 202: begin
              wrtins(' subq $0,%1 # decrement by n', ep^.q, ep^.r1);
              if dochkovf then begin
                wrtins(' jno 1f # skip no overflow');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # set line number', sline);
                wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end
            end;

            {mdc}
            254: begin
              wrtins(' movq %1,%2 # copy template pointer to data', ep^.r1, ep^.r2);
              wrtins(' addq $0,%1 # skip template to data', ep^.q, ep^.r1)
            end;

            {ckvi,ckvb,ckvc,ckvx}
            175, 179, 180, 203: begin 
              wrtins(' cmpq $0,%1 # check this tag value', ep^.q, ep^.r1);
              wrtins(' sete %1l # set boolean equal', ep^.t1);
              wrtins(' orq %1,%2 # or with running total', ep^.t1, ep^.r2);
            end;

            {cvbi,cvbx,cvbb,cvbc}
            100, 115, 116, 121: begin
              wrtins(' movq $0,%rdi # load tagfield offset', ep^.q);
              wrtins(' movq $0,%rsi # load size of variant', ep^.q1);
              wrtins(' leaq @s(%rip),%rdx # load logical variant table', ep^.lt^);
              if ep^.op = 100 then
                wrtins(' movq (%1),%r8 # get existing tag value', ep^.l^.r1)
              else
                wrtins(' movzx (%1),%r8 # get existing tag value', ep^.q, ep^.l^.r1);
              wrtins(' call psystem_tagchgvar # check valid tag change')
            end;

            {ivti,ivtx,ivtb,ivtc}
            192,101,102,111: begin
              wrtins(' movq $0,%rdi # load tagfield offset', ep^.q);
              wrtins(' movq $0,%rsi # load size of variant', ep^.q1);
              wrtins(' leaq @s(%rip),%rdx # load logical variant table', ep^.lt^);
              if ep^.op = 100 then
                wrtins(' movq (%1),%r8 # get existing tag value ', ep^.q, ep^.l^.r1)
              else
                wrtins(' movzx (%1),%r8 # get existing tag value', ep^.q, ep^.l^.r1);
              wrtins(' call psystem_tagchginv # invalidate tag changes')
            end;

            {cps}
            176: begin 
              wrtins(' cmpq %1,%2 # compare container lengths', ep^.r^.r2, ep^.l^.r2);
              wrtins(' je 1f # skip equal', ep^.q, ep^.r^.r1);
              wrtins(' leaq modnam(%rip),%rdi # load module name');
              wrtins(' movq $0,%rsi # load line number', sline);
              wrtins(' movq $ContainerMismatch,%rdx # load error code');
              wrtins(' call psystem_errore # process error');
              wrtins('1:');
            end;

            {cpc}
            177: begin
              wrtins(' movq $0,%rdi # get level number', ep^.q);
              wrtins(' call psystem_cmptmp # compare templates')
            end;

            {cta}
            191: begin
              wrtins(' movq $0,%rdi # get tag offset', ep^.q);
              wrtins(' movq $0,%rsi # get tag nesting level', ep^.q1);
              wrtins(' leaq @s(%rip),%rdx # index logical variant table', ep^.lt^);
              wrtins(' call psystem_tagchkass # check tag assignment')
            end;

            {lpa}
            114: begin 
              wrtins(' leaq @s(%rip),%1 # load procedure/function address', ep^.r1, ep^.fn^);
              wrtins(' movq @l(%rbp),%1 # load display pointer', ep^.q1, ep^.p, ep^.r2)
            end;

            {ldci,ldcc,ldcb}
            123,127,126:
              wrtins(' movq $0,%1 # load quad constant', ep^.vi, ep^.r1); 

            {ldcn}
            125:
              wrtins(' movq $0,%1 # load nil value', ep^.r1);

            {ldcr}
            124:
              wrtins(' movsd real^0(%rip),%1 # load real constant', ep^.realn, ep^.r1);

            {ldcs}
            7: begin
              wrtins(' leaq set^0(%rip),%rsi # index constant set', ep^.setn);
              wrtins(' leaq ^-@s^0(%rbp),%rdi # index temp', ep^.r1a, lclspc^);
              wrtins(' movsq # move');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' movsq');
              wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp    ', ep^.r1a, ep^.r1, lclspc^);
            end;

            {chki,chkb,chkc,chkx}
            26, 98, 99, 199: begin 
              wrtins(' movq $0,%1 # load low bound', ep^.vi, ep^.t1);
              wrtins(' cmpq %1,%2 # compare', ep^.t1, ep^.r1);
              wrtins(' jge 1f # skip if greater or equal');
              wrtins(' leaq modnam(%rip),%rdi # load module name');
              wrtins(' movq $0,%rsi # load line number', sline);
              wrtins(' movq $ValueOutOfRange,%rdx # load error code');
              wrtins(' call psystem_errore # process error');
              wrtins('1:        ');
              wrtins(' movq $0,%1 # load high bound', ep^.vi2, ep^.t1);
              wrtins(' cmpq %1,%2 # compare', ep^.t1, ep^.r1);
              wrtins(' jle 1f # skip if less or equal');
              wrtins(' leaq modnam(%rip),%rdi # load module name');
              wrtins(' movq $0,%rsi # load line number', sline);
              wrtins(' movq $ValueOutOfRange,%rdx # load error code');
              wrtins(' call psystem_errore # process error');
              wrtins('1:')
            end;

            {chka}
            95: begin 
              wrtins(' orq %1,%1 # check nil', ep^.r1);
              wrtins(' jnz 1f # skip if not');
              wrtins(' leaq modnam(%rip),%rdi # load module name');
              wrtins(' movq $0,%rsi # load line number', sline);
              wrtins(' movq $DereferenceOfNilPointer,%rdx # load error code');
              wrtins(' call psystem_errore # process error');
              wrtins('1:')
            end;

            {chks}
            97: begin
              wrtins(' movq $0,%rdi # load low bound', ep^.vi);
              wrtins(' movq $0,%rsi # load high bound', ep^.vi2);
              wrtins(' call psystem_chksetbnd # check set in bounds');
              wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp set', ep^.r1a, ep^.r1, lclspc^)
            end;

            {ckla}
            190: begin
              if ep^.q <> 0 then begin
                wrtins(' orq %1,%1 # check nil', ep^.r1);
                wrtins(' jge 1f # skip greater or equal', ep^.r2);
                wrtins(' leaq modnam(%rip),%rdi # load module name');
                wrtins(' movq $0,%rsi # load line number', sline);
                wrtins(' movq $DereferenceOfNilPointer,%rdx # load error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end
            end;

            56 {lca}: wrtins(' leaq string^0(%rip),%1 # load string constant address', ep^.strn, ep^.r1); 

            {grts,less}
            158,170: ; { are invalid }

            {equs,neqs,geqs,leqs}
            140,146,152,164: begin 
              case ep^.op of
                140: wrtins(' call psystem_setequ # check set equal');
                146: begin
                  wrtins(' call psystem_setequ # check set equal');
                  wrtins(' xor $0,%rax # invert equal status', 1);
                end;
                152,164: wrtins(' call psystem_setinc # check set inclusion');
              end;
              if ep^.r1 <> rgrax then
                wrtins(' movq %rax,%1 # move result to final register', ep^.r1);
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
              wrtins(' cmp %1,%2 # compare', ep^.r^.r1, ep^.l^.r1);
              case ep^.op of
                17,137,138,139,141: wrtins(' sete %1l # set equal', ep^.r1);
                18,143,144,145,147: wrtins(' setne %1l # set not equal', ep^.l^.r1);
                149,150,151,153: wrtins(' setge %1l # set greater or equal', ep^.l^.r1);
                155,156,157,159: wrtins(' setg %1l # set greater', ep^.l^.r1);
                161,162,163,165: wrtins(' setle %1l # set less or equal', ep^.l^.r1);
                167,168,169,171: wrtins(' setl %1l # set less', ep^.l^.r1)
              end;
              wrtins(' movsx %1l,%1 # sign extend boolean', ep^.l^.r1)
            end;

            {ordi,ordb,ordc,ordx}
            59, 134, 136, 200: ; { ord is a no-op }

            {lcp}
            135: begin 
              wrtins(' leaq ^0(%1),%2 # get length/template', ptrsize, ep^.l^.r1, ep^.r2);
              wrtins(' movq (%1),%1 # get pointer', ep^.l^.r1)
            end;

            {sgs}
            32: begin
              wrtins(' leaq ^-@s^0(%rbp),%rsi # index temp', ep^.r1a, lclspc^);
              wrtins(' call psystem_setsgl # make singleton set');
              wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^);
            end;

            {flt,flo}
            33,34: wrtins(' cvtsi2sd %1,%2 # convert integer to real', ep^.l^.r1, ep^.r1);

            {trc}
            35: begin
              if dochkovf then begin
                wrtins(' movsd real_int_max(%rip),%1 # load maximum int val', ep^.t1);
                wrtins(' cmpnltsd %1,%2 # compare real less or equal', ep^.l^.r1, ep^.t1);
                wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
                wrtins(' orq %1,%1 # check zero', ep^.t2);
                wrtins(' jz 2f # skip zero');
                wrtins(' movsd real_int_min(%rip),%1 # load minimum int val', ep^.t1);
                wrtins(' cmplesd %1,%2 # compare real greater or equal', ep^.l^.r1, ep^.t1);
                wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
                wrtins(' orq %1,%1 # check zero', ep^.t2);
                wrtins(' jnz 1f # skip not zero');
                wrtins('2:');
                wrtins(' leaq modnam(%rip),%rdi # load module name');
                wrtins(' movq $0,%rsi # load line number', sline);
                wrtins(' movq $RealArgumentTooLarge,%rdx # load error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end;                            
              wrtins(' cvttsd2si %1,%2 # trucate real to integer', ep^.l^.r1, ep^.r1);
            end;

            {ngi}
            36: wrtins(' negq %1 # negate integer', ep^.r1);

            {ngr}
            37: begin
              wrtins(' xorpd %1,%1  # clear register', ep^.r1, ep^.r1);
              wrtins(' subsd %1,%2  # find 0-real', ep^.l^.r1, ep^.r1)
            end;

            {sqi}
            38: begin 
              wrtins(' imulq %1,%1 # square integer', ep^.r1);
              if dochkovf then begin
                wrtins(' jno 1f # skip no overflow');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # set line number', sline);
                wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end
            end;

            {sqr}
            39: begin 
              wrtins(' mulsd %1,%1 # square real', ep^.r1);
            end;

            {abi}
            40: begin
              wrtins(' orq %1,%1 # check positive', ep^.r1);
              wrtins(' jns 1f # skip if so', ep^.r1);
              wrtins(' negq %1 # negate integer', ep^.r1);
              wrtins('1:');

            end;

            {abr}
            41: begin 
              wrtins(' movq $0x7fffffffffffffff,%1 # set mask', ep^.t1);
              wrtins(' movq %1,%2 # move to real', ep^.t1, ep^.t2);
              wrtins(' andpd %1,%2 # mask off sign bit', ep^.t2, ep^.r1)
            end;

            {notb}
            42: begin
              wrtins(' orq %1,%1 # test boolean', ep^.r1);
              wrtins(' movq $0,%1 # set true', 1, ep^.r1);
              wrtins(' jz 1f # skip if so', ep^.r2);
              wrtins(' movq $0,%1 # otherwise set false', ep^.r1);
              wrtins('1:')
            end;

            {noti}
            205: begin 
              if dodbgchk then begin
                wrtins(' orq %1,%1 # test signed', ep^.r1);
                wrtins(' jns 1f # skip if not');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # set line number', sline);
                wrtins(' movq $BooleanOperatorOfNegative,%rdx # set error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:');
              end;
              wrtins(' notq %1 # not integer', ep^.r1);
              wrtins(' movq $0,%1 # clear sign bit', pmmaxint, ep^.t1);
              wrtins(' andq %1,%2', pmmaxint, ep^.t1, ep^.r1)
            end;

            {odd}
            50: begin 
              wrtins(' andq $0,%1 # mask bit 0', 1, ep^.r1);
            end;

            {rnd}
            62: begin
              if dochkovf then begin
                wrtins(' movsd real_int_max(%rip),%1 # load maximum int val', ep^.t1);
                wrtins(' cmpnltsd %1,%2 # compare real less or equal', ep^.l^.r1, ep^.t1);
                wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
                wrtins(' orq %1,%1 # check zero', ep^.t2);
                wrtins(' jz 2f # skip zero');
                wrtins(' movsd real_int_min(%rip),%1 # load minimum int val', ep^.t1);
                wrtins(' cmplesd %1,%2 # compare real greater or equal', ep^.l^.r1, ep^.t1);
                wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
                wrtins(' orq %1,%1 # check zero', ep^.t2);
                wrtins(' jnz 1f # skip not zero');
                wrtins('2:');
                wrtins(' leaq modnam(%rip),%rdi # load module name');
                wrtins(' movq $0,%rsi # load line number', sline);
                wrtins(' movq $RealArgumentTooLarge,%rdx # load error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end;
              wrtins(' cvtsd2si %1,%2 # round to integer', ep^.l^.r1, ep^.r1)
            end;

            {chr}
            60: ; { chr is no-op }

            {and,ior,xor}
            43,44,206: begin 
              if dodbgchk then begin
                wrtins(' orq %1,%1 # check signed', ep^.l^.r1);
                wrtins(' jns 1f # skip if not');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # get line number', sline);
                wrtins(' movq $BooleanOperatorOfNegative,%rdx # get error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:');
                wrtins(' orq %1,%1 # check signed', ep^.r^.r1);
                wrtins(' jns 1f # skip if not');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # get line number', sline);
                wrtins(' movq $BooleanOperatorOfNegative,%rdx # get error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:');
              end;
              case ep^.op of
                43: wrtins(' andq %1,%2 # and integers', ep^.r^.r1, ep^.l^.r1);
                44: wrtins(' orq %1,%2 # or integers', ep^.r^.r1, ep^.l^.r1);
                206: wrtins(' xorq %1,%2 # xor integers', ep^.r^.r1, ep^.l^.r1)
              end
            end;

            {dif,int,uni}
            45,46,47: begin
              case ep^.op of 
                45: wrtins(' call psystem_setdif # find set difference');
                46: wrtins(' call psystem_setint # find set intersection');
                47: wrtins(' call psystem_setuni # find set union');
              end;
              wrtins(' leaq ^-@s^0(%rbp),%1 # reindex the temp', ep^.r1a, ep^.r1, lclspc^);
              puttmp(ep^.r^.r1a)
            end;

            {inn}
            48: begin
              wrtins(' call psystem_setsin # find set membership');
              if ep^.r1 <> rgrax then
                wrtins(' movq %rax,%1 # move result to target reg', ep^.r1);
              puttmp(ep^.r^.r1a)
            end;

            {mod}
            49: begin 
              wrtins(' cmpq $0,%1 # check zero divide', ep^.r^.r1);
              wrtins(' jg 1f # skip <= 0');
              wrtins(' leaq modnam(%rip),%rdi # index module name');
              wrtins(' movq $0,%rsi # set line number', sline);
              wrtins(' movq $InvalidDivisorToMod,%rdx # set error code');
              wrtins(' call psystem_errore # process error');
              wrtins('1:');
              wrtins(' xorq %rdx,%rdx # clear upper dividend');
              wrtins(' subq $0,%rax # find sign of dividend', ep^.r^.r1);
              wrtins(' jns 1f # skip positive', ep^.r^.r1);
              wrtins(' decq %rdx # set sign of upper dividend');
              wrtins('1:');
              wrtins(' idivq %1 # divide integer', ep^.r^.r1);
              if ep^.r1 <> rgrdx then
                wrtins(' movq %rdx,%1 # place result', ep^.r1)
            end;

            {dvi}
            53: begin 
              wrtins(' cmpq $0,%1 # check zero divide', ep^.r^.r1);
              wrtins(' jne 1f # skip no overflow');
              wrtins(' leaq modnam(%rip),%rdi # index module name');
              wrtins(' movq $0,%rsi # set line number', sline);
              wrtins(' movq $ZeroDivide,%rdx # set error code');
              wrtins(' call psystem_errore # process error');
              wrtins('1:');
              wrtins(' xorq %rdx,%rdx # clear upper dividend');
              wrtins(' subq $0,%rax # find sign of dividend', ep^.r^.r1);
              wrtins(' jns 1f # skip positive', ep^.r^.r1);
              wrtins(' decq %rdx # set sign of upper dividend ');
              wrtins('1:');
              wrtins(' idivq %1 # divide integer', ep^.r^.r1);
              if ep^.r1 <> rgrax then
                wrtins(' movq %rax,%1 # move result into place', ep^.r1)
            end;

            {mpi}
            51: begin
              wrtins(' imulq %1,%2 # multiply integers', ep^.r^.r1, ep^.l^.r1);
              if dochkovf then begin
                wrtins(' jno 1f # skip no overflow');
                wrtins(' leaq modnam(%rip),%rdi # index module name');
                wrtins(' movq $0,%rsi # set line number', sline);
                wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end
            end;

            {mpr}
            52: wrtins(' mulsd %1,%2 # multiply reals', ep^.r^.r1, ep^.l^.r1);

            {dvr}
            54: begin
              if dodbgchk then begin
                wrtins(' movsd real_zero(%rip),%1 # load real zero', ep^.t1);
                wrtins(' cmpeqsd %1,%2 # compare real equal', ep^.r^.r1, ep^.t1);
                wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
                wrtins(' orq %1,%1 # check zero', 1, ep^.t2);
                wrtins(' jz 1f # skip not zero', ep^.r^.r1);
                wrtins(' leaq modnam(%rip),%rdi # load module name');
                wrtins(' movq $0,%rsi # load line number', sline);
                wrtins(' movq $ZeroDivide,%rdx # load error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:');
              end;
              wrtins(' divsd %1,%2 # divide reals   ', ep^.r^.r1, ep^.l^.r1)
            end;

            {rgs}
            110: begin 
              wrtins(' leaq ^-@s^0(%rbp),%rdx # index temp', ep^.r1a, lclspc^);
              wrtins(' call psystem_setrgs # set range of values');
              wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^);
            end;

            { dupi, dupa, dupr, dups, dupb, dupc }
            181, 182, 183, 184, 185, 186: ;

            {cks}
            187: ;

            {csp}
            15: begin
              { need irregular handling for nwl and dsl }
              if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then callnwldsl(ep)
              else callsp(ep, sptable[ep^.q], spfunc[ep^.q])
            end;

            {sfr}
            245:
              if ep^.lb <> nil then begin
                if dodbgchk then begin
                  wrtins(' movq %rsp,%rax # copy stack pointer');
                  wrtins(' subq $s,%rax # set new stack depth', ep^.lb^);
                  wrtins('1:');
                  wrtins(' cmpq %rax,%rsp # check done');
                  wrtins(' jbe 2f # skip if below stack');
                  wrtins(' pushq $0 # clear stack'); 
                  wrtins(' jmp 1b # loop');
                  wrtins('2:')
                end else
                  wrtins(' subq $s,%rsp # set new stack depth', ep^.lb^);
              end;

            {cup,cuf}
            12, 246: begin
              genexp(ep^.sl); { process sfr start link }
              stkadrs := stkadr; { save stack track here }
              pshpar(ep^.pl); { process parameters first }
              if ep^.blk <> nil then begin
                write(prr, ' ':opcspc, 'call'); lftjst(parspc-(4+opcspc)); fl := parspc; 
                wrtblks(ep^.blk^.parent, true, fl); wrtblksht(ep^.blk, fl); 
                lftjst(cmtspc-fl); writeln(prr, '# call user procedure')
              end else wrtins(' call @s # call user procedure', ep^.fn^);
              if ep^.op = 246{cuf} then begin
                if ep^.rc = 1 then begin
                  if ep^.r1 <> rgxmm0 then
                    wrtins(' movq %xmm0,%1 # place result', ep^.r1)
                end else if ep^.rc = 2 then begin { move set from stack to temp }
                    wrtins(' movq %rsp,%rsi # index set on stack');
                    wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                    wrtins(' movsq # move');
                    wrtins(' movsq');
                    wrtins(' movsq');
                    wrtins(' movsq');
                    wrtins(' addq $0,%rsp # remove set from stack', setsize);
                    wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
                end else if ep^.rc = 3 then begin { move structure from stack to temp }
                    wrtins(' movq %rsp,%rsi # index structure on stack');
                    wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                    wrtins(' movq $0,%rcx # load size', ep^.q2, lclspc^);
                    wrtins(' repnz # move');
                    wrtins(' movsb');
                    wrtins(' addq $0,%rsp # remove structure from stack', ep^.q3);
                    wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)                
                end else begin
                  if ep^.r1 <> rgrax then
                    wrtins(' movq %rax,%1 # place result', ep^.r1);
                end
              end;
              stkadr := stkadrs { restore stack position }
            end;

            {cip,cif}
            113,247: begin
              genexp(ep^.sl); { process sfr start link }
              stkadrs := stkadr; { save stack track here }
              pshpar(ep^.pl); { process parameters first }
              genexp(ep^.l); { load procedure address }
              wrtins(' movq %rbp,%r15 # move our frame pointer to preserved register');
              wrtins(' movq ^0(%1),%rbp # set callee frame pointer', 1*ptrsize, ep^.l^.r1);
              wrtins(' call *(%1) # call indirect', ep^.l^.r1);
              if ep^.op = 247{cif} then begin 
                if ep^.rc = 1 then begin
                  if ep^.r1 <> rgxmm0 then
                    wrtins(' movq %xmm0,%1 # place result', ep^.r1)
                end else if ep^.rc = 2 then begin { move set from stack to temp }
                    wrtins(' movq %rsp,%rsi # index set on stack');
                    wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                    wrtins(' movsq # move');
                    wrtins(' movsq');
                    wrtins(' movsq');
                    wrtins(' movsq');
                    wrtins(' addq $0,%rsp # remove set from stack', setsize);
                    wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
                end else if ep^.rc = 3 then begin { move structure from stack to temp }
                    wrtins(' movq %rsp,%rsi # index structure on stack');
                    wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                    wrtins(' movq $0,%rcx # load size', ep^.q2, lclspc^);
                    wrtins(' repnz # move');
                    wrtins(' movsb');
                    wrtins(' addq $0,%rsp # remove structure from stack', ep^.q3);
                    wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
                end else begin
                  if ep^.r1 <> rgrax then
                    wrtins(' movq %rax,%1 # place result', ep^.r1)
                end
              end;
              wrtins(' movq %r15,%rbp # restore our frame pointer');
              stkadr := stkadrs { restore stack position }
            end;

            {cuv,cvf}
            27,249: begin
              genexp(ep^.sl); { process sfr start link }
              stkadrs := stkadr; { save stack track here }
              pshpar(ep^.pl); { process parameters first }
              if ep^.qs <> nil then wrtins(' call *@s(%rip) # call vectored', ep^.qs^)
              else wrtins(' call *@g(%rip) # call vectored', q);
              if ep^.op = 249{cvf} then begin
                if ep^.rc = 1 then begin
                  if ep^.r1 <> rgxmm0 then
                  wrtins(' movq %xmm0,%1 # place result', ep^.r1)
                end else if ep^.rc = 2 then begin { move set from stack to temp }
                    wrtins(' movq %rsp,%rsi # index set on stack');
                    wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                    wrtins(' movsq # move');
                    wrtins(' movsq');
                    wrtins(' movsq');
                    wrtins(' movsq');
                    wrtins(' addq $0,%rsp # remove set from stack', setsize);
                    wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
                end else if ep^.rc = 3 then begin { move structure from stack to temp }
                    wrtins(' movq %rsp,%rsi # index structure on stack');
                    wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                    wrtins(' movq $0,%rcx # load size', ep^.q2, lclspc^);
                    wrtins(' repnz # move');
                    wrtins(' movsb');
                    wrtins(' addq $0,%rsp # remove structure from stack', ep^.q3);
                    wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)                
                end else begin
                  if ep^.r1 <> rgrax then
                    wrtins(' movq %rax,%1 # place result', ep^.r1);
                end
              end;
              stkadr := stkadrs { restore stack position }
            end;

            {cke}
            188: begin
              wrtins(' movq $0,%1 # start running boolean', ep^.r2);
              ep2 := ep^.cl; 
              while ep2 <> nil do begin 
                ep2^.r1 := ep^.r1; ep2^.r2 := ep^.r2; ep2^.t1 := ep^.t1; 
                genexp(ep2); ep2 := ep2^.next 
              end;   
              wrtins(' jnz 1f # skip any variant active');
              wrtins(' leaq modnam(%rip),%rdi # set module name');
              wrtins(' movq $0,%rsi # set line number', sline);
              wrtins(' movq $VariantNotActive,%rdx # set error code');
              wrtins(' call psystem_errore # process error');
              wrtins('1:');
            end;

            {wbs}
            243: begin
              wrtins(' call psystem_withenter # establish with reference');
            end;

            {cxs}
            211: begin           
              wrtins(' decq %1 # 0 base index', ep^.r^.r1);
              if dodbgchk then begin
                wrtins(' cmpq %1,%2 # check index < length', ep^.l^.r2, ep^.r^.r1);
                wrtins(' jb 1f # skip below');
                wrtins(' leaq modnam(%rip),%rdi # load module name');
                wrtins(' movq $0,%rsi # load line number', sline);
                wrtins(' movq $ValueOutOfRange,%rdx # load error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:');
              end;
              wrtins(' movq $0,%rax # get element size', ep^.q);
              wrtins(' mulq %1 # find index*size', ep^.r^.r1);
              wrtins(' addq %rax,%1 # add to base', ep^.l^.r1);
              if ep^.r1 <> ep^.l^.r1 then
                wrtins(' movq %rax,%1 # move to result', ep^.r1)
            end;

            {cxc}
            212:begin
              { ep^.l^.r1: base addr, ep^.l^.r2: template addr, ep^.t1: temp reg}
              wrtins(' decq %1 # 0 base index', ep^.r^.r1);
              if dodbgchk then begin
                wrtins(' cmpq (%1),%2 # check index < length', ep^.l^.r2, ep^.r^.r1);
                wrtins(' jb 1f # skip below ');
                wrtins(' leaq modnam(%rip),%rdi # load module name');
                wrtins(' movq $0,%rsi # load line number', sline);
                wrtins(' movq $ValueOutOfRange,%rdx # load error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:        ');
              end;
              wrtins(' movq $0,%1 # get # levels-1', ep^.q-1, ep^.t1);
              wrtins(' movq $0,%rax # get base element size', ep^.q1);
              wrtins(' movq %1,%rdx # copy template address', ep^.l^.r2);
              wrtins('1:');
              wrtins(' addq $0,%rdx # next template location', intsize);
              wrtins(' mulq (%rdx) # add template to size');
              wrtins(' subq $0,%1 # count down levels', 1, ep^.t1);
              wrtins(' jnz 1b # loop over templates');     
              wrtins(' addq $0,%1 # advance template slot', intsize, ep^.l^.r2);  
              wrtins(' mulq %1 # find index*size', ep^.r^.r1);
              wrtins(' addq %rax,%1 # add to base', ep^.l^.r1);   
            end;

            {lft} 
            213: wrtins(' leaq @s(%rip),%1 # index template', q, ep^.r2, ep^.lt^);

            {max} 
            214: begin
              if dodbgchk then begin
                wrtins(' cmpq $0,%1 # chk lvl < 1', 1, ep^.r^.r1);
                wrtins(' jb 2f # skip if below');
                wrtins(' cmpq $0,%1 # compare', ep^.q, ep^.r^.r1);
                wrtins(' jbe 1f # skip if less or equal');
                wrtins('2:');
                wrtins(' leaq modnam(%rip),%rdi # load module name');
                wrtins(' movq $0,%rsi # load line number', sline);
                wrtins(' movq $InvalidContainerLevel,%rdx # load error code');
                wrtins(' call psystem_errore # process error');
                wrtins('1:')
              end;
              if ep^.q <> 1 then begin
                wrtins(' movq $0,%1 # get total lvl', ep^.q, ep^.t1);
                wrtins(' subq %1,%2 # find tl-al', ep^.r^.r1, ep^.t1);
                wrtins(' salq $4,%1 # *16 (long)', ep^.t1);
                wrtins(' addq %1,%2 # add to base template', ep^.l^.r2, ep^.t1);
                wrtins(' movq (%1),%2 # add to base template', ep^.t1, ep^.r1)
              end
            end;

            {equv,neqv,lesv,grtv,leqv,geqv} 
            215,216,217,218,219,220: begin
              wrtins(' call psystem_strcmp # compare strings'); 
              wrtins(' cmpq $0,%rax # compare -0+ result');
              case ep^.op of
                215{equv}: wrtins(' sete %1l # set equal', ep^.r1);
                216{neqv}: wrtins(' setne %1l # set not equal', ep^.r1);
                220{geqv}: wrtins(' setge %1l # set greater or equal', ep^.r1);
                218{grtv}: wrtins(' setg %1l # set greater', ep^.r1);
                219{leqv}: wrtins(' setle %1l # set less or equal', ep^.r1);
                217{lesv}: wrtins(' setl %1l # set less', ep^.r1);
              end;
              wrtins(' movsx %1l,%1 # sign extend boolean', ep^.r1)
            end;

            {spc} 
            222: begin
              wrtins(' movq (%1),%1 # fetch template length', ep^.l^.r2)
            end;

            {ccs} 
            223: begin
              if ep^.q = 1 then begin
                wrtins(' movq $0,%rax # get base element size', ep^.q1);
                wrtins(' mulq %1 # find base size*len', ep^.l^.r2);
                wrtins(' movq %rax,%1 # move to total length', ep^.t2);
              end else begin
                wrtins(' movq $0,%1 # get # levels', ep^.q, ep^.t1);
                wrtins(' movq $0,%1 # get base element size', ep^.q1, ep^.t2);
                wrtins(' movq %1,%2 # copy template address', ep^.l^.r2, ep^.t3);
                wrtins('1:');
                wrtins(' movq (%1),%rax # get size from template', ep^.t3);
                wrtins(' mulq %1 # multiply by size', ep^.t2);
                wrtins(' movq %rax,%1 # add template to size', ep^.t2);
                wrtins(' addq $0,%1 # next template location', intsize, ep^.t3);
                wrtins(' decq %1 # count down levels', ep^.t1);
                wrtins(' jnz 1b # loop over templates'); 
              end;
              wrtins(' subq %1,%rsp # allocate on stack', ep^.t2);  
              wrtins(' andq $0xfffffffffffffff0,%rsp # align stack');
              wrtins(' movq %1,%rsi # move source', ep^.l^.r1, ep^.t3);
              wrtins(' movq %rsp,%rdi # move dest', ep^.l^.r1, ep^.t3);
              wrtins(' movq %1,%rcx # move source', ep^.t2);
              wrtins(' repnz # move');
              wrtins(' movsb');
              wrtins(' movq %rsp,%1 # index copy', ep^.r1);
              wrtins(' movq %1,%2 # index template', ep^.l^.r2, ep^.r2)
            end;

            {ldp} 
            225: begin
              wrtins(' movq ^0(%1),%2 # get template adr', intsize, ep^.l^.r1, ep^.r2);
              wrtins(' movq (%1),%2 # get data adr', ep^.l^.r1, ep^.r1);
            end;

            {mpc}
            248: ; { registers are all assigned }

            {cpl}
            251: ; { registers are all assigned }

          end;
          for r := rgxmm15 downto rgrax do if r in ep^.rs then begin
            if r in [rgrax..rgr15] then begin
              wrtins(' popq %1 # restore used quad register', r);
              stkadr := stkadr-intsize
            end else begin
              wrtins(' movsd (%rsp),%1 # restore used real register', r);
              wrtins(' addq $0,%rsp # remove from stack', realsize);
              stkadr := stkadr-intsize
            end
          end;
          write(prr, '# generating~: '); dmpety(prr, ep); writeln(prr)
        end
      end;

      { get number of parameters of procedure/function/system call to parameters
        list }
      procedure getparn(ep: expptr; pn: integer);
      var pp: expptr;
      begin
          { pull parameters into list in reverse }
          while (pn > 0) and (estack <> nil) do 
            begin popstk(pp); pp^.next := ep^.pl; ep^.pl := pp; pn := pn-1 end
      end;

      { get parameters of procedure/function/system call to parameters list }
      procedure getpar(ep: expptr);
      begin
          ep^.sl := nil;
          { for function overloads, sfr can be on top }
          if estack^.op = 245{sfr} then popstk(ep^.sl); { get sfr start }
          getparn(ep, ep^.pn); { get those parameters into list }
          if ep^.sl = nil then popstk(ep^.sl); { get sfr start }
          if ep^.sl^.op <> 245{sfr} then errorl('system error');
      end;

      { reverse parameters list }
      procedure revpar(ep: expptr);
      var pl, pp: expptr;
      begin
        pl := nil;
        while ep^.pl <> nil do begin 
          pp := ep^.pl; ep^.pl := ep^.pl^.next;
          pp^.next := pl; pl := pp
        end;
        ep^.pl := pl
      end;

      { reorder last parameter }
      procedure ordpar(ep: expptr);
      var lp, pp: expptr;
      begin
        pp := ep^.pl;
        lp := nil;
        if pp <> nil then 
          while pp^.next <> nil do begin lp := pp; pp := pp^.next end;
        if (pp = nil) or (lp = nil) then errorl('system error');
        lp^.next := nil;
        pp^.next := ep^.pl;
        ep^.pl := pp
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
          getexp(d); d^ := s^; d^.next := nil; d^.sl := nil; d^.al := nil; 
          d^.pl := nil;
          duptre(s^.l, d^.l); duptre(s^.r, d^.r); duptre(s^.x1, d^.x1);
          duptre(s^.cl, d^.cl)
        end
      end;

      { pass though the rest of the line }
      procedure pass;
      begin
        while (ch <> '!') and not eoln(prd) do getnxt;
        while not eoln(prd) do begin write(prr, ch); getnxt end;
        writeln(prr, ch)
      end;

      function alflen(var s: alfa): integer;
      var i: alfainx; len: integer;
      begin
        i := 1; while (i < maxalfa) and (s[i] <> ' ') do i := i+1;
        if s[i] = ' ' then alflen := i-1 else alflen := i
      end;
     
      procedure par;
      begin
        lftjst(parfld); pass
      end;

      procedure parp;
      begin
        read(prd,p); write(prr,p:1); 
        lftjst(parfld-digits(p)); pass
      end;

      procedure parq;
      begin
        read(prd,q); write(prr,q:1); 
        lftjst(parfld-digits(q)); pass
      end;

      procedure parpq;
      begin
        read(prd,p,q); write(prr,p:1,' ',q:1); 
        lftjst(parfld-(digits(p)+1+digits(q))); pass
      end;

      procedure parqq;
      begin
        read(prd,q,q1); write(prr,q:1,' ',q1:1); 
        lftjst(parfld-digits(q)+1+digits(q1)); pass
      end;

      { evaluate and push n arguments depth first }
      procedure pshexps(n: integer);
      var ep: expptr; frereg: regset;
      begin
        if n > 0 then begin
          pshexps(n-1);
          frereg := allreg; popstk(ep); assreg(ep, frereg, rgnull, rgnull);
          dmptre(ep); genexp(ep);
          wrtins(' pushq %1 # place on stack', ep^.r1)
        end
      end;

    begin { assemble } 
      p := 0;  q := 0;  q1 := 0; q2 := 0; q3 := 0; q4 := 0; op := 0; stkadr := 0;
      getname;
      { note this search removes the top instruction from use }
      while (instr[op]<>name) and (op < maxins) do op := op+1;
      if op = maxins then errorl('illegal instruction');
      prtline; write(prr, op:3, ': ', name:alflen(name)); lftjst(8-alflen(name));
      case op of

        { *** non-terminals *** }

        {lodi,lodx,loda,lodr,lods,lodb,lodc,lda}
        0,193,105,106,107,108,109,4: begin parpq;
          q1 := -p*ptrsize; getexp(ep); attach(ep); pshstk(ep) 
        end;

        {adi,adr,sbi,sbr}
        28, 29, 30, 31: begin par;
          getexp(ep); popstk(ep^.r);
          popstk(ep^.l); pshstk(ep)
        end;

        {lip} 
        120: begin parpq;
          q1 := -p*ptrsize; getexp(ep); pshstk(ep) 
        end;

        { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
        142, 148, 154, 160, 166, 172: begin parq;
          getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
        end;

        {lao} 
        5: begin while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
          sp := nil;
          if prd^ = 'l' then begin 
            getnxt; labelsearch(def, val, sp, blk);
            write(prr, p:1, ' l '); write(prr, sp^); 
            lftjst(parfld-(digits(p)+3+max(sp^))); pass
          end else parq;
          getexp(ep); ep^.fl := sp; attach(ep); pshstk(ep)
        end;

        {lto} 
        234: begin labelsearch(def, val, sp, blk); write(prr, 'l '); 
          write(prr, sp^); lftjst(parfld-(2+max(sp^))); pass;
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
          while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
          sp := nil;
          if prd^ = 'l' then begin 
            getnxt; labelsearch(def, val, sp, blk);
            write(prr, p:1, ' l '); write(prr, sp^); 
            lftjst(parfld-(digits(p)+3+max(sp^))); pass
          end else parq;
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

        {ckvi,ckvb,ckvc,ckvx}
        175, 179, 180, 203: begin parq;
          getexp(ep); pshstk(ep) 
        end;

        {cpc}
        177: begin par;
          getexp(ep); popstk(ep2); popstk(ep3);
          duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); deltre(ep)
        end;

        {lpa}
        114: begin read(prd,p); labelsearch(def, val, sp, blk); 
          write(prr, p:1, ' l '); write(prr, sp^); 
          lftjst(parfld-(digits(p)+3+max(sp^))); pass;
          q1 := -p*ptrsize; getexp(ep); ep^.fn := sp; pshstk(ep);
        end;

        {ldcs,ldci,ldcr,ldcn,ldcb,ldcc}
        7, 123, 124, 125, 126, 127: begin case op of

          123: begin read(prd,i); write(prr, i:1); lftjst(parfld-digits(i)); 
            pass;
            getexp(ep); attach(ep); ep^.vi := i; pshstk(ep) 
          end;

          124: begin read(prd,r); write(prr, r); lftjst(parfld-23); pass;
            getexp(ep); attach(ep);
            pshstk(ep); new(cstp); cstp^.ct := creal; 
            cstp^.r := r; realnum := realnum+1; 
            cstp^.realn := realnum; cstp^.next := csttbl; 
            csttbl := cstp; ep^.realn := realnum 
          end;

          125: begin par;
            getexp(ep); pshstk(ep) 
          end;

          126: begin read(prd,i); write(prr, i:1); lftjst(parfld-digits(i)); 
            pass;
            getexp(ep); attach(ep); ep^.vi := i; pshstk(ep) 
          end;

          127: begin
            skpspc;
            if ch in ['0'..'9'] then begin i := 0;
              while ch in ['0'..'9'] do
                begin i := i*10+ord(ch)-ord('0'); getnxt end;
              c := chr(i);
              write(prr, i:1); lftjst(parfld-digits(i)); pass
            end else begin
              if ch <> '''' then errorl('illegal character');
              getnxt;  c := ch;
              getnxt;
              if ch <> '''' then errorl('illegal character');
              write(prr, '''', c, ''''); lftjst(parfld-3); pass
            end;
            getexp(ep); attach(ep); ep^.vi := ord(c); pshstk(ep)
          end;

          7: begin skpspc;
            if ch <> '(' then errorl('ldcs() expected');
            s := [ ];  getnxt; write(prr, '('); fl := 1;
            while ch<>')' do
              begin read(prd,s1); write(prr, s1, ' '); fl := fl+digits(s1)+1; 
                    getnxt; s := s + [s1] end;
            write(prr, ')'); fl := fl+1;
            getexp(ep); attach(ep); pshstk(ep);
            lftjst(parfld-fl); pass;
            new(cstp); cstp^.ct := cset; cstp^.s := s;
            setnum := setnum+1; cstp^.setn := setnum;
            cstp^.next := csttbl; csttbl := cstp; ep^.setn := setnum
          end

          end (*case*)
        end;

        {chki,chks,chkb,chkc,ckla,chkx}
        26, 97, 98, 99, 190, 199: begin read(prd,lb,ub); 
          write(prr, lb:1, ' ', ub:1); lftjst(parfld-(digits(lb)+1+digits(ub)));
          pass;
          getexp(ep); popstk(ep^.l); 
          pshstk(ep); ep^.vi := lb; ep^.vi2 := ub
        end;

        {chka}
        95: begin read(prd,lb,ub); 
          write(prr, lb:1, ' ', ub:1); lftjst(parfld-(digits(lb)+1+digits(ub)));
          pass;
          if lb <> 0 then begin getexp(ep); popstk(ep^.l); pshstk(ep) end
        end;

        {lca}
        56: begin read(prd,l); write(prr, l:1, ' '); fl := digits(l)+1; skpspc;
          for i := 1 to strlen do str[i] := ' ';
          if ch <> '''' then errorl('bad string format');
          i := 0;
          repeat
            if eoln(prd) then errorl('unterminated string');
            getnxt;
            c := ch; if (ch = '''') and (prd^ = '''') then 
              begin getnxt; c := ' ' end;
            if c <> '''' then begin
              if i >= strlen then errorl('string overflow');
              str[i+1] := ch; { accumulate string }
              i := i+1
            end
          until c = '''';
          getexp(ep); attach(ep); pshstk(ep);
          write(prr, '"', str:l,'"'); fl := fl+1+l+1; lftjst(parfld-fl); pass;
          new(cstp); cstp^.ct := cstr; cstp^.str := strp(str); 
          cstp^.strl := l; strnum := strnum+1; cstp^.strn := strnum;
          cstp^.next := csttbl; csttbl := cstp; ep^.strn := strnum
        end;

        {grts,less}
        158,170: errorl('Invalid operand');

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
        167, 168, 169, 171: begin par;
          getexp(ep); 
          { reverse order for leqs }
          if op = 164 then begin popstk(ep^.l); popstk(ep^.r) end
          else begin popstk(ep^.r); popstk(ep^.l) end;
          pshstk(ep)
        end;

        {brk}
        19: ; { unused }

        {ord}
        59, 134, 136, 200: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep);
        end;

        {lcp}
        135: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {sgs}
        32: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;
 
        {flt}
        33: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {flo}
        34: begin par; 
          getexp(ep); popstk(ep2); popstk(ep^.l);
          pshstk(ep); pshstk(ep2)
        end;

        {trc}
        35: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep); 
        end;

        {ngi,ngr}
        36,37: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {sqi,sqr}
        38,39: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep)
        end;

        {abi,abr}
        40,41: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {notb,odd,chr,rnd,noti}
        42,50,60,62,205: begin par; 
          getexp(ep); popstk(ep^.l); pshstk(ep)
        end;

        {and,ior,xor,dif,int,uni,inn,mod,mpi,mpr,dvi,dvr,rgs}
        43,44,45,46,47,48,49,51,52,53,54,110,206: begin par;
          getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
        end;

        { At this level we just duplicate the tree. At lower levels we can
          optimize this. }

        { dupi, dupa, dupr, dups, dupb, dupc }
        181, 182, 183, 184, 185, 186: begin par; 
          ep2 := nil;
          if estack <> nil then if estack^.op = 188{cke} then popstk(ep2);
          duptre(estack, ep); pshstk(ep);
          if ep2 <> nil then pshstk(ep2)
        end;

        {cks}
        187: begin par; 
          getexp(ep); pshstk(ep)
        end;

        {sfr}
        245: begin labelsearch(def, val, sp, blk); write(prr, 'l '); 
          write(prr, sp^); lftjst(parfld-(2+max(sp^))); pass;
          getexp(ep); pshstk(ep);
          ep^.lb := nil;
          if (def and (val <> 0)) or not def then ep^.lb := sp
        end;

        {cuf}
        246: begin labelsearch(def, val, sp, blk); write(prr, 'l '); 
          write(prr, sp^); read(prd,q,q1,q2,q3); write(prr, ' ', q:1, ' ', q1:1, ' ', q2:1, ' ', q3:1); 
          lftjst(parfld-(2+max(sp^)+1+digits(q)+1+digits(q1)+1+digits(q2)+1+digits(q3))); pass;
          getexp(ep); ep^.fn := sp; ep^.pn := q; ep^.rc := q1; ep^.blk := blk;
          getpar(ep); pshstk(ep);
        end;

        {cif}
        247: begin 
          read(prd,q,q1,q2, q3); write(prr,q:1,' ',q1:1, ' ', q2:1, ' ', q3:1); 
          lftjst(parfld-digits(q)+1+digits(q1)+1+digits(q2)+1+digits(q3)); pass;
          getexp(ep); ep^.pn := q; ep^.rc := q1; popstk(ep^.l); getpar(ep);
          pshstk(ep);
        end;

        {cvf}
        249: begin 
          while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
          sp := nil;
          if prd^ = 'l' then begin 
            getnxt; labelsearch(def, val, sp, blk);
            write(prr, p:1, ' l '); write(prr, sp^); 
            read(prd,q1,q2,q3,q4); write(prr, q1:1, ' ', q2:1, ' ', q3:1, ' ', q4:1);
            lftjst(parfld-(3+max(sp^)+1+digits(q1)+1+digits(q2)+1+digits(q3)+1+digits(q4)))
          end else begin 
            read(prd,q,q1,q2,q3,q4); write(prr,q,' ',q1, ' ', q2:1, ' ', q3:1, ' ', q4:1); 
            lftjst(parfld-(digits(q)+1+digits(q1)+1+digits(q2)+1+digits(q3)+1+digits(q4)))
          end;
          pass;
          getexp(ep); ep^.qs := sp; ep^.pn := q1; getpar(ep);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
          genexp(ep); deltre(ep)
        end;

        {cke}
        188: begin par; 
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

        {wbs}
        243: begin par;
          getexp(ep);
          popstk(ep^.l);
          pshstk(ep)
        end;

        {cxs}
        211: begin parq;
          getexp(ep);
          popstk(ep^.r); popstk(ep^.l);
          pshstk(ep)
        end;

        {cxc}
        212: begin parqq;
          getexp(ep);
          popstk(ep^.r); popstk(ep^.l);
          pshstk(ep)
        end;

        {lft} 
        213: begin labelsearch(def, val, sp, blk); write(prr, 'l '); 
          write(prr, sp^); lftjst(parfld-(2+max(sp^))); pass;
          getexp(ep);
          popstk(ep^.l); ep^.lt := sp;
          pshstk(ep)
        end;

        {max} 
        214: begin parq;
          getexp(ep);
          popstk(ep^.r); popstk(ep^.l);
          pshstk(ep)
        end;

        {equv,neqv,lesv,grtv,leqv,geqv} 
        215,216,217,218,219,220: begin par;
          getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
        end;

        {spc} 
        222: begin par;
          getexp(ep);
          popstk(ep^.l);
          pshstk(ep)
        end;

        {ccs} 
        223: begin parqq;
          getexp(ep);
          popstk(ep^.l);
          pshstk(ep)
        end;

        {ldp} 
        225: begin par;
          getexp(ep);
          popstk(ep^.l);
          pshstk(ep)
        end;

        {mpc}
        248: begin parqq;
          ep4 := nil; popstk(ep2); i := q;
          while i > 0 do begin
            ep2^.next := ep4;
            ep4 := ep2;
            popstk(ep2);
            i := i-1
          end;
          getexp(ep); popstk(ep3);
          if q1 = 0 then begin ep^.l := ep2; ep^.r := ep3 end
          else begin ep^.l := ep3; ep^.r := ep2 end;
          pshstk(ep);
          while ep4 <> nil do begin ep := ep4; ep4 := ep4^.next; pshstk(ep) end;
        end;

        { cpl }
        251: begin par;
          getexp(ep);
          popstk(ep2); duptre(ep2, ep3); pshstk(ep2);
          ep^.l := ep3; pshstk(ep)
        end;

        { *** calls can be terminal or non-terminal *** }

        {csp} 
        15: begin skpspc; getname;
          while name<>sptable[q] do begin 
            q := q+1; if q > maxsp then errorl('std proc/func not found')
          end; 
          write(prr, sptable[q]:alflen(sptable[q])); 
          lftjst(parfld-alflen(sptable[q])); pass;
          getexp(ep); 
          if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then 
            begin getparn(ep, maxint); revpar(ep); ordpar(ep) end
          else getparn(ep, sppar[q]);
          if spfunc[q] then pshstk(ep) { non-terminal, stack it }
          else begin { terminal, execute here }
            if spkeep[ep^.q] then begin
              if ep^.pl = nil then errorl('System error');
              duptre(ep^.pl, ep2); pshstk(ep2)
            end;
            frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
            dmptre(ep); genexp(ep);
            deltre(ep)
          end
        end;

        {cuv}
        27: begin 
          while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
          sp := nil;
          if prd^ = 'l' then begin 
            getnxt; labelsearch(def, val, sp, blk);
            write(prr, p:1, ' l '); write(prr, sp^); 
            read(prd,q1); write(prr, q1:1);
            lftjst(parfld-(3+max(sp^)+1+digits(q1)))
          end else begin 
            read(prd,q,q1); write(prr,q,' ',q1); 
            lftjst(parfld-(digits(q)+1+digits(q1)))
          end;
          pass;
          getexp(ep); ep^.qs := sp; ep^.pn := q1; getpar(ep);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
          genexp(ep); deltre(ep)
        end;

        { *** terminals *** }

        {cvbi,cvbx,cvbb,cvbc}
        100, 115, 116, 121,
        {ivti,ivtx,ivtb,ivtc,cta}
        192,101,102,111,191: begin read(prd,q, q1); 
          labelsearch(def, val, sp, blk); 
          write(prr,q:1, ' ', q1:1, ' l '); write(prr, sp^); 
          lftjst(parfld-(digits(q)+1+digits(q1)+3+max(sp^))); pass;
          getexp(ep); ep^.lt := sp; popstk(ep2); popstk(ep3); 
          duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); deltre(ep)
        end;

        {cup}
        12: begin labelsearch(def, val, sp, blk); write(prr, 'l '); 
          write(prr, sp^); read(prd, q1); write(prr, ' ', q1:1); 
          lftjst(parfld-(2+max(sp^)+1+digits(q1))); pass;
          getexp(ep); ep^.fn := sp; ep^.pn := q1; ep^.blk := blk; getpar(ep);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
          genexp(ep); deltre(ep);
        end;

        {cip}
        113: begin parq;
          getexp(ep); ep^.pn := q; popstk(ep^.l); getpar(ep);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
          genexp(ep); deltre(ep);
        end;

        {rip}
        13: parq;

        {stri,stra}
        2,70: begin parpq;
          frereg := allreg;
          popstk(ep); attach(ep); if p <> blkstk^.lvl then getreg(r1, frereg); 
          assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          if p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
            wrtins(' movq %1,@l(%2) # store qword', q, p, ep^.r1, r1)
          end else 
            wrtins(' movq %1,@l(%rbp) # store qword', q, p, ep^.r1);
          deltre(ep)
        end;

        {strx,strb,strc} 
        195,73,74: begin parpq;
          frereg := allreg; if p <> blkstk^.lvl then getreg(r1, frereg);
          popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          if p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
            wrtins(' movb %1l,@l(%2) # store byte', q, p, ep^.r1, r1)
          end else
            wrtins(' movb %1l,@l(%rbp) # store byte', q, p, ep^.r1);
          deltre(ep)
        end;

        {strr}
        71: begin parpq;
          frereg := allreg; if p <> blkstk^.lvl then getreg(r1, frereg);
          popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          if p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
            wrtins(' movsd %1,@l(%2) # store real', q, p, ep^.r1, r1)
          end else
            wrtins(' movsd %1,@l(%rbp) # store real', q, p, ep^.r1);
          deltre(ep)
        end;

        {strs} 
        72:begin parpq;
          frereg := allreg; popstk(ep); attach(ep); assreg(ep, frereg, rgrsi, rgnull); 
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          if p <> blkstk^.lvl then
            wrtins(' movq ^0(%rbp),%rdi # get display pointer', -p*ptrsize)
          else
            wrtins(' movq %rbp,%rdi # get display pointer', -p*ptrsize);
          wrtins(' leaq @l(%rdi),%rdi # index destination', q, p);
          wrtins(' movsq # move set');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' movsq');
          puttmp(ep^.r1a); deltre(ep)
        end;

        {sev}
        253: begin parpq;
          frereg := allreg-[rgrax];
          getreg(r1, frereg); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' popq %rax # get exception vector'); 
          wrtins(' pushq %rax # replace'); 
          if p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
            wrtins(' movq %rax,@l(%1) # store qword', q, p, r1)
          end else
            wrtins(' movq %rax,@l(%rbp) # store qword', q, p)
        end;

        {mst}
        11: begin read(prd,p); labelsearch(def, val, lclspc, blk); 
          labelsearch(def2, val2, sp2, blk);
          write(prr,p:1, ' l '); write(prr, lclspc^); write(prr, ' l '); 
          write(prr, sp2^); lftjst(parfld-(digits(p)+3+max(lclspc^)+3+max(sp2^))); pass;
          if blkstk <> nil then
            if blkstk^.btyp in [btproc, btfunc] then begin
              write(prr, '        .globl   '); wrtblklng(blkstk); writeln(prr);
              write(prr, '        .type    '); wrtblklng(blkstk); writeln(prr, ', @function');
              wrtblklabs(blkstk);
            end;
          frereg := allreg;
          { We limit to the enter instruction }
          if p >= 32 then errorl('Too many nested levels');
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' pushq $0 # place current ep');
          wrtins(' pushq $0 # place bottom of stack');
          wrtins(' pushq $0 # place previous ep');
          wrtins(' enterq $1,$0 # enter frame', p+1);
          wrtins(' movq %rsp,%rax # copy sp');
          { find sp-locals }
          write(prr, '        subq    $'); write(prr, lclspc^); write(prr, '+'); 
          write(prr, blkstk^.tmpnam^); writeln(prr, ',%rax # find sp-locals');
          wrtins('1:', lclspc^);
          wrtins(' cmpq %rax,%rsp # check have reached stack');
          wrtins(' je 2f # skip if so');
          wrtins(' pushq $0 # push 0 word for locals');
          wrtins(' jmp 1b # loop', 7);
          wrtins('2:', lclspc^);
          wrtins(' movq %rsp,^0(%rbp) # set bottom of stack', marksb);
          { note there is no way to know locals space in advance }
          wrtins(' andq $0xfffffffffffffff0,%rsp # align stack');
          { save protected registers and keep aligned }
          wrtins(' pushq %rbx # save protected registers and keep aligned');
          wrtins(' pushq %r12');
          wrtins(' pushq %r13');
          wrtins(' pushq %r14');
          wrtins(' pushq %r15');
          wrtins(' pushq %r15 # second push aligns');
          tmpoff := -(p+1)*ptrsize;
          tmpspc := 0; { clear temps }
          stkadr := 0;
          { note ep is unused at this time }
          botstk
        end;

        {mov}
        55: begin parq;
          frereg := allreg; popstk(ep); popstk(ep2); dmptre(ep); dmptre(ep2);
          assreg(ep2, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
          assreg(ep, frereg, rgrsi, rgnull);
          genexp(ep2); genexp(ep);
          wrtins(' movq $0,%rcx # load the length of move', q);
          wrtins(' repnz # move/copy');
          wrtins(' movsb');
          deltre(ep); deltre(ep2);
          botstk
        end;

        {dmp}
        117: begin parq;
          { unfortunately case statements can jump to an expected dump }
          if estack <> nil then begin popstk(ep); deltre(ep) end
        end;

        {sroi,sroa,sror,srob,sroc,srox}
        3, 75, 76, 78, 79, 196: begin
          while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
          sp := nil;
          if prd^ = 'l' then begin 
            getnxt; labelsearch(def, val, sp, blk);
            write(prr, p:1, ' l '); write(prr, sp^); 
            lftjst(parfld-(digits(p)+3+max(sp^))); pass
          end else parq;
          frereg := allreg;
          popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); dmptre(ep); 
          genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          if (op = 78{srob}) or (op = 79){sroc} or (op = 196){srox} then begin
            if sp <> nil then
              wrtins(' movb %1l,@s(%rip) # store byte to global', ep^.r1, sp^)
            else
              wrtins(' movb %1l,@g(%rip) # store byte to global', q, ep^.r1)
          end else if op = 76{sror} then begin
            if sp <> nil then
              wrtins(' movsd %1l,@s(%rip) # store real to global', ep^.r1, sp^)
            else
              wrtins(' movsd %1l,@g(%rip) # store real to global', q, ep^.r1)
          end else begin {sroi, sroa}
            if sp <> nil then
              wrtins(' movq %1,@s(%rip) # store quad to global', q, ep^.r1, sp^)
            else
              wrtins(' movq %1,@g(%rip) # store quad to global', q, ep^.r1)
          end;
          deltre(ep)
        end;

        {sros}
        77: begin
          while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
          sp := nil;
          if prd^ = 'l' then begin 
            getnxt; labelsearch(def, val, sp, blk);
            write(prr, p:1, ' l '); write(prr, sp^); 
            lftjst(parfld-(digits(p)+3+max(sp^))); pass
          end else parq;
          frereg := allreg;
          popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); dmptre(ep); 
          genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' leaq ^-@s^0(%rbp),%rsi # index temp set', ep^.r1a, lclspc^);
          if sp <> nil then
            wrtins(' leaq @g(%rip),%rdi # index global destination', ep^.r1, sp^)
          else
            wrtins(' leaq @g(%rip),%rdi # index global destination', q, ep^.r1);
          wrtins(' movsq # move');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' movsq');
          puttmp(ep^.r1a); deltre(ep)
        end;

        {aps}
        178: begin parq;
          frereg := allreg; popstk(ep2); popstk(ep);
          assreg(ep, frereg, rgrdi, rgrcx); frereg := frereg-[rgrdi, rgrcx];
          assreg(ep2, frereg, rgrsi, rgnull);
          dmptre(ep); genexp(ep);
          dmptre(ep2); genexp(ep2);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq $0,%rax # get size', q);
          wrtins(' mulq %rcx # find len*size');
          wrtins(' movq %rax,%rcx # place size');
          wrtins(' repnz # move data  ');
          wrtins(' movsb    ');
          dmptre(ep); deltre(ep2); deltre(ep); 
          botstk  
        end; 

        {pck}
        63: begin parqq;
          frereg := allreg; popstk(ep);
          popstk(ep2); popstk(ep3); dmptre(ep3); dmptre(ep2); dmptre(ep);
          assreg(ep, frereg, rgrdx, rgnull); frereg := frereg-[rgrdx];
          assreg(ep2, frereg, rgrcx, rgnull); frereg := frereg-[rgrcx];
          assreg(ep3, frereg, rgr8, rgnull);
          genexp(ep); genexp(ep2); genexp(ep3);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq $0,%rdi # get size of packed array', q);
          wrtins(' movq $0,%rsi # get size of unpacked array', q1);
          wrtins(' call psystem_pack # pack the array');
          deltre(ep); deltre(ep2); deltre(ep3); 
          botstk 
        end;

        {upk}
        64: begin parqq;
          frereg := allreg; popstk(ep);
          popstk(ep2); popstk(ep3); dmptre(ep3); dmptre(ep2); dmptre(ep); 
          assreg(ep, frereg, rgrdx, rgnull); frereg := frereg-[rgrdx];
          assreg(ep2, frereg, rgrcx, rgnull); frereg := frereg-[rgrcx];
          assreg(ep3, frereg, rgr8, rgnull);
          genexp(ep); genexp(ep2); genexp(ep3);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq $0,%rdi # load size of packed array', q);
          wrtins(' movq $0,%rsi # load size of unpacked array', q1);
          wrtins(' call psystem_unpack # unpack the array');
          deltre(ep); deltre(ep2); deltre(ep3); 
          botstk 
        end;

        {ujp}
        23: begin labelsearch(def, val, sp, blk); write(prr, 'l ');
          write(prr, sp^); lftjst(parfld-(2+max(sp^))); pass;
          wrtins(' jmp @s', sp^);
          if estack <> nil then begin { put in unresolved cache }
            getexp(ep); ep^.qs := sp;
            ep^.l := estack; estack := nil; ep^.next := jmpstr; jmpstr := ep;
          end
        end;

        {fjp,tjp}
        24,119: begin labelsearch(def, val, sp, blk); write(prr, 'l '); 
          write(prr, sp^); lftjst(parfld-(2+max(sp^))); pass;
          frereg := allreg; popstk(ep); 
          assreg(ep, frereg, rgnull, rgnull); dmptre(ep); genexp(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' orb %1l,%1l # move boolean to flags', ep^.r1);
          if op = 24{fjp} then wrtins(' jz @s # go if false', sp^)
          else {tjp} wrtins(' jnz @s # go if true', sp^);
          deltre(ep)
        end;

        {xjp}
        25: begin labelsearch(def, val, sp, blk); write(prr, 'l '); 
          write(prr, sp^); lftjst(parfld-(3+max(sp^))); pass;
          frereg := allreg; popstk(ep); getreg(r1, frereg);
          assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq %1,%2 # make factoring copy of index', ep^.r1, r1);
          wrtins(' salq $2,%1 # *4', ep^.r1);
          wrtins(' addq %2,%1 # *5', ep^.r1, r1);
          wrtins(' leaq @s(%rip),%1 # index case jump table', r1, sp^);
          wrtins(' addq %2,%1 # add scaled index to base', ep^.r1, r1);
          wrtins(' jmp *%1', ep^.r1);
          deltre(ep); 
          botstk 
        end;

        {ipj}
        112: begin read(prd,p); labelsearch(def, val, sp, blk); write(prr, p:1, ' l ');
          write(prr, sp^); lftjst(parfld-(digits(p)+3+max(sp^))); pass;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq ^0(%rbp),%rbp # get frame pointer for target', -p*ptrsize);
          wrtins(' movq ^0(%rbp),%rsp # get stack for target', marksb);
          wrtins(' andq $0xfffffffffffffff0,%rsp # align stack');
          wrtins(' jmp @s # goto jump target', sp^);
          botstk 
        end;

        {vbs}
        92: begin parq;
          frereg := allreg; popstk(ep); 
          assreg(ep, frereg, rgrdi, rgnull); dmptrel(ep, 19); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq %rdi,%rsi # set start of variable block');
          wrtins(' addq $0,%rsi # set end of variable block', ep^.q-1);
          wrtins(' call psystem_varenter # establish variable reference block');
          deltre(ep);
          botstk
        end;

        {vbe}
        96: begin par;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' call psystem_varexit # remove variable reference block');
          botstk
        end;

        {ret}
        22: begin par;
          frereg := allreg;
          wrtins(' ret      ');
          botstk
        end;

        {retp,retm}
        14,237: begin parq;
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' popq %r15 # undo alignment push');
          wrtins(' popq %r15 # restore protected registers');
          wrtins(' popq %r14');
          wrtins(' popq %r13');
          wrtins(' popq %r12');
          wrtins(' popq %rbx');
          wrtins(' leave # undo frame');
          wrtins(' addq $0,%rsp # remove frame data', marksize);
          wrtins(' popq %rcx # get return address');
          wrtins(' addq $0,%rsp # remove caller parameters', q);
          if op = 237{retm} then
            wrtins(' movq %rsp,%rax # index result in rax');
          wrtins(' pushq %rcx # replace return address');
          wrtins(' ret # return to caller');
          write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
          botstk; deltmp
        end;

        {reti,reta,retx,retc,retb}
        128,132,204,130,131: begin parq;
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' popq %r15 # undo alignment push');
          wrtins(' popq %r15 # restore protected registers');
          wrtins(' popq %r14');
          wrtins(' popq %r13');
          wrtins(' popq %r12');
          wrtins(' popq %rbx');
          wrtins(' leave # undo frame');
          wrtins(' addq $0,%rsp # remove frame data', marksize);
          wrtins(' popq %rcx # get return address');
          wrtins(' addq $0,%rsp # remove caller parameters', q);
          wrtins(' popq %rax # get qword result');
          if op in [204{retx},130{retc},131{retb}] then
            wrtins(' andq $0,%rax # mask byte result', 255);
          wrtins(' pushq %rcx # replace return address');
          wrtins(' ret # return to caller');
          write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
          botstk; deltmp
        end;

        {retr}
        129: begin parq;
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          { restore protected registers }
          wrtins(' popq %r15 # undo alignment push');
          wrtins(' popq %r15 # restore protected registers');
          wrtins(' popq %r14');
          wrtins(' popq %r13');
          wrtins(' popq %r12');
          wrtins(' popq %rbx');
          wrtins(' leave # undo frame');
          wrtins(' addq $0,%rsp # remove frame data', marksize);
          wrtins(' popq %rcx # get return address');
          wrtins(' addq $0,%rsp # remove caller parameters', q);
          wrtins(' movsd (%rsp),%xmm0 # move real from stack to xmm0');
          wrtins(' addq $0,%rsp # remove real from stack', realsize);
          wrtins(' pushq %rcx # restore return address');
          wrtins(' ret # return to caller');
          write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
          botstk; deltmp
        end;

        {rets}
        236: begin parq;
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          { restore protected registers }
          wrtins(' popq %r15 # undo alignment push');
          wrtins(' popq %r15 # restore protected registers');
          wrtins(' popq %r14');
          wrtins(' popq %r13');
          wrtins(' popq %r12');
          wrtins(' popq %rbx');
          wrtins(' leave # undo frame');
          wrtins(' addq $0,%rsp # remove frame data', marksize);
          wrtins(' popq %rcx # get return address');
          wrtins(' addq $0,%rsp # remove caller parameters', q);
          wrtins(' pushq %rcx # restore return address');
          wrtins(' ret # return to caller');
          write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
          botstk; deltmp
        end;

        {stoi,stoa,stor,stob,stoc,stox}
        6, 80, 81, 83, 84, 197: begin par;
          frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
          getreg(ep^.r1, frereg);
          assreg(ep, frereg, ep^.r1, rgnull);
          assreg(ep2, frereg, rgnull,  rgnull);
          dmptre(ep); dmptre(ep2);
          genexp(ep); genexp(ep2);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          case op of
            6{stoi},80{stoa}: wrtins(' movq %1,(%2) # store quad to address', q, ep2^.r1, ep^.r1);
            81{stor}: wrtins(' movsd %1,(%2) # store real to address', q, ep2^.r1, ep^.r1);
            83{stob},84{stoc},197{stox}:
              wrtins(' movb %1l,(%2) # store byte to address', q, ep2^.r1, ep^.r1)
          end;
          deltre(ep); deltre(ep2)
        end;

        {stos}
        82: begin par; 
          frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
          assreg(ep, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
          assreg(ep2, frereg, rgrsi,  rgnull);
          dmptre(ep); dmptre(ep2);
          genexp(ep); genexp(ep2);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movsq # store set to address');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' movsq');
          puttmp(ep2^.r1a);
          deltre(ep); deltre(ep2)
        end;

        {stom} 
        235: begin parqq; 
          frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
          assreg(ep, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
          assreg(ep2, frereg, rgrsi,  rgnull);
          dmptre(ep); dmptre(ep2);
          genexp(ep); genexp(ep2);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq $0,%rcx # set length', q);
          wrtins(' repnz # move structure to address');
          wrtins(' movsb');
          puttmp(ep2^.r1a);
          deltre(ep); deltre(ep2)
        end;

        {stp}
        58: par; { unused }

        {inv} { a no-op in pgen }
        189: begin par; 
          frereg := allreg; popstk(ep); dmptre(ep); deltre(ep); 
          botstk
        end;

        61 {ujc}: begin par;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' call psystem_caseerror');
          botstk
        end;
     
        {cjp}
        8: begin read(prd,q,q1); labelsearch(def, val, sp, blk); 
          write(prr,q:1, ' ', q1:1, ' l '); write(prr, sp^); write(prr, ' '); 
          lftjst(parfld-(digits(q)+1+digits(q1)+3+max(sp^))); pass;
          frereg := allreg; popstk(ep); 
          assreg(ep, frereg, rgnull, rgnull);
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' cmpq $0,%1 # check against low bound', q, ep^.r1);
          wrtins(' jl 1f # skip if lower');
          wrtins(' cmpq $0,%1 # check against high bound', q1, ep^.r1);
          wrtins(' jle @s # if less or equal, jump to target', sp^);
          wrtins('1:');
          pshstk(ep)
        end;

        {wbe}
        244: begin par;
          frereg := allreg;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' call psystem_withexit # remove last with');
          botstk
        end;

        {vip}
        133: begin parqq;
          frereg := allreg;
          popstk(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          pshexps(q); 
          assreg(ep, frereg, rgrdx, rgnull); dmptre(ep); genexp(ep);
          wrtins(' movq $0,%rdi # load # levels', q);
          wrtins(' movq $0,%rsi # base element size', q1);
          wrtins(' movq %rsp,%rcx # load array dimension list');
          wrtins(' call psystem_vip # fill template and allocate variable');
          wrtins(' addq $0,%rsp # dump dimensions from stack', q*intsize);
          deltre(ep);
          botstk
        end;

        {vis}
        122: begin parqq;
          frereg := allreg;
          popstk(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          pshexps(q); 
          assreg(ep, frereg, rgrdx, rgnull); dmptre(ep); genexp(ep);
          wrtins(' movq $0,%rdi # index level', q, r1);
          wrtins(' movq $0,%rsi # base element size', q1);
          wrtins(' movq %rsp,%rcx # load array dimension list');
          wrtins(' pushq %rdx # save variable address');
          wrtins(' call psystem_vis # fill template and allocate variable');
          wrtins(' popq %rdx # restore variable address');
          wrtins(' addq $0,%rsp # dump dimensions from stack', q*intsize);
          wrtins(' popq %rbx # get return address');
          wrtins(' subq %rax,%rsp # allocate vector on stack', q*intsize);
          wrtins(' movq %rsp,(%rdx) # set variable address');
          wrtins(' pushq %rbx # replace return address');
          deltre(ep);
          botstk
        end;

        {vin}
        226: begin parqq;
          frereg := allreg;
          popstk(ep); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          pshexps(q); 
          assreg(ep, frereg, rgrdx, rgnull); dmptre(ep); genexp(ep);
          wrtins(' movq $0,%rdi # load # levels', q, r1);
          wrtins(' movq $0,%rsi # base element size', q1);
          wrtins(' movq %rsp,%rcx # load array dimension list');
          wrtins(' call psystem_vin # fill template and allocate variable');
          wrtins(' addq $0,%rsp # dump dimensions from stack', q*intsize);
          deltre(ep);
          botstk
        end;

        {suv}
        91: begin labelsearch(def, val, sp, blk); 
          while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
          sp2 := nil;
          if prd^ = 'l' then begin 
            getnxt; labelsearch(def, val, sp2, blk);
            write(prr,' l '); write(prr, sp^);
            write(prr,'l '); write(prr, sp2^); 
            lftjst(parfld-(3+max(sp^)+2+max(sp2^))); pass
          end else begin
            read(prd,q1); write(prr,' l '); write(prr, sp^);
            write(' ',q1:1); 
            lftjst(parfld-(3+max(sp^)+1+digits(q1))); pass
          end;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' leaq @s(%rip),%rax # get new vector address', sp^);

          if sp2 <> nil then wrtins(' movq %rax,@s(%rip) #  place new vector', sp2^)
          else wrtins(' movq %rax,@g(%rip) # place new vector', q1);
        end;

        {cal}
        21: begin labelsearch(def, val, sp, blk); write(prr, 'l ');
          write(prr, sp^); lftjst(parfld-(2+max(sp^))); pass;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' call @s # call routine/initializer', sp^);
        end;

        {bge}
        207: begin labelsearch(def, val, sp, blk); write(prr, 'l ');
          write(prr, sp^); lftjst(parfld-(2+max(sp^))); pass;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' pushq psystem_expadr(%rip) # save current exception frame');
          wrtins(' pushq psystem_expstk(%rip)');
          wrtins(' pushq psystem_expmrk(%rip)');          
          wrtins(' pushq $0 # place dummy vector');
          wrtins(' leaq @s(%rip),%rax # place new exception frame', sp^);
          wrtins(' movq %rax,psystem_expadr(%rip)');
          wrtins(' movq %rsp,psystem_expstk(%rip)');
          wrtins(' movq %rbp,psystem_expmrk(%rip)');
          botstk
        end;        

        {ede}
        208: begin par;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' popq %rax # Dispose vector');
          wrtins(' popq psystem_expmrk(%rip) # restore previous exception frame');
          wrtins(' popq psystem_expstk(%rip)');
          wrtins(' popq psystem_expadr(%rip)');
          botstk
        end;

        {mse}
        209: begin par;
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' popq %rdx # get error vector');
          wrtins(' popq psystem_expmrk(%rip) # restore previous exception frame');
          wrtins(' popq psystem_expstk(%rip)');
          wrtins(' popq psystem_expadr(%rip)');
          wrtins(' movq psystem_expadr(%rip),%rax');

          wrtins(' orq %rax,%rax');
          wrtins(' jnz 1f # skip if less or equal');
          wrtins('1:');
          wrtins(' leaq modnam(%rip),%rdi # load module name');
          wrtins(' movq $0,%rsi # load line number', sline);
{??? Why didn't this stop unhandled exceptions ???}
          wrtins(' call psystem_errorv # process error');

          wrtins(' movq psystem_expmrk(%rip),%rbp # throw to new frame');
          wrtins(' popq psystem_expstk(%rip)');
          wrtins(' popq psystem_expadr(%rip)');
          wrtins(' popq %rax # dump dummy vector for this frame');
          wrtins(' pushq %rdx # set new vector');
          botstk
        end;

        {apc}
        210: begin parqq;
          frereg := allreg;
          popstk(ep); popstk(ep2);
          assreg(ep, frereg, rgrdx, rgnull); frereg := frereg-[rgrdx];
          assreg(ep2, frereg, rgrcx, rgr8); 
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          dmptre(ep2); genexp(ep2);
          dmptre(ep); genexp(ep);
          wrtins(' movq $0,%rdi # load # levels ', q, r1);
          wrtins(' movq $0,%rsi # base element size       ', q1);
          wrtins(' call psystem_apc # assign containers   ');
          deltre(ep); deltre(ep2);
          botstk
        end;

        {vdp,vdd} 
        221,227: begin par;
          frereg := allreg;
          popstk(ep);
          assreg(ep, frereg, rgrdi, rgnull);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          dmptre(ep); genexp(ep);
          wrtins(' movq $0,%rsi # load size', 1);
          wrtins(' call psystem_dsp # dispose of vector');
          deltre(ep); 
          botstk
        end;

        {scp} 
        224: begin par;
          frereg := allreg;
          { complex pointer, store address }
          popstk(ep2); popstk(ep);
          getreg(r1, frereg);
          assreg(ep, frereg, r1, rgnull);
          assreg(ep2, frereg, rgnull, rgnull);
          dmptre(ep); dmptre(ep2);
          genexp(ep); genexp(ep2);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq %1,(%2) # store array address', ep2^.r1, ep^.r1);
          wrtins(' addq $0,%1 # skip to template', intsize, ep^.r1);
          wrtins(' movq %1,(%2) # store template', ep2^.r2, ep^.r1)
        end;

        {ctb} 
        238: begin parqq;
          frereg := allreg;
          popstk(ep);
          frereg := frereg-[rgrdi,rgrsi,rgrcx];
          assreg(ep, frereg, rgnull, rgnull);
          dmptre(ep); genexp(ep);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq %1,%rdi # index destination', q, ep^.r1);
          wrtins(' movq %rsp,%rsi # index stack data', q);
          wrtins(' movq $0,%rcx # set length', q);
          wrtins(' repnz # copy to buffer');
          wrtins(' movsb    ');
          wrtins(' addq $0,%rsp # remove from stack', q1)
        end;

        {cps}
        176: begin par; 
          getexp(ep); popstk(ep2); popstk(ep3);
          duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
          frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); deltre(ep)
        end;

        {cpp} 
        239: parqq; { this is a no-op to us }

        {cpr} 
        240: parqq; { this is a no-op to us }

        {sfs}
        252: begin parqq; 
          frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
          assreg(ep2, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
          assreg(ep, frereg, rgrsi,  rgnull);
          dmptre(ep); dmptre(ep2);
          genexp(ep); genexp(ep2);
          writeln(prr, '# generating: ', op:3, ': ', instr[op]);
          wrtins(' movq $0,%rcx # set length', q);
          wrtins(' repnz # move structure to address');
          wrtins(' movsb');
          puttmp(ep^.r1a);
          deltre(ep); deltre(ep2)
        end;

        {*** These instructions are stack arrangers in the interpreter and are ignored here *** }

        {lsa} 
        241: parq;

        {lsp}
        250: par;

      end; (*case*)

      getlin; { next intermediate line }

   end; (*assemble*)

   procedure gencst;
   var r: record case boolean of

          true:  (s: settype);
          false: (b: packed array [1..setsize] of byte);

       end;
       i: 1..setsize;
       ti: 1..maxtmp;
   procedure gencstlst(cp: cstptr); forward;
   procedure gencstety(cp: cstptr);
   var i: integer;
       ti: 1..maxtmp;
   begin
     case cp^.ct of
       cstr: begin
         write(prr, '        .string "');
         writeq(prr, cp^.str^, cp^.strl);
         writeln(prr, '"') 
       end;
       creal: writeln(prr, '        .double ', cp^.r);
       cset: begin
         write(prr, '        .byte   ');
         r.s := cp^.s;
         for i := 1 to setsize do begin
           write(prr, r.b[i]:1); if i < setsize then write(prr, ',') 
         end;
         writeln(prr)
       end;
       ctmp: begin
         for ti := 1 to cp^.tsize do
           writeln(prr, '        .quad    ', cp^.ta[ti])
       end;
       ctab: gencstlst(cp^.tb);
       cint: writeln(prr, '        .quad   ', cp^.i:1);
       cchr: writeln(prr, '        .byte   ', cp^.c:1);
       cbol: writeln(prr, '        .byte   ', cp^.i:1);
       cvalx: writeln(prr, '        .byte   ', cp^.i:1);
     end
   end;
   procedure gencstlst(cp: cstptr);
   var ad: address;
   procedure align(a: integer);
   begin
     while (ad mod a) <> 0 do begin
       writeln(prr, '        .byte   1');
       ad := ad+1
     end
   end;
   begin
     ad := 0;
     while cp <> nil do begin
       case cp^.ct of
         cstr: ad := ad+cp^.strl;
         creal: begin align(realal); ad := ad+realsize end;
         cset: begin align(setal); ad := ad+setsize end;
         ctmp: ad := ad+(cp^.tsize+1)*intsize;
         ctab: ad := ad+cp^.csize;
         cint: begin align(intal); ad := ad+intsize end;
         cchr: begin align(charal); ad := ad+charsize end;
         cbol: begin align(boolal); ad := ad+boolsize end;
         cvalx: ad := ad+1;
       end;
       gencstety(cp);
       cp := cp^.next
   end;
   end;
   begin
     while csttbl <> nil do begin
       case csttbl^.ct of
         cstr: writeln(prr, 'string', csttbl^.strn:1, ':');
         creal: writeln(prr, 'real', csttbl^.realn:1, ':');
         cset: writeln(prr, 'set', csttbl^.setn:1, ':');
         ctmp: begin
           writeln(prr, 'template', csttbl^.tn:1, ':');
           write(prr, modnam^); writeln(prr, '.', csttbl^.tn:1, ':')
         end;
         ctab: begin
           writeln(prr, 'constant_table', csttbl^.cn:1, ':');
           write(prr, modnam^); writeln(prr, '.', csttbl^.cn:1, ':')
         end;
         cint: writeln(prr, 'value', csttbl^.intn:1, ':');
         cchr: writeln(prr, 'character', csttbl^.chrn:1, ':');
         cbol: writeln(prr, 'boolean', csttbl^.boln:1, ':');
         cvalx: writeln(prr, 'byte_value', csttbl^.valxn:1, ':');
       end;
       gencstety(csttbl);
       csttbl := csttbl^.next
     end
   end;

begin (*xlate*)

   init;
   writeln(prr, '# Header file locations');
   writeln(prr, 'inputoff = 0');
   writeln(prr, 'outputoff = 2');
   writeln(prr, 'prdoff = 4');
   writeln(prr, 'prroff = 6');
   writeln(prr, 'erroroff = 8');
   writeln(prr, 'listoff = 10');
   writeln(prr, 'commandoff = 12');
   writeln(prr);
   writeln(prr, '# Logical file numbers for header files');
   writeln(prr, 'inputfn = 1');
   writeln(prr, 'outputfn = 2');
   writeln(prr, 'prdfn = 3');
   writeln(prr, 'prrfn = 4');
   writeln(prr, 'errorfn = 5');
   writeln(prr, 'listfn = 6');
   writeln(prr, 'commandfn = 7');
   writeln(prr);
   errorcode;
   writeln(prr);
   mpb;
   writeln(prr);
   writeln(prr, '        .text');
   writeln(prr, '#');
   writeln(prr, '# Code section');
   writeln(prr, '#');
   generate;
   writeln(prr, '#');
   writeln(prr, '# Constants section');
   writeln(prr, '#');
   writeln(prr, '        jmp     1f');
   writeln(prr, 'modnam:');
   write(prr, '        .string  "'); write(prr, modnam^); writeln(prr, '"');
   writeln(prr, 'real_zero:');
   writeln(prr, '        .double  0.0');
   writeln(prr, 'real_int_max:');
   writeln(prr, '        .double  9223372036854775807');
   writeln(prr, 'real_int_min:');
   writeln(prr, '        .double  -9223372036854775807');

   gencst;

   writeln(prr, '1:');

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

{ place options in flags }
procedure plcopt;
var oi: 1..maxopt;
begin
  for oi := 1 to 26 do if options[oi] then
    case oi of
      7:  dodmplab   := option[oi];
      8:  dosrclin   := option[oi];
      11: doechlin   := option[oi];
      14: dorecycl   := option[oi];
      15: dochkovf   := option[oi];
      16: dochkrpt   := option[oi];
      13: donorecpar := option[oi];
      17: dochkdef   := option[oi];
      19: iso7185    := option[oi];
      23: dodebug    := option[oi];
      1:  dodbgflt   := option[oi];
      6:  dodbgsrc   := option[oi];
      5:  dodckout   := option[oi];
      9:  dochkvbk   := option[oi];
      2:; 3:; 4:; 12:; 20:; 21:; 22:;
      24:; 25:; 26:; 10:; 18:;
    end
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

  for oi := 1 to maxopt do begin option[oi] := false; options[oi] := false end;

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
  dodbgchk := true;  { do debug checks }
  doechlin := false; { don't echo command lines }

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
  if dodbgchk then;

  blkstk := nil; { clear symbols block stack }
  blklst := nil; { clear symbols block discard list }
  level := 0; { clear level count }

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

  { get the command line }
  getcommandline;
  cmdpos := 1;
  paroptions; { parse command line options }
  { parse header files }
  parhdrfilnam(prd, prdval, srcfil, '.p6 ');
  if not prdval then begin
    writeln('*** Error: input filename not found');
    goto 99
  end;
  paroptions; { parse command line options }
  parhdrfil(prr, prrval, '.s  ');
  if not prrval then begin
    writeln('*** Error: output filename not found');
    goto 99
  end;
  { load command line options }
  paroptions;
  plcopt; { place options }

  rewrite(prr);

  writeln('Generating program');

  writeln(prr, '#');
  write(prr, '# File generated by P6 Pascal AMD64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write(prr, '.x');
  writeln(prr);
  writeln(prr, '#');
  writeln(prr, '        .file "', srcfil:*, '.pas"');
  writeln(prr, '        .file 1 "', srcfil:*, '.pas"');
  writeln(prr);

  xlate; (* assembles and stores code *)

  99 : { abort run }

  writeln;
  writeln('Program generation complete');

end.
