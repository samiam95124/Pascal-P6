(*$c+,t-,d-,l+,s+*)
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
* Adaption from P4 to P6 by:                                                   *
*                                                                              *
*    Scott A. Franco                                                           *
*    samiam@moorecad.com                                                       *
*                                                                              *
* AMD64 code generator for GCC                                                 *
*                                                                              *
* This is the code generator backend for 64 bit AMD64 processor model running  *
* with a gcc code base.                                                        *
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

#include "mpb64.inc"

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
                       short:   boolean; { there is a short name }
                       { block type }
                       btyp:    (btprog, btmod, btproc, btfunc);
                       en:      integer { encounter number }
                     end;

var   pc          : address;   (*program address register*)
      pctop,lsttop: address;   { top of code store }
      op : instyp; p : lvltyp; q : address;  (*instruction register*)
      q1,q2: address; { extra parameter }
      gblsiz: address; { size of globals }
      sdi         : 0..maxdef; { index for that }
      cp          : address;  (* pointer to next free constant position *)
      mp,sp,np,ep : address;  (* address registers *)
      (*mp  points to beginning of a data segment
        sp  points to top of the stack
        ep  points to the maximum extent of the stack
        np  points to top of the dynamically allocated area*)
      bitmsk      : packed array [0..7] of byte; { bits in byte }
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
      flipend: boolean; { endian mode is opposing }

      interpreting: boolean;

      { !!! remove this next statement for self compile }
      {elide}prd,prr     : text;{noelide}(*prd for read only, prr for write only *)

      instr       : array[instyp] of alfa; (* mnemonic instruction codes *)
      insp        : array[instyp] of boolean; { instruction includes a p parameter }
      insq        : array[instyp] of 0..32; { length of q parameter }
      insr        : array[instyp] of integer; { number of stack words in result }
      sptable     : array[sctyp] of alfa; (*standard functions and procedures*)
      spfunc      : array[sctyp] of boolean; (*standard function or procedure
                                                  is function*)
      sppar       : array[sctyp] of integer; (*standard functions and procedures
                                                  number of parameters*)
      spkeep      : array[sctyp] of boolean; { keep the file parameter }
      srclin      : integer; { current source line executing }
      option      : array ['a'..'z'] of boolean; { option array }
      csttbl      : cstptr; { constants table }
      strnum      : integer; { string constant label count }
      realnum     : integer; { real constants label count }
      parlvl      : integer; { parameter level }
      blkstk      : pblock; { stack of symbol blocks }
      blklst      : pblock; { discard list of symbols blocks }

      filtable    : array [1..maxfil] of text; { general (temp) text file holders }
      { general (temp) binary file holders }
      bfiltable   : array [1..maxfil] of bytfil;
      { file state holding }
      filstate    : array [1..maxfil] of (fclosed, fread, fwrite);
      { file buffer full status }
      filbuff     : array [1..maxfil] of boolean;

      (*locally used for interpreting one instruction*)
      ad,ad1,ad2,
      ad3         : address;
      b           : boolean;
      i,j,k,i1,i2 : integer;
      c           : char;
      i3, i4      : integer;
      r1, r2      : real;
      b1, b2      : boolean;
      s1, s2      : settype;
      c1          : char;
      a1, a2, a3  : address;
      pcs         : address;
      bai         : integer;

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

{ assign symbol identifier fixed to variable length string, including
  allocation, with length specified }
procedure strassvfl(var a: strvsp; var b: labbuf; l: integer);
var i, j: integer; p, lp: strvsp;
begin p := nil; a := nil; j := 1;
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
begin l := lablen; p := nil; a := nil; j := 1;
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
begin l := strlen; p := nil; a := nil; j := 1;
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
                    next: expptr; { next entry link }
                    op:   instyp; { operator type }
                    p:   lvltyp; q, q1, q2: address; { p and q parameters }
                    r1, r2, r3: reg; { result registers }
                    t1, t2: reg; { temporary registers }
                    l, r: expptr; { right and left links }
                    x1: expptr; { extra link }
                    pl: expptr; { parameter link for functions }
                    sl: expptr; { sfr start link }
                    strn: integer; { string number }
                    realn: integer; { real number }
                    vali: integer; { integer value }
                    rs: regset; { push/pop mask }
                    wkeep: boolean; { will hold this value }
                    keep: boolean; { hold this value }
                    fn: strvsp; { function call name }
                    lb: strvsp; { label for sfr }
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
        stacklvl: integer;
        parreg: array [1..12] of reg; { parameter registers }

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
         instr[  0]:='lodi      '; insp[  0] := true;  insq[  0] := intsize;    insr[  0] := 1;  
         instr[  1]:='ldoi      '; insp[  1] := false; insq[  1] := intsize;    insr[  1] := 1; 
         instr[  2]:='stri      '; insp[  2] := true;  insq[  2] := intsize;    insr[  2] := 0;  
         instr[  3]:='sroi      '; insp[  3] := false; insq[  3] := intsize;    insr[  3] := 0; 
         instr[  4]:='lda       '; insp[  4] := true;  insq[  4] := intsize;    insr[  4] := 1;  
         instr[  5]:='lao       '; insp[  5] := false; insq[  5] := intsize;    insr[  5] := 1; 
         instr[  6]:='stoi      '; insp[  6] := false; insq[  6] := 0;          insr[  6] := 0; 
         instr[  7]:='ldcs      '; insp[  7] := false; insq[  7] := intsize;    insr[  7] := 1; 
         instr[  8]:='cjp       '; insp[  8] := false; insq[  8] := intsize*2;  insr[  8] := 0; 
         instr[  9]:='indi      '; insp[  9] := false; insq[  9] := intsize;    insr[  9] := 1; 
         instr[ 10]:='inci      '; insp[ 10] := false; insq[ 10] := intsize;    insr[ 10] := 1; 
         instr[ 11]:='mst       '; insp[ 11] := true;  insq[ 11] := intsize*2;  insr[ 11] := 0;  
         instr[ 12]:='cup       '; insp[ 12] := false; insq[ 12] := intsize;    insr[ 12] := 0; 
         instr[ 13]:='rip       '; insp[ 13] := false; insq[ 13] := adrsize;    insr[ 13] := 0; 
         instr[ 14]:='retp      '; insp[ 14] := false; insq[ 14] := intsize;    insr[ 14] := 0; 
         instr[ 15]:='csp       '; insp[ 15] := false; insq[ 15] := 1;          insr[ 15] := 0; 
         instr[ 16]:='ixa       '; insp[ 16] := false; insq[ 16] := intsize;    insr[ 16] := 1; 
         instr[ 17]:='equa      '; insp[ 17] := false; insq[ 17] := 0;          insr[ 17] := 1; 
         instr[ 18]:='neqa      '; insp[ 18] := false; insq[ 18] := 0;          insr[ 18] := 1; 
         instr[ 19]:='brk*      '; insp[ 19] := false; insq[ 19] := 0;          insr[ 19] := 0; 
         instr[ 20]:='lnp*      '; insp[ 20] := false; insq[ 20] := intsize;    insr[ 20] := 0; 
         instr[ 21]:='cal       '; insp[ 21] := false; insq[ 21] := intsize;    insr[ 21] := 1; 
         instr[ 22]:='ret       '; insp[ 22] := false; insq[ 22] := 0;          insr[ 22] := 0; 
         instr[ 23]:='ujp       '; insp[ 23] := false; insq[ 23] := intsize;    insr[ 23] := 0; 
         instr[ 24]:='fjp       '; insp[ 24] := false; insq[ 24] := intsize;    insr[ 24] := 0; 
         instr[ 25]:='xjp       '; insp[ 25] := false; insq[ 25] := intsize;    insr[ 25] := 0; 
         instr[ 26]:='chki      '; insp[ 26] := false; insq[ 26] := intsize;    insr[ 26] := 1; 
         instr[ 27]:='cuv       '; insp[ 27] := false; insq[ 27] := intsize;    insr[ 27] := 0; 
         instr[ 28]:='adi       '; insp[ 28] := false; insq[ 28] := 0;          insr[ 28] := 1; 
         instr[ 29]:='adr       '; insp[ 29] := false; insq[ 29] := 0;          insr[ 29] := 1; 
         instr[ 30]:='sbi       '; insp[ 30] := false; insq[ 30] := 0;          insr[ 30] := 1; 
         instr[ 31]:='sbr       '; insp[ 31] := false; insq[ 31] := 0;          insr[ 31] := 1; 
         instr[ 32]:='sgs       '; insp[ 32] := false; insq[ 32] := 0;          insr[ 32] := 1; 
         instr[ 33]:='flt       '; insp[ 33] := false; insq[ 33] := 0;          insr[ 33] := 1; 
         instr[ 34]:='flo       '; insp[ 34] := false; insq[ 34] := 0;          insr[ 34] := 2; 
         instr[ 35]:='trc       '; insp[ 35] := false; insq[ 35] := 0;          insr[ 35] := 1; 
         instr[ 36]:='ngi       '; insp[ 36] := false; insq[ 36] := 0;          insr[ 36] := 1; 
         instr[ 37]:='ngr       '; insp[ 37] := false; insq[ 37] := 0;          insr[ 37] := 1; 
         instr[ 38]:='sqi       '; insp[ 38] := false; insq[ 38] := 0;          insr[ 38] := 1; 
         instr[ 39]:='sqr       '; insp[ 39] := false; insq[ 39] := 0;          insr[ 39] := 1; 
         instr[ 40]:='abi       '; insp[ 40] := false; insq[ 40] := 0;          insr[ 40] := 1; 
         instr[ 41]:='abr       '; insp[ 41] := false; insq[ 41] := 0;          insr[ 41] := 1; 
         instr[ 42]:='notb      '; insp[ 42] := false; insq[ 42] := 0;          insr[ 42] := 1; 
         instr[ 43]:='and       '; insp[ 43] := false; insq[ 43] := 0;          insr[ 43] := 1; 
         instr[ 44]:='ior       '; insp[ 44] := false; insq[ 44] := 0;          insr[ 44] := 1; 
         instr[ 45]:='dif       '; insp[ 45] := false; insq[ 45] := 0;          insr[ 45] := 1; 
         instr[ 46]:='int       '; insp[ 46] := false; insq[ 46] := 0;          insr[ 46] := 1; 
         instr[ 47]:='uni       '; insp[ 47] := false; insq[ 47] := 0;          insr[ 47] := 1; 
         instr[ 48]:='inn       '; insp[ 48] := false; insq[ 48] := 0;          insr[ 48] := 1; 
         instr[ 49]:='mod       '; insp[ 49] := false; insq[ 49] := 0;          insr[ 49] := 1; 
         instr[ 50]:='odd       '; insp[ 50] := false; insq[ 50] := 0;          insr[ 50] := 1; 
         instr[ 51]:='mpi       '; insp[ 51] := false; insq[ 51] := 0;          insr[ 51] := 1; 
         instr[ 52]:='mpr       '; insp[ 52] := false; insq[ 52] := 0;          insr[ 52] := 1; 
         instr[ 53]:='dvi       '; insp[ 53] := false; insq[ 53] := 0;          insr[ 53] := 1; 
         instr[ 54]:='dvr       '; insp[ 54] := false; insq[ 54] := 0;          insr[ 54] := 1; 
         instr[ 55]:='mov       '; insp[ 55] := false; insq[ 55] := intsize;    insr[ 55] := 0; 
         instr[ 56]:='lca       '; insp[ 56] := false; insq[ 56] := intsize;    insr[ 56] := 1; 
         instr[ 57]:='deci      '; insp[ 57] := false; insq[ 57] := intsize;    insr[ 57] := 1; 
         instr[ 58]:='stp*      '; insp[ 58] := false; insq[ 58] := 0;          insr[ 58] := 0; 
         instr[ 59]:='ordi      '; insp[ 59] := false; insq[ 59] := 0;          insr[ 59] := 1; 
         instr[ 60]:='chr       '; insp[ 60] := false; insq[ 60] := 0;          insr[ 60] := 1; 
         instr[ 61]:='ujc       '; insp[ 61] := false; insq[ 61] := intsize;    insr[ 61] := 0; 
         instr[ 62]:='rnd       '; insp[ 62] := false; insq[ 62] := 0;          insr[ 62] := 1; 
         instr[ 63]:='pck       '; insp[ 63] := false; insq[ 63] := intsize*2;  insr[ 63] := 0;
         instr[ 64]:='upk       '; insp[ 64] := false; insq[ 64] := intsize*2;  insr[ 64] := 0;
         instr[ 65]:='ldoa      '; insp[ 65] := false; insq[ 65] := intsize;    insr[ 65] := 1;
         instr[ 66]:='ldor      '; insp[ 66] := false; insq[ 66] := intsize;    insr[ 66] := 1;
         instr[ 67]:='ldos      '; insp[ 67] := false; insq[ 67] := intsize;    insr[ 67] := 1;
         instr[ 68]:='ldob      '; insp[ 68] := false; insq[ 68] := intsize;    insr[ 68] := 1;
         instr[ 69]:='ldoc      '; insp[ 69] := false; insq[ 69] := intsize;    insr[ 69] := 1;
         instr[ 70]:='stra      '; insp[ 70] := true;  insq[ 70] := intsize;    insr[ 70] := 0; 
         instr[ 71]:='strr      '; insp[ 71] := true;  insq[ 71] := intsize;    insr[ 71] := 0; 
         instr[ 72]:='strs      '; insp[ 72] := true;  insq[ 72] := intsize;    insr[ 72] := 0; 
         instr[ 73]:='strb      '; insp[ 73] := true;  insq[ 73] := intsize;    insr[ 73] := 0; 
         instr[ 74]:='strc      '; insp[ 74] := true;  insq[ 74] := intsize;    insr[ 74] := 0; 
         instr[ 75]:='sroa      '; insp[ 75] := false; insq[ 75] := intsize;    insr[ 75] := 0;
         instr[ 76]:='sror      '; insp[ 76] := false; insq[ 76] := intsize;    insr[ 76] := 1;
         instr[ 77]:='sros      '; insp[ 77] := false; insq[ 77] := intsize;    insr[ 77] := 1;
         instr[ 78]:='srob      '; insp[ 78] := false; insq[ 78] := intsize;    insr[ 78] := 0;
         instr[ 79]:='sroc      '; insp[ 79] := false; insq[ 79] := intsize;    insr[ 79] := 0;
         instr[ 80]:='stoa      '; insp[ 80] := false; insq[ 80] := 0;          insr[ 80] := 0;
         instr[ 81]:='stor      '; insp[ 81] := false; insq[ 81] := 0;          insr[ 81] := 0;
         instr[ 82]:='stos      '; insp[ 82] := false; insq[ 82] := 0;          insr[ 82] := 0;
         instr[ 83]:='stob      '; insp[ 83] := false; insq[ 83] := 0;          insr[ 83] := 0;
         instr[ 84]:='stoc      '; insp[ 84] := false; insq[ 84] := 0;          insr[ 84] := 0;
         instr[ 85]:='inda      '; insp[ 85] := false; insq[ 85] := intsize;    insr[ 85] := 1;
         instr[ 86]:='indr      '; insp[ 86] := false; insq[ 86] := intsize;    insr[ 86] := 1;
         instr[ 87]:='inds      '; insp[ 87] := false; insq[ 87] := intsize;    insr[ 87] := 1;
         instr[ 88]:='indb      '; insp[ 88] := false; insq[ 88] := intsize;    insr[ 88] := 1;
         instr[ 89]:='indc      '; insp[ 89] := false; insq[ 89] := intsize;    insr[ 89] := 1;
         instr[ 90]:='inca      '; insp[ 90] := false; insq[ 90] := intsize;    insr[ 90] := 1;
         instr[ 91]:='suv       '; insp[ 91] := false; insq[ 91] := intsize*2;  insr[ 91] := 0;
         instr[ 92]:='vbs       '; insp[ 92] := false; insq[ 92] := intsize;    insr[ 92] := 0;
         instr[ 93]:='incb      '; insp[ 93] := false; insq[ 93] := intsize;    insr[ 93] := 1;
         instr[ 94]:='incc      '; insp[ 94] := false; insq[ 94] := intsize;    insr[ 94] := 1;
         instr[ 95]:='chka      '; insp[ 95] := false; insq[ 95] := intsize;    insr[ 95] := 1;
         instr[ 96]:='vbe       '; insp[ 96] := false; insq[ 96] := 0;          insr[ 96] := 0;
         instr[ 97]:='chks      '; insp[ 97] := false; insq[ 97] := intsize;    insr[ 97] := 0;
         instr[ 98]:='chkb      '; insp[ 98] := false; insq[ 98] := intsize;    insr[ 98] := 1;
         instr[ 99]:='chkc      '; insp[ 99] := false; insq[ 99] := intsize;    insr[ 99] := 1;
         instr[100]:='cvbi      '; insp[100] := false; insq[100] := intsize*3;  insr[100] := 2;
         instr[101]:='ivtx      '; insp[101] := false; insq[101] := intsize*3;  insr[101] := 2;
         instr[102]:='ivtb      '; insp[102] := false; insq[102] := intsize*3;  insr[102] := 2;
         instr[103]:='decb      '; insp[103] := false; insq[103] := intsize;    insr[103] := 1;
         instr[104]:='decc      '; insp[104] := false; insq[104] := intsize;    insr[104] := 1;
         instr[105]:='loda      '; insp[105] := true;  insq[105] := intsize;    insr[105] := 1; 
         instr[106]:='lodr      '; insp[106] := true;  insq[106] := intsize;    insr[106] := 1; 
         instr[107]:='lods      '; insp[107] := true;  insq[107] := intsize;    insr[107] := 1; 
         instr[108]:='lodb      '; insp[108] := true;  insq[108] := intsize;    insr[108] := 1; 
         instr[109]:='lodc      '; insp[109] := true;  insq[109] := intsize;    insr[109] := 1; 
         instr[110]:='rgs       '; insp[110] := false; insq[110] := 0;          insr[110] := 1;
         instr[111]:='ivtc      '; insp[111] := false; insq[111] := intsize*3;  insr[111] := 2;
         instr[112]:='ipj       '; insp[112] := true;  insq[112] := intsize;    insr[112] := 0; 
         instr[113]:='cip       '; insp[113] := false; insq[113] := 0;          insr[113] := 0;
         instr[114]:='lpa       '; insp[114] := true;  insq[114] := intsize;    insr[114] := 2; 
         instr[115]:='cvbx      '; insp[115] := false; insq[115] := intsize*3;  insr[115] := 2;
         instr[116]:='cvbb      '; insp[116] := false; insq[116] := intsize*3;  insr[116] := 2;
         instr[117]:='dmp       '; insp[117] := false; insq[117] := intsize;    insr[117] := 0;
         instr[118]:='swp       '; insp[118] := false; insq[118] := intsize;    insr[118] := 2;
         instr[119]:='tjp       '; insp[119] := false; insq[119] := intsize;    insr[119] := 0;
         instr[120]:='lip       '; insp[120] := true;  insq[120] := intsize;    insr[120] := 2; 
         instr[121]:='cvbc      '; insp[121] := false; insq[121] := intsize*3;  insr[121] := 2;
         instr[122]:='vis       '; insp[122] := false; insq[122] := intsize*2;  insr[122] := 1;
         instr[123]:='ldci      '; insp[123] := false; insq[123] := intsize;    insr[123] := 1;
         instr[124]:='ldcr      '; insp[124] := false; insq[124] := intsize;    insr[124] := 1;
         instr[125]:='ldcn      '; insp[125] := false; insq[125] := 0;          insr[125] := 1;
         instr[126]:='ldcb      '; insp[126] := false; insq[126] := boolsize;   insr[126] := 1;
         instr[127]:='ldcc      '; insp[127] := false; insq[127] := charsize;   insr[127] := 1;
         instr[128]:='reti      '; insp[128] := false; insq[128] := intsize;    insr[128] := 1;
         instr[129]:='retr      '; insp[129] := false; insq[129] := intsize;    insr[129] := 1;
         instr[130]:='retc      '; insp[130] := false; insq[130] := intsize;    insr[130] := 1;
         instr[131]:='retb      '; insp[131] := false; insq[131] := intsize;    insr[131] := 1;
         instr[132]:='reta      '; insp[132] := false; insq[132] := intsize;    insr[132] := 1;
         instr[133]:='vip       '; insp[133] := false; insq[133] := intsize*2;  insr[133] := 0;
         instr[134]:='ordb      '; insp[134] := false; insq[134] := 0;          insr[134] := 1;
         instr[135]:='lcp       '; insp[135] := false; insq[135] := 0;          insr[135] := 2;
         instr[136]:='ordc      '; insp[136] := false; insq[136] := 0;          insr[136] := 1;
         instr[137]:='equi      '; insp[137] := false; insq[137] := 0;          insr[137] := 1;
         instr[138]:='equr      '; insp[138] := false; insq[138] := 0;          insr[138] := 1;
         instr[139]:='equb      '; insp[139] := false; insq[139] := 0;          insr[139] := 1;
         instr[140]:='equs      '; insp[140] := false; insq[140] := 0;          insr[140] := 1;
         instr[141]:='equc      '; insp[141] := false; insq[141] := 0;          insr[141] := 1;
         instr[142]:='equm      '; insp[142] := false; insq[142] := intsize;    insr[142] := 1;
         instr[143]:='neqi      '; insp[143] := false; insq[143] := 0;          insr[143] := 1;
         instr[144]:='neqr      '; insp[144] := false; insq[144] := 0;          insr[144] := 1;
         instr[145]:='neqb      '; insp[145] := false; insq[145] := 0;          insr[145] := 1;
         instr[146]:='neqs      '; insp[146] := false; insq[146] := 0;          insr[146] := 1;
         instr[147]:='neqc      '; insp[147] := false; insq[147] := 0;          insr[147] := 1;
         instr[148]:='neqm      '; insp[148] := false; insq[148] := intsize;    insr[148] := 1;
         instr[149]:='geqi      '; insp[149] := false; insq[149] := 0;          insr[149] := 1;
         instr[150]:='geqr      '; insp[150] := false; insq[150] := 0;          insr[150] := 1;
         instr[151]:='geqb      '; insp[151] := false; insq[151] := 0;          insr[151] := 1;
         instr[152]:='geqs      '; insp[152] := false; insq[152] := 0;          insr[152] := 1;
         instr[153]:='geqc      '; insp[153] := false; insq[153] := 0;          insr[153] := 1;
         instr[154]:='geqm      '; insp[154] := false; insq[154] := intsize;    insr[154] := 1;
         instr[155]:='grti      '; insp[155] := false; insq[155] := 0;          insr[155] := 1;
         instr[156]:='grtr      '; insp[156] := false; insq[156] := 0;          insr[156] := 1;
         instr[157]:='grtb      '; insp[157] := false; insq[157] := 0;          insr[157] := 1;
         instr[158]:='grts      '; insp[158] := false; insq[158] := 0;          insr[158] := 1;
         instr[159]:='grtc      '; insp[159] := false; insq[159] := 0;          insr[159] := 1;
         instr[160]:='grtm      '; insp[160] := false; insq[160] := intsize;    insr[160] := 1;
         instr[161]:='leqi      '; insp[161] := false; insq[161] := 0;          insr[161] := 1;
         instr[162]:='leqr      '; insp[162] := false; insq[162] := 0;          insr[162] := 1;
         instr[163]:='leqb      '; insp[163] := false; insq[163] := 0;          insr[163] := 1;
         instr[164]:='leqs      '; insp[164] := false; insq[164] := 0;          insr[164] := 1;
         instr[165]:='leqc      '; insp[165] := false; insq[165] := 0;          insr[165] := 1;
         instr[166]:='leqm      '; insp[166] := false; insq[166] := intsize;    insr[166] := 1;
         instr[167]:='lesi      '; insp[167] := false; insq[167] := 0;          insr[167] := 1;
         instr[168]:='lesr      '; insp[168] := false; insq[168] := 0;          insr[168] := 1;
         instr[169]:='lesb      '; insp[169] := false; insq[169] := 0;          insr[169] := 1;
         instr[170]:='less      '; insp[170] := false; insq[170] := 0;          insr[170] := 1;
         instr[171]:='lesc      '; insp[171] := false; insq[171] := 0;          insr[171] := 1;
         instr[172]:='lesm      '; insp[172] := false; insq[172] := intsize;    insr[172] := 1;
         instr[173]:='---       '; insp[173] := false; insq[173] := 0;          insr[173] := 0;
         instr[174]:='mrkl*     '; insp[174] := false; insq[174] := intsize;    insr[174] := 0;
         instr[175]:='ckvi      '; insp[175] := false; insq[175] := intsize;    insr[175] := 2;
         instr[176]:='cps       '; insp[176] := false; insq[176] := 0;          insr[176] := 3;
         instr[177]:='cpc       '; insp[177] := false; insq[177] := intsize;    insr[177] := 3;
         instr[178]:='aps       '; insp[178] := false; insq[178] := intsize;    insr[178] := 0;
         instr[179]:='ckvb      '; insp[179] := false; insq[179] := intsize;    insr[179] := 2;
         instr[180]:='ckvc      '; insp[180] := false; insq[180] := intsize;    insr[180] := 2;
         instr[181]:='dupi      '; insp[181] := false; insq[181] := 0;          insr[181] := 1;
         instr[182]:='dupa      '; insp[182] := false; insq[182] := 0;          insr[182] := 2;
         instr[183]:='dupr      '; insp[183] := false; insq[183] := 0;          insr[183] := 1;
         instr[184]:='dups      '; insp[184] := false; insq[184] := 0;          insr[184] := 1;
         instr[185]:='dupb      '; insp[185] := false; insq[185] := 0;          insr[185] := 1;
         instr[186]:='dupc      '; insp[186] := false; insq[186] := 0;          insr[186] := 1;
         instr[187]:='cks       '; insp[187] := false; insq[187] := 0;          insr[187] := 2;
         instr[188]:='cke       '; insp[188] := false; insq[188] := 0;          insr[188] := 0;
         instr[189]:='inv       '; insp[189] := false; insq[189] := 0;          insr[189] := 0;
         instr[190]:='ckla      '; insp[190] := false; insq[190] := intsize;    insr[190] := 1;
         instr[191]:='cta       '; insp[191] := false; insq[191] := intsize*3;  insr[191] := 2;
         instr[192]:='ivti      '; insp[192] := false; insq[192] := intsize*3;  insr[192] := 2;
         instr[193]:='lodx      '; insp[193] := true;  insq[193] := intsize;    insr[193] := 1; 
         instr[194]:='ldox      '; insp[194] := false; insq[194] := intsize;    insr[194] := 1;
         instr[195]:='strx      '; insp[195] := true;  insq[195] := intsize;    insr[195] := 0; 
         instr[196]:='srox      '; insp[196] := false; insq[196] := intsize;    insr[196] := 1;
         instr[197]:='stox      '; insp[197] := false; insq[197] := 0;          insr[197] := 0;
         instr[198]:='indx      '; insp[198] := false; insq[198] := intsize;    insr[198] := 1;
         instr[199]:='chkx      '; insp[199] := false; insq[199] := intsize;    insr[199] := 1;
         instr[200]:='ordx      '; insp[200] := false; insq[200] := 0;          insr[200] := 1;
         instr[201]:='incx      '; insp[201] := false; insq[201] := intsize;    insr[201] := 1;
         instr[202]:='decx      '; insp[202] := false; insq[202] := intsize;    insr[202] := 1;
         instr[203]:='ckvx      '; insp[203] := false; insq[203] := intsize;    insr[203] := 2;
         instr[204]:='retx      '; insp[204] := false; insq[204] := intsize;    insr[204] := 1;
         instr[205]:='noti      '; insp[205] := false; insq[205] := 0;          insr[205] := 1;
         instr[206]:='xor       '; insp[206] := false; insq[206] := 0;          insr[206] := 1;
         instr[207]:='bge       '; insp[207] := false; insq[207] := intsize;    insr[207] := 4;
         instr[208]:='ede       '; insp[208] := false; insq[208] := 0;          insr[208] := 0;
         instr[209]:='mse       '; insp[209] := false; insq[209] := 0;          insr[209] := 0;
         instr[210]:='apc       '; insp[210] := false; insq[210] := intsize*2;  insr[210] := 0;
         instr[211]:='cxs       '; insp[211] := false; insq[211] := intsize;    insr[211] := 1;
         instr[212]:='cxc       '; insp[212] := false; insq[212] := intsize*2;  insr[212] := 2;
         instr[213]:='lft       '; insp[213] := false; insq[213] := intsize;    insr[213] := 2;
         instr[214]:='max       '; insp[214] := false; insq[214] := intsize;    insr[214] := 1;
         instr[215]:='equv      '; insp[215] := false; insq[215] := 0;          insr[215] := 1;
         instr[216]:='neqv      '; insp[216] := false; insq[216] := 0;          insr[216] := 1;
         instr[217]:='lesv      '; insp[217] := false; insq[217] := 0;          insr[217] := 1;
         instr[218]:='grtv      '; insp[218] := false; insq[218] := 0;          insr[218] := 1;
         instr[219]:='leqv      '; insp[219] := false; insq[219] := 0;          insr[219] := 1;
         instr[220]:='geqv      '; insp[220] := false; insq[220] := 0;          insr[220] := 1;
         instr[221]:='vdp       '; insp[221] := false; insq[221] := 0;          insr[221] := 0;
         instr[222]:='spc       '; insp[222] := false; insq[222] := 0;          insr[222] := 2;
         instr[223]:='ccs       '; insp[223] := false; insq[223] := intsize*2;  insr[223] := 2;
         instr[224]:='scp       '; insp[224] := false; insq[224] := 0;          insr[224] := 0;
         instr[225]:='ldp       '; insp[225] := false; insq[225] := 0;          insr[225] := 2;
         instr[226]:='vin       '; insp[226] := false; insq[226] := intsize*2;  insr[226] := 0;
         instr[227]:='vdd       '; insp[227] := false; insq[227] := 0;          insr[227] := 0;
         { ltc and lto are aliases to ldo and lao instructions }
         instr[228]:='ltci      '; insp[228] := false; insq[228] := intsize;    insr[228] := 1;
         instr[229]:='ltcr      '; insp[229] := false; insq[229] := intsize;    insr[229] := 1;
         instr[230]:='ltcs      '; insp[230] := false; insq[230] := intsize;    insr[230] := 1;
         instr[231]:='ltcb      '; insp[231] := false; insq[231] := intsize;    insr[231] := 1;
         instr[232]:='ltcc      '; insp[232] := false; insq[232] := intsize;    insr[232] := 1;
         instr[233]:='ltcx      '; insp[233] := false; insq[233] := intsize;    insr[233] := 1;
         instr[234]:='lto       '; insp[234] := false; insq[234] := intsize;    insr[234] := 1;
         instr[235]:='stom      '; insp[235] := false; insq[235] := intsize*2;  insr[235] := 0;
         instr[236]:='rets      '; insp[236] := false; insq[236] := intsize;    insr[236] := 1;
         instr[237]:='retm      '; insp[237] := false; insq[237] := intsize*2;  insr[237] := 1;
         instr[238]:='ctb       '; insp[238] := false; insq[238] := intsize*2;  insr[238] := 0;
         instr[239]:='cpp       '; insp[239] := false; insq[239] := intsize*2;  insr[239] := 1;
         instr[240]:='cpr       '; insp[240] := false; insq[240] := intsize*2;  insr[240] := 1;
         instr[241]:='lsa       '; insp[241] := false; insq[241] := intsize;    insr[241] := 1;
         instr[242]:='eext*     '; insp[242] := false; insq[242] := 0;          insr[242] := 0;
         instr[243]:='wbs       '; insp[243] := false; insq[243] := 0;          insr[243] := 1;
         instr[244]:='wbe       '; insp[244] := false; insq[244] := 0;          insr[244] := 0;
         instr[245]:='sfr       '; insp[245] := false; insq[245] := intsize;    insr[245] := 0;
         instr[246]:='cuf       '; insp[246] := false; insq[246] := intsize;    insr[246] := 0;
         instr[247]:='cif       '; insp[247] := false; insq[247] := 0;          insr[247] := 0;

         sptable[ 0]:='get       '; spfunc[ 0]:=false; sppar[ 0]:=1; spkeep[ 0]:=false;
         sptable[ 1]:='put       '; spfunc[ 1]:=false; sppar[ 1]:=1; spkeep[ 1]:=false;
         sptable[ 2]:='thw       '; spfunc[ 2]:=false; sppar[ 2]:=1; spkeep[ 2]:=false;   
         sptable[ 3]:='rln       '; spfunc[ 3]:=false; sppar[ 3]:=1; spkeep[ 3]:=true;
         sptable[ 4]:='new       '; spfunc[ 4]:=false; sppar[ 4]:=2; spkeep[ 4]:=false;   
         sptable[ 5]:='wln       '; spfunc[ 5]:=false; sppar[ 5]:=1; spkeep[ 5]:=true;
         sptable[ 6]:='wrs       '; spfunc[ 6]:=false; sppar[ 6]:=4; spkeep[ 6]:=true;   
         sptable[ 7]:='eln       '; spfunc[ 7]:=false; sppar[ 7]:=1; spkeep[ 7]:=false;
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
         sptable[37]:='rib       '; spfunc[37]:=false; sppar[37]:=3; spkeep[37]:=true;
         sptable[38]:='rcb       '; spfunc[38]:=false; sppar[38]:=3; spkeep[38]:=true;   
         sptable[39]:='nwl       '; spfunc[39]:=false; sppar[39]:=0; spkeep[39]:=false; { special }
         sptable[40]:='dsl       '; spfunc[40]:=false; sppar[40]:=0; spkeep[40]:=false; { special }
         sptable[41]:='eof       '; spfunc[41]:=true;  sppar[41]:=1; spkeep[41]:=false;
         sptable[42]:='efb       '; spfunc[42]:=true;  sppar[42]:=1; spkeep[42]:=false;   
         sptable[43]:='fbv       '; spfunc[43]:=false; sppar[43]:=1; spkeep[43]:=false;
         sptable[44]:='fvb       '; spfunc[44]:=false; sppar[44]:=1; spkeep[44]:=false;
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
         sptable[77]:='rdsp      '; spfunc[77]:=false; sppar[77]:=4; spkeep[77]:=true;
         sptable[78]:='aeft      '; spfunc[78]:=false; sppar[78]:=3; spkeep[78]:=false;
         sptable[79]:='aefb      '; spfunc[79]:=false; sppar[79]:=3; spkeep[79]:=false;
         sptable[80]:='rdie      '; spfunc[80]:=false; sppar[80]:=3; spkeep[80]:=false;
         sptable[81]:='rdre      '; spfunc[81]:=false; sppar[81]:=1; spkeep[81]:=false;

         for i:= 1 to 10 do word[i]:= ' ';
         for i:= 0 to maxlabel do
             with labeltab[i] do begin val:=-1; st:= entered end;
         { initalize file state }
         for i := 1 to maxfil do filstate[i] := fclosed;

         { !!! remove this next statement for self compile }
         {elide}reset(prd);{noelide}

         sline := 0; { set no line of source }
         iline := 1; { set 1st line of intermediate }
         flablst := nil; { clear far label list }
         estack := nil; stacklvl := 0; efree := nil;
         allreg := [rgrax, rgrbx, rgr11, rgr12, rgr13, rgr14, rgr15,
                    rgxmm0, rgxmm1, rgxmm2, rgxmm3, rgxmm4, rgxmm5, rgxmm6,
                    rgxmm7, rgxmm8, rgxmm9, rgxmm10, rgxmm11, rgxmm12, rgxmm13,
                    rgxmm14, rgxmm15];
         frereg := allreg;
         { set parameter registers }
         parreg[1] :=rgrdi;
         parreg[2] :=rgrsi;
         parreg[3] :=rgrdx;
         parreg[4] :=rgrcx;
         parreg[5] :=rgr8;
         parreg[6] :=rgr9;
         parreg[7] :=rgnull;
         parreg[8] :=rgnull;
         parreg[9] :=rgnull;
         parreg[10] :=rgnull;
         parreg[11] :=rgnull;
         parreg[12] :=rgnull;
   end;(*init*)

   procedure errorl(string: beta); (*error in loading*)
   begin writeln;
      writeln('*** Program load error: [', sline:1, ',', iline:1, '] ', string);
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
   var i, p: integer;
   begin
     strassvf(labeltab[x].ref, sn); strchrass(labeltab[x].ref, snl+1, '.'); i := snl+2;
     p := maxpow10;
     while p > 0 do begin
       if ((x div p) mod 10 <> 0) or (p = 1) then begin
         strchrass(labeltab[x].ref, i, chr((x div p) mod 10+ord('0'))); 
         i := i+1;
       end;
       p := p div 10
     end
   end;

   procedure update(x: labelrg; pc: boolean); (*when a label definition lx is found*)
      var curr,succ,ad: address; (*resp. current element and successor element
                               of a list of future references*)
          op: instyp; q : address;  (*instruction register*)
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
                   else if (sn[i] = '(') or (sn[i] = ')') then sn[i] := '_'
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
          'v': getlin; { variant logical table }
          't': getlin; { template }
          'n': getlin; { start constant table }
          'x': getlin;
          'c': getlin;
       end
     end
   end; (*generate*)

    procedure assemble; (*translate symbolic code into machine code and store*)

      var name :alfa; r :real; s :settype;
          i,x,s1,lb,ub,l:integer; c: char;
          str: strbuf; { buffer for string constants }
          cstp: cstptr;
          ep, ep2, ep3, ep4, ep5, pp: expptr;
          r1, r2: reg; ors: set of reg; rage: array [reg] of integer;
          rcon: array [reg] of expptr; domains: array [1..maxreg] of expptr;
          sp, sp2: strvsp; def, def2: boolean; val, val2: integer;

      procedure lookup(x: labelrg); (* search in label table*)
      begin case labeltab[x].st of
                entered: begin q := labeltab[x].val;
                           labeltab[x].val := pc
                         end;
                defined: q:= labeltab[x].val
            end(*case label..*)
      end;(*lookup*)

      procedure labelsearch(var def: boolean; var val: integer; var sp: strvsp);
         var x: integer; flp: flabelp;
      begin def := false; val := 0; flp := nil; skpspc; 
        if ch <> 'l' then errorl('Label format error       ');
        getnxt; parlab(x,sp);
        if sp <> nil then begin { far label }
          new(flp); flp^.next := flablst; flablst := flp;
          flp^.val := pc; flp^.ref := sp; q := 0
        end else begin { near label }
          lookup(x); if labeltab[x].ref = nil then putlabel(x);
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
        else new(ep);
        ep^.next := nil; ep^.op := op; ep^.p := p; ep^.q := q; ep^.q1 := q1;
        ep^.q2 := q2; ep^.l := nil; ep^.r := nil; ep^.x1 := nil; ep^.sl := nil;
        ep^.pl := nil; ep^.r1 := rgnull; ep^.r2 := rgnull; ep^.r3 := rgnull;
        ep^.rs := []; ep^.wkeep := false; ep^.keep := false; ep^.fn := nil; 
        ep^.lb := nil
      end;
      
      procedure putexp(ep: expptr);
      begin
        ep^.next := efree; efree := ep
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
        if estack <> nil then errorl('Stack balance             ');
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
        if ep^.pl <> nil then deltre(ep^.pl);
        putexp(ep)
      end;
      
      procedure dmptrel(ep: expptr; lvl: integer);
      var l: expptr;
      begin
        writeln(prr, '# ', ' ': lvl, ep^.op:3, ': ', instr[ep^.op]);
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

      procedure dmpstk;
      var ep: expptr;
      begin
        ep := estack;
        while ep <> nil do begin
          writeln(prr, '# Stack: ', ep^.op:3, ': ', instr[ep^.op]);
          ep := ep^.next
        end
      end;

      procedure dmplst(ep: expptr);
      begin
        while ep <> nil do begin
          dmptre(ep);
          ep := ep^.next
        end
      end;

      procedure getreg(var r: reg; var rf: regset);
      begin
        r := rgrax;
        while not (r in rf) and (r < rgr15) do r := succ(r);
        if not (r in rf) then errorl('Out of registers         ');
        rf := rf-[r];
      end;

      procedure getfreg(var r: reg; var rf: regset);
      begin
        r := rgxmm0;
        while not (r in rf) and (r < rgxmm15) do r := succ(r);
        if not (r in rf) then errorl('Out of registers         ');
        rf := rf-[r];
      end;

      procedure assreg(ep: expptr; rf: regset; r1, r2: reg);
      var rs: regset; pp: expptr;

      procedure resreg(r: reg);
      begin
        if not (r in rf) and (r <> r1) and (r <> r2) then 
          begin ep^.rs := ep^.rs+[r]; rf := rf-[r] end
      end;

      { assign registers to parameters in call }
      procedure asspar(ep: expptr);
      var pp: expptr; pc: integer;
      begin
        pp := ep^.pl; pc := 1;
        while pp <> nil do begin
          if insr[pp^.op] = 2 then begin { double register }
            assreg(pp, frereg, parreg[pc], parreg[pc+1]);
            pc := pc+2
          end else begin { single register }
            assreg(pp, frereg, parreg[pc], rgnull);
            pc := pc+1
          end;            
          pp := pp^.next
        end
      end;

      begin
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
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) end;

          {adr,sbr}
          29, 31: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) end;

          {equr,neqr,geqr,grtr,leqr,lesr}
          138,144,150,156,162,168: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgnull, rgnull); assreg(ep^.r, rf, rgnull, rgnull) end;

          {grts,less}
          158,170: ; { are invalid }

          {equs,neqs,geqs,leqs}
          140,146,152,164: begin resreg(rgrax);
            assreg(ep^.l, rf, rgrdi, rgnull); 
            assreg(ep^.r, rf, rgrsi, rgnull)
          end;

          {adi,adr,sbi,sbr,equ,neq,geq,grt,leq,les}
          28, 30, 17, 137, 139, 141, 18, 143, 145, 
          147, 19, 149, 151, 153, 20, 155, 157, 159, 21, 
          161, 163, 165, 167, 169, 171: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) 
          end;

          120{lip}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            ep^.r2 := r2; if ep^.r2 = rgnull then getreg(ep^.r2, rf) end; 

          {equm,neqm,geqm,grtm,leqm,lesm}
          142, 148, 154, 160, 166, 172: begin resreg(rgrax); resreg(rgrdx); 
            assreg(ep^.l, rf, rgrdi, rgnull); 
            assreg(ep^.r, rf, rgrsi, rgnull);  
          end;

          5{lao}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          16{ixa}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrax, r2); assreg(ep^.r, rf, ep^.r1, rgnull);
            resreg(rgrdx) end;

          118{swp}: ; { done at top level }

          {ldoi,ldoa,ldor,ldob,ldoc,ldox}
          1,65,66,68,69,194:begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          {ldos}
          67: begin resreg(rgrsi); resreg(rgrdi); ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          {ind,inda,indb,indc,indx}
          9, 85,88,89,198: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
            assreg(ep^.l, rf, ep^.r1, rgnull) 
          end;

          {indr}
          86: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf); 
            assreg(ep^.l, rf, rgnull, rgnull) 
          end;

          {inds}
          87: begin resreg(rgrsi); resreg(rgrdi); ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          {inc,dec}
          10, 90, 93, 94, 57, 103, 104, 201, 202: begin 
            ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
          end;

          {suv}
          91: ;

          {ckvi,ckvb,ckvc,ckvx}
          175, 179, 180, 203: begin
            ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull); assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {cvbi,cvbx,cvbb,cvbc}
          100, 115, 116, 121: begin resreg(rgrdi); resreg(rgrsi); resreg(rgrdx);
            resreg(rgr8); assreg(ep^.l, rf, rgrcx, rgnull); assreg(ep^.r, rf, rgr9, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgrax
          end;

          {ivti,ivtx,ivtb,ivtc}
          192,101,102,111: begin resreg(rgrdi); resreg(rgrsi); resreg(rgrdx);
            resreg(rgr8); assreg(ep^.l, rf, rgrcx, rgnull); assreg(ep^.r, rf, rgr9, rgnull);
            ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgrax
          end;

          {cps}
          176: begin resreg(rgrdi); ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
            if ep^.r2 = rgnull then getreg(ep^.r2, rf)
          end;

          {cpc}
          177: begin resreg(rgrdi);
            assreg(ep^.l, rf, rgnull, rgrsi); assreg(ep^.r, rf, rgnull, rgrdx)
          end;

          {cta}
          191: begin resreg(rgrdi); resreg(rgrsi); resreg(rgrdx);
            assreg(ep^.l, rf, rgnull, rgrcx); assreg(ep^.r, rf, rgnull, rgr8)
          end;

          {lpa}
          114: begin ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
            if ep^.r2 = rgnull then getreg(ep^.r2, rf)
          end;

          {ldci,ldcc,ldcb}
          123,127,126: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          {ldcn}
          125: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) end;

          {ldcr}
          124: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) end;

          {ldcs}
          7: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          {chk}
          26, 95, 98, 99, 199: begin resreg(rgrax); ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            getreg(ep^.r2, rf); assreg(ep^.l, rf, rgnull, rgnull);
          end;

          {chks}
          97: begin resreg(rgrdi); resreg(rgrsi); ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          {ckla}
          190: begin resreg(rgrax); ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf)
          end;

          56 {lca}: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) 
          end;

          {ordi,ordb,ordc,ordx}
          59, 134, 136, 200: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, r1, r2)
          end;

          {lcp}
          135: begin ep^.r1 := r1; ep^.r2 := r2;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf)
          end;

          {sgs}
          32: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdi, rgnull)  
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
          205: begin resreg(rgrax); ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2) 
          end;

          {notb,odd,rnd,chr}
          42,50,60,62: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2) 
          end;

          {and,ior,xor}
          43,44,206: begin resreg(rgrax); ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) 
          end;

          {dif,int,uni}
          45,46,47: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then ep^.r1 := rgrdi;
            assreg(ep^.l, rf, rgrdi, r2); assreg(ep^.r, rf, rgrsi, rgnull) 
          end;

          {inn}
          48: begin 
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else begin resreg(rgrax); ep^.r1 := r1 end;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdi, r2); assreg(ep^.r, rf, rgrsi, rgnull) 
          end;

          {mod}
          49: begin 
            if (r1 = rgnull) and (rgrdx in rf) then ep^.r1 := rgrdx
            else begin resreg(rgrdx); ep^.r1 := r1 end;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrax, r2); assreg(ep^.r, rf, rgnull, rgnull) 
          end;

          {dvi}
          53: begin 
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else begin resreg(rgrax); ep^.r1 := r1 end;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrax, r2); assreg(ep^.r, rf, rgnull, rgnull) 
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
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else begin resreg(rgrax); ep^.r1 := r1 end;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgrdi, r2); assreg(ep^.r, rf, rgrsi, rgnull) 
          end;

          { dupi, dupa, dupr, dups, dupb, dupc }
          181, 182, 183, 184, 185, 186: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull); 
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {cks}
          187: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, ep^.r1, rgnull); 
            assreg(ep^.r, rf, rgnull, rgnull)
          end;

          {csp}
          15: begin
            if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then begin
              { need irregular allocation for nwl and dsl }
              pp := ep^.pl; assreg(pp, frereg, rgrsi, rgnull);
              pp := pp^.next; assreg(pp, frereg, rgrcx, rgnull);
              resreg(rgrax); resreg(rgrbx); resreg(rgrcx); resreg(rgrdi)
            end else if spfunc[ep^.q] then begin { function }
              if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
              else begin resreg(rgrax); ep^.r1 := r1 end;
              if ep^.r1 = rgnull then getreg(ep^.r1, rf)
            end;
            asspar(ep)
          end;
 
          {cuf}
          246: begin 
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else begin resreg(rgrax); ep^.r1 := r1 end;
            asspar(ep)
          end;

          {cup}
          12: asspar(ep);

          {cip}
          113: begin
            asspar(ep); assreg(ep^.l, rf, rgnull, rgnull)
          end;

        end
      end;

      { Interpret instruction macro string.
        The macros are:
        $0 - Immediate integer 1
        $1 - Immediate integer 2
        $s - Immediate symbol
        %1 - Register 1
        %2 - register 2
        +0 - Immediate integer 1
        +1 - Immediate integer 2
        -0 - Immediate integer 1
        -1 - Immediate integer 2
        ^0 - Immediate integer 1 without leader
        ^1 - Immediate integer 2 without leader
        @  - Symbol
      }
      procedure wrtins40(si: insstr40; i1, i2: integer; r1, r2: reg; sn: strvsp);
      var i,j: 1..insmax40; n: integer;
      procedure next;
      begin if i = insmax40 then errorl('Error in instruction     '); i := i+1
      end;
        
      begin
        write(prr, ' ':tabspc);
        i := 1; while si[i] <> ' ' do begin write(prr, si[i]); next end;
        j := i;
        while j <= tabspc do begin write(prr, ' '); j := j+1 end;
        while (i < insmax40) and (si[i] = ' ') do next;
        while i < insmax40 do begin
          if si[i] = '$' then begin next; write(prr, '$');
            if si[i] = '0' then write(prr, i1:1) 
            else if si[i] = '1' then write(prr, i2:1)
            else if si[i] = 's' then writevp(prr, sn)
            else write(prr, si[i])
          end else if si[i] = '%' then begin next; write(prr, '%');
            if si[i] = '1' then wrtreg(prr, r1) 
            else if si[i] = '2' then wrtreg(prr, r2)
            else write(prr, si[i])
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
          end else if si[i] = '@' then begin next; writevp(prr, sn) end
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
      var r: reg;

      { push parameters to call depth first }
      procedure pshpar(ep: expptr);
      begin
        if ep <> nil then begin
          pshpar(ep^.next);
          dmptrel(ep, 1); genexp(ep);
          if ep^.r2 <> rgnull then
            wrtins20('pushq %1  ', 0, 0, ep^.r2, rgnull, nil);
          if ep^.r1 in [rgrax..rgr15] then
            wrtins20('pushq %1  ', 0, 0, ep^.r1, rgnull, nil);
          if ep^.r1 in [rgxmm0..rgxmm15] then begin
            wrtins20('subq -0,%esp        ', realsize, 0, rgnull, rgnull, nil);
            wrtins20('movsd %1,(%esp)     ', 0, 0, ep^.r1, rgnull, nil)
          end
        end
      end;

      procedure callsp(ep: expptr; var sc: alfa; r: boolean);
      var si: insstr20; i: integer; pp: expptr; pc: integer;
      begin
        { evaluate all parameters }
        pp := ep^.pl;
        while pp <> nil do begin 
          if not pp^.keep then genexp(pp); 
          pp := pp^.next; pc := pc+1 
        end;
        if ep^.pl <> nil then if ep^.pl^.keep then
          { restore kept from stack }
          wrtins10('popq %rdi ', 0, 0, rgnull, rgnull, nil);
        { activate keep }
        if ep^.pl <> nil then ep^.pl^.keep := ep^.pl^.wkeep;
        { must do this after parameter eval }
        if ep^.pl <> nil then if ep^.pl^.keep then { stack kept }
          wrtins10('pushq %rdi', 0, 0, rgrdi, rgnull, nil);
        si := 'call psystem_       ';
        for i := 1 to maxalfa do if sc[i] <> ' ' then si[14+i-1] := sc[i];
        wrtins20(si, 0, 0, rgnull, rgnull, nil);
        if r then begin
          if ep^.r1 <> rgrax then 
            wrtins20('movq %rax,%1        ', ep^.p, 0, ep^.r1, rgnull, nil);
          if (ep^.r2 <> rgnull) and (ep^.r2 <> rgrdx) then
            wrtins20('movq %rdx,%1        ', ep^.p, 0, ep^.r2, rgnull, nil);
        end
      end;

      begin
        if ep <> nil then begin
          if ep^.op <> 113{cip} then genexp(ep^.l); genexp(ep^.r); 
          genexp(ep^.x1);
          for r := rgrax to rgr15 do if r in ep^.rs then
              wrtins10('push %1   ', 0, 0, r, rgnull, nil);
          case ep^.op of

            {lodi,loda}
            0,105: begin
              wrtins20('movq ^0(%rbp),%1    ', ep^.q1, 0, ep^.r1, rgnull, nil);
              wrtins20('movq ^0(%1),%1      ', ep^.q, 0, ep^.r1, rgnull, nil);
            end;

            {lodx,lodb,lodc}
            193,108,109: begin
              wrtins20('movq ^0(%rbp),%1    ', ep^.p, 0, ep^.r1, rgnull, nil);
              wrtins20('movzx ^0(%1),%1      ', ep^.q, 0, ep^.r1, rgnull, nil);
            end;

            {lodr}
            106: begin
              wrtins20('movq ^0(%rbp),%1    ', ep^.p, 0, ep^.t1, rgnull, nil);
              wrtins20('movsd (%rax),%2     ', 0, 0, ep^.t1, ep^.r1, nil)
            end;

            {lods}
            107: begin
              wrtins20('movq ^0(%rbp),%rsi  ', ep^.p, 0, rgnull, rgnull, nil);
              wrtins20('lea ^0(%rsi),%rsi   ', ep^.q, 0, ep^.r1, rgnull, nil);
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull, nil);
              wrtins20('movq %rax,%rsi      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%rdi      ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%1        ', 0, 0, rgnull, ep^.r1, nil)
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
                150{geqr}: wrtins20('cmpnltsd %1,%2      ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                156{grtr}: wrtins20('cmpltsd %2,%1       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                162{leqr}: wrtins20('cmpltsd %2,%1       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
                168{lesr}: wrtins20('cmpltsd %1,%2       ', 0, 0, ep^.r^.r1, ep^.l^.r1, nil);
              end;
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
              wrtins20('movq $0,%rdx        ', q, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_strcmp ', 0, 0, rgnull, rgnull, nil); 
              wrtins20('cmpq $0,%rax        ', 0, 0, rgnull, rgnull, nil);
              case ep^.op of
                142{equm}: wrtins10('sete %1   ', 0, 0, ep^.l^.r1, rgnull, nil);
                148{neqm}: wrtins10('setne %1  ', 0, 0, ep^.l^.r1, rgnull, nil);
                154{geqm}: wrtins10('setae %1  ', 0, 0, ep^.l^.r1, rgnull, nil);
                160{grtm}: wrtins10('seta %1   ', 0, 0, ep^.l^.r1, rgnull, nil);
                166{leqm}: wrtins10('setbe %1  ', 0, 0, ep^.l^.r1, rgnull, nil);
                172{lesm}: wrtins10('setb %1   ', 0, 0, ep^.l^.r1, rgnull, nil);
              end
            end;

            5{lao}:
              wrtins40('leaq globals_start+0(%rip),%1          ', ep^.q, 0, ep^.r1, rgnull, nil);

            16{ixa}: begin 
              wrtins20('movq $0,%rax        ', q, 0, rgnull, rgnull, nil);
              wrtins10('mul %1    ', 0, 0, ep^.l^.r1, rgnull, nil);
              wrtins20('add %rax,%1         ', 0, 0, ep^.r^.r1, rgnull, nil);
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
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%rdi      ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%1        ', 0, 0, rgnull, ep^.l^.r1, nil);
            end;

            {indi,inda}
            9,85: 
              wrtins20('movq ^0(%1),%1  ', q, 0, ep^.l^.r1, rgnull, nil);

            {indr}
            86: 
              wrtins20('movsd ^0(%1),%1  ', q, 0, ep^.l^.r1, ep^.r1, nil);

            {indb,indc,indx}
            88,89,198: wrtins20('movzx +0(%1),%1  ', q, 0, ep^.l^.r1, rgnull, nil);

            {inds}
            87: begin 
              wrtins20('leaq ^0(%1),%1  ', q, 0, ep^.l^.r1, rgnull, nil);
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%rdi      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%1        ', 0, 0, rgnull, ep^.l^.r1, nil)
            end;

            {inci,inca,incb,incc,incx}
            10, 90, 93, 94, 201: 
              wrtins20('addq $0,%1          ', q, 0, ep^.r1, rgnull, nil);

            {deci,decb,decc,decx}
            57, 103, 104, 202: 
              wrtins20('subq $0,%1          ', q, 0, ep^.r1, rgnull, nil);

            {ckvi,ckvb,ckvc,ckvx}
            175, 179, 180, 203: begin 
              wrtins20('cmpq $0,%1          ', q, 0, ep^.r^.r1, rgnull, nil);
              wrtins20('sete %1             ', 0, 0, ep^.r^.r1, rgnull, nil);
              wrtins20('orq %1,%2           ', 0, 0, ep^.r^.r1, ep^.r1, nil);
            end;

            {cvbi,cvbx,cvbb,cvbc}
            100, 115, 116, 121: begin
              wrtins20('movq $0,%rdi        ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rsi        ', ep^.q1, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rdx        ', ep^.q2, 0, rgnull, rgnull, nil);
              if ep^.op = 100 then
                wrtins20('movq (%1),%r8       ', ep^.q, 0, ep^.r^.r1, rgnull, nil)
              else
                wrtins20('movzx (%1),%r8      ', ep^.q, 0, ep^.r^.r1, rgnull, nil);
              wrtins30('call psystem_tagchgvar         ', 0, 0, rgnull, rgnull, nil)
            end;

            {ivti,ivtx,ivtb,ivtc}
            192,101,102,111: begin
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rsi         ', ep^.q1, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rdx         ', ep^.q2, 0, rgnull, rgnull, nil);
              if ep^.op = 100 then
                wrtins20('movq (%1),%r8        ', ep^.q, 0, ep^.r^.r1, rgnull, nil)
              else
                wrtins20('movzx (%1),%r8       ', ep^.q, 0, ep^.r^.r1, rgnull, nil);
              wrtins30('call psystem_tagchginv         ', 0, 0, rgnull, rgnull, nil)
            end;

            {cps}
            176: begin 
              wrtins20('cmpq %1,%2        ', 0, 0, ep^.r^.r2, ep^.l^.r2, nil);
              wrtins20('je .+21           ', ep^.q, 0, ep^.r^.r1, rgnull, nil);
              wrtins20('movq $0,%rdi      ', ContainerMismatch, 0, rgnull, rgnull, nil);
              wrtins30('call psystem_errorv         ', 0, 0, rgnull, rgnull, nil)
            end;

            {cpc}
            177: begin 
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins30('call psystem_cmptmp            ', 0, 0, rgnull, rgnull, nil)
            end;

            {cta}
            191: begin
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rsi         ', ep^.q1, 0, rgnull, rgnull, nil);
              wrtins20('movq $0,%rdx         ', ep^.q2, 0, rgnull, rgnull, nil);
              wrtins30('call psystem_tagchkass           ', 0, 0, rgnull, rgnull, nil)
            end;

            {lpa}
            114: begin 
              wrtins20('leaq @s(%rip),%1    ', 0, 0, ep^.r1, rgnull, ep^.fn);
              wrtins20('movq ^0(%rbp),%1    ', ep^.q1, 0, ep^.r2, rgnull, nil)
            end;

            {ldci,ldcc,ldcb}
            123,127,126:
              wrtins20('movq $0,%1          ', ep^.vali, 0, ep^.r1, rgnull, nil); 

            {ldcn}
            125:
              wrtins20('movq $0,%1          ', 0, 0, ep^.r1, rgnull, nil);

            {ldcr}
            124: begin 
               write(prr, '        movsd   real', ep^.realn:1, '(%rip),%'); 
               wrtreg(prr, ep^.r1); writeln(prr)
            end;

            {ldcs}
            7: begin 
               write(prr, '        leaq    set', ep^.realn:1, '(%rip),%'); 
               wrtreg(prr, ep^.r1); writeln(prr)
            end;

            {chki,chka,chkb,chkc,chkx}
            26, 95, 98, 99, 199: begin 
              wrtins20('movq $0,%1          ', 0, 0, ep^.r2, rgnull, nil);
              wrtins20('cmpq (%1),%2        ', 0, 0, ep^.r2, ep^.r1, nil);
              wrtins20('jae .+21            ', 0, 0, ep^.r2, rgnull, nil);
              wrtins20('movq $ValueOutOfRange,%rax    ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call errore         ', 0, 0, rgnull, rgnull, nil);
              wrtins20('cmpq ^0(%1),%2      ', intsize, 0, ep^.r2, ep^.r1, nil);
              wrtins20('jbe .+11            ', 0, 0, ep^.r2, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil)
            end;

            {chks}
            97: begin
              wrtins20('movq $0,%rsi         ', ep^.q, 0, rgnull, rgnull, nil);
              wrtins20('movq ^0(%rsi),%rdi   ', intsize, 0, rgnull, rgnull, nil);
              wrtins20('movq (%rsi),%rsi     ', intsize, 0, rgnull, rgnull, nil);
              wrtins30('call psystem_chksetbnd         ', 0, 0, rgnull, rgnull, nil)
            end;

            {ckla}
            190: begin
              if ep^.q <> 0 then begin
                wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
                wrtins20('jbe .+17            ', 0, 0, ep^.r2, rgnull, nil);
                wrtins20('movq $DereferenceOfNilPointer,%rax      ', 0, 0, rgnull, rgnull, nil);
                wrtins20('call psystem_errorv ', 0, 0, rgnull, rgnull, nil)
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
              wrtins20('movq %eax,%1        ', 0, 0, ep^.l^.r1, rgnull, nil);
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
                17,137,138,139,141: wrtins10('sete %1   ', 0, 0, ep^.r1, rgnull, nil);
                18,143,144,145,147: wrtins10('setne %1  ', 0, 0, ep^.l^.r1, rgnull, nil);
                149,150,151,153: wrtins10('setae %1  ', 0, 0, ep^.l^.r1, rgnull, nil);
                155,156,157,159: wrtins10('seta %1   ', 0, 0, ep^.l^.r1, rgnull, nil);
                161,162,163,165: wrtins10('setbe %1  ', 0, 0, ep^.l^.r1, rgnull, nil);
                167,168,169,171: wrtins10('setb %1   ', 0, 0, ep^.l^.r1, rgnull, nil);
              end
            end;

            {ord}
            59, 134, 136, 200: ; { ord is a no-op }

            {lcp}
            135: begin 
              wrtins20('movq (%1),%2        ', 0, 0, ep^.l^.r1, ep^.r1, nil);
              wrtins20('movq ^0(%1),%2      ', ptrsize, 0, ep^.l^.r1, ep^.r2, nil)
            end;

            {sgs}
            32: begin
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%rsi      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_setsgl ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%1        ', 0, 0, ep^.r1, rgnull, nil)
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
              wrtins20('subsd %1,%2         ', 0, 0, ep^.r1, ep^.l^.r1, nil)
            end;

            {sqi}
            38: begin 
              wrtins20('addq %1,%1          ', 0, 0, ep^.r1, rgnull, nil);
            end;

            {sqr}
            39: begin 
              wrtins20('addsd %1,%1         ', 0, 0, ep^.r1, rgnull, nil);
            end;

            {abi}
            40: begin
              wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('jns .+8             ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('negq %1             ', 0, 0, ep^.r1, rgnull, nil)
            end;

            {abr}
            41: begin 
              wrtins30('movq $8000000000000000,%1    ', 0, 0, ep^.t1, rgnull, nil);
              wrtins20('movq %1,%2          ', 0, 0, ep^.t1, ep^.t2, nil);
              wrtins20('xorpd %1,%2         ', 0, 0, ep^.t2, ep^.r1, nil)
            end;

            {notb}
            42: begin 
              wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('movq $1%1           ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('jz .+17             ', 0, 0, ep^.r2, rgnull, nil);
              wrtins20('movq $0%1           ', 0, 0, ep^.r1, rgnull, nil)
            end;

            {noti}
            205: begin 
              wrtins20('orq %1,%1           ', 0, 0, ep^.r1, rgnull, nil);
              wrtins20('jns .+17            ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq $BooleanOperatorOfNegative,%rax      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins20('not %1              ', 0, 0, ep^.r1, rgnull, nil)
            end;

            {odd}
            50: begin 
              wrtins20('andq $1,%1          ', 0, 0, ep^.r1, rgnull, nil);
            end;

            {rnd}
            62: wrtins20('cvtsd2si %1,%2      ', 0, 0, ep^.l^.r1, ep^.r1, nil);

            {chr}
            60: ; { chr is no-op }

            {and,ior,xor}
            43,44,206: begin 
              wrtins20('orq %1,%1           ', 0, 0, ep^.l^.r1, rgnull, nil);
              wrtins20('jns .+17            ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq $BooleanOperatorOfNegative,%rax      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
              wrtins20('orq %1,%1           ', 0, 0, ep^.r^.r1, rgnull, nil);
              wrtins20('jns .+17            ', 0, 0, rgnull, rgnull, nil);
              wrtins20('movq $BooleanOperatorOfNegative,%rax      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull, nil);
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
              wrtins20('addq $0,%rsp        ', setsize, 0, rgnull, rgnull, nil);
              if ep^.r1 <> ep^.l^.r1 then
                wrtins20('movq %1,%1           ', 0, 0, rgrdi, ep^.l^.r1, nil)
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
              wrtins10('idivq %1  ', 0, 0, ep^.r^.r1, rgnull, nil);
              if ep^.r1 <> rgrax then
                wrtins20('movq %rax,%1        ', 0, 0, ep^.r1, rgnull, nil)
            end;

            {mpi}
            51: wrtins10('imulq %1,%2        ', 0, 0, ep^.r^.r1, rgnull, nil);

            {mpr}
            52: wrtins10('imulsd %1,%2       ', 0, 0, ep^.r^.r1, rgnull, nil);

            {dvr}
            54: wrtins10('idivsd %1,%2       ', 0, 0, ep^.r^.r1, rgnull, nil);

            {rgs}
            110: begin 
              wrtins20('subq ^0,%rsp        ', setsize, 0, rgnull, rgnull, nil);
              wrtins20('movq %rsp,%rdx      ', 0, 0, rgnull, rgnull, nil);
              wrtins20('call psystem_setrgs ', 0, 0, rgnull, rgnull, nil);
              if ep^.r1 <> rgrdx then
                wrtins20('movq %rdx,%1        ', 0, 0, ep^.r1, rgnull, nil)
            end;

            { dupi, dupa, dupr, dups, dupb, dupc }
            181, 182, 183, 184, 185, 186: 
              wrtins10('movq %2,%1', 0, 0, ep^.l^.r1, ep^.r^.r1, nil);

            {cks}
            187: wrtins10('xorq %1,%1', 0, 0, ep^.r^.r1, rgnull, nil);

            {csp}
            15: begin
              if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then begin
                { need irregular handling for nwl and dsl }
                pp := ep^.pl; genexp(pp);
                pp := pp^.next; genexp(pp);
                wrtins20('movq $0,%rax        ', intsize, 0, rgnull, rgnull, nil);
                wrtins20('mulq %rcx ', 0, 0, rgnull, rgnull, nil);
                wrtins20('addq %rsp,%rax      ', 0, 0, rgnull, rgnull, nil);
                wrtins20('movq (%rax),%rdi    ', 0, 0, rgnull, rgnull, nil);
                wrtins20('movq %rsp,%rcx      ', 0, 0, rgnull, rgnull, nil);
                wrtins20('movq %rcx,%rbx      ', 0, 0, rgnull, rgnull, nil);
                if ep^.q = 39 then
                  wrtins20('call psystem_nwl    ', 0, 0, rgnull, rgnull, nil)
                else
                  wrtins20('call psystem_dsl    ', 0, 0, rgnull, rgnull, nil);
                wrtins20('orq %rbx,%rbx       ', 0, 0, rgnull, rgnull, nil);
                wrtins20('jz .+16   ', 0, 0, rgnull, rgnull, nil);  
                wrtins20('popq %rax ', 0, 0, rgnull, rgnull, nil);
                wrtins20('decq %rbx ', 0, 0, rgnull, rgnull, nil);
                wrtins20('jmp .-12  ', 0, 0, rgnull, rgnull, nil);
              end else callsp(ep, sptable[ep^.q], spfunc[ep^.q]);
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

            {cip}
            113: begin
              genexp(ep^.sl); { process sfr start link }
              pshpar(ep^.pl); { process parameters first }
              genexp(ep^.l); { load procedure address }
              wrtins20('movq ^0(%1),%rbp   ', 1*ptrsize, 0, ep^.l^.r1, rgnull, nil);
              wrtins10('call *(%1)', 0, 0, ep^.l^.r1, rgnull, nil);
            end;

          end;
          for r := rgr15 downto rgrax do if r in ep^.rs then 
            wrtins20('pop %1              ', 0, 0, r, rgnull, nil)
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
          wrtins20('pushq %1  ', 0, 0, ep^.r2, rgnull, nil);
        if ep^.r1 in [rgrax..rgr15] then
          wrtins20('pushq %1  ', 0, 0, ep^.r1, rgnull, nil);
        if ep^.r1 in [rgxmm0..rgxmm15] then begin
          wrtins20('subq -0,%esp        ', realsize, 0, rgnull, rgnull, nil);
          wrtins20('movsd %1,(%esp)     ', 0, 0, ep^.r1, rgnull, nil)
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
          while pn > 0 do 
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

    begin { assemble } 
      p := 0;  q := 0;  op := 0;
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
          q1 := -p*ptrsize; getexp(ep); pshstk(ep) 
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
        5: begin read(prd,q); writeln(prr,q:1); getexp(ep); pshstk(ep)
        end;

        {ixa}
        16: begin read(prd,q); writeln(prr,q:1); getexp(ep); 
          popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
        end;

        {swp}
        118: begin read(prd,q); writeln(prr,q:1); popstk(ep); 
          popstk(ep2); pshstk(ep); pshstk(ep2) 
        end;

        {ldoi,loda,lodr,lods,lodb,lodc,lodx}
        1, 65, 66, 67, 68, 69, 194: begin read(prd,q); writeln(prr,q:1);
          getexp(ep); pshstk(ep) 
        end;

        {indi,inda,indr,inds,indb,indc,indx}
        9, 85, 86, 87, 88, 89, 198: begin read(prd,q); writeln(prr,q:1) end;

        {inci,inca,incb,incc,incx,deci,deca,decb,decc,decx}
        10, 90, 93, 94, 201, 57, 103, 104, 202: begin read(prd,q); 
          writeln(prr,q:1); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {ckvi,ckvb,ckvc,ckvx}
        175, 179, 180, 203: begin read(prd,q); writeln(prr,q:1); 
          getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {cvbi,cvbx,cvbb,cvbc}
        100, 115, 116, 121: begin read(prd,q, q1, q2); 
          writeln(prr,q:1, ' ', q1:1, ' ', q2:1); getexp(ep); popstk(ep^.r); 
          popstk(ep^.l); pshstk(ep) 
        end;

        {ivti,ivtx,ivtb,ivtc}
        192,101,102,111: begin read(prd,q, q1); writeln(prr,q:1, ' ', q1:1);
          getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
        end;

        {cps}
        176: begin writeln(prr); getexp(ep); popstk(ep^.r); popstk(ep^.l); 
          pshstk(ep) 
        end;

        {cpc}
        177: begin writeln(prr); getexp(ep); popstk(ep^.r); popstk(ep^.l);
          pshstk(ep) 
        end;

        {cta}
        191: begin read(prd,q, q1, q2); 
          writeln(prr,q:1, ' ', q1:1, ' ', q1:1, ' ', q2:1); getexp(ep); 
          popstk(ep^.l); popstk(ep^.r); popstk(ep^.x1); pshstk(ep) 
        end;

        {lpa}
        114: begin read(prd,p); labelsearch(def, val, sp); writeln(prr); 
          q1 := -p*ptrsize; getexp(ep); ep^.fn := sp; pshstk(ep);
        end;

        {ldcs,ldci,ldcr,ldcn,ldcb,ldcc}
        7, 123, 124, 125, 126, 127: begin case op of

          123: begin read(prd,i); writeln(prr, i:1); 
            getexp(ep); ep^.vali := i; pshstk(ep) 
          end;

          124: begin read(prd,r); writeln(prr, r); getexp(ep);
            pshstk(ep); new(cstp); cstp^.ct := creal; 
            cstp^.r := r; realnum := realnum+1; 
            cstp^.realn := realnum; cstp^.next := csttbl; 
            csttbl := cstp; ep^.realn := realnum 
          end;

          125: begin writeln(prr); getexp(ep); pshstk(ep) 
          end;

          126: begin read(prd,i); writeln(prr, i:1); 
            getexp(ep); ep^.vali := i; pshstk(ep) 
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
            getexp(ep); ep^.vali := ord(c); pshstk(ep)
          end;

          7: begin skpspc;
            if ch <> '(' then errorl('ldcs() expected          ');
            s := [ ];  getnxt;
            while ch<>')' do
              begin read(prd,s1); getnxt; s := s + [s1] end;
            getexp(ep);
            pshstk(ep);
            writeln(prr)
          end

          end (*case*)
        end;

        {chki,chka,chks,chkb,chkc,ckla,chkx}
        26, 95, 97, 98, 99, 190, 199: begin read(prd,lb,ub); 
          writeln(prr, lb:1, ' ', ub:1); getexp(ep); popstk(ep^.l); 
          pshstk(ep) 
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
          getexp(ep);
          pshstk(ep);
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
        34: begin writeln(prr); getexp(ep); popstk(ep^.l); popstk(ep^.r);
          pshstk(ep)
        end;

        {trc}
        35: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep); 
        end;

        {ngi,ngr}
        36,37: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {sqi,sqr}
        38,39: begin writeln(prr); getexp(ep); popstk(ep^.l); popstk(ep^.r);
          pshstk(ep)
        end;

        {abi,abr}
        40,41: begin writeln(prr); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {notb,odd,chr,rnd,noti}
        42,50,60,62,205: begin writeln(prr); getexp(ep); popstk(ep^.l); 
          pshstk(ep)
        end;

        {and,ior,xor,dif,int,uni,inn,mod,mpi,mpr,dvi,dvr,rgs}
        43,44,46,47,48,49,51,52,53,54,110,206: begin writeln(prr); getexp(ep);
          popstk(ep^.l); popstk(ep^.r); pshstk(ep) 
        end;

        { duplicate is a stack operator. We emulate it with a dummy entry that
          gets copied from the original. }

        { dupi, dupa, dupr, dupb, dupc }
        181, 182, 183, 185, 186: begin writeln(prr); getexp(ep);
          popstk(ep^.l); getexp(ep^.r); pshstk(ep); pshstk(ep) 
        end;

        { duplicate set is possible, but not used anywhere in the compiler,
          so we don't implement it here. }

        {dups}
        184: errorl('Operator not implemented');

        {cks}
        187: begin writeln(prr); 
          getexp(ep); popstk(ep^.l); getexp(ep^.r); pshstk(ep)
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
        247: begin labelsearch(def, val, sp); write(prr, 'l '); writevp(prr, sp); 
          writeln(prr);
          getexp(ep); ep^.fn := sp; getpar(ep); pshstk(ep);
        end;

        { *** calls can be terminal or non-terminal *** }

        {csp} 
        15: begin skpspc; getname;
          while name<>sptable[q] do begin 
            q := q+1; if q > maxsp then errorl('std proc/func not found  ')
          end; 
          writeln(prr, sptable[q]);
          getexp(ep); getparn(ep, sppar[q]);
          if spfunc[q] then pshstk(ep) { non-terminal, stack it }
          else begin { terminal, execute here }
            frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
            dmptre(ep); 
            if spkeep[ep^.q] then ep^.pl^.wkeep := true;
            genexp(ep); 
            if spkeep[ep^.q] then begin { hold over file parameter }
              pp := ep^.pl; ep^.pl := ep^.pl^.next;
              pshstk(pp)
            end;
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
          wrtins20('movq ^0(%rsp),%rbp    ', q, 0, rgnull, rgnull, nil)
        end;

        {stri,stra}
        2,70: begin read(prd,p,q); writeln(prr,p:1,' ', q:1);
          frereg := allreg; getreg(r1, frereg); 
          popstk(ep); assreg(ep, frereg, rgnull, rgnull);
          dmptre(ep); genexp(ep);
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
          wrtins20('movq ^0(%rbp),%1    ', -p*ptrsize, 0, r1, rgnull, nil);
          wrtins20('movb %1,^0(%2)      ', q, 0, ep^.r1, r1, nil);
          deltre(ep); 
          botstk 
        end;

        {strr}
        71: begin read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          frereg := allreg; getreg(r1, frereg);
          popstk(ep); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep); 
          wrtins20('movq ^0(%rbp),%1    ', -p*ptrsize, 0, r1, rgnull, nil);
          wrtins20('movsd %1,^0(%2)     ', q, 0, ep^.r1, r1, nil);
          deltre(ep); 
          botstk 
        end;

        {strs} 
        72:begin read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          frereg := allreg; popstk(ep); assreg(ep, frereg, rgnull, rgnull); 
          dmptre(ep); genexp(ep);
          wrtins20('movq ^0(%rbp),%rdi  ', -p*ptrsize, 0, ep^.t1, rgnull, nil);
          wrtins20('lea %1,^0(%2)       ', q, 0, ep^.r1, ep^.t1, nil);
          wrtins20('movq %rsp,%rsi      ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull, nil);
          wrtins20('add $0,%rsp         ', setsize, 0, rgnull, rgnull, nil);
          deltre(ep); 
          botstk 
        end;

        {mst}
        11: begin read(prd,p); labelsearch(def, val, sp); labelsearch(def2, val2, sp2);
          write(prr,p:1, ' l '); writevp(prr, sp); write(prr, ' l '); 
          writevp(prr, sp2); writeln(prr);
          if blkstk <> nil then
            if blkstk^.btyp in [btproc, btfunc] then wrtblklab(blkstk);
          frereg := allreg;
          { We limit to the enter instruction }
          if p >= 32 then errorl('Too many nested levels   ');
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { place current ep }
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { place bottom of stack }
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { previous ep }
          wrtins20('enter $1,$0        ', p+1, 0, rgnull, rgnull, nil); { enter frame }
          wrtins20('movq %rsp,%rax     ', 0, 0, rgnull, rgnull, nil); { copy sp }
          wrtins20('subq $s,%rax       ', 0, 0, rgnull, rgnull, sp); { find sp-locals }
          wrtins20('cmpq %rax,%rsp     ', 0, 0, rgnull, rgnull, nil); { check stack is there }
          wrtins10('je .+0    ', 6, 0, rgnull, rgnull, nil); { skip if so }
          wrtins10('pushq $0  ', 0, 0, rgnull, rgnull, nil); { push 0 word }
          wrtins10('jmp .-0   ', 7, 0, rgnull, rgnull, nil); { loop }
          wrtins20('movq %rsp,^0(%rbp) ', marksb, 0, rgnull, rgnull, nil);
          { note ep is unused at this time }
          botstk
        end;

        {mov}
        55: begin read(prd,q); writeln(prr,q:1); 
          frereg := allreg; popstk(ep); popstk(ep2); dmptre(ep); dmptre(ep2);
          deltre(ep); deltre(ep2);
          botstk
        end;

        {dmp}
        117: begin read(prd,q); writeln(prr,q:1); 
          frereg := allreg; popstk(ep); 
          wrtins10('popq %1   ', 0, 0, ep^.r1, rgnull, nil);
          dmptrel(ep, 19); deltre(ep); 
          botstk 
        end;

        {sro}
        3, 75, 76, 77, 78, 79, 196: begin read(prd,q); writeln(prr,q:1);
          frereg := allreg;
          popstk(ep); assreg(ep, frereg, rgnull, rgnull); dmptre(ep); 
          genexp(ep);
          wrtins40('movq %1,globals_start+0(%rip) ', q, 0, ep^.r1, rgnull, nil);
          deltre(ep); 
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
          frereg := allreg;
          botstk
        end;

        {fjp,tjp,xjp}
        24,25,119: begin labelsearch(def, val, sp); write(prr, 'l '); 
          writevp(prr, sp); writeln(prr);
          frereg := allreg; popstk(ep); 
          dmptre(ep); deltre(ep); 
          botstk 
        end;

        {ipj}
        112: begin read(prd,p); labelsearch(def, val, sp); writeln(prr, p:1);
          frereg := allreg;
          wrtins20('movq ^0(%rbp),%rbp    ', -p*ptrsize, 0, rgnull, rgnull, nil);
          wrtins20('movq ^0(%rbp),%rsp    ', marksb, 0, rgnull, rgnull, nil);
          wrtins10('jmp @s    ', 0, 0, rgnull, rgnull, sp);
          botstk 
        end;

        {vbs}
        92: begin read(prd,q); writeln(prr, q:1); 
          frereg := allreg; popstk(ep); 
          assreg(ep, frereg, rgrdi, rgnull); dmptrel(ep, 19); genexp(ep);
          wrtins20('movq %rdi,%rsi         ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsi           ', ep^.q-1, 0, rgnull, rgnull, nil);
          wrtins20('call varenter          ', 0, 0, rgnull, rgnull, nil);
          deltre(ep);
          botstk
        end;

        {vbe}
        96: begin 
          frereg := allreg;
          wrtins20('call varenter          ', 0, 0, rgnull, rgnull, nil);
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
          wrtins10('leave     ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', marksize, 0, rgnull, rgnull, nil);
          wrtins10('popq %rax  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', q, 0, rgnull, rgnull, nil);
          wrtins10('pushq %rax ', 0, 0, rgnull, rgnull, nil);
          wrtins10('ret        ', 0, 0, rgnull, rgnull, nil);
          botstk
        end;

        {reti,reta,retx,retc,retb}
        128,132,204,130,131: begin read(prd,q); writeln(prr, q:1);
          frereg := allreg;
          wrtins10('leave     ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', marksize, 0, rgnull, rgnull, nil);
          wrtins10('popq %rbx  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', q, 0, rgnull, rgnull, nil);
          wrtins10('popq %rax  ', 0, 0, rgnull, rgnull, nil);
          if (op = 130{retc}) or (op = 131{retb}) then
            wrtins20('andq $0,%rax        ', 255, 0, rgnull, rgnull, nil);
          wrtins10('pushq %rbx ', 0, 0, rgnull, rgnull, nil);
          wrtins10('ret        ', 0, 0, rgnull, rgnull, nil);
          botstk
        end;

        {retr}
        129: begin read(prd,q); writeln(prr, q:1);
          frereg := allreg;
          wrtins10('leave     ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', marksize, 0, rgnull, rgnull, nil);
          wrtins10('popq %rbx  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', q, 0, rgnull, rgnull, nil);
          wrtins20('movsd (%rsp),%xmm0  ', 0, 0, rgnull, rgnull, nil);
          wrtins20('addq $0,%rsp        ', realsize, 0, rgnull, rgnull, nil);
          wrtins10('pushq %rbx ', 0, 0, rgnull, rgnull, nil);
          wrtins10('ret        ', 0, 0, rgnull, rgnull, nil);
          botstk
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

        {sto}
        6, 80, 81, 82, 83, 84, 197: begin writeln(prr); 
          frereg := allreg; popstk(ep); 
          popstk(ep2); dmptre(ep2); dmptre(ep); deltre(ep); deltre(ep2); 
          botstk
        end;

        {stp}
        58:; { unused }

        {cke}
        188: begin writeln(prr); 
          frereg := allreg; popstk(ep); dmptre(ep); deltre(ep);
          botstk 
        end;

        {inv}
        189: begin writeln(prr); 
          frereg := allreg; popstk(ep); dmptre(ep); deltre(ep); 
          botstk
        end;

        61 {ujc}: begin writeln(prr);
          frereg := allreg;
          botstk
        end;

        { these are all Pascaline unimplemented }

        {suv}
        91,
        {cjp}
        8,
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
        241: errorl('Intermediate not implemented');

      end; (*case*)

      getlin; { next intermediate line }

   end; (*assemble*)

   procedure genstrcst;
   begin
     while csttbl <> nil do begin
       case csttbl^.ct of
         cstr: begin writeln(prr, 'string', csttbl^.strn:1, ':');
           write(prr, '        .string "');
           writev(prr, csttbl^.str, csttbl^.strl);
           writeln(prr, '"') end;
         creal: begin writeln(prr, 'real', csttbl^.realn:1, ':');
           writeln(prr, '        .double ', csttbl^.r) end
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
   writeln(prr, '        .text');
   writeln(prr, '#');
   writeln(prr, '# Code section');
   writeln(prr, '#');
   generate;
   writeln(prr, '#');
   writeln(prr, '# Constants section');
   writeln(prr, '#');
   genstrcst;

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

  csttbl := nil; strnum := 0; realnum := 0; gblsiz := 0; parlvl := maxint;

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

  blkstk := nil; { clear symbols block stack }
  blklst := nil; { clear symbols block discard list }

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
