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
*    Scott A. Franco                                                            *
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

      majorver   = 1; { major version number }
      minorver   = 2; { minor version number }
      experiment = false; { is version experimental? }

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
      sptable     : array[sctyp] of alfa; (*standard functions and procedures*)
      spfunc      : array[sctyp] of boolean; (*standard function or procedure
                                                  is function*)
      sppar       : array[sctyp] of integer; (*standard functions and procedures
                                                  number of parameters*)
      srclin      : integer; { current source line executing }
      option      : array ['a'..'z'] of boolean; { option array }
      csttbl      : cstptr; { constants table }
      strnum      : integer; { string constant label count }
      realnum     : integer; { real constants label count }

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
                           st: labelst
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
                    r1, r2: reg; { result registers }
                    t1, t2: reg; { temporary registers }
                    l, r: expptr; { right and left links }
                    x1:   expptr; { extra link }
                    strn: integer; { string number }
                    realn: integer; { real number }
                    vali: integer; { integer value }
                    rs: regset; { push/pop mask }
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
        frereg: regset;

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
         instr[  0]:='lodi      '; insp[  0] := true;  insq[  0] := intsize;
         instr[  1]:='ldoi      '; insp[  1] := false; insq[  1] := intsize;
         instr[  2]:='stri      '; insp[  2] := true;  insq[  2] := intsize;
         instr[  3]:='sroi      '; insp[  3] := false; insq[  3] := intsize;
         instr[  4]:='lda       '; insp[  4] := true;  insq[  4] := intsize;
         instr[  5]:='lao       '; insp[  5] := false; insq[  5] := intsize;
         instr[  6]:='stoi      '; insp[  6] := false; insq[  6] := 0;
         instr[  7]:='ldcs      '; insp[  7] := false; insq[  7] := intsize;
         instr[  8]:='cjp       '; insp[  8] := false; insq[  8] := intsize*2;
         instr[  9]:='indi      '; insp[  9] := false; insq[  9] := intsize;
         instr[ 10]:='inci      '; insp[ 10] := false; insq[ 10] := intsize;
         instr[ 11]:='mst       '; insp[ 11] := true;  insq[ 11] := 0;
         instr[ 12]:='cup       '; insp[ 12] := true;  insq[ 12] := intsize;
         instr[ 13]:='ents      '; insp[ 13] := false; insq[ 13] := intsize;
         instr[ 14]:='retp      '; insp[ 14] := false; insq[ 14] := 0;
         instr[ 15]:='csp       '; insp[ 15] := false; insq[ 15] := 1;
         instr[ 16]:='ixa       '; insp[ 16] := false; insq[ 16] := intsize;
         instr[ 17]:='equa      '; insp[ 17] := false; insq[ 17] := 0;
         instr[ 18]:='neqa      '; insp[ 18] := false; insq[ 18] := 0;
         instr[ 19]:='---       '; insp[ 19] := false; insq[ 19] := 0;
         instr[ 20]:='---       '; insp[ 20] := false; insq[ 20] := intsize;
         instr[ 21]:='cal       '; insp[ 21] := false; insq[ 21] := intsize;
         instr[ 22]:='ret       '; insp[ 22] := false; insq[ 22] := 0;
         instr[ 23]:='ujp       '; insp[ 23] := false; insq[ 23] := intsize;
         instr[ 24]:='fjp       '; insp[ 24] := false; insq[ 24] := intsize;
         instr[ 25]:='xjp       '; insp[ 25] := false; insq[ 25] := intsize;
         instr[ 26]:='chki      '; insp[ 26] := false; insq[ 26] := intsize;
         instr[ 27]:='cuv       '; insp[ 27] := false; insq[ 27] := intsize;
         instr[ 28]:='adi       '; insp[ 28] := false; insq[ 28] := 0;
         instr[ 29]:='adr       '; insp[ 29] := false; insq[ 29] := 0;
         instr[ 30]:='sbi       '; insp[ 30] := false; insq[ 30] := 0;
         instr[ 31]:='sbr       '; insp[ 31] := false; insq[ 31] := 0;
         instr[ 32]:='sgs       '; insp[ 32] := false; insq[ 32] := 0;
         instr[ 33]:='flt       '; insp[ 33] := false; insq[ 33] := 0;
         instr[ 34]:='flo       '; insp[ 34] := false; insq[ 34] := 0;
         instr[ 35]:='trc       '; insp[ 35] := false; insq[ 35] := 0;
         instr[ 36]:='ngi       '; insp[ 36] := false; insq[ 36] := 0;
         instr[ 37]:='ngr       '; insp[ 37] := false; insq[ 37] := 0;
         instr[ 38]:='sqi       '; insp[ 38] := false; insq[ 38] := 0;
         instr[ 39]:='sqr       '; insp[ 39] := false; insq[ 39] := 0;
         instr[ 40]:='abi       '; insp[ 40] := false; insq[ 40] := 0;
         instr[ 41]:='abr       '; insp[ 41] := false; insq[ 41] := 0;
         instr[ 42]:='not       '; insp[ 42] := false; insq[ 42] := 0;
         instr[ 43]:='and       '; insp[ 43] := false; insq[ 43] := 0;
         instr[ 44]:='ior       '; insp[ 44] := false; insq[ 44] := 0;
         instr[ 45]:='dif       '; insp[ 45] := false; insq[ 45] := 0;
         instr[ 46]:='int       '; insp[ 46] := false; insq[ 46] := 0;
         instr[ 47]:='uni       '; insp[ 47] := false; insq[ 47] := 0;
         instr[ 48]:='inn       '; insp[ 48] := false; insq[ 48] := 0;
         instr[ 49]:='mod       '; insp[ 49] := false; insq[ 49] := 0;
         instr[ 50]:='odd       '; insp[ 50] := false; insq[ 50] := 0;
         instr[ 51]:='mpi       '; insp[ 51] := false; insq[ 51] := 0;
         instr[ 52]:='mpr       '; insp[ 52] := false; insq[ 52] := 0;
         instr[ 53]:='dvi       '; insp[ 53] := false; insq[ 53] := 0;
         instr[ 54]:='dvr       '; insp[ 54] := false; insq[ 54] := 0;
         instr[ 55]:='mov       '; insp[ 55] := false; insq[ 55] := intsize;
         instr[ 56]:='lca       '; insp[ 56] := false; insq[ 56] := intsize;
         instr[ 57]:='deci      '; insp[ 57] := false; insq[ 57] := intsize;
         instr[ 58]:='stp       '; insp[ 58] := false; insq[ 58] := 0;
         instr[ 59]:='ordi      '; insp[ 59] := false; insq[ 59] := 0;
         instr[ 60]:='chr       '; insp[ 60] := false; insq[ 60] := 0;
         instr[ 61]:='ujc       '; insp[ 61] := false; insq[ 61] := intsize;
         instr[ 62]:='rnd       '; insp[ 62] := false; insq[ 62] := 0;
         instr[ 63]:='pck       '; insp[ 63] := false; insq[ 63] := intsize*2;
         instr[ 64]:='upk       '; insp[ 64] := false; insq[ 64] := intsize*2;
         instr[ 65]:='ldoa      '; insp[ 65] := false; insq[ 65] := intsize;
         instr[ 66]:='ldor      '; insp[ 66] := false; insq[ 66] := intsize;
         instr[ 67]:='ldos      '; insp[ 67] := false; insq[ 67] := intsize;
         instr[ 68]:='ldob      '; insp[ 68] := false; insq[ 68] := intsize;
         instr[ 69]:='ldoc      '; insp[ 69] := false; insq[ 69] := intsize;
         instr[ 70]:='stra      '; insp[ 70] := true;  insq[ 70] := intsize;
         instr[ 71]:='strr      '; insp[ 71] := true;  insq[ 71] := intsize;
         instr[ 72]:='strs      '; insp[ 72] := true;  insq[ 72] := intsize;
         instr[ 73]:='strb      '; insp[ 73] := true;  insq[ 73] := intsize;
         instr[ 74]:='strc      '; insp[ 74] := true;  insq[ 74] := intsize;
         instr[ 75]:='sroa      '; insp[ 75] := false; insq[ 75] := intsize;
         instr[ 76]:='sror      '; insp[ 76] := false; insq[ 76] := intsize;
         instr[ 77]:='sros      '; insp[ 77] := false; insq[ 77] := intsize;
         instr[ 78]:='srob      '; insp[ 78] := false; insq[ 78] := intsize;
         instr[ 79]:='sroc      '; insp[ 79] := false; insq[ 79] := intsize;
         instr[ 80]:='stoa      '; insp[ 80] := false; insq[ 80] := 0;
         instr[ 81]:='stor      '; insp[ 81] := false; insq[ 81] := 0;
         instr[ 82]:='stos      '; insp[ 82] := false; insq[ 82] := 0;
         instr[ 83]:='stob      '; insp[ 83] := false; insq[ 83] := 0;
         instr[ 84]:='stoc      '; insp[ 84] := false; insq[ 84] := 0;
         instr[ 85]:='inda      '; insp[ 85] := false; insq[ 85] := intsize;
         instr[ 86]:='indr      '; insp[ 86] := false; insq[ 86] := intsize;
         instr[ 87]:='inds      '; insp[ 87] := false; insq[ 87] := intsize;
         instr[ 88]:='indb      '; insp[ 88] := false; insq[ 88] := intsize;
         instr[ 89]:='indc      '; insp[ 89] := false; insq[ 89] := intsize;
         instr[ 90]:='inca      '; insp[ 90] := false; insq[ 90] := intsize;
         instr[ 91]:='suv       '; insp[ 91] := false; insq[ 91] := intsize*2;
         instr[ 92]:='vbs       '; insp[ 92] := false; insq[ 92] := intsize;
         instr[ 93]:='incb      '; insp[ 93] := false; insq[ 93] := intsize;
         instr[ 94]:='incc      '; insp[ 94] := false; insq[ 94] := intsize;
         instr[ 95]:='chka      '; insp[ 95] := false; insq[ 95] := intsize;
         instr[ 96]:='vbe       '; insp[ 96] := false; insq[ 96] := 0;
         instr[ 97]:='chks      '; insp[ 97] := false; insq[ 97] := intsize;
         instr[ 98]:='chkb      '; insp[ 98] := false; insq[ 98] := intsize;
         instr[ 99]:='chkc      '; insp[ 99] := false; insq[ 99] := intsize;
         instr[100]:='cvbi      '; insp[100] := false; insq[100] := intsize*3;
         instr[101]:='ivtx      '; insp[101] := false; insq[101] := intsize*3;
         instr[102]:='ivtb      '; insp[102] := false; insq[102] := intsize*3;
         instr[103]:='decb      '; insp[103] := false; insq[103] := intsize;
         instr[104]:='decc      '; insp[104] := false; insq[104] := intsize;
         instr[105]:='loda      '; insp[105] := true;  insq[105] := intsize;
         instr[106]:='lodr      '; insp[106] := true;  insq[106] := intsize;
         instr[107]:='lods      '; insp[107] := true;  insq[107] := intsize;
         instr[108]:='lodb      '; insp[108] := true;  insq[108] := intsize;
         instr[109]:='lodc      '; insp[109] := true;  insq[109] := intsize;
         instr[110]:='rgs       '; insp[110] := false; insq[110] := 0;
         instr[111]:='ivtc      '; insp[111] := false; insq[111] := intsize*3;
         instr[112]:='ipj       '; insp[112] := true;  insq[112] := intsize;
         instr[113]:='cip       '; insp[113] := true;  insq[113] := 0;
         instr[114]:='lpa       '; insp[114] := true;  insq[114] := intsize;
         instr[115]:='cvbx      '; insp[115] := false; insq[115] := intsize*3;
         instr[116]:='cvbb      '; insp[116] := false; insq[116] := intsize*3;
         instr[117]:='dmp       '; insp[117] := false; insq[117] := intsize;
         instr[118]:='swp       '; insp[118] := false; insq[118] := intsize;
         instr[119]:='tjp       '; insp[119] := false; insq[119] := intsize;
         instr[120]:='lip       '; insp[120] := true;  insq[120] := intsize;
         instr[121]:='cvbc      '; insp[121] := false; insq[121] := intsize*3;
         instr[122]:='vis       '; insp[122] := false; insq[122] := intsize*2;
         instr[123]:='ldci      '; insp[123] := false; insq[123] := intsize;
         instr[124]:='ldcr      '; insp[124] := false; insq[124] := intsize;
         instr[125]:='ldcn      '; insp[125] := false; insq[125] := 0;
         instr[126]:='ldcb      '; insp[126] := false; insq[126] := boolsize;
         instr[127]:='ldcc      '; insp[127] := false; insq[127] := charsize;
         instr[128]:='reti      '; insp[128] := false; insq[128] := 0;
         instr[129]:='retr      '; insp[129] := false; insq[129] := 0;
         instr[130]:='retc      '; insp[130] := false; insq[130] := 0;
         instr[131]:='retb      '; insp[131] := false; insq[131] := 0;
         instr[132]:='reta      '; insp[132] := false; insq[132] := 0;
         instr[133]:='vip       '; insp[133] := false; insq[133] := intsize*2;
         instr[134]:='ordb      '; insp[134] := false; insq[134] := 0;
         instr[135]:='lcp       '; insp[135] := false; insq[135] := 0;
         instr[136]:='ordc      '; insp[136] := false; insq[136] := 0;
         instr[137]:='equi      '; insp[137] := false; insq[137] := 0;
         instr[138]:='equr      '; insp[138] := false; insq[138] := 0;
         instr[139]:='equb      '; insp[139] := false; insq[139] := 0;
         instr[140]:='equs      '; insp[140] := false; insq[140] := 0;
         instr[141]:='equc      '; insp[141] := false; insq[141] := 0;
         instr[142]:='equm      '; insp[142] := false; insq[142] := intsize;
         instr[143]:='neqi      '; insp[143] := false; insq[143] := 0;
         instr[144]:='neqr      '; insp[144] := false; insq[144] := 0;
         instr[145]:='neqb      '; insp[145] := false; insq[145] := 0;
         instr[146]:='neqs      '; insp[146] := false; insq[146] := 0;
         instr[147]:='neqc      '; insp[147] := false; insq[147] := 0;
         instr[148]:='neqm      '; insp[148] := false; insq[148] := intsize;
         instr[149]:='geqi      '; insp[149] := false; insq[149] := 0;
         instr[150]:='geqr      '; insp[150] := false; insq[150] := 0;
         instr[151]:='geqb      '; insp[151] := false; insq[151] := 0;
         instr[152]:='geqs      '; insp[152] := false; insq[152] := 0;
         instr[153]:='geqc      '; insp[153] := false; insq[153] := 0;
         instr[154]:='geqm      '; insp[154] := false; insq[154] := intsize;
         instr[155]:='grti      '; insp[155] := false; insq[155] := 0;
         instr[156]:='grtr      '; insp[156] := false; insq[156] := 0;
         instr[157]:='grtb      '; insp[157] := false; insq[157] := 0;
         instr[158]:='grts      '; insp[158] := false; insq[158] := 0;
         instr[159]:='grtc      '; insp[159] := false; insq[159] := 0;
         instr[160]:='grtm      '; insp[160] := false; insq[160] := intsize;
         instr[161]:='leqi      '; insp[161] := false; insq[161] := 0;
         instr[162]:='leqr      '; insp[162] := false; insq[162] := 0;
         instr[163]:='leqb      '; insp[163] := false; insq[163] := 0;
         instr[164]:='leqs      '; insp[164] := false; insq[164] := 0;
         instr[165]:='leqc      '; insp[165] := false; insq[165] := 0;
         instr[166]:='leqm      '; insp[166] := false; insq[166] := intsize;
         instr[167]:='lesi      '; insp[167] := false; insq[167] := 0;
         instr[168]:='lesr      '; insp[168] := false; insq[168] := 0;
         instr[169]:='lesb      '; insp[169] := false; insq[169] := 0;
         instr[170]:='less      '; insp[170] := false; insq[170] := 0;
         instr[171]:='lesc      '; insp[171] := false; insq[171] := 0;
         instr[172]:='lesm      '; insp[172] := false; insq[172] := intsize;
         instr[173]:='ente      '; insp[173] := false; insq[173] := intsize;
         instr[174]:='---       '; insp[174] := false; insq[174] := intsize;
         instr[175]:='ckvi      '; insp[175] := false; insq[175] := intsize;
         instr[176]:='cps       '; insp[176] := false; insq[176] := 0;
         instr[177]:='cpc       '; insp[177] := false; insq[177] := intsize;
         instr[178]:='aps       '; insp[178] := false; insq[178] := intsize;
         instr[179]:='ckvb      '; insp[179] := false; insq[179] := intsize;
         instr[180]:='ckvc      '; insp[180] := false; insq[180] := intsize;
         instr[181]:='dupi      '; insp[181] := false; insq[181] := 0;
         instr[182]:='dupa      '; insp[182] := false; insq[182] := 0;
         instr[183]:='dupr      '; insp[183] := false; insq[183] := 0;
         instr[184]:='dups      '; insp[184] := false; insq[184] := 0;
         instr[185]:='dupb      '; insp[185] := false; insq[185] := 0;
         instr[186]:='dupc      '; insp[186] := false; insq[186] := 0;
         instr[187]:='cks       '; insp[187] := false; insq[187] := 0;
         instr[188]:='cke       '; insp[188] := false; insq[188] := 0;
         instr[189]:='inv       '; insp[189] := false; insq[189] := 0;
         instr[190]:='ckla      '; insp[190] := false; insq[190] := intsize;
         instr[191]:='cta       '; insp[191] := false; insq[191] := intsize*2;
         instr[192]:='ivt       '; insp[192] := false; insq[192] := intsize*2;
         instr[193]:='lodx      '; insp[193] := true;  insq[193] := intsize;
         instr[194]:='ldox      '; insp[194] := false; insq[194] := intsize;
         instr[195]:='strx      '; insp[195] := true;  insq[195] := intsize;
         instr[196]:='srox      '; insp[196] := false; insq[196] := intsize;
         instr[197]:='stox      '; insp[197] := false; insq[197] := 0;
         instr[198]:='indx      '; insp[198] := false; insq[198] := intsize;
         instr[199]:='chkx      '; insp[199] := false; insq[199] := intsize;
         instr[200]:='ordx      '; insp[200] := false; insq[200] := 0;
         instr[201]:='incx      '; insp[201] := false; insq[201] := intsize;
         instr[202]:='decx      '; insp[202] := false; insq[202] := intsize;
         instr[203]:='ckvx      '; insp[203] := false; insq[203] := intsize;
         instr[204]:='retx      '; insp[204] := false; insq[204] := 0;
         instr[205]:='noti      '; insp[205] := false; insq[205] := 0;
         instr[206]:='xor       '; insp[206] := false; insq[206] := 0;
         instr[207]:='bge       '; insp[207] := false; insq[207] := intsize;
         instr[208]:='ede       '; insp[208] := false; insq[208] := 0;
         instr[209]:='mse       '; insp[209] := false; insq[209] := 0;
         instr[210]:='apc       '; insp[210] := false; insq[210] := intsize*2;
         instr[211]:='cxs       '; insp[211] := false; insq[211] := intsize;
         instr[212]:='cxc       '; insp[212] := false; insq[212] := intsize*2;
         instr[213]:='lft       '; insp[213] := false; insq[213] := intsize;
         instr[214]:='max       '; insp[214] := false; insq[214] := intsize;
         instr[215]:='equv      '; insp[215] := false; insq[215] := 0;
         instr[216]:='neqv      '; insp[216] := false; insq[216] := 0;
         instr[217]:='lesv      '; insp[217] := false; insq[217] := 0;
         instr[218]:='grtv      '; insp[218] := false; insq[218] := 0;
         instr[219]:='leqv      '; insp[219] := false; insq[219] := 0;
         instr[220]:='geqv      '; insp[220] := false; insq[220] := 0;
         instr[221]:='vdp       '; insp[221] := false; insq[221] := 0;
         instr[222]:='spc       '; insp[222] := false; insq[222] := 0;
         instr[223]:='ccs       '; insp[223] := false; insq[223] := intsize*2;
         instr[224]:='scp       '; insp[224] := false; insq[224] := 0;
         instr[225]:='ldp       '; insp[225] := false; insq[225] := 0;
         instr[226]:='vin       '; insp[226] := false; insq[226] := intsize*2;
         instr[227]:='vdd       '; insp[227] := false; insq[227] := 0;
         { ltc and lto are aliases to ldo and lao instructions }
         instr[228]:='ltci      '; insp[228] := false; insq[228] := intsize;
         instr[229]:='ltcr      '; insp[229] := false; insq[229] := intsize;
         instr[230]:='ltcs      '; insp[230] := false; insq[230] := intsize;
         instr[231]:='ltcb      '; insp[231] := false; insq[231] := intsize;
         instr[232]:='ltcc      '; insp[232] := false; insq[232] := intsize;
         instr[233]:='ltcx      '; insp[233] := false; insq[233] := intsize;
         instr[234]:='lto       '; insp[234] := false; insq[234] := intsize;
         instr[235]:='stom      '; insp[235] := false; insq[235] := intsize*2;
         instr[236]:='rets      '; insp[236] := false; insq[236] := 0;
         instr[237]:='retm      '; insp[237] := false; insq[237] := intsize;
         instr[238]:='ctb       '; insp[238] := false; insq[238] := intsize*2;
         instr[239]:='cpp       '; insp[239] := false; insq[239] := intsize*2;
         instr[240]:='cpr       '; insp[240] := false; insq[240] := intsize*2;
         instr[241]:='lsa       '; insp[241] := false; insq[241] := intsize;
         instr[242]:='---       '; insp[242] := false; insq[242] := 0;

         sptable[ 0]:='get       '; spfunc[ 0]:=false; sppar[ 0]:=1;   
         sptable[ 1]:='put       '; spfunc[ 1]:=false; sppar[ 1]:=1;
         sptable[ 2]:='thw       '; spfunc[ 2]:=false; sppar[ 2]:=1;   
         sptable[ 3]:='rln       '; spfunc[ 3]:=false; sppar[ 3]:=1;
         sptable[ 4]:='new       '; spfunc[ 4]:=false; sppar[ 4]:=2;   
         sptable[ 5]:='wln       '; spfunc[ 5]:=false; sppar[ 5]:=1;
         sptable[ 6]:='wrs       '; spfunc[ 6]:=false; sppar[ 6]:=4;   
         sptable[ 7]:='eln       '; spfunc[ 7]:=false; sppar[ 7]:=1;
         sptable[ 8]:='wri       '; spfunc[ 8]:=false; sppar[ 8]:=3;   
         sptable[ 9]:='wrr       '; spfunc[ 9]:=false; sppar[ 9]:=3;
         sptable[10]:='wrc       '; spfunc[10]:=false; sppar[10]:=3;   
         sptable[11]:='rdi       '; spfunc[11]:=false; sppar[11]:=2;
         sptable[12]:='rdr       '; spfunc[12]:=false; sppar[12]:=2;   
         sptable[13]:='rdc       '; spfunc[13]:=false; sppar[13]:=2;
         sptable[14]:='sin       '; spfunc[14]:=true;  sppar[14]:=1;   
         sptable[15]:='cos       '; spfunc[15]:=true;  sppar[15]:=1;
         sptable[16]:='exp       '; spfunc[16]:=true;  sppar[16]:=1;   
         sptable[17]:='log       '; spfunc[17]:=true;  sppar[17]:=1;
         sptable[18]:='sqt       '; spfunc[18]:=true;  sppar[18]:=1;   
         sptable[19]:='atn       '; spfunc[19]:=true;  sppar[19]:=1;
         sptable[20]:='---       '; spfunc[20]:=false; sppar[20]:=1;   
         sptable[21]:='pag       '; spfunc[21]:=false; sppar[21]:=1;
         sptable[22]:='rsf       '; spfunc[22]:=false; sppar[22]:=1;   
         sptable[23]:='rwf       '; spfunc[23]:=false; sppar[23]:=1;
         sptable[24]:='wrb       '; spfunc[24]:=false; sppar[24]:=3;   
         sptable[25]:='wrf       '; spfunc[25]:=false; sppar[25]:=4;
         sptable[26]:='dsp       '; spfunc[26]:=false; sppar[26]:=2;   
         sptable[27]:='wbf       '; spfunc[27]:=false; sppar[27]:=3;
         sptable[28]:='wbi       '; spfunc[28]:=false; sppar[28]:=2;   
         sptable[29]:='wbr       '; spfunc[29]:=false; sppar[29]:=2;
         sptable[30]:='wbc       '; spfunc[30]:=false; sppar[30]:=2;   
         sptable[31]:='wbb       '; spfunc[31]:=false; sppar[31]:=2;
         sptable[32]:='rbf       '; spfunc[32]:=false; sppar[32]:=3;   
         sptable[33]:='rsb       '; spfunc[33]:=false; sppar[33]:=1;
         sptable[34]:='rwb       '; spfunc[34]:=false; sppar[34]:=1;   
         sptable[35]:='gbf       '; spfunc[35]:=false; sppar[35]:=2;
         sptable[36]:='pbf       '; spfunc[36]:=false; sppar[36]:=2;   
         sptable[37]:='rib       '; spfunc[37]:=false; sppar[37]:=3;
         sptable[38]:='rcb       '; spfunc[38]:=false; sppar[38]:=3;   
         sptable[39]:='nwl       '; spfunc[39]:=false; sppar[39]:=0; { special }
         sptable[40]:='dsl       '; spfunc[40]:=false; sppar[40]:=0; { special }
         sptable[41]:='eof       '; spfunc[41]:=true;  sppar[41]:=1;
         sptable[42]:='efb       '; spfunc[42]:=true;  sppar[42]:=1;   
         sptable[43]:='fbv       '; spfunc[43]:=false; sppar[43]:=1;
         sptable[44]:='fvb       '; spfunc[44]:=false; sppar[44]:=1;
         sptable[45]:='wbx       '; spfunc[45]:=false; sppar[45]:=2;
         sptable[46]:='asst      '; spfunc[46]:=false; sppar[46]:=3;
         sptable[47]:='clst      '; spfunc[47]:=false; sppar[47]:=1;
         sptable[48]:='pos       '; spfunc[48]:=false; sppar[48]:=2;
         sptable[49]:='upd       '; spfunc[49]:=false; sppar[49]:=1;
         sptable[50]:='appt      '; spfunc[50]:=false; sppar[50]:=1;
         sptable[51]:='del       '; spfunc[51]:=false; sppar[51]:=2;
         sptable[52]:='chg       '; spfunc[52]:=false; sppar[52]:=4;
         sptable[53]:='len       '; spfunc[53]:=true;  sppar[53]:=1;
         sptable[54]:='loc       '; spfunc[54]:=true;  sppar[54]:=1;
         sptable[55]:='exs       '; spfunc[55]:=true;  sppar[55]:=2;
         sptable[56]:='assb      '; spfunc[56]:=false; sppar[56]:=3;
         sptable[57]:='clsb      '; spfunc[57]:=false; sppar[57]:=1;
         sptable[58]:='appb      '; spfunc[58]:=false; sppar[58]:=1;
         sptable[59]:='hlt       '; spfunc[59]:=false; sppar[59]:=0;
         sptable[60]:='ast       '; spfunc[60]:=false; sppar[60]:=1;
         sptable[61]:='asts      '; spfunc[61]:=false; sppar[61]:=3;
         sptable[62]:='wrih      '; spfunc[62]:=false; sppar[62]:=3;
         sptable[63]:='wrio      '; spfunc[63]:=false; sppar[63]:=3;
         sptable[64]:='wrib      '; spfunc[64]:=false; sppar[64]:=3;
         sptable[65]:='wrsp      '; spfunc[65]:=false; sppar[65]:=3;
         sptable[66]:='wiz       '; spfunc[66]:=false; sppar[66]:=3;
         sptable[67]:='wizh      '; spfunc[67]:=false; sppar[67]:=3;
         sptable[68]:='wizo      '; spfunc[68]:=false; sppar[68]:=3;
         sptable[69]:='wizb      '; spfunc[69]:=false; sppar[69]:=3;
         sptable[70]:='rds       '; spfunc[70]:=false; sppar[70]:=3;
         sptable[71]:='ribf      '; spfunc[71]:=false; sppar[71]:=5;
         sptable[72]:='rdif      '; spfunc[72]:=false; sppar[72]:=3;
         sptable[73]:='rdrf      '; spfunc[73]:=false; sppar[73]:=3;
         sptable[74]:='rcbf      '; spfunc[74]:=false; sppar[74]:=5;
         sptable[75]:='rdcf      '; spfunc[75]:=false; sppar[75]:=3;
         sptable[76]:='rdsf      '; spfunc[76]:=false; sppar[76]:=4;
         sptable[77]:='rdsp      '; spfunc[77]:=false; sppar[77]:=4;
         sptable[78]:='aeft      '; spfunc[78]:=false; sppar[78]:=3;
         sptable[79]:='aefb      '; spfunc[79]:=false; sppar[79]:=3;
         sptable[80]:='rdie      '; spfunc[80]:=false; sppar[80]:=3;
         sptable[81]:='rdre      '; spfunc[81]:=false; sppar[81]:=1;

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
         estack := nil; efree := nil;
         frereg := [rgrax, rgrbx, rgr11, rgr12, rgr13, rgr14, rgr15];
   end;(*init*)

   procedure errorl(string: beta); (*error in loading*)
   begin writeln;
      writeln('*** Program load error: [', iline:1, '] ', string);
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

   procedure update(x: labelrg); (*when a label definition lx is found*)
      var curr,succ,ad: address; (*resp. current element and successor element
                               of a list of future references*)
          op: instyp; q : address;  (*instruction register*)
   begin
      if labeltab[x].st=defined then errorl('duplicated label         ')
      else begin
             if labeltab[x].val<>-1 then (*forward reference(s)*)
             curr:= labeltab[x].val;
             labeltab[x].st := defined;
             labeltab[x].val:= labelvalue;
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

   procedure parlab(var x: integer; var fl: strvsp);
   var i,j: integer;
   begin fl := nil;
     getlab; if ch <> '.' then errorl('Symbols format error     ');
     if prd^ in ['0'..'9'] then read(prd, x) { near label }
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
     writeln(prr, '        pushq   %rbp');
     writeln(prr, '        movq    %rsp, %rbp');
     writeln(prr, '# Set up default files');
     writeln(prr, '        movb    $inputfn,globals_start+inputoff(%rip)');
     writeln(prr, '        movb    $outputfn,globals_start+outputoff(%rip)');
     writeln(prr, '        movb    $errorfn,globals_start+erroroff(%rip)');
     writeln(prr, '        movb    $listfn,globals_start+listoff(%rip)');
     writeln(prr, '        movb    $commandfn,globals_start+commandoff(%rip)');
   end;

   procedure postamble;
   begin
     writeln(prr, '        movq    $0,%rax');
     writeln(prr, '        popq    %rbp');
     writeln(prr, '        ret');
   end;

   procedure assemble; forward;

   procedure generate;(*generate segment of code*)
      var x: integer; (* label number *)
          again: boolean;
          c,ch1: char;
          ls: strvsp;
   begin
      again := true;
      while again do begin
        if eof(prd) then errorl('unexpected eof on input  ');
        getnxt;(* first character of line*)
        if not (ch in ['!', 'l', 'q', ' ', ':', 'o', 'b', 'e', 'g', 'f', 
                       't']) then
          errorl('unexpected line start    ');
        case ch of
          '!': begin write(prr, '# '); while not eoln(prd) do 
                 begin read(prd, ch); write(prr, ch) end;
                 writeln(prr);
               end;
          'l': begin getnxt; parlab(x,ls); 
                     write(prr, '# l ', sn:snl, '.', x:1);
                     if ls <> nil then
                       errorl('Invalid intermediate     ');
                     getnxt;
                     if ch='=' then 
                       begin read(prd,labelvalue); 
                             write(prr, '=', labelvalue:1) end
                       else labelvalue:= pc;
                     update(x); getlin; writeln(prr)
               end;
          'q': begin again := false; getlin end;
          ' ': begin getnxt; 
                     while not eoln(prd) and (ch = ' ') do getnxt;
                     if not eoln(prd) and (ch <> ' ') then assemble
                     else getlin 
               end;
          ':': begin { source line }

                  read(prd,x); { get source line number }
                  sline := x; writeln(prr, '# :', x:1);
                  { skip the rest of the line, which would be the
                    contents of the source line if included }
                  while not eoln(prd) do
                     read(prd, c); { get next character }
                  getlin { source line }

               end;
          'o': begin { option }
                 write(prr, '# o ');
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
          'b': begin getlin; preamble end; { block start }
          'e': begin getlin; postamble end; { block end }
          'g': begin read(prd, gblsiz); getlin end; { set globals space }
          'f': getlin; { source error count }
          't': getlin; { template }
       end
     end
   end; (*generate*)

   procedure assemble; (*translate symbolic code into machine code and store*)

      var name :alfa; r :real; s :settype;
          i,x,s1,lb,ub,l:integer; c: char;
          str: strbuf; { buffer for string constants }
          cstp: cstptr;
          ep, ep2, ep3, ep4, ep5: expptr;
          r1, r2: reg; ors: set of reg; rage: array [reg] of integer;
          rcon: array [reg] of expptr; domains: array [1..maxreg] of expptr;
          totreg: integer;

      procedure lookup(x: labelrg); (* search in label table*)
      begin case labeltab[x].st of
                entered: begin q := labeltab[x].val;
                           labeltab[x].val := pc
                         end;
                defined: q:= labeltab[x].val
            end(*case label..*)
      end;(*lookup*)

      procedure labelsearch;
         var x: integer; sp: strvsp; flp: flabelp;
      begin skpspc; if ch <> 'l' then errorl('Label format error       ');
            getnxt; parlab(x,sp);
            if sp <> nil then begin { far label }
              new(flp); flp^.next := flablst; flablst := flp;
              flp^.val := pc; flp^.ref := sp; q := 0
            end else lookup(x) { near label }
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

      procedure getexp(var ep: expptr);
      begin
        if efree <> nil then begin ep := efree; efree := ep^.next end
        else new(ep); 
        ep^.next := nil; ep^.op := op; ep^.p := p; ep^.q := q; ep^.l := nil; 
        ep^.r := nil; ep^.r1 := rgnull; ep^.r2 := rgnull; ep^.r3 := rgnull;
        ep^.rs := []
      end;
      
      procedure putexp(ep: expptr);
      begin
        ep^.next := efree; efree := ep
      end;
      
      procedure pshstk(ep: expptr);
      begin
        ep^.next := estack; estack := ep
      end;
      
      procedure popstk(var ep: expptr);
      begin
        if estack = nil then errorl('Expression underflow     ');
        ep := estack; estack := estack^.next; ep^.next := nil
      end;

      procedure botstk;
      begin
        if estack <> nil then errorl('Stack balance             ');
      end;
      
      procedure deltre(ep: expptr);
      begin
        if ep^.l <> nil then deltre(ep^.l);
        if ep^.r <> nil then deltre(ep^.r);
        putexp(ep)
      end;
      
      procedure dmptrel(ep: expptr; lvl: integer);
      begin
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
        writeln(prr, '# ', ' ': lvl, ep^.op:3, ': ', instr[ep^.op]);
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
          writeln('Stack: ', ep^.op:3, ': ', instr[ep^.op]);
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
      var rs: regset;

      procedure resreg(r: reg);
      begin
        if not (r in rf) and (r <> r1) and (r <> r2) then 
          begin ep^.rs := ep^.rs+[r]; rf := rf-[r] end
      end;

      begin
        if r1 <> rgnull then rf := rf-[r1];
        if r2 <> rgnull then rf := rf-[r2];
        case ep^.op of

          {lodi,lodx,loda,lodb,lodc,lda}
          0,193,105,108,109,4: begin resreg(rgrax); ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

          {lodr}
          106: begin resreg(rgrax); ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) end;

          {lods}
          107: begin resreg(rgrax); resreg(rgrsi); resreg(rgrdi); ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) end;

          {adr,sbr}
          29, 31: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            assreg(ep^.l, rf, r1, r2); assreg(ep^.r, rf, rgnull, rgnull) end;

          {equr,neqr,geqr,grtr,leqr,lesr}
          138,144,150,156,162,168: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, rgnull, rgnull); assreg(ep^.r, rf, rgnull, rgnull) end;

          {adi,adr,sbi,sbr,equ,neq,geq,grt,leq,les}
          28, 30, 17, 137, 139, 140, 141, 18, 143, 145, 146, 
          147, 19, 149, 151, 152, 153, 20, 155, 157, 158, 159, 21, 
          161, 163, 164, 165, 167, 169, 170, 171: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, r1, r2); assreg(ep^.r, rf, rgnull, rgnull) end;  

          120{lip}: begin resreg(rgrax); ep^.r1 := r1;
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
            assreg(ep^.l, rf, rgrax, r2); assreg(ep^.r, rf, r1, rgnull);
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
          114: begin resreg(rgrdi); ep^.r1 := r1; ep^.r2 := r2;
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

          {ord}
          59, 134, 136, 200: begin ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf) 
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
            getreg(ep^.t1, rf); getfreg(ep^.t2) 
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
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else begin resreg(rgrax); ep^.r1 := r1 end;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            if sppar[ep^.q] >= 5 then assreg(ep5, frereg, rgrdi, rgnull);
            if sppar[ep^.q] >= 4 then assreg(ep4, frereg, rgrdi, rgnull);
            if sppar[ep^.q] >= 3 then assreg(ep3, frereg, rgrsi, rgnull);
            if sppar[ep^.q] >= 2 then assreg(ep2, frereg, rgrdx, rgnull);
            if sppar[ep^.q] >= 1 then assreg(ep, frereg, rgrcx, rgnull)
          end;

        end
      end;

      procedure wrtins40(si: insstr40; i1, i2: integer; r1, r2: reg);
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
          end else if si[i] = '%' then begin next; write(prr, '%');
            if si[i] = '1' then wrtreg(prr, r1) 
            else if si[i] = '2' then wrtreg(prr, r2)
          end else if si[i] = '+' then begin next; write(prr, '+');
            if si[i] = '0' then write(prr, i1:1) 
            else if si[i] = '1' then write(prr, i2:1)
          end else if si[i] = '-' then begin next; write(prr, '-');
            if si[i] = '0' then write(prr, i1:1) 
            else if si[i] = '1' then write(prr, i2:1)
          end else write(prr, si[i]);
          i := i+1
        end;
        writeln(prr)
      end;

      procedure wrtins10(si: insstr10; i1, i2: integer; r1, r2: reg);
      var s: insstr40;
          i: 1..insmax40;
      begin
        for i := 1 to insmax40 do s[i] := ' ';
        for i := 1 to insmax10 do s[i] := si[i];
        wrtins40(s, i1, i2, r1, r2)
      end;

      procedure wrtins20(si: insstr20; i1, i2: integer; r1, r2: reg);
      var s: insstr40;
          i: 1..insmax40;
      begin
        for i := 1 to insmax40 do s[i] := ' ';
        for i := 1 to insmax20 do s[i] := si[i];
        wrtins40(s, i1, i2, r1, r2)
      end;

      procedure wrtins30(si: insstr30; i1, i2: integer; r1, r2: reg);
      var s: insstr40;
          i: 1..insmax40;
      begin
        for i := 1 to insmax40 do s[i] := ' ';
        for i := 1 to insmax30 do s[i] := si[i];
        wrtins40(s, i1, i2, r1, r2)
      end;

      procedure callsp(ep: expptr); forward;

      procedure genexp(ep: expptr);
      var r: reg;
      begin
        if ep <> nil then begin
          genexp(ep^.l); genexp(ep^.r); genexp(ep^.x1);
          for r := rgrax to rgr15 do if r in ep^.rs then
              wrtins10('push %r1  ', 0, 0, r, rgnull);
          case ep^.op of

            {lodi,loda}
            0,105: begin
              wrtins20('movq $0,%rax         ', ep^.p, 0, rgnull, rgnull);
              wrtins20('call psystem_base    ', 0, 0, rgnull, rgnull);
              wrtins20('add $0,%rax          ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movq (%rax),%r2       ', 0, 0, rgnull, ep^.r1)
            end;

            {lodx,lodb,lodc}
            193,108,109: begin
              wrtins20('movq $0,%rax        ', ep^.p, 0, rgnull, rgnull);
              wrtins20('call psystem_base   ', 0, 0, rgnull, rgnull);
              wrtins20('add $0,%rax         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movzx (%rax),%r2    ', 0, 0, rgnull, ep^.r1)
            end;

            {lodr}
            106: begin
              wrtins20('movq $0,%rax        ', ep^.p, 0, rgnull, rgnull);
              wrtins20('call psystem_base   ', 0, 0, rgnull, rgnull);
              wrtins20('add $0,%rax         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movsd (%rax),%r2    ', 0, 0, rgnull, ep^.r1)
            end;

            {lods}
            107: begin
              wrtins20('movq $0,%rax        ', ep^.p, 0, rgnull, rgnull);
              wrtins20('call psystem_base   ', 0, 0, rgnull, rgnull);
              wrtins20('add $0,%rax         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull);
              wrtins20('movq %rax,%rsi      ', 0, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%rdi      ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%r1       ', 0, 0, rgnull, ep^.r1);
            end;

            {lda}
            4: begin
              wrtins20('movq $0,%rax        ', ep^.p, 0, rgnull, rgnull);
              wrtins20('call psystem_base   ', 0, 0, rgnull, rgnull);
              wrtins20('add $0,%rgrax       ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movq %rax,%r2       ', 0, 0, rgnull, ep^.l^.r1)
            end;

            {adi}
            28: 
              wrtins20('add %r1,%r2         ', 0, 0, ep^.r^.r1, ep^.l^.r1);

            {adr}
            29: 
              wrtins20('addsd %r1,%r2       ', 0, 0, ep^.r^.r1, ep^.l^.r1);

            {sbi}
            30: 
              wrtins20('sub %r1,%r2         ', 0, 0, ep^.r^.r1, ep^.l^.r1);

            {sbr}
            31: 
              wrtins20('subsd %r1,%r2       ', 0, 0, ep^.r^.r1, ep^.l^.r1);

            {equr}
            128: begin 
              wrtins20('cmpeqsd %r1,%r2     ', 0, 0, ep^.r^.r1, ep^.l^.r1);
              wrtins20('movq %r1,%r2        ', 0, 0, ep^.l^.r1, ep^.r1);
              wrtins20('andq %r1,$0         ', 1, 0, ep^.r1, rgnull) 
            end;

            120{lip}: begin 
              wrtins20('movq $0,%rax        ', ep^.p, 0, rgnull, rgnull);
              wrtins20('call psystem_base   ', 0, 0, rgnull, rgnull);
              wrtins20('movq +0(%rax),%r1   ', ep^.q, 0, ep^.r1, rgnull);
              wrtins20('movq +0(%rax),%r1   ', ep^.q+ptrsize, 0, ep^.r2, rgnull);
            end;  

            {equm}
            142: begin 
              wrtins20('movq $0,%rdx        ', q, 0, rgnull, rgnull);
              wrtins20('call psystem_strcmp', 0, 0, rgnull, rgnull); 
              wrtins20('cmpq $0,%rax       ', 0, 0, rgnull, rgnull);
              case ep^.op of
                142{equm}: wrtins10('sete %r1  ', 0, 0, ep^.l^.r1, rgnull);
                148{neqm}: wrtins10('setne %r1 ', 0, 0, ep^.l^.r1, rgnull);
                154{geqm}: wrtins10('setae %r1 ', 0, 0, ep^.l^.r1, rgnull);
                160{grtm}: wrtins10('seta %r1  ', 0, 0, ep^.l^.r1, rgnull);
                166{leqm}: wrtins10('setbe %r1 ', 0, 0, ep^.l^.r1, rgnull);
                172{lesm}: wrtins10('setb %r1  ', 0, 0, ep^.l^.r1, rgnull);
              end
            end;

            5{lao}:
              wrtins40('leaq globals_start+0(%rip),%r1         ', ep^.q, 0, ep^.r1, rgnull);

            16{ixa}: begin 
              wrtins20('movq $0,%rax        ', q, 0, rgnull, rgnull);
              wrtins10('mul %r1   ', 0, 0, ep^.l^.r1, rgnull);
              wrtins20('add %rax,%r1        ', 0, 0, ep^.r^.r1, rgnull);
            end;

            118{swp}: ; { done at top level }

            {ldoi,loda}
            1,65:
              wrtins40('movzx globals_start+0(%rip),%r1         ', ep^.q, 0, ep^.r1, rgnull);

            {ldob,ldoc,ldox}
            68,69,194:
              wrtins40('movzx globals_start+0(%rip),%r1         ', ep^.q, 0, ep^.r1, rgnull);

            {ldor}
            66: 
              wrtins40('movsd globals_start+0(%rip),%r1         ', ep^.q, 0, ep^.r1, rgnull);

            {ldos}
            67: begin
              wrtins40('leaq globals_start+0(%rip),%rsi         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%rdi      ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins10('movsq     ', 0, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%r1       ', 0, 0, rgnull, ep^.l^.r1);
            end;

            {indi,inda}
            9,85: 
              wrtins20('movq +0(%r1),%r1', q, 0, ep^.l^.r1, rgnull);

            {indr}
            86: 
              wrtins20('movsd +0(%r1),%r1', q, 0, ep^.l^.r1, ep^.r1);

            {indb,indc,indx}
            88,89,198: wrtins20('movzx +0(%r1),%r1', q, 0, ep^.l^.r1, rgnull);

            {inds}
            87: begin 
              wrtins20('leaq +0(%r1),%r1', q, 0, ep^.l^.r1, rgnull);
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%rdi      ', 0, 0, rgnull, rgnull);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull);
              wrtins20('movsq               ', 0, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%r1       ', 0, 0, rgnull, ep^.l^.r1);
            end;

            {inci,inca,incb,incc,incx}
            10, 90, 93, 94, 201: 
              wrtins20('addq $0,%r1         ', q, 0, ep^.r1, rgnull);

            {deci,decb,decc,decx}
            57, 103, 104, 202: 
              wrtins20('subq $0,%r1         ', q, 0, ep^.r1, rgnull);

            {ckvi,ckvb,ckvc,ckvx}
            175, 179, 180, 203: begin 
              wrtins20('cmpq $0,%r1         ', q, 0, ep^.r^.r1, rgnull);
              wrtins20('sete %r1            ', 0, 0, ep^.r^.r1, rgnull);
              wrtins20('orq %r1,%r2         ', 0, 0, ep^.r^.r1, ep^.r1);
            end;

            {cvbi,cvbx,cvbb,cvbc}
            100, 115, 116, 121: begin
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movq $0,%rsi         ', ep^.q1, 0, rgnull, rgnull);
              wrtins20('movq $0,%rdx         ', ep^.q2, 0, rgnull, rgnull);
              if ep^.op = 100 then
                wrtins20('movq (%r1),%r8       ', ep^.q, 0, ep^.r^.r1, rgnull)
              else
                wrtins20('movzx (%r1),%r8      ', ep^.q, 0, ep^.r^.r1, rgnull);
              wrtins30('call psystem_tagchgvar         ', 0, 0, rgnull, rgnull);
            end;

            {ivti,ivtx,ivtb,ivtc}
            192,101,102,111: begin
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movq $0,%rsi         ', ep^.q1, 0, rgnull, rgnull);
              wrtins20('movq $0,%rdx         ', ep^.q2, 0, rgnull, rgnull);
              if ep^.op = 100 then
                wrtins20('movq (%r1),%r8       ', ep^.q, 0, ep^.r^.r1, rgnull)
              else
                wrtins20('movzx (%r1),%r8      ', ep^.q, 0, ep^.r^.r1, rgnull);
              wrtins30('call psystem_tagchginv         ', 0, 0, rgnull, rgnull);
            end;

            {cps}
            176: begin 
              wrtins20('cmpq %r1,%r2      ', 0, 0, ep^.r^.r2, ep^.l^.r2);
              wrtins20('je .+21           ', ep^.q, 0, ep^.r^.r1, rgnull);
              wrtins20('movq $0,%rdi      ', ContainerMismatch, 0, rgnull, rgnull);
              wrtins30('call psystem_errorv         ', 0, 0, rgnull, rgnull);
            end;

            {cpc}
            177: begin 
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull);
              wrtins30('call psystem_cmptmp            ', 0, 0, rgnull, rgnull);
            end;


            {cta}
            191: begin
              wrtins20('movq $0,%rdi         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movq $0,%rsi         ', ep^.q1, 0, rgnull, rgnull);
              wrtins20('movq $0,%rdx         ', ep^.q2, 0, rgnull, rgnull);
              wrtins30('call psystem_tagchkass           ', 0, 0, rgnull, rgnull);
            end;

            {lpa}
            114: begin 
              wrtins20('movq $0,%rax         ', ep^.p, 0, rgnull, rgnull);
              wrtins20('call psystem_base    ', 0, 0, rgnull, rgnull);
              wrtins20('movq %rax,%r1        ', 0, 0, ep^.r2, rgnull);
              wrtins20('movq $0,%rax         ', ep^.q, 0, ep^.r1, rgnull);
            end;

            {ldci,ldcc,ldcb}
            123,127,126:
              wrtins20('movq $0,%r1         ', ep^.vali, 0, ep^.r1, rgnull); 

            {ldcn}
            125:
              wrtins20('movq $0,%r1         ', 0, 0, ep^.r1, rgnull);

            {ldcr}
            124: begin 
               write(prr, '        movsd   real', ep^.realn:1, '(%rip),%'); 
               wrtreg(prr, ep^.r1); writeln(prr);
            end;

            {ldcs}
            7: begin 
               write(prr, '        leaq    set', ep^.realn:1, '(%rip),%'); 
               wrtreg(prr, ep^.r1); writeln(prr);
            end;

            {chki,chka,chkb,chkc,chkx}
            26, 95, 98, 99, 199: begin 
              wrtins20('movq $0,%r1         ', 0, 0, ep^.r2, rgnull);
              wrtins20('cmpq (%r1),%r2      ', 0, 0, ep^.r2, ep^.r1);
              wrtins20('jae .+21            ', 0, 0, ep^.r2, rgnull);
              wrtins20('movq $ValueOutOfRange,%rax    ', 0, 0, rgnull, rgnull);
              wrtins20('call errore         ', 0, 0, rgnull, rgnull);
              wrtins20('cmpq +0(%r1),%r2    ', intsize, 0, ep^.r2, ep^.r1);
              wrtins20('jbe .+11            ', 0, 0, ep^.r2, rgnull);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull);
            end;

            {chks}
            97: begin
              wrtins20('movq $0,%rsi         ', ep^.q, 0, rgnull, rgnull);
              wrtins20('movq +0(%rsi),%rdi   ', intsize, 0, rgnull, rgnull);
              wrtins20('movq (%rsi),%rsi     ', intsize, 0, rgnull, rgnull);
              wrtins30('call psystem_chksetbnd         ', 0, 0, rgnull, rgnull);
            end;

            {ckla}
            190: begin
              if ep^.q <> 0 then begin
                wrtins20('orq %r1,%r1         ', 0, 0, ep^.r1, rgnull);
                wrtins20('jbe .+17            ', 0, 0, ep^.r2, rgnull);
                wrtins20('movq $DereferenceOfNilPointer,%rax      ', 0, 0, rgnull, rgnull);
                wrtins20('call psystem_errorv ', 0, 0, rgnull, rgnull)
              end
            end;


            56 {lca}: begin 
               write(prr, '        leaq    string', ep^.strn:1, '(%rip),%'); 
               wrtreg(prr, ep^.r1); writeln(prr);
            end;

            {ord}
            59, 134, 136, 200: ; { ord is a no-op }


            {lcp}
            135: begin 
              wrtins20('movq (%r1),%r2      ', 0, 0, ep^.l^.r1, ep^.r1);
              wrtins20('movq +0(%r1),%r2    ', ptrsize, 0, ep^.l^.r1, ep^.r2);
            end;

            {sgs}
            32: begin
              wrtins20('add $0,%rsp         ', -setsize, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%rsi      ', 0, 0, rgnull, rgnull);
              wrtins20('call psystem_setsgl ', 0, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%r1       ', 0, 0, ep^.r1, rgnull);
            end;

            {flt,flo}
            33,34: wrtins20('cvtsi2sd %r1,%r2    ', 0, 0, ep^.l^.r1, ep^.r1);

            {trc}
            35: wrtins20('cvttsd2si %r1,%r2   ', 0, 0, ep^.l^.r1, ep^.r1);

            {ngi}
            36: wrtins20('negq %r1    ', 0, 0, ep^.r1, rgnull);

            {ngr}
            37: begin 
              wrtins20('subsd %r1,%r1       ', 0, 0, ep^.r1, ep^.r1);
              wrtins20('subsd %r1,%r2       ', 0, 0, ep^.r1, ep^.l^.r1);  
            end;

            {sqi}
            38: begin 
              wrtins20('addq %r1,%r1        ', 0, 0, ep^.r1, rgnull);
            end;

            {sqr}
            39: begin 
              wrtins20('addsd %r1,%r1       ', 0, 0, ep^.r1, rgnull);
            end;

            {abi}
            40: begin
              wrtins20('orq %r1,%r1         ', 0, 0, ep^.r1, rgnull);
              wrtins20('jns .+8             ', 0, 0, ep^.r1, rgnull);
              wrtins20('negq %r1            ', 0, 0, ep^.r1, rgnull)
            end;

            {abr}
            41: begin 
              wrtins30('movq $8000000000000000,%r1    ', 0, 0, ep^.t1, rgnull);
              wrtins20('movq %r1,%r2        ', 0, 0, ep^.t1, ep^.t2);
              wrtins20('xorpd %r1,%r2       ', 0, 0, ep^.t2, ep^.r1)
            end;

            {notb}
            42: begin 
              wrtins20('orq %r1,%r1         ', 0, 0, ep^.r1, rgnull);
              wrtins20('movq $1%r1          ', 0, 0, ep^.r1, rgnull);
              wrtins20('jz .+17            ', 0, 0, ep^.r2, rgnull);
              wrtins20('movq $0%r1          ', 0, 0, ep^.r1, rgnull)
            end;

            {noti}
            205: begin 
              wrtins20('orq %r1,%r1         ', 0, 0, ep^.r1, rgnull);
              wrtins20('jns .+17            ', 0, 0, rgnull, rgnull);
              wrtins20('movq $BooleanOperatorOfNegative,%rax      ', 0, 0, rgnull, rgnull);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull);
              wrtins20('not %r1             ', 0, 0, ep^.r1, rgnull)
            end;

            {odd}
            50: begin 
              wrtins20('andq $1,%r1         ', 0, 0, ep^.r1, rgnull);
            end;

            {rnd}
            62: wrtins20('cvtsd2si %r1,%r2    ', 0, 0, ep^.l^.r1, ep^.r1);

            {chr}
            60: ; { no-op }


            {and,ior,xor}
            43,44,206: begin 
              wrtins20('orq %r1,%r1         ', 0, 0, ep^.l^.r1, rgnull);
              wrtins20('jns .+17            ', 0, 0, rgnull, rgnull);
              wrtins20('movq $BooleanOperatorOfNegative,%rax      ', 0, 0, rgnull, rgnull);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull);
              wrtins20('orq %r1,%r1         ', 0, 0, ep^.r^.r1, rgnull);
              wrtins20('jns .+17            ', 0, 0, rgnull, rgnull);
              wrtins20('movq $BooleanOperatorOfNegative,%rax      ', 0, 0, rgnull, rgnull);
              wrtins20('call psystem_errore ', 0, 0, rgnull, rgnull);
              case ep^.op of
                43: wrtins20('andq %r1,%r2         ', 0, 0, ep^.l^.r1, ep^.r^.r1);
                44: wrtins20('orq %r1,%r2         ', 0, 0, ep^.l^.r1, ep^.r^.r1);
                206: wrtins20('xorq %r1,%r2        ', 0, 0, ep^.l^.r1, ep^.r^.r1)
              end
            end;

            {dif,int,uni}
            45,46,47: begin
              case ep^.op of             
                45: wrtins20('call psystem_setdif ', 0, 0, rgnull, rgnull);
                46: wrtins20('call psystem_setint ', 0, 0, rgnull, rgnull);
                47: wrtins20('call psystem_setuni ', 0, 0, rgnull, rgnull);
              end;
              wrtins20('addq $0,%rsp        ', setsize, 0, rgnull, rgnull);
              if ep^.r1 <> ep^.l^.r1 then
                wrtins20('movq %r1,%r1         ', 0, 0, rgrdi, ep^.l^.r1);
            end;

            {inn}
            48: begin
              wrtins20('call psystem_setinc ', 0, 0, rgnull, rgnull);
              if ep^.r1 <> rgrax then
                wrtins20('movq %rax,%r1         ', 0, 0, ep^.r1, rgnull);
            end;

            {mod}
            49: begin 
              wrtins20('xorq %rdx,%rdx      ', 0, 0, rgnull, rgnull);
              wrtins10('idivq %r1 ', 0, 0, ep^.r^.r1, rgnull);
              if ep^.r1 <> rgrdx then
                wrtins20('movq %rdx,%r1       ', 0, 0, ep^.r1, rgnull);
            end;

            {dvi}
            53: begin 
              wrtins20('xorq %rdx,%rdx      ', 0, 0, rgnull, rgnull);
              wrtins10('idivq %r1 ', 0, 0, ep^.r^.r1, rgnull);
              if ep^.r1 <> rgrax then
                wrtins20('movq %rax,%r1       ', 0, 0, ep^.r1, rgnull);
            end;

            {mpi}
            51: wrtins10('imulq %r1,%r2       ', 0, 0, ep^.r^.r1, rgnull);

            {mpr}
            52: wrtins10('imulsd %r1,%r2      ', 0, 0, ep^.r^.r1, rgnull);

            {dvr}
            54: wrtins10('idivsd %r1,%r2      ', 0, 0, ep^.r^.r1, rgnull);

            {rgs}
            110: begin 
              wrtins20('subq +0,%rsp        ', setsize, 0, rgnull, rgnull);
              wrtins20('movq %rsp,%rdx      ', 0, 0, rgnull, rgnull);
              wrtins20('call psystem_setrgs ', 0, 0, rgnull, rgnull);
              if ep^.r1 <> rgrdx then
                wrtins20('movq %rdx,%r1       ', 0, 0, ep^.r1, rgnull);
            end;

            { dupi, dupa, dupr, dups, dupb, dupc }
            181, 182, 183, 184, 185, 186: begin 
              wrtins20('movq %r2,%r1        ', 0, 0, ep^.l^.r1, ep^.r^.r1);
            end;

            {cks}
            187: begin 
              wrtins10('xorq %r1,%r1        ', 0, 0, ep^.r^.r1, rgnull);
            end;

            {csp}
            15: callsp(ep);

          end;
          for r := rgr15 downto rgrax do if r in ep^.rs then 
            wrtins20('pop %r1             ', 0, 0, r, rgnull)
        end
      end;

      procedure callsppar(p: integer; var sc: alfa; r: boolean);
      var si: insstr20;
      begin
        if p >= 1 then popstk(ep);
        if p >= 2 then popstk(ep2);
        if p >= 3 then popstk(ep3);
        if p >= 4 then popstk(ep4);
        if p >= 5 then popstk(ep5);
        if not r then begin
          if p >= 5 then assreg(ep5, frereg, rgrdi, rgnull);
          if p >= 4 then assreg(ep4, frereg, rgrdi, rgnull);
          if p >= 3 then assreg(ep3, frereg, rgrsi, rgnull);
          if p >= 2 then assreg(ep2, frereg, rgrdx, rgnull);
          if p >= 1 then assreg(ep, frereg, rgrcx, rgnull);
        end;
        if p >= 1 then begin dmptrel(ep); genexp(ep) end;
        if p >= 2 then begin dmptrel(ep2); genexp(ep) end;
        if p >= 3 then begin dmptrel(ep3); genexp(ep) end;
        if p >= 4 then begin dmptrel(ep4); genexp(ep) end;
        if p >= 5 then begin dmptrel(ep5); genexp(ep) end;
        if p >= 1 then deltre(ep);
        if p >= 2 then deltre(ep2);
        if p >= 3 then deltre(ep3);
        if p >= 4 then deltre(ep4);
        if p >= 5 then deltre(ep5);
        si := 'call psystem_       ';
        for i := 1 to maxalfa do si[13+i-1] := sc[i];
        wrtins20(sc, 0, 0, rgnull, rgnull);
        if r then begin
          if ep^.r1 <> rgrax then 
            wrtins20('movq %rax,%r1       ', ep^.p, 0, ep^.r1, rgnull);
          if (ep^.r2 <> rgnull) and (ep^.r2 <> rgrdx)
            wrtins20('movq %rdx,%r1       ', ep^.p, 0, ep^.r2, rgnull);
        end
      end;

      { evaluate and push a parameter list, right to left }
      procedure pushpar(ep: expptr; p: integer);
      begin
        if (ep <> nil) and (p < 6) then pushpar(ep^.next, p+1);
        if p > 6 then assreg(ep, frereg, rgnull, rgnull); 
        else case p of
          1: assreg(ep, frereg, rgdi, rgnull);
          2: assreg(ep, frereg, rgsi, rgnull);
          3: assreg(ep, frereg, rgdx, rgnull);
          4: assreg(ep, frereg, rgcx, rgnull);
          5: assreg(ep, frereg, rg8, rgnull);
          1: assreg(ep, frereg, rg9, rgnull);
        end;
        dmptrel(ep); genexp(ep);
        if ep^.r2 <> rgnull then
          wrtins20('pushq %r1 ', 0, 0, ep^.r2, rgnull);
        if ep^.r1 in [rgrax..rgr15] then
          wrtins20('pushq %r1 ', 0, 0, ep^.r1, rgnull);
        if ep^.r1 in [rgxmm0..rgxmm15] then begin
          wrtins20('subq -0,%esp        ', realsize, 0, rgnull, rgnull);
          wrtins20('movsd %r1,(%esp)    ', 0, 0, e[^.r1, rgnull)
        end
        deltre(ep)
      end;

      procedure callsp(ep: expptr);
      begin
        if ep^.q > maxsp then errorl('Invalid std proc or func ');
        writeln(prr, '# ', sline:6, ': ', iline:6, ': ', q:3, ': -> ', sptable[q]);
        if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then begin
          popstk(ep); popstk(ep2);
          assreg(ep, frereg, rgrsi, rgnull);
          assreg(ep2, frereg, rgrcx, rgnull);
          pushpar(ep2^.next, maxint);
          dmptrel(ep); genexp(ep); dmptrel(ep2); genexp(ep2);
          wrtins20('movq $0,%rax        ', intsize, 0, rgnull, rgnull);
          wrtins20('mulq %rcx ', 0, 0, rgnull, rgnull);
          wrtins20('addq %rsp,%rax      ', 0, 0, rgnull, rgnull);
          wrtins20('movq (%rax),%rdi    ', 0, 0, rgnull, rgnull);
          wrtins20('movq %rsp,%rcx      ', 0, 0, rgnull, rgnull);
          wrtins20('movq %rcx,%rbx      ', 0, 0, rgnull, rgnull);
          if ep^.q = 39 then
            wrtins20('call psystem_nwl    ', 0, 0, rgnull, rgnull)
          else
            wrtins20('call psystem_dsl    ', 0, 0, rgnull, rgnull);
          wrtins20('orq %rbx,%rbx       ', 0, 0, rgnull, rgnull);
          wrtins20('jz .+16   ', 0, 0, rgnull, rgnull);  
          wrtins20('popq %rax ', 0, 0, rgnull, rgnull);
          wrtins20('decq %rbx ', 0, 0, rgnull, rgnull);
          wrtins20('jmp .-12  ', 0, 0, rgnull, rgnull);
        end else callsppar(sppar[ep^.q], sptable[ep^.q], spfunc[ep^.q]);
      end;

   begin { assemble } 
      p := 0;  q := 0;  op := 0;
      getname;
      { note this search removes the top instruction from use }
      while (instr[op]<>name) and (op < maxins) do op := op+1;
      if op = maxins then errorl('illegal instruction      ');
       
      write(prr, '# ', sline:6, ': ', iline:6, ': ', op:3, ': ', name:4, ' ');
      case op of

        { *** non-terminals *** }

        {lodi,lodx,loda,lodr,lods,lodb,lodc,loda}
        0,193,105,106,107,108,109,4: begin 
          read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          getexp(ep); pshstk(ep) 
        end;

        {adi,adr,sbi,sbr}
        28, 29, 30, 31: begin writeln(prr); getexp(ep); popstk(ep^.r); 
          popstk(ep^.l); pshstk(ep);  
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

        {ldo}
        1, 65, 66, 67, 68, 69, 194: begin read(prd,q); writeln(prr,q:1);
          getexp(ep); pshstk(ep) 
        end;

        {ind}
        9, 85, 86, 87, 88, 89, 198: begin read(prd,q); writeln(prr,q:1) end;

        {inc,dec}
        10, 90, 93, 94, 57, 103, 104, 201, 202: begin read(prd,q); 
          writeln(prr,q:1); getexp(ep); popstk(ep^.l); pshstk(ep) 
        end;

        {ckv}
        175, 179, 180, 203: begin read(prd,q); writeln(prr,q:1); getexp(ep);
          popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
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

        {ldc}
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
            end else begin
              if ch <> '''' then
                errorl('illegal character        ');
              getnxt;  c := ch;
              getnxt;
              if ch <> '''' then
                errorl('illegal character        ');
            end;
            getexp(ep); ep^.vali := ord(ch); pshstk(ep);
            writeln(prr)
          end;

          7: begin skpspc;
            if ch <> '(' then
              errorl('ldcs() expected          ');
            s := [ ];  getnxt;
            while ch<>')' do
            begin read(prd,s1); getnxt; s := s + [s1]
            end;
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

        { equ,neq,geq,grt,leq,les with no parameter }
        17, 137, 138, 139, 140, 141,
        18, 143, 144, 145, 146, 147,
        19, 149, 150, 151, 152, 153,
        20, 155, 156, 157, 158, 159,
        161, 162, 163, 164, 165,
        167, 168, 169, 170, 171: begin writeln(prr); getexp(ep); 
          popstk(ep^.r); popstk(ep^.l); pshstk(ep)
        end;

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

        {flo]
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

        {notb,noti,odd,rnd,chr}
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
        187: begin writeln(prr); getexp(ep); popstk(ep^.l); getexp(ep^.r); 
          pshstk(ep)
        end;

        { system calls can be terminal or non-terminal (quasi-terminal?) }

        {csp} 
        15: begin skpspc; getname;
          while name<>sptable[q] do begin 
            q := q+1; if q > maxsp then errorl('std proc/func not found  ')
          end; 
          writeln(prr);
          getexp(ep); 
          if spfunc[q] then pshstk(ep) { non-terminal, stack it }
          else callsp(ep) { terminal, execute here }
        end;

        { *** terminals *** }

        {stri,stra}
        2,70: begin read(prd,p,q); writeln(prr,p:1,' ', q:1);
          popstk(ep); assreg(ep, frereg-[rgrax], rgnull, rgnull); dmptre(ep);
          genexp(ep);
          wrtins20('movq $0,%rax         ', p, 0, rgnull, rgnull);
          wrtins20('call psystem_base    ', 0, 0, rgnull, rgnull);
          wrtins20('add $0,%rax          ', q, 0, rgnull, rgnull);
          wrtins20('movq %r1,(%rax)      ', 0, 0, ep^.r1, rgnull)
          deltre(ep); botstk 
        end;

        {strx,strb,strc} 
        195,73,74: begin
          read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          popstk(ep); assreg(ep, frereg-[rgrax], rgnull, rgnull); dmptre(ep);
          genexp(ep);
          wrtins20('movq $0,%rax         ', p, 0, rgnull, rgnull);
          wrtins20('call psystem_base    ', 0, 0, rgnull, rgnull);
          wrtins20('add $0,%rax          ', q, 0, rgnull, rgnull);
          wrtins20('movq %r1,(%rax)      ', 0, 0, ep^.r1, rgnull)
          deltre(ep); botstk 
        end;

        {strr}
        71: begin read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          popstk(ep); assreg(ep, frereg-[rgrax], rgnull, rgnull); dmptre(ep);
          genexp(ep);
          wrtins20('movq $0,%rax         ', p, 0, rgnull, rgnull);
          wrtins20('call psystem_base    ', 0, 0, rgnull, rgnull);
          wrtins20('add $0,%rax          ', q, 0, rgnull, rgnull);
          wrtins20('movsd %r1,(%rax)      ', 0, 0, ep^.r1, rgnull)
          deltre(ep); botstk 
        end;

        {strs} 
        72:begin read(prd,p,q); writeln(prr,p:1,' ', q:1); 
          popstk(ep); assreg(ep, frereg-[rgrax], rgnull, rgnull); dmptre(ep);
          genexp(ep);
          wrtins20('movq $0,%rax        ', ep^.p, 0, rgnull, rgnull);
          wrtins20('call psystem_base   ', 0, 0, rgnull, rgnull);
          wrtins20('add $0,%rax         ', ep^.q, 0, rgnull, rgnull);
          wrtins20('movq %rax,%rdi      ', 0, 0, rgnull, rgnull);
          wrtins20('movq %rsp,%rsi      ', 0, 0, rgnull, rgnull);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull);
          wrtins10('movsq     ', 0, 0, rgnull, rgnull);
          wrtins20('add $0,%rsp         ', setsize, 0, rgnull, rgnull);
          deltre(ep); botstk 
        end;

        {cup,cuv}
        12, 27: begin read(prd,p); labelsearch; writeln(prr,p:1);
          i = 1; pushpar(stack, 1);
          wrtins10('movq %rsp,%rbp      ', 0, 0, rgnull, rgnull);
          wrtins10('addq $0,%rbp        ', p+marksize, 0, rgnull, rgnull);
          wrtins10('call .+5  ', 0, 0, rgnull, rgnull);
          wrtins10('popq %rax ', 0, 0, rgnull, rgnull);
          wrtins10('movq %rax,+0(%ebp)  ', markra, 0, rgnull, rgnull);
          wrtins10('call                ', 0, 0, rgnull, rgnull);
        end;

        {mst}
        11: begin read(prd,p); writeln(prr,p:1)
        end;

        {cip} 
        113: begin read(prd,p); writeln(prr,p:1); popstk(ep); dmptre(ep);
          deltre(ep); botstk 
        end;

        {mov}
        55: begin read(prd,q); writeln(prr,q:1); popstk(ep); 
          popstk(ep2); dmptre(ep); dmptre(ep2); deltre(ep); deltre(ep2) 
        end;

        {dmp}
        117: begin read(prd,q); writeln(prr,q:1); popstk(ep); dmptrel(ep, 19);
          deltre(ep); botstk 
        end;

        {sro}
        3, 75, 76, 77, 78, 79, 196: begin read(prd,q); writeln(prr,q:1);
          popstk(ep); dmptre(ep); deltre(ep); botstk 
        end;

        {apc}
        178: begin writeln(prr); popstk(ep); popstk(ep2); dmptre(ep2); 
          dmptre(ep); deltre(ep2); deltre(ep); botstk  
        end; 

        {pck, upk}
        63, 64: begin read(prd,q,q1); writeln(prr,q:1, ' ', q1:1); popstk(ep);
          popstk(ep2); popstk(ep3); dmptre(ep3); dmptre(ep2); dmptre(ep); 
          deltre(ep); deltre(ep2); deltre(ep3); botstk 
        end;

        {ujp}
        23: begin read(prd,q); writeln(prr, q:1) 
        end;

        {fjp,tjp,xjp}
        24,25,119: begin read(prd,q); writeln(prr, q:1); popstk(ep); 
          dmptre(ep); deltre(ep); botstk 
        end;

        {ents}
        13: begin labelsearch; writeln(prr) 
        end;

        {ente}
        173: ; { we don't do stack overflow checking in this version }

        {ipj}
        112: begin read(prd,p); labelsearch; writeln(prr, p:1) 
        end;

        {lpa}
        114: begin read(prd,p); labelsearch; writeln(prr); getexp(ep); 
          pshstk(ep);
        end;

        {vbs}
        92: begin read(prd,q); writeln(prr, q:1); popstk(ep); 
          assreg(ep, frereg, rgrdi, rgnull); dmptrel(ep, 19); genexp(ep);
          wrtins20('movq %rdi,%rsi         ', 0, 0, rgnull, rgnull);
          wrtins20('addq $0,%rsi           ', ep^.q-1, 0, rgnull, rgnull);
          wrtins20('call varenter          ', 0, 0, rgnull, rgnull);
          deltre(ep) 
        end;

        {vbe}
        96: wrtins20('call varenter          ', 0, 0, rgnull, rgnull);

        {ret}
        22: writeln(prr);

        {retp}
        14: begin writeln(prr); botstk
        end;

        {reti,retr,retc,retb,reta,retx}
        128, 129, 130, 131, 132, 204: begin writeln(prr); popstk(ep); 
          dmptre(ep); deltre(ep); botstk
        end;

        {vip,vis}
        133, 122: writeln(prr); { ??? fill me in }

        {vin}
        226: writeln(prr); { ??? fill me in }

        {sto}
        6, 80, 81, 82, 83, 84, 197: begin writeln(prr); popstk(ep); 
          popstk(ep2); dmptre(ep2); dmptre(ep); deltre(ep); deltre(ep2); 
          botstk
        end;

        {stp}
        58:;

        {cke}
        188: begin writeln(prr); popstk(ep); popstk(ep2); dmptre(ep2); 
          dmptre(ep); deltre(ep); deltre(ep2); botstk 
        end;

        {inv}
        189: begin writeln(prr); popstk(ep); dmptre(ep); deltre(ep); botstk
        end;

        61 {ujc}: writeln(prr);

        { these are all Pascaline unimplemented }

        {suv}
        91:
        {cjp}
        8:
        {cal}
        21:
        {bge}
        207:
        {ede}
        208:
        {mse}
        209:
        {apc}
        210:
        {cxs}
        211:
        {cxc}
        212:
        {lft} 
        213:
        {max} 
        214:
        {equv} 
        215:
        {neqv} 
        216: 
        {lesv} 
        217: 
        {grtv} 
        218: 
        {leqv} 
        219: 
        {geqv} 
        220: 
        {vdp} 
        221: 
        {spc} 
        222: 
        {ccs} 
        223: 
        {scp} 
        224: 
        {ldp} 
        225: 
        {vin} 
        226: 
        {vdd} 
        227: 
        {ltci} 
        228: 
        {ltcr} 
        229:
        {ltcs} 
        230: 
        {ltcb} 
        231: 
        {ltcc} 
        232: 
        {ltcx} 
        233: 
        {lto} 
        234: 
        {stom} 
        235: 
        {rets} 
        236: 
        {retm} 
        237: 
        {ctb} 
        238: 
        {cpp} 
        239: 
        {cpr} 
        240: 
        {lsa} 
        241: error("Intermediate not implemented");

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
   writeln(prr, '        inputoff   = 0');
   writeln(prr, '        outputoff  = 2');
   writeln(prr, '        erroroff   = 4');
   writeln(prr, '        listoff    = 6');
   writeln(prr, '        commandoff = 8');
   writeln(prr);
   writeln(prr, '# Logical file numbers for header files');
   writeln(prr, '        inputfn   = 1');
   writeln(prr, '        outputfn  = 2');
   writeln(prr, '        errorfn   = 3');
   writeln(prr, '        listfn    = 4');
   writeln(prr, '        commandfn = 5');
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

  csttbl := nil; strnum := 0; realnum := 0; gblsiz := 0;

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
  writeln(prr, '# File generated by P6 Pascal AMD64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln(prr, '#');
  writeln(prr);
  
  xlate; (* assembles and stores code *)

  1 : { abort run }

  writeln;
  writeln('Program generation complete');

end.