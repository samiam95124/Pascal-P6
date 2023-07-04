(*$c+,t-,d-,l-*)
{*******************************************************************************
*                                                                              *
*                         PASCAL-P6 PORTABLE INTERPRETER                       *
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
*                           Portable Pascal compiler                           *
*                           ************************                           *
*                                                                              *
*                                 Pascal P5                                    *
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
* Adaption from P4 to P5 by:                                                   *
*                                                                              *
*    Scott A. Moore                                                            *
*    samiam@moorecad.com                                                       *
*                                                                              *
* Note for the implementation.                                                 *
* ===========================                                                  *
* This interpreter is written for the case where all the fundamental types     *
* take one storage unit.                                                       *
*                                                                              *
* In an actual implementation, the handling of the sp pointer has to take      *
* into account the fact that the types may have lengths different from one:    *
* in push and pop operations the sp has to be increased and decreased not      *
* by 1, but by a number depending on the type concerned.                       *
*                                                                              *
* However, where the number of units of storage has been computed by the       *
* compiler, the value must not be corrected, since the lengths of the types    *
* involved have already been taken into account.                               *
*                                                                              *
* P5 errors added:                                                             *
*                                                                              *
* 182 identifier too long                                                      *
* 183 For index variable must be local to this block                           *
* 184 Interprocedure goto does not reference outter block of destination       *
*                                                                              *
* P5 instructions modified:                                                    *
*                                                                              *
* lca'string'       '                                                          *
*                                                                              *
* was changed to                                                               *
*                                                                              *
* lca 'string'''                                                               *
*                                                                              *
* That is, lca has a space before the opening quote, no longer pads to the     *
* right, and represents single quotes with a quote image. pint converts quote  *
* images back to single quotes, and pads out strings to their full length.     *
*                                                                              *
* In addition, the way files work was extensively modified. Original P5 could  *
* not represent files as fully expressed variables, such as within an array    *
* or record, and were effectively treated as constants. To treat them as true  *
* variable accesses, the stacking order of the file in all file subroutines    *
* was changed so that the file is on the bottom. This matches the source       *
* order of the file in write(f, ...) or read(f, ...). Also, the file           *
* operations now leave the file on the stack for the duration of a write or    *
* read, then dump them using a specific new instruction "dmp". This allows     *
* multiparameter writes and reads to be effectively a chain of single          *
* operations using one file reference. Finally, files were tied to the type    *
* ending 'a', because files are now full variable references.                  *
*                                                                              *
* Layout of memory in store:                                                   *
*                                                                              *
*    maxstr ->    ---------------------                                        *
*                 | Stack             |                                        *
*        sp ->    ---------------------                                        *
*                 | Free space        |                                        *
*        np ->    ---------------------                                        *
*                 | Heap              |                                        *
*        gbtop -> ---------------------                                        *
*                 | Globals           |                                        *
*        pctop -> ---------------------                                        *
*                 | Constants         |                                        *
*                 ---------------------                                        *
*                 | Code              |                                        *
*                 ---------------------                                        *
*                                                                              *
* The constants are loaded upside down from the top of memory. The heap grows  *
* down, the stack grows up, and when they cross, it is an overflow error.      *
*                                                                              *
* This is the mach (machine) module. This is a cut down interpreter derived    *
* from pint, whose only job is to interpret the code. It loads from prd in     *
* binary deck form, then interprets the code.                                  *
*                                                                              *
* ---------------------------------------------------------------------------- *
*                                                                              *
*                                   LICENSE                                    *
*                                                                              *
* ---------------------------------------------------------------------------- *
*                                                                              *
* This software is based on, and represents an enhanced version, of Pascal-P5, *
* which is itself based on Pascal-P4, and was enhanced from that version       *
* substantially.                                                               *
*                                                                              *
* Pascal-P4 is unlicensed and exists in the public domain. It has:             *
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
* I, Scott Franco, have extensively expanded the original software. The        *
* the changes made by me are held in copyright by me and released under the    *
* BSD "2-clause" license, the least restrictive open source license available. *
*                                                                              *
*******************************************************************************}

{ Set default configuration flags. This gives proper behavior even if no
  preprocessor flags are passed in. 
  
  The defaults are:
  WRDSIZ32       - 32 bit compiler.
  ISO7185_PASCAL - uses ISO 7185 standard language only.
}
#if !defined(WRDSIZ16) && !defined(WRDSIZ32) && !defined(WRDSIZ64)
#define WRDSIZ32 1
#endif

#if !defined(LENDIAN) && !defined(BENDIAN)
#define LENDIAN
#endif

#if !defined(GNU_PASCAL) && !defined(ISO7185_PASCAL) && !defined(PASCALINE)
#define ISO7185_PASCAL
#endif

program pmach(input,output,prd,prr
              { Pascaline start !
              ,command
              ! Pascaline end }
              );

label 99;

const

      { ************************************************************************

      Program object sizes and characteristics, sync with pint. These define
      the machine specific characteristics of the target.

      The configurations are as follows:

      type                  #bits 16    #bits 32  #bits 64
      ===========================================================
      integer               16          32        64
      real                  32          64        64
      char                  8           8         8
      boolean               8           8         8
      set                   256         256       256
      pointers              16          32        64
      marks                 16          32        64 (bytes)
      File logical number   8           8         8

      Both endian types are supported. There is no alignment needed, but you
      may wish to use alignment to tune the runtime speed.

      The machine characteristics dependent on byte accessable machines. This
      table is all you should need to adapt to any byte addressable machine.

      }

#ifdef WRDSIZ16
#include "mpb16.inc"
#endif

#ifdef WRDSIZ32
#include "mpb32.inc"
#endif

#ifdef WRDSIZ64
#include "mpb64.inc"
#endif

      { ******************* end of pcom and pint common parameters *********** }

      { internal constants }

      { !!! Need to use the small size memory to self compile, otherwise, by
        definition, pint cannot fit into its own memory. }
#ifndef SELF_COMPILE
#ifdef WRDSIZ16
      maxstr      = 31999;  { maximum size of addressing for program/var }
      maxtop      = 32000;  { maximum size of addressing for program/var+1 }
      maxdef      = 4000;   { maxstr / 8 for defined bits }
#else
      maxstr      = 16777215;  { maximum size of addressing for program/var }
      maxtop      = 16777216;  { maximum size of addressing for program/var+1 }
      maxdef      = 2097152;   { maxstr / 8 for defined bits }
#endif
#else
      maxstr     =  2000000;   { maximum size of addressing for program/var }
      maxtop     =  2000001;   { maximum size of addressing for program/var+1 }
      maxdef      = 250000;    { maxstr /8 for defined bits }
#endif
      
      maxdigh     = 6;       { number of digits in hex representation of maxstr }
      maxdigd     = 8;       { number of digits in decimal representation of maxstr }
      maxast      = 100;     { maximum size of assert message }
      maxdbf      = 30;      { size of numeric conversion buffer }
      maxcmd      = 250;     { size of command line buffer }

      codemax     = maxstr;  { set size of code store to maximum possible }

      maxlabel = 5000;       { total possible labels in intermediate }
      maxcstfx = 10000;      { maximum constant fixup in intermediate }
      maxgblfx = 10000;      { maximum global access fixup in intermediate }
      resspc   = 0;          { reserve space in heap (if you want) }

      { locations of header files after program block mark, each header
        file is two values, a file number and a single character buffer }
      filres     = 2;        { space reserved for file }  
      inputoff   = 0;        { 'input' file address }
      outputoff  = 2;        { 'output' file address }
      prdoff     = 4;        { 'prd' file address }
      prroff     = 6;        { 'prr' file address }
      erroroff   = 8;        { 'error' file address }
      listoff    = 10;       { 'list' file address }
      commandoff = 12;       { 'command' file address }

      { assigned logical channels for header files }
      inputfn    = 1;        { 'input' file no. }
      outputfn   = 2;        { 'output' file no. }
      prdfn      = 3;        { 'prd' file no. }
      prrfn      = 4;        { 'prr' file no. }
      errorfn    = 5;        { 'error' file no. }
      listfn     = 6;        { 'list' file no. }
      commandfn  = 7;        { 'command' file no. }
      
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
      DisposeOfWithReferencedBlock       = 118;
      WithBaseListEmpty                  = 119;
      ExternalsNotEnabled                = 120;
      privexceptiontop                   = 120;

      maxsp       = 81;   { number of predefined procedures/functions }
      maxins      = 255;  { maximum instruction code, 0-255 or byte }
      maxfil      = 100;  { maximum number of general (temp) files }
      fillen      = 2000; { maximum length of filenames }
      maxsym      = 20;   { maximum length of symbol/module name }
      maxopt      = 26;   { number of options }
      optlen      = 10;   { maximum length of option words }

#include "version.inc"

type

#if defined(WRDSIZ16) && defined(GNU_PASCAL)
      /* for GNU 16 bit mode, use both 16 bit defines and redefine integer and
         real size to match */
      { Allow GNU Pascal extensions }
      {$gnu-pascal}
      pminteger = shortint;
      pmreal = shortreal;
      pmaddress = shortint;
      { Restore to ISO 7185 Pascal language }
      {$classic-pascal-level-0}
#else
      { define the internally used types }
      pminteger = integer;
      pmreal = real;
      pmaddress = -maxstr..maxtop;
#endif

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
      address     = integer; { address }

      beta        = packed array[1..25] of char; (*error message*)
      settype     = set of setlow..sethigh;
#ifdef GNU_PASCAL
{$gnu-pascal}
#endif
      ibyte       = byte; { 8-bit byte }
      bytfil      = packed file of byte; { untyped file of bytes }
#ifdef GNU_PASCAL
{$classic-pascal-level-0}
#endif
      charptr     = ^char; { pointer to character }
      fileno      = 0..maxfil; { logical file number }
      filnam      = packed array [1..fillen] of char; { filename strings }
      filsts      = (fnone, fclosed, fread, fwrite);
      cmdinx      = 1..maxcmd; { index for command line buffer }
      cmdnum      = 0..maxcmd; { length of command line buffer }
      cmdbuf      = packed array [cmdinx] of char; { buffer for command line }
      { VAR reference block }
      varptr       = ^varblk;
      varblk       = record 
                       next: varptr; { next entry }
                       s, e: address { start and end address of block }
                     end;
      { with reference block }
      wthptr       = ^wthblk;
      wthblk       = record
                       next: wthptr; { next entry }
                       b: address    { address of block }
                     end;
      symnam      = packed array [1..maxsym] of char; { symbol/module name }
      optinx      = 1..optlen;
      optstr      = packed array [optinx] of char;

var   pc          : address;   (*program address register*)
      pctop       : address;   { top of code store }
      gbtop       : address;   { top of globals, size of globals }
      op : instyp; p : lvltyp; q : address;  (*instruction register*)
      q1,q2: address; { extra parameters }
      store       : packed array [0..maxstr] of ibyte; { complete program storage }
      storedef    : packed array [0..maxdef] of ibyte; { defined bits }
      sdi         : 0..maxdef; { index for that }
      mp,sp,np,ep : address;  (* address registers *)
      expadr      : address; { exception address of exception handler starts }
      expstk      : address; { exception address of sp at handlers }
      expmrk      : address; { exception address of mp at handlers }
      (*mp  points to beginning of a data segment
        sp  points to top of the stack
        ep  points to the maximum extent of the stack
        np  points to top of the dynamically allocated area*)
      bitmsk      : packed array [0..7] of ibyte; { bits in byte }
      option      : array [1..maxopt] of boolean; { option array }
      options     : array [1..maxopt] of boolean; { option was set array }
      opts        : array [1..maxopt] of optstr;
      optsl       : array [1..maxopt] of optstr;
      
      { check flags: these turn on runtime checks }
      dochkovf: boolean; { check arithmetic overflow }

      { debug flags: turn these on for various dumps and traces }
      dosrclin: boolean; { add source line sets to code }
      dorecycl: boolean; { obey heap space recycle requests }
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

      { unused (here) flags }
      dodmplab: boolean; { dump label definitions }
      dodebug:  boolean; { start up debug on entry }
      dodbgflt: boolean; { enter debug on fault }
      dodbgsrc: boolean; { do source file debugging }
      dodckout: boolean; { do output code deck }
      dochkvbk: boolean; { do check VAR blocks }
      doechlin: boolean; { do echo command line for testing (unused) }
      
      iso7185: boolean; { iso7185 standard flag }
      flipend: boolean; { endian mode is opposing }
      
      { !!! remove this next statement for self compile }
#ifndef SELF_COMPILE
      prd,prr     : text; (*prd for read only, prr for write only *)
#endif

      srclin      : integer; { current source line executing }
      cmdlin      : cmdbuf; { command line }
      cmdlen      : cmdnum; { length of command line }
      cmdpos      : cmdinx; { current position in command line }
      stopins     : boolean; { stop instruction executed }

      filtable    : array [1..maxfil] of text; { general (temp) text file holders }
      { general (temp) binary file holders }
      bfiltable   : array [1..maxfil] of bytfil;
      { file state holding }
      filstate    : array [1..maxfil] of filsts;
      { file buffer full status }
      filbuff     : array [1..maxfil] of boolean;
      { file name has been assigned }
      filanamtab  : array [1..maxfil] of boolean;
      varlst      : varptr; { active var block pushdown stack }
      varfre      : varptr; { free var block entries }
      wthlst      : wthptr; { active with block pushdown stack }
      wthcnt      : integer; { number of outstanding with levels }
      wthfre      : wthptr; { free with block entries }
      maxpow10    : integer; { maximum power of 10 }
      decdig      : integer; { digits in unsigned decimal }
      maxpow16    : integer; { maximum power of 16 }
      hexdig      : integer; { digits in unsigned hex }
      maxpow8     : integer; { maximum power of 8 }
      octdig      : integer; { digits in unsigned octal }
      maxpow2     : integer; { maximum power of 2 }
      bindig      : integer; { digits in unsigned binary }
      newline     : boolean; { output is on new line (unused) }
      extvecbase  : integer; { base of external vectors }
      exitcode    : integer; { exit code for program }
      breakflag   : boolean; { user break signaled }
      
      i           : integer;
      c1          : char;
      ad          : address;
      bai         : integer;
      oi          : 1..maxopt;

(*--------------------------------------------------------------------*)

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

procedure wrthex(var tf: text; v: integer; f: integer; lz: boolean);
begin
  wrtnum(tf, v, 16, f, lz)
end;

{ print hex full word with leading zeros for diagnostics }
procedure prthex(v: integer);
begin
  wrthex(output, v, maxdigh, true)
end;

procedure dmpmem(s, e: address);
var c: integer;
begin
    c := 0;
    while s <= e do begin
        if c = 0 then begin wrtnum(output, s, 16, 8, true); write(': ') end;
        wrtnum(output, store[s], 16, 2, true); write(' '); c := c+1; s := s+1;
        if c = 16 then begin writeln; c := 0 end
    end;
    if c <> 0 then writeln;
    writeln
end;

{ Low level error check and handling }

procedure errors(a: address; l: address);
begin writeln; write('*** Runtime error'); 
      if srclin > 0 then write(' [', srclin:1, ']');
      write(': ');
      if l > maxast then l := maxast;
      while l > 0 do begin write(chr(store[a])); a := a+1; l := l-1 end;
      goto 99
end;(*errori*)

{ Error handling:

  To throw a standard exception, errore() is used. To bypass the interceptable
  exceptions and directly print, use errorv().
 
}

{ throw an exception by vector }
procedure errore(ei: integer); forward;

{ handle exception vector }
procedure errorv(ea: address);
begin writeln; write('*** Runtime error');
  if srclin > 0 then write(' [', srclin:1, ']');
  write(': ');
  case ea of
  
    { Exceptions that can be intercepted }
    ValueOutOfRange:                    writeln('Value out of range');
    ArrayLengthMatch:                   writeln('Array length match');
    CaseValueNotFound:                  writeln('Case value not found');
    ZeroDivide:                         writeln('Zero divide');
    InvalidOperand:                     writeln('Invalid operand');
    NilPointerDereference:              writeln('Nil pointer dereference');
    RealOverflow:                       writeln('Real overflow');
    RealUnderflow:                      writeln('Real underflow');
    RealProcessingFault:                writeln('Real processing fault');
    TagValueNotActive:                  writeln('Tag value not active');
    TooManyFiles:                       writeln('Too many files');
    FileIsOpen:                         writeln('File is open');
    FileAlreadyNamed:                   writeln('File already named');
    FileNotOpen:                        writeln('File not open');
    FileModeIncorrect:                  writeln('File mode incorrect');
    InvalidFieldSpecification:          writeln('Invalid field specification');
    InvalidRealNumber:                  writeln('Invalid real number');
    InvalidFractionSpecification:       writeln('Invalid fraction specification');
    InvalidIntegerFormat:               writeln('Invalid integer format');
    IntegerValueOverflow:               writeln('Integer value overflow');
    InvalidRealFormat:                  writeln('Invalid real format');
    EndOfFile:                          writeln('End of file');
    InvalidFilePosition:                writeln('Invalid file position');
    FilenameTooLong:                    writeln('Filename too long');
    FileOpenFail:                       writeln('File open fail');
    FileSIzeFail:                       writeln('File size fail');
    FileCloseFail:                      writeln('File close fail');
    FileReadFail:                       writeln('File read fail');
    FileWriteFail:                      writeln('File write fail');
    FilePositionFail:                   writeln('File position fail');
    FileDeleteFail:                     writeln('File delete fail');
    FileNameChangeFail:                 writeln('File name change fail');
    SpaceAllocateFail:                  writeln('Space allocate fail');
    SpaceReleaseFail:                   writeln('Space release fail');
    SpaceAllocateNegative:              writeln('Space allocate negative');
    CannotPerformSpecial:               writeln('Cannot perform special');
    CommandLineTooLong:                 writeln('Command line too long');
    ReadPastEOF:                        writeln('Read past eof');
    FileTransferLengthZero:             writeln('File transfer length zero');
    FileSizeTooLarge:                   writeln('File size too large');
    FilenameEmpty:                      writeln('Filename empty');
    CannotOpenStandard:                 writeln('Cannot open standard');
    TooManyTemporaryFiles:              writeln('Too many temporary files');
    InputBufferOverflow:                writeln('Input buffer overflow');
    TooManyThreads:                     writeln('Too many threads');
    CannotStartThread:                  writeln('Cannot start thread');
    InvalidThreadHandle:                writeln('Invalid thread handle');
    CannotStopThread:                   writeln('Cannot stop thread');
    TooManyIntertaskLocks:              writeln('Too many inter task locks');
    InvalidLockHandle:                  writeln('Invalid lock handle');
    LockSequenceFail:                   writeln('Lock sequence fail');
    TooManySignals:                     writeln('Too many signals');
    CannotCreateSignal:                 writeln('Cannot create signal');
    InvalidSignalHandle:                writeln('Invalid signal handle');
    CannotDeleteSignal:                 writeln('Cannot delete signal');
    CannotSendSignal:                   writeln('Cannot send signal');
    WaitForSignalFail:                  writeln('Wait for signal fail');
    FieldNotBlank:                      writeln('Field not blank');
    ReadOnWriteOnlyFile:                writeln('Read on write only file');              
    WriteOnReadOnlyFile:                writeln('Write on read only file');              
    FileBufferVariableUndefined:        writeln('File buffer variable undefined');      
    NondecimalRadixOfNegative:          writeln('Nondecimal radix of negative');        
    InvalidArgumentToLn:                writeln('Invalid argument to ln');              
    InvalidArgumentToSqrt:              writeln('Invalid argument to sqrt');            
    CannotResetOrRewriteStandardFile:   writeln('Cannot reset or rewrite standard file'); 
    CannotResetWriteOnlyFile:           writeln('Cannot reset write only file');         
    CannotRewriteReadOnlyFile :         writeln('Cannot rewrite read only file');        
    SetElementOutOfRange:               writeln('Set element out of range');             
    RealArgumentTooLarge:               writeln('Real argument too large');             
    BooleanOperatorOfNegative:          writeln('Boolean operator of negative');        
    InvalidDivisorToMod:                writeln('Invalid divisor to mod');              
    PackElementsOutOfBounds:            writeln('Pack elements out of bounds');          
    UnpackElementsOutOfBounds:          writeln('Unpack elements out of bounds');
    CannotResetClosedTempFile:          writeln('Cannot reset closed temp file');        
                      
    { Exceptions that can't be intercepted }
    UndefinedLocationAccess:            writeln('Undefined location access');
    FunctionNotImplemented:             writeln('Function not implemented');
    InvalidInISO7185Mode:               writeln('Invalid in ISO 7185 mode');              
    HeapFormatInvalid:                  writeln('Heap format invalid');                 
    DisposeOfUninitalizedPointer:       writeln('Dispose of uninitalized pointer');      
    DisposeOfNilPointer:                writeln('Dispose of nil pointer');               
    BadPointerValue:                    writeln('Bad pointer value');                   
    BlockAlreadyFreed:                  writeln('Block already freed');                 
    InvalidStandardProcedureOrFunction: writeln('Invalid standard procedure or function');
    InvalidInstruction:                 writeln('Invalid instruction');                
    NewDisposeTagsMismatch:             writeln('New dispose tags mismatch');            
    PCOutOfRange:                       writeln('Pc out of range');                      
    StoreOverflow:                      writeln('Store overflow');                    
    StackBalance:                       writeln('Stack balance');                     
    SetInclusion:                       writeln('Set inclusion');                     
    UninitializedPointer:               writeln('Uninitialized pointer');             
    DereferenceOfNilPointer:            writeln('Dereference of nil pointer');          
    PointerUsedAfterDispose:            writeln('Pointer used after dispose');          
    VariantNotActive:                   writeln('Variant not active');                 
    InvalidCase:                        writeln('Invalid case');                      
    SystemError:                        writeln('System error');                      
    ChangeToAllocatedTagfield:          writeln('Change to allocated tag field');  
    UnhandledException:                 writeln('Unhandled exception');   
    ProgramCodeAssertion:               writeln('Program code assertion');
    VarListEmpty:                       writeln('VAR block list empty');
    ChangeToVarReferencedVariant:       writeln('Change to VAR referenced variant');
    DisposeOfVarReferencedBlock:        writeln('Dispose of VAR referenced block');
    VarReferencedFileBufferModified:    writeln('VAR referenced file buffer modified');
    ContainerMismatch:                  writeln('Container length(s) do not match');
    InvalidContainerLevel:              writeln('InvalidContainerLevel');
    DisposeOfWithReferencedBlock:       writeln('Dispose of with referenced block');
    WithBaseListEmpty:                  writeln('With base list empty');
    ExternalsNotEnabled:                writeln('Externals not enabled');
  end;
  goto 99
end;

procedure errorm(ea: address);
begin 
  { check is a standard exception }
  if (ea-pctop >= exceptionbase) and 
     (ea-pctop <= exceptiontop) then errorv(ea-pctop)
  else errorv(UnhandledException)
end;

{ get bit from defined array }

function getdef(a: address): boolean;

var b: ibyte;
    r: boolean;

begin

  if dochkdef then begin

    b := storedef[a div 8]; { get byte }
    r := odd(b div bitmsk[a mod 8])

  end else r := true; { always set defined }

  getdef := r

end;

{ put bit to defined array }

procedure putdef(a: address; b: boolean);

var sb: ibyte;
    r:  boolean;

begin

  if dochkdef then begin

    sb := storedef[a div 8]; { get byte }
    { test bit as is }
    r := odd(sb div bitmsk[a mod 8]);
    if r <> b then begin

      if b then sb := sb+bitmsk[a mod 8]
      else sb := sb-bitmsk[a mod 8];
      storedef[a div 8] := sb

    end

  end

end;

procedure chkdef(a: address);
begin
   if dochkdef then if not getdef(a) then errorv(UndefinedLocationAccess)
end;

(*-------------------------------------------------------------------------*)

                        { Boolean integer emulation }

  { Boolean emulation can be remapped directly to operators for performance
    reasons, but is typically left alone. The results are the same. }
  
(*-------------------------------------------------------------------------*)

function bnot(a: integer): integer;
var i, r, p: integer;
begin
  r := 0; p := 1; i := maxint;
  while i <> 0 do begin
    if not odd(a) then r := r+p;
    a := a div 2; i := i div 2;
    if i > 0 then p := p*2
  end;
  bnot := r
end;

function bor(a, b: integer): integer;
var i, r, p: integer;
begin
  r := 0; p := 1; i := maxint;
  while i <> 0 do begin
    if odd(a) or odd(b) then r := r+p;
    a := a div 2; b := b div 2; i := i div 2;
    if i > 0 then p := p*2
  end;
  bor := r
end;

function band(a, b: integer): integer;
var i, r, p: integer;
begin
  r := 0; p := 1;  i := maxint;
  while i <> 0 do begin
    if odd(a) and odd(b) then r := r+p;
    a := a div 2; b := b div 2; i := i div 2;
    if i > 0 then p := p*2
  end;
  band := r
end;

function bxor(a, b: integer): integer;
var i, r, p: integer;
begin
  r := 0; p := 1; i := maxint;
  while i <> 0 do begin
    if odd(a) <> odd(b) then r := r+p;
    a := a div 2; b := b div 2; i := i div 2;
    if i > 0 then p := p*2
  end;
  bxor := r
end;

{ End of language extension routines }

{ find lower case of character }
function lcase(c: char): char;
begin
  if c in ['A'..'Z'] then c := chr(ord(c)-ord('A')+ord('a'));
  lcase := c
end { lcase };

(*--------------------------------------------------------------------*)

{ "fileofy" routines for command line processing.
  
  These routines implement the command header file by reading from a
  buffer where the command line is stored. The assumption here is that
  there is a fairly simple call to retrieve the command line.
  
  If it is wanted, the command file primitives can be used to implement
  another type of interface, say, reading from an actual file.
  
  The command buffer is designed to never be completely full.
  The last two locations indicate:
  
  maxcmd: end of file
  maxcmd-1: end of line
  
  These locations are always left as space, thus eoln returns space as
  the standard specifies.
}
  
function bufcommand: char; 
begin bufcommand := cmdlin[cmdpos] end;

procedure getcommand; 
begin if cmdpos <= cmdlen+1 then cmdpos := cmdpos+1 end;

function eofcommand: boolean; 
begin eofcommand := cmdpos > cmdlen+1 end;

function eolncommand: boolean; 
begin eolncommand := cmdpos >= cmdlen+1 end;

procedure readlncommand; 
begin cmdpos := maxcmd end;

(*--------------------------------------------------------------------*)

{ Language extension routines }
  
#ifdef GNU_PASCAL
#include "extend_gnu_pascal.inc"
#endif

#ifdef ISO7185_PASCAL
#include "extend_iso7185_pascal.inc"
#endif

#ifdef PASCALINE
#include "extend_pascaline.inc"
#endif

(*--------------------------------------------------------------------*)

{ The external assigns read a filename off the command line. The original name
  of the header file is also passed in, and can be used to process. However,
  this implementation ignores them and just reads the names in order off the
  command line (as provided for in Annex C of the Pascaline standard).
  
  The processing of command line filenames does not exclude the use of the
  command file. The command file simply starts reading after all filename
  parameters have been removed.
}

procedure assignexternaltext(var f: text; var fn: filnam);
var fne: filnam; i: integer;
begin
  for i := 1 to fillen do fne[i] := ' ';
  { skip leading spaces }
  while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
  i := 1;
  while not eolncommand and not eofcommand and 
        (bufcommand in ['a'..'z',  'A'..'Z', '_', '0'..'9']) do begin
    if i = fillen then errorv(FilenameTooLong);
    fne[i] := bufcommand;
    getcommand;
    i := i+1
  end;
  assigntext(f, fne) { assign to that }
end;

procedure assignexternalbin(var f: bytfil; var fn: filnam);
var fne: filnam; i: integer;
begin
  for i := 1 to fillen do fne[i] := ' ';
  { skip leading spaces }
  while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
  i := 1;
  while not eolncommand and not eofcommand and 
        (bufcommand in ['a'..'z',  'A'..'Z', '_', '0'..'9']) do begin
    if i = fillen then errorv(FileNameTooLong);
    fne[i] := bufcommand;
    getcommand;
    i := i+1
  end;
  assignbin(f, fne) { assign to that }
end;

(*--------------------------------------------------------------------*)

{ Accessor functions

  These translate store variables to internal, and convert to and from store RAM
  formats.

  The acessors are fairly machine independent, they rely here on the machine
  being byte addressable. The endian format is inherent to the machine.

  The exception are the get/put int8,16,32,64 and 128 bit routines, which are
  dependent on the endian mode of the machine.

}

{ check running on a little endian processor }

function litend: boolean;

var r: record case boolean of

         true: (i: integer);
         false: (b: ibyte);

       end;

begin

   r.i := 1;
   litend := r.b <> 0

end;

function getint(a: address): integer;

var r: record case boolean of

          true:  (i: integer);
          false: (b: packed array [1..intsize] of ibyte);

       end;
    i: 1..intsize;

begin
   if dochkdef then chkdef(a);
   if flipend then for i := intsize downto 1 do r.b[i] := store[a+(intsize-i)]
   else for i := 1 to intsize do r.b[i] := store[a+i-1];

   getint := r.i
end;

procedure putint(a: address; x: integer);

var r: record case boolean of

          true:  (i: integer);
          false: (b: packed array [1..intsize] of ibyte);

       end;
    i: 1..intsize;

begin

   r.i := x;
   if flipend then for i := intsize downto 1 do
     begin store[a+(intsize-i)] := r.b[i]; putdef(a+(intsize-i), true) end
   else for i := 1 to intsize do
     begin store[a+i-1] := r.b[i]; putdef(a+i-1, true) end

end;

function getrel(a: address): real;

var r: record case boolean of

          true:  (r: real);
          false: (b: packed array [1..realsize] of ibyte);

       end;
    i: 1..realsize;

begin

   if dochkdef then chkdef(a);
   if flipend then for i := realsize downto 1 do r.b[i] := store[a+(realsize-i)]
   else for i := 1 to realsize do r.b[i] := store[a+i-1];
   getrel := r.r

end;

procedure putrel(a: address; f: real);

var r: record case boolean of

          true:  (r: real);
          false: (b: packed array [1..realsize] of ibyte);

       end;
    i: 1..realsize;

begin

   r.r := f;
   if flipend then for i := realsize downto 1 do
     begin store[a+(realsize-i)] := r.b[i]; putdef(a+(realsize-i), true) end
   else for i := 1 to realsize do
     begin store[a+i-1] := r.b[i]; putdef(a+i-1, true) end

end;

function getbol(a: address): boolean;

var b: boolean;

begin

   if dochkdef then chkdef(a);
   if store[a] = 0 then b := false else b := true;
   getbol := b

end;

procedure putbol(a: address; b: boolean);

begin

   store[a] := ord(b); putdef(a, true)

end;

procedure getset(a: address; var s: settype);

var r: record case boolean of

          true:  (s: settype);
          false: (b: packed array [1..setsize] of ibyte);

       end;
    i: 1..setsize;

begin

   if dochkdef then chkdef(a);
   for i := 1 to setsize do r.b[i] := store[a+i-1];
   s := r.s

end;

procedure putset(a: address; s: settype);

var r: record case boolean of

          true:  (s: settype);
          false: (b: packed array [1..setsize] of ibyte);

       end;
    i: 1..setsize;

begin

   r.s := s;
   for i := 1 to setsize do
     begin store[a+i-1] := r.b[i]; putdef(a+i-1, true) end

end;

function getchr(a: address): char;

begin

   if dochkdef then chkdef(a);
   getchr := chr(store[a])

end;

procedure putchr(a: address; c: char);

begin

   store[a] := ord(c); putdef(a, true)

end;

function getbyt(a: address): ibyte;

begin

   if dochkdef then chkdef(a);
   getbyt := store[a]

end;

procedure putbyt(a: address; b: ibyte);

begin

   store[a] := b; putdef(a, true)

end;

function getadr(a: address): address;

var r: record case boolean of

          true:  (a: address);
          false: (b: packed array [1..adrsize] of ibyte);

       end;
    i: 1..adrsize;

begin

   if dochkdef then chkdef(a);
   if flipend then for i := adrsize downto 1 do r.b[i] := store[a+(adrsize-i)]
   else for i := 1 to adrsize do r.b[i] := store[a+i-1];

   getadr := r.a

end;

procedure putadr(a: address; ad: address);

var r: record case boolean of

          true:  (a: address);
          false: (b: packed array [1..adrsize] of ibyte);

       end;
    i: 1..adrsize;

begin

   r.a := ad;
   if flipend then for i := adrsize downto 1 do
     begin store[a+(adrsize-i)] := r.b[i]; putdef(a+(adrsize-i), true) end
   else for i := 1 to adrsize do
     begin store[a+i-1] := r.b[i]; putdef(a+i-1, true) end
     
end;

{ Swap pointer on top with second on stack. The size of the second is given. }

procedure swpstk(l: address);

var sb: packed array [1..maxsize] of ibyte;
    p:  address;
    i:  1..maxsize;

begin

   { get the top pointer }
   p := getadr(sp);
   { load up the second on stack }
   for i := 1 to l do sb[i] := store[sp+adrsize+i-1];
   putadr(sp+l, p); { place pointer at bottom }
   for i := 1 to l do begin 
     store[sp+i-1] := sb[i]; { place second as new top }
     putdef(sp+i-1, true)
   end

end;

{ end of accessor functions

(*--------------------------------------------------------------------*)

{ external routines }

procedure newspc(len: address; var blk: address); forward;
procedure valfil(fa: address); forward;

#ifdef EXTERNALS

#ifdef GNU_PASCAL
#include "externals_gnu_pascal.inc"
#endif

#ifdef ISO7185_PASCAL
#include "externals_iso7185_pascal.inc"
#endif

#ifdef PASCALINE
#include "externals_pascaline.inc"
#endif

#endif

(*--------------------------------------------------------------------*)

{ Push/pop

  These routines handle both the data type, and their lengths on the stack.

}

procedure popint(var i: integer); begin i := getint(sp); sp := sp+intsize end;
procedure pshint(i: integer); begin sp := sp-intsize; putint(sp, i) end;
procedure poprel(var r: real); begin r := getrel(sp); sp := sp+realsize end;
procedure pshrel(r: real); begin sp := sp-realsize; putrel(sp, r) end;
procedure popset(var s: settype); begin getset(sp, s); sp := sp+setsize end;
procedure pshset(s: settype); begin sp := sp-setsize; putset(sp, s) end;
procedure popadr(var a: address); begin a := getadr(sp); sp := sp+adrsize end;
procedure pshadr(a: address); begin sp := sp-adrsize; putadr(sp, a) end;

{ throw an exception by vector }
procedure errore {(ei: integer)} ;
var ad: address;
begin
  if expadr = 0 then errorm(pctop+ei); { no surrounding frame, throw system }
  mp := expmrk; sp := expstk; pc := expadr; popadr(ad); pshadr(pctop+ei); 
  ep := getadr(mp+market) { get the mark ep }
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
  if (flc mod algn) <> 0 then begin
    l := flc+1;
    flc := l - algn  +  (algn-l) mod algn
  end
end (*align*);

{ clear filename string }

procedure clrfn(var fn: filnam);
var i: 1..fillen;
begin
  for i := 1 to fillen do fn[i] := ' '
end;

(*--------------------------------------------------------------------*)

{ load code file }

procedure load;

   var  ad, ad2: address;
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
       
begin (*load*)
  ad := 0; l := 1;
  while not eof(prd) and (l > 0) do begin
    read(prd, c); if c <> ':' then errorl;
    readhex(l, 2); readhex(i, 16); ad2 := i; 
    if (ad <> ad2) and (l > 0) then errorl;
    cs := 0; 
    for i := 1 to l do 
      begin readhex(b, 2); putbyt(ad, b); cs := (cs+b) mod 256; ad := ad+1 end;
    readhex(csc, 2); if cs <> csc then errorl;
    readln(prd)
  end;
  pctop := ad
end; (*load*)

(*------------------------------------------------------------------------*)

{ runtime handlers }

procedure varenter(s, e: address);
var vp: varptr;
begin
  if varfre <> nil then begin vp := varfre; varfre := vp^.next end
  else new(vp);
  vp^.s := s; vp^.e := e; vp^.next := varlst; varlst := vp
end;

procedure varexit;
var vp: varptr;
begin
  if varlst = nil then errorv(VarListEmpty);
  vp := varlst; varlst := vp^.next; vp^.next := varfre; varfre := vp
end;

function varinc(s, e: address): boolean;
var vp: varptr; f: boolean;
begin
  vp := varlst; f := false;
  while (vp <> nil) and not f do begin
    f := (vp^.s >= s) and (vp^.e <= e);
    vp := vp^.next 
  end;
  
  varinc := f
end;

procedure withenter(b: address);
var wp: wthptr;
begin
  if wthfre <> nil then begin wp := wthfre; wthfre := wp^.next end
  else new(wp);
  wp^.b := b; wp^.next := wthlst; wthlst := wp;
  wthcnt := wthcnt+1
end;

procedure withexit;
var wp: wthptr;
begin
  if wthlst = nil then errorv(WithBaseListEmpty);
  wp := wthlst; wthlst := wp^.next; wp^.next := wthfre; wthfre := wp;
  wthcnt := wthcnt-1
end;

function withsch(b: address): boolean;
var wp: wthptr; f: boolean;
begin
  wp := wthlst; f := false;
  while (wp <> nil) and not f do begin
    f := wp^.b = b;
    wp := wp^.next
  end;
  withsch := f
end;

procedure compare(var b: boolean; var a1, a2: address);
(*comparing is only correct if result by comparing integers will be*)
var i: integer;
begin
  i := 0; b := true;
  while b and (i<q) do begin
    chkdef(a1+i); chkdef(a2+i);
    if store[a1+i] = store[a2+i] then i := i+1
    else b := false
  end;
  if i = q then i := i-1; { point at last location }
  a1 := a1+i; a2 := a2+i
end; (*compare*)

procedure valfil{(fa: address)}; { attach file to file entry }
var i,ff: integer;
begin
   if store[fa] = 0 then begin { no file }
     if fa = pctop+inputoff then ff := inputfn
     else if fa = pctop+outputoff then ff := outputfn
     else if fa = pctop+prdoff then ff := prdfn
     else if fa = pctop+prroff then ff := prrfn
     else if fa = pctop+erroroff then ff := errorfn
     else if fa = pctop+listoff then ff := listfn
     else if fa = pctop+commandoff then ff := commandfn
     else begin
       i := commandfn+1; { start search after the header files }
       ff := 0;
       while i <= maxfil do begin
         if filstate[i] = fnone then 
           begin ff := i; filstate[i] := fclosed; i := maxfil+1 end
         else i := i+1
       end;
       if ff = 0 then errore(TooManyFiles);
     end;
     store[fa] := ff; putdef(fa, true)
   end
end;

procedure valfilwm(fa: address); { validate file write mode }
begin
   valfil(fa); { validate file address }
   if filstate[store[fa]] <> fwrite then errore(FileModeIncorrect)
end;

procedure valfilrm(fa: address); { validate file read mode }
begin
   valfil(fa); { validate file address }
   if filstate[store[fa]] <> fread then errore(FileModeIncorrect)
end;

procedure getop; { get opcode }

begin

   op := store[pc]; pc := pc+1

end;

procedure getp; { get p parameter }

begin

   p := store[pc]; pc := pc+1

end;

procedure getq; { get q parameter }

begin

   q := getadr(pc); pc := pc+adrsize

end;

procedure getq1; { get q1 parameter }

begin

   q1 := getadr(pc); pc := pc+adrsize

end;

procedure getq2; { get q2 parameter }

begin

   q2 := getadr(pc); pc := pc+adrsize

end;

{

   Blocks in the heap are dead simple. The block begins with a length, including
   the length itself. If the length is positive, the block is free. If negative,
   the block is allocated. This means that AddressOfBLock+abs(lengthOfBlock) is
   address of the next block, and RequestedSize <= LengthOfBLock+adrsize is a
   reasonable test for if a free block fits the requested size, since it will
   never be true of occupied blocks.

}

{ find free block using length }

procedure fndfre(len: address; var blk: address);
var l, b: address;
begin
  b := 0; { set no block found }
  blk := gbtop; { set to bottom of heap }
  while blk < np do begin { search blocks in heap }
     l := getadr(blk); { get length }
     if (abs(l) < heapal) or (blk+abs(l) > np) then errorv(HeapFormatInvalid);
     if l >= len+adrsize then begin b := blk; blk := np end { found }
     else blk := blk+abs(l) { go next block }
  end;
  if b > 0 then begin { block was found }
     putadr(b, -l); { allocate block }
     blk := b+adrsize; { set base address }
     if l > len+adrsize+adrsize+resspc then begin
        { If there is enough room for the block, header, and another header,
          then a reserve factor if desired. }
        putadr(b, -(len+adrsize)); { allocate block }
        b := b+len+adrsize; { go to top of allocated block }
        putadr(b, l-(len+adrsize)) { set length of stub space }
     end
  end else blk := 0 { set no block found }
end;

{ coalesce space in heap }

procedure cscspc;
var done: boolean;
    ad, ad1, l, l1: address;
begin
   { first, colapse all free blocks at the heap top }
   l := 0; 
   while (l >= 0) and (np > gbtop) do begin
     { find last entry }
     ad := gbtop;
     while ad < np do begin ad1 := ad; ad := ad+abs(getadr(ad)) end;
     l := getadr(ad1); { get header length }
     if l >= 0 then np := ad1; { release to free space }
   end;
   { now, walk up and collapse adjacent free blocks }
   ad := gbtop; { index bottom }
   while ad < np do begin
      l := getadr(ad); { get header length }
      if l >= 0 then begin { free }
         ad1 := ad+l; { index next block }
         if ad1 < np then begin { not against end }
            l1 := getadr(ad1); { get length next }
            if l1 >=0 then
               putadr(ad, l+l1) { both blocks are free, combine the blocks }
            else ad := ad+l+abs(l1) { skip both blocks }
         end else ad := ad1         { skip to end, done }
      end else ad := ad+abs(l) { this block is not free, skip it }
   end
end;

{ allocate space in heap }

procedure newspc{(len: address; var blk: address)};
var ad,ad1: address;
begin
  alignu(adrsize, len); { align to units of address }
  fndfre(len, blk); { try finding an existing free block }
  if blk = 0 then begin { allocate from heap top }
     ad := np; { save base of new block }
     np := np+(len+adrsize); { find new heap top }
     ad1 := np; { save address }
     alignu(heapal, np); { align to arena }
     len := len+(np-ad1); { adjust length upwards for alignment }
     if np > sp then errore(SpaceAllocateFail);
     putadr(ad, -(len+adrsize)); { allocate block }
     blk := ad+adrsize { index start of block }
  end;
  { clear block and set undefined }
  for ad := blk to blk+len-1 do begin store[ad] := 0; putdef(ad, false) end
end;

{ dispose of space in heap }

procedure dspspc(len, blk: address);
var ad: address;
begin
   len := len; { shut up compiler check }
   if blk = 0 then errorv(DisposeOfUninitalizedPointer)
   else if blk = nilval then errorv(DisposeOfNilPointer)
   else if (blk < gbtop) or (blk >= np) then errorv(BadPointerValue);
   ad := blk-adrsize; { index header }
   if getadr(ad) >= 0 then errorv(BlockAlreadyFreed);
   if dorecycl and not dochkrpt and not donorecpar then begin 
      { obey recycling requests }
      putadr(ad, abs(getadr(ad))); { set block free }
      cscspc { coalesce free space }
   end else if dochkrpt or donorecpar then begin { perform special recycle }
      { check can break off top block }
      len := abs(getadr(ad)); { get length }
      if len >= adrsize*2 then begin
        if donorecpar then putadr(ad+adrsize, -(abs(getadr(ad))-adrsize))
        else putadr(ad+adrsize, abs(getadr(ad))-adrsize);
      end;
      { the "marker" is a block with a single address. Since it can't
        hold more than that, it will never be reused }
      putadr(ad, adrsize) { indicate freed but fixed block }
   end
end;

{ check pointer indexes free entry }

function isfree(blk: address): boolean;
begin
   isfree := getadr(blk-adrsize) = adrsize
end;

{ system routine call}

procedure callsp;
   var line: boolean;
       i, j, k, w, l, f: integer;
       c: char;
       b: boolean;
       ad,ad1,ad2: address;
       r, r1: real;
       fn: fileno;
       mn,mx: integer;
       fl1, fl2: filnam;
       rd: integer;
       lz: boolean;
       fld: boolean;

   function buffn(fn: fileno): char;
   begin 
     if fn <= commandfn then case fn of
       inputfn:   buffn := input^;
       prdfn:     buffn := prd^;
       outputfn,prrfn,errorfn,
       listfn:    errore(ReadOnWriteOnlyFile);
       commandfn: buffn := bufcommand;
     end else begin
       if filstate[fn] <> fread then errore(FileModeIncorrect);
       buffn := filtable[fn]^
     end
   end;
   
   procedure getfn(fn: fileno);
   begin 
     if fn <= commandfn then case fn of
       inputfn:   get(input);
       
       prdfn:     get(prd);
       outputfn,prrfn,errorfn,
       listfn:    errore(ReadOnWriteOnlyFile);
       commandfn: getcommand;
     end else begin
       if filstate[fn] <> fread then errore(FileModeIncorrect);
       get(filtable[fn])
     end
   end;
   
   function eoffn(fn: fileno): boolean;
   begin 
     if fn <= commandfn then case fn of
       inputfn:   eoffn := eof(input);
       outputfn:  eoffn := eof(output);
       prdfn:     eoffn := eof(prd);
       prrfn:     eoffn := eof(prr);
       errorfn:   eoffn := eof(output);
       listfn:    eoffn := eof(output);
       commandfn: eoffn := eofcommand;
     end else begin
       if filstate[fn] = fclosed then errore(FileNotOPen);
       eoffn := eof(filtable[fn])
     end
   end;
   
   function eolnfn(fn: fileno): boolean;
   begin 
     if fn <= commandfn then case fn of
       inputfn:   eolnfn := eoln(input);
       outputfn:  eolnfn := eoln(output);
       prdfn:     eolnfn := eoln(prd);
       prrfn:     eolnfn := eoln(prr);
       errorfn:   eolnfn := eoln(output);
       listfn:    eolnfn := eoln(output);
       commandfn: eolnfn := eolncommand;
     end else begin
       if filstate[fn] = fclosed then errore(FileNotOpen);
       eolnfn := eoln(filtable[fn])
     end
   end;

   procedure readi(fn: fileno; var i: integer; var w: integer; fld: boolean);
   var s: integer;
       d: integer;
   function chkbuf: char;
   begin if w > 0 then chkbuf := buffn(fn) else chkbuf := ' ' end;
   procedure getbuf; 
   begin 
     if w > 0 then begin
       if eoffn(fn) then errore(EndOfFile);
       getfn(fn); w := w-1 
     end
   end;
   function chkend: boolean;
   begin
     chkend := (w = 0) or eoffn(fn)
   end;
   begin
      s := +1; { set sign }
      { skip leading spaces }
      while (chkbuf = ' ') and not chkend do getbuf;
      if not (chkbuf in ['+', '-', '0'..'9']) then
        errore(InvalidIntegerFormat);
      if chkbuf = '+' then getbuf
      else if chkbuf = '-' then begin getbuf; s := -1 end;
      if not (chkbuf in ['0'..'9']) then errore(InvalidIntegerFormat);
      i := 0; { clear initial value }
      while (chkbuf in ['0'..'9']) do begin { parse digit }
        d := ord(chkbuf)-ord('0');
        if (i > maxint div 10) or 
           ((i = maxint div 10) and (d > maxint mod 10)) then 
          errore(IntegerValueOverFlow);
        i := i*10+d; { add in new digit }
        getbuf
      end;
      i := i*s; { place sign }
      { if fielded, validate the rest of the field is blank }
      if fld then while not chkend do begin 
        if chkbuf <> ' ' then errore(FieldNotBlank);
        getbuf
      end
   end;

   procedure readr(fn: fileno; var r: real; w: integer; fld: boolean);
   var i: integer; { integer holding }
       e: integer; { exponent }
       d: integer; { digit }
       s: boolean; { sign }
   function chkbuf: char;
   begin if w > 0 then chkbuf := buffn(fn) else chkbuf := ' ' end;
   procedure getbuf; 
   begin
     if w > 0 then begin
       if eoffn(fn) then errore(EndOfFile);
       getfn(fn); w := w-1 
     end
   end;
   function chkend: boolean;
   begin
     chkend := (w = 0) or eoffn(fn)
   end;
   { find power of ten }
   function pwrten(e: integer): real;
   var t: real; { accumulator }
       p: real; { current power }
   begin
      p := 1.0e+1; { set 1st power }
      t := 1.0; { initalize result }
      repeat
         if odd(e) then t := t*p; { if bit set, add this power }
         e := e div 2; { index next bit }
         p := sqr(p) { find next power }
      until e = 0;
      pwrten := t
   end;
   begin
      e := 0; { clear exponent }
      s := false; { set sign }
      r := 0.0; { clear result }
      { skip leading spaces }
      while (chkbuf = ' ') and not chkend do getbuf;
      { get any sign from number }
      if chkbuf = '-' then begin getbuf; s := true end
      else if chkbuf = '+' then getbuf;
      if not (chkbuf in ['0'..'9']) then errore(InvalidRealNumber);
      while (chkbuf in ['0'..'9']) do begin { parse digit }
         d := ord(chkbuf)-ord('0');
         r := r*10+d; { add in new digit }
         getbuf
      end;
      if chkbuf in ['.', 'e', 'E'] then begin { it's a real }
         if chkbuf = '.' then begin { decimal point }
            getbuf; { skip '.' }
            if not (chkbuf in ['0'..'9']) then errore(InvalidRealNumber);
            while (chkbuf in ['0'..'9']) do begin { parse digit }
               d := ord(chkbuf)-ord('0');
               r := r*10+d; { add in new digit }
               getbuf;
               e := e-1 { count off right of decimal }
            end;
         end;
         if chkbuf in ['e', 'E'] then begin { exponent }
            getbuf; { skip 'e' }
            if not (chkbuf in ['0'..'9', '+', '-']) then
               errore(InvalidRealNumber);
            readi(fn, i, w, fld); { get exponent }
            { find with exponent }
            e := e+i
         end;
         if e < 0 then r := r/pwrten(e) else r := r*pwrten(e)
      end;
      if s then r := -r;
      { if fielded, validate the rest of the field is blank }
      if fld then while not chkend do begin 
        if chkbuf <> ' ' then errore(FieldNotBlank);
        getbuf
      end
   end;

   procedure readc(fn: fileno; var c: char; w: integer; fld: boolean);
   function chkbuf: char;
   begin if w > 0 then chkbuf := buffn(fn) else chkbuf := ' ' end;
   procedure getbuf; 
   begin 
     if w > 0 then begin
       if eoffn(fn) then errore(EndOfFile);
       getfn(fn); w := w-1 
     end
   end;
   function chkend: boolean;
   begin
     chkend := (w = 0) or eoffn(fn)
   end;
   begin 
      c := chkbuf; getbuf;
      { if fielded, validate the rest of the field is blank }
      if fld then while not chkend do begin 
        if chkbuf <> ' ' then errore(FieldNotBlank);
        getbuf
      end
   end;(*readc*)
   
   procedure reads(fn: fileno; ad: address; l: integer; w: integer; 
                   fld: boolean);
   var c: char;
   function chkbuf: char;
   begin if w > 0 then chkbuf := buffn(fn) else chkbuf := ' ' end;
   procedure getbuf; 
   begin 
     if w > 0 then begin
       if eoffn(fn) then errore(EndOfFile);
       getfn(fn); w := w-1 
     end
   end;
   function chkend: boolean;
   begin
     chkend := (w = 0) or eoffn(fn)
   end;
   begin
     if w < 0 then begin w := abs(w); if w < l then l := w;
       while l > 0 do begin
         c := chkbuf; getbuf; putchr(ad, c); ad := ad+1; l := l-1
       end;
       { if fielded, validate the rest of the field is blank }
       if fld then while not chkend do begin
         if chkbuf <> ' ' then errore(FieldNotBlank);
         getbuf
       end
     end else begin if w < l then l := w;
       if fld then while w > l do begin
         if chkbuf <> ' ' then errore(FieldNotBlank);
         getbuf
       end;
       while l > 0 do begin
         c := chkbuf; getbuf; putchr(ad, c); ad := ad+1; l := l-1
       end
     end
   end;(*reads*)
   
   procedure readsp(fn: fileno; ad: address; l: integer);
   var c: char;
   begin
     while (l > 0) and not eolnfn(fn) do begin
       if eoffn(fn) then errore(EndOfFile);
       read(filtable[fn], c); putchr(ad, c); ad := ad+1; l := l-1
     end;
     while l > 0 do begin putchr(ad, ' '); ad := ad+1; l := l-1 end
   end;

   procedure writestr(var f: text; ad: address; w: integer; l: integer);
      var i: integer;
   begin (* l and w are numbers of characters *)
         if l > abs(w) then l := abs(w);
         if w >= 1 then for i := 1 to w-l do write(f,' '); 
         for i := 0 to l-1 do write(f, getchr(ad+i));
         if w < 1 then for i:=1 to abs(w)-l do write(f,' ') 
   end;(*writestr*)
   
   procedure writestrp(var f: text; ad: address; l: integer);
      var i: integer;
   begin
         ad1 := ad+l-1; { find end }
         while (l > 0) and (getchr(ad1) = ' ') do 
           begin ad1 := ad1-1; l := l-1 end;
         for i := 0 to l-1 do write(f, getchr(ad+i));
   end;
   
   procedure writec(var f: text; c: char; w: integer);
   var i: integer;
   begin
     if w < 1 then begin 
       if abs(w) > 0 then write(f, c); 
       for i := 1 to abs(w)-1 do write(f,' ') 
     end else write(f, c:w)
   end;

   procedure getfile(var f: text);
   begin if eof(f) then errore(EndOfFile);
         get(f);
   end;(*getfile*)

   procedure putfile(var f: text; var ad: address; fn: fileno);
   begin if not filbuff[fn] then errore(FileBufferVariableUndefined);
         f^:= getchr(ad+fileidsize); put(f);
         filbuff[fn] := false
   end;(*putfile*)
   
   procedure writei(var f: text; w: integer; fl: integer; r: integer; 
                    lz: boolean);
   var digit: array [1..maxdbf] of char; i: integer;
       sgn: boolean; d, ds: integer;
   procedure filllz(n: integer); 
   begin while n > 0 do begin write(f, '0'); n := n-1 end 
   end;
   begin
     if w < 0 then begin 
       sgn := true; w := abs(w);
       if r <> 10 then errore(NondecimalRadixOfNegative) 
     end else sgn := false;
     for i := 1 to maxdbf do digit[i] := ' ';
     i := maxdbf; d := 0;
     repeat 
       if (w mod r) < 10 then digit[i] := chr(w mod r+ord('0'))
       else digit[i] := chr(w mod r -10 +ord('a'));
       w := w div r; i := i-1; d := d+1
     until w = 0;   
     if sgn then ds := d+1 else ds := d; { add sign }
     if ds > abs(fl) then if fl < 0 then fl := -ds else fl := ds;
     if (fl > 0) and (fl > ds) then 
       if lz then filllz(fl-ds) else write(f, ' ':fl-ds);
     if sgn then write(f, '-');
     for i := maxdbf-d+1 to maxdbf do write(f, digit[i]);
     if (fl < 1) and (abs(fl) > ds) then write(f, ' ':abs(fl)-ds)
   end;
   
   procedure writerf(var f: text; r: real; fl: integer; fr: integer);
   var c: integer; p: real;
 
   begin
     if fl < 1 then begin
       c := 2; if r < 0 then c := c+1;
       p := 10.0;
       while p <= abs(r) do begin c := c+1; p := p*10 end;
       write(f, r:c:fr);
       if abs(fl) > c then write(f, ' ':abs(fl)-(c+fr))
     end else write(f, r:fl:fr)
   end;

begin (*callsp*)
      if q > maxsp then errorv(InvalidStandardProcedureOrFunction);

      case q of
           0 (*get*): begin popadr(ad); valfil(ad); fn := store[ad];
                        if varinc(ad+fileidsize, ad+fileidsize) then
                          errorv(VarReferencedFileBufferModified); 
                        getfn(fn)
                      end;
           1 (*put*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of                              
                              outputfn: putfile(output, ad, fn); 
                              prrfn: putfile(prr, ad, fn);
                              errorfn: putfile(output, ad, fn); 
                              listfn: putfile(output, ad, fn); 
                              inputfn,prdfn,
                              commandfn: errore(WriteOnReadOnlyFile)
                           end else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                putfile(filtable[fn], ad, fn)
                           end
                      end;
           3 (*rln*): begin popadr(ad); pshadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                              inputfn: begin
                                 if eof(input) then errore(EndOfFile);
                                 readln(input)
                              end;
                              prdfn: begin
                                 if eof(prd) then errore(EndOfFile);
                                 readln(prd)
                              end;
                              outputfn,prrfn,errorfn,
                              listfn: errore(ReadOnWriteOnlyFile);
                              commandfn: readlncommand
                           end else begin
                                if filstate[fn] <> fread then
                                   errore(FileModeIncorrect);
                                if eof(filtable[fn]) then errore(EndOfFile);
                                readln(filtable[fn])
                           end
                      end;
           4 (*new*): begin popadr(ad1); newspc(ad1, ad);
                      (*top of stack gives the length in units of storage *)
                            popadr(ad1); putadr(ad1, ad)
                      end;
           39 (*nwl*): begin popadr(ad1); popint(i);
                            l := 0; if (i = 0) and donorecpar then l := 1;
                            newspc(ad1+(i+l+1)*intsize, ad); 
                            ad1 := ad+(i+l)*intsize; putint(ad1, i+adrsize+1);
                            k := i;
                            while k > 0 do
                              begin ad1 := ad1-intsize; popint(j);
                                    putint(ad1, j); k := k-1
                              end;
                            popadr(ad1); putadr(ad1, ad+(i+l+1)*intsize)
                      end;
           5 (*wln*): begin popadr(ad); pshadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                              outputfn: writeln(output); 
                              prrfn: writeln(prr);
                              errorfn: writeln(output);
                              listfn: writeln(output); 
                              prdfn,inputfn,
                              commandfn: errore(WriteOnReadOnlyFile)
                           end else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                writeln(filtable[fn])
                           end
                      end;
           6 (*wrs*): begin popint(w); popadr(ad1); popint(l); 
                           popadr(ad); pshadr(ad); valfil(ad); fn := store[ad];
                           if (w < 1) and iso7185 then 
                             errore(InvalidFieldSpecification);
                           if fn <= commandfn then case fn of
                              outputfn: writestr(output, ad1, w, l); 
                              prrfn: writestr(prr, ad1, w, l);
                              errorfn: writestr(output, ad1, w, l);
                              listfn: writestr(output, ad1, w, l); 
                              prdfn,inputfn,
                              commandfn: errore(WriteOnReadOnlyFile);
                           end else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                writestr(filtable[fn], ad1, w, l)
                           end;
                      end;
           65 (*wrsp*): begin popadr(ad1); popint(l);
                           popadr(ad); pshadr(ad); valfil(ad); fn := store[ad];
                           if (w < 1) and iso7185 then 
                             errore(InvalidFieldSpecification);
                           if fn <= commandfn then case fn of
                             outputfn: writestrp(output, ad1, l); 
                             prrfn: writestrp(prr, ad1, l);
                             errorfn: writestrp(output, ad1, l); 
                             listfn: writestrp(output, ad1, l); 
                             prdfn,inputfn,
                             commandfn: errore(WriteOnReadOnlyFile);
                           end else begin
                             if filstate[fn] <> fwrite then
                               errore(FileModeIncorrect);
                             writestrp(filtable[fn], ad1, l)
                           end;
                      end;
          41 (*eof*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                        pshint(ord(eoffn(fn)))
                      end;
          42 (*efb*): begin
                        popadr(ad); valfilrm(ad); fn := store[ad];
                        { eof is file eof, and buffer not full }
                        pshint(ord(eof(bfiltable[fn]) and not filbuff[fn]))
                      end;
           7 (*eln*): begin popadr(ad); valfil(ad); fn := store[ad];
                        pshint(ord(eolnfn(fn)))
                      end;
           8 (*wri*),
           62 (*wrih*),
           63 (*wrio*),
           64 (*wrib*),
           66 (*wiz*),
           67 (*wizh*),
           68 (*wizo*),
           69 (*wizb*): begin popint(w); popint(i); popadr(ad); pshadr(ad);
                            rd := 10;
                            if (q = 62) or (q = 67) then rd := 16
                            else if (q = 63) or (q = 68) then rd := 8
                            else if (q = 64) or (q = 69) then rd := 2;
                            lz := (q >= 66) and (q <= 69);
                            valfil(ad); fn := store[ad];
                            if (w < 1) and iso7185 then 
                              errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: writei(output, i, w, rd, lz); 
                                 prrfn: writei(prr, i, w, rd, lz);
                                 errorfn: writei(output, i, w, rd, lz); 
                                 listfn: writei(output, i, w, rd, lz); 
                                 prdfn,inputfn,
                                 commandfn: errore(WriteOnReadOnlyFile)
                              end
                            else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                writei(filtable[fn], i, w, rd, lz)
                            end
                      end;
           9 (*wrr*): begin popint(w); poprel(r); popadr(ad); pshadr(ad);
                            valfil(ad); fn := store[ad];
                            if w < 1 then errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: write(output, r: w); 
                                 prrfn: write(prr, r:w);
                                 errorfn: write(output, r: w); 
                                 listfn: write(output, r: w); 
                                 prdfn,inputfn,
                                 commandfn: errore(WriteOnReadOnlyFile)
                              end
                            else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                write(filtable[fn], r:w)
                            end;
                      end;
           10(*wrc*): begin popint(w); popint(i); c := chr(i); popadr(ad);
                            pshadr(ad); valfil(ad); fn := store[ad];
                            if (w < 1) and iso7185 then 
                              errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: writec(output, c, w); 
                                 prrfn: writec(prr, c, w);
                                 errorfn: writec(output, c, w); 
                                 listfn: writec(output, c, w); 
                                 prdfn,inputfn,
                                 commandfn: errore(WriteOnReadOnlyfile)
                              end
                            else begin
                                if filstate[fn] <> fwrite then
                                   errore(FIleModeIncorrect);
                                writec(filtable[fn], c, w)
                            end
                      end;
           11(*rdi*),
           72(*rdif*): begin w := maxint; fld := q = 72; if fld then popint(w);
                           popadr(ad1); popadr(ad); pshadr(ad); 
                           valfil(ad); fn := store[ad]; readi(fn, i, w, fld);
                           putint(ad1, i);
                      end;
           37(*rib*),
           71(*ribf*): begin w := maxint; fld := q = 71; popint(mx); popint(mn); 
                           if fld then popint(w); popadr(ad1); popadr(ad);
                           pshadr(ad); valfil(ad); fn := store[ad];
                           readi(fn, i, w, fld); 
                           if (i < mn) or (i > mx) then 
                             errore(ValueOutOfRange);
                           putint(ad1, i);
                      end;
           12(*rdr*),
           73(*rdrf*): begin w := maxint; fld := q = 73; if fld then popint(w);
                           popadr(ad1); popadr(ad); pshadr(ad); 
                           valfil(ad); fn := store[ad];
                           readr(fn, r, w, fld); putrel(ad1, r)
                      end;
           13(*rdc*),
           75(*rdcf*): begin w := maxint; fld := q = 75; if fld then popint(w);
                           popadr(ad1); popadr(ad); pshadr(ad); 
                           valfil(ad); fn := store[ad];
                           readc(fn, c, w, fld); putchr(ad1, c)
                      end;
           38(*rcb*),
           74(*rcbf*): begin w := maxint; fld := q = 74; popint(mx); popint(mn); 
                            if fld then popint(w); popadr(ad1); popadr(ad);
                            pshadr(ad); valfil(ad);
                            fn := store[ad];
                            readc(fn, c, w, fld);
                            if (ord(c) < mn) or (ord(c) > mx) then
                              errore(ValueOutOfRange);
                            putchr(ad1, c)
                      end;
           14(*sin*): begin poprel(r1); pshrel(sin(r1)) end;
           15(*cos*): begin poprel(r1); pshrel(cos(r1)) end;
           16(*exp*): begin poprel(r1); pshrel(exp(r1)) end;
           17(*log*): begin poprel(r1);
                            if r1 <= 0 then errore(InvalidArgumentToLn);
                            pshrel(ln(r1)) end;
           18(*sqt*): begin poprel(r1);
                            if r1 < 0 then errore(InvalidArgumentToSqrt);
                            pshrel(sqrt(r1)) end;
           19(*atn*): begin poprel(r1); pshrel(arctan(r1)) end;
           { placeholder for "mark" }
           20(*sav*): errorv(InvalidStandardProcedureOrFunction);
           21(*pag*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                                outputfn: page(output); 
                                prrfn: page(prr);
                                errorfn: page(output); 
                                listfn: page(output); 
                                prdfn,inputfn,
                                commandfn: errore(WriteOnReadOnlyFile)
                              end
                           else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                page(filtable[fn])
                           end
                      end;
           22(*rsf*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                                prdfn: reset(prd);
                                prrfn: errore(CannotResetWriteOnlyFile);
                                outputfn,errorfn,listfn,inputfn,
                                commandfn: 
                                  errore(CannotResetOrRewriteStandardFile)
                              end
                           else begin
                                if (filstate[fn] = fclosed) and 
                                   not filanamtab[fn] then
                                  errore(CannotResetClosedTempFile);
                                filstate[fn] := fread;
                                reset(filtable[fn]);
                                filbuff[fn] := false
                           end
                      end;
           23(*rwf*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                                prrfn: rewrite(prr);
                                prdfn: errore(CannotRewriteReadOnlyFile);
                                errorfn,listfn,outputfn,inputfn,
                                commandfn: 
                                  errore(CannotResetOrRewriteStandardFile)
                              end
                           else begin
                                filstate[fn] := fwrite;
                                rewrite(filtable[fn])
                           end
                      end;
           24(*wrb*): begin popint(w); popint(i); b := i <> 0; popadr(ad);
                            pshadr(ad); valfil(ad); fn := store[ad];
                            if w < 1 then errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: write(output, b:w); 
                                 prrfn: write(prr, b:w);
                                 errorfn: write(output, b:w); 
                                 listfn: write(output, b:w); 
                                 prdfn,inputfn,
                                 commandfn: errore(WriteOnReadOnlyFile)
                              end
                            else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                write(filtable[fn], b:w)
                            end
                      end;
           25(*wrf*): begin popint(f); popint(w); poprel(r); popadr(ad); pshadr(ad);
                            valfil(ad); fn := store[ad];
                            if (w < 1) and iso7185 then 
                              errore(InvalidFieldSpecification);
                            if f < 1 then errore(InvalidFractionSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: writerf(output, r, w, f); 
                                 prrfn: writerf(prr, r, w, f);
                                 errorfn: writerf(output, r, w, f); 
                                 listfn: writerf(output, r, w, f); 
                                 prdfn,
                                 inputfn,
                                 commandfn: errore(WriteOnReadOnlyFile)
                              end
                            else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                writerf(filtable[fn], r, w, f)
                            end
                      end;
           26(*dsp*): begin
                           popadr(ad1); popadr(ad); 
                           if varinc(ad, ad+ad1-1) then 
                             errorv(DisposeOfVarReferencedBlock);
                           if withsch(ad) then
                             errorv(DisposeOfWithReferencedBlock);
                           dspspc(ad1, ad)
                      end;
           40(*dsl*): begin
                           popadr(ad1); popint(i);
                           l := 0; if (i = 0) and donorecpar then l := 1; 
                           ad := getadr(sp+i*intsize); { get rec addr }
                           if getint(ad-intsize) <= adrsize then
                             errorv(BlockAlreadyFreed);
                           if i <> getint(ad-intsize)-adrsize-1 then
                             errorv(NewDisposeTagsMismatch);
                           ad := ad-intsize*2; ad2 := sp;
                           k := i;
                           while k > 0 do
                             begin
                               if getint(ad) <> getint(ad2) then
                                 errorv(NewDisposeTagsMismatch);
                               ad := ad-intsize; ad2 := ad2+intsize; k := k-1
                             end;
                           ad := ad+intsize; ad1 := ad1+(i+1)*intsize;
                           ad := ad-(l*intsize); ad1 := ad1+(l*intsize);
                           if varinc(ad, ad+ad1-1) then
                             errorv(DisposeOfVarReferencedBlock);
                           if withsch(ad) then
                             errorv(DisposeOfWithReferencedBlock);
                           dspspc(ad1, ad);
                           while i > 0 do begin popint(j); i := i-1 end;
                           popadr(ad);
                           if donorecpar then 
                             putadr(ad-adrsize, adrsize)
                      end;
           27(*wbf*): begin popint(l); popadr(ad1); popadr(ad); pshadr(ad);
                           valfilwm(ad); fn := store[ad];
                           for i := 1 to l do begin
                              chkdef(ad1); write(bfiltable[fn], store[ad1]);
                              ad1 := ad1+1
                           end
                      end;
           28(*wbi*): begin popint(i); popadr(ad); pshadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to intsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            popint(i)
                      end;
           45(*wbx*): begin popint(i); popadr(ad); pshadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            write(bfiltable[fn], store[sp]);
                            popint(i)
                      end;
           29(*wbr*): begin poprel(r); popadr(ad); pshadr(ad); pshrel(r);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to realsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            poprel(r)
                      end;
           30(*wbc*): begin popint(i); c := chr(i); popadr(ad); pshadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to charsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            popint(i)
                      end;
           31(*wbb*): begin popint(i); popadr(ad); pshadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to boolsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            popint(i)
                      end;
           32(*rbf*): begin popint(l); popadr(ad1); popadr(ad); pshadr(ad);
                            valfilrm(ad); fn := store[ad];
                            if filbuff[fn] then
                            for i := 1 to l do begin
                              store[ad1+i-1] := store[ad+fileidsize+i-1]; 
                              putdef(ad1+i-1, true)
                            end else
                              for i := 1 to l do begin
                                if eof(bfiltable[fn]) then
                                  errore(EndOfFile);
                                read(bfiltable[fn], store[ad1]);
                                putdef(ad1, true);
                                ad1 := ad1+1
                              end
                      end;
           33(*rsb*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if (filstate[fn] = fclosed) and 
                              not filanamtab[fn] then
                             errore(CannotResetClosedTempFile);
                           filstate[fn] := fread;
                           reset(bfiltable[fn]);
                           filbuff[fn] := false
                      end;
           34(*rwb*): begin popadr(ad); valfil(ad); fn := store[ad];
                           filstate[fn] := fwrite;
                           rewrite(bfiltable[fn]);
                           filbuff[fn] := false
                      end;
           35(*gbf*): begin popint(i); popadr(ad); valfilrm(ad);
                           fn := store[ad];
                           if varinc(ad+fileidsize, ad+fileidsize+i-1) then
                             errorv(VarReferencedFileBufferModified);
                           if filbuff[fn] then filbuff[fn] := false
                           else
                             for j := 1 to i do
                                read(bfiltable[fn], store[ad+fileidsize+j-1])
                      end;
           36(*pbf*): begin popint(i); popadr(ad); valfilwm(ad);
                        fn := store[ad];
                        if not filbuff[fn] then
                          errore(FileBufferVariableUndefined);
                        for j := 1 to i do
                          write(bfiltable[fn], store[ad+fileidsize+j-1]);
                        filbuff[fn] := false;
                      end;
           43 (*fbv*): begin popadr(ad); pshadr(ad); valfil(ad);
                          fn := store[ad];
                          if fn = inputfn then putchr(ad+fileidsize, input^)
                          else if fn = prdfn then putchr(ad+fileidsize, prd^)
                          else if fn = commandfn then 
                            putchr(ad+fileidsize, bufcommand)
                          else begin
                            if filstate[fn] = fread then
                            putchr(ad+fileidsize, filtable[fn]^)
                          end;
                          filbuff[fn] := true
                        end;
           44 (*fvb*): begin popint(i); popadr(ad); pshadr(ad); valfil(ad);
                          fn := store[ad];
                          { load buffer only if in read mode, and buffer is
                            empty }
                          if (filstate[fn] = fread) and not filbuff[fn] then
                            begin
                              for j := 1 to i do begin
                                read(bfiltable[fn], store[ad+fileidsize+j-1]);
                                putdef(ad+fileidsize+j-1, true)
                              end
                            end;
                          filbuff[fn] := true
                        end;
           { extended Pascaline file handlers }
           46 (*asst*): begin popint(i); popadr(ad1); popadr(ad); valfil(ad); 
                         fn := store[ad]; clrfn(fl1);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         assigntext(filtable[fn], fl1); 
                         filanamtab[fn] := true
                       end;
           56 (*assb*): begin popint(i); popadr(ad1); popadr(ad); valfil(ad); 
                         fn := store[ad]; clrfn(fl1); 
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]); 
                         assignbin(bfiltable[fn], fl1); 
                         filanamtab[fn] := true
                       end;
           47 (*clst*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         closetext(filtable[fn]); filstate[fn] := fclosed
                       end;
           57 (*clsb*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         closebin(bfiltable[fn]); filstate[fn] := fclosed
                       end;
           48 (*pos*): begin popint(i); popadr(ad); valfil(ad); fn := store[ad];
                         if i < 1 then errore(InvalidFilePosition); 
                         positionbin(bfiltable[fn], i)
                       end;
           49 (*upd*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         updatebin(bfiltable[fn]); filstate[fn] := fwrite;
                         filbuff[fn] := false
                       end;
           50 (*appt*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         appendtext(filtable[fn]); filstate[fn] := fwrite;
                         filbuff[fn] := false
                       end;
           58 (*appb*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         appendbin(bfiltable[fn]); filstate[fn] := fwrite;
                         filbuff[fn] := false
                       end;
           51 (*del*): begin popint(i); popadr(ad1); clrfn(fl1); 
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]); 
                         deletefile(fl1) 
                       end;
           52 (*chg*): begin popint(i); popadr(ad1); popint(l); popadr(ad); 
                         clrfn(fl1); clrfn(fl2);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         for j := 1 to l do fl2[j] := chr(store[ad+j-1]); 
                         changefile(fl2, fl1) 
                       end;
           53 (*len*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         pshint(lengthbin(bfiltable[fn]))
                       end;
           54 (*loc*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         pshint(locationbin(bfiltable[fn]))
                       end;
           55 (*exs*): begin popint(i); popadr(ad1); clrfn(fl1); 
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]); 
                         pshint(ord(existsfile(fl1))) 
                       end;  
           59 (*hlt*): goto 99;
           60 (*ast*): begin popint(i); 
                         if i = 0 then errorv(ProgramCodeAssertion);
                       end;
           61 (*asts*): begin popint(i); popadr(ad); popint(j);
                         if j = 0 then errors(ad, i);
                       end;
           70(*rds*),
           76(*rdsf*): begin w := maxint; fld := q = 76; popint(i); 
                         if fld then popint(w); popadr(ad1); popadr(ad); 
                         pshadr(ad); valfil(ad); fn := store[ad];
                         reads(fn, ad1, i, w, fld);
                       end;
           77(*rdsp*): begin popint(i); popadr(ad1); popadr(ad); pshadr(ad);
                         valfil(ad); fn := store[ad];
                         readsp(fn, ad1, i)
                       end;
           78(*aeft*): begin popint(i); popadr(ad1); popadr(ad); valfil(ad); 
                         fn := store[ad]; clrfn(fl1);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         assignexternaltext(filtable[fn], fl1);
                         filanamtab[fn] := true
                       end;
           79(*aefb*): begin popint(i); popadr(ad1); popadr(ad); valfil(ad); 
                         fn := store[ad]; clrfn(fl1);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         assignexternalbin(bfiltable[fn], fl1);
                         filanamtab[fn] := true
                       end;
           80(*rdie*): begin w := maxint; popint(i); popadr(ad1); popadr(ad); 
                         readi(commandfn, i, w, false); putint(ad, i);
                       end;
           81(*rdre*): begin w := maxint; popint(i); popadr(ad1); popadr(ad); 
                         readr(commandfn, r, w, false); putrel(ad, r);
                       end;
           2(*thw*):   begin popadr(ad1); mp := expmrk; sp := expstk; 
                         pc := expadr; popadr(ad2); pshadr(ad1); 
                         ep := getadr(mp+market) { get the mark ep }
                         { release to search vectors }
                       end;
                       
      end;(*case q*)
end;(*callsp*)

procedure sinins;
var ad,ad1,ad2,ad3,ad4: address; b: boolean; i,j,k,i1,i2 : integer; c, c1: char;
    i3,i4: integer; r1,r2: real; b1,b2: boolean; s1,s2: settype; 
    a1,a2,a3: address;
begin
  if pc >= pctop then errorv(PCOutOfRange);
  
  { uncomment for instruction trace }
  {
  wrtnum(output, pc, 16, maxdigh, true);
  write('/');
  wrtnum(output, sp, 16, maxdigh, true);
  write(' ');
  wrtnum(output, getbyt(pc), 16, 2, true);
  writeln;
  }

  { fetch instruction from byte store }
  getop;
  
  (*execute*)

  case op of

    0   (*lodi*): begin getp; getq; pshint(getint(getadr(mp-p*ptrsize)+q)) end;
    193 (*lodx*): begin getp; getq; pshint(getbyt(getadr(mp-p*ptrsize)+q)) end;
    105 (*loda*): begin getp; getq; pshadr(getadr(getadr(mp-p*ptrsize)+q)) end;
    106 (*lodr*): begin getp; getq; pshrel(getrel(getadr(mp-p*ptrsize)+ q)) end;
    107 (*lods*): begin getp; getq; getset(getadr(mp-p*ptrsize)+q, s1); pshset(s1) end;
    108 (*lodb*): begin getp; getq; pshint(ord(getbol(getadr(mp-p*ptrsize)+q))) end;
    109 (*lodc*): begin getp; getq; pshint(ord(getchr(getadr(mp-p*ptrsize)+q))) end;

    1   (*ldoi*): begin getq; pshint(getint(q)) end;
    194 (*ldox*): begin getq; pshint(getbyt(q)) end;
    65  (*ldoa*): begin getq; pshadr(getadr(q)) end;
    66  (*ldor*): begin getq; pshrel(getrel(q)) end;
    67  (*ldos*): begin getq; getset(q, s1); pshset(s1) end;
    68  (*ldob*): begin getq; pshint(ord(getbol(q))) end;
    69  (*ldoc*): begin getq; pshint(ord(getchr(q))) end;

    2   (*stri*): begin getp; getq; popint(i); putint(getadr(mp-p*ptrsize)+q, i) end;
    195 (*strx*): begin getp; getq; popint(i); putbyt(getadr(mp-p*ptrsize)+q, i) end;
    70  (*stra*): begin getp; getq; popadr(ad); putadr(getadr(mp-p*ptrsize)+q, ad) end;
    71  (*strr*): begin getp; getq; poprel(r1); putrel(getadr(mp-p*ptrsize)+q, r1) end;
    72  (*strs*): begin getp; getq; popset(s1); putset(getadr(mp-p*ptrsize)+q, s1) end;
    73  (*strb*): begin getp; getq; popint(i1); b1 := i1 <> 0; 
                        putbol(getadr(mp-p*ptrsize)+q, b1) end;
    74  (*strc*): begin getp; getq; popint(i1); c1 := chr(i1); 
                        putchr(getadr(mp-p*ptrsize)+q, c1) end;

    3   (*sroi*): begin getq; popint(i); putint(q, i) end;
    196 (*srox*): begin getq; popint(i); putbyt(q, i) end;
    75  (*sroa*): begin getq; popadr(ad); putadr(q, ad) end;
    76  (*sror*): begin getq; poprel(r1); putrel(q, r1) end;
    77  (*sros*): begin getq; popset(s1); putset(q, s1) end;
    78  (*srob*): begin getq; popint(i1); b1 := i1 <> 0; putbol(q, b1) end;
    79  (*sroc*): begin getq; popint(i1); c1 := chr(i1); putchr(q, c1) end;

    4 (*lda*): begin getp; getq; pshadr(getadr(mp-p*ptrsize)+q) end;
    5 (*lao*): begin getq; pshadr(q) end;

    6   (*stoi*): begin popint(i); popadr(ad); putint(ad, i) end;
    197 (*stox*): begin popint(i); popadr(ad); putbyt(ad, i) end;
    80  (*stoa*): begin popadr(ad1); popadr(ad); putadr(ad, ad1) end;
    81  (*stor*): begin poprel(r1); popadr(ad); putrel(ad, r1) end;
    82  (*stos*): begin popset(s1); popadr(ad); putset(ad, s1) end;
    83  (*stob*): begin popint(i1); b1 := i1 <> 0; popadr(ad); putbol(ad, b1) 
                  end;
    84  (*stoc*): begin popint(i1); c1 := chr(i1); popadr(ad); putchr(ad, c1) 
                  end;

    235 (*stom*): begin getq; getq1; ad1 := getadr(sp+q1); ad2 := sp;
                    for i := 0 to q-1 do begin
                      store[ad1+i] := store[ad2+i]; putdef(ad1+i, getdef(ad2+i))
                    end;
                    sp := sp+q1+adrsize
                  end;
    238 (*ctb*): begin getq; getq1; popadr(ad1); ad2 := sp;
                    for i := 0 to q-1 do begin
                      store[ad1+i] := store[ad2+i]; putdef(ad1+i, getdef(ad2+i))
                    end;
                    sp := sp+q1; pshadr(ad1)
                  end;

    127 (*ldcc*): begin pshint(ord(getchr(pc))); pc := pc+1 end;
    126 (*ldcb*): begin pshint(ord(getbol(pc))); pc := pc+1 end;
    123 (*ldci*): begin i := getint(pc); pc := pc+intsize; pshint(i) end;
    125 (*ldcn*): pshadr(nilval) (* load nil *) ;
    124 (*ldcr*): begin getq; pshrel(getrel(q)) end;
    7   (*ldcs*): begin getq; getset(q, s1); pshset(s1) end;

    9   (*indi*): begin getq; popadr(ad); pshint(getint(ad+q)) end;
    198 (*indx*): begin getq; popadr(ad); pshint(getbyt(ad+q)) end;
    85  (*inda*): begin getq; popadr(ad); ad1 := getadr(ad+q); 
                        pshadr(ad1) end;
    86  (*indr*): begin getq; popadr(ad); pshrel(getrel(ad+q)) end;
    87  (*inds*): begin getq; popadr(ad); getset(ad+q, s1); pshset(s1) end;
    88  (*indb*): begin getq; popadr(ad); pshint(ord(getbol(ad+q))) end;
    89  (*indc*): begin getq; popadr(ad); pshint(ord(getchr(ad+q))) end;

    93 (*incb*),
    94 (*incc*),
    201 (*incx*),
    10 (*inci*): begin getq; popint(i1);
                   if dochkovf then if (i1<0) = (q<0) then
                      if maxint-abs(i1) < abs(q) then
                        errore(IntegerValueOverflow);
                   pshint(i1+q)
                   end;
    90 (*inca*): begin getq; popadr(a1); pshadr(a1+q) end;

    245 (*sfr*): begin getq;
                   { allocate function result as zeros }
                   for j := 1 to q div intsize do pshint(0);
                   { set function result undefined }
                   for j := 1 to q do putdef(sp+j-1, false)
                 end;
    11 (*mst*): begin getp; getq; getq1;
                  pshadr(0); { place current ep }
                  pshadr(0); { place bottom of stack }
                  pshadr(ep); { previous ep }
                  pshadr(mp); { save old mp on stack }
                  ad1 := mp; { save old mp }
                  mp := sp; { set new mp }
                  { copy old display to stack }
                  for i := 1 to p do begin ad1 := ad1-ptrsize; pshadr(getadr(ad1)) end;
                  pshadr(mp); { push new mp to complete display } 
                  ad := mp+q; (*q = length of dataseg*)
                  if ad <= np then errorv(StoreOverflow);
                  { clear allocated memory and set undefined }
                  while sp > ad do 
                    begin sp := sp-1; store[sp] := 0; putdef(sp, false) end;
                  putadr(mp+marksb, sp); { set bottom of stack }
                  ep := sp+q1; if ep <= np then errorv(StoreOverFlow);
                  putadr(mp+market, ep) { place current ep }
                end;

    12 (*cup*),
    246 (*cup*): begin (*q=entry point*)
                 getq; pshadr(pc); pc := q
                end;

    27 (*cuv*): begin (*q=vector entry point*)
                 getq; pshadr(pc); pc := getadr(q)
                end;

    91 (*suv*): begin getq; getq1; putadr(q1, q) end;

    { For characters and booleans, need to clean 8 bit results because
      only the lower 8 bits were stored to. }
    130 (*retc*): begin getq;
                   ep := getadr(mp+markep);
                   sp := mp; { index old mark }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q; { remove parameters }
                   { clean result }
                   putint(sp, ord(getchr(sp)))
                 end;
    131 (*retb*): begin getq;
                   ep := getadr(mp+markep);
                   sp := mp; { index old mark }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q; { remove parameters }
                   { clean result }
                   putint(sp, ord(getbol(sp)))
                 end;
    14  (*retp*),
    128 (*reti*),
    204 (*retx*),
    236 (*rets*),
    129 (*retr*),
    132 (*reta*): begin getq;
                   ep := getadr(mp+markep);
                   sp := mp; { index old mark }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q { remove parameters }
                 end;

    237 (*retm*): begin getq;
                   ep := getadr(mp+markep);
                   sp := mp; { index old mark }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q { remove parameters }
                 end;
               
    15 (*csp*): begin q := store[pc]; pc := pc+1; callsp end;

    16 (*ixa*): begin getq; popint(i); popadr(a1); pshadr(q*i+a1) end;

    17  { equa }: begin popadr(a2); popadr(a1); pshint(ord(a1=a2)) end;
    139 { equb },
    141 { equc },
    137 { equi }: begin popint(i2); popint(i1); pshint(ord(i1=i2)) end;
    138 { equr }: begin poprel(r2); poprel(r1); pshint(ord(r1=r2)) end;
    140 { equs }: begin popset(s2); popset(s1); pshint(ord(s1=s2)) end;
    142 { equm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2); 
                        pshint(ord(b)) end;
    215 (*equv*): begin popadr(a2); popint(i); q := i; popadr(a1); popint(i1);
                        compare(b, a1, a2); pshint(ord(b)) end;

    18  { neqa }: begin popadr(a2); popadr(a1); pshint(ord(a1<>a2)) end;
    145 { neqb },
    147 { neqc },
    143 { neqi }: begin popint(i2); popint(i1); pshint(ord(i1<>i2)) end;
    144 { neqr }: begin poprel(r2); poprel(r1); pshint(ord(r1<>r2)) end;
    146 { neqs }: begin popset(s2); popset(s1); pshint(ord(s1<>s2)) end;
    148 { neqm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b)) end;
    216 (*neqv*): begin popadr(a2); popint(i); q := i; popadr(a1); popint(i1);
                        compare(b, a1, a2); pshint(ord(not b)) end;

    151 { geqb },
    153 { geqc },
    149 { geqi }: begin popint(i2); popint(i1); pshint(ord(i1>=i2)) end;
    150 { geqr }: begin poprel(r2); poprel(r1); pshint(ord(r1>=r2)) end;
    152 { geqs }: begin popset(s2); popset(s1); pshint(ord(s1>=s2)) end;
    154 { geqm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(b or (store[a1] >= store[a2])))
                  end;
    220 (*geqv*): begin popadr(a2); popint(i); q := i; popadr(a1); popint(i1);
                        compare(b, a1, a2);
                        pshint(ord(b or (store[a1] >= store[a2]))) end;

    157 { grtb },
    159 { grtc },
    155 { grti }: begin popint(i2); popint(i1); pshint(ord(i1>i2)) end;
    156 { grtr }: begin poprel(r2); poprel(r1); pshint(ord(r1>r2)) end;
    158 { grts }: errorv(SetInclusion);
    160 { grtm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] > store[a2])))
                  end;
    218 (*grtv*): begin popadr(a2); popint(i); q := i; popadr(a1); popint(i1);
                        compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] > store[a2]))) end;

    163 { leqb },
    165 { leqc },
    161 { leqi }: begin popint(i2); popint(i1); pshint(ord(i1<=i2)) end;
    162 { leqr }: begin poprel(r2); poprel(r1); pshint(ord(r1<=r2)) end;
    164 { leqs }: begin popset(s2); popset(s1); pshint(ord(s1<=s2)) end;
    166 { leqm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(b or (store[a1] <= store[a2])))
                  end;
    219 (*leqv*): begin popadr(a2); popint(i); q := i; popadr(a1); popint(i1);
                        compare(b, a1, a2);
                        pshint(ord(b or (store[a1] <= store[a2]))) end;

    169 { lesb },
    171 { lesc },
    167 { lesi }: begin popint(i2); popint(i1); pshint(ord(i1<i2)) end;
    168 { lesr }: begin poprel(r2); poprel(r1); pshint(ord(r1<r2)) end;
    170 { less }: errorv(SetInclusion);
    172 { lesm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] < store[a2])))
                  end;
    217 (*lesv*): begin popadr(a2); popint(i); q := i; popadr(a1); popint(i1);
                        compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] < store[a2]))) end;

    23 (*ujp*): begin getq; pc := q end;
    24 (*fjp*): begin getq; popint(i); if i = 0 then pc := q end;
    25 (*xjp*): begin getq; popint(i1); pc := i1*ujplen+q end;

    95 (*chka*),
    190 (*ckla*): begin getq; popadr(a1); pshadr(a1);
                       {     0 = assign pointer including nil
                         Not 0 = assign pointer from heap address }
                       if a1 = 0 then
                          { if zero, but not nil, it's never been assigned }
                          errorv(UninitializedPointer)
                       else if (q <> 0) and (a1 = nilval) then
                          { q <> 0 means deref, and it was nil
                            (which is not zero) }
                          errorv(DereferenceOfNilPointer)
                       else if ((a1 < gbtop) or (a1 >= np)) and
                               (a1 <> nilval) then
                          { outside heap space (which could have
                            contracted!) }
                          errorv(BadPointerValue)
                       else if (dochkrpt or donorecpar) and 
                               (a1 <> nilval) then begin
                         { perform use of freed space check }
                         if isfree(a1) then
                           { attempt to dereference or assign a freed
                             block }
                           errorv(PointerUsedAfterDispose)
                       end
                 end;
    97 (*chks*): begin getq; popset(s1); pshset(s1);
                   for j := setlow to getint(q)-1 do
                     if j in s1 then errorv(SetElementOutOfRange);
                   for j := getint(q+intsize)+1 to sethigh do
                     if j in s1 then errorv(SetElementOutOfRange);
                 end;
    98 (*chkb*),
    99 (*chkc*),
    199 { chkx },
    26 (*chki*): begin getq; popint(i1); pshint(i1);
                  if (i1 < getint(q)) or (i1 > getint(q+intsize)) then
                  errore(ValueOutOfRange)
                end;

    187 { cks }: pshint(0);
    175 { ckvi },
    203 { ckvx },
    179 { ckvb },
    180 { ckvc }: begin getq; popint(i2); popint(i1);
                    pshint(i1); pshint(ord((i1 = q) or (i2 <> 0)));
                  end;
    188 { cke }: begin popint(i2); popint(i1);
                    if i2 = 0 then errorv(VariantNotActive)
                  end;

    { all the dups are defined, but not all used }
    185 { dupb },
    186 { dupc },
    181 { dupi }: begin popint(i1); pshint(i1); pshint(i1) end;
    182 { dupa }: begin popadr(a1); pshadr(a1); pshadr(a1) end;
    183 { dupr }: begin poprel(r1); pshrel(r1); pshrel(r1) end;
    184 { dups }: begin popset(s1); pshset(s1); pshset(s1) end;

    189 { inv }: begin popadr(ad); putdef(ad, false) end;

    28 (*adi*): begin popint(i2); popint(i1);
                  if dochkovf then if (i1<0) = (i2<0) then
                    if maxint-abs(i1) < abs(i2) then
                      errore(IntegerValueOverflow);
                pshint(i1+i2) end;
    29 (*adr*): begin poprel(r2); poprel(r1); pshrel(r1+r2) end;
    30 (*sbi*): begin popint(i2); popint(i1);
                  if dochkovf then if (i1<0) <> (i2<0) then
                    if maxint-abs(i1) < abs(i2) then
                      errore(IntegerValueOverflow);
                pshint(i1-i2) end;
    31 (*sbr*): begin poprel(r2); poprel(r1); pshrel(r1-r2) end;
    32 (*sgs*): begin popint(i1); pshset([i1]); end;
    33 (*flt*): begin popint(i1); pshrel(i1) end;

    { note that flo implies the tos is float as well }
    34 (*flo*): begin poprel(r1); popint(i1); pshrel(i1); pshrel(r1) end;

    35 (*trc*): begin poprel(r1);
                  if dochkovf then if (r1 < -maxint) or (r1 > maxint) then
                    errore(RealArgumentTooLarge);
                  pshint(trunc(r1)) end;
    36 (*ngi*): begin popint(i1); pshint(-i1) end;
    37 (*ngr*): begin poprel(r1); pshrel(-r1) end;
    38 (*sqi*): begin popint(i1);
                if dochkovf then if i1 <> 0 then
                  if abs(i1) > maxint div abs(i1) then
                    errore(IntegerValueOverflow);
                pshint(sqr(i1)) end;
    39 (*sqr*): begin poprel(r1); pshrel(sqr(r1)) end;
    40 (*abi*): begin popint(i1); pshint(abs(i1)) end;
    41 (*abr*): begin poprel(r1); pshrel(abs(r1)) end;
    42 (*notb*): begin popint(i1); b1 := i1 <> 0; pshint(ord(not b1)) end;
    205 (*noti*): begin popint(i1); 
                      if i1 < 0 then errore(BooleanOperatorOfNegative); 
                      pshint(bnot(i1)) end;
    43 (*and*): begin popint(i2);
                      if i2 < 0 then errore(BooleanOperatorOfNegative);
                      popint(i1); 
                      if i1 < 0 then errore(BooleanOperatorOfNegative);
                      pshint(band(i1, i2)) end;
    44 (*ior*): begin popint(i2);
                      if i2 < 0 then errore(BooleanOperatorOfNegative);
                      popint(i1);
                      if i1 < 0 then errore(BooleanOperatorOfNegative);
                      pshint(bor(i1, i2)) end;
    206 (*xor*): begin popint(i2); b2 := i2 <> 0;
                      if i2 < 0 then errore(BooleanOperatorOfNegative);
                      popint(i1); b1 := i1 <> 0;
                      if i1 < 0 then errore(BooleanOperatorOfNegative);
                      pshint(bxor(i1, i2)) end;
    45 (*dif*): begin popset(s2); popset(s1); pshset(s1-s2) end;
    46 (*int*): begin popset(s2); popset(s1); pshset(s1*s2) end;
    47 (*uni*): begin popset(s2); popset(s1); pshset(s1+s2) end;
    48 (*inn*): begin popset(s1); popint(i1); pshint(ord(i1 in s1)) end;
    49 (*mod*): begin popint(i2); popint(i1);
                  if dochkovf then if i2 <= 0 then 
                    errore(InvalidDivisorToMod);
                  pshint(i1 mod i2) end;
    50 (*odd*): begin popint(i1); pshint(ord(odd(i1))) end;
    51 (*mpi*): begin popint(i2); popint(i1);
                  if dochkovf then if (i1 <> 0) and (i2 <> 0) then
                    if abs(i1) > maxint div abs(i2) then
                      errore(IntegerValueOverflow);
                  pshint(i1*i2) end;
    52 (*mpr*): begin poprel(r2); poprel(r1); pshrel(r1*r2) end;
    53 (*dvi*): begin popint(i2); popint(i1);
                      if dochkovf then if i2 = 0 then errore(ZeroDivide);
                      pshint(i1 div i2) end;
    54 (*dvr*): begin poprel(r2); poprel(r1);
                      if dochkovf then if r2 = 0.0 then errore(ZeroDivide);
                      pshrel(r1/r2) end;
    55 (*mov*): begin getq; popint(i2); popint(i1);
                 for i3 := 0 to q-1 do
                   begin store[i1+i3] := store[i2+i3];
                         putdef(i1+i3, getdef(i2+i3)) end;
                 (* q is a number of storage units *)
                end;
    56 (*lca*): begin getq; pshadr(q) end;

    103 (*decb*),
    104 (*decc*),
    202 (*decx*),
    57  (*deci*): begin getq; popint(i1);
                    if dochkovf then if (i1<0) <> (q<0) then
                      if maxint-abs(i1) < abs(q) then
                        errore(IntegerValueOverflow);
                    pshint(i1-q) end;

    58 (*stp*): stopins := true;

    134 (*ordb*),
    136 (*ordc*),
    200 (*ordx*),
    59  (*ordi*): ; { ord is a no-op }

    60 (*chr*): ; { chr is a no-op }

    61 (*ujc*): errorv(InvalidCase);
    62 (*rnd*): begin poprel(r1);
                  if dochkovf then if (r1 < -(maxint+0.5)) or (r1 > maxint+0.5) then
                    errore(RealArgumentTooLarge);
                  pshint(round(r1)) end;
    63 (*pck*): begin getq; getq1; popadr(a3); popadr(a2); popadr(a1);
                 if a2+q > q1 then errore(PackElementsOutOfBounds);
                 for i4 := 0 to q-1 do begin chkdef(a1+a2);
                    store[a3+i4] := store[a1+a2];
                    putdef(a3+i4, getdef(a1+a2));
                    a2 := a2+1
                 end
               end;
    64 (*upk*): begin getq; getq1; popadr(a3); popadr(a2); popadr(a1);
                 if a3+q > q1 then errore(UnpackElementsOutOfBounds);
                 for i4 := 0 to q-1 do begin chkdef(a1+i4);
                    store[a2+a3] := store[a1+i4];
                    putdef(a2+a3, getdef(a1+i4));
                    a3 := a3+1
                 end
               end;

    110 (*rgs*): begin popint(i2); popint(i1); pshset([i1..i2]) end;
    112 (*ipj*): begin getp; getq; pc := q;
                 mp := getadr(mp-p*ptrsize); { index the mark to restore }
                 { restore marks until we reach the destination level }
                 sp := getadr(mp+marksb); { get the stack bottom }
                 ep := getadr(mp+market) { get the mark ep }
               end;
    113 (*cip*): begin popadr(ad); ad1 := mp;
                mp := getadr(ad+1*ptrsize); pshadr(pc); pc := getadr(ad)
              end;
    13 (*rip*): begin getq; mp := getadr(sp+q) end;
    114 (*lpa*): begin getp; getq; { place procedure address on stack }
                pshadr(getadr(mp-p*ptrsize));
                pshadr(q)
              end;
    117 (*dmp*): begin getq; sp := sp+q end; { remove top of stack }

    118 (*swp*): begin getq; swpstk(q) end;

    119 (*tjp*): begin getq; popint(i); if i <> 0 then pc := q end;

    120 (*lip*): begin getp; getq; ad := getadr(mp-p*ptrsize) + q;
                   ad1 := getadr(ad); ad2 := getadr(ad+1*ptrsize);
                   pshadr(ad2); pshadr(ad1); 
                 end;

    191 (*cta*): begin getq; getq1; getq2; popint(i); popadr(ad); pshadr(ad);
                       pshint(i); ad := ad-q-intsize; ad1 := getadr(ad);
                       if ad1 < intsize then
                         errorv(SystemError);
                       ad1 := ad1-adrsize-1;
                       if ad1 >= q1 then begin
                         ad := ad-ad1*intsize;
                         if (i < 0) or (i >= getint(q2)) then
                           errorv(ValueOutOfRange);
                         if getadr(ad+(q1-1)*intsize) <> 
                            getint(q2+(i+1)*intsize) then
                           errorv(ChangeToAllocatedTagfield);
                       end
                 end;

    192 (*ivti*),
    101 (*ivtx*),
    102 (*ivtb*),
    111 (*ivtc*): begin getq; getq1; getq2; popint(i); popadr(ad);
                      pshadr(ad); pshint(i);
                      if (i < 0) or (i >= getint(q2)) then 
                        errorv(ValueOutOfRange);
                      if dochkdef then begin
                        b := getdef(ad);
                        if b then begin
                          if op = 192 then j := getint(ad) else j := getbyt(ad);
                          b := getint(q2+(i+1)*intsize) <> 
                               getint(q2+(j+1)*intsize);
                        end;
                        if b then begin
                          ad := ad+q;
                          for j := 1 to q1 do
                            begin putdef(ad, false); ad := ad+1 end
                        end
                      end
                end;
                
    100 (*cvbi*),
    115 (*cvbx*),
    116 (*cvbb*),
    121 (*cvbc*): begin getq; getq1; getq2; popint(i); popadr(ad);
                      pshadr(ad); pshint(i);
                      if (i < 0) or (i >= getint(q2)) then 
                        errorv(ValueOutOfRange);
                      b := getdef(ad);
                      if b then begin
                        if op = 100 then j := getint(ad) else j := getbyt(ad);
                        b := getint(q2+(i+1)*intsize) <> 
                             getint(q2+(j+1)*intsize)
                      end;
                      if b then begin 
                        ad := ad+q; 
                        if varinc(ad, ad+q1-1) then 
                          errorv(ChangeToVarReferencedVariant); 
                      end
                end;

    243 (*wbs*): begin popadr(ad); pshadr(ad); withenter(ad) end;
    244 (*wbe*): withexit;

    174 (*mrkl*): begin getq; srclin := q end;
       
    207 (*bge*): begin getq;
                   { save current exception framing }
                   pshadr(expadr); pshadr(expstk); pshadr(expmrk);
                   pshadr(0); { place dummy vector }
                   { place new exception frame }
                   expadr := q; expstk := sp; expmrk := mp
                 end;
    208 (*ede*): begin popadr(a1); { dispose vector }
                   { restore previous exception frame }
                   popadr(expmrk); popadr(expstk); popadr(expadr)
                 end;
    209 (*mse*): begin popadr(a1);
                   { restore previous exception frame }
                   popadr(expmrk); popadr(expstk); popadr(expadr);
                   { if there is no surrounding frame, handle fixed }
                   if expadr = 0 then errorm(a1)
                   else begin { throw to new frame }
                     mp := expmrk; sp := expstk; pc := expadr;
                     popadr(a2); pshadr(a1);
                     ep := getadr(mp+market) { get the mark ep }
                     { release to search vectors }
                   end
                 end;
    8 (*cjp*): begin getq; getq1; popint(i1); pshint(i1);
                  if (i1 >= getint(q)) and (i1 <= getint(q+intsize)) then
                    begin pc := q1; popint(i1) end
                end;
    20 (*lnp*): begin getq; np := q; gbtop := np; ad := pctop; 
                  { clear global memory and set undefined }
                  while np > ad do
                    begin store[ad] := 0; putdef(ad, false); ad := ad+1 end
                end;
    21 (*cal*): begin getq; pshadr(pc); pc := q end;
    22 (*ret*): popadr(pc);  
    92 (*vbs*): begin getq; popadr(ad); varenter(ad, ad+q-1) end;
    96 (*vbe*): varexit;
    19 (*brk*): ; { breaks are no-ops here }
    122 (*vis*): begin getq; getq1; popadr(ad); ad1 := ad+q*intsize;
                   for i := 1 to q do begin
                     popint(i1); putint(ad1, i1); ad1 := ad1-intsize; q1 := q1*i1;
                   end;
                   popadr(ad1); sp := sp-q1; putadr(ad, sp); pshadr(ad1)
                 end;
    133 (*vip*): begin getq; getq1; popadr(ad); ad1 := ad+q*intsize;
                   for i := 1 to q do begin
                     popint(i1); putint(ad1, i1); ad1 := ad1-intsize; q1 := q1*i1;
                   end;
                   newspc(q1, ad2); putadr(ad1, ad2)
                 end;
    226 (*vin*): begin getq; getq1; popadr(ad); ad2 := sp; 
                   for i := 1 to q do 
                     begin q1 := q1*getint(ad2); ad2 := ad2+intsize end;
                   newspc(q1+q*intsize, ad2); putadr(ad, ad2);
                   for i := 1 to q do 
                     begin popint(i1); putint(ad2, i1); ad2 := ad2+intsize;end
                 end;
    135 (*lcp*): begin popadr(ad); pshadr(ad+ptrsize); pshadr(getadr(ad)) end; 
    176 (*cps*): begin popadr(ad1); popint(i1); popadr(ad2); popint(i2);
                       pshint(i2); pshadr(ad2); pshint(i1); pshadr(ad1);
                       if i1 <> i2 then errorv(ContainerMismatch)
                 end;
    177 (*cpc*): begin getq; popadr(ad1); popadr(ad2); popadr(ad3); popadr(ad4);
                       pshadr(ad4); pshadr(ad3); pshadr(ad2); pshadr(ad1);
                       for i := 1 to q do begin
                         if getint(ad2) <> getint(ad4) then 
                           errorv(ContainerMismatch);
                         ad2 := ad2+ptrsize; ad4 := ad4+ptrsize
                       end
                 end;

    178 (*aps*): begin getq; popadr(ad1); popadr(ad); popadr(ad); popadr(i1); 
                       for i := 0 to i1*q-1 do begin
                         store[ad+i] := store[ad1+i]; putdef(ad+i, getdef(ad1+i)) 
                       end
                 end;
    210 (*apc*): begin getq; getq1; popadr(ad1); popadr(ad); popadr(ad); 
                       popadr(ad2);
                       for i := 1 to q do 
                         begin q1 := q1*getint(ad2); ad2 := ad2+intsize end;
                       for i := 0 to q1-1 do begin
                         store[ad+i] := store[ad1+i]; putdef(ad+i, getdef(ad1+i)) 
                       end
                 end;
    211 (*cxs*): begin getq; popint(i); popadr(ad); popint(i1);
                       if (i < 1) or (i > i1) then errore(ValueOutOfRange);
                       pshadr(ad+(i-1)*q)
                 end;
    212 (*cxc*): begin getq; getq1; popint(i); popadr(ad); popadr(ad1);
                       ad2 := ad1+ptrsize;
                       for j := 1 to q-1 do
                         begin q1 := q1*getint(ad2); ad2 := ad2+intsize end;
                       if (i < 1) or (i > getint(ad1)) then 
                         errore(ValueOutOfRange);
                       pshadr(ad1+ptrsize); pshadr(ad+(i-1)*q1) 
                 end;
    213 (*lft*): begin getq; popadr(ad); pshadr(q); pshadr(ad) end;
    214 (*max*): begin getq; popint(i); popadr(ad1); 
                       if q > 1 then popadr(ad) else popint(i1);
                       if (i < 1) or (i > q) then errorv(InvalidContainerLevel);
                       if q = 1 then i := i1
                       else i := getint(ad+(q-i)*intsize);
                       pshint(i)
                 end;
    221 (*vdp*),
    227 (*vdd*): begin popadr(ad); dspspc(0, ad) end;
    222 (*spc*): begin popadr(ad); popadr(ad1); pshint(getint(ad1)); pshadr(ad) end;
    223 (*ccs*): begin getq; getq1; popadr(ad); popadr(ad1); ad3 := ad1;
                       if q = 1 then q1 := q1*ad1
                       else for i := 1 to q do 
                         begin q1 := q1*getint(ad3); ad3 := ad3+intsize end;
                       ad2 := sp-q1; alignd(stackelsize, ad2); sp := ad2;
                       for i := 0 to q1-1 do begin
                         store[ad2+i] := store[ad+i]; putdef(ad2+i, getdef(ad+i));
                       end;
                       pshadr(ad1); pshadr(ad2)
                 end;
    224 (*scp*): begin popadr(ad); popadr(ad1); popadr(ad2); putadr(ad2, ad); 
                       putadr(ad2+ptrsize, ad1) end;
    225 (*ldp*): begin popadr(ad); pshadr(getadr(ad+ptrsize)); 
                       pshadr(getadr(ad)) end;
    239 (*cpp*): begin getq; getq1; ad := sp+q; sp := sp-q1; ad1 := sp;
                       for i := 1 to q1 do begin
                         store[ad1] := store[ad]; putdef(ad1, getdef(ad)); 
                         ad := ad+1; ad1 := ad1+1 
                       end
                 end;
    240 (*cpr*): begin getq; getq1; ad := sp+q+q1; ad1 := sp+q;
                       for i := 1 to q do begin
                         ad := ad-1; ad1 := ad1-1;
                         store[ad] := store[ad1]; putdef(ad, getdef(ad1)) 
                       end;
                       sp := sp+q1
                 end;
                 
    241 (*lsa*): begin getq; pshadr(sp+q) end;

    242 (*eext*): begin
#ifdef EXTERNALS
                    ExecuteExternal(pc-extvecbase);
                    { set stack below function result, if any }
                    sp := mp;
                    {???fixme???}
                    {
                    pc := getadr(mp+markra);
                    }
                    ep := getadr(mp+markep);
                    {???fixme???}
                    {
                    mp := getadr(mp+markdl)
                    }
#else
                    errorv(ExternalsNotEnabled)
#endif
                  end;

    { illegal instructions }
    173, 228, 229, 230, 231, 232, 233, 234, 247, 248, 249, 250, 251, 252,
    253, 254, 255: errorv(InvalidInstruction)

  end
end;

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

  write('P6 Pascal pmach interpreter vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  writeln;

  for oi := 1 to maxopt do begin option[oi] := false; options[oi] := false end;
  { preset options }
  dochkovf := true;  { check arithmetic overflow }
  dosrclin := true;  { add source line sets to code }
  dorecycl := true;  { obey heap space recycle requests }
  dochkrpt := false;  { check reuse of freed entry (automatically) }
  donorecpar := false; { check reuse, but leave whole block unused }
  dochkdef := true;  { check undefined accesses }
  iso7185 := false;  { iso7185 standard mode }
  varlst := nil; { set no VAR block entries }
  varfre := nil;
  wthlst := nil; { set no with block entries }
  wthcnt := 0;
  wthfre := nil;
  exitcode:= 0; { clear program exit code }
  { place the external vectors table }
  extvecbase := 11;
  { endian flip status is set if the host processor and the target disagree on
    endian mode }
  flipend := litend <> lendian;
  fndpow(maxpow10, 10, decdig);
  fndpow(maxpow16, 16, hexdig);
  fndpow(maxpow8, 8, octdig);
  fndpow(maxpow2, 2, bindig); bindig := bindig+1; { add sign bit }

  extendinit; { initialize extentions package }

  { get the command line }
  getcommandline(cmdlin, cmdlen);
  cmdpos := 1;
  { load command line options }
  paroptions;
  plcopt; { place options }
  
  { !!! remove this next statement for self compile }
#ifndef SELF_COMPILE
  reset(prd);
#endif
  
  { !!! remove this next statement for self compile }
#ifndef SELF_COMPILE
  rewrite(prr);
#endif

  { construct bit equivalence table }
  i := 1;
  for bai := 0 to 7 do begin bitmsk[bai] := i; i := i*2 end;
  
  for sdi := 0 to maxdef do storedef[sdi] := 0; { clear storage defined flags }

  writeln('loading program');
  load; (* assembles and stores code *)

  { initialize file state }
  for i := 1 to maxfil do 
    begin filstate[i] := fnone; filanamtab[i] := false end;
  
  pc := 0; sp := maxtop; np := -1; mp := maxtop; ep := 5; srclin := 1;
  expadr := 0; expstk := 0; expmrk := 0;
  
  writeln('Running program');
  writeln;
  repeat
    stopins := false; { set no stop flag }
    sinins
  until stopins; { until stop instruction is seen }

  99: { abort run }

  writeln;
  writeln('program complete');

  { give external package a chance to exit }
  exitprogram(exitcode)

end.
