(*$c+,t-,d-,l-*)
{*******************************************************************************
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
* New layout of memory in store:                                               *
*                                                                              *
*    maxstr ->    ---------------------                                        *
*                 | Constants         |                                        *
*        cp ->    ---------------------                                        *
*                 | Stack             |                                        *
*        sp ->    ---------------------                                        *
*                 | Free space        |                                        *
*        np ->    ---------------------                                        *
*                 | Heap              |                                        *
*        gbtop -> ---------------------                                        *
*                 | Globals           |                                        *
*        pctop -> ---------------------                                        *
*                 | Code              |                                        *
*                 ---------------------                                        *
*                                                                              *
* The constants are loaded upside down from the top of memory. The heap grows  *
* down, the stack grows up, and when they cross, it is an overflow error.      *
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

      { type               #32 #64 }
      intsize     =        4   {8};  { size of integer }
      intal       =        4;        { alignment of integer }
      intdig      =        11  {20}; { number of decimal digits in integer }
      inthex      =        8   {16}; { number of hex digits of integer }
      realsize    =        8;        { size of real }
      realal      =        4;        { alignment of real }
      charsize    =        1;        { size of char }
      charal      =        1;        { alignment of char }
      charmax     =        1;
      boolsize    =        1;        { size of boolean }
      boolal      =        1;        { alignment of boolean }
      ptrsize     =        4   {8};  { size of pointer }
      adrsize     =        4   {8};  { size of address }
      adral       =        4;        { alignment of address }
      setsize     =       32;        { size of set }
      setal       =        1;        { alignment of set }
      filesize    =        1;        { required runtime space for file (lfn) }
      fileidsize  =        1;        { size of the lfn only }
      exceptsize  =        1;        { size of exception variable }
      exceptal    =        1;      
      stackal     =        4;        { alignment of stack }
      stackelsize =        4   {8};  { stack element size }
      maxsize     =       32;        { this is the largest type that can be on
                                       the stack }
      { Heap alignment should be either the natural word alignment of the
        machine, or the largest object needing alignment that will be allocated.
        It can also be used to enforce minimum block allocation policy. }
      heapal      =        4;        { alignment for each heap arena }
      gbsal       =        4;        { globals area alignment }
      sethigh     =      255;        { Sets are 256 values }
      setlow      =        0;
      ordmaxchar  =      255;        { Characters are 8 bit ISO/IEC 8859-1 }
      ordminchar  =        0;
      maxresult   = realsize;        { maximum size of function result }
      marksize    =       32   {56}; { maxresult+6*ptrsize }
      { Value of nil is 1 because this allows checks for pointers that were
        initialized, which would be zero (since we clear all space to zero).
        In the new unified code/data space scheme, 0 and 1 are always invalid
        addresses, since the startup code is at least that long. }
      nilval      =        1;  { value of 'nil' }
      { beginning of code, offset by program preamble:

        2:    mst
        6/10: cup
        1:    stp

      }
      begincode   =        9   {13};

      { Mark element offsets

        Mark format is:

        -8:  Function return value, 64 bits, enables a full real result.
        -12:  Static link.
        -16: Dynamic link.
        -20: Saved EP from previous frame.
        -24: Stack bottom after locals allocate. Used for interprocdural gotos.
        -28: EP from current frame. Used for interprocedural gotos.
        -32: Return address

      }
      markfv      =        -8   {0};  { function value }
      marksl      =        -12  {8};  { static link }
      markdl      =        -16  {16}; { dynamic link }
      markep      =        -20  {24}; { (old) maximum frame size }
      marksb      =        -24  {32}; { stack bottom }
      market      =        -28  {40}; { current ep }
      markra      =        -32  {48}; { return address }

      { ******************* end of pcom and pint common parameters *********** }

      { internal constants }

      { !!! Need to use the small size memory to self compile, otherwise, by
        definition, pint cannot fit into its own memory. }
      {elide}maxstr      = 16777215;{noelide}  { maximum size of addressing for program/var }
      {remove maxstr     =  2000000; remove}  { maximum size of addressing for program/var }
      maxdef      = 2097152; { maxstr / 8 for defined bits }
      maxdigh     = 6;       { number of digits in hex representation of maxstr }
      maxdigd     = 8;       { number of digits in decimal representation of maxstr }
      maxast      = 100;     { maximum size of assert message }
      maxdbf      = 30;      { size of numeric conversion buffer }
      maxcmd      = 250;     { size of command line buffer }

      codemax     = maxstr;  { set size of code store to maximum possible }

      maxlabel = 5000;       { total possible labels in intermediate }
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
      exceptiontop                       = 86;
      
      { Exceptions that can't be caught.
        Note that these don't have associated exception variables. }
      
      UndefinedLocationAccess            = 87;
      FunctionNotImplemented             = 88;
      InvalidInISO7185Mode               = 89;
      HeapFormatInvalid                  = 90;
      DisposeOfUninitalizedPointer       = 91;
      DisposeOfNilPointer                = 92;
      BadPointerValue                    = 93;
      BlockAlreadyFreed                  = 94;
      InvalidStandardProcedureOrFunction = 95;
      InvalidInstruction                 = 96;
      NewDisposeTagsMismatch             = 97;
      PCOutOfRange                       = 98;
      StoreOverflow                      = 99;
      StackBalance                       = 100;
      SetInclusion                       = 101;
      UninitializedPointer               = 102;
      DereferenceOfNilPointer            = 103;
      PointerUsedAfterDispose            = 104;
      VariantNotActive                   = 105;
      InvalidCase                        = 106;
      SystemError                        = 107;
      ChangeToAllocatedTagfield          = 108;
      UnhandledException                 = 109;
      ProgramCodeAssertion               = 110;

      stringlgth  = 1000; { longest string length we can buffer }
      maxsp       = 81;   { number of predefined procedures/functions }
      maxins      = 255;  { maximum instruction code, 0-255 or byte }
      maxfil      = 100;  { maximum number of general (temp) files }
      maxalfa     = 10;   { maximum number of characters in alfa type }
      ujplen      = 5;    { length of ujp instruction (used for case jumps) }
      fillen      = 2000; { maximum length of filenames }
      maxbrk      = 10;   { maximum number of breakpoints }
      brkins      = 19;   { breakpoint instruction no. }
      mrkins      = 174;  { source line marker instruction executed }
      mrkinsl     = 5;    { length of that instruction } 
      varsqt      = 10;   { variable string quanta }
      { 25k lines is well above my personal limit. I tend to split files with
        more than 10,000 lines. Obviously others think that's excessive. }
      maxsrc      = 25000; { maximum number of source lines in source file }
      extsrc      = '.pas'; { extention for source file }
      maxwth      = 10;   { maximum number of watched addresses }   

      { version numbers }

      majorver   = 0; { major version number }
      minorver   = 1; { minor version number }
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
      address     = -maxstr..maxstr; { address }

      beta        = packed array[1..25] of char; (*error message*)
      settype     = set of setlow..sethigh;
      alfainx     = 1..maxalfa; { index for alfa type }
      alfa        = packed array[alfainx] of char;
      byte        = 0..255; { 8-bit byte }
      bytfil      = packed file of byte; { untyped file of bytes }
      fileno      = 0..maxfil; { logical file number }
      filnam      = packed array [1..fillen] of char; { filename strings }
      filsts      = (fclosed, fread, fwrite);
      cmdinx      = 1..maxcmd; { index for command line buffer }
      cmdbuf      = packed array [cmdinx] of char; { buffer for command line }
      break       = record ss: byte; sa: address; line: 0..maxsrc end;
      brkinx      = 1..maxbrk;
      brknum      = 0..maxbrk;
      { Here is the variable length string containment to save on space. strings
        are only stored in their length rounded to the nearest 10th. }
      strvsp = ^strvs; { pointer to variable length id string }
      strvs = record { id string variable length }
                str:   packed array [1..varsqt] of char; { data contained }
                next:  strvsp { next }
              end;
      psymbol     = ^symbol;
      symbol      = record
                      next:   psymbol; { next list symbol }  
                      name:   strvsp; { name }
                      styp:   (stglobal, stlocal); { area type }
                      off:    address; { offset address }
                      digest: strvsp { type digest }
                    end;
      pblock       = ^block;
      block        = record
                       next:    pblock; { next list block }
                       name:    strvsp; { name of block }
                       symbols: psymbol; { symbol list for block }
                       btyp:    (btprog, btproc, btfunc); { block type }
                       bstart:  address; { block start address }
                       bend:    address; { block end address }
                     end;
      wthinx       = 1..maxwth; { index for watch table }
      wthnum       = 0..maxwth; { watch table number }
      { watch symbol/type table entry }
      wthrec       = record sp: psymbol; p: integer end;
      { parser control record }
      parctl       = record b: strvsp; l, p: integer end;  

var   pc          : address;   (*program address register*)
      pctop,lsttop: address;   { top of code store }
      gbtop, gbsiz: address;   { top of globals, size of globals }
      gbset       : boolean;   { global size was set }
      op : instyp; p : lvltyp; q : address;  (*instruction register*)
      q1: address; { extra parameter }
      store       : packed array [0..maxstr] of byte; { complete program storage }
      storedef    : packed array [0..maxdef] of byte; { defined bits }
      sdi         : 0..maxdef; { index for that }
      cp          : address;  (* pointer to next free constant position *)
      mp,sp,np,ep : address;  (* address registers *)
      expadr      : address; { exception address of exception handler starts }
      expstk      : address; { exception address of sp at handlers }
      expmrk      : address; { exception address of mp at handlers }
      (*mp  points to beginning of a data segment
        sp  points to top of the stack
        ep  points to the maximum extent of the stack
        np  points to top of the dynamically allocated area*)
      bitmsk      : packed array [0..7] of byte; { bits in byte }
      maxdig      : integer; { number of decimal digits in integer }
      
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
      dochkdef: boolean; { check undefined accesses }
      
      { other flags }
      iso7185: boolean; { iso7185 standard flag }

      debugstart: boolean; { we have started debug mode }

      { !!! remove this next statement for self compile }
      {elide}prd,prr     : text;{noelide}(*prd for read only, prr for write only *)

      instr       : array[instyp] of alfa; (* mnemonic instruction codes *)
      sptable     : array[0..maxsp] of alfa; (*standard functions and procedures*)
      insp        : array[instyp] of boolean; { instruction includes a p parameter }
      insq        : array[instyp] of 0..16; { length of q parameter }
      srclin      : integer; { current source line executing }
      option      : array ['a'..'z'] of boolean; { option array }
      cmdlin      : cmdbuf; { command line }
      cmdlen      : cmdinx; { length of command line }
      cmdpos      : cmdinx; { current position in command line }
      brktbl      : array [brkinx] of break; { breakpoint table }
      bi          : brkinx; { index for same }
      stopins     : boolean; { stop instruction executed }
      breakins    : boolean; { break instruction executed }
      sourcemark  : boolean; { source line instruction executed }
      stopwatch   : boolean; { stop on watch address matched }
      watchmatch  : boolean; { watch address was matched on instruction }

      filtable    : array [1..maxfil] of text; { general (temp) text file holders }
      { general (temp) binary file holders }
      bfiltable   : array [1..maxfil] of bytfil;
      { file state holding }
      filstate    : array [1..maxfil] of filsts;
      { file buffer full status }
      filbuff     : array [1..maxfil] of boolean;
      
      srcfnm      : filnam; { filename for current source file }
      srcfnl      : 1..fillen; { length of source file name }
      srcbuf      : cmdbuf; { input source line buffer }
      strcnt      : integer; { string allocation count }
      lintrk      : array [1..maxsrc] of address; { addresses of lines }
      blkstk      : pblock; { stack of symbol blocks }
      blklst      : pblock; { discard list of symbols blocks }
      wthtbl      : array [wthinx] of address; { watch table }
      wi          : wthinx; { index for that }
      { symbol/type tracking for watch entries }
      wthsym      : array [wthinx] of wthrec;
      { address of watchpoint instruction store in progress }
      stoad       : address;
      startins    : address; { starting address of current instruction }

      i           : integer;
      c1          : char;
      ad          : address;
      bai         : integer;

(*--------------------------------------------------------------------*)

procedure debug; forward;

{ Low level error check and handling }

{ print in hex (carefull, it chops high digits freely!) }

procedure wrthex(v: integer; { value } f: integer { field });
var p,i,d,t,n: integer;
    digits: packed array [1..8] of char;
function digit(d: integer): char;
var c: char;
begin
  if d < 10 then c := chr(d+ord('0'))
  else c := chr(d-10+ord('A'));
  digit := c
end;
begin
   n := 8; { number of digits }
   if v < 0 then begin { signed }
     v := v+1+maxint; { convert number to 31 bit unsigned }
     t := v div 268435456+8; { extract high digit }
     digits[8] := digit(t); { place high digit }
     v := v mod 268435456; { remove digit }
     n := 7 { set number of digits-1 }
   end;
   p := 1;
   for i := 1 to n do begin
      d := v div p mod 16; { extract digit }
      digits[i] := digit(d); { place }
      if i < 8 then p := p*16
   end;
   for i := f downto 1 do write(digits[i]) { output }
end;

procedure errors(a: address; l: address);
begin writeln; write('*** Runtime error'); 
      if srclin > 0 then write(' [', srclin:1, ']');
      write(': ');
      if l > maxast then l := maxast;
      while l > 0 do begin write(chr(store[a])); a := a+1; l := l-1 end;
      if dodebug or dodbgflt then debug { enter debugger on fault }
      else goto 1
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
  end;
  if dodebug or dodbgflt then debug { enter debugger on fault }
  else goto 1
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

var b: byte;
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

var sb: byte;
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

(*--------------------------------------------------------------------*)

{ 

Language extension routines. These routines allow specification of semantic
functions beyond the base ISO 7185 specification. 

There are two kinds of files, and both need to be represented here:
text files and binary files. Binary files are files of bytes, and everything
besides text is broken down into bytes and transferred via byte files. 
Note that several functions don't have text equivalents, like length,
location, position and update. 

Each alternate set of routine contents are marked as:

XXX equivalence start
...
XXX equivalence end

Where "equivalence" is the language standard, like ISO7185, GPC, FPC, etc.
These represent the code for that option. A script can be used to select or
unselect those contents.
  
}
  
procedure assigntext(var f: text; var fn: filnam);
{$gnu-pascal}
var s: string(fillen);
    i, l: integer;
{$classic-pascal-level-0}
begin
  { ISO7185 start !  
  errorv(FunctionNotImplemented)
  ! ISO7185 end }

  {$gnu-pascal} 
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  assign(f, s);
  {$classic-pascal-level-0}
end;

procedure assignbin(var f: bytfil; var fn: filnam);
{$gnu-pascal}
var s: string(fillen);
    i, l: integer;
{$classic-pascal-level-0}
begin
  { ISO7185 start ! 
  errorv(FunctionNotImplemented)
  ! ISO7185 end }
  
  {$gnu-pascal} 
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  assign(f, s);
  {$classic-pascal-level-0}
end;

procedure closetext(var f: text);

begin
  { ISO7185 start !
  errorv(FunctionNotImplemented) 
  ! ISO7185 end }
  
  {$gnu-pascal} 
  close(f)
  {$classic-pascal-level-0}
end;

procedure closebin(var f: bytfil);

begin
  { ISO7185 start !
  errorv(FunctionNotImplemented) 
  ! ISO7185 end }
  
  {$gnu-pascal} 
  close(f)
  {$classic-pascal-level-0}
end;

function lengthbin(var f: bytfil): integer;
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented)
  lengthbin := 1
  ! ISO7185 end }
  
  {$gnu-pascal} 
  if empty(f) then
    lengthbin := 0
  else
    lengthbin := LastPosition (f) + 1;
  {$classic-pascal-level-0}
end;

function locationbin(var f: bytfil): integer;
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented) 
  locationbin := 1
  ! ISO7185 end } 
  
  {$gnu-pascal} 
  locationbin := position(f);
  {$classic-pascal-level-0}
end;

procedure positionbin(var f: bytfil; p: integer);
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented) 
  ! ISO7185 end }
  
  {$gnu-pascal}
  seek(f, p-1);
  {$classic-pascal-level-0}
end;

procedure updatebin(var f: bytfil);
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented)
  ! ISO7185 end }
  
  {$gnu-pascal}
  append(f);
  seek(f, 0);
  {$classic-pascal-level-0}
end;

procedure appendtext(var f: text);
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented)
  ! ISO7185 end }
  
  {$gnu-pascal}
  append(f);
  {$classic-pascal-level-0}
end;

procedure appendbin(var f: bytfil);
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented) 
  ! ISO7185 end }
  
  {$gnu-pascal}
  append(f);
  {$classic-pascal-level-0}
end;

{$gnu-pascal}
function open(fn: Cstring; f: integer): integer; external name 'open';
function c_close(fd: integer): integer; external name 'close';
{$classic-pascal-level-0}

function existsfile(var fn: filnam): boolean;
{$gnu-pascal}
var s: string(fillen);
    i, l, r, fd: integer;
{$classic-pascal-level-0}
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented)
  existsfile := true
  ! ISO7185 end }
  
  {$gnu-pascal}
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  fd := open(s, 0);
  if fd > 0 then r := c_close(fd);
  existsfile := fd >= 0
  {$classic-pascal-level-0}
end;

{$gnu-pascal}
function remove(fn: Cstring): integer; external name 'remove';
{$classic-pascal-level-0}
procedure deletefile(var fn: filnam);
{$gnu-pascal}
var s: string(fillen);
    i, l, r: integer;
{$classic-pascal-level-0}
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented)
  ! ISO7185 end }
  
  {$gnu-pascal}
  l := fillen;
  while (fn[l] = ' ') and (l > 1) do l := l-1;
  s := '';
  for i := 1 to l do s := s+fn[i];
  r := remove(s);
  if r <> 0 then errore(FileDeleteFail);
  {$classic-pascal-level-0}
end;

{$gnu-pascal}
function rename(fns, fnd: Cstring): integer; external name 'rename';
{$classic-pascal-level-0}
procedure changefile(var fnd, fns: filnam);
{$gnu-pascal}
var ss,sd: string(fillen);
    i, l, r: integer;
{$classic-pascal-level-0}
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented)
  ! ISO7185 end }
  
  {$gnu-pascal}
  l := fillen;
  while (fns[l] = ' ') and (l > 1) do l := l-1;
  ss := '';
  for i := 1 to l do ss := ss+fns[i];
  l := fillen;
  while (fnd[l] = ' ') and (l > 1) do l := l-1;
  sd := '';
  for i := 1 to l do sd := sd+fnd[i];
  r := rename(ss,sd);
  if r <> 0 then errore(FileNameChangeFail)
  {$classic-pascal-level-0}
end;

procedure getcommandline(var cb: cmdbuf; var l: cmdinx);
var i: cmdinx;
    x, p: integer;
procedure putcmd(c: char);
begin
  if i >= maxcmd-1 then errore(CommandLineTooLong);
  cb[i] := c; i := i+1
end;
begin
  { ISO7185 start !
  errorv(FunctionNotImplemented)
  ! ISO7185 end }
  
  {$gnu-pascal}
  for i := 1 to maxcmd do cb[i] := ' '; i := 1;
  for p := 1 to paramcount do begin
    for x := 1 to length(paramstr(p)) do putcmd(paramstr(p)[x]); 
    if p < paramcount then putcmd(' ')
  end;
  l := i-1
  {$classic-pascal-level-0}
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
begin if cmdpos <= cmdlen then cmdpos := cmdpos+1 end;

function eofcommand: boolean; 
begin eofcommand := cmdpos > cmdlen+1 end;

function eolncommand: boolean; 
begin eolncommand := cmdpos >= cmdlen+1 end;

procedure readlncommand; 
begin cmdpos := maxcmd end;

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
        (bufcommand in ['a'..'z',  'A'..'Z', '_']) do begin
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
        (bufcommand in ['a'..'z',  'A'..'Z', '_']) do begin
    if i = fillen then errorv(FileNameTooLong);
    fne[i] := bufcommand;
    getcommand;
    i := i+1
  end;
  assignbin(f, fne) { assign to that }
end;

(*-------------------------------------------------------------------------*)

                { character and string quanta functions }

(*-------------------------------------------------------------------------*)

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
  strcnt := strcnt+1 { count }
end;

{ recycle string quanta list }
procedure putstrs(p: strvsp);
var p1: strvsp;
begin
  while p <> nil do begin
    p1 := p; p := p^.next; dispose(p1); strcnt := strcnt-1
  end
end;
  
{ write variable length id string to file }
procedure writev(var f: text; s: strvsp; fl: integer);
var i: integer; c: char;
begin i := 1;
  while fl > 0 do begin
    c := ' '; if s <> nil then begin c := s^.str[i]; i := i+1 end;
    write(f, c); fl := fl-1;
    if i > varsqt then begin s := s^.next; i := 1 end
  end
end;

{ find padded length of variable length id string }
function lenpv(s: strvsp): integer;
var i, l, lc: integer;
begin l := 1; lc := 0;
  while s <> nil do begin
    for i := 1 to varsqt do begin
      if s^.str[i] <> ' ' then lc := l;
      l := l+1; { count characters }
    end;
    s := s^.next
  end;
  lenpv := lc
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
 
{ assign symbol identifier fixed to variable length string, including 
  allocation }
procedure strassvf(var a: strvsp; var b: filnam);
var i, j, l: integer; p, lp: strvsp;
begin l := fillen; p := nil; a := nil; j := 1;
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

{ compare variable length id string to fixed }
function strequvf(a: strvsp; var b: filnam): boolean;
var m: boolean; i, j: integer; c: char;
begin
  m := true; j := 1;
  for i := 1 to fillen do begin
    c := ' '; if a <> nil then begin c := a^.str[j]; j := j+1 end;
    if lcase(c) <> lcase(b[i]) then m := false;
    if j > varsqt then begin a := a^.next; j := 1 end
  end;
  strequvf := m
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

function getint(a: address): integer;

var r: record case boolean of

          true:  (i: integer);
          false: (b: packed array [1..intsize] of byte);

       end;
    i: 1..intsize;

begin

   if dochkdef then chkdef(a);
   for i := 1 to intsize do r.b[i] := store[a+i-1];

   getint := r.i

end;

procedure putint(a: address; x: integer);

var r: record case boolean of

          true:  (i: integer);
          false: (b: packed array [1..intsize] of byte);

       end;
    i: 1..intsize;

begin

   r.i := x;
   for i := 1 to intsize do
     begin store[a+i-1] := r.b[i]; putdef(a+i-1, true) end

end;

function getrel(a: address): real;

var r: record case boolean of

          true:  (r: real);
          false: (b: packed array [1..realsize] of byte);

       end;
    i: 1..realsize;

begin

   if dochkdef then chkdef(a);
   for i := 1 to realsize do r.b[i] := store[a+i-1];
   getrel := r.r

end;

procedure putrel(a: address; f: real);

var r: record case boolean of

          true:  (r: real);
          false: (b: packed array [1..realsize] of byte);

       end;
    i: 1..realsize;

begin

   r.r := f;
   for i := 1 to realsize do
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
          false: (b: packed array [1..setsize] of byte);

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
          false: (b: packed array [1..setsize] of byte);

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

function getbyt(a: address): byte;

begin

   if dochkdef then chkdef(a);
   getbyt := store[a]

end;

procedure putbyt(a: address; b: byte);

begin

   store[a] := b; putdef(a, true)

end;

function getadr(a: address): address;

var r: record case boolean of

          true:  (a: address);
          false: (b: packed array [1..adrsize] of byte);

       end;
    i: 1..adrsize;

begin

   if dochkdef then chkdef(a);
   for i := 1 to adrsize do r.b[i] := store[a+i-1];
   getadr := r.a

end;

procedure putadr(a: address; ad: address);

var r: record case boolean of

          true:  (a: address);
          false: (b: packed array [1..adrsize] of byte);

       end;
    i: 1..adrsize;

begin

   r.a := ad;
   for i := 1 to adrsize do
     begin store[a+i-1] := r.b[i]; putdef(a+i-1, true) end

end;

{ Swap pointer on top with second on stack. The size of the second is given. }

procedure swpstk(l: address);

var sb: packed array [1..maxsize] of byte;
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

{ chkstd: called whenever a non-ISO7185 construct is being processed }

procedure chkstd;
begin
  if iso7185 then errorv(InvalidInISO7185Mode)
end;
  
function isbrk(a: address): boolean;
var i: brkinx;
    m: boolean;
begin m := false;
  for i := 1 to maxbrk do if brktbl[i].sa = a then m := true;
  isbrk := m
end;

function isbrkl(l: integer): boolean;
var i: brkinx;
    m: boolean;
begin m := false;
  for i := 1 to maxbrk do if brktbl[i].line = l then m := true;
  isbrkl := m
end;

{ list single instruction at address }

procedure lstins(var ad: address);

var ads: address;
    op: instyp; p : lvltyp; q, q1 : address;  (*instruction register*)

begin

   { fetch instruction from byte store }
   ads := ad;
   op := store[ad]; ad := ad+1;
   if insp[op] then begin p := store[ad]; ad := ad+1 end;
   if insq[op] > 0 then begin

      if insq[op] = 1 then q := store[ad]
      else if insq[op] = intsize then q := getint(ad)
      else begin q := getint(ad); q1 := getint(ad+intsize) end;
      ad := ad+insq[op]

   end;
   write(': ');
   wrthex(op, 2);
   write(' ', instr[op]:10, '  ');
   if insp[op] then begin

      wrthex(p, 2);
      if insq[op] > 0 then begin write(','); wrthex(q, inthex) end;
      if insq[op] > intsize then  begin write(','); wrthex(q1, inthex) end

   end else if insq[op] > 0 then begin

      write('   '); wrthex(q, inthex);
      if insq[op] > intsize then begin write(','); wrthex(q1, inthex) end

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

{ clear filename string }

procedure clrfn(var fn: filnam);
var i: 1..fillen;
begin
  for i := 1 to fillen do fn[i] := ' '
end;

(*--------------------------------------------------------------------*)

{ load intermediate file }

procedure load;
   type  labelst  = (entered,defined); (*label situation*)
         labelrg  = 0..maxlabel;       (*label range*)
         labelrec = record
                          val: address;
                           st: labelst
                    end;
   var  word : array[alfainx] of char; ch  : char;
        labeltab: array[labelrg] of labelrec;
        labelvalue: address;
        iline: integer; { line number of intermediate file }

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
         instr[ 19]:='brk       '; insp[ 19] := false; insq[ 19] := 0;
         instr[ 20]:='---       '; insp[ 20] := false; insq[ 20] := 0;
         instr[ 21]:='---       '; insp[ 21] := false; insq[ 21] := 0;
         instr[ 22]:='---       '; insp[ 22] := false; insq[ 22] := 0;
         instr[ 23]:='ujp       '; insp[ 23] := false; insq[ 23] := intsize;
         instr[ 24]:='fjp       '; insp[ 24] := false; insq[ 24] := intsize;
         instr[ 25]:='xjp       '; insp[ 25] := false; insq[ 25] := intsize;
         instr[ 26]:='chki      '; insp[ 26] := false; insq[ 26] := intsize;
         instr[ 27]:='---       '; insp[ 27] := false; insq[ 27] := 0;
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
         instr[ 42]:='notb      '; insp[ 42] := false; insq[ 42] := 0;
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
         instr[ 91]:='---       '; insp[ 91] := false; insq[ 91] := intsize;
         instr[ 92]:='---       '; insp[ 92] := false; insq[ 92] := intsize;
         instr[ 93]:='incb      '; insp[ 93] := false; insq[ 93] := intsize;
         instr[ 94]:='incc      '; insp[ 94] := false; insq[ 94] := intsize;
         instr[ 95]:='chka      '; insp[ 95] := false; insq[ 95] := intsize;
         instr[ 96]:='---       '; insp[ 96] := false; insq[ 96] := intsize;
         instr[ 97]:='chks      '; insp[ 97] := false; insq[ 97] := intsize;
         instr[ 98]:='chkb      '; insp[ 98] := false; insq[ 98] := intsize;
         instr[ 99]:='chkc      '; insp[ 99] := false; insq[ 99] := intsize;
         instr[100]:='---       '; insp[100] := false; insq[100] := intsize;
         instr[101]:='---       '; insp[101] := false; insq[101] := intsize;
         instr[102]:='---       '; insp[102] := false; insq[102] := intsize;
         instr[103]:='decb      '; insp[103] := false; insq[103] := intsize;
         instr[104]:='decc      '; insp[104] := false; insq[104] := intsize;
         instr[105]:='loda      '; insp[105] := true;  insq[105] := intsize;
         instr[106]:='lodr      '; insp[106] := true;  insq[106] := intsize;
         instr[107]:='lods      '; insp[107] := true;  insq[107] := intsize;
         instr[108]:='lodb      '; insp[108] := true;  insq[108] := intsize;
         instr[109]:='lodc      '; insp[109] := true;  insq[109] := intsize;
         instr[110]:='rgs       '; insp[110] := false; insq[110] := 0;
         instr[111]:='---       '; insp[111] := false; insq[111] := 0;
         instr[112]:='ipj       '; insp[112] := true;  insq[112] := intsize;
         instr[113]:='cip       '; insp[113] := true;  insq[113] := 0;
         instr[114]:='lpa       '; insp[114] := true;  insq[114] := intsize;
         instr[115]:='---       '; insp[115] := false; insq[115] := 0;
         instr[116]:='---       '; insp[116] := false; insq[116] := 0;
         instr[117]:='dmp       '; insp[117] := false; insq[117] := intsize;
         instr[118]:='swp       '; insp[118] := false; insq[118] := intsize;
         instr[119]:='tjp       '; insp[119] := false; insq[119] := intsize;
         instr[120]:='lip       '; insp[120] := true;  insq[120] := intsize;
         instr[121]:='---       '; insp[121] := false; insq[121] := 0;
         instr[122]:='---       '; insp[122] := false; insq[122] := 0;
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
         instr[133]:='---       '; insp[133] := false; insq[133] := 0;
         instr[134]:='ordb      '; insp[134] := false; insq[134] := 0;
         instr[135]:='---       '; insp[135] := false; insq[135] := 0;
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
         instr[174]:='mrkl*     '; insp[174] := false; insq[174] := intsize;
         instr[175]:='ckvi      '; insp[175] := false; insq[175] := intsize;
         instr[176]:='---       '; insp[176] := false; insq[176] := intsize;
         instr[177]:='---       '; insp[177] := false; insq[177] := intsize;
         instr[178]:='---       '; insp[178] := false; insq[178] := intsize;
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

         sptable[ 0]:='get       ';     sptable[ 1]:='put       ';
         sptable[ 2]:='thw       ';     sptable[ 3]:='rln       ';
         sptable[ 4]:='new       ';     sptable[ 5]:='wln       ';
         sptable[ 6]:='wrs       ';     sptable[ 7]:='eln       ';
         sptable[ 8]:='wri       ';     sptable[ 9]:='wrr       ';
         sptable[10]:='wrc       ';     sptable[11]:='rdi       ';
         sptable[12]:='rdr       ';     sptable[13]:='rdc       ';
         sptable[14]:='sin       ';     sptable[15]:='cos       ';
         sptable[16]:='exp       ';     sptable[17]:='log       ';
         sptable[18]:='sqt       ';     sptable[19]:='atn       ';
         sptable[20]:='---       ';     sptable[21]:='pag       ';
         sptable[22]:='rsf       ';     sptable[23]:='rwf       ';
         sptable[24]:='wrb       ';     sptable[25]:='wrf       ';
         sptable[26]:='dsp       ';     sptable[27]:='wbf       ';
         sptable[28]:='wbi       ';     sptable[29]:='wbr       ';
         sptable[30]:='wbc       ';     sptable[31]:='wbb       ';
         sptable[32]:='rbf       ';     sptable[33]:='rsb       ';
         sptable[34]:='rwb       ';     sptable[35]:='gbf       ';
         sptable[36]:='pbf       ';     sptable[37]:='rib       ';
         sptable[38]:='rcb       ';     sptable[39]:='nwl       ';
         sptable[40]:='dsl       ';     sptable[41]:='eof       ';
         sptable[42]:='efb       ';     sptable[43]:='fbv       ';
         sptable[44]:='fvb       ';     sptable[45]:='wbx       ';
         sptable[46]:='asst      ';     sptable[47]:='clst      ';
         sptable[48]:='pos       ';     sptable[49]:='upd       ';
         sptable[50]:='appt      ';     sptable[51]:='del       ';
         sptable[52]:='chg       ';     sptable[53]:='len       ';
         sptable[54]:='loc       ';     sptable[55]:='exs       ';
         sptable[56]:='assb      ';     sptable[57]:='clsb      ';
         sptable[58]:='appb      ';     sptable[59]:='hlt       ';
         sptable[60]:='ast       ';     sptable[61]:='asts      ';
         sptable[62]:='wrih      ';     sptable[63]:='wrio      ';
         sptable[64]:='wrib      ';     sptable[65]:='wrsp      ';
         sptable[66]:='wiz       ';     sptable[67]:='wizh      ';
         sptable[68]:='wizo      ';     sptable[69]:='wizb      ';
         sptable[70]:='rds       ';     sptable[71]:='ribf      ';
         sptable[72]:='rdif      ';     sptable[73]:='rdrf      ';
         sptable[74]:='rcbf      ';     sptable[75]:='rdcf      ';
         sptable[76]:='rdsf      ';     sptable[77]:='rdsp      ';
         sptable[78]:='aeft      ';     sptable[79]:='aefb      ';
         sptable[80]:='rdie      ';     sptable[81]:='rdre      ';
         
         pc := begincode;
         cp := maxstr; { set constants pointer to top of storage }
         for i:= 1 to 10 do word[i]:= ' ';
         for i:= 0 to maxlabel do
             with labeltab[i] do begin val:=-1; st:= entered end;
         { initalize file state }
         for i := 1 to maxfil do filstate[i] := fclosed;

         { !!! remove this next statement for self compile }
         {elide}reset(prd);{noelide}

         iline := 1; { set 1st line of intermediate }
         
         gbset := false { global size not set }
         
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
          endlist: boolean;
          op: instyp; q : address;  (*instruction register*)
   begin
      if labeltab[x].st=defined then errorl('duplicated label         ')
      else begin
             if labeltab[x].val<>-1 then (*forward reference(s)*)
             begin curr:= labeltab[x].val; endlist:= false;
                while not endlist do begin
                      ad := curr;
                      op := store[ad]; { get instruction }
                      q := getadr(ad+1+ord(insp[op]));
                      succ:= q; { get target address from that }
                      q:= labelvalue; { place new target address }
                      ad := curr;
                      putadr(ad+1+ord(insp[op]), q);
                      if succ=-1 then endlist:= true
                                 else curr:= succ
                 end
             end;
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

   procedure assemble; forward;

   procedure generate;(*generate segment of code*)
      var x: integer; (* label number *)
          again: boolean;
          c,ch1: char;
          i,j: integer;
          ext: packed array [1..4] of char;
          sn: filnam;
          snl: 1..fillen;
          bp: pblock;
          sp: psymbol;
          ad: address;
          sgn: boolean;
   procedure getlab;
   var i: 1..fillen;
   begin skpspc; for i := 1 to fillen do sn[i] := ' '; snl := 1;
     if ch = ' ' then errorl('Symbols format error     ');
     while ch <> ' ' do begin
       if snl >= fillen then errorl('Symbols format error     ');
       sn[snl] := ch; getnxt; snl := snl+1
     end;
     snl := snl-1
   end;
   begin
      again := true;
      while again do
            begin if eof(prd) then errorl('unexpected eof on input  ');
                  getnxt;(* first character of line*)
                  if not (ch in ['i', 'l', 'q', ' ', ':', 'o', 'g', 'b',
                                 'e', 's']) then
                    errorl('unexpected line start    ');
                  case ch of
                       'i': getlin; { comment }
                       'l': begin read(prd,x);
                                  getnxt;
                                  if ch='=' then read(prd,labelvalue)
                                            else labelvalue:= pc;
                                  update(x); getlin
                            end;
                       'q': begin again := false; getlin end;
                       ' ': begin getnxt; 
                                  while not eoln(prd) and (ch = ' ') do getnxt;
                                  if not eoln(prd) and (ch <> ' ') then assemble
                                  else getlin 
                            end;
                       ':': begin { source line }
                               read(prd,x); { get source line number }
                               lintrk[x] := pc; { place in line tracking }
                               if dosrclin then begin
                                  { pass source line register instruction }
                                  store[pc] := 174; putdef(pc, true); pc := pc+1;
                                  putint(pc, x); pc := pc+intsize
                               end;
                               { skip the rest of the line, which would be the
                                 contents of the source line if included }
                               while not eoln(prd) do
                                  read(prd, c); { get next character }
                               getlin { source line }
                            end;
                       'o': begin { option }
                              getnxt;
                              while not eoln(prd) and (ch = ' ') do getnxt;
                              repeat
                                if not (ch in ['a'..'z']) then 
                                  errorl('No valid option found    ');
                                ch1 := ch; getnxt;
                                option[ch1] := ch = '+'; getnxt;
                                case ch1 of
                                  'g': dodmplab := option[ch1];
                                  'h': dosrclin := option[ch1];
                                  'n': dorecycl := option[ch1];
                                  'o': dochkovf := option[ch1];
                                  'p': dochkrpt := option[ch1];
                                  'q': dochkdef := option[ch1];
                                  's': iso7185  := option[ch1];
                                  'w': dodebug  := option[ch1];
                                  'r': dodbgflt := option[ch1];
                                  'f': dodbgsrc := option[ch1];
                                  'b':; 'c':; 'd':; 'l':; 't':; 'u':;
                                  'v':; 'x':; 'y':; 'z':; 'm':; 'a':;
                                  'k':; 'i':; 'e':; 'j':;
                                  { unused: m,a,i,e }
                                end
                              until not (ch in ['a'..'z']);
                              getlin
                            end;
                       'g': begin read(prd,gbsiz); gbset := true; getlin end;
                       'b': begin 
                              getnxt; skpspc;
                              if not (ch in ['p', 'r', 'f']) then
                                errorl('Block type is invalid    ');
                              ch1 := ch; { save block type }
                              getnxt; skpspc; getlab;
                              new(bp); strassvf(bp^.name, sn); 
                              bp^.symbols := nil;
                              case ch1 of { block type }
                                'p': bp^.btyp := btprog;
                                'r': bp^.btyp := btproc;
                                'f': bp^.btyp := btfunc
                              end;
                              bp^.bend := -1;
                              { put onto block stack }
                              bp^.next := blkstk; blkstk := bp;
                              if ch1 = 'p' then begin { it's a program block }
                                { has to have room for extention }
                                if snl >= fillen-4 then 
                                  errorl('Block name too long      ');
                                { add file extension }
                                ext := extsrc;
                                for i := 1 to 4 do begin
                                  sn[snl+1] := ext[i]; snl := snl+1 
                                end;
                                { place as source file }
                                srcfnm := sn; srcfnl := snl
                              end;
                              blkstk^.bstart := pc; { set start address }   
                              getlin
                            end;
                       'e': begin { end block }
                              if blkstk = nil then 
                                errorl('No block to end          ');
                              { mark block non-inclusive }
                              blkstk^.bend := pc;
                              bp := blkstk; { remove from block stack }
                              blkstk := blkstk^.next;
                              bp^.next := blklst; { put to discard list }
                              blklst := bp;
                              if blkstk <> nil then blkstk^.bstart := pc;
                              getlin
                            end;
                       's': begin { symbol }
                              getnxt; getlab;
                              new(sp); strassvf(sp^.name, sn);
                              skpspc; 
                              if not (ch in ['g', 'l']) then
                                errorl('Symbol type is invalid   ');
                              if ch = 'g' then sp^.styp := stglobal 
                              else sp^.styp := stlocal;
                              getnxt;
                              skpspc;
                              if not (ch in ['0'..'9','-']) then 
                                errorl('No offset found          ');
                              sgn := ch = '-'; if ch = '-' then getnxt;
                              ad := 0; while ch in ['0'..'9'] do 
                                begin ad := ad*10+ord(ch)-ord('0'); getnxt end;
                              if sgn then ad := -ad;
                              sp^.off := ad; getlab;
                              strassvf(sp^.digest, sn);
                              if blkstk = nil then 
                                errorl('Symbol not in block      ');
                              { place in block symbol list }
                              sp^.next := blkstk^.symbols; 
                              blkstk^.symbols := sp;
                              getlin
                            end;
                  end;
            end
   end; (*generate*)

   procedure assemble; (*translate symbolic code into machine code and store*)
      var name :alfa; r :real; s :settype;
          i,x,s1,lb,ub,l:integer; c: char;
          str: packed array [1..stringlgth] of char; { buffer for string constants }
          t: integer; { [sam] temp for compiler bug }

      procedure lookup(x: labelrg); (* search in label table*)
      begin case labeltab[x].st of
                entered: begin q := labeltab[x].val;
                           labeltab[x].val := pc
                         end;
                defined: q:= labeltab[x].val
            end(*case label..*)
      end;(*lookup*)

      procedure labelsearch;
         var x: labelrg;
      begin while (ch<>'l') and not eoln(prd) do read(prd,ch);
            read(prd,x); lookup(x)
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

      procedure storeop;
      begin
        if pc+1 > cp then errorl('Program code overflow    ');
        store[pc] := op; putdef(pc, true); pc := pc+1
      end;

      procedure storep;
      begin
        if pc+1 > cp then errorl('Program code overflow    ');
        store[pc] := p; putdef(pc, true); pc := pc+1
      end;

      procedure storeq;
      begin
        if pc+adrsize > cp then errorl('Program code overflow    ');
         putadr(pc, q); pc := pc+adrsize
      end;

      procedure storeq1;
      begin
        if pc+adrsize > cp then errorl('Program code overflow    ');
         putadr(pc, q1); pc := pc+adrsize
      end;

   begin  p := 0;  q := 0;  op := 0;
      getname;
      { note this search removes the top instruction from use }
      while (instr[op]<>name) and (op < maxins) do op := op+1;
      if op = maxins then errorl('illegal instruction      ');
       
      case op of  (* get parameters p,q *)

          (*lod,str,lda,lip*)
          0, 193, 105, 106, 107, 108, 109, 195,
          2, 70, 71, 72, 73, 74,4,120: begin read(prd,p,q); storeop; storep;
                                             storeq
                                       end;

          12(*cup*): begin read(prd,p); labelsearch; storeop;
                           storep; storeq
                     end;

          11,113(*mst,cip*): begin read(prd,p); storeop; storep end;

          { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
          142, 148, 154, 160, 166, 172,

          (*lao,ixa,mov,dmp,swp*)
          5,16,55,117,118,

          (*ldo,sro,ind,inc,dec,ckv*)
          1, 194, 196, 198, 65, 66, 67, 68, 69,
          3, 75, 76, 77, 78, 79,
          9, 85, 86, 87, 88, 89,
          10, 90, 91, 92, 93, 94,
          57, 100, 101, 102, 103, 104,
          175, 176, 177, 178, 179, 180, 201, 202, 
          203: begin read(prd,q); storeop; storeq end;

          (*pck,upk,cta,ivt*)
          63, 64, 191, 192: begin read(prd,q); read(prd,q1); storeop; storeq;
                                  storeq1 end;

          (*ujp,fjp,xjp,tjp,bge*)
          23,24,25,119,207,

          (*ents,ente*)
          13, 173: begin labelsearch; storeop; storeq end;

          (*ipj,lpa*)
          112,114: begin read(prd,p); labelsearch; storeop; storep; storeq end;

          15 (*csp*): begin skpspc; getname;
                           while name<>sptable[q] do
                           begin q := q+1; if q > maxsp then
                                 errorl('std proc/func not found  ')
                           end; storeop;
                           if pc+1 > cp then
                             errorl('Program code overflow    ');
                           store[pc] := q; putdef(pc, true); pc := pc+1
                      end;

          7, 123, 124, 125, 126, 127 (*ldc*): begin case op of  (*get q*)
                           123: begin read(prd,i); storeop;
                                      if pc+intsize > cp then
                                         errorl('Program code overflow    ');
                                      putint(pc, i); pc := pc+intsize
                                end;

                           124: begin read(prd,r);
                                      cp := cp-realsize;
                                      alignd(realal, cp);
                                      if cp <= 0 then
                                         errorl('constant table overflow  ');
                                      putrel(cp, r); q := cp;
                                      storeop; storeq
                                end;

                           125: storeop; (*p,q = 0*)

                           126: begin read(prd,q); storeop;
                                      if pc+1 > cp then
                                        errorl('Program code overflow    ');
                                      putbol(pc, q <> 0); pc := pc+1 end;

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
                                  storeop;
                                  if pc+1 > cp then
                                    errorl('Program code overflow    ');
                                  putchr(pc, c); pc := pc+1
                                end;
                           7: begin skpspc;
                                   if ch <> '(' then
                                     errorl('ldcs() expected          ');
                                   s := [ ];  getnxt;
                                   while ch<>')' do
                                   begin read(prd,s1); getnxt; s := s + [s1]
                                   end;
                                   cp := cp-setsize;
                                   alignd(setal, cp);
                                   if cp <= 0 then
                                      errorl('constant table overflow  ');
                                   putset(cp, s);
                                   q := cp;
                                   storeop; storeq
                                end
                           end (*case*)
                     end;

           26, 95, 96, 97, 98, 99, 190, 199,8 (*chk,cjp*): begin
                         read(prd,lb,ub);
                         { cjp is compare with jump }
                         if op = 8 then begin labelsearch; q1 := q end;
                         if (op = 95) or (op = 190) then q := lb
                         else
                         begin
                           cp := cp-intsize;
                           alignd(intal, cp);
                           if cp <= 0 then errorl('constant table overflow  ');
                           putint(cp, ub);
                           cp := cp-intsize;
                           alignd(intal, cp);
                           if cp <= 0 then errorl('constant table overflow  ');
                           putint(cp, lb); q := cp
                         end;
                         storeop; storeq; 
                         if op = 8 then storeq1
                       end;

           56 (*lca*): begin read(prd,l); skpspc;
                         for i := 1 to stringlgth do str[i] := ' ';
                         if ch <> '''' then errorl('bad string format        ');
                         i := 0;
                         repeat
                           if eoln(prd) then errorl('unterminated string      ');
                           getnxt;
                           c := ch; if (ch = '''') and (prd^ = '''') then begin
                             getnxt; c := ' '
                           end;
                           if c <> '''' then begin
                             if i >= stringlgth then errorl('string overflow          ');
                             str[i+1] := ch; { accumulate string }
                             i := i+1
                           end
                         until c = '''';
                         { place in storage }
                         cp := cp-l;
                         if cp <= 0 then errorl('constant table overflow  ');
                         q := cp;
                         for x := 1 to l do putchr(q+x-1, str[x]);
                         { this should have worked, the for loop is faulty
                           because the calculation for end is done after the i
                           set
                         for i := 0 to i-1 do putchr(q+i, str[i+1]);
                         }
                         storeop; storeq
                       end;

          14, 128, 129, 130, 131, 132, 204, (*ret*)

          { equ,neq,geq,grt,leq,les with no parameter }
          17, 137, 138, 139, 140, 141,
          18, 143, 144, 145, 146, 147,
          19, 149, 150, 151, 152, 153,
          20, 155, 156, 157, 158, 159,
          21, 161, 162, 163, 164, 165,
          22, 167, 168, 169, 170, 171,

          59, 133, 134, 135, 136, 200, (*ord*)

          6, 80, 81, 82, 83, 84, 197, (*sto*)

          { eof,adi,adr,sbi,sbr,sgs,flt,flo,trc,ngi,ngr,sqi,sqr,abi,abr,notb,
            noti,and,ior,xor,dif,int,uni,inn,mod,odd,mpi,mpr,dvi,dvr,stp,chr,
            rnd,rgs,fbv,fvb,ede,mse }
          27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
          48,49,50,51,52,53,54,58,60,62,110,111,
          115,116,205,206,208,209,

          { dupi, dupa, dupr, dups, dupb, dupc, cks, cke, inv }
          181, 182, 183, 184, 185, 186,187,188,189: storeop;

                      (*ujc must have same length as ujp, so we output a dummy
                        q argument*)
          61 (*ujc*): begin storeop; q := 0; storeq end

      end; (*case*)

      getlin { next intermediate line }

   end; (*assemble*)

begin (*load*)
   init;
   generate;
   pctop := pc; { save top of code store }
   gbtop := pctop+gbsiz; { set top of globals }
   lsttop := pctop; { save as top of listing }
   pc := 0;
   generate;
   if not gbset then errorl('global space not set     ');
   alignu(stackal, pctop); { align the end of code for globals }
   alignd(heapal, cp); { align the start of cp for stack top }
   gbtop := pctop+gbsiz; { set top of globals }
   alignu(gbsal, gbtop); { align the globals top }
   if dodmplab then dmplabs { Debug: dump label definitions }
end; (*load*)

(*------------------------------------------------------------------------*)

{ runtime handlers }

function base(ld :integer):address;
   var ad :address;
begin ad := mp;
   while ld>0 do begin ad := getadr(ad+marksl); ld := ld-1 end;
   base := ad
end; (*base*)

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

procedure valfil(fa: address); { attach file to file entry }
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
         if filstate[i] = fclosed then begin ff := i; i := maxfil end;
         i := i+1
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

{

   Blocks in the heap are dead simple. The block begins with a length, including
   the length itself. If the length is positive, the block is free. If negative,
   the block is allocated. This means that AddressOfBLock+abs(lengthOfBlock) is
   address of the next block, and RequestedSize <= LengthOfBLock+adrsize is a
   reasonable test for if a free block fits the requested size, since it will
   never be true of occupied blocks.

}

{ report all space in heap }

procedure repspc;
var l, ad: address;
begin
   writeln;
   writeln('Heap space breakdown');
   writeln;
   ad := gbtop; { index the bottom of heap }
   while ad < np do begin
      l := getadr(ad); { get next block length }
      write('addr: '); wrthex(ad, maxdigh); write(': ', abs(l):6, ': ');
      if l >= 0 then writeln('free') else writeln('alloc');
      ad := ad+abs(l)
   end
end;

{ find free block using length }

procedure fndfre(len: address; var blk: address);
var l, b: address;
begin
  b := 0; { set no block found }
  blk := gbtop; { set to bottom of heap }
  while blk < np do begin { search blocks in heap }
     l := getadr(blk); { get length }
     if (abs(l) < heapal) or (abs(l) > np) then errorv(HeapFormatInvalid);
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

procedure newspc(len: address; var blk: address);
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
   if dorecycl and not dochkrpt then begin { obey recycling requests }
      putadr(ad, abs(getadr(ad))); { set block free }
      cscspc { coalesce free space }
   end else if dochkrpt then begin { perform special recycle }
      { check can break off top block }
      len := abs(getadr(ad)); { get length }
      if len >= adrsize*2 then putadr(ad+adrsize, abs(getadr(ad))-adrsize);
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
     while l > 0 do begin
       c := chkbuf; getbuf; putchr(ad, c); ad := ad+1; l := l-1
     end;
     { if fielded, validate the rest of the field is blank }
     if fld then while not chkend do begin 
       if chkbuf <> ' ' then errore(FieldNotBlank);
       getbuf
     end
   end;(*reads*)
   
   procedure readsp(fn: fileno; ad: address; l: integer);
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
   var c: integer; p: integer; digit: array [1..maxdbf] of char; i: integer;
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

      { trace routine executions }
      if dotrcrot then writeln(pc:6, '/', sp:6, '-> ', q:2);

      case q of
           0 (*get*): begin popadr(ad); valfil(ad); fn := store[ad]; getfn(fn)
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
           39 (*nwl*): begin popadr(ad1); popint(i); { size of record, size of f const list }
                            newspc(ad1+(i+1)*intsize, ad); { alloc record, size of list, number in list }
                            ad1 := ad+i*intsize; putint(ad1, i+adrsize+1);
                            k := i; { save n tags for later }
                            while k > 0 do
                              begin ad1 := ad1-intsize; popint(j);
                                    putint(ad1, j); k := k-1
                              end;
                            { get pointer to dest var, place base above taglist and
                              list of fixed consts }
                            popadr(ad1); putadr(ad1, ad+(i+1)*intsize)
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
           6 (*wrs*): begin popint(l); popint(w); popadr(ad1);
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
           65 (*wrsp*): begin popint(l); popadr(ad1); popadr(ad); pshadr(ad);
                           valfil(ad); fn := store[ad];
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
                           popadr(ad1); popadr(ad); dspspc(ad1, ad)
                      end;
           40(*dsl*): begin
                           popadr(ad1); popint(i); { get size of record and n tags }
                           ad := getadr(sp+i*intsize); { get rec addr }
                           { under us is either the number of tags or the length of the block, if it
                             was freed. Either way, it is >= adrsize if not free }
                           if getint(ad-intsize) <= adrsize then
                             errorv(BlockAlreadyFreed);
                           if i <> getint(ad-intsize)-adrsize-1 then
                             errorv(NewDisposeTagsMismatch);
                           ad := ad-intsize; ad2 := sp;
                           { ad = top of tags in dynamic, ad2 = top of tags in
                             stack }
                           k := i;
                           while k > 0 do
                             begin
                               if getint(ad) <> getint(ad2) then
                                 errorv(NewDisposeTagsMismatch);
                               ad := ad-intsize; ad2 := ad2+intsize; k := k-1
                             end;
                           dspspc(ad1+(i+1)*intsize, ad+intsize);
                           while i > 0 do begin popint(j); i := i-1 end;
                           popadr(ad)
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
                            if filbuff[fn] then { buffer data exists }
                            for i := 1 to l do begin
                              store[ad1+i-1] := store[ad+fileidsize+i-1]; 
                              putdef(ad1+i-1, true)
                            end else begin
                              if eof(bfiltable[fn]) then
                                errore(EndOfFile);
                              for i := 1 to l do begin
                                read(bfiltable[fn], store[ad1]);
                                putdef(ad1, true);
                                ad1 := ad1+1
                              end
                            end
                      end;
           33(*rsb*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if filstate[fn] = fclosed then
                             errore(FileNotOpen);
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
                          else begin
                            if filstate[fn] = fread then
                            putchr(ad+fileidsize, filtable[fn]^)
                          end
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
                         assigntext(filtable[fn], fl1) 
                       end;
           56 (*assb*): begin popint(i); popadr(ad1); popadr(ad); valfil(ad); 
                         fn := store[ad]; clrfn(fl1); 
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]); 
                         assignbin(bfiltable[fn], fl1) 
                       end;
           47 (*clst*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         closetext(filtable[fn])
                       end;
           57 (*clsb*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         closebin(bfiltable[fn])
                       end;
           48 (*pos*): begin popint(i); popadr(ad); valfil(ad); fn := store[ad];
                         if i < 1 then errore(InvalidFilePosition); 
                         positionbin(bfiltable[fn], i)
                       end;
           49 (*upd*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         updatebin(bfiltable[fn])
                       end;
           50 (*appt*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         appendtext(filtable[fn])
                       end;
           58 (*appb*): begin popadr(ad); valfil(ad); fn := store[ad]; 
                         appendbin(bfiltable[fn])
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
           59 (*hlt*): goto 1;
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
                         assignexternaltext(filtable[fn], fl1) 
                       end;
           79(*aefb*): begin popint(i); popadr(ad1); popadr(ad); valfil(ad); 
                         fn := store[ad]; clrfn(fl1);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         assignexternalbin(bfiltable[fn], fl1) 
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

{ print source lines }
procedure prtsrc(s, e: integer; comp: boolean);
var f: text; i: integer; c: char; newline: boolean; si,ei: address; 
begin
  if not existsfile(srcfnm) then 
    writeln('*** Source file ', srcfnm:srcfnl, ' not found')
  else begin
    assigntext(f, srcfnm); reset(f); i := 1;
    newline := true;
    while (i < s) and not eof(f) do begin readln(f); i := i+1 end;
    while (i <= e) and not eof(f) do begin
      if newline then begin { output line head }
        write(i:4, ': ');
        if isbrkl(i) then write('b') else write(' ');
        if lintrk[i] = pc then write('*') else write(' ');
        write(' ');
        newline := false
      end;
      if not eoln(f) then begin read(f, c); write(c) end
      else begin 
        readln(f); writeln;
        if comp then begin { coordinated listing mode }
          si := lintrk[i]; ei := lintrk[i+1];
          if (si >= 0) and (ei >= 0) then
            while si <= ei-1 do begin { list machine instructions }
              if isbrk(si) then write('b') else write(' ');
              if pc = si then write('*') else write(' ');
              write(' ');
              wrthex(si, maxdigh);
              lstins(si);
              writeln 
            end
        end; 
        i := i+1; newline := true 
      end 
    end;
    closetext(f)
  end
end;

function iswatch(ad: address): boolean;
var wi: wthinx;
begin
  wi := 1; 
  while (wi < maxwth) and (wthtbl[wi] <> ad) and (wthtbl[wi] > 0) do wi := wi+1;
  iswatch := wthtbl[wi] = ad  
end;

procedure singleins;
var ad,ad1,ad2,ad3: address; b: boolean; i,j,k,i1,i2 : integer; c, c1: char;
    i3,i4: integer; r1,r2: real; b1,b2: boolean; s1,s2: settype; 
    a1,a2,a3: address;
begin
  if pc >= pctop then errorv(PCOutOfRange);
  { fetch instruction from byte store }
  startins := pc; { save starting pc }
  getop;

  (*execute*)

  { trace executed instructions }
  if dotrcins then begin
    if isbrk(startins) then write('b ') else write('  ');
    if pc = startins then write('*') else write(' ');
    write(' ');
    wrthex(startins, maxdigh);
    write('/');
    wrthex(sp, maxdigh);
    lstins(startins);
    writeln
  end;
  case op of

    0   (*lodi*): begin getp; getq; pshint(getint(base(p) + q)) end;
    193 (*lodx*): begin getp; getq; pshint(getbyt(base(p) + q)) end;
    105 (*loda*): begin getp; getq; pshadr(getadr(base(p) + q)) end;
    106 (*lodr*): begin getp; getq; pshrel(getrel(base(p) + q)) end;
    107 (*lods*): begin getp; getq; getset(base(p) + q, s1); pshset(s1) end;
    108 (*lodb*): begin getp; getq; pshint(ord(getbol(base(p) + q))) end;
    109 (*lodc*): begin getp; getq; pshint(ord(getchr(base(p) + q))) end;

    1   (*ldoi*): begin getq; pshint(getint(pctop+q)) end;
    194 (*ldox*): begin getq; pshint(getbyt(pctop+q)) end;
    65  (*ldoa*): begin getq; pshadr(getadr(pctop+q)) end;
    66  (*ldor*): begin getq; pshrel(getrel(pctop+q)) end;
    67  (*ldos*): begin getq; getset(pctop+q, s1); pshset(s1) end;
    68  (*ldob*): begin getq; pshint(ord(getbol(pctop+q))) end;
    69  (*ldoc*): begin getq; pshint(ord(getchr(pctop+q))) end;

    2   (*stri*): begin getp; getq; stoad := base(p)+q; 
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popint(i); putint(stoad, i) end
                  end;
    195 (*strx*): begin getp; getq; stoad := base(p)+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popint(i); putbyt(stoad, i) end
                  end;
    70  (*stra*): begin getp; getq; stoad := base(p)+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popadr(ad); putadr(stoad, ad) end
                  end;
    71  (*strr*): begin getp; getq; stoad := base(p)+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin poprel(r1); putrel(stoad, r1) end
                  end;
    72  (*strs*): begin getp; getq; stoad := base(p)+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popset(s1); putset(stoad, s1) end
                  end;
    73  (*strb*): begin getp; getq; stoad := base(p)+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else 
                          begin popint(i1); b1 := i1 <> 0; putbol(stoad, b1) end 
                  end;
    74  (*strc*): begin getp; getq; stoad := base(p)+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else 
                          begin popint(i1); c1 := chr(i1); putchr(stoad, c1) end
                  end;

    3   (*sroi*): begin getq; stoad := pctop+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popint(i); putint(stoad, i) end
                  end;
    196 (*srox*): begin getq; stoad := pctop+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popint(i); putbyt(stoad, i) end
                  end;
    75  (*sroa*): begin getq; stoad := pctop+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popadr(ad); putadr(stoad, ad) end
                  end;
    76  (*sror*): begin getq; stoad := pctop+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin poprel(r1); putrel(stoad, r1) end
                  end;
    77  (*sros*): begin getq; stoad := pctop+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else begin popset(s1); putset(stoad, s1) end
                  end;
    78  (*srob*): begin getq; stoad := pctop+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else 
                          begin popint(i1); b1 := i1 <> 0; putbol(stoad, b1) end
                  end;
    79  (*sroc*): begin getq; stoad := pctop+q;
                        if iswatch(stoad) and stopwatch then watchmatch := true
                        else 
                          begin popint(i1); c1 := chr(i1); putchr(stoad, c1) end
                  end;

    4 (*lda*): begin getp; getq; pshadr(base(p)+q) end;
    5 (*lao*): begin getq; pshadr(pctop+q) end;

    6   (*stoi*): begin popint(i); popadr(stoad);
                        if iswatch(stoad) and stopwatch then 
                          begin watchmatch := true; pshadr(stoad); pshint(i) end
                        else putint(stoad, i) 
                  end;
    197 (*stox*): begin popint(i); popadr(stoad); 
                        if iswatch(stoad) and stopwatch then 
                          begin watchmatch := true; pshadr(stoad); pshint(i) end
                        else putbyt(stoad, i) 
                  end;
    80  (*stoa*): begin popadr(ad1); popadr(stoad); 
                        if iswatch(stoad) and stopwatch then 
                          begin watchmatch := true; pshadr(stoad); pshint(i) end
                        else putadr(stoad, ad1) 
                  end;
    81  (*stor*): begin poprel(r1); popadr(stoad); 
                        if iswatch(stoad) and stopwatch then 
                          begin watchmatch := true; pshadr(stoad); pshint(i) end
                        else putrel(stoad, r1) 
                  end;
    82  (*stos*): begin popset(s1); popadr(stoad); 
                        if iswatch(stoad) and stopwatch then 
                          begin watchmatch := true; pshadr(stoad); pshint(i) end
                        else putset(stoad, s1) 
                  end;
    83  (*stob*): begin popint(i1); b1 := i1 <> 0; popadr(stoad);
                        if iswatch(stoad) and stopwatch then 
                          begin watchmatch := true; pshadr(stoad); pshint(i) end
                        else putbol(stoad, b1) 
                  end;
    84  (*stoc*): begin popint(i1); c1 := chr(i1); popadr(stoad);
                        if iswatch(stoad) and stopwatch then 
                          begin watchmatch := true; pshadr(stoad); pshint(i) end
                        else putchr(stoad, c1) 
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

    11 (*mst*): begin (*p=level of calling procedure minus level of called
                        procedure + 1;  set dl and sl, decrement sp*)
                 (* then length of this element is
                    max(intsize,realsize,boolsize,charsize,ptrsize *)
                 getp;
                 ad := sp; { save mark base }
                 { allocate mark }
                 for j := 1 to marksize div intsize do pshint(0);
                 putadr(ad+marksl, base(p)); { sl }
                 (* the length of this element is ptrsize *)
                 putadr(ad+markdl, mp); { dl }
                 (* idem *)
                 putadr(ad+markep, ep); { ep }
                 (* idem *)
                end;

    12 (*cup*): begin (*p=no of locations for parameters, q=entry point*)
                 getp; getq;
                 mp := sp+(p+marksize); { mp to base of mark }

                 putadr(mp+markra, pc); { place ra }
                 pc := q
                end;

    13 (*ents*): begin getq; ad := mp+q; (*q = length of dataseg*)
                    if ad <= np then begin
                      errorv(StoreOverflow);
                    end;
                    { clear allocated memory and set undefined }
                    while sp > ad do
                      begin sp := sp-1; store[sp] := 0; putdef(sp, false);
                      end;
                    putadr(mp+marksb, sp) { set bottom of stack }
                 end;

    173 (*ente*): begin getq; ep := sp+q;
                    if ep <= np then errorv(StoreOverFlow);
                    putadr(mp+market, ep) { place current ep }
                  end;
                  (*q = max space required on stack*)
                  
    14  (*retp*): begin
                   if sp <> getadr(mp+marksb) then 
                     errorv(StackBalance);
                   sp := mp;
                   pc := getadr(mp+markra); { get ra }
                   ep := getadr(mp+markep); { get old ep }
                   mp := getadr(mp+markdl)  { get dl }
                 end;
    { For characters and booleans, need to clean 8 bit results because
      only the lower 8 bits were stored to. }
    130 (*retc*): begin
                   if sp <> getadr(mp+marksb) then 
                     errorv(StackBalance);
                   putint(mp+markfv+maxresult div 2, 
                          ord(getchr(mp+markfv+maxresult div 2)));
                   { set stack below function result }
                   sp := mp+markfv+maxresult div 2; 
                   pc := getadr(mp+markra);
                   ep := getadr(mp+markep);
                   mp := getadr(mp+markdl)
                 end;
    131 (*retb*): begin
                   if sp <> getadr(mp+marksb) then 
                     errorv(StackBalance);
                   putint(mp+markfv+maxresult div 2, 
                          ord(getbol(mp+markfv+maxresult div 2)));
                   { set stack below function result }
                   sp := mp+markfv+maxresult div 2;
                   pc := getadr(mp+markra);
                   ep := getadr(mp+markep);
                   mp := getadr(mp+markdl)
                 end;
    128 (*reti*),
    204 (*retx*): begin
                   if sp <> getadr(mp+marksb) then 
                     errorv(StackBalance);
                   { set stack below function result }
                   sp := mp+markfv+maxresult div 2;
                   pc := getadr(mp+markra);
                   ep := getadr(mp+markep);
                   mp := getadr(mp+markdl)
                 end;
    129 (*retr*): begin
                   if sp <> getadr(mp+marksb) then 
                     errorv(StackBalance);
                   sp := mp+markfv; { set stack below function result }
                   pc := getadr(mp+markra);
                   ep := getadr(mp+markep);
                   mp := getadr(mp+markdl)
                 end;
    132  (*reta*): begin
                   if sp <> getadr(mp+marksb) then 
                     errorv(StackBalance);
                   { set stack below function result }  
                   sp := mp+markfv+maxresult div 2;
                   pc := getadr(mp+markra);
                   ep := getadr(mp+markep);
                   mp := getadr(mp+markdl)
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

    18  { neqa }: begin popadr(a2); popadr(a1); pshint(ord(a1<>a2)) end;
    145 { neqb },
    147 { neqc },
    143 { neqi }: begin popint(i2); popint(i1); pshint(ord(i1<>i2)) end;
    144 { neqr }: begin poprel(r2); poprel(r1); pshint(ord(r1<>r2)) end;
    146 { neqs }: begin popset(s2); popset(s1); pshint(ord(s1<>s2)) end;
    148 { neqm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b)) end;

    151 { geqb },
    153 { geqc },
    149 { geqi }: begin popint(i2); popint(i1); pshint(ord(i1>=i2)) end;
    150 { geqr }: begin poprel(r2); poprel(r1); pshint(ord(r1>=r2)) end;
    152 { geqs }: begin popset(s2); popset(s1); pshint(ord(s1>=s2)) end;
    154 { geqm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(b or (store[a1] >= store[a2])))
                  end;

    157 { grtb },
    159 { grtc },
    155 { grti }: begin popint(i2); popint(i1); pshint(ord(i1>i2)) end;
    156 { grtr }: begin poprel(r2); poprel(r1); pshint(ord(r1>r2)) end;
    158 { grts }: errorv(SetInclusion);
    160 { grtm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] > store[a2])))
                  end;

    163 { leqb },
    165 { leqc },
    161 { leqi }: begin popint(i2); popint(i1); pshint(ord(i1<=i2)) end;
    162 { leqr }: begin poprel(r2); poprel(r1); pshint(ord(r1<=r2)) end;
    164 { leqs }: begin popset(s2); popset(s1); pshint(ord(s1<=s2)) end;
    166 { leqm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(b or (store[a1] <= store[a2])))
                  end;

    169 { lesb },
    171 { lesc },
    167 { lesi }: begin popint(i2); popint(i1); pshint(ord(i1<i2)) end;
    168 { lesr }: begin poprel(r2); poprel(r1); pshint(ord(r1<r2)) end;
    170 { less }: errorv(SetInclusion);
    172 { lesm }: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] < store[a2])))
                  end;

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
                       else if dochkrpt and (a1 <> nilval) then begin
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

    189 { inv }: begin popadr(stoad);
                       if iswatch(stoad) and stopwatch then 
                         begin pshadr(stoad); watchmatch := true end
                       else putdef(stoad, false) 
                 end;

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
                 mp := base(p); { index the mark to restore }
                 { restore marks until we reach the destination level }
                 sp := getadr(mp+marksb); { get the stack bottom }
                 ep := getadr(mp+market) { get the mark ep }
               end;
    113 (*cip*): begin getp; popadr(ad);
                mp := sp+(p+marksize);
                { replace next link mp with the one for the target }
                putadr(mp+marksl, getadr(ad+1*ptrsize));
                putadr(mp+markra, pc);
                pc := getadr(ad)
              end;
    114 (*lpa*): begin getp; getq; { place procedure address on stack }
                pshadr(base(p));
                pshadr(q)
              end;
    117 (*dmp*): begin getq; sp := sp+q end; { remove top of stack }

    118 (*swp*): begin getq; swpstk(q) end;

    119 (*tjp*): begin getq; popint(i); if i <> 0 then pc := q end;

    120 (*lip*): begin getp; getq; ad := base(p) + q;
                   ad1 := getadr(ad); ad2 := getadr(ad+1*ptrsize);
                   pshadr(ad2); pshadr(ad1); 
                 end;

    191 (*cta*): begin getq; getq1; popint(i); popadr(ad); pshadr(ad);
                       pshint(i); ad := ad-q-intsize; ad1 := getadr(ad);
                       if ad1 < intsize then
                         errorv(SystemError);
                       ad1 := ad1-adrsize-1;
                       if ad1 >= q1 then begin
                         ad := ad-ad1*intsize;
                         if getadr(ad+(q1-1)*intsize) <> i then
                           errorv(ChangeToAllocatedTagfield);
                       end
                 end;

    192 (*ivt*): begin getq; getq1; popint(i); popadr(ad);
                      pshadr(ad); pshint(i);
                      if false and dochkdef then begin
                        b := getdef(ad);
                        if b then b := i <> getint(ad);
                        if b then begin
                          ad := ad+q;
                          for j := 1 to q1 do
                            begin putdef(ad, false); ad := ad+1 end
                        end
                      end
                end;

    174 (*mrkl*): begin getq; srclin := q;
                        if dotrcsrc then
                          if dodbgsrc then begin
                            writeln;
                            prtsrc(srclin-1, srclin+1, false);
                            writeln
                          end else writeln('Source line executed: ', q:1);
                        sourcemark := true
                  end;
       
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
                
    19 (*brk*): breakins := true;

    { illegal instructions }
    20,  21,  22,  27,  91,  92,  96,  100, 101, 102, 111, 115, 116, 121,
    122, 133, 135, 176, 177, 178, 210, 211, 212, 213, 214, 215, 216, 217,
    218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231,
    232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245,
    246, 247, 248, 249, 250, 251, 252, 253, 254,
    255: errorv(InvalidInstruction)

  end
end;

procedure debug;

label 2,3;

var dbc, tdc: parctl; cn: alfa; dbgend: boolean; s,e: address; i,x,l,p: integer;
    bp: pblock; syp: psymbol; sn, sn2: filnam; snl: 1..fillen; si,ei: integer;
    ens: array [1..100] of integer; sim: boolean; enum: boolean; ad: address;
    wi: wthinx; fw: wthnum; undef: boolean;

procedure getlin(var pc: parctl);
var c: char;
begin 
  with pc do begin
    b := nil; l := 0; write('debug> '); 
    while not eoln do begin read(c); l := l+1; strchrass(b, l, c) end;
    readln; p := 1
  end
end;

function chkchr(var pc: parctl): char;
begin
  with pc do if p <= l then chkchr := strchr(b, p) else chkchr := ' '
end;

procedure nxtchr(var pc: parctl);
begin
  with pc do if p <= l then p := p+1
end;

function chkend(var pc: parctl): boolean;
begin
  with pc do chkend := p > l
end;

procedure skpspc(var pc: parctl);
begin
  while (chkchr(pc) = ' ') and not chkend(pc) do nxtchr(pc)
end;

procedure expect(var pc: parctl; c: char);
begin
  if chkchr(pc) <> c then begin writeln('*** Expected ''', c, ''''); goto 2 end;
  nxtchr(pc)
end;

procedure getnum(var pc: parctl; var n: integer);
var r: integer;
begin
   n := 0; r := 10; skpspc(pc); 
   if not (chkchr(pc) in ['$', '&', '%', '0'..'9']) then begin
     writeln('*** Number expected'); goto 2
   end;
   if chkchr(pc) = '$' then begin r := 16; nxtchr(pc) end
   else if chkchr(pc) = '&' then begin r := 8; nxtchr(pc) end
   else if chkchr(pc) = '%' then begin r := 2; nxtchr(pc) end;  
   while chkchr(pc) in ['0'..'9','A'..'F','a'..'f'] do begin
     if ((r = 10) and (chkchr(pc) > '9')) or
        ((r = 2) and (chkchr(pc) > '1')) or
        ((r = 8) and (chkchr(pc) > '7')) then begin
       writeln('*** Digit beyond radix');
       goto 2
     end; 
     if chkchr(pc) in ['0'..'9'] then n := n*r+ord(chkchr(pc))-ord('0')
     else if chkchr(pc) in ['A'..'F'] then n := n*r+ord(chkchr(pc))-ord('A')+10
     else n := n*r+ord(chkchr(pc))-ord('a')+10;
     nxtchr(pc)
   end
end;

procedure getnam(var pc: parctl);
var i: alfainx;
begin
  skpspc(pc);
  for i := 1 to maxalfa do cn[i] := ' '; i := 1;
  while (chkchr(pc) <> ' ') and not chkend(pc) do 
    begin cn[i] := chkchr(pc); i := i+1; nxtchr(pc) end
end;

procedure getbrk;
var i: 1..maxbrk;
begin
  for i := 1 to maxbrk do 
    if brktbl[i].sa >= 0 then
      store[brktbl[i].sa] := brktbl[i].ss
end;

procedure putbrk;
var i: 1..maxbrk;
begin
  for i := 1 to maxbrk do with brktbl[i] do 
    if sa > 0 then begin ss := store[sa]; store[sa] := brkins end
end;

procedure prtrng(a, b: address);
var i: 1..maxdigh;
begin
  wrthex(a, maxdigh); write('-');
  if b+1 > a then wrthex(b, maxdigh) else for i := 1 to maxdigh do write('*');
  writeln(' (',b+1-a:maxdigd,')')
end;

procedure dmpmem(s, e: address);
   var i, x: integer;
       bs: array [1..16] of byte;
       f, l: boolean;
       ba: address;
begin l := false; for i := 1 to 16 do bs[i] := 0;
   while s <= e do begin
     ba := s; i := 1; f := true;
     while (s <= e) and (i <= 16) do begin  
       if bs[i] <> store[s] then f := false;
       bs[i] := store[s]; s := s+1; i := i+1 
     end;
     if not f or (i < 16) then begin
       if l then begin
         writeln;
         for x := 1 to maxdigh do write('*'); write(': ');
         for x := 1 to 16 do write('** ');
       end;
       writeln; wrthex(ba, maxdigh); write(': ');
       for x := 1 to i-1 do begin wrthex(bs[x], 2); write(' ') end;
       l := false
     end else l := true
   end;
   writeln;
   writeln
end;

procedure prthdr;
var ad: address;
begin
  writeln;
  if dodbgsrc then prtsrc(srclin-1, srclin+1,false) { do source level }
  else begin { machine level }
    write('pc: '); wrthex(pc, maxdigh); write(' sp: '); wrthex(sp, maxdigh);
    write(' mp: '); wrthex(mp, maxdigh); write(' np: '); wrthex(np, maxdigh);
    write(' cp: '); wrthex(cp, maxdigh);
    writeln;
    if isbrk(pc) then write('b ') else write('  ');
    wrthex(pc, maxdigh); ad := pc; lstins(ad)
  end; 
  writeln
end;

procedure dmpdsp(mp: address);

begin
  writeln;
  write('Mark @'); wrthex(mp, maxdigh); writeln;
  write('sl: '); wrthex(mp+marksl, 8); write(': '); 
  if getdef(mp+marksl) then wrthex(getadr(mp+marksl), 8) 
  else write('********'); writeln;
  write('dl: '); wrthex(mp+markdl, 8); write(': '); 
  if getdef(mp+markdl) then wrthex(getadr(mp+markdl), 8) 
  else write('********'); writeln;
  write('ep: '); wrthex(mp+markep, 8); write(': '); 
  if getdef(mp+markep) then wrthex(getadr(mp+markep), 8) 
  else write('********'); writeln;
  write('sb: '); wrthex(mp+marksb, 8); write(': '); 
  if getdef(mp+marksb) then wrthex(getadr(mp+marksb), 8) 
  else write('********'); writeln;
  write('et: '); wrthex(mp+market, 8); write(': '); 
  if getdef(mp+market) then wrthex(getadr(mp+market), 8) 
  else write('********'); writeln;
  write('ra: '); wrthex(mp+markra, 8); write(': '); 
  if getdef(mp+markra) then wrthex(getadr(mp+markra), 8) 
  else write('********'); writeln;
  writeln
end;

{ test if there are any frames }
function noframe: boolean;
begin
  { for this mark system, the mp points at frame top, so sp = mp means not
    allocated }
  noframe := sp = mp
end;

{ find active symbol }
procedure symadr(var fs: psymbol; var ma: address);
var bp: pblock; cpc: address;
procedure fndsym;
var bp: pblock; sp: psymbol;
begin
  fs := nil;
  bp := blklst; { index the block list }
  while bp <> nil do begin { traverse block list }
    if (cpc >= bp^.bstart) and (cpc < bp^.bend) then begin { search this section }
      sp := bp^.symbols;
      while sp <> nil do begin { traverse symbols list }
        if strequvf(sp^.name, sn) then begin fs := sp; sp := nil; bp := nil end
        else sp := sp^.next
      end;
      bp := nil { no need to search other blocks }
    end;
    if bp <> nil then bp := bp^.next
  end
end;
begin
  fs := nil;
  if not noframe then begin { there is an active frame }
    ma := mp; { set current mark }
    cpc := pc; { set current pc }
    repeat
      fndsym; { find in this frame }
      if fs = nil then begin
        cpc := getadr(ma+markra); { get next frame }
        ma := getadr(ma+marksl)
      end
    until (fs <> nil) or (mp = cp); { found or no next frame }
  end
end; 

procedure getsym(var pc: parctl);
var i: 1..fillen;
begin for i := 1 to fillen do sn[i] := ' '; snl := 1; skpspc(pc);
  if not (chkchr(pc) in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then 
    begin writeln('*** Symbol name expected'); goto 2 end;
  while chkchr(pc) in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do begin
    if snl >= fillen then begin writeln('*** Symbol name too long'); goto 2 end;
    sn[snl] := chkchr(pc); nxtchr(pc); snl := snl+1
  end;
  snl := snl-1
end;

function isbyte(v: integer): boolean; 
begin isbyte := (v >= 0) and (v <= 255) end;

procedure getrng(var pc: parctl; var enum: boolean; var s, e: integer);
var si: integer; c: char;
begin enum := false; { not enumeration }
  { check for common types (yep, boolean and char can be a range) }
  if chkchr(pc) = 'b' then begin s := 0; e := 1 end
  else if chkchr(pc) = 'c' then begin s := ordminchar; e := ordmaxchar end
  else begin
    expect(pc, 'x'); { must be subrange or enum }
    expect(pc, '('); if chkchr(pc) in ['0'..'9'] then begin { subrange }
      getnum(pc, s); expect(pc, ','); getnum(pc, e); expect(pc, ')')
    end else begin { enum }
      enum := true; s := 0; e := 0; si := 1; 
      repeat ens[si] := p; getsym(pc); e := e+1; c := chkchr(pc); 
        if c = ',' then nxtchr(pc);
        if si < 100 then si := si+1
      until c <> ',';
      expect(pc, ')'); e := e-1
     end
   end
end; 

procedure skptyp(var pc: parctl); forward;

{ skip fieldlist }
procedure skiplist(var pc: parctl);
var c: char; i: integer;
begin
  expect(pc, '(');
  while chkchr(pc) <> ')' do begin
    getsym(pc); expect(pc, ':'); getnum(pc, i); expect(pc, ':'); skptyp(pc);
    if chkchr(pc) = '(' then begin nxtchr(pc);
      { tagfield, parse sublists }
      while chkchr(pc) <> ')' do begin getnum(pc, i); skiplist(pc) end;
      expect(pc, ')')
    end;
    c := chkchr(pc); if c = ',' then begin nxtchr(pc); write(',') end
  end;
  expect(pc, ')')
end;

{ skip over, don't print, type }
procedure skptyp{(var pc: parctl)};
var s,e: integer; c: char; enum: boolean;
begin
  case chkchr(pc) of
    'i','b','c','n', 'p', 'e': nxtchr(pc);
    'x': begin nxtchr(pc); expect(pc, '(');
           if chkchr(pc) in ['0'..'9'] then begin
             getnum(pc, s); expect(pc, ','); getnum(pc, e); expect(pc, ')');
           end else begin 
             repeat getsym(pc);
               c := chkchr(pc); if chkchr(pc) = ',' then nxtchr(pc);
             until c <> ',';
             expect(pc, ')');
           end
         end;
    's': begin nxtchr(pc); getrng(pc, enum, s, e); 
           if not enum then nxtchr(pc)
         end;
    'a': begin nxtchr(pc); getrng(pc, enum, s, e); 
           if not enum then nxtchr(pc) 
         end;
    'r': begin nxtchr(pc); skiplist(pc) end; 
  end
end;

procedure setpar(var pc: parctl; td: strvsp; p: integer);
begin pc.b := td; pc.l := lenpv(td); pc.p := p end;

{ print value by type }
procedure prttyp(var ad: address; td: strvsp; var p: integer; byt: boolean);
var i,x: integer; b: boolean; c: char; s, e: integer; l: integer;
    sn, sns: filnam; snl, snsl: 1..fillen; first: boolean; enum: boolean;
    ad2, ad3: address; tdc: parctl; ps: integer;

function bitset(ad: address; bit: integer): boolean;
var b: byte;
begin
   b := getbyt(ad+bit div 8);
   bitset := odd(b div bitmsk[bit mod 8])
end;

{ process fieldlist }
procedure fieldlist(var pc: parctl);
var i, x: integer; c: char; off: address;
begin
  expect(pc, '('); 
  while chkchr(pc) <> ')' do begin
    getsym(pc); expect(pc, ':'); getnum(pc, i); off := i; expect(pc, ':');
    ad2 := ad+off; ad3 := ad2; prttyp(ad2, td, pc.p, false);
    if chkchr(pc) = '(' then begin nxtchr(pc);
      { tagfield, parse sublists }
      while chkchr(pc) <> ')' do begin
        { need to fetch and check if this case is active }
        getnum(pc, i); 
        { get actual tagfield according to size }
        if ad2-ad3 = 1 then x := getbyt(ad+off) else x := getint(ad+off);
        if i = x then begin write('('); fieldlist(pc); write(')') end
        else skiplist(pc)
      end;
      expect(pc, ')')
    end;
    c := chkchr(pc); if c = ',' then begin nxtchr(pc); write(',') end
  end;
  expect(pc, ')')
end;

begin
  setpar(tdc, td, p); { set up type digest for parse }
  case chkchr(tdc) of
    'i': begin nxtchr(tdc);
           if getdef(ad) then begin
             { fetch according to size }
             if byt then begin i := getbyt(ad); ad := ad+1 end 
             else begin i := getint(ad); ad := ad+intsize end;
             write(i:1) 
           end else write('Undefined') 
         end;
    'b': begin nxtchr(tdc); 
           if getdef(ad) then begin 
             b := getbol(ad); 
             if b = false then write('false(0)') else write('true(1)')
           end else write('Undefined'); 
           ad := ad+boolsize 
         end;
    'c': begin nxtchr(tdc);
           if getdef(ad) then write(getchr(ad)) else write('Undefined'); 
           ad := ad+charsize 
         end;
    'n': begin nxtchr(tdc);
           if getdef(ad) then write(getrel(ad)) else write('Undefined'); 
           ad := ad+realsize 
         end;
    'x': begin nxtchr(tdc); expect(tdc, '(');
           { It's subrange or enumerated. Subrange has a subtype. Note all 
           subranges are reduced to numeric. }
           if chkchr(tdc) in ['0'..'9'] then begin
             getnum(tdc, s); expect(tdc, ','); getnum(tdc, e); expect(tdc, ')');
             prttyp(ad, td, tdc.p, isbyte(s) and isbyte(e)) { eval subtype }
           end else begin { it's an enumeration, that's terminal }
             if getdef(ad) then begin
               { fetch according to size }
               if byt then begin i := getbyt(ad); ad := ad+1 end 
               else begin i := getint(ad); ad := ad+intsize end;
               s := i;
               { now get the enum label according to number }
               repeat getsym(tdc); i := i-1;
                 c := chkchr(tdc); if chkchr(tdc) = ',' then nxtchr(tdc);
                 if i = -1 then begin sns := sn; snsl := snl end
               until c <> ',';
               expect(tdc, ')');
               write(sns:snsl, '(', s:1, ')')
             end else write('Undefined')     
           end
         end;
    'p': writeln('Pointer');
    's': begin nxtchr(tdc); getrng(tdc, enum, s, e);
           write('['); first := true;
           for i := s to e do
             if bitset(ad, i) then begin
               if not first then write(','); 
               if not enum then begin
                 if chkchr(tdc) = 'c' then write('''', chr(i), '''')
                 else if chkchr(tdc) = 'b' then begin 
                   if i = 0 then write('false(0)')
                   else write('true(1)')
                 end else write(i:1)
               end else if e <= 100 then begin { output symbolic }
                 x := ens[i]; { get start position }
                 repeat
                   c := strchr(td, x); 
                   if (c <> ',') and (c <> ')') then begin write(c); x := x+1 end
                 until (c = ')') or (c = ',') 
               end else write(i:1); 
               first := false 
             end;
           write(']')
         end;
    'a': begin nxtchr(tdc); getrng(tdc, enum, s, e); { get range of index }
           if not enum then nxtchr(tdc); { discard index type, we don't need it }
           write('array ');
           { print whole array }
           ps := tdc.p; 
           for i := s to e do 
             begin prttyp(ad, td, tdc.p, false); tdc.p := ps; 
                   if i < e then write(', ') 
             end;
           write(' end');
         end;
    'r': begin nxtchr(tdc); write('record '); fieldlist(tdc); write(' end') end; 
    'e': writeln('Exception');
  end;
  p := tdc.p { put back digest position (string does not change) }
end;

{ process variable reference }
procedure vartyp(var sp: psymbol; var ad: address; var p: integer);
var tdc: parctl; fnd, act: boolean; foff: address; ad2, ad3: address;
    enum: boolean; s, e: integer; sz: integer; ma: address; i: integer;
    fp: integer; ps: integer;

function siztyp: integer; forward;

function sizlst: integer;
var i, x: integer; c: char; sz, sz2, mxsz: integer;
begin
  sz := 0;
  expect(tdc, '('); 
  while chkchr(tdc) <> ')' do begin
    getsym(tdc); expect(tdc, ':'); getnum(tdc, i); expect(tdc, ':');
    sz := sz+siztyp;
    if chkchr(tdc) = '(' then begin nxtchr(tdc); mxsz := 0;
      { tagfield, parse sublists }
      while chkchr(tdc) <> ')' do begin
        getnum(tdc, i); sz2 := sizlst; if sz2 > mxsz then mxsz := sz2
      end;
      expect(tdc, ')');
      sz := sz+mxsz
    end;
    c := chkchr(tdc); if c = ',' then begin nxtchr(tdc); write(',') end
  end;
  expect(tdc, ')');
  sizlst := sz
end;

function siztyp{: integer};
var sz: integer; enum: boolean; s, e: integer;
begin
  case chkchr(tdc) of
    'i': begin nxtchr(tdc); sz := intsize end;
    'b': begin nxtchr(tdc); sz := boolsize end;
    'c': begin nxtchr(tdc); sz := charsize end;
    'n': begin nxtchr(tdc); sz := realsize end;
    'x': begin getrng(tdc, enum, s, e); if not enum then nxtchr(tdc); 
           if isbyte(s) and isbyte(e) then sz := 1 else sz := intsize;
         end;
    'p': begin nxtchr(tdc); sz := ptrsize end;
    's': begin nxtchr(tdc); sz := setsize end;
    'a': begin nxtchr(tdc); getrng(tdc, enum, s, e); { get range of index }
           if not enum then nxtchr(tdc); sz := siztyp*(e-s+1)
         end;
    'r': begin nxtchr(tdc); sz := sizlst end; 
    'e': begin nxtchr(tdc); sz := exceptsize end;
  end;
  siztyp := sz { return size }
end;

procedure matrec(isact: boolean);
var i, x, sz: integer; c: char; off: address;

function strmat: boolean;
var i: integer;
begin i := 1; 
  while (lcase(sn[i]) = lcase(sn2[i])) and (sn[i] <> ' ') and (sn2[i] <> ' ') do
    i := i+1;
  strmat := (sn[i] = ' ') and (sn2[i] = ' ')
end;

begin { matrec }
  expect(tdc, '('); 
  while chkchr(tdc) <> ')' do begin
    getsym(tdc); expect(tdc, ':'); getnum(tdc, i); off := i; expect(tdc, ':');
    if strmat then 
      begin fnd := true; foff := off; act := isact; fp := tdc.p end;
    sz := siztyp;
    if chkchr(tdc) = '(' then begin nxtchr(tdc);
      { tagfield, parse sublists }
      while chkchr(tdc) <> ')' do begin
        { need to fetch and check if this case is active }
        getnum(tdc, i); 
        { get actual tagfield according to size }
        if sz = 1 then x := getbyt(ad+off) else x := getint(ad+off);
        matrec((i = x) and isact);
      end;
      expect(tdc, ')')
    end;
    c := chkchr(tdc); if c = ',' then nxtchr(tdc);
  end;
  expect(tdc, ')')
end;

begin { vartyp }
  getsym(dbc); symadr(sp, ma);
  if sp = nil then 
    begin writeln('*** symbol not found in current context(s)'); goto 2 end;
  if sp^.styp <> stglobal then
    begin writeln('*** Locals not implemented'); goto 2 end;
  ad := pctop+syp^.off; { find address }
  setpar(tdc, sp^.digest, p); { set up type digest for parse }
  while chkchr(dbc) in ['.', '[', '^'] do begin
    if chkchr(dbc) = '.' then begin { record field }
      if chkchr(tdc) <> 'r' then 
        begin writeln('*** Type is not record'); goto 2 end;
      nxtchr(dbc); nxtchr(tdc); getsym(dbc); sn2 := sn; { get field and save }
      fnd := false; act := false; matrec(true);
      if not fnd then begin writeln('*** Record field not found'); goto 2 end
      else if not act then 
        begin writeln('*** Record field not active'); goto 2 end;
      ad := ad+foff; { set field address }
      tdc.p := fp { set type position }
    end else if chkchr(dbc) = '[' then begin { array index }
      if chkchr(tdc) <> 'a' then
        begin writeln('*** Type is not array'); goto 2 end;
      nxtchr(dbc); nxtchr(tdc); getrng(tdc, enum, s, e);
      if not enum then nxtchr(tdc);
      getnum(dbc, i); expect(dbc, ']');
      if (i < s) or (i > e) then
        begin writeln('*** Index out of range'); goto 2 end;
      ps := tdc.p; sz := siztyp; ad := ad+(i-s)*sz; { find element address }
      tdc.p := ps { back to base type }
    end else if chkchr(dbc) = '^' then begin { pointer dereference }
      writeln('*** Pointer dereference not implemented');
      goto 2
    end
  end;
  p := tdc.p { put digest position }
end;

{ process expression or structured reference }
procedure exptyp(var sp: psymbol; var ad: address; var p, i: integer; 
                 var simple: boolean; var undef: boolean);
type opstr = packed array [1..4] of char;

var r: integer;

function matop(os: opstr): boolean;
var i,ps: integer;
begin
   i := 1; ps := dbc.p;
   while (os[i] = chkchr(dbc)) and (os[i] <> ' ') do
     begin i := i+1; nxtchr(dbc) end;
   matop := (os[i] = ' ') and 
            not (chkchr(dbc) in ['a'..'z', 'A'..'Z', '_','0'..'9']);
   dbc.p := ps
end;

procedure sexpr(var sp: psymbol; var ad: address; var p, i: integer; 
                 var simple: boolean; var undef: boolean);
var r: integer; c: char;

procedure term(var sp: psymbol; var ad: address; var p, i: integer; 
                 var simple: boolean; var undef: boolean);
var r: integer;

procedure factor(var sp: psymbol; var ad: address; var p, i: integer; 
                 var simple: boolean; var undef: boolean);
var tdc: parctl; enum: boolean; s, e: integer;               
begin undef := false; skpspc(dbc);
  if chkchr(dbc) in ['$','&','%','0'..'9'] then
    begin getnum(dbc, i); simple := true end
  else if matop('not ') then begin
    nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); factor(sp, ad, p, i, simple, undef);
    if undef then begin writeln('*** operand is undefined'); goto 2 end;
    if not simple then 
      begin writeln('*** Structured operand to operator'); goto 2 end;
    i := bnot(i)
  end else if chkchr(dbc) in ['a'..'z', 'A'..'Z', '_'] then begin
    p := 1; vartyp(sp, ad, p);
    setpar(tdc, sp^.digest, p); { set up type digest for parse }
    if chkchr(tdc) in ['i', 'b','c','p','x'] then begin
      { scalar }
      simple := true;
      if getdef(ad) then case chkchr(tdc) of
        'i','p': i := getint(ad);
        'b','c': i := getbyt(ad);
        'x': begin nxtchr(tdc); getrng(tdc, enum, s, e); 
                   if not enum then nxtchr(tdc);
                   if isbyte(s) and isbyte(e) then i := getbyt(ad) 
                   else i := getint(ad)
             end
      end else undef := true
    end else simple := false;
    p := tdc.p
  end else if chkchr(dbc) = '(' then begin
    nxtchr(dbc); exptyp(sp, ad, p, i, simple, undef);
    expect(dbc, ')')
  end else begin
    writeln('*** Error in factor'); goto 2
  end
end;

procedure right;
begin factor(sp, ad, p, r, simple, undef);
  if undef then begin writeln('*** operand is undefined'); goto 2 end;
  if not simple then 
    begin writeln('*** Structured operand to operator'); goto 2 end
end;

begin factor(sp, ad, p, i, simple, undef); 
  skpspc(dbc);
  while (chkchr(dbc) in ['*', '/']) or matop('div ') or matop('mod ') or 
     matop('and ') do begin { operator }
    if undef then begin writeln('*** operand is undefined'); goto 2 end;
    if not simple then 
      begin writeln('*** Structured operand to operator'); goto 2 end;
    if chkchr(dbc) = '*' then begin nxtchr(dbc); right; i := i*r end
    else if chkchr(dbc) = '/' then
      begin writeln('*** Reals not implemented'); goto 2 end 
    else if matop('div  ') then 
      begin nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); right; i := i div r end
    else if matop('mod  ') then 
      begin nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); right; i := i mod r end
    else if matop('and  ') then 
      begin nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); right; i := band(i,r) end;
    skpspc(dbc)
  end
end;

procedure right;
begin term(sp, ad, p, r, simple, undef);
  if undef then begin writeln('*** operand is undefined'); goto 2 end;
  if not simple then 
    begin writeln('*** Structured operand to operator'); goto 2 end
end;

begin skpspc(dbc); c := chkchr(dbc); if c in ['+','-'] then nxtchr(dbc);
  term(sp, ad, p, i, simple, undef); 
  if c in ['+','-'] then begin
    if undef then begin writeln('*** operand is undefined'); goto 2 end;
    if not simple then begin writeln('*** Operand must be integer'); goto 2 end;
    if c = '-' then i := -i;
  end;
  skpspc(dbc);
  while (chkchr(dbc) in ['+', '-']) or matop('or  ') or matop('xor ') do begin
    if undef then begin writeln('*** operand is undefined'); goto 2 end; 
    if not simple then 
      begin writeln('*** Structured operand to operator'); goto 2 end;
    if chkchr(dbc) = '+' then begin nxtchr(dbc); right; i := i+r end
    else if chkchr(dbc) = '-' then begin nxtchr(dbc); right; i := i-r end
    else if matop('or  ') then 
      begin nxtchr(dbc); nxtchr(dbc); right; i := bor(i, r) end
    else if matop('xor ') then 
      begin nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); right; i := bxor(i, r) end;
    skpspc(dbc)
  end
end;

procedure right;
begin sexpr(sp, ad, p, r, simple, undef);
  if undef then begin writeln('*** operand is undefined'); goto 2 end;
  if not simple then 
    begin writeln('*** Structured operand to operator'); goto 2 end
end;

begin sexpr(sp, ad, p, i, simple, undef); skpspc(dbc);
  if (chkchr(dbc) in ['=', '<', '>']) or matop('in') then begin { operator }
    if undef then begin writeln('*** operand is undefined'); goto 2 end;
    if not simple then 
      begin writeln('*** Structured operand to operator'); goto 2 end;
    if chkchr(dbc) = '=' then begin nxtchr(dbc); right; i := ord(i = r) end
    else if matop('<>  ') then 
      begin nxtchr(dbc); nxtchr(dbc); right; i := ord(i <> r) end
    else if matop('<=  ') then 
      begin nxtchr(dbc); nxtchr(dbc); right; i := ord(i <= r) end
    else if matop('>=  ') then 
      begin nxtchr(dbc); nxtchr(dbc); right; i := ord(i >= r) end
    else if chkchr(dbc) = '<' then begin nxtchr(dbc); right; i := ord(i < r) end
    else if chkchr(dbc) = '>' then begin nxtchr(dbc); right; i := ord(i > r) end
    else if matop('in  ') then begin
      writeln('*** Sets not implemented');
      goto 2
    end
  end
end;

{ process expression }
procedure expr(var i: integer);
var ad: address; sp: psymbol; p: integer; simple: boolean; undef: boolean;
begin
  { get complex or simple reference }
  exptyp(sp, ad, p, i, simple, undef);
  if undef then begin writeln('*** operand is undefined'); goto 2 end;
  if not simple then begin writeln('*** Value is structured'); goto 2 end;
end;

function watchno(ad: address): wthnum;
var wn: wthnum; wi: wthinx;
begin
  wn := 0; for wi := 1 to maxwth do if wthtbl[wi] = ad then wn := wi;
  watchno := wn
end;

begin { debug }
  if watchmatch then begin { a variable watch matched, handle special }
    watchmatch := false;
    fw := watchno(stoad); { get the watch number }
    if fw > 0 then begin
      { obviously system error if we don't find the watch, but just ignore }
      pc := startins; { return to instruction start }
      write('Watch variable: @'); wrthex(pc, 8); write(': '); 
      writev(output, wthsym[fw].sp^.name, lenpv(wthsym[fw].sp^.name));
      write('@'); wrthex(wthtbl[fw], 8); write(': ');
      ad := wthtbl[fw]; p := wthsym[fw].p;
      if not getdef(ad) then write('*') 
      else prttyp(ad, wthsym[fw].sp^.digest, p, false);
      stopwatch := false; { let instruction run }
      singleins;
      stopwatch := true;
      write(' -> ');
      ad := wthtbl[fw]; p := wthsym[fw].p; 
      if not getdef(ad) then write('*')
      else prttyp(ad, wthsym[fw].sp^.digest, p, false);
      writeln
    end;
    goto 3 { skip main section }
  end;
  if not debugstart then begin
    writeln;
    writeln('P6 debug mode');
    writeln
  end;
  getbrk;
  { if we broke on line, fix the line to point }
  if isbrk(pc) then begin { standing on a breakpoint }
      x := 0;
      for i := 1 to maxbrk do if brktbl[i].sa = pc then x := i;
      if x >= 1 then if brktbl[x].line > 0 then srclin := brktbl[x].line
  end;
  { if we broke on a source marker, execute it then back up.
    This is because break on source line always will do this, and we need the
    source line from the instruction. }
  if store[pc] = mrkins then begin singleins; pc := pc-mrkinsl end;
  dbgend := false;
  debugstart := true; { set we started }
  prthdr;
  2: { error reenter interpreter }
  repeat
    getlin(dbc);
    skpspc(dbc);
    if not chkend(dbc) then begin
      getnam(dbc);
      if cn = 'li        ' then begin { list instructions }
        s := 0; e := lsttop-1;
        skpspc(dbc); 
        if not chkend(dbc) then begin expr(i); s := i end;
        skpspc(dbc); 
        if chkchr(dbc) = ':' then 
          begin nxtchr(dbc); expr(i); e := s+i-1 end
        else if not chkend(dbc) then begin expr(i); e := i end;
        if e > lsttop-1 then e := lsttop-1;
        writeln('Addr    Op Ins            P  Q');
        writeln('----------------------------------');
        while s <= e do begin
          if isbrk(s) then write('b') else write(' ');
          if pc = s then write('*') else write(' ');
          write(' ');
          wrthex(s, maxdigh);
          lstins(s);
          writeln
        end
      end else if cn = 'd         ' then begin { dump memory }
        s := 0; e := lsttop;
        skpspc(dbc); if not chkend(dbc) then begin expr(i); s := i end;
        skpspc(dbc);
        if chkchr(dbc) = ':' then 
          begin nxtchr(dbc); expr(i); e := s+i-1 end
        else if not chkend(dbc) then begin expr(i); e := i end;
        if e > maxstr then e := maxstr;
        dmpmem(s, e)
      end else if cn = 'ds        ' then begin { dump storage specs }
        writeln;
        writeln('Storage areas occupied');
        writeln;
        write('Program     '); prtrng(0, pctop-1);
        write('Globals     '); prtrng(pctop, gbtop-1);
        write('Stack/Heap  '); prtrng(gbtop, cp-1);
        write('Constants   '); prtrng(cp, maxstr);
        writeln
      end else if cn = 'dd        ' then begin { dump displays }
        if noframe then 
          begin writeln; writeln('No displays active'); writeln end
        else begin
          i := 1; skpspc(dbc); if not chkend(dbc) then expr(i);
          s := mp;
          repeat dmpdsp(s); s := getadr(s+marksl); i := i-1
          until (i = 0) or (s = getadr(s+marksl))
        end
      end else if cn = 'b         ' then begin { place breakpoint source }
        expr(l); if l > maxsrc then writeln('*** Invalid source line')
        else begin
          if lintrk[l] < 0 then writeln('*** Invalid source line')
          else begin s := lintrk[l]; i := 1;
            while store[s] = mrkins do begin { walk over source line markers }
              { source markers should always be within valid code, but we bail
                on out of memory store or taking too long to find code to keep
                this from locking up }
              if (s > maxstr) or (i > 100) then 
                begin writeln('*** Could not place breakpoint'); goto 2 end;
              s := s+mrkinsl; i := i+1
            end;
            x := 0; for i := 1 to maxbrk do if brktbl[i].sa < 0 then x := i;
            if x = 0 then writeln('*** Breakpoint table full')
            else brktbl[x].sa := s; brktbl[x].line := l
          end
        end
      end else if cn = 'bi        ' then begin { place breakpoint instruction }
        expr(i); s := i;
        x := 0; for i := 1 to maxbrk do if brktbl[i].sa < 0 then x := i;
        if x = 0 then writeln('*** Breakpoint table full')
        else brktbl[x].sa := s; brktbl[x].line := 0
      end else if cn = 'c         ' then begin { clear breakpoint }
        skpspc(dbc); if not chkend(dbc) then begin
          expr(i); s := i; i := 0;
          for i := 1 to maxbrk do if brktbl[i].sa = s then x := i;
          if i = 0 then writeln('*** No breakpoint at address')
          else brktbl[x].sa := -1
        end else for i := 1 to maxbrk do brktbl[i].sa := -1
      end else if cn = 'lb        ' then begin { list breakpoints }
        writeln;
        writeln('Breakpoints:');
        writeln;
        for i := 1 to maxbrk do if brktbl[i].sa >= 0 then begin
          if brktbl[i].line > 0 then write(i:2, ':', brktbl[i].line:4, ': ')
          else write(i:2, ':****', ': ');
          wrthex(brktbl[i].sa, maxdigh); writeln
        end;
        writeln
      end else if (cn = 'si        ') or
                  (cn = 'sis       ') then begin { step instruction }
        i := 1; skpspc(dbc); if not chkend(dbc) then expr(i);
        while i > 0 do begin 
          singleins; if cn = 'si        ' then prthdr; i := i-1;
          { if we hit break or stop, just stay on that instruction }
          if breakins then begin
            writeln('*** Break instruction hit');
            pc := pc-1; i := 0 
          end else if stopins then begin
            writeln('*** Stop instruction hit');
            pc := pc-1; i := 0
          end
        end
      end else if (cn = 'l         ') or
                  (cn = 'lc        ') then begin { list source }
        s := 0; e := maxsrc;
        skpspc(dbc); if not chkend(dbc) then begin expr(i); s := i end;
        skpspc(dbc);
        if chkchr(dbc) = ':' then 
          begin nxtchr(dbc); expr(i); e := s+i-1 end
        else if not chkend(dbc) then begin expr(i); e := i end;
        writeln;
        prtsrc(s, e, cn = 'lc        ');
        writeln
      end else if (cn = 's         ') or
                  (cn = 'ss        ') then begin { step source line }
        i := 1; skpspc(dbc); if not chkend(dbc) then expr(i);
        while i > 0 do begin
          repeat singleins until stopins or sourcemark; 
          singleins; if cn = 's         ' then prthdr; i := i-1;
          { if we hit break or stop, just stay on that instruction }
          if breakins then begin
            writeln('*** Break instruction hit');
            pc := pc-1; i := 0 
          end else if stopins then begin
            writeln('*** Stop instruction hit');
            pc := pc-1; i := 0
          end
        end
      end else if cn = 'p         ' then begin { print (various) }
        { process variable/expression reference }
        exptyp(syp, s, x, i, sim, undef); 
        writeln;
        if undef then write('*') { can't print, undefined }
        else if sim then write(i:1) { write simple result }
        else prttyp(s, syp^.digest, x, false); { print the resulting tail }
        writeln;
        writeln
      end else if cn = 'e         ' then begin { enter (hex) }
        expr(i); s := i; { get address }
        repeat
          expr(i); 
          if (i > 255) or (i < 0) then 
            begin writeln('*** Bad byte value'); s := -1 end
          else begin
            store[s] := i;
            s := s+1
          end
        until chkend(dbc) or (s < 0);
      end else if cn = 'st        ' then begin { set (variable) }
        vartyp(syp, ad, p); expr(i); setpar(tdc, syp^.digest, p);
        if chkchr(tdc) in ['i', 'b','c','p','x'] then begin
          case chkchr(tdc) of
            'i','p': putint(ad, i);
            'b','c': putbyt(ad, i);
            'x': begin nxtchr(tdc); getrng(tdc, enum, si, ei); 
                   if not enum then nxtchr(tdc);
                   if isbyte(si) and isbyte(ei) then putbyt(ad, i)
                   else putint(ad, i)
             end
          end
        end else begin writeln('*** Cannot set complex type'); goto 2 end
      end else if cn = 'w         ' then begin { watch (variable) }
        vartyp(syp, ad, p); setpar(tdc, syp^.digest, p);
        { find free watch entry }
        fw := 0; for wi := maxwth downto 1 do if wthtbl[wi] < 0 then fw := wi;
        if fw = 0 then begin writeln('*** Watch table full'); goto 2 end;
        wthtbl[fw] := ad; wthsym[fw].sp := syp; wthsym[fw].p := p
      end else if cn = 'lw        ' then begin { list watch table }
        writeln;
        writeln('Watch table:');
        writeln;  
        for wi := 1 to maxwth do if wthtbl[wi] >= 0 then
          begin write(wi:1, ': '); wrthex(wthtbl[wi], 8); writeln end;
        writeln
      end else if cn = 'cw        ' then begin { clear watch table }
        if not chkend(dbc) then begin expr(i);
          if (i < 1) or (i > maxwth) then
            begin writeln('*** Invalid watch number'); goto 2 end;
          { move entries down to gap }
          for x := 1 to maxwth-1 do wthtbl[x] := wthtbl[x+1];
          wthtbl[maxwth] := -1
        end else for wi := 1 to maxwth do wthtbl[wi] := -1
      end else if cn = 'pg        ' then begin { print globals }
        writeln('*** Command not implemented')
      end else if cn = 'pl        ' then begin { print locals }
        writeln('*** Command not implemented')  
      end else if cn = 'hs        ' then repspc { report heap space }
      else if cn = 'pc        ' then begin { set pc }
        if not chkend(dbc) then begin expr(i); pc := i end
        else begin
          writeln;
          write('pc: '); wrthex(pc, 8); writeln;
          writeln
        end  
      end else if cn = 'sp        ' then begin { set sp }
        if not chkend(dbc) then begin expr(i); sp := i end
        else begin
          writeln;
          write('sp: '); wrthex(sp, 8); writeln;
          writeln
        end  
      end else if cn = 'mp        ' then begin { set mp }
        if not chkend(dbc) then begin expr(i); mp := i end
        else begin
          writeln;
          write('mp: '); wrthex(mp, 8); writeln;
          writeln
        end  
      end else if cn = 'np        ' then begin { set np }
        if not chkend(dbc) then begin expr(i); np := i end
        else begin
          writeln;
          write('np: '); wrthex(np, 8); writeln;
          writeln
        end  
      end else if cn = 'cp        ' then begin { set cp }
        if not chkend(dbc) then begin expr(i); cp := i end
        else begin
          writeln;
          write('cp: '); wrthex(cp, 8); writeln;
          writeln
        end  
      end else if cn = 'ti        ' then 
        dotrcins := true { trace instructions }
      else if cn = 'nti       ' then 
        dotrcins := false { no trace instructions }
      else if cn = 'tr        ' then 
        dotrcrot := true { trace routine executions }
      else if cn = 'ntr       ' then 
        dotrcrot := false { no trace routine executions }
      else if cn = 'ts        ' then 
        dotrcsrc := true { trace source lines }
      else if cn = 'nts       ' then 
        dotrcsrc := false { no trace source lines }  
      else if cn = 'ps        ' then prthdr { print status } 
      else if cn = 'r         ' then dbgend := true
      else if cn = 'q         ' then goto 1
      else if (cn = 'h         ') or
              (cn = 'help      ') then begin
        writeln;
        writeln('Commands:');
        writeln;
        writeln('l   [s[ e|:l]  List source lines');
        writeln('lc  [s[ e|:l]  List source and machine lines coordinated');
        writeln('li  [s[ e|:l]  List machine instructions');
        writeln('d   [s[ e|:l]  Dump memory');
        writeln('e   a v[ v]... Enter byte values to memory address');
        writeln('st  d v        Set program variable');
        writeln('ds             Dump storage parameters');
        writeln('dd  [s]        Dump display frames');
        writeln('b   a          Place breakpoint at source line number');
        writeln('bi  a          Place breakpoint at instruction');
        writeln('c   [a]        Clear breakpoint/all breakpoints');
        writeln('lb             List active breakpoints');
        writeln('w   a          Watch variable');
        writeln('lw             List watch table');
        writeln('cw  [n]        Clear watch table entry/all watch entries');
        writeln('s   [n]        Step next source line execution');
        writeln('ss  [n]        Step next source line execution silently');
        writeln('si  [n]        Step instructions');
        writeln('sis [n]        Step instructions silently');
        writeln('hs             Report heap space');
        writeln('pc  [a]        Set/print pc contents');
        writeln('sp  [a]        Set/print sp contents');
        writeln('mp  [a]        Set/print mp contents');
        writeln('np  [a]        Set/print np contents');
        writeln('cp  [a]        Set cp contents');
        writeln('ti             Turn instruction tracing on');
        writeln('nti            Turn instruction tracing off');
        writeln('tr             Turn system routine tracing on');
        writeln('ntr            Turn system routine tracing off');
        writeln('ts             Turn source line tracing on');
        writeln('nts            Turn source line tracing off');
        writeln('r              Run program from current pc');
        writeln('ps             Print current registers and instruction');
        writeln('q              Quit interpreter');
        writeln
      end
      { these are internal debugger commands } 
        else if cn = 'listline  ' then begin
        writeln;
        writeln('Defined line to address entries:');
        writeln;
        for i := 1 to maxsrc do if lintrk[i] >= 0 then begin
          write(i:4, ':'); wrthex(lintrk[i], 8); writeln
        end;
        writeln
      end else if cn = 'dumpsymbol' then begin
        writeln;
        writeln('Symbols:');
        writeln;
        bp := blklst;
        while bp <> nil do begin
          write('Block: '); writev(output, bp^.name, 20);
          write(' ');
          case bp^.btyp of
            btprog: write('program');
            btproc: write('procedure');
            btfunc: write('function')
          end;
          write(' '); wrthex(bp^.bstart, 8); write(' '); wrthex(bp^.bend, 8);
          writeln;
          writeln;
          syp := bp^.symbols;
          while syp <> nil do begin
            write('   Symbol: '); writev(output, syp^.name, 40);
            write(' ');
            case syp^.styp of
              stglobal: write('global');
              stlocal: write('local ')
            end; 
            write(' ', syp^.off:10, ' ');
            writev(output, syp^.digest, lenpv(syp^.digest));
            writeln; syp := syp^.next
          end;
          writeln;
          bp := bp^.next
        end;
        writeln
      end else writeln('*** Command error')
    end
  until dbgend;
  { single step past entry breakpoint (if it exists) }
  if isbrk(pc) then singleins;
  putbrk; { put back breakpoints }
  3: { exit from watch }
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
  if markfv = 0 then;     
  if maxresult = 0 then;  
  if ordminchar = 0 then; 
  if ordmaxchar = 0 then; 
  if stackelsize = 0 then; 

  write('P6 Pascal interpreter vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  writeln;

  { find integer parameters }
  i := maxint;  maxdig := 0;
  while i > 0 do begin maxdig := maxdig+1; i := i div 10 end;

  for c1 := 'a' to 'z' do option[c1] := false;
  
  { preset options }
  dochkovf := true;  { check arithmetic overflow }
  dodmplab := false; { dump label definitions }
  dotrcrot := false; { trace routine executions }
  dotrcins := false; { trace instruction executions }
  dosrclin := true;  { add source line sets to code }
  dotrcsrc := false; { trace source line executions (requires dosrclin) }
  dorecycl := true;  { obey heap space recycle requests }
  dochkrpt := false; { check reuse of freed entry (automatically) }
  dochkdef := true;  { check undefined accesses }
  iso7185 := false;  { iso7185 standard mode }
  dodebug := false;  { no debug }
  dodbgflt := false; { no debug on fault }
  dodbgsrc := false; { no source level debug }

  strcnt := 0; { clear string quanta allocation count }
  blkstk := nil; { clear symbols block stack }
  blklst := nil; { clear symbols block discard list }
  
  { clear source filename }
  for i := 1 to fillen do srcfnm[i] := ' ';
  
  { get the command line }
  getcommandline(cmdlin, cmdlen);
  cmdpos := 1;
  
  { clear breakpoint table }
  for bi := 1 to maxbrk do brktbl[bi].sa := -1;
  
  { clear source line tracking }
  for i := 1 to maxsrc do lintrk[i] := -1;
  
  { clear watch table }
  for wi := 1 to maxwth do wthtbl[wi] := -1;
        
  { !!! remove this next statement for self compile }
  {elide}rewrite(prr);{noelide}

  { construct bit equivalence table }
  i := 1;
  for bai := 0 to 7 do begin bitmsk[bai] := i; i := i*2 end;
  
  for sdi := 0 to maxdef do storedef[sdi] := 0; { clear storage defined flags }

  writeln('Assembling/loading program');
  load; (* assembles and stores code *)
  
  pc := 0; sp := cp; np := gbtop; mp := cp; ep := 5; srclin := 1;
  expadr := 0; expstk := 0; expmrk := 0;
  
  { clear globals }
  for ad := pctop to gbtop-1 do begin store[ad] := 0; putdef(ad, false) end;

  { set breakpoint at 0 to kick off debugger }
  if dodebug then
    begin brktbl[1].sa := 0; brktbl[1].ss := store[0]; brktbl[1].line := 1; 
          store[0] := brkins end;
  
  debugstart := false;
  writeln('Running program');
  writeln;
  repeat
    stopins := false; { set no stop flag }
    breakins := false; { set no break instruction }
    sourcemark := false; { set no source line instruction }
    stopwatch := true; { set stop on watch match }
    watchmatch := false; { set no watch was matched }
    singleins;
    { if breakpoint hit, back up pc and go debugger }
    if breakins or (stopins and dodebug) or watchmatch then begin
      if stopins then begin writeln; writeln('*** Stop instruction hit') end; 
      pc := pc-1; breakins := false; stopins := false; writeln; 
      if not watchmatch then writeln('=== break ==='); 
      debug 
    end
  until stopins; { until stop instruction is seen }

  1 : { abort run }

  writeln;
  writeln('program complete');

end.
