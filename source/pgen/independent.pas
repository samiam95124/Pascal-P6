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

module independent(input,output);

joins services;

uses endian,    { endian mode }
     mpb,       { machine parameter block }
     version,   { current version number }
     parcmd,    { command line parsing }
     registers; { cpu specific registers }

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
maxsp       = 111;     { number of predefined procedures/functions }
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
ctype = (cstr, creal, cset, ctmp, ctab, cint, cchr, cbol, cvalx, crst);
cstptr = ^cstrec; { pointer to string constant entry table }
cstrec = record 
  next: cstptr; 
  case ct: ctype of
      cstr:  (str: pstring; strl: integer; strn: integer);
      creal: (r:   real; realn: integer);
      cset:  (s:   settype; setn: integer);
      ctmp:  (ta:  array [1..maxtmp] of integer; tsize: integer; tn: integer);
      ctab:  (tb:  cstptr; csize: integer; cs: pstring; cn: integer);
      cint:  (i:   integer; intn: integer);
      cchr:  (c:   integer; chrn: integer);
      cbol:  (b:   integer; boln: integer);
      cvalx: (x:   integer; valxn: integer);
      crst:  ();
end;
psymbol     = ^symbol;
symbol      = record
                next:   psymbol; { next list symbol }
                name:   pstring; { name }
                { area type }
                styp:   (stglobal, stlocal, stparam, stfixg, stfixl);
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
{ intermediate instruction table }
inspar = array [instyp] of record
  instr: alfa;    { mnemonic instruction codes }
  insr:  integer; { number of stack words in result }
  insf:  boolean; { result is real }
  inss:  boolean  { result is set }
end;
{ standard functions and procedures table }
spfpar = array [sctyp] of record
  sptable : alfa; (*standard functions and procedures*)
  spfunc  : boolean; (*standard function or procedure is function*)
  sppar   : integer; (*standard functions and procedures number of 
                       parameters*)
  spkeep  : boolean; { keep the file parameter }
end;
labelst  = (entered,defined); (*label situation*)
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

fixed
{

  Instruction parameters table

  Notes:
  1. Instructions marked with "*" are for internal use only.
     The "*" mark both shows in the listing, and also prevents
     their use in the intermediate file, since only alpha
     characters are allowed as opcode labels.
  2. "---" entries are no longer used, but left here to keep the
     original instruction numbers from P4. They could be safely
     assigned to other instructions if the space is needed.
}
instab: inspar = array
       { memonic       SW RReal  RSet }
  record 'lodi      ', 1, false, false end,
  record 'ldoi      ', 1, false, false end,
  record 'stri      ', 0, false, false end,
  record 'sroi      ', 0, false, false end,
  record 'lda       ', 1, false, false end,
  record 'lao       ', 1, false, false end,
  record 'stoi      ', 0, false, false end,
  record 'ldcs      ', 1, false, true  end,
  record 'cjp       ', 0, false, false end,
  record 'indi      ', 1, false, false end,
  record 'inci      ', 1, false, false end,
  record 'mst       ', 0, false, false end,
  record 'cup       ', 0, false, false end,
  record 'rip       ', 0, false, false end,
  record 'retp      ', 0, false, false end,
  record 'csp       ', 0, false, false end,
  record 'ixa       ', 1, false, false end,
  record 'equa      ', 1, false, false end,
  record 'neqa      ', 1, false, false end,
  record 'brk*      ', 0, false, false end,
  record 'lnp*      ', 0, false, false end,
  record 'cal       ', 1, false, false end,
  record 'ret       ', 0, false, false end,
  record 'ujp       ', 0, false, false end,
  record 'fjp       ', 0, false, false end,
  record 'xjp       ', 0, false, false end,
  record 'chki      ', 1, false, false end,
  record 'cuv       ', 0, false, false end,
  record 'adi       ', 1, false, false end,
  record 'adr       ', 1, true,  false end, 
  record 'sbi       ', 1, false, false end,
  record 'sbr       ', 1, true,  false end, 
  record 'sgs       ', 1, false, true  end,
  record 'flt       ', 1, true,  false end,
  record 'flo       ', 2, true,  false end,
  record 'trc       ', 1, false, false end,
  record 'ngi       ', 1, false, false end,
  record 'ngr       ', 1, true,  false end, 
  record 'sqi       ', 1, false, false end,
  record 'sqr       ', 1, true,  false end, 
  record 'abi       ', 1, false, false end,
  record 'abr       ', 1, true,  false end, 
  record 'notb      ', 1, false, false end,
  record 'and       ', 1, false, false end,
  record 'ior       ', 1, false, false end,
  record 'dif       ', 1, false, true  end,
  record 'int       ', 1, false, true  end,
  record 'uni       ', 1, false, true  end,
  record 'inn       ', 1, false, false end,
  record 'mod       ', 1, false, false end,
  record 'odd       ', 1, false, false end,
  record 'mpi       ', 1, false, false end,
  record 'mpr       ', 1, true,  false end, 
  record 'dvi       ', 1, false, false end,
  record 'dvr       ', 1, true,  false end, 
  record 'mov       ', 0, false, false end,
  record 'lca       ', 1, false, false end,
  record 'deci      ', 1, false, false end,
  record 'stp*      ', 0, false, false end,
  record 'ordi      ', 1, false, false end,
  record 'chr       ', 1, false, false end,
  record 'ujc       ', 0, false, false end,
  record 'rnd       ', 1, false, false end,
  record 'pck       ', 0, false, false end,
  record 'upk       ', 0, false, false end,
  record 'ldoa      ', 1, false, false end,
  record 'ldor      ', 1, true,  false end, 
  record 'ldos      ', 1, false, true  end,
  record 'ldob      ', 1, false, false end,
  record 'ldoc      ', 1, false, false end,
  record 'stra      ', 0, false, false end,
  record 'strr      ', 0, false, false end,
  record 'strs      ', 0, false, false end,
  record 'strb      ', 0, false, false end,
  record 'strc      ', 0, false, false end,
  record 'sroa      ', 0, false, false end,
  record 'sror      ', 0, true,  false end, 
  record 'sros      ', 0, false, false end,
  record 'srob      ', 0, false, false end,
  record 'sroc      ', 0, false, false end,
  record 'stoa      ', 0, false, false end,
  record 'stor      ', 0, false, false end,
  record 'stos      ', 0, false, false end,
  record 'stob      ', 0, false, false end,
  record 'stoc      ', 0, false, false end,
  record 'inda      ', 1, false, false end,
  record 'indr      ', 1, true,  false end, 
  record 'inds      ', 1, false, true  end,
  record 'indb      ', 1, false, false end,
  record 'indc      ', 1, false, false end,
  record 'inca      ', 1, false, false end,
  record 'suv       ', 0, false, false end,
  record 'vbs       ', 0, false, false end,
  record 'incb      ', 1, false, false end,
  record 'incc      ', 1, false, false end,
  record 'chka      ', 1, false, false end,
  record 'vbe       ', 0, false, false end,
  record 'chks      ', 0, false, true  end,
  record 'chkb      ', 1, false, false end,
  record 'chkc      ', 1, false, false end,
  record 'cvbi      ', 2, false, false end,
  record 'ivtx      ', 2, false, false end,
  record 'ivtb      ', 2, false, false end,
  record 'decb      ', 1, false, false end,
  record 'decc      ', 1, false, false end,
  record 'loda      ', 1, false, false end,
  record 'lodr      ', 1, true,  false end, 
  record 'lods      ', 1, false, true  end,
  record 'lodb      ', 1, false, false end,
  record 'lodc      ', 1, false, false end,
  record 'rgs       ', 1, false, true  end,
  record 'ivtc      ', 2, false, false end,
  record 'ipj       ', 0, false, false end,
  record 'cip       ', 0, false, false end,
  record 'lpa       ', 2, false, false end,
  record 'cvbx      ', 2, false, false end,
  record 'cvbb      ', 2, false, false end,
  record 'dmp       ', 0, false, false end,
  record 'swp       ', 2, false, false end,
  record 'tjp       ', 0, false, false end,
  record 'lip       ', 2, false, false end,
  record 'cvbc      ', 2, false, false end,
  record 'vis       ', 1, false, false end,
  record 'ldci      ', 1, false, false end,
  record 'ldcr      ', 1, true,  false end, 
  record 'ldcn      ', 1, false, false end,
  record 'ldcb      ', 1, false, false end,
  record 'ldcc      ', 1, false, false end,
  record 'reti      ', 1, false, false end,
  record 'retr      ', 1, false, false end,
  record 'retc      ', 1, false, false end,
  record 'retb      ', 1, false, false end,
  record 'reta      ', 1, false, false end,
  record 'vip       ', 0, false, false end,
  record 'ordb      ', 1, false, false end,
  record 'lcp       ', 2, false, false end,
  record 'ordc      ', 1, false, false end,
  record 'equi      ', 1, false, false end,
  record 'equr      ', 1, false, false end,
  record 'equb      ', 1, false, false end,
  record 'equs      ', 1, false, false end,
  record 'equc      ', 1, false, false end,
  record 'equm      ', 1, false, false end,
  record 'neqi      ', 1, false, false end,
  record 'neqr      ', 1, false, false end,
  record 'neqb      ', 1, false, false end,
  record 'neqs      ', 1, false, false end,
  record 'neqc      ', 1, false, false end,
  record 'neqm      ', 1, false, false end,
  record 'geqi      ', 1, false, false end,
  record 'geqr      ', 1, false, false end,
  record 'geqb      ', 1, false, false end,
  record 'geqs      ', 1, false, false end,
  record 'geqc      ', 1, false, false end,
  record 'geqm      ', 1, false, false end,
  record 'grti      ', 1, false, false end,
  record 'grtr      ', 1, false, false end,
  record 'grtb      ', 1, false, false end,
  record 'grts      ', 1, false, false end,
  record 'grtc      ', 1, false, false end,
  record 'grtm      ', 1, false, false end,
  record 'leqi      ', 1, false, false end,
  record 'leqr      ', 1, false, false end,
  record 'leqb      ', 1, false, false end,
  record 'leqs      ', 1, false, false end,
  record 'leqc      ', 1, false, false end,
  record 'leqm      ', 1, false, false end,
  record 'lesi      ', 1, false, false end,
  record 'lesr      ', 1, false, false end,
  record 'lesb      ', 1, false, false end,
  record 'less      ', 1, false, false end,
  record 'lesc      ', 1, false, false end,
  record 'lesm      ', 1, false, false end,
  record '---       ', 0, false, false end,
  record 'mrkl*     ', 0, false, false end,
  record 'ckvi      ', 2, false, false end,
  record 'cps       ', 3, false, false end,
  record 'cpc       ', 3, false, false end,
  record 'aps       ', 0, false, false end,
  record 'ckvb      ', 2, false, false end,
  record 'ckvc      ', 2, false, false end,
  record 'dupi      ', 1, false, false end,
  record 'dupa      ', 2, false, false end,
  record 'dupr      ', 1, true,  false end, 
  record 'dups      ', 1, false, true  end,
  record 'dupb      ', 1, false, false end,
  record 'dupc      ', 1, false, false end,
  record 'cks       ', 2, false, false end,
  record 'cke       ', 0, false, false end,
  record 'inv       ', 0, false, false end,
  record 'ckla      ', 1, false, false end,
  record 'cta       ', 2, false, false end,
  record 'ivti      ', 2, false, false end,
  record 'lodx      ', 1, false, false end,
  record 'ldox      ', 1, false, false end,
  record 'strx      ', 0, false, false end,
  record 'srox      ', 1, false, false end,
  record 'stox      ', 0, false, false end,
  record 'indx      ', 1, false, false end,
  record 'chkx      ', 1, false, false end,
  record 'ordx      ', 1, false, false end,
  record 'incx      ', 1, false, false end,
  record 'decx      ', 1, false, false end,
  record 'ckvx      ', 2, false, false end,
  record 'retx      ', 1, false, false end,
  record 'noti      ', 1, false, false end,
  record 'xor       ', 1, false, false end,
  record 'bge       ', 4, false, false end,
  record 'ede       ', 0, false, false end,
  record 'mse       ', 0, false, false end,
  record 'apc       ', 0, false, false end,
  record 'cxs       ', 1, false, false end,
  record 'cxc       ', 2, false, false end,
  record 'lft       ', 2, false, false end,
  record 'max       ', 1, false, false end,
  record 'equv      ', 1, false, false end,
  record 'neqv      ', 1, false, false end,
  record 'lesv      ', 1, false, false end,
  record 'grtv      ', 1, false, false end,
  record 'leqv      ', 1, false, false end,
  record 'geqv      ', 1, false, false end,
  record 'vdp       ', 0, false, false end,
  record 'spc       ', 2, false, false end,
  record 'ccs       ', 2, false, false end,
  record 'scp       ', 0, false, false end,
  record 'ldp       ', 2, false, false end,
  record 'vin       ', 0, false, false end,
  record 'vdd       ', 0, false, false end,
  { ltc and lto are aliases to ldo and lao instructions }
  record 'ltci      ', 1, false, false end,
  record 'ltcr      ', 1, true,  false end, 
  record 'ltcs      ', 1, false, false end,
  record 'ltcb      ', 1, false, false end,
  record 'ltcc      ', 1, false, false end,
  record 'ltcx      ', 1, false, false end,
  record 'lto       ', 1, false, false end,
  record 'stom      ', 0, false, false end,
  record 'rets      ', 1, false, true  end,
  record 'retm      ', 1, false, false end,
  record 'ctb       ', 0, false, false end,
  record 'cpp       ', 1, false, false end,
  record 'cpr       ', 1, false, false end,
  record 'lsa       ', 1, false, false end,
  record 'eext*     ', 0, false, false end,
  record 'wbs       ', 1, false, false end,
  record 'wbe       ', 0, false, false end,
  record 'sfr       ', 0, false, false end,
  record 'cuf       ', 0, false, false end,
  record 'cif       ', 0, false, false end,
  record 'mpc       ', 2, false, false end,
  record 'cvf       ', 0, false, false end,
  record 'lsp       ', 2, false, false end,
  record 'cpl       ', 1, false, false end,
  record 'sfs       ', 0, false, false end,
  { sev is an alias for stra in pint. It has meaning to pgen. }
  record 'sev       ', 0, false, false end,
  record 'mdc       ', 1, false, false end,
  record '---       ', 0, false, false end
end;

{ 

Standard functions and procedures table 

}
sfptab: spfpar = array
   record 'get       ', false, 1, false end,
   record 'put       ', false, 1, false end,
   record 'thw       ', false, 1, false end,   
   record 'rln       ', false, 1, true end,
   record 'new       ', false, 2, false end,   
   record 'wln       ', false, 1, true end,
   record 'wrs       ', false, 4, true end,   
   record 'eln       ', true,  1, false end,
   record 'wri       ', false, 3, true end,   
   record 'wrr       ', false, 3, true end,
   record 'wrc       ', false, 3, true end,   
   record 'rdi       ', false, 2, true end,
   record 'rdr       ', false, 2, true end,   
   record 'rdc       ', false, 2, true end,
   record 'sin       ', true,  1, false end,   
   record 'cos       ', true,  1, false end,
   record 'exp       ', true,  1, false end,
   record 'log       ', true,  1, false end,
   record 'sqt       ', true,  1, false end,   
   record 'atn       ', true,  1, false end,
   record '---       ', false, 1, false end,   
   record 'pag       ', false, 1, false end,
   record 'rsf       ', false, 1, false end,   
   record 'rwf       ', false, 1, false end,
   record 'wrb       ', false, 3, true end,   
   record 'wrf       ', false, 4, true end,
   record 'dsp       ', false, 2, false end,   
   record 'wbf       ', false, 3, true end,
   record 'wbi       ', false, 2, true end,   
   record 'wbr       ', false, 2, true end,
   record 'wbc       ', false, 2, true end,   
   record 'wbb       ', false, 2, true end,
   record 'rbf       ', false, 3, true end,   
   record 'rsb       ', false, 1, false end,
   record 'rwb       ', false, 1, false end,   
   record 'gbf       ', false, 2, false end,
   record 'pbf       ', false, 2, false end,   
   record 'rib       ', false, 4, true end,
   record 'rcb       ', false, 4, true end,   
   record 'nwl       ', false, 3, false end, { special }
   record 'dsl       ', false, 3, false end, { special }
   record 'eof       ', true,  1, false end,
   record 'efb       ', true,  1, false end,   
   record 'fbv       ', false, 1, true end,
   record 'fvb       ', false, 2, true end,
   record 'wbx       ', false, 2, true end,
   record 'asst      ', false, 3, false end,
   record 'clst      ', false, 1, false end,
   record 'pos       ', false, 2, false end,
   record 'upd       ', false, 1, false end,
   record 'appt      ', false, 1, false end,
   record 'del       ', false, 2, false end,
   record 'chg       ', false, 4, false end,
   record 'len       ', true,  1, false end,
   record 'loc       ', true,  1, false end,
   record 'exs       ', true,  2, false end,
   record 'assb      ', false, 3, false end,
   record 'clsb      ', false, 1, false end,
   record 'appb      ', false, 1, false end,
   record 'hlt       ', false, 0, false end,
   record 'ast       ', false, 1, false end,
   record 'asts      ', false, 3, false end,
   record 'wrih      ', false, 3, true end,
   record 'wrio      ', false, 3, true end,
   record 'wrib      ', false, 3, true end,
   record 'wrsp      ', false, 3, true end,
   record 'wiz       ', false, 3, true end,
   record 'wizh      ', false, 3, true end,
   record 'wizo      ', false, 3, true end,
   record 'wizb      ', false, 3, true end,
   record 'rds       ', false, 3, true end,
   record 'ribf      ', false, 5, true end,
   record 'rdif      ', false, 3, true end,
   record 'rdrf      ', false, 3, true end,
   record 'rcbf      ', false, 5, true end,
   record 'rdcf      ', false, 3, true end,
   record 'rdsf      ', false, 4, true end,
   record 'rdsp      ', false, 3, true end,
   record 'aeft      ', false, 3, false end,
   record 'aefb      ', false, 3, false end,
   record 'rdie      ', false, 3, false end,
   record 'rdre      ', false, 3, false end,
   record 'rdx       ', false, 2, true end,
   record 'rdxf      ', false, 3, true end,
   record 'rxb       ', false, 4, true end,
   record 'rxbf      ', false, 5, true end,
   record 'rdsc      ', false, 3, true end,
   record 'rdih      ', false, 2, true end,
   record 'rdio      ', false, 2, true end,    
   record 'rdib      ', false, 2, true end,
   record 'rifh      ', false, 3, true end,    
   record 'rifo      ', false, 3, true end,
   record 'rifb      ', false, 3, true end,   
   record 'ribh      ', false, 4, true end,
   record 'ribo      ', false, 4, true end,    
   record 'ribb      ', false, 4, true end,
   record 'rbfh      ', false, 5, true end,
   record 'rbfo      ', false, 5, true end,
   record 'rbfb      ', false, 5, true end,
   record 'rdxh      ', false, 2, true end,
   record 'rdxo      ', false, 2, true end,
   record 'rdxb      ', false, 2, true end,
   record 'rxfh      ', false, 3, true end,    
   record 'rxfo      ', false, 3, true end,
   record 'rxfb      ', false, 3, true end,
   record 'rxbh      ', false, 4, true end,
   record 'rxbo      ', false, 4, true end,
   record 'rxbb      ', false, 4, true end,
   record 'rbxh      ', false, 5, true end,    
   record 'rbxo      ', false, 5, true end,
   record 'rbxb      ', false, 5, true end,
   record 'sete      ', false, 1, false end
end;

var   

op : instyp; p : lvltyp; q : address;  (*instruction register*)
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
domrklin: boolean; { mark source line translations in assembly }

{ other flags }
iso7185: boolean; { iso7185 standard flag }

prd,prr     : text; (*prd for read only, prr for write only *)
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
lindig      : boolean; { line diagnostic for gdb encountered }
errret      : boolean; { error flagged }

word:           array[alfainx] of char;
labeltab:       array[labelrg] of labelrec;
labelvalue:     address;
sline:          integer; { line number of Pascal source file }
iline:         integer; { line number of intermediate file }
sn:             labbuf;
snl:            1..lablen;
flablst:        flabelp; { list of far labels }
estack, efree:  expptr; { expression stack }
jmpstr:         expptr; { unresolved jump cache }
frereg, allreg: regset;
stacklvl:       integer;
expsn:          integer; { expression entries sn }
tmpoff:         address; { starting address of set temps offset in stack }
tmpspc:         address; { size of temps area }
tmplst:         tmpptr; { list of active temps }
tmpfre:         tmpptr; { free temp entries }
lclspc:         pstring; { label for locals space }
inplin:         linbuf; { buffer for input line }
inpinx:         lininx; { current input line position }
inplen:         0..maxlin; { length of input line }

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
begin
  str := strl(s, max(s))
end;

{ make string from padded string }
function strp(view s: string): pstring;
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

procedure init;
   var i: integer;
begin 
      for i:= 1 to 10 do word[i]:= ' ';
      for i:= 0 to maxlabel do
          with labeltab[i] do begin val:=-1; st:= entered; ref := nil; blk := nil end;

      { !!! remove this next statement for self compile }
      reset(prd);

      sline := 0; { set no line of source }
      iline := 0; { set no line of intermediate }
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
end;(*init*)

virtual procedure abort;

begin

  writeln('*** Abort procedure not defined');
  halt

end;

procedure wrtlin;
begin
  writeln(sline:6, ': ', iline:6, ': ', inplin:inplen);
  writeln(' ':6+2+6+2, '^': inpinx)
end;

procedure error(view es: string); (*error in loading*)
begin writeln; wrtlin;
   writeln('*** pgen: Program translation error: ', es);
   errret := true; { set there was an error }
   abort
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
var p: integer; digit: boolean; sc: packed array [1..1] of char;
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
  if ep^.free then error('System fault: dbl free');
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
   if labeltab[x].st=defined then error('Duplicated label')
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

procedure getlin;
var i: lininx;
begin
 for i := 1 to maxlin do inplin[i] := ' ';
 inpinx := 1;
 inplen := 0;
 if not eof(prd) then begin
   while not eoln(prd) do begin
     read(prd, inplin[inpinx]); 
     if inpinx = maxlin-1 then begin
       writeln; 
       writeln('*** ', sline:6, ': ', iline:6, 
               ': Input line too long, truncated');
       abort
     end else begin inplen := inplen+1; inpinx := inpinx+1 end
   end;
   readln(prd); 
   inpinx := 1;
   iline := iline+1; { next intermediate line }
!;writeln(sline:6, ': ', iline:6, ': ', inplin:inplen);
   writeln(prr, '# ', sline:6, ': ', iline:6, ': ', inplin:inplen)
 end
end;

function eofinp: boolean;
begin
  eofinp := (inplen = 0) and eof(prd)
end;

function eolinp: boolean;
begin
  if eofinp then eolinp := true
  else if inpinx > inplen then eolinp := true
  else eolinp := false
end;

function ch: char;
begin
  if not eolinp then ch := inplin[inpinx] else ch := ' '
end;

procedure getnxt; { get next character }
begin
  if not eolinp then inpinx := inpinx+1
end;

function chla: char;
var l: lininx;
begin
  l := inpinx; getnxt; chla := ch; inpinx := l
end;

procedure skpspc; { skip spaces }
begin
  while (ch = ' ') and not eolinp do getnxt
end;

procedure getint(var i: integer); { get integer }
var s: integer;
begin
  skpspc; i := 0; s := +1;
  if ch = '-' then s := -1;
  if ch in ['-', '+'] then getnxt;
  if not (ch in ['0'..'9']) then error('Number expected');
  while ch in ['0'..'9'] do begin
    i := i*10+(ord(ch)-ord('0'));
    getnxt
  end;
  i := i*s
end;

procedure getreal(var r: real); { get real }
var i: integer; { integer holding }
    e: integer; { exponent }
    d: integer; { digit }
    s: boolean; { sign }
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
  while (ch = ' ') and not eolinp do getnxt;
  { get any sign from number }
  if ch = '-' then begin getnxt; s := true end
  else if ch = '+' then getnxt;
  if not (ch in ['0'..'9']) then error('Invalid real number');
  while (ch in ['0'..'9']) do begin { parse digit }
     d := ord(ch)-ord('0');
     r := r*10+d; { add in new digit }
     getnxt
  end;
  if ch in ['.', 'e', 'E'] then begin { it's a real }
     if ch = '.' then begin { decimal point }
        getnxt; { skip '.' }
        if not (ch in ['0'..'9']) then error('Invalid real number');
        while (ch in ['0'..'9']) do begin { parse digit }
           d := ord(ch)-ord('0');
           r := r*10+d; { add in new digit }
           getnxt;
           e := e-1 { count off right of decimal }
        end;
     end;
     if ch in ['e', 'E'] then begin { exponent }
        getnxt; { skip 'e' }
        if not (ch in ['0'..'9', '+', '-']) then
           error('Invalid real number');
        getint(i); { get exponent }
        { find with exponent }
        e := e+i
     end;
     if e < 0 then r := r/pwrten(e) else r := r*pwrten(e)
  end;
  if s then r := -r
end;

procedure getlab;
var i: 1..lablen;
begin skpspc; for i := 1 to lablen do sn[i] := ' '; snl := 1;
  if not (ch in ['a'..'z','A'..'Z','_']) then
    error('Symbols format error');
  while ch in ['a'..'z','A'..'Z','0'..'9','_'] do begin
    if snl >= lablen then error('Symbols format error');
    sn[snl] := ch; getnxt; snl := snl+1
  end;
  snl := snl-1
end;

procedure getsds;
var i: 1..lablen;
begin skpspc; for i := 1 to lablen do sn[i] := ' '; snl := 1;
  if ch = ' ' then error('Symbols format error');
  while ch <> ' ' do begin
    if snl >= lablen then error('Symbols format error');
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
var j: integer; cs: packed array [1..1] of char;
begin fl := nil;
  getlab; if ch <> '.' then error('Symbols format error');
  getnxt; 
  if ch in ['0'..'9'] then getint(x) { near label }
  else begin { far label }
    fl := strp(sn); cat(fl, '.');
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

virtual procedure assemble;

begin

  writeln('*** Procedure assemble not defined');
  halt

end;

procedure generate; (*generate segment of code*)
   var x: integer; (* label number *)
       again: boolean;
       ch1: char;
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
       srcfil(maxflen): string; { name of input source file }
       p(fillen), n(fillen), e(fillen): string; { filename components }
       ad: address;
begin
   again := true; csttab := false;
   while again and not eofinp do begin
     getlin; { load next intermediate line }
     if not (ch in ['!', 'l', 'q', ' ', ':', 'o', 'g', 'b', 'e', 's', 'f',
                    'v', 't', 'n', 'x', 'c', 'p', 'r']) then
       error('unexpected line start');
     ch1 := ch; getnxt;
     case ch1 of
       '!': ;
       'l': begin parlab(x,ls); 
                  if ls <> nil then
                    error('Invalid intermediate');
                  if ch='=' then 
                    begin getnxt; getint(i); labelvalue := i; ispc := false end
                  else ispc := true;
                  update(x, ispc);
            end;
       'q': again := false;
       ' ': begin skpspc;
                  if not eolinp and (ch <> ' ') then assemble
            end;
       ':': begin { source line }
               getint(x); { get source line number }
               sline := x;
               if lindig then { gdb line diagnostic active }
                 writeln(prr, '        .loc 1 ', x:1, ' 1') { write debug line }
            end;
       'o': begin { option }
              skpspc;
              repeat
                if not (ch in ['a'..'z', 'A'..'Z', '_']) then
                  error('No valid option found');
                getlab; if snl > optlen then error('Option is too long');
                for i := 1 to optlen do os[i] := sn[i];
                oi := 1;
                while (oi < maxopt) and (os <> opts[oi]) and (os <> optsl[oi]) do
                  oi := oi+1;
                if (os = opts[oi]) or (os = optsl[oi]) then begin
                  ch1 := chr(oi+ord('a')-1);
                  option[oi] := true; 
                  if ch = '-' then option[oi] := false;
                  if (ch = '-') or (ch = '+') then getnxt;
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
                    28: domrklin   := option[oi];
                    2:; 3:; 4:; 12:; 20:; 21:; 22:;
                    24:; 25:; 26:; 10:; 18:;
                  end
                end else error('No valid option found');
                skpspc
              until not (ch in ['a'..'z'])
            end;
       'g': begin getint(i); gblsiz := i end; { set globals space }
       'b': begin { block start }
              skpspc;
              if not (ch in ['p', 'm', 'r', 'f']) then
                error('Block type is invalid');
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
              if ch1 in ['p', 'm'] then begin
                modnam := bp^.bname;
                preamble
              end;
              level := level+1; { count block levels }
              bp^.lvl := level { set }
            end;
       'e': begin 
              skpspc;
              if not (ch in ['p', 'm', 'r', 'f']) then
                error('Block type is invalid');
              if ch = 'p' then postamble;
              if blkstk = nil then error('System error');
              bp := blkstk;
              blkstk := blkstk^.next;
              bp^.next := blklst;
              blklst := bp;
              level := level-1 { count block levels }
            end;
      's': begin { symbol }
             getlab;
             new(sp); sp^.name := strp(sn);
             sn2 := sn; snl2 := snl;
             skpspc;
             if not (ch in ['g', 'l','p','f','c']) then
               error('Symbol type is invalid');
             if ch = 'g' then sp^.styp := stglobal
             else if ch = 'p' then sp^.styp := stparam
             else sp^.styp := stlocal;
             ch1 := ch;
             getnxt;
             skpspc;
             if not (ch in ['0'..'9','-']) then
               error('No offset found');
             sgn := ch = '-'; if ch = '-' then getnxt;
             ad := 0; while ch in ['0'..'9'] do
               begin
                 if ad <= maxstr div 10 then
                   ad := ad*10+ord(ch)-ord('0')
                 else error('Symbol offset > max');
                 getnxt
               end;
             if sgn then ad := -ad;
             sp^.off := ad; getsds;
             sp^.digest := strp(sn);
             if anyshort(blkstk) and (ch1 in ['g','l','p']) then begin
               wrtblks(blkstk, true, fl); 
               if ch1 = 'g' then 
                 writeln(prr, sn2:snl2, ' = globals_start+', ad:1)
               else
                 writeln(prr, sn2:snl2, ' = ', ad:1)
             end;
             if ch1 = 'g' then begin
               write(prr, '        .globl   ');
               wrtblks(blkstk, false, fl);
               writeln(prr, sn2:snl2);
               wrtblks(blkstk, false, fl); 
               writeln(prr, sn2:snl2, ' = globals_start+', ad:1)
             end else if ch1 in ['l','p'] then begin
               wrtblks(blkstk, false, fl); 
               writeln(prr, sn2:snl2, ' = ', ad:1)
             end;
             { place in block symbol list }
             sp^.next := blkstk^.symbols;
             blkstk^.symbols := sp
           end;
       'f': ; { source error count (unused) }
       'v': begin { variant logical table }
             skpspc;
             if ch <> 'l' then error('Label format error');
             getnxt; parlab(x,ls); 
             getint(vl);
             for vi := 1 to vl do getint(vt[vi]);
             update(x, true);
             write(prr, '        .quad   ', vl:1);
             for vi := 1 to vl do
               write(prr, ',', vt[vi]:1);
             writeln(prr)
            end;
       't': begin { template }
             skpspc;
             if ch <> 'l' then
               error('Label format error');
             getnxt; parlab(x,ls);
             if ls <> nil then
               error('Invalid intermediate');
             getint(l);
             new(cstp2); cstp2^.ct := ctmp; cstp2^.next := csttbl; 
             csttbl := cstp2;
             cstp2^.tsize := l; cstp2^.tn := x; ti := 1;
             while not eolinp do begin
               if ti = maxtmp then error('Too many template indexes');
               getint(tv); cstp2^.ta[ti] := tv; 
               ti := ti+1;
               { dump remainer }
               while not eolinp and (ch <= ' ') do getnxt
             end
           end;
      'n': begin { start constant table }
             if csttab then
               error('Already in constant table');
             csttab := true; { flag in table }
             skpspc;
             if ch <> 'l' then error('Label format error');
             getnxt; parlab(x,ls);
             getint(l); { note the size is unused }
             new(cstp); cstp^.ct := ctab; cstp^.tb := nil; 
             cstp^.next := csttbl; csttbl := cstp;
             cstp^.csize := l; cstp^.cn := x; cstp^.cs := ls
             { note mixed constants with other operands is
               neither encouraged nor forbidden }
           end;
      'x': begin
             if not csttab then
               error('No constant table active');
             cstp2 := cstp^.tb; cstp^.tb := nil;
             while cstp2 <> nil do begin
               cstp3 := cstp2; cstp2 := cstp2^.next;
               cstp3^.next := cstp^.tb; cstp^.tb := cstp3
             end;
             csttab := false
           end;
      'c': begin
             if not csttab then error('No constant table active');
             skpspc;
             if not (ch in ['i','r','p','s','c','b','x'])
               then error('Invalid const table type');
             ch1 := ch; getnxt;
             case ch1 of { constant type }
               'i': begin
                      getint(v); new(cstp2); 
                      cstp2^.ct := cint; cstp2^.next := cstp^.tb; 
                      cstp^.tb := cstp2; cstp2^.i := v; cstp2^.intn := 0
                    end;
               'r': begin
                      getreal(r); new(cstp2); 
                      cstp2^.ct := creal; cstp2^.next := cstp^.tb; 
                      cstp^.tb := cstp2; cstp2^.r := r; cstp2^.realn := 0
                    end;
               'p': begin
                      skpspc;
                      if ch <> '(' then error('''('' expected for set');
                      s := [ ]; getnxt;
                      while ch<>')' do
                        begin getint(i);
                          getnxt; s := s + [i] end;
                      new(cstp2); cstp2^.ct := cset;
                      cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                      cstp2^.s := s; cstp2^.setn := 0
                    end;
               's': begin
                      skpspc;
                      if ch <> '''' then error('quote expected for string');
                      getnxt; i := 1;
                      while ch<>'''' do
                        begin sn[i] := ch; i := i+1; getnxt end;
                      new(cstp2); cstp2^.ct := cstr;
                      cstp2^.next := cstp^.tb; cstp^.tb := cstp2;
                      cstp2^.str := strp(sn); cstp2^.strl := i-1; 
                      cstp2^.strn := 0
                    end;
               'c': begin
                      { chars are output as values }
                      getint(i); new(cstp2); 
                      cstp2^.ct := cchr; cstp2^.next := cstp^.tb; 
                      cstp^.tb := cstp2; cstp2^.c := i; cstp2^.chrn := 0
                    end;
               'b': begin
                      { booleans are output as values }
                      getint(i); new(cstp2); 
                      cstp2^.ct := cbol; cstp2^.next := cstp^.tb; 
                      cstp^.tb := cstp2; cstp2^.b := i; cstp2^.boln := 0
                    end;
               'x': begin
                      getint(v); new(cstp2); 
                      cstp2^.ct := cvalx; cstp2^.next := cstp^.tb; 
                      cstp^.tb := cstp2; cstp2^.x := v; cstp2^.valxn := 0
                    end;
             end
           end;
      'r': begin
             if not csttab then error('No constant table active');
             new(cstp2); cstp2^.ct := crst; 
             cstp2^.next := cstp^.tb; cstp^.tb := cstp2
           end;
      'p': begin { filename and path }
             skpspc;
             for i := 1 to max(srcfil) do srcfil[i] := ' ';
             skpspc; i := 1;
             while ch <> ' ' do begin 
               srcfil[i] := ch; getnxt; i := i+1;
               if i = max(srcfil) then error('filename to long')
             end;
             services.brknam(srcfil, p, n, e);
             if domrklin then begin
             writeln(prr, '        .file   "',n:*, '.', e:*, '"'); 
             writeln(prr, '        .file   1 "',srcfil:*, '"'); 
             lindig := true; { set line diagnostic active for gdb }
             end
           end;
    end
  end
end; (*generate*)

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
  if fp = nil then error('System error: tmp addr');
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
  if estack = nil then error('Expression underflow');
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

procedure labelsearch(var def: boolean; var val: integer; var sp: pstring; 
                     var blk: pblock);
var x: integer; flp: flabelp;
begin def := false; val := 0; flp := nil; blk := nil; skpspc; 
 if ch <> 'l' then error('Label format error');
 getnxt; parlab(x,sp);
 if sp <> nil then begin { far label }
   new(flp); flp^.next := flablst; flablst := flp; flp^.ref := sp
 end else begin { near label }
   if labeltab[x].ref = nil then putlabel(x);
   sp := labeltab[x].ref; def := labeltab[x].st = defined; 
   val := labeltab[x].val; blk := labeltab[x].blk
 end
end;(*labelsearch*)

procedure getname(var name: alfa);
var i: alfainx;
begin
 if eofinp then error('Unexpected eof on input');
 for i := 1 to maxalfa do word[i] := ' ';
 i := 1; { set 1st character of word }
 if not (ch in ['a'..'z']) then error('No operation label');
 while ch in ['a'..'z'] do begin
   if i = maxalfa then error('Opcode label is too long');
   word[i] := ch;
   i := i+1;
   getnxt { next character }
 end;
 pack(word,1,name)
end; (*getname*)

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
 write(f, ep^.op:3, ': ', instab[ep^.op].instr:4, ' ');
 if ep^.op = 15{csp} then write(f, ep^.q:1, ': ', sfptab[ep^.q].sptable:4) 
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
   errret := true; { set there was an error }
   abort
 end
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
begin if i > max(si) then error('Error in instruction     '); i := i+1 end;
 
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

procedure gencst;
var r: record case boolean of

       true:  (s: settype);
       false: (b: packed array [1..setsize] of byte);

    end;
procedure gencstlst(cp: cstptr); forward;
procedure gencstety(cp: cstptr);
var i: integer;
    ti: 1..maxtmp;
begin
  case cp^.ct of
    cstr: begin
      write(prr, '        .ascii  "');
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
    cbol: writeln(prr, '        .byte   ', cp^.b:1);
    cvalx: writeln(prr, '        .byte   ', cp^.x:1);
    crst: ;
  end
end;
procedure gencstlst(cp: cstptr);
var ad: address;
procedure align(a: integer);
begin
  while (ad mod a) <> 0 do begin
    writeln(prr, '        .byte   0');
    ad := ad+1
  end
end;
begin { gencstlst }
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
      crst: ad := 0;
    end;
    gencstety(cp);
    cp := cp^.next
  end
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
        if csttbl^.cs <> nil then begin
          writeln(prr, '        .globl  ', csttbl^.cs^);
          writeln(prr, csttbl^.cs^, ':')
        end else begin 
          write(prr, modnam^); write(prr, '.'); write(prr, csttbl^.cn:1, ':')
        end
      end;
      cint: writeln(prr, 'value', csttbl^.intn:1, ':');
      cchr: writeln(prr, 'character', csttbl^.chrn:1, ':');
      cbol: writeln(prr, 'boolean', csttbl^.boln:1, ':');
      cvalx: writeln(prr, 'byte_value', csttbl^.valxn:1, ':');
      crst: ;
    end;
    gencstety(csttbl);
    csttbl := csttbl^.next
  end
end;

{ translate intermediate file }
procedure xlate;

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
  for oi := 1 to maxopt do if options[oi] then
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
      28: domrklin   := option[oi];
      2:; 3:; 4:; 12:; 20:; 21:; 22:;
      24:; 25:; 26:; 10:; 18:;
    end
end;

procedure proginit;
var oi: 1..maxopt;
begin

  { Suppress unreferenced errors. }
  refer(adral);
  refer(adral);     
  refer(boolal);    
  refer(charmax);   
  refer(charal);     
  refer(codemax);    
  refer(filesize);   
  refer(intdig);     
  refer(ordminchar); 
  refer(ordmaxchar); 
  refer(stackelsize); 

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
  domrklin := true;  { mark assembly lines }

  { supress warnings }
  refer(dochkovf);
  refer(dodmplab);
  refer(dotrcrot);
  refer(dotrcins);
  refer(dosrclin);
  refer(dotrcsrc);
  refer(dorecycl);
  refer(dochkrpt);
  refer(donorecpar);
  refer(dochkdef);
  refer(iso7185);
  refer(dodebug);
  refer(dodbgflt);
  refer(dodbgsrc);
  refer(dosrcprf);
  refer(dochkcov);
  refer(doanalys);
  refer(dodckout);
  refer(dochkvbk);
  refer(dodbgchk);

  blkstk := nil; { clear symbols block stack }
  blklst := nil; { clear symbols block discard list }
  level := 0; { clear level count }
  lindig := false; { set no encounter line diagnostic }
  errret := false; { set no error on return }

  { supress warning }
  refer(blklst);

  fndpow(maxpow10, 10, decdig);
  fndpow(maxpow16, 16, hexdig);
  fndpow(maxpow8, 8, octdig);
  fndpow(maxpow2, 2, bindig); bindig := bindig+1; { add sign bit }

end;

procedure parcmdlin;

begin

  { get the command line }
  getcommandline;
  cmdpos := 1;
  paroptions; { parse command line options }
  { parse header files }
  parhdrfil(prd, prdval, '.p6 ');
  if not prdval then begin
    writeln('*** Error: input filename not found');
    errret := true; { set there was an error }
    abort
  end;
  paroptions; { parse command line options }
  parhdrfil(prr, prrval, '.s  ');
  if not prrval then begin
    writeln('*** Error: output filename not found');
    errret := true; { set there was an error }
    abort
  end;
  { load command line options }
  paroptions;
  plcopt; { place options }

end;

begin
end.
