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
* I80386 code generator for GCC                                                *
*                                                                              *
* This is the code generator backend for 32 bit I80386 processor model running *
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
      stackal     =        4;        { alignment of stack }
      stackelsize =        4   {8};  { stack element size }
      maxsize     =       32;        { this is the largest type that can be on
                                       the stack }
      { Heap alignment should be either the natural word alignment of the
        machine, or the largest object needing alignment that will be allocated.
        It can also be used to enforce minimum block allocation policy. }
      heapal      =        4;        { alignment for each heap arena }
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

        0:  Function return value, 64 bits, enables a full real result.
        8:  Static link.
        12: Dynamic link.
        16: Saved EP from previous frame.
        20: Stack bottom after locals allocate. Used for interprocdural gotos.
        24: EP from current frame. Used for interprocedural gotos.
        28: Return address

      }
      markfv      =        0   {0};  { function value }
      marksl      =        8   {8};  { static link }
      markdl      =        12  {16}; { dynamic link }
      markep      =        16  {24}; { (old) maximum frame size }
      marksb      =        20  {32}; { stack bottom }
      market      =        24  {40}; { current ep }
      markra      =        28  {48}; { return address }

      { ******************* end of pcom and pint common parameters *********** }

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

      stringlgth  = 1000;    { longest string length we can buffer }
      maxsp       = 45;      { number of predefined procedures/functions }
      maxins      = 255;     { maximum instruction code, 0-255 or byte }
      maxfil      = 100;     { maximum number of general (temp) files }
      maxalfa     = 10;      { maximum number of characters in alfa type }
      ujplen      = 5;       { length of ujp instruction (used for case jumps) }

      { coder parameters }
      maxreg      = 1000;    { maximum virtual registers to allocate }
      maxphy      = 6;       { maximum physical registers to allocate }

      { check flags: these turn on runtime checks }
      dochkovf    = true;    { check arithmetic overflow }

      { debug flags: turn these on for various dumps and traces }

      dodmpins    = false;    { dump instructions after assembly }
      dodmplab    = false;    { dump label definitions }
      dodmpsto    = false;    { dump storage area specs }
      dotrcrot    = false;    { trace routine executions }
      dotrcins    = false;    { trace instruction executions }
      dopmd       = true{false};    { perform post-mortem dump on error }
      dosrclin    = true;     { add source line sets to code }
      dotrcsrc    = false;    { trace source line executions (requires dosrclin) }
      dodmpspc    = false;    { dump heap space after execution }
      dorecycl    = true;     { obey heap space recycle requests }
      { invoke a special recycle mode that creates single word entries on
        recycle of any object, breaking off and recycling the rest. Once
        allocated, each entry exists forever, and accesses to it can be
        checked. }
      dochkrpt    = false;    { check reuse of freed entry (automatically
                                invokes dorecycl = false }
      dochkdef    = true;     { check undefined accesses }

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
      address     = -maxstr..maxstr; { address }

      beta        = packed array[1..25] of char; (*error message*)
      settype     = set of setlow..sethigh;
      alfainx     = 1..maxalfa; { index for alfa type }
      alfa        = packed array[alfainx] of char;
      byte        = 0..255; { 8-bit byte }
      bytfil      = packed file of byte; { untyped file of bytes }
      fileno      = 0..maxfil; { logical file number }

var   pc          : address;   (*program address register*)
      pctop,lsttop: address;   { top of code store }
      op : instyp; p : lvltyp; q : address;  (*instruction register*)
      q1: address; { extra parameter }
      store       : packed array [0..maxstr] of byte; { complete program storage }
      storedef    : packed array [0..maxdef] of byte; { defined bits }
      sdi         : 0..maxdef; { index for that }
      cp          : address;  (* pointer to next free constant position *)
      mp,sp,np,ep : address;  (* address registers *)
      (*mp  points to beginning of a data segment
        sp  points to top of the stack
        ep  points to the maximum extent of the stack
        np  points to top of the dynamically allocated area*)
      bitmsk      : packed array [0..7] of byte; { bits in byte }

      interpreting: boolean;

      { !!! remove this next statement for self compile }
      {elide}prd,prr     : text;{noelide}(*prd for read only, prr for write only *)

      instr       : array[instyp] of alfa; (* mnemonic instruction codes *)
      sptable     : array[0..maxsp] of alfa; (*standard functions and procedures*)
      insp        : array[instyp] of boolean; { instruction includes a p parameter }
      insq        : array[instyp] of 0..16; { length of q parameter }
      srclin      : integer; { current source line executing }

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
         instr[  8]:='---       '; insp[  8] := false; insq[  8] := 0;
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

         { sav (mark) and rst (release) were removed }
         sptable[ 0]:='get       ';     sptable[ 1]:='put       ';
         sptable[ 2]:='---       ';     sptable[ 3]:='rln       ';
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

         pc := begincode;
         cp := maxstr; { set constants pointer to top of storage }
         for i:= 1 to 10 do word[i]:= ' ';
         for i:= 0 to maxlabel do
             with labeltab[i] do begin val:=-1; st:= entered end;
         { initalize file state }
         for i := 1 to maxfil do filstate[i] := fclosed;

         { !!! remove this next statement for self compile }
         {elide}reset(prd);{noelide}

         iline := 1 { set 1st line of intermediate }
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
   begin
      again := true;
      while again do
            begin if eof(prd) then errorl('unexpected eof on input  ');
                  getnxt;(* first character of line*)
                  if not (ch in ['i', 'l', 'q', ' ', '!', ':']) then
                    errorl('unexpected line start    ');
                  case ch of
                       'i': getlin;
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

                            end
                  end;
            end
   end; (*generate*)

   procedure assemble; (*translate symbolic code into machine code and store*)
   
      const
        insmax = 30;
      type 
        { registers in target }
        reg = (rgnull, rgrax, rgrbx, rgrcx, rgrdx, rgrsi, rgrdi, rgrbp, rgrsp, 
               r8, r9, r10, r11, r12, r13, r14, r15
               rgal, rgbl, rgcl, rgdl);
        { stack and expression tree entries }
        expptr = ^expstk;
        expstk = record
                   op:   instyp; { operator type }
                   p :   lvltyp; q : address; { p and q parameters }
                   r1 :  integer; { registers }
                   l, r: expptr; { right and left links }
                   x1:   expptr; { extra link }
                 end;
    
        insstr = packed array [1..insmax] of char;
      var name :alfa; r :real; s :settype;
          i,x,s1,lb,ub,l:integer; c: char;
          str: packed array [1..stringlgth] of char; { buffer for string constants }
          t: integer; { [sam] temp for compiler bug }
          ep, ep2, ep3, estack, efree: expptr;
          r, r2: reg; ors: set of reg; rage: array [reg] of integer;
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
      
      procedure getexp(var ep: expptr);
      begin
        if efree <> nil then begin ep := efree; efree := ep^.l end
        else new(ep); 
        ep^.r := 0; ep^.op := op; ep^.p = p; ep^.q := q; ep^.l := nil; ep^.r := nil; 
      end;
      
      procedure putexp(ep: expptr);
      begin
        ep^.l := efree; efree := ep
      end;
      
      procedure pshstk(ep: expptr);
      begin
        ep^.l := estack; estack := ep
      end;
      
      procedure popstk(var ep: expptr);
      begin
        if estack = nil then errorl('Expression underflow   ');
        ep := estack; estack := estack^.l
      end;
      
      procedure deltre(ep: expptr);
      begin
        if ep^.l <> nil then deltre(ep^.l);
        if ep^.r <> nil then deltre(ep^.r);
        putexp(ep)
      end;
      
   begin  p := 0;  q := 0;  op := 0; estack := nil; efree := nil;
      getname;
      { note this search removes the top instruction from use }
      while (instr[op]<>name) and (op < maxins) do op := op+1;
      if op = maxins then errorl('illegal instruction      ');
       
      case op of  (* get parameters p,q *)

{ there are three classes of instructions, a load, and operator, and a store.
  A load is a leaf operation. An operator takes one or more operands. A store is
  terminal. A tree is constructed from leaves, operators and rooted at the
  store. At the store, the entire tree is disposed of. }

          0{lodi}, 193{lodx}, 105{loda}, 106{lodr}, 107{lods}, 108{lodb}, 
          109{lodc}, 4{loda}: begin read(prd,p,q); getexp(ep); pshstk(ep) end;

          28{adi}: begin getexp(ep); popstk(ep^.r); popstck(ep^.l); pshstk(ep) 
            end;
                                                 
          2{stri}, 70{stra}: begin read(prd,p,q); popstk(ep); dmptre(ep); 
            deltre(ep) end;
          195{strx}, 73{strb}, 74{strc}: begin
            read(prd,p,q); popstk(ep); dmptre(ep); deltre(ep)
          end;
          71{strr}: begin read(prd,p,q); popstk(ep); dmptre(ep); deltre(ep)
            end;
          72{strs}: begin read(prd,p,q); popstk(ep); dmptre(ep) end;

          120{lip}: begin read(prd,p,q); getexp(ep); pshstk(ep) end;

          12{cup}: begin read(prd,p); labelsearch end;

          11{mst}: begin read(prd,p) end;

          113{cip}: begin read(prd,p); popstk(ep); dmptre(ep) end;

          { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
          142, 148, 154, 160, 166, 172: begin read(prd,q); getexp(ep); 
            popstk(ep^.r); popstck(ep^.l); pshstk(ep) end;

          5{lao}: begin read(prd,p,q); getexp(ep); pshstk(ep) end;

          16{ixa}: begin read(prd,q); getexp(ep); popstk(ep^.r); popstck(ep^.l);
            pshstk(ep) end;

          55{mov}: begin read(prd,q); popstk(ep); popstk(ep2) end;


          117{dmp}: begin read(prd,q); popstk(ep); dmptre(ep) end;
          118{swp}: begin read(prd,q); popstk(ep); popstk(ep2); pshstk(ep);
            pshstk(ep2) end;

          {ldo}
          1, 65, 66, 67, 68, 69, 194: begin read(prd,q); getexp(ep); 
            pshstk(ep) end;

          {sro}
          3, 75, 76, 77, 78, 79, 196: begin read(prd,q); popstk(ep); 
            dmptre(ep) end;

          {ind}
          9, 85, 86, 87, 88, 89, 198: read(prd,q);

          {inc,dec}
          10, 90, 91, 92, 93, 94, 57, 103, 104, 201, 202: begin read(prd,q); 
            getexp(ep); popstk(ep^.l); pshstk(ep) end;

          {ckv}
          175, 179, 180, 203: begin read(prd,q); getexp(ep); popstk(ep^.r); 
            popstk(ep^.l) end;


          {cvbi,cvbx,cvbb,cvbc}
          100, 115, 116, 121: begin read(prd,q); read(prd,q1); getexp(ep); 
            popstk(ep^.r); popstk(ep^.l) end;

          {ivti,ivtx,ivtb,ivtc}
          192,101,102,111: begin read(prd,q); read(prd,q1); getexp(ep); 
            popstk(ep^.r); popstk(ep^.l) end;

          {cps}
          176: begin getexp(ep); popstk(ep^.r); popstk(ep^.l) end;

          {cpc}
          177: begin getexp(ep); popstk(ep^.r); popstk(ep^.l) end;

          {apc}
          178: begin popstk(ep); popstk(ep2); dmptre(ep2); dmptre(ep) end; 

          {pck, upk}
          63, 64: begin read(prd,q,q1); popstk(ep); popstk(ep2); popstk(ep3); 
            dmptre(ep3); dmptre(ep2); dmptre(ep3) end;

          {cta}
          191: begin read(prd,q, q1, q2); getexp(ep); popstk(ep^.l); 
            popstk(ep^.r); popstk(ep^.x1); pshstk(ep) end;


          {ujp}
          23: read(prd,q);

          {fjp,tjp,xjp}
          24,25,119: begin read(prd,q); popstk(ep); dmptre(ep) end;

          {ents,ente}
          13, 173: labelsearch;
          {ipj}
          112: begin read(prd,p); labelsearch end;

          {lpa}
          114: begin read(prd,p,q); getexp(ep); pshstk(ep) end;

          15 {csp}: begin skpspc; getname;
                           while name<>sptable[q] do
                           begin q := q+1; if q > maxsp then
                                 errorl('std proc/func not found  ')
                           end; 
                           { note the number of expression trees removed from 
                             the stack are determined by the routine number 
                             here }
                      end;

          7, 123, 124, 125, 126, 127 (*ldc*): begin case op of  (*get q*)
                           123: begin read(prd,i); getexp(ep); pshstk(ep) end;

                           124: begin read(prd,r); getexp(ep); pshstk(ep) end;

                           125: begin getexp(ep); pshstk(ep) end;

                           126: begin read(prd,q); getexp(ep); pshstk(ep) end;

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
                                  getexp(ep);
                                  pshstk(ep);
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
                                end
                           end (*case*)
                     end;

           {chk}
           26, 95, 97, 98, 99, 190, 199: begin read(prd,lb,ub); getexp(ep); 
             popstk(ep^.l) end;

           {vbs}
           92: begin read(prd,q); getexp(ep); popexp(ep^.l) end;

           {vbe}
           96:;

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
                         getexp(ep);
                         pshstk(ep);
                       end;

          {ret}
          14, 128, 129, 130, 131, 132, 204: begin getexp(ep); pshstk(ep) end;


          { equ,neq,geq,grt,leq,les with no parameter }
          17, 137, 138, 139, 140, 141,
          18, 143, 144, 145, 146, 147,
          19, 149, 150, 151, 152, 153,
          20, 155, 156, 157, 158, 159,
          21, 161, 162, 163, 164, 165,
          22, 167, 168, 169, 170, 171: begin getexp(ep); popstk(ep^.r); 
            popstk(ep^.l); pushstk(ep) end;

          {ord}
          59, 134, 136, 200: begin getexp(ep); popstk(ep^.l); pshstk(ep) end;

          {vip,vis}
          133, 122:; { ??? fill me in }

          {vin}
          226:; { ??? fill me in }

          {lcp}
          135: begin getexp(ep); popstk(ep^.l); pshstk(ep) end;

??? end
          6, 80, 81, 82, 83, 84, 197, {sto}

          { eof,adr,sbi,sbr,sgs,flt,flo,trc,ngi,ngr,sqi,sqr,abi,abr,not,and,
            ior,dif,int,uni,inn,mod,odd,mpi,mpr,dvi,dvr,stp,chr,rnd,rgs,fbv,
            fvb }
          27,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
          48,49,50,51,52,53,54,58,60,62,110,

          { dupi, dupa, dupr, dups, dupb, dupc, cks, cke, inv }
          181, 182, 183, 184, 185, 186,187,188,189: storeop;

                      (*ujc must have same length as ujp, so we output a dummy
                        q argument*)
          61 {ujc}: begin storeop; q := 0; storeq end

      end; (*case*)

      getlin { next intermediate line }

   end; (*assemble*)

begin (*load*)

   init;
   generate;
   pctop := pc; { save top of code store }
   lsttop := pctop; { save as top of listing }
   pc := 0;
   generate;
   alignu(stackal, pctop); { align the end of code for stack bottom }
   alignd(heapal, cp); { align the start of cp for heap top }

   if dodmpsto then begin { dump storage overview }

      writeln;
      writeln('Storage areas occupied');
      writeln;
      write('Program     '); wrthex(0, maxdigh); write('-'); wrthex(pctop-1, maxdigh);
      writeln(' (',pctop:maxdigd,')');
      write('Stack/Heap  '); wrthex(pctop, maxdigh); write('-'); wrthex(cp-1, maxdigh);
      writeln(' (',cp-pctop+1:maxdigd,')');
      write('Constants   '); wrthex(cp, maxdigh); write('-'); wrthex(maxstr, maxdigh);
      writeln(' (',maxstr-(cp):maxdigd,')');
      writeln

   end;
   if dodmplab then dmplabs { Debug: dump label definitions }

end; (*load*)


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

  write('P5 Pascal I80386/gcc 32 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  writeln;

  rewrite(prr);

  writeln('Generating program');
  load; (* assembles and stores code *)

  writeln;
  writeln('Program generation complete');

end.
