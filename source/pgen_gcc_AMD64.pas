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
      labbuf      = packed array [1..lablen] of char; { label buffer }
      strbuf      = packed array [1..strlen] of char;
      { Here is the variable length string containment to save on space. strings
        are only stored in their length rounded to the nearest 10th. }
      strvsp = ^strvs; { pointer to variable length id string }
      strvs = record { id string variable length }
                str:   packed array [1..varsqt] of char; { data contained }
                next:  strvsp { next }
              end;
      strptr = ^strrec; { pointer to string constant entry table }
      strrec = record next: strptr; str: strvsp; strl, strn: integer end;

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
      sptable     : array[0..maxsp] of alfa; (*standard functions and procedures*)
      insp        : array[instyp] of boolean; { instruction includes a p parameter }
      insq        : array[instyp] of 0..32; { length of q parameter }
      srclin      : integer; { current source line executing }
      option      : array ['a'..'z'] of boolean; { option array }
      strtbl      : strptr; { string constant table }
      strnum      : integer; { string constant table label count }

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
                rgr8, rgr9, rgr10, rgr11, rgr12, rgr13, rgr14, rgr15);
         regset = set of reg;
         { stack and expression tree entries }
         expptr = ^expstk;
         expstk = record
                    next: expptr; { next entry link }
                    op:   instyp; { operator type }
                    p :   lvltyp; q : address; { p and q parameters }
                    r1, r2, r3 : reg; { registers }
                    l, r: expptr; { right and left links }
                    x1:   expptr; { extra link }
                    strn: integer; { string number }
                    vali: integer; { integer value }
                    valr: integer; { real value }
                  end;

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
         instr[ 19]:='brk*      '; insp[ 19] := false; insq[ 19] := 0;
         instr[ 20]:='lnp*      '; insp[ 20] := false; insq[ 20] := intsize;
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
         instr[174]:='mrkl*     '; insp[174] := false; insq[174] := intsize;
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
         instr[242]:='eext*     '; insp[242] := false; insq[242] := 0;

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
   
      const
        insmax = 30;

      var name :alfa; r :real; s :settype;
          i,x,s1,lb,ub,l:integer; c: char;
          str: strbuf; { buffer for string constants }
          sbp: strptr;
          t: integer; { [sam] temp for compiler bug }
          ep, ep2, ep3, ep4: expptr;
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
          rgnull: write(f, '<null>');
          rgrax:  write(f, 'rax');
          rgrbx:  write(f, 'rbx');
          rgrcx:  write(f, 'rcx');
          rgrdx:  write(f, 'rdx');
          rgrsi:  write(f, 'rsi');
          rgrdi:  write(f, 'rdi');
          rgrbp:  write(f, 'rbp');
          rgrsp:  write(f, 'rsp');
          rgr8:   write(f, 'r8');
          rgr9:   write(f, 'r9');
          rgr10:  write(f, 'r10');
          rgr11:  write(f, 'r11');
          rgr12:  write(f, 'r12');
          rgr13:  write(f, 'r13');
          rgr14:  write(f, 'r14');
          rgr15:  write(f, 'r15');
        end
      end;

      procedure getexp(var ep: expptr);
      begin
        if efree <> nil then begin ep := efree; efree := ep^.next end
        else new(ep); 
        ep^.next := nil; ep^.op := op; ep^.p := p; ep^.q := q; ep^.l := nil; 
        ep^.r := nil; ep^.r1 := rgnull; ep^.r2 := rgnull; ep^.r3 := rgnull;
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

      procedure assreg(ep: expptr; rf: regset; r1, r2: reg);
      begin
        case ep^.op of
          0{lodi}, 193{lodx}, 105{loda}, 106{lodr}, 107{lods}, 108{lodb}, 
          109{lodc}, 4{loda}: begin end;
          {adi,adr,sbi,sbr}
          28, 29, 30, 31: begin end;   
          120{lip}: begin end;   
          { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
          142, 148, 154, 160, 166, 172: begin ep^.r1 := r1; 
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            assreg(ep^.l, rf, r1, r2); assreg(ep^.r, rf, rgnull, rgnull) end;

          5{lao}: ep^.r1 := r1;

          16{ixa}: ;
          118{swp}: ;
          {ldo}
          1, 65, 66, 67, 68, 69, 194: ;
          {ind}
          9, 85, 86, 87, 88, 89, 198: ;
          {inc,dec}
          10, 90, 91, 93, 94, 57, 103, 104, 201, 202: ;
          {ckv}
          175, 179, 180, 203: ;
          {cvbi,cvbx,cvbb,cvbc}
          100, 115, 116, 121: ;
          {ivti,ivtx,ivtb,ivtc}
          192,101,102,111: ;
          {cps}
          176: ;
          {cpc}
          177: ;
          {cta}
          191: ;
          {lpa}
          114: ;

          {ldci,ldcc,ldcb}
          123,127,126: ep^.r1 := r1;

          {ldcn}
          125: ep^.r1 := r1;

          {ldcs,ldcr}
          7, 124: ;

           {chk}
           26, 95, 97, 98, 99, 190, 199: ;
           {vbs}
           92: ;
           {vbe}
           96:;
           56 {lca}: ep^.r1 := r1;

          {ord}
          59, 134, 136, 200: ep^.r1 := r1;

          {lcp}
          135: ;
          {sgs}
          32: ;
     
          {flt}
          33: ;

          {flo]
          34: ;

          {trc}
          35: ;

          {ngi,ngr}
          36,37: ;

          {sqi,sqr}
          38,39: ;

          {abi,abr}
          40,41: ;

          {notb,noti,odd,rnd,chr}
          42,50,60,62,205: ;

          {and,ior,xor,dif,int,uni,inn,mod,mpi,mpr,dvi,dvr,rgs}
          43,44,46,47,48,49,51,52,53,54,110,206: begin end;
          { dupi, dupa, dupr, dups, dupb, dupc }
          181, 182, 183, 184, 185, 186: ;

          {cks}
          187: ;
        end
      end;

      procedure genexp(ep: expptr);
      begin
        if ep <> nil then begin
          genexp(ep^.l); genexp(ep^.r); genexp(ep^.x1);
          case ep^.op of
            0{lodi}, 193{lodx}, 105{loda}, 106{lodr}, 107{lods}, 108{lodb}, 
            109{lodc}, 4{loda}: begin end;
            {adi,adr,sbi,sbr}
            28, 29, 30, 31: begin end;   
            120{lip}: begin end;   
            { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
            142, 148, 154, 160, 166, 172: begin end;      

            5{lao}: begin 
              write(prr, '        movq    $globals_start+', ep^.q:1, ',%');
              wrtreg(prr, ep^.r1); writeln(prr); 
              write(prr, '        movq    (%'); wrtreg(prr, ep^.r1); 
              write(prr, '),%'); wrtreg(prr, ep^.r1); writeln(prr) end;

            16{ixa}: begin end;
            118{swp}: begin end;
            {ldo}
            1, 65, 66, 67, 68, 69, 194: begin end;
            {ind}
            9, 85, 86, 87, 88, 89, 198: begin end;
            {inc,dec}
            10, 90, 91, 93, 94, 57, 103, 104, 201, 202: begin end;
            {ckv}
            175, 179, 180, 203: begin end;
            {cvbi,cvbx,cvbb,cvbc}
            100, 115, 116, 121: begin end;
            {ivti,ivtx,ivtb,ivtc}
            192,101,102,111: begin end;
            {cps}
            176: begin end;
            {cpc}
            177: begin end;
            {cta}
            191: begin end;
            {lpa}
            114: begin end;

            {ldci,ldcc,ldcb}
            123,127,126: begin 
              write(prr, '        movq    $',  ep^.vali:1, ',%');
              wrtreg(prr, ep^.r1); writeln(prr) end;

            {ldcn}
            125: begin write(prr, '        movq    $0,%'); 
              wrtreg(prr, ep^.r1); writeln(prr) end;

            {ldcs,ldcr}
            7, 124: ;

             {chk}
             26, 95, 97, 98, 99, 190, 199: begin end;
             {vbs}
             92: begin end;
             {vbe}
             96:;
             56 {lca}: begin 
               write(prr, '        leaq    string', ep^.strn:1, '(%rip),%'); 
               wrtreg(prr, ep^.r1); writeln(prr);
            end;

            {ord}
            59, 134, 136, 200: begin getexp(ep); popstk(ep^.l); pshstk(ep); 
              writeln(prr) end;
            {lcp}
            135: begin end;
            {sgs}
            32: begin end;
     
            {flt}
            33: begin end;

            {flo]
            34: begin end;

            {trc}
            35: begin end;

            {ngi,ngr}
            36,37: begin end;

            {sqi,sqr}
            38,39: begin end;

            {abi,abr}
            40,41: begin end;

            {notb,noti,odd,rnd,chr}
            42,50,60,62,205: begin end;

            {and,ior,xor,dif,int,uni,inn,mod,mpi,mpr,dvi,dvr,rgs}
            43,44,46,47,48,49,51,52,53,54,110,206: begin end;
            { dupi, dupa, dupr, dups, dupb, dupc }
            181, 182, 183, 184, 185, 186: begin end;

            {cks}
            187: begin end;
          end
        end
      end;

      procedure callsp;
      begin (*callsp*)
        if q > maxsp then errorl('Invalid std proc or func ');
        writeln(prr, '# ', sline:6, ': ', iline:6, ': ', q:3, ': -> ', sptable[q]);
        case q of
          0 (*get*): begin popstk(ep); popstk(ep2); dmptrel(ep, 19); 
            dmptrel(ep2, 19); deltre(ep); pshstk(ep2) end;
          1 (*put*): begin popstk(ep); popstk(ep2); dmptrel(ep, 19); dmptrel(ep2, 19); 
            deltre(ep); pshstk(ep2) end;
          3 (*rln*): begin popstk(ep); dmptrel(ep, 19); pshstk(ep2) end;
          39 (*nwl*): ;
          { do we need to preserve the file across calls? }
          5 (*wln*): begin popstk(ep); assreg(ep, [], rgrdi, rgnull); dmptrel(ep, 19); 
            genexp(ep); writeln(prr, '        call    _psystem_wln');
            pshstk(ep) end;
          6 (*wrs*): begin popstk(ep); popstk(ep2); popstk(ep3); popstk(ep4);
            assreg(ep4, frereg, rgrdi, rgnull); 
            assreg(ep2, frereg, rgrsi, rgnull);
            assreg(ep3, frereg, rgrdx, rgnull); 
            assreg(ep, frereg, rgrcx, rgnull);
            dmptrel(ep, 19); genexp(ep); dmptrel(ep2, 19); genexp(ep2); 
            dmptrel(ep3, 19); genexp(ep3); dmptrel(ep4, 19); genexp(ep4);
            writeln(prr, '        call    _psystem_wrs');
            deltre(ep); deltre(ep2); deltre(ep3); pshstk(ep4) end;
          65 (*wrsp*):;
          41 (*eof*):;
          42 (*efb*):;
          7 (*eln*):;
          8 (*wri*),
          62 (*wrih*),
          63 (*wrio*),
          64 (*wrib*),
          66 (*wiz*),
          67 (*wizh*),
          68 (*wizo*),
          69 (*wizb*):;
          9 (*wrr*):;
          10(*wrc*):;
          11(*rdi*),
          72(*rdif*):;
          37(*rib*),
          71(*ribf*):;
          12(*rdr*),
          73(*rdrf*):;
          13(*rdc*),
          75(*rdcf*):;
          38(*rcb*),
          74(*rcbf*):;
          14(*sin*):;
          15(*cos*):;
          16(*exp*):;
          17(*log*):  ;                  
          18(*sqt*):;
          19(*atn*):;
          20(*sav*): errorl('Invalid std proc or func ');
          21(*pag*):;
          22(*rsf*):;
          23(*rwf*):;
          24(*wrb*):;
          25(*wrf*):;
          26(*dsp*):;
          40(*dsl*):;
          27(*wbf*):;
          28(*wbi*):;
          45(*wbx*): ;
          29(*wbr*):;
          30(*wbc*):;
          31(*wbb*):;
          32(*rbf*):;
          33(*rsb*):;
          34(*rwb*):;
          35(*gbf*):;
          36(*pbf*):;
          43 (*fbv*): ;
          44 (*fvb*):;
          { extended Pascaline file handlers }
          46 (*asst*):;
          56 (*assb*):;
          47 (*clst*):;
          57 (*clsb*):;
          48 (*pos*):;
          49 (*upd*):;
          50 (*appt*):;
          58 (*appb*):;
          51 (*del*):;
          52 (*chg*):;
          53 (*len*):;
          54 (*loc*):;
          55 (*exs*):;
          59 (*hlt*):;
          60 (*ast*):;
          61 (*asts*):;
          70(*rds*),
          76(*rdsf*):;
          77(*rdsp*):;
          78(*aeft*):;
          79(*aefb*):;
          80(*rdie*):;
          81(*rdre*): ;
          2(*thw*): ;

        end;(*case q*)
   end;(*callsp*)

   begin { assemble } 
      p := 0;  q := 0;  op := 0;
      getname;
      { note this search removes the top instruction from use }
      while (instr[op]<>name) and (op < maxins) do op := op+1;
      if op = maxins then errorl('illegal instruction      ');
       
      write(prr, '# ', sline:6, ': ', iline:6, ': ', op:3, ': ', name:4, ' ');
      case op of  (* get parameters p,q *)

{ there are three classes of instructions, a load, and operator, and a store.
  A load is a leaf operation. An operator takes one or more operands. A store is
  terminal. A tree is constructed from leaves, operators and rooted at the
  store. At the store, the entire tree is disposed of. }

          0{lodi}, 193{lodx}, 105{loda}, 106{lodr}, 107{lods}, 108{lodb}, 
          109{lodc}, 4{loda}: begin read(prd,p,q); writeln(prr,p:1,' ', q:1);
            getexp(ep); pshstk(ep) end;

          {adi,adr,sbi,sbr}
          28, 29, 30, 31: begin getexp(ep); popstk(ep^.r); popstk(ep^.l); 
            pshstk(ep); writeln(prr) end;
                                                 
          2{stri}, 70{stra}: begin read(prd,p,q); writeln(prr,p:1,' ', q:1);
            popstk(ep); dmptre(ep); deltre(ep); botstk end;
          195{strx}, 73{strb}, 74{strc}: begin
            read(prd,p,q); writeln(prr,p:1,' ', q:1); popstk(ep); dmptre(ep);
            deltre(ep); botstk end;
          71{strr}: begin read(prd,p,q); writeln(prr,p:1,' ', q:1); popstk(ep);
            dmptre(ep); deltre(ep); botstk end;
          72{strs}: begin read(prd,p,q); writeln(prr,p:1,' ', q:1); popstk(ep);
            dmptre(ep); deltre(ep); botstk end;

          120{lip}: begin read(prd,p,q); writeln(prr,p:1,' ', q:1); getexp(ep);
            pshstk(ep) end;

          {cup,cuv}
          12, 27: begin read(prd,p); writeln(prr,p:1); labelsearch end;

          11{mst}: begin read(prd,p); writeln(prr,p:1); end;

          113{cip}: begin read(prd,p); writeln(prr,p:1); popstk(ep); dmptre(ep);
            deltre(ep); botstk end;

          { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
          142, 148, 154, 160, 166, 172: begin read(prd,q); writeln(prr,q:1); 
            getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) end;

          5{lao}: begin read(prd,q); writeln(prr,q:1); getexp(ep);
            pshstk(ep) end;

          16{ixa}: begin read(prd,q); writeln(prr,q:1); getexp(ep); 
            popstk(ep^.r); popstk(ep^.l); pshstk(ep) end;

          55{mov}: begin read(prd,q); writeln(prr,q:1); popstk(ep); 
            popstk(ep2); dmptre(ep); dmptre(ep2); deltre(ep); deltre(ep2) end;

          { dmp is a strange one. It is used for stack management, and needs to
            be checked against each situation it is used in. }
          117{dmp}: begin read(prd,q); writeln(prr,q:1); popstk(ep); dmptrel(ep, 19);
            deltre(ep); botstk end;

          118{swp}: begin read(prd,q); writeln(prr,q:1); popstk(ep); 
            popstk(ep2); pshstk(ep); pshstk(ep2) end;

          {ldo}
          1, 65, 66, 67, 68, 69, 194: begin read(prd,q); writeln(prr,q:1);
            getexp(ep); pshstk(ep) end;

          {sro}
          3, 75, 76, 77, 78, 79, 196: begin read(prd,q); writeln(prr,q:1);
            popstk(ep); dmptre(ep); deltre(ep); botstk end;

          {ind}
          9, 85, 86, 87, 88, 89, 198: begin read(prd,q); writeln(prr,q:1) end;

          {inc,dec}
          10, 90, 91, 93, 94, 57, 103, 104, 201, 202: begin read(prd,q); 
            writeln(prr,q:1); getexp(ep); popstk(ep^.l); pshstk(ep) end;

          {ckv}
          175, 179, 180, 203: begin read(prd,q); writeln(prr,q:1); getexp(ep);
            popstk(ep^.r); popstk(ep^.l) end;

          {cvbi,cvbx,cvbb,cvbc}
          100, 115, 116, 121: begin read(prd,q); writeln(prr,q:1); read(prd,q1);
            getexp(ep); popstk(ep^.r); popstk(ep^.l) end;

          {ivti,ivtx,ivtb,ivtc}
          192,101,102,111: begin read(prd,q, q1); writeln(prr,q:1, ' ', q1:1);
            getexp(ep); popstk(ep^.r); popstk(ep^.l) end;

          {cps}
          176: begin getexp(ep); popstk(ep^.r); popstk(ep^.l); writeln(prr) end;

          {cpc}
          177: begin getexp(ep); popstk(ep^.r); popstk(ep^.l); writeln(prr) end;

          {apc}
          178: begin popstk(ep); popstk(ep2); dmptre(ep2); dmptre(ep); 
            deltre(ep2); deltre(ep); botstk; writeln(prr) end; 

          {pck, upk}
          63, 64: begin read(prd,q,q1); writeln(prr,q:1, ' ', q1:1); popstk(ep);
            popstk(ep2); popstk(ep3); dmptre(ep3); dmptre(ep2); dmptre(ep); 
            deltre(ep); deltre(ep2); deltre(ep3); botstk end;

          {cta}
          191: begin read(prd,q, q1, q2); 
            writeln(prr,q:1, ' ', q1:1, ' ', q1:1, ' ', q2:1); getexp(ep); 
            popstk(ep^.l); popstk(ep^.r); popstk(ep^.x1); pshstk(ep) end;

          {ujp}
          23: begin read(prd,q); writeln(prr, q:1) end;

          {fjp,tjp,xjp}
          24,25,119: begin read(prd,q); writeln(prr, q:1); popstk(ep); 
            dmptre(ep); deltre(ep); botstk end;

          {ents,ente}
          13, 173: begin labelsearch; writeln(prr) end;

          {ipj}
          112: begin read(prd,p); writeln(prr, p:1); labelsearch end;

          {lpa}
          114: begin read(prd,p,q); getexp(ep); pshstk(ep); writeln(prr) end;

          15 {csp}: begin skpspc; getname;
                           while name<>sptable[q] do
                           begin q := q+1; if q > maxsp then
                                 errorl('std proc/func not found  ')
                           end; 
                           writeln(prr);
                           callsp
                      end;

          7, 123, 124, 125, 126, 127 (*ldc*): begin case op of  (*get q*)
                           123: begin read(prd,i); writeln(prr, i:1); 
                             getexp(ep); ep^.vali := i; pshstk(ep) end;

                           124: begin read(prd,r); writeln(prr, r); getexp(ep);
                             pshstk(ep) end;

                           125: begin getexp(ep); pshstk(ep); writeln(prr) end;

                           126: begin read(prd,i); writeln(prr, i:1); 
                             getexp(ep); ep^.vali := i; pshstk(ep) end;

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

           {chk}
           26, 95, 97, 98, 99, 190, 199: begin read(prd,lb,ub); 
             writeln(prr, lb:1, ' ', ub:1); getexp(ep); popstk(ep^.l) end;

           {vbs}
           92: begin read(prd,q); writeln(prr, q:1); getexp(ep); 
             popstk(ep^.l) end;

           {vbe}
           96:;

           56 {lca}: begin read(prd,l); write(prr, l:1, ' '); skpspc;
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
             new(sbp); strassvsb(sbp^.str, str); sbp^.strl := l; 
             strnum := strnum+1; sbp^.strn := strnum; sbp^.next := strtbl;
             strtbl := sbp; ep^.strn := strnum
           end;

          {ret}
          22: writeln(prr);

          {retp}
          14: begin botstk; writeln(prr) end;

          {reti,retr,retc,retb,reta,retx}
          128, 129, 130, 131, 132, 204: begin popstk(ep); dmptre(ep); 
            deltre(ep); botstk; writeln(prr) end;


          { equ,neq,geq,grt,leq,les with no parameter }
          17, 137, 138, 139, 140, 141,
          18, 143, 144, 145, 146, 147,
          19, 149, 150, 151, 152, 153,
          20, 155, 156, 157, 158, 159,
          21, 161, 162, 163, 164, 165,
          167, 168, 169, 170, 171: begin getexp(ep); popstk(ep^.r); 
            popstk(ep^.l); pshstk(ep); writeln(prr) end;

          {ord}
          59, 134, 136, 200: begin getexp(ep); popstk(ep^.l); pshstk(ep); 
            writeln(prr) end;

          {vip,vis}
          133, 122: writeln(prr); { ??? fill me in }

          {vin}
          226: writeln(prr); { ??? fill me in }

          {lcp}
          135: begin getexp(ep); popstk(ep^.l); pshstk(ep); writeln(prr) end;

          {sto}
          6, 80, 81, 82, 83, 84, 197: begin popstk(ep); popstk(ep2); 
            dmptre(ep2); dmptre(ep); deltre(ep); deltre(ep2); botstk; 
            writeln(prr) end;

          {sgs}
          32: begin getexp(ep); popstk(ep^.l); pshstk(ep); writeln(prr) end;
 
          {flt}
          33: begin getexp(ep); popstk(ep^.l); pshstk(ep); writeln(prr) end;

          {flo]
          34: begin getexp(ep); popstk(ep^.l); popstk(ep^.r) pshstk(ep); 
            writeln(prr) end;

          {trc}
          35: begin getexp(ep); popstk(ep^.l); pshstk(ep); writeln(prr) end;

          {ngi,ngr}
          36,37: begin getexp(ep); popstk(ep^.l); pshstk(ep); writeln(prr) end;

          {sqi,sqr}
          38,39: begin getexp(ep); popstk(ep^.l); popstk(ep^.r); pshstk(ep); 
            writeln(prr) end;

          {abi,abr}
          40,41: begin getexp(ep); popstk(ep^.l); pshstk(ep); writeln(prr) end;

          {notb,noti,odd,rnd,chr}
          42,50,60,62,205: begin getexp(ep); popstk(ep^.l); pshstk(ep); 
            writeln(prr) end;

          {and,ior,xor,dif,int,uni,inn,mod,mpi,mpr,dvi,dvr,rgs}
          43,44,46,47,48,49,51,52,53,54,110,206: begin getexp(ep); popstk(ep^.l);
            popstk(ep^.r); pshstk(ep); writeln(prr) end;

          {stp}
          58:;

          { dupi, dupa, dupr, dups, dupb, dupc }
          181, 182, 183, 184, 185, 186: begin getexp(ep); popstk(ep^.l);
            pshstk(ep); pshstk(ep); writeln(prr) end;

          {cks}
          187: begin getexp(ep); popstk(ep^.l); getexp(ep^.r); pshstk(ep); 
            writeln(prr) end;

          {cke}
          188: begin popstk(ep); popstk(ep2); dmptre(ep2); dmptre(ep); 
            deltre(ep); deltre(ep2); botstk; writeln(prr) end;


          { inv }
          189: begin popstk(ep); dmptre(ep); deltre(ep); botstk; 
            writeln(prr) end;

          61 {ujc}: writeln(prr);

      end; (*case*)

      getlin; { next intermediate line }

   end; (*assemble*)

   procedure genstrcst;
   begin
     while strtbl <> nil do begin
       writeln(prr, 'string', strtbl^.strn:1, ':');
       write(prr, '        .string "');
       writev(prr, strtbl^.str, strtbl^.strl);
       writeln(prr, '"');
       strtbl := strtbl^.next
     end
   end;

begin (*xlate*)

   init;
   writeln(prr, '        .text');
   generate;
   genstrcst;

   writeln(prr, '        .bss');
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

  strtbl := nil; strnum := 0; gblsiz := 0;

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
