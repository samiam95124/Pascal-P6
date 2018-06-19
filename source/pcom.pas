(*$c+,t-,d-,l-*)
{*******************************************************************************
*                                                                              *
*                     Portable Pascal assembler/interpreter                    *
*                     *************************************                    *
*                                                                              *
*                                 Pascal P6                                    *
*                                                                              *
*                                 ETH May 76                                   *
*                                                                              *
* Authors:                                                                     *
*    Urs Ammann                                                                *
*    Kesav Nori                                                                *
*    Christian Jacobi                                                          *
*    K. Jensen                                                                 *
*    N. Wirth                                                                  *
*                                                                              *
*    Address:                                                                  *
*       Institut Fuer Informatik                                               *
*       Eidg. Technische Hochschule                                            *
*       CH-8096 Zuerich                                                        *
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
* Adaption from P5 to P6 by:                                                   *
*                                                                              *
*    Scott A. Franco                                                           *
*    samiam@moorecad.com                                                       *
*                                                                              *
*    The comments marked with brackets are mine [sam]                          *
*                                                                              *
* Please see accompanying documentation concerning this software.              *
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

program pcom(output,prd,prr);

label 99; { terminate immediately }

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

   displimit   = 300;
   maxlevel    = 255;
   { strglgth used to define the size of all strings in pcom and pint. With the
     string quanta system, string lengths are effectively unlimited, but there
     it still sets the size of some buffers in pcom. }
   strglgth    = 250;
   { lcaftermarkstack is a very pcom specific way of stating the size of a mark
     in pint. However, it is used frequently in Perberton's documentation, so I
     left it, but equated it to the more portable marksize. }
   lcaftermarkstack = -marksize;
   fileal      = charal;
   (* stackelsize = minimum size for 1 stackelement
                  = k*stackal
      stackal     = scm(all other al-constants)
      charmax     = scm(charsize,charal)
                    scm = smallest common multiple
      lcaftermarkstack >= maxresult+3*ptrsize+max(x-size)
                        = k1*stackelsize          *)
   parmal     = stackal;
   parmsize   = stackelsize;
   recal      = stackal;
   maxaddr    =  maxint;
   maxsp      = 85;  { number of standard procedures/functions }
   maxins     = 92;  { maximum number of instructions }
   maxids     = 250; { maximum characters in id string (basically, a full line) }
   maxstd     = 74;  { number of standard identifiers }
   maxres     = 66;  { number of reserved words }
   reslen     = 9;   { maximum length of reserved words }
   explen     = 32;  { length of exception names }
   maxrld     = 22;  { maximum length of real in digit form }
   varsqt     = 10;  { variable string quanta }
   prtlln     = 10;  { number of label characters to print in dumps }
   minocc     = 50;  { minimum occupancy for case tables }
   cstoccmax=4000; cixmax=10000;
   fillen     = maxids;
   extsrc      = '.pas'; { extention for source file }

   { default field sizes for write }
   intdeff    = 11; { default field length for integer }
   reldeff    = 22; { default field length for real }
   chrdeff    = 1;  { default field length for char (usually 1) }
   boldeff    = 5;  { default field length for boolean (usually 5 for 'false' }

   { version numbers }
   majorver   = 0; { major version number }
   minorver   = 1; { minor version number }
   experiment = true; { is version experimental? }

type                                                        (*describing:*)
                                                            (*************)

                                                            (*basic symbols*)
                                                            (***************)

     symbol = (ident,intconst,realconst,stringconst,notsy,mulop,addop,relop,
               lparent,rparent,lbrack,rbrack,comma,semicolon,period,arrow,
               colon,becomes,range,labelsy,constsy,typesy,varsy,funcsy,progsy,
               procsy,setsy,packedsy,arraysy,recordsy,filesy,beginsy,ifsy,
               casesy,repeatsy,whilesy,forsy,withsy,gotosy,endsy,elsesy,untilsy,
               ofsy,dosy,tosy,downtosy,thensy,nilsy,forwardsy,modulesy,usessy,
               privatesy,externalsy,viewsy,fixedsy,processsy,monitorsy,sharesy,
               classsy,issy,overloadsy,overridesy,referencesy,joinssy,staticsy,
               inheritedsy,selfsy,virtualsy,trysy,exceptsy,extendssy,onsy,
               resultsy,operatorsy,outsy,propertysy,channelsy,streamsy,othersy,
               hexsy,octsy,binsy,numsy);
     operatort = (mul,rdiv,andop,idiv,imod,plus,minus,orop,ltop,leop,geop,gtop,
                  neop,eqop,inop,noop,xorop);
     setofsys = set of symbol;
     chtp = (letter,number,special,illegal,
             chstrquo,chcolon,chperiod,chlt,chgt,chlparen,chspace,chlcmt,chrem,
             chhex,choct,chbin);
     { Here is the variable length string containment to save on space. strings
       strings are only stored in their length rounded to the nearest 10th. }
     strvsp = ^strvs; { pointer to variable length id string }
     strvs = record { id string variable length }
                 str:   packed array [1..varsqt] of char; { data contained }
                 next:  strvsp { next }
               end;

                                                            (*constants*)
                                                            (***********)
     setty = set of setlow..sethigh;
     cstclass = (reel,pset,strg);
     csp = ^ constant;
     constant = record
                       next: csp; { next entry link }
                       case cclass: cstclass of
                         reel: (rval: real);
                         pset: (pval: setty);
                         strg: (slgth: 0..strglgth; sval: strvsp)
                       end;

     valu = record case intval: boolean of
                     true:  (ival: integer);
                     false: (valp: csp)
                   end;

                                                           (*data structures*)
                                                           (*****************)
     levrange = 0..maxlevel; addrrange = -maxaddr..maxaddr; stkoff = -maxaddr..0;
     structform = (scalar,subrange,pointer,power,arrays,records,files,
                   tagfld,variant,exceptf);
     declkind = (standard,declared);
     stp = ^ structure;
     ctp = ^ identifier;

     structure = record
                   snm: integer; { serial number }
                   next: stp; { next entry link }
                   marked: boolean;   (*for test phase only*)
                   size: addrrange;
                   packing: boolean; { packing status }
                   case form: structform of
                     scalar:   (case scalkind: declkind of
                                  declared: (fconst: ctp); standard: ());
                     subrange: (rangetype: stp; min,max: valu);
                     pointer:  (eltype: stp);
                     power:    (elset: stp; matchpack: boolean);
                     arrays:   (aeltype,inxtype: stp);
                     records:  (fstfld: ctp; recvar: stp; recyc: stp);
                     files:    (filtype: stp);
                     tagfld:   (tagfieldp: ctp; fstvar: stp);
                     variant:  (nxtvar,subvar,caslst: stp; varfld: ctp; 
                                varval: valu);
                     exceptf:  ()
                   end;

                                                            (*names*)
                                                            (*******)

     idclass = (types,konst,vars,field,proc,func);
     setofids = set of idclass;
     idkind = (actual,formal);
     idstr = packed array [1..maxids] of char;
     restr = packed array [1..reslen] of char;
     expstr = packed array [1..explen] of char;
     csstr = packed array [1..strglgth] of char;
     rlstr = packed array [1..maxrld] of char;
     keyrng = 1..31; { range of standard call keys }
     filnam = packed array [1..fillen] of char; { filename strings }
     filptr = ^filrec;
     filrec = record next: filptr; fn: filnam; mn: strvsp; f: text;
                     priv: boolean end;
     partyp = (ptval, ptvar, ptview, ptout);
     { procedure function attribute }
     fpattr = (fpanone,fpaoverload,fpastatic,fpavirtual,fpaoverride,
               fpaoperator);
     identifier = record
                   snm: integer; { serial number }
                   name: strvsp; llink, rlink: ctp;
                   idtype: stp; next: ctp; keep: boolean; refer: boolean;
                   case klass: idclass of
                     types: ();
                     konst: (values: valu);
                     vars:  (vkind: idkind; vlev: levrange; vaddr: addrrange;
                             threat: boolean; forcnt: integer; part: partyp; 
                             hdr: boolean; vext: boolean; vmod: filptr);
                     field: (fldaddr: addrrange; varnt: stp; varlb: ctp;
                             tagfield: boolean; taglvl: integer;
                             varsaddr: addrrange; varssize: addrrange);
                     proc, func:  (pfaddr: addrrange; pflist: ctp; { param list }
                                   asgn: boolean; { assigned }
                                   pext: boolean; pmod: filptr; pfattr: fpattr;
                                   pfvaddr: addrrange; pfvid: ctp; 
                                   grpnxt, grppar: ctp;
                                   case pfdeckind: declkind of
                              standard: (key: keyrng);
                              declared: (pflev: levrange; pfname: integer;
                                          case pfkind: idkind of
                                           actual: (forwdecl, externl: boolean);
                                           formal: ()))
                   end;
     
     where = (blck,crec,vrec,rec);

                                                            (*expressions*)
                                                            (*************)
     attrkind = (cst,varbl,expr);
     vaccess = (drct,indrct,inxd);

     attr = record symptr: ctp; typtr: stp;
              case kind: attrkind of
                cst:   (cval: valu);
                varbl: (packing: boolean; packcom: boolean;
                        tagfield: boolean; taglvl: integer; varnt: stp;
                        ptrref: boolean; vartagoff: addrrange;
                        varssize: addrrange;
                        case access: vaccess of
                          drct: (vlevel: levrange; dplmt: addrrange);
                          indrct: (idplmt: addrrange);
           inxd: ());
           expr: ()
              end;

                                                                 (*labels*)
                                                                 (********)
     lbp = ^ labl;
     labl = record { 'goto' label }
                   nextlab: lbp;     { next list link }
                   defined: boolean; { label defining point was seen }
                   labval,           { numeric value of label }
                   labname: integer; { internal sequental name of label }
                   labid:   strvsp;  { id in case of identifier label }
                   vlevel: levrange; { procedure level of definition }
                   slevel:  integer; { statement level of definition }
                   ipcref:  boolean; { was referenced by another proc/func }
                   minlvl:  integer; { minimum goto reference statement lvl }
                   bact:    boolean; { containing block is active }
                   refer:   boolean  { was referred to }
            end;

     disprange = 0..displimit;
     disprec = record                      (*=blck:   id is variable id*)
                 fname: ctp; flabel: lbp;  (*=crec:   id is field id in record with*)
                 fconst: csp; fstruct: stp;
                 packing: boolean;         { used for with derived from packed }
                 packcom: boolean;         { used for with derived from packed }
                 ptrref: boolean;          { used for with derived from pointer }
                 modnam: strvsp;           { module name for block (if exists) }
                 case occur: where of      (*   constant address*)
                   crec: (clev: levrange;  (*=vrec:   id is field id in record with*)
                          cdspl: addrrange);(*   variable address*)
                   vrec: (vdspl: addrrange);
                   blck: (bname: ctp);     { block id }
                   rec: ()
               end;                        (* --> procedure withstatement*)
               
     { external file tracking entries }
     extfilep = ^filerec;
     filerec = record filename:idstr; nextfile:extfilep end;

     { case statement tracking entries }
     cip = ^caseinfo;
     caseinfo = record next: cip;
                  csstart: integer;
                  cslabs,cslabe: integer
                end;

     stdrng = 1..maxstd; { range of standard name entries }
     oprange = 0..maxins;
     modtyp = (mtprogram, mtmodule); { type of current module }
      
(*-------------------------------------------------------------------------*)

var

    { !!! remove this statement for self compile }
    {elide}prd,prr: text;{noelide}       { output code file }

                                    (*returned by source program scanner
                                     insymbol:
                                     **********)

    sy: symbol;                     (*last symbol*)
    op: operatort;                  (*classification of last symbol*)
    val: valu;                      (*value of last constant*)
    lgth: integer;                  (*length of last string constant*)
    id: idstr;                      (*last identifier (possibly truncated)*)
    kk: 1..maxids;                  (*nr of chars in last identifier*)
    ch: char;                       (*last character*)
    eol: boolean;                   (*end of line flag*)
    
    { pushback system, last and next variables }
    lsy: symbol; lop: operatort; lval: valu; llgth: integer;
    lid: idstr; lkk: 1..maxids;
    nsy: symbol; nop: operatort; nval: valu; nlgth: integer;
    nid: idstr; nkk: 1..maxids; nvalid: boolean;

                                    (*counters:*)
                                    (***********)

    chcnt: integer;                 (*character counter*)
    ic,gc: addrrange;               (*data location and instruction counter*)
    lc:    stkoff;
    linecount: integer;
    lineout: integer;

                                    (*switches:*)
                                    (***********)

    dp: boolean;                    (*declaration part*)
    list: boolean;                  { -- l: source program listing }
    prcode: boolean;                { -- c: print symbolic code }
    prtables: boolean;              { -- t: displaying ident and struct tables }
    chkvar: boolean;                { -- v: check variant records }
    debug: boolean;                 { -- d: Debug checks }
    chkref: boolean;                { -- r: Reference checks }
    chkudtc, chkudtf: boolean;      { -- u: Check undefined tagfields, candidate
                                         and final }
    iso7185: boolean;               { -- s: restrict to iso7185 language }
    dodmplex: boolean;              { -- x: dump lexical }
    doprtryc: boolean;              { -- z: dump recycling tracker counts }
    doprtlab: boolean;              { -- b: print labels }
    dodmpdsp: boolean;              { -- y: dump the display }
    
    { switches passed through to pint }

    { -- o: check arithmetic overflow }
    { -- a: dump instructions after assembly }
    { -- g: dump label definitions }
    { -- f: dump storage area specs }
    { -- e: trace routine executions }
    { -- i: trace instruction executions }
    { -- m: perform post-mortem dump on error }
    { -- h: add source line sets to code }
    { -- j: trace source line executions (requires dosrclin) }
    { -- k: dump heap space after execution }
    { -- n: obey heap space recycle requests }
    { -- p: check reuse of freed entry } 
    { -- q: check undefined accesses }   
    
    option: array ['a'..'z'] of     { option array }
              boolean;


                                    (*pointers:*)
                                    (***********)
    parmptr,
    intptr,crdptr,realptr,charptr,
    boolptr,nilptr,textptr,
    exceptptr: stp;                 (*pointers to entries of standard ids*)
    utypptr,ucstptr,uvarptr,
    ufldptr,uprcptr,ufctptr,        (*pointers to entries for undeclared ids*)
    fwptr: ctp;                     (*head of chain of forw decl type ids*)
    outputptr,inputptr,
    prdptr,prrptr,errorptr,
    listptr,commandptr: ctp;        { pointers to default files }
    usclrptr: ctp;                  { used to satisfy broken record tag fields }
    fextfilep: extfilep;            (*head of chain of external files*)

                                    (*bookkeeping of declaration levels:*)
                                    (************************************)

    level: levrange;                (*current static level*)
    disx,                           (*level of last id searched by searchid*)
    top: disprange;                 (*top of display*)
    ptop: disprange;                { top of pile }

    display:                        (*where:   means:*)
      array [disprange] of disprec;
      
    pile:                           { pile of joined/class contexts } 
      array [disprange] of disprec;

                                    (*error messages:*)
                                    (*****************)

    errinx: 0..10;                  (*nr of errors in current source line*)
    errlist:
      array [1..10] of
        packed record pos: integer;
                      nmr: 1..507
               end;



                                    (*expression compilation:*)
                                    (*************************)

    gattr: attr;                    (*describes the expr currently compiled*)

                                    (*structured constants:*)
                                    (***********************)

    constbegsys,simptypebegsys,typebegsys,blockbegsys,selectsys,facbegsys,
    statbegsys,typedels,pfbegsys: setofsys;
    chartp : array[char] of chtp;
    rw:  array [1..maxres(*nr. of res. words*)] of restr;
    rsy: array [1..maxres(*nr. of res. words*)] of symbol;
    ssy: array [char] of symbol;
    rop: array [1..maxres(*nr. of res. words*)] of operatort;
    sop: array [char] of operatort;
    na:  array [stdrng] of restr;
    mn:  array [0..maxins] of packed array [1..4] of char;
    sna: array [1..maxsp] of packed array [1..4] of char;
    cdx: array [0..maxins] of integer;
    cdxs: array [1..6, 1..7] of integer;
    pdx: array [1..maxsp] of integer;
    ordint: array [char] of integer;

    intlabel,mxint10,maxpow10: integer;
    entname,extname,nxtname: integer;
    errtbl: array [1..507] of boolean; { error occurence tracking }
    toterr: integer; { total errors in program }
    stackbot, topnew, topmin: integer;
    cstptr: array [1..cstoccmax] of csp;
    cstptrix: 0..cstoccmax;
    (*allows referencing of noninteger constants by an index
      (instead of a pointer), which can be stored in the p2-field
      of the instruction record until writeout.
      --> procedure load, procedure writeout*)
    curmod: modtyp; { type of current module }
    nammod: strvsp; { name of current module }
    incstk: filptr; { stack of included files }
    inclst: filptr; { discard list for includes }

    { Recycling tracking counters, used to check for new/dispose mismatches. }
    strcnt: integer; { strings }
    cspcnt: integer; { constants }
    stpcnt: integer; { structures }
    ctpcnt: integer; { identifiers }
    lbpcnt: integer; { label counts }
    filcnt: integer; { file tracking counts }
    cipcnt: integer; { case entry tracking counts }
    
    { serial numbers to label structure and identifier entries for dumps }
    ctpsnm: integer;
    stpsnm: integer;

    f: boolean; { flag for if error number list entries were printed }
    i: 1..507; { index for error number tracking array }
    c: char;

(*-------------------------------------------------------------------------*)

                           { recycling controls }

(*-------------------------------------------------------------------------*)

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

  { get label entry }
  procedure getlab(var p: lbp);
  begin
     new(p); { get new entry }
     lbpcnt := lbpcnt+1 { add to count }
  end;

  { recycle label entry }
  procedure putlab(p: lbp);
  begin
     putstrs(p^.labid); { release any id label } 
     dispose(p); { release entry }
     lbpcnt := lbpcnt-1 { remove from count }
  end;

  { push constant entry to list }
  procedure pshcst(p: csp);
  begin
     { push to constant list }
     p^.next := display[top].fconst;
     display[top].fconst := p;
     cspcnt := cspcnt+1 { count entries }
  end;

  { recycle constant entry }
  procedure putcst(p: csp);
  begin
     { recycle string if present }
     if p^.cclass = strg then putstrs(p^.sval);
     dispose(p); { release entry }
     cspcnt := cspcnt-1 { remove from count }
  end;

  { push structure entry to list }
  procedure pshstc(p: stp);
  begin
     { push to structures list }
     p^.next := display[top].fstruct;
     display[top].fstruct := p;
     stpcnt := stpcnt+1; { count entries }
     stpsnm := stpsnm+1; { identify entry in dumps }
     p^.snm := stpsnm
  end;

  { recycle structure entry }
  procedure putstc(p: stp);
  begin
     dispose(p); { release entry }
     stpcnt := stpcnt-1
  end;

  { initialize and register identifier entry }
  procedure ininam(p: ctp);
  begin
     ctpcnt := ctpcnt+1; { count entry }
     p^.keep := false; { clear keepme flag }
     p^.refer := false; { clear referred flag }
     ctpsnm := ctpsnm+1; { identify entry in dumps }
     p^.snm := ctpsnm
  end;
  
  { recycle identifier entry }
  procedure putnam(p: ctp);
  var p1: ctp;
  begin
     if (p^.klass = proc) or (p^.klass = func) then begin
        while p^.pflist <> nil do begin
          { scavenge the parameter list }
          p1 := p^.pflist; p^.pflist := p1^.next;
          putnam(p1) { release }
        end;
        while p^.grpnxt <> nil do begin
          { scavenge the group list }
          p1 := p^.grpnxt; p^.grpnxt := p1^.grpnxt;
          putnam(p1) { release }
        end;
     end;
     putstrs(p^.name); { release name string }
     dispose(p); { release entry }
     ctpcnt := ctpcnt-1 { remove from count }
  end;
  
  { recycle identifier list }
  procedure putnamlst(p: ctp);
  var p1: ctp;
  begin
    while p <> nil do begin
       p1 := p; p := p^.next; putnam(p1) { release }
    end
  end;
  
  { recycle identifier tree }
  procedure putnams(p: ctp);
  begin
    if p <> nil then begin
      putnams(p^.llink); { release left }
      putnams(p^.rlink); { release right }
      { "keep" means it is a parameter and stays with it's procedure or
        function entry. }
      if not p^.keep then putnam(p) { release the id entry }
    end
  end;

  { scrub display level }
  procedure putdsp(var dr: disprec);
     var llp: lbp; lvp: csp; lsp: stp;
     { release substructure }
     procedure putsub(p: stp);
        var p1: stp;
     begin
        { clear record recycle list if record }
        if p^.form = records then begin
           { clear structure list }
           while p^.recyc <> nil do begin
              { remove top of list }
              p1 := p^.recyc; p^.recyc := p1^.next;
              putsub(p1) { release that element }
           end;
           putnams(p^.fstfld) { clear id list }
        end else if p^.form = tagfld then begin
              if p^.tagfieldp <> nil then
                 { recycle anonymous tag fields }
                 if p^.tagfieldp^.name = nil then putnam(p^.tagfieldp)
        end;
        putstc(p) { release head entry }
     end;
  begin { putdsp }
    putnams(dr.fname); { dispose of identifier tree }
    { dispose of label list }
    while dr.flabel <> nil do begin
      llp := dr.flabel; dr.flabel := llp^.nextlab; putlab(llp)
    end;
    { dispose of constant list }
    while dr.fconst <> nil do begin
      lvp := dr.fconst; dr.fconst := lvp^.next; putcst(lvp)
    end;
    { dispose of structure list }
    while dr.fstruct <> nil do begin
      { remove top from list }
      lsp := dr.fstruct; dr.fstruct := lsp^.next; putsub(lsp)
    end;
    { dispose of module name }
    putstrs(dr.modnam)
  end; { putdsp }

  { scrub all display levels until given }
  procedure putdsps(l: disprange);
  var t: disprange;
  begin
    if l > top then begin
      writeln('*** Error: Compiler internal error');
      goto 99
    end;
    t := top;
    while t > l do begin
      putdsp(display[t]); t := t-1
    end
  end;
  
  { scrub the pile }
  procedure putpile;
  var t: disprange;
  begin
    if ptop > 0 then for t := ptop-1 downto 0 do putdsp(pile[t])
  end;

  { get external file entry }
  procedure getfil(var p: extfilep);
  begin
     new(p); { get new entry }
     filcnt := filcnt+1 { count entry }
  end;

  { recycle external file entry }
  procedure putfil(p: extfilep);
  begin
     dispose(p); { release entry }
     filcnt := filcnt-1 { count entry }
  end;

  { get case tracking entry }
  procedure getcas(var p: cip);
  begin
     new(p); { get new entry }
     cipcnt := cipcnt+1 { count entry }
  end;

  { recycle case tracking entry }
  procedure putcas(p: cip);
  begin
     dispose(p); { release entry }
     cipcnt := cipcnt-1 { count entry }
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

  { find reserved word string equal to id string }
  function strequri(a: restr; var b: idstr): boolean;
  var m: boolean; i: integer;
  begin
    m := true;
    for i := 1 to reslen do if lcase(a[i]) <> lcase(b[i]) then m := false;
    for i := reslen+1 to maxids do if b[i] <> ' ' then m := false;
    strequri := m
  end { equstr };

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

  { write variable length padded string to file }
  procedure writevp(var f: text; s: strvsp);
  var i: integer;
  begin
    while s <> nil do begin
      for i := 1 to varsqt do if s^.str[i] <> ' ' then write(f, s^.str[i]);
      s := s^.next
    end;
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

  { assign identifier fixed to variable length string, including allocation }
  procedure strassvf(var a: strvsp; var b: idstr);
  var i, j, l: integer; p, lp: strvsp;
  begin l := maxids; p := nil; a := nil; j := 1;
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

  { assign reserved word fixed to variable length string, including allocation }
  procedure strassvr(var a: strvsp; b: restr);
  var i, j, l: integer; p, lp: strvsp;
  begin l := reslen; p := nil; a := nil; lp := nil; j := 1;
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
  
  { assign exception word fixed to variable length string, including allocation }
  procedure strassve(var a: strvsp; b: expstr);
  var i, j, l: integer; p, lp: strvsp;
  begin l := explen; p := nil; a := nil; lp := nil; j := 1;
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

  { assign constant string fixed to variable length string, including allocation }
  procedure strassvc(var a: strvsp; b: csstr; l: integer);
  var i, j: integer; p, lp: strvsp;
  begin p := nil; a := nil; lp := nil; j := 1;
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

  { assign variable length string to fixed identifier }
  procedure strassfv(var a: idstr; b: strvsp);
  var i, j: integer;
  begin for i := 1 to maxids do a[i] := ' '; i := 1;
     while b <> nil do begin
        for j := 1 to varsqt do begin a[i] := b^.str[j]; i := i+1 end;
        b := b^.next
     end
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

  { compare variable length id strings, a < b }
  function strltnvv(a, b: strvsp): boolean;
  var i: integer; ca, cb: char;
  begin ca := ' '; cb := ' ';
    while (a <> nil) or (b <> nil) do begin
      i := 1;
      while (i <= varsqt) and ((a <> nil) or (b <> nil)) do begin
        if a <> nil then ca := lcase(a^.str[i]) else ca := ' ';
        if b <> nil then cb := lcase(b^.str[i]) else cb := ' ';
        if ca <> cb then begin a := nil; b := nil end;
        i := i+1
      end;
      if a <> nil then a := a^.next; if b <> nil then b := b^.next
    end;
    strltnvv := ca < cb
  end;

  { compare variable length id string to fixed }
  function strequvf(a: strvsp; var b: idstr): boolean;
  var m: boolean; i, j: integer; c: char;
  begin
    m := true; j := 1;
    for i := 1 to maxids do begin
      c := ' '; if a <> nil then begin c := a^.str[j]; j := j+1 end;
      if lcase(c) <> lcase(b[i]) then m := false;
      if j > varsqt then begin a := a^.next; j := 1 end
    end;
    strequvf := m
  end;

  { compare variable length id string to fixed, a < b }
  function strltnvf(a: strvsp; var b: idstr): boolean;
  var m: boolean; i, j, f: integer; c: char;
  begin
    m := true; i := 1; j := 1;
    while i < maxids do begin
      c := ' '; if a <> nil then begin c := a^.str[j]; j := j+1 end;
      if lcase(c) <> lcase(b[i]) then begin f := i; i := maxids end else i := i+1;
      if j > varsqt then begin a := a^.next; j := 1 end
    end;
    strltnvf := lcase(c) < lcase(b[f])
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
   
  { concatenate reserved word fixed to variable length string, including 
    allocation }
  procedure strcatvr(var a: strvsp; b: restr);
  var i, j, l: integer;
  begin l := reslen;
    while (l > 1) and (b[l] = ' ') do l := l-1; { find length of fixed string }
    if b[l] = ' ' then l := 0;
    j := lenpv(a); j := j+1;
    for i := 1 to l do begin strchrass(a, j, b[i]); j := j+1 end
  end;

(*-------------------------------------------------------------------------*)

                        { Boolean integer emulation }

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

(*--------------------------------------------------------------------*)

{ 

Language extension routines. These routines allow specification of semantic
functions beyond the base ISO 7185 specification. 

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
  { ISO7185 start - 
  fn := fn; reset(f); 
  writeln; writeln('*** assigntext function not implemented in ISO 7185');
  goto 99
  - ISO7185 end }

  { Pascaline start -
  assign(f, fn);
  - Pascaline end }

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
  { ISO7185 start -
  reset(f);
  writeln; writeln('*** closetext function not implemented in ISO 7185');
  goto 99
  - ISO7185 end }
  
  { Pascaline start -
  close(f);
  - Pascaline end }
  
  {$gnu-pascal} 
  close(f)
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
  { ISO7185 start -
  fn := fn;
  writeln; writeln('*** existsfile function not implemented in ISO 7185');
  goto 99;
  existsfile := true
  - ISO7185 end }
  
  { Pascaline start -
  existsfile := exists(fn);
  - Pascaline end }
  
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

(*-------------------------------------------------------------------------*)

  { dump the display }
  procedure prtdsp;
  var i: integer;
  procedure prtlnk(p: ctp; f: integer);
  var i: integer;
  begin
    if p <> nil then begin
      for i := 1 to f do write(' ');
      writev(output, p^.name, 10); writeln;
      if p^.llink <> nil then prtlnk(p^.llink, f+3);
      if p^.rlink <> nil then prtlnk(p^.rlink, f+3)
    end
  end;
  begin
    writeln;
    writeln('Display:');
    writeln;
    for i := 0 to displimit do if display[i].fname <> nil then begin

       writeln('level ', i:1);
       writeln;
       prtlnk(display[i].fname, 0);
       writeln

    end;
    writeln;
  end;
  
  { this block of functions wraps source reads }
  function eofinp: boolean;
  begin
    if incstk <> nil then begin
      if incstk^.priv then eofinp := true else eofinp := eof(incstk^.f) 
    end else eofinp := eof(prd)
  end;
  
  function eolninp: boolean;
  begin 
    if eofinp then eolninp := true
    else if incstk <> nil then eolninp := eoln(incstk^.f) 
         else eolninp := eoln(prd)
  end;
  
  procedure readinp(var c: char);
  begin
    if incstk <> nil then read(incstk^.f, c) else read(prd, c)
  end;
  
  function bufinp: char;
  begin
    if incstk <> nil then bufinp := incstk^.f^ else bufinp := prd^
  end;
  { --- }

  procedure endofline;
    var lastpos,freepos,currpos,currnmr,f,k: integer;
  begin
    if errinx > 0 then   (*output error messages*)
      begin write(linecount:6,' ****  ':9);
        lastpos := -1; freepos := 1;
        for k := 1 to errinx do
          begin
            with errlist[k] do
              begin currpos := pos; currnmr := nmr end;
            if currpos = lastpos then write(',')
            else
              begin
                while freepos < currpos do
                  begin write(' '); freepos := freepos + 1 end;
                write('^');
                lastpos := currpos
              end;
            if currnmr < 10 then f := 1
            else if currnmr < 100 then f := 2
              else f := 3;
            write(currnmr:f);
            freepos := freepos + f + 1
          end;
        writeln(output); errinx := 0
      end;
    linecount := linecount + 1;
    if list and (not eofinp) then
      begin write(linecount:6,'  ':2);
        if dp then write(lc:7) else write(ic:7);
        write(' ')
      end;
    chcnt := 0
  end  (*endofline*) ;

  { output lines passed to intermediate }
  procedure outline;
  begin
    while lineout < linecount do begin
      lineout := lineout+1;
      { output line marker in intermediate file }
      if not eofinp then begin
        writeln(prr, ':', lineout:1);
      end
    end
  end;

  { check in private section }
  function inpriv: boolean;
  begin inpriv := false;
    if incstk <> nil then inpriv := incstk^.priv
  end;
        
  procedure errmsg(ferrnr: integer);
  begin case ferrnr of
    1:   write('Error in simple type');
    2:   write('Identifier expected');
    3:   write('''program'' expected');
    4:   write(''')'' expected');
    5:   write(''':'' expected');
    6:   write('Illegal symbol');
    7:   write('Error in parameter list');
    8:   write('''of'' expected');
    9:   write('''('' expected');
    10:  write('Error in type');
    11:  write('''['' expected');
    12:  write(''']'' expected');
    13:  write('''end'' expected');
    14:  write(''';'' expected');
    15:  write('Integer expected');
    16:  write('''='' expected');
    17:  write('''begin'' expected');
    18:  write('Error in declaration part');
    19:  write('Error in field-list');
    20:  write(''','' expected');
    21:  write('''.'' expected');
    22:  write('Integer or identifier expected');
    23:  write('''except'' expected');
    24:  write('''on'' or ''except'' expected');

    50:  write('Error in constant');
    51:  write(''':='' expected');
    52:  write('''then'' expected');
    53:  write('''until'' expected');
    54:  write('''do'' expected');
    55:  write('''to''/''downto'' expected');
    56:  write('''if'' expected');
    57:  write('''file'' expected');
    58:  write('Error in factor');
    59:  write('Error in variable');

    101: write('Identifier declared twice');
    102: write('Low bound exceeds highbound');
    103: write('Identifier is not of appropriate class');
    104: write('Identifier not declared');
    105: write('Sign not allowed');
    106: write('Number expected');
    107: write('Incompatible subrange types');
    109: write('Type must not be real');
    110: write('Tagfield type must be scalar or subrange');
    111: write('Incompatible with tagfield type');
    112: write('Index type must not be real');
    113: write('Index type must be scalar or subrange');
    114: write('Base type must not be real');
    115: write('Base type must be scalar or subrange');
    116: write('Error in type of standard procedure parameter');
    117: write('Unsatisfied forward reference');
    118: write('Forward reference type identifier in variable declaration');
    119: write('Forward declared; repetition of parameter list not allowed');
    120: write('Function result type must be scalar, subrange or point');
    121: write('File value parameter, or parameter containing file, not allowed');
    122: write('Forward declared function; repetition of result type not allowed');
    123: write('Missing result type in function declaration');
    124: write('F-format for real only');
    125: write('Error in type of standard function parameter');
    126: write('Number of parameters does not agree with declaration');
    127: write('Illegal parameter substitution');
    128: write('Result type of parameter function does not agree with declaration');
    129: write('Type conflict of operands');
    130: write('Expression is not of set type');
    131: write('Tests on equality allowed only');
    132: write('Strict inclusion not allowed');
    133: write('File comparison not allowed');
    134: write('Illegal type of operand(s)');
    135: write('Type of operand must be Boolean');
    136: write('Set element type must be scalar or subrange');
    137: write('Set element types not compatible');
    138: write('Type of variable is not array');
    139: write('Index type is not compatible with declaration');
    140: write('Type of variable is not record');
    141: write('Type of variable must be file or pointer');
    142: write('Illegal parameter substitution');
    143: write('Illegal type of loop control variable');
    144: write('Illegal type of expression');
    145: write('Type conflict');
    146: write('Assignment of files not allowed');
    147: write('Label type incompatible with selecting expression');
    148: write('Subrange bounds must be scalar');
    149: write('Index type must not be integer');
    150: write('Assignment to standard function is not allowed');
    151: write('Assignment to formal function is not allowed');
    152: write('No such field in this record');
    153: write('Type error in read');
    154: write('Actual parameter must be a variable');
    155: write('Control variable must ~ot be declared on intermediate');
    156: write('Multidefined case label');
    157: write('Too many cases in case statement');
    158: write('Missing corresponding variant declaration');
    159: write('Real or string tagfields not allowed');
    160: write('Previous declaration was not forward');
    161: write('Again forward declared');
    162: write('Parameter size must be constant');
    163: write('Missing variant in declaration');
    164: write('Substitution of standard proc/func not allowed');
    165: write('Multidefined label');
    166: write('Multideclared label');
    167: write('Undeclared label');
    168: write('Undefined label');
    169: write('Error in base set');
    170: write('Value parameter expected');
    171: write('Standard file was redeclared');
    172: write('Undeclared external file');
    173: write('Fortran procedure or function expected');
    174: write('Pascal procedure or function expected');
    175: write('Missing file "input" in program heading');
    176: write('Missing file "output" in program heading');
    177: write('Assiqnment to function identifier not allowed here');
    178: write('Multidefined record variant');
    179: write('X-opt of actual proc/funcdoes not match formal declaration');
    180: write('Control variable must not be formal');
    181: write('Constant part of address out of ranqe');
    182: write('identifier too long');
    183: write('For index variable must be local to this block');
    184: write('Interprocedure goto does not reference outter block of destination');
    185: write('Goto references deeper nested statement');
    186: begin write('Label referenced by goto at lesser statement level or ');
               write('differently nested statement') end;
    187: write('Goto references label in different nested statement');
    188: write('Label referenced by goto in different nested statement');
    189: write('Parameter lists of formal and actual parameters not congruous');
    190: write('File component may not contain other files');
    191: write('Cannot assign from file or component containing files');
    192: write('Assignment to function that is not active');
    193: write('Function does not assign to result');
    194: write('Exponent too large');
    195: write('For loop index is threatened');
    197: write('Var parameter cannot be packed');
    198: write('Var parameter cannot be a tagfield');
    199: write('Var parameter must be same type');
    200: write('Cannot threaten view parameter');
    201: write('Error in real constant: digit expected');
    202: write('String constant must not exceed source line');
    203: write('Integer constant exceeds range');
    204: write('8 or 9 in octal number');
    205: write('Zero string not allowed');
    206: write('Integer part of real constant exceeds ranqe');
    207: write('Digit beyond radix');
    208: write('Type must be string');
    209: write('''procedure'' or ''function'' expected');
    210: write('No function active to set result');
    211: write('Anonymous function result must be at function end');
    212: write('Function result assigned before result given');
    213: write('Cannot take boolean integer operation on negative');
    214: write('Must apply $, & or % posfix modifier to integer');
    215: write('Must apply * (padded string field) to string');
    216: write('Original and forwarded procedure/function parameters not congruous');
    217: write('Missing file ''prd'' in program heading');
    218: write('Missing file ''prr'' in program heading');
    219: write('Missing file ''error'' in program heading');
    220: write('Missing file ''list'' in program heading');
    221: write('Missing file ''command'' in program heading');
    222: write('Value out of character range');
    223: write('Type converter/restrictor must be scalar or subrange');
    224: write('Type to be converted must be scalar or subrange');
    225: write('In constant range first value must be less than or equal to second');
    226: write('Type of variable is not exception');
    227: write('Type too complex to track');
    228: write('Cannot apply ''virtual'' attribute to nested procedure or function');
    229: write('Cannot apply ''override'' attribute to nested procedure or function');
    230: write('Cannot override virtual from same module, must be external');
    231: write('No virtual found to override');
    232: write('Cannot overload virtual procedure or function');
    233: write('Inherited not applied to user procedure/function call');
    234: write('Inherited applied to non-virtual procedure/function');
    235: write('Override not defined for inherited call');

    250: write('Too many nested scopes of identifiers');
    251: write('Too many nested procedures and/or functions');
    252: write('Too many forward references of procedure entries');
    253: write('Procedure too long');
    254: write('Too many long constants in this procedure');
    255: write('Too many errors on this source line');
    256: write('Too many external references');
    257: write('Too many externals');
    258: write('Too many local files');
    259: write('Expression too complicated');
    260: write('Too many exit labels');
    261: write('Label beyond valid integral value (>9999)');
    262: write('Function/procedure cannot be applied to text files');
    263: write('No function to open/close external files');
    264: write('External file not found');
    265: write('Filename too long');
    266: write('''private'' has no meaning here');
    267: write('Too many nested module joins');
    268: write('Qualified identifier not found');

    300: write('Division by zero');
    301: write('No case provided for this value');
    302: write('Index expression out of bounds');
    303: write('Value to be assigned is out of bounds');
    304: write('Element expression out of range');
    305: write('Cannot use non-decimal with real format');

    397: write('Feature not valid in ISO 7185 Pascal');
    398: write('Implementation restriction');
    399: write('Feature not implemented');

    400,401,402,403,404,405,406,407,
    500,501,502,503,
    504,505,506,507: write('Compiler internal error');
    end
  end;

  procedure error(ferrnr: integer);
  begin
    if incstk = nil then begin { supress errors in includes }

      { This diagnostic is here because error buffers error numbers til the end
        of line, and sometimes you need to know exactly where they occurred. }

      {
      writeln; writeln('error: ', ferrnr:1);
      }
    
      errtbl[ferrnr] := true; { track this error }
      if errinx >= 9 then
        begin errlist[10].nmr := 255; errinx := 10 end
      else
        begin errinx := errinx + 1;
          errlist[errinx].nmr := ferrnr
        end;
      errlist[errinx].pos := chcnt;
      toterr := toterr+1
    end
  end (*error*) ;
  
  { chkstd: called whenever a non-ISO7185 construct is being processed }
  procedure chkstd;
  begin
    if iso7185 then error(397)
  end;
  
  procedure insymbol;
    (*read next basic symbol of source program and return its
    description in the global variables sy, op, id, val and lgth*)
    label 1, 2;
    var i,j,k,v,r: integer;
        string: csstr;
        lvp: csp; test, ferr: boolean;
        iscmte: boolean;
        ev: integer;
        rv: real;
        sgn: integer;

    procedure nextch;
    begin if eol then
      begin if list then writeln(output); endofline
      end;
      if not eofinp then
       begin eol := eolninp; readinp(ch);
        if list then write(ch);
        chcnt := chcnt + 1
       end
      else
        begin writeln('   *** eof ','encountered');
          test := false; eol := true
        end
    end;

    procedure options;
    var
      ch1 : char; dummy: boolean;
      procedure switch(var opt: boolean );
      begin
        nextch;
        if (ch='+') or (ch='-') then begin
          opt := ch = '+';
          option[ch1] := opt;
          writeln(prr, 'o ', ch1, ch);
          nextch;
        end else begin { just default to on }
          opt := true;
          option[ch1] := true;
          writeln(prr, 'o ', ch1, '+')
        end
      end; { switch() }
    begin { options() }
      repeat
        nextch;
        ch1 := lcase(ch);
        if ch1 = 't' then switch(prtables)
        else if ch1 = 'l' then begin
          switch(list);
          if not list then writeln(output)
        end
        else if ch1 = 'd' then switch(debug)
        else if ch1 = 'c' then switch(prcode)
        else if ch1 = 'v' then switch(chkvar)
        else if ch1 = 'r' then switch(chkref)
        else if ch1 = 'u' then switch(chkudtc)
        else if ch1 = 's' then switch(iso7185)
        else if ch1 = 'x' then switch(dodmplex)
        else if ch1 = 'z' then switch(doprtryc)
        else if ch1 = 'b' then switch(doprtlab)
        else if ch1 = 'y' then switch(dodmpdsp)
        else if ch1 in ['a'..'z'] then
          switch(dummy) { pass through unknown options }
        else begin 
          { skip all likely option chars }
          while ch in ['a'..'z','A'..'Z','+','-','0'..'9','_'] do
            nextch;
        end;
      until ch <> ',';
    end (*options*) ;
    
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
    
    procedure escchr(var cp: integer);
    type escstr = packed array [1..5] of char; { escape string }
    var c: char; l: 0..4; i: 1..strglgth;
    
    function match(es: escstr): boolean;
    var i: 1..4;
    begin
      i := 1;
      { move to first mismatch or end }
      while (es[i] = string[cp+i-1]) and (es[i] <> ' ') and (i < 4) do i := i+1;
      match := es[i] = ' '      
    end;
    
    begin
      cp := cp+1; { past '\' }
      c := ' '; { set none found }
      if match('xoff ') then begin c := chr(19); l := 4 end
      else if match('dle  ') then begin c := chr(16); l := 3 end 
      else if match('dc1  ') then begin c := chr(17); l := 3 end  
      else if match('xon  ') then begin c := chr(17); l := 3 end  
      else if match('dc2  ') then begin c := chr(18); l := 3 end  
      else if match('dc3  ') then begin c := chr(19); l := 3 end  
      else if match('dc4  ') then begin c := chr(20); l := 3 end  
      else if match('nak  ') then begin c := chr(21); l := 3 end  
      else if match('syn  ') then begin c := chr(22); l := 3 end  
      else if match('etb  ') then begin c := chr(23); l := 3 end  
      else if match('can  ') then begin c := chr(24); l := 3 end  
      else if match('nul  ') then begin c := chr(0); l := 3 end   
      else if match('soh  ') then begin c := chr(1); l := 3 end   
      else if match('stx  ') then begin c := chr(2); l := 3 end   
      else if match('etx  ') then begin c := chr(3); l := 3 end   
      else if match('eot  ') then begin c := chr(4); l := 3 end   
      else if match('enq  ') then begin c := chr(5); l := 3 end   
      else if match('ack  ') then begin c := chr(6); l := 3 end   
      else if match('bel  ') then begin c := chr(7); l := 3 end   
      else if match('sub  ') then begin c := chr(26); l := 3 end  
      else if match('esc  ') then begin c := chr(27); l := 3 end  
      else if match('del  ') then begin c := chr(127); l := 3 end 
      else if match('bs   ') then begin c := chr(8); l := 2 end   
      else if match('ht   ') then begin c := chr(9); l := 2 end   
      else if match('lf   ') then begin c := chr(10); l := 2 end  
      else if match('vt   ') then begin c := chr(11); l := 2 end  
      else if match('ff   ') then begin c := chr(12); l := 2 end  
      else if match('cr   ') then begin c := chr(13); l := 2 end  
      else if match('so   ') then begin c := chr(14); l := 2 end  
      else if match('si   ') then begin c := chr(15); l := 2 end  
      else if match('em   ') then begin c := chr(25); l := 2 end  
      else if match('fs   ') then begin c := chr(28); l := 2 end  
      else if match('gs   ') then begin c := chr(29); l := 2 end  
      else if match('rs   ') then begin c := chr(30); l := 2 end  
      else if match('us   ') then begin c := chr(31); l := 2 end; 
      if c <> ' ' then begin { replace }
        string[cp-1] := c; { overwrite '\' }
        for i := cp to strglgth-l do string[i] := string[i+l];
        lgth := lgth-l
      end else begin { gap common forced }
        for i := cp-1 to strglgth-1 do string[i] := string[i+1]
      end
    end;

  begin (*insymbol*)
    { copy current to last scanner block }
    lsy := sy; lop := op; lval := val; llgth := lgth; lid := id; lkk := kk;
    if nvalid then begin { there is a lookahead }
      { copy next to current }
      sy := nsy; op := nop; val := nval; lgth := nlgth; id := nid; kk := nkk;
      nvalid := false; { set no next now }
      goto 2 { skip getting next tolken }
    end;
    outline;
  1:
    { Skip both spaces and controls. This allows arbitrary formatting characters
      in the source. }
    repeat while (ch <= ' ') and not eol do nextch;
      test := eol;
      if test then nextch
    until not test;
    if chartp[ch] = illegal then
      begin sy := othersy; op := noop;
        error(399); nextch
      end
    else
    case chartp[ch] of
      letter:
        begin k := 0; ferr := true; for i := 1 to maxids do id[i] := ' ';
          repeat
            if k < maxids then
             begin k := k + 1; id[k] := ch end
            else if ferr then begin error(182); ferr := false end;
            nextch
          until chartp[ch] in [special,illegal,chstrquo,chcolon,
                                chperiod,chlt,chgt,chlparen,chspace,chlcmt];
          if k >= kk then kk := k
          else
            repeat id[kk] := ' '; kk := kk - 1
            until kk = k;
          sy := ident; op := noop;
          if k <= reslen then
            for i := 1 to maxres do
              if strequri(rw[i], id) then
                begin sy := rsy[i]; op := rop[i];
                  { if in ISO 7185 mode and keyword is extended, then revert it
                    to label. Note that forward and external get demoted to
                    "word symbols" in ISO 7185 }
                  if iso7185 and (sy >= forwardsy) then
                    begin sy := ident; op := noop end 
                end;
      end;
      chhex, choct, chbin, number:
        begin op := noop; i := 0; r := 10;
          if chartp[ch] = chhex then begin chkstd; r := 16; nextch end
          else if chartp[ch] = choct then begin chkstd; r := 8; nextch end
          else if chartp[ch] = chbin then begin chkstd; r := 2; nextch end;
          if (r = 10) or (chartp[ch] = number) or (chartp[ch] = letter) then 
          begin
            v := 0;
            repeat 
              if ch <> '_' then
                if v <= maxint div r then 
                  v := v*r+ordint[ch] 
                else begin error(203); v := 0 end;
              nextch
            until (chartp[ch] <> number) and ((chartp[ch] <> letter) or 
                  (r < 16) or iso7185);
            val.intval := true;
            val.ival := v; 
            sy := intconst;
            if ((ch = '.') and (bufinp <> '.') and (bufinp <> ')')) or
               (lcase(ch) = 'e') then
              begin
                { its a real, reject non-decimal radixes }
                if r <> 10 then error(305);
                rv := v; ev := 0;
                if ch = '.' then begin
                  nextch;
                  if chartp[ch] <> number then error(201);
                  repeat 
                    rv := rv*10+ordint[ch]; nextch; ev := ev-1 
                  until chartp[ch] <> number;
                end; 
                if lcase(ch) = 'e' then
                  begin nextch; sgn := +1;
                    if (ch = '+') or (ch ='-') then begin
                      if ch = '-' then sgn := -1;
                      nextch
                    end;
                    if chartp[ch] <> number then error(201)
                    else begin ferr := true; i := 0;
                      repeat
                        if ferr then begin
                          if i <= mxint10 then i := i*10+ordint[ch]
                          else begin error(194); ferr := false end;
                        end;
                        nextch
                      until chartp[ch] <> number;
                      ev := ev+i*sgn
                    end
                  end;
                if ev < 0 then rv := rv/pwrten(ev) else rv := rv*pwrten(ev);
                new(lvp,reel); pshcst(lvp); sy:= realconst;
                lvp^.cclass := reel;
                with lvp^ do lvp^.rval := rv;
                val.intval := false;
                val.valp := lvp
              end
          end else { convert radix to symbol }
            if r = 16 then sy := hexsy
            else if r = 8 then sy := octsy
            else sy := binsy
        end;
      chstrquo:
        begin lgth := 0; sy := stringconst;  op := noop;
          for i := 1 to strglgth do string[i] := ' ';
          repeat
            repeat nextch; lgth := lgth + 1;
                   if lgth <= strglgth then string[lgth] := ch
            until (eol) or (ch = '''');
            if eol then error(202) else nextch
          until ch <> '''';
          string[lgth] := ' '; { get rid of trailing quote }
          lgth := lgth - 1;   (*now lgth = nr of chars in string*)
          { see if string contains character escapes }
          i := 1;
          if not iso7185 then
            while i <= strglgth do begin
            if string[i] = chr(92){\} then begin
              if string[i+1] in ['$','&','%','0'..'9'] then begin
                j := i+1; v := 0; k := 1; 
                { parse in radix and only correct number of digits to keep from
                  eating follow on characters }
                if string[j] = '$' then begin j := j+1;
                  if not (string[j] in ['0'..'9','a'..'f','A'..'F']) then 
                    error(207); 
                  while (string[j] in ['0'..'9', 'a'..'f', 'A'..'F']) and 
                        (k <= 2) do begin
                    v := v*16+ordint[string[j]]; j := j+1; k := k+1
                  end; k := k+1
                end else if string[j] = '&' then begin j := j+1;
                  if not (string[j] in ['0'..'7']) then error(207);
                  while (string[j] in ['0'..'7']) and (k <= 3) do begin
                    v := v*8+ordint[string[j]]; j := j+1; k := k+1
                  end; k := k+1
                end else if string[j] = '%' then begin j := j+1;
                  if not (string[j] in ['0'..'1']) then error(207);
                  while (string[j] in ['0'..'1']) and (k <= 8) do begin
                    v := v*2+ordint[string[j]]; j := j+1; k := k+1
                  end; k := k+1
                end else begin
                  while (string[j] in ['0'..'9']) and (k <= 3) do begin
                    v := v*10+ordint[string[j]]; j := j+1; k := k+1
                  end
                end;
                if v > ordmaxchar then error(222);
                string[i] := chr(v);
                for j := i+1 to strglgth-k+1 do string[j] := string[j+k-1];
                lgth := lgth-k+1
              end else escchr(i) { process force sequence }
            end;
            i := i+1 { pass escaped char or forced char }
          end; 
          if lgth = 1 then begin
            { this is an artifact of the original code. If the string is a
              single character, we store it as an integer even though the
              symbol stays a string }
            val.intval := true; val.ival := ord(string[1])
          end else
            begin
              if lgth = 0 then error(205);
              new(lvp,strg); pshcst(lvp);
              lvp^.cclass:=strg;
              if lgth > strglgth then
                begin error(399); lgth := strglgth end;
              with lvp^ do
                begin slgth := lgth; strassvc(sval, string, strglgth) end;
              val.intval := false;
              val.valp := lvp
            end
        end;
      chcolon:
        begin op := noop; nextch;
          if ch = '=' then
            begin sy := becomes; nextch end
          else sy := colon
        end;
      chperiod:
        begin op := noop; nextch;
          if ch = '.' then begin sy := range; nextch end
          else if ch = ')' then begin sy := rbrack; nextch end
          else sy := period
        end;
      chlt:
        begin nextch; sy := relop;
          if ch = '=' then
            begin op := leop; nextch end
          else
            if ch = '>' then
              begin op := neop; nextch end
            else op := ltop
        end;
      chgt:
        begin nextch; sy := relop;
          if ch = '=' then
            begin op := geop; nextch end
          else op := gtop
        end;
      chlparen:
       begin nextch;
         if ch = '*' then
           begin nextch;
             if (ch = '$') and (incstk = nil) then options;
             repeat
               while (ch <> '}') and (ch <> '*') and not eofinp do nextch;
               iscmte := ch = '}'; nextch
             until iscmte or (ch = ')') or eofinp;
             if not iscmte then nextch; goto 1
           end
         else if ch = '.' then begin sy := lbrack; nextch end
         else sy := lparent;
         op := noop
       end;
      chlcmt:
       begin nextch;
         if ch = '$' then options;
         repeat
            while (ch <> '}') and (ch <> '*') and not eofinp do nextch;
            iscmte := ch = '}'; nextch
         until iscmte or (ch = ')') or eofinp;
         if not iscmte then nextch; goto 1
       end;
      chrem: 
       begin chkstd; 
         repeat nextch until eol; { '!' skip next line }
         goto 1 
       end;
      special:
        begin sy := ssy[ch]; op := sop[ch];
          nextch
        end;
      chspace: sy := othersy
    end; (*case*)

    if dodmplex then begin {  lexical dump }

      writeln;
      write('symbol: ');
      case sy of
         ident:       write('ident: ', id:10);
         intconst:    write('int const: ', val.ival:1);
         realconst:   begin write('real const: ', val.valp^.rval: 9) end;
         stringconst: begin write('string const: ''');
                            writev(output, val.valp^.sval, val.valp^.slgth);
                            write('''') end;
         notsy: write('not'); mulop: write('*'); addop: write('+');
         relop: write('<'); lparent: write('('); rparent: write(')');
         lbrack: write('['); rbrack: write(']'); comma: write(',');
         semicolon: write(';'); period: write('.'); arrow: write('^');
         colon: write(':'); becomes: write(':='); range: write('..');
         labelsy: write('label'); constsy: write('const'); typesy: write('type');
         varsy: write('var'); funcsy: write('function'); progsy: write('program');
         procsy: write('procedure'); setsy: write('set');
         packedsy: write('packed'); arraysy: write('array');
         recordsy: write('record'); filesy: write('file');
         beginsy: write('begin'); ifsy: write('if'); casesy: write('case');
         repeatsy: write('repeat'); whilesy: write('while');
         forsy: write('for'); withsy: write('with'); gotosy: write('goto');
         endsy: write('end'); elsesy: write('else'); untilsy: write('until');
         ofsy: write('of'); dosy: write('do'); tosy: write('to');
         downtosy: write('downto'); thensy: write('then'); 
         forwardsy: write('forward'); modulesy: write('module'); 
         usessy: write('uses'); privatesy:write('private'); 
         externalsy: write('external'); viewsy: write('view'); 
         fixedsy: write('fixed'); processsy: write('process');
         monitorsy: write('monitor'); sharesy: write('share');
         classsy: write('class'); issy: write('is'); 
         overloadsy: write('overload'); overridesy: write('override');
         referencesy: write('reference'); joinssy: write('joins');
         staticsy: write('static'); inheritedsy: write('inherited');
         selfsy: write('self'); virtualsy: write('virtual');
         trysy: write('try'); exceptsy: write('except');
         extendssy: write('extends'); onsy: write('on');
         resultsy: write('result'); operatorsy: write('operator');
         outsy: write('out'); propertysy: write('property');
         channelsy: write('channel'); streamsy: write('stream');
         othersy: write('<other>'); hexsy: write('$'); octsy: write('&');
         binsy: write('%'); numsy: write('#');
      end;
      writeln

    end;
    2:;
  end (*insymbol*) ;
  
  procedure pushback;
  begin
    if nvalid then error(506); { multiple pushbacks }
    { put current tolken to future }
    nsy := sy; nop := op; nval := val; nlgth := lgth; nid := id; nkk := kk;
    { get current from last }
    sy := lsy; op := lop; val := lval; lgth := llgth; id := lid; kk := lkk;
    nvalid := true { set there is a next tolken }
  end;

  procedure enterid(fcp: ctp);
    (*enter id pointed at by fcp into the name-table,
     which on each declaration level is organised as
     an unbalanced binary tree*)
    var lcp, lcp1: ctp; lleft: boolean;
  begin
    lcp := display[top].fname;
    if lcp = nil then
      display[top].fname := fcp
    else
      begin
        repeat lcp1 := lcp;
          if strequvv(lcp^.name, fcp^.name) then begin 
            (*name conflict, follow right link*)
            if incstk <> nil then begin
              writeln; write('*** Duplicate in uses/joins: '); 
              writevp(output, fcp^.name);
              writeln
            end; 
            error(101); 
            lcp := lcp^.rlink; lleft := false
          end else
            if strltnvv(lcp^.name, fcp^.name) then
              begin lcp := lcp^.rlink; lleft := false end
            else begin lcp := lcp^.llink; lleft := true end
        until lcp = nil;
        if lleft then lcp1^.llink := fcp else lcp1^.rlink := fcp
      end;
    fcp^.llink := nil; fcp^.rlink := nil
  end (*enterid*) ;

  procedure searchsection(fcp: ctp; var fcp1: ctp);
    (*to find record fields and forward declared procedure id's
     --> procedure proceduredeclaration
     --> procedure selector*)
     label 1;
  begin
    while fcp <> nil do
      if strequvf(fcp^.name, id) then goto 1
      else if strltnvf(fcp^.name, id) then fcp := fcp^.rlink
        else fcp := fcp^.llink;
1:  fcp1 := fcp
  end (*searchsection*) ;

  procedure schsecidnenm(lcp: ctp; fidcls: setofids; var fcp: ctp; 
                         var mm: boolean);
  label 1;
  begin
    mm := false;
    while lcp <> nil do begin
      if strequvf(lcp^.name, id) then
        if lcp^.klass in fidcls then goto 1
        else begin mm := true; lcp := lcp^.rlink end
        else
          if strltnvf(lcp^.name, id) then lcp := lcp^.rlink
          else lcp := lcp^.llink
    end;
    1: fcp := lcp
  end (*searchidne*) ;

  procedure searchidnenm(fidcls: setofids; var fcp: ctp; var mm: boolean);
    label 1;
    var disxl: disprange;
  begin
    mm := false; disx := 0;
    for disxl := top downto 0 do
      begin 
        schsecidnenm(display[disxl].fname, fidcls, fcp, mm);
        if fcp <> nil then begin disx := disxl; goto 1 end
      end;
    1:;
  end (*searchidne*) ;

  procedure searchidne(fidcls: setofids; var fcp: ctp);
    var mm: boolean;
  begin
    searchidnenm(fidcls, fcp, mm);
    if mm then error(103)
  end (*searchidne*) ;

  procedure schsecidne(lcp: ctp; fidcls: setofids; var fcp: ctp);
    var mm: boolean;
  begin
    schsecidnenm(lcp, fidcls, fcp, mm);
    if mm then error(103)
  end (*searchidne*) ;
  
  procedure searchid(fidcls: setofids; var fcp: ctp);
    label 1;
    var lcp: ctp; pn, fpn: disprange; pdf: boolean;
  begin
    searchidne(fidcls, lcp); { perform no error search }
    if lcp = nil then begin
      { search module leader in the pile }
      pdf := false; 
      if ptop > 0 then for pn := ptop-1 downto 0 do 
        if strequvf(pile[pn].modnam, id) then begin fpn := pn; pdf := true end;
      if pdf then begin { module name was found }
        insymbol; if sy <> period then error(21) else insymbol;
        if sy <> ident then error(2)
        else schsecidne(pile[fpn].fname,fidcls,lcp); { search qualifed name }
        if lcp = nil then error(268) { not found }
      end
    end;    
    if lcp <> nil then begin lcp^.refer := true; goto 1 end; { found }
    (*search not successful
     --> procedure simpletype*)
      error(104);
      (*to avoid returning nil, reference an entry
       for an undeclared id of appropriate class
       --> procedure enterundecl*)
      if types in fidcls then lcp := utypptr
      else
        if vars in fidcls then lcp := uvarptr
        else
          if field in fidcls then lcp := ufldptr
          else
            if konst in fidcls then lcp := ucstptr
            else
              if proc in fidcls then lcp := uprcptr
              else lcp := ufctptr;
1:  fcp := lcp
  end (*searchid*) ;
  
  procedure getbounds(fsp: stp; var fmin,fmax: integer);
    (*get internal bounds of subrange or scalar type*)
    (*assume fsp<>intptr and fsp<>realptr*)
  begin
    fmin := 0; fmax := 0;
    if fsp <> nil then
    with fsp^ do
      if form = subrange then
        begin fmin := min.ival; fmax := max.ival end
      else
          if fsp = charptr then
            begin fmin := ordminchar; fmax := ordmaxchar
            end
          else
            if fsp = intptr then
              begin fmin := -maxint; fmax := maxint
              end
            else
              if fconst <> nil then
                fmax := fconst^.values.ival
  end (*getbounds*) ;

  function isbyte(fsp: stp): boolean;
    { check structure is byte }
  var fmin, fmax: integer;
  begin 
    getbounds(fsp, fmin, fmax); 
    isbyte := (fmin >= 0) and (fmax <= 255)
  end;
  
  function basetype(fsp: stp): stp;
    { remove any subrange types }
  function issub(fsp: stp): boolean;
  begin
     if fsp <> nil then issub := fsp^.form = subrange
     else issub := false
  end;
  begin
    if fsp <> nil then
      while issub(fsp) do
        fsp := fsp^.rangetype;
    basetype := fsp
  end;

  { alignment for general memory placement }
  function alignquot(fsp: stp): integer;
  begin
    alignquot := 1;
    if fsp <> nil then
      with fsp^ do
        case form of
          scalar:   if fsp=intptr then alignquot := intal
                    else if fsp=boolptr then alignquot := boolal
                    else if scalkind=declared then alignquot := intal
                    else if fsp=charptr then alignquot := charal
                    else if fsp=realptr then alignquot := realal
                    else (*parmptr*) alignquot := parmal;
          subrange: alignquot := alignquot(rangetype);
          pointer:  alignquot := adral;
          power:    alignquot := setal;
          files:    alignquot := fileal;
          arrays:   alignquot := alignquot(aeltype);
          records:  alignquot := recal;
          exceptf:  alignquot := exceptal;
          variant,tagfld: error(501)
        end
  end (*alignquot*);

  procedure alignu(fsp: stp; var flc: addrrange);
    var k,l: integer;
  begin
    k := alignquot(fsp);
    l := flc-1;
    flc := l + k  -  (k+l) mod k
  end (*align*);
  
  procedure alignd(fsp: stp; var flc: stkoff);
    var k,l: integer;
  begin
    k := alignquot(fsp);
    l := flc+1;
    flc := l - k  +  (k-l) mod k;
  end (*align*);
  
  { align for stack }
  function aligns(flc: addrrange): addrrange;
    var l: integer;
  begin
    l := flc+1;
    flc := l - stackal  +  (stackal-l) mod stackal;
    aligns := flc
  end (*aligns*);

  procedure printtables(fb: boolean);
    (*print data structure and name table*)

    var i, lim: disprange;

    procedure marker;
      (*mark data structure entries to avoid multiple printout*)
      var i: integer;

      procedure markctp(fp: ctp); forward;

      procedure markstp(fp: stp);
        (*mark data structures, prevent cycles*)
      begin
        if fp <> nil then
          with fp^ do
            begin marked := true;
              case form of
              scalar:   ;
              subrange: markstp(rangetype);
              pointer:  (*don't mark eltype: cycle possible; will be marked
                        anyway, if fp = true*) ;
              power:    markstp(elset) ;
              arrays:   begin markstp(aeltype); markstp(inxtype) end;
              records:  begin markctp(fstfld); markstp(recvar) end;
              files:    markstp(filtype);
              tagfld:   markstp(fstvar);
              variant:  begin markstp(nxtvar); markstp(subvar) end;
              exceptf:  ;
              end (*case*)
            end (*with*)
      end (*markstp*);

      procedure markctp{(fp: ctp)};
      begin
        if fp <> nil then
          with fp^ do
            begin markctp(llink); markctp(rlink);
              markstp(idtype)
            end
      end (*markctp*);

    begin (*marker*)
      for i := top downto lim do
        markctp(display[i].fname)
    end (*marker*);
    
    procedure wrtctp(ip: ctp);
    begin
      if ip = nil then write('<nil>':intdig) else write(ip^.snm:intdig)
    end;
    
    procedure wrtstp(sp: stp);
    begin
      if sp = nil then write('<nil>':intdig) else write(sp^.snm:intdig)
    end;

    procedure followctp(fp: ctp); forward;

    procedure followstp(fp: stp);
    begin
      if fp <> nil then
        with fp^ do
          if marked then
            begin marked := false; write('S: '); wrtstp(fp); 
              write(' ', size:intdig, ' ');
              case form of
              scalar:   begin write('scalar':intdig, ' ');
                          if scalkind = standard then
                            write('standard':intdig)
                          else write('declared':intdig,' '); wrtctp(fconst);
                          writeln(output)
                        end;
              subrange: begin
                          write('subrange':intdig,' '); wrtstp(rangetype); write(' ');
                          if rangetype <> realptr then
                            write(min.ival:intdig, ' ', max.ival:intdig)
                          else
                            if (min.valp <> nil) and (max.valp <> nil) then begin
                              write(' '); write(min.valp^.rval:9);
                              write(' '); write(max.valp^.rval:9)
                            end;
                          writeln; followstp(rangetype);
                        end;
              pointer:  begin write('pointer':intdig,' '); wrtstp(eltype); 
                              writeln end;
              power:    begin write('set':intdig,' '); wrtstp(elset); writeln;
                          followstp(elset)
                        end;
              arrays:   begin
                          write('array':intdig,' '); wrtstp(aeltype); 
                          write(' '); wrtstp(inxtype); writeln;
                          followstp(aeltype); followstp(inxtype)
                        end;
              records:  begin
                          write('record':intdig,' '); wrtctp(fstfld); write(' ');
                          wrtstp(recvar); write(' '); wrtstp(recyc); writeln;
                          followctp(fstfld); followstp(recvar)
                        end;
              files:    begin write('file':intdig,' '); wrtstp(filtype); writeln;
                          followstp(filtype)
                        end;
              tagfld:   begin write('tagfld':intdig,' '); wrtctp(tagfieldp);
                          write(' '); wrtstp(fstvar); writeln;
                          followstp(fstvar)
                        end;
              variant:  begin write('variant':intdig,' '); wrtstp(nxtvar);
                          write(' '); wrtstp(subvar); write(' '); wrtstp(caslst);
                          writeln(' ',varval.ival);
                          followstp(nxtvar); followstp(subvar)
                        end;
              exceptf:  begin writeln('except':intdig) end
              end (*case*)
            end (*if marked*)
    end (*followstp*);

    procedure followctp{(fp: ctp)};
    begin
      if fp <> nil then
        with fp^ do
          begin write('C: '); wrtctp(fp); write(' ');
                writev(output, name, intdig); write(' '); wrtctp(llink);
                write(' '); wrtctp(rlink); write(' '); wrtstp(idtype); write(' ');
            case klass of
              types: write('type':intdig);
              konst: begin write('constant':intdig,' '); wrtctp(next); write(' ');
                       if idtype <> nil then
                         if idtype = realptr then
                           begin
                             if values.valp <> nil then begin
                               write(values.valp^.rval:9)
                             end
                           end
                         else
                           if idtype^.form = arrays then  (*stringconst*)
                             begin
                               if values.valp <> nil then
                                 begin
                                   with values.valp^ do
                                     writev(output, sval, slgth)
                                 end
                             end
                           else write(values.ival:intdig)
                     end;
              vars:  begin write('variable':intdig, ' ');
                       if vkind = actual then write('actual':intdig)
                       else write('formal':intdig);
                       write(' '); wrtctp(next); 
                       write(' ', vlev:intdig,' ',vaddr:intdig, ' ');
                       if threat then write('threat':intdig) else write(' ':intdig);
                       write(' ', forcnt:intdig, ' ');
                       case part of 
                         ptval: write('value':intdig, ' '); 
                         ptvar: write('var':intdig, ' ');
                         ptview: write('view':intdig, ' ');
                         ptout:write('out':intdig, ' ');
                       end;
                       if hdr then write('header':intdig) else write(' ':intdig);
                       if vext then write('external':intdig) else write(' ':intdig);
                       if vext then write(vmod^.fn:intdig) else write(' ':intdig)
                     end;
              field: begin write('field':intdig,' '); wrtctp(next); write(' ');
                           write(fldaddr:intdig,' '); wrtstp(varnt); write(' ');
                           wrtctp(varlb); write(' ');
                       if tagfield then write('tagfield':intdig) else write(' ':intdig);
                       write(' ', taglvl:intdig, ' ',varsaddr:intdig, ' ', varssize:intdig)
                     end;             
              proc,
              func:  begin
                       if klass = proc then write('procedure':intdig, ' ')
                       else write('function':intdig, ' ');
                       if asgn then write('assigned':intdig, ' ') else write(' ':intdig, ' ');
                       if pext then write('external':intdig, ' ') else write(' ':intdig, ' ');
                       if pext then write(pmod^.fn:intdig) else write(' ':intdig);
                       if pfdeckind = standard then
                         write('standard':intdig, '-', key:intdig)
                       else
                         begin write('declared':intdig,'-'); wrtctp(next); write('-');
                           write(pflev:intdig,' ',pfname:intdig, ' ');
                           if pfkind = actual then
                             begin write('actual':intdig, ' ');
                               if forwdecl then write('forward':intdig, ' ')
                               else write('notforward':intdig, ' ');
                               if externl then write('extern':intdig)
                               else write('not extern':intdig);
                             end
                           else write('formal':intdig)
                         end
                     end
            end (*case*);
            writeln;
            followctp(llink); followctp(rlink);
            followstp(idtype)
          end (*with*)
    end (*followctp*);

  begin (*printtables*)
    writeln(output); writeln(output); writeln(output);
    if fb then lim := 0
    else begin lim := top; write(' local') end;
    writeln(' tables:'); writeln(output);
    writeln('C: ', 'Entry #':intdig, ' ', 'Id':intdig, ' ', 'llink':intdig, ' ',
            'rlink':intdig, ' ', 'Typ':intdig, ' ', 'Class':intdig);
    writeln('S: ', 'Entry #':intdig, ' ', 'Size':intdig, ' ', 'Form ':intdig);
    write('===============================================================');
    writeln('==========================');
    marker;
    for i := top downto lim do
      followctp(display[i].fname);
    writeln(output);
    if not eol then write(' ':chcnt+16)
  end (*printtables*);

  procedure chkrefs(p: ctp; var w: boolean);
  begin
    if chkref then begin
      if p <> nil then begin
        chkrefs(p^.llink, w); { check left }
        chkrefs(p^.rlink, w); { check right }
        if not p^.refer then begin if not w then writeln;
          writev(output, p^.name, 10); writeln(' unreferenced'); w := true
        end
      end
    end
  end;
  
  function chkext(fcp: ctp): boolean;
  begin chkext := false;
    if fcp <> nil then begin
      if fcp^.klass = vars then chkext := fcp^.vext
      else if (fcp^.klass = proc) or (fcp^.klass = func) then 
        chkext := fcp^.pext
    end
  end;

  procedure genlabel(var nxtlab: integer);
  begin intlabel := intlabel + 1;
    nxtlab := intlabel
  end (*genlabel*);

  procedure prtlabel(labname: integer);
  begin
    write(prr, 'l '); writevp(prr, nammod); write(prr, '.', labname:1)
  end;
  
  procedure prtflabel(fcp: ctp);
  begin
    write(prr, 'l '); 
    if fcp^.klass = vars then writevp(prr, fcp^.vmod^.mn) 
    else writevp(prr, fcp^.pmod^.mn); 
    write(prr, '.'); 
    writevp(prr, fcp^.name)
  end;

  procedure putlabel(labname: integer);
  begin 
    if prcode then begin prtlabel(labname); writeln(prr) end 
  end (*putlabel*);
      
  procedure searchlabel(var llp: lbp; level: disprange; isid: boolean);
  var fllp: lbp; { found label entry }
  begin
    fllp := nil; { set no label found }
    llp := display[level].flabel; { index top of label list }
    while llp <> nil do begin { traverse }
      if isid and (llp^.labid <> nil) then begin { id type label }
        if strequvf(llp^.labid, id) then begin
          fllp := llp; { set entry found }
          llp := nil { stop }        
        end else llp := llp^.nextlab { next in list }
      end else if not isid and (llp^.labval = val.ival) then begin { found }
        fllp := llp; { set entry found }
        llp := nil { stop }
      end else llp := llp^.nextlab { next in list }
    end;
    llp := fllp { return found entry or nil }
  end;

  procedure newlabel(var llp: lbp; isid: boolean);
  var lbname: integer;
  begin
    with display[top] do
      begin getlab(llp);
        with llp^ do
          begin labid := nil; labval := 0; 
            if isid then strassvf(labid, id) { id type label }
            else labval := val.ival; { numeric type label }
            if labval > 9999 then error(261);
            genlabel(lbname); defined := false; nextlab := flabel; 
            labname := lbname; vlevel := level; slevel := 0; 
            ipcref := false; minlvl := maxint; bact := false; 
            refer := false
          end;
        flabel := llp
      end
  end;

  procedure prtlabels;
  var llp: lbp; { found label entry }
  begin
    writeln;
    writeln('Labels: ');
    writeln;
    llp := display[level].flabel; { index top of label list }
    while llp <> nil do with llp^ do begin { traverse }
      writeln('label: ', labval:1, ' defined: ', defined,
              ' internal: ', labname:1, ' vlevel: ', vlevel:1,
              ' slevel: ', slevel:1, ' ipcref: ', ipcref:1,
              ' minlvl: ', minlvl:1);
      writeln('   bact: ', bact);
      llp := llp^.nextlab { next in list }
    end
  end;

  procedure mesl(i: integer);
  begin topnew := topnew + i;
    if topnew < topmin then topmin := topnew;
    if toterr = 0 then
      if (topnew > 0) and prcode then error(500) { stack should never go positive }
  end;
  
  procedure mes(i: integer);
  begin mesl(cdx[i]) end;
  
  procedure mest(i: integer; fsp: stp);
  
    function mestn(fsp: stp): integer;
    var ss: integer;
    begin ss := 1;
      if fsp<>nil then
        with fsp^ do
          case form of
           scalar: if fsp=intptr then ss := 1
                   else
                     if fsp=boolptr then ss := 3
                     else
                       if fsp=charptr then ss := 4
                       else
                         if scalkind = declared then ss := 1
                         else ss := 2;
           subrange: ss := mestn(rangetype);
           pointer,
           files,
           exceptf:  ss := 5;
           power:    ss := 6;
           records,arrays: ss := 7;
           tagfld,variant: error(501)
          end;
      mestn := ss
    end;
  
  begin (*mest*)
    if (cdx[i] < 1) or (cdx[i] > 6) then error(502);
    mesl(cdxs[cdx[i]][mestn(fsp)]);
  end (*mest*);

  procedure putic;
  begin if ic mod 10 = 0 then writeln(prr,'i',ic:5) end;

  procedure gen0(fop: oprange);
  begin
    if prcode then begin putic; writeln(prr,mn[fop]:4) end;
    ic := ic + 1; mes(fop)
  end (*gen0*) ;

  procedure gen1s(fop: oprange; fp2: integer; symptr: ctp);
    var k, j: integer; p: strvsp;
  begin
    if prcode then
      begin putic; write(prr,mn[fop]:4);
        if fop = 30 then
          begin writeln(prr,sna[fp2]:12);
            mesl(pdx[fp2]);
          end
        else
          begin
            if fop = 38 then
               begin with cstptr[fp2]^ do begin p := sval; j := 1;
                   write(prr,' ',slgth:4,' ''');
                   for k := 1 to lenpv(p) do begin
                     if p^.str[j] = '''' then write(prr, '''''')
                     else write(prr,p^.str[j]:1);
                     j := j+1; if j > varsqt then begin
                       p := p^.next; j := 1
                     end
                   end
                 end;
                 writeln(prr,'''')
               end
            else if fop = 42 then writeln(prr,chr(fp2))
            else if fop = 67 then writeln(prr,fp2:4)
            else if chkext(symptr) then 
              begin write(prr, ' '); prtflabel(symptr); writeln(prr) end
            else writeln(prr,fp2:12);
            if fop = 42 then mes(0)
            else mes(fop)
          end
      end;
    ic := ic + 1
  end (*gen1s*) ;
  
  procedure gen1(fop: oprange; fp2: integer);
  begin
    gen1s(fop, fp2, nil)
  end;
  
  procedure gen2(fop: oprange; fp1,fp2: integer);
    var k : integer;
  begin
    if prcode then
      begin putic; write(prr,mn[fop]:4);
        case fop of
          45,50,54,56,74,62,63,81,82: 
            begin
              writeln(prr,' ',fp1:3,fp2:8);
              mes(fop)
            end;
          47,48,49,52,53,55:
            begin write(prr,chr(fp1));
              if chr(fp1) = 'm' then write(prr,' ',fp2:11);
              writeln(prr);
              case chr(fp1) of
                'i': mesl(cdxs[cdx[fop]][1]);
                'r': mesl(cdxs[cdx[fop]][2]);
                'b': mesl(cdxs[cdx[fop]][3]);
                'c': mesl(cdxs[cdx[fop]][4]);
                'a': mesl(cdxs[cdx[fop]][5]);
                's': mesl(cdxs[cdx[fop]][6]);
                'm': mesl(cdxs[cdx[fop]][7])
              end
            end;
          51:
            begin
              case fp1 of
                1: begin writeln(prr,'i ',fp2:1);
                     mesl(cdxs[cdx[fop]][1]) 
                   end;
                2: begin write(prr,'r ');
                     with cstptr[fp2]^ do write(prr,rval:23);
                     writeln(prr);
                     mesl(cdxs[cdx[fop]][2]);
                   end;
                3: begin writeln(prr,'b ',fp2:1);
                     mesl(cdxs[cdx[fop]][3]) 
                   end;
                4: begin writeln(prr,'n');
                     mesl(-ptrsize)
                   end;
                6: begin
                if chartp[chr(fp2)] = illegal then
                     { output illegal characters as numbers }
                     writeln(prr,'c  ':3,fp2:1)
                   else 
                     writeln(prr,'c ''':3,chr(fp2),'''');
                     mesl(cdxs[cdx[fop]][4])
                   end;
                5: begin write(prr,'s(');
                     with cstptr[fp2]^ do
                       for k := setlow to sethigh do
                         if k in pval then write(prr,k:4);
                     writeln(prr,')');
                     mesl(cdxs[cdx[fop]][6])
                   end
              end
            end
        end
      end;
    ic := ic + 1
  end (*gen2*) ;

  procedure gentypindicator(fsp: stp);
  begin
    if fsp<>nil then
      with fsp^ do
        case form of
         scalar: if fsp=intptr then write(prr,'i')
                 else
                   if fsp=boolptr then write(prr,'b')
                   else
                     if fsp=charptr then write(prr,'c')
                     else
                       if scalkind = declared then begin
                         if fsp^.size = 1 then write(prr, 'x')
                         else write(prr,'i')
                       end else write(prr,'r');
         subrange: if fsp^.size = 1 then write(prr, 'x')
                   else gentypindicator(rangetype);
         pointer,
         files,
         exceptf:  write(prr,'a');
         power:    write(prr,'s');
         records,arrays: write(prr,'m');
         tagfld,variant: error(503)
        end
  end (*typindicator*);

  procedure gen0t(fop: oprange; fsp: stp);
  begin
    if prcode then
      begin putic;
        write(prr,mn[fop]:4);
        gentypindicator(fsp);
        writeln(prr);
      end;
    ic := ic + 1; mest(fop, fsp)
  end (*gen0t*);

  procedure gen1ts(fop: oprange; fp2: integer; fsp: stp; symptr: ctp);
  begin
    if prcode then
      begin putic;
        write(prr,mn[fop]:4);
        gentypindicator(fsp);
        write(prr, ' ');
        if chkext(symptr) then prtflabel(symptr) else write(prr,fp2:11);
        writeln(prr)
      end;
    ic := ic + 1; mest(fop, fsp)
  end (*gen1ts*);
  
  procedure gen1t(fop: oprange; fp2: integer; fsp: stp);
  begin
    gen1ts(fop, fp2, fsp, nil)
  end;

  procedure gen2t(fop: oprange; fp1,fp2: integer; fsp: stp);
  begin
    if prcode then
      begin putic;
        write(prr,mn[fop]: 4);
        gentypindicator(fsp);
        writeln(prr,' ', fp1:3+5*ord(abs(fp1)>99),fp2:11);
      end;
    ic := ic + 1; mest(fop, fsp)
  end (*gen2t*);

  procedure genujpxjp(fop: oprange; fp2: integer);
  begin
   if prcode then
      begin putic; write(prr,mn[fop]:4, ' '); prtlabel(fp2); writeln(prr) end;
    ic := ic + 1; mes(fop)
  end (*genujpxjp*);
  
  procedure gencjp(fop: oprange; fp1,fp2,fp3: integer);
  begin
   if prcode then
      begin putic; 
        write(prr,mn[fop]:4, ' ', fp1:3+5*ord(abs(fp1)>99),fp2:11,
                    ' '); prtlabel(fp3); writeln(prr) 
      end;
    ic := ic + 1; mes(fop)
  end (*gencjp*);

  procedure genipj(fop: oprange; fp1, fp2: integer);
  begin
   if prcode then
      begin putic; write(prr,mn[fop]:4,fp1:4,' '); prtlabel(fp2); writeln(prr) end;
    ic := ic + 1; mes(fop)
  end (*genipj*);

  procedure gencupent(fop: oprange; fp1,fp2: integer; fcp: ctp);
  begin
    if prcode then
      begin putic;
        if fop = 32 then begin { create ents or ente instructions }
          if fp1 = 1 then 
            begin write(prr,mn[fop]:4,'s '); prtlabel(fp2) end
          else 
            begin write(prr,mn[fop]:4,'e '); prtlabel(fp2) end;
          writeln(prr);
          mes(fop)
        end else begin
          write(prr,mn[fop]:4,fp1:4,' ');
          if chkext(fcp) then prtflabel(fcp) else prtlabel(fp2);
          writeln(prr);
          mesl(fp1)
        end
      end;
    ic := ic + 1
  end;

  procedure genlpa(fp1,fp2: integer);
  begin
    if prcode then
      begin putic;
        write(prr,mn[68]:4,fp2:4, ' '); prtlabel(fp1); writeln(prr);
      end;
    ic := ic + 1; mes(68)
  end (*genlpa*);
  
  procedure gensuv(fp1, fp2: integer; sym: ctp);
  begin
    if prcode then begin
      putic; write(prr,mn[92(*suv*)]:4);
      write(prr, ' '); prtlabel(fp1); 
      if chkext(sym) then 
        begin write(prr, ' '); prtflabel(sym); writeln(prr) end
      else writeln(prr, ' ', fp2:1)
    end;
    ic := ic + 1; mes(92)
  end;

  function comptypes(fsp1,fsp2: stp) : boolean; forward;

  function stringt(fsp: stp) : boolean;
  var fmin, fmax: integer;
  begin stringt := false;
    if fsp <> nil then
      if fsp^.form = arrays then
        if fsp^.packing then begin
        { if the index is nil, either the array is a string constant or the
          index type was in error. Either way, we call it a string }
        if fsp^.inxtype = nil then fmin := 1
        else getbounds(fsp^.inxtype,fmin,fmax);
        if comptypes(fsp^.aeltype,charptr) and (fmin = 1) then stringt := true
      end
  end (*stringt*);
  
  { check structure is, or contains, a file }
  function filecomponent(fsp: stp): boolean;
  var f: boolean;
    { tour identifier tree }
    function filecomponentre(lcp: ctp): boolean;
    var f: boolean;
    begin
      f := false; { set not file by default }
      if lcp <> nil then with lcp^ do begin
        if filecomponent(idtype) then f := true;
        if filecomponentre(llink) then f := true;
        if filecomponentre(rlink) then f := true
      end;
      filecomponentre := f
    end;
  begin
    f := false; { set not a file by default }
    if fsp <> nil then with fsp^ do case form of
      scalar:   ;
      subrange: ;
      pointer:  ;
      power:    ;
      arrays:   if filecomponent(aeltype) then f := true;
      records:  if filecomponentre(fstfld) then f := true;
      files:    f := true;
      tagfld:   ;
      variant:  ;
      exceptf:  ;
    end;
    filecomponent := f
  end;
    
  function comptypes{(fsp1,fsp2: stp) : boolean};
    (*decide whether structures pointed at by fsp1 and fsp2 are compatible*)
  begin
    comptypes := false; { set default is false }
    { remove any subranges }
    fsp1 := basetype(fsp1);
    fsp2 := basetype(fsp2);
    { Check equal. Aliases of the same type will also be equal. }
    if fsp1 = fsp2 then comptypes := true
    else
      if (fsp1 <> nil) and (fsp2 <> nil) then
        if fsp1^.form = fsp2^.form then
          case fsp1^.form of
            scalar: ;
            { Subranges are compatible if either type is a subrange of the
              other, or if the base type is the same. }
            subrange: ; { done above }
            { Sets are compatible if they have the same base types and packed/
              unpacked status, or one of them is the empty set. The empty set
              is indicated by a nil base type, which is identical to a base
              type in error. Either way, we treat them as compatible.

              Set types created for set constants have a flag that disables
              packing matches. This is because set constants can be packed or
              unpacked by context. }
            power: comptypes := (comptypes(fsp1^.elset, fsp2^.elset) and
                                  ((fsp1^.packing = fsp2^.packing) or
                                   not fsp1^.matchpack or
                                   not fsp2^.matchpack)) or
                                (fsp1^.elset = nil) or (fsp2^.elset = nil);
            { Arrays are compatible if they are string types and equal in size }
            arrays: comptypes := stringt(fsp1) and stringt(fsp2) and
                                 (fsp1^.size = fsp2^.size );
            { Pointers, must either be the same type or aliases of the same
              type, or one must be nil. The nil pointer is indicated by a nil
              base type, which is identical to a base type in error. Either
              way, we treat them as compatible. }
            pointer: comptypes := (fsp1^.eltype = nil) or (fsp2^.eltype = nil);
            { records and files must either be the same type or aliases of the
              same type }
            records: ;
            files:
          end (*case*)
        else (*fsp1^.form <> fsp2^.form*)
          { subranges of a base type match the base type }
          if fsp1^.form = subrange then
            comptypes := fsp1^.rangetype = fsp2
          else
            if fsp2^.form = subrange then
              comptypes := fsp1 = fsp2^.rangetype
            else comptypes := false
      else comptypes := true { one of the types is in error }
  end (*comptypes*) ;

  procedure skip(fsys: setofsys);
  (*skip input string until relevant symbol found*)
  begin
    if not eofinp then
      begin while not(sy in fsys) and (not eofinp) do insymbol;
        if not (sy in fsys) then insymbol
      end
  end (*skip*) ;
      
  procedure constexpr(fsys: setofsys; var fsp: stp; var fvalu: valu); forward;
  
  procedure constfactor(fsys: setofsys; var fsp: stp; var fvalu: valu);
    var lsp: stp; lcp: ctp;
  begin lsp := nil; fvalu.intval := true; fvalu.ival := 0;
    if not(sy in constbegsys) then
      begin error(50); skip(fsys+constbegsys) end;
    if sy in constbegsys then
      begin
        if sy = lparent then begin chkstd;
          insymbol; constexpr(fsys+[rparent], fsp, fvalu);
          if sy = rparent then insymbol else error(4);
          lsp := fsp
        end else if sy = notsy then begin chkstd;
          insymbol; constfactor(fsys+[rparent], fsp, fvalu);
          if (fsp <> intptr) and (fsp <> boolptr) then error(134)
          else if fvalu.ival < 0 then error(213)
          else fvalu.ival := bnot(fvalu.ival);
          { not boolean does not quite work here }
          if fsp = boolptr then fvalu.ival := band(fvalu.ival, 1);
          lsp := fsp
        end else if sy = stringconst then
          begin
            { note: this is a bit redundant since insymbol does this 
              conversion }
            if lgth = 1 then lsp := charptr
            else
              begin
                new(lsp,arrays); pshstc(lsp);
                with lsp^ do
                  begin aeltype := charptr; inxtype := nil;
                     size := lgth*charsize; form := arrays;
                     packing := true
                  end
              end;
            fvalu := val; insymbol
          end
        else
          begin
            if sy = ident then
              begin searchid([konst],lcp);
                with lcp^ do
                  begin lsp := idtype; fvalu := values end;
                insymbol;
              end
            else
              if sy = intconst then
                begin lsp := intptr; fvalu := val; insymbol end
              else
                if sy = realconst then
                  begin lsp := realptr; fvalu := val; insymbol end
                else
                  begin error(106); skip(fsys) end
          end;
        if not (sy in fsys) then
          begin error(6); skip(fsys) end
        end;
    fsp := lsp
  end (*constfactor*) ;

  procedure constterm(fsys: setofsys; var fsp: stp; var fvalu: valu);
  var lvp: csp; lv: valu; lop: operatort; lsp: stp; 
  begin
    constfactor(fsys+[mulop], fsp, fvalu);
    while (sy = mulop) and (op in [mul,rdiv,idiv,imod,andop]) do begin
      chkstd; lv := fvalu; lsp := fsp; lop := op; insymbol; 
      constfactor(fsys+[mulop], fsp, fvalu);
      lvp := nil;
      if ((lop in [mul,minus]) and ((lsp = realptr) or (fsp = realptr))) or
         (lop = rdiv) then 
        begin new(lvp,reel); pshcst(lvp); lvp^.cclass := reel end;
      case lop of { operator }
        { * } mul: if (lsp = intptr) and (fsp = intptr) then
                      fvalu.ival := lv.ival*fvalu.ival
                    else if (lsp = realptr) and (fsp = realptr) then
                      lvp^.rval := lv.valp^.rval*fvalu.valp^.rval
                    else if (lsp = realptr) and (fsp = intptr) then
                      lvp^.rval := lv.valp^.rval*fvalu.ival
                    else if (lsp = intptr) and (fsp = realptr) then
                      lvp^.rval := lv.ival*fvalu.valp^.rval
                    else error(134);
        { / } rdiv: if (lsp = intptr) and (fsp = intptr) then
                       lvp^.rval := lv.ival/fvalu.ival
                     else if (lsp = realptr) and (fsp = realptr) then
                       lvp^.rval := lv.valp^.rval/fvalu.valp^.rval
                     else if (lsp = realptr) and (fsp = intptr) then
                       lvp^.rval := lv.valp^.rval/fvalu.ival
                     else if (lsp = intptr) and (fsp = realptr) then
                       lvp^.rval := lv.ival/fvalu.valp^.rval
                     else error(134);
        { div } idiv: if (lsp = intptr) and (fsp = intptr) then
                       fvalu.ival := lv.ival div fvalu.ival
                     else error(134);
        { mod } imod: if (lsp = intptr) and (fsp = intptr) then
                       fvalu.ival := lv.ival mod fvalu.ival
                     else error(134);
        { and } andop: if ((lsp = intptr) and (fsp = intptr)) or
                         ((lsp = boolptr) and (fsp = boolptr)) then
                       if (lv.ival < 0) or (fvalu.ival < 0) then error(213)
                       else fvalu.ival := band(lv.ival, fvalu.ival)
                     else error(134);
      end;
      if lvp <> nil then fvalu.valp := lvp; { place result }
      { mixed types or / = real }
      if (lsp = realptr) or (lop = rdiv) then fsp := realptr
    end
  end (*constterm*) ;

  procedure constexpr{(fsys: setofsys; var fsp: stp; var fvalu: valu)};
  var sign: (none,pos,neg); lvp,svp: csp; lv: valu; lop: operatort; lsp: stp;
  begin sign := none; svp := nil;
    if (sy = addop) and (op in [plus,minus]) then
              begin if op = plus then sign := pos else sign := neg;
                insymbol
              end;
    constterm(fsys+[addop], fsp, fvalu);
    if sign > none then begin { apply sign to number }
      if (fsp <> intptr) and (fsp <> realptr) then error(106);
      if sign = neg then { must flip sign }
        if fsp = intptr then fvalu.ival := -fvalu.ival
        else begin new(lvp,reel); pshcst(lvp); lvp^.cclass := reel;
          lvp^.rval := -fvalu.valp^.rval; fvalu.valp := lvp; svp := lvp;
        end
    end;
    while (sy = addop) and (op in [plus,minus,orop,xorop]) do begin
      chkstd; lv := fvalu; lsp := fsp; lop := op; insymbol;
      constterm(fsys+[addop], fsp, fvalu); 
      lvp := nil;
      if (lop in [plus,minus]) and ((lsp = realptr) or (fsp = realptr)) then 
        begin new(lvp,reel); pshcst(lvp); lvp^.cclass := reel end;
      case lop of { operator }
        { + } plus: if (lsp = intptr) and (fsp = intptr) then
                      fvalu.ival := lv.ival+fvalu.ival
                    else if (lsp = realptr) and (fsp = realptr) then
                      lvp^.rval := lv.valp^.rval+fvalu.valp^.rval
                    else if (lsp = realptr) and (fsp = intptr) then
                      lvp^.rval := lv.valp^.rval+fvalu.ival
                    else if (lsp = intptr) and (fsp = realptr) then
                      lvp^.rval := lv.ival+fvalu.valp^.rval
                    else error(134);
        { - } minus: if (lsp = intptr) and (fsp = intptr) then
                       fvalu.ival := lv.ival-fvalu.ival
                     else if (lsp = realptr) and (fsp = realptr) then
                       lvp^.rval := lv.valp^.rval-fvalu.valp^.rval
                     else if (lsp = realptr) and (fsp = intptr) then
                       lvp^.rval := lv.valp^.rval-fvalu.ival
                     else if (lsp = intptr) and (fsp = realptr) then
                       lvp^.rval := lv.ival-fvalu.valp^.rval
                     else error(134);
        { or } orop: if ((lsp = intptr) and (fsp = intptr)) or
                         ((lsp = boolptr) and (fsp = boolptr)) then
                       if (lv.ival < 0) or (fvalu.ival < 0) then error(213)
                       else fvalu.ival := bor(lv.ival, fvalu.ival)
                     else error(134);
        { xor } xorop: if ((lsp = intptr) and (fsp = intptr)) or
                         ((lsp = boolptr) and (fsp = boolptr)) then
                       if (lv.ival < 0) or (fvalu.ival < 0) then error(213)
                       else fvalu.ival := bxor(lv.ival, fvalu.ival)
                     else error(134)
      end;
      { if left negated, recycle it just once }
      if svp <> nil then begin putcst(svp); svp := nil end;
      if lvp <> nil then fvalu.valp := lvp; { place result }
      if lsp = realptr then fsp := realptr { mixed types = real }
    end
  end (*constexpr*) ;
    
  procedure body(fsys: setofsys; fprocp: ctp); forward;

  procedure declare(fsys: setofsys);
    var lsy: symbol;

    { resolve all pointer references in the forward list }
    procedure resolvep;
    var ids: idstr; lcp1, lcp2: ctp; mm, fe: boolean;
    begin
      ids := id;
      fe := true;
      while fwptr <> nil do begin
        lcp1 := fwptr;
        fwptr := lcp1^.next;
        strassfv(id, lcp1^.name);
        searchidnenm([types], lcp2, mm);
        if lcp2 <> nil then begin
          lcp1^.idtype^.eltype := lcp2^.idtype;
          lcp2^.refer := true
        end else begin
          if fe then begin error(117); writeln(output) end;
          write('*** undefined type-id forward reference: ');
          writev(output, lcp1^.name, prtlln); writeln;
          fe := false
        end;
        putnam(lcp1)
      end;
      id := ids
    end;

    procedure typ(fsys: setofsys; var fsp: stp; var fsize: addrrange);
      var lsp,lsp1,lsp2: stp; oldtop: disprange; lcp: ctp;
          lsize,displ: addrrange; lmin,lmax, t: integer;
          test: boolean; ispacked: boolean; lvalu: valu;

      procedure simpletype(fsys:setofsys; var fsp:stp; var fsize:addrrange);
        var lsp,lsp1: stp; lcp,lcp1: ctp; ttop: disprange;
            lcnt: integer; lvalu: valu;
      begin fsize := 1;
        if not (sy in simptypebegsys) then
          begin error(1); skip(fsys + simptypebegsys) end;
        if sy in simptypebegsys then
          begin
            if sy = lparent then
              begin ttop := top;   (*decl. consts local to innermost block*)
                while display[top].occur <> blck do top := top - 1;
                new(lsp,scalar,declared); pshstc(lsp);
                with lsp^ do
                  begin size := intsize; form := scalar;
                    scalkind := declared; packing := false
                  end;
                lcp1 := nil; lcnt := 0;
                repeat insymbol;
                  if sy = ident then
                    begin new(lcp,konst); ininam(lcp);
                      with lcp^ do
                        begin strassvf(name, id); idtype := lsp; next := lcp1;
                          values.intval := true; values.ival := lcnt; 
                          klass := konst
                        end;
                      enterid(lcp);
                      lcnt := lcnt + 1;
                      lcp1 := lcp; insymbol
                    end
                  else error(2);
                  if not (sy in fsys + [comma,rparent]) then
                    begin error(6); skip(fsys + [comma,rparent]) end
                until sy <> comma;
                lsp^.fconst := lcp1; top := ttop;
                if sy = rparent then insymbol else error(4);
                { resize for byte if needed }
                if isbyte(lsp) then lsp^.size := 1;
                fsize := lsp^.size
              end
            else
              begin
                if sy = ident then
                  begin searchid([types,konst],lcp);
                    insymbol;
                    if lcp^.klass = konst then
                      begin new(lsp,subrange); pshstc(lsp);
                        with lsp^, lcp^ do
                          begin form := subrange; rangetype := idtype; 
                            if stringt(rangetype) then
                              begin error(148); rangetype := nil end;
                            if rangetype = realptr then 
                              begin error(109); rangetype := nil end;
                            if not values.intval then 
                              begin min.intval := true; min.ival := 1 end
                            else min := values;
                            size := intsize; packing := false
                          end;
                        if sy = range then insymbol else error(5);
                        constexpr(fsys,lsp1,lvalu);
                        if not lvalu.intval then 
                          begin lsp^.max.intval := true; lsp^.max.ival := 1 end 
                        else lsp^.max := lvalu;
                        if lsp^.rangetype <> lsp1 then error(107);
                        if isbyte(lsp) then lsp^.size := 1
                      end
                    else
                      begin lsp := lcp^.idtype;
                        if lsp <> nil then fsize := lsp^.size
                      end
                  end (*sy = ident*)
                else
                  begin new(lsp,subrange); pshstc(lsp);
                    lsp^.form := subrange; lsp^.packing := false;
                    constexpr(fsys + [range],lsp1,lvalu);
                    if stringt(lsp1) then
                      begin error(148); lsp1 := nil end;
                    if lsp1 = realptr then begin error(109); lsp1 := nil end;
                    with lsp^ do begin
                      rangetype:=lsp1; 
                      if lvalu.intval then min:=lvalu else 
                        begin min.intval := true; min.ival := 1 end;
                      size:=intsize 
                    end;
                    if sy = range then insymbol else error(5);
                    constexpr(fsys,lsp1,lvalu);
                    if lvalu.intval then lsp^.max := lvalu
                    else begin lsp^.max.intval := true; lsp^.max.ival := 1 end;
                    if lsp^.rangetype <> lsp1 then error(107);
                    if isbyte(lsp) then lsp^.size := 1;
                    fsize := lsp^.size
                  end;
                if lsp <> nil then
                  with lsp^ do
                    if form = subrange then
                      if rangetype <> nil then
                        if rangetype = realptr then error(399)
                        else
                          if min.ival > max.ival then 
                            begin error(102); 
                              { swap to fix and suppress further errors }
                              t := min.ival; min.ival := max.ival; max.ival := t 
                            end
              end;
            fsp := lsp;
            if not (sy in fsys) then
              begin error(6); skip(fsys) end
          end
            else fsp := nil
      end (*simpletype*) ;

      procedure fieldlist(fsys: setofsys; var frecvar: stp; vartyp: stp;
                          varlab: ctp; lvl: integer; var fstlab: ctp);
        var lcp,lcp1,lcp2,nxt,nxt1: ctp; lsp,lsp1,lsp2,lsp3,lsp4: stp;
            minsize,maxsize,lsize: addrrange; lvalu,rvalu: valu;
            test: boolean; mm: boolean;
      begin nxt1 := nil; lsp := nil; fstlab := nil;
        if not (sy in (fsys+[ident,casesy])) then
          begin error(19); skip(fsys + [ident,casesy]) end;
        while sy = ident do
          begin nxt := nxt1;
            repeat
              if sy = ident then
                begin new(lcp,field); ininam(lcp); 
                  if fstlab = nil then fstlab := lcp;
                  with lcp^ do
                    begin strassvf(name, id); idtype := nil; next := nxt;
                      klass := field; varnt := vartyp; varlb := varlab;
                      tagfield := false; taglvl := lvl; varsaddr := 0;
                      varssize := 0
                    end;
                  nxt := lcp;
                  enterid(lcp);
                  insymbol
                end
              else error(2);
              if not (sy in [comma,colon]) then
                begin error(6); skip(fsys + [comma,colon,semicolon,casesy])
                end;
              test := sy <> comma;
              if not test  then insymbol
            until test;
            if sy = colon then insymbol else error(5);
            typ(fsys + [casesy,semicolon],lsp,lsize);
            while nxt <> nxt1 do
              with nxt^ do
                begin alignu(lsp,displ);
                  idtype := lsp; fldaddr := displ;
                  nxt := next; displ := displ + lsize
                end;
            nxt1 := lcp;
            while sy = semicolon do
              begin insymbol;
                if not (sy in fsys + [ident,casesy,semicolon]) then
                  begin error(19); skip(fsys + [ident,casesy]) end
              end
          end (*while*);
        nxt := nil;
        while nxt1 <> nil do
          with nxt1^ do
            begin lcp := next; next := nxt; nxt := nxt1; nxt1 := lcp end;
        if sy = casesy then
          begin new(lsp,tagfld); pshstc(lsp);
            with lsp^ do
              begin tagfieldp := nil; fstvar := nil; form:=tagfld; 
                    packing := false end;
            frecvar := lsp;
            insymbol;
            if sy = ident then
              begin
                { find possible type first }
                searchidnenm([types],lcp1,mm);
                { now set up as field id }
                new(lcp,field); ininam(lcp);
                with lcp^ do
                  begin strassvf(name, id); idtype := nil; klass:=field;
                    next := nil; fldaddr := displ; varnt := vartyp;
                    varlb := varlab; tagfield := true; taglvl := lvl;
                    varsaddr := 0; varssize := 0
                  end;
                insymbol;
                if sy = colon then begin
                  enterid(lcp); insymbol;
                  if sy = ident then begin searchid([types],lcp1); insymbol end
                  else begin error(2); skip(fsys + [ofsy,lparent]); lcp1 := nil end
                end else begin
                   if lcp1 = nil then begin error(104); lcp1 := usclrptr end;
                   { If type only (undiscriminated variant), kill the id. }
                   if mm then error(103);
                   putstrs(lcp^.name); { release name string }
                   lcp^.name := nil { set no tagfield }
                end;
                if lcp1 <> nil then begin
                  lsp1 := lcp1^.idtype;
                  if lsp1 <> nil then
                    begin alignu(lsp1,displ);
                      lcp^.fldaddr := displ;
                      { only allocate field if named or if undiscriminated
                        tagfield checks are on }
                      if (lcp^.name <> nil) or chkudtf then
                        displ := displ+lsp1^.size;
                      if (lsp1^.form <= subrange) or stringt(lsp1) then
                        begin if comptypes(realptr,lsp1) then error(109)
                          else if stringt(lsp1) then error(399);
                          lcp^.idtype := lsp1; lsp^.tagfieldp := lcp;
                        end
                      else error(110);
                    end
                  end
              end
            else begin error(2); skip(fsys + [ofsy,lparent]) end;
            lsp^.size := displ;
            if sy = ofsy then insymbol else error(8);
            lsp1 := nil; minsize := displ; maxsize := displ;
            repeat lsp2 := nil;
              if not (sy in fsys + [semicolon]) then
              begin
                repeat constexpr(fsys + [comma,colon,lparent,range],lsp3,lvalu); 
                  rvalu := lvalu; lsp4 := lsp3; if sy = range then begin chkstd;
                    insymbol; constexpr(fsys + [comma,colon,lparent],lsp4,rvalu)
                  end;
                  if lsp^.tagfieldp <> nil then begin
                    if not comptypes(lsp^.tagfieldp^.idtype,lsp3)then error(111);
                    if not comptypes(lsp^.tagfieldp^.idtype,lsp4)then error(111);
                  end;
                  { fix up for error processing }
                  if not lvalu.intval then 
                    begin lvalu.intval := true; lvalu.ival := 1 end;
                  if not rvalu.intval then 
                    begin rvalu.intval := true; rvalu.ival := 1 end;
                  if lvalu.ival > rvalu.ival then error(225);
                  repeat { case range }
                    new(lsp3,variant); pshstc(lsp3);
                    with lsp3^ do
                      begin nxtvar := lsp1; subvar := lsp2; varval := lvalu;
                            caslst := lsp2; form := variant; packing := false
                      end;
                    lsp4 := lsp1;
                    while lsp4 <> nil do
                      with lsp4^ do
                        begin
                          if varval.ival = lvalu.ival then error(178);
                          lsp4 := nxtvar
                        end;
                    lsp1 := lsp3; lsp2 := lsp3;
                    lvalu.ival := lvalu.ival+1 { next range value }
                  until lvalu.ival > rvalu.ival; { range is complete }
                  test := sy <> comma;
                  if not test then insymbol
                until test;
                if sy = colon then insymbol else error(5);
                if sy = lparent then insymbol else error(9);
                alignu(nilptr, displ); { max align all variants }
                if lcp <> nil then lcp^.varsaddr := displ;
                fieldlist(fsys + [rparent,semicolon],lsp2,lsp3,lcp, lvl+1,lcp2);
                if displ > maxsize then maxsize := displ;
                if lcp <> nil then lcp^.varssize := maxsize-lcp^.varsaddr;
                while lsp3 <> nil do
                  begin lsp4 := lsp3^.subvar; lsp3^.subvar := lsp2;
                    lsp3^.varfld := lcp2;
                    lsp3^.size := displ;
                    lsp3 := lsp4
                  end;
                if sy = rparent then
                  begin insymbol;
                    if not (sy in fsys + [semicolon]) then
                      begin error(6); skip(fsys + [semicolon]) end
                  end
                else error(4);
              end;
              test := sy <> semicolon;
              if not test then
                begin displ := minsize;
                      insymbol
                end
            until test;
            displ := maxsize;
            lsp^.fstvar := lsp1;
          end
        else frecvar := nil
      end (*fieldlist*) ;

    begin (*typ*)
      lsp := nil;
      if not (sy in typebegsys) then
         begin error(10); skip(fsys + typebegsys) end;
      if sy in typebegsys then
        begin
          if sy in simptypebegsys then simpletype(fsys,fsp,fsize)
          else
    (*^*)     if sy = arrow then
              begin new(lsp,pointer); pshstc(lsp); fsp := lsp;
                with lsp^ do
                  begin eltype := nil; size := ptrsize; form:=pointer; 
                        packing := false end;
                insymbol;
                if sy = ident then
                  begin { forward reference everything }
                    new(lcp,types); ininam(lcp);
                    with lcp^ do
                      begin strassvf(name,id); idtype := lsp;
                        next := fwptr; klass := types
                      end;
                    fwptr := lcp;
                    insymbol;
                  end
                else error(2);
              end
            else
              begin
                ispacked := false; { set not packed by default }
                if sy = packedsy then
                  begin insymbol; ispacked := true; { packed }
                    if not (sy in typedels) then
                      begin
                        error(10); skip(fsys + typedels)
                      end
                  end;
    (*array*)     if sy = arraysy then
                  begin insymbol;
                    if (sy <> lbrack) and iso7185 then error(11);
                    if (sy <> lbrack) and not iso7185 then begin
                      { process Pascaline array }
                      lsp1 := nil;
                      repeat new(lsp,arrays); pshstc(lsp);
                        with lsp^ do
                          begin aeltype := lsp1; inxtype := nil; form:=arrays;
                                packing := ispacked end;
                        lsp1 := lsp;
                        constexpr(fsys+[comma,ofsy],lsp2,lvalu);
                        if lsp2 <> nil then if lsp2 <> intptr then error(15);
                        if not lvalu.intval then 
                          begin lvalu.intval := true; lvalu.ival := 1 end;
                        lsp1^.size := lsize;
                        { build subrange type based on 1..n }
                        new(lsp2,subrange); pshstc(lsp2);
                          with lsp2^ do
                            begin rangetype := intptr; min.ival := 1; 
                                  max := lvalu end;
                        lsp^.inxtype := lsp2;
                        test := sy <> comma;
                        if not test then insymbol
                      until test                      
                    end else begin if sy = lbrack then insymbol;
                      { process ISO 7185 array }
                      lsp1 := nil;
                      repeat new(lsp,arrays); pshstc(lsp);
                        with lsp^ do
                          begin aeltype := lsp1; inxtype := nil; form:=arrays;
                                packing := ispacked end;
                        lsp1 := lsp;
                        simpletype(fsys + [comma,rbrack,ofsy],lsp2,lsize);
                        lsp1^.size := lsize;
                        if lsp2 <> nil then
                          if lsp2^.form <= subrange then
                            begin
                              if lsp2 = realptr then
                                begin error(109); lsp2 := nil end
                              else
                                if lsp2 = intptr then
                                  begin error(149); lsp2 := nil end;
                              lsp^.inxtype := lsp2
                            end
                          else begin error(113); lsp2 := nil end;
                        test := sy <> comma;
                        if not test then insymbol
                      until test;
                      if sy = rbrack then insymbol else error(12)
                    end;
                    if sy = ofsy then insymbol else error(8);
                    typ(fsys,lsp,lsize);
                    repeat
                      with lsp1^ do
                        begin lsp2 := aeltype; aeltype := lsp;
                          if inxtype <> nil then
                            begin getbounds(inxtype,lmin,lmax);
                              alignu(lsp,lsize);
                              lsize := lsize*(lmax - lmin + 1);
                              size := lsize
                            end
                        end;
                      lsp := lsp1; lsp1 := lsp2
                    until lsp1 = nil
                  end
                else
    (*record*)      if sy = recordsy then
                    begin insymbol;
                      oldtop := top;
                      if top < displimit then
                        begin top := top + 1;
                          with display[top] do
                            begin fname := nil;
                                  flabel := nil;
                                  fconst := nil;
                                  fstruct := nil;
                                  packing := false;
                                  packcom := false;
                                  ptrref := false;
                                  modnam := nil;
                                  occur := rec
                            end
                        end
                      else error(250);
                      displ := 0;
                      fieldlist(fsys-[semicolon]+[endsy],lsp1,nil,nil,1,lcp);
                      new(lsp,records);
                      with lsp^ do
                        begin fstfld := display[top].fname;
                          display[top].fname := nil;
                          recvar := lsp1; size := displ; form := records;
                          packing := ispacked;
                          recyc := display[top].fstruct;
                          display[top].fstruct := nil
                        end;
                      putdsps(oldtop); top := oldtop;
                      { register the record late because of the purge above }
                      pshstc(lsp);
                      if sy = endsy then insymbol else error(13)
                    end
                  else
    (*set*)        if sy = setsy then
                      begin insymbol;
                        if sy = ofsy then insymbol else error(8);
                        simpletype(fsys,lsp1,lsize);
                        if lsp1 <> nil then
                          if lsp1^.form > subrange then
                            begin error(115); lsp1 := nil end
                          else
                            if lsp1 = realptr then
                              begin error(114); lsp1 := nil end
                            else if lsp1 = intptr then
                              begin error(169); lsp1 := nil end
                            else
                              begin getbounds(lsp1,lmin,lmax);
                                if (lmin < setlow) or (lmax > sethigh)
                                  then error(169);
                              end;
                        new(lsp,power); pshstc(lsp);
                        with lsp^ do
                          begin elset:=lsp1; size:=setsize; form:=power;
                                packing := ispacked; matchpack := true end;
                      end
                    else
    (*file*)        if sy = filesy then
                          begin insymbol;
                            if sy = ofsy then insymbol else error(8);
                            typ(fsys,lsp1,lsize);
                            if filecomponent(lsp1) then error(190);
                            new(lsp,files); pshstc(lsp);
                            with lsp^ do
                              begin filtype := lsp1; size := filesize+lsize;
                                form := files; packing := ispacked
                              end
                          end
                    else fsp := nil;
                fsp := lsp
              end;
          if not (sy in fsys) then
            begin error(6); skip(fsys) end
        end
      else fsp := nil;
      if fsp = nil then fsize := 1 else fsize := fsp^.size
    end (*typ*) ;

    procedure labeldeclaration;
      var llp: lbp;
          test: boolean;
    begin
      repeat
        if (sy = intconst) or (sy = ident) then begin
          if sy = ident then chkstd;
          searchlabel(llp, top, sy = ident); { search preexisting label }
          if llp <> nil then error(166) { multideclared label }
          else newlabel(llp, sy = ident);
          insymbol
        end else if iso7185 then error(15) else error(22);
        if not ( sy in fsys + [comma, semicolon] ) then
          begin error(6); skip(fsys+[comma,semicolon]) end;
        test := sy <> comma;
        if not test then insymbol
      until test;
      if sy = semicolon then insymbol else error(14)
    end (* labeldeclaration *) ;

    procedure constdeclaration;
      var lcp: ctp; lsp: stp; lvalu: valu;
    begin
      if sy <> ident then
        begin error(2); skip(fsys + [ident]) end;
      while sy = ident do
        begin new(lcp,konst); ininam(lcp);
          with lcp^ do
            begin strassvf(name, id); idtype := nil; next := nil; klass:=konst;
              refer := false
            end;
          insymbol;
          if (sy = relop) and (op = eqop) then insymbol else error(16);
          constexpr(fsys + [semicolon],lsp,lvalu);
          enterid(lcp);
          lcp^.idtype := lsp; lcp^.values := lvalu;
          if sy = semicolon then
            begin insymbol;
              if not (sy in fsys + [ident]) then
                begin error(6); skip(fsys + [ident]) end
            end
          else error(14)
        end
    end (*constdeclaration*) ;

    procedure typedeclaration;
      var lcp: ctp; lsp: stp; lsize: addrrange;
    begin
      if sy <> ident then
        begin error(2); skip(fsys + [ident]) end;
      while sy = ident do
        begin new(lcp,types); ininam(lcp);
          with lcp^ do
            begin strassvf(name, id); idtype := nil; klass := types;
              refer := false
            end;
          insymbol;
          if (sy = relop) and (op = eqop) then insymbol else error(16);
          typ(fsys + [semicolon],lsp,lsize);
          enterid(lcp);
          lcp^.idtype := lsp;
          if sy = semicolon then
            begin insymbol;
              if not (sy in fsys + [ident]) then
                begin error(6); skip(fsys + [ident]) end
            end
          else error(14)
        end;
      resolvep
    end (*typedeclaration*) ;

    { write shorthand type }
    procedure wrttyp(var f: text; tp: stp);
    const maxtrk = 4000;
    var typtrk: array [1..maxtrk] of stp; cti: integer; err: boolean;
    
    procedure wrttypsub(tp: stp);
    var x, y, fi: integer;
    
    procedure nxtcti;
    begin
      cti := cti+1; if cti <= maxtrk then typtrk[cti] := nil
    end;
    
    procedure nxtctis(i: integer);
    var x: integer;
    begin
      for x := 1 to i do nxtcti
    end;
    
    procedure wrtchr(c: char);
    begin
      write(f, c); nxtcti
    end;

    procedure wrtint(i: integer);
    var p, d: integer;
    begin
       p := 10; d := 1;
       while (i >= p) and (p < maxpow10) do begin p := p*10; d := d+1 end;
       write(f, i:1);
       nxtctis(d)
    end;
    
    procedure wrtrfd(fld: ctp);
    begin
      while fld <> nil do begin
        with fld^ do begin
          writev(f, name, lenpv(name)); nxtctis(lenpv(name));
          wrtchr(':'); 
          if klass = field then wrtint(fldaddr) else wrtchr('?'); 
          wrtchr(':'); wrttypsub(idtype); 
        end;
        fld := fld^.next;
        if fld <> nil then wrtchr(',')  
      end
    end;
    
    procedure wrtvar(sp: stp);
    begin
      while sp <> nil do with sp^ do
        if form = variant then begin
          wrtint(varval.ival); wrtchr('('); wrtrfd(varfld); wrtchr(')');
          sp := nxtvar
        end else sp := nil
    end;

    { enums are backwards, so print thus }
    procedure wrtenm(ep: ctp; i: integer);
    begin
      if ep <> nil then begin
        wrtenm(ep^.next, i+1); 
        writev(f, ep^.name, lenpv(ep^.name)); nxtctis(lenpv(ep^.name));
        if i > 0 then wrtchr(',')
      end
    end;
    
    begin { wrttypsub }
      if cti > maxtrk then begin
        if not err then error(227);
        err := true
      end else typtrk[cti] := tp; { track this type entry }
      if tp <> nil then with tp^ do case form of
        scalar: begin 
                  if tp = intptr then wrtchr('i')
                  else if tp = boolptr then wrtchr('b')
                  else if tp = charptr then wrtchr('c')
                  else if tp = realptr then wrtchr('n')
                  else if scalkind = declared then
                    begin wrtchr('x'); wrtchr('('); wrtenm(fconst, 0); 
                          wrtchr(')') end
                  else wrtchr('?')
                end;
        subrange: begin
                    wrtchr('x'); wrtchr('('); wrtint(min.ival); wrtchr(',');
                    wrtint(max.ival); wrtchr(')'); 
                    wrttypsub(rangetype)
                  end;
        pointer: begin 
                   wrtchr('p'); fi := 0; y := cti; 
                   if y > maxtrk then y := maxtrk;
                   if eltype <> nil then
                     for x := y downto 1 do if typtrk[x] = eltype then fi := x;
                   { if there is a cycle, output type digest position, otherwise
                     the actual type }
                   if fi > 0 then wrtint(fi) else wrttypsub(eltype) 
                 end;
        power: begin wrtchr('s'); wrttypsub(elset) end;
        arrays: begin wrtchr('a'); wrttypsub(inxtype); wrttypsub(aeltype) end;
        records: begin wrtchr('r'); wrtchr('('); wrtrfd(fstfld); 
                   if recvar <> nil then if recvar^.form = tagfld then
                     begin wrtchr(','); wrtrfd(recvar^.tagfieldp);
                           wrtchr('('); wrtvar(recvar^.fstvar); 
                           wrtchr(')') end;
                   wrtchr(')') end; 
        files: begin wrtchr('f'); wrttypsub(filtype) end;
        variant: wrtchr('?');
        exceptf: wrtchr('e')
      end else wrtchr('?')
    end;
    
    begin { wrttyp }
      cti := 1; { set 1st position in tracking }
      err := false; { set no error }
      wrttypsub(tp) { issue type }
    end;

    procedure wrtsym(lcp: ctp; typ: char);
    begin
      if prcode then begin
        with lcp^ do begin
          write(prr, 's '); 
          writev(prr, name, lenpv(name)); write(prr, ' ', typ);
          write(prr, ' ', vaddr:1, ' '); wrttyp(prr, idtype); writeln(prr)
        end
      end
    end;
    
    procedure vardeclaration;
      var lcp,nxt: ctp; lsp: stp; lsize: addrrange;
          test: boolean;
    begin nxt := nil;
      repeat
        repeat
          if sy = ident then
            begin new(lcp,vars); ininam(lcp);
              with lcp^ do
               begin strassvf(name, id); next := nxt; klass := vars;
                  idtype := nil; vkind := actual; vlev := level;
                  refer := false; threat := false; forcnt := 0; part := ptval;
                  hdr := false; vext := incstk <> nil; vmod := incstk
                end;
              enterid(lcp);
              nxt := lcp;
              insymbol;
            end
          else error(2);
          if not (sy in fsys + [comma,colon] + typedels) then
            begin error(6); skip(fsys+[comma,colon,semicolon]+typedels) end;
          test := sy <> comma;
          if not test then insymbol
        until test;
        if sy = colon then insymbol else error(5);
        typ(fsys + [semicolon] + typedels,lsp,lsize);
        resolvep; { resolve pointer defs before symbol generate }
        while nxt <> nil do
          with nxt^ do
            begin
              idtype := lsp; 
              { globals are alloc/increment, locals are decrement/alloc }
              if level <= 1 then 
                begin alignu(lsp,gc); vaddr := gc; gc := gc + lsize end
              else 
                begin lc := lc - lsize; alignd(lsp,lc); vaddr := lc end;
              { mark symbol }
              if prcode then
                if level <= 1 then wrtsym(nxt, 'g') else wrtsym(nxt, 'l');
              nxt := next
            end;
        if sy = semicolon then
          begin insymbol;
            if not (sy in fsys + [ident]) then
              begin error(6); skip(fsys + [ident]) end
          end
        else error(14)
      until (sy <> ident) and not (sy in typedels);
      resolvep
    end (*vardeclaration*) ;

    procedure procdeclaration(fsy: symbol);
      var oldlev: 0..maxlevel; lcp,lcp1,lcp2: ctp; lsp: stp;
          forw,virt,ovrl: boolean; oldtop: disprange;
          llc: stkoff; lbname: integer; plst: boolean; fpat: fpattr;

      procedure pushlvl(forw: boolean; lcp: ctp);
      begin
        if level < maxlevel then level := level + 1 else error(251);
        if top < displimit then
          begin top := top + 1;
            with display[top] do
              begin
                if forw then fname := lcp^.pflist
                else fname := nil;
                flabel := nil; fconst := nil; fstruct := nil; packing := false;
                packcom := false; ptrref := false; modnam := nil;
                occur := blck; bname := lcp
              end
          end
        else error(250);
      end;

      procedure parameterlist(fsy: setofsys; var fpar: ctp; var plst: boolean);
        var lcp,lcp1,lcp2,lcp3: ctp; lsp: stp; lkind: idkind;
          llc,lsize: addrrange; count: integer; pt: partyp;
          oldlev: 0..maxlevel; oldtop: disprange;
          oldlevf: 0..maxlevel; oldtopf: disprange;
          lcs: addrrange;
          test: boolean;
          dummy: boolean;
      begin
        plst := false;
        if forw then begin { isolate duplicated list in level }
          oldlevf := level; oldtopf := top; pushlvl(false, nil)  
        end; 
        lcp1 := nil;
        if not (sy in fsy + [lparent]) then
          begin error(7); skip(fsys + fsy + [lparent]) end;
        if sy = lparent then
          begin if forw and iso7185 then error(119); plst := true;
            insymbol;
            if not (sy in [ident,varsy,procsy,funcsy,viewsy,outsy]) then
              begin error(7); skip(fsys + [ident,rparent]) end;
            while sy in [ident,varsy,procsy,funcsy,viewsy,outsy] do
              begin
                if sy = procsy then
                  begin
                    insymbol; lcp := nil;
                    if sy = ident then
                      begin new(lcp,proc,declared,formal); ininam(lcp);
                        lc := lc-ptrsize*2; { mp and addr }
                        alignd(parmptr,lc);
                        with lcp^ do
                          begin strassvf(name, id); idtype := nil; next := lcp1;
                            pflev := level (*beware of parameter procedures*);
                            klass:=proc; pfdeckind:=declared; pflist := nil;
                            pfkind:=formal; pfaddr := lc; pext := false; 
                            pmod := nil; keep := true; pfattr := fpanone;
                            grpnxt := nil; grppar := nil; pfvid := nil
                          end;
                        enterid(lcp);
                        lcp1 := lcp;
                        insymbol
                      end
                    else error(2);
                    oldlev := level; oldtop := top; pushlvl(false, lcp);
                    lcs := lc; parameterlist([semicolon,rparent],lcp2,dummy); 
                    lc := lcs; if lcp <> nil then lcp^.pflist := lcp2;
                    if not (sy in fsys+[semicolon,rparent]) then
                      begin error(7);skip(fsys+[semicolon,rparent]) end;
                    level := oldlev; putdsps(oldtop); top := oldtop
                  end
                else
                  begin
                    if sy = funcsy then
                      begin lcp2 := nil;
                        insymbol;
                        if sy = ident then
                          begin new(lcp,func,declared,formal); ininam(lcp);
                            lc := lc-ptrsize*2; { mp and addr }
                            alignd(parmptr,lc);
                            with lcp^ do
                              begin strassvf(name, id); idtype := nil; next := lcp1;
                                pflev := level (*beware param funcs*);
                                klass:=func;pfdeckind:=declared; pflist := nil;
                                pfkind:=formal; pfaddr:=lc; pext := false; 
                                pmod := nil; keep := true; pfattr := fpanone;
                                grpnxt := nil; grppar := nil; pfvid := nil
                              end;
                            enterid(lcp);
                            lcp1 := lcp;
                            insymbol;
                          end
                        else error(2);
                        oldlev := level; oldtop := top; pushlvl(false, lcp);
                        lcs := lc; 
                        parameterlist([colon,semicolon,rparent],lcp2,dummy); 
                        lc := lcs; if lcp <> nil then lcp^.pflist := lcp2;
                        if not (sy in fsys+[colon]) then
                          begin error(7);skip(fsys+[comma,semicolon,rparent]) end;
                        if sy = colon then
                          begin insymbol;
                            if sy = ident then
                              begin searchid([types],lcp2);
                                lsp := lcp2^.idtype;
                                lcp^.idtype := lsp;
                                if lsp <> nil then
                                 if not(lsp^.form in[scalar,subrange,pointer])
                                    then begin error(120); lsp := nil end;
                                insymbol
                              end
                            else error(2);
                            if not (sy in fsys + [semicolon,rparent]) then
                              begin error(7);skip(fsys+[semicolon,rparent])end
                          end
                        else error(5);
                        level := oldlev; putdsps(oldtop); top := oldtop
                      end
                    else
                      begin
                        pt := ptval; 
                        if sy = varsy then pt := ptvar
                        else if sy = viewsy then pt := ptview
                        else if sy = outsy then pt := ptout;
                        if (sy = varsy) or (sy = outsy) then
                          begin lkind := formal; insymbol end
                        else begin lkind := actual; 
                          if sy = viewsy then insymbol 
                        end;
                        lcp2 := nil;
                        count := 0;
                        repeat
                          if sy = ident then
                            begin new(lcp,vars); ininam(lcp);
                              with lcp^ do
                                begin strassvf(name,id); idtype:=nil; klass:=vars;
                                  vkind := lkind; next := lcp2; vlev := level;
                                  keep := true; refer := false; threat := false;
                                  forcnt := 0; part := pt; hdr := false; 
                                  vext := false; vmod := nil
                                end;
                              enterid(lcp);
                              lcp2 := lcp; count := count+1;
                              insymbol;
                            end
                          else error(2);
                          if not (sy in [comma,colon] + fsys) then
                            begin error(7);skip(fsys+[comma,semicolon,rparent])
                            end;
                          test := sy <> comma;
                          if not test then insymbol
                        until test;
                        if sy = colon then
                          begin insymbol;
                            if sy = ident then
                              begin searchid([types],lcp);
                                lsp := lcp^.idtype;
                                lsize := ptrsize;
                                if lsp <> nil then
                                  if lkind=actual then begin
                                    if lsp^.form<=power then lsize := lsp^.size
                                    else if lsp^.form=files then error(121);
                                    { type containing file not allowed either }
                                    if filecomponent(lsp) then error(121)
                                  end;
                                alignu(parmptr,lsize);
                                lcp3 := lcp2;
                                lc := lc-count*lsize;
                                alignd(parmptr,lc);
                                llc := lc;
                                while lcp2 <> nil do
                                  begin lcp := lcp2;
                                    with lcp2^ do
                                      begin idtype := lsp;
                                        vaddr := llc;
                                        llc := llc+lsize;
                                        { if the type is structured, and is
                                          a view parameter, promote to formal }
                                        if lsp <> nil then
                                          if (lsp^.form <= power) and 
                                             (part = ptview) then 
                                            vkind := formal
                                      end;
                                    lcp2 := lcp2^.next
                                  end;
                                lcp^.next := lcp1; lcp1 := lcp3;
                                insymbol
                              end
                            else begin error(2);
                              { set any id list to tear down }
                              while lcp2 <> nil do begin lcp2^.keep := false; lcp2 := lcp2^.next end
                            end;
                            if not (sy in fsys + [semicolon,rparent]) then
                              begin error(7);skip(fsys+[semicolon,rparent])end
                          end
                        else begin error(5);
                          { set any id list to tear down }
                          while lcp2 <> nil do begin lcp2^.keep := false; lcp2 := lcp2^.next end
                        end
                      end;
                  end;
                if sy = semicolon then
                  begin insymbol;
                    if not (sy in fsys + [ident,varsy,procsy,funcsy]) then
                      begin error(7); skip(fsys + [ident,rparent]) end
                  end
              end (*while*) ;
            if sy = rparent then
              begin insymbol;
                if not (sy in fsy + fsys) then
                  begin error(6); skip(fsy + fsys) end
              end
            else error(4);
            lcp3 := nil;
            (*reverse pointers and reserve local cells for copies of multiple
             values*)
            while lcp1 <> nil do
              with lcp1^ do
                begin lcp2 := next; next := lcp3;
                  if klass = vars then
                    if idtype <> nil then
                      { if value variable, and structured, we make a copy to
                        play with. However, structured is treated as var if
                        it is view, since that is protected }
                      if (vkind=actual) and (idtype^.form>power) then
                        begin
                          lc := lc-idtype^.size;
                          alignd(idtype,lc);
                          vaddr := lc
                        end;
                  lcp3 := lcp1; lcp1 := lcp2
                end;
            fpar := lcp3
          end
            else fpar := nil;
      if forw then begin { isolate duplicated list in level }
        level := oldlevf; putdsps(oldtopf); top := oldtopf
      end
    end (*parameterlist*) ;

    procedure compparam(pla, plb: ctp);
    begin
      while (pla <> nil) and (plb <> nil) do begin
        if not comptypes(pla^.idtype,plb^.idtype) then error(216);
        pla := pla^.next; plb := plb^.next
      end;
      if (pla <> nil) or (plb <> nil) then error(216)
    end;

    begin (*procdeclaration*)
      { parse and skip any attribute }
      fpat := fpanone;
      if fsy in [overloadsy,staticsy,virtualsy,overridesy,operatorsy] then begin
        chkstd;
        case fsy of { attribute }
          overloadsy: fpat := fpaoverload;
          staticsy: fpat := fpastatic;
          virtualsy: begin fpat := fpavirtual; if top > 1 then error(228) end;
          overridesy: begin fpat := fpaoverride; if top > 1 then error(229) end
        end;
        if (sy <> procsy) and (sy <> funcsy) then error(209)
        else fsy := sy; insymbol
      end;
      llc := lc; lc := lcaftermarkstack; forw := false; virt := false; 
      ovrl := false;
      if sy = ident then
        begin searchsection(display[top].fname,lcp); (*decide whether forw.*)
          if lcp <> nil then
            begin
              if lcp^.klass = proc then begin
                forw := lcp^.forwdecl and (fsy=procsy) and (lcp^.pfkind=actual);
                virt := (lcp^.pfattr = fpavirtual) and (fpat = fpaoverride) and
                        (fsy=procsy)and(lcp^.pfkind=actual);
                ovrl := (fsy=procsy) and (lcp^.pfkind=actual) and 
                        (fpat = fpaoverload)
              end else if lcp^.klass = func then begin
                  forw:=lcp^.forwdecl and (fsy=funcsy) and (lcp^.pfkind=actual);
                  virt := (lcp^.pfattr = fpavirtual) and (fpat = fpaoverride) and
                          (fsy=funcsy)and(lcp^.pfkind=actual);
                  ovrl := (fsy=procsy) and (lcp^.pfkind=actual) and 
                          (fpat = fpaoverload)
              end else forw := false;
              if not forw and not virt then error(160);
              if virt and not chkext(lcp) then error (230);
              if ovrl and (lcp^.pfattr = fpavirtual) then error(232);
            end
          else if fpat = fpaoverride then error(231);
          lcp1 := lcp; { save original }
          if not forw then
            begin
              if fsy = procsy then new(lcp,proc,declared,actual)
              else new(lcp,func,declared,actual); ininam(lcp);
              with lcp^ do
                begin strassvf(name, id); idtype := nil; next := nil;
                  externl := false; pflev := level; genlabel(lbname);
                  pfdeckind := declared; pfkind := actual; pfname := lbname;
                  if fsy = procsy then klass := proc
                  else klass := func; 
                  pflist := nil; asgn := false; 
                  pext := incstk <> nil; pmod := incstk; refer := false; 
                  pfattr := fpat; grpnxt := nil; grppar := nil;
                  if pfattr in [fpavirtual, fpaoverride] then begin { alloc vector }
                    if pfattr = fpavirtual then begin
                      { have to create a label for far references to virtual }
                      new(lcp2,vars); ininam(lcp2);
                      with lcp2^ do begin
                        strassvf(name, id); strcatvr(name, '__virtvec');
                        idtype := nilptr; vkind := actual;
                        next := nil; vlev := 0; vaddr := gc; klass := vars;
                        threat := false; forcnt := 0; part := ptval; hdr := false; 
                        vext := incstk <> nil; vmod := incstk
                      end;
                      enterid(lcp2); lcp^.pfvid := lcp2;
                      wrtsym(lcp2, 'g')
                    end;
                    pfvaddr := gc; gc := gc+adrsize 
                  end
                end;
              if virt or ovrl then 
                { just insert to group list for this proc/func } 
                begin lcp^.grpnxt := lcp1^.grpnxt; lcp1^.grpnxt := lcp; 
                      lcp^.grppar := lcp1 end
              else enterid(lcp)
            end
          else
            begin lcp1 := lcp^.pflist;
              while lcp1 <> nil do
                begin
                  with lcp1^ do
                    if klass = vars then
                      if idtype <> nil then
                          if vaddr < lc then lc := vaddr;
                  lcp1 := lcp1^.next
                end
            end;
          insymbol
        end
      else
        begin error(2);
          if fsy = procsy then lcp := uprcptr else lcp := ufctptr
        end;
      oldlev := level; oldtop := top;
      pushlvl(forw, lcp);
      if fsy = procsy then
        begin parameterlist([semicolon],lcp1,plst);
          if not forw then lcp^.pflist := lcp1
          else begin
            if plst then compparam(lcp^.pflist, lcp1);
            putnamlst(lcp1) { redeclare, dispose of copy }
          end
        end
      else
        begin parameterlist([semicolon,colon],lcp1,plst);
          if not forw then lcp^.pflist := lcp1 
          else begin
            if plst then compparam(lcp^.pflist, lcp1);
            putnamlst(lcp1); { redeclare, dispose of copy }
          end;
          if sy = colon then
            begin insymbol;
              if sy = ident then
                begin if forw and iso7185 then error(122);
                  searchid([types],lcp1);
                  lsp := lcp1^.idtype;
                  if forw then begin
                    if not comptypes(lcp^.idtype, lsp) then error(216)
                  end else lcp^.idtype := lsp;
                  if lsp <> nil then
                    if not (lsp^.form in [scalar,subrange,pointer]) then
                      begin error(120); lcp^.idtype := nil end;
                  insymbol
                end
              else begin error(2); skip(fsys + [semicolon]) end
            end
          else
            if not forw or plst then error(123)
        end;
      if sy = semicolon then insymbol else error(14);
      if ((sy = ident) and strequri('forward  ', id)) or (sy = forwardsy)  then
        begin
          if forw then error(161)
          else lcp^.forwdecl := true;
          insymbol;
          if sy = semicolon then insymbol else error(14);
          if not (sy in fsys) then
            begin error(6); skip(fsys) end
        end
      else
        begin lcp^.forwdecl := false;
          { output block begin marker }
          if prcode then begin
            if lcp^.klass = proc then write(prr, 'b r ') else write(prr, 'b f ');
            writev(prr, lcp^.name, lenpv(lcp^.name)); writeln(prr);
          end;
          { output parameter symbols }
          lcp1 := lcp^.pflist;
          while lcp1 <> nil do begin wrtsym(lcp1, 'p'); lcp1 := lcp1^.next end;
          declare(fsys);
          body(fsys + [semicolon],lcp);
          if sy = semicolon then
            begin if prtables then printtables(false); insymbol;
              if iso7185 then begin { handle according to standard }
                if not (sy in [beginsy]+pfbegsys) then
                  begin error(6); skip(fsys) end
              end else begin
                if not (sy in 
                        [labelsy,constsy,typesy,varsy,beginsy]+pfbegsys) then
                  begin error(6); skip(fsys) end
              end
            end
          else begin error(14); skip([semicolon]) end;
          { output block end marker }
          if prcode then
            if lcp^.klass = proc then writeln(prr, 'e r') 
            else writeln(prr, 'e f');
          if lcp^.klass = func then
            if lcp <> ufctptr then
              if not lcp^.asgn then error(193) { no function result assign }
        end;
      level := oldlev; putdsps(oldtop); top := oldtop; lc := llc;
    end (*procdeclaration*) ;
    
  begin (*declare*)
    dp := true;
    repeat
      repeat
        if sy = privatesy then begin insymbol;
          if level > 1 then error(266);
          if (incstk <> nil) and (level <= 1) then 
            incstk^.priv := true { flag private encountered }
        end;
        if not inpriv then begin { if private, get us out quickly }
          if sy = labelsy then
            begin insymbol; labeldeclaration end;
          if sy = constsy then
            begin insymbol; constdeclaration end;
          if sy = typesy then
            begin insymbol; typedeclaration end;
          if sy = varsy then
            begin insymbol; vardeclaration end;
          while sy in pfbegsys do
            begin lsy := sy; insymbol; procdeclaration(lsy) end
        end
      until inpriv or iso7185 or (sy = beginsy) or eofinp or
            not (sy in [privatesy,labelsy,constsy,typesy,varsy]+pfbegsys);
      if (sy <> beginsy) and not inpriv then
        begin error(18); skip(fsys) end
    until (sy in statbegsys) or eofinp or inpriv;
    dp := false;
  end (*declare*) ;

  procedure body{(fsys: setofsys)};
    var
        segsize, gblsize: integer;
        lcmin: stkoff;
        llc1: stkoff; lcp: ctp;
        llp: lbp;
        fp: extfilep;
        test: boolean;
        printed: boolean;
        lsize: addrrange;
        stalvl: integer; { statement nesting level }

    { add statement level }
    procedure addlvl;
    begin
      stalvl := stalvl+1
    end;

    { remove statement level }
    procedure sublvl;
    var llp: lbp;
    begin
       stalvl := stalvl-1;
       { traverse label list for current block and remove any label from
         active status whose statement block has closed }
       llp := display[top].flabel;
       while llp <> nil do with llp^ do begin
         if slevel > stalvl then bact := false;
         if refer and (minlvl > stalvl) then
           minlvl := stalvl;
         llp := nextlab { link next }
       end
    end;

    procedure checkbnds(fsp: stp);
      var lmin,lmax: integer;
          fsp2: stp;
    begin
      if fsp <> nil then begin
        { if set use the base type for the check }
        fsp2 := fsp;
        if fsp^.form = power then fsp := fsp^.elset;
        if fsp <> nil then
          if fsp <> intptr then
            if fsp <> realptr then
              if fsp^.form <= subrange then
                begin
                  getbounds(fsp,lmin,lmax);
                  gen2t(45(*chk*),lmin,lmax,fsp2)
                end
      end
    end (*checkbnds*);
    
    procedure load;
    begin
      with gattr do
        if typtr <> nil then
          begin
            case kind of
              cst: if (typtr^.form <= subrange) and (typtr <> realptr) then
                       if typtr = boolptr then gen2(51(*ldc*),3,cval.ival)
                       else
                         if typtr=charptr then
                           gen2(51(*ldc*),6,cval.ival)
                         else gen2(51(*ldc*),1,cval.ival)
                     else
                       if typtr = nilptr then gen2(51(*ldc*),4,0)
                       else
                         if cstptrix >= cstoccmax then error(254)
                         else
                           begin cstptrix := cstptrix + 1;
                             cstptr[cstptrix] := cval.valp;
                             if typtr = realptr then
                               gen2(51(*ldc*),2,cstptrix)
                             else
                               gen2(51(*ldc*),5,cstptrix)
                           end;
              varbl: case access of
                       drct:   if vlevel<=1 then 
                                 gen1ts(39(*ldo*),dplmt,typtr,symptr)
                               else gen2t(54(*lod*),level-vlevel,dplmt,typtr);
                       indrct: gen1t(35(*ind*),idplmt,typtr);
                       inxd:   error(400)
                     end;
              expr:
            end;
            kind := expr;
            { operand is loaded, and subranges are now normalized to their
              base type }
            typtr := basetype(typtr);
            symptr := nil { break variable association }
          end
    end (*load*) ;
    
    procedure loadaddress;
    begin
      with gattr do
        if typtr <> nil then
          begin
            case kind of
              cst:   if stringt(typtr) then
                       if cstptrix >= cstoccmax then error(254)
                       else
                         begin cstptrix := cstptrix + 1;
                           cstptr[cstptrix] := cval.valp;
                           gen1(38(*lca*),cstptrix)
                         end
                     else error(403);
              varbl: case access of
                       drct:   if vlevel <= 1 then gen1s(37(*lao*),dplmt,symptr)
                               else gen2(50(*lda*),level-vlevel,dplmt);
                       indrct: if idplmt <> 0 then
                                 gen1t(34(*inc*),idplmt,nilptr);
                       inxd:   error(404)
                     end;
              expr:  error(405)
            end;
            kind := varbl; access := indrct; idplmt := 0; packing := false;
            symptr := nil { break variable association }
          end
    end (*loadaddress*) ;

    procedure store(var fattr: attr);
    begin
      with fattr do
        if typtr <> nil then
          case access of
            drct:   if vlevel <= 1 then gen1ts(43(*sro*),dplmt,typtr,symptr)
                    else gen2t(56(*str*),level-vlevel,dplmt,typtr);
            indrct: if idplmt <> 0 then error(401)
                    else gen0t(26(*sto*),typtr);
            inxd:   error(402)
          end
    end (*store*) ;
    
    procedure genfjp(faddr: integer);
    begin load;
      if gattr.typtr <> nil then
        if gattr.typtr <> boolptr then error(144);
      if prcode then 
        begin putic; write(prr,mn[33]:4,' '); prtlabel(faddr); writeln(prr) end;
      ic := ic + 1; mes(33)
    end (*genfjp*) ;

    procedure statement(fsys: setofsys);
      var lcp: ctp; llp: lbp; inherit: boolean;

      procedure expression(fsys: setofsys; threaten: boolean); forward;

      function taggedrec(fsp: stp): boolean;
      var b: boolean;
      begin b := false;
        if fsp <> nil then
          if fsp^.form = tagfld then b := true
          else if fsp^.form = records then
            if fsp^.recvar <> nil then
              b := fsp^.recvar^.form = tagfld;
        taggedrec := b
      end;

      procedure selector(fsys: setofsys; fcp: ctp; skp: boolean);
      var lattr: attr; lcp: ctp; lsize: addrrange; lmin,lmax: integer; 
          id: stp;
      function schblk(fcp: ctp): boolean;
      var i: disprange; f: boolean;
      begin
         f := false;
         for i := level downto 2 do if display[i].bname = fcp then f := true;
         schblk := f
      end;
      procedure checkvrnt(lcp: ctp);
      var vp: stp; vl: ctp; gattrs: attr;
      begin
        if chkvar then begin
        if lcp^.klass = field then begin
          vp := lcp^.varnt; vl := lcp^.varlb;
          if vp <> nil then if vl^.name <> nil then begin { is a variant }
            gattrs := gattr;
            with gattr, vl^ do begin
              typtr := idtype;
              case access of
                drct:   dplmt := dplmt + fldaddr;
                indrct: begin
                          idplmt := idplmt + fldaddr;
                          gen0t(76(*dup*),nilptr)
                        end;
                inxd:   error(406)
              end;
              load;
              gen0(78(*cks*));
              while vp <> nil do begin
                gen1t(75(*ckv*),vp^.varval.ival, basetype(idtype));
                vp := vp^.caslst
              end;
              gen0(77(*cke*));
            end;
            gattr := gattrs
          end
        end
      end
      end;
      begin { selector }
        with fcp^, gattr do
          begin symptr := nil; typtr := idtype; kind := varbl; packing := false;
            packcom := false; tagfield := false; ptrref := false;
            case klass of
              vars: begin symptr := fcp;
                  if typtr <> nil then packing := typtr^.packing;
                  if vkind = actual then
                    begin access := drct; vlevel := vlev;
                      dplmt := vaddr
                    end
                  else
                    begin gen2t(54(*lod*),level-vlev,vaddr,nilptr);
                      access := indrct; idplmt := 0
                    end;
                end;
              field:
                with display[disx] do begin
                  gattr.packcom := display[disx].packing;
                  if typtr <> nil then
                    gattr.packing := display[disx].packing or typtr^.packing;
                  gattr.ptrref := display[disx].ptrref;
                  gattr.tagfield := fcp^.tagfield;
                  gattr.taglvl := fcp^.taglvl;
                  gattr.varnt := fcp^.varnt;
                  if gattr.tagfield then
                    gattr.vartagoff := fcp^.varsaddr-fldaddr;
                  gattr.varssize := fcp^.varssize;
                  if occur = crec then
                    begin access := drct; vlevel := clev;
                      dplmt := cdspl + fldaddr
                    end
                  else if occur = vrec then
                    begin
                      { override to local for with statement }
                      gen2t(54(*lod*),0,vdspl,nilptr);
                      access := indrct; idplmt := fldaddr
                    end
                  else
                    begin
                      if level = 1 then gen1t(39(*ldo*),vdspl,nilptr)
                      else gen2t(54(*lod*),0,vdspl,nilptr);
                      access := indrct; idplmt := fldaddr
                    end
                end;
              func:
                if pfdeckind = standard then
                  begin error(150); typtr := nil end
                else
                  begin
                    if pfkind = formal then error(151)
                    else
                      if not schblk(fcp) then error(192);
                      begin access := drct; vlevel := pflev + 1;
                        { determine size of FR. This is a bit of a hack 
                          against the fact that int/ptr results fit in
                          the upper half of the FR. }
                        id := basetype(fcp^.idtype);
                        lsize := maxresult; if id <> nil then lsize := id^.size;
                        if lsize < maxresult then
                          (*impl. relat. addr. of fct. result*)
                          dplmt := markfv+trunc(maxresult/2)
                        else
                          dplmt := markfv   (*impl. relat. addr. of fct. result*)
                      end
                  end
            end (*case*)
          end (*with*);
        if not (sy in selectsys + fsys) and not skp then
          begin error(59); skip(selectsys + fsys) end;
        while sy in selectsys do
          begin
      (*[*) if sy = lbrack then
              begin gattr.ptrref := false;
                repeat lattr := gattr;
                  with lattr do
                    if typtr <> nil then begin
                      if typtr^.form <> arrays then
                        begin error(138); typtr := nil end
                    end;
                  loadaddress;
                  insymbol; expression(fsys + [comma,rbrack], false);
                  load;
                  if gattr.typtr <> nil then
                    if gattr.typtr^.form<>scalar then error(113)
                    else if not comptypes(gattr.typtr,intptr) then
                           gen0t(58(*ord*),gattr.typtr);
                  if lattr.typtr <> nil then
                    with lattr.typtr^ do
                      begin
                        if comptypes(inxtype,gattr.typtr) then
                          begin
                            if inxtype <> nil then
                              begin getbounds(inxtype,lmin,lmax);
                                if debug then
                                  gen2t(45(*chk*),lmin,lmax,intptr);
                                if lmin>0 then gen1t(31(*dec*),lmin,intptr)
                                else if lmin<0 then
                                  gen1t(34(*inc*),-lmin,intptr);
                                (*or simply gen1(31,lmin)*)
                              end
                          end
                        else error(139);
                        with gattr do
                          begin typtr := aeltype; kind := varbl;
                            access := indrct; idplmt := 0; packing := false;
                            packcom := false; tagfield := false; ptrref := false
                          end;
                        if gattr.typtr <> nil then
                          begin
                            gattr.packcom := lattr.packing;
                            gattr.packing :=
                              lattr.packing or gattr.typtr^.packing;
                            lsize := gattr.typtr^.size;
                            gen1(36(*ixa*),lsize)
                          end
                      end
                  else gattr.typtr := nil
                until sy <> comma;
                if sy = rbrack then insymbol else error(12)
              end (*if sy = lbrack*)
            else
      (*.*)   if sy = period then
                begin
                  with gattr do
                    begin
                      if typtr <> nil then begin
                        if typtr^.form <> records then
                          begin error(140); typtr := nil end
                      end;
                      insymbol;
                      if sy = ident then
                        begin
                          if typtr <> nil then
                            begin searchsection(typtr^.fstfld,lcp);
                              if lcp = nil then
                                begin error(152); typtr := nil end
                              else
                                with lcp^ do
                                  begin checkvrnt(lcp);
                                    typtr := idtype;
                                    gattr.packcom := gattr.packing;
                                    if typtr <> nil then
                                      gattr.packing :=
                                        gattr.packing or typtr^.packing;
                                    gattr.tagfield := lcp^.tagfield;
                                    gattr.taglvl := lcp^.taglvl;
                                    gattr.varnt := lcp^.varnt;
                                    if gattr.tagfield then
                                      gattr.vartagoff := lcp^.varsaddr-fldaddr;
                                    gattr.varssize := lcp^.varssize;
                                    case access of
                                      drct:   dplmt := dplmt + fldaddr;
                                      indrct: idplmt := idplmt + fldaddr;
                                      inxd:   error(407)
                                    end
                                  end
                            end;
                          insymbol
                        end (*sy = ident*)
                      else error(2)
                    end (*with gattr*)
                end (*if sy = period*)
              else
      (*^*)     begin
                  if gattr.typtr <> nil then
                    with gattr,typtr^ do
                      if form = pointer then
                        begin load; typtr := eltype;
                          if debug then begin
                             if taggedrec(eltype) then
                               gen2t(80(*ckl*),1,maxaddr,nilptr)
                             else gen2t(45(*chk*),1,maxaddr,nilptr);
                          end;
                          with gattr do
                            begin kind := varbl; access := indrct; idplmt := 0;
                              packing := false; packcom := false; 
                              tagfield := false; ptrref := true;
                            end
                        end
                      else
                        if form = files then begin loadaddress;
                           { generate buffer validate for file }
                           if typtr = textptr then 
                             gen1(30(*csp*), 46(*fbv*))
                           else begin
                             gen2(51(*ldc*),1,filtype^.size);
                             gen1(30(*csp*),47(*fvb*))
                           end;
                           { index buffer }
                           gen1t(34(*inc*),fileidsize,gattr.typtr);
                           typtr := filtype;
                        end else error(141);
                  insymbol
                end;
            if not (sy in fsys + selectsys) then
              begin error(6); skip(fsys + selectsys) end
          end (*while*)
      end (*selector*) ;

      procedure call(fsys: setofsys; fcp: ctp; inherit: boolean);
        var lkey: keyrng;

        procedure variable(fsys: setofsys; threaten: boolean);
          var lcp: ctp;
        begin
          if sy = ident then
            begin searchid([vars,field],lcp); insymbol end
          else begin error(2); lcp := uvarptr end;
          if threaten and (lcp^.klass = vars) then with lcp^ do begin
            if vlev < level then threat := true;
            if forcnt > 0 then error(195);
            if part = ptview then error(200)
          end;
          selector(fsys,lcp, false)
        end (*variable*) ;
        
        procedure chkhdr;
        var lcp: ctp; dummy: boolean;
        begin
          if sy = ident then begin { test for file }
            searchidnenm([vars],lcp,dummy);
            if (lcp = inputptr) and not inputptr^.hdr then error(175)
            else if (lcp = outputptr) and not outputptr^.hdr then error(176)
            else if (lcp = prdptr) and not outputptr^.hdr then error(217)
            else if (lcp = prrptr) and not outputptr^.hdr then error(218)
            else if (lcp = errorptr) and not outputptr^.hdr then error(219)
            else if (lcp = listptr) and not outputptr^.hdr then error(220)
            else if (lcp = commandptr) and not outputptr^.hdr then error(221)
          end
        end;

        procedure getputresetrewriteprocedure;
        begin chkhdr; variable(fsys + [rparent], false); loadaddress;
          if gattr.typtr <> nil then
            if gattr.typtr^.form <> files then error(116);
          if lkey <= 2 then begin
            if gattr.typtr = textptr then gen1(30(*csp*),lkey(*get,put*))
            else begin
              if gattr.typtr <> nil then
                gen2(51(*ldc*),1,gattr.typtr^.filtype^.size);
              if lkey = 1 then gen1(30(*csp*),38(*gbf*))
              else gen1(30(*csp*),39(*pbf*))
            end
          end else
            if gattr.typtr = textptr then begin
              if lkey = 3 then gen1(30(*csp*),25(*reset*))
              else gen1(30(*csp*),26(*rewrite*))
            end else begin
              if lkey = 3 then gen1(30(*csp*),36(*reset*))
              else gen1(30(*csp*),37(*rewrite*))
            end
        end (*getputresetrewrite*) ;

        procedure pageprocedure;
        var llev:levrange;
        begin
          llev := 1;
          if sy = lparent then
          begin insymbol; chkhdr;
            variable(fsys + [rparent], false); loadaddress;
            if gattr.typtr <> nil then
              if gattr.typtr <> textptr then error(116);
            if sy = rparent then insymbol else error(4)
          end else begin
            if not outputptr^.hdr then error(176);
            gen1(37(*lao*),outputptr^.vaddr);
          end;
          gen1(30(*csp*),24(*page*))
        end (*page*) ;

        procedure readprocedure;
          var lsp : stp;
              txt: boolean; { is a text file }
              deffil: boolean; { default file was loaded }
              test: boolean;
              lmin,lmax: integer;
              len:addrrange;
              fld, spad: boolean;
        begin
          txt := true; deffil := true;
          if sy = lparent then
            begin insymbol; chkhdr;
              variable(fsys + [comma,colon,rparent], true);
              lsp := gattr.typtr; test := false;
              if lsp <> nil then
                if lsp^.form = files then
                  with gattr, lsp^ do
                    begin
                      txt := lsp = textptr;
                      if not txt and (lkey = 11) then error(116);
                      loadaddress; deffil := false;
                      if sy = rparent then
                        begin if lkey = 5 then error(116);
                          test := true
                        end
                      else
                        if sy <> comma then
                          begin error(116); 
                            skip(fsys + [comma,colon,rparent]) 
                          end;
                      if sy = comma then
                        begin insymbol; 
                          variable(fsys + [comma,colon,rparent], true)
                        end
                      else test := true
                    end
                else if not inputptr^.hdr then error(175);
             if not test then
              repeat loadaddress;
                if deffil then begin
                  { file was not loaded, we load and swap so that it ends up
                    on the bottom.}
                  gen1(37(*lao*),inputptr^.vaddr);
                  gen1(72(*swp*),ptrsize); { note 2nd is always pointer }
                  deffil := false
                end;
                if txt then begin lsp := gattr.typtr;
                  fld := false; spad := false;
                  if sy = colon then begin { field }
                    chkstd; insymbol; 
                    if (sy = mulop) and (op = mul) then begin
                      spad := true; insymbol;
                      if not stringt(lsp) then error(215);
                    end else begin
                      expression(fsys + [comma,rparent], false);
                      if gattr.typtr <> nil then
                        if basetype(gattr.typtr) <> intptr then error(116);
                      load; fld := true 
                    end
                  end;
                  if lsp <> nil then
                    if (lsp^.form <= subrange) or 
                       (stringt(lsp) and not iso7185) then
                      if comptypes(intptr,lsp) then begin
                        if debug then begin
                          getbounds(lsp, lmin, lmax);
                          gen1t(51(*ldc*),lmin,basetype(lsp));
                          gen1t(51(*ldc*),lmax,basetype(lsp));
                          if fld then gen1(30(*csp*),74(*ribf*))
                          else gen1(30(*csp*),40(*rib*))
                        end else if fld then gen1(30(*csp*),75(*rdif*))
                                 else gen1(30(*csp*),3(*rdi*))
                      end else
                        if comptypes(realptr,lsp) then
                          if fld then gen1(30(*csp*),76(*rdrf*))
                          else gen1(30(*csp*),4(*rdr*))
                        else
                          if comptypes(charptr,lsp) then begin
                            if debug then begin
                              getbounds(lsp, lmin, lmax);
                              gen2(51(*ldc*),6,lmin);
                              gen2(51(*ldc*),6,lmax);
                              if fld then gen1(30(*csp*),77(*rcbf*))
                              else gen1(30(*csp*),41(*rcb*))
                            end else if fld then gen1(30(*csp*),78(*rdcf*))
                                     else gen1(30(*csp*),5(*rdc*))
                          end else if stringt(lsp) then begin
                            len := lsp^.size div charmax;
                            gen2(51(*ldc*),1,len);
                            if fld then gen1(30(*csp*),79(*rdsf*))
                            else if spad then gen1(30(*csp*),80(*rdsp*))
                            else gen1(30(*csp*),73(*rds*))
                          end else error(399)
                    else error(116);
                end else begin { binary file }
                  if not comptypes(gattr.typtr,lsp^.filtype) then error(129);
                  gen2(51(*ldc*),1,lsp^.filtype^.size);
                  gen1(30(*csp*),35(*rbf*))
                end;
                test := sy <> comma;
                if not test then
                  begin insymbol; variable(fsys + [comma,colon,rparent], true)
                  end
              until test;
              if sy = rparent then insymbol else error(4)
            end
          else begin
            if not inputptr^.hdr then error(175);
            if lkey = 5 then error(116);
            gen1(37(*lao*),inputptr^.vaddr);
          end;
          if lkey = 11 then gen1(30(*csp*),21(*rln*));
          { remove the file pointer from stack }
          gen1(71(*dmp*),ptrsize);
        end (*read*) ;

        procedure writeprocedure;
          var lsp,lsp1: stp; default, default1: boolean; llkey: 1..15;
              len:addrrange;
              txt: boolean; { is a text file }
              byt: boolean; { is a byte file }
              deffil: boolean; { default file was loaded }
              test: boolean;
              r: integer; { radix of print }
              spad: boolean; { write space padded string }
              ledz: boolean; { use leading zeros }
        begin llkey := lkey; txt := true; deffil := true; byt := false;
          if sy = lparent then
          begin insymbol; chkhdr;
          expression(fsys + [comma,colon,rparent,hexsy,octsy,binsy], false);
          lsp := gattr.typtr; test := false;
          if lsp <> nil then
            if lsp^.form = files then
              with gattr, lsp^ do
                begin lsp1 := lsp;
                  txt := lsp = textptr;
                  if not txt then begin
                    if lkey = 12 then error(116);
                    byt := isbyte(lsp^.filtype)
                  end;
                  loadaddress; deffil := false;
                  if sy = rparent then
                    begin if llkey = 6 then error(116);
                      test := true
                    end
                  else
                    if sy <> comma then
                      begin error(116); skip(fsys+[comma,rparent]) end;
                  if sy = comma then
                    begin insymbol;
                      expression(fsys+[comma,colon,rparent,hexsy,octsy,binsy], 
                                 false)
                    end
                  else test := true
                end
            else if not outputptr^.hdr then error(176);
          if not test then
          repeat
            lsp := gattr.typtr;
            if lsp <> nil then
              if lsp^.form <= subrange then load else loadaddress;
            lsp := basetype(lsp); { remove any subrange }
            if deffil then begin
              { file was not loaded, we load and swap so that it ends up
                on the bottom.}
              gen1(37(*lao*),outputptr^.vaddr);
              if lsp <> nil then
                if lsp^.form <= subrange then begin
                if lsp^.size < stackelsize then
                  { size of 2nd is minimum stack }
                  gen1(72(*swp*),stackelsize) 
                else
                  gen1(72(*swp*),lsp^.size) { size of 2nd is operand }
              end else
                gen1(72(*swp*),ptrsize); { size of 2nd is pointer }
              deffil := false
            end;
            if txt then begin
              { check radix markers }
              r := 10;
              if sy = hexsy then begin r := 16; insymbol end
              else if sy = octsy then begin r := 8; insymbol end
              else if sy = binsy then begin r := 2; insymbol end;
              if (r <> 10) and (lsp <> intptr) then error(214);
              spad := false; { set no padded string }
              ledz := false; { set no leading zero }
              if sy = colon then
                begin insymbol; 
                  if (sy = mulop) and (op = mul) then begin
                    spad := true; insymbol;
                    if not stringt(lsp) then error(215)
                  end else begin
                    if sy = numsy then 
                      begin chkstd; ledz := true; insymbol end;
                    expression(fsys + [comma,colon,rparent], false);
                    if gattr.typtr <> nil then
                      if basetype(gattr.typtr) <> intptr then error(116);
                    load; 
                  end;
                  default := false
                end
              else default := true;
              if sy = colon then
                begin insymbol; 
                  expression(fsys + [comma,rparent], false);
                  if gattr.typtr <> nil then
                    if basetype(gattr.typtr) <> intptr then error(116);
                  if lsp <> realptr then error(124);
                  load; default1 := false
                end else default1 := true;
              if lsp = intptr then
                begin if default then gen2(51(*ldc*),1,intdeff);
                  if ledz then begin { leading zeros }
                    if r = 10 then gen1(30(*csp*),69(*wiz*))
                    else if r = 16 then gen1(30(*csp*),70(*wizh*))
                    else if r = 8 then gen1(30(*csp*),71(*wizo*))
                    else if r = 2 then gen1(30(*csp*),72(*wizb*))
                  end else begin
                    if r = 10 then gen1(30(*csp*),6(*wri*))
                    else if r = 16 then gen1(30(*csp*),65(*wrih*))
                    else if r = 8 then gen1(30(*csp*),66(*wrio*))
                    else if r = 2 then gen1(30(*csp*),67(*wrib*))
                  end
                end
              else
                if lsp = realptr then
                  begin
                    if default1 then begin
                      if default then gen2(51(*ldc*),1,reldeff);
                      gen1(30(*csp*),8(*wrr*))
                    end else begin
                      if default then gen2(51(*ldc*),1,reldeff);
                      gen1(30(*csp*),28(*wrf*))
                    end
                  end
                else
                  if lsp = charptr then
                    begin if default then gen2(51(*ldc*),1,chrdeff);
                      gen1(30(*csp*),9(*wrc*))
                    end
                  else
                    if lsp = boolptr then
                      begin if default then gen2(51(*ldc*),1,boldeff);
                        gen1(30(*csp*),27(*wrb*))
                      end
                    else
                      if lsp <> nil then
                        begin
                          if lsp^.form = scalar then error(399)
                          else
                            if stringt(lsp) then
                              begin len := lsp^.size div charmax;
                                if default then
                                      gen2(51(*ldc*),1,len);
                                gen2(51(*ldc*),1,len);
                                if spad then gen1(30(*csp*),68(*wrsp*))
                                else gen1(30(*csp*),10(*wrs*))
                              end
                            else error(116)
                        end
            end else begin { binary file }
              if not comptypes(lsp1^.filtype,lsp) then error(129);
              if lsp <> nil then
                if (lsp = intptr) and not byt then gen1(30(*csp*),31(*wbi*))
                else
                  if lsp = realptr then gen1(30(*csp*),32(*wbr*))
                  else
                    if lsp = charptr then gen1(30(*csp*),33(*wbc*))
                    else
                      if lsp = boolptr then gen1(30(*csp*),34(*wbb*))
                      else
                        if lsp^.form <= subrange then begin
                          if byt then gen1(30(*csp*),48(*wbx*))
                          else gen1(30(*csp*),31(*wbi*))
                        end else begin
                                gen2(51(*ldc*),1,lsp1^.filtype^.size);
                                gen1(30(*csp*),30(*wbf*))
                              end
            end;
            test := sy <> comma;
            if not test then
              begin insymbol; 
                expression(fsys + [comma,colon,rparent,hexsy,octsy,binsy], 
                           false)
              end
          until test;
          if sy = rparent then insymbol else error(4)
          end else begin
            if not outputptr^.hdr then error(176);
            if lkey = 6 then error(116);
            gen1(37(*lao*),outputptr^.vaddr);
          end;
          if llkey = 12 then (*writeln*)
            gen1(30(*csp*),22(*wln*));
          { remove the file pointer from stack }
          gen1(71(*dmp*),ptrsize);
        end (*write*) ;

        procedure packprocedure;
          var lsp,lsp1: stp; lb, bs: integer; lattr: attr;
        begin variable(fsys + [comma,rparent], false); loadaddress;
          lsp := nil; lsp1 := nil; lb := 1; bs := 1;
          lattr := gattr;
          if gattr.typtr <> nil then
            with gattr.typtr^ do
              if form = arrays then
                begin lsp := inxtype; lsp1 := aeltype;
                  if (inxtype = charptr) or (inxtype = boolptr) then lb := 0
                  else if inxtype^.form = subrange then lb := inxtype^.min.ival;
                  bs := aeltype^.size
                end
              else error(116);
          if sy = comma then insymbol else error(20);
          expression(fsys + [comma,rparent], false); load;
          if gattr.typtr <> nil then
            if gattr.typtr^.form <> scalar then error(116)
            else
              if not comptypes(lsp,gattr.typtr) then error(116);
          gen2(51(*ldc*),1,lb);
          gen0(21(*sbi*));
          gen2(51(*ldc*),1,bs);
          gen0(15(*mpi*));
          if sy = comma then insymbol else error(20);
          variable(fsys + [rparent], false); loadaddress;
          if gattr.typtr <> nil then
            with gattr.typtr^ do
              if form = arrays then
                begin
                  if not comptypes(aeltype,lsp1) then error(116)
                end
              else error(116);
          if (gattr.typtr <> nil) and (lattr.typtr <> nil) then
            gen2(62(*pck*),gattr.typtr^.size,lattr.typtr^.size)
        end (*pack*) ;

        procedure unpackprocedure;
          var lsp,lsp1: stp; lattr,lattr1: attr; lb, bs: integer;
        begin variable(fsys + [comma,rparent], false); loadaddress;
          lattr := gattr;
          lsp := nil; lsp1 := nil; lb := 1; bs := 1;
          if gattr.typtr <> nil then
            with gattr.typtr^ do
              if form = arrays then lsp1 := aeltype
              else error(116);
          if sy = comma then insymbol else error(20);
          variable(fsys + [comma,rparent], false); loadaddress;
          lattr1 := gattr;
          if gattr.typtr <> nil then
            with gattr.typtr^ do
              if form = arrays then
                begin
                  if not comptypes(aeltype,lsp1) then error(116);
                  if (inxtype = charptr) or (inxtype = boolptr) then lb := 0
                  else if inxtype^.form = subrange then lb := inxtype^.min.ival;
                  bs := aeltype^.size;
                  lsp := inxtype;
                end
              else error(116);
          if sy = comma then insymbol else error(20);
          expression(fsys + [rparent], false); load;
          if gattr.typtr <> nil then
            if gattr.typtr^.form <> scalar then error(116)
            else
              if not comptypes(lsp,gattr.typtr) then error(116);
          gen2(51(*ldc*),1,lb);
          gen0(21(*sbi*));
          gen2(51(*ldc*),1,bs);
          gen0(15(*mpi*));
          if (lattr.typtr <> nil) and (lattr1.typtr <> nil) then
            gen2(63(*upk*),lattr.typtr^.size,lattr1.typtr^.size)
        end (*unpack*) ;

        procedure newdisposeprocedure(disp: boolean);
          label 1;
          var lsp,lsp1,lsp2: stp; varts: integer;
              lsize: addrrange; lval: valu; tagc: integer; tagrec: boolean;
        begin
          if disp then begin 
            expression(fsys + [comma, rparent], false);
            load
          end else begin
            variable(fsys + [comma,rparent], false); 
            loadaddress
          end;
          lsp := nil; varts := 0; lsize := 0; tagc := 0; tagrec := false;
          if gattr.typtr <> nil then
            with gattr.typtr^ do
              if form = pointer then
                begin
                  if eltype <> nil then
                    begin lsize := eltype^.size;
                      if eltype^.form = records then lsp := eltype^.recvar
                    end
                end
              else error(116);
          tagrec := taggedrec(lsp);
          while sy = comma do
            begin insymbol;constexpr(fsys + [comma,rparent],lsp1,lval);
              if not lval.intval then 
                    begin lval.intval := true; lval.ival := 1 end;
              varts := varts + 1; lsp2 := lsp1;
              (*check to insert here: is constant in tagfieldtype range*)
              if lsp = nil then error(158)
              else
                if lsp^.form <> tagfld then error(162)
                else
                  if lsp^.tagfieldp <> nil then
                    if stringt(lsp1) or (lsp1 = realptr) then error(159)
                    else
                      if comptypes(lsp^.tagfieldp^.idtype,lsp1) then
                        begin
                          lsp1 := lsp^.fstvar;
                          while lsp1 <> nil do
                            with lsp1^ do
                              if varval.ival = lval.ival then
                                begin lsize := size; lsp := subvar;
                                  if debug then begin
                                    if lsp2=charptr then
                                      gen2(51(*ldc*),6,varval.ival)
                                    else gen2(51(*ldc*),1,varval.ival)
                                  end;
                                  tagc := tagc+1;
                                  goto 1
                                end
                              else lsp1 := nxtvar;
                          lsize := lsp^.size; lsp := nil;
                        end
                      else error(116);
        1:  end (*while*) ;
          if debug and tagrec then gen2(51(*ldc*),1,tagc);
          gen2(51(*ldc*),1,lsize);
          if debug and tagrec then begin
            if lkey = 9 then gen1(30(*csp*),42(*nwl*))
            else gen1(30(*csp*),43(*dsl*));
            mesl(tagc*intsize)
          end else begin
            if lkey = 9 then gen1(30(*csp*),12(*new*))
            else gen1(30(*csp*),29(*dsp*))
          end;
        end (*newdisposeprocedure*) ;

        procedure absfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr = intptr then gen0(0(*abi*))
            else
              if gattr.typtr = realptr then gen0(1(*abr*))
              else begin error(125); gattr.typtr := intptr end
        end (*abs*) ;

        procedure sqrfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr = intptr then gen0(24(*sqi*))
            else
              if gattr.typtr = realptr then gen0(25(*sqr*))
              else begin error(125); gattr.typtr := intptr end
        end (*sqr*) ;

        procedure truncfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr <> realptr then error(125);
          gen0(27(*trc*));
          gattr.typtr := intptr
        end (*trunc*) ;

        procedure roundfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr <> realptr then error(125);
          gen0(61(*rnd*));
          gattr.typtr := intptr
        end (*round*) ;

        procedure oddfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr <> intptr then error(125);
          gen0(20(*odd*));
          gattr.typtr := boolptr
        end (*odd*) ;

        procedure ordfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr^.form >= power then error(125);
          gen0t(58(*ord*),gattr.typtr);
          gattr.typtr := intptr
        end (*ord*) ;

        procedure chrfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr <> intptr then error(125);
          gen0(59(*chr*));
          gattr.typtr := charptr
        end (*chr*) ;

        procedure predsuccfunction;
        begin
          if gattr.typtr <> nil then
            if gattr.typtr^.form <> scalar then error(125);
          if lkey = 7 then gen1t(31(*dec*),1,gattr.typtr)
          else gen1t(34(*inc*),1,gattr.typtr)
        end (*predsucc*) ;

        procedure eofeolnfunction;
        begin
          if sy = lparent then
            begin insymbol; variable(fsys + [rparent], false);
              if sy = rparent then insymbol else error(4);
              loadaddress
            end
          else begin
            if not inputptr^.hdr then error(175);
            gen1(37(*lao*),inputptr^.vaddr);
            gattr.typtr := textptr
          end;
          if gattr.typtr <> nil then
            if gattr.typtr^.form <> files then error(125)
            else if (lkey = 10) and (gattr.typtr <> textptr) then error(116);
          if lkey = 9 then begin
            if gattr.typtr = textptr then gen1(30(*csp*),44(*eof*))
            else gen1(30(*csp*),45(*efb*))
          end else gen1(30(*csp*),14(*eln*));
            gattr.typtr := boolptr
        end (*eof*) ;
        
        procedure assignprocedure;
          var len: addrrange; lattr: attr;
        begin chkstd; chkhdr;
          variable(fsys+[comma,rparent], false); loadaddress;
          if gattr.typtr <> nil then
            if gattr.typtr^.form <> files then error(125);
          if sy = comma then insymbol else error(20);  
          lattr := gattr;
          expression(fsys + [rparent], false); loadaddress;  
          if not stringt(gattr.typtr) then error(208);
          if gattr.typtr <> nil then begin
            len := gattr.typtr^.size div charmax;
            gen2(51(*ldc*),1,len);
            if lattr.typtr = textptr then { text }
              gen1(30(*csp*),49(*asst*))
            else { binary }
              gen1(30(*csp*),59(*assb*))
          end
        end;
        
        procedure closeupdateappendprocedure;
        begin chkstd; chkhdr;
          variable(fsys+[rparent], false); loadaddress;
          if gattr.typtr <> nil then
            if gattr.typtr^.form <> files then error(125);
          if lkey = 20 then begin
            if gattr.typtr = textptr then { text } 
              gen1(30(*csp*),50(*clst*))
            else { binary }
              gen1(30(*csp*),60(*clst*))
          end else if lkey = 24 then begin
            if gattr.typtr = textptr then error(262);
            gen1(30(*csp*),52(*upd*))
          end else begin
            if gattr.typtr = textptr then { text }
              gen1(30(*csp*),53(*appt*))
            else { binary }
              gen1(30(*csp*),61(*appb*))
          end
        end;
        
        procedure positionprocedure;
        begin chkstd; chkhdr;
          variable(fsys+[comma,rparent], false); loadaddress;
          if gattr.typtr <> nil then begin
            if gattr.typtr^.form <> files then error(125);
            if gattr.typtr = textptr then error(262);
          end;
          if sy = comma then insymbol else error(20);  
          expression(fsys + [rparent], false); load;  
          if gattr.typtr <> nil then
            if gattr.typtr <> intptr then error(125);
          gen1(30(*csp*),51(*pos*));
        end;
        
        procedure deleteprocedure;
        var len: addrrange;
        begin chkstd;
          expression(fsys + [rparent], false); loadaddress;  
          if not stringt(gattr.typtr) then error(208);
          if gattr.typtr <> nil then begin
            len := gattr.typtr^.size div charmax;
            gen2(51(*ldc*),1,len);
            gen1(30(*csp*),54(*del*));
          end
        end;
        
        procedure changeprocedure;
        var len: addrrange;
        begin chkstd;
          expression(fsys + [comma,rparent], false); loadaddress;  
          if not stringt(gattr.typtr) then error(208);
          if gattr.typtr <> nil then begin
            len := gattr.typtr^.size div charmax;
            gen2(51(*ldc*),1,len)
          end;
          if sy = comma then insymbol else error(20);
          expression(fsys + [rparent], false); loadaddress;  
          if not stringt(gattr.typtr) then error(208);
          if gattr.typtr <> nil then begin
            len := gattr.typtr^.size div charmax;
            gen2(51(*ldc*),1,len)
          end;
          gen1(30(*csp*),55(*del*));
        end;
        
        procedure lengthlocationfunction;
        begin chkstd; chkhdr;
          if sy = lparent then insymbol else error(9);
          variable(fsys+[rparent], false); loadaddress;
          if gattr.typtr <> nil then begin
            if gattr.typtr^.form <> files then error(125);
            if gattr.typtr = textptr then error(262);
          end;
          if lkey = 21 then gen1(30(*csp*),56(*len*))
          else gen1(30(*csp*),57(*loc*));
          if sy = rparent then insymbol else error(4);
          gattr.typtr := intptr
        end;
              
        procedure existsfunction;
        var len: addrrange;
        begin chkstd;
          if sy = lparent then insymbol else error(9);
          expression(fsys + [rparent], false); loadaddress;
          if not stringt(gattr.typtr) then error(208);
          if gattr.typtr <> nil then begin
            len := gattr.typtr^.size div charmax;
            gen2(51(*ldc*),1,len)
          end;
          gen1(30(*csp*),58(*exs*));
          if sy = rparent then insymbol else error(4);
          gattr.typtr := boolptr
        end;
        
        procedure haltprocedure;
        begin chkstd;
          gen1(30(*csp*),62(*hlt*))
        end;
        
        procedure assertprocedure;
        var len: addrrange;
        begin chkstd;
          expression(fsys+[comma,rparent], false); load;
          if gattr.typtr <> nil then
            if gattr.typtr <> boolptr then error(135);
          if sy = comma then begin insymbol;
            expression(fsys + [rparent], false); loadaddress;
            if not stringt(gattr.typtr) then error(208);
            if gattr.typtr <> nil then begin
              len := gattr.typtr^.size div charmax;
              gen2(51(*ldc*),1,len);
              gen1(30(*csp*),64(*asts*))
            end
          end else
            gen1(30(*csp*),63(*ast*))
        end;
        
        procedure throwprocedure;
        begin chkstd;
          variable(fsys+[rparent], false); loadaddress;
          if gattr.typtr <> nil then begin
            if gattr.typtr^.form <> exceptf then error(226);
          end;
          gen1(30(*csp*),85(*thw*));
        end;

        procedure callnonstandard(fcp: ctp; inherit: boolean);
          var nxt,lcp: ctp; lsp: stp; lkind: idkind; lb: boolean;
              locpar, llc: addrrange; varp: boolean; lsize: addrrange;

        procedure compparam(pla, plb: ctp);
        begin
          while (pla <> nil) and (plb <> nil) do begin
            if not comptypes(pla^.idtype,plb^.idtype) then error(189);
            pla := pla^.next; plb := plb^.next
          end;
          if (pla <> nil) or (plb <> nil) then error(189)
        end;

        begin locpar := 0;
          with fcp^ do
            begin nxt := pflist; lkind := pfkind;
              if pfkind = actual then begin { it's a system call }
                if not externl then gen1(41(*mst*),level-pflev)
              end else gen1(41(*mst*),level-pflev) { its an indirect }
            end;
          if sy = lparent then
            begin llc := lc;
              repeat lb := false; (*decide whether proc/func must be passed*)
                if nxt = nil then error(126)
                else lb := nxt^.klass in [proc,func];
                insymbol;
                if lb then   (*pass function or procedure*)
                  begin
                    if sy <> ident then
                      begin error(2); skip(fsys + [comma,rparent]) end
                    else if nxt <> nil then
                      begin
                        if nxt^.klass = proc then searchid([proc],lcp)
                        else
                          begin searchid([func],lcp);
                            { compare result types }
                            if not comptypes(lcp^.idtype,nxt^.idtype) then
                              error(128)
                          end;
                        { compare parameter lists }
                        if (nxt^.klass in [proc,func]) and
                           (lcp^.klass in [proc,func]) then
                          compparam(nxt^.pflist, lcp^.pflist);
                        if lcp^.pfkind = actual then genlpa(lcp^.pfname,level-lcp^.pflev)
                        else gen2(74(*lip*),level-lcp^.pflev,lcp^.pfaddr);
                        locpar := locpar+ptrsize*2;
                        insymbol;
                        if not (sy in fsys + [comma,rparent]) then
                          begin error(6); skip(fsys + [comma,rparent]) end
                      end
                  end (*if lb*)
                else
                  begin varp := false;
                    if nxt <> nil then varp := nxt^.vkind = formal;
                    if varp then variable(fsys + [comma,rparent], varp)
                    else expression(fsys + [comma,rparent], varp);
                    if gattr.typtr <> nil then
                      begin
                        if nxt <> nil then
                          begin lsp := nxt^.idtype;
                            if lsp <> nil then
                              begin
                                if (nxt^.vkind = actual) then begin
                                  if lsp^.form <= power then
                                    begin load;
                                      if debug then checkbnds(lsp);
                                      if comptypes(realptr,lsp)
                                         and (gattr.typtr = intptr) then
                                        begin gen0(10(*flt*));
                                          gattr.typtr := realptr
                                        end;
                                      locpar := locpar+lsp^.size;
                                      alignu(parmptr,locpar);
                                    end
                                  else
                                    begin
                                      loadaddress;
                                      locpar := locpar+ptrsize;
                                      alignu(parmptr,locpar)
                                    end;
                                    if not comptypes(lsp,gattr.typtr) then
                                      error(142)
                                end else begin
                                  if gattr.kind = varbl then
                                    begin if gattr.packcom then error(197);
                                      if gattr.tagfield then error(198);
                                      loadaddress;
                                      locpar := locpar+ptrsize;
                                      alignu(parmptr,locpar);
                                    end
                                  else error(154);
                                  if lsp <> gattr.typtr then error(199)
                                end

                              end
                          end
                      end
                  end;
                if nxt <> nil then nxt := nxt^.next
              until sy <> comma;
              lc := llc;
              if sy = rparent then insymbol else error(4)
            end (*if lparent*);
          if lkind = actual then
            begin if nxt <> nil then error(126);
              with fcp^ do
                begin
                  if externl then gen1(30(*csp*),pfname)
                  else begin
                    if pfattr = fpavirtual then begin
                      if inherit then begin lcp := fcp^.grpnxt;
                        if lcp = nil then error(235);
                        if lcp^.pfattr <> fpaoverride then error(507);
                        { inherited calls will never be far }
                        gen1(91(*cuv*),lcp^.pfvaddr)
                      end else gen1s(91(*cuv*),fcp^.pfvid^.vaddr,fcp^.pfvid)
                    end else begin
                      if inherit then error(234);
                      gencupent(46(*cup*),locpar,pfname,fcp)
                    end;
                    if fcp^.klass = func then begin
                      { add size of function result back to stack }
                      lsize := fcp^.idtype^.size;
                      alignu(parmptr,lsize);
                      mesl(-lsize)
                    end
                  end
                end
            end
          else begin { call procedure or function parameter }
            gen2(50(*lda*),level-fcp^.pflev,fcp^.pfaddr);
            gen1(67(*cip*),locpar);
            mesl(locpar); { remove stack parameters }
            if fcp^.klass = func then begin
              { add size of function result back to stack }
              lsize := fcp^.idtype^.size;
              alignu(parmptr,lsize);
              mesl(-lsize)
            end
          end;
          gattr.typtr := fcp^.idtype
        end (*callnonstandard*) ;

      begin (*call*)
        if fcp^.pfdeckind = standard then
          begin lkey := fcp^.key; if inherit then error(233);
            if fcp^.klass = proc then
              begin
                if not(lkey in [5,6,11,12,17,29]) then
                  if sy = lparent then insymbol else error(9);
                case lkey of
                  1,2,
                  3,4:    getputresetrewriteprocedure;
                  17:     pageprocedure;
                  5,11:   readprocedure;
                  6,12:   writeprocedure;
                  7:      packprocedure;
                  8:      unpackprocedure;
                  9,18:   newdisposeprocedure(lkey = 18);
                  19:     assignprocedure;
                  20, 24, 
                  25:     closeupdateappendprocedure;
                  23:     positionprocedure;
                  27:     deleteprocedure;
                  28:     changeprocedure;
                  29:     haltprocedure;
                  30:     assertprocedure;
                  31:     throwprocedure;
                  10,13:  error(399)
                end;
                if not(lkey in [5,6,11,12,17,29]) then
                  if sy = rparent then insymbol else error(4)
              end
            else
              begin
                if (lkey <= 8) or (lkey = 16) then
                  begin
                    if sy = lparent then insymbol else error(9);
                    expression(fsys+[rparent], false); load
                  end;
                case lkey of
                  1:     absfunction;
                  2:     sqrfunction;
                  3:     truncfunction;
                  16:    roundfunction;
                  4:     oddfunction;
                  5:     ordfunction;
                  6:     chrfunction;
                  7,8:   predsuccfunction;
                  9,10:  eofeolnfunction;
                  21,22: lengthlocationfunction;
                  26:    existsfunction;
                end;
                if (lkey <= 8) or (lkey = 16) then
                  if sy = rparent then insymbol else error(4)
              end;
          end (*standard procedures and functions*)
        else callnonstandard(fcp,inherit)
      end (*call*) ;

      procedure expression{(fsys: setofsys; threaten: boolean)};
        var lattr: attr; lop: operatort; typind: char; lsize: addrrange;

        procedure simpleexpression(fsys: setofsys; threaten: boolean);
          var lattr: attr; lop: operatort; signed: boolean;

          procedure term(fsys: setofsys; threaten: boolean);
            var lattr: attr; lop: operatort;

            procedure factor(fsys: setofsys; threaten: boolean);
              var lcp: ctp; lvp: csp; varpart: boolean; inherit: boolean;
                  cstpart: setty; lsp: stp; tattr, rattr: attr; test: boolean;
            begin
              if not (sy in facbegsys) then
                begin error(58); skip(fsys + facbegsys);
                  gattr.typtr := nil
                end;
              while sy in facbegsys do
                begin inherit := false;
                  if sy = inheritedsy then begin insymbol; inherit := true;
                    if not (sy in facbegsys) then
                    begin error(58); skip(fsys + facbegsys); 
                          gattr.typtr := nil end;
                    if sy <> ident then error(233);
                  end;
                  if sy in facbegsys then case sy of
            (*id*)    ident:
                      begin searchid([types,konst,vars,field,func],lcp);
                        insymbol;
                        if lcp^.klass = func then
                          begin call(fsys,lcp, inherit);
                            with gattr do
                              begin kind := expr;
                                if typtr <> nil then
                                  if typtr^.form=subrange then
                                    typtr := typtr^.rangetype
                              end
                          end
                        else begin if inherit then error(233);
                          if lcp^.klass = konst then
                            with gattr, lcp^ do
                              begin typtr := idtype; kind := cst;
                                cval := values
                              end
                          else 
                            if lcp^.klass = types then begin
                              { type convert/restrict }
                              chkstd; 
                              if lcp^.idtype <> nil then 
                                if (lcp^.idtype^.form <> scalar) and
                                   (lcp^.idtype^.form <> subrange) then 
                                error(223);
                              { if simple underfined error and no () trailer,
                                then assume it is just an undefined id }
                              if (lcp <> utypptr) or (sy = lparent) then begin
                                if sy <> lparent then error(9);
                                insymbol; expression(fsys + [rparent], false);
                                load;
                                if sy = rparent then insymbol else error(4);
                                if gattr.typtr <> nil then
                                  if (gattr.typtr^.form <> scalar) and 
                                     (gattr.typtr^.form <> subrange) then 
                                     error(224);
                                { bounds check to target type }
                                checkbnds(lcp^.idtype);
                                gattr.typtr := lcp^.idtype { retype }
                              end 
                            end else
                              begin selector(fsys,lcp,false);
                                if threaten and (lcp^.klass = vars) then with lcp^ do begin
                                  if vlev < level then threat := true;
                                  if forcnt > 0 then error(195);
                                  if part = ptview then error(200)
                                end;
                                if gattr.typtr<>nil then(*elim.subr.types to*)
                                  with gattr,typtr^ do(*simplify later tests*)
                              end
                        end
                      end;
            (*cst*)   intconst:
                      begin
                        with gattr do
                          begin typtr := intptr; kind := cst;
                            cval := val
                          end;
                        insymbol
                      end;
                      realconst:
                      begin
                        with gattr do
                          begin typtr := realptr; kind := cst;
                            cval := val
                          end;
                        insymbol
                      end;
                      stringconst:
                      begin
                        with gattr do
                          begin
                            if lgth = 1 then typtr := charptr
                            else
                              begin new(lsp,arrays); pshstc(lsp);
                                with lsp^ do
                                  begin aeltype := charptr; form:=arrays;
                                    packing := true;
                                    inxtype := nil; size := lgth*charsize
                                  end;
                                typtr := lsp
                              end;
                            kind := cst; cval := val
                          end;
                        insymbol
                      end;
            (* ( *)   lparent:
                      begin insymbol; expression(fsys + [rparent], false);
                        if sy = rparent then insymbol else error(4)
                      end;
            (*not*)   notsy:
                      begin insymbol; factor(fsys, false);
                        load; gen0t(19(*not*),gattr.typtr);
                        if gattr.typtr <> nil then
                          if (gattr.typtr <> boolptr) and 
                            ((gattr.typtr <> intptr) or iso7185) then
                            begin error(135); gattr.typtr := nil end;
                      end;
            (*[*)     lbrack:
                      begin insymbol; cstpart := [ ]; varpart := false;
                        new(lsp,power); pshstc(lsp);
                        with lsp^ do
                          begin elset:=nil;size:=setsize;form:=power;
                                packing := false; matchpack := false end;
                        if sy = rbrack then
                          begin
                            with gattr do
                              begin typtr := lsp; kind := cst end;
                            insymbol
                          end
                        else
                          begin
                            repeat
                              expression(fsys + [comma,range,rbrack], false);
                              rattr.typtr := nil;
                              if sy = range then begin insymbol;
                                { if the left side is not constant, load it
                                  and coerce it to integer now }
                                if gattr.kind <> cst then begin
                                  load;
                                  if not comptypes(gattr.typtr,intptr)
                                  then gen0t(58(*ord*),gattr.typtr);
                                end;
                                tattr := gattr;
                                expression(fsys + [comma,rbrack], false);
                                rattr := gattr; gattr := tattr;
                              end;
                              if gattr.typtr <> nil then
                                if (gattr.typtr^.form <> scalar) and 
                                   (gattr.typtr^.form <> subrange) then
                                  begin error(136); gattr.typtr := nil end
                                else if comptypes(gattr.typtr,realptr) then
                                  begin error(109); gattr.typtr := nil end
                                else
                                  if comptypes(lsp^.elset,gattr.typtr) then
                                    begin
                                      if rattr.typtr <> nil then begin { x..y form }
                                        if (rattr.typtr^.form <> scalar) and
                                           (rattr.typtr^.form <> subrange) then
                                          begin error(136); rattr.typtr := nil end
                                        else if comptypes(rattr.typtr,realptr) then
                                          begin error(109); rattr.typtr := nil end
                                        else
                                          if comptypes(lsp^.elset,rattr.typtr) then
                                            begin
                                              if (gattr.kind = cst) and
                                                 (rattr.kind = cst) then
                                                if (rattr.cval.ival < setlow) or
                                                   (rattr.cval.ival > sethigh) or
                                                   (gattr.cval.ival < setlow) or
                                                   (gattr.cval.ival > sethigh) then
                                                  error(304)
                                                else
                                                  cstpart := cstpart+
                                                    [gattr.cval.ival..rattr.cval.ival]
                                              else
                                                begin
                                                  if gattr.kind = cst then begin
                                                    load;
                                                    if not comptypes(gattr.typtr,intptr)
                                                      then gen0t(58(*ord*),gattr.typtr)
                                                  end;
                                                  tattr := gattr; gattr := rattr;
                                                  load;
                                                  gattr := tattr;
                                                  if not comptypes(rattr.typtr,intptr)
                                                  then gen0t(58(*ord*),rattr.typtr);
                                                  gen0(64(*rgs*));
                                                  if varpart then gen0(28(*uni*))
                                                  else varpart := true
                                                end
                                            end
                                          else error(137)
                                      end else begin
                                        if gattr.kind = cst then
                                          if (gattr.cval.ival < setlow) or
                                            (gattr.cval.ival > sethigh) then
                                            error(304)
                                          else
                                            cstpart := cstpart+[gattr.cval.ival]
                                        else
                                          begin load;
                                            if not comptypes(gattr.typtr,intptr)
                                            then gen0t(58(*ord*),gattr.typtr);
                                            gen0(23(*sgs*));
                                            if varpart then gen0(28(*uni*))
                                            else varpart := true
                                          end
                                      end;
                                      lsp^.elset := gattr.typtr;
                                      gattr.typtr := lsp
                                    end
                                  else error(137);
                              test := sy <> comma;
                              if not test then insymbol
                            until test;
                            if sy = rbrack then insymbol else error(12)
                          end;
                        if varpart then
                          begin
                            if cstpart <> [ ] then
                              begin new(lvp,pset); pshcst(lvp);
                                lvp^.pval := cstpart;
                                lvp^.cclass := pset;
                                if cstptrix = cstoccmax then error(254)
                                else
                                  begin cstptrix := cstptrix + 1;
                                    cstptr[cstptrix] := lvp;
                                    gen2(51(*ldc*),5,cstptrix);
                                    gen0(28(*uni*)); gattr.kind := expr
                                  end
                              end
                          end
                        else
                          begin new(lvp,pset); pshcst(lvp);
                            lvp^.cclass := pset;
                            lvp^.pval := cstpart;
                            gattr.cval.intval := false;
                            gattr.cval.valp := lvp
                          end
                      end;
            (*nil*)   nilsy: with gattr do
                               begin typtr := nilptr; kind := cst;
                                     cval.intval := true;
                                     cval.ival := nilval;
                                     insymbol
                               end
                  end (*case*) ;
                  if not (sy in fsys) then
                    begin error(6); skip(fsys + facbegsys) end
                end (*while*)
            end (*factor*) ;

          begin (*term*)
            factor(fsys + [mulop], threaten);
            while sy = mulop do
              begin load; lattr := gattr; lop := op;
                insymbol; factor(fsys + [mulop], threaten); load;
                if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
                  case lop of
          (***)     mul:  if (lattr.typtr=intptr)and(gattr.typtr=intptr)
                          then gen0(15(*mpi*))
                          else
                            begin
                              if lattr.typtr = intptr then
                                begin gen0(9(*flo*));
                                  lattr.typtr := realptr
                                end
                              else
                                if gattr.typtr = intptr then
                                  begin gen0(10(*flt*));
                                    gattr.typtr := realptr
                                  end;
                              if (lattr.typtr = realptr)
                                and(gattr.typtr=realptr)then gen0(16(*mpr*))
                              else
                                if(lattr.typtr^.form=power)
                                  and comptypes(lattr.typtr,gattr.typtr)then
                                  gen0(12(*int*))
                                else begin error(134); gattr.typtr:=nil end
                            end;
          (* / *)   rdiv: begin
                            if gattr.typtr = intptr then
                              begin gen0(10(*flt*));
                                gattr.typtr := realptr
                              end;
                            if lattr.typtr = intptr then
                              begin gen0(9(*flo*));
                                lattr.typtr := realptr
                              end;
                            if (lattr.typtr = realptr)
                              and (gattr.typtr=realptr)then gen0(7(*dvr*))
                            else begin error(134); gattr.typtr := nil end
                          end;
          (*div*)   idiv: if (lattr.typtr = intptr)
                            and (gattr.typtr = intptr) then gen0(6(*dvi*))
                          else begin error(134); gattr.typtr := nil end;
          (*mod*)   imod: if (lattr.typtr = intptr)
                            and (gattr.typtr = intptr) then gen0(14(*mod*))
                          else begin error(134); gattr.typtr := nil end;
          (*and*)   andop:if ((lattr.typtr = boolptr) and (gattr.typtr = boolptr)) or
                             ((lattr.typtr=intptr) and (gattr.typtr=intptr) and
                              not iso7185) then gen0(4(*and*))
                          else begin error(134); gattr.typtr := nil end
                  end (*case*)
                else gattr.typtr := nil
              end (*while*)
          end (*term*) ;

        begin (*simpleexpression*)
          signed := false;
          if (sy = addop) and (op in [plus,minus]) then
            begin signed := op = minus; insymbol end;
          term(fsys + [addop], threaten);
          if signed then
            begin load;
              if gattr.typtr = intptr then gen0(17(*ngi*))
              else
                if gattr.typtr = realptr then gen0(18(*ngr*))
                else begin error(134); gattr.typtr := nil end
            end;
          while sy = addop do
            begin load; lattr := gattr; lop := op;
              insymbol; term(fsys + [addop], threaten); load;
              if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
                case lop of
        (*+*)       plus:
                    if (lattr.typtr = intptr)and(gattr.typtr = intptr) then
                      gen0(2(*adi*))
                    else
                      begin
                        if lattr.typtr = intptr then
                          begin gen0(9(*flo*));
                            lattr.typtr := realptr
                          end
                        else
                          if gattr.typtr = intptr then
                            begin gen0(10(*flt*));
                              gattr.typtr := realptr
                            end;
                        if (lattr.typtr = realptr)and(gattr.typtr = realptr)
                          then gen0(3(*adr*))
                        else if(lattr.typtr^.form=power)
                               and comptypes(lattr.typtr,gattr.typtr) then
                               gen0(28(*uni*))
                             else begin error(134); gattr.typtr:=nil end
                      end;
        (*-*)       minus:
                    if (lattr.typtr = intptr)and(gattr.typtr = intptr) then
                      gen0(21(*sbi*))
                    else
                      begin
                        if lattr.typtr = intptr then
                          begin gen0(9(*flo*));
                            lattr.typtr := realptr
                          end
                        else
                          if gattr.typtr = intptr then
                            begin gen0(10(*flt*));
                              gattr.typtr := realptr
                            end;
                        if (lattr.typtr = realptr)and(gattr.typtr = realptr)
                          then gen0(22(*sbr*))
                        else
                          if (lattr.typtr^.form = power)
                            and comptypes(lattr.typtr,gattr.typtr) then
                            gen0(5(*dif*))
                          else begin error(134); gattr.typtr := nil end
                      end;
        (*or*)      orop:
                    if ((lattr.typtr=boolptr) and (gattr.typtr=boolptr)) or 
                       ((lattr.typtr=intptr) and (gattr.typtr=intptr) and 
                        not iso7185) then gen0(13(*ior*))
                    else begin error(134); gattr.typtr := nil end;
        (*xor*)     xorop:
                    if ((lattr.typtr=boolptr) and (gattr.typtr=boolptr)) or 
                       ((lattr.typtr=intptr) and (gattr.typtr=intptr) and 
                        not iso7185) then gen0(83(*ixor*))
                    else begin error(134); gattr.typtr := nil end
                end (*case*)
              else gattr.typtr := nil
            end (*while*)
        end (*simpleexpression*) ;

      begin (*expression*)
        simpleexpression(fsys + [relop], threaten);
        if sy = relop then
          begin
            if gattr.typtr <> nil then
              if gattr.typtr^.form <= power then load
              else loadaddress;
            lattr := gattr; lop := op;
            if lop = inop then
              if not comptypes(gattr.typtr,intptr) then
                gen0t(58(*ord*),gattr.typtr);
            insymbol; simpleexpression(fsys, threaten);
            if gattr.typtr <> nil then
              if gattr.typtr^.form <= power then load
              else loadaddress;
            if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
              if lop = inop then
                if gattr.typtr^.form = power then
                  if comptypes(lattr.typtr,gattr.typtr^.elset) then
                    gen0(11(*inn*))
                  else begin error(129); gattr.typtr := nil end
                else begin error(130); gattr.typtr := nil end
              else
                begin
                  if lattr.typtr <> gattr.typtr then
                    if lattr.typtr = intptr then
                      begin gen0(9(*flo*));
                        lattr.typtr := realptr
                      end
                    else
                      if gattr.typtr = intptr then
                        begin gen0(10(*flt*));
                          gattr.typtr := realptr
                        end;
                  if comptypes(lattr.typtr,gattr.typtr) then
                    begin lsize := lattr.typtr^.size;
                      case lattr.typtr^.form of
                        scalar:
                          if lattr.typtr = realptr then typind := 'r'
                          else
                            if lattr.typtr = boolptr then typind := 'b'
                            else
                              if lattr.typtr = charptr then typind := 'c'
                              else typind := 'i';
                        pointer:
                          begin
                            if lop in [ltop,leop,gtop,geop] then error(131);
                            typind := 'a'
                          end;
                        power:
                          begin if lop in [ltop,gtop] then error(132);
                            typind := 's'
                          end;
                        arrays:
                          begin
                            if not stringt(lattr.typtr)
                              then error(134);
                            typind := 'm'
                          end;
                        records:
                          begin
                            error(134);
                            typind := 'm'
                          end;
                        files:
                          begin error(133); typind := 'f' end
                      end;
                      case lop of
                        ltop: gen2(53(*les*),ord(typind),lsize);
                        leop: gen2(52(*leq*),ord(typind),lsize);
                        gtop: gen2(49(*grt*),ord(typind),lsize);
                        geop: gen2(48(*geq*),ord(typind),lsize);
                        neop: gen2(55(*neq*),ord(typind),lsize);
                        eqop: gen2(47(*equ*),ord(typind),lsize)
                      end
                    end
                  else error(129)
                end;
            gattr.typtr := boolptr; gattr.kind := expr
          end (*sy = relop*)
      end (*expression*) ;

      procedure assignment(fcp: ctp; skp: boolean);
        var lattr, lattr2: attr; tagasc: boolean;
      begin tagasc := false; selector(fsys + [becomes],fcp,skp);
        if (sy = becomes) or skp then
          begin
            { if function result, set assigned }
            if fcp^.klass = func then fcp^.asgn := true
            else if fcp^.klass = vars then with fcp^ do begin
               if vlev < level then threat := true;
               if forcnt > 0 then error(195);
               if part = ptview then error(200)
            end;
            tagasc := false;
            if gattr.kind = varbl then tagasc := gattr.tagfield and debug;
            lattr2 := gattr; { save access before load }
            if gattr.typtr <> nil then
              if (gattr.access<>drct) or (gattr.typtr^.form>power) or
                 tagasc then { if tag checking, force address load }
                loadaddress;
            lattr := gattr;
            insymbol; expression(fsys, false);
            if gattr.typtr <> nil then
              if gattr.typtr^.form <= power then load
              else loadaddress;
            if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
              begin
                if comptypes(realptr,lattr.typtr)and(gattr.typtr=intptr)then
                  begin gen0(10(*flt*));
                    gattr.typtr := realptr
                  end;
                if comptypes(lattr.typtr,gattr.typtr) then begin
                  if filecomponent(gattr.typtr) then error(191);
                  with lattr2 do
                    if kind = varbl then begin
                      if access = indrct then
                        if debug and tagfield and ptrref then
                          { check tag assignment to pointer record }
                          gen2(81(*cta*),idplmt,taglvl);
                      if debug and tagfield then 
                        gen2(82(*ivt*),vartagoff,varssize)
                    end;
                  { if tag checking, bypass normal store }
                  if tagasc then
                     gen0t(26(*sto*),lattr.typtr)
                  else case lattr.typtr^.form of
                    scalar,
                    subrange,
                    power: begin
                                if debug then checkbnds(lattr.typtr);
                                store(lattr)
                              end;
                    pointer: begin
                               if debug then begin
                                 if taggedrec(lattr.typtr^.eltype) then
                                   gen2t(80(*ckl*),0,maxaddr,nilptr)
                                 else gen2t(45(*chk*),0,maxaddr,nilptr);
                               end;
                               store(lattr)
                             end;
                    arrays,
                    records: gen1(40(*mov*),lattr.typtr^.size);
                    files: error(146)
                  end;
                end else error(129)
              end
          end (*sy = becomes*)
        else error(51)
      end (*assignment*) ;

      procedure gotostatement;
        var llp: lbp; ttop,ttop1: disprange;

      begin
        if (sy = intconst) or (sy = ident) then
          begin
            if sy = ident then chkstd;
            ttop := top;
            while display[ttop].occur <> blck do ttop := ttop - 1;
            ttop1 := ttop;
            repeat
              searchlabel(llp, ttop, sy = ident); { find label }
              if llp <> nil then with llp^ do begin
                refer := true;
                if defined then
                  if slevel > stalvl then { defining point level greater than
                                            present statement level }
                    error(185) { goto references deeper nested statement }
                  else if (slevel > 1) and not bact then
                    error(187); { Goto references label in different nested
                                  statement }
                { establish the minimum statement level a goto appeared at }
                if minlvl > stalvl then minlvl := stalvl;
                if ttop = ttop1 then
                  genujpxjp(57(*ujp*),labname)
                else begin { interprocedural goto }
                  genipj(66(*ipj*),level-vlevel,labname);
                  ipcref := true
                end
              end;
              ttop := ttop - 1
            until (llp <> nil) or (ttop = 0);
            if llp = nil then begin
              error(167); { undeclared label }
              newlabel(llp, sy = ident); { create dummy label in current context }
              llp^.refer := true
            end;
            insymbol
          end
        else if iso7185 then error(15) else error(22);
      end (*gotostatement*) ;

      procedure compoundstatement;
      var test: boolean;
      begin
        addlvl;
        repeat
          repeat statement(fsys + [semicolon,endsy])
          until not (sy in statbegsys);
          test := sy <> semicolon;
          if not test then insymbol
        until test;
        if sy = endsy then insymbol else error(13);
        sublvl
      end (*compoundstatemenet*) ;

      procedure ifstatement;
        var lcix1,lcix2: integer;
      begin expression(fsys + [thensy], false);
        genlabel(lcix1); genfjp(lcix1);
        if sy = thensy then insymbol else error(52);
        addlvl;
        statement(fsys + [elsesy]);
        sublvl;
        if sy = elsesy then
          begin genlabel(lcix2); genujpxjp(57(*ujp*),lcix2);
            putlabel(lcix1);
            insymbol;
            addlvl;
            statement(fsys);
            sublvl;
            putlabel(lcix2)
          end
        else putlabel(lcix1)
      end (*ifstatement*) ;

      procedure casestatement;
        label 1;
        var lsp,lsp1,lsp2: stp; fstptr,lpt1,lpt2,lpt3: cip; lvals,lvale: valu;
            laddr, lcix, lcix1, lelse, lelse2, lmin, lmax: integer;
            test: boolean; i,occ: integer;
      function casecount(cp: cip): integer;
      var c: integer;
      begin c := 0; 
        while cp <> nil do 
          begin c := c+cp^.cslabe-cp^.cslabs+1; cp := cp^.next end;
        casecount := c
      end;
      begin expression(fsys + [ofsy,comma,colon], false);
        load; genlabel(lcix); lelse := 0;
        lsp := gattr.typtr;
        if lsp <> nil then
          if (lsp^.form <> scalar) or (lsp = realptr) then
            begin error(144); lsp := nil end
          else if not comptypes(lsp,intptr) then gen0t(58(*ord*),lsp);
        genujpxjp(57(*ujp*),lcix);
        mesl(+intsize); { remove selector from stack }
        if sy = ofsy then insymbol else error(8);
        fstptr := nil; genlabel(laddr);
        repeat
          lpt3 := nil; genlabel(lcix1);
          if not(sy in [semicolon,endsy,elsesy]) then
            begin
              repeat constexpr(fsys + [comma,colon,range],lsp1,lvals);
                if not lvals.intval then 
                    begin lvals.intval := true; lvals.ival := 1 end;
                lvale := lvals;
                if sy = range then begin
                  chkstd; insymbol; 
                  constexpr(fsys + [comma,colon],lsp2,lvale);
                  if not lvale.intval then 
                    begin lvale.intval := true; lvale.ival := 1 end;
                  if lvale.ival < lvals.ival then error(225)
                end;
                if lsp <> nil then
                  if comptypes(lsp,lsp1) then
                    begin lpt1 := fstptr; lpt2 := nil;
                      while lpt1 <> nil do
                        with lpt1^ do
                          begin
                            if (cslabs <= lvale.ival) and
                               (cslabe >= lvals.ival) then error(156);
                            if cslabs <= lvals.ival then goto 1;
                            lpt2 := lpt1; lpt1 := next
                          end;
          1:          getcas(lpt3);
                      with lpt3^ do
                        begin next := lpt1; cslabs := lvals.ival; 
                          cslabe := lvale.ival; csstart := lcix1
                        end;
                      if lpt2 = nil then fstptr := lpt3
                      else lpt2^.next := lpt3
                    end
                  else error(147);
                test := sy <> comma;
                if not test then insymbol
              until test;
              if sy = colon then insymbol else error(5);
              putlabel(lcix1);
              repeat
                addlvl;
                statement(fsys + [semicolon]);
                sublvl
              until not (sy in statbegsys);
              if lpt3 <> nil then genujpxjp(57(*ujp*),laddr);
            end;
          test := sy <> semicolon;
          if not test then insymbol
        until test;
        if sy = elsesy then begin chkstd; insymbol; genlabel(lelse);
          genlabel(lelse2); putlabel(lelse2);
          mesl(-intsize); { put selector on stack }
          gen1(71(*dmp*),intsize); 
          putlabel(lelse);
          addlvl;
          statement(fsys + [semicolon]);
          sublvl;
          genujpxjp(57(*ujp*),laddr);
          if sy = semicolon then insymbol
        end;
        putlabel(lcix);
        mesl(-intsize); { put selector back on stack }
        if fstptr <> nil then
          begin lmax := fstptr^.cslabe;
            (*reverse pointers*)
            lpt1 := fstptr; fstptr := nil;
            repeat lpt2 := lpt1^.next; lpt1^.next := fstptr;
              fstptr := lpt1; lpt1 := lpt2
            until lpt1 = nil;
            lmin := fstptr^.cslabs;
            { find occupancy }
            occ := casecount(fstptr)*100 div (lmax-lmin+1);
            if lmax - lmin < cixmax then 
              begin
                if occ >= minocc then begin { build straight vector table }
                  if lelse > 0 then begin
                    gen0t(76(*dup*),intptr); 
                    gen2(51(*ldc*),1,lmin);
                    gen2(53(*les*),ord('i'),0);
                    genujpxjp(73(*tjp*),lelse2); 
                    gen0t(76(*dup*),intptr);
                    gen2(51(*ldc*),1,lmax); 
                    gen2(49(*grt*),ord('i'),0);
                    genujpxjp(73(*tjp*),lelse2);
                  end else gen2t(45(*chk*),lmin,lmax,intptr);
                  gen2(51(*ldc*),1,lmin); gen0(21(*sbi*)); genlabel(lcix);
                  genujpxjp(44(*xjp*),lcix); putlabel(lcix);
                  repeat
                    with fstptr^ do
                      begin
                        while cslabs > lmin do begin
                           if lelse > 0 then genujpxjp(57(*ujp*),lelse)
                           else gen0(60(*ujc error*));
                           lmin := lmin+1
                        end;
                        for i := cslabs to cslabe do
                          genujpxjp(57(*ujp*),csstart);
                        lpt1 := fstptr; fstptr := next; lmin := cslabe+1;
                        putcas(lpt1);
                      end
                  until fstptr = nil;
                end else begin
                  { devolve to comp/jmp seq }
                  repeat
                    with fstptr^ do begin
                      gencjp(87(*cjp*),cslabs,cslabe,csstart);   
                      lpt1 := fstptr; fstptr := next; lmin := cslabe+1;
                      putcas(lpt1);
                    end
                  until fstptr = nil;
                  if lelse > 0 then genujpxjp(57(*ujp*),lelse2);
                  mesl(+intsize) { remove selector from stack }
                end;
                putlabel(laddr)
              end
            else begin
              error(157);
              repeat
                with fstptr^ do
                  begin
                    lpt1 := fstptr; fstptr := next;
                    putcas(lpt1);
                  end
              until fstptr = nil
            end
          end;
        if sy = endsy then insymbol else error(13)
      end (*casestatement*) ;

      procedure repeatstatement;
        var laddr: integer;
      begin genlabel(laddr); putlabel(laddr);
        addlvl;
        repeat
          statement(fsys + [semicolon,untilsy]);
          if sy in statbegsys then error(14)
        until not(sy in statbegsys);
        while sy = semicolon do
          begin insymbol;
            repeat
              statement(fsys + [semicolon,untilsy]);
              if sy in statbegsys then error(14);
            until not (sy in statbegsys);
          end;
        if sy = untilsy then
          begin insymbol; expression(fsys, false); genfjp(laddr)
          end
        else error(53);
        sublvl
      end (*repeatstatement*) ;

      procedure whilestatement;
        var laddr, lcix: integer;
      begin genlabel(laddr); putlabel(laddr);
        expression(fsys + [dosy], false); genlabel(lcix); genfjp(lcix);
        if sy = dosy then insymbol else error(54);
        addlvl;
        statement(fsys);
        sublvl;
        genujpxjp(57(*ujp*),laddr); putlabel(lcix)
      end (*whilestatement*) ;

      procedure forstatement;
        var lattr: attr;  lsy: symbol;
            lcix, laddr: integer;
                  llc, lcs: addrrange;
            typind: char; (* added for typing [sam] *)
            typ: stp;
      begin lcp := nil; llc := lc;
        with lattr do
          begin symptr := nil; typtr := nil; kind := varbl;
            access := drct; vlevel := level; dplmt := 0; packing := false
          end;
        typind := 'i'; (* default to integer [sam] *)
        if sy = ident then
          begin searchid([vars],lcp);
            with lcp^, lattr do
              begin typtr := idtype; kind := varbl; packing := false;
                if threat or (forcnt > 0) then error(195); forcnt := forcnt+1;
                if part = ptview then error(200);
                if vkind = actual then
                  begin access := drct; vlevel := vlev;
                    if vlev <> level then error(183);
                    dplmt := vaddr
                  end
                else begin error(155); typtr := nil end
              end;
            (* determine type of control variable [sam] *)
            if lattr.typtr = boolptr then typind := 'b'
            else if lattr.typtr = charptr then typind := 'c';
            if lattr.typtr <> nil then
              if (lattr.typtr^.form > subrange)
                 or comptypes(realptr,lattr.typtr) then
                begin error(143); lattr.typtr := nil end;
            insymbol
          end
        else
          begin error(2); skip(fsys + [becomes,tosy,downtosy,dosy]) end;
        if sy = becomes then
          begin insymbol; expression(fsys + [tosy,downtosy,dosy], false);
            typ := basetype(gattr.typtr); { get base type }
            if typ <> nil then
                if typ^.form <> scalar then error(144)
                else
                  if comptypes(lattr.typtr,gattr.typtr) then begin
                    load; alignd(intptr,lc);
                    { store start to temp }
                    gen2t(56(*str*),0,lc-intsize,intptr);
                  end else error(145)
          end
        else
          begin error(51); skip(fsys + [tosy,downtosy,dosy]) end;
        if sy in [tosy,downtosy] then
          begin lsy := sy; insymbol; expression(fsys + [dosy], false);
            typ := basetype(gattr.typtr); { get base type }
            if typ <> nil then
            if typ^.form <> scalar then error(144)
              else
                if comptypes(lattr.typtr,gattr.typtr) then
                  begin
                    load; alignd(intptr,lc);
                    if not comptypes(lattr.typtr,intptr) then
                      gen0t(58(*ord*),gattr.typtr);
                    gen2t(56(*str*),0,lc-intsize*2,intptr);
                    { set initial value of index }
                    gen2t(54(*lod*),0,lc-intsize,intptr);
                    if debug and (lattr.typtr <> nil) then 
                      checkbnds(lattr.typtr);
                    store(lattr);
                    genlabel(laddr); putlabel(laddr);
                    gattr := lattr; load;
                    if not comptypes(gattr.typtr,intptr) then
                      gen0t(58(*ord*),gattr.typtr);
                    gen2t(54(*lod*),0,lc-intsize*2,intptr);
                    lcs := lc;
                    lc := lc - intsize*2;
                    if lc < lcmin then lcmin := lc;
                    if lsy = tosy then gen2(52(*leq*),ord(typind),1)
                    else gen2(48(*geq*),ord(typind),1);
                  end
                else error(145)
          end
        else begin error(55); skip(fsys + [dosy]) end;
        genlabel(lcix); genujpxjp(33(*fjp*),lcix);
        if sy = dosy then insymbol else error(54);
        addlvl;
        statement(fsys);
        sublvl;
        gattr := lattr; load;
        if not comptypes(gattr.typtr,intptr) then
          gen0t(58(*ord*),gattr.typtr);
        gen2t(54(*lod*),0,lcs-intsize*2,intptr);
        gen2(47(*equ*),ord(typind),1);
        genujpxjp(73(*tjp*),lcix);
        gattr := lattr; load;
        if lsy=tosy then gen1t(34(*inc*),1,gattr.typtr)
        else  gen1t(31(*dec*),1,gattr.typtr);
        if debug and (lattr.typtr <> nil) then 
          checkbnds(lattr.typtr);
        store(lattr);
        genujpxjp(57(*ujp*),laddr); putlabel(lcix);
        gattr := lattr; loadaddress; gen0(79(*inv*));
        lc := llc;
        if lcp <> nil then lcp^.forcnt := lcp^.forcnt-1
      end (*forstatement*) ;

      procedure withstatement;
        var lcp: ctp; lcnt1: disprange; llc: addrrange;
            test: boolean;
      begin lcnt1 := 0; llc := lc;
        repeat
          if sy = ident then
            begin searchid([vars,field],lcp); insymbol end
          else begin error(2); lcp := uvarptr end;
          selector(fsys + [comma,dosy],lcp,false);
          if gattr.typtr <> nil then
            if gattr.typtr^.form = records then
              if top < displimit then
                begin top := top + 1; lcnt1 := lcnt1 + 1;
                  with display[top] do
                    begin fname := gattr.typtr^.fstfld;
                      flabel := nil;
                      flabel := nil;
                      fconst := nil;
                      fstruct := nil;
                      packing := gattr.packing;
                      packcom := gattr.packcom;
                      ptrref := gattr.ptrref;
                      modnam := nil
                    end;
                  if gattr.access = drct then
                    with display[top] do
                      begin occur := crec; clev := gattr.vlevel;
                        cdspl := gattr.dplmt
                      end
                  else
                    begin loadaddress;
                      alignd(nilptr,lc);
                      lc := lc-ptrsize;
                      gen2t(56(*str*),0,lc,nilptr);
                      with display[top] do
                        begin occur := vrec; vdspl := lc end;
                      if lc < lcmin then lcmin := lc
                    end
                end
              else error(250)
            else error(140);
          test := sy <> comma;
          if not test then insymbol
        until test;
        if sy = dosy then insymbol else error(54);
        addlvl;
        statement(fsys);
        sublvl;
        { purge display levels }
        while lcnt1 > 0 do begin
           { don't recycle the record context }
           display[top].fname := nil;
           putdsp(display[top]); { purge }
           top := top-1; lcnt1 := lcnt1-1; { count off }
        end;
        lc := llc;
      end (*withstatement*) ;
      
      procedure trystatement;
      var test: boolean; lcp: ctp; lattr: attr;
          endlbl, noexplbl, bgnexplbl, onendlbl,onstalbl: integer;
      begin genlabel(endlbl); genlabel(noexplbl); genlabel(bgnexplbl);
        genujpxjp(84(*bge*),bgnexplbl);
        addlvl;
        repeat
          statement(fsys + [semicolon,onsy,exceptsy,elsesy]);
          if sy in statbegsys then error(14)
        until not(sy in statbegsys);
        while sy = semicolon do
          begin insymbol;
            repeat
              statement(fsys + [semicolon,onsy,exceptsy,elsesy]);
              if sy in statbegsys then error(14);
            until not (sy in statbegsys);
          end;
        sublvl;
        genujpxjp(57(*ujp*),noexplbl);
        putlabel(bgnexplbl);
        if (sy <> onsy) and (sy <> exceptsy) then error(24);
        while sy = onsy do begin insymbol; genlabel(onstalbl); 
          genlabel(onendlbl);
          repeat
            if sy = ident then begin
              searchid([vars],lcp); 
              with lcp^, lattr do
                begin typtr := idtype; kind := varbl; packing := false;
                  if threat or (forcnt > 0) then error(195); forcnt := forcnt+1;
                  if part = ptview then error(200);
                  if vkind = actual then
                    begin access := drct; vlevel := vlev;
                      if vlev <> level then error(183);
                      dplmt := vaddr
                    end
                  else begin error(155); typtr := nil end
                end;                
              if lcp^.idtype <> nil then 
                if lcp^.idtype^.form <> exceptf then error(226);
              insymbol;
              gen0t(76(*dup*),nilptr);{ make copy of original vector }
              gattr := lattr; loadaddress; { load compare vector }
              gen2(47(*equ*),ord('a'),lsize);
              genujpxjp(73(*tjp*),onstalbl);
            end else begin error(2); skip(fsys+[onsy,exceptsy,elsesy]) end;
            test := sy <> comma;
            if not test then insymbol
          until test;
          genujpxjp(57(*ujp*),onendlbl);
          if sy = exceptsy then insymbol else 
            begin error(23); skip(fsys+[onsy,exceptsy,elsesy]) end;
          putlabel(onstalbl);
          addlvl;
          statement(fsys+[exceptsy]);
          sublvl;
          genujpxjp(57(*ujp*),endlbl);
          putlabel(onendlbl)
        end;
        if sy = exceptsy then begin addlvl; 
          insymbol; statement(fsys+[elsesy]); sublvl; 
          genujpxjp(57(*ujp*),endlbl)
        end;
        gen0(86(*mse*));
        putlabel(noexplbl);
        if sy = elsesy then begin addlvl;
          insymbol; statement(fsys); sublvl 
        end;
        sublvl;
        putlabel(endlbl);
        gen0(85(*ede*))
      end (*trystatement*) ;

    begin (*statement*)
      if (sy = intconst) or (sy = ident) then begin (*label*)
          { and here is why Wirth didn't include symbolic labels in Pascal.
            We are ambiguous with assigns and calls, so must look ahead for 
            the ':' }
        searchlabel(llp, level, sy = ident); { search label }
        insymbol; { look ahead }
        if sy = colon then begin { process as label }
           insymbol; { skip ':' }
           if llp <> nil then with llp^ do begin { found }
             if defined then error(165); { multidefined label }
             bact := true; { set in active block now }
             slevel := stalvl; { establish statement level }
             defined := true; { set defined }
             if ipcref and (stalvl > 1) then
               error(184) { intraprocedure goto does not reference outter block }
             else if minlvl < stalvl then
               { Label referenced by goto at lesser statement level or 
                 differently nested statement }
               error(186);
             putlabel(labname) { output label to intermediate }
           end else begin { not found }
             error(167); { undeclared label }
             newlabel(llp, false) { create a dummy level }
           end
        end else pushback { back to ident }
      end;
      if not (sy in fsys + statbegsys + [ident,resultsy,inheritedsy]) then
        begin error(6); skip(fsys) end;
      inherit := false;
      if sy in statbegsys + [ident,resultsy,inheritedsy] then
        begin
          case sy of
            inheritedsy,
            ident:    begin 
                        if sy = inheritedsy then 
                          begin insymbol; inherit := true end;
                        searchid([vars,field,func,proc],lcp); insymbol;
                        if lcp^.klass = proc then call(fsys,lcp,inherit)
                        else begin if inherit then error(233);
                          assignment(lcp, false)
                        end
                      end;
            beginsy:  begin insymbol; compoundstatement end;
            gotosy:   begin insymbol; gotostatement end;
            ifsy:     begin insymbol; ifstatement end;
            casesy:   begin insymbol; casestatement end;
            whilesy:  begin insymbol; whilestatement end;
            repeatsy: begin insymbol; repeatstatement end;
            forsy:    begin insymbol; forstatement end;
            withsy:   begin insymbol; withstatement end;
            trysy:    begin insymbol; trystatement end;
            { process result as a pseudostatement }
            resultsy: begin
              if fprocp <> nil then if fprocp^.klass <> func then error(210)
              else begin
                if fprocp^.asgn then error(212);
                fprocp^.asgn := true
              end;
              assignment(fprocp, true);
              if not (sy = endsy) or (stalvl > 1) then error(211)
            end 
          end;
          if not (sy in [semicolon,endsy,elsesy,untilsy,exceptsy,onsy]) then
            begin error(6); skip(fsys) end
        end
    end (*statement*) ;

    { validate and start external header files }
    procedure externalheader;
    var valp: csp; saveid: idstr; llcp:ctp;
    begin
      saveid := id;
      while fextfilep <> nil do begin
        with fextfilep^ do begin
          id := filename;
          searchidne([vars],llcp);
          if llcp = nil then begin
            { a header file was never defined in a var statement }
            writeln(output);
            writeln('*** Error: Undeclared external file ''',
                           fextfilep^.filename:8, '''');
            toterr := toterr+1;
            llcp := uvarptr
          end;
          if llcp^.idtype<>nil then
            if (llcp^.idtype^.form<>files) and (llcp^.idtype <> intptr) and
               (llcp^.idtype <> realptr) then
              begin writeln(output);
                writeln('*** Error: Undeclared external file ''',
                               fextfilep^.filename:8, '''');
                toterr := toterr+1
              end
          else begin { process header file }
            llcp^.hdr := true; { appears in header }
            { check is a standard header file }
            if not (strequri('input    ', filename) or
                    strequri('output   ', filename) or
                    strequri('prd      ', filename) or
                    strequri('prr      ', filename) or
                    strequri('error    ', filename) or
                    strequri('list     ', filename) or
                    strequri('command  ', filename)) then begin 
              gen1(37(*lao*),llcp^.vaddr); { load file/variable address }
              { put name in constants table }
              new(valp,strg); valp^.cclass := strg;
              valp^.slgth := lenpv(llcp^.name); 
              valp^.sval := llcp^.name;
              if cstptrix >= cstoccmax then error(254)
              else begin cstptrix := cstptrix + 1;
                cstptr[cstptrix] := valp;
                gen1(38(*lca*),cstptrix)
              end;
              cstptrix := cstptrix - 1;
              { load length of name }
              gen2(51(*ldc*),1,valp^.slgth);
              if llcp^.idtype = intptr then { integer }
                gen1(30(*csp*),83(*rdie*))
              else if llcp^.idtype = realptr then { real }
                gen1(30(*csp*),84(*rdir*))
              else if llcp^.idtype = textptr then { text }
                gen1(30(*csp*),81(*aeft*))
              else { binary }
                gen1(30(*csp*),82(*aefb*));
              dispose(valp,strg)
            end
          end
        end;
        fp := fextfilep; fextfilep := fextfilep^.nextfile; putfil(fp)
      end;
      id := saveid
    end;
    
    procedure initvirt;
    procedure schvirt(lcp: ctp);
    var lcp1,lcp2: ctp;
    begin
      if lcp <> nil then begin
        if lcp^.klass in [proc,func] then begin
          if not chkext(lcp) then begin
            if (lcp^.pfattr = fpavirtual) then
              gensuv(lcp^.pfname,lcp^.pfvaddr,lcp)
            else if lcp^.pfattr = fpaoverride then begin
              lcp1 := lcp^.grppar; { link parent }
              lcp2 := lcp1^.pfvid; { get vector symbol }
              { copy old vector to store }
              gen1ts(39(*ldo*),lcp2^.vaddr,lcp2^.idtype,lcp2);
              gen1t(43(*sro*),lcp^.pfvaddr,nilptr);
              { place new vector }
              gensuv(lcp^.pfname,lcp2^.pfvaddr,lcp2);
            end
          end;
          schvirt(lcp^.grpnxt);
        end;
        schvirt(lcp^.llink); schvirt(lcp^.rlink)
      end
    end;
    begin
      schvirt(display[top].fname)
    end;
          
  begin (*body*)
    stalvl := 0; { clear statement nesting level }
    cstptrix := 0; topnew := 0; topmin := 0;
    { if processing procedure/function, use that entry label, otherwise set 
      at program }
    if fprocp <> nil then putlabel(fprocp^.pfname) else putlabel(entname);
    genlabel(segsize); genlabel(stackbot); 
    genlabel(gblsize);
    gencupent(32(*ents*),1,segsize,fprocp); 
    gencupent(32(*ente*),2,stackbot,fprocp);
    if fprocp <> nil then (*copy multiple values into local cells*)
      begin llc1 := lcaftermarkstack;
        lcp := fprocp^.pflist;
        while lcp <> nil do
          with lcp^ do
            begin
              if klass = vars then
                if idtype <> nil then
                  if idtype^.form > power then
                    begin
                      llc1 := llc1 - ptrsize;
                      alignd(parmptr,llc1);
                      if vkind = actual then
                        begin
                          gen2(50(*lda*),0,vaddr);
                          gen2t(54(*lod*),0,llc1,nilptr);
                          gen1(40(*mov*),idtype^.size);
                        end
                    end
                  else 
                    begin
                      llc1 := llc1 - idtype^.size;
                      alignd(parmptr,llc1);
                    end;
              lcp := lcp^.next;
            end;
      end;
    lcmin := lc;
    addlvl;
    if level = 1 then begin { perform module setup tasks }
      externalheader; { process external header files }
      initvirt { process virtual procedure/function sets }
    end;
    if sy = beginsy then insymbol else error(17);
    repeat
      repeat statement(fsys + [semicolon,endsy])
      until not (sy in statbegsys);
      test := sy <> semicolon;
      if not test then insymbol
    until test;
    sublvl;
    if sy = endsy then insymbol else error(13);
    llp := display[top].flabel; (*test for undefined and unreferenced labels*)
    while llp <> nil do
      with llp^ do
        begin
          if not defined or not refer then
            begin if not defined then error(168);
              writeln(output); write('label ',labval:11);
              if not refer then write(' unreferenced');
              writeln;
              write(' ':chcnt+16)
            end;
          llp := nextlab
        end;
    printed := false; 
    if (fprocp <> nil) or iso7185 then chkrefs(display[top].fname, printed);
    if toterr = 0 then
      if (topnew <> 0) and prcode then 
        error(504); { stack should have wound to zero }
    if fprocp <> nil then
      begin
        if fprocp^.idtype = nil then gen1(42(*ret*),ord('p'))
        else gen0t(42(*ret*),basetype(fprocp^.idtype));
        alignd(parmptr,lcmin);
        if prcode then
        begin prtlabel(segsize); writeln(prr,'=',lcmin:1);
           prtlabel(stackbot); writeln(prr,'=',topmin:1)
          end
      end
    else
      begin gen1(42(*ret*),ord('p'));
        alignd(parmptr,lcmin);
        if prcode then
        begin
            prtlabel(segsize); writeln(prr,'=',lcmin:1);
            prtlabel(stackbot); writeln(prr,'=',topmin:1)
          end;
        ic := 0;
        if prtables then
          begin writeln(output); printtables(true)
          end
      end;
  end (*body*) ;

  procedure openinput(var ff: boolean);
  var fp: filptr; i, x: integer; es: packed array [1..4] of char;
  begin ff := true; es := extsrc;
    { have not previously parsed this module }
    new(fp); 
    with fp^ do begin
      next := incstk; incstk := fp; strassvf(mn, id); priv := false;
      fn := id; i := fillen; while (i > 1) and (fn[i] = ' ') do i := i-1;
      if i > fillen-4-1 then error(265);
      for x := 1 to 4 do begin i := i+1; fn[i] := es[x] end;
      if not existsfile(fn) then begin
        error(264);
        incstk := incstk^.next;
        dispose(fp);
        ff := false 
      end else begin assigntext(f, fn); reset(f) end
    end
  end;
    
  procedure closeinput;
  var fp: filptr;
  begin
    if incstk = nil then error(505);
    closetext(incstk^.f);
    { remove top include entry }
    fp := incstk; incstk := incstk^.next;
    fp^.next := inclst; { put on discard list }
    inclst := fp
  end;
  
  procedure putinp(var fl: filptr);
  var fp: filptr;
  begin
    while fl <> nil do begin
      fp := fl; fl := fl^.next; putstrs(fp^.mn); dispose(fp)
    end
  end;
  
  procedure cancelfwd(fcp: ctp);
  begin
    if fcp <> nil then begin
      if fcp^.klass in [proc, func] then fcp^.forwdecl := false;
      cancelfwd(fcp^.llink); cancelfwd(fcp^.rlink)
    end
  end; 
    
  procedure modulep(fsys:setofsys); forward;
    
  procedure usesjoins;
  var sys: symbol; prcodes: boolean; ff: boolean; chs: char; eols: boolean;
      lists: boolean; nammods, modnams, thismod: strvsp; gcs: addrrange; 
      curmods: modtyp; entnames: integer; sym: symbol;
  function schnam(fp: filptr): boolean;
  begin schnam := false;
    while fp <> nil do 
      begin if fp^.fn = id then schnam := true; fp := fp^.next end
  end;
  begin
    sym := sy; insymbol; { skip uses/joins }
    repeat { modules }
      if sy <> ident then error(2) else begin
        if not schnam(incstk) and not schnam(inclst) then begin 
          chs := ch; eols := eol; prcodes := prcode; lists := list; gcs := gc; 
          nammods := nammod; curmods := curmod; entnames := entname;
          openinput(ff);
          if ff then begin
            ch := ' '; eol := true; prcode := false; list := false;
            insymbol; modnams := display[top].modnam; 
            display[top].modnam := nil;
            modulep(blockbegsys+statbegsys-[casesy]);
            thismod := display[top].modnam; display[top].modnam := modnams; 
            cancelfwd(display[top].fname); closeinput
          end;
          ch := chs; eol := eols;prcode := prcodes; list := lists; gc := gcs; 
          nammod := nammods; curmod := curmods; entname := entnames
        end;
        insymbol; { skip id }
        if sym = joinssy then begin { post process joins level }
          if ptop >= displimit then error(267)
          else begin
            pile[ptop] := display[top]; { copy out definitions from display }
            pile[ptop].modnam := thismod; { put back module name }
            ptop := ptop+1;
            { clear display for next }
            with display[top] do
              begin fname := nil; flabel := nil; fconst := nil; fstruct := nil;
                    packing := false; packcom := false; ptrref := false; 
                    occur := blck; bname := nil end
          end
        end else putstrs(thismod)
      end;
      sys := sy;
      if sy = comma then insymbol
    until sys <> comma;
    if sy = semicolon then insymbol else error(14)
  end;
    
  procedure modulep{(fsys:setofsys)};
    var extfp:extfilep; segsize: integer;
  begin
    cstptrix := 0; topnew := 0; topmin := 0; nammod := nil; genlabel(entname); 
    genlabel(extname); genlabel(nxtname);
    chkudtf := chkudtc; { finalize undefined tag checking flag }
    { set type of module parsing }
    curmod := mtprogram;
    if sy = modulesy then curmod := mtmodule;
    if (sy = progsy) or (sy = modulesy) then
      begin insymbol; 
        if sy <> ident then error(2) else begin
          strassvf(nammod, id); { place module name }
          strassvf(display[top].modnam, id);
          if prcode then begin
            writeln(prr, 'i');
            if curmod = mtprogram then 
              begin write(prr, 'i Program '); writevp(prr, nammod); writeln(prr) end
            else 
              begin write(prr, 'i Module '); writevp(prr, nammod); writeln(prr) end;
            writeln(prr, 'i');
            if curmod = mtmodule then 
              writeln(prr, 'b m ', id:kk) { mark module block start }
            else
              writeln(prr, 'b p ', id:kk) { mark program block start }
          end;
          insymbol;
          { mark stack, generate call to startup block }
          gen1(41(*mst*),0); gencupent(46(*cup*),0,entname,nil);
          if curmod = mtmodule then begin
            { for module we need call next in module stack, then call exit
              module }
            genujpxjp(89(*cal*),nxtname); 
            gen1(41(*mst*),0); gencupent(46(*cup*),0,extname,nil)
          end;
          gen0(90(*ret*)) { return last module stack }
        end;
        if not (sy in [lparent,semicolon]) then error(14);
        if sy = lparent  then
          begin
            repeat insymbol;
              if sy = ident then
                begin getfil(extfp);
                  with extfp^ do
                    begin filename := id; nextfile := fextfilep end;
                  fextfilep := extfp;
                  { check 'input' or 'output' appears in header for defaults }
                  if strequri('input    ', id) then inputptr^.hdr := true
                  else if strequri('output   ', id) then outputptr^.hdr := true
                  else if strequri('prd      ', id) then prdptr^.hdr := true
                  else if strequri('prr      ', id) then prrptr^.hdr := true
                  else if strequri('error    ', id) then errorptr^.hdr := true
                  else if strequri('list     ', id) then listptr^.hdr := true
                  else if strequri('command  ', id) then commandptr^.hdr := true;
                  insymbol;
                  if not ( sy in [comma,rparent] ) then error(20)
                end
              else error(2)
            until sy <> comma;
            if sy <> rparent then error(4);
            insymbol;
            if sy <> semicolon then error(14)
          end;
        if sy = semicolon then insymbol
      end else error(3);
    { must process joins first so that the module (1) display level is clean.
      Otherwise this could create a situation where joins rely on other 
      modules }
    if sy = joinssy then usesjoins; { process joins }
    if sy = usessy then usesjoins; { process uses }
    declare(fsys);
    if not inpriv then body(fsys,nil);
    if curmod = mtmodule then begin
      if sy = semicolon then begin
        insymbol;
        if sy <> beginsy then error(17)
      end;
      if sy = beginsy then begin
        { gen exit block }
        entname := extname; body(fsys, nil);
      end else begin { generate dummy terminator block }
        genlabel(segsize); genlabel(stackbot); putlabel(extname);
        gencupent(32(*ents*),1,segsize,nil); 
        gencupent(32(*ente*),2,stackbot,nil);
        gen1(42(*ret*),ord('p'));
        if prcode then begin
          prtlabel(segsize); writeln(prr,'=',0:1);
          prtlabel(stackbot); writeln(prr,'=',0:1)
        end
      end;
      if prcode then begin
        putlabel(nxtname); { set skip module stack }
        writeln(prr,'g ',gc:1);
        writeln(prr, 'e m') { mark module block end }
      end
    end else begin { program }
      if prcode then begin
        writeln(prr,'g ',gc:1);
        writeln(prr, 'e p') { mark program block end }
      end
    end;
    if (sy <> period) and not inpriv then begin error(21); skip([period]) end;
    if prcode then begin
      writeln(prr, 'f ', toterr:1);
      { only terminate intermediate if we are a cap cell (program) }
      if curmod = mtprogram then writeln(prr,'q')
    end;
    if list then writeln;
    if errinx <> 0 then
      begin list := false; endofline end;
    putstrs(nammod) { release module name }
  end (*modulep*) ;

  procedure stdnames;
  begin
    { 'mark' and 'release' were removed and replaced with placeholders }
    na[ 1] := 'false    '; na[ 2] := 'true     '; na[ 3] := 'input    ';
    na[ 4] := 'output   '; na[ 5] := 'get      '; na[ 6] := 'put      ';
    na[ 7] := 'reset    '; na[ 8] := 'rewrite  '; na[ 9] := 'read     ';
    na[10] := 'write    '; na[11] := 'pack     '; na[12] := 'unpack   ';
    na[13] := 'new      '; na[14] := 'assign   '; na[15] := 'readln   ';
    na[16] := 'writeln  '; na[17] := 'abs      '; na[18] := 'sqr      '; 
    na[19] := 'trunc    '; na[20] := 'odd      '; na[21] := 'ord      '; 
    na[22] := 'chr      '; na[23] := 'pred     '; na[24] := 'succ     '; 
    na[25] := 'eof      '; na[26] := 'eoln     '; na[27] := 'sin      '; 
    na[28] := 'cos      '; na[29] := 'exp      '; na[30] := 'sqrt     '; 
    na[31] := 'ln       '; na[32] := 'arctan   '; na[33] := 'prd      '; 
    na[34] := 'prr      '; na[35] := 'close    '; na[36] := 'maxint   '; 
    na[37] := 'round    '; na[38] := 'page     '; na[39] := 'dispose  ';
    na[40] := 'length   '; na[41] := 'location '; na[42] := 'position ';
    na[43] := 'update   '; na[44] := 'append   '; na[45] := 'exists   ';
    na[46] := 'delete   '; na[47] := 'change   '; na[48] := 'error    ';
    na[49] := 'list     '; na[50] := 'command  '; na[51] := 'halt     ';
    na[52] := 'linteger '; na[53] := 'maxlint  '; na[54] := 'cardinal ';
    na[55] := 'maxcrd   '; na[56] := 'lcardinal'; na[57] := 'maxlcrd  ';
    na[58] := 'sreal    '; na[59] := 'lreal    '; na[60] := 'maxreal  ';
    na[61] := 'maxsreal '; na[62] := 'maxlreal '; na[63] := 'integer  ';
    na[64] := 'real     '; na[65] := 'char     '; na[66] := 'boolean  ';
    na[67] := 'text     '; na[68] := 'maxchr   '; na[69] := 'assert   ';
    na[70] := 'error    '; na[71] := 'list     '; na[72] := 'command  ';
    na[73] := 'exception'; na[74] := 'throw    ';
    


  end (*stdnames*) ;

  procedure enterstdtypes;

  begin                                                 (*type underlying:*)
                                                        (******************)

    new(intptr,scalar,standard); pshstc(intptr);               (*integer*)
    with intptr^ do
      begin size := intsize; form := scalar; scalkind := standard; 
            packing := false end;
    new(crdptr,scalar,standard); pshstc(crdptr);               (*cardinal*)
    with crdptr^ do
      begin size := intsize; form := subrange; rangetype := intptr; 
            min.intval := true; min.ival := 0; 
            max.intval := true; max.ival := maxint; packing := false end;
    new(realptr,scalar,standard); pshstc(realptr);             (*real*)
    with realptr^ do
      begin size := realsize; form := scalar; scalkind := standard; 
            packing := false end;
    new(charptr,scalar,standard); pshstc(charptr);             (*char*)
    with charptr^ do
      begin size := charsize; form := scalar; scalkind := standard;
            packing := false end;
    new(boolptr,scalar,declared); pshstc(boolptr);             (*boolean*)
    with boolptr^ do
      begin size := boolsize; form := scalar; scalkind := declared;
            packing := false end;
    new(nilptr,pointer); pshstc(nilptr);                       (*nil*)
    with nilptr^ do
      begin eltype := nil; size := ptrsize; form := pointer;
            packing := false end;
    (*for alignment of parameters*)
    new(parmptr,scalar,standard); pshstc(parmptr);
    with parmptr^ do
      begin size := parmsize; form := scalar; scalkind := standard;
            packing := false end ;
    new(textptr,files); pshstc(textptr);                       (*text*)
    with textptr^ do
      begin filtype := charptr; size := filesize+charsize; form := files;
            packing := false end;
    new(exceptptr,exceptf); pshstc(exceptptr);                 (*exception*)
    with exceptptr^ do
      begin size := exceptsize; form := exceptf; packing := false end;
    
  end (*enterstdtypes*) ;

  procedure entstdnames;
    var cp,cp1: ctp; i: integer;

  procedure entstdprocfunc(idc: idclass; sn: stdrng; kn: keyrng; idt: stp);
  begin
    if idc = proc then new(cp,proc,standard)
    else new(cp,func,standard);
    ininam(cp);
    with cp^ do
      begin strassvr(name, na[sn]); idtype := idt;
        pflist := nil; next := nil; key := kn;
        klass := idc; pfdeckind := standard; pfaddr := 0; pext := false;
        pmod := nil; pfattr := fpanone; grpnxt := nil; grppar := nil;
        pfvid := nil; pflist := nil
      end; enterid(cp)
  end;
  
  procedure entstdtyp(sn: stdrng; idt: stp);
  begin
    new(cp,types); ininam(cp);
    with cp^ do
      begin strassvr(name, na[sn]); idtype := idt; klass := types end;
    enterid(cp)
  end;
  
  procedure entstdintcst(sn: stdrng; idt: stp; i: integer);
  begin
    new(cp,konst); ininam(cp);
    with cp^ do
      begin strassvr(name, na[sn]); idtype := idt; next := nil; 
        values.intval := true; values.ival := i; klass := konst end;
    enterid(cp)
  end;
  
  procedure entstdrlcst(sn: stdrng; idt: stp; r: real);
  var lvp: csp;
  begin
    new(cp,konst); ininam(cp); new(lvp,reel); pshcst(lvp); lvp^.cclass := reel;
    lvp^.rval := r;
    with cp^ do
      begin strassvr(name, na[sn]); idtype := idt; next := nil; 
        values.valp := lvp; klass := konst end;
    enterid(cp)
  end;
  
  procedure entstdhdr(sn: stdrng);
  begin
    new(cp,vars); ininam(cp); 
    with cp^ do
    begin strassvr(name, na[sn]); idtype := textptr; klass := vars;
      vkind := actual; next := nil; vlev := 1;
      vaddr := gc; gc := gc+filesize+charsize; { files are global now }
      threat := false; forcnt := 0; part := ptval; hdr := false; vext := false; 
      vmod := nil
    end;
    enterid(cp)
  end;
  
  procedure entstdexp(en: expstr);
  begin
    new(cp,vars); ininam(cp); 
    with cp^ do
    begin strassve(name, en); idtype := exceptptr; klass := vars;
      vkind := actual; next := nil; vlev := 1;
      vaddr := gc; gc := gc+exceptsize;
      threat := false; forcnt := 0; part := ptval; hdr := false; vext := false; 
      vmod := nil
    end;
    enterid(cp)
  end;
      
  begin                                                       (*name:*)
                                                              (*******)

    entstdtyp(63, intptr);                                    (*integer*)
    entstdtyp(52, intptr);                                    (*linteger*)
    entstdtyp(54, crdptr);                                    (*cardinal*)
    entstdtyp(56, crdptr);                                    (*lcardinal*)
    entstdtyp(64, realptr);                                   (*real*)
    entstdtyp(58, realptr);                                   (*sreal*)
    entstdtyp(59, realptr);                                   (*lreal*)
    entstdtyp(65, charptr);                                   (*char*)
    entstdtyp(66, boolptr);                                   (*boolean*)
    usclrptr := cp; { save to satisfy broken tags }
    entstdtyp(67, textptr);                                   (*text*)
    entstdtyp(73, exceptptr);                                 (*exception*)

    cp1 := nil;
    for i := 1 to 2 do
      begin new(cp,konst); ininam(cp);                        (*false,true*)
        with cp^ do
          begin strassvr(name, na[i]); idtype := boolptr;
            next := cp1; values.intval := true; values.ival := i - 1; 
            klass := konst
          end;
        enterid(cp); cp1 := cp
      end;
    boolptr^.fconst := cp;


    entstdhdr(3); inputptr := cp;                             (*input*)
    entstdhdr(4); outputptr := cp;                            (*output*)
    entstdhdr(33); prdptr := cp;                              (*prd*)
    entstdhdr(34); prrptr := cp;                              (*prr*)
    entstdhdr(70); errorptr := cp;                            (*error*)
    entstdhdr(71); listptr := cp;                             (*list*)
    entstdhdr(72); commandptr := cp;                          (*command*)
    
    for i := 27 to 32 do
      begin
        new(cp,vars); ininam(cp);                                (*parameter of predeclared functions*)
        with cp^ do
          begin strassvr(name, '         '); idtype := realptr; klass := vars;
            vkind := actual; next := nil; vlev := 1; vaddr := 0;
            threat := false; forcnt := 0; part := ptval; hdr := false; 
            vext := false; vmod := nil
          end;
        new(cp1,func,declared,actual); ininam(cp1);            (*sin,cos,exp*)
        with cp1^ do                                           (*sqrt,ln,arctan*)
          begin strassvr(name, na[i]); idtype := realptr; pflist := cp;
            forwdecl := false; externl := true; pflev := 0; pfname := i - 12;
            klass := func; pfdeckind := declared; pfkind := actual;
            pfaddr := 0; pext := false; pmod := nil; pfattr := fpanone; 
            grpnxt := nil; grppar := nil; pfvid := nil            
          end;
        enterid(cp1)
      end;

    entstdintcst(36, intptr, maxint);                          (*maxint*)
    entstdintcst(53, intptr, maxint);                          (*maxlint*)
    entstdintcst(55, crdptr, maxint);                          (*maxcrd*)
    entstdintcst(57, crdptr, maxint);                          (*maxlcrd*)
    entstdintcst(68, charptr, ordmaxchar);                     (*maxlcrd*)
    entstdrlcst(60, realptr, 1.797693134862314e308);           (*maxreal*)
    entstdrlcst(61, realptr, 1.797693134862314e308);           (*maxsreal*)
    entstdrlcst(62, realptr, 1.797693134862314e308);           (*maxlreal*)
    
    entstdprocfunc(proc, 5,  1,  nil);     { get }
    entstdprocfunc(proc, 6,  2,  nil);     { put }
    entstdprocfunc(proc, 7,  3,  nil);     { reset }
    entstdprocfunc(proc, 8,  4,  nil);     { rewrite }
    entstdprocfunc(proc, 9,  5,  nil);     { read }
    entstdprocfunc(proc, 10, 6,  nil);     { write }
    entstdprocfunc(proc, 11, 7,  nil);     { pack }
    entstdprocfunc(proc, 12, 8,  nil);     { unpack }
    entstdprocfunc(proc, 13, 9,  nil);     { new }
    entstdprocfunc(proc, 15, 11, nil);     { readln }
    entstdprocfunc(proc, 16, 12, nil);     { writeln }
    entstdprocfunc(func, 17, 1,  nil);     { abs }
    entstdprocfunc(func, 18, 2,  nil);     { sqr }
    entstdprocfunc(func, 19, 3,  nil);     { trunc }
    entstdprocfunc(func, 20, 4,  nil);     { odd }
    entstdprocfunc(func, 21, 5,  nil);     { ord }
    entstdprocfunc(func, 22, 6,  nil);     { chr }
    entstdprocfunc(func, 23, 7,  nil);     { pred }
    entstdprocfunc(func, 24, 8,  nil);     { succ }
    entstdprocfunc(func, 25, 9,  nil);     { eof }
    entstdprocfunc(func, 26, 10, nil);     { eoln }
    entstdprocfunc(func, 37, 16, nil);     { round }
    entstdprocfunc(proc, 38, 17, nil);     { page }    
    entstdprocfunc(proc, 39, 18, nil);     { dispose }
    { Note: I was to lazy to overload the keys on these }
    entstdprocfunc(proc, 14, 19, nil);     { assign }
    entstdprocfunc(proc, 35, 20, nil);     { close }
    entstdprocfunc(func, 40, 21, intptr);  { length }
    entstdprocfunc(func, 41, 22, intptr);  { location }
    entstdprocfunc(proc, 42, 23, nil);     { position }
    entstdprocfunc(proc, 43, 24, nil);     { update }
    entstdprocfunc(proc, 44, 25, nil);     { append }
    entstdprocfunc(func, 45, 26, boolptr); { exists }
    entstdprocfunc(proc, 46, 27, nil);     { delete }
    entstdprocfunc(proc, 47, 28, nil);     { change }
    entstdprocfunc(proc, 51, 29, nil);     { halt }
    entstdprocfunc(proc, 69, 30, nil);     { assert }
    entstdprocfunc(proc, 74, 31, nil);     { throw }
    
    { standard exceptions }
    entstdexp('ValueOutOfRange                 ');
    entstdexp('ArrayLengthMatch                ');
    entstdexp('CaseValueNotFound               ');
    entstdexp('ZeroDivide                      ');
    entstdexp('InvalidOperand                  ');
    entstdexp('NilPointerDereference           ');
    entstdexp('RealOverflow                    ');
    entstdexp('RealUnderflow                   ');
    entstdexp('RealProcessingFault             ');
    entstdexp('TagValueNotActive               ');
    entstdexp('TooManyFiles                    ');
    entstdexp('FileIsOpen                      ');
    entstdexp('FileAlreadyNamed                ');
    entstdexp('FileNotOpen                     ');
    entstdexp('FileModeIncorrect               ');
    entstdexp('InvalidFieldSpecification       ');
    entstdexp('InvalidRealNumber               ');
    entstdexp('InvalidFractionSpecification    ');
    entstdexp('InvalidIntegerFormat            ');
    entstdexp('IntegerValueOverflow            ');
    entstdexp('InvalidRealFormat               ');
    entstdexp('EndOfFile                       ');
    entstdexp('InvalidFilePosition             ');
    entstdexp('FilenameTooLong                 ');
    entstdexp('FileOpenFail                    ');
    entstdexp('FileSIzeFail                    ');
    entstdexp('FileCloseFail                   ');
    entstdexp('FileReadFail                    ');
    entstdexp('FileWriteFail                   ');
    entstdexp('FilePositionFail                ');
    entstdexp('FileDeleteFail                  ');
    entstdexp('FileNameChangeFail              ');
    entstdexp('SpaceAllocateFail               ');
    entstdexp('SpaceReleaseFail                ');
    entstdexp('SpaceAllocateNegative           ');
    entstdexp('CannotPerformSpecial            ');
    entstdexp('CommandLineTooLong              ');
    entstdexp('ReadPastEOF                     ');
    entstdexp('FileTransferLengthZero          ');
    entstdexp('FileSizeTooLarge                ');
    entstdexp('FilenameEmpty                   ');
    entstdexp('CannotOpenStandard              ');
    entstdexp('TooManyTemporaryFiles           ');
    entstdexp('InputBufferOverflow             ');
    entstdexp('TooManyThreads                  ');
    entstdexp('CannotStartThread               ');
    entstdexp('InvalidThreadHandle             ');
    entstdexp('CannotStopThread                ');
    entstdexp('TooManyIntertaskLocks           ');
    entstdexp('InvalidLockHandle               ');
    entstdexp('LockSequenceFail                ');
    entstdexp('TooManySignals                  ');
    entstdexp('CannotCreateSignal              ');
    entstdexp('InvalidSignalHandle             ');
    entstdexp('CannotDeleteSignal              ');
    entstdexp('CannotSendSignal                ');
    entstdexp('WaitForSignalFail               ');
    entstdexp('FieldNotBlank                   ');
    entstdexp('ReadOnWriteOnlyFile             ');
    entstdexp('WriteOnReadOnlyFile             ');
    entstdexp('FileBufferVariableUndefined     ');
    entstdexp('NondecimalRadixOfNegative       ');
    entstdexp('InvalidArgumentToLn             ');  
    entstdexp('InvalidArgumentToSqrt           ');  
    entstdexp('CannotResetOrRewriteStandardFile');  
    entstdexp('CannotResetWriteOnlyFile        ');  
    entstdexp('CannotRewriteReadOnlyFile       ');  
    entstdexp('SetElementOutOfRange            ');  
    entstdexp('RealArgumentTooLarge            ');  
    entstdexp('BooleanOperatorOfNegative       ');  
    entstdexp('InvalidDivisorToMod             ');  
    entstdexp('PackElementsOutOfBounds         ');  
    entstdexp('UnpackElementsOutOfBounds       '); 
    entstdexp('CannotResetClosedTempFile       '); 
  end (*entstdnames*) ;

  procedure enterundecl;
  begin
    new(utypptr,types); ininam(utypptr);
    with utypptr^ do
      begin strassvr(name, '         '); idtype := nil; klass := types end;
    new(ucstptr,konst); ininam(ucstptr);
    with ucstptr^ do
      begin strassvr(name, '         '); idtype := nil; next := nil;
        klass := konst; values.intval := true; values.ival := 0
      end;
    new(uvarptr,vars); ininam(uvarptr);
    with uvarptr^ do
      begin strassvr(name, '         '); idtype := nil; vkind := actual;
        next := nil; vlev := 0; vaddr := 0; klass := vars;
        threat := false; forcnt := 0; part := ptval; hdr := false; 
        vext := false; vmod := nil
      end;
    new(ufldptr,field); ininam(ufldptr);
    with ufldptr^ do
      begin strassvr(name, '         '); idtype := nil; next := nil; fldaddr := 0;
        klass := field
      end;
    new(uprcptr,proc,declared,actual); ininam(uprcptr);
    with uprcptr^ do
      begin strassvr(name, '         '); idtype := nil; forwdecl := false;
        next := nil; externl := false; pflev := 0; genlabel(pfname);
        klass := proc; pflist := nil; pfdeckind := declared; pfkind := actual;
        pmod := nil; grpnxt := nil; grppar := nil; pfvid := nil
      end;
    new(ufctptr,func,declared,actual); ininam(ufctptr);
    with ufctptr^ do
      begin strassvr(name, '         '); idtype := nil; next := nil;
        forwdecl := false; externl := false; pflev := 0; genlabel(pfname);
        klass := func; pflist := nil; pfdeckind := declared; pfkind := actual;
        pmod := nil; grpnxt := nil; grppar := nil; pfvid := nil
      end
  end (*enterundecl*) ;

  { tear down storage allocations from enterundecl }
  procedure exitundecl;
  begin
    putnam(utypptr);
    putnam(ucstptr);
    putnam(uvarptr);
    putnam(ufldptr);
    putnam(uprcptr);
    putnam(ufctptr);
  end (*exitundecl*) ;

  procedure initscalars;
  var i: integer; c: char;
  begin fwptr := nil; for c := 'a' to 'z' do option[c] := false;
    prtables := false; option['t'] := false; list := true; option['l'] := true;
    prcode := true; option['c'] := true; debug := true; option['d'] := true;
    chkvar := true; option['v'] := true; chkref := true; option['r'] := true;
    chkudtc := true; option['u'] := true; option['s'] := false; iso7185 := false;
    dodmplex := false; doprtryc := false; doprtlab := false; dodmpdsp := false;
    dp := true; errinx := 0;
    intlabel := 0; kk := maxids; fextfilep := nil;
    lc := lcaftermarkstack; gc := 0;
    (* note in the above reservation of buffer store for 2 text files *)
    ic := 3; eol := true; linecount := 0; lineout := 0; 
    incstk := nil; inclst := nil;
    ch := ' '; chcnt := 0;
    mxint10 := maxint div 10;
    maxpow10 := 1; while maxpow10 < mxint10 do maxpow10 := maxpow10*10;
    
    for i := 1 to 500 do errtbl[i] := false; { initialize error tracking }
    toterr := 0; { clear error count }
    { clear the recycling tracking counters }
    strcnt := 0; { strings }
    cspcnt := 0; { constants }
    stpcnt := 0; { structures }
    ctpcnt := 0; { identifiers }
    lbpcnt := 0; { label counts }
    filcnt := 0; { file tracking counts }
    cipcnt := 0; { case entry tracking counts }
    
    { clear id counts }
    ctpsnm := 0;
    stpsnm := 0
  end (*initscalars*) ;

  procedure initsets;
  begin
    constbegsys := [lparent,notsy,intconst,realconst,stringconst,ident];
    simptypebegsys := [lparent,addop,intconst,realconst,stringconst,ident];
    typebegsys:=[arrow,packedsy,arraysy,recordsy,setsy,filesy]+simptypebegsys;
    typedels := [arraysy,recordsy,setsy,filesy];
    pfbegsys := [procsy,funcsy,overloadsy,staticsy,virtualsy,overridesy,
                 operatorsy];
    blockbegsys := [privatesy,labelsy,constsy,typesy,varsy,beginsy]+pfbegsys;
    selectsys := [arrow,period,lbrack];
    facbegsys := [intconst,realconst,stringconst,ident,lparent,lbrack,notsy,nilsy,
                  inheritedsy];
    statbegsys := [beginsy,gotosy,ifsy,whilesy,repeatsy,forsy,withsy,casesy,
                   trysy];
  end (*initsets*) ;

  procedure inittables;
    procedure reswords;
    begin
      rw[ 1] := 'if       '; rw[ 2] := 'do       '; rw[ 3] := 'of       ';
      rw[ 4] := 'to       '; rw[ 5] := 'in       '; rw[ 6] := 'or       ';
      rw[ 7] := 'end      '; rw[ 8] := 'for      '; rw[ 9] := 'var      ';
      rw[10] := 'div      '; rw[11] := 'mod      '; rw[12] := 'set      ';
      rw[13] := 'and      '; rw[14] := 'not      '; rw[15] := 'nil      ';
      rw[16] := 'then     '; rw[17] := 'else     '; rw[18] := 'with     ';
      rw[19] := 'goto     '; rw[20] := 'case     '; rw[21] := 'type     ';
      rw[22] := 'file     '; rw[23] := 'begin    '; rw[24] := 'until    ';
      rw[25] := 'while    '; rw[26] := 'array    '; rw[27] := 'const    ';
      rw[28] := 'label    '; rw[29] := 'repeat   '; rw[30] := 'record   ';
      rw[31] := 'downto   '; rw[32] := 'packed   '; rw[33] := 'program  ';
      rw[34] := 'function '; rw[35] := 'procedure'; rw[36] := 'forward  ';
      rw[37] := 'module   '; rw[38] := 'uses     '; rw[39] := 'private  ';
      rw[40] := 'external '; rw[41] := 'view     '; rw[42] := 'fixed    ';
      rw[43] := 'process  '; rw[44] := 'monitor  '; rw[45] := 'share    ';
      rw[46] := 'class    '; rw[47] := 'is       '; rw[48] := 'overload ';
      rw[49] := 'override '; rw[50] := 'reference'; rw[51] := 'joins    ';
      rw[52] := 'static   '; rw[53] := 'inherited'; rw[54] := 'self     ';
      rw[55] := 'virtual  '; rw[56] := 'try      '; rw[57] := 'except   ';
      rw[58] := 'extends  '; rw[59] := 'on       '; rw[60] := 'result   ';
      rw[61] := 'operator '; rw[62] := 'out      '; rw[63] := 'property ';
      rw[64] := 'channel  '; rw[65] := 'stream   '; rw[66] := 'xor      ';
    end (*reswords*) ;

    procedure symbols;
    begin
      rsy[ 1] := ifsy;       rsy[ 2] := dosy;       rsy[ 3] := ofsy;
      rsy[ 4] := tosy;       rsy[ 5] := relop;      rsy[ 6] := addop;
      rsy[ 7] := endsy;      rsy[ 8] := forsy;      rsy[ 9] := varsy;
      rsy[10] := mulop;      rsy[11] := mulop;      rsy[12] := setsy;
      rsy[13] := mulop;      rsy[14] := notsy;      rsy[15] := nilsy;
      rsy[16] := thensy;     rsy[17] := elsesy;     rsy[18] := withsy;
      rsy[19] := gotosy;     rsy[20] := casesy;     rsy[21] := typesy;
      rsy[22] := filesy;     rsy[23] := beginsy;    rsy[24] := untilsy;
      rsy[25] := whilesy;    rsy[26] := arraysy;    rsy[27] := constsy;
      rsy[28] := labelsy;    rsy[29] := repeatsy;   rsy[30] := recordsy;
      rsy[31] := downtosy;   rsy[32] := packedsy;   rsy[33] := progsy;
      rsy[34] := funcsy;     rsy[35] := procsy;     rsy[36] := forwardsy;  
      rsy[37] := modulesy;   rsy[38] := usessy;      rsy[39] := privatesy;  
      rsy[40] := externalsy; rsy[41] := viewsy;      rsy[42] := fixedsy;    
      rsy[43] := processsy;  rsy[44] := monitorsy;   rsy[45] := sharesy;    
      rsy[46] := classsy;    rsy[47] := issy;        rsy[48] := overloadsy; 
      rsy[49] := overridesy; rsy[50] := referencesy; rsy[51] := joinssy;    
      rsy[52] := staticsy;   rsy[53] := inheritedsy; rsy[54] := selfsy;     
      rsy[55] := virtualsy;  rsy[56] := trysy;       rsy[57] := exceptsy;   
      rsy[58] := extendssy;  rsy[59] := onsy;        rsy[60] := resultsy;   
      rsy[61] := operatorsy; rsy[62] := outsy;       rsy[63] := propertysy; 
      rsy[64] := channelsy;  rsy[65] := streamsy;    rsy[66] := addop;    
      
      ssy['+'] := addop ;   ssy['-'] := addop;    ssy['*'] := mulop;
      ssy['/'] := mulop ;   ssy['('] := lparent;  ssy[')'] := rparent;
      ssy['$'] := othersy ; ssy['='] := relop;    ssy[' '] := othersy;
      ssy[','] := comma ;   ssy['.'] := period;   ssy['''']:= othersy;
      ssy['['] := lbrack ;  ssy[']'] := rbrack;   ssy[':'] := colon;
      ssy['^'] := arrow ;   ssy['<'] := relop;    ssy['>'] := relop;
      ssy[';'] := semicolon; ssy['@'] := arrow;   ssy['#'] := numsy;
    end (*symbols*) ;

    procedure rators;
      var i: integer;
    begin
      for i := 1 to maxres (*nr of res words*) do rop[i] := noop;
      rop[5] := inop; rop[10] := idiv; rop[11] := imod;
      rop[6] := orop; rop[13] := andop; rop[66] := xorop;
      for i := ordminchar to ordmaxchar do sop[chr(i)] := noop;
      sop['+'] := plus; sop['-'] := minus; sop['*'] := mul; sop['/'] := rdiv;
      sop['='] := eqop; sop['<'] := ltop;  sop['>'] := gtop;
    end (*rators*) ;

    procedure procmnemonics;
    begin
      { There are two mnemonics that have no counterpart in the
        assembler/interpreter: wro, pak. I didn't find a generator for them, and
        suspect they are abandoned. }
      sna[ 1] :=' get'; sna[ 2] :=' put'; sna[ 3] :=' rdi'; sna[ 4] :=' rdr';
      sna[ 5] :=' rdc'; sna[ 6] :=' wri'; sna[ 7] :=' wro'; sna[ 8] :=' wrr';
      sna[ 9] :=' wrc'; sna[10] :=' wrs'; sna[11] :=' pak'; sna[12] :=' new';
      sna[13] :=' rst'; sna[14] :=' eln'; sna[15] :=' sin'; sna[16] :=' cos';
      sna[17] :=' exp'; sna[18] :=' sqt'; sna[19] :=' log'; sna[20] :=' atn';
      sna[21] :=' rln'; sna[22] :=' wln'; sna[23] :=' sav';
      { new procedure/function memonics for p5/p6 }
      sna[24] :=' pag'; sna[25] :=' rsf'; sna[26] :=' rwf'; sna[27] :=' wrb';
      sna[28] :=' wrf'; sna[29] :=' dsp'; sna[30] :=' wbf'; sna[31] :=' wbi';
      sna[32] :=' wbr'; sna[33] :=' wbc'; sna[34] :=' wbb'; sna[35] :=' rbf';
      sna[36] :=' rsb'; sna[37] :=' rwb'; sna[38] :=' gbf'; sna[39] :=' pbf';
      sna[40] :=' rib'; sna[41] :=' rcb'; sna[42] :=' nwl'; sna[43] :=' dsl';
      sna[44] :=' eof'; sna[45] :=' efb'; sna[46] :=' fbv'; sna[47] :=' fvb';
      sna[48] :=' wbx'; sna[49] :='asst'; sna[50] :='clst'; sna[51] :=' pos'; 
      sna[52] :=' upd'; sna[53] :='appt'; sna[54] :=' del'; sna[55] :=' chg'; 
      sna[56] :=' len'; sna[57] :=' loc'; sna[58] :=' exs'; sna[59] :='assb'; 
      sna[60] :='clsb'; sna[61] :='appb'; sna[62] :=' hlt'; sna[63] :=' ast';
      sna[64] :='asts'; sna[65] :='wrih'; sna[66] :='wrio'; sna[67] :='wrib';
      sna[68] :='wrsp'; sna[69] :='wiz '; sna[70] :='wizh'; sna[71] :='wizo';
      sna[72] :='wizb'; sna[73] :='rds '; sna[74] :='ribf'; sna[75] :='rdif';
      sna[76] :='rdrf'; sna[77] :='rcbf'; sna[78] :='rdcf'; sna[79] :='rdsf';
      sna[80] :='rdsp'; sna[81] :='aeft'; sna[82] :='aefb'; sna[83] :='rdie';
      sna[84] :='rdre'; sna[85] :=' thw';

    end (*procmnemonics*) ;

    procedure instrmnemonics;
    begin
      { ??? memnemonics are placeholders }
      mn[ 0] :=' abi'; mn[ 1] :=' abr'; mn[ 2] :=' adi'; mn[ 3] :=' adr';
      mn[ 4] :=' and'; mn[ 5] :=' dif'; mn[ 6] :=' dvi'; mn[ 7] :=' dvr';
      mn[ 8] :=' ???'; mn[ 9] :=' flo'; mn[10] :=' flt'; mn[11] :=' inn';
      mn[12] :=' int'; mn[13] :=' ior'; mn[14] :=' mod'; mn[15] :=' mpi';
      mn[16] :=' mpr'; mn[17] :=' ngi'; mn[18] :=' ngr'; mn[19] :=' not';
      mn[20] :=' odd'; mn[21] :=' sbi'; mn[22] :=' sbr'; mn[23] :=' sgs';
      mn[24] :=' sqi'; mn[25] :=' sqr'; mn[26] :=' sto'; mn[27] :=' trc';
      mn[28] :=' uni'; mn[29] :=' stp'; mn[30] :=' csp'; mn[31] :=' dec';
      mn[32] :=' ent'; mn[33] :=' fjp'; mn[34] :=' inc'; mn[35] :=' ind';
      mn[36] :=' ixa'; mn[37] :=' lao'; mn[38] :=' lca'; mn[39] :=' ldo';
      mn[40] :=' mov'; mn[41] :=' mst'; mn[42] :=' ret'; mn[43] :=' sro';
      mn[44] :=' xjp'; mn[45] :=' chk'; mn[46] :=' cup'; mn[47] :=' equ';
      mn[48] :=' geq'; mn[49] :=' grt'; mn[50] :=' lda'; mn[51] :=' ldc';
      mn[52] :=' leq'; mn[53] :=' les'; mn[54] :=' lod'; mn[55] :=' neq';
      mn[56] :=' str'; mn[57] :=' ujp'; mn[58] :=' ord'; mn[59] :=' chr';
      mn[60] :=' ujc';
      { new instruction memonics for p5/p6 }
      mn[61] :=' rnd'; mn[62] :=' pck'; mn[63] :=' upk'; mn[64] :=' rgs';
      mn[65] :=' ???'; mn[66] :=' ipj'; mn[67] :=' cip'; mn[68] :=' lpa';
      mn[69] :=' ???'; mn[70] :=' ???'; mn[71] :=' dmp'; mn[72] :=' swp';
      mn[73] :=' tjp'; mn[74] :=' lip'; mn[75] :=' ckv'; mn[76] :=' dup';
      mn[77] :=' cke'; mn[78] :=' cks'; mn[79] :=' inv'; mn[80] :=' ckl';
      mn[81] :=' cta'; mn[82] :=' ivt'; mn[83] :=' xor'; mn[84] :=' bge';
      mn[85] :=' ede'; mn[86] :=' mse'; mn[87] :=' cjp'; mn[88] :=' lnp';
      mn[89] :=' cal'; mn[90] :=' ret'; mn[91] :=' cuv'; mn[92] :=' suv';

    end (*instrmnemonics*) ;

    procedure chartypes;
    var i : integer;
    begin
      for i := ordminchar to ordmaxchar do chartp[chr(i)] := illegal;
      chartp['a'] := letter  ;
      chartp['b'] := letter  ; chartp['c'] := letter  ;
      chartp['d'] := letter  ; chartp['e'] := letter  ;
      chartp['f'] := letter  ; chartp['g'] := letter  ;
      chartp['h'] := letter  ; chartp['i'] := letter  ;
      chartp['j'] := letter  ; chartp['k'] := letter  ;
      chartp['l'] := letter  ; chartp['m'] := letter  ;
      chartp['n'] := letter  ; chartp['o'] := letter  ;
      chartp['p'] := letter  ; chartp['q'] := letter  ;
      chartp['r'] := letter  ; chartp['s'] := letter  ;
      chartp['t'] := letter  ; chartp['u'] := letter  ;
      chartp['v'] := letter  ; chartp['w'] := letter  ;
      chartp['x'] := letter  ; chartp['y'] := letter  ;
      chartp['z'] := letter  ;
      chartp['A'] := letter  ;
      chartp['B'] := letter  ; chartp['C'] := letter  ;
      chartp['D'] := letter  ; chartp['E'] := letter  ;
      chartp['F'] := letter  ; chartp['G'] := letter  ;
      chartp['H'] := letter  ; chartp['I'] := letter  ;
      chartp['J'] := letter  ; chartp['K'] := letter  ;
      chartp['L'] := letter  ; chartp['M'] := letter  ;
      chartp['N'] := letter  ; chartp['O'] := letter  ;
      chartp['P'] := letter  ; chartp['Q'] := letter  ;
      chartp['R'] := letter  ; chartp['S'] := letter  ;
      chartp['T'] := letter  ; chartp['U'] := letter  ;
      chartp['V'] := letter  ; chartp['W'] := letter  ;
      chartp['X'] := letter  ; chartp['Y'] := letter  ;
      chartp['Z'] := letter  ;
      chartp['_'] := letter  ;
      chartp['0'] := number  ;
      chartp['1'] := number  ; chartp['2'] := number  ;
      chartp['3'] := number  ; chartp['4'] := number  ;
      chartp['5'] := number  ; chartp['6'] := number  ;
      chartp['7'] := number  ; chartp['8'] := number  ;
      chartp['9'] := number  ; chartp['+'] := special ;
      chartp['-'] := special ; chartp['*'] := special ;
      chartp['/'] := special ; chartp['('] := chlparen;
      chartp[')'] := special ; chartp['$'] := special ;
      chartp['='] := special ; chartp[' '] := chspace ;
      chartp[','] := special ; chartp['.'] := chperiod;
      chartp['''']:= chstrquo; chartp['['] := special ;
      chartp[']'] := special ; chartp[':'] := chcolon ;
      chartp['^'] := special ; chartp[';'] := special ;
      chartp['<'] := chlt    ; chartp['>'] := chgt    ;
      chartp['{'] := chlcmt  ; chartp['}'] := special ;
      chartp['@'] := special ; chartp['!'] := chrem   ;
      chartp['$'] := chhex   ; chartp['&'] := choct   ;
      chartp['%'] := chbin   ; chartp['#'] := special ;

      for i := ordminchar to ordmaxchar do ordint[chr(i)] := 0;
      ordint['0'] := 0;  ordint['1'] := 1;  ordint['2'] := 2;
      ordint['3'] := 3;  ordint['4'] := 4;  ordint['5'] := 5;
      ordint['6'] := 6;  ordint['7'] := 7;  ordint['8'] := 8;
      ordint['9'] := 9;  ordint['a'] := 10; ordint['b'] := 11; 
      ordint['c'] := 12; ordint['d'] := 13; ordint['e'] := 14; 
      ordint['f'] := 15; ordint['A'] := 10; ordint['B'] := 11; 
      ordint['C'] := 12; ordint['D'] := 13; ordint['E'] := 14; 
      ordint['F'] := 15;
    end;

    procedure initdx;
    begin
      { [sam] if your sizes are not even multiples of
        stackelsize, you are going to need to compensate this.
        entries marked with * go to secondary table }
      cdx[ 0] :=  0;                   cdx[ 1] :=  0;                 
      cdx[ 2] := +intsize;             cdx[ 3] := +realsize;
      cdx[ 4] := +intsize;             cdx[ 5] := +setsize;           
      cdx[ 6] := +intsize;             cdx[ 7] := +realsize;
      cdx[ 8] :=  0;                   cdx[ 9] := +intsize-realsize; 
      cdx[10] :=  -realsize+intsize;   cdx[11] := +setsize;
      cdx[12] := +setsize;             cdx[13] := +intsize; 
      cdx[14] := +intsize;             cdx[15] := +intsize;
      cdx[16] := +realsize;            cdx[17] :=  0; 
      cdx[18] :=  0;                   cdx[19] :=  2{*};
      cdx[20] :=  0;                   cdx[21] := +intsize; 
      cdx[22] := +realsize;            cdx[23] := +intsize-setsize;
      cdx[24] :=  0;                   cdx[25] :=  0; 
      cdx[26] := 1{*};                 cdx[27] := +realsize-intsize;
      cdx[28] := +setsize;             cdx[29] :=  0; 
      cdx[30] :=  0;                   cdx[31] :=  2{*};
      cdx[32] :=  0;                   cdx[33] := +intsize; 
      cdx[34] :=  2{*};                cdx[35] :=  3{*};
      cdx[36] := +intsize;             cdx[37] := -adrsize; 
      cdx[38] := -adrsize;             cdx[39] :=  4{*};
      cdx[40] := +adrsize*2;           cdx[41] :=  0; 
      cdx[42] :=  2{*};                cdx[43] :=  5{*};
      cdx[44] := +intsize;             cdx[45] :=  2{*}; 
      cdx[46] :=  0;                   cdx[47] :=  6{*};
      cdx[48] :=  6{*};                cdx[49] :=  6{*}; 
      cdx[50] := -adrsize;             cdx[51] :=  4{*};
      cdx[52] :=  6{*};                cdx[53] :=  6{*}; 
      cdx[54] :=  4{*};                cdx[55] :=  6{*};
      cdx[56] :=  5{*};                cdx[57] :=  0; 
      cdx[58] :=  2{*};                cdx[59] :=  0;
      cdx[60] :=  0;                   cdx[61] :=  +realsize-intsize; 
      cdx[62] := +adrsize*3;           cdx[63] := +adrsize*3;
      cdx[64] := +intsize*2-setsize;   cdx[65] :=  0; 
      cdx[66] :=  0;                   cdx[67] := +ptrsize;
      cdx[68] := -adrsize*2;           cdx[69] :=  0; 
      cdx[70] :=  0;                   cdx[71] := +ptrsize;
      cdx[72] :=  0;                   cdx[73] := +intsize; 
      cdx[74] := -adrsize*2;           cdx[75] :=  2{*};
      cdx[76] :=  4{*};                cdx[77] :=  +intsize*2;
      cdx[78] := -intsize;             cdx[79] :=  +adrsize;
      cdx[80] :=  2{*};                cdx[81] :=  0;
      cdx[82] :=  0;                   cdx[83] := +intsize;
      cdx[84] := -adrsize;             cdx[85] := +adrsize;
      cdx[86] := 0;                    cdx[87] := 0;
      cdx[88] := 0;                    cdx[89] := 0;
      cdx[90] := 0;                    cdx[91] := 0;
      cdx[92] := 0;

      { secondary table order is i, r, b, c, a, s, m }
      cdxs[1][1] := +(adrsize+intsize);  { stoi }
      cdxs[1][2] := +(adrsize+realsize); { stor }
      cdxs[1][3] := +(adrsize+intsize);  { stob }
      cdxs[1][4] := +(adrsize+intsize);  { stoc }
      cdxs[1][5] := +(adrsize+adrsize);  { stoa }
      cdxs[1][6] := +(adrsize+setsize);  { stos }
      cdxs[1][7] := 0;
      
      cdxs[2][1] := 0; { deci/inci/ordi/chki/reti/noti }   
      cdxs[2][2] := 0; { chkr/retr }
      cdxs[2][3] := 0; { decb/incb/ordb/chkb/retb/notb }
      cdxs[2][4] := 0; { decc/incc/ordc/chkc/retc }
      cdxs[2][5] := 0; { chka/reta/ckl }
      cdxs[2][6] := 0; { chks }
      cdxs[2][7] := 0;
      
      cdxs[3][1] := +adrsize-intsize;  { indi }
      cdxs[3][2] := +adrsize-realsize; { indr }
      cdxs[3][3] := +adrsize-intsize;  { indb }
      cdxs[3][4] := +adrsize-intsize;  { indc }
      cdxs[3][5] := +adrsize-adrsize;  { inda }
      cdxs[3][6] := +adrsize-setsize;  { inds }
      cdxs[3][7] := 0;

      cdxs[4][1] := -intsize;  { ldoi/ldc/lodi/dupi }
      cdxs[4][2] := -realsize; { ldor/ldc/lodr/dupr }
      cdxs[4][3] := -intsize;  { ldob/ldc/lodb/dupb }
      cdxs[4][4] := -intsize;  { ldoc/ldc/lodc/dupc }
      cdxs[4][5] := -adrsize;  { ldoa/ldc/loda/dupa }
      cdxs[4][6] := -setsize;  { ldos/ldc/lods/dups }
      cdxs[4][7] := 0;
      
      cdxs[5][1] := +intsize;  { sroi/stri }
      cdxs[5][2] := +realsize; { sror/strr }
      cdxs[5][3] := +intsize;  { srob/strb }
      cdxs[5][4] := +intsize;  { sroc/strc }
      cdxs[5][5] := +adrsize;  { sroa/stra }
      cdxs[5][6] := +setsize;  { sros/strs }
      cdxs[5][7] := 0;
      
      { note that all of the comparisions share the same table }
      cdxs[6][1] := +(intsize+intsize)-intsize; { equi/neqi/geqi/grti/leqi/lesi }
      cdxs[6][2] := +(realsize+realsize)-intsize; { equr/neqr/geqr/grtr/leqr/lesr }
      cdxs[6][3] := +(intsize+intsize)-intsize; { equb/neqb/geqb/grtb/leqb/lesb }
      cdxs[6][4] := +(intsize+intsize)-intsize; { equc/neqc/geqc/grtc/leqc/lesc }
      cdxs[6][5] := +(adrsize+intsize)-adrsize; { equa/neqa/geqa/grta/leqa/lesa }
      cdxs[6][6] := +(setsize+setsize)-intsize; { equs/neqs/geqs/grts/leqs/less }
      cdxs[6][7] := +(adrsize+adrsize)-intsize; { equm/neqm/geqm/grtm/leqm/lesm }
      
      pdx[ 1] := +adrsize;             pdx[ 2] := +adrsize; 
      pdx[ 3] := +adrsize;             pdx[ 4] := +adrsize;
      pdx[ 5] := +adrsize;             pdx[ 6] := +adrsize*2; 
      pdx[ 7] := 0;                    pdx[ 8] := +(realsize+intsize);
      pdx[ 9] := +intsize*2;           pdx[10] := +(adrsize+intsize*2); 
      pdx[11] :=  0;                   pdx[12] := +ptrsize*2;
      pdx[13] :=  0;                   pdx[14] := +adrsize-intsize; 
      pdx[15] :=  0;                   pdx[16] :=  0;
      pdx[17] :=  0;                   pdx[18] :=  0; 
      pdx[19] :=  0;                   pdx[20] :=  0;
      pdx[21] :=  0;                   pdx[22] :=  0; 
      pdx[23] :=  0;                   pdx[24] := +adrsize;
      pdx[25] := +adrsize;             pdx[26] := +adrsize;
      pdx[27] := +intsize*2;           pdx[28] := +(realsize+intsize*2);
      pdx[29] := +adrsize*2;           pdx[30] := +(adrsize+intsize); 
      pdx[31] := +intsize;             pdx[32] := +realsize;
      pdx[33] := +intsize;             pdx[34] := +intsize; 
      pdx[35] := +(intsize+adrsize);   pdx[36] := +adrsize;
      pdx[37] := +adrsize;             pdx[38] := +(intsize+adrsize); 
      pdx[39] := +(intsize+adrsize);   pdx[40] := +(adrsize+intsize*2);
      pdx[41] := +(adrsize+intsize*2); pdx[42] := +(adrsize+intsize*2);
      pdx[43] := +(adrsize+intsize*2); pdx[44] := +adrsize-intsize;     
      pdx[45] := +adrsize-intsize;     pdx[46] :=  0;                   
      pdx[47] := +intsize;             pdx[48] := +intsize;
      pdx[49] := +adrsize*2+intsize;   pdx[50] := +adrsize;
      pdx[51] := +adrsize+intsize;     pdx[52] := +adrsize;
      pdx[53] := +adrsize;             pdx[54] := +adrsize+intsize;
      pdx[55] := +adrsize*2+intsize*2; pdx[56] := +adrsize-intsize;
      pdx[57] := +adrsize-intsize;     pdx[58] := +adrsize+intsize-intsize;
      pdx[59] := +adrsize*2+intsize;   pdx[60] := +adrsize;
      pdx[61] := +adrsize;             pdx[62] := 0;
      pdx[63] := +intsize;             pdx[64] := +adrsize+intsize+intsize;
      pdx[65] := +adrsize*2;           pdx[66] := +adrsize*2;
      pdx[67] := +adrsize*2;           pdx[68] := +(adrsize+intsize);
      pdx[69] := +adrsize*2;           pdx[70] := +adrsize*2;
      pdx[71] := +adrsize*2;           pdx[72] := +adrsize*2;
      pdx[73] := +adrsize+intsize;     pdx[74] := +(adrsize+intsize*3);
      pdx[75] := +adrsize+intsize;     pdx[76] := +adrsize+intsize;
      pdx[77] := +(adrsize+intsize*3); pdx[78] := +adrsize+intsize;
      pdx[79] := +adrsize+intsize*2;   pdx[80] := +adrsize+intsize;
      pdx[81] := +adrsize*2+intsize;   pdx[82] := +adrsize*2+intsize;
      pdx[83] := +adrsize*2+intsize;   pdx[84] := +adrsize*2+intsize;
      pdx[85] := +adrsize;
                                                      
    end;

  begin (*inittables*)
    reswords; symbols; rators;
    instrmnemonics; procmnemonics;
    chartypes; initdx;
  end (*inittables*) ;

begin
  
  { Suppress unreferenced errors. These are all MPB (machine parameter
    block) equations that need to stay the same between front end and backend. }
  if heapal = 0 then;    
  if inthex = 0 then;    
  if market = 0 then;    
  if markep = 0 then;    
  if markdl = 0 then;    
  if markra = 0 then;    
  if marksb = 0 then;    
  if markfv = 0 then;    
  if marksl = 0 then;    
  if maxresult = 0 then; 
  if maxsize = 0 then;   

  (*initialize*)
  (************)
  initscalars; initsets; inittables;
  
  write('P6 Pascal compiler vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  if iso7185 then begin
    writeln('Pascal-P6 complies with the requirements of level 0 of ISO/IEC 7185.');
    writeln
  end;

  (*enter standard names and standard types:*)
  (******************************************)
  level := 0; top := 0; ptop := 0;
  with display[0] do
    begin fname := nil; flabel := nil; fconst := nil; fstruct := nil;
          packing := false; packcom := false; ptrref := false; modnam := nil;
          occur := blck; bname := nil end;
  enterstdtypes; stdnames; entstdnames; enterundecl;
  top := 1; level := 1;
  with display[1] do
    begin fname := nil; flabel := nil; fconst := nil; fstruct := nil;
          packing := false; packcom := false; ptrref := false; modnam := nil;
          occur := blck; bname := nil end;

  (*compile:*)
  (**********)

  { !!! remove these statements for self compile }
  {elide}reset(prd); rewrite(prr);{noelide} { open output file }
 
  { write generator comment }
  writeln(prr, 'i');
  writeln(prr, 'i Pascal intermediate file Generated by P6 Pascal compiler vs. ',
          majorver:1, '.', minorver:1);
  writeln(prr, 'i');
  
  { write initial option values }
  write(prr, 'o '); 
  for c := 'a' to 'z' do 
    if not (c in ['g','h','n','o','p','q','s','w','r']) then
      begin write(prr, c); 
    if option[c] then write(prr, '+') else write(prr, '-')
  end;
  writeln(prr);

  nvalid := false; { set no lookahead }
  { init for lookahead }
  sy := ident; op := mul; lgth := 0; kk := 1; ch := ' ';
  insymbol;
  modulep(blockbegsys+statbegsys-[casesy]);
  { release file tracking entries }
  putinp(incstk); putinp(inclst);
  outline;

  { dispose of levels 0 and 1 }
  putdsp(display[1]);
  putdsp(display[0]);
  
  { dispose of the pile }
  putpile;

  { remove undeclared ids }
  exitundecl;

  writeln;
  writeln('Errors in program: ', toterr:1);
  { output error report as required }
  f := true;
  for i := 1 to 500 do if errtbl[i] then begin
    if f then begin
      writeln;
      writeln('Error numbers in listing:');
      writeln('-------------------------');
      f := false
    end;
    write(i:3, '  '); errmsg(i); writeln
  end;
  if not f then writeln;

  if doprtryc then begin { print recyling tracking counts }

    writeln;
    writeln('Recycling tracking counts:');
    writeln;
    writeln('string quants:              ', strcnt:1);
    writeln('constants:                  ', cspcnt:1);
    writeln('structures:                 ', stpcnt:1);
    writeln('identifiers:                ', ctpcnt:1);
    writeln('label counts:               ', lbpcnt:1);
    writeln('file tracking counts:       ', filcnt:1);
    writeln('case entry tracking counts: ', cipcnt:1);
    writeln;

  end;

  if doprtlab then prtlabels; { dump labels}
  if dodmpdsp then prtdsp; { dump display }
  
  { perform errors for recycling balance }

  if strcnt <> 0 then
     writeln('*** Error: Compiler internal error: string recycle balance: ',
             strcnt:1);
  if cspcnt <> 0 then
     writeln('*** Error: Compiler internal error: constant recycle balance: ',
             cspcnt:1);
  if stpcnt <> 0 then
     writeln('*** Error: Compiler internal error: structure recycle balance: ',
             stpcnt:1);
  if ctpcnt <> 0 then
     writeln('*** Error: Compiler internal error: identifier recycle balance: ',
             ctpcnt:1);
  if lbpcnt <> 0 then
     writeln('*** Error: Compiler internal error: label recycle balance: ',
             lbpcnt:1);
  if filcnt <> 0 then
     writeln('*** Error: Compiler internal error: file recycle balance: ',
             filcnt:1);
  if cipcnt <> 0 then
     writeln('*** Error: Compiler internal error: case recycle balance: ',
             cipcnt:1);

  99:

end.
