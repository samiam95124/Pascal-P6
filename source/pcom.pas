{$l-}
{*******************************************************************************
*                                                                              *
*                         PASCAL-P6 PORTABLE INTERPRETER                       *
*                                                                              *
* LICENSING:                                                                   *
*                                                                              *
* Copyright (c) 2022, Scott A. Franco                                          *
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
  PASCALINE      - uses the Pascaline standard language.
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

program pcom(output,prd,prr);

label 99; { terminate immediately }

const

      { ************************************************************************

      Program object sizes and characteristics, sync with pint. These define
      the machine specific characteristics of the target.

      The configurations are as follows:

      type                  #bits 16  #bits 32  #bits 64
      ===========================================================
      integer               16        32        64
      real                  32        64        64
      char                  8         8
      boolean               8         8
      set                   256       256
      pointers              16        32        64
      marks                 16        32        64 (bytes)
      File logical number   8         8         8

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

   displimit   = 300;
   maxlevel    = 255;
   { strglgth used to define the size of all strings in pcom and pint. With the
     string quanta system, string lengths are effectively unlimited, but there
     it still sets the size of some buffers in pcom. }
   strglgth    = 250;
   fileal      = charal;
   (* stackelsize = minimum size for 1 stackelement
                  = k*stackal
      stackal     = scm(all other al-constants)
      charmax     = scm(charsize,charal)
                    scm = smallest common multiple *)
   parmal     = stackal;
   parmsize   = stackelsize;
   recal      = stackal;
   maxaddr    = pmmaxint;
   maxsp      = 85;   { number of standard procedures/functions }
   maxins     = 122;  { maximum number of instructions }
   maxids     = 250;  { maximum characters in id string (basically, a full line) }
   maxstd     = 82;   { number of standard identifiers }
   maxres     = 66;   { number of reserved words }
   reslen     = 9;    { maximum length of reserved words }
   maxopt     = 26;   { number of options }
   optlen     = 10;   { maximum length of option words }
   explen     = 32;   { length of exception names }
   maxrld     = 22;   { maximum length of real in digit form }
   varsqt     = 10;   { variable string quanta }
   prtlln     = 10;   { number of label characters to print in dumps }
   minocc     = 50;   { minimum occupancy for case tables }
   varmax     = 1000; { maximum number of logical variants to track }
   cstoccmax=4000; cixmax=10000;
   fillen     = maxids;
   extsrc     = '.pas'; { extention for source file }
   maxftl     = 516; { maximum fatal error }
   maxcmd     = 250; { size of command line buffer }
   maxlin     = 250; { size of source line buffer }

   { default field sizes for write }
   intdeff    = 11; { default field length for integer }
   reldeff    = 22; { default field length for real }
   chrdeff    = 1;  { default field length for char (usually 1) }
   boldeff    = 5;  { default field length for boolean (usually 5 for 'false' }

#include  "version.inc"

   { standard exceptions. Used for extension routines, this is a subset. }
   CommandLineTooLong      = 1;
   FunctionNotImplemented  = 2;
   FileDeleteFail          = 3;
   FileNameChangeFail      = 4;

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
                  neop,eqop,inop,noop,xorop,notop,bcmop);
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
     levrange = 0..maxlevel; addrrange = -maxaddr..maxaddr; stkoff = -maxaddr..maxaddr;
     structform = (scalar,subrange,pointer,power,arrays,arrayc,records,files,
                   tagfld,variant,exceptf);
     declkind = (standard,declared);
     varinx = 0..varmax;
     vartbl = array [0..varmax] of integer; { variant value to logical table }
     vartpt = ^vartbl;
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
                     arrays:   (aeltype,inxtype: stp; tmpl: integer);
                     arrayc:   (abstype: stp);
                     records:  (fstfld: ctp; recvar: stp; recyc: stp);
                     files:    (filtype: stp);
                     tagfld:   (tagfieldp: ctp; fstvar: stp; vart: vartpt;
                                varts: varinx);
                     variant:  (nxtvar,subvar,caslst: stp; varfld: ctp;
                                varval: valu; varln: integer);
                     exceptf:  ()
                   end;

                                                            (*names*)
                                                            (*******)

     { copyback buffers }
     cbbufp =^cbbuf;
     cbbuf = record next: cbbufp; { next link }
               addr: addrrange;   { global address of buffer }
               id: ctp;           { parameter occupying buffer }
               size: addrrange    { size of variable in buffer }
             end;

     idclass = (types,konst,fixedt,vars,field,proc,func,alias);
     setofids = set of idclass;
     idkind = (actual,formal);
     idstr = packed array [1..maxids] of char;
     restr = packed array [1..reslen] of char;
     optinx = 1..optlen;
     optstr = packed array [optinx] of char;
     expstr = packed array [1..explen] of char;
     csstr = packed array [1..strglgth] of char;
     rlstr = packed array [1..maxrld] of char;
     keyrng = 1..32; { range of standard call keys }
     filnam = packed array [1..fillen] of char; { filename strings }
     filptr = ^filrec;
     filrec = record next: filptr; fn: filnam; mn: strvsp; f: text;
                     priv: boolean; linecounts, lineouts: integer end;
     partyp = (ptval, ptvar, ptview, ptout);
     { procedure function attribute }
     fpattr = (fpanone,fpaoverload,fpastatic,fpavirtual,fpaoverride);
     identifier = record
                   snm: integer; { serial number }
                   name: strvsp; llink, rlink: ctp;
                   idtype: stp; next: ctp; keep: boolean;
                   refer: boolean; cbb: cbbufp;
                   case klass: idclass of
                     types: ();
                     konst: (values: valu);
                     vars:  (vkind: idkind; vlev: levrange; vaddr: addrrange;
                             isloc: boolean; threat: boolean; forcnt: integer; 
                             part: partyp; hdr: boolean; vext: boolean; 
                             vmod: filptr; inilab: integer; skplab: integer; 
                             ininxt: ctp; dblptr: boolean);
                     fixedt: (floc: integer; fext: boolean; fmod: filptr);
                     field: (fldaddr: addrrange; varnt: stp; varlb: ctp;
                             tagfield: boolean; taglvl: integer;
                             varsaddr: addrrange; varssize: addrrange;
                             vartl: integer);
                     proc, func:  (pfaddr: addrrange; pflist: ctp; { param list }
                                   locpar: addrrange; { size of parameters }
                                   locstr: addrrange; { start of locals }
                                   locspc: addrrange; { space occupied by locals }
                                   asgn: boolean; { assigned }
                                   pext: boolean; pmod: filptr; pfattr: fpattr;
                                   pfvaddr: addrrange; pfvid: ctp;
                                   grppar, grpnxt: ctp;
                                   case pfdeckind: declkind of
                              standard: (key: keyrng);
                              declared: (pflev: levrange; pfname: integer;
                                          case pfkind: idkind of
                                           actual: (forwdecl, sysrot, extern: boolean);
                                           formal: ()));
                     alias: (actid: ctp; { actual id })
                   end;

     where = (blck,crec,vrec,rec);

                                                            (*expressions*)
                                                            (*************)
     attrkind = (cst,varbl,expr);
     vaccess = (drct,indrct,inxd);

     attr = record symptr: ctp; typtr: stp; spv: boolean;
              case kind: attrkind of
                cst:   (cval: valu);
                varbl: (packing: boolean; packcom: boolean;
                        tagfield: boolean; taglvl: integer; varnt: stp;
                        ptrref: boolean; vartagoff: addrrange;
                        varssize: addrrange; vartl: integer; pickup: boolean;
                        dblptr: boolean;
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
                 define: boolean;          { is this a defining block? }
                 modnam: strvsp;           { module name for block (if exists) }
                 inilst: ctp;              { initializer list }
                 oprprc: array [operatort] of ctp; { operator functions }
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

     { tag tracking entries }
     ttp = ^tagtrk;
     tagtrk = record
                ival: integer;
                next: ttp
              end;

     { 'with' tracking entries }
     wtp = ^wthtrk;
     wthtrk = record next: wtp;
                 sl: integer
              end;

     stdrng = 1..maxstd; { range of standard name entries }
     oprange = 0..maxins;
     modtyp = (mtprogram, mtmodule); { type of current module }
     byte = 0..255; { 8-bit byte }
     bytfil = packed file of byte; { untyped file of bytes }
     cmdinx = 1..maxcmd; { index for command line buffer }
     cmdnum = 0..maxcmd; { length of command line buffer }
     cmdbuf = packed array [cmdinx] of char; { buffer for command line }
     lininx = 1..maxlin; { index for source line buffer }
     linbuf = packed array [lininx] of char; { buffer for source lines }

(*-------------------------------------------------------------------------*)

var

    { !!! remove this statement for self compile }
#ifndef SELF_COMPILE
    prd: text;                      { input source file }
    prr: text;                      { output code file }
#endif

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
    lc,lcs: stkoff;
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
    chkvbk: boolean;                { -- i: check VAR block violations }
    experr: boolean;                { -- ee/experror: expanded error 
                                         descriptions }

    { debug flags from backend, these are not set here }
    dochkovf: boolean; { check arithmetic overflow }
    dodmplab: boolean; { dump label definitions }
    dotrcrot: boolean; { trace routine executions }
    dotrcins: boolean; { trace instruction executions }
    dosrclin: boolean; { add source line sets to code }
    dotrcsrc: boolean; { trace source line executions (requires dosrclin) }
    dorecycl: boolean; { obey heap space recycle requests }
    dodebug:  boolean; { start up debug on entry }
    dodbgflt: boolean; { enter debug on fault }
    dodbgsrc: boolean; { do source file debugging }
    dochkrpt: boolean; { check reuse of freed entry (automatically }
    donorecpar: boolean; { break returned blocks as occupied, not free }
    dochkdef: boolean; { check undefined accesses }
    dosrcprf: boolean; { do source level profiling }
    dochkcov: boolean; { do code coverage }
    doanalys: boolean; { do analyze }
    dodckout: boolean; { do output code deck }
    dochkvbk: boolean; { do check VAR blocks }
    doechlin: boolean; { do echo command line for testing (unused) }
   

    { switches passed through to pint }

    { -- o: check arithmetic overflow }
    { -- g: dump label definitions }
    { -- f: perform source level debugging }
    { -- m: break heap returned blocks as occupied }
    { -- h: add source line sets to code }
    { -- n: obey heap space recycle requests }
    { -- p: check reuse of freed entry }
    { -- q: check undefined accesses }
    { -- w: enter debugger on run }
    { -- a: enter debugger on fault }
    { -- e: output P-machine code deck and stop }

    { unused options }

    { -- j }
    { -- k }
    { -- z }

    option: array [1..maxopt] of    { option array }
              boolean;

                                    (*pointers:*)
                                    (***********)
    parmptr,
    intptr,crdptr,realptr,charptr,
    boolptr,nilptr,textptr,
    exceptptr,stringptr,pstringptr,
    byteptr,vectorptr,matrixptr,
    abyteptr,scharptr: stp;         (*pointers to entries of standard ids*)
    utypptr,ucstptr,uvarptr,
    ufldptr,uprcptr,ufctptr,        (*pointers to entries for undeclared ids*)
    fwptr: ctp;                     (*head of chain of forw decl type ids*)
    outputptr,inputptr,
    prdptr,prrptr,errorptr,
    listptr,commandptr: ctp;        { pointers to default files }
    usclrptr: ctp;                  { used to satisfy broken record tag fields }
    fextfilep: extfilep;            (*head of chain of external files*)
    wthstk: wtp;                    { stack of with entries active }

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
                      nmr: 1..maxftl
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
    mn:  array [0..maxins] of packed array [1..3] of char;
    sna: array [1..maxsp] of packed array [1..4] of char;
    opts: array [1..maxopt] of optstr;
    optsl: array [1..maxopt] of optstr;
    cdx: array [0..maxins] of integer;
    cdxs: array [1..6, 1..8] of integer;
    pdx: array [1..maxsp] of integer;
    ordint: array [char] of integer;

    intlabel,mxint10,maxpow10: integer;
    entname,extname,nxtname: integer;
    errtbl: array [1..maxftl] of integer; { error occurence tracking }
    toterr: integer; { total errors in program }
    topnew, topmin: integer;
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
    cbblst: cbbufp; { copyback buffer entry list }
    srclin: linbuf; { buffer for input source lines }
    srcinx: lininx; { index for input buffer }
    srclen: 0..maxlin; { size of input line }

    { Recycling tracking counters, used to check for new/dispose mismatches. }
    strcnt: integer; { strings }
    cspcnt: integer; { constants }
    stpcnt: integer; { structures }
    ctpcnt: integer; { identifiers }
    lbpcnt: integer; { label counts }
    filcnt: integer; { file tracking counts }
    cipcnt: integer; { case entry tracking counts }
    ttpcnt: integer; { tag tracking entry counts }
    wtpcnt: integer; { with tracking entry counts }

    { serial numbers to label structure and identifier entries for dumps }
    ctpsnm: integer;
    stpsnm: integer;

    { command line handling }
    cmdlin: cmdbuf; { command line }
    cmdlen: cmdnum; { length of command line }
    cmdpos: cmdinx; { current position in command line }

    breakflag: boolean; { user break signaled }

    f: boolean; { flag for if error number list entries were printed }
    i: 1..maxftl; { index for error number tracking array }
    oi: 1..maxopt; oni: optinx;

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
     { release entry }
     case p^.cclass of
       reel: dispose(p, reel);
       pset: dispose(p, pset);
       strg: dispose(p, strg)
     end;
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
     { release entry }
     case p^.form of
       scalar:   if p^.scalkind = declared then dispose(p, scalar, declared)
                                           else dispose(p, scalar, standard);
       subrange: dispose(p, subrange);
       pointer:  dispose(p, pointer);
       power:    dispose(p, power);
       arrays:   dispose(p, arrays);
       arrayc:   dispose(p, arrayc);
       records:  dispose(p, records);
       files:    dispose(p, files);
       tagfld:   begin dispose(p^.vart); dispose(p, tagfld) end;
       variant:  dispose(p, variant);
       exceptf:  dispose(p, exceptf)
     end;
     stpcnt := stpcnt-1
  end;

  { initialize and register identifier entry }
  procedure ininam(p: ctp);
  begin
     ctpcnt := ctpcnt+1; { count entry }
     { clear fixed entries }
     p^.idtype := nil; p^.keep := false; p^.refer := false;
     p^.name := nil; p^.llink := nil; p^.rlink := nil; p^.next := nil;
     p^.cbb := nil;
     ctpsnm := ctpsnm+1; { identify entry in dumps }
     p^.snm := ctpsnm
  end;

  procedure putnam(p: ctp); forward;

  { recycle parameter list }
  procedure putparlst(p: ctp);
  var p1: ctp;
  begin
    while p <> nil do begin
      p1 := p; p := p^.next;
      putnam(p1) { release }
    end
  end;

  { recycle identifier entry }
  procedure putnam{(p: ctp)};
  var p1: ctp;
  begin
     if (p^.klass = proc) or (p^.klass = func) then begin
        putparlst(p^.pflist); p^.pflist := nil;
        if p = p^.grppar then while p^.grpnxt <> nil do begin
          { scavenge the group list }
          p1 := p^.grpnxt; p^.grpnxt := p1^.grpnxt;
          putnam(p1) { release }
        end
     end;
     if p^.klass <> alias then putstrs(p^.name); { release name string }
     { release entry according to class }
     case p^.klass of
       types: dispose(p, types);
       konst: dispose(p, konst);
       vars:  dispose(p, vars);
       fixedt: dispose(p, fixedt);
       field: dispose(p, field);
       proc: if p^.pfdeckind = standard then dispose(p, proc, standard)
                                        else if p^.pfkind = actual then
                                            dispose(p, proc, declared, actual)
                                          else dispose(p, proc, declared, formal);
       func: if p^.pfdeckind = standard then dispose(p, func, standard)
                                        else if p^.pfkind = actual then
                                            dispose(p, func, declared, actual)
                                          else dispose(p, func, declared, formal);
       alias: dispose(p, alias)
     end;
     ctpcnt := ctpcnt-1 { remove from count }
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

  { initialize display record }
  procedure inidsp(var dr: disprec);
    var oi: operatort;
  begin
    with dr do begin
      fname := nil;
      flabel := nil;
      fconst := nil;
      fstruct := nil;
      packing := false;
      packcom := false;
      ptrref := false;
      define := false;
      modnam := nil;
      inilst := nil;
      for oi := mul to bcmop do oprprc[oi] := nil
    end
  end;

  { scrub display level }
  procedure putdsp(var dr: disprec);
     var llp: lbp; lvp: csp; lsp: stp; oi: operatort;
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
    putstrs(dr.modnam);
    for oi := mul to bcmop do
      if dr.oprprc[oi] <> nil then putnam(dr.oprprc[oi]);
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

  { get tag tracking entry }
  procedure gettag(var p: ttp);
  begin
     new(p); { get new entry }
     ttpcnt := ttpcnt+1 { count entry }
  end;

  { recycle tag tracking entry }
  procedure puttag(p: ttp);
  begin
     dispose(p); { release entry }
     ttpcnt := ttpcnt-1 { count entry }
  end;

  { push to with stack }
  procedure pshwth(sl: integer);
  var p: wtp;
  begin
    new(p); { get a new entry }
    p^.next := wthstk; { push to stack }
    wthstk := p;
    p^.sl := sl; { mark level }
    wtpcnt := wtpcnt+1 { count entry }
  end;

  { pop from with stack }
  procedure popwth;
  var p: wtp;
  begin
    if wthstk = nil then begin
      writeln; writeln('*** Compiler error: with stack underflow');
      goto 99
    end else begin
      p := wthstk;
      wthstk := p^.next;
      dispose(p);
      wtpcnt := wtpcnt-1
    end
  end;

  { get copyback buffer }
  procedure getcbb(var p: cbbufp; id: ctp; size: integer);
    var lp, fp: cbbufp;
  begin p := nil;
    if id <> nil then if id^.idtype <> nil then begin
      lp := cbblst; fp := nil;
      { find a matching buffer }
      while lp <> nil do begin
        if (lp^.id = nil) and (lp^.size = size) then fp := lp;
        lp := lp^.next
      end;
      if fp = nil then begin { no existing buffer, or all occupied, get new }
        new(fp); fp^.next := lp; lp := fp; fp^.addr := gc;
        gc := gc+size
      end;
      fp^.id := id;
      fp^.size := size;
      p := fp
    end
  end;

  { release copyback buffer }
  procedure putcbb(p: cbbufp);
  begin
    if p <> nil then p^.id := nil
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

  { find padded length of variable length id string }
  function lenpv(s: strvsp): integer;
  var lc, cc, i: integer;
  begin lc := 0; cc := 0;
    while s <> nil do begin
      for i := 1 to varsqt do begin
        cc := cc+1; if s^.str[i] <> ' ' then lc := cc
      end;
      s := s^.next
    end;
    lenpv := lc
  end;

  { write padded string to file }
  procedure writevp(var f: text; s: strvsp);
  var l, cc, i: integer;
  begin l := lenpv(s); cc := 0;
    while s <> nil do begin
      for i := 1 to varsqt do begin
        cc := cc+1; if cc <= l then write(f, s^.str[i])
      end;
      s := s^.next
    end
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

procedure errorv(v: integer);

begin
  errore(v)
end;

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

#ifdef GNU_PASCAL
#include "extend_gnu_pascal.inc"
#endif

#ifdef ISO7185_PASCAL
#include "extend_iso7185_pascal.inc"
#endif

#ifdef PASCALINE
#include "extend_pascaline.inc"
#endif

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

  { this block of functions wraps source reads ******************************* }

  function incact: boolean;
  begin
    incact := incstk <> nil
  end;

  function fileeof: boolean;
  begin
    if incact then fileeof := eof(incstk^.f) else fileeof := eof(prd);
  end;

  function fileeoln: boolean;
  begin
    if incact then fileeoln := eoln(incstk^.f) 
    else fileeoln := eoln(prd);
  end;

  procedure readline;
  var ovf: boolean;
      i: lininx;
  begin
    ovf := false;
    srclen := 0; srcinx := 1; for i := 1 to maxlin do srclin[i] := ' ';
    if not fileeof then begin
      while not fileeoln do begin
        if incact then read(incstk^.f, srclin[srcinx])
        else read(prd, srclin[srcinx]); 
        if srclen = maxlin-1 then begin
          if not ovf then 
            begin writeln; writeln('*** Input line too long, truncated') end;
          ovf := true
        end else begin srclen := srclen+1; srcinx := srcinx+1 end
      end;
      if incact then readln(incstk^.f)
      else readln(prd);   
    end;
    srcinx := 1;
    if prcode then
      if srclen = 0 then writeln(prr, '!') 
      else writeln(prr, '! ', srclin:srclen)
  end;

  function eofinp: boolean;
  begin
    if srclen <> 0 then eofinp := false else eofinp := fileeof
  end;

  function eolninp: boolean;
  begin
    if eofinp then eolninp := true
    else if srcinx > srclen then eolninp := true
    else eolninp := false
  end;

  function bufinp: char;
  begin
    if not eolninp then bufinp := srclin[srcinx] else bufinp := ' '
  end;

  procedure readinp(var c: char);
  begin
    c := bufinp;
    if eolninp then readline else srcinx := srcinx+1
  end;

  { ************************************************************************** }

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
    25:  write('Illegal source character');
    26:  write('String constant too long');
    27:  write(''','' or '')'' expected');
    28:  write('''array'' expected');
    29:  write(''','' or ''end'' expected');
    30:  write('''..'' expected');

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
    120: write('Function result type must be scalar, subrange or pointer');
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
    155: write('Control variable must not be declared on intermediate');
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
    200: write('Tagfield constants must cover entire tagfield type');
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
    236: write('Type error in write');
    237: write('Array size too large');
    238: write('Invalid array length, must be >= 1');
    239: write('Variant case exceeds allowable range');
    240: write('Header parameter already included');
    241: write('Invalid tolken separator');
    242: write('Identifier referenced before defining point');
    243: write('Initializer expression must be integer');
    244: write('Type incorrect for fixed');
    245: write('Initializer incompatible with fixed element');
    246: write('Initializer out of range of fixed element type');
    247: write('Incorrect number of initializers for type');
    248: write('Fixed cannot contain variant record');
    249: write('New overload ambiguous with previous');
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
    269: write('Number of initializers for parameterised declaration do not ',
               'match');
    270: write('Container array type specified without initializer(s)');
    271: write('Number of initializers does not match container array levels');
    272: write('Cannot declare container array in fixed context');
    273: write('Must be container array');
    274: write('Function result type must be scalar, subrange, pointer, set, ',
               'array or record');
    275: write('Number of parameters does not agree with declaration of any ',
               'overload');
    276: write('Different overload parameters converge with different modes');
    277: write('No overload found to match parameter');
    278: write('Must be variable reference');
    279: write('''procedure'', ''function'' or ''operator'' expected');
    280: write('Attribute has no meaning used on operator overload');
    281: write('Expression/assignment operator expected');
    282: write('Overload operator is ambiguous with system operator');
    283: write('New operator overload ambiguous with previous');
    284: write('Different operator overload parameters converge with ',
               'different modes');
    285: write('Parameter type not allowed in operator overload parameter ');
    286: write('Parameter mode not allowed in operator overload parameter ');
    287: write('Variable reference is not addressable');
    288: write('Left side of assignment overload operator must be out mode');
    289: write('Var parameter must be compatible with parameter');
    290: write('Cannot threaten view parameter');
    291: write('Set element out of implementation range');
    292: write('Function expected in this context');
    293: write('Procedure expected in this context');
    294: write('Cannot forward an external declaration');
    295: write('procedure or function previously declared external');

    300: write('Division by zero');
    301: write('No case provided for this value');
    302: write('Index expression out of bounds');
    303: write('Value to be assigned is out of bounds');
    304: write('Element expression out of range');
    305: write('Cannot use non-decimal with real format');
    306: write('Integer overflow');

    397: write('Feature not valid in ISO 7185 Pascal');
    398: write('Implementation restriction');
    { as of the implementation of full ISO 7185, this error is no longer used }
    399: write('Feature not implemented');

    { * marks spared compiler errors }
    400,401,402,403,404,406,407,
    500,501,502,503,
    504,505,506,507,508,509,510,511,512,513,514,515,516: write('Compiler internal error');
    end
  end;

  procedure endofline;
    var lastpos,freepos,currpos,currnmr,f,j,k: integer; df: boolean;
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
        writeln; 
        if experr then begin
          for k := 1 to errinx do 
            begin df := false;
              for j := 1 to k-1 do 
                if errlist[j].nmr = errlist[k].nmr then df := true;
              if not df then begin
                write(linecount:6,' ****  ':9); write(errlist[k].nmr:3, ' ');
                errmsg(errlist[k].nmr); writeln
              end
            end
        end;
        errinx := 0;
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
      if not eofinp and prcode then begin
        writeln(prr, ':', lineout:1);
      end
    end
  end;

  procedure markline;
  begin
    outline;
    if prcode then writeln(prr, ':', linecount:1)
  end;

  { check in private section }
  function inpriv: boolean;
  begin inpriv := false;
    if incact then inpriv := incstk^.priv
  end;

  procedure error(ferrnr: integer);
  begin
    if not incact then begin { supress errors in includes }

      { This diagnostic is here because error buffers error numbers til the end
        of line, and sometimes you need to know exactly where they occurred. }

#ifdef IMM_ERR
      writeln; writeln('error: ', ferrnr:1);
#endif

      errtbl[ferrnr] := errtbl[ferrnr]+1; { track this error }
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

  procedure prtsym(sy: symbol);
  begin
    case sy of
      ident: write('ident'); intconst: write('intconst');
      realconst: write('realconst'); stringconst: write('string const');
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
    end
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
        strend: boolean;

    procedure nextch;
    begin if eol then
      begin if list then writeln(output); endofline end;
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
      optst: optstr; oni: optinx; oi: 1..maxopt;
      procedure switch(var opt: boolean);
      var oni: optinx;
      begin
        if (ch='+') or (ch='-') then begin
          opt := ch = '+';
          option[oi] := opt;
          if prcode then begin
            write(prr, 'o', ' ':7);
            for oni :=  1 to optlen do 
              if optsl[oi, oni] <> ' ' then write(prr, optsl[oi, oni]);
            writeln(prr, ch)
          end;
          nextch;
        end else begin { just default to on }
          opt := true;
          option[oi] := true;
          if prcode then begin
            write(prr, 'o', ' ':7);
            for oni :=  1 to optlen do 
              if optsl[oi, oni] <> ' ' then write(prr, optsl[oi, oni]);
            writeln(prr, '+')
          end
        end
      end; { switch() }
    begin { options() }
      nextch;
      repeat
        oni := 1; optst := '          ';
        while ch in ['a'..'z', 'A'..'Z', '0'..'9'] do begin
          ch1 := lcase(ch); 
          if optst[oni] = ' ' then optst[oni] := ch1; 
          if oni < optlen then oni := oni+1;
          nextch
        end;
        oi := 1;
        while (oi < maxopt) and (optst <> opts[oi]) and (optst <> optsl[oi]) do
          oi := oi+1;
        if (optst = opts[oi]) or (optst = optsl[oi]) then case oi of
          1:  switch(dummy);
          2:  switch(doprtlab);
          3:  switch(prcode);
          4:  switch(debug);
          5:  switch(dummy);
          6:  switch(dummy);
          7:  switch(dummy);
          8:  switch(dummy);
          9:  switch(chkvbk);
          10: switch(experr);
          11: switch(dummy);
          12: if not incact then begin 
                switch(list); if not list then writeln(output)
              end;
          13: switch(dummy);
          14: switch(dummy);
          15: switch(dummy);
          16: switch(dummy);
          17: switch(dummy);
          18: switch(chkref);
          19: switch(iso7185);
          20: switch(prtables);
          21: switch(chkudtc);
          22: switch(chkvar);
          23: switch(dummy);
          24: switch(dodmplex);
          25: switch(dodmpdsp);
          26: switch(dummy);
        end else begin
          { skip all likely option chars }
          while ch in ['a'..'z','A'..'Z','+','-','0'..'9','_'] do
            nextch;
        end;
        ch1 := ch; if ch1 = ',' then nextch
      until ch1 <> ','
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

    procedure plcchr(c: char);
    begin
      if not eol then begin
        lgth := lgth+1;
        if lgth <= strglgth then string[lgth] := c
      end
    end;

    procedure escchr;
    type escstr = packed array [1..5] of char; { escape string }
    var c: char; l: 0..4; si: lininx; i: 1..5;

    function match(es: escstr): boolean;
    var i: 1..5;
    begin
      i := 1;
      { move to first mismatch or end }
      while (es[i] = srclin[si+i-1]) and (es[i] <> ' ') and (i <= 4) do i := i+1;
      match := es[i] = ' '
    end;

    begin
      si := srcinx-1; { move back to after '\' }
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
      if c <> ' ' then begin { found escape }
        plcchr(c);
        for i := 1 to l do nextch { skip escape sequence }
      end else { place common forced }
        begin plcchr(ch); nextch end
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
        error(25); nextch
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
          until not (chartp[ch] in [letter, number]);
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
                  if iso7185 and ((sy >= forwardsy) or (op > noop)) then
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
                if v <= pmmaxint div r then
                  v := v*r+ordint[ch]
                else begin error(203); v := 0 end;
              nextch
            until (chartp[ch] <> number) and ((ch <> '_') or iso7185) and
                  ((chartp[ch] <> letter) or (r < 16) or iso7185);
            { separator must be non-alpha numeric or 'e' with decimal radix }
            if ((chartp[ch] = letter) and not ((lcase(ch) = 'e') and (r = 10))) or
               (chartp[ch] = number) then error(241);
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
                      if i > maxexp then begin
                         i := 0;
                         if ferr then error(194)
                      end;
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
        begin nextch; lgth := 0; sy := stringconst; op := noop; strend := false;
          for i := 1 to strglgth do string[i] := ' ';
          repeat 
            { force character if '\' and not ISO 7185 mode }
            if (ch = chr(92)) and not iso7185 then begin
              nextch; { skip '\' }
              if ch in ['$','&','%','0'..'9'] then begin 
                { character code }
                j := i+1; v := 0; k := 1;
                { parse in radix and only correct number of digits to keep from
                  eating follow on characters }
                if ch = '$' then begin nextch;
                  if not (ch in ['0'..'9','a'..'f','A'..'F']) then error(207);
                  while (ch in ['0'..'9', 'a'..'f', 'A'..'F']) and
                        (k <= 2) do begin
                    v := v*16+ordint[ch]; nextch; k := k+1
                  end
                end else if ch = '&' then begin nextch;
                  if not (ch in ['0'..'7']) then error(207);
                  while (ch in ['0'..'7']) and (k <= 3) do begin
                    v := v*8+ordint[ch]; nextch; k := k+1
                  end
                end else if ch = '%' then begin nextch;
                  if not (ch in ['0'..'1']) then error(207);
                  while (ch in ['0'..'1']) and (k <= 8) do begin
                    v := v*2+ordint[ch]; nextch; k := k+1
                  end
                end else begin
                  while (ch in ['0'..'9']) and (k <= 3) do begin
                    v := v*10+ordint[ch]; nextch; k := k+1
                  end
                end;
                if v > ordmaxchar then error(222);
                plcchr(chr(v));
              end else escchr { process force sequence }
            end else if ch = '''' then
              begin nextch; 
                if ch = '''' then 
                  begin plcchr(ch); nextch end else strend := true
              end
            else begin plcchr(ch); nextch end { place regular char }
          until eol or strend;
          if eol and not strend then error(202);
          if lgth = 1 then begin
            { this is an artifact of the original code. If the string is a
              single character, we store it as an integer even though the
              symbol stays a string }
            val.intval := true; val.ival := ord(string[1])
          end else begin
            if (lgth = 0) and iso7185 then error(205);
            new(lvp,strg); pshcst(lvp);
            lvp^.cclass:=strg;
            if lgth > strglgth then
              begin error(26); lgth := strglgth end;
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
             if (ch = '$') and not incact then options;
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
      write('symbol: '); prtsym(sy);
      if sy in [ident,intconst,realconst,stringconst] then
        case sy of
          ident:       write(': ', id:10);
          intconst:    write(': ', val.ival:1);
          realconst:   write(': ', val.valp^.rval: 9);
          stringconst: begin write('string const: ''');
                             writev(output, val.valp^.sval, val.valp^.slgth);
                             write('''') end;
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

  procedure prtclass(klass: idclass);
  begin
    case klass of
      types:  write('types');
      konst:  write('konst');
      fixedt: write('fixedt');
      vars:   write('vars');
      field:  write('field');
      proc:   write('proc');
      func:   write('func');
      alias:  write('alias');
    end
  end;

  procedure prtform(form: structform);
  begin
    case form of
      scalar:   write('scalar');
      subrange: write('subrange');
      pointer:  write('pointer');
      power:    write('power');
      arrays:   write('arrays');
      arrayc:   write('arrayc');
      records:  write('records');
      files:    write('files');
      tagfld:   write('tagfld');
      variant:  write('variant');
      exceptf:  write('exceptf');
    end
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
            if incact then begin
              writeln; write('*** Duplicate in uses/joins: ');
              writevp(output, fcp^.name);
              writeln
            end;
            { give appropriate error }
            if lcp^.klass = alias then error(242) else error(101);
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
1:  if fcp <> nil then
      if fcp^.klass = alias then fcp := fcp^.actid;
    fcp1 := fcp
  end (*searchsection*) ;

  procedure schsecidnenm(lcp: ctp; fidcls: setofids; var fcp: ctp;
                         var mm: boolean);
  var lcp1: ctp;

  function inclass(lcp: ctp): ctp;
  var fcp: ctp;
  begin fcp := nil;
    if lcp^.klass in [proc,func] then begin
      lcp := lcp^.grppar;
      while lcp <> nil do begin
        if lcp^.klass in fidcls then fcp := lcp;
        lcp := lcp^.grpnxt
      end
    end else if lcp^.klass in fidcls then fcp := lcp;
    inclass := fcp
  end;

  begin
    mm := false; fcp := nil;
    while lcp <> nil do begin
      if strequvf(lcp^.name, id) then begin
        lcp1 := lcp; if lcp1^.klass = alias then lcp1 := lcp1^.actid;
        lcp1 := inclass(lcp1);
        if lcp1 <> nil then begin fcp := lcp1; lcp := nil end
        else begin mm := true; lcp := lcp^.rlink end
      end else
        if strltnvf(lcp^.name, id) then lcp := lcp^.rlink
        else lcp := lcp^.llink
    end
  end (*searchidnenm*) ;

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
  end (*searchidnenm*) ;

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
    var lcp, lcp1: ctp; pn, fpn: disprange; pdf: boolean;
  begin
    pdf := false;
    searchidne(fidcls, lcp); { perform no error search }
    if lcp = nil then begin
      { search module leader in the pile }
      if ptop > 0 then for pn := ptop-1 downto 0 do
        if strequvf(pile[pn].modnam, id) then begin fpn := pn; pdf := true end;
      if pdf then begin { module name was found }
        insymbol; if sy <> period then error(21) else insymbol;
        if sy <> ident then error(2)
        else schsecidne(pile[fpn].fname,fidcls,lcp); { search qualifed name }
        if lcp = nil then begin error(268); pdf := false end { not found }
      end
    end;
    if lcp <> nil then begin { found }
      lcp^.refer := true;
      if (disx <> top) and (display[top].define) and not pdf then begin
        { downlevel, create an alias and link to bottom }
        new(lcp1, alias); ininam(lcp1); lcp1^.klass := alias;
        lcp1^.name := lcp^.name; lcp1^.actid := lcp;
        enterid(lcp1)
      end
    end else begin (*search not successful
      --> procedure simpletype*)
      error(104);
      (*to avoid returning nil, reference an entry
       for an undeclared id of appropriate class
       --> procedure enterundecl*)
      if types in fidcls then lcp := utypptr
      else
        if (vars in fidcls) or (fixedt in fidcls) then lcp := uvarptr
        else
          if field in fidcls then lcp := ufldptr
          else
            if konst in fidcls then lcp := ucstptr
            else
              if proc in fidcls then lcp := uprcptr
              else lcp := ufctptr
    end;
    fcp := lcp
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
              begin fmin := -pmmaxint; fmax := pmmaxint
              end
            else
              if fconst <> nil then
                fmax := fconst^.values.ival
  end (*getbounds*) ;

  { get span of type }
  function span(fsp: stp): integer;
  var fmin, fmax: integer;
  begin
    getbounds(fsp, fmin, fmax); span := fmax-fmin+1
  end;

  { get span of array index }
  function spana(fsp: stp): integer;
  begin
    if fsp <> nil then begin
      if fsp^.form <> arrays then error(512);
      { if the index type is nil, assume string and take the array size as the
        span }
      if fsp^.inxtype = nil then spana := fsp^.size
      else spana := span(fsp^.inxtype)
    end
  end;

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
          arrayc:   alignquot := alignquot(abstype);
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
    if (flc mod k) <> 0 then begin
      l := flc+1;
      flc := l - k  +  (k-l) mod k
    end
  end (*align*);

  { align for stack }
  function aligns(flc: addrrange): addrrange;
    var l: integer;
  begin
    if (flc mod stackal) <> 0 then begin
      l := flc+1;
      flc := l - stackal  +  (stackal-l) mod stackal
    end;
    aligns := flc
  end (*aligns*);

  procedure wrtctp(ip: ctp);
  begin
    if ip = nil then write('<nil>':intdig) else write(ip^.snm:intdig)
  end;

  procedure wrtstp(sp: stp);
  begin
    if sp = nil then write('<nil>':intdig) else write(sp^.snm:intdig)
  end;

  procedure prtstp(sp: stp);
  begin
    if sp = nil then write('<nil>':intdig)
    else with sp^ do begin
      write(sp^.snm:intdig);
      write(' ', size:intdig, ' ');
      case form of
        scalar:   begin write('scalar':intdig, ' ');
                    if scalkind = standard then write('standard':intdig)
                    else begin write('declared':intdig,' '); wrtctp(fconst) end
                  end;
        subrange: begin
                    write('subrange':intdig,' '); wrtstp(rangetype); write(' ');
                    if rangetype <> realptr then
                      write(min.ival:intdig, ' ', max.ival:intdig)
                    else
                      if (min.valp <> nil) and (max.valp <> nil) then begin
                        write(' '); write(min.valp^.rval:9);
                        write(' '); write(max.valp^.rval:9)
                      end
                  end;
        pointer:  begin write('pointer':intdig,' '); wrtstp(eltype) end;
        power:    begin write('set':intdig,' '); wrtstp(elset); write(' ');
                        write(matchpack:intdig) end;
        arrays:   begin
                    write('array':intdig,' '); wrtstp(inxtype); write(' ');
                    wrtstp(aeltype); end;
        arrayc:   begin write('array':intdig,' '); wrtstp(abstype) end;
        records:  begin
                    write('record':intdig,' '); wrtctp(fstfld); write(' ');
                    wrtstp(recvar); write(' '); wrtstp(recyc)
                  end;
        files:    begin write('file':intdig,' '); wrtstp(filtype) end;
        tagfld:   begin write('tagfld':intdig,' '); wrtctp(tagfieldp);
                    write(' '); wrtstp(fstvar)
                  end;
        variant:  begin write('variant':intdig,' '); wrtstp(nxtvar);
                    write(' '); wrtstp(subvar); write(' '); wrtstp(caslst);
                    write(' '); wrtctp(varfld);
                    write(' ',varval.ival:intdig, ' ', varln:intdig)
                  end;
        exceptf:  begin write('except':intdig) end
      end (*case*)
    end
  end;

  procedure prtctp(cp: ctp);
  begin
    if cp = nil then write('<nil>':intdig)
    else with cp^ do begin
      write(cp^.snm:intdig); write(' '); writev(output, name, intdig);
      write(' '); wrtctp(llink); write(' '); wrtctp(rlink); write(' ');
      wrtstp(idtype); write(' ');
      case klass of
        types: write('type':intdig);
        konst: begin write('constant':intdig,' '); wrtctp(next); write(' ');
                 if idtype <> nil then
                   if idtype = realptr then
                     begin
                       if values.valp <> nil then write(values.valp^.rval:9)
                     end
                   else
                     if idtype^.form = arrays then  (*stringconst*)
                       begin
                         if values.valp <> nil then
                           begin
                             with values.valp^ do writev(output, sval, slgth)
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
                 if hdr then write('header':intdig, ' ') else write(' ':intdig, ' ');
                 if vext then write('external':intdig, ' ') else write(' ':intdig, ' ');
                 if vext then write(vmod^.fn:intdig, ' ') else write(' ':intdig, ' ');
                 write(inilab:intdig, ' '); wrtctp(ininxt);
                 write(' ', dblptr:intdig);
               end;
        fixedt: begin write('fixed':intdig, ' ', floc:intdig, ' ');
                 if fext then write('external':intdig) else write(' ':intdig);
                 if fext then write(fmod^.fn:intdig) else write(' ':intdig)
               end;
        field: begin write('field':intdig,' '); wrtctp(next); write(' ');
                     write(fldaddr:intdig,' '); wrtstp(varnt); write(' ');
                     wrtctp(varlb); write(' ');
                 if tagfield then write('tagfield':intdig) else write(' ':intdig);
                 write(' ', taglvl:intdig, ' ',varsaddr:intdig, ' ', varssize:intdig);
                 write(' ', vartl:intdig)
               end;
        proc,
        func:  begin
                 if klass = proc then write('procedure':intdig, ' ')
                 else write('function':intdig, ' ');
                 write(pfaddr:intdig, ' '); wrtctp(pflist); write(' ');
                 if asgn then write('assigned':intdig, ' ') else write(' ':intdig, ' ');
                 if pext then write('external':intdig, ' ') else write(' ':intdig, ' ');
                 if pext then write(pmod^.fn:intdig) else write(' ':intdig); write(' ');
                 case pfattr of
                   fpanone: write(' ':intdig);
                   fpaoverload: write('overload':intdig);
                   fpastatic: write('static':intdig);
                   fpavirtual: write('virtual':intdig);
                   fpaoverride: write('override': intdig);
                 end;
                 write(' ', pfvaddr:intdig, ' '); wrtctp(pfvid); write(' '); wrtctp(grppar);
                 write(' '); wrtctp(grpnxt); write(' ');
                 if pfdeckind = standard then
                   write('standard':intdig, ' ', key:intdig)
                 else
                   begin write('declared':intdig,' '); wrtctp(pflist); write(' ');
                     write(pflev:intdig,' ',pfname:intdig, ' ');
                     if pfkind = actual then
                       begin write('actual':intdig, ' ');
                         if forwdecl then write('forward':intdig, ' ')
                         else write('not forward':intdig, ' ');
                         if sysrot then write('system routine':intdig)
                         else write('not system routine':intdig);
                         if extern then write('external':intdig)
                         else write('not external':intdig)
                       end
                     else write('formal':intdig)
                   end
               end;
        alias: begin write('alias':intdig, ' '); wrtctp(actid); end;
      end (*case*);
    end
  end;

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
              arrayc:   markstp(abstype);
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

    procedure followctp(fp: ctp); forward;

    procedure followstp(fp: stp);
    begin
      if fp <> nil then
        with fp^ do
          if marked then
            begin marked := false; write('S: '); prtstp(fp); writeln;
              case form of
                scalar:   ;
                subrange: followstp(rangetype);
                pointer:  ;
                power:    followstp(elset);
                arrays:   begin followstp(aeltype); followstp(inxtype) end;
                arrayc:   followstp(abstype);
                records:  begin followctp(fstfld); followstp(recvar) end;
                files:    followstp(filtype);
                tagfld:   followstp(fstvar);
                variant:  begin followstp(nxtvar); followstp(subvar) end;
                exceptf:  ;
              end (*case*)
            end (*if marked*)
    end (*followstp*);

    procedure followctp{(fp: ctp)};
    begin
      if fp <> nil then
        with fp^ do
          begin write('C: '); prtctp(fp); writeln;
            followctp(llink); followctp(rlink);
            followstp(idtype)
          end (*with*)
    end (*followctp*);

  begin (*printtables*)
    writeln(output); writeln(output); writeln(output);
    if fb then lim := 0
    else begin lim := top; write(' local') end;
    writeln(' tables:', top:1, '-', lim:1, ':'); writeln(output);
    writeln('C: ', 'Entry #':intdig, ' ', 'Id':intdig, ' ', 'llink':intdig, ' ',
            'rlink':intdig, ' ', 'Typ':intdig, ' ', 'Class':intdig);
    writeln('S: ', 'Entry #':intdig, ' ', 'Size':intdig, ' ', 'Form ':intdig);
    write('===============================================================');
    writeln('==========================');
    marker;
    for i := top downto lim do
      begin writeln('Level: ', i:1); followctp(display[i].fname) end;
    writeln(output);
    if not eol then write(' ':chcnt+16)
  end (*printtables*);

  procedure chkrefs(p: ctp; var w: boolean);
  begin
    if chkref then begin
      if p <> nil then begin
        chkrefs(p^.llink, w); { check left }
        chkrefs(p^.rlink, w); { check right }
        if not p^.refer and (p^.klass <> alias) and not incact then begin
          if not w then writeln; writev(output, p^.name, 10);
          writeln(' unreferenced'); w := true
        end
      end
    end
  end;

  function chkext(fcp: ctp): boolean;
  begin chkext := false;
    if fcp <> nil then begin
      if fcp^.klass = vars then chkext := fcp^.vext
      else if fcp^.klass = fixedt then chkext := fcp^.fext
      else if (fcp^.klass = proc) or (fcp^.klass = func) then
        chkext := fcp^.pext
    end
  end;

  function chkfix(fcp: ctp): boolean;
  begin chkfix := false;
    if fcp <> nil then chkfix := fcp^.klass = fixedt
  end;

  { id contains a procedure in overload list }
  function hasovlproc(fcp: ctp): boolean;
  begin hasovlproc := false;
    if fcp <> nil then
      if fcp^.klass in [proc, func] then begin
        fcp := fcp^.grppar;
        while fcp <> nil do begin
          if fcp^.klass = proc then hasovlproc := true;
          fcp := fcp^.grpnxt
        end
      end
  end;

  { id contains a function in overload list }
  function hasovlfunc(fcp: ctp): boolean;
  begin hasovlfunc := false;
    if fcp <> nil then
      if fcp^.klass in [proc, func] then begin
        fcp := fcp^.grppar;
        while fcp <> nil do begin
          if fcp^.klass = func then hasovlfunc := true;
          fcp := fcp^.grpnxt
        end
      end
  end;

  { return overload function from list }
  function ovlfunc(fcp: ctp): ctp;
  var rcp: ctp;
  begin rcp := nil;
    if fcp <> nil then
      if fcp^.klass in [proc, func] then begin
        fcp := fcp^.grppar;
        while fcp <> nil do begin
          if fcp^.klass = func then rcp := fcp;
          fcp := fcp^.grpnxt
        end
      end;
    ovlfunc := rcp
  end;

  { return override procedure/function from list }
  function ovrpf(fcp: ctp): ctp;
  var rcp: ctp;
  begin rcp := nil;
    if fcp <> nil then
      if fcp^.klass in [proc, func] then begin
        fcp := fcp^.grppar;
        while fcp <> nil do begin
          if fcp^.pfattr = fpaoverride then rcp := fcp;
          fcp := fcp^.grpnxt
        end
      end;
    ovrpf := rcp
  end;

  procedure genlabel(var nxtlab: integer);
  begin intlabel := intlabel + 1;
    nxtlab := intlabel
  end (*genlabel*);

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
      arrayc: begin wrtchr('v'); wrttypsub(abstype) end;
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

  procedure prtlabel(labname: integer);
  begin 
    if prcode then begin
      write(prr, 'l '); writevp(prr, nammod); write(prr, '.', labname:1)
    end
  end;

  procedure prtflabel(fcp: ctp);
  begin
    if prcode then begin
      write(prr, 'l ');
      if fcp^.klass = vars then writevp(prr, fcp^.vmod^.mn)
      else if fcp^.klass = fixedt then writevp(prr, fcp^.fmod^.mn)
      else writevp(prr, fcp^.pmod^.mn);
      write(prr, '.');
      writevp(prr, fcp^.name)
    end
  end;

  procedure prtpartyp(fcp: ctp);
  var plst: ctp;
  begin
    plst := fcp^.pflist;
    while plst <> nil do begin
      wrttyp(prr, plst^.idtype);
      if plst^.next <> nil then write(prr, '_');
      plst := plst^.next
    end
  end;

  procedure putlabel(labname: integer);
  begin
    if prcode then begin prtlabel(labname); writeln(prr) end
  end (*putlabel*);

  procedure searchlabel(var llp: lbp; level: disprange; isid: boolean);
  var fllp: lbp; { found label entry }
      lv: integer;
  begin lv := -1;
    if not isid then if val.intval then lv := val.ival;
    fllp := nil; { set no label found }
    llp := display[level].flabel; { index top of label list }
    while llp <> nil do begin { traverse }
      if isid and (llp^.labid <> nil) then begin { id type label }
        if strequvf(llp^.labid, id) then begin
          fllp := llp; { set entry found }
          llp := nil { stop }
        end else llp := llp^.nextlab { next in list }
      end else if not isid and (llp^.labval = lv) then begin { found }
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
            ipcref := false; minlvl := pmmaxint; bact := false;
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
           records,arrays,arrayc: ss := 7;
           tagfld,variant: error(501)
          end;
      mestn := ss
    end;

  begin (*mest*)
    if (cdx[i] < 1) or (cdx[i] > 6) then error(502);
    mesl(cdxs[cdx[i]][mestn(fsp)]);
  end (*mest*);

  procedure gen0(fop: oprange);
  begin
    if prcode then writeln(prr,mn[fop]:11);
    ic := ic + 1; mes(fop)
  end (*gen0*) ;

  procedure gen1s(fop: oprange; fp2: integer; symptr: ctp);
    var k, j: integer; p: strvsp;
  begin
    if prcode then
      begin write(prr,mn[fop]:11);
        if fop = 30 then
          begin writeln(prr,' ':5,sna[fp2]:4);
            mesl(pdx[fp2]);
          end
        else
          begin
            if fop = 38 then
               begin with cstptr[fp2]^ do begin p := sval; j := 1;
                   write(prr,' ':5,slgth:1,' ''');
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
            else if fop = 67 then writeln(prr,' ':5,fp2:1)
            else if fop = 105 then begin write(prr,' ':5); putlabel(fp2) end
            else if chkext(symptr) then
              begin write(prr,' ':5); prtflabel(symptr); writeln(prr) end
            else if chkfix(symptr) then
              begin write(prr,' ':5); prtlabel(symptr^.floc); writeln(prr) end
            else writeln(prr,' ':5,fp2:1);
            if fop = 42 then mes(0)
            else if fop = 71 then mesl(fp2)
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
      begin write(prr,mn[fop]:11);
        case fop of
          42: begin
            writeln(prr,chr(fp1),' ':4,fp2:1);
            mes(0)
          end;
          45,50,54,56,74,62,63,81,82,96,97,102,104,109,112,115,116,117:
            begin
              writeln(prr,' ':5,fp1:1,' ',fp2:1);
              if fop = 116 then mesl(-fp2)
              else if fop = 117 then mesl(fp2-fp1)
              else mes(fop)
            end;
          47,48,49,52,53,55:
            begin write(prr,chr(fp1));
              if chr(fp1) = 'm' then write(prr,' ':4,fp2:1);
              writeln(prr);
              case chr(fp1) of
                'i': mesl(cdxs[cdx[fop]][1]);
                'r': mesl(cdxs[cdx[fop]][2]);
                'b': mesl(cdxs[cdx[fop]][3]);
                'c': mesl(cdxs[cdx[fop]][4]);
                'a': mesl(cdxs[cdx[fop]][5]);
                's': mesl(cdxs[cdx[fop]][6]);
                'm': mesl(cdxs[cdx[fop]][7]);
                'v': mesl(cdxs[cdx[fop]][8]);
              end
            end;
          51:
            begin
              case fp1 of
                1: begin writeln(prr,'i',' ':4,fp2:1);
                     mesl(cdxs[cdx[fop]][1])
                   end;
                2: begin write(prr,'r',' ':4);
                     with cstptr[fp2]^ do write(prr,rval:23);
                     writeln(prr);
                     mesl(cdxs[cdx[fop]][2]);
                   end;
                3: begin writeln(prr,'b',' ':4,fp2:1);
                     mesl(cdxs[cdx[fop]][3])
                   end;
                4: begin writeln(prr,'n');
                     mesl(-ptrsize)
                   end;
                6: begin
                if chartp[chr(fp2)] = illegal then
                     { output illegal characters as numbers }
                     writeln(prr,'c',' ':4,fp2:1)
                   else
                     writeln(prr,'c',' ':4, '''',chr(fp2),'''');
                     mesl(cdxs[cdx[fop]][4])
                   end;
                5: begin write(prr,'s',' ':4, '(');
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
    if (fsp <> nil) and prcode then
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
         records,arrays,arrayc: write(prr,'m');
         tagfld,variant: error(503)
        end
  end (*typindicator*);

  procedure gen0t(fop: oprange; fsp: stp);
  begin
    if prcode then
      begin
        write(prr,mn[fop]:11);
        gentypindicator(fsp);
        writeln(prr);
      end;
    ic := ic + 1; mest(fop, fsp)
  end (*gen0t*);

  procedure gen1ts(fop: oprange; fp2: integer; fsp: stp; symptr: ctp);
  begin
    if prcode then
      begin
        write(prr,mn[fop]:11);
        gentypindicator(fsp);
        write(prr, ' ':4);
        if chkext(symptr) then prtflabel(symptr)
        else if chkfix(symptr) then prtlabel(symptr^.floc)
        else write(prr,fp2:1);
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
      begin
        write(prr,mn[fop]:11);
        gentypindicator(fsp);
        writeln(prr,' ':4, fp1:1,' ',fp2:1);
      end;
    ic := ic + 1; mest(fop, fsp)
  end (*gen2t*);

  procedure genujpxjpcal(fop: oprange; fp2: integer);
  begin
   if prcode then
      begin write(prr,mn[fop]:11, ' ':5); prtlabel(fp2); writeln(prr) end;
    ic := ic + 1; mes(fop)
  end (*genujpxjpcal*);

  procedure gencjp(fop: oprange; fp1,fp2,fp3: integer);
  begin
   if prcode then
      begin
        write(prr,mn[fop]:11, ' ':5, fp1:3+5*ord(abs(fp1)>99),' ',fp2:11,
                    ' '); prtlabel(fp3); writeln(prr)
      end;
    ic := ic + 1; mes(fop)
  end (*gencjp*);

  procedure genipj(fop: oprange; fp1, fp2: integer);
  begin
   if prcode then
      begin write(prr,mn[fop]:11,' ':5,fp1:1,' '); prtlabel(fp2); writeln(prr) end;
    ic := ic + 1; mes(fop)
  end (*genipj*);

  procedure gencupcuf(fop: oprange; fp1,fp2: integer; fcp: ctp);
  begin
    if prcode then
      begin
        write(prr,mn[fop]:11, ' ':5);
        if chkext(fcp) then begin
          prtflabel(fcp);
          write(prr, '@'); { this keeps the user from aliasing it }
          if fcp^.klass = proc then write(prr, 'p') else write(prr, 'f');
          if fcp^.pflist <> nil then begin
            write(prr, '_');
            prtpartyp(fcp)
          end
        end else prtlabel(fp2);
        writeln(prr);
        mesl(fp1)
      end;
    ic := ic + 1
  end;

  procedure gencuv(fp1,fp2: integer; fcp: ctp);
  begin
    if prcode then
      begin
        write(prr,mn[91(*cuv*)]:11,' ':5);
        if chkext(fcp) then prtflabel(fcp) else writeln(prr,fp2:12);
        writeln(prr);
        mes(91)
      end;
    ic := ic + 1
  end;

  procedure genlpa(fp1,fp2: integer);
  begin
    if prcode then
      begin
        write(prr,mn[68(*lpa*)]:11,' ':5, fp2:4, ' '); prtlabel(fp1); writeln(prr);
      end;
    ic := ic + 1; mes(68)
  end (*genlpa*);

  procedure gensuv(fp1, fp2: integer; sym: ctp);
  begin
    if prcode then begin
      write(prr,mn[92(*suv*)]:11, ' ':5); prtlabel(fp1);
      if chkext(sym) then
        begin write(prr, ' '); prtflabel(sym); writeln(prr) end
      else writeln(prr, ' ', fp2:1)
    end;
    ic := ic + 1; mes(92)
  end;

  procedure genctaivtcvb(fop: oprange; fp1,fp2,fp3: integer; fsp: stp);
  begin if fp3 < 0 then error(511);
    if prcode then
      begin write(prr,mn[fop]:11);
            if fop <> 81(*cta*) then gentypindicator(fsp);
            write(prr,' ':4,fp1:1,' ',fp2:1,' ');
            mes(fop); putlabel(fp3)
      end;
    ic := ic + 1
  end (*genctaivtcvb*) ;

  procedure gensfr(lb: integer);
  begin
    if prcode then begin
      write(prr,mn[121(*sfr*)]:11, ' ':5); prtlabel(lb); writeln(prr);
    end
  end;

  procedure genmst(lev: levrange; fp1,fp2: integer);
  begin
    if prcode then begin
      write(prr,mn[41(*mst*)]:11, ' ':5, lev:1, ' '); prtlabel(fp1); 
      write(prr, ' '); prtlabel(fp2); writeln(prr)
    end
  end;

  procedure gensca(c: char);
  begin
    if prcode then begin
      write(prr,mn[(*lca*)38]:11,1:6,' ''');
      if c = '''' then write(prr,'''') else write(prr,c);
      writeln(prr,'''');
      mes(38)
    end
   end;

  function comptypes(fsp1,fsp2: stp) : boolean; forward;

  { check integer or subrange of }
  function intt(fsp: stp): boolean;
    var t: stp;
  begin intt := false;
    if fsp <> nil then begin
      t := basetype(fsp);
      if t = intptr then intt := true
    end
  end;

  { check real }
  function realt(fsp: stp): boolean;
  begin realt := false;
    if fsp <> nil then
      if fsp = realptr then realt := true
  end;

  { the type test for character includes very broad definitions of char,
    including packed character arrays of 1 length, and even packed character
    array containers, because they could be length 1 }
  function chart(fsp: stp): boolean;
    var t: stp; fmin, fmax: integer;
  begin chart := false;
    if fsp <> nil then begin
      t := basetype(fsp);
      if t = charptr then chart := true
      else if (t^.form = arrays) and t^.packing then begin
        if (t^.inxtype = nil) and (t^.size = 1) then chart := true
        else if chart(t^.aeltype) and intt(t^.inxtype) then begin
          getbounds(t^.inxtype,fmin,fmax);
          if (fmin = 1) and (fmax = 1) then chart := true
        end
      end else if (t^.form = arrayc) and t^.packing then begin
        if chart(t^.abstype) then chart := true
      end
    end
  end;

  { check boolean }
  function bolt(fsp: stp): boolean;
    var t: stp;
  begin bolt := false;
    if fsp <> nil then begin
      t := basetype(fsp);
      if t = boolptr then bolt := true
    end
  end;

  function stringt(fsp: stp) : boolean;
  var fmin, fmax: integer;
  begin stringt := false;
    if fsp <> nil then
      if (fsp^.form = arrays) or (fsp^.form = arrayc) then
        if fsp^.packing then begin
        if fsp^.form = arrays then begin
          { if the index is nil, either the array is a string constant or the
            index type was in error. Either way, we call it a string }
          if fsp^.inxtype = nil then stringt := true
          else begin 
            { common string must pass test of 1..N where N>1 }
            getbounds(fsp^.inxtype,fmin,fmax);
            stringt := (fsp^.aeltype = charptr) and (fmin = 1) and (fmax > 1)
          end
        end else stringt := fsp^.abstype = charptr
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
      arrayc:   if filecomponent(abstype) then f := true;
      records:  if filecomponentre(fstfld) then f := true;
      files:    f := true;
      tagfld:   ;
      variant:  ;
      exceptf:  ;
    end;
    filecomponent := f
  end;

  { check array type, fixed or container }
  function arrayt(fsp: stp): boolean;
  begin
    if fsp = nil then arrayt := false
    else arrayt := (fsp^.form = arrays) or (fsp^.form = arrayc);
  end;

  { check set type }
  function sett(fsp: stp): boolean;
  begin
    if fsp = nil then sett := false
    else sett := fsp^.form = power
  end;

  { check pointer type }
  function ptrt(fsp: stp): boolean;
  begin
    if fsp = nil then ptrt := false
    else ptrt := fsp^.form = pointer
  end;

  { check simple type }
  function simt(fsp: stp): boolean;
  begin
    if fsp = nil then simt := false
    else simt := (fsp^.form = scalar) or (fsp^.form = subrange)
  end;

  { check ordinal type }
  function ordt(fsp: stp): boolean;
  begin
    if fsp = nil then ordt := false
    else ordt := ((fsp^.form = scalar) or (fsp^.form = subrange)) and
                 not realt(fsp)
  end;

  { check file type }
  function filet(fsp: stp): boolean;
  begin
    if fsp = nil then filet := false
    else filet := fsp^.form = files
  end;

  function comptypes{(fsp1,fsp2: stp) : boolean};
    (*decide whether structures pointed at by fsp1 and fsp2 are compatible*)
    var ty1, ty2: stp;
  begin
    comptypes := false; { set default is false }
    { remove any subranges }
    fsp1 := basetype(fsp1);
    fsp2 := basetype(fsp2);
    { Check equal. Aliases of the same type will also be equal. }
    if fsp1 = fsp2 then comptypes := true
    else
      if (fsp1 <> nil) and (fsp2 <> nil) then
        { if the structure forms are the same, or they are both array types }
        if (fsp1^.form = fsp2^.form) or (arrayt(fsp1) and arrayt(fsp2)) then
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
            { Arrays are compatible if they are string types and equal in size,
              or are one or both containers, equally packed, and with equal
              base types }
            arrays,
            arrayc: begin
              if ((fsp1^.form = arrayc) or (fsp2^.form = arrayc)) and
                 (fsp1^.packing = fsp2^.packing) then begin
                { one or both are containers and have same packing status }
                if fsp1^.form = arrays then ty1 := fsp1^.aeltype
                else ty1 := fsp1^.abstype;
                if fsp2^.form = arrays then ty2 := fsp2^.aeltype
                else ty2 := fsp2^.abstype;
                { compatible if bases are compatible }
                comptypes := comptypes(ty1, ty2)
              end else
                { note containers have no size to compare, but will test as
                  compatible arrays before this string test is applied }
                comptypes := stringt(fsp1) and stringt(fsp2) and
                             (fsp1^.size = fsp2^.size );
            end;
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

  function cmpparlst(pla, plb: ctp): boolean; forward;

  { compare two parameters }
  function cmppar(pa, pb: ctp): boolean;
  begin cmppar := false;
    if (pa <> nil) and (pb <> nil) then
      if (pa^.klass in [proc,func]) or (pb^.klass in [proc,func]) then begin
        if cmpparlst(pa^.pflist, pb^.pflist)
          then cmppar := comptypes(pa^.idtype,pb^.idtype)
      end else cmppar := comptypes(pa^.idtype,pb^.idtype)
  end;

  { compare parameter lists }
  function cmpparlst{(pla, plb: ctp): boolean};
  begin cmpparlst := true;
    while (pla <> nil) and (plb <> nil) do begin
      if not cmppar(pla,plb) then cmpparlst := false;
      pla := pla^.next; plb := plb^.next
    end;
    if (pla <> nil) or (plb <> nil) then cmpparlst := false
  end;

  procedure skip(fsys: setofsys);
  (*skip input string until relevant symbol found*)
  begin
    if not eofinp then
      begin while not(sy in fsys) and (not eofinp) do insymbol;
        if not (sy in fsys) then insymbol
      end
  end (*skip*) ;

  { output fixed array template }
  procedure arrtmp(sp: stp);
  var tp: stp; lc: integer; l, h: integer;
  begin
    if prcode and (sp <> nil) then begin
      { check fixed array type }
      if sp^.form = arrays then begin
        { count levels }
        lc := 0;
        tp := sp; while tp <> nil do
          if tp^.form = arrays then begin lc := lc+1; tp := tp^.aeltype end
          else tp := nil;
        write(prr, 't',' ':7);
        genlabel(sp^.tmpl); prtlabel(sp^.tmpl);
        write(prr, ' ', lc:1);
        while sp <> nil do
          if sp^.form = arrays then begin getbounds(sp^.inxtype, l, h);
            write(prr, ' ', h-l+1:1); lc := lc+1; sp := sp^.aeltype
          end else sp := nil;
        writeln(prr)
      end
    end
  end;

  procedure constexpr(fsys: setofsys; var fsp: stp; var fvalu: valu); forward;

  procedure constfactor(fsys: setofsys; var fsp: stp; var fvalu: valu);
    var lsp: stp; lcp: ctp; lvp: csp; test: boolean; lv: valu; i: integer;
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
                  begin form := arrays; aeltype := charptr; inxtype := nil;
                    tmpl := -1; size := lgth*charsize; packing := true
                  end;
                arrtmp(lsp) { output fixed template }
              end;
            fvalu := val; insymbol
          end
        else if sy = lbrack then begin
          { set }
          insymbol;
          new(lvp,pset); pshcst(lvp); lvp^.cclass := pset; lvp^.pval := [];
          if sy <> rbrack then repeat
            constexpr(fsys+[rbrack,comma,range], fsp, fvalu);
            if not fvalu.intval then error(134);
            if sy = range then begin
              insymbol; lv := fvalu;
              constexpr(fsys+[rbrack,comma], fsp, fvalu);
              if not fvalu.intval then error(134);
              if (lv.ival < setlow) or (lv.ival > sethigh) or 
                 (fvalu.ival < setlow) or (fvalu.ival > sethigh) then error(291)
              else for i := lv.ival to fvalu.ival do lvp^.pval := lvp^.pval+[i]
            end else begin
              if (fvalu.ival < setlow) or (fvalu.ival > sethigh) then error(291)
              else lvp^.pval := lvp^.pval+[fvalu.ival]
            end;
            test := sy <> comma;
            if not test then insymbol
          until test;
          if sy = rbrack then insymbol else error(12);
          fvalu.intval := false; fvalu.valp := lvp;
          new(lsp,power); pshstc(lsp);
            with lsp^ do 
              begin form:=power; elset:=nil; size:=setsize; packing := false; 
                    matchpack := false end;
        end else
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
        { * } mul: if (lsp = intptr) and (fsp = intptr) then begin
                     if (lv.ival <> 0) and (fvalu.ival <> 0) then
                       if abs(lv.ival) > pmmaxint div abs(fvalu.ival) then
                         begin error(306); fvalu.ival := 0 end
                      else fvalu.ival := lv.ival*fvalu.ival
                    end else if (lsp = realptr) and (fsp = realptr) then
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
        else if fsp = realptr then begin new(lvp,reel); pshcst(lvp);
          lvp^.cclass := reel; lvp^.rval := -fvalu.valp^.rval;
          fvalu.valp := lvp; svp := lvp;
        end else begin fvalu.intval := true; fvalu.ival := 0 end
    end;
    while (sy = addop) and (op in [plus,minus,orop,xorop]) do begin
      chkstd; lv := fvalu; lsp := fsp; lop := op; insymbol;
      constterm(fsys+[addop], fsp, fvalu);
      lvp := nil;
      if (lop in [plus,minus]) and ((lsp = realptr) or (fsp = realptr)) then
        begin new(lvp,reel); pshcst(lvp); lvp^.cclass := reel end;
      case lop of { operator }
        { + } plus: if (lsp = intptr) and (fsp = intptr) then begin
                      if (lv.ival<0) = (fvalu.ival<0) then
                        if pmmaxint-abs(lv.ival) < abs(fvalu.ival) then
                          begin error(306); fvalu.ival := 0 end
                        else fvalu.ival := lv.ival+fvalu.ival
                    end else if (lsp = realptr) and (fsp = realptr) then
                      lvp^.rval := lv.valp^.rval+fvalu.valp^.rval
                    else if (lsp = realptr) and (fsp = intptr) then
                      lvp^.rval := lv.valp^.rval+fvalu.ival
                    else if (lsp = intptr) and (fsp = realptr) then
                      lvp^.rval := lv.ival+fvalu.valp^.rval
                    else error(134);
        { - } minus: if (lsp = intptr) and (fsp = intptr) then begin
                       if (lv.ival<0) <> (fvalu.ival>0) then
                         if pmmaxint-abs(lv.ival) < abs(fvalu.ival) then
                           begin error(306); fvalu.ival := 0 end
                       else fvalu.ival := lv.ival-fvalu.ival
                     end else if (lsp = realptr) and (fsp = realptr) then
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

  { find number of containers }
  function containers(lsp: stp): integer;
  var cc: integer;
  begin cc := 0;
    while lsp <> nil do
      if lsp^.form = arrayc then begin lsp := lsp^.abstype; cc := cc+1 end
      else lsp := nil;
    containers := cc
  end;

  { find base size of container or array series }
  function containerbase(lsp: stp): integer;
  var bp: stp;
  begin bp := nil;
    while lsp <> nil do
      if lsp^.form = arrayc then lsp := lsp^.abstype
      else if lsp^.form = arrays then lsp := lsp^.aeltype
      else begin bp := lsp; lsp := nil end;
    if bp = nil then containerbase := 0
    else containerbase := bp^.size
  end;

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
                     drct:   if vlevel<=1 then begin
                               if (chkext(symptr) or chkfix(symptr)) and
                                  (dplmt <> 0) then begin
                                 { labeled base with offset, need to change
                                   to address load with offset }
                                 if chkfix(symptr) then
                                   gen1s(114(*lto*),dplmt,symptr)
                                 else gen1s(37(*lao*),dplmt,symptr);
                                 gen1t(35(*ind*),dplmt,typtr);
                               end else begin
                                 if chkfix(symptr) then
                                   gen1ts(8(*ltc*),dplmt,typtr,symptr)
                                 else gen1ts(39(*ldo*),dplmt,typtr,symptr)
                               end
                             end else 
                               gen2t(54(*lod*),level-(level-vlevel),dplmt,
                                     typtr);
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
                     drct:   if vlevel <= 1 then begin
                               if chkfix(symptr) then
                                 gen1s(114(*lto*),dplmt,symptr)
                               else gen1s(37(*lao*),dplmt,symptr);
                               { if there is an offset left in the address,
                                 apply it now }
                               if (chkext(symptr) or chkfix(symptr)) and
                                  (dplmt <> 0) then
                                  gen1t(34(*inc*),idplmt,nilptr);
                             end else gen2(50(*lda*),level-(level-vlevel),dplmt);
                     indrct: if idplmt <> 0 then
                               gen1t(34(*inc*),idplmt,nilptr);
                     inxd:   error(404)
                   end;
            expr:  gen1(118(*lsa*),0)
          end;
          if typtr^.form = arrayc then if pickup then begin
            { it's a container, load a complex pointer based on that }
            if dblptr then gen0(111(*ldp*)) else begin
              gen0(98(*lcp*));
              { if level is at bottom, simplify the template }
              if containers(typtr) = 1 then gen0(108(*spc*))
            end
          end;
          kind := varbl; access := indrct; idplmt := 0; packing := false;
          symptr := nil { break variable association }
        end
  end (*loadaddress*) ;

  procedure store(var fattr: attr);
    var lsize: addrrange;
  begin
    with fattr do
      if typtr <> nil then
        case access of
          drct:   if vlevel <= 1 then gen1ts(43(*sro*),dplmt,typtr,symptr)
                  else gen2t(56(*str*),level-(level-vlevel),dplmt,typtr);
          indrct: if idplmt <> 0 then error(401)
                  else if typtr^.form in [records,arrays] then begin
                    lsize := typtr^.size;
                    alignu(parmptr,lsize);
                    gen2t(26(*sto*),typtr^.size, lsize,typtr);
                    mesl(adrsize+lsize)
                  end else
                    gen0t(26(*sto*),typtr);
          inxd:   error(402)
        end
  end (*store*) ;

  { rationalize binary container operator }
  procedure containerop(var lattr: attr);
  var cc: integer; len:addrrange;
  begin
    { check one or both operands is container }
    if (lattr.typtr^.form = arrayc) or
       (gattr.typtr^.form = arrayc) then begin
      { one or both are containers, find the index level }
      if lattr.typtr^.form = arrayc then
        cc := containers(lattr.typtr)
      else
        cc := containers(gattr.typtr);
      if gattr.kind = expr then begin
        { have to pull pointer over stack bubble }
        len := gattr.typtr^.size;
        alignu(parmptr,len);
        gen1(118(*lsa*),len);
        gen0(111(*ldp*));
        gen1(118(*lsa*),ptrsize*2)
      end;
      if gattr.typtr^.form = arrays then begin
        { right is fixed }
        if cc = 1 then begin
          { load simple template }
          gen2(51(*ldc*),1,spana(gattr.typtr));
          gen1(72(*swp*),stackelsize)
        end else
          { load complex fixed template }
          gen1(105(*lft*),gattr.typtr^.tmpl)
      end else if lattr.typtr^.form = arrays then begin
        { left is fixed }
        if cc = 1 then
          { load simple template }
          gen2(51(*ldc*),1,spana(lattr.typtr))
        { load complex fixed template }
        else gen1(105(*lft*),lattr.typtr^.tmpl);
        gen1(72(*swp*),ptrsize*3) { swap under right side and fix addr }
      end;
      { compare templates }
      if cc = 1 then gen0(99(*cps*)) { simple compare }
      else gen1(100(*cpc*),cc); { complex compare }
    end
  end;

  function parnum(fcp: ctp): integer;
    var pn: integer;
  begin
    pn := 0; fcp := fcp^.pflist;
    while fcp <> nil do begin pn := pn+1; fcp := fcp^.next end;
    parnum := pn
  end;

  function partype(fcp: ctp; pn: integer): stp;
  begin fcp := fcp^.pflist;
    while (pn > 1) and (fcp <> nil) do begin fcp := fcp^.next; pn := pn-1 end;
    if fcp = nil then partype := nil else partype := fcp^.idtype
  end;

  { compare parameter type to actual type }
  function cmptyp(pt, at: stp): boolean;
  begin cmptyp := false;
    if comptypes(pt, at) then cmptyp := true
    else if realt(pt) and intt(at) then cmptyp := true
  end;

  function ischrcst(var at: attr): boolean;
  begin
    ischrcst := (at.typtr = charptr) and (at.kind = cst)
  end;

  { find matching uary operator overload }
  procedure fndopr1(opr: operatort; var fcp: ctp);
    var dt: disprange; fcp2: ctp;
  begin fcp := nil;
    if not iso7185 then begin
      dt := top; { search top down }
      repeat
        while (dt > 0) and (display[dt].oprprc[opr] = nil) do dt := dt-1;
        fcp2 := display[dt].oprprc[opr];
        fcp := nil; { set not found }
        while fcp2 <> nil do begin
          if parnum(fcp2) = 1 then
            if cmptyp(partype(fcp2, 1), gattr.typtr) then fcp := fcp2;
          fcp2 := fcp2^.grpnxt
        end;
        if dt > 0 then dt := dt-1
      until (fcp <> nil) or (dt = 0)
    end
  end;

  { find matching binary operator overload }
  procedure fndopr2(opr: operatort; var lattr: attr; var fcp: ctp);
    var dt: disprange; fcp2: ctp;
  begin fcp := nil;
    if not iso7185 then begin
      dt := top; { search top down }
      repeat
        while (dt > 0) and (display[dt].oprprc[opr] = nil) do dt := dt-1;
        fcp2 := display[dt].oprprc[opr];
        fcp := nil; { set not found }
        while fcp2 <> nil do begin
          if parnum(fcp2) = 2 then
            if cmptyp(partype(fcp2, 1), lattr.typtr) then
              if cmptyp(partype(fcp2, 2), gattr.typtr) then fcp := fcp2;
          fcp2 := fcp2^.grpnxt
        end;
        if dt > 0 then dt := dt-1
      until (fcp <> nil) or (dt = 0)
    end
  end;

  procedure expression(fsys: setofsys; threaten: boolean); forward;

  procedure callop1(fcp: ctp); forward;

  procedure callop2(fcp: ctp; var lattr: attr); forward;

  { check any overloads exist for given operator }
  function isopr(opt: operatort): boolean;
    var dt: disprange;
  begin isopr := false;
    dt := top;
    while (dt > 0) and (display[dt].oprprc[opt] = nil) do dt := dt-1;
    isopr := display[dt].oprprc[opt] <> nil
  end;

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
      id: stp; lastptr: boolean; cc: integer; ct: boolean;
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
      if (vp <> nil) and (vl <> nil) then
        if (vl^.name <> nil) or chkudtf then begin { is a variant }
        if chkudtf and (vl^.name = nil) and (vp <> nil) then begin
          { tagfield is unnamed and checking is on, force tagfield
            assignment }
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
            loadaddress;
            gen2(51(*ldc*),1,vp^.varval.ival);
            if chkvbk then
              genctaivtcvb(95(*cvb*),vl^.varsaddr-fldaddr,vl^.varssize,
                           vl^.vartl,vl^.idtype);
            if debug then
              genctaivtcvb(82(*ivt*),vl^.varsaddr-fldaddr,vl^.varssize,
                           vl^.vartl,vl^.idtype);
            gen0t(26(*sto*),basetype(idtype));
          end;
          gattr := gattrs
        end;
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
    lastptr := false; { set last index op not ptr }
    with fcp^, gattr do
      begin symptr := nil; typtr := idtype; spv := false; kind := varbl;
        packing := false; packcom := false; tagfield := false; ptrref := false;
        vartl := -1; pickup := true; dblptr := false;
        case klass of
          vars: begin symptr := fcp;
              if typtr <> nil then
                begin packing := typtr^.packing; dblptr := fcp^.dblptr end;
              if vkind = actual then
                begin access := drct; vlevel := vlev;
                  dplmt := vaddr
                end
              else
                begin
                  { if container, just load the address of it, the complex
                    pointer is loaded when the address is loaded }
                  ct := false; if typtr <> nil then ct := typtr^.form = arrayc;
                  if ct then gen2(50(*lda*),level-(level-vlev),vaddr)
                  else gen2t(54(*lod*),level-(level-vlev),vaddr,nilptr);
                  access := indrct; idplmt := 0
                end;
            end;
          fixedt: begin symptr := fcp;
              if typtr <> nil then packing := typtr^.packing;
              access := drct; vlevel := 0; dplmt := 0
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
              gattr.vartl := fcp^.vartl;
              if occur = crec then
                begin access := drct; vlevel := clev;
                  dplmt := cdspl + fldaddr
                end
              else if occur = vrec then
                begin
                  { override to local for with statement }
                  gen2t(54(*lod*),level,vdspl,nilptr);
                  access := indrct; idplmt := fldaddr
                end
              else
                begin
                  if level = 1 then gen1t(39(*ldo*),vdspl,nilptr)
                  else gen2t(54(*lod*),level,vdspl,nilptr);
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
                    lsize := parmsize; if id <> nil then lsize := id^.size;
                    dplmt := marksize+ptrsize+adrsize+locpar { addr of fr }
                  end
              end;
            proc: { nothing, its an error case }
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
                  if not arrayt(typtr) then begin error(138); typtr := nil end
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
                    if form = arrayc then begin
                      { note containers merge index and bounds check }
                      if gattr.typtr <> intptr then error(139)
                    end else if comptypes(inxtype,gattr.typtr) then
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
                      begin
                        if lattr.typtr^.form = arrays then typtr := aeltype
                        else typtr := abstype;
                        kind := varbl;
                        access := indrct; idplmt := 0; packing := false;
                        packcom := false; tagfield := false; ptrref := false;
                        vartl := -1; pickup := false; dblptr := false;
                      end;
                    if gattr.typtr <> nil then
                      begin
                        gattr.packcom := lattr.packing;
                        gattr.packing :=
                          lattr.packing or gattr.typtr^.packing;
                        lsize := gattr.typtr^.size; { get base size }
                        cc := containers(lattr.typtr);
                        if lattr.typtr^.form = arrays then gen1(36(*ixa*),lsize)
                        else if cc = 1 then
                          gen1(103(*cxs*),lsize) { simple container index }
                        else begin { complex container index }
                          gen2(104(*cxc*),cc,containerbase(gattr.typtr));
                          { if level is at bottom, simplify the template }
                          if cc = 2 then gen0(108(*spc*))
                        end
                      end
                  end
              else gattr.typtr := nil
            until sy <> comma;
            if sy = rbrack then insymbol else error(12);
            lastptr := false { set not pointer op }
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
                                { only set ptr offset ref if last was ptr }
                                gattr.ptrref := lastptr;
                                gattr.vartl := lcp^.vartl;
                                gattr.pickup := false;
                                gattr.dblptr := false;
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
                end; (*with gattr*)
              lastptr := false { set last not ptr op }
            end (*if sy = period*)
          else
  (*^*)     begin
              if gattr.typtr <> nil then
                with gattr,typtr^ do
                  if form = pointer then
                    begin load;
                      if eltype <> nil then
                        if eltype^.form = arrayc then begin
                        { it's a container, load a complex pointer based on
                          that }
                        gen0t(76(*dup*),nilptr); { copy that }
                        { index data }
                        gen1t(34(*inc*),containers(eltype)*intsize,nilptr);
                        { if level is at bottom, simplify the template }
                        if containers(eltype) = 1 then gen0(108(*spc*))
                      end;
                      typtr := eltype;
                      if debug then begin
                         if taggedrec(eltype) then
                           gen2t(80(*ckl*),1,maxaddr,nilptr)
                         else gen2t(45(*chk*),1,maxaddr,nilptr);
                      end;
                      with gattr do
                        begin kind := varbl; access := indrct; idplmt := 0;
                          packing := false; packcom := false;
                          tagfield := false; ptrref := true; vartl := -1;
                          pickup := false; dblptr := false;
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
              insymbol;
              lastptr := true { set last was ptr op }
            end;
        if not (sy in fsys + selectsys) then
          begin error(6); skip(fsys + selectsys) end
      end (*while*)
  end (*selector*) ;

  procedure fixpar(fsp,asp: stp);
    var cc: integer;
  begin
    if fsp <> nil then begin
      if (asp^.form = arrays) and (fsp^.form = arrayc) then begin
        { fixed into container }
        cc := containers(fsp);
        if cc = 1 then begin
          { load simple template }
          gen2(51(*ldc*),1,spana(asp));
          gen1(72(*swp*),stackelsize)
        end else
          { load complex fixed template }
          gen1(105(*lft*),asp^.tmpl)
      end else if (asp^.form = arrayc) and
                  (fsp^.form = arrays) then begin
        { container into fixed, load template for fixed side }
        cc := containers(asp);
        if cc = 1 then begin
          { load simple template }
          gen2(51(*ldc*),1,span(fsp));
          gen2(51(*ldc*),4,0) { load dummy address }
        end else begin
          { load complex fixed template }
          gen2(51(*ldc*),4,0); { load dummy address }
          gen1(105(*lft*),fsp^.tmpl);
        end;
        { compare templates }
        if cc = 1 then gen0(99(*cps*)) { simple compare }
        else gen1(100(*cpc*),cc); { complex compare }
        { discard the templates }
        gen1(71(*dmp*),ptrsize*2);
        gen1(72(*swp*),ptrsize);
        gen1(71(*dmp*),ptrsize)
      end
    end
  end;

  procedure call(fsys: setofsys; fcp: ctp; inherit: boolean; isfunc: boolean);
    var lkey: keyrng;

    procedure variable(fsys: setofsys; threaten: boolean);
      var lcp: ctp;
    begin
      if sy = ident then
        begin searchid([vars,fixedt,field],lcp); insymbol end
      else begin error(2); lcp := uvarptr end;
      if threaten and (lcp^.klass = vars) then with lcp^ do begin
        if vlev < level then threat := true;
        if forcnt > 0 then error(195);
        if part = ptview then error(290)
      end;
      selector(fsys,lcp, false);
      if gattr.kind = expr then error(287)
    end (*variable*) ;

    procedure chkhdr;
    var lcp: ctp; dummy: boolean;
    begin
      if sy = ident then begin { test for file }
        searchidnenm([vars],lcp,dummy);
        if (lcp = inputptr) and not inputptr^.hdr then error(175)
        else if (lcp = outputptr) and not outputptr^.hdr then error(176)
        else if (lcp = prdptr) and not prdptr^.hdr then error(217)
        else if (lcp = prrptr) and not prrptr^.hdr then error(218)
        else if (lcp = errorptr) and not errorptr^.hdr then error(219)
        else if (lcp = listptr) and not listptr^.hdr then error(220)
        else if (lcp = commandptr) and not commandptr^.hdr then error(221)
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
          cp: boolean;
    begin
      txt := true; deffil := true; cp := false;
      if sy = lparent then
        begin insymbol; chkhdr;
          variable(fsys + [comma,colon,rparent], true);
          if gattr.typtr <> nil then cp := gattr.typtr^.form = arrayc;
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
                      variable(fsys + [comma,colon,rparent], true);
                      if gattr.typtr <> nil then 
                        cp := gattr.typtr^.form = arrayc
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
              { note 2nd is always pointer }
              if cp then gen1(72(*swp*),ptrsize+intsize) 
              else gen1(72(*swp*),ptrsize);
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
                        if not cp then gen2(51(*ldc*),1,len)
                        else gen1(72(*swp*),intsize);
                        if fld then gen1(30(*csp*),79(*rdsf*))
                        else if spad then gen1(30(*csp*),80(*rdsp*))
                        else gen1(30(*csp*),73(*rds*))
                      end else error(153)
                else error(116);
            end else begin { binary file }
              if not comptypes(gattr.typtr,lsp^.filtype) then error(129);
              gen2(51(*ldc*),1,lsp^.filtype^.size);
              gen1(30(*csp*),35(*rbf*))
            end;
            test := sy <> comma;
            if not test then
              begin insymbol; variable(fsys + [comma,colon,rparent], true);
                if gattr.typtr <> nil then cp := gattr.typtr^.form = arrayc
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
          len:addrrange; strspc: addrrange;
          txt: boolean; { is a text file }
          byt: boolean; { is a byte file }
          deffil: boolean; { default file was loaded }
          test: boolean;
          r: integer; { radix of print }
          spad: boolean; { write space padded string }
          ledz: boolean; { use leading zeros }
          cpx: boolean; { is complex pointer }
          onstk: boolean; { expression result on stack }
    begin llkey := lkey; txt := true; deffil := true; byt := false; 
      if sy = lparent then
      begin insymbol; chkhdr;
      expression(fsys + [comma,colon,rparent,hexsy,octsy,binsy], false);
      onstk := gattr.kind = expr;
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
                             false);
                  onstk := gattr.kind = expr
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
          if lsp <> nil then begin
            if lsp^.form <= subrange then begin
              if lsp^.size < stackelsize then
                { size of 2nd is minimum stack }
                gen1(72(*swp*),stackelsize)
              else
                gen1(72(*swp*),lsp^.size) { size of 2nd is operand }
            end else
              { 2nd is pointer, either simple or complex }
              if lsp^.form = arrayc then gen1(72(*swp*),ptrsize*2)
              else gen1(72(*swp*),ptrsize);
          end;
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
          { if string and not container, convert to complex }
          if lsp <> nil then
            if stringt(lsp) and not (lsp^.form = arrayc) then begin
            len := lsp^.size div charmax;
            gen2(51(*ldc*),1,len); { load len }
            gen1(72(*swp*),stackelsize) { swap ptr and len }
          end;
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
                      if lsp^.form = scalar then error(236)
                      else
                        if stringt(lsp) then begin
                          if onstk then begin 
                            strspc := lsp^.size;
                            alignu(parmptr,strspc);
                            gen1(118(*lsa*),strspc+ptrsize+intsize);
                            gen1t(35(*ind*),0,nilptr);
                            gen1(72(*swp*),stackelsize*2)
                          end;
                          if lsp = nil then cpx := false
                          else cpx := lsp^.form = arrayc;
                          if cpx then begin { complex }
                            if default then begin
                              { no field, need to duplicate len to make the
                                field }
                              gen1(72(*swp*),stackelsize); { swap ptr and len }
                              gen0t(76(*dup*),nilptr); { make copy len }
                              gen1(72(*swp*),stackelsize*2); { back in order }
                            end
                          end else begin { standard array }
                            len := lsp^.size div charmax;
                            if default then gen2(51(*ldc*),1,len)
                          end;
                          if spad then gen1(30(*csp*),68(*wrsp*))
                          else gen1(30(*csp*),10(*wrs*));
                          if onstk then gen1(71(*dmp*),strspc+ptrsize)
                        end else error(116)
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
                       false);
            onstk := gattr.kind = expr
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
      gen1(71(*dmp*),ptrsize)
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
      var lsp,lsp1,lsp2,lsp3: stp; varts: integer;
          lsize: addrrange; lval: valu; tagc: integer; tagrec: boolean;
          ct: boolean; cc,pc: integer;
    begin
      if disp then begin
        expression(fsys + [comma, rparent], false);
        load
      end else begin
        variable(fsys + [comma,rparent], false);
        loadaddress
      end;
      ct := false;
      if gattr.typtr <> nil then
        if gattr.typtr^.form = pointer then
          if gattr.typtr^.eltype <> nil then
            ct := gattr.typtr^.eltype^.form = arrayc;
      if ct then begin { container array }
        if disp then gen0(113(*vdd*))
        else begin lsp := gattr.typtr^.eltype;
          cc := containers(lsp); { find no. containers }
          pc := 0;
          while sy = comma do begin insymbol;
            expression(fsys+[comma,rparent], false); load;
            if gattr.typtr <> nil then
              if basetype(gattr.typtr) <> intptr then error(243);
            pc := pc+1;
            gen1(72(*swp*),ptrsize) { keep the var address on top }
          end;
          if pc <> cc then error(269);
          { issue vector init dynamic instruction }
          gen2(112(*vin*),pc,containerbase(lsp));
          { remove initializers, var addr }
          mesl(pc*intsize+adrsize)
        end
      end else begin
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
                        lsp3 := lsp; lsp1 := lsp^.fstvar;
                        while lsp1 <> nil do
                          with lsp1^ do
                            if varval.ival = lval.ival then
                              begin lsize := size; lsp := subvar;
                                if debug then begin
                                  if lsp3^.vart = nil then error(510);
                                  if lsp2=charptr then
                                    gen2(51(*ldc*),6,lsp3^.vart^[varval.ival])
                                  else
                                    gen2(51(*ldc*),1,lsp3^.vart^[varval.ival])
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
        end
      end
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
      gen1(30(*csp*),85(*thw*))
    end;

    procedure maxfunction;
      var lattr: attr;
    begin chkstd;
      if sy = lparent then insymbol else error(9);
      variable(fsys+[rparent,comma], false); loadaddress;
      if gattr.typtr <> nil then
        if gattr.typtr^.form <> arrayc then error(273);
      lattr := gattr;
      if sy = comma then begin insymbol;
        expression(fsys + [rparent], false); load;
        if gattr.typtr <> nil then if gattr.typtr <> intptr then error(125)
      end else gen2(51(*ldc*),1,1); { default level 1 }
      gen1(106(*max*),containers(lattr.typtr));
      if sy = rparent then insymbol else error(4);
      gattr.typtr := intptr
    end;

    { assign buffer to id and copy top of stack to it, replacing with address }
    procedure cpy2adr(id: ctp; var at: attr);
      var lsize: addrrange;
    begin
      lsize := at.typtr^.size;
      alignu(parmptr,lsize);
      getcbb(id^.cbb,id,at.typtr^.size); { get a buffer }
      gen1(37(*lao*),id^.cbb^.addr); { load address }
      { store to that }
      gen2(115(*ctb*),at.typtr^.size,lsize);
      mesl(lsize)
    end;

    procedure callnonstandard(fcp: ctp; inherit: boolean);
      var nxt,lcp: ctp; lsp: stp; lkind: idkind; lb: boolean;
          locpar, llc, soff: addrrange; varp: boolean; lsize: addrrange;
          frlab: integer; prcnt: integer; fcps: ctp; ovrl: boolean;
          test: boolean; match: boolean; e: boolean; mm: boolean;
    { This overload does not match, sequence to the next, same parameter.
      Set sets fcp -> new proc/func, nxt -> next parameter in new list. }
    procedure nxtprc;
      var pc: integer; fcpn, fcpf: ctp;
      { compare parameter lists until current }
      function cmplst(pl1, pl2: ctp): boolean;
        var pc: integer; pll1, pll2: ctp;
      begin cmplst := false; pc := 1;
        pll1 := nil; pll2 := nil;
        while (pc < prcnt) and cmppar(pl1, pl2) do begin
          pll1 := pl1; pll2 := pl2;
          if pl1 <> nil then pl1 := pl1^.next;
          if pl2 <> nil then pl2 := pl2^.next;
          pc := pc+1
        end;
        { compare last entry }
        if (pll1 <> nil) and (pll2 <> nil) then cmplst := cmppar(pll1, pll2)
        else cmplst := (pll1 = nil) and (pll2 = nil) { reached end of both lists }
      end;
    begin pc := 1;
      fcpn := fcp^.grpnxt; { go next proc/func, which may not exist }
      fcpf := nil; { set none found }
      while fcpn <> nil do begin { search next for match }
        if (isfunc and (fcpn^.klass = func)) or (not isfunc and (fcpn^.klass = proc)) then
          if cmplst(fcp^.pflist, fcpn^.pflist) then
            begin fcpf := fcpn; fcpn := nil end
          else fcpn := fcpn^.grpnxt { next group proc/func }
        else fcpn := fcpn^.grpnxt
      end;
      fcp := fcpf; { set found/not found }
      if fcp <> nil then begin { recover parameter position in new list }
        nxt := fcp^.pflist;
        while (pc < prcnt) do begin if nxt <> nil then nxt := nxt^.next; pc := pc+1 end
      end
    end;
    begin { callnonstandard }
      soff := abs(topnew); { save stack net offset }
      fcps := fcp; fcp := fcp^.grppar; locpar := 0; genlabel(frlab);
      while ((isfunc and (fcp^.klass <> func)) or 
             (not isfunc and (fcp^.klass <> proc))) and (fcp^.grpnxt <> nil) do
        fcp := fcp^.grpnxt;
      if isfunc and (fcp^.klass <> func) then error(292)
      else if not isfunc and (fcp^.klass <> proc) then error(293);
      prcnt := 1; ovrl := fcp^.grpnxt <> nil;
      with fcp^ do
        begin nxt := pflist; lkind := pfkind;
          { I don't know why these are dups, guess is a badly formed far call }
          if pfkind = actual then begin { it's a system call }
            if not sysrot then gensfr(frlab)
          end else gensfr(frlab) { its an indirect }
        end;
      if sy = lparent then
        begin llc := lc; insymbol;
          repeat lb := false; (*decide whether proc/func must be passed*)
            if nxt = nil then begin
              { out of parameters, try to find another overload }
              nxtprc;
              if nxt = nil then begin
                { dispatch error according to overload status }
                if ovrl then error(275) else error(126);
                fcp := fcps
              end
            end;
            e := false;
            if (sy = ident) and (fcp^.grpnxt <> nil) then begin
              { next is id, and proc/func is overload, try proc/func parameter }
              match := false;
              searchidnenm([proc,func],lcp,mm);
              if lcp <> nil then if lcp^.klass in [proc,func] then begin
                { Search matching overload. For proc/func parameters, we allow
                  all features of the target to match, including function
                  result. }
                repeat
                  if nxt^.klass = proc then begin
                    if cmpparlst(nxt^.pflist, lcp^.pflist) then match := true
                  end else if nxt^.klass = func then begin
                    if cmpparlst(nxt^.pflist, lcp^.pflist) then
                      if comptypes(lcp^.idtype,nxt^.idtype) then match := true
                  end;
                  if not match then nxtprc { no match get next overload }
                until match or (fcp = nil);
                if fcp = nil then begin error(277); e := true; fcp := fcps end
              end
            end;
            { match same thing for all procs/funcs }
            if nxt <> nil then lb := nxt^.klass in [proc,func];
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
                          if not e then error(128)
                      end;
                    { compare parameter lists }
                    if (nxt^.klass in [proc,func]) and
                       (lcp^.klass in [proc,func]) then
                      if not cmpparlst(nxt^.pflist, lcp^.pflist) then
                        if not e then error(189);
                    if lcp^.pfkind = actual then 
                      genlpa(lcp^.pfname,level-(level-lcp^.pflev))
                    else gen2(74(*lip*),level-(level-lcp^.pflev),lcp^.pfaddr);
                    locpar := locpar+ptrsize*2;
                    insymbol;
                    if not (sy in fsys + [comma,rparent]) then
                      begin error(6); skip(fsys + [comma,rparent]) end
                  end
              end (*if lb*)
            else
              begin varp := false;
                if nxt <> nil then varp := nxt^.vkind = formal;
                expression(fsys + [comma,rparent], varp);
                { find the appropriate overload }
                match := false;
                repeat
                  if (nxt <> nil) and (gattr.typtr <> nil) then
                    if nxt^.idtype <> nil then begin
                      if comptypes(nxt^.idtype, gattr.typtr) or 
                         { special rule: const char matches container }
                         ((nxt^.idtype^.form = arrayc) and 
                          chart(gattr.typtr) and (gattr.kind = cst)) then 
                        match := true
                      else if comptypes(realptr,nxt^.idtype) and
                              (gattr.typtr = intptr) then match := true
                    end;
                    if not match then nxtprc { no match get next overload }
                until match or (fcp = nil);
                if fcp = nil then begin error(277); e := true; fcp := fcps end;
                { override variable status for view parameter }
                if nxt <> nil then varp := (nxt^.vkind = formal) and not (nxt^.part = ptview);
                if varp and (gattr.kind <> varbl) then error(278);
                if gattr.typtr <> nil then
                  begin
                    if nxt <> nil then
                      begin lsp := nxt^.idtype;
                        if lsp <> nil then
                          begin
                            if (nxt^.vkind = actual) or (nxt^.part = ptview) then begin
                              if not comptypes(lsp,gattr.typtr) and not
                                 { special rule: const char matches container }
                                 ((nxt^.idtype^.form = arrayc) and 
                                  chart(gattr.typtr) and (gattr.kind = cst)) and not
                                 (comptypes(realptr,lsp) and 
                                  (gattr.typtr = intptr)) then 
                                if not e then error(142);
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
                              else if stringt(lsp) and ischrcst(gattr) then
                                begin { is char to string }
                                  gen2(51(*ldc*),1,1);
                                  gensca(chr(gattr.cval.ival));
                                  locpar := locpar+ptrsize*2;
                                  alignu(parmptr,locpar)
                                end else begin
                                  if gattr.kind = expr then cpy2adr(nxt,gattr)
                                  else loadaddress;
                                  fixpar(lsp,gattr.typtr);
                                  if lsp^.form = arrayc then
                                    locpar := locpar+ptrsize*2
                                  else locpar := locpar+ptrsize;
                                  alignu(parmptr,locpar)
                                end
                            end else begin
                              if gattr.kind = varbl then
                                begin if gattr.packcom then error(197);
                                  if gattr.tagfield then error(198);
                                  if gattr.kind = expr then cpy2adr(nxt,gattr)
                                  else loadaddress;
                                  fixpar(lsp,gattr.typtr);
                                  if lsp^.form = arrayc then
                                    locpar := locpar+ptrsize*2
                                  else locpar := locpar+ptrsize;
                                  alignu(parmptr,locpar);
                                end
                              else error(154);
                              if (lsp^.form = arrayc) and not iso7185 then begin
                                if not comptypes(lsp, gattr.typtr)
                                   and not e then error(289)
                              end else if lsp <> gattr.typtr then
                                if not e then error(199)
                            end
                          end
                      end
                  end
              end;
            if nxt <> nil then begin nxt := nxt^.next; prcnt := prcnt+1 end;
            test := sy <> comma;
            if sy = comma then insymbol;
          until test;
          lc := llc;
          if sy = rparent then insymbol else error(4)
        end (*if lparent*);
      { not out of proto parameters, sequence until we are or there are no
        candidate overloads }
      while (nxt <> nil) and (fcp <> nil) do nxtprc;
      if fcp = nil then fcp := fcps;
      { find function result size }
      lsize := 0;
      if (fcp^.klass = func) and (fcp^.idtype <> nil) then begin
          lsize := fcp^.idtype^.size;
          alignu(parmptr,lsize);
      end;
      if prcode then begin prtlabel(frlab); writeln(prr,'=',lsize:1) end;
      if lkind = actual then
        begin if nxt <> nil then if ovrl then error(275) else error(126);
          with fcp^ do
            begin
              if sysrot then gen1(30(*csp*),pfname)
              else begin
                if (pfattr = fpavirtual) or (pfattr = fpaoverride) then begin
                  if inherit then begin
                    fcp := ovrpf(fcp); if fcp = nil then error(516);
                    if fcp^.pfattr <> fpaoverride then error(507);
                    { inherited calls will never be far }
                    gencuv(locpar,fcp^.pfvaddr,nil)
                  end else begin
                    lcp := fcp^.grppar;
                    if lcp^.pfvid <> nil then 
                      gencuv(locpar,lcp^.pfvid^.vaddr,lcp^.pfvid)
                  end
                end else begin
                  if inherit then error(234);
                  if fcp^.klass = func then 
                    gencupcuf(122(*cuf*),locpar,pfname,fcp)
                  else
                    gencupcuf(46(*cup*),locpar,pfname,fcp)
                end;
                mesl(-lsize)
              end
            end
        end
      else begin { call procedure or function parameter }
        gen2(50(*lda*),level-(level-fcp^.pflev),fcp^.pfaddr);
        gen0(67(*cip*));
        gen1(32(*rip*),lcs+lsize+soff);
        mesl(locpar); { remove stack parameters }
        mesl(-lsize)
      end;
      gattr.typtr := fcp^.idtype;
      { clean any copyback buffers out of list }
      nxt := fcp^.pflist;
      while nxt <> nil do
        begin putcbb(nxt^.cbb); nxt^.cbb := nil; nxt := nxt^.next end
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

              10,13:  error(508)
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
              32:    maxfunction;
            end;
            if (lkey <= 8) or (lkey = 16) then
              if sy = rparent then insymbol else error(4)
          end;
      end (*standard procedures and functions*)
    else begin callnonstandard(fcp,inherit); markline end
  end (*call*) ;

  function psize(sp: stp): addrrange;
  var ps: addrrange;
  begin ps := 0;
    if sp <> nil then begin
      if sp^.form = arrayc then ps := ptrsize*2
      else if sp^.form <= power then ps := sp^.size
      else ps := ptrsize;
      alignu(parmptr, ps)
    end;
    psize := ps
  end;

  { call operator type with 1 parameter }
  procedure callop1{(fcp: ctp)};
    var frlab: integer; lsize: addrrange; locpar, locpars: addrrange;
        sp: stp;
  begin
    sp := partype(fcp, 1);
    genlabel(frlab); gensfr(frlab);
    { find uncoerced parameters size }
    locpars := psize(gattr.typtr);
    { find final parameters size }
    locpar := psize(sp);
    { find function result size }
    lsize := fcp^.idtype^.size;
    alignu(parmptr,lsize);
    { generate a stack hoist of parameters. Basically the common math on stack
      not formatted the same way as function calls, so we hoist the parameters
      over the mark, call and then drop the function result downwards. }
    if gattr.typtr <> nil then
        if (gattr.kind = expr) and (gattr.typtr^.form > power) then
          gen1(118(*lsa*),lsize)
        else gen2(116(*cpp*),lsize,locpars);
    { do coercions }
    if realt(sp) and intt(gattr.typtr) then
      begin gen0(10(*flt*)); gattr.typtr := realptr end;
    fixpar(sp,gattr.typtr);
    if prcode then begin prtlabel(frlab); writeln(prr,'=',lsize:1) end;
    gencupcuf(122(*cuf*),locpar,fcp^.pfname,fcp);
    gen2(117(*cpr*),lsize,locpars);
    gattr.typtr := fcp^.idtype
  end;

  { call operator type with 2 parameters }
  procedure callop2{(fcp: ctp; var lattr: attr)};
    var frlab: integer; lsize: addrrange;
        locpar, locpars, lpl, lpr, lpls, lprs: addrrange;
        lsp, rsp: stp;
    { check actual type can be coerced into formal }
    function fungible(fsp,asp: stp): boolean;
    begin fungible := false;
      if (fsp <> nil) and (asp <> nil) then begin
        if realt(fsp) and intt(asp) then fungible := true
        else if ((fsp^.form = arrayc) and (asp^.form = arrays)) or
                ((fsp^.form = arrays) and (asp^.form = arrayc)) then
          fungible := true
      end
    end;
  begin
    lsp := partype(fcp, 1); rsp := partype(fcp, 2);
    genlabel(frlab); gensfr(frlab);
    { find uncoerced parameters size }
    lpls := psize(lattr.typtr); lprs := psize(gattr.typtr);
    locpars := lpls+lprs;
    { find final parameters size }
    lpl := psize(lsp); lpr := psize(rsp); locpar := lpl+lpr;
    { find function result size }
    lsize := 0;
    if fcp^.klass = func then begin
      lsize := fcp^.idtype^.size;
      alignu(parmptr,lsize)
    end;
    { generate a stack hoist of parameters. Basically the common math on stack
      not formatted the same way as function calls, so we hoist the parameters
      over the mark, call and then drop the function result downwards. }
    if fungible(lsp, lattr.typtr) or (lattr.kind = expr) or
       fungible(rsp, gattr.typtr) or (gattr.kind = expr) then begin
      { bring the parameters up and convert them one by one }
      if lattr.typtr <> nil then
        if (lattr.kind = expr) and (lattr.typtr^.form > power) then
          gen1(118(*lsa*),lsize+lprs)
        else gen2(116(*cpp*),lsize+lprs,lpls);
      { do coercions }
      if realt(lsp) and intt(lattr.typtr) then
        begin gen0(10(*flt*)); lattr.typtr := realptr end;
      fixpar(lsp,lattr.typtr);
      if gattr.typtr <> nil then
        if (gattr.kind = expr) and (gattr.typtr^.form > power) then
          gen1(118(*lsa*),lsize+lpl)
        else gen2(116(*cpp*),lsize+lpl,lprs);
      { do coercions }
      if realt(rsp) and intt(gattr.typtr) then
        begin gen0(10(*flt*)); gattr.typtr := realptr end;
      fixpar(rsp,gattr.typtr);
    end else gen2(116(*cpp*),lsize,locpar); { get both params }
    if prcode then begin prtlabel(frlab); writeln(prr,'=',lsize:1) end;
    gencupcuf(122(*cuf*),locpar,fcp^.pfname,fcp);
    gen2(117(*cpr*),lsize,locpars);
    gattr.typtr := fcp^.idtype
  end;

  procedure expression{(fsys: setofsys; threaten: boolean)};
    var lattr: attr; lop: operatort; typind: char; lsize, lsizspc: addrrange;
        fcp: ctp; onstkl, onstkr, lschrcst, rschrcst, revcmp: boolean;

    procedure simpleexpression(fsys: setofsys; threaten: boolean);
      var lattr: attr; lop: operatort; fsy: symbol; fop: operatort; fcp: ctp;

      procedure term(fsys: setofsys; threaten: boolean);
        var lattr: attr; lop: operatort; fcp: ctp;

        procedure factor(fsys: setofsys; threaten: boolean);
          var lcp,fcp: ctp; lvp: csp; varpart: boolean; inherit: boolean;
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
                  begin searchid([types,konst,vars,fixedt,field,func,proc],lcp);
                    insymbol;
                    if hasovlfunc(lcp) or hasovlproc(lcp) then
                      begin call(fsys,lcp, inherit, true);
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
                              if part = ptview then error(290)
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
                              begin form:=arrays; aeltype := charptr;
                                packing := true; inxtype := nil; tmpl := -1;
                                size := lgth*charsize
                              end;
                            arrtmp(lsp); { output fixed template }
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
                    if gattr.kind <> expr then
                      if gattr.typtr <> nil then
                        if gattr.typtr^.form <= power then load else loadaddress;
                    fndopr1(notop, fcp);
                    if fcp <> nil then callop1(fcp) else begin
                      if (gattr.typtr = boolptr) or
                         ((gattr.typtr = intptr) and not iso7185) then
                        gen0t(19(*not*),gattr.typtr)
                      else begin error(135); gattr.typtr := nil end
                    end
                  end;
        (*[*)     lbrack:
                  begin insymbol; cstpart := [ ]; varpart := false;
                    new(lsp,power); pshstc(lsp);
                    with lsp^ do
                      begin form:=power; elset:=nil;size:=setsize;
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
                              else begin error(137); gattr.typtr := nil end;
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
          begin
            if gattr.kind <> expr then
              if gattr.typtr <> nil then
                if gattr.typtr^.form <= power then load else loadaddress;
            lattr := gattr; lop := op;
            insymbol; factor(fsys + [mulop], threaten);
            if gattr.kind <> expr then
              if gattr.typtr <> nil then
                if gattr.typtr^.form <= power then load else loadaddress;
            if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
              case lop of
      (***)     mul: begin fndopr2(lop, lattr, fcp);
                  if fcp <> nil then callop2(fcp, lattr) else begin
                    if (lattr.typtr=intptr) and (gattr.typtr=intptr)
                        then gen0(15(*mpi*))
                        else
                          begin
                            { convert either integer to real }
                            if lattr.typtr = intptr then
                              begin gen0(9(*flo*));
                                lattr.typtr := realptr
                              end
                            else
                              if gattr.typtr = intptr then
                                begin gen0(10(*flt*));
                                  gattr.typtr := realptr
                                end;
                            if (lattr.typtr = realptr) and
                               (gattr.typtr=realptr) then gen0(16(*mpr*))
                            else if (lattr.typtr^.form=power) and
                                    comptypes(lattr.typtr,gattr.typtr) then
                              gen0(12(*int*))
                            else begin error(134); gattr.typtr:=nil end
                          end
                  end
                end;
      (* / *)   rdiv: begin fndopr2(lop, lattr, fcp);
                  if fcp <> nil then callop2(fcp, lattr) else begin
                    { convert either integer to real }
                    if gattr.typtr = intptr then
                      begin gen0(10(*flt*)); gattr.typtr := realptr end;
                    if lattr.typtr = intptr then
                      begin gen0(9(*flo*)); lattr.typtr := realptr end;
                    if (lattr.typtr = realptr) and
                       (gattr.typtr=realptr) then gen0(7(*dvr*))
                    else begin error(134); gattr.typtr := nil end
                  end
                end;
      (*div*)   idiv: begin fndopr2(lop, lattr, fcp);
                  if fcp <> nil then callop2(fcp, lattr) else begin
                    if (lattr.typtr = intptr) and (gattr.typtr = intptr) then
                      gen0(6(*dvi*))
                    else begin error(134); gattr.typtr := nil end
                  end
                end;
      (*mod*)   imod: begin fndopr2(lop, lattr, fcp);
                  if fcp <> nil then callop2(fcp, lattr) else begin
                    if (lattr.typtr = intptr) and (gattr.typtr = intptr) then
                      gen0(14(*mod*))
                    else begin error(134); gattr.typtr := nil end
                  end
                end;
      (*and*)   andop: begin fndopr2(lop, lattr, fcp);
                  if fcp <> nil then callop2(fcp, lattr) else begin
                    if ((lattr.typtr = boolptr) and (gattr.typtr = boolptr)) or
                       ((lattr.typtr=intptr) and (gattr.typtr=intptr) and
                        not iso7185) then gen0(4(*and*))
                    else begin error(134); gattr.typtr := nil end
                  end
                end
              end (*case*)
            else gattr.typtr := nil
          end (*while*)
      end (*term*) ;

    begin (*simpleexpression*)
      fsy := sy; fop := op;
      if (sy = addop) and (op in [plus,minus]) then insymbol;
      term(fsys + [addop], threaten);
      if (fsy = addop) and (fop in [plus, minus]) then begin
        if gattr.kind <> expr then
            if gattr.typtr <> nil then
              if gattr.typtr^.form <= power then load else loadaddress;
        fndopr1(fop, fcp);
        if fcp <> nil then callop1(fcp) else begin
          if fop = minus then begin
            if gattr.typtr = intptr then gen0(17(*ngi*))
            else
              if gattr.typtr = realptr then gen0(18(*ngr*))
              else begin error(134); gattr.typtr := nil end
          end else begin
            if (gattr.typtr <> intptr) and
               (gattr.typtr <> realptr) then
              begin error(134); gattr.typtr := nil end
          end
        end
      end;
      while sy = addop do
        begin
          if gattr.kind <> expr then
            if gattr.typtr <> nil then
              if gattr.typtr^.form <= power then load else loadaddress;
          lattr := gattr; lop := op;
          insymbol; term(fsys + [addop], threaten);
          if gattr.kind <> expr then
            if gattr.typtr <> nil then
              if gattr.typtr^.form <= power then load else loadaddress;
          if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
            case lop of
    (*+,-*)    plus,minus: begin fndopr2(lop, lattr, fcp);
                 if fcp <> nil then callop2(fcp, lattr) else begin
                   if (lattr.typtr = intptr) and (gattr.typtr = intptr) then begin
                     if lop = plus then gen0(2(*adi*)) else gen0(21(*sbi*))
                   end else begin
                     { convert either integer to real }
                     if lattr.typtr = intptr then
                       begin gen0(9(*flo*));
                         lattr.typtr := realptr
                       end
                     else
                       if gattr.typtr = intptr then
                         begin gen0(10(*flt*));
                           gattr.typtr := realptr
                         end;
                     if (lattr.typtr = realptr) and
                        (gattr.typtr = realptr) then begin
                       if lop = plus then gen0(3(*adr*)) else gen0(22(*sbr*))
                     end else if (lattr.typtr^.form=power) and
                                 comptypes(lattr.typtr,gattr.typtr) then begin
                       if lop = plus then gen0(28(*uni*)) else gen0(5(*dif*))
                     end else begin error(134); gattr.typtr:=nil end
                   end
                 end
               end;
    (*or,xor*) orop, xorop: begin fndopr2(lop, lattr, fcp);
                 if fcp <> nil then callop2(fcp, lattr) else begin
                   if ((lattr.typtr=boolptr) and (gattr.typtr=boolptr)) or
                      ((lattr.typtr=intptr) and (gattr.typtr=intptr) and
                       not iso7185) then begin
                      if lop = orop then gen0(13(*ior*)) else gen0(83(*ixor*))
                   end else begin error(134); gattr.typtr := nil end
                 end
               end
            end (*case*)
          else gattr.typtr := nil
        end (*while*)
    end (*simpleexpression*) ;

  begin (*expression*)
    revcmp := false;
    simpleexpression(fsys + [relop], threaten);
    onstkl := gattr.kind = expr; lschrcst := ischrcst(gattr);
    if sy = relop then begin
      if gattr.typtr <> nil then
        if gattr.typtr^.form <= power then load
        else loadaddress;
      lattr := gattr; lop := op;
      if (lop = inop) and (gattr.typtr <> nil) then
        if not comptypes(gattr.typtr,intptr) and 
          (gattr.typtr^.form <= subrange) then
            gen0t(58(*ord*),gattr.typtr);
      insymbol; simpleexpression(fsys, threaten);
      onstkr := gattr.kind = expr; rschrcst := ischrcst(gattr);
      if gattr.typtr <> nil then
        if gattr.typtr^.form <= power then load
        else loadaddress;
      if (lattr.typtr <> nil) and (gattr.typtr <> nil) then begin
        fndopr2(lop, lattr, fcp);
        if fcp <> nil then callop2(fcp, lattr) else begin
          if lop = inop then
            if gattr.typtr^.form = power then
              if comptypes(lattr.typtr,gattr.typtr^.elset) then
                gen0(11(*inn*))
              else begin error(129); gattr.typtr := nil end
            else begin error(130); gattr.typtr := nil end
          else
            begin
              { convert either integer to real }
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
              if comptypes(lattr.typtr,gattr.typtr) or
                 (lschrcst and (gattr.typtr^.form = arrayc)) or
                 ((lattr.typtr^.form = arrayc) and rschrcst) then
                begin lsize := lattr.typtr^.size;
                  typind := ' ';
                  case lattr.typtr^.form of
                    scalar:
                      if lschrcst and (gattr.typtr^.form = arrayc) then
                        begin
                          { load char ptr under }
                          gen2(51(*ldc*),1,1);
                          gensca(chr(gattr.cval.ival));
                          typind := 'v';
                          revcmp := true
                        end
                      else if lattr.typtr = realptr then typind := 'r'
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
                    arrays, arrayc:
                      begin
                        if not stringt(lattr.typtr) then error(134);
                        if rschrcst and (lattr.typtr^.form = arrayc) then begin 
                          gen1(71(*dmp*),intsize); { discard char }
                          { rationalize character }
                          gen2(51(*ldc*),1,1);
                          gensca(chr(gattr.cval.ival));
                          typind := 'v'
                        end else begin
                          lsizspc := lsize; alignu(parmptr,lsizspc);
                          if (lattr.typtr^.form = arrayc) or
                             (gattr.typtr^.form = arrayc) then typind := 'v'
                          else typind := 'm';
                          containerop(lattr); { rationalize binary container }
                          if onstkr then begin { pull left up }
                            gen1(118(*lsa*),ptrsize+lsizspc);
                            gen1t(35(*ind*),0,nilptr);
                            gen1(72(*swp*),stackelsize)
                          end
                        end
                      end;
                    records:
                      begin
                        error(134);
                        typind := 'm'
                      end;
                    files:
                      begin error(133); typind := 'f' end
                  end;
                  if typind <> ' ' then if revcmp then begin 
                    case lop of
                      { reverse flipped operands }
                      ltop: gen2(49(*grt*),ord(typind),lsize);
                      leop: gen2(48(*geq*),ord(typind),lsize);
                      gtop: gen2(53(*les*),ord(typind),lsize);
                      geop: gen2(52(*leq*),ord(typind),lsize);
                      neop: gen2(55(*neq*),ord(typind),lsize);
                      eqop: gen2(47(*equ*),ord(typind),lsize)
                    end;
                    gen1(72(*swp*),intsize); { swap for previous const }
                    gen1(71(*dmp*),ptrsize) { dump it }
                  end else case lop of
                    ltop: gen2(53(*les*),ord(typind),lsize);
                    leop: gen2(52(*leq*),ord(typind),lsize);
                    gtop: gen2(49(*grt*),ord(typind),lsize);
                    geop: gen2(48(*geq*),ord(typind),lsize);
                    neop: gen2(55(*neq*),ord(typind),lsize);
                    eqop: gen2(47(*equ*),ord(typind),lsize)
                  end;
                  if lattr.typtr^.form = arrays then begin
                    alignu(parmptr,lsize);
                    if onstkl and onstkr then 
                      begin gen1(72(*swp*),lsizspc*2); gen1(71(*dmp*),lsizspc*2) 
                      end
                    else if onstkr then 
                      begin gen1(72(*swp*),lsizspc+ptrsize); 
                            gen1(71(*dmp*),lsizspc+ptrsize) end
                    else if onstkl then 
                      begin gen1(72(*swp*),lsizspc); gen1(71(*dmp*),lsizspc) end
                  end
                end
              else error(129)
            end;
          gattr.typtr := boolptr; gattr.kind := expr
        end
      end
    end (*sy = relop*)
  end (*expression*) ;

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
          lcp2^.refer := true;
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
          lsize,displ: addrrange; lmin,lmax, span: integer;
          test: boolean; ispacked: boolean; lvalu: valu;

      procedure simpletype(fsys:setofsys; var fsp:stp; var fsize:addrrange);
        var lsp,lsp1: stp; lcp,lcp1: ctp; ttop: disprange;
            lcnt: integer; lvalu: valu; t: integer;
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
                  begin form := scalar; size := intsize; scalkind := declared;
                    packing := false
                  end;
                lcp1 := nil; lcnt := 0;
                repeat insymbol;
                  if sy = ident then
                    begin new(lcp,konst); ininam(lcp);
                      with lcp^ do
                        begin klass := konst; strassvf(name, id); idtype := lsp;
                          next := lcp1; values.intval := true;
                          values.ival := lcnt;
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
                        if sy = range then insymbol else error(30);
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
                    if sy = range then insymbol else error(30);
                    constexpr(fsys,lsp1,lvalu);
                    if lvalu.intval then lsp^.max := lvalu
                    else begin lsp^.max.intval := true; lsp^.max.ival := 1 end;
                    if lsp^.rangetype <> lsp1 then error(107);
                    if isbyte(lsp) then lsp^.size := 1;
                    fsize := lsp^.size
                  end;
                if lsp <> nil then
                  with lsp^ do
                    if form = subrange then begin
                      if rangetype <> nil then
                        if rangetype = realptr then
                          begin error(109); rangetype := intptr end;
                      if min.ival > max.ival then
                        begin error(102);
                          { swap to fix and suppress further errors }
                          t := min.ival; min.ival := max.ival; max.ival := t
                        end
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
            test: boolean; mm: boolean; varlnm, varcn, varcmx: varinx;
            varcof: boolean; tagp,tagl: ttp; mint, maxt: integer; ferr: boolean;
        procedure ordertag(var tp: ttp);
          var lp, p, p2, p3: ttp;
        begin
          if tp <> nil then begin
            lp := tp; tp := tp^.next; lp^.next := nil;
            while tp <> nil do begin
              p := tp;  tp := tp^.next; p^.next := nil; p2 := lp; p3 := nil;
              while (p^.ival > p2^.ival) and (p2^.next <> nil) do
                begin p3 := p2; p2 := p2^.next end;
              if p^.ival > p2^.ival then p2^.next := p
              else if p3 = nil then begin p^.next := lp; lp := p end
              else begin p^.next := p3^.next; p3^.next := p end
            end
          end;
          tp := lp
        end;
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
                    begin klass := field; strassvf(name, id); idtype := nil;
                      next := nxt; fldaddr := 0; varnt := vartyp;
                      varlb := varlab; tagfield := false; taglvl := lvl;
                      varsaddr := 0; varssize := 0; vartl := -1
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
            if lsp <> nil then
              if lsp^.form = arrayc then error(272);
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
              begin form := tagfld; tagfieldp := nil; fstvar := nil;
                    packing := false; new(vart);
                    for varcn := 0 to varmax do vart^[varcn] := 0
              end;
            varlnm := 1; varcof := false; varcmx := 0;
            frecvar := lsp;
            insymbol;
            if sy = ident then
              begin
                { find possible type first }
                searchidnenm([types],lcp1,mm);
                { now set up as field id }
                new(lcp,field); ininam(lcp);
                with lcp^ do
                  begin klass:=field; strassvf(name, id); idtype := nil;
                    next := nil; fldaddr := displ; varnt := vartyp;
                    varlb := varlab; tagfield := true; taglvl := lvl;
                    varsaddr := 0; varssize := 0; vartl := -1
                  end;
                lsp^.tagfieldp := lcp;
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
                        begin if comptypes(realptr,lsp1) then error(159)
                          else if stringt(lsp1) then error(159);
                          lcp^.idtype := lsp1
                        end
                      else error(110);
                    end
                  end
              end
            else begin error(2); skip(fsys + [ofsy,lparent]) end;
            lsp^.size := displ;
            if sy = ofsy then insymbol else error(8);
            lsp1 := nil; minsize := displ; maxsize := displ;
            tagl := nil;
            mint := -maxint; maxt := maxint;
            if lsp^.tagfieldp <> nil then
              if lsp^.tagfieldp^.idtype <> nil then begin
              getbounds(lsp^.tagfieldp^.idtype, mint, maxt);
              if maxt-mint+1 > varmax then error(239)
            end;
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
                    gettag(tagp); tagp^.ival := lvalu.ival; tagp^.next := tagl;
                    tagl := tagp;
                    new(lsp3,variant); pshstc(lsp3);
                    with lsp3^ do
                      begin form := variant; varln := varlnm;
                            nxtvar := lsp1; subvar := lsp2; varval := lvalu;
                            caslst := lsp2; packing := false
                      end;
                    if (lvalu.ival >= 0) and (lvalu.ival <= varmax) then
                      lsp^.vart^[lvalu.ival] := varlnm; { set case to logical }
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
                  if lvalu.ival-1 > varcmx then varcmx := lvalu.ival-1;
                  if lvalu.ival > varmax then
                    { errors supressed for multiple overflows in list }
                    begin if not varcof then error(239); varcof := true end;
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
              varlnm := varlnm+1;
              test := sy <> semicolon;
              if not test then
                begin displ := minsize;
                      insymbol
                end
            until test;
            displ := maxsize;
            lsp^.fstvar := lsp1;
            lsp^.varts := 0;
            if lcp <> nil then begin
              if varcmx >= 0 then lsp^.varts := varcmx+1;
              { output LVN table }
              if prcode then begin
                write(prr, 'v',' ':7);
                genlabel(lcp^.vartl); prtlabel(lcp^.vartl);
                write(prr, ' ', lsp^.varts:1);
                for varcn := 0 to lsp^.varts-1 do
                  write(prr, ' ', lsp^.vart^[varcn]:1);
                writeln(prr)
              end
            end;
            if lsp^.tagfieldp <> nil then begin
              ordertag(tagl);
              tagp := tagl; ferr := false;
              while (tagp <> nil) and (mint <= maxt) and not ferr do begin
                if tagp^.ival <> mint then begin error(200); ferr := true end
                else begin mint := mint+1; tagp := tagp^.next end
              end;
              if (mint <= maxt) and not ferr then error(200)
            end;
            while tagl <> nil do
              begin tagp := tagl; tagl := tagl^.next; puttag(tagp) end
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
                  begin form:=pointer; eltype := nil; size := ptrsize;
                        packing := false end;
                insymbol;
                if sy = ident then
                  begin { forward reference everything }
                    new(lcp,types); ininam(lcp);
                    with lcp^ do
                      begin klass := types; strassvf(name,id); idtype := lsp;
                        next := fwptr;
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
                    if (sy = ofsy) and not iso7185 then begin
                      lsp1 := nil;
                      { process container array }
                      new(lsp,arrayc); pshstc(lsp);
                      with lsp^ do
                          begin form:=arrayc; abstype := lsp1;
                                packing := ispacked end;
                      lsp1 := lsp
                    end else if (sy <> lbrack) and not iso7185 then begin
                      { process Pascaline array }
                      lsp1 := nil;
                      repeat new(lsp,arrays); pshstc(lsp);
                        with lsp^ do
                          begin form:=arrays; aeltype := lsp1; inxtype := nil;
                                tmpl := -1; packing := ispacked end;
                        lsp1 := lsp;
                        constexpr(fsys+[comma,ofsy],lsp2,lvalu);
                        if lsp2 <> nil then if lsp2 <> intptr then error(15);
                        if not lvalu.intval then
                          begin lvalu.intval := true; lvalu.ival := 1 end;
                        if lvalu.ival <= 0 then
                          begin error(238); lvalu.ival := 1 end;
                        lsp1^.size := lsize;
                        { build subrange type based on 1..n }
                        new(lsp2,subrange); pshstc(lsp2);
                          with lsp2^ do
                            begin form := subrange; rangetype := intptr;
                                  min.ival := 1; max := lvalu end;
                        lsp^.inxtype := lsp2;
                        test := sy <> comma;
                        if not test then insymbol
                      until test
                    end else begin if sy = lbrack then insymbol;
                      { process ISO 7185 array }
                      lsp1 := nil;
                      repeat new(lsp,arrays); pshstc(lsp);
                        with lsp^ do
                          begin form:=arrays; aeltype := lsp1; inxtype := nil;
                                tmpl := -1; packing := ispacked end;
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
                      with lsp1^ do begin
                        if lsp1^.form = arrays then begin
                          if lsp <> nil then 
                            if lsp^.form = arrayc then error(272);
                          lsp2 := aeltype; aeltype := lsp;
                          if inxtype <> nil then begin
                            getbounds(inxtype,lmin,lmax);
                            span := lmax-lmin+1;
                            if span < 1 then error(509);
                            if lsize > pmmaxint div span then
                              begin error(237); lsize := 1 end
                            else lsize := lsize*span;
                            size := lsize
                          end;
                          arrtmp(lsp1) { output fixed template }
                        end else
                          { note containers are only one deep, and have no size }
                          begin lsp2 := abstype; abstype := lsp; size := 0 end
                      end;
                      lsp := lsp1; lsp1 := lsp2
                    until lsp1 = nil
                  end
                else
    (*record*)      if sy = recordsy then
                    begin insymbol;
                      oldtop := top;
                      if top < displimit then
                        begin top := top + 1; inidsp(display[top]);
                              display[top].occur := rec
                        end
                      else error(250);
                      displ := 0;
                      fieldlist(fsys-[semicolon]+[endsy],lsp1,nil,nil,1,lcp);
                      new(lsp,records);
                      with lsp^ do
                        begin form := records; fstfld := display[top].fname;
                          display[top].fname := nil;
                          recvar := lsp1; size := displ;
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
                          begin form:=power; elset:=lsp1; size:=setsize;
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
                              begin form := files; filtype := lsp1;
                                 size := filesize+lsize; packing := ispacked
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
            begin klass:=konst; strassvf(name, id); idtype := nil; next := nil;
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
            begin klass := types; strassvf(name, id); idtype := nil;
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

    procedure wrtsym(lcp: ctp; typ: char);
    begin
      if prcode then begin
        with lcp^ do begin
          write(prr, 's',' ':7);
          writev(prr, name, lenpv(name)); write(prr, ' ', typ);
          write(prr, ' ', vaddr:1, ' '); wrttyp(prr, idtype); writeln(prr)
        end
      end
    end;

    procedure vardeclaration;
      var lcp,nxt: ctp; lsp: stp; lsize: addrrange;
          test: boolean; maxpar, curpar: integer; cc: integer;
    begin nxt := nil;
      repeat { id:type group }
        maxpar := 0;
        repeat {ids }
          lcp := nil;
          if sy = ident then
            begin new(lcp,vars); ininam(lcp); curpar := 0;
              with lcp^ do
               begin klass := vars; strassvf(name, id); next := nxt;
                  idtype := nil; vkind := actual; vlev := level;
                  refer := false; isloc := false; threat := false; forcnt := 0;
                  part := ptval; hdr := false; vext := incact; 
                  vmod := incstk; inilab := -1; ininxt := nil; dblptr := false;
                end;
              enterid(lcp);
              nxt := lcp;
              insymbol;
            end
          else error(2);
          if (sy = lparent) and not iso7185 then begin
            { parameterized type specification }
            if (nxt <> nil) and (lcp <>nil) then begin { gen code strip label }
              lcp^.ininxt := display[top].inilst; display[top].inilst := lcp;
              genlabel(lcp^.inilab); putlabel(lcp^.inilab); 
              genlabel(lcp^.skplab)
            end;
            insymbol;
            repeat
              expression(fsys+[comma,rparent], false); load; curpar := curpar+1;
              if gattr.typtr <> nil then
                    if basetype(gattr.typtr) <> intptr then error(243);
              if not (sy in [comma,rparent]) then
                begin error(27);
                      skip(fsys+[comma,rparent,colon,semicolon]+typedels) end;
              test := sy <> comma;
              if not test then insymbol
            until test;
            if lcp <> nil then genujpxjpcal(57(*ujp*),lcp^.skplab);
            if sy = rparent then insymbol else error(4)
          end;
          if (maxpar <> 0) and (curpar <> maxpar) then error(269);
          if curpar > maxpar then maxpar := curpar;
          if not (sy in fsys + [comma,colon] + typedels) then
            begin error(6); skip(fsys+[comma,colon,semicolon]+typedels) end;
          test := sy <> comma;
          if not test then insymbol
        until test;
        { At this point, initializers, if they exist, are on stack in groups
          according to the id they belong to, and maxpar indicates how many per
          id. This must be so because we don't know the type or location of the
          assocated variable yet. }
        if sy = colon then insymbol else error(5);
        typ(fsys + [semicolon] + typedels,lsp,lsize);
        cc := containers(lsp); { find # containers }
        if cc > 0 then
          { change variable from size of base to pointer+template for containers }
          lsize := ptrsize+cc*intsize;
        resolvep; { resolve pointer defs before symbol generate }
        if lsp <> nil then
          if (lsp^.form = arrayc) and (maxpar = 0) then error(270)
          else if maxpar <> containers(lsp) then error(271);
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
              if maxpar > 0 then begin
                putlabel(nxt^.skplab);
                { load variable address }
                if level <= 1 then gen1(37(*lao*),vaddr)
                else gen2(50(*lda*),level-(level-vlev),vaddr);
                if level <= 1 then
                  { issue vector init ptr instruction }
                  gen2(97(*vip*),maxpar,containerbase(lsp))
                else
                  { issue vector init stack instruction }
                  gen2(96(*vis*),maxpar,containerbase(lsp));
                gen0(90(*ret*)); { issue code strip return }
                { remove initializers, var addr }
                mesl(maxpar*intsize+adrsize)
              end;
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

    procedure fixeddeclaration;
      var lcp: ctp; lsp: stp; lsize: addrrange;
          v: integer; d: boolean; dummy: stp;
    procedure fixeditem(fsys: setofsys; lsp: stp; size: integer; var v: integer; var d: boolean);
      var fvalu: valu; lsp1: stp; lcp: ctp; i, min, max: integer;
          test: boolean;
    begin v := 0; d := false;
      if lsp <> nil then begin
        case lsp^.form of
          scalar: if lsp^.scalkind = declared then begin
                    { enumerated type }
                    if sy = ident then begin
                      searchid([konst],lcp);
                      if not comptypes(lsp, lcp^.idtype) then error(245);
                      if lcp^.values.intval then begin
                        if prcode then begin
                          if lsp = boolptr then
                            writeln(prr, 'c b ', lcp^.values.ival:1)
                          else if size = 1 then
                            writeln(prr, 'c x ', lcp^.values.ival:1)
                          else writeln(prr, 'c i ', lcp^.values.ival:1);
                        end;
                        v := lcp^.values.ival; d := true
                      end else error(513);
                      insymbol
                    end else error(2)
                  end else begin
                    { get value to satisfy entry }
                    constexpr(fsys,lsp1,fvalu);
                    if lsp1 <> nil then
                      if (lsp = realptr) and (lsp1 = intptr) then begin
                        { integer to real, convert }
                        if not fvalu.intval then error(515)
                        else if prcode then writeln(prr, 'c r ', fvalu.ival:1)
                      end else if comptypes(lsp, lsp1) then begin
                        { constants possible are i: integer, r: real,
                          p: (power) set, s: string (including set), c: char,
                          b: boolean, x: byte integer }
                        if lsp = charptr then begin
                          if fvalu.intval then begin
                            if prcode then write(prr, 'c c ', fvalu.ival:1);
                            v := fvalu.ival; d := true
                          end
                        end else if fvalu.intval then begin
                          if (size = 1) and prcode then
                            write(prr, 'c x ', fvalu.ival:1)
                          else write(prr, 'c i ', fvalu.ival:1);
                          v := fvalu.ival; d := true
                        end else if (fvalu.valp^.cclass = reel) and prcode then
                          write(prr, 'c r ', fvalu.valp^.rval:23); 
                        if prcode then writeln(prr)
                      end else error(245)
                  end;
          subrange: begin fixeditem(fsys,lsp^.rangetype,lsp^.size,v,d);
                      if d then
                        if (v < lsp^.min.ival) or (v > lsp^.min.ival) then
                          error(246)
                    end;
          power: begin { get value to satisfy entry }
                   constexpr(fsys,lsp1,fvalu);
                   if comptypes(lsp, lsp1) then begin
                     if prcode then begin
                       write(prr, 'c p (');
                       for i := setlow to sethigh do
                         if i in fvalu.valp^.pval then write(prr,' ',i:1);
                       writeln(prr, ')')
                     end
                   end else error(245)
                 end;
          arrays: begin getbounds(lsp^.inxtype, min, max);
                    if (sy = stringconst) and stringt(lsp) then begin
                      constexpr(fsys,lsp1,fvalu);
                      if comptypes(lsp, lsp1) then begin
                        { string constant matches array }
                        if fvalu.valp^.slgth <> max then error(245);
                        if prcode then begin
                          write(prr, 'c ');
                          write(prr, 's ''');
                          writev(prr, fvalu.valp^.sval, fvalu.valp^.slgth);
                          writeln(prr, '''')
                        end
                      end else error(245)
                    end else begin
                      { iterate array elements }
                      i := min; if sy = arraysy then insymbol else error(28);
                      repeat
                        fixeditem(fsys+[comma,endsy],lsp^.aeltype, lsp^.aeltype^.size, v, d);
                        i := i+1;
                        if not (sy in [comma,endsy]) then
                          begin error(29); skip(fsys+[comma,endsy]+typedels) end;
                        test := sy <> comma;
                        if not test then insymbol
                      until test;
                      if i-1 <> max then error(247);
                      if sy = endsy then insymbol else error(13)
                    end
                  end;
          records: begin lcp := lsp^.fstfld;
                     if lsp^.recvar <> nil then error(248);
                     if sy = recordsy then insymbol else error(28);
                     i := 1; max := 1;
                     repeat
                       if lcp = nil then
                         { ran out of data items, dummy parse a constant }
                         constexpr(fsys+[comma,endsy],dummy,fvalu)
                       else fixeditem(fsys+[comma,endsy],lcp^.idtype,
                                      lcp^.idtype^.size, v, d);
                       max := max+1;
                       if lcp <> nil then begin lcp := lcp^.next; i := i+1 end;
                       if not (sy in [comma,endsy]) then
                         begin error(29); skip(fsys+[comma,endsy]+typedels) end;
                       test := sy <> comma;
                       if not test then insymbol
                     until test;
                     if i <> max then error(247);
                     if sy = endsy then insymbol else error(13)
                   end;
          pointer, arrayc, files, tagfld, variant,exceptf: error(244);
        end
      end
    end;
    begin
      repeat { id:type group }
        lcp := nil;
        if sy = ident then
          begin new(lcp,fixedt); ininam(lcp);
            with lcp^ do
             begin klass := fixedt; strassvf(name, id);
               idtype := nil; floc := -1; fext := incact; fmod := incstk
             end;
            enterid(lcp);
            insymbol;
          end
        else error(2);
        if not (sy in fsys + [colon] + typedels) then
          begin error(6); skip(fsys+[comma,colon,semicolon]+typedels) end;
        if sy = colon then insymbol else error(5);
        typ(fsys + [semicolon,relop] + typedels,lsp,lsize);
        if lcp <> nil then lcp^.idtype := lsp;
        if (sy = relop) and (op = eqop) then begin
          insymbol;
          { start fixed constants }
          if prcode then write(prr, 'n ');
          genlabel(lcp^.floc); prtlabel(lcp^.floc);
          if prcode then writeln(prr, ' ', lsize:1);
          fixeditem(fsys+[semicolon], lsp, lsp^.size, v, d);
          if prcode then writeln(prr, 'x')
        end else error(16);
        if sy = semicolon then
          begin insymbol;
            if not (sy in fsys + [ident]) then
              begin error(6); skip(fsys + [ident]) end
          end
        else error(14)
      until (sy <> ident) and not (sy in typedels)
    end (*fixeddeclaration*) ;

    procedure procdeclaration(fsy: symbol);
      var oldlev: 0..maxlevel; lcp,lcp1,lcp2: ctp; lsp: stp;
          forw,extl,virt,ovrl: boolean; oldtop: disprange;
          llc: stkoff; lbname: integer; plst: boolean; fpat: fpattr;
          ops: restr; opt: operatort;

      procedure pushlvl(forw: boolean; lcp: ctp);
      begin
        if level < maxlevel then level := level + 1 else error(251);
        if top < displimit then
          begin top := top + 1;
            with display[top] do
              begin inidsp(display[top]);
                if forw then fname := lcp^.pflist;
                { use the defining point status of the parent block }
                define := display[top-1].define;
                occur := blck; bname := lcp
              end
          end
        else error(250);
      end;

      procedure parameterlist(fsy: setofsys; var fpar: ctp; var plst: boolean;
                              opr: boolean; opt: operatort);
        var lcp,lcp1,lcp2,lcp3: ctp; lsp: stp; lkind: idkind;
          llc,lsize: addrrange; count: integer; pt: partyp;
          oldlev: 0..maxlevel; oldtop: disprange;
          oldlevf: 0..maxlevel; oldtopf: disprange;
          lcs: addrrange; test: boolean; dummy: boolean; first: boolean;
      procedure joinlists;
      var lcp, lcp3: ctp;
      begin first := true;
        { we missed the type for this id list, meaning the types are nil. Add
          the new list as is for error recovery }
        if lcp2 <> nil then begin
          lcp3 := lcp2; { save sublist head }
          { find sublist end }
          lcp := nil;
          while lcp2 <> nil do begin lcp := lcp2; lcp2 := lcp2^.next end;
          { join lists }
          lcp^.next := lcp1;
          lcp1 := lcp3
        end
      end;
      begin { parameterlist }
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
                    insymbol; lcp := nil; if opr then error(285);
                    if sy = ident then
                      begin new(lcp,proc,declared,formal); ininam(lcp);
                        lc := lc-ptrsize*2; { mp and addr }
                        alignd(parmptr,lc);
                        with lcp^ do
                          begin klass:=proc; strassvf(name, id); idtype := nil;
                            next := lcp1;
                            pflev := level (*beware of parameter procedures*);
                            pfdeckind:=declared; pflist := nil;
                            pfkind:=formal; pfaddr := lc; pext := false;
                            pmod := nil; keep := true; pfattr := fpanone;
                            grpnxt := nil; grppar := lcp; pfvid := nil
                          end;
                        enterid(lcp);
                        lcp1 := lcp;
                        insymbol
                      end
                    else error(2);
                    oldlev := level; oldtop := top; pushlvl(false, lcp);
                    lcs := lc; parameterlist([semicolon,rparent],lcp2,dummy, false, noop);
                    lc := lcs; if lcp <> nil then lcp^.pflist := lcp2;
                    if not (sy in fsys+[semicolon,rparent]) then
                      begin error(7);skip(fsys+[semicolon,rparent]) end;
                    level := oldlev; putdsps(oldtop); top := oldtop
                  end
                else
                  begin
                    if sy = funcsy then
                      begin lcp2 := nil; if opr then error(285);
                        insymbol;
                        if sy = ident then
                          begin new(lcp,func,declared,formal); ininam(lcp);
                            lc := lc-ptrsize*2; { mp and addr }
                            alignd(parmptr,lc);
                            with lcp^ do
                              begin klass:=func; strassvf(name, id);
                                idtype := nil; next := lcp1;
                                pflev := level (*beware param funcs*);
                                pfdeckind:=declared; pflist := nil;
                                pfkind:=formal; pfaddr:=lc; pext := false;
                                pmod := nil; keep := true; pfattr := fpanone;
                                grpnxt := nil; grppar := lcp; pfvid := nil
                              end;
                            enterid(lcp);
                            lcp1 := lcp;
                            insymbol;
                          end
                        else error(2);
                        oldlev := level; oldtop := top; pushlvl(false, lcp);
                        lcs := lc;
                        parameterlist([colon,semicolon,rparent],lcp2,dummy, false, noop);
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
                        if opr then begin
                          if first and (opt = bcmop) then begin
                            if pt <> ptout then error(288)
                          end else if opr and (pt <> ptval) and (pt <> ptview) then
                            error(286)
                        end;
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
                                begin klass:=vars; strassvf(name,id);
                                  idtype:=nil; vkind := lkind; next := lcp2;
                                  vlev := level; keep := true; refer := false;
                                  isloc := false; threat := false; forcnt := 0; 
                                  part := pt; hdr := false; vext := false; 
                                  vmod := nil; vaddr := 0; inilab := -1; 
                                  ininxt := nil; dblptr := true
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
                                if lsp <> nil then begin
                                  if lsp^.form = arrayc then lsize := ptrsize*2;
                                  if lkind=actual then begin
                                    if lsp^.form<=power then lsize := lsp^.size
                                    else if lsp^.form=files then error(121);
                                    { type containing file not allowed either }
                                    if filecomponent(lsp) then error(121)
                                  end
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
                                          if (lsp^.form >= power) and
                                             (part = ptview) then
                                            vkind := formal
                                      end;
                                    lcp2 := lcp2^.next
                                  end;
                                lcp^.next := lcp1; lcp1 := lcp3;
                                insymbol
                              end
                            else begin error(2); joinlists end;
                            if not (sy in fsys + [semicolon,rparent]) then
                              begin error(7);skip(fsys+[semicolon,rparent])end
                          end
                        else begin error(5); joinlists end
                      end;
                  end;
                first := false;
                if sy = semicolon then
                  begin insymbol;
                    if not (sy in fsys + [ident,varsy,procsy,funcsy,viewsy,outsy]) then
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
            lc := -level*ptrsize; { set locals top }
            while lcp1 <> nil do
              with lcp1^ do
                begin lcp2 := next; next := lcp3;
                  if klass = vars then
                    if idtype <> nil then
                      { if value variable, and structured, we make a copy to
                        play with. However, structured is treated as var if
                        it is view, since that is protected }
                      if (vkind=actual) and (idtype^.form>power) and
                         (idtype^.form <> arrayc) then
                        begin
                          lc := lc-idtype^.size;
                          alignd(parmptr,lc);
                          vaddr := lc;
                          isloc := true { flag is a local now }
                        end;
                  lcp3 := lcp1; lcp1 := lcp2
                end;
            fpar := lcp3
          end else begin fpar := nil; lc := -level*ptrsize end;
        if forw then begin { isolate duplicated list in level }
          level := oldlevf; putdsps(oldtopf); top := oldtopf
        end
    end (*parameterlist*) ;

    { for overloading, same as strict cmpparlst(), but includes read = integer
      and string = char }
    function compparamovl(pla, plb: ctp): boolean;
      var f: boolean; t1, t2: stp;
    begin f := true;
      while (pla <> nil) and (plb <> nil) do begin
        if not cmppar(pla,plb) then begin
          { incompatible, but check special cases }
          t1 := basetype(pla^.idtype);
          t2 := basetype(plb^.idtype);
          if not ((intt(t1) and realt(t2)) or
                 (realt(t1) and intt(t2)) or
                 (chart(t1) and chart(t2))) then f := false
        end;
        pla := pla^.next; plb := plb^.next
      end;
      if (pla <> nil) or (plb <> nil) then f := false;
      compparamovl := f
    end;

    { check parameter lists converge with different modes }
    function conpar(pla, plb: ctp): boolean;
      var f: boolean;
    { find bidirectionally assignment compatible }
    function comp(t1, t2: stp): boolean;
    begin comp := false;
      if comptypes(t1, t2) then comp := true
      else if (intt(t1) and realt(t2)) or (realt(t1) and intt(t2)) or
              (chart(t1) and chart(t2)) then comp := true
    end;
    begin f := false;
      while (pla <> nil) and (plb <> nil) do begin
        if comp(pla^.idtype,plb^.idtype) then
          if pla^.part <> plb^.part then begin f := true; pla := nil end
          else begin pla := pla^.next; plb := plb^.next end
        else pla := nil
      end;
      conpar := f
    end;

    { check overload proc/funcs against each other, first list is group }
    procedure chkovlpar(lcp1, lcp2: ctp);
      var e: boolean;
    begin
      e := false;
      while lcp1 <> nil do begin
        if (lcp1 <> lcp2) and (lcp1^.klass = lcp2^.klass) then begin
          if compparamovl(lcp2^.pflist, lcp1^.pflist) then begin
            if not e then if fsy = operatorsy then error(283)
                          else error(249);
            e := true
          end;
          if conpar(lcp2^.pflist, lcp1^.pflist) then begin
            if not e then if fsy = operatorsy then error(284)
                          else error(276);
            e := true
          end;
        end;
        lcp1 := lcp1^.grpnxt
      end
    end;

    { find space occupied by parameter list }
    function parmspc(plst: ctp): addrrange;
    var locpar: addrrange;
    begin
      locpar := 0;
      while plst <> nil do begin
        if (plst^.idtype <> nil) and (plst^.klass = vars) then begin
          if (plst^.part = ptval) or (plst^.part = ptview) then begin
            if plst^.idtype^.form <= power then 
              locpar := locpar+plst^.idtype^.size
            else if plst^.idtype^.form = arrayc then
              locpar := locpar+ptrsize*2
            else locpar := locpar+ptrsize
          end else begin
            if plst^.idtype^.form = arrayc then locpar := locpar+ptrsize*2
            else locpar := locpar+ptrsize
          end
        end else if (plst^.klass = proc) or (plst^.klass = func) then 
          locpar := locpar+ptrsize*2;
        alignu(parmptr,locpar);   
        plst := plst^.next
      end;
      parmspc := locpar
    end;

    { offset addresses in parameter list }
    procedure parmoff(plst: ctp; off: addrrange);
    begin
      while plst <> nil do begin
        if plst^.klass = vars then begin
          if not plst^.isloc then plst^.vaddr := plst^.vaddr+off
        end else if (plst^.klass = proc) or (plst^.klass = func) then 
          plst^.pfaddr := plst^.pfaddr+off;
        plst := plst^.next
      end
    end;

    begin (*procdeclaration*)
      { parse and skip any attribute }
      fpat := fpanone;
      opt := bcmop; { avoid undefined error }
      if fsy in [overloadsy,staticsy,virtualsy,overridesy] then begin
        chkstd;
        case fsy of { attribute }
          overloadsy: fpat := fpaoverload;
          staticsy: fpat := fpastatic;
          virtualsy: begin fpat := fpavirtual; if top > 1 then error(228) end;
          overridesy: begin fpat := fpaoverride; if top > 1 then error(229) end;
        end;
        if (sy <> procsy) and (sy <> funcsy) and (sy <> operatorsy) then
          if iso7185 then error(209) else error(279)
        else fsy := sy; insymbol
      end;
      { set parameter address start to zero, offset later }
      llc := lc; lc := 0; forw := false; extl := false; virt := false;
      ovrl := false;
      if (sy = ident) or (fsy = operatorsy) then
        begin
          if fsy = operatorsy then begin { process operator definition }
            if not (sy in [mulop,addop,relop,notsy,becomes]) then
              begin error(281); lcp := nil;
                skip(fsys+[mulop,addop,relop,notsy,arrow,lparent,semicolon])
              end
            else begin
              if sy = notsy then op := notop
              else if sy = becomes then op := bcmop;
              lcp := display[top].oprprc[op] { pick up an operator leader }
            end;
            if fpat <> fpanone then error(280);
            opt := op { save operator for later }
          end else
            searchsection(display[top].fname,lcp); { find previous definition }
          if lcp <> nil then
            begin
              { set flags according to attribute }
              if lcp^.klass = proc then begin
                forw := lcp^.forwdecl and (fsy=procsy) and (lcp^.pfkind=actual);
                extl := lcp^.extern and (fsy=procsy) and (lcp^.pfkind=actual);
                virt := (lcp^.pfattr = fpavirtual) and (fpat = fpaoverride) and
                        (fsy=procsy)and(lcp^.pfkind=actual);
                ovrl := ((fsy=procsy)or(fsy=funcsy)) and (lcp^.pfkind=actual) and
                        (fpat = fpaoverload)
              end else if lcp^.klass = func then begin
                  forw:=lcp^.forwdecl and (fsy=funcsy) and (lcp^.pfkind=actual);
                  extl:=lcp^.extern and (fsy=funcsy) and (lcp^.pfkind=actual);
                  virt := (lcp^.pfattr = fpavirtual) and (fpat = fpaoverride) and
                          (fsy=funcsy)and(lcp^.pfkind=actual);
                  ovrl := ((fsy=procsy)or(fsy=funcsy)) and (lcp^.pfkind=actual) and
                          (fpat = fpaoverload)
              end else forw := false;
              if not forw and not virt and not ovrl and
                 (fsy <> operatorsy) then error(160);
              if virt and not chkext(lcp) then error (230);
              if ovrl and (lcp^.pfattr = fpavirtual) then error(232);
            end
          else if fpat = fpaoverride then error(231);
          lcp1 := lcp; { save original }
          if not forw then { create a new proc/func entry }
            begin
              if (fsy = procsy) or ((fsy = operatorsy) and (opt = bcmop)) then
                new(lcp,proc,declared,actual)
              else { func/opr } new(lcp,func,declared,actual);
              ininam(lcp);
              with lcp^ do
                begin
                  if (fsy = procsy) or
                     ((fsy = operatorsy) and (opt = bcmop)) then
                    klass := proc else klass := func;
                  if fsy = operatorsy then begin
                    { Synth a label based on the operator. This is done for
                      downstream diagnostics. }
                    case op of { operator }
                      mul:   ops := '*        '; rdiv:  ops := '/        ';
                      andop: ops := 'and      '; idiv:  ops := 'div      ';
                      imod:  ops := 'div      '; plus:  ops := '+        ';
                      minus: ops := '-        '; orop:  ops := 'or       ';
                      ltop:  ops := '<        '; leop:  ops := '<=       ';
                      geop:  ops := '>        '; gtop:  ops := '>=       ';
                      neop:  ops := '<>       '; eqop:  ops := '=        ';
                      inop:  ops := 'in       '; xorop: ops := 'xor      ';
                      notop: ops := 'not      '; bcmop: ops := ':=       ';
                    end;
                    strassvr(name, ops)
                  end else strassvf(name, id);
                  idtype := nil; next := nil;
                  sysrot := false; extern := false; pflev := level; 
                  genlabel(lbname); pfdeckind := declared; pfkind := actual; 
                  pfname := lbname; pflist := nil; asgn := false;
                  pext := incact; pmod := incstk; refer := false;
                  pfattr := fpat; grpnxt := nil; grppar := lcp;
                  if pfattr in [fpavirtual, fpaoverride] then begin { alloc vector }
                    if pfattr = fpavirtual then begin
                      { have to create a label for far references to virtual }
                      new(lcp2,vars); ininam(lcp2);
                      with lcp2^ do begin klass := vars;
                        strassvf(name, id); strcatvr(name, '__virtvec');
                        idtype := nilptr; vkind := actual; next := nil;
                        vlev := 0; vaddr := gc; isloc := false; threat := false;
                        forcnt := 0; part := ptval; hdr := false; 
                        vext := incact; vmod := incstk; inilab := -1; 
                        ininxt := nil; dblptr := false;
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
              else if fsy = operatorsy then begin
                if display[top].oprprc[op] = nil then display[top].oprprc[op] := lcp
                else begin { already an operator this level, insert into group }
                  lcp^.grpnxt := lcp1^.grpnxt; lcp1^.grpnxt := lcp;
                  lcp^.grppar := lcp1
                end
              end else enterid(lcp)
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
          if (fsy = procsy) or ((fsy = operatorsy) and (opt = bcmop)) then
            lcp := uprcptr else lcp := ufctptr
        end;
      oldlev := level; oldtop := top;
      { procedure/functions have an odd defining status. The parameter list does
        not have defining points, but the rest of the routine definition does. }
      pushlvl(forw, lcp); display[top].define := false;
      if (fsy = procsy) or ((fsy = operatorsy) and (opt = bcmop)) then
        parameterlist([semicolon],lcp1,plst, fsy = operatorsy, opt)
      else parameterlist([semicolon,colon],lcp1,plst, fsy = operatorsy, opt);
      if not forw then begin
        lcp^.pflist := lcp1; lcp^.locpar := parmspc(lcp^.pflist); 
        parmoff(lcp^.pflist, marksize+ptrsize+adrsize+lcp^.locpar);
        if ovrl or (fsy = operatorsy) then begin { compare against overload group }
          lcp2 := lcp^.grppar; { index top of overload group }
          chkovlpar(lcp2, lcp)
        end;
        lcp^.locstr := lc { save locals counter }
      end else begin
        if plst then if not cmpparlst(lcp^.pflist, lcp1) then error(216);
        putparlst(lcp1); { redeclare, dispose of copy }
        lc := lcp^.locstr { reset locals counter }
      end;
      if (fsy = funcsy) or ((fsy = operatorsy) and not (opt = bcmop)) then
        { function }
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
                  if iso7185 then begin
                    if not (lsp^.form in [scalar,subrange,pointer]) then
                      begin error(120); lcp^.idtype := nil end
                  end else begin
                    if not (lsp^.form in [scalar,subrange,pointer,power,
                                          arrays,records]) then
                      begin error(274); lcp^.idtype := nil end
                  end;
                insymbol
              end
            else begin error(2); skip(fsys + [semicolon]) end
          end
        else
          if not forw or plst then error(123);
      if sy = semicolon then insymbol else error(14);
      if ((sy = ident) and strequri('forward  ', id)) or (sy = forwardsy) or 
         (sy = externalsy) then
        begin
          if forw then { previously forwarded }
            if (sy = externalsy) then error(294) else error(161);
          if extl and not ovrl then error(295);
          if sy = externalsy then begin chkstd; lcp^.extern  := true end
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
            writev(prr, lcp^.name, lenpv(lcp^.name)); 
            write(prr, '@'); { this keeps the user from aliasing it }
            if lcp^.klass = proc then write(prr, 'p') else write(prr, 'f');
            if lcp^.pflist <> nil then begin
              write(prr, '_');
              prtpartyp(lcp)
            end;
            writeln(prr);
          end;
          { output parameter symbols }
          lcp1 := lcp^.pflist;
          while lcp1 <> nil do begin wrtsym(lcp1, 'p'); lcp1 := lcp1^.next end;
          { now we change to a block with defining points }
          display[top].define := true;
          declare(fsys);
          lcp^.locspc := lcp^.locstr-lc;
          lcs := lcp^.locspc;
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
              if not lcp^.asgn and not incact then 
                error(193) { no function result assign }
        end;
      level := oldlev; putdsps(oldtop); top := oldtop; lc := llc;
    end (*procdeclaration*) ;

  begin (*declare*)
    dp := true;
    repeat
      repeat
        if sy = privatesy then begin insymbol;
          if level > 1 then error(266);
          if incact and (level <= 1) then
            incstk^.priv := true { flag private encountered }
        end;
        if not inpriv then begin { if private, get us out quickly }
          if sy = labelsy then
            begin insymbol; labeldeclaration end;
          if sy = constsy then
            begin insymbol; constdeclaration end;
          if sy = typesy then
            begin insymbol; typedeclaration end;
          if sy = fixedsy then
            begin insymbol; fixeddeclaration end;
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
        segsize, gblsize, stackbot: integer;
        lcmin: stkoff;
        llc1: stkoff; lcp: ctp;
        llp: lbp;
        fp: extfilep;
        test: boolean;
        printed: boolean;
        lsize: addrrange;
        stalvl: integer; { statement nesting level }
        ilp: ctp;

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

    procedure genfjp(faddr: integer);
    begin load;
      if gattr.typtr <> nil then
        if gattr.typtr <> boolptr then error(144);
      if prcode then
        begin write(prr,' ':8,mn[33]:4,' '); prtlabel(faddr); writeln(prr) end;
      ic := ic + 1; mes(33)
    end (*genfjp*) ;

    procedure statement(fsys: setofsys);
      var lcp: ctp; llp: lbp; inherit: boolean;

      procedure assignment(fcp: ctp; skp: boolean);
        var lattr, lattr2: attr; tagasc, schrcst: boolean; fcp2: ctp; 
            len: addrrange;
      begin tagasc := false; selector(fsys + [becomes],fcp,skp);
        if (sy = becomes) or skp then
          begin
            if gattr.kind = expr then error(287);
            { if function result, set assigned }
            if fcp^.klass = func then fcp^.asgn := true
            else if fcp^.klass = vars then with fcp^ do begin
               if vlev < level then threat := true;
               if forcnt > 0 then error(195);
               if part = ptview then error(290)
            end;
            tagasc := false;
            if gattr.kind = varbl then 
              tagasc := gattr.tagfield and (debug or chkvbk);
            lattr2 := gattr; { save access before load }
            if gattr.typtr <> nil then
              if (gattr.access<>drct) or (gattr.typtr^.form>power) or
                 tagasc then { if tag checking, force address load }
                if gattr.kind <> expr then loadaddress;
            lattr := gattr;
            insymbol; expression(fsys, false); schrcst := ischrcst(gattr);
            if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
              { process expression rights as load }
              if (gattr.typtr^.form <= power) or (gattr.kind = expr) then begin
                if (lattr.typtr^.form = arrayc) and schrcst then begin
                  { load as string }
                  gen2(51(*ldc*),1,1);
                  gensca(chr(gattr.cval.ival))
                end else load
              end else loadaddress;
            if (lattr.typtr <> nil) and (gattr.typtr <> nil) then begin
              fndopr2(bcmop, lattr, fcp2);
              if fcp2 <> nil then callop2(fcp2, lattr) else begin
                if comptypes(realptr,lattr.typtr)and(gattr.typtr=intptr)then
                  begin gen0(10(*flt*));
                    gattr.typtr := realptr
                  end;
                if comptypes(lattr.typtr,gattr.typtr) or 
                   ((lattr.typtr^.form = arrayc) and schrcst) then begin
                  if filecomponent(gattr.typtr) then error(191);
                  with lattr2 do
                    if kind = varbl then begin
                      if access = indrct then
                        if debug and tagfield and ptrref then
                          { check tag assignment to pointer record }
                          genctaivtcvb(81(*cta*),idplmt,taglvl,vartl,
                                       lattr2.typtr);
                      if chkvbk and tagfield then
                        genctaivtcvb(95(*cvb*),vartagoff,varssize,vartl,
                                     lattr2.typtr);
                      if debug and tagfield then
                        genctaivtcvb(82(*ivt*),vartagoff,varssize,vartl,
                                     lattr2.typtr)
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
                    arrays, arrayc: begin
                      containerop(lattr); { rationalize binary container op }
                      if (lattr.typtr^.form = arrayc) or
                         (gattr.typtr^.form = arrayc) then begin
                        { assign complex pointer }
                        if (containers(lattr.typtr) = 1) or 
                          (containers(gattr.typtr) = 1) then
                          gen1(101(*aps*),containerbase(gattr.typtr))
                        else gen2(102(*apc*),containers(lattr.typtr),
                                  containerbase(gattr.typtr));
                        if gattr.kind = expr then begin 
                          len := gattr.typtr^.size; alignu(parmptr,len); 
                          gen1(71(*dmp*),len+ptrsize*2)
                        end
                      end else begin { standard array assign }
                        { onstack from expr }
                        if gattr.kind = expr then store(lattr)
                        { addressed }
                        else gen1(40(*mov*),lattr.typtr^.size)
                      end
                    end;
                    records: begin
                      { onstack from expr }
                      if gattr.kind = expr then store(lattr)
                      { addressed }
                      else gen1(40(*mov*),lattr.typtr^.size);
                    end;
                    files: error(146)
                  end;
                end else error(129)
              end
            end
          end (*sy = becomes*)
        else error(51)
      end (*assignment*) ;

      procedure gotostatement;
        var llp: lbp; ttop,ttop1: disprange;
            wp: wtp;
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
                { remove any with statement levels to target }
                wp := wthstk;
                while wp <> nil do begin
                  if wp^.sl <> slevel then gen0(120(*wbe*));
                  wp := wp^.next
                end;
                if ttop = ttop1 then
                  genujpxjpcal(57(*ujp*),labname)
                else begin { interprocedural goto }
                  genipj(66(*ipj*),level-(level-vlevel),labname);
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
          begin genlabel(lcix2); genujpxjpcal(57(*ujp*),lcix2);
            putlabel(lcix1);
            markline;
            insymbol;
            addlvl;
            statement(fsys);
            sublvl;
            putlabel(lcix2);
            markline
          end
        else begin putlabel(lcix1); markline end
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
        genujpxjpcal(57(*ujp*),lcix);
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
              markline;
              repeat
                addlvl;
                statement(fsys + [semicolon]);
                sublvl
              until not (sy in statbegsys);
              if lpt3 <> nil then genujpxjpcal(57(*ujp*),laddr);
            end;
          test := sy <> semicolon;
          if not test then insymbol
        until test;
        if sy = elsesy then begin chkstd; insymbol; genlabel(lelse);
          genlabel(lelse2); putlabel(lelse2);
          mesl(-intsize); { put selector on stack }
          gen1(71(*dmp*),intsize);
          putlabel(lelse);
          markline;
          addlvl;
          statement(fsys + [semicolon]);
          sublvl;
          genujpxjpcal(57(*ujp*),laddr);
          if sy = semicolon then insymbol
        end;
        putlabel(lcix);
        markline;
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
                    genujpxjpcal(73(*tjp*),lelse2);
                    gen0t(76(*dup*),intptr);
                    gen2(51(*ldc*),1,lmax);
                    gen2(49(*grt*),ord('i'),0);
                    genujpxjpcal(73(*tjp*),lelse2);
                  end else gen2t(45(*chk*),lmin,lmax,intptr);
                  gen2(51(*ldc*),1,lmin); gen0(21(*sbi*)); genlabel(lcix);
                  genujpxjpcal(44(*xjp*),lcix); putlabel(lcix);
                  repeat
                    with fstptr^ do
                      begin
                        while cslabs > lmin do begin
                           if lelse > 0 then genujpxjpcal(57(*ujp*),lelse)
                           else gen0(60(*ujc error*));
                           lmin := lmin+1
                        end;
                        for i := cslabs to cslabe do
                          genujpxjpcal(57(*ujp*),csstart);
                        lpt1 := fstptr; fstptr := next; lmin := cslabe+1
                      end;
                      putcas(lpt1)
                  until fstptr = nil;
                end else begin
                  { devolve to comp/jmp seq }
                  repeat
                    with fstptr^ do begin
                      gencjp(87(*cjp*),cslabs,cslabe,csstart);
                      lpt1 := fstptr; fstptr := next; lmin := cslabe+1
                    end;
                    putcas(lpt1)
                  until fstptr = nil;
                  if lelse > 0 then genujpxjpcal(57(*ujp*),lelse2);
                  mesl(+intsize) { remove selector from stack }
                end;
                putlabel(laddr);
                markline
              end
            else begin
              error(157);
              repeat
                with fstptr^ do
                  begin lpt1 := fstptr; fstptr := next end;
                putcas(lpt1);
              until fstptr = nil
            end
          end;
        if sy = endsy then insymbol else error(13)
      end (*casestatement*) ;

      procedure repeatstatement;
        var laddr: integer;
      begin genlabel(laddr); putlabel(laddr); markline;
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
      begin genlabel(laddr); putlabel(laddr); markline;
        expression(fsys + [dosy], false); genlabel(lcix); genfjp(lcix);
        if sy = dosy then insymbol else error(54);
        addlvl;
        statement(fsys);
        sublvl;
        genujpxjpcal(57(*ujp*),laddr); putlabel(lcix); markline
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
                if part = ptview then error(290);
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
                    gen2t(56(*str*),level,lc-intsize,intptr);
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
                    gen2t(56(*str*),level,lc-intsize*2,intptr);
                    { set initial value of index }
                    gen2t(54(*lod*),level,lc-intsize,intptr);
                    if debug and (lattr.typtr <> nil) then
                      checkbnds(lattr.typtr);
                    store(lattr);
                    genlabel(laddr); putlabel(laddr); markline;
                    gattr := lattr; load;
                    if not comptypes(gattr.typtr,intptr) then
                      gen0t(58(*ord*),gattr.typtr);
                    gen2t(54(*lod*),level,lc-intsize*2,intptr);
                    lcs := lc;
                    lc := lc - intsize*2;
                    if lc < lcmin then lcmin := lc;
                    if lsy = tosy then gen2(52(*leq*),ord(typind),1)
                    else gen2(48(*geq*),ord(typind),1);
                  end
                else error(145)
          end
        else begin error(55); skip(fsys + [dosy]) end;
        genlabel(lcix); genujpxjpcal(33(*fjp*),lcix);
        if sy = dosy then insymbol else error(54);
        addlvl;
        statement(fsys);
        sublvl;
        gattr := lattr; load;
        if not comptypes(gattr.typtr,intptr) then
          gen0t(58(*ord*),gattr.typtr);
        gen2t(54(*lod*),level,lcs-intsize*2,intptr);
        gen2(47(*equ*),ord(typind),1);
        genujpxjpcal(73(*tjp*),lcix);
        gattr := lattr; load;
        if lsy=tosy then gen1t(34(*inc*),1,gattr.typtr)
        else  gen1t(31(*dec*),1,gattr.typtr);
        if debug and (lattr.typtr <> nil) then
          checkbnds(lattr.typtr);
        store(lattr);
        genujpxjpcal(57(*ujp*),laddr); putlabel(lcix); markline;
        gattr := lattr; loadaddress; gen0(79(*inv*));
        lc := llc;
        if lcp <> nil then lcp^.forcnt := lcp^.forcnt-1
      end (*forstatement*) ;

      procedure withstatement;
        var lcp: ctp; lcnt1: disprange; llc: addrrange;
            test: boolean;
            wbscnt: integer;
      begin lcnt1 := 0; llc := lc; wbscnt := 0;
        repeat
          if sy = ident then
            begin searchid([vars,field],lcp); insymbol end
          else begin error(2); lcp := uvarptr end;
          selector(fsys + [comma,dosy],lcp,false);
          if gattr.kind = expr then error(287);
          if gattr.typtr <> nil then
            if gattr.typtr^.form = records then
              if top < displimit then
                begin top := top + 1; lcnt1 := lcnt1 + 1;
                  with display[top] do
                    begin inidsp(display[top]); fname := gattr.typtr^.fstfld;
                      packing := gattr.packing;
                      packcom := gattr.packcom;
                      ptrref := gattr.ptrref
                    end;
                  if gattr.access = drct then
                    with display[top] do
                      begin occur := crec; clev := gattr.vlevel;
                        cdspl := gattr.dplmt
                      end
                  else
                    begin loadaddress;
                      if debug and gattr.ptrref then
                        begin gen0(119(*wbs*)); wbscnt := wbscnt+1; pshwth(stalvl) end;
                      alignd(nilptr,lc);
                      lc := lc-ptrsize;
                      gen2t(56(*str*),level,lc,nilptr);
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
          while wbscnt > 0 do begin gen0(120(*wbe*)); wbscnt := wbscnt-1; popwth end;
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
        genujpxjpcal(84(*bge*),bgnexplbl);
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
        genujpxjpcal(57(*ujp*),noexplbl);
        putlabel(bgnexplbl); markline;
        if (sy <> onsy) and (sy <> exceptsy) then error(24);
        while sy = onsy do begin insymbol; genlabel(onstalbl);
          genlabel(onendlbl);
          repeat
            if sy = ident then begin
              searchid([vars],lcp);
              with lcp^, lattr do
                begin typtr := idtype; kind := varbl; packing := false;
                  if threat or (forcnt > 0) then error(195); forcnt := forcnt+1;
                  if part = ptview then error(290);
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
              genujpxjpcal(73(*tjp*),onstalbl);
            end else begin error(2); skip(fsys+[onsy,exceptsy,elsesy]) end;
            test := sy <> comma;
            if not test then insymbol
          until test;
          genujpxjpcal(57(*ujp*),onendlbl);
          if sy = exceptsy then insymbol else
            begin error(23); skip(fsys+[onsy,exceptsy,elsesy]) end;
          putlabel(onstalbl);
          markline;
          addlvl;
          statement(fsys+[exceptsy]);
          sublvl;
          genujpxjpcal(57(*ujp*),endlbl);
          putlabel(onendlbl);
          markline
        end;
        if sy = exceptsy then begin addlvl;
          insymbol; statement(fsys+[elsesy]); sublvl;
          genujpxjpcal(57(*ujp*),endlbl)
        end;
        gen0(86(*mse*));
        putlabel(noexplbl);
        markline;
        if sy = elsesy then begin addlvl;
          insymbol; statement(fsys); sublvl
        end;
        sublvl;
        putlabel(endlbl);
        markline;
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
             putlabel(labname); { output label to intermediate }
             markline
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
                        if hasovlproc(lcp) then begin
                          if hasovlfunc(lcp) then begin
                            { could be proc or func, need disambiguate }
                            if sy = becomes then begin
                              if inherit then error(233);
                              assignment(ovlfunc(lcp), false)
                            end else call(fsys,lcp,inherit,false)
                          end else call(fsys,lcp,inherit,false)
                        end else begin if inherit then error(233);
                          if hasovlfunc(lcp) then assignment(ovlfunc(lcp), false)
                          else assignment(lcp, false)
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
              if fprocp <> nil then
                if fprocp^.klass <> func then error(210)
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
              if lcp1 <> nil then begin
                lcp2 := lcp1^.pfvid; { get vector symbol }
                if lcp2 <> nil then begin
                  { copy old vector to store }
                  gen1ts(39(*ldo*),lcp2^.vaddr,lcp2^.idtype,lcp2);
                  gen1t(43(*sro*),lcp^.pfvaddr,nilptr);
                  { place new vector }
                  gensuv(lcp^.pfname,lcp2^.pfvaddr,lcp2);
                end
              end
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
    markline;
    genlabel(segsize); genlabel(stackbot);
    genlabel(gblsize);
    genmst(level-1,segsize,stackbot);
    if fprocp <> nil then (*copy multiple values into local cells*)
      begin llc1 := marksize+ptrsize+adrsize+fprocp^.locpar; { index params }
        lcp := fprocp^.pflist;
        while lcp <> nil do
          with lcp^ do
            begin
              if klass = vars then
                if idtype <> nil then begin
                  if idtype^.form > power then
                    begin
                      if idtype^.form = arrayc then llc1 := llc1 - ptrsize*2
                      else llc1 := llc1-ptrsize;
                      alignd(parmptr,llc1);
                      if vkind = actual then
                        if idtype^.form = arrayc then begin
                          { Container array. These are not preallocated, so we
                            have to create a copy on stack. }
                          gen2(50(*lda*),level,llc1); { index the pointer }
                          gen0(111(*ldp*)); { load complex pointer }
                          { copy complex to stack }
                          gen2(109(*ccs*),containers(idtype),containerbase(idtype));
                          gen2(50(*lda*),level,vaddr); { load dest addr }
                          gen1(72(*swp*),stackelsize*2); { swap that under cp }
                          gen0(110(*scp*)) { store complex pointer }
                        end else begin
                          gen2(50(*lda*),level,vaddr);
                          gen2t(54(*lod*),level,llc1,nilptr);
                          gen1(40(*mov*),idtype^.size);
                        end
                    end
                  else
                    begin
                      if vkind = formal then llc1 := llc1-ptrsize
                      else llc1 := llc1-idtype^.size;
                      alignd(parmptr,llc1);
                    end;
                  if chkvbk and (vkind = formal) then begin
                    { establish var block }
                    gen2t(54(*lod*),level,llc1,nilptr);
                    gen1(93(*vbs*),idtype^.size)
                  end
                end;
              lcp := lcp^.next;
            end;
      end;
    lcmin := lc;
    addlvl;
    if (level = 1) and not incact then begin { perform module setup tasks }
      externalheader; { process external header files }
      initvirt { process virtual procedure/function sets }
    end;
    { call initializer code strips }
    ilp := display[top].inilst;
    while ilp <> nil do
      begin genujpxjpcal(89(*cal*),ilp^.inilab); ilp := ilp^.ininxt end;
    if sy = beginsy then insymbol else error(17);
    repeat
      repeat statement(fsys + [semicolon,endsy])
      until not (sy in statbegsys);
      test := sy <> semicolon;
      if not test then insymbol
    until test;
    { deinitialize containers }
    if level = 1 then begin
      ilp := display[top].inilst;
      while ilp <> nil do begin
        gen1t(39(*ldo*),ilp^.vaddr,nilptr);
        gen0(107(*vdp*));
        ilp := ilp^.ininxt end
    end;
    sublvl;
    if sy = endsy then insymbol else error(13);
    llp := display[top].flabel; (*test for undefined and unreferenced labels*)
    while llp <> nil do
      with llp^ do
        begin
          if not defined or not refer then
            begin if not defined then error(168);
              writeln(output); write('label ',labval:11);
              if not refer and not incact then write(' unreferenced');
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
        { output var block ends for each var parameter }
        lcp := fprocp^.pflist;
        while lcp <> nil do
          with lcp^ do begin
            if klass = vars then
              if chkvbk and (vkind = formal) then gen0(94(*vbe*));
            lcp := next
          end;
        if fprocp^.idtype = nil then gen2(42(*ret*),ord('p'),fprocp^.locpar)
        else if fprocp^.idtype^.form in [records, arrays] then
          gen2t(42(*ret*),fprocp^.locpar,fprocp^.idtype^.size,basetype(fprocp^.idtype))
        else gen1t(42(*ret*),fprocp^.locpar,basetype(fprocp^.idtype));
        alignd(parmptr,lcmin);
        if prcode then
          begin prtlabel(segsize); writeln(prr,'=',lcmin:1);
            prtlabel(stackbot); writeln(prr,'=',topmin:1)
          end
      end
    else
      begin gen2(42(*ret*),ord('p'),0);
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
  { for any error, back out the include level }
  procedure err;
  begin
    incstk := incstk^.next;
    dispose(fp);
    ff := false
  end;
  begin ff := true; es := extsrc;
    { have not previously parsed this module }
    new(fp);
    with fp^ do begin
      next := incstk; incstk := fp; strassvf(mn, id); priv := false; 
      linecounts := linecount; lineouts := lineout;
      fn := id; i := fillen; while (i > 1) and (fn[i] = ' ') do i := i-1;
      if i > fillen-4-1 then begin err; error(265) end
      else begin
        for x := 1 to 4 do begin i := i+1; fn[i] := es[x] end;
        if not existsfile(fn) then begin err; error(264) end
        else begin assigntext(f, fn); reset(f) end
      end
    end
  end;

  procedure closeinput;
  var fp: filptr;
  begin
    if not incact then error(505);
    closetext(incstk^.f);
    linecount := incstk^.linecounts; lineout := incstk^.lineouts;
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
    thismod := nil;
    repeat { modules }
      if sy <> ident then error(2) else begin
        if not schnam(incstk) and not schnam(inclst) then begin
          chs := ch; eols := eol; prcodes := prcode; lists := list; gcs := gc;
          nammods := nammod; curmods := curmod; entnames := entname;
          openinput(ff);
          if ff then begin
            ch := ' '; eol := true; prcode := false; list := false;
            readline; insymbol; modnams := display[top].modnam;
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
                    define := true; occur := blck; bname := nil end
          end
        end else putstrs(thismod)
      end;
      sys := sy;
      if sy = comma then insymbol
    until sys <> comma;
    if sy = semicolon then insymbol else error(14)
  end;

  function searchext: boolean;
  var fp: extfilep; f: boolean;
  begin f := false; fp := fextfilep;
    while fp <> nil do
      begin if id = fp^.filename then f := true; fp := fp^.nextfile end;
    searchext := f
  end;

  procedure modulep{(fsys:setofsys)};
    var extfp,newfl:extfilep; segsize, stackbot: integer;
        nulllab: integer;
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
            writeln(prr, '!');
            if curmod = mtprogram then
              begin write(prr, '! Program '); writevp(prr, nammod); writeln(prr) end
            else
              begin write(prr, '! Module '); writevp(prr, nammod); writeln(prr) end;
            writeln(prr, '!');
            if curmod = mtmodule then
              writeln(prr, 'b', ' ':7, 'm', ' ':7, id:kk) { mark module block start }
            else
              writeln(prr, 'b', ' ':7, 'p', ' ':7, id:kk) { mark program block start }
          end;
          insymbol;
          { mark stack, generate call to startup block }
          genlabel(nulllab); gensfr(nulllab);
          if prcode then begin prtlabel(nulllab); writeln(prr,'=0') end;
          gencupcuf(46(*cup*),0,entname,nil);
          if curmod = mtmodule then begin
            { for module we need call next in module stack, then call exit
              module }
            genujpxjpcal(89(*cal*),nxtname);
            gensfr(nulllab); gencupcuf(46(*cup*),0,extname,nil)
          end;
          gen0(90(*ret*)) { return last module stack }
        end;
        if not (sy in [lparent,semicolon]) then error(14);
        if sy = lparent  then
          begin
            newfl := nil;
            repeat insymbol;
              if sy = ident then
                begin 
                  if not incact then begin
                    getfil(extfp); if searchext then error(240);
                    with extfp^ do
                      begin filename := id; nextfile := fextfilep end;
                    fextfilep := extfp
                  end;
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
            { reverse the header list into order }
            if not incact then begin
              newfl := nil;
              while fextfilep <> nil do 
                begin extfp := fextfilep; fextfilep := fextfilep^.nextfile; 
                      extfp^.nextfile := newfl; newfl := extfp end;
              fextfilep := newfl
            end;
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
        genmst(level,segsize,stackbot);
        gen2(42(*ret*),ord('p'),0);
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
        writeln(prr,'g', ' ':7,gc:1);
        writeln(prr, 'e', ' ':7, 'p') { mark program block end }
      end
    end;
    if (sy <> period) and not inpriv then begin error(21); skip([period]) end;
    if prcode then begin
      writeln(prr, 'f', ' ':7, toterr:1);
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
    na[73] := 'exception'; na[74] := 'throw    '; na[75] := 'max      ';
    na[76] := 'string   '; na[77] := 'pstring  '; na[78] := 'byte     ';
    na[79] := 'vector   '; na[80] := 'matrix   '; na[81] := 'abyte    ';
    na[82] := 'schar    ';

  end (*stdnames*) ;

  procedure enterstdtypes;

  begin                                                 (*type underlying:*)
                                                        (******************)

    new(intptr,scalar,standard); pshstc(intptr);               (*integer*)
    with intptr^ do
      begin form := scalar; size := intsize; scalkind := standard;
            packing := false end;
    new(crdptr,subrange); pshstc(crdptr);                      (*cardinal*)
    with crdptr^ do
      begin form := subrange; size := intsize; rangetype := intptr;
            min.intval := true; min.ival := 0;
            max.intval := true; max.ival := pmmaxint; packing := false end;
    new(realptr,scalar,standard); pshstc(realptr);             (*real*)
    with realptr^ do
      begin form := scalar; size := realsize; scalkind := standard;
            packing := false end;
    new(charptr,scalar,standard); pshstc(charptr);             (*char*)
    with charptr^ do
      begin form := scalar; size := charsize; scalkind := standard;
            packing := false end;
    new(boolptr,scalar,declared); pshstc(boolptr);             (*boolean*)
    with boolptr^ do
      begin form := scalar; size := boolsize; scalkind := declared;
            packing := false end;
    new(nilptr,pointer); pshstc(nilptr);                       (*nil*)
    with nilptr^ do
      begin form := pointer; eltype := nil; size := ptrsize;
            packing := false end;
    (*for alignment of parameters*)
    new(parmptr,scalar,standard); pshstc(parmptr);
    with parmptr^ do
      begin form := scalar; size := parmsize; scalkind := standard;
            packing := false end ;
    new(textptr,files); pshstc(textptr);                       (*text*)
    with textptr^ do
      begin form := files; filtype := charptr; size := filesize+charsize;
            packing := false end;
    new(exceptptr,exceptf); pshstc(exceptptr);                 (*exception*)
    with exceptptr^ do
      begin form := exceptf; size := exceptsize; packing := false end;

    { common types }
    new(stringptr,arrayc); pshstc(stringptr);                  (*string*)
    with stringptr^ do
      begin form := arrayc; size := 0; packing := true; abstype := charptr end;
    new(pstringptr,pointer); pshstc(pstringptr);               (*string pointer*)
    with pstringptr^ do
      begin form := pointer; size := ptrsize; packing := false;
            eltype := stringptr end;
    new(byteptr,subrange); pshstc(byteptr);
    with byteptr^ do
      begin form := subrange; size := 1; packing := false; rangetype := intptr;
            min.intval := true; min.ival := 0; max.intval := true;
            max.ival := 255 end;
    new(abyteptr,arrayc); pshstc(abyteptr);                    (*byte array*)
    with abyteptr^ do
      begin form := arrayc; size := 0; packing := false; abstype := byteptr end;
    new(vectorptr,arrayc); pshstc(vectorptr);                  (*vector*)
    with vectorptr^ do
      begin form := arrayc; size := 0; packing := false; abstype := intptr end;
    new(matrixptr,arrayc); pshstc(matrixptr);                  (*matrix*)
    with matrixptr^ do
      begin form := arrayc; size := 0; packing := false;
            abstype := vectorptr end;
    new(scharptr,power); pshstc(scharptr);                    (*set of char*)
    with scharptr^ do
      begin form := power; size := setsize; packing := false; elset := charptr;
            matchpack := true end;
  end (*enterstdtypes*) ;

  procedure entstdnames;
    var cp,cp1: ctp; i: integer;

  procedure entstdprocfunc(idc: idclass; sn: stdrng; kn: keyrng; idt: stp);
  begin
    if idc = proc then new(cp,proc,standard)
    else new(cp,func,standard);
    ininam(cp);
    with cp^ do
      begin klass := idc; strassvr(name, na[sn]); idtype := idt;
        pflist := nil; next := nil; key := kn;
        pfdeckind := standard; pfaddr := 0; pext := false;
        pmod := nil; pfattr := fpanone; grpnxt := nil; grppar := cp;
        pfvid := nil; pflist := nil
      end; enterid(cp)
  end;

  procedure entstdtyp(sn: stdrng; idt: stp);
  begin
    new(cp,types); ininam(cp);
    with cp^ do
      begin klass := types; strassvr(name, na[sn]); idtype := idt end;
    enterid(cp)
  end;

  procedure entstdintcst(sn: stdrng; idt: stp; i: integer);
  begin
    new(cp,konst); ininam(cp);
    with cp^ do
      begin klass := konst; strassvr(name, na[sn]); idtype := idt; next := nil;
        values.intval := true; values.ival := i end;
    enterid(cp)
  end;

  procedure entstdrlcst(sn: stdrng; idt: stp; r: real);
  var lvp: csp;
  begin
    new(cp,konst); ininam(cp); new(lvp,reel); pshcst(lvp); lvp^.cclass := reel;
    lvp^.rval := r;
    with cp^ do
      begin klass := konst; strassvr(name, na[sn]); idtype := idt; next := nil;
        values.intval := false; values.valp := lvp end;
    enterid(cp)
  end;

  procedure entstdhdr(sn: stdrng);
  begin
    new(cp,vars); ininam(cp);
    with cp^ do
    begin klass := vars; strassvr(name, na[sn]); idtype := textptr;
      vkind := actual; next := nil; vlev := 1;
      vaddr := gc; gc := gc+filesize+charsize; { files are global now }
      isloc := false; threat := false; forcnt := 0; part := ptval; hdr := false;
      vext := false; vmod := nil; inilab := -1; ininxt := nil; dblptr := false
    end;
    enterid(cp)
  end;

  procedure entstdexp(en: expstr);
  begin
    new(cp,vars); ininam(cp);
    with cp^ do
    begin klass := vars; strassve(name, en); idtype := exceptptr;
      vkind := actual; next := nil; vlev := 1;
      vaddr := gc; gc := gc+exceptsize;
      isloc := false; threat := false; forcnt := 0; part := ptval; hdr := false;
      vext := false; vmod := nil; inilab := -1; ininxt := nil; dblptr := false
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
    entstdtyp(76, stringptr);                                 (*string*)
    entstdtyp(77, pstringptr);                                (*pointer to string*)
    entstdtyp(78, byteptr);                                   (*byte*)
    entstdtyp(79, vectorptr);                                 (*vector*)
    entstdtyp(80, matrixptr);                                 (*matrix*)
    entstdtyp(81, abyteptr);                                  (*array of bytes*)
    entstdtyp(82, scharptr);                                  (*set of char*)

    cp1 := nil;
    for i := 1 to 2 do
      begin new(cp,konst); ininam(cp);                        (*false,true*)
        with cp^ do
          begin klass := konst; strassvr(name, na[i]); idtype := boolptr;
            next := cp1; values.intval := true; values.ival := i - 1;
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
          begin klass := vars; strassvr(name, '         '); idtype := realptr;
            vkind := actual; next := nil; vlev := 1; vaddr := 0;
            isloc := false; threat := false; forcnt := 0; part := ptval; 
            hdr := false; vext := false; vmod := nil; inilab := -1; 
            ininxt := nil; dblptr := false
          end;
        new(cp1,func,declared,actual); ininam(cp1);            (*sin,cos,exp*)
        with cp1^ do                                           (*sqrt,ln,arctan*)
          begin klass := func; strassvr(name, na[i]); idtype := realptr;
            pflist := cp; forwdecl := false; sysrot := true; extern := false; 
            pflev := 0; pfname := i - 12; pfdeckind := declared; 
            pfkind := actual; pfaddr := 0; pext := false; pmod := nil; 
            pfattr := fpanone; grpnxt := nil; grppar := cp1; pfvid := nil
          end;
        enterid(cp1)
      end;

    entstdintcst(36, intptr, pmmaxint);                          (*maxint*)
    entstdintcst(53, intptr, pmmaxint);                          (*maxlint*)
    entstdintcst(55, crdptr, pmmaxint);                          (*maxcrd*)
    entstdintcst(57, crdptr, pmmaxint);                          (*maxlcrd*)
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
    entstdprocfunc(func, 75, 32, intptr);  { max }

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
      begin klass := types; strassvr(name, '         '); idtype := nil end;
    new(ucstptr,konst); ininam(ucstptr);
    with ucstptr^ do
      begin klass := konst; strassvr(name, '         '); idtype := nil;
        next := nil; values.intval := true; values.ival := 0
      end;
    new(uvarptr,vars); ininam(uvarptr);
    with uvarptr^ do
      begin klass := vars; strassvr(name, '         '); idtype := nil;
        vkind := actual; next := nil; vlev := 0; vaddr := 0;
        isloc := false; threat := false; forcnt := 0; part := ptval; 
        hdr := false; vext := false; vmod := nil; inilab := -1; ininxt := nil;
        dblptr := false
      end;
    new(ufldptr,field); ininam(ufldptr);
    with ufldptr^ do
      begin klass := field; strassvr(name, '         '); idtype := nil;
        next := nil; fldaddr := 0; varnt := nil; varlb := nil;
        tagfield := false; taglvl := 0; varsaddr := 0;
        varssize := 0; vartl := -1
      end;
    new(uprcptr,proc,declared,actual); ininam(uprcptr);
    with uprcptr^ do
      begin klass := proc; strassvr(name, '         '); idtype := nil;
        forwdecl := false; next := nil; sysrot := false; extern := false; 
        pflev := 0; genlabel(pfname); pflist := nil; pfdeckind := declared;
        pfkind := actual; pmod := nil; grpnxt := nil; grppar := uprcptr;
        pfvid := nil
      end;
    new(ufctptr,func,declared,actual); ininam(ufctptr);
    with ufctptr^ do
      begin klass := func; strassvr(name, '         '); idtype := nil;
        next := nil; forwdecl := false; sysrot := false; extern := false; 
        pflev := 0; genlabel(pfname); pflist := nil; pfdeckind := declared;
        pfkind := actual; pmod := nil; grpnxt := nil; grppar := ufctptr;
        pfvid := nil
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
  var i: integer; oi: 1..maxopt;
  begin fwptr := nil; for oi := 1 to maxopt do option[oi] := false;
    prtables := false; option[20] := false; list := true; option[12] := true;
    prcode := true; option[3] := true; debug := true; option[4] := true;
    chkvar := true; option[22] := true; chkref := true; option[18] := true;
    chkudtc := true; option[21] := true; option[19] := false; iso7185 := false;
    dodmplex := false; doprtryc := false; doprtlab := false; dodmpdsp := false;
    chkvbk := true; option[9] := true; experr := true; option[10] := true;
    dp := true; errinx := 0;
    intlabel := 0; kk := maxids; fextfilep := nil; wthstk := nil;
    lc := -ptrsize; gc := 0;
    (* note in the above reservation of buffer store for 2 text files *)
    ic := 3; eol := true; linecount := 0; lineout := 0;
    incstk := nil; inclst := nil; cbblst := nil;
    ch := ' '; chcnt := 0;
    mxint10 := maxint div 10;
    maxpow10 := 1; while maxpow10 < mxint10 do maxpow10 := maxpow10*10;

    for i := 1 to maxftl do errtbl[i] := 0; { initialize error tracking }
    toterr := 0; { clear error count }
    { clear the recycling tracking counters }
    strcnt := 0; { strings }
    cspcnt := 0; { constants }
    stpcnt := 0; { structures }
    ctpcnt := 0; { identifiers }
    lbpcnt := 0; { label counts }
    filcnt := 0; { file tracking counts }
    cipcnt := 0; { case entry tracking counts }
    ttpcnt := 0; { tag tracking entry counts }
    wtpcnt := 0; { with tracking entry counts }

    { clear id counts }
    ctpsnm := 0;
    stpsnm := 0
  end (*initscalars*) ;

  procedure initsets;
  begin
    constbegsys := [lparent,notsy,intconst,realconst,stringconst,ident,lbrack];
    simptypebegsys := [lparent,addop,intconst,realconst,stringconst,ident];
    typebegsys:=[arrow,packedsy,arraysy,recordsy,setsy,filesy]+simptypebegsys;
    typedels := [arraysy,recordsy,setsy,filesy];
    pfbegsys := [procsy,funcsy,overloadsy,staticsy,virtualsy,overridesy,
                 operatorsy];
    blockbegsys := [privatesy,labelsy,constsy,typesy,fixedsy,varsy,beginsy]+pfbegsys;
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
    var i: integer;
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

      for i := ordminchar to ordmaxchar do ssy[chr(i)] := othersy;
      ssy['+'] := addop ;   ssy['-'] := addop;    ssy['*'] := mulop;
      ssy['/'] := mulop ;   ssy['('] := lparent;  ssy[')'] := rparent;
      ssy['$'] := othersy ; ssy['='] := relop;    ssy[' '] := othersy;
      ssy[','] := comma ;   ssy['.'] := period;   ssy['''']:= othersy;
      ssy['['] := lbrack ;  ssy[']'] := rbrack;   ssy[':'] := colon;
      ssy['^'] := arrow ;   ssy['<'] := relop;    ssy['>'] := relop;
      ssy[';'] := semicolon; ssy['@'] := arrow;   ssy['#'] := numsy;
      ssy['}'] := othersy;
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
      sna[ 1] :='get '; sna[ 2] :='put '; sna[ 3] :='rdi '; sna[ 4] :='rdr ';
      sna[ 5] :='rdc '; sna[ 6] :='wri '; sna[ 7] :='wro '; sna[ 8] :='wrr ';
      sna[ 9] :='wrc '; sna[10] :='wrs '; sna[11] :='pak '; sna[12] :='new ';
      sna[13] :='rst '; sna[14] :='eln '; sna[15] :='sin '; sna[16] :='cos ';
      sna[17] :='exp '; sna[18] :='sqt '; sna[19] :='log '; sna[20] :='atn ';
      sna[21] :='rln '; sna[22] :='wln '; sna[23] :='sav ';
      { new procedure/function memonics for p5/p6 }
      sna[24] :='pag '; sna[25] :='rsf '; sna[26] :='rwf '; sna[27] :='wrb ';
      sna[28] :='wrf '; sna[29] :='dsp '; sna[30] :='wbf '; sna[31] :='wbi ';
      sna[32] :='wbr '; sna[33] :='wbc '; sna[34] :='wbb '; sna[35] :='rbf ';
      sna[36] :='rsb '; sna[37] :='rwb '; sna[38] :='gbf '; sna[39] :='pbf ';
      sna[40] :='rib '; sna[41] :='rcb '; sna[42] :='nwl '; sna[43] :='dsl ';
      sna[44] :='eof '; sna[45] :='efb '; sna[46] :='fbv '; sna[47] :='fvb ';
      sna[48] :='wbx '; sna[49] :='asst'; sna[50] :='clst'; sna[51] :='pos ';
      sna[52] :='upd '; sna[53] :='appt'; sna[54] :='del '; sna[55] :='chg ';
      sna[56] :='len '; sna[57] :='loc '; sna[58] :='exs '; sna[59] :='assb';
      sna[60] :='clsb'; sna[61] :='appb'; sna[62] :='hlt '; sna[63] :='ast ';
      sna[64] :='asts'; sna[65] :='wrih'; sna[66] :='wrio'; sna[67] :='wrib';
      sna[68] :='wrsp'; sna[69] :='wiz '; sna[70] :='wizh'; sna[71] :='wizo';
      sna[72] :='wizb'; sna[73] :='rds '; sna[74] :='ribf'; sna[75] :='rdif';
      sna[76] :='rdrf'; sna[77] :='rcbf'; sna[78] :='rdcf'; sna[79] :='rdsf';
      sna[80] :='rdsp'; sna[81] :='aeft'; sna[82] :='aefb'; sna[83] :='rdie';
      sna[84] :='rdre'; sna[85] :=' thw';

    end (*procmnemonics*) ;

    procedure instrmnemonics;
    begin { --- are unused codes }
      mn[  0] :='abi'; mn[  1] :='abr'; mn[  2] :='adi'; mn[  3] :='adr';
      mn[  4] :='and'; mn[  5] :='dif'; mn[  6] :='dvi'; mn[  7] :='dvr';
      mn[  8] :='ltc'; mn[  9] :='flo'; mn[ 10] :='flt'; mn[ 11] :='inn';
      mn[ 12] :='int'; mn[ 13] :='ior'; mn[ 14] :='mod'; mn[ 15] :='mpi';
      mn[ 16] :='mpr'; mn[ 17] :='ngi'; mn[ 18] :='ngr'; mn[ 19] :='not';
      mn[ 20] :='odd'; mn[ 21] :='sbi'; mn[ 22] :='sbr'; mn[ 23] :='sgs';
      mn[ 24] :='sqi'; mn[ 25] :='sqr'; mn[ 26] :='sto'; mn[ 27] :='trc';
      mn[ 28] :='uni'; mn[ 29] :='stp'; mn[ 30] :='csp'; mn[ 31] :='dec';
      mn[ 32] :='rip'; mn[ 33] :='fjp'; mn[ 34] :='inc'; mn[ 35] :='ind';
      mn[ 36] :='ixa'; mn[ 37] :='lao'; mn[ 38] :='lca'; mn[ 39] :='ldo';
      mn[ 40] :='mov'; mn[ 41] :='mst'; mn[ 42] :='ret'; mn[ 43] :='sro';
      mn[ 44] :='xjp'; mn[ 45] :='chk'; mn[ 46] :='cup'; mn[ 47] :='equ';
      mn[ 48] :='geq'; mn[ 49] :='grt'; mn[ 50] :='lda'; mn[ 51] :='ldc';
      mn[ 52] :='leq'; mn[ 53] :='les'; mn[ 54] :='lod'; mn[ 55] :='neq';
      mn[ 56] :='str'; mn[ 57] :='ujp'; mn[ 58] :='ord'; mn[ 59] :='chr';
      mn[ 60] :='ujc'; mn[ 61] :='rnd'; mn[ 62] :='pck'; mn[ 63] :='upk';
      mn[ 64] :='rgs'; mn[ 65] :='???'; mn[ 66] :='ipj'; mn[ 67] :='cip';
      mn[ 68] :='lpa'; mn[ 69] :='???'; mn[ 70] :='???'; mn[ 71] :='dmp';
      mn[ 72] :='swp'; mn[ 73] :='tjp'; mn[ 74] :='lip'; mn[ 75] :='ckv';
      mn[ 76] :='dup'; mn[ 77] :='cke'; mn[ 78] :='cks'; mn[ 79] :='inv';
      mn[ 80] :='ckl'; mn[ 81] :='cta'; mn[ 82] :='ivt'; mn[ 83] :='xor';
      mn[ 84] :='bge'; mn[ 85] :='ede'; mn[ 86] :='mse'; mn[ 87] :='cjp';
      mn[ 88] :='lnp'; mn[ 89] :='cal'; mn[ 90] :='ret'; mn[ 91] :='cuv';
      mn[ 92] :='suv'; mn[ 93] :='vbs'; mn[ 94] :='vbe'; mn[ 95] :='cvb';
      mn[ 96] :='vis'; mn[ 97] :='vip'; mn[ 98] :='lcp'; mn[ 99] :='cps';
      mn[100] :='cpc'; mn[101] :='aps'; mn[102] :='apc'; mn[103] :='cxs';
      mn[104] :='cxc'; mn[105] :='lft'; mn[106] :='max'; mn[107] :='vdp';
      mn[108] :='spc'; mn[109] :='ccs'; mn[110] :='scp'; mn[111] :='ldp';
      mn[112] :='vin'; mn[113] :='vdd'; mn[114] :='lto'; mn[115] :='ctb';
      mn[116] :='cpp'; mn[117] :='cpr'; mn[118] :='lsa'; mn[119] :='wbs';
      mn[120] :='wbe'; mn[121] :='sfr'; mn[122] :='cuf';

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
      cdx[  0] :=  0;                   cdx[  1] :=  0;
      cdx[  2] := +intsize;             cdx[  3] := +realsize;
      cdx[  4] := +intsize;             cdx[  5] := +setsize;
      cdx[  6] := +intsize;             cdx[  7] := +realsize;
      cdx[  8] :=  4{*};                cdx[  9] := +intsize-realsize;
      cdx[ 10] := -realsize+intsize;    cdx[ 11] := +setsize;
      cdx[ 12] := +setsize;             cdx[ 13] := +intsize;
      cdx[ 14] := +intsize;             cdx[ 15] := +intsize;
      cdx[ 16] := +realsize;            cdx[ 17] :=  0;
      cdx[ 18] :=  0;                   cdx[ 19] :=  2{*};
      cdx[ 20] :=  0;                   cdx[ 21] := +intsize;
      cdx[ 22] := +realsize;            cdx[ 23] := +intsize-setsize;
      cdx[ 24] :=  0;                   cdx[ 25] :=  0;
      cdx[ 26] := 1{*};                 cdx[ 27] := +realsize-intsize;
      cdx[ 28] := +setsize;             cdx[ 29] :=  0;
      cdx[ 30] :=  0;                   cdx[ 31] :=  2{*};
      cdx[ 32] :=  0;                   cdx[ 33] := +intsize;
      cdx[ 34] :=  2{*};                cdx[ 35] :=  3{*};
      cdx[ 36] := +intsize;             cdx[ 37] := -adrsize;
      cdx[ 38] := -adrsize;             cdx[ 39] :=  4{*};
      cdx[ 40] := +adrsize*2;           cdx[ 41] :=  0;
      cdx[ 42] :=  2{*};                cdx[ 43] :=  5{*};
      cdx[ 44] := +intsize;             cdx[ 45] :=  2{*};
      cdx[ 46] :=  0;                   cdx[ 47] :=  6{*};
      cdx[ 48] :=  6{*};                cdx[ 49] :=  6{*};
      cdx[ 50] := -adrsize;             cdx[ 51] :=  4{*};
      cdx[ 52] :=  6{*};                cdx[ 53] :=  6{*};
      cdx[ 54] :=  4{*};                cdx[ 55] :=  6{*};
      cdx[ 56] :=  5{*};                cdx[ 57] :=  0;
      cdx[ 58] :=  2{*};                cdx[ 59] :=  0;
      cdx[ 60] :=  0;                   cdx[ 61] :=  +realsize-intsize;
      cdx[ 62] := +adrsize*3;           cdx[ 63] := +adrsize*3;
      cdx[ 64] := +intsize*2-setsize;   cdx[ 65] :=  0;
      cdx[ 66] :=  0;                   cdx[ 67] := +ptrsize;
      cdx[ 68] := -adrsize*2;           cdx[ 69] :=  0;
      cdx[ 70] :=  0;                   cdx[ 71] := +ptrsize;
      cdx[ 72] :=  0;                   cdx[ 73] := +intsize;
      cdx[ 74] := -adrsize*2;           cdx[ 75] :=  2{*};
      cdx[ 76] :=  4{*};                cdx[ 77] :=  +intsize*2;
      cdx[ 78] := -intsize;             cdx[ 79] :=  +adrsize;
      cdx[ 80] :=  2{*};                cdx[ 81] :=  0;
      cdx[ 82] :=  0;                   cdx[ 83] := +intsize;
      cdx[ 84] := -adrsize;             cdx[ 85] := +adrsize;
      cdx[ 86] := 0;                    cdx[ 87] := 0;
      cdx[ 88] := 0;                    cdx[ 89] := 0;
      cdx[ 90] := 0;                    cdx[ 91] := 0;
      cdx[ 92] := 0;                    cdx[ 93] := +intsize;
      cdx[ 94] := 0;                    cdx[ 95] := 0;
      cdx[ 96] := 0;                    cdx[ 97] := 0;
      cdx[ 98] := -adrsize;             cdx[ 99] := 0;
      cdx[100] := 0;                    cdx[101] := +ptrsize*4;
      cdx[102] := +ptrsize*4;           cdx[103] := +intsize+ptrsize;
      cdx[104] := +intsize;             cdx[105] := -adrsize;
      cdx[106] := +ptrsize*2;           cdx[107] := +ptrsize;
      cdx[108] := 0;                    cdx[109] := 0;
      cdx[110] := +ptrsize*3;           cdx[111] := -adrsize;
      cdx[112] := 0;                    cdx[113] := +ptrsize;
      cdx[114] := -adrsize;             cdx[115] := 0;
      cdx[116] := 0;                    cdx[117] := 0;
      cdx[118] := -adrsize;             cdx[119] := 0;
      cdx[120] := 0;                    cdx[121] := 0;
      cdx[122] := 0;

      { secondary table order is i, r, b, c, a, s, m }
      cdxs[1][1] := +(adrsize+intsize);  { stoi }
      cdxs[1][2] := +(adrsize+realsize); { stor }
      cdxs[1][3] := +(adrsize+intsize);  { stob }
      cdxs[1][4] := +(adrsize+intsize);  { stoc }
      cdxs[1][5] := +(adrsize+adrsize);  { stoa }
      cdxs[1][6] := +(adrsize+setsize);  { stos }
      cdxs[1][7] := 0;
      cdxs[1][8] := 0;

      cdxs[2][1] := 0; { deci/inci/ordi/chki/reti/noti }
      cdxs[2][2] := 0; { chkr/retr }
      cdxs[2][3] := 0; { decb/incb/ordb/chkb/retb/notb }
      cdxs[2][4] := 0; { decc/incc/ordc/chkc/retc }
      cdxs[2][5] := 0; { chka/reta/ckl }
      cdxs[2][6] := 0; { chks }
      cdxs[2][7] := 0;
      cdxs[2][8] := 0;

      cdxs[3][1] := +adrsize-intsize;  { indi }
      cdxs[3][2] := +adrsize-realsize; { indr }
      cdxs[3][3] := +adrsize-intsize;  { indb }
      cdxs[3][4] := +adrsize-intsize;  { indc }
      cdxs[3][5] := +adrsize-adrsize;  { inda }
      cdxs[3][6] := +adrsize-setsize;  { inds }
      cdxs[3][7] := 0;
      cdxs[3][8] := 0;

      cdxs[4][1] := -intsize;  { ldoi/ldc/lodi/dupi/ltc }
      cdxs[4][2] := -realsize; { ldor/ldc/lodr/dupr/ltc }
      cdxs[4][3] := -intsize;  { ldob/ldc/lodb/dupb/ltc }
      cdxs[4][4] := -intsize;  { ldoc/ldc/lodc/dupc/ltc }
      cdxs[4][5] := -adrsize;  { ldoa/ldc/loda/dupa/ltc }
      cdxs[4][6] := -setsize;  { ldos/ldc/lods/dups/ltc }
      cdxs[4][7] := 0;
      cdxs[4][8] := 0;

      cdxs[5][1] := +intsize;  { sroi/stri }
      cdxs[5][2] := +realsize; { sror/strr }
      cdxs[5][3] := +intsize;  { srob/strb }
      cdxs[5][4] := +intsize;  { sroc/strc }
      cdxs[5][5] := +adrsize;  { sroa/stra }
      cdxs[5][6] := +setsize;  { sros/strs }
      cdxs[5][7] := 0;
      cdxs[5][8] := 0;

      { note that all of the comparisions share the same table }
      cdxs[6][1] := +(intsize+intsize)-intsize; { equi/neqi/geqi/grti/leqi/lesi }
      cdxs[6][2] := +(realsize+realsize)-intsize; { equr/neqr/geqr/grtr/leqr/lesr }
      cdxs[6][3] := +(intsize+intsize)-intsize; { equb/neqb/geqb/grtb/leqb/lesb }
      cdxs[6][4] := +(intsize+intsize)-intsize; { equc/neqc/geqc/grtc/leqc/lesc }
      cdxs[6][5] := +(adrsize+intsize)-adrsize; { equa/neqa/geqa/grta/leqa/lesa }
      cdxs[6][6] := +(setsize+setsize)-intsize; { equs/neqs/geqs/grts/leqs/less }
      cdxs[6][7] := +(adrsize+adrsize)-intsize; { equm/neqm/geqm/grtm/leqm/lesm }
      cdxs[6][8] := +(adrsize*2+adrsize*2)-intsize; { equv/neqv/geqv/grtv/leqv/lesv }

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

  extendinit; { initialize extentions package }

  { Suppress unreferenced errors. These are all MPB (machine parameter
    block) equations that need to stay the same between front end and backend. }
  if heapal = 0 then;
  if inthex = 0 then;
  if market = 0 then;
  if markep = 0 then;
  if marksb = 0 then;
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
  end else begin
    writeln('Pascal-P6 complies with the requirements of Pascaline version 0.4');
    writeln('and the following annexes: A,B,C,E.');
    writeln
  end;

  (*enter standard names and standard types:*)
  (******************************************)
  level := 0; top := 0; ptop := 0;
  with display[0] do
    begin inidsp(display[0]); define := true; occur := blck; bname := nil end;
  enterstdtypes; stdnames; entstdnames; enterundecl;
  top := 1; level := 1;
  with display[1] do
    begin inidsp(display[1]); define := true; occur := blck; bname := nil end;

  { get command line }
  getcommandline(cmdlin, cmdlen);
  cmdpos := 1;
  { load command line options }
  paroptions;

  (*compile:*)
  (**********)

  { !!! remove these statements for self compile }
#ifndef SELF_COMPILE
  reset(prd); rewrite(prr); { open output file }
#endif

  { write generator comment }
  if prcode then begin
    writeln(prr, '!');
    writeln(prr, '! Pascal intermediate file Generated by P6 Pascal compiler vs. ',
            majorver:1, '.', minorver:1);
    writeln(prr, '!');

    { write initial option values }
    write(prr, 'o ');
    for oi := 1 to maxopt do
      { exclude pint options and unused }
      if not (oi in [7,8,14,15,16,13,17,19,23,1,6,5,18,11,26]) then
        begin 
      for oni :=  1 to optlen do 
        if optsl[oi, oni] <> ' ' then write(prr, optsl[oi, oni]);
      if option[oi] then write(prr, '+') else write(prr, '-');
      write(prr, ' ')
    end;
    writeln(prr)
  end;

  nvalid := false; { set no lookahead }
  { init for lookahead }
  sy := ident; op := mul; lgth := 0; kk := 1; ch := ' ';
  readline;
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
  for i := 1 to maxftl do if errtbl[i] > 0 then begin
    if f then begin
      writeln;
      writeln('Error numbers in listing:');
      writeln('-------------------------');
      f := false
    end;
    write(i:3, ' ', errtbl[i]:3, ' '); errmsg(i); writeln
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
    writeln('tag entry tracking counts:  ', ttpcnt:1);
    writeln('with entry tracking counts: ', wtpcnt:1);
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
  if ttpcnt <> 0 then
     writeln('*** Error: Compiler internal error: tag recycle balance: ',
             cipcnt:1);
  if wtpcnt <> 0 then
     writeln('*** Error: Compiler internal error: with recycle balance: ',
             wtpcnt:1);

  99:

end.
