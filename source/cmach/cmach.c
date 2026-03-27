/*******************************************************************************
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
* by 1, but by a number dep}ing on the type concerned.                       *
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
* }ing 'a', because files are now full variable references.                  *
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
* This version translated to C from pmach.pas [saf]                            *
*                                                                              *
* Speeds:                                                                      *
*                                                                              *
* Packaged mode, DOCHKDEF and DOSRCLIN both off: 1.147s on fbench.             *
* GPC direct compile of fbench: 0.184s                                         *
*                                                                              *
* Thus the interpreted version is 6.2 times slower, and is under the 10 times  *
* slower "rule of thumb" I have observed with interpreters.                    *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>

/*******************************************************************************

                                     KNOBS

*******************************************************************************/

/* Set default configuration flags. This gives proper behavior even if no
  preprocessor flags are passed in.

  The defaults are:
  WRDSIZ32       - 32 bit compiler.
  ISO7185_PASCAL - uses ISO 7185 standard language only.
*/
#if !defined(WRDSIZ32) && !defined(WRDSIZ64)
#define WRDSIZ32 1
#endif

/* Check flags: these turn on runtime checks
 *
 * These flags can be overridden by compiler define set
 */
#ifndef DOCHKOVF
#define DOCHKOVF TRUE /* check arithmetic overflow */
#endif

#ifndef DOSRCLIN
#define DOSRCLIN TRUE /* add source line sets to code */
#endif

#ifndef DORECYCL
#define DORECYCL TRUE /* obey heap space recycle requests */
#endif

/* invoke a special recycle mode that creates single word entries on
  recycle of any object, breaking off and recycling the rest. Once
  allocated, each entry exists forever, and accesses to it can be
  checked. */
#ifndef DOCHKRPT
#define DOCHKRPT FALSE /* check reuse of freed entry (automatically
                           invokes dorecycl = false */
#endif

/*
 * Companion flag to DOCHKRPT: break returned blocks as occupied. not free.
 * This means that The entire disposed block will be kept out of circulation,
 * but the first word will be kept as a "disposed block" marker.
 *
 * This helps catch errors when references exist within blocks. This could
 * happen for several reasons, for example a with reference, a var reference,
 * or similar cached pointer. Thus this flag modifies DOCHKRPT to leave returned
 * space totally unused.
 */
#ifndef DONORECPAR
#define DONORECPAR FALSE /* do not recycle part of returned entries */
#endif

#ifndef DOCHKDEF
#define DOCHKDEF TRUE /* check undefined accesses */
#endif

#ifndef ISO7185
#define ISO7185 FALSE /* iso7185 standard flag */
#endif

/*******************************************************************************

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

*/

/******************************************************************************

16 bit Machine Parameter block (MPB)

******************************************************************************/

#ifdef WRDSIZ16
#include "mpb16.inc"
#endif

#ifdef WRDSIZ32
#include "mpb32.inc"
#endif

#ifdef WRDSIZ64
#include "mpb64.inc"
#endif

/* ******************* end of pcom and pint common parameters *********** */

/* internal constants */

#define TRUE 1                /* value of true */
#define FALSE 0               /* value of false */

#ifdef WRDSIZ16
#define MAXSTR       31999 /* maximum size of addressing for program/var */
#define MAXTOP       32000 /* maximum size of addressing for program/var+1 */
#define MAXDEF       4000  /* maxstr / 8 for defined bits */
#else
#define MAXSTR       16777215 /* maximum size of addressing for program/var */
#define MAXTOP       16777216 /* maximum size of addressing for program/var+1 */
#define MAXDEF       2097152  /* maxstr / 8 for defined bits */
#endif

#define MAXDIGH      6        /* number of digits in hex representation of maxstr */
#define MAXDIGD      8        /* number of digits in decimal representation of maxstr */
#define MAXAST       100      /* maximum size of assert message */
#define MAXDBF       30       /* size of numeric conversion buffer */
#define MAXCMD       250      /* size of command line buffer */

#define CODEMAX      MAXSTR   /* set size of code store to maximum possible */

#define MAXLABEL  5000        /* total possible labels in intermediate */
#define MAXCSTFX  10000       /* maximum constant fixup in intermediate */
#define MAXGBLFX  10000       /* maximum global access fixup in intermediate */
#define RESSPC    0           /* reserve space in heap (if you want) */

/* locations of header files after program block mark, each header
  file is two values, a file number and a single character buffer */
#define FILRES      2         /* space reserved for file */
#define INPUTOFF    0         /* 'input' file address */
#define OUTPUTOFF   2         /* 'output' file address */
#define PRDOFF      4         /* 'prd' file address */
#define PRROFF      6         /* 'prr' file address */
#define ERROROFF    8         /* 'error' file address */
#define LISTOFF     10        /* 'list' file address */
#define COMMANDOFF  12        /* 'command' file address */

/* assigned logical channels for header files */
#define INPUTFN     1         /* 'input' file no. */
#define OUTPUTFN    2         /* 'output' file no. */
#define PRDFN       3         /* 'prd' file no. */
#define PRRFN       4         /* 'prr' file no. */
#define ERRORFN     5         /* 'error' file no. */
#define LISTFN      6         /* 'list' file no. */
#define COMMANDFN   7         /* 'command' file no. */

/* locations of standard exceptions */
#define EXCEPTIONBASE                       14
#define VALUEOUTOFRANGE                     14
#define ARRAYLENGTHMATCH                    15
#define CASEVALUENOTFOUND                   16
#define ZERODIVIDE                          17
#define INVALIDOPERAND                      18
#define NILPOINTERDEREFERENCE               19
#define REALOVERFLOW                        20
#define REALUNDERFLOW                       21
#define REALPROCESSINGFAULT                 22
#define TAGVALUENOTACTIVE                   23
#define TOOMANYFILES                        24
#define FILEISOPEN                          25
#define FILEALREADYNAMED                    26
#define FILENOTOPEN                         27
#define FILEMODEINCORRECT                   28
#define INVALIDFIELDSPECIFICATION           29
#define INVALIDREALNUMBER                   30
#define INVALIDFRACTIONSPECIFICATION        31
#define INVALIDINTEGERFORMAT                32
#define INTEGERVALUEOVERFLOW                33
#define INVALIDREALFORMAT                   34
#define ENDOFFILE                           35
#define INVALIDFILEPOSITION                 36
#define FILENAMETOOLONG                     37
#define FILEOPENFAIL                        38
#define FILESIZEFAIL                        39
#define FILECLOSEFAIL                       40
#define FILEREADFAIL                        41
#define FILEWRITEFAIL                       42
#define FILEPOSITIONFAIL                    43
#define FILEDELETEFAIL                      44
#define FILENAMECHANGEFAIL                  45
#define SPACEALLOCATEFAIL                   46
#define SPACERELEASEFAIL                    47
#define SPACEALLOCATENEGATIVE               48
#define CANNOTPERFORMSPECIAL                49
#define COMMANDLINETOOLONG                  50
#define READPASTEOF                         51
#define FILETRANSFERLENGTHZERO              52
#define FILESIZETOOLARGE                    53
#define FILENAMEEMPTY                       54
#define CANNOTOPENSTANDARD                  55
#define TOOMANYTEMPORARYFILES               56
#define INPUTBUFFEROVERFLOW                 57
#define TOOMANYTHREADS                      58
#define CANNOTSTARTTHREAD                   59
#define INVALIDTHREADHANDLE                 60
#define CANNOTSTOPTHREAD                    61
#define TOOMANYINTERTASKLOCKS               62
#define INVALIDLOCKHANDLE                   63
#define LOCKSEQUENCEFAIL                    64
#define TOOMANYSIGNALS                      65
#define CANNOTCREATESIGNAL                  66
#define INVALIDSIGNALHANDLE                 67
#define CANNOTDELETESIGNAL                  68
#define CANNOTSENDSIGNAL                    69
#define WAITFORSIGNALFAIL                   70
#define FIELDNOTBLANK                       71
#define READONWRITEONLYFILE                 72
#define WRITEONREADONLYFILE                 73
#define FILEBUFFERVARIABLEUNDEFINED         74
#define NONDECIMALRADIXOFNEGATIVE           75
#define INVALIDARGUMENTTOLN                 76
#define INVALIDARGUMENTTOSQRT               77
#define CANNOTRESETORREWRITESTANDARDFILE    78
#define CANNOTRESETWRITEONLYFILE            79
#define CANNOTREWRITEREADONLYFILE           80
#define SETELEMENTOUTOFRANGE                81
#define REALARGUMENTTOOLARGE                82
#define BOOLEANOPERATOROFNEGATIVE           83
#define INVALIDDIVISORTOMOD                 84
#define PACKELEMENTSOUTOFBOUNDS             85
#define UNPACKELEMENTSOUTOFBOUNDS           86
#define CANNOTRESETCLOSEDTEMPFILE           87
#define READCHARACTERMISMATCH               88
#define EXCEPTIONTOP                        88

/* Exceptions that can't be caught.
  Note that these don't have associated exception variables. */

#define UNDEFINEDLOCATIONACCESS             89
#define FUNCTIONNOTIMPLEMENTED              90
#define INVALIDINISO7185MODE                91
#define HEAPFORMATINVALID                   92
#define DISPOSEOFUNINITALIZEDPOINTER        93
#define DISPOSEOFNILPOINTER                 94
#define BADPOINTERVALUE                     95
#define BLOCKALREADYFREED                   96
#define INVALIDSTANDARDPROCEDUREORFUNCTION  97
#define INVALIDINSTRUCTION                  98
#define NEWDISPOSETAGSMISMATCH              99
#define PCOUTOFRANGE                        100
#define STOREOVERFLOW                       101
#define STACKBALANCE                        102
#define SETINCLUSION                        103
#define UNINITIALIZEDPOINTER                104
#define DEREFERENCEOFNILPOINTER             105
#define POINTERUSEDAFTERDISPOSE             106
#define VARIANTNOTACTIVE                    107
#define INVALIDCASE                         108
#define SYSTEMERROR                         109
#define CHANGETOALLOCATEDTAGFIELD           110
#define UNHANDLEDEXCEPTION                  111
#define PROGRAMCODEASSERTION                112
#define VARLISTEMPTY                        113
#define CHANGETOVARREFERENCEDVARIANT        114
#define DISPOSEOFVARREFERENCEDBLOCK         115
#define VARREFERENCEDFILEBUFFERMODIFIED     116
#define CONTAINERMISMATCH                   117
#define INVALIDCONTAINERLEVEL               118
#define DISPOSEOFWITHREFERENCEDBLOCK        119
#define WITHBASELISTEMPTY                   120
#define EXTERNALSNOTENABLED                 121
#define PRIVEXCEPTIONTOP                    121

#define MAXSP        111  /* number of predefined procedures/functions */
#define MAXINS       255  /* maximum instruction code, 0-255 or byte */
#define MAXFIL       100  /* maximum number of general (temp) files */
#define FILLEN       2000 /* maximum length of filenames */
#define REALEF       8    /* real extra field in floating format -1.0e+00 */
#define MAXOPT       26   /* number of options */
#define OPTLEN       10   /* maximum length of option words */

/* version numbers */

#define MAJORVER 0
#define MINORVER 4
#define EXPERIMENT 1

/* typedefs */

/* These equates define the instruction layout. I have choosen a 32 bit
  layout for the instructions defined by (4 bit) digit:

     byte 0:   Instruction code
     byte 1:   P parameter
     byte 2-5: Q parameter

  This means that there are 256 instructions, 256 procedure levels,
  and 2gb of total addressing. This could be 4gb if we get rid of the
  need for negatives. */
typedef unsigned char byte;    /* 8-bit byte */
typedef long boolean;           /* true/false */
typedef byte lvltyp;           /* procedure/function level */
typedef byte instyp;           /* instruction */
typedef enum {
    pi_lodi,    /*   0 */ pi_ldoi,    /*   1 */ pi_stri,    /*   2 */
    pi_sroi,    /*   3 */ pi_lda,     /*   4 */ pi_lao,     /*   5 */
    pi_stoi,    /*   6 */ pi_ldcs,    /*   7 */ pi_cjp,     /*   8 */
    pi_indi,    /*   9 */ pi_inci,    /*  10 */ pi_mst,     /*  11 */
    pi_cup,     /*  12 */ pi_rip,     /*  13 */ pi_retp,    /*  14 */
    pi_csp,     /*  15 */ pi_ixa,     /*  16 */ pi_equa,    /*  17 */
    pi_neqa,    /*  18 */ pi_brk,     /*  19 */ pi_lnp,     /*  20 */
    pi_cal,     /*  21 */ pi_ret,     /*  22 */ pi_ujp,     /*  23 */
    pi_fjp,     /*  24 */ pi_xjp,     /*  25 */ pi_chki,    /*  26 */
    pi_cuv,     /*  27 */ pi_adi,     /*  28 */ pi_adr,     /*  29 */
    pi_sbi,     /*  30 */ pi_sbr,     /*  31 */ pi_sgs,     /*  32 */
    pi_flt,     /*  33 */ pi_flo,     /*  34 */ pi_trc,     /*  35 */
    pi_ngi,     /*  36 */ pi_ngr,     /*  37 */ pi_sqi,     /*  38 */
    pi_sqr,     /*  39 */ pi_abi,     /*  40 */ pi_abr,     /*  41 */
    pi_notb,    /*  42 */ pi_and,     /*  43 */ pi_ior,     /*  44 */
    pi_dif,     /*  45 */ pi_int,     /*  46 */ pi_uni,     /*  47 */
    pi_inn,     /*  48 */ pi_mod,     /*  49 */ pi_odd,     /*  50 */
    pi_mpi,     /*  51 */ pi_mpr,     /*  52 */ pi_dvi,     /*  53 */
    pi_dvr,     /*  54 */ pi_mov,     /*  55 */ pi_lca,     /*  56 */
    pi_deci,    /*  57 */ pi_stp,     /*  58 */ pi_ph11,    /*  59 */
    pi_ph12,    /*  60 */ pi_ujc,     /*  61 */ pi_rnd,     /*  62 */
    pi_pck,     /*  63 */ pi_upk,     /*  64 */ pi_ldoa,    /*  65 */
    pi_ldor,    /*  66 */ pi_ldos,    /*  67 */ pi_ldob,    /*  68 */
    pi_ldoc,    /*  69 */ pi_stra,    /*  70 */ pi_strr,    /*  71 */
    pi_strs,    /*  72 */ pi_strb,    /*  73 */ pi_strc,    /*  74 */
    pi_sroa,    /*  75 */ pi_sror,    /*  76 */ pi_sros,    /*  77 */
    pi_srob,    /*  78 */ pi_sroc,    /*  79 */ pi_stoa,    /*  80 */
    pi_stor,    /*  81 */ pi_stos,    /*  82 */ pi_stob,    /*  83 */
    pi_stoc,    /*  84 */ pi_inda,    /*  85 */ pi_indr,    /*  86 */
    pi_inds,    /*  87 */ pi_indb,    /*  88 */ pi_indc,    /*  89 */
    pi_inca,    /*  90 */ pi_suv,     /*  91 */ pi_vbs,     /*  92 */
    pi_incb,    /*  93 */ pi_incc,    /*  94 */ pi_chka,    /*  95 */
    pi_vbe,     /*  96 */ pi_chks,    /*  97 */ pi_chkb,    /*  98 */
    pi_chkc,    /*  99 */ pi_cvbi,    /* 100 */ pi_ivtx,    /* 101 */
    pi_ivtb,    /* 102 */ pi_decb,    /* 103 */ pi_decc,    /* 104 */
    pi_loda,    /* 105 */ pi_lodr,    /* 106 */ pi_lods,    /* 107 */
    pi_lodb,    /* 108 */ pi_lodc,    /* 109 */ pi_rgs,     /* 110 */
    pi_ivtc,    /* 111 */ pi_ipj,     /* 112 */ pi_cip,     /* 113 */
    pi_lpa,     /* 114 */ pi_cvbx,    /* 115 */ pi_cvbb,    /* 116 */
    pi_dmp,     /* 117 */ pi_swp,     /* 118 */ pi_tjp,     /* 119 */
    pi_lip,     /* 120 */ pi_cvbc,    /* 121 */ pi_vis,     /* 122 */
    pi_ldci,    /* 123 */ pi_ldcr,    /* 124 */ pi_ldcn,    /* 125 */
    pi_ldcb,    /* 126 */ pi_ldcc,    /* 127 */ pi_reti,    /* 128 */
    pi_retr,    /* 129 */ pi_retc,    /* 130 */ pi_retb,    /* 131 */
    pi_reta,    /* 132 */ pi_vip,     /* 133 */ pi_ph13,    /* 134 */
    pi_lcp,     /* 135 */ pi_ph14,    /* 136 */ pi_equi,    /* 137 */
    pi_equr,    /* 138 */ pi_equb,    /* 139 */ pi_equs,    /* 140 */
    pi_equc,    /* 141 */ pi_equm,    /* 142 */ pi_neqi,    /* 143 */
    pi_neqr,    /* 144 */ pi_neqb,    /* 145 */ pi_neqs,    /* 146 */
    pi_neqc,    /* 147 */ pi_neqm,    /* 148 */ pi_geqi,    /* 149 */
    pi_geqr,    /* 150 */ pi_geqb,    /* 151 */ pi_geqs,    /* 152 */
    pi_geqc,    /* 153 */ pi_geqm,    /* 154 */ pi_grti,    /* 155 */
    pi_grtr,    /* 156 */ pi_grtb,    /* 157 */ pi_grts,    /* 158 */
    pi_grtc,    /* 159 */ pi_grtm,    /* 160 */ pi_leqi,    /* 161 */
    pi_leqr,    /* 162 */ pi_leqb,    /* 163 */ pi_leqs,    /* 164 */
    pi_leqc,    /* 165 */ pi_leqm,    /* 166 */ pi_lesi,    /* 167 */
    pi_lesr,    /* 168 */ pi_lesb,    /* 169 */ pi_less,    /* 170 */
    pi_lesc,    /* 171 */ pi_lesm,    /* 172 */ pi_ph1,     /* 173 */
    pi_mrkl,    /* 174 */ pi_ckvi,    /* 175 */ pi_cps,     /* 176 */
    pi_cpc,     /* 177 */ pi_aps,     /* 178 */ pi_ckvb,    /* 179 */
    pi_ckvc,    /* 180 */ pi_dupi,    /* 181 */ pi_dupa,    /* 182 */
    pi_dupr,    /* 183 */ pi_dups,    /* 184 */ pi_dupb,    /* 185 */
    pi_dupc,    /* 186 */ pi_cks,     /* 187 */ pi_cke,     /* 188 */
    pi_inv,     /* 189 */ pi_ckla,    /* 190 */ pi_cta,     /* 191 */
    pi_ivti,    /* 192 */ pi_lodx,    /* 193 */ pi_ldox,    /* 194 */
    pi_strx,    /* 195 */ pi_srox,    /* 196 */ pi_stox,    /* 197 */
    pi_indx,    /* 198 */ pi_chkx,    /* 199 */ pi_ph15,    /* 200 */
    pi_incx,    /* 201 */ pi_decx,    /* 202 */ pi_ckvx,    /* 203 */
    pi_retx,    /* 204 */ pi_noti,    /* 205 */ pi_xor,     /* 206 */
    pi_bge,     /* 207 */ pi_ede,     /* 208 */ pi_mse,     /* 209 */
    pi_apc,     /* 210 */ pi_cxs,     /* 211 */ pi_cxc,     /* 212 */
    pi_lft,     /* 213 */ pi_max,     /* 214 */ pi_equv,    /* 215 */
    pi_neqv,    /* 216 */ pi_lesv,    /* 217 */ pi_grtv,    /* 218 */
    pi_leqv,    /* 219 */ pi_geqv,    /* 220 */ pi_vdp,     /* 221 */
    pi_spc,     /* 222 */ pi_ccs,     /* 223 */ pi_scp,     /* 224 */
    pi_ldp,     /* 225 */ pi_vin,     /* 226 */ pi_vdd,     /* 227 */
    pi_ph3,     /* 228 */ pi_ph4,     /* 229 */ pi_ph5,     /* 230 */
    pi_ph6,     /* 231 */ pi_ph7,     /* 232 */ pi_ph8,     /* 233 */
    pi_ph9,     /* 234 */ pi_stom,    /* 235 */ pi_rets,    /* 236 */
    pi_retm,    /* 237 */ pi_ctb,     /* 238 */ pi_cpp,     /* 239 */
    pi_cpr,     /* 240 */ pi_lsa,     /* 241 */ pi_eext,    /* 242 */
    pi_wbs,     /* 243 */ pi_wbe,     /* 244 */ pi_sfr,     /* 245 */
    pi_cuf,     /* 246 */ pi_cif,     /* 247 */ pi_mpc,     /* 248 */
    pi_cvf,     /* 249 */ pi_ph10,    /* 250 */ pi_cpl,     /* 251 */
    pi_sfs,     /* 252 */ pi_sev,     /* 253 */ pi_mdc,     /* 254 */
    pi_ph2,     /* 255 */
    /* past the 255 instruction, these instructions don't fit
       into a single byte. Thus after this, these are pseudo
       instructions that don't end up as machine instructions. */
    pi_ltci,    /* 256 */ pi_ltcr,    /* 257 */ pi_ltcs,    /* 258 */
    pi_ltcb,    /* 259 */ pi_ltcc,    /* 260 */ pi_ltcx,    /* 261 */
    pi_lto,     /* 262 */ pi_lsp,     /* 263 */ pi_ordi,    /* 264 */
    pi_chr,     /* 265 */ pi_ordb,    /* 266 */ pi_ordc,    /* 267 */
    pi_ordx     /* 268 */
} mi;
typedef long address;           /* address */

typedef char beta[25];         /* error message */
typedef byte settype[SETSIZE]; /* standard set */

typedef long filnum;            /* logical file number */
typedef char filnam[FILLEN];   /* filename strings */
typedef enum {
  fsnone,
  fsclosed,
  fsread,
  fswrite
} filsts;                      /* file states */
typedef long cmdinx;            /* index for command line buffer */
typedef long cmdnum;            /* length of command line buffer */
typedef char cmdbuf[MAXCMD];   /* buffer for command line */
/* VAR reference block */
typedef struct _varblk *varptr;
typedef struct _varblk {
    varptr next;  /* next entry */
    address s, e; /* start and end address of block */
} varblk;
/* with reference block */
typedef struct _wthblk *wthptr;
typedef struct _wthblk {
    wthptr next;  /* next entry */
    address b;    /* address of block */
} wthblk;

/******************************* Constants ************************************/

char* opts[] = {
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "ee",
    "",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z"
};

char* optsl[] = {
    "debugflt",
    "prtlab",
    "lstcod",
    "chkdebug",
    "machdeck",
    "debugsrc",
    "prtlabdef",
    "sourceset",
    "varblk",
    "experror",
    "",
    "list",
    "breakheap",
    "recycle",
    "chkoverflo",
    "chkreuse",
    "chkundef",
    "reference",
    "iso7185",
    "prttables",
    "undeftag",
    "chkvar",
    "debug",
    "prtlex",
    "prtdisplay",
    "lineinfo"
};

/**************************** Global Variables ********************************/

address pc;      /*program address register*/
address pctop;   /* top of code store */
address gbtop;   /* top of globals, size of globals */
instyp op; lvltyp p; address q;  /*instruction register*/
address q1,q2; /* extra parameters */
byte store[MAXSTR] /* complete program storage */
/* package mode fills the program store, sets pctop and executes the prepackaged
   program. */
#ifdef PACKAGE
#include "program_code.c"
#endif
;
byte storedef[MAXDEF]; /* defined bits */
long sdi; /* index for that */
/* mp  points to {ning of a data segment
   sp  points to top of the stack
   ep  points to the maximum extent of the stack
   np  points to top of the dynamically allocated area */
address mp,sp,np,ep;  /* address registers */
address expadr; /* exception address of exception handler starts */
address expstk; /* exception address of sp at handlers */
address expmrk; /* exception address of mp at handlers */

byte bitmsk[8]; /* bits in byte */

long    srclin;  /* current source line executing */
cmdbuf  cmdlin;  /* command line */
cmdnum  cmdlen;  /* length of command line */
cmdinx  cmdpos;  /* current position in command line */
boolean stopins; /* stop instruction executed */

FILE* filtable[MAXFIL+1]; /* general file holders */
filnam filnamtab[MAXFIL+1]; /* assigned name of files */
boolean filanamtab[MAXFIL+1]; /* name has been assigned flags */
filsts filstate[MAXFIL+1]; /* file state holding */
boolean filbuff[MAXFIL+1]; /* file buffer full status */
boolean fileoln[MAXFIL+1]; /* last file character read was eoln */
boolean filbof[MAXFIL+1]; /* beginning of file */
varptr varlst; /* active var block pushdown stack */
varptr varfre; /* free var block entries */
wthptr wthlst; /* active with block pushdown stack */
int wthcnt; /* number of outstanding with levels */
wthptr wthfre; /* free with block entries */
int exitcode; /* exit code for program */
boolean option[MAXOPT]; /* option array */

/* check flags: these turn on runtime checks */
boolean dochkovf; /* check arithmetic overflow */
/* debug flags: turn these on for various dumps and traces */
boolean dodmplab; /* dump label definitions */
boolean dotrcrot; /* trace routine executions */
boolean dotrcins; /* trace instruction executions */
boolean dosrclin; /* add source line sets to code */
boolean dotrcsrc; /* trace source line executions (requires dosrclin) */
boolean dorecycl; /* obey heap space recycle requests */
boolean dodebug; /* start up debug on entry */
boolean dodbgflt; /* enter debug on fault */
/* Don't set this option unless you have file language extensions!
  It will just cause the run to fail */
boolean dodbgsrc; /* do source file debugging */
/* invoke a special recycle mode that creates single word entries on
  recycle of any object, breaking off and recycling the rest. Once
  allocated, each entry exists forever, and accesses to it can be
  checked. */
boolean dochkrpt; /* check reuse of freed entry (automatically
                     invokes dorecycl = false */
boolean donorecpar; /* companion flag to dochkrpt: break returned blocks
                       as occupied, not free. This essentially converts
                       disposed blocks to flagged but dead entries that
                       generate errors on use. */
boolean dochkdef; /* check undefined accesses */
boolean dosrcprf; /* do source level profiling */
boolean dochkcov; /* do code coverage */
boolean doanalys; /* do analyze */
boolean dodckout; /* do output code deck */
boolean dochkvbk; /* do check VAR blocks */
boolean iso7185; /* iso7185 standard flag */

long i;
char c1;
address ad;
long bai;

void dmpmem(address s, address e)
{
    long c;

    c = 0;
    while (s <= e) {
        if (!c) printf("%08lX: ", s);
        printf("%02X ", store[s]); c++;
        s++;
        if (c == 16) { printf("\n"); c = 0; }
    }
    if (c) printf("\n");
    printf("\n");
}

/*--------------------------------------------------------------------*/

/* Low level error check and handling */

void finish(long e)
{
    for (i = COMMANDFN+1; i <= MAXFIL; i++) 
      if (filstate[i] != fsclosed && filstate[i] != fsnone) {
        fclose(filtable[i]);
        if (!filanamtab[i]) remove(filnamtab[i]);
    }
    printf("\n");
#ifndef PACKAGE
    printf("program complete\n");
#endif
    exit(e);
}

void errors(address a, address l)
{ printf("\n*** Runtime error\n");
      if (srclin > 0) printf(" [%ld]: ", srclin);
      if (l > MAXAST) l = MAXAST;
      while (l > 0) { printf("%c", store[a]); a = a+1; l = l-1; }
      finish(1);
} /*errori*/

/* Error handling:

  To throw a standard exception, errore() is used. To bypass the interceptable
  exceptions and directly print, use errorv().

*/

/* handle exception vector */
void errorv(address ea)
{ printf("\n*** Runtime error");
  if (srclin > 0) printf(" [%ld]: ", srclin);
  switch (ea) {

    /* Exceptions that can be intercepted */
    case VALUEOUTOFRANGE:                    printf("Value out of range\n"); break;
    case ARRAYLENGTHMATCH:                   printf("Array length match\n"); break;
    case CASEVALUENOTFOUND:                  printf("Case value not found\n"); break;
    case ZERODIVIDE:                         printf("Zero divide\n"); break;
    case INVALIDOPERAND:                     printf("Invalid operand\n"); break;
    case NILPOINTERDEREFERENCE:              printf("Nil pointer dereference\n"); break;
    case REALOVERFLOW:                       printf("Real overflow\n"); break;
    case REALUNDERFLOW:                      printf("Real underflow\n"); break;
    case REALPROCESSINGFAULT:                printf("Real processing fault\n"); break;
    case TAGVALUENOTACTIVE:                  printf("Tag value not active\n"); break;
    case TOOMANYFILES:                       printf("Too many files\n"); break;
    case FILEISOPEN:                         printf("File is open\n"); break;
    case FILEALREADYNAMED:                   printf("File already named\n"); break;
    case FILENOTOPEN:                        printf("File not open\n"); break;
    case FILEMODEINCORRECT:                  printf("File mode incorrect\n"); break;
    case INVALIDFIELDSPECIFICATION:          printf("Invalid field specification\n"); break;
    case INVALIDREALNUMBER:                  printf("Invalid real number\n"); break;
    case INVALIDFRACTIONSPECIFICATION:       printf("Invalid fraction specification\n"); break;
    case INVALIDINTEGERFORMAT:               printf("Invalid integer format\n"); break;
    case INTEGERVALUEOVERFLOW:               printf("Integer value overflow\n"); break;
    case INVALIDREALFORMAT:                  printf("Invalid real format\n"); break;
    case ENDOFFILE:                          printf("End of file\n"); break;
    case INVALIDFILEPOSITION:                printf("Invalid file position\n"); break;
    case FILENAMETOOLONG:                    printf("Filename too long\n"); break;
    case FILEOPENFAIL:                       printf("File open fail\n"); break;
    case FILESIZEFAIL:                       printf("File size fail\n"); break;
    case FILECLOSEFAIL:                      printf("File close fail\n"); break;
    case FILEREADFAIL:                       printf("File read fail\n"); break;
    case FILEWRITEFAIL:                      printf("File write fail\n"); break;
    case FILEPOSITIONFAIL:                   printf("File position fail\n"); break;
    case FILEDELETEFAIL:                     printf("File delete fail\n"); break;
    case FILENAMECHANGEFAIL:                 printf("File name change fail\n"); break;
    case SPACEALLOCATEFAIL:                  printf("Space allocate fail\n"); break;
    case SPACERELEASEFAIL:                   printf("Space release fail\n"); break;
    case SPACEALLOCATENEGATIVE:              printf("Space allocate negative\n"); break;
    case CANNOTPERFORMSPECIAL:               printf("Cannot perform special\n"); break;
    case COMMANDLINETOOLONG:                 printf("Command line too long\n"); break;
    case READPASTEOF:                        printf("Read past eof\n"); break;
    case FILETRANSFERLENGTHZERO:             printf("File transfer length zero\n"); break;
    case FILESIZETOOLARGE:                   printf("File size too large\n"); break;
    case FILENAMEEMPTY:                      printf("Filename empty\n"); break;
    case CANNOTOPENSTANDARD:                 printf("Cannot open standard\n"); break;
    case TOOMANYTEMPORARYFILES:              printf("Too many temporary files\n"); break;
    case INPUTBUFFEROVERFLOW:                printf("Input buffer overflow\n"); break;
    case TOOMANYTHREADS:                     printf("Too many threads\n"); break;
    case CANNOTSTARTTHREAD:                  printf("Cannot start thread\n"); break;
    case INVALIDTHREADHANDLE:                printf("Invalid thread handle\n"); break;
    case CANNOTSTOPTHREAD:                   printf("Cannot stop thread\n"); break;
    case TOOMANYINTERTASKLOCKS:              printf("Too many inter task locks\n"); break;
    case INVALIDLOCKHANDLE:                  printf("Invalid lock handle\n"); break;
    case LOCKSEQUENCEFAIL:                   printf("Lock sequence fail\n"); break;
    case TOOMANYSIGNALS:                     printf("Too many signals\n"); break;
    case CANNOTCREATESIGNAL:                 printf("Cannot create signal\n"); break;
    case INVALIDSIGNALHANDLE:                printf("Invalid signal handle\n"); break;
    case CANNOTDELETESIGNAL:                 printf("Cannot delete signal\n"); break;
    case CANNOTSENDSIGNAL:                   printf("Cannot send signal\n"); break;
    case WAITFORSIGNALFAIL:                  printf("Wait for signal fail\n"); break;
    case FIELDNOTBLANK:                      printf("Field not blank\n"); break;
    case READONWRITEONLYFILE:                printf("Read on write only file\n"); break;
    case WRITEONREADONLYFILE:                printf("Write on read only file\n"); break;
    case FILEBUFFERVARIABLEUNDEFINED:        printf("File buffer variable undefined\n"); break;
    case NONDECIMALRADIXOFNEGATIVE:          printf("Nondecimal radix of negative\n"); break;
    case INVALIDARGUMENTTOLN:                printf("Invalid argument to ln\n"); break;
    case INVALIDARGUMENTTOSQRT:              printf("Invalid argument to sqrt\n"); break;
    case CANNOTRESETORREWRITESTANDARDFILE:   printf("Cannot reset or rewrite standard file\n"); break;
    case CANNOTRESETWRITEONLYFILE:           printf("Cannot reset write only file\n"); break;
    case CANNOTREWRITEREADONLYFILE :         printf("Cannot rewrite read only file\n"); break;
    case SETELEMENTOUTOFRANGE:               printf("Set element out of range\n"); break;
    case REALARGUMENTTOOLARGE:               printf("Real argument too large\n"); break;
    case BOOLEANOPERATOROFNEGATIVE:          printf("Boolean operator of negative\n"); break;
    case INVALIDDIVISORTOMOD:                printf("Invalid divisor to mod\n"); break;
    case PACKELEMENTSOUTOFBOUNDS:            printf("Pack elements out of bounds\n"); break;
    case UNPACKELEMENTSOUTOFBOUNDS:          printf("Unpack elements out of bounds\n"); break;
    case CANNOTRESETCLOSEDTEMPFILE:          printf("Cannot reset closed temp file\n"); break;
    case READCHARACTERMISMATCH:              printf("Read character mismatch"); break;

    /* Exceptions that can't be intercepted */
    case UNDEFINEDLOCATIONACCESS:            printf("Undefined location access\n"); break;
    case FUNCTIONNOTIMPLEMENTED:             printf("Function not implemented\n"); break;
    case INVALIDINISO7185MODE:               printf("Invalid in ISO 7185 mode\n"); break;
    case HEAPFORMATINVALID:                  printf("Heap format invalid\n"); break;
    case DISPOSEOFUNINITALIZEDPOINTER:       printf("Dispose of uninitalized pointer\n"); break;
    case DISPOSEOFNILPOINTER:                printf("Dispose of nil pointer\n"); break;
    case BADPOINTERVALUE:                    printf("Bad pointer value\n"); break;
    case BLOCKALREADYFREED:                  printf("Block already freed\n"); break;
    case INVALIDSTANDARDPROCEDUREORFUNCTION: printf("Invalid standard procedure or function\n"); break;
    case INVALIDINSTRUCTION:                 printf("Invalid instruction\n"); break;
    case NEWDISPOSETAGSMISMATCH:             printf("New dispose tags mismatch\n"); break;
    case PCOUTOFRANGE:                       printf("Pc out of range\n"); break;
    case STOREOVERFLOW:                      printf("Store overflow\n"); break;
    case STACKBALANCE:                       printf("Stack balance\n"); break;
    case SETINCLUSION:                       printf("Set inclusion\n"); break;
    case UNINITIALIZEDPOINTER:               printf("Uninitialized pointer\n"); break;
    case DEREFERENCEOFNILPOINTER:            printf("Dereference of nil pointer\n"); break;
    case POINTERUSEDAFTERDISPOSE:            printf("Pointer used after dispose\n"); break;
    case VARIANTNOTACTIVE:                   printf("Variant not active\n"); break;
    case INVALIDCASE:                        printf("Invalid case\n"); break;
    case SYSTEMERROR:                        printf("System error\n"); break;
    case CHANGETOALLOCATEDTAGFIELD:          printf("Change to allocated tag field\n"); break;
    case UNHANDLEDEXCEPTION:                 printf("Unhandled exception\n"); break;
    case PROGRAMCODEASSERTION:               printf("Program code assertion\n"); break;
    case VARLISTEMPTY:                       printf("VAR block list empty\n"); break;
    case CHANGETOVARREFERENCEDVARIANT:       printf("Change to VAR referenced variant\n"); break;
    case DISPOSEOFVARREFERENCEDBLOCK:        printf("Dispose of VAR referenced block\n"); break;
    case VARREFERENCEDFILEBUFFERMODIFIED:    printf("VAR referenced file buffer modified\n"); break;
    case CONTAINERMISMATCH:                  printf("Container length(s) do not match\n"); break;
    case INVALIDCONTAINERLEVEL:              printf("InvalidContainerLevel\n"); break;
    case DISPOSEOFWITHREFERENCEDBLOCK:       printf("Dispose of with referenced block\n"); break;
    case WITHBASELISTEMPTY:                  printf("With base list empty\n"); break;
    case EXTERNALSNOTENABLED:                printf("Externals not enabled"); break;
  }
  finish(1);
}

void errorm(address ea)
{
  /* check is a standard exception */
  if (ea-pctop >= EXCEPTIONBASE &&
      ea-pctop <= EXCEPTIONTOP) errorv(ea-pctop);
  else errorv(UNHANDLEDEXCEPTION);
}

/* get bit from defined array */
boolean getdef(address a)
{
    if (dochkdef) return (!!((storedef[(a)/8])&(1<<(a)%8)));
    else return (TRUE);
}

void putdef(address a, boolean b)
{
    if (dochkdef) b?((storedef[(a)/8]) |= 
        (1<<(a)%8)):((storedef[(a)/8]) &= ~(1<<(a)%8));
}

void putswt(address s, address e, boolean b)
{
    if (dochkdef) { long i; for (i = s; i <= e; i++) putdef(i, b); }
}

void chkdef(address a)
{
    if (dochkdef) getdef(a)?0:errorv(UNDEFINEDLOCATIONACCESS);
}

/* Command line processing */

void getcommandline(int argc, char* argv[], cmdbuf cb, cmdnum* l)
{
    cmdinx i;
    long x;
    char *p;

    for (i = 0; i < MAXCMD; i++) cb[i] = ' '; i = 0;
    while (argc) { /* walk input parameters and concatenate */
        p = *argv;
        while (*p) {
            if (i >= MAXCMD) {
                fprintf(stderr,
                        "*** Too many/too long command line parameters\n");
                finish(1);
            }
            cb[i++] = *p++;
        }
        argc--; argv++; /* advance */
        if (argc) { /* still more */
            if (i >= MAXCMD) {
                fprintf(stderr,
                        "*** Too many/too long command line parameters\n");
                finish(1);
            }
            cb[i++] = ' ';
        }
    }
    *l = i;
}

/*--------------------------------------------------------------------*/

/* "fileofy" routines for command line processing.

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
*/

char bufcommand(void)
{ return (cmdlin[cmdpos]); }

void getcommand(void)
{ if (cmdpos <= cmdlen+1) cmdpos = cmdpos+1; }

boolean eofcommand(void)
{ return (cmdpos > cmdlen+1); }

boolean eolncommand(void)
{ return (cmdpos >= cmdlen+1); }

void readlncommand(void)
{ cmdpos = MAXCMD; }


/*
  Parse options from command line.
*/
void paroptions(void)

{

    char optst[10+1]; 
    int  oni; 
    int  oi; 
    char ch1;

    do {
        while (!eolncommand() && !eofcommand() && bufcommand() == ' ') getcommand();
        if (bufcommand() == '-') {
            getcommand();
            if (bufcommand() == '-') getcommand();
            if (!isalpha(bufcommand())) {
                fprintf(stderr, "*** No valid option found");
                finish(1);
            }
            oni = 0;
            while (isalpha(bufcommand())) {
                ch1 = tolower(bufcommand()); 
                if (oni < OPTLEN) optst[oni++] = ch1; 
                getcommand();
            }
            optst[oni] = 0;
            oi = 0;
            while (oi < MAXOPT-1 && strcmp(optst, opts[oi]) && strcmp(optst, optsl[oi])) 
                oi = oi+1;
            if (!strcmp(optst, opts[oi]) || !strcmp(optst, optsl[oi])) {
                option[oi] = TRUE; if (bufcommand() == '-') option[oi] = FALSE;
                if (bufcommand() == '+' || bufcommand() == '-') getcommand();
                switch (oi+1) {
                    case 7:  dodmplab   = option[oi];
                    case 8:  dosrclin   = option[oi];
                    case 14: dorecycl   = option[oi];
                    case 15: dochkovf   = option[oi];
                    case 16: dochkrpt   = option[oi];
                    case 13: donorecpar = option[oi];
                    case 17: dochkdef   = option[oi];
                    case 19: iso7185    = option[oi];
                    case 23: dodebug    = option[oi];
                    case 1:  dodbgflt   = option[oi];
                    case 6:  dodbgsrc   = option[oi];
                    case 5:  dodckout   = option[oi];
                    case 9:  dochkvbk   = option[oi];
                }
            }
        }
        while (!eolncommand() && !eofcommand() && bufcommand() == ' ') getcommand();
    } while (bufcommand() == '-');
}

/* The external assigns read a filename off the command line. The original name
  of the header file is also passed in, and can be used to process. However,
  this implementation ignores them and just reads the names in order off the
  command line (as provided for in Annex C of the Pascaline standard).

  The processing of command line filenames does not exclude the use of the
  command file. The command file simply starts reading after all filename
  parameters have been removed.

*/

void assignexternal(filnum fn, char hfn[])
{
    long i;

    /* skip leading spaces */
    while (!eolncommand() && !eofcommand() && bufcommand() == ' ') getcommand();
    i = 0;
    while (!eolncommand() && !eofcommand() && bufcommand() != ' ') {
        if (i >= FILLEN) errorv(FILENAMETOOLONG);
        filnamtab[fn][i] = bufcommand();
        getcommand();
        i = i+1;
    }
    if (i >= FILLEN) errorv(FILENAMETOOLONG);
    if (i == 0) errorv(FILENAMEEMPTY);
    filnamtab[fn][i] = 0; /* terminate */
}

/*--------------------------------------------------------------------*/

/* Accessor functions

  These translate store variables to internal, and convert to and from store RAM
  formats.

  The acessors are fairly machine independent, they rely here on the machine
  being byte addressable. The endian format is inherent to the machine.

  The exception are the get/put int8,16,32,64 and 128 bit routines, which are
  dependent on the endian mode of the machine.

*/

#define getint(a) (chkdef(a), (*((long*)(store+(a)))))
#define putint(a, x) { *((long*)(store+(a))) = x; putswt(a, (a)+INTSIZE-1, TRUE); }

#define getrel(a) (chkdef(a), *((double*)(store+(a))))
#define putrel(a, f) do { *((double*)(store+(a))) = f; putswt(a, (a)+REALSIZE-1, TRUE); } while(0)

#define getbol(a) (chkdef(a), store[a])
#define putbol(a, b) do { store[a] = b; putdef(a, TRUE); } while(0)

#define getchr(a) (chkdef(a), store[a])
#define putchr(a, c) do { store[a] = c; putdef(a, TRUE); } while(0)

#define getbyt(a) (chkdef(a), store[a])
#define putbyt(a, b) do { store[a] = b; putdef(a, TRUE); } while(0)

#define getadr(a) (chkdef(a), (*((address*)(store+(a)))))
#define putadr(a, ad) do { *((address*)(store+(a))) = ad; putswt(a, (a)+ADRSIZE-1, TRUE); } while(0)

void getset(address a, settype s)

{
    long i;

    chkdef(a);
    for (i = 0; i < SETSIZE; i++) s[i] = store[a+i];
}

void putset(address a, settype s)

{
   long i;

   for (i = 0; i < SETSIZE; i++) store[a+i] = s[i];
   putswt(a, a+SETSIZE-1, TRUE);
}

/* Swap pointer on top with second on stack. The size of the second is given. */

void swpstk(address l)

{
    byte sb[MAXSIZE];
    address p;
    long i;

   /* get the top pointer */
   p = getadr(sp);
   /* load up the second on stack */
   for (i = 0; i < l; i++) sb[i] = store[sp+ADRSIZE+i];
   putadr(sp+l, p); /* place pointer at bottom */
   for (i = 0; i < l; i++) {
     store[sp+i] = sb[i]; /* place second as new top */
     putdef(sp+i, TRUE);
   }
}

/* end of accessor functions */

/*--------------------------------------------------------------------*/

/* external routines */

#ifdef EXTERNALS
#include "externals.inc"
#endif

/*--------------------------------------------------------------------*/

/* Push/pop

  These routines handle both the data type, and their lengths on the stack.

*/

#define popint(i) do { i = getint(sp); sp = sp+INTSIZE; } while(0)
#define pshint(i) do { sp = sp-INTSIZE; putint(sp, i); } while(0)
#define poprel(r) do { r = getrel(sp); sp = sp+REALSIZE; } while(0)
#define pshrel(r) do { sp = sp-REALSIZE; putrel(sp, r); } while(0)
#define popset(s) do { getset(sp, s); sp = sp+SETSIZE; } while(0)
#define pshset(s) do { sp = sp-SETSIZE; putset(sp, s); } while(0)
#define popadr(a) do { a = getadr(sp); sp = sp+ADRSIZE; } while(0)
#define pshadr(a) do { sp = sp-ADRSIZE; putadr(sp, a); } while(0)

/*--------------------------------------------------------------------*/

/* set operations */

/* print set, diagnostic */
void prtset(settype s)
{
    long i;

    for (i = 0; i < SETSIZE; i++)
        if (!!(s[i/8] & 1<<i%8)) printf("1"); else printf("0");
}

void sset(settype s, long b)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s[i] = 0;
    s[b/8] |= 1<<b%8;
}

void rset(settype s, long b1, long b2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s[i] = 0;
    if (b1 > b2) { i = b1; b1 = b2; b2 = i; }
    for (i = b1; i <= b2; i++) s[i/8] |= 1<<i%8;
}

void suni(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s1[i] = s1[i] | s2[i];
}

void sint(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s1[i] = s1[i] & s2[i];
}

void sdif(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s1[i] = s1[i] & ~s2[i];
}

boolean sisin(long i, settype s)
{
    return (!!(s[i/8] & 1<<i%8));
}

boolean sequ(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) if (s1[i] != s2[i]) return (FALSE);
    return (TRUE);
}

boolean sinc(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++)
        if ((s1[i] & s2[i]) != s2[i]) return (FALSE);
    return (TRUE);
}

/* throw an exception by vector */
void errore(long ei)
{
    address ad;

    if (expadr == 0) errorm(pctop+ei); /* no surrounding frame, throw system */
    mp = expmrk; sp = expstk; pc = expadr; popadr(ad); pshadr(pctop+ei);
    ep = getadr(mp+MARKET); /* get the mark ep */
}

/* align address, upwards */

void alignu(address algn, address* flc)
{
    long l;
    l = *flc-1;
    *flc = l+algn-(algn+l)%algn;
} /*align*/

/* align address, upwards */

void alignd(address algn, address* flc)
{
    long l;
    if ((*flc%algn) != 0) {
      l = *flc+1;
      *flc = l-algn+(algn-l)%algn;
    }
} /*align*/

/* clear filename string */

void clrfn(filnam fn)
{ long i; for (i = 0; i < FILLEN; i++) fn[i] = ' '; }

/*--------------------------------------------------------------------*/

/* load code file */

void errorl(void) /*error in loading*/
{ printf("\n*** Invalid code deck\n"); finish(1); }

void load(FILE* fp)

{
    address ad, ad2;
    long i, l, cs, csc, b;
    long c;

    ad = 0; l = 1;
    while (l > 0 && (c = fgetc(fp)) != EOF) {
        if (c != ':') errorl();
        fscanf(fp, "%2lx%16lx", &l, &i); ad2 = i;
        if (ad != ad2 && l > 0) errorl();
        cs = 0;
        for (i = 1; i <= l; i++) {
            fscanf(fp, "%2lx", &b); putbyt(ad, b); cs = (cs+b)%256;
            ad = ad+1;
        };
        fscanf(fp, "%2lx\n", &csc); if (cs != csc) errorl();
    }
    pctop = ad;
    /* uncomment for program code dump */
    /* 
    printf("core:\n");
    for (ad =0; ad < 0x100; ad++) {
        if (!(ad%16)) printf("\n");
        printf("%02x ", getbyt(ad));
    }
    printf("\n\n");
    */
} /*load*/

/*------------------------------------------------------------------------*/

/* runtime handlers */

void varenter(address s, address e)

{
    varptr vp;

    if (varfre) { vp = varfre; varfre = vp->next; }
    else vp = (varptr) malloc(sizeof(varblk));
    vp->s = s; vp->e = e; vp->next = varlst; varlst = vp;
}

void varexit(void)
{
    varptr vp;

    if (!varlst) errorv(VARLISTEMPTY);
    vp = varlst; varlst = vp->next; vp->next = varfre; varfre = vp;
}

long varinc(address s, address e)
{
    varptr vp;
    long f;

    vp = varlst; f = FALSE;
    while (vp && !f) {
        f = (vp->s >= s && vp->e <= e);
        vp = vp->next;
    }

    return (f);
}

void withenter(address b)
{
    wthptr wp;

    if (wthfre) { wp = wthfre; wthfre = wp->next; }
    else wp = (wthptr) malloc(sizeof(wthblk));
    wp->b = b; wp->next = wthlst; wthlst = wp;
    wthcnt++;
}

void withexit(void)
{
    wthptr wp;

    if (!wthlst) errorv(WITHBASELISTEMPTY);
    wp = wthlst; wthlst = wp->next; wp->next = wthfre; wthfre = wp;
    wthcnt--;
}

boolean withsch(address b)
{
    wthptr  wp;
    boolean f;

    wp = wthlst; f = FALSE;
    while (wp && !f) {
        f = wp->b == b;
        wp = wp->next;
    }

    return (f);
}

void compare(boolean* b, address* a1, address* a2)
/*comparing is only correct if result by comparing integers will be*/
{
    long i;

    i = 0; *b = TRUE;
    while (*b && i<q) {
        chkdef(*a1+i); chkdef(*a2+i);
        if (store[*a1+i] == store[*a2+i]) i = i+1;
        else *b = FALSE;
    }
    if (i == q) i = i-1; /* point at last location */
    *a1 = *a1+i; *a2 = *a2+i;
} /*compare*/

void valfil(address fa) /* attach file to file entry */
{
    long i,ff;

    if (store[fa] == 0) { /* no file */
        if (fa == pctop+INPUTOFF) ff = INPUTFN;
        else if (fa == pctop+OUTPUTOFF) ff = OUTPUTFN;
        else if (fa == pctop+PRDOFF) ff = PRDFN;
        else if (fa == pctop+PRROFF) ff = PRRFN;
        else if (fa == pctop+ERROROFF) ff = ERRORFN;
        else if (fa == pctop+LISTOFF) ff = LISTFN;
        else if (fa == pctop+COMMANDOFF) ff = COMMANDFN;
        else {
            i = COMMANDFN+1; /* start search after the header files */
            ff = 0;
            while (i <= MAXFIL) {
              if (filstate[i] == fsnone)
                { ff = i; filstate[i] = fsclosed; i = MAXFIL+1; }
              else i = i+1;
            }
            if (ff == 0) errore(TOOMANYFILES);
        }
        store[fa] = ff; putdef(fa, TRUE);
    }
}

void valfilwm(address fa) /* validate file write mode */
{
    valfil(fa); /* validate file address */
    if (filstate[store[fa]] != fswrite) errore(FILEMODEINCORRECT);
}

void valfilrm(address fa) /* validate file read mode */
{
    valfil(fa); /* validate file address */
    if (filstate[store[fa]] != fsread) errore(FILEMODEINCORRECT);
}

/* get opcode */
#define getop() do { op = store[pc]; pc = pc+1; } while(0)
/* get p parameter */
#define getp() do { p = store[pc]; pc = pc+1; } while(0)
/* get q parameter */
#define getq() do { q = getadr(pc); pc = pc+ADRSIZE; } while(0)
/* get q1 parameter */
#define getq1() do { q1 = getadr(pc); pc = pc+ADRSIZE; } while(0)
/* get q2 parameter */
#define getq2() do { q2 = getadr(pc); pc = pc+ADRSIZE; } while(0)

/*

   Blocks in the heap are dead simple. The block begins with a length, including
   the length itself. If the length is positive, the block is free. If negative,
   the block is allocated. This means that AddressOfBLock+abs(lengthOfBlock) is
   address of the next block, and RequestedSize <= LengthOfBLock+adrsize is a
   reasonable test for if a free block fits the requested size, since it will
   never be true of occupied blocks.

*/

/* dump block structure on heap */

void dmpblk(void)
{
    address l, blk, c;

    printf("\n");
    printf("Blocks in heap:\n");
    printf("\n");
    c = 1;
    blk = gbtop; /* set to bottom of heap */
    while (blk < np) { /* search blocks in heap */
        l = getadr(blk); /* get length */
        printf("%ld: Addr: %08lx Len: %08lx Occ: %d\n", c, blk, labs(l), l < 0);
        c++;
        if (labs(l) < HEAPAL || labs(l) > np) errorv(HEAPFORMATINVALID);
        blk = blk+labs(l); /* go next block */
    }
    printf("\n");
}

/* find free block using length */

void fndfre(address len, address* blk)
{
    address l, b;

    b = 0; /* set no block found */
    *blk = gbtop; /* set to bottom of heap */
    while (*blk < np) { /* search blocks in heap */
        l = getadr(*blk); /* get length */
        if (labs(l) < HEAPAL || *blk+labs(l) > np) errorv(HEAPFORMATINVALID);
        if (l >= len+ADRSIZE) { b = *blk; *blk = np; } /* found */
        else *blk = *blk+labs(l); /* go next block */
    }
    if (b > 0) { /* block was found */
        putadr(b, -l); /* allocate block */
        *blk = b+ADRSIZE; /* set base address */
        if (l > len+ADRSIZE+ADRSIZE+RESSPC) {
            /* If there is enough room for the block, header, and another
               header, then a reserve factor if desired. */
            putadr(b, -(len+ADRSIZE)); /* allocate block */
            b = b+len+ADRSIZE; /* go to top of allocated block */
            putadr(b, l-(len+ADRSIZE)); /* set length of stub space */
        }
    } else *blk = 0; /* set no block found */
}

/* coalesce space in heap */

void cscspc(void)
{
    boolean done;
    address ad, ad1, l, l1;

   /* first, colapse all free blocks at the heap top */
   l = 0;
   while (l >= 0 && np > gbtop) {
        /* find last entry */
        ad = gbtop;
        while (ad < np) { ad1 = ad; ad = ad+labs(getadr(ad)); }
        l = getadr(ad1); /* get header length */
        if (l >= 0) np = ad1; /* release to free space */
    }
    /* now, walk up and collapse adjacent free blocks */
    ad = gbtop; /* index bottom */
    while (ad < np) {
        l = getadr(ad); /* get header length */
        if (l >= 0) { /* free */
            ad1 = ad+l; /* index next block */
            if (ad1 < np) { /* not against end */
                l1 = getadr(ad1); /* get length next */
                if (l1 >=0) /* both blocks are free, combine the blocks */
                    putadr(ad, l+l1);
                else ad = ad+l+labs(l1); /* skip both blocks */
            } else ad = ad1; /* skip to end, done */
        } else ad = ad+labs(l); /* this block is not free, skip it */
    }
}

/* allocate space in heap */

void newspc(address len, address* blk)
{
    address ad,ad1;

    alignu(ADRSIZE, &len); /* align to units of address */
    fndfre(len, blk); /* try finding an existing free block */
    if (*blk == 0) { /* allocate from heap top */
        ad = np; /* save base of new block */
        np = np+(len+ADRSIZE); /* find new heap top */
        ad1 = np; /* save address */
        alignu(HEAPAL, &np); /* align to arena */
        len = len+(np-ad1); /* adjust length upwards for alignment */
        if (np > ep) errore(SPACEALLOCATEFAIL);
        putadr(ad, -(len+ADRSIZE)); /* allocate block */
        *blk = ad+ADRSIZE; /* index start of block */
    }
    /* clear block and set undefined */
    for (ad = *blk; ad < *blk+len; ad++) { store[ad] = 0; putdef(ad, FALSE); }
}

/* dispose of space in heap */

void dspspc(address len, address blk)
{
    address ad;

   if (blk == 0) errorv(DISPOSEOFUNINITALIZEDPOINTER);
   else if (blk == NILVAL) errorv(DISPOSEOFNILPOINTER);
   else if (blk < gbtop || blk >= np) errorv(BADPOINTERVALUE);
   ad = blk-ADRSIZE; /* index header */
   if (getadr(ad) >= 0) errorv(BLOCKALREADYFREED);
   if (dorecycl && !dochkrpt && !donorecpar) { /* obey recycling requests */
        putadr(ad, labs(getadr(ad))); /* set block free */
        cscspc(); /* coalesce free space */
   } else if (dochkrpt || donorecpar) { /* perform special recycle */
        /* check can break off top block */
        len = labs(getadr(ad)); /* get length */
        if (len >= ADRSIZE*2) {

            if (donorecpar) putadr(ad+ADRSIZE, -(labs(getadr(ad))-ADRSIZE));
            else putadr(ad+ADRSIZE, labs(getadr(ad))-ADRSIZE);

        }
        /* the "marker" is a block with a single address. Since it can't
           hold more than that, it will never be reused */
        putadr(ad, ADRSIZE); /* indicate freed but fixed block */
    }
}

/* check pointer indexes free entry */

boolean isfree(address blk)
{   return (getadr(blk-ADRSIZE) == ADRSIZE); }


/* system routine call */

boolean eoffile(FILE* fp)
{ long c; c = fgetc(fp); if (c != EOF) ungetc(c, fp); return (c == EOF); }

boolean eolnfile(FILE* fp)
{ long c; c = fgetc(fp); if (c != EOF) ungetc(c, fp); return (c == '\n'); }

char chkfile(FILE* fp)
{
    long c;
    c = fgetc(fp); if (c != EOF) ungetc(c, fp);
    return ((c=='\n'||c==EOF)?' ':c);
}

long lengthfile(FILE* fp)
{
    long s, p;

    s = ftell(fp); fseek(fp, 0, SEEK_END);
    p = ftell(fp); fseek(fp, s, SEEK_SET);

    return (p);
}

char buffn(filnum fn)
{
    long c;

    if (fn <= COMMANDFN) switch(fn) {
        case INPUTFN:   c = chkfile(stdin); break;
        case PRDFN:     c = chkfile(filtable[PRDFN]); break;
        case OUTPUTFN: case PRRFN: case ERRORFN:
        case LISTFN:    errore(READONWRITEONLYFILE); break;
        case COMMANDFN: c = bufcommand(); break;
    } else {
        if (filstate[fn] != fsread) errore(FILEMODEINCORRECT);
        c = chkfile(filtable[fn]);
    }

    return (c);
}

void getfneoln(FILE* fp, filnum fn)
{
    long c;

    c = fgetc(fp);
    if (c == EOF && !fileoln[fn]) fileoln[fn] = TRUE;
    else fileoln[fn] = c == '\n';
    if (c != EOF) filbof[fn] = FALSE;
}
void getfn(filnum fn)
{
    if (fn <= COMMANDFN) switch (fn) {
        case INPUTFN:   getfneoln(stdin, INPUTFN); break;
        case PRDFN:     getfneoln(filtable[PRDFN], PRDFN); break;
        case OUTPUTFN: case PRRFN: case ERRORFN:
        case LISTFN:    errore(READONWRITEONLYFILE); break;
        case COMMANDFN: getcommand(); break;
    } else {
        if (filstate[fn] != fsread) errore(FILEMODEINCORRECT);
        getfneoln(filtable[fn], fn);
    }
}

boolean chkeoffn(FILE* fp, filnum fn)
{
    if (fn == INPUTFN) {
        if ((eoffile(fp) && fileoln[fn]) || filbof[fn]) return (TRUE);
        else return (FALSE);
    } else {
        if (filstate[fn] == fswrite)
            return ftell(filtable[fn]) >= lengthfile(filtable[fn]);
        else if (filstate[fn] == fsread) {
            if ((eoffile(filtable[fn]) && fileoln[fn]) || filbof[fn])
                return (TRUE);
            else return (FALSE);
        } else { errore(FILENOTOPEN); return (FALSE); }
    }
}

boolean eoffn(filnum fn)
{
    boolean eof;

    if (fn <= COMMANDFN) switch (fn) {
        case INPUTFN:   eof = chkeoffn(stdin, INPUTFN); break;
        case OUTPUTFN:  eof = TRUE; break;
        case PRDFN:     eof = chkeoffn(filtable[PRDFN], PRDFN); break;
        case PRRFN:     eof = chkeoffn(filtable[PRRFN], PRRFN); break;
        case ERRORFN:   eof = TRUE; break;
        case LISTFN:    eof = TRUE; break;
        case COMMANDFN: eof = eofcommand(); break;
    } else eof = chkeoffn(filtable[fn], fn);

    return (eof);
}

boolean chkeolnfn(FILE* fp, filnum fn)
{
    if ((eoffile(fp) && !fileoln[fn]) && !filbof[fn]) return (TRUE);
    else return (eolnfile(fp));
}

boolean eolnfn(filnum fn)
{
    boolean eoln;

    if (fn <= COMMANDFN) switch (fn) {
        case INPUTFN:   eoln = chkeolnfn(stdin, INPUTFN); break;
        case PRDFN:     eoln = chkeolnfn(filtable[PRDFN], PRDFN); break;
        case PRRFN:     eoln = chkeolnfn(filtable[PRRFN], PRRFN); break;
        case ERRORFN: case OUTPUTFN:
        case LISTFN:    errore(FILEMODEINCORRECT); break;
        case COMMANDFN: eoln = eolncommand(); break;
    } else {
        if (filstate[fn] == fsclosed) errore(FILENOTOPEN);
        eoln = chkeolnfn(filtable[fn], fn);
    }

    return (eoln);
}

void readline(filnum fn)
{
    while (!eolnfn(fn)) {
        if (eoffn(fn)) errore(ENDOFFILE);
        getfn(fn);
    }
    if (eolnfn(fn)) getfn(fn);
}

char chkbuf(filnum fn, long w)
{ if (w > 0) return buffn(fn); else return(' '); }

boolean chkend(filnum fn, long w)
{ return (w == 0 || eoffn(fn)); }

void getbuf(filnum fn, long* w)
{
  if (*w > 0) {
    if (eoffn(fn)) errore(ENDOFFILE);
    getfn(fn); *w = *w-1;
  }
}

int valchr(char c, long r)
{
  c = tolower(c);
  return (
    (((c >= '0' && c <= '9') || c == '_') && (r == 10)) ||
    (((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || c == '_') && (r == 16)) ||
    (((c >= '0' && c <= '1') || c == '_') && (r == 2)) ||
    (((c >= '0' && c <= '7') || c == '_') && (r == 8))
  );
} 

void readi(filnum fn, long *i, long* w, boolean fld, int r)
{
    long s;
    long d;

   s = +1; /* set sign */
   /* skip leading spaces */
   while (chkbuf(fn, *w) == ' ' && !chkend(fn, *w)) getbuf(fn, w);
   if (!(chkbuf(fn, *w) == '+' || chkbuf(fn, *w) == '-' ||
         chkbuf(fn, *w) == '$' || chkbuf(fn, *w) == '&' || 
         chkbuf(fn, *w) == '%' ||
         valchr(chkbuf(fn, *w),r))) errore(INVALIDINTEGERFORMAT);
   if (chkbuf(fn, *w) == '$') { getbuf(fn, w); r = 16; }
   else if (chkbuf(fn, *w) == '&') { getbuf(fn, w); r = 8; }
   else if (chkbuf(fn, *w) == '%') { getbuf(fn, w); r = 2; }
   else if (chkbuf(fn, *w) == '+') getbuf(fn, w);
   else if (chkbuf(fn, *w) == '-') { getbuf(fn, w); s = -1; }
   if (!(valchr(chkbuf(fn, *w),r)))
     errore(INVALIDINTEGERFORMAT);
   *i = 0; /* clear initial value */
   while (valchr(chkbuf(fn, *w),r)) { /* parse digit */
     while (chkbuf(fn, *w) == '_' && !chkend(fn, *w)) getbuf(fn, w);
     if (valchr(chkbuf(fn, *w),r)) {
       if (isdigit(chkbuf(fn, *w))) d = chkbuf(fn, *w)-'0';
       else d = tolower(chkbuf(fn, *w))-'a'+10;
       if (*i > LONG_MAX/r ||
           *i == LONG_MAX/r && d > LONG_MAX%r)
         errore(INTEGERVALUEOVERFLOW);
       *i = *i*r+d; /* add in new digit */
       getbuf(fn, w);
     }
   }
   *i = *i*s; /* place sign */
   /* if fielded, validate the rest of the field is blank */
   if (fld) while (!chkend(fn, *w)) {
     if (chkbuf(fn, *w) != ' ') errore(FIELDNOTBLANK);
     getbuf(fn, w);
   }
}

/* find power of ten */
double pwrten(long e)
{
    double t; /* accumulator */
    double p; /* current power */

   p = 1.0e+1; /* set 1st power */
   t = 1.0; /* initalize result */
   do {
      if (e&1) t = t*p; /* if bit set, add this power */
      e = e / 2; /* index next bit */
      p = p*p; /* find next power */
   } while (e != 0);
   return (t);
}

void readr(filnum fn, double* r, long w, boolean fld)
{
    long i; /* integer holding */
    long e; /* exponent */
    long d; /* digit */
    boolean s; /* sign */

   e = 0; /* clear exponent */
   s = FALSE; /* set sign */
   *r = 0.0; /* clear result */
   /* skip leading spaces */
   while (chkbuf(fn, w) == ' ' && !chkend(fn, w)) getbuf(fn, &w);
   /* get any sign from number */
   if (chkbuf(fn, w) == '-') { getbuf(fn, &w); s = TRUE; }
   else if (chkbuf(fn, w) == '+') getbuf(fn, &w);
   if (!(isdigit(chkbuf(fn, w)))) errore(INVALIDREALNUMBER);
   while (isdigit(chkbuf(fn, w))) { /* parse digit */
      d = chkbuf(fn, w)-'0';
      *r = *r*10+d; /* add in new digit */
      getbuf(fn, &w);
   }
   if (chkbuf(fn, w) == '.' || tolower(chkbuf(fn, w)) == 'e') { /* it's a real */
      if (chkbuf(fn, w) == '.') { /* decimal point */
         getbuf(fn, &w); /* skip '.' */
         if (!(isdigit(chkbuf(fn, w)))) errore(INVALIDREALNUMBER);
         while (isdigit(chkbuf(fn, w))) { /* parse digit */
            d = chkbuf(fn, w)-'0';
            *r = *r*10+d; /* add in new digit */
            getbuf(fn, &w);
            e = e-1; /* count off right of decimal */
         }
      }
      if (tolower(chkbuf(fn, w)) == 'e') { /* exponent */
         getbuf(fn, &w); /* skip 'e' */
         if (!(isdigit(chkbuf(fn, w)) || chkbuf(fn, w) == '+' ||
               chkbuf(fn, w) == '-'))
            errore(INVALIDREALNUMBER);
         readi(fn, &i, &w, fld, 10); /* get exponent */
         /* find with exponent */
         e = e+i;
      }
      if (e < 0) *r = *r/pwrten(e); else *r = *r*pwrten(e);
   }
   if (s) *r = -*r;
   /* if fielded, validate the rest of the field is blank */
   if (fld) while (!chkend(fn, w)) {
     if (chkbuf(fn, w) != ' ') errore(FIELDNOTBLANK);
     getbuf(fn, &w);
   }
}

void readc(filnum fn, char* c, long w, boolean fld)
{
   *c = chkbuf(fn, w); getbuf(fn, &w);
   /* if fielded, validate the rest of the field is blank */
   if (fld) while (!chkend(fn, w)) {
     if (chkbuf(fn, w) != ' ') errore(FIELDNOTBLANK);
     getbuf(fn, &w);
   }
} /*readc*/

void reads(filnum fn, address ad, long l, long w, boolean fld)
{
  long c;
  if (w < 0) { w = labs(w); if (w < l) l = w;
    while (l > 0) {
      c = chkbuf(fn, w); getbuf(fn, &w); putchr(ad, c); ad = ad+1; l = l-1;
    }
    /* if fielded, validate the rest of the field is blank */
    if (fld) while (!chkend(fn, w)) {
      if (chkbuf(fn, w) != ' ') errore(FIELDNOTBLANK);
      getbuf(fn, &w);
    }
  } else { if (w < l) l = w;
    if (fld) while (w > l) {
      if (chkbuf(fn, w) != ' ') errore(FIELDNOTBLANK);
      getbuf(fn, &w);
    }
    while (l > 0) {
      c = chkbuf(fn, w); getbuf(fn, &w); putchr(ad, c); ad = ad+1; l = l-1;
    }
  }
} /*reads*/

void readsp(filnum fn, address ad,  long l)
{
  char c;

  while (l > 0 && !eolnfn(fn)) {
    if (eoffn(fn)) errore(ENDOFFILE);
    c = fgetc(filtable[fn]); putchr(ad, c); ad = ad+1; l = l-1;
  }
  while (l > 0) { putchr(ad, ' '); ad = ad+1; l = l-1; }
}

void readsc(filnum fn, address ad,  long l)
{
  char c;

  while (l > 0 && !eolnfn(fn)) {
    if (eoffn(fn)) errore(ENDOFFILE);
    c = fgetc(filtable[fn]); 
    if (c != getchr(ad)) errore(READCHARACTERMISMATCH);
    ad = ad+1; l = l-1;
  }
}

void writestrp(FILE* f, address ad, long l)
{
    long i;
    address ad1;

    ad1 = ad+l-1; /* find end */
    while (l > 0 && getchr(ad1) == ' ')
           { ad1 = ad1-1; l = l-1; }
    for (i = 0; i < l; i++) fprintf(f, "%c", getchr(ad+i));
}

void filllz(FILE* f, long n)
{ while (n > 0) { fputc('0', f); n--; } }

/* Write integer */
void writei(FILE* f, long w, long fl, long r, long lz)
{
    long i, d, ds;
    char digit[MAXDBF];
    boolean sgn;

    if (w < 0) {
        sgn = TRUE; w = labs(w);
        if (r != 10) errore(NONDECIMALRADIXOFNEGATIVE) ;
    } else sgn = FALSE;
    for (i = 0; i < MAXDBF; i++) digit[i] = ' ';
    i = MAXDBF-1; d = 0;
    do {
        if (w % r < 10) digit[i] = w % r+'0';
        else digit[i] = w % r -10 +'a';
        w = w / r; i = i-1; d = d+1;
    } while (w != 0);
    if (sgn) ds = d+1; else ds = d; /* add sign */
    if (ds > labs(fl)) { if (fl < 0) fl = -ds; else fl = ds; }
    if (fl > 0 && fl > ds)
      { if (lz) filllz(f, fl-ds); else fprintf(f, "%*c", (int)(fl-ds), ' '); }
    if (sgn) fputc('-', f);
    for (i = MAXDBF-d; i < MAXDBF; i++) fputc(digit[i], f);
    if (fl < 1 && labs(fl) > ds) fprintf(f, "%*c", (int)(labs(fl)-ds), ' ');
}

void writeb(FILE* f, boolean b, long w)
{
    long l;

    if (b) {
        l = 4; if (l > w) l = w; /* limit string to field */
        fprintf(f, "%*.*s", (int)w, (int)l, "true");
    } else {
        l = 5; if (l > w) l = w; /* limit string to field */
        fprintf(f, "%*.*s", (int)w, (int)l, "false");
    }
}

void putfile(FILE* f, address ad, filnum fn)
{
    if (!filbuff[fn]) errore(FILEBUFFERVARIABLEUNDEFINED);
    fputc(getchr(ad+FILEIDSIZE), f);
    filbuff[fn] = FALSE;
} /*putfile*/

void resetfn(filnum fn, boolean bin)
{
    /* file was closed, no assigned name, give it a temp name */
    if (filstate[fn] == fsclosed && !filanamtab[fn]) tmpnam(filnamtab[fn]);
    if (filstate[fn] != fsclosed && filstate[fn] != fsnone)
        if (fclose(filtable[fn])) errore(FILECLOSEFAIL);
    if (!(filtable[fn] = fopen(filnamtab[fn], bin?"rb":"r")))
        errore(FILEOPENFAIL);
    filstate[fn] = fsread;
    filbuff[fn] = FALSE;
    fileoln[fn] = FALSE;
    filbof[fn] = FALSE;
}

void rewritefn(filnum fn, boolean bin)
{
    /* file was closed, no assigned name, give it a temp name */
    if (filstate[fn] == fsclosed && !filanamtab[fn]) tmpnam(filnamtab[fn]);
    if (filstate[fn] != fsclosed && filstate[fn] != fsnone)
        if (fclose(filtable[fn])) errore(FILECLOSEFAIL);
    if (!(filtable[fn] = fopen(filnamtab[fn], bin?"wb":"w")))
        errore(FILEOPENFAIL);
    filstate[fn] = fswrite;
    filbuff[fn] = FALSE;
}

void callsp(void)

{
    boolean line;
    long i, j, k, w, l, f;
    unsigned char c;
    boolean b;
    address ad,ad1,ad2;
    double r, r1;
    filnum fn;
    long mn,mx;
    filnam fl1, fl2;
    long rd;
    boolean lz;
    boolean fld;
    FILE* fp;
    int rx;

    /* system routine call trace diagnostic */
    /*
    printf("callsp: q: %ld\n", q);
    */

    if (q > MAXSP) errorv(INVALIDSTANDARDPROCEDUREORFUNCTION);

    switch (q) {

    case 0 /*get*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (varinc(ad+FILEIDSIZE, ad+FILEIDSIZE))
                          errorv(VARREFERENCEDFILEBUFFERMODIFIED);
                    getfn(fn); break;
    case 1 /*put*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                       case OUTPUTFN: putfile(stdout, ad, fn); break;
                       case PRRFN: putfile(filtable[PRRFN], ad, fn); break;
                       case ERRORFN: putfile( stdout, ad, fn); break;
                       case LISTFN: putfile(stdout, ad, fn); break;
                       case INPUTFN: case PRDFN:
                       case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                    } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         putfile(filtable[fn], ad, fn);
                    }
                    break;
    case 3 /*rln*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                       case INPUTFN: readline(INPUTFN); break;
                       case PRDFN: readline(PRDFN); break;
                       case OUTPUTFN: prrfn: errorfn:
                       case LISTFN: errore(READONWRITEONLYFILE); break;
                       case COMMANDFN: readlncommand(); break;
                    } else {
                         if (filstate[fn] != fsread) errore(FILEMODEINCORRECT);
                         readline(fn);
                    }
                    break;
    case 4 /*new*/: popadr(ad1); newspc(ad1, &ad);
                    /*top of stack gives the length in units of storage */
                    popadr(ad1); putadr(ad1, ad);
                    break;
    case 39 /*nwl*/: /* size of record, size of f const list */
                     popadr(ad1); popint(i);
                     l = 0; if (i == 0 && donorecpar) l = 1;
                     /* alloc record, size of list, number in list */
                     newspc(ad1+(i+l+1)*INTSIZE, &ad);
                     ad1 = ad+(i+l)*INTSIZE; putint(ad1, i+ADRSIZE+1);
                     k = i; /* save n tags for later */
                     while (k > 0)
                     {
                        ad1 = ad1-INTSIZE; popint(j);
                        putint(ad1, j); k = k-1;
                     }
                     /* get pointer to dest var, place base above taglist and
                        list of fixed consts */
                     popadr(ad1); putadr(ad1, ad+(i+l+1)*INTSIZE);
                     break;
    case 5 /*wln*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                       case OUTPUTFN: fprintf(stdout, "\n"); break;
                       case PRRFN: fprintf(filtable[PRRFN], "\n"); break;
                       case ERRORFN: fprintf( stdout, "\n"); break;
                       case LISTFN: fprintf(stdout, "\n"); break;
                       case PRDFN: case INPUTFN:
                       case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                    } else {
                       if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                       fprintf(filtable[fn], "\n");
                    }
                    break;
    case 6 /*wrs*/: popint(w); popadr(ad1); popint(l);
                    popadr(ad); valfil(ad); fn = store[ad];
                    if (w < 1 && iso7185) errore(INVALIDFIELDSPECIFICATION);
                    if (l > labs(w)) l = labs(w); /* limit string to field */
                    if (fn <= COMMANDFN) switch (fn) {
                       case OUTPUTFN: fprintf(stdout, "%*.*s", (int)w, (int)l,
                                              (char*)(store+ad1)); break;
                       case PRRFN: fprintf(filtable[PRRFN], "%*.*s", (int)w, (int)l,
                                           (char*)(store+ad1)); break;
                       case ERRORFN: fprintf( stdout, "%*.*s", (int)w, (int)l,
                                             (char*)(store+ad1));
                                     break;
                       case LISTFN: fprintf(stdout, "%*.*s", (int)w, (int)l,
                                            (char*)(store+ad1));
                                    break;
                       case PRDFN: case INPUTFN:
                       case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                    } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         fprintf(filtable[fn], "%*.*s", (int)w, (int)l,
                                 (char*)(store+ad1));
                    }
                    break;
    case 65 /*wrsp*/: popadr(ad1); popint(l); popadr(ad);
                    valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                      case OUTPUTFN: writestrp(stdout, ad1, l); break;
                      case PRRFN: writestrp(filtable[PRRFN], ad1, l); break;
                      case ERRORFN: writestrp( stdout, ad1, l); break;
                      case LISTFN: writestrp(stdout, ad1, l); break;
                      case PRDFN: case INPUTFN:
                      case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                    } else {
                      if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                      writestrp(filtable[fn], ad1, l);
                    }
                    break;
    case 41 /*eof*/: popadr(ad); valfil(ad); fn = store[ad];
                    pshint(eoffn(fn));
                    break;
    case 42 /*efb*/:
                 popadr(ad); valfilrm(ad); fn = store[ad];
                 if (filstate[fn] == fswrite) pshint(TRUE);
                 else if (filstate[fn] == fsread)
                   pshint(eoffile(filtable[fn]) && !filbuff[fn]);
                 break;
    case 7 /*eln*/: popadr(ad); valfil(ad); fn = store[ad];
                    pshint(eolnfn(fn));
                    break;
    case 8 /*wri*/:
    case 62 /*wrih*/:
    case 63 /*wrio*/:
    case 64 /*wrib*/:
    case 66 /*wiz*/:
    case 67 /*wizh*/:
    case 68 /*wizo*/:
    case 69 /*wizb*/: popint(w); popint(i); popadr(ad);
                     rd = 10;
                     if (q == 62 || q == 67) rd = 16;
                     else if (q == 63 || q == 68) rd = 8;
                     else if (q == 64 || q == 69) rd = 2;
                     lz = q >= 66 && q <= 69;
                     valfil(ad); fn = store[ad];
                     if (w < 1 && iso7185) errore(INVALIDFIELDSPECIFICATION);
                     if (fn <= COMMANDFN) switch (fn) {
                          case OUTPUTFN: writei(stdout, i, w, rd, lz); break;
                          case PRRFN: writei(filtable[PRRFN], i, w, rd, lz); break;
                          case ERRORFN: writei( stdout, i, w, rd, lz); break;
                          case LISTFN: writei(stdout, i, w, rd, lz); break;
                          case PRDFN: case INPUTFN:
                          case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                     } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         writei(filtable[fn], i, w, rd, lz);
                     }
                     break;
    case 9 /*wrr*/: popint(w); poprel(r); popadr(ad);
                     valfil(ad); fn = store[ad];
                     if (w < 1) errore(INVALIDFIELDSPECIFICATION);
                     if (w < REALEF) w = REALEF; /* set minimum width */
                     l = w-REALEF+1; /* assign leftover to fractional digits w/o sign */
                     if (fn <= COMMANDFN) switch (fn) {
                          case OUTPUTFN: fprintf(stdout, "%*.*e", (int)w, (int)l, r); break;
                          case PRRFN: fprintf(filtable[PRRFN], "%*.*e", (int)w, (int)l, r); break;
                          case ERRORFN: fprintf( stdout, "%*.*e", (int)w, (int)l, r); break;
                          case LISTFN: fprintf(stdout, "%*.*e", (int)w, (int)l, r); break;
                          case PRDFN: case INPUTFN:
                          case COMMANDFN: errore(WRITEONREADONLYFILE);
                     } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         fprintf(filtable[fn], "%*.*e", (int)w, (int)l, r);
                     };
                     break;
    case 10/*wrc*/: popint(w); popint(i); c = i; popadr(ad);
                     valfil(ad); fn = store[ad];
                     if (w < 1 && iso7185) errore(INVALIDFIELDSPECIFICATION);
                     if (fn <= COMMANDFN) switch (fn) {
                          case OUTPUTFN: fprintf(stdout, "%*c", (int)w, c); break;
                          case PRRFN: fprintf(filtable[PRRFN], "%*c", (int)w, c); break;
                          case ERRORFN: fprintf( stdout, "%*c", (int)w, c); break;
                          case LISTFN: fprintf(stdout, "%*c", (int)w, c); break;
                          case PRDFN: case INPUTFN:
                          case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                     } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         fprintf(filtable[fn], "%*c", (int)w, c);
                     }
                     break;
    case 11/*rdi*/:
    case 72/*rdif*/:
    case 82/*rdx*/:
    case 83/*rdxf*/: 
    case 87/*rdih*/:
    case 88/*rdio*/:
    case 89/*rdib*/:
    case 90/*rifh*/:
    case 91/*rifo*/:
    case 92/*rifb*/:
    case 99/*rdxh*/:
    case 100/*rdxo*/:
    case 101/*rdxb*/:
    case 102/*rxfh*/:
    case 103/*rxfo*/:
    case 104/*rxfb*/: w = LONG_MAX; 
                     fld = q == 72||q == 83||q == 90|| q == 91||q == 92||
                           q == 102||q == 103||q == 104; 
                     rx = 10;
                     if (q == 87||q == 90||q == 99||q == 102) rx = 16;
                     else if (q == 88||q == 91||q == 100||q == 103) rx = 8;
                     else if (q == 89||q == 92||q == 101||q == 104) rx = 2;
                     if (fld) popint(w);
                     popadr(ad1); popadr(ad);
                     valfil(ad); fn = store[ad]; readi(fn, &i, &w, fld, rx);
                     if (q == 82||q == 83||q == 99||q == 100||q == 101||
                         q == 102||q == 103|| q == 104) {
                       if (i < 0||i > 255) errore(VALUEOUTOFRANGE);
                       putbyt(ad1, i);
                     } else putint(ad1, i);
                     break;
    case 37/*rib*/:
    case 71/*ribf*/:
    case 84/*rxb*/:
    case 85/*rxbf*/: 
    case 93/*ribh*/:
    case 94/*ribo*/:
    case 95/*ribb*/:
    case 96/*rbfh*/:
    case 97/*rbfo*/:
    case 98/*rbfb*/:
    case 105/*rxbh*/:
    case 106/*rxbo*/:
    case 107/*rxbb*/:
    case 108/*rbxh*/:
    case 109/*rbxo*/:
    case 110/*rbxb*/: w = LONG_MAX; 
                    fld = q == 71||q == 85||q == 96||q == 97||q == 98||
                          q == 108||q == 109||q == 110; 
                    rx = 10;
                    if (q == 93||q == 96||q == 105||q == 108) rx = 16;
                    else if (q == 94||q == 97||q == 106||q == 109) rx = 8;
                    else if (q == 95||q == 98||q == 107||q == 110) rx = 2;
                    popint(mx); popint(mn);
                    if (fld) popint(w); popadr(ad1); popadr(ad);
                    valfil(ad); fn = store[ad];
                    readi(fn, &i, &w, fld, rx);
                    if (i < mn || i > mx) errore(VALUEOUTOFRANGE);
                    /* note: value should be in byte range */
                    if (q == 82||q == 83||q == 105||q == 106||q == 107||
                        q == 108||q == 109||q == 110) {
                      if (i < 0||i > 255) errore(VALUEOUTOFRANGE);
                      putbyt(ad1, i);
                    } else putint(ad1, i);
                    break;
    case 12/*rdr*/:
    case 73/*rdrf*/: w = LONG_MAX; fld = q == 73; if (fld) popint(w);
                    popadr(ad1); popadr(ad);
                    valfil(ad); fn = store[ad];
                    readr(fn, &r, w, fld); putrel(ad1, r);
                    break;
    case 13/*rdc*/:
    case 75/*rdcf*/: w = LONG_MAX; fld = q == 75; if (fld) popint(w);
                    popadr(ad1); popadr(ad);
                    valfil(ad); fn = store[ad];
                    readc(fn, (char*)&c, w, fld); putchr(ad1, c);
                    break;
    case 38/*rcb*/:
    case 74/*rcbf*/: w = LONG_MAX; fld = q == 74; popint(mx); popint(mn);
                     if (fld) popint(w); popadr(ad1); popadr(ad);
                     valfil(ad);
                     fn = store[ad];
                     readc(fn, (char*)&c, w, fld);
                     if (c < mn || c > mx) errore(VALUEOUTOFRANGE);
                     putchr(ad1, c);
                     break;
    case 14/*sin*/: poprel(r1); pshrel(sin(r1)); break;
    case 15/*cos*/: poprel(r1); pshrel(cos(r1)); break;
    case 16/*exp*/: poprel(r1); pshrel(exp(r1)); break;
    case 17/*log*/: poprel(r1); if (r1 <= 0) errore(INVALIDARGUMENTTOLN);
                    pshrel(log(r1)); break;
    case 18/*sqt*/: poprel(r1); if (r1 < 0) errore(INVALIDARGUMENTTOSQRT);
                    pshrel(sqrt(r1)); break;
    case 19/*atn*/: poprel(r1); pshrel(atan(r1)); break;
    /* placeholder for "mark" */
    case 20/*sav*/: errorv(INVALIDSTANDARDPROCEDUREORFUNCTION);
    case 21/*pag*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                         case OUTPUTFN: fprintf(stdout, "\f"); break;
                         case PRRFN: fprintf(filtable[PRRFN], "\f"); break;
                         case ERRORFN: fprintf( stdout, "\f"); break;
                         case LISTFN: fprintf(stdout, "\f"); break;
                         case PRDFN: case INPUTFN:
                         case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                    } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         fprintf(filtable[fn], "\f");
                    }
                    break;
    case 22/*rsf*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                         case PRDFN: resetfn(PRDFN, FALSE); break;
                         case PRRFN: errore(CANNOTRESETWRITEONLYFILE); break;
                         case OUTPUTFN: case ERRORFN: case LISTFN: case INPUTFN:
                         case COMMANDFN:
                           errore(CANNOTRESETORREWRITESTANDARDFILE); break;
                    } else {
                         if (filstate[fn] == fsclosed && !filanamtab[fn])
                           errore(CANNOTRESETCLOSEDTEMPFILE);
                         resetfn(fn, FALSE);
                    }
                    break;
    case 23/*rwf*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                         case PRRFN: rewritefn(PRRFN, FALSE); break;
                         case PRDFN: errore(CANNOTREWRITEREADONLYFILE); break;
                         case ERRORFN: case LISTFN: case OUTPUTFN: case INPUTFN:
                         case COMMANDFN:
                           errore(CANNOTRESETORREWRITESTANDARDFILE); break;
                    } else rewritefn(fn, FALSE);
                    break;
    case 24/*wrb*/: popint(w); popint(i); b = i != 0; popadr(ad);
                     valfil(ad); fn = store[ad];
                     if (w < 1) errore(INVALIDFIELDSPECIFICATION);
                     if (fn <= COMMANDFN) switch (fn) {
                          case OUTPUTFN: writeb(stdout, b, w); break;
                          case PRRFN: writeb(filtable[PRRFN], b, w); break;
                          case ERRORFN: writeb( stdout, b, w); break;
                          case LISTFN: writeb(stdout, b, w); break;
                          case PRDFN: case INPUTFN:
                          case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                     } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         writeb(filtable[fn], b, w);
                     }
                     break;
    case 25/*wrf*/: popint(f); popint(w); poprel(r); popadr(ad);
                     valfil(ad); fn = store[ad];
                     if (w < 1 && iso7185) errore(INVALIDFIELDSPECIFICATION);
                     if (f < 1) errore(INVALIDFRACTIONSPECIFICATION);
                     if (fn <= COMMANDFN) switch (fn) {
                          case OUTPUTFN: fprintf(stdout, "%*.*f", (int)w, (int)f, r);
                                         break;
                          case PRRFN: fprintf(filtable[PRRFN], "%*.*f", (int)w, (int)f, r);
                                      break;
                          case ERRORFN: fprintf( stdout, "%*.*f", (int)w, (int)f, r);
                                        break;
                          case LISTFN: fprintf(stdout, "%*.*f", (int)w, (int)f, r); break;
                          case PRDFN: case INPUTFN:
                          case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                     } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         fprintf(filtable[fn], "%*.*f", (int)w, (int)f, r);
                     }
                     break;
    case 26/*dsp*/: popadr(ad1); popadr(ad);
                    if (varinc(ad, ad+ad1-1))
                      errorv(DISPOSEOFVARREFERENCEDBLOCK);
                    if (withsch(ad)) errorv(DISPOSEOFWITHREFERENCEDBLOCK);
                    dspspc(ad1, ad); break;
    case 40/*dsl*/: popadr(ad1); popint(i); /* get size of record and n tags */
                    /* add padding for zero tag case */
                    l = 0; if (i == 0 && donorecpar) l = 1;
                    ad = getadr(sp+i*INTSIZE); /* get rec addr */
                    j = i; /* save tag count */
                    /* under us is either the number of tags or the length of the block, if it
                      was freed. Either way, it is >= adrsize if not free */
                    if (getint(ad-INTSIZE) <= ADRSIZE)
                        errorv(BLOCKALREADYFREED);
                    if (i != getint(ad-INTSIZE)-ADRSIZE-1)
                        errorv(NEWDISPOSETAGSMISMATCH);
                    ad = ad-INTSIZE*2; ad2 = sp;
                    /* ad = top of tags in dynamic, ad2 = top of tags in
                      stack */
                    k = i;
                    while (k > 0)
                    {
                        if (getint(ad) != getint(ad2))
                            errorv(NEWDISPOSETAGSMISMATCH);
                        ad = ad-INTSIZE; ad2 = ad2+INTSIZE; k = k-1;
                    }
                    ad = ad+INTSIZE; ad1 = ad1+(i+1)*INTSIZE;
                    /* ajust for dummy */
                    ad = ad-(l*INTSIZE); ad1 = ad1+(l*INTSIZE);
                    if (varinc(ad, ad+ad1-1))
                      errorv(DISPOSEOFVARREFERENCEDBLOCK);
                    if (withsch(ad)) errorv(DISPOSEOFWITHREFERENCEDBLOCK);
                    dspspc(ad1, ad);
                    while (i > 0) { popint(j); i = i-1; };
                    popadr(ad);
                    if (donorecpar) {
                        /* This flag means we are going to keep the entry, even
                         * after disposal. We place a dummy tag below the
                         * pointer just to indicate the space was freed.
                         */
                         putadr(ad-ADRSIZE, ADRSIZE);
                    }
                    break;
    case 27/*wbf*/: popint(l); popadr(ad1); popadr(ad);
                    valfilwm(ad); fn = store[ad];
                    for (i = 0; i < l; i++) {
                       chkdef(ad1); fputc(store[ad1], filtable[fn]);
                       ad1 = ad1+1;
                    }
                    break;
    case 28/*wbi*/: popint(i); popadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < INTSIZE; i++)
                        fputc(store[sp+i], filtable[fn]);
                     popint(i);
                     break;
    case 45/*wbx*/: popint(i); popadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     fputc(store[sp], filtable[fn]); popint(i);
                     break;
    case 29/*wbr*/: poprel(r); popadr(ad); pshrel(r);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < REALSIZE; i++)
                        fputc(store[sp+i], filtable[fn]);
                     poprel(r);
                     break;
    case 30/*wbc*/: popint(i); c = i; popadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < CHARSIZE; i++)
                        fputc(store[sp+i], filtable[fn]);
                     popint(i);
                     break;
    case 31/*wbb*/: popint(i); popadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < BOOLSIZE; i++)
                         fputc(store[sp+i], filtable[fn]);
                     popint(i);
                     break;
    case 32/*rbf*/: popint(l); popadr(ad1); popadr(ad);
                     valfilrm(ad); fn = store[ad];
                     if (filbuff[fn]) /* buffer data exists */
                       for (i = 0; i < l; i++) {
                         store[ad1+i] = store[ad+FILEIDSIZE+i];
                         putdef(ad1+i, TRUE);
                       }
                     else
                       for (i = 0; i < l; i++) {
                         if (eoffile(filtable[fn])) errore(ENDOFFILE);
                         store[ad1] = fgetc(filtable[fn]);
                         putdef(ad1, TRUE);
                         ad1 = ad1+1;
                       }
                     break;
    case 33/*rsb*/: popadr(ad); valfil(ad); fn = store[ad]; 
                     if (filstate[fn] == fsclosed && !filanamtab[fn])
                       errore(CANNOTRESETCLOSEDTEMPFILE);
                     resetfn(fn, TRUE);
                    break;
    case 34/*rwb*/: popadr(ad); valfil(ad); fn = store[ad];
                    rewritefn(fn, TRUE); break;
    case 35/*gbf*/: popint(i); popadr(ad); valfilrm(ad);
                    fn = store[ad];
                    if (varinc(ad+FILEIDSIZE, ad+FILEIDSIZE+i-1))
                        errorv(VARREFERENCEDFILEBUFFERMODIFIED);
                    if (filbuff[fn]) filbuff[fn] = FALSE;
                    else
                      for (j = 0; j < i; j++)
                         store[ad+FILEIDSIZE+j] = fgetc(filtable[fn]);
                    break;
    case 36/*pbf*/: popint(i); popadr(ad); valfilwm(ad);
                 fn = store[ad];
                 if (!filbuff[fn]) errore(FILEBUFFERVARIABLEUNDEFINED);
                 for (j = 0; j < i; j++)
                   fputc(store[ad+FILEIDSIZE+j], filtable[fn]);
                 filbuff[fn] = FALSE;
                 break;
    case 43 /*fbv*/: popadr(ad); valfil(ad);
                   fn = store[ad];
                   if (fn == INPUTFN) putchr(ad+FILEIDSIZE, buffn(INPUTFN));
                   else if (fn == PRDFN) putchr(ad+FILEIDSIZE, buffn(PRDFN));
                   else if (fn == COMMANDFN) putchr(ad+FILEIDSIZE, bufcommand());
                   else {
                     if (filstate[fn] == fsread)
                       putchr(ad+FILEIDSIZE, buffn(fn));
                   }
                   filbuff[fn] = TRUE;
                   break;
    case 44 /*fvb*/: popint(i); popadr(ad); valfil(ad);
                   fn = store[ad];
                   /* load buffer only if in read mode, and buffer is
                     empty */
                   if (filstate[fn] == fsread && !filbuff[fn]) {
                       for (j = 0; j < i; j++) {
                         store[ad+FILEIDSIZE+j] = fgetc(filtable[fn]);
                         putdef(ad+FILEIDSIZE+j, TRUE);
                       }
                   }
                   filbuff[fn] = TRUE;
                   break;
    /* extended Pascaline file handlers */
    case 46 /*asst*/:
    case 56 /*assb*/: popint(i); popadr(ad1); popadr(ad); valfil(ad);
                  fn = store[ad];
                  for (j = 0; j < i; j++) {
                    if (j >= FILLEN) errore(FILENAMETOOLONG);
                    filnamtab[fn][j] = store[ad1+j];
                  }
                  if (j >= FILLEN) errore(FILENAMETOOLONG);
                  filnamtab[fn][j] = 0;
                  filanamtab[fn] = TRUE; /* set name assigned */
                  break;
    case 47 /*clst*/:
    case 57 /*clsb*/: popadr(ad); valfil(ad); fn = store[ad];
                if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                /* if the file is temp, remove now */
                if (!filanamtab[fn]) remove(filnamtab[fn]);
                filanamtab[fn] = FALSE; /* break any name association */
                filstate[fn] = fsclosed;
                break;
    case 48 /*pos*/: popint(i); popadr(ad); valfil(ad); fn = store[ad];
                if (i < 1) errore(INVALIDFILEPOSITION);
                if (fseek(filtable[fn], i-1, SEEK_SET)) errore(FILEPOSITIONFAIL);
                break;
    case 49 /*upd*/: popadr(ad); valfil(ad); fn = store[ad];
                if (filstate[fn] == fswrite) {
                  if (fseek(filtable[fn], 0, SEEK_SET)) errore(FILEPOSITIONFAIL);
                } else {
                  if (filstate[fn] == fsread)
                    if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                  if (!fopen(filnamtab[fn], "rb+")) errore(FILEOPENFAIL);
                }
                filstate[fn] = fswrite; filbuff[fn] = FALSE;
                break;
    case 50 /*appt*/: popadr(ad); valfil(ad); fn = store[ad];
                if (filstate[fn] == fswrite) {
                  if (fseek(filtable[fn], 0, SEEK_SET)) errore(FILEPOSITIONFAIL);
                } else {
                  if (filstate[fn] == fsread)
                    if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                  if (!fopen(filnamtab[fn], "r+")) errore(FILEOPENFAIL);
                  if (fseek(filtable[fn], 0, SEEK_END)) errore(FILEPOSITIONFAIL);
                }
                filstate[fn] = fswrite; filbuff[fn] = FALSE;
                break;
    case 58 /*appb*/: popadr(ad); valfil(ad); fn = store[ad];
                if (filstate[fn] == fswrite) {
                  if (fseek(filtable[fn], 0, SEEK_SET)) errore(FILEPOSITIONFAIL);
                } else {
                  if (filstate[fn] == fsread)
                    if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                  if (!fopen(filnamtab[fn], "rb+")) errore(FILEOPENFAIL);
                  if (fseek(filtable[fn], 0, SEEK_END)) errore(FILEPOSITIONFAIL);
                }
                filstate[fn] = fswrite; filbuff[fn] = FALSE;
                break;
    case 51 /*del*/: popint(i); popadr(ad1);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j]; fl1[j] = 0;
                  i = remove(fl1); if (i) errorv(FILEDELETEFAIL);
                  break;
    case 52 /*chg*/: popint(i); popadr(ad1); popint(l); popadr(ad);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j]; fl1[j] = 0;
                  for (j = 0; j < l; j++) fl2[j] = store[ad+j]; fl2[j] = 0;
                  if (rename(fl1, fl2)) errorv(FILENAMECHANGEFAIL);
                  break;
    case 53 /*len*/: popadr(ad); valfil(ad); fn = store[ad];
                pshint(lengthfile(filtable[fn]));
                break;
    case 54 /*loc*/: popadr(ad); valfil(ad); fn = store[ad];
                  if ((i = ftell(filtable[fn])) < 0) errorv(FILEPOSITIONFAIL);
                  pshint(i+1);
                  break;
    case 55 /*exs*/: popint(i); popadr(ad1);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j]; fl1[j] = 0;
                  if ((fp = fopen(fl1, "r"))) fclose(fp);
                  pshint(!!fp);
                  break;
    case 59 /*hlt*/: finish(1); break;
    case 60 /*ast*/: popint(i);
                  if (i == 0) errorv(PROGRAMCODEASSERTION);
                 break;
    case 61 /*asts*/: popint(i); popadr(ad); popint(j);
                  if (j == 0) errors(ad, i);
                  break;
    case 70/*rds*/:
    case 76/*rdsf*/: w = LONG_MAX; fld = q == 76;
                  if (fld) popint(w); popadr(ad1); popint(i);
                  popadr(ad); valfil(ad); fn = store[ad];
                  reads(fn, ad1, i, w, fld);
                  break;
    case 77/*rdsp*/: popadr(ad1); popint(i); popadr(ad);
                  valfil(ad); fn = store[ad];
                  readsp(fn, ad1, i);
                  break;
    case 86/*rdsc*/: popadr(ad1); popint(i); 
                  popadr(ad); valfil(ad); fn = store[ad];
                  readsc(fn, ad1, i);
                  break;
    case 78/*aeft*/: popint(i); popadr(ad1); popadr(ad); valfil(ad);
                  fn = store[ad]; clrfn(fl1);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j];
                  assignexternal(fn, fl1);
                  filanamtab[fn] = TRUE;
                  break;
    case 79/*aefb*/: popint(i); popadr(ad1); popadr(ad); valfil(ad);
                  fn = store[ad]; clrfn(fl1);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j];
                  assignexternal(fn, fl1);
                  filanamtab[fn] = TRUE;
                  break;
    case 80/*rdie*/: w = LONG_MAX; popint(i); popadr(ad1); popadr(ad);
                  readi(COMMANDFN, &i, &w, FALSE, 10); putint(ad, i);
                  break;
    case 81/*rdre*/: w = LONG_MAX; popint(i); popadr(ad1); popadr(ad);
                  readr(COMMANDFN, &r, w, FALSE); putrel(ad, r);
                  break;
    case 2/*thw*/: popadr(ad1); mp = expmrk; sp = expstk;
                  pc = expadr; popadr(ad2); pshadr(ad1);
                  ep = getadr(mp+MARKET); /* get the mark ep */
                  /* release to search vectors */
                  break;
    case 111/*sete*/: popint(exitcode); /* set exit code */
                  break;

    } /*case q*/
} /*callsp*/

void sinins()

{
    address ad,ad1,ad2,ad3,ad4; boolean b; long i,j,k,i1,i2; char c, c1; long i3,i4;
    double r1,r2; boolean b1,b2; settype s1,s2; address a1,a2,a3;

    /* instruction execution trace diagnostic */
    /*
    printf("%08lx/%08lx: %02x\n", pc, sp, store[pc]);
    */

    if (pc >= pctop) errorv(PCOUTOFRANGE);

    /* fetch instruction from byte store */
    getop();

    /*execute*/

    switch (op) {

    case pi_lodi: getp(); getq(); pshint(getint(getadr(mp-p*PTRSIZE) + q)); break;
    case pi_lodx: getp(); getq(); pshint(getbyt(getadr(mp-p*PTRSIZE) + q)); break;
    case pi_loda: getp(); getq(); pshadr(getadr(getadr(mp-p*PTRSIZE) + q)); break;
    case pi_lodr: getp(); getq(); pshrel(getrel(getadr(mp-p*PTRSIZE) + q)); break;
    case pi_lods: getp(); getq(); pshadr(getadr(mp-p*PTRSIZE) + q); break;
    case pi_lodb: getp(); getq(); pshint(getbol(getadr(mp-p*PTRSIZE) + q)); break;
    case pi_lodc: getp(); getq(); pshint(getchr(getadr(mp-p*PTRSIZE) + q)); break;

    case pi_ldoi: getq(); pshint(getint(q)); break;
    case pi_ldox: getq(); pshint(getbyt(q)); break;
    case pi_ldoa: getq(); pshadr(getadr(q)); break;
    case pi_ldor: getq(); pshrel(getrel(q)); break;
    case pi_ldos: getq(); pshadr(q); break;
    case pi_ldob: getq(); pshint(getbol(q)); break;
    case pi_ldoc: getq(); pshint(getchr(q)); break;

    case pi_stri: getp(); getq(); popint(i); putint(getadr(mp-p*PTRSIZE)+q, i); break;
    case pi_strx: getp(); getq(); popint(i); putbyt(getadr(mp-p*PTRSIZE)+q, i); break;
    case pi_stra: getp(); getq(); popadr(ad); putadr(getadr(mp-p*PTRSIZE)+q, ad); break;
    case pi_strr: getp(); getq(); poprel(r1); putrel(getadr(mp-p*PTRSIZE)+q, r1); break;
    case pi_strs: getp(); getq(); popadr(ad); getset(ad, s1); putset(getadr(mp-p*PTRSIZE)+q, s1); break;
    case pi_strb: getp(); getq(); popint(i1); b1 = i1 != 0;
                       putbol(getadr(mp-p*PTRSIZE)+q, b1); break;
    case pi_strc: getp(); getq(); popint(i1); c1 = i1;
                         putchr(getadr(mp-p*PTRSIZE)+q, c1); break;

    case pi_sroi: getq(); popint(i); putint(q, i); break;
    case pi_srox: getq(); popint(i); putbyt(q, i); break;
    case pi_sroa: getq(); popadr(ad); putadr(q, ad); break;
    case pi_sror: getq(); poprel(r1); putrel(q, r1); break;
    case pi_sros: getq(); popadr(ad); getset(ad, s1); putset(q, s1); break;
    case pi_srob: getq(); popint(i1); b1 = i1 != 0; putbol(q, b1); break;
    case pi_sroc: getq(); popint(i1); c1 = i1; putchr(q, c1); break;

    case pi_lda: getp(); getq(); pshadr(getadr(mp-p*PTRSIZE)+q); break;
    case pi_lao: getq(); pshadr(q); break;

    case pi_stoi: popint(i); popadr(ad); putint(ad, i); break;
    case pi_stox: popint(i); popadr(ad); putbyt(ad, i); break;
    case pi_stoa: popadr(ad1); popadr(ad); putadr(ad, ad1); break;
    case pi_stor: poprel(r1); popadr(ad); putrel(ad, r1); break;
    case pi_stos: popadr(ad); getset(ad, s1); popadr(ad1); putset(ad1, s1); break;
    case pi_stob: popint(i1); b1 = i1 != 0; popadr(ad); putbol(ad, b1);
                       break;
    case pi_stoc: popint(i1); c1 = i1; popadr(ad); putchr(ad, c1);
                       break;

    case pi_stom: getq(); getq1(); ad1 = getadr(sp+q1); ad2 = sp;
                    for (i = 0; i < q; i++) {
                      store[ad1+i] = store[ad2+i]; putdef(ad1+i, getdef(ad2+i));
                    }
                    sp = sp+q1+ADRSIZE;
                    break;
    case pi_ctb: getq(); getq1(); popadr(ad1); ad2 = sp;
                    for (i = 0; i < q; i++) {
                      store[ad1+i] = store[ad2+i]; putdef(ad1+i, getdef(ad2+i));
                    }
                    sp = sp+q1; pshadr(ad1);
                    break;
    case pi_sfs: getq(); getq1(); popadr(ad1); ad2 = sp;
                    for (i = 0; i < q; i++) {
                      store[ad1+i] = store[ad2+i]; putdef(ad1+i, getdef(ad2+i));
                    }
                    sp = sp+q1;
                    break;

    case pi_ldcc: pshint(getchr(pc)); pc = pc+1; break;
    case pi_ldcb: pshint(getbol(pc)); pc = pc+1; break;
    case pi_ldci: i = getint(pc); pc = pc+INTSIZE; pshint(i); break;
    case pi_ldcn: pshadr(NILVAL); break; /* load nil */
    case pi_ldcr: getq(); pshrel(getrel(q)); break;
    case pi_ldcs: getq(); pshadr(q); break;

    case pi_indi: getq(); popadr(ad); pshint(getint(ad+q)); break;
    case pi_indx: getq(); popadr(ad); pshint(getbyt(ad+q)); break;
    case pi_inda: getq(); popadr(ad); ad1 = getadr(ad+q); pshadr(ad1); break;
    case pi_indr: getq(); popadr(ad); pshrel(getrel(ad+q)); break;
    case pi_inds: getq(); popadr(ad); pshadr(ad+q); break;
    case pi_indb: getq(); popadr(ad); pshint(getbol(ad+q)); break;
    case pi_indc: getq(); popadr(ad); pshint(getchr(ad+q)); break;
    case pi_incb:
    case pi_incc:
    case pi_incx:
    case pi_inci: getq(); popint(i1);
                   if (dochkovf) if (i1<0 == q<0)
                      if (LONG_MAX-labs(i1) < labs(q))
                        errore(INTEGERVALUEOVERFLOW);
                   pshint(i1+q);
                   break;
    case pi_inca: getq(); popadr(a1); pshadr(a1+q); break;

    case pi_sfr: getq();
                 /* allocate function result as zeros */
                 for (j = 0; j < q/INTSIZE; j++) pshint(0);
                 /* set function result undefined */
                 for (j = 0; j < q; j++) putdef(sp+j, FALSE);
                 break;

    case pi_mst: getp(); getq(); getq1();
                  pshadr(0); /* place current ep */
                  pshadr(0); /* place bottom of stack */
                  pshadr(ep); /* previous ep */
                  pshadr(mp); /* save old mp on stack */
                  ad1 = mp; /* save old mp */
                  mp = sp; /* set new mp */
                  /* copy old display to stack */
                  for (i = 1; i <= p; i++) { ad1 = ad1-PTRSIZE; pshadr(getadr(ad1)); }
                  pshadr(mp); /* push new mp to complete display */
                  ad = sp-q; /* q = length of dataseg */
                  if (ad <= np) errorv(STOREOVERFLOW);
                  /* clear allocated memory and set undefined */
                  while (sp > ad) 
                    { sp = sp-1; store[sp] = 0; putdef(sp, FALSE); }
                  putadr(mp+MARKSB, sp); /* set bottom of stack */
                  ep = sp-q1; if (ep <= np) errorv(STOREOVERFLOW);
                  putadr(mp+MARKET, ep); /* place current ep */
                  break;

    case pi_cup:
    case pi_cuf: /* q=entry point */
                 getq(); pshadr(pc); pc = q;
                 break;

    case pi_cuv:
    case pi_cvf: /*q=vector entry point*/
                 getq(); pshadr(pc); pc = getadr(q);
                 break;

    case pi_suv: getq(); getq1(); putadr(q1, q); break;

    /* For characters and booleans, need to clean 8 bit results because
      only the lower 8 bits were stored to. */
    case pi_retc: getq();
                   ep = getadr(mp+MARKEP);
                   sp = mp; /* index old mark */
                   popadr(mp); /* restore old mp */
                   sp = sp+MARKSIZE; /* skip mark */
                   popadr(pc); /* load return address */
                   sp = sp+q; /* remove parameters */
                   /* clean result */
                   putint(sp, getchr(sp));
                   break;

    case pi_retb:  getq();
                   ep = getadr(mp+MARKEP);
                   sp = mp; /* index old mark */
                   popadr(mp); /* restore old mp */
                   sp = sp+MARKSIZE; /* skip mark */
                   popadr(pc); /* load return address */
                   sp = sp+q; /* remove parameters */
                   /* clean result */
                   putint(sp, getbol(sp));
                   break;
    case pi_retp:
    case pi_reti:
    case pi_retx:
    case pi_rets:
    case pi_retr:
    case pi_reta: getq();
                   ep = getadr(mp+MARKEP);
                   sp = mp; /* index old mark */
                   popadr(mp); /* restore old mp */
                   sp = sp+MARKSIZE; /* skip mark */
                   popadr(pc); /* load return address */
                   sp = sp+q; /* remove parameters */
                   break;

    case pi_retm: getq();
                   ep = getadr(mp+MARKEP);
                   sp = mp; /* index old mark */
                   popadr(mp); /* restore old mp */
                   sp = sp+MARKSIZE; /* skip mark */
                   popadr(pc); /* load return address */
                   sp = sp+q; /* remove parameters */
                   break;

    case pi_csp: q = store[pc]; pc = pc+1; callsp(); break;

    case pi_ixa: getq(); popint(i); popadr(a1); pshadr(q*i+a1); break;

    case pi_equa: popadr(a2); popadr(a1); pshint(a1==a2); break;
    case pi_equb:
    case pi_equc:
    case pi_equi: popint(i2); popint(i1); pshint(i1==i2); break;
    case pi_equr: poprel(r2); poprel(r1); pshint(r1==r2); break;
    case pi_equs: popadr(ad1); getset(ad1, s2); popadr(ad); getset(ad, s1);
                        pshint(sequ(s1,s2)); break;
    case pi_equm: getq(); popadr(a2); popadr(a1);
                         compare(&b, &a1, &a2); pshint(b); break;
    case pi_equv: popadr(a2); popint(i); q = i; popadr(a1); popint(i1);
                        compare(&b, &a1, &a2); pshint(b); break;

    case pi_neqa: popadr(a2); popadr(a1); pshint(a1!=a2); break;
    case pi_neqb:
    case pi_neqc:
    case pi_neqi: popint(i2); popint(i1); pshint(i1!=i2); break;
    case pi_neqr: poprel(r2); poprel(r1); pshint(r1!=r2); break;
    case pi_neqs: popadr(ad1); getset(ad1, s2); popadr(ad); getset(ad, s1);
                        pshint(!sequ(s1,s2)); break;
    case pi_neqm: getq(); popadr(a2); popadr(a1);
                         compare(&b, &a1, &a2); pshint(!b); break;
    case pi_neqv: popadr(a2); popint(i); q = i; popadr(a1); popint(i1);
                         compare(&b, &a1, &a2); pshint(!b); break;

    case pi_geqb:
    case pi_geqc:
    case pi_geqi: popint(i2); popint(i1); pshint(i1>=i2); break;
    case pi_geqr: poprel(r2); poprel(r1); pshint(r1>=r2); break;
    case pi_geqs: popadr(ad1); getset(ad1, s2); popadr(ad); getset(ad, s1);
                        pshint(sinc(s1,s2)); break;
    case pi_geqm: getq(); popadr(a2); popadr(a1);
                         compare(&b, &a1, &a2);
                         pshint(b || (store[a1] >= store[a2])); break;
    case pi_geqv: popadr(a2); popint(i); q = i; popadr(a1); popint(i1);
                         compare(&b, &a1, &a2);
                         pshint(b || store[a1] >= store[a2]); break;

    case pi_grtb:
    case pi_grtc:
    case pi_grti: popint(i2); popint(i1); pshint(i1>i2); break;
    case pi_grtr: poprel(r2); poprel(r1); pshint(r1>r2); break;
    case pi_grts: errorv(SETINCLUSION); break;
    case pi_grtm: getq(); popadr(a2); popadr(a1);
                         compare(&b, &a1, &a2);
                         pshint(!b && (store[a1] > store[a2])); break;
    case pi_grtv: popadr(a2); popint(i); q = i; popadr(a1); popint(i1);
                         compare(&b, &a1, &a2);
                         pshint(!b && store[a1] > store[a2]); break;

    case pi_leqb:
    case pi_leqc:
    case pi_leqi: popint(i2); popint(i1); pshint(i1<=i2); break;
    case pi_leqr: poprel(r2); poprel(r1); pshint(r1<=r2); break;
    case pi_leqs: popadr(ad1); getset(ad1, s2); popadr(ad); getset(ad, s1);
                        pshint(sinc(s2,s1)); break;
    case pi_leqm: getq(); popadr(a2); popadr(a1);
                         compare(&b, &a1, &a2);
                         pshint(b || (store[a1] <= store[a2])); break;
    case pi_leqv: popadr(a2); popint(i); q = i; popadr(a1); popint(i1);
                         compare(&b, &a1, &a2);
                         pshint(b || store[a1] <= store[a2]); break;

    case pi_lesb:
    case pi_lesc:
    case pi_lesi: popint(i2); popint(i1); pshint(i1<i2); break;
    case pi_lesr: poprel(r2); poprel(r1); pshint(r1<r2); break;
    case pi_less: errorv(SETINCLUSION); break;
    case pi_lesm: getq(); popadr(a2); popadr(a1);
                         compare(&b, &a1, &a2);
                         pshint(!b && (store[a1] < store[a2])); break;
    case pi_lesv: popadr(a2); popint(i); q = i; popadr(a1); popint(i1);
                         compare(&b, &a1, &a2);
                         pshint(!b && store[a1] < store[a2]); break;

    case pi_ujp: getq(); pc = q; break;
    case pi_fjp: getq(); popint(i); if (i == 0) pc = q; break;
    case pi_xjp: getq(); popint(i1); pc = i1*UJPLEN+q; break;

    case pi_chka:
    case pi_ckla: getq(); popadr(a1); pshadr(a1);
                       /*     0 = assign pointer including nil
                         Not 0 = assign pointer from heap address */
                       if (a1 == 0)
                          /* if zero, but not nil, it's never been assigned */
                          errorv(UNINITIALIZEDPOINTER);
                       else if (q != 0 && a1 == NILVAL)
                          /* q != 0 means deref, and it was nil
                            (which is not zero) */
                          errorv(DEREFERENCEOFNILPOINTER);
                       else if ((a1 < gbtop || a1 >= np) &&
                               (a1 != NILVAL))
                          /* outside heap space (which could have
                            contracted!) */
                          errorv(BADPOINTERVALUE);
                       else if ((dochkrpt || donorecpar) && a1 != NILVAL) {
                         /* perform use of freed space check */
                         if (isfree(a1))
                           /* attempt to dereference or assign a freed
                             block */
                           errorv(POINTERUSEDAFTERDISPOSE);
                       }
                       break;
    case pi_chks: getq(); popadr(ad); getset(ad, s1); pshadr(ad);
                      for (j = SETLOW; j <= getint(q)-1; j++)
                        if (sisin(j, s1)) errorv(SETELEMENTOUTOFRANGE);
                      for (j = getint(q+INTSIZE)+1; j <= SETHIGH; j++)
                        if (sisin(j, s1)) errorv(SETELEMENTOUTOFRANGE);
                      break;
    case pi_chkb:
    case pi_chkc:
    case pi_chkx:
    case pi_chki: getq(); popint(i1); pshint(i1);
                  if (i1 < getint(q) || i1 > getint(q+INTSIZE))
                    errore(VALUEOUTOFRANGE);
                  break;

    case pi_cks: pshint(0); break;
    case pi_ckvi:
    case pi_ckvx:
    case pi_ckvb:
    case pi_ckvc: getq(); popint(i2); popint(i1);
                    pshint(i1); pshint(i1 == q || i2 != 0);
                    break;;
    case pi_cke: popint(i2); popint(i1);
                    if (i2 == 0) errorv(VARIANTNOTACTIVE);
                    break;

    /* all the dups are defined, but not all used */
    case pi_dupb:
    case pi_dupc:
    case pi_dupi: popint(i1); pshint(i1); pshint(i1); break;
    case pi_dupa: popadr(a1); pshadr(a1); pshadr(a1); break;
    case pi_dupr: poprel(r1); pshrel(r1); pshrel(r1); break;
    case pi_dups: popadr(ad); pshadr(ad); pshadr(ad); break;

    case pi_inv: popadr(ad); putdef(ad, FALSE); break;

    case pi_adi: popint(i2); popint(i1);
                  if (dochkovf) if (i1<0 == i2<0)
                    if (LONG_MAX-labs(i1) < labs(i2)) errore(INTEGERVALUEOVERFLOW);
                  pshint(i1+i2); break;
    case pi_adr: poprel(r2); poprel(r1); pshrel(r1+r2); break;
    case pi_sbi: popint(i2); popint(i1);
                  if (dochkovf) if (i1<0 != i2<0)
                    if (LONG_MAX-labs(i1) < labs(i2)) errore(INTEGERVALUEOVERFLOW);
                  pshint(i1-i2); break;
    case pi_sbr: poprel(r2); poprel(r1); pshrel(r1-r2); break;
    case pi_sgs: popadr(ad); popint(i1); sset(s1, i1); putset(ad, s1);
                     pshadr(ad); break;
    case pi_flt: popint(i1); pshrel(i1); break;

    /* note that flo implies the tos is float as well */
    case pi_flo: poprel(r1); popint(i1); pshrel(i1); pshrel(r1); break;

    case pi_trc: poprel(r1);
                  if (dochkovf) 
                    if (r1 < (double)-LONG_MAX || r1 > (double)LONG_MAX)
                      errore(REALARGUMENTTOOLARGE);
                  pshint(trunc(r1)); break;
    case pi_ngi: popint(i1); pshint(-i1); break;
    case pi_ngr: poprel(r1); pshrel(-r1); break;
    case pi_sqi: popint(i1);
                if (dochkovf) if (i1 != 0)
                  if (labs(i1) > LONG_MAX/labs(i1)) errore(INTEGERVALUEOVERFLOW);
                pshint(i1*i1); break;
    case pi_sqr: poprel(r1); pshrel(r1*r1); break;
    case pi_abi: popint(i1); pshint(labs(i1)); break;
    case pi_abr: poprel(r1); pshrel(fabs(r1)); break;
    case pi_notb: popint(i1); b1 = i1 != 0; pshint(!b1); break;
    case pi_noti: popint(i1);
                      if (i1 < 0) errore(BOOLEANOPERATOROFNEGATIVE);
                      pshint(~i1 & LONG_MAX); break;
    case pi_and: popint(i2);
                      if (i2 < 0) errore(BOOLEANOPERATOROFNEGATIVE);
                      popint(i1);
                      if (i1 < 0) errore(BOOLEANOPERATOROFNEGATIVE);
                      pshint(i1 & i2); break;
    case pi_ior: popint(i2);
                      if (i2 < 0) errore(BOOLEANOPERATOROFNEGATIVE);
                      popint(i1);
                      if (i1 < 0) errore(BOOLEANOPERATOROFNEGATIVE);
                      pshint(i1 | i2); break;
    case pi_xor: popint(i2); b2 = i2 != 0;
                      if (i2 < 0) errore(BOOLEANOPERATOROFNEGATIVE);
                      popint(i1); b1 = i1 != 0;
                      if (i1 < 0) errore(BOOLEANOPERATOROFNEGATIVE);
                      pshint(i1 ^ i2); break;
    case pi_dif: popadr(ad); popadr(ad1); getset(ad1, s2);
                     popadr(ad1); getset(ad1, s1); sdif(s1, s2);
                     putset(ad, s1); pshadr(ad); break;
    case pi_int: popadr(ad); popadr(ad1); getset(ad1, s2);
                     popadr(ad1); getset(ad1, s1); sint(s1, s2);
                     putset(ad, s1); pshadr(ad); break;
    case pi_uni: popadr(ad); popadr(ad1); getset(ad1, s2);
                     popadr(ad1); getset(ad1, s1); suni(s1, s2);
                     putset(ad, s1); pshadr(ad);
                     break;
    case pi_inn: popadr(ad); getset(ad, s1); popint(i1); pshint(sisin(i1, s1)); break;
    case pi_mod: popint(i2); popint(i1);
                  if (dochkovf) if (i2 <= 0) errore(INVALIDDIVISORTOMOD);
                  pshint(i1 % i2); break;
    case pi_odd: popint(i1); pshint(i1&1); break;
    case pi_mpi: popint(i2); popint(i1);
                  if (dochkovf) if (i1 != 0 && i2 != 0)
                    if (labs(i1) > LONG_MAX / labs(i2))
                      errore(INTEGERVALUEOVERFLOW);
                  pshint(i1*i2); break;
    case pi_mpr: poprel(r2); poprel(r1); pshrel(r1*r2); break;
    case pi_dvi: popint(i2); popint(i1);
                      if (dochkovf) if (i2 == 0) errore(ZERODIVIDE);
                      pshint(i1/i2); break;
    case pi_dvr: poprel(r2); poprel(r1);
                      if (dochkovf) if (r2 == 0.0) errore(ZERODIVIDE);
                      pshrel(r1/r2); break;
    case pi_mov: getq(); popint(i2); popint(i1);
                 for (i3 = 0; i3 <= q-1; i3++)
                   { store[i1+i3] = store[i2+i3];
                         putdef(i1+i3, getdef(i2+i3)); };
                 /* q is a number of storage units */
                 break;
    case pi_lca: getq(); pshadr(q); break;

    case pi_decb:
    case pi_decc:
    case pi_decx:
    case pi_deci: getq(); popint(i1);
                    if (dochkovf) if (i1<0 != q<0)
                      if (LONG_MAX-labs(i1) < labs(q))
                        errore(INTEGERVALUEOVERFLOW);
                    pshint(i1-q); break;

    case pi_stp: stopins = TRUE; break;


    case pi_ujc: errorv(INVALIDCASE); break;
    case pi_rnd: poprel(r1);
                  if (dochkovf) 
                    if (r1 < -((double)LONG_MAX+0.5) || r1 > (double)LONG_MAX+0.5)
                      errore(REALARGUMENTTOOLARGE);
                  pshint(round(r1)); break;
    case pi_pck: getq(); getq1(); popadr(a3); popadr(a2); popadr(a1);
                 if (a2+q > q1) errore(PACKELEMENTSOUTOFBOUNDS);
                 for (i4 = 0; i4 <= q-1; i4++) { chkdef(a1+a2);
                    store[a3+i4] = store[a1+a2];
                    putdef(a3+i4, getdef(a1+a2));
                    a2 = a2+1;
                 }
                 break;
    case pi_upk: getq(); getq1(); popadr(a3); popadr(a2); popadr(a1);
                 if (a3+q > q1) errore(UNPACKELEMENTSOUTOFBOUNDS);
                 for (i4 = 0; i4 <= q-1; i4++) { chkdef(a1+i4);
                    store[a2+a3] = store[a1+i4];
                    putdef(a2+a3, getdef(a1+i4));
                    a3 = a3+1;
                 } break;

    case pi_rgs: popadr(ad); popint(i2); popint(i1); rset(s1, i1, i2);
                      putset(ad, s1); pshadr(ad);
                      break;
    case pi_ipj: getp(); getq(); pc = q;
                 mp = getadr(mp-p*PTRSIZE); /* index the mark to restore */
                 sp = getadr(mp+MARKSB); /* get the stack bottom */
                 ep = getadr(mp+MARKET); /* get the mark ep */
                 break;
    case pi_cip:
    case pi_cif: popadr(ad); ad1 = mp;
                mp = getadr(ad+1*PTRSIZE); pshadr(pc); pc = getadr(ad);
                break;
    case pi_rip: getq(); mp = getadr(sp+q); break;
    case pi_lpa: getp(); getq(); /* place procedure address on stack */
                pshadr(getadr(mp-p*PTRSIZE));
                pshadr(q);
                break;
    case pi_dmp: getq(); sp = sp+q; break; /* remove top of stack */

    case pi_swp: getq(); swpstk(q); break;

    case pi_tjp: getq(); popint(i); if (i != 0) pc = q; break;

    case pi_lip: getp(); getq(); ad = getadr(mp-p*PTRSIZE) + q;
                   ad1 = getadr(ad); ad2 = getadr(ad+1*PTRSIZE);
                   pshadr(ad2); pshadr(ad1);
                   break;

    case pi_cta: getq(); getq1(); getq2(); popint(i); popadr(ad); pshadr(ad);
                       pshint(i); ad = ad-q-INTSIZE; ad1 = getadr(ad);
                       if (ad1 < INTSIZE)
                         errorv(SYSTEMERROR);
                       ad1 = ad1-ADRSIZE-1;
                       if (ad1 >= q1) {
                         ad = ad-ad1*INTSIZE;
                         if (i < 0 || i >= getint(q2))
                           errorv(VALUEOUTOFRANGE);
                         if (getadr(ad+(q1-1)*INTSIZE) != getint(q2+(i+1)*INTSIZE))
                           errorv(CHANGETOALLOCATEDTAGFIELD);
                       }
                      break;

    case pi_ivti:
    case pi_ivtx:
    case pi_ivtb:
    case pi_ivtc: getq(); getq1(); getq2(); popint(i); popadr(ad);
                      pshadr(ad); pshint(i);
                      if (i < 0 || i >= getint(q2)) errorv(VALUEOUTOFRANGE);
                      if (dochkdef) {
                        b = getdef(ad);
                        if (b) {
                          if (op == pi_ivti) j = getint(ad); else j = getbyt(ad);
                          b = getint(q2+(i+1)*INTSIZE) !=
                              getint(q2+(j+1)*INTSIZE);
                        }
                        if (b) {
                          ad = ad+q;
                          for (j = 1; j <= q1; j++)
                            { putdef(ad, FALSE); ad = ad+1; }
                        }
                      }
                      break;

    case pi_cvbi:
    case pi_cvbx:
    case pi_cvbb:
    case pi_cvbc: getq(); getq1(); getq2(); popint(i); popadr(ad);
                      pshadr(ad); pshint(i);
                      if (i < 0 || i >= getint(q2)) errorv(VALUEOUTOFRANGE);
                      b = getdef(ad);
                      if (b) {
                        if (op == pi_cvbi) j = getint(ad); else j = getbyt(ad);
                        b = getint(q2+(i+1)*INTSIZE) !=
                            getint(q2+(j+1)*INTSIZE);
                      }
                      if (b) {
                        ad = ad+q;
                        if (varinc(ad, ad+q1-1))
                            errorv(CHANGETOVARREFERENCEDVARIANT);
                      }
                      break;

    case pi_wbs: popadr(ad); pshadr(ad); withenter(ad); break;
    case pi_wbe: withexit(); break;

    case pi_mrkl: getq(); srclin = q; break;

    case pi_bge: getq();
                   /* save current exception framing */
                   pshadr(expadr); pshadr(expstk); pshadr(expmrk);
                   pshadr(0); /* place dummy vector */
                   /* place new exception frame */
                   expadr = q; expstk = sp; expmrk = mp;
                   break;
    case pi_ede: popadr(a1); /* dispose vector */
                   /* restore previous exception frame */
                   popadr(expmrk); popadr(expstk); popadr(expadr);
                   break;
    case pi_mse: popadr(a1);
                   /* restore previous exception frame */
                   popadr(expmrk); popadr(expstk); popadr(expadr);
                   /* if there is no surrounding frame, handle fixed */
                   if (expadr == 0) errorm(a1);
                   else { /* throw to new frame */
                     mp = expmrk; sp = expstk; pc = expadr;
                     popadr(a2); pshadr(a1);
                     ep = getadr(mp+MARKET); /* get the mark ep */
                     /* release to search vectors */
                   }
                   break;
    case pi_sev: getp(); getq(); a1 = getadr(mp-p*PTRSIZE)+q;
                   popadr(ad); pshadr(ad); putadr(a1, ad);
                   break;
    case pi_cjp: getq(); getq1(); popint(i1); pshint(i1);
                  if (i1 >= getint(q) && i1 <= getint(q+INTSIZE))
                    { pc = q1; popint(i1); }
                  break;
    case pi_lnp: getq(); np = q; gbtop = np; ad = pctop;
                  /* clear global memory and set undefined */
                  while (np > ad)
                    { store[ad] = 0; putdef(ad, FALSE); ad = ad+1; }
                  break;
    case pi_cal: getq(); pshadr(pc); pc = q; break;
    case pi_ret: popadr(pc); break;
    case pi_vbs: getq(); popadr(ad); varenter(ad, ad+q-1); break;
    case pi_vbe: varexit(); break;
    case pi_brk: break; /* breaks are no-ops here */
    case pi_vis: getq(); getq1(); popadr(ad); ad1 = ad+q*INTSIZE;
                   for (i = 1; i <= q; i++) {
                     popint(i1); putint(ad1, i1); ad1 = ad1-INTSIZE; q1 = q1*i1;
                   }
                   popadr(ad1); sp = sp-q1; putadr(ad, sp); pshadr(ad1);
                   break;
    case pi_vip: getq(); getq1(); popadr(ad); ad1 = ad+q*INTSIZE;
                   for (i = 1; i <= q; i++) {
                     popint(i1); putint(ad1, i1); ad1 = ad1-INTSIZE; q1 = q1*i1;
                   }
                   newspc(q1, &ad2); putadr(ad1, ad2);
                   break;
    case pi_vin: getq(); getq1(); popadr(ad); ad2 = sp;
                   for (i = 1; i <= q; i++)
                     { q1 = q1*getint(ad2); ad2 = ad2+INTSIZE; }
                   newspc(q1+q*INTSIZE, &ad2); putadr(ad, ad2);
                   for (i = 1; i <= q; i++)
                     { popint(i1); putint(ad2, i1); ad2 = ad2+INTSIZE; }
                   break;
    case pi_lcp: popadr(ad); pshadr(ad+PTRSIZE); pshadr(getadr(ad)); break;
    case pi_cps: popadr(ad1); popint(i1); popadr(ad2); popint(i2);
                       pshint(i2); pshadr(ad2); pshint(i1); pshadr(ad1);
                       if (i1 != i2) errorv(CONTAINERMISMATCH);
                      break;
    case pi_cpc: getq(); popadr(ad1); popadr(ad2); popadr(ad3); popadr(ad4);
                       pshadr(ad4); pshadr(ad3); pshadr(ad2); pshadr(ad1);
                       for (i = 1; i <= q; i++) {
                         if (getint(ad2) != getint(ad4))
                           errorv(CONTAINERMISMATCH);
                         ad2 = ad2+PTRSIZE; ad4 = ad4+PTRSIZE;
                       }
                      break;

    case pi_aps: getq(); popadr(ad1); popadr(ad); popadr(ad); popadr(i1);
                       for (i = 0; i <= i1*q-1; i++) {
                         store[ad+i] = store[ad1+i]; putdef(ad+i, getdef(ad1+i));
                       }
                      break;
    case pi_apc: getq(); getq1(); popadr(ad1); popadr(ad); popadr(ad);
                       popadr(ad2);
                       for (i = 1; i <= q; i++)
                         { q1 = q1*getint(ad2); ad2 = ad2+INTSIZE; };
                       for (i = 0; i <= q1-1; i++) {
                         store[ad+i] = store[ad1+i]; putdef(ad+i, getdef(ad1+i));
                       }
                      break;
    case pi_cxs: getq(); popint(i); popadr(ad); popint(i1);
                       if (i < 1 || i > i1) errore(VALUEOUTOFRANGE);
                       pshadr(ad+(i-1)*q);
                      break;
    case pi_cxc: getq(); getq1(); popint(i); popadr(ad); popadr(ad1);
                       ad2 = ad1+PTRSIZE;
                       for (j = 1; j <= q-1; j++)
                         { q1 = q1*getint(ad2); ad2 = ad2+INTSIZE; }
                       if (i < 1 || i > getint(ad1))
                         errore(VALUEOUTOFRANGE);
                       pshadr(ad1+PTRSIZE); pshadr(ad+(i-1)*q1);
                       break;
    case pi_lft: getq(); popadr(ad); pshadr(q); pshadr(ad); break;
    case pi_max: getq(); popint(i); popadr(ad1);
                       if (q > 1) popadr(ad); else popint(i1);
                       if (i < 1 || i > q) errorv(INVALIDCONTAINERLEVEL);
                       if (q == 1) i = i1;
                       else i = getint(ad+(q-i)*INTSIZE);
                       pshint(i);
                      break;
    case pi_vdp:
    case pi_vdd: popadr(ad); dspspc(0, ad); break;
    case pi_spc: popadr(ad); popadr(ad1); pshint(getint(ad1)); pshadr(ad); break;
    case pi_ccs: getq(); getq1(); popadr(ad); popadr(ad1); ad3 = ad1;
                       if (q == 1) q1 = q1*ad1;
                       else for (i = 1; i <= q; i++)
                         { q1 = q1*getint(ad3); ad3 = ad3+INTSIZE; }
                       ad2 = sp-q1; alignd(STACKELSIZE, &ad2); sp = ad2;
                       for (i = 0; i <= q1-1; i++) {
                         store[ad2+i] = store[ad+i]; putdef(ad2+i, getdef(ad+i));
                       };
                       pshadr(ad1); pshadr(ad2);
                     break;
    case pi_scp: popadr(ad); popadr(ad1); popadr(ad2); putadr(ad2, ad);
                       putadr(ad2+PTRSIZE, ad1); break;
    case pi_ldp: popadr(ad); pshadr(getadr(ad+PTRSIZE));
                       pshadr(getadr(ad)); break;
    case pi_cpp: getq(); getq1(); ad = sp+q; sp = sp-q1; ad1 = sp;
                      for (i = 0; i < q1; i++) {
                        store[ad1] = store[ad]; putdef(ad1, getdef(ad));
                        ad = ad+1; ad1 = ad1+1;
                      }
                      break;
    case pi_cpr: getq(); getq1(); ad = sp+q+q1; ad1 = sp+q;
                      for (i = 0; i < q; i++) {
                        ad = ad-1; ad1 = ad1-1;
                        store[ad] = store[ad1]; putdef(ad, getdef(ad1));
                      }
                      sp = sp+q1;
                      break;
    case pi_lsa: getq(); ad = sp+q; pshadr(ad); break;
    case pi_cpl: popadr(ad1); popadr(ad2); pshadr(ad2); pshadr(ad1);
                      pshadr(ad2);
                      break;
    case pi_mdc: getq(); popint(i1); pshint(i1); pshint(i1+q);
                      break;

    case pi_eext:
#ifdef EXTERNALS
                    ExecuteExternal(pc-extvecbase);
                    /* set stack below function result, if any */
                    sp = mp;
                    {???fixme???}
                    {
                    pc = getadr(mp+MARKRA);
                    }
                    ep = getadr(mp+MARKEP);
                    {???fixme???}
                    {
                    mp = getadr(mp+MARKDL);
                    }
#else
                    errorv(EXTERNALSNOTENABLED);
#endif
                    break;

    /* illegal instructions */
    /* 173, 228, 229, 230, 231, 232, 233, 234, 248, 250,
       255 */
    default: errorv(INVALIDINSTRUCTION); break;

  }
}

int main (int argc, char *argv[])

{
    FILE* fp;
    address i;
    char fname[FILLEN];

    printf("P6 Pascal cmach interpreter vs. %d.%d", MAJORVER, MINORVER);
    if (EXPERIMENT) printf(".x");
    printf("\n");
    printf("\n");

    varlst = NULL; /* set no VAR block entries */
    varfre = NULL;
    wthlst = NULL; /* set no with block entries */
    wthcnt = 0;
    wthfre = NULL;
    exitcode = 0; /* clear program exit code */

    /* Set options. Note not all have internal equivalents. */
    dochkovf = DOCHKOVF; /* check arithmetic overflow */
    dodmplab = FALSE; /* dump label definitions */
    dotrcrot = FALSE; /* trace routine executions */
    dotrcins = FALSE; /* trace instruction executions */
    dosrclin = DOSRCLIN; /* add source line sets to code */
    dotrcsrc = FALSE; /* trace source line executions (requires dosrclin) */
    dorecycl = DORECYCL; /* obey heap space recycle requests */
    dodebug = FALSE; /* start up debug on entry */
    dodbgflt = FALSE; /* enter debug on fault */
    dodbgsrc = FALSE; /* do source file debugging */
    dochkrpt = DOCHKRPT; /* check reuse of freed entry (automatically */
    donorecpar = DONORECPAR; /* break returned blocks as occupied, not free */
    dochkdef = DOCHKDEF; /* check undefined accesses */
    dosrcprf = FALSE; /* do source level profiling */
    dochkcov = FALSE; /* do code coverage */
    doanalys = FALSE; /* do analyze */
    dodckout = FALSE; /* do output code deck */
    dochkvbk = FALSE; /* do check VAR blocks */
    iso7185 = FALSE; /* iso7185 standard flag */

    argc--; argv++; /* discard the program parameter */

    /* initialize file state */
    for (i = 1; i <= MAXFIL; i++) {
        filstate[i] = fsnone; /* never opened */
        filanamtab[i] = FALSE; /* no name assigned */
    }

    /* construct bit equivalence table */
    i = 1;
    for (bai = 0; bai <= 7; bai++) { bitmsk[bai] = i; i = i*2; }

    for (sdi = 0; sdi <= MAXDEF; sdi++)
      storedef[sdi] = 0; /* clear storage defined flags */

#ifndef PACKAGE
    if (argc < 1) {
        printf("*** Usage: cmach <codefile> [<params>]...\n");
        finish(1);
    }
    strcpy(fname, *argv);
    if (!strchr(fname, '.')) strcat(fname, ".p6o");
    fp = fopen(fname, "r");
    if (!fp) {
        printf("*** Cannot open file %s\n", fname);
        finish(1);
    }
    argv++; argc--; /* skip that parameter */
    printf("loading program\n");
#endif

#ifdef PCTOP
    pctop = PCTOP;
    for (i = 0; i < PCTOP; i++) putdef(i, TRUE);
#endif
    if (store[0] == 0) /* there is already a program in store */
        load(fp); /* assembles and stores code */

    /* set status of standard files */
    filstate[INPUTFN] = fsread;
    filstate[OUTPUTFN] = fswrite;
    filstate[ERRORFN] = fswrite;
    filstate[LISTFN] = fswrite;
    filstate[COMMANDFN] = fsread;

    /* get the command line */
    getcommandline(argc, argv, cmdlin, &cmdlen);
    cmdpos = 0;
    /* load command line options */
    paroptions();

    /* prep for the run */
    pc = 0; sp = MAXTOP; np = -1; mp = MAXTOP; ep = 5; srclin = 1;
    expadr = 0; expstk = 0; expmrk = 0;

#ifndef PACKAGE
    printf("Running program\n");
    printf("\n");
#endif
    do {
        stopins = FALSE; /* set no stop flag */
        sinins();
    } while (!stopins); /* until stop instruction is seen */

    finish(exitcode); /* exit program with good status */

    return 0;
}
