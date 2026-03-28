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

program pint(input,output,command);

uses endian,  { endian mode }
     mpb,     { machine parameter block }
     version, { current version number }
     parcmd,  { command line parsing }
     pint_mem, { low level vm access for pint }
     extlink, { external routine linkages }
     extend;  { extension routines }

label 99;

const

      { ************************************************************************

      Program object sizes and characteristics, sync with pint. These define
      the machine specific characteristics of the target.

      The configurations are as follows:

      type                  #bits 16  #bits 32  #bits 64
      ===========================================================
      integer               16        32        64
      real                  32        64        64
      char                  8         8         8
      boolean               8         8         8
      set                   256       256       256
      pointers              16        32        64
      marks                 16        32        64 (bytes)
      File logical number   8         8         8

      Both endian types are supported. There is no alignment needed, but you
      may wish to use alignment to tune the runtime speed.

      The machine characteristics dependent on byte accessable machines. This
      table is all you should need to adapt to any byte addressable machine.

      }

      { ******************* end of pcom and pint common parameters *********** }

      { internal constants }
      maxdef      = maxstr div 8; { number of definition bits }

      maxdigh     = 6;       { number of digits in hex representation of maxstr }
      maxdigd     = 8;       { number of digits in decimal representation of maxstr }
      maxast      = 100;     { maximum size of assert message }
      maxdbf      = 30;      { size of numeric conversion buffer }

      codemax     = maxstr;  { set size of code store to maximum possible }

      maxlabel  = 30000;      { total possible near labels in intermediate }
      maxcstfx  = 10000;     { maximum constant fixup in intermediate }
      maxgblfx  = 10000;     { maximum global access fixup in intermediate }
      resspc    = 0;         { reserve space in heap (if you want) }

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
      filmax     = 13;       { maximum reserved file region }

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

      stringlgth  = 1000; { longest string length we can buffer }
      maxsp       = 111;  { number of predefined procedures/functions }
      maxinsm     = 255;  { maximum machine instruction code, 0-255 or byte }
      maxins      = 263;  { maximum instruction code including pseudo-instructions }
      maxalfa     = 10;   { maximum number of characters in alfa type }
      fillen      = 20000; { maximum length of filenames }
      maxbrk      = 10;   { maximum number of breakpoints }
      brkins      = 19;   { breakpoint instruction no. }
      mrkins      = 174;  { source line marker instruction executed }
      mrkinsl     = 1;    { length of that instruction (minus line address) }
      mstins      = 11;   { mark stack instruction }
      varsqt      = 10;   { variable string quanta }
      { 25k lines is well above my personal limit. I tend to split files with
        more than 10,000 lines. Obviously others think that's excessive. }
      maxsrc      = 25000; { maximum number of source lines in source file }
      extsrc      = '.pas'; { extention for source file }
      maxwth      = 10;   { maximum number of watched addresses }
      maxana      = 10;   { maximum depth of analyzer traces }
      maxsym      = 4000; { maximum length of symbol/module name }

type

      { define the internally used types }
      pminteger = integer;
      pmreal = real;
      pmaddress = -maxstr..maxtop;

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
      mi          = (pi_lodi,    {   0 } pi_ldoi,    {   1 } pi_stri,    {   2 }
                     pi_sroi,    {   3 } pi_lda,     {   4 } pi_lao,     {   5 }
                     pi_stoi,    {   6 } pi_ldcs,    {   7 } pi_cjp,     {   8 }
                     pi_indi,    {   9 } pi_inci,    {  10 } pi_mst,     {  11 }
                     pi_cup,     {  12 } pi_rip,     {  13 } pi_retp,    {  14 }
                     pi_csp,     {  15 } pi_ixa,     {  16 } pi_equa,    {  17 }
                     pi_neqa,    {  18 } pi_brk,     {  19 } pi_lnp,     {  20 }
                     pi_cal,     {  21 } pi_ret,     {  22 } pi_ujp,     {  23 }
                     pi_fjp,     {  24 } pi_xjp,     {  25 } pi_chki,    {  26 }
                     pi_cuv,     {  27 } pi_adi,     {  28 } pi_adr,     {  29 }
                     pi_sbi,     {  30 } pi_sbr,     {  31 } pi_sgs,     {  32 }
                     pi_flt,     {  33 } pi_flo,     {  34 } pi_trc,     {  35 }
                     pi_ngi,     {  36 } pi_ngr,     {  37 } pi_sqi,     {  38 }
                     pi_sqr,     {  39 } pi_abi,     {  40 } pi_abr,     {  41 }
                     pi_notb,    {  42 } pi_and,     {  43 } pi_ior,     {  44 }
                     pi_dif,     {  45 } pi_int,     {  46 } pi_uni,     {  47 }
                     pi_inn,     {  48 } pi_mod,     {  49 } pi_odd,     {  50 }
                     pi_mpi,     {  51 } pi_mpr,     {  52 } pi_dvi,     {  53 }
                     pi_dvr,     {  54 } pi_mov,     {  55 } pi_lca,     {  56 }
                     pi_deci,    {  57 } pi_stp,     {  58 } pi_ordi,    {  59 }
                     pi_chr,     {  60 } pi_ujc,     {  61 } pi_rnd,     {  62 }
                     pi_pck,     {  63 } pi_upk,     {  64 } pi_ldoa,    {  65 }
                     pi_ldor,    {  66 } pi_ldos,    {  67 } pi_ldob,    {  68 }
                     pi_ldoc,    {  69 } pi_stra,    {  70 } pi_strr,    {  71 }
                     pi_strs,    {  72 } pi_strb,    {  73 } pi_strc,    {  74 }
                     pi_sroa,    {  75 } pi_sror,    {  76 } pi_sros,    {  77 }
                     pi_srob,    {  78 } pi_sroc,    {  79 } pi_stoa,    {  80 }
                     pi_stor,    {  81 } pi_stos,    {  82 } pi_stob,    {  83 }
                     pi_stoc,    {  84 } pi_inda,    {  85 } pi_indr,    {  86 }
                     pi_inds,    {  87 } pi_indb,    {  88 } pi_indc,    {  89 }
                     pi_inca,    {  90 } pi_suv,     {  91 } pi_vbs,     {  92 }
                     pi_incb,    {  93 } pi_incc,    {  94 } pi_chka,    {  95 }
                     pi_vbe,     {  96 } pi_chks,    {  97 } pi_chkb,    {  98 }
                     pi_chkc,    {  99 } pi_cvbi,    { 100 } pi_ivtx,    { 101 }
                     pi_ivtb,    { 102 } pi_decb,    { 103 } pi_decc,    { 104 }
                     pi_loda,    { 105 } pi_lodr,    { 106 } pi_lods,    { 107 }
                     pi_lodb,    { 108 } pi_lodc,    { 109 } pi_rgs,     { 110 }
                     pi_ivtc,    { 111 } pi_ipj,     { 112 } pi_cip,     { 113 }
                     pi_lpa,     { 114 } pi_cvbx,    { 115 } pi_cvbb,    { 116 }
                     pi_dmp,     { 117 } pi_swp,     { 118 } pi_tjp,     { 119 }
                     pi_lip,     { 120 } pi_cvbc,    { 121 } pi_vis,     { 122 }
                     pi_ldci,    { 123 } pi_ldcr,    { 124 } pi_ldcn,    { 125 }
                     pi_ldcb,    { 126 } pi_ldcc,    { 127 } pi_reti,    { 128 }
                     pi_retr,    { 129 } pi_retc,    { 130 } pi_retb,    { 131 }
                     pi_reta,    { 132 } pi_vip,     { 133 } pi_ordb,    { 134 }
                     pi_lcp,     { 135 } pi_ordc,    { 136 } pi_equi,    { 137 }
                     pi_equr,    { 138 } pi_equb,    { 139 } pi_equs,    { 140 }
                     pi_equc,    { 141 } pi_equm,    { 142 } pi_neqi,    { 143 }
                     pi_neqr,    { 144 } pi_neqb,    { 145 } pi_neqs,    { 146 }
                     pi_neqc,    { 147 } pi_neqm,    { 148 } pi_geqi,    { 149 }
                     pi_geqr,    { 150 } pi_geqb,    { 151 } pi_geqs,    { 152 }
                     pi_geqc,    { 153 } pi_geqm,    { 154 } pi_grti,    { 155 }
                     pi_grtr,    { 156 } pi_grtb,    { 157 } pi_grts,    { 158 }
                     pi_grtc,    { 159 } pi_grtm,    { 160 } pi_leqi,    { 161 }
                     pi_leqr,    { 162 } pi_leqb,    { 163 } pi_leqs,    { 164 }
                     pi_leqc,    { 165 } pi_leqm,    { 166 } pi_lesi,    { 167 }
                     pi_lesr,    { 168 } pi_lesb,    { 169 } pi_less,    { 170 }
                     pi_lesc,    { 171 } pi_lesm,    { 172 } pi_ph1,     { 173 }
                     pi_mrkl,    { 174 } pi_ckvi,    { 175 } pi_cps,     { 176 }
                     pi_cpc,     { 177 } pi_aps,     { 178 } pi_ckvb,    { 179 }
                     pi_ckvc,    { 180 } pi_dupi,    { 181 } pi_dupa,    { 182 }
                     pi_dupr,    { 183 } pi_dups,    { 184 } pi_dupb,    { 185 }
                     pi_dupc,    { 186 } pi_cks,     { 187 } pi_cke,     { 188 }
                     pi_inv,     { 189 } pi_ckla,    { 190 } pi_cta,     { 191 }
                     pi_ivti,    { 192 } pi_lodx,    { 193 } pi_ldox,    { 194 }
                     pi_strx,    { 195 } pi_srox,    { 196 } pi_stox,    { 197 }
                     pi_indx,    { 198 } pi_chkx,    { 199 } pi_ordx,    { 200 }
                     pi_incx,    { 201 } pi_decx,    { 202 } pi_ckvx,    { 203 }
                     pi_retx,    { 204 } pi_noti,    { 205 } pi_xor,     { 206 }
                     pi_bge,     { 207 } pi_ede,     { 208 } pi_mse,     { 209 }
                     pi_apc,     { 210 } pi_cxs,     { 211 } pi_cxc,     { 212 }
                     pi_lft,     { 213 } pi_max,     { 214 } pi_equv,    { 215 }
                     pi_neqv,    { 216 } pi_lesv,    { 217 } pi_grtv,    { 218 }
                     pi_leqv,    { 219 } pi_geqv,    { 220 } pi_vdp,     { 221 }
                     pi_spc,     { 222 } pi_ccs,     { 223 } pi_scp,     { 224 }
                     pi_ldp,     { 225 } pi_vin,     { 226 } pi_vdd,     { 227 }
                     pi_ph3,     { 228 } pi_ph4,     { 229 } pi_ph5,     { 230 }
                     pi_ph6,     { 231 } pi_ph7,     { 232 } pi_ph8,     { 233 }
                     pi_ph9,     { 234 } pi_stom,    { 235 } pi_rets,    { 236 }
                     pi_retm,    { 237 } pi_ctb,     { 238 } pi_cpp,     { 239 }
                     pi_cpr,     { 240 } pi_lsa,     { 241 } pi_eext,    { 242 }
                     pi_wbs,     { 243 } pi_wbe,     { 244 } pi_sfr,     { 245 }
                     pi_cuf,     { 246 } pi_cif,     { 247 } pi_mpc,     { 248 }
                     pi_cvf,     { 249 } pi_ph10,    { 250 } pi_cpl,     { 251 }
                     pi_sfs,     { 252 } pi_sev,     { 253 } pi_mdc,     { 254 }
                     pi_ph2,     { 255 }
                     { past the 255 instruction, these instructions don't fit
                       into a single byte. Thus after this, these are pseudo
                       instructions that don't end up as machine instructions. }
                     pi_ltci,    { 256 } pi_ltcr,    { 257 } pi_ltcs,    { 258 }
                     pi_ltcb,    { 259 } pi_ltcc,    { 260 } pi_ltcx,    { 261 }
                     pi_lto,     { 262 } pi_lsp,     { 263 }
                     pi_s2c      { 264 }
                    );
      beta        = packed array[1..25] of char; (*error message*)
      alfainx     = 1..maxalfa; { index for alfa type }
      alfa        = packed array[alfainx] of char;
      insrec      = record
                      mn:  alfa;    { mnemonic instruction code }
                      p:   boolean; { instruction includes a p parameter }
                      q:   0..32;   { length of q parameter }
                      mi:  mi       { instruction code enumeration }
                    end;
      ibyte       = byte; { 8-bit byte }
      bytfil      = packed file of byte; { untyped file of bytes }
      charptr     = ^char; { pointer to character }
      filnam      = packed array [1..fillen] of char; { filename strings }
      break       = record
                      ss: ibyte; { byte under breakpoint }
                      sa: address; { code address }
                      line: 0..maxsrc; { source line if associated, or 0 }
                      trace: boolean; { is a tracepoint }
                      temp: boolean { is a temporary breakpoint }
                    end;
      brkinx      = 1..maxbrk;
      brknum      = 0..maxbrk;
      { Here is the variable length string containment to save on space. strings
        are only stored in their length rounded to the nearest 10th. }
      strvsp = ^strvs; { pointer to variable length id string }
      strvs = record { id string variable length }
                str:   packed array [1..varsqt] of char; { data contained }
                next:  strvsp { next }
              end;
      lintrkt      = array [1..maxsrc] of address; { addresses of lines }
      linprft      = array [1..maxsrc] of integer; { line profiling }
      psymbol     = ^symbol;
      symbol      = record
                      next:   psymbol; { next list symbol }
                      name:   strvsp; { name }
                      { area type }
                      styp:   (stglobal, stlocal, stparam, stfixg, stfixl);
                      off:    address; { offset address }
                      digest: strvsp; { type digest }
                    end;
      pblock       = ^block;
      block        = record
                       next:    pblock; { next list block }
                       incnxt:  pblock; { included blocks list }
                       name:    strvsp; { name of block, including type }
                       bname:   strvsp; { name of block, not including type }
                       fname:   strvsp; { filename of block(module) }
                       symbols: psymbol; { symbol list for block }
                       { block type }
                       btyp:    (btprog, btmod, btproc, btfunc);
                       bestart: address; { block enclosing start }
                       bstart:  address; { block start address }
                       bend:    address; { block end address }
                       lintrk: ^lintrkt; { addresses of lines }
                       linprf: ^linprft; { line profiling }
                     end;
      wthinx       = 1..maxwth; { index for watch table }
      wthnum       = 0..maxwth; { watch table number }
      { watch symbol/type table entry }
      wthrec       = record sp: psymbol; p: integer end;
      { parser control record }
      parctl       = record b: strvsp; l, p: integer end;
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
      { debug commands }
      dbgcmd = (dcnone, dcli, dcd, dcd8, dcd16, dcd32, dcd64, dcdb16, dcdb32,
                dcdb64, dcdl16, dcdl32, dcdl64, dcds, dcdd, dcdf, dcdst, dcb, 
                dctp, dcbi, dctpi, dcc, dclb, dcsi, dcsis, dcl, dclc, dcs, dcss,
                dcp, dce, dcst, dcw, dclw, dccw, dclia, dclsa, dcpg, dcpl, dcpp,
                dchs, dcti, dcnti, dctr, dcntr, dcts, dcnts, dcspf, dcnspf,dcic,
                dcnic, dcan, dcnan, dcps, dcr, dcq, dcso, dcsso, dcsio, dcsiso,
                dcret, dchelp, dch, dclistline, dcdumpsymbo);
      filext = packed array [1..4] of char; { filename extension }

fixed

      { instruction table }
      ins: array[instyp] of insrec = array
        record 'lodi      ', true , intsize, pi_lodi end,
        record 'ldoi      ', false, intsize, pi_ldoi end,
        record 'stri      ', true , intsize, pi_stri end,
        record 'sroi      ', false, intsize, pi_sroi end,
        record 'lda       ', true , intsize, pi_lda end,
        record 'lao       ', false, intsize, pi_lao end,
        record 'stoi      ', false, 0, pi_stoi end,
        record 'ldcs      ', false, intsize, pi_ldcs end,
        record 'cjp       ', false, intsize*2, pi_cjp end,
        record 'indi      ', false, intsize, pi_indi end,
        record 'inci      ', false, intsize, pi_inci end,
        record 'mst       ', true , intsize*2, pi_mst end,
        record 'cup       ', false, intsize, pi_cup end,
        record 'rip       ', false, adrsize, pi_rip end,
        record 'retp      ', false, intsize, pi_retp end,
        record 'csp       ', false, 1, pi_csp end,
        record 'ixa       ', false, intsize, pi_ixa end,
        record 'equa      ', false, 0, pi_equa end,
        record 'neqa      ', false, 0, pi_neqa end,
        record 'brk*      ', false, 0, pi_brk end,
        record 'lnp*      ', false, intsize, pi_lnp end,
        record 'cal       ', false, intsize, pi_cal end,
        record 'ret       ', false, 0, pi_ret end,
        record 'ujp       ', false, intsize, pi_ujp end,
        record 'fjp       ', false, intsize, pi_fjp end,
        record 'xjp       ', false, intsize, pi_xjp end,
        record 'chki      ', false, intsize, pi_chki end,
        record 'cuv       ', false, intsize, pi_cuv end,
        record 'adi       ', false, 0, pi_adi end,
        record 'adr       ', false, 0, pi_adr end,
        record 'sbi       ', false, 0, pi_sbi end,
        record 'sbr       ', false, 0, pi_sbr end,
        record 'sgs       ', false, 0, pi_sgs end,
        record 'flt       ', false, 0, pi_flt end,
        record 'flo       ', false, 0, pi_flo end,
        record 'trc       ', false, 0, pi_trc end,
        record 'ngi       ', false, 0, pi_ngi end,
        record 'ngr       ', false, 0, pi_ngr end,
        record 'sqi       ', false, 0, pi_sqi end,
        record 'sqr       ', false, 0, pi_sqr end,
        record 'abi       ', false, 0, pi_abi end,
        record 'abr       ', false, 0, pi_abr end,
        record 'notb      ', false, 0, pi_notb end,
        record 'and       ', false, 0, pi_and end,
        record 'ior       ', false, 0, pi_ior end,
        record 'dif       ', false, 0, pi_dif end,
        record 'int       ', false, 0, pi_int end,
        record 'uni       ', false, 0, pi_uni end,
        record 'inn       ', false, 0, pi_inn end,
        record 'mod       ', false, 0, pi_mod end,
        record 'odd       ', false, 0, pi_odd end,
        record 'mpi       ', false, 0, pi_mpi end,
        record 'mpr       ', false, 0, pi_mpr end,
        record 'dvi       ', false, 0, pi_dvi end,
        record 'dvr       ', false, 0, pi_dvr end,
        record 'mov       ', false, intsize, pi_mov end,
        record 'lca       ', false, intsize, pi_lca end,
        record 'deci      ', false, intsize, pi_deci end,
        record 'stp*      ', false, 0, pi_stp end,
        record 'ordi      ', false, 0, pi_ordi end,
        record 'chr       ', false, 0, pi_chr end,
        record 'ujc       ', false, intsize, pi_ujc end,
        record 'rnd       ', false, 0, pi_rnd end,
        record 'pck       ', false, intsize*2, pi_pck end,
        record 'upk       ', false, intsize*2, pi_upk end,
        record 'ldoa      ', false, intsize, pi_ldoa end,
        record 'ldor      ', false, intsize, pi_ldor end,
        record 'ldos      ', false, intsize, pi_ldos end,
        record 'ldob      ', false, intsize, pi_ldob end,
        record 'ldoc      ', false, intsize, pi_ldoc end,
        record 'stra      ', true , intsize, pi_stra end,
        record 'strr      ', true , intsize, pi_strr end,
        record 'strs      ', true , intsize, pi_strs end,
        record 'strb      ', true , intsize, pi_strb end,
        record 'strc      ', true , intsize, pi_strc end,
        record 'sroa      ', false, intsize, pi_sroa end,
        record 'sror      ', false, intsize, pi_sror end,
        record 'sros      ', false, intsize, pi_sros end,
        record 'srob      ', false, intsize, pi_srob end,
        record 'sroc      ', false, intsize, pi_sroc end,
        record 'stoa      ', false, 0, pi_stoa end,
        record 'stor      ', false, 0, pi_stor end,
        record 'stos      ', false, 0, pi_stos end,
        record 'stob      ', false, 0, pi_stob end,
        record 'stoc      ', false, 0, pi_stoc end,
        record 'inda      ', false, intsize, pi_inda end,
        record 'indr      ', false, intsize, pi_indr end,
        record 'inds      ', false, intsize, pi_inds end,
        record 'indb      ', false, intsize, pi_indb end,
        record 'indc      ', false, intsize, pi_indc end,
        record 'inca      ', false, intsize, pi_inca end,
        record 'suv       ', false, intsize*2, pi_suv end,
        record 'vbs       ', false, intsize, pi_vbs end,
        record 'incb      ', false, intsize, pi_incb end,
        record 'incc      ', false, intsize, pi_incc end,
        record 'chka      ', false, intsize, pi_chka end,
        record 'vbe       ', false, 0, pi_vbe end,
        record 'chks      ', false, intsize, pi_chks end,
        record 'chkb      ', false, intsize, pi_chkb end,
        record 'chkc      ', false, intsize, pi_chkc end,
        record 'cvbi      ', false, intsize*3, pi_cvbi end,
        record 'ivtx      ', false, intsize*3, pi_ivtx end,
        record 'ivtb      ', false, intsize*3, pi_ivtb end,
        record 'decb      ', false, intsize, pi_decb end,
        record 'decc      ', false, intsize, pi_decc end,
        record 'loda      ', true , intsize, pi_loda end,
        record 'lodr      ', true , intsize, pi_lodr end,
        record 'lods      ', true , intsize, pi_lods end,
        record 'lodb      ', true , intsize, pi_lodb end,
        record 'lodc      ', true , intsize, pi_lodc end,
        record 'rgs       ', false, 0, pi_rgs end,
        record 'ivtc      ', false, intsize*3, pi_ivtc end,
        record 'ipj       ', true , intsize, pi_ipj end,
        record 'cip       ', false, 0, pi_cip end,
        record 'lpa       ', true , intsize, pi_lpa end,
        record 'cvbx      ', false, intsize*3, pi_cvbx end,
        record 'cvbb      ', false, intsize*3, pi_cvbb end,
        record 'dmp       ', false, intsize, pi_dmp end,
        record 'swp       ', false, intsize, pi_swp end,
        record 'tjp       ', false, intsize, pi_tjp end,
        record 'lip       ', true , intsize, pi_lip end,
        record 'cvbc      ', false, intsize*3, pi_cvbc end,
        record 'vis       ', false, intsize*2, pi_vis end,
        record 'ldci      ', false, intsize, pi_ldci end,
        record 'ldcr      ', false, intsize, pi_ldcr end,
        record 'ldcn      ', false, 0, pi_ldcn end,
        record 'ldcb      ', false, boolsize, pi_ldcb end,
        record 'ldcc      ', false, charsize, pi_ldcc end,
        record 'reti      ', false, intsize, pi_reti end,
        record 'retr      ', false, intsize, pi_retr end,
        record 'retc      ', false, intsize, pi_retc end,
        record 'retb      ', false, intsize, pi_retb end,
        record 'reta      ', false, intsize, pi_reta end,
        record 'vip       ', false, intsize*2, pi_vip end,
        record 'ordb      ', false, 0, pi_ordb end,
        record 'lcp       ', false, 0, pi_lcp end,
        record 'ordc      ', false, 0, pi_ordc end,
        record 'equi      ', false, 0, pi_equi end,
        record 'equr      ', false, 0, pi_equr end,
        record 'equb      ', false, 0, pi_equb end,
        record 'equs      ', false, 0, pi_equs end,
        record 'equc      ', false, 0, pi_equc end,
        record 'equm      ', false, intsize, pi_equm end,
        record 'neqi      ', false, 0, pi_neqi end,
        record 'neqr      ', false, 0, pi_neqr end,
        record 'neqb      ', false, 0, pi_neqb end,
        record 'neqs      ', false, 0, pi_neqs end,
        record 'neqc      ', false, 0, pi_neqc end,
        record 'neqm      ', false, intsize, pi_neqm end,
        record 'geqi      ', false, 0, pi_geqi end,
        record 'geqr      ', false, 0, pi_geqr end,
        record 'geqb      ', false, 0, pi_geqb end,
        record 'geqs      ', false, 0, pi_geqs end,
        record 'geqc      ', false, 0, pi_geqc end,
        record 'geqm      ', false, intsize, pi_geqm end,
        record 'grti      ', false, 0, pi_grti end,
        record 'grtr      ', false, 0, pi_grtr end,
        record 'grtb      ', false, 0, pi_grtb end,
        record 'grts      ', false, 0, pi_grts end,
        record 'grtc      ', false, 0, pi_grtc end,
        record 'grtm      ', false, intsize, pi_grtm end,
        record 'leqi      ', false, 0, pi_leqi end,
        record 'leqr      ', false, 0, pi_leqr end,
        record 'leqb      ', false, 0, pi_leqb end,
        record 'leqs      ', false, 0, pi_leqs end,
        record 'leqc      ', false, 0, pi_leqc end,
        record 'leqm      ', false, intsize, pi_leqm end,
        record 'lesi      ', false, 0, pi_lesi end,
        record 'lesr      ', false, 0, pi_lesr end,
        record 'lesb      ', false, 0, pi_lesb end,
        record 'less      ', false, 0, pi_less end,
        record 'lesc      ', false, 0, pi_lesc end,
        record 'lesm      ', false, intsize, pi_lesm end,
        record '---       ', false, 0, pi_ph1 end,
        record 'mrkl*     ', false, intsize, pi_mrkl end,
        record 'ckvi      ', false, intsize, pi_ckvi end,
        record 'cps       ', false, 0, pi_cps end,
        record 'cpc       ', false, intsize, pi_cpc end,
        record 'aps       ', false, intsize, pi_aps end,
        record 'ckvb      ', false, intsize, pi_ckvb end,
        record 'ckvc      ', false, intsize, pi_ckvc end,
        record 'dupi      ', false, 0, pi_dupi end,
        record 'dupa      ', false, 0, pi_dupa end,
        record 'dupr      ', false, 0, pi_dupr end,
        record 'dups      ', false, 0, pi_dups end,
        record 'dupb      ', false, 0, pi_dupb end,
        record 'dupc      ', false, 0, pi_dupc end,
        record 'cks       ', false, 0, pi_cks end,
        record 'cke       ', false, 0, pi_cke end,
        record 'inv       ', false, 0, pi_inv end,
        record 'ckla      ', false, intsize, pi_ckla end,
        record 'cta       ', false, intsize*3, pi_cta end,
        record 'ivti      ', false, intsize*3, pi_ivti end,
        record 'lodx      ', true , intsize, pi_lodx end,
        record 'ldox      ', false, intsize, pi_ldox end,
        record 'strx      ', true , intsize, pi_strx end,
        record 'srox      ', false, intsize, pi_srox end,
        record 'stox      ', false, 0, pi_stox end,
        record 'indx      ', false, intsize, pi_indx end,
        record 'chkx      ', false, intsize, pi_chkx end,
        record 'ordx      ', false, 0, pi_ordx end,
        record 'incx      ', false, intsize, pi_incx end,
        record 'decx      ', false, intsize, pi_decx end,
        record 'ckvx      ', false, intsize, pi_ckvx end,
        record 'retx      ', false, intsize, pi_retx end,
        record 'noti      ', false, 0, pi_noti end,
        record 'xor       ', false, 0, pi_xor end,
        record 'bge       ', false, intsize, pi_bge end,
        record 'ede       ', false, 0, pi_ede end,
        record 'mse       ', false, 0, pi_mse end,
        record 'apc       ', false, intsize*2, pi_apc end,
        record 'cxs       ', false, intsize, pi_cxs end,
        record 'cxc       ', false, intsize*2, pi_cxc end,
        record 'lft       ', false, intsize, pi_lft end,
        record 'max       ', false, intsize, pi_max end,
        record 'equv      ', false, 0, pi_equv end,
        record 'neqv      ', false, 0, pi_neqv end,
        record 'lesv      ', false, 0, pi_lesv end,
        record 'grtv      ', false, 0, pi_grtv end,
        record 'leqv      ', false, 0, pi_leqv end,
        record 'geqv      ', false, 0, pi_geqv end,
        record 'vdp       ', false, 0, pi_vdp end,
        record 'spc       ', false, 0, pi_spc end,
        record 'ccs       ', false, intsize*2, pi_ccs end,
        record 'scp       ', false, 0, pi_scp end,
        record 'ldp       ', false, 0, pi_ldp end,
        record 'vin       ', false, intsize*2, pi_vin end,
        record 'vdd       ', false, 0, pi_vdd end,
        record '---       ', false, 0, pi_ph3 end,
        record '---       ', false, 0, pi_ph4 end,
        record '---       ', false, 0, pi_ph5 end,
        record '---       ', false, 0, pi_ph6 end,
        record '---       ', false, 0, pi_ph7 end,
        record '---       ', false, 0, pi_ph8 end,
        record '---       ', false, 0, pi_ph9 end,
        record 'stom      ', false, intsize*2, pi_stom end,
        record 'rets      ', false, intsize, pi_rets end,
        record 'retm      ', false, intsize*2, pi_retm end,
        record 'ctb       ', false, intsize*2, pi_ctb end,
        record 'cpp       ', false, intsize*2, pi_cpp end,
        record 'cpr       ', false, intsize*2, pi_cpr end,
        record 'lsa       ', false, intsize, pi_lsa end,
        record 'eext*     ', false, 0, pi_eext end,
        record 'wbs       ', false, 0, pi_wbs end,
        record 'wbe       ', false, 0, pi_wbe end,
        record 'sfr       ', false, intsize, pi_sfr end,
        record 'cuf       ', false, intsize, pi_cuf end,
        record 'cif       ', false, 0, pi_cif end,
        record 'mpc       ', false, 0, pi_mpc end,
        record 'cvf       ', false, intsize, pi_cvf end,
        record '---       ', false, 0, pi_ph10 end,
        record 'cpl       ', false, 0, pi_cpl end,
        record 'sfs       ', false, intsize*2, pi_sfs end,
        record 'sev       ', true , intsize, pi_sev end,
        record 'mdc       ', false, intsize, pi_mdc end,
        record '---       ', false, 0, pi_ph2 end,
        { pseudo-instructions, not machine executable }
        record 'ltci      ', false, intsize, pi_ltci end,
        record 'ltcr      ', false, intsize, pi_ltcr end,
        record 'ltcs      ', false, intsize, pi_ltcs end,
        record 'ltcb      ', false, intsize, pi_ltcb end,
        record 'ltcc      ', false, intsize, pi_ltcc end,
        record 'ltcx      ', false, intsize, pi_ltcx end,
        record 'lto       ', false, intsize, pi_lto end,
        record 'lsp       ', false, 0, pi_lsp end,
        record 's2c       ', false, intsize, pi_s2c end
      end;

var   pc, pcs     : address;   (*program address register*)
      pctop,lsttop: address;   { top of code store }
      gbtop, gbsiz: address;   { top of globals, size of globals }
      gbset       : boolean;   { global size was set }
      op : instyp; p : lvltyp; q : address;  (*instruction register*)
      q1, q2: address; { extra parameters }
      store       : packed array [0..maxstr] of ibyte; { complete program storage }
      storedef    : packed array [0..maxdef] of ibyte; { defined bits }
      storecov    : packed array [0..maxdef] of ibyte; { coverage bits }
      sdi         : 0..maxdef; { index for that }
      cststr      : address; { start of constants block }
      mp,sp,np,ep : address;  (* address registers *)
      expadr      : address; { exception address of exception handler starts }
      expstk      : address; { exception address of sp at handlers }
      expmrk      : address; { exception address of mp at handlers }
      (*mp  points to beginning of a data segment
        sp  points to top of the stack
        ep  points to the maximum extent of the stack
        np  points to top of the dynamically allocated area*)
      bitmsk      : packed array [0..7] of ibyte; { bits in byte }
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
      amd64_sysv: boolean; { use SYS V AMD64 ABI calling convention }
      doechlin: boolean; { echo input command line }

      { other flags }
      iso7185: boolean; { iso7185 standard flag }
      flipend: boolean; { endian mode is opposing }

      debugstart: boolean; { we have started debug mode }

      prd,prr     : text; (*prd for read only, prr for write only *)

      sptable     : array[0..maxsp] of alfa; (*standard functions and procedures*)
      srclin      : integer; { current source line executing }
      brktbl      : array [brkinx] of break; { breakpoint table }
      bi          : brkinx; { index for same }
      anitbl      : array [1..maxana] of address; { instruction analyzer queue }
      aniptr      : 1..maxana; { input pointer }
      anstbl      : array [1..maxana] of 0..maxsrc; { source analyzer queue }
      ansmtbl     : array [1..maxana] of pblock; { source analyzer module queue }
      ansptr      : 1..maxana; { input pointer }
      stopins     : boolean; { stop instruction executed }
      breakins    : boolean; { break instruction executed }
      sourcemark  : boolean; { source line instruction executed }
      stopwatch   : boolean; { stop on watch address matched }
      watchmatch  : boolean; { watch address was matched on instruction }

      { general (temp) binary file holders }
      bfiltable   : array [1..maxfil] of bytfil;
      { file buffer full status }
      filbuff     : array [1..maxfil] of boolean;
      { file name has been assigned }
      filanamtab  : array [1..maxfil] of boolean;

      strcnt      : integer; { string allocation count }
      blkstk      : pblock; { stack of symbol blocks }
      blklst      : pblock; { discard list of symbols blocks }
      wthtbl      : array [wthinx] of address; { watch table }
      wi          : wthinx; { index for that }
      { symbol/type tracking for watch entries }
      wthsym      : array [wthinx] of wthrec;
      { address of watchpoint instruction store in progress }
      stoad       : address;
      errsinprg   : integer; { errors in source program }
      boolsym     : psymbol; { symbol for boolean result }
      realsym     : psymbol; { symbol for real result }
      intsym      : psymbol; { symbol for integer result }
      charsym     : psymbol; { symbol for character result }
      tmpsym      : psymbol; { list of expression temp symbols }
      maxpow10    : integer; { maximum power of 10 }
      decdig      : integer; { digits in unsigned decimal }
      maxpow16    : integer; { maximum power of 16 }
      hexdig      : integer; { digits in unsigned hex }
      maxpow8     : integer; { maximum power of 8 }
      octdig      : integer; { digits in unsigned octal }
      maxpow2     : integer; { maximum power of 2 }
      bindig      : integer; { digits in unsigned binary }
      curmod      : pblock; { currently active block }
      varlst      : varptr; { active var block pushdown stack }
      varfre      : varptr; { free var block entries }
      wthlst      : wthptr; { active with block pushdown stack }
      wthcnt      : integer; { number of outstanding with levels }
      wthfre      : wthptr; { free with block entries }
      extvecs     : integer; { number of external vectors }
      extvecbase  : integer; { base of external vectors }
      exitcode    : integer; { exit code for program }
      breakflag   : boolean; { user break signaled }
      dbgcmds     : array [dbgcmd] of alfa; { debug command strings }
      prdval      : boolean; { input source file parsed }
      prrval      : boolean; { output source file parsed }

      i           : integer;
      ad          : address;
      bai         : integer;
      ai          : 1..maxana;
      oi          : 1..maxopt;
      bp, pbp     : pblock;
      lno         : integer;
      
(*--------------------------------------------------------------------*)

procedure debug; forward;

{ Low level error check and handling }

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

procedure lstins(var ad: address); forward;

procedure dmpins;
begin
    wrthex(output, pcs, maxdigh, true);
    write('/');
    wrthex(output, sp, maxdigh, true);
    lstins(pcs);
    writeln
end;

procedure errors(a: address; l: address);
begin writeln; write('*** Runtime error');
      if srclin > 0 then write(' [', srclin:1, ']');
      write(': ');
      if l > maxast then l := maxast;
      while l > 0 do begin write(chr(store[a])); a := a+1; l := l-1 end;
      if dodebug or dodbgflt then debug { enter debugger on fault }
      else goto 99
end;(*errori*)

{ Error handling:

  To throw a standard exception, errore() is used. To bypass the interceptable
  exceptions and directly print, use errorv().

}

{ throw an exception by vector }
override procedure errore(ei: integer); forward;

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
    ReadCharacterMismatch:              writeln('Read character mismatch');

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

  { Uncomment for machine instruction dump on fault }

  {
  dmpins;
  }

  if dodebug or dodbgflt then debug { enter debugger on fault }
  else goto 99
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

{ get bit from coverage array }
function getcov(a: address): boolean;

var b: ibyte;
    r: boolean;

begin
  b := storecov[a div 8]; { get byte }
  r := odd(b div bitmsk[a mod 8]);
  getcov := r
end;

{ put bit to coverage array }
procedure putcov(a: address; b: boolean);

var sb: ibyte;
    r:  boolean;

begin
  sb := storecov[a div 8]; { get byte }
  { test bit as is }
  r := odd(sb div bitmsk[a mod 8]);
  if r <> b then begin
    if b then sb := sb+bitmsk[a mod 8]
    else sb := sb-bitmsk[a mod 8];
    storecov[a div 8] := sb
  end
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
  refer(fn); { unused }
  for i := 1 to fillen do fne[i] := ' ';
  { skip leading spaces }
  while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
  i := 1;
  while not eolncommand and not eofcommand and (bufcommand <> ' ') do begin
    if i = fillen then errorv(FilenameTooLong);
    fne[i] := bufcommand;
    getcommand;
    i := i+1
  end;
  if fne[1] = ' ' then errorv(FileNameEmpty);
  assign(f, fne) { assign to that }
end;

procedure assignexternalbin(var f: bytfil; var fn: filnam);
var fne: filnam; i: integer;
begin
  refer(fn); { unused }
  for i := 1 to fillen do fne[i] := ' ';
  { skip leading spaces }
  while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
  i := 1;
  while not eolncommand and not eofcommand and (bufcommand <> ' ') do begin
    if i = fillen then errorv(FileNameTooLong);
    fne[i] := bufcommand;
    getcommand;
    i := i+1
  end;
  if fne[1] = ' ' then errorv(FileNameEmpty);
  assign(f, fne) { assign to that }
end;

(*-------------------------------------------------------------------------*)

                { character and string quanta functions }

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

{ find padded length of variable length id string }
function lenpv(s: strvsp): integer;
var l, tl: integer;
begin tl := 0;
  while s <> nil do begin
    l := varsqt; 
    if s^.next = nil then begin
      while (s^.str[l] = ' ') and (l > 1) do l := l-1;
      if s^.str[l] = ' ' then l := 0;
    end;
    tl := tl+l;
    s := s^.next
  end;
  lenpv := tl
end;

{ find length of fixed padded string }
function lenp(var s: filnam): integer;
var i: integer;
begin
  i := fillen; while (i > 1) and (s[i] = ' ') do i := i-1;
  if s[i] = ' ' then i := 0;
  lenp := i
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
  allocation, with length specified }
procedure strassvfl(var a: strvsp; var b: filnam; l: integer);
var i, j: integer; p, lp: strvsp;
begin p := nil; a := nil; j := 1; lp := nil;
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
procedure strassvf(var a: strvsp; var b: filnam);
var l: integer;
begin l := fillen;
  while (l > 1) and (b[l] = ' ') do l := l-1; { find length of fixed string }
  if b[l] = ' ' then l := 0;
  strassvfl(a, b, l) { perform assign }
end;

{ assign variable length string to fixed identifier }
procedure strassfv(var a: filnam; b: strvsp);
var i, j: integer;
begin for i := 1 to fillen do a[i] := ' '; i := 1;
  while b <> nil do begin
    for j := 1 to varsqt do begin a[i] := b^.str[j]; i := i+1 end;
    b := b^.next
  end
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

override function getint(a: address): integer;

var r: record case boolean of

          true:  (i: pminteger);
          false: (b: packed array [1..intsize] of ibyte);

       end;
    i: 1..intsize;

begin
   if dochkdef then chkdef(a);
   if flipend then for i := intsize downto 1 do r.b[i] := store[a+(intsize-i)]
   else for i := 1 to intsize do r.b[i] := store[a+i-1];

   getint := r.i
end;

override procedure putint(a: address; x: integer);

var r: record case boolean of

          true:  (i: pminteger);
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

override function getrel(a: address): real;

var r: record case boolean of

          true:  (r: pmreal);
          false: (b: packed array [1..realsize] of ibyte);

       end;
    i: 1..realsize;

begin

   if dochkdef then chkdef(a);
   if flipend then for i := realsize downto 1 do r.b[i] := store[a+(realsize-i)]
   else for i := 1 to realsize do r.b[i] := store[a+i-1];
   getrel := r.r

end;

override procedure putrel(a: address; f: real);

var r: record case boolean of

          true:  (r: pmreal);
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

override function getbol(a: address): boolean;

var b: boolean;

begin

   if dochkdef then chkdef(a);
   if store[a] = 0 then b := false else b := true;
   getbol := b

end;

override procedure putbol(a: address; b: boolean);

begin

   store[a] := ord(b); putdef(a, true)

end;

override procedure getset(a: address; var s: settype);

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

override procedure putset(a: address; s: settype);

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

override function getchr(a: address): char;

begin

   if dochkdef then chkdef(a);
   getchr := chr(store[a])

end;

override procedure putchr(a: address; c: char);

begin

   store[a] := ord(c); putdef(a, true)

end;

override function getbyt(a: address): ibyte;

begin

   if dochkdef then chkdef(a);
   getbyt := store[a]

end;

override procedure putbyt(a: address; b: ibyte);

begin

   store[a] := b; putdef(a, true)

end;

override function getadr(a: address): address;

var r: record case boolean of

          true:  (a: integer{pmaddress});
          false: (b: packed array [1..adrsize] of ibyte);

       end;
    i: 1..adrsize;

begin

   if dochkdef then chkdef(a);
   if flipend then for i := adrsize downto 1 do r.b[i] := store[a+(adrsize-i)]
   else for i := 1 to adrsize do r.b[i] := store[a+i-1];

   getadr := r.a

end;

override procedure putadr(a: address; ad: address);

var r: record case boolean of

          true:  (a: integer{pmaddress});
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

{ end of accessor functions }

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
override procedure errore(ei: integer);
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

function fndbrk(a: address): integer;
var i, x: integer;
begin
  x := 0;
  for i := 1 to maxbrk do if brktbl[i].sa = a then x := i;
  fndbrk := x
end;

procedure lstbrk;
var i: brkinx;
begin
  writeln;
  writeln('Breakpoints:');
  writeln;
  writeln('No  Src  Addr   Trc/brk');
  writeln('=======================');
  for i := 1 to maxbrk do if brktbl[i].sa >= 0 then begin
    if brktbl[i].line > 0 then write(i:2, ':', brktbl[i].line:4, ': ')
    else write(i:2, ':****', ': ');
    wrthex(output, brktbl[i].sa, maxdigh, true); write(' ');
    if brktbl[i].trace then write('t') else write('b'); writeln;
  end;
  writeln
end;

function isbrk(a: address): boolean;
var i: brknum;
begin i := fndbrk(a); isbrk := i > 0 end;

function isbrkl(l: integer): boolean;
var i: brkinx;
    m: boolean;
begin m := false;
  for i := 1 to maxbrk do
    if (brktbl[i].sa >= 0) and (brktbl[i].line = l) and not brktbl[i].trace then
      m := true;
  isbrkl := m
end;

function istrc(a: address): boolean;
var i: brkinx;
    m: boolean;
begin m := false;
  for i := 1 to maxbrk do if (brktbl[i].sa = a) and brktbl[i].trace then
    m := true;
  istrc := m
end;

function istrcl(l: integer): boolean;
var i: brkinx;
    m: boolean;
begin m := false;
  for i := 1 to maxbrk do
    if (brktbl[i].sa >= 0) and (brktbl[i].line = l) and brktbl[i].trace then
      m := true;
  istrcl := m
end;

function istmp(a: address): boolean;
var i: brkinx;
    m: boolean;
begin m := false;
  for i := 1 to maxbrk do if (brktbl[i].sa = a) and brktbl[i].temp then
    m := true;
  istmp := m
end;

function istmpl(l: integer): boolean;
var i: brkinx;
    m: boolean;
begin m := false;
  for i := 1 to maxbrk do
    if (brktbl[i].sa >= 0) and (brktbl[i].line = l) and brktbl[i].temp then
      m := true;
  istmpl := m
end;

{ list single instruction at address }

procedure lstins{(var ad: address)};

var op: instyp; p : lvltyp; q, q1,q2 : integer;  (*instruction register*)

begin

   { fetch instruction from byte store }
   op := store[ad]; ad := ad+1;
   if ins[op].p then begin p := store[ad]; ad := ad+1 end;
   if ins[op].q > 0 then begin

      if ins[op].q = 1 then q := store[ad]
      else begin
        q := getint(ad);
        if ins[op].q > intsize then q1 := getint(ad+intsize);
        if ins[op].q > intsize*2 then q2 := getint(ad+intsize*2);
      end;
      ad := ad+ins[op].q

   end;
   write(': ');
   wrthex(output, op, 2, true);
   write(' ', ins[op].mn:10, '  ');
   if ins[op].p then begin

      wrthex(output, p, 2, true);
      if ins[op].q > 0 then
        begin write(','); wrthex(output, q, inthex, true) end;
      if ins[op].q > intsize then
        begin write(','); wrthex(output, q1, inthex, true) end;
      if ins[op].q > intsize*2 then
        begin write(','); wrthex(output, q2, inthex, true) end

   end else if ins[op].q > 0 then begin

      write('   '); wrthex(output, q, inthex, true);
      if ins[op].q > intsize then
        begin write(','); wrthex(output, q1, inthex, true) end;
      if ins[op].q > intsize*2 then
        begin write(','); wrthex(output, q2, inthex, true) end

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
  if (flc mod algn) <> 0 then begin
    l := flc+1;
    flc := l - algn  +  (algn-l) mod algn
  end
end (*align*);

{ align upwards with space clear }
procedure alignuc(algn: address; var flc: address);
var flcs,ad: address;
begin
  flcs := flc; alignu(algn, flc); for ad := flcs to flc-1 do putbyt(ad, 0)
end;

{ align downwards with space clear }
procedure aligndc(algn: address; var flc: address);
var flcs,ad: address;
begin
  flcs := flc; alignd(algn, flc); for ad := flc to flcs-1 do putbyt(ad, 0)
end;

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
         flabelp = ^flabel;
         flabel = record
                       next: flabelp;
                       val: address;
                       ref: strvsp
                     end;
         cstfixrg  = 1..maxcstfx; { constant fixup range }
         gblfixrg  = 1..maxgblfx; { globals fixup range }

   var  word : array[alfainx] of char; ch  : char;
        labeltab: array[labelrg] of labelrec;
        labelvalue: address;
        iline: integer; { line number of intermediate file }
        cstfixtab: array [cstfixrg] of address;
        cstfixi: 0..maxcstfx;
        ci: cstfixrg;
        gblfixtab: array [gblfixrg] of address;
        gblfixi: 0..maxgblfx;
        gi: gblfixrg;
        gbloff: address; { load offset of globals }
        ad, ad2, crf: address;
        cp: address;  (* pointer to next free constant position *)
        npadr: address;
        sn: filnam;
        snl: 1..fillen;
        flablst: flabelp; { list of far labels }
        i: integer;

   procedure clrlab;
   var i: integer;
   begin
     for i:= 0 to maxlabel do
       with labeltab[i] do begin val:=-1; st:= entered end
   end;

   procedure init;
   begin
         sptable[ 0]  :='get       ';     sptable[ 1]  :='put       ';
         sptable[ 2]  :='thw       ';     sptable[ 3]  :='rln       ';
         sptable[ 4]  :='new       ';     sptable[ 5]  :='wln       ';
         sptable[ 6]  :='wrs       ';     sptable[ 7]  :='eln       ';
         sptable[ 8]  :='wri       ';     sptable[ 9]  :='wrr       ';
         sptable[10]  :='wrc       ';     sptable[11]  :='rdi       ';
         sptable[12]  :='rdr       ';     sptable[13]  :='rdc       ';
         sptable[14]  :='sin       ';     sptable[15]  :='cos       ';
         sptable[16]  :='exp       ';     sptable[17]  :='log       ';
         sptable[18]  :='sqt       ';     sptable[19]  :='atn       ';
         sptable[20]  :='---       ';     sptable[21]  :='pag       ';
         sptable[22]  :='rsf       ';     sptable[23]  :='rwf       ';
         sptable[24]  :='wrb       ';     sptable[25]  :='wrf       ';
         sptable[26]  :='dsp       ';     sptable[27]  :='wbf       ';
         sptable[28]  :='wbi       ';     sptable[29]  :='wbr       ';
         sptable[30]  :='wbc       ';     sptable[31]  :='wbb       ';
         sptable[32]  :='rbf       ';     sptable[33]  :='rsb       ';
         sptable[34]  :='rwb       ';     sptable[35]  :='gbf       ';
         sptable[36]  :='pbf       ';     sptable[37]  :='rib       ';
         sptable[38]  :='rcb       ';     sptable[39]  :='nwl       ';
         sptable[40]  :='dsl       ';     sptable[41]  :='eof       ';
         sptable[42]  :='efb       ';     sptable[43]  :='fbv       ';
         sptable[44]  :='fvb       ';     sptable[45]  :='wbx       ';
         sptable[46]  :='asst      ';     sptable[47]  :='clst      ';
         sptable[48]  :='pos       ';     sptable[49]  :='upd       ';
         sptable[50]  :='appt      ';     sptable[51]  :='del       ';
         sptable[52]  :='chg       ';     sptable[53]  :='len       ';
         sptable[54]  :='loc       ';     sptable[55]  :='exs       ';
         sptable[56]  :='assb      ';     sptable[57]  :='clsb      ';
         sptable[58]  :='appb      ';     sptable[59]  :='hlt       ';
         sptable[60]  :='ast       ';     sptable[61]  :='asts      ';
         sptable[62]  :='wrih      ';     sptable[63]  :='wrio      ';
         sptable[64]  :='wrib      ';     sptable[65]  :='wrsp      ';
         sptable[66]  :='wiz       ';     sptable[67]  :='wizh      ';
         sptable[68]  :='wizo      ';     sptable[69]  :='wizb      ';
         sptable[70]  :='rds       ';     sptable[71]  :='ribf      ';
         sptable[72]  :='rdif      ';     sptable[73]  :='rdrf      ';
         sptable[74]  :='rcbf      ';     sptable[75]  :='rdcf      ';
         sptable[76]  :='rdsf      ';     sptable[77]  :='rdsp      ';
         sptable[78]  :='aeft      ';     sptable[79]  :='aefb      ';
         sptable[80]  :='rdie      ';     sptable[81]  :='rdre      ';
         sptable[82]  :='rdx       ';     sptable[83]  :='rdxf      ';
         sptable[84]  :='rxb       ';     sptable[85]  :='rxbf      ';
         sptable[86]  :='rdsc      ';     sptable[87]  :='rdih      ';
         sptable[88]  :='rdio      ';     sptable[89]  :='rdib      '; 
         sptable[90]  :='rifh      ';     sptable[91]  :='rifo      ';
         sptable[92]  :='rifb      ';     sptable[93]  :='ribh      '; 
         sptable[94]  :='ribo      ';     sptable[95]  :='ribb      ';
         sptable[96]  :='rbfh      ';     sptable[97]  :='rbfo      '; 
         sptable[98]  :='rbfb      ';     sptable[99]  :='rdxh      ';
         sptable[100] :='rdxo      ';     sptable[101] :='rdxb      '; 
         sptable[102] :='rxfh      ';     sptable[103] :='rxfo      ';
         sptable[104] :='rxfb      ';     sptable[105] :='rxbh      '; 
         sptable[106] :='rxbo      ';     sptable[107] :='rxbb      ';
         sptable[108] :='rbxh      ';     sptable[109] :='rbxo      '; 
         sptable[110] :='rbxb      ';     sptable[111] :='sete      '; 

         { constants are stored at top of memory, but relocated to the top of
           the code deck }
         cp := maxtop; { set constants pointer to top of storage }
         for i:= 1 to 10 do word[i]:= ' ';
         clrlab;
         cstfixi := 0; { set no constant fixups }
         gblfixi := 0; { set no global fixups }
         npadr := -1;
         iline := 1; { set 1st line of intermediate }
         gbset := false; { global size not set }
         gbloff := 0; { set global offset }
         flablst := nil { clear far label list }
   end;(*init*)

   procedure errorl(string: beta); (*error in loading*)
   begin writeln;
      writeln('*** Program load error: [', iline:1, '] ', string);
      goto 99
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
          q : address;  (*instruction register*)
   begin
      if labeltab[x].st=defined then errorl('duplicated label         ')
      else begin
             if labeltab[x].val<>-1 then (*forward reference(s)*)
             begin curr:= labeltab[x].val; endlist:= false;
                while not endlist do begin
                      ad := curr;
                      q := getadr(ad);
                      succ:= q; { get target address from that }
                      q:= labelvalue; { place new target address }
                      ad := curr;
                      putadr(ad, q);
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

   procedure getlab;
   var i: 1..fillen;
   begin skpspc; for i := 1 to fillen do sn[i] := ' '; snl := 1;
     if not (ch in ['a'..'z','A'..'Z','_']) then
       errorl('Symbols format error     ');
     while ch in ['a'..'z','A'..'Z','0'..'9','_'] do begin
       if snl >= fillen then errorl('Symbols format error     ');
       sn[snl] := ch; getnxt; snl := snl+1
     end;
     snl := snl-1
   end;

   procedure getsds;
   var i: 1..fillen;
   begin skpspc; for i := 1 to fillen do sn[i] := ' '; snl := 1;
     if ch = ' ' then errorl('Symbols format error     ');
     while ch <> ' ' do begin
       if snl >= fillen then errorl('Symbols format error     ');
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
       getnxt; strassvf(fl, sn); strchrass(fl, snl+1, '.'); i := snl+2; getsds;
       for j := 1 to snl do begin strchrass(fl, i, sn[j]); i := i+1 end
     end
   end;

   procedure gblrlc(off: address; crf: address);
   var sp: psymbol; bp: pblock;
   begin
     if blklst <> nil then begin { there is a top block }
       bp := blklst;
       while bp <> nil do begin
         if bp^.btyp in [btprog, btmod] then begin { it is a module }
           sp := bp^.symbols;
           while sp <> nil do begin
             if sp^.styp = stglobal then sp^.off := sp^.off+off
             else if sp^.styp = stfixg then sp^.off := sp^.off+crf;
             sp := sp^.next
           end
         end;
         bp := bp^.next
       end
     end
   end;

   { relocate far labels }
   procedure flabrlc;
   var ad: address; flp: flabelp; c: char; rt: integer; symerr: boolean;
   { find symbols reference }
   function symref(lsp: strvsp): address;
   var mods, syms: filnam; i,x: 1..fillen; bp, fbp: pblock; sp, fsp: psymbol;
   begin
     { break name into module.symbol components }
     for i := 1 to fillen do begin mods[i] := ' '; syms[i] := ' ' end;
     i := 1;
     c := strchr(lsp, i);
     while c <> '.' do
       begin mods[i] := c; i := i+1; c := strchr(lsp, i) end;
     i := i+1; c := strchr(lsp, i); x := 1;
     while c <> ' ' do
       begin syms[x] := c; i := i+1; x := x+1; c := strchr(lsp, i) end;
     bp := blklst; fbp := nil; { find module }
     while bp <> nil do begin
       if strequvf(bp^.name, mods) then begin fbp := bp; bp := nil end
       else bp := bp^.next
     end;
     if fbp = nil then errorl('Module not present       ');
     sp := fbp^.symbols; fsp := nil;
     while sp <> nil do begin
       if strequvf(sp^.name, syms) then begin fsp := sp; sp := nil end
       else sp := sp^.next
     end;
     if fsp <> nil then begin
       if not (fsp^.styp in [stglobal, stfixg]) then 
         errorl('Symbol not global        ');
       symref := fsp^.off { return address of symbol }
     end else begin { search for routine }
       bp := fbp^.incnxt; fbp := nil;
       while bp <> nil do begin
         if strequvf(bp^.name, syms) then begin fbp := bp; bp := nil end
         else bp := bp^.next
       end;
       if fbp = nil then begin
         write('Symbol not present: '); writevp(output, lsp); writeln;
         symref := 0;
         symerr := true
       end else symref := fbp^.bstart
     end
   end;

   { find external routine, if exists }
   function extref(lsp: strvsp): integer;
   var mods, syms: symnam; rt: integer; i,x: integer;
   begin
     { break name into module.symbol components }
     for i := 1 to maxsym do begin mods[i] := ' '; syms[i] := ' ' end;
     i := 1;
     c := strchr(lsp, i);
     while c <> '.' do
       begin mods[i] := c; i := i+1; c := strchr(lsp, i) end;
     i := i+1; c := strchr(lsp, i); x := 1;
     while c <> ' ' do
       begin syms[x] := c; i := i+1; x := x+1; c := strchr(lsp, i) end;
     rt := 0;
     LookupExternal(mods, syms, rt);
     extref := rt
   end;

   begin { flabrlc }
     symerr := false; { set no symbols error }
     while flablst <> nil do begin { empty far label list }
       flp := flablst; flablst := flablst^.next;
       ad := flp^.val; op := store[ad];
       rt := extref(flp^.ref);
       if rt > 0 then putadr(ad, rt+extvecbase-1)
       else putadr(ad, symref(flp^.ref));
       dispose(flp)
     end;
     if symerr then errorl('Missing symbols found    ');
   end;

   function isprog: boolean;
   begin
     if blkstk <> nil then { there is a top block }
       isprog := blkstk^.btyp = btprog
     else isprog := false
   end;

   procedure assemble; forward;

   procedure generate;(*generate segment of code*)
      var x: integer; (* label number *)
          again: boolean;
          c,ch1: char;
          i,l: integer;
          ext: packed array [1..4] of char;
          bp: pblock;
          sp: psymbol;
          ad: address;
          sgn: boolean;
          ls, ds: strvsp;
          csttab: boolean;
          cstadr: address;
          cstoff: address;
          r: real;
          s: settype;
          optst: optstr; oni: optinx; oi: 1..maxopt;

   procedure gblrlc;
   var sp: psymbol;
   begin
     if blkstk <> nil then begin { there is a top block }
       if blkstk^.btyp in [btprog, btmod] then begin { it is a module }
         sp := blkstk^.symbols;
         while sp <> nil do begin
           if sp^.styp = stglobal then sp^.off := sp^.off+gbloff;
           sp := sp^.next
         end
       end
     end
   end;
   
   procedure stripext(var ds: strvsp; ss: strvsp);
   var i, x: integer;
   begin
     i := 1; ds := nil;
     while not (strchr(ss, i) in [' ', '.']) do i := i+1;
     if strchr(ss, i) = '.' then begin
       x := 1; i := i+1;
       while strchr(ss, i) <> ' ' do 
         begin strchrass(ds, x, strchr(ss, i)); i := i+1; x := x+1 end
     end
   end;

   procedure fndsym(sn: strvsp; var sp: psymbol);
   var lp: psymbol;
   begin
     sp := nil;
     if blkstk <> nil then begin { there is a top block }
       lp := blkstk^.symbols;
       while lp <> nil do begin
         if strequvv(lp^.name, sn) then begin sp := lp; lp := nil end
         else lp := lp^.next
       end
     end
   end;

   begin (*generate*)
     again := true; csttab := false;
     while again do begin
       if eof(prd) then errorl('unexpected eof on input  ');
       getnxt;(* first character of line*)
       if not (ch in ['!', 'l', 'q', ' ', ':', 'o', 'g', 'b',
                      'e', 's', 'f','v','t','n','c','x','p','r']) then
         errorl('unexpected line start    ');
       case ch of
         '!': getlin; { comment }
         'l': begin getnxt; parlab(x,ls);
                    if ls <> nil then
                      errorl('Invalid intermediate     ');
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
                 { place in line tracking }
                 if curmod <> nil then 
                   if curmod^.lintrk^[x] = -1 then curmod^.lintrk^[x] := pc;
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
                repeat
                  while not eoln(prd) and (ch = ' ') do getnxt;
                  if not (ch in ['a'..'z', 'A'..'Z', '_']) then
                    errorl('No valid option found    ');
                  oni := 1; optst := '          ';
                  while ch in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do begin
                    ch1 := lcase(ch);
                    if optst[oni] = ' ' then optst[oni] := ch1;
                    if oni < optlen then oni := oni+1;
                    getnxt
                  end;
                  oi := 1;
                  while (oi < maxopt) and (optst <> opts[oi]) and (optst <> optsl[oi]) do
                    oi := oi+1;
                  if (optst = opts[oi]) or (optst = optsl[oi]) then begin
                    option[oi] := ch = '+'; 
                    if (ch = '+') or (ch = '-') then getnxt;
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
                      29: begin amd64_sysv := option[oi];
                            if amd64_sysv then
                              errorl('Call convention mismatch ')
                          end;
                      2:; 3:; 4:; 12:; 20:; 21:; 22:;
                      24:; 25:; 26:; 10:; 18:; 27:; 28:;
                    end
                  end else errorl('No valid option found    ');
                  while not eoln(prd) and (ch = ' ') do getnxt
                until not (ch in ['a'..'z']);
                getlin
              end;
         'g': begin read(prd,i);
                    { if not program, adjust so files and
                      exceptions from all other modules merge into
                      program }
                    if not isprog then i := i-exceptiontop;
                    gblrlc; gbset := true;
                    gbloff := gbloff+i; gbsiz := gbsiz+i;
                    getlin end;
         'b': begin
                getnxt; skpspc;
                if not (ch in ['p', 'm', 'r', 'f']) then
                  errorl('Block type is invalid    ');
                ch1 := ch; { save block type }
                getnxt; skpspc; getsds;
                new(bp); strassvf(bp^.name, sn);
                { get basename, without type }
                l := 1;
                while (l < fillen) and (sn[l] <> '@') do l := l+1;
                if sn[l] = '@' then strassvfl(bp^.bname, sn, l-1)
                else strassvf(bp^.bname, sn); { just use whole name }
                bp^.symbols := nil;
                bp^.incnxt := nil;
                case ch1 of { block type }
                  'p': bp^.btyp := btprog;
                  'm': bp^.btyp := btmod;
                  'r': bp^.btyp := btproc;
                  'f': bp^.btyp := btfunc
                end;
                bp^.bend := -1;
                if blkstk <> nil then begin
                  { process block inclusions }
                  bp^.incnxt := blkstk^.incnxt; { insert to list }
                  blkstk^.incnxt := bp
                end;
                { put onto block stack }
                bp^.next := blkstk; blkstk := bp;
                bp^.fname := nil;
                { check module }
                if bp^.btyp in [btprog, btmod] then begin
                  { create line track data for module }
                  new(bp^.lintrk); new(bp^.linprf);
                  { clear source line tracking }
                  for i := 1 to maxsrc do
                    bp^.lintrk^[i] := -1;
                  { clear line profiling }
                  for i := 1 to maxsrc do bp^.linprf^[i] := 0;
                  { has to have room for extention }
                  if snl >= fillen-4 then
                    errorl('Block name too long      ');
                  { add file extension }
                  ext := extsrc;
                  for i := 1 to 4 do begin
                    sn[snl+1] := ext[i]; snl := snl+1
                  end;
                  { place as source file }
                  strassvf(bp^.fname, sn);
                  curmod := bp { set this block current }
                end;
                blkstk^.bestart := pc; { set enclosure start }
                blkstk^.bstart := pc; { set start address }
                getlin
              end;
         'e': begin { end block }
                getnxt; skpspc;
                if not (ch in ['p', 'm', 'r', 'f']) then
                  errorl('Block type is invalid    ');
                if ch in ['p','m'] then begin { end module }
                  clrlab; { clear near labels }
                  { if this module active, clear it }
                  if curmod = blkstk then curmod := nil
                end;
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
                if not (ch in ['g', 'l','p','f','c']) then
                  errorl('Symbol type is invalid   ');
                case ch of
                  'g': sp^.styp := stglobal;
                  'p': sp^.styp := stparam;
                  'l': sp^.styp := stlocal;
                  'f': sp^.styp := stfixg;
                  'c': sp^.styp := stfixl
                end;
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
                sp^.off := ad; getsds;
                strassvf(sp^.digest, sn);
                if blkstk = nil then
                  errorl('Symbol not in block      ');
                { place in block symbol list }
                sp^.next := blkstk^.symbols;
                blkstk^.symbols := sp;
                getlin
              end;
         'f': begin { faults (errors) }
                read(prd,i); errsinprg := errsinprg+i; getlin
              end;
         'v': begin { variant logical table }
                getnxt; skpspc;
                if ch <> 'l' then
                  errorl('Label format error       ');
                getnxt; parlab(x,ls);
                if ls <> nil then
                  errorl('Invalid intermediate     ');
                read(prd,l); cp := cp-(l*intsize+intsize);
                ad := cp; putint(ad, l); ad := ad+intsize;
                while not eoln(prd) do begin
                  read(prd,i); putint(ad, i); ad := ad+intsize;
                end;
                labelvalue:=cp;
                update(x);
                getlin
              end;
         't': begin { fixed template }
                getnxt; skpspc;
                if ch <> 'l' then
                  errorl('Label format error       ');
                getnxt; parlab(x,ls);
                if ls <> nil then
                  errorl('Invalid intermediate     ');
                read(prd,l); cp := cp-(l*intsize); ad := cp;
                while not eoln(prd) do begin
                  read(prd,i); putint(ad, i); ad := ad+intsize;
                  { this is a gpc compiler bug, \cr is passing the eoln filter }
                  while not eoln(prd) and (prd^ <= ' ') do get(prd)
                end;
                labelvalue:=cp;
                update(x);
                getlin
              end;
         'n': begin { start constant table }
                if csttab then
                  errorl('Already in constant table');
                csttab := true; { flag in table }
                getnxt; skpspc;
                if ch <> 'l' then
                  errorl('Label format error       ');
                getnxt; parlab(x,ls);
                { Note the constant table must start on maximium
                  natural alignment to match it's structure type.
                  We use stackal for that. }
                read(prd,l); cp := cp-l; alignd(stackal, cp);
                cstadr := cp; labelvalue:=cstadr; cstoff := 0;
                if ls <> nil then begin
                  stripext(ds, ls); fndsym(ds, sp); 
                  if sp = nil then errorl('Symbol not found         ');
                  sp^.off := cstadr
                end else update(x);
                getlin
                { note mixed constants with other operands is
                  neither encouraged nor forbidden }
              end;
         'x': begin
                if not csttab then errorl('No constant table active ');
                csttab := false;
                getlin
              end;
         'c': begin
                if not csttab then errorl('No constant table active ');
                getnxt; skpspc;
                if not (ch in ['i','r','p','s','c','b','x'])
                  then errorl('Invalid const table type ');
                case ch of { constant type }
                  'i': begin
                         getnxt; read(prd,i); alignu(intal, cstoff);
                         putint(cstadr+cstoff,i); cstoff := cstoff+intsize
                       end;
                  'r': begin
                         getnxt; read(prd,r); alignu(realal, cstoff);
                         putrel(cstadr+cstoff,r); cstoff := cstoff+realsize
                       end;
                  'p': begin
                         getnxt; skpspc;
                         if ch <> '(' then errorl('''('' expected for set     ');
                         s := [ ]; getnxt;
                         while ch<>')' do
                           begin read(prd,i); getnxt; s := s + [i] end;
                         alignu(setal, cstoff);
                         putset(cstadr+cstoff,s); cstoff := cstoff+setsize
                       end;
                  's': begin
                         getnxt; skpspc;
                         if ch <> '''' then errorl('quote expected for string');
                         getnxt; alignu(charal, cstoff);
                         while ch<>'''' do
                           begin putchr(cstadr+cstoff, ch); getnxt; 
                                 cstoff := cstoff+charsize end
                       end;
                  'c': begin
                         getnxt;
                         { chars are output as values }
                         read(prd,i); alignu(charal, cstoff);
                         putchr(cstadr+cstoff,chr(i)); cstoff := cstoff+charsize
                       end;
                  'b': begin
                         getnxt;
                         { booleans are output as values }
                         read(prd,i); alignu(boolal, cstoff);
                         putbyt(cstadr+cstoff,i); cstoff := cstoff+boolsize
                       end;
                  'x': begin
                         getnxt; read(prd,i);
                         putbyt(cstadr+cstoff,i); cstoff := cstoff+1
                       end;
                end;
                getlin
              end;
         'r': begin
                if not csttab then errorl('No constant table active ');
                cstadr := cstadr+cstoff; cstoff := 0;
                getlin
              end;
         'p': getlin; { ignore filename/path }
       end;
     end
   end; (*generate*)

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

   procedure assemble; (*translate symbolic code into machine code and store*)
      var name :alfa; r :real; s :settype;
          i,x,s1,lb,ub,l:integer; c: char;
          str: packed array [1..stringlgth] of char; { buffer for string constants }

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

      procedure putcstfix;
      begin
        if cstfixi = maxcstfx then errorl('Too many constants in pgm');
        cstfixi := cstfixi+1; cstfixtab[cstfixi] := pc
      end;

      procedure putgblfix;
      begin
        if gblfixi = maxgblfx then errorl('Too many globals in pgm  ');
        gblfixi := gblfixi+1; gblfixtab[gblfixi] := pc
      end;

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

   begin  p := 0;  q := 0;  op := 0; (*assemble*)
      getname;
      { note this search removes the top instruction from use }
      while (ins[op].mn<>name) and (op < maxins) do op := op+1;
      if (ins[op].mn<>name) then errorl('illegal instruction      ');
      case ins[op].mi of  (* get parameters p,q *)

          pi_lodi, pi_lodx, pi_loda, pi_lodr,
          pi_lods, pi_lodb, pi_lodc, pi_strx,
          pi_stri, pi_stra, pi_strr, pi_strs,
          pi_strb, pi_strc, pi_lda, pi_lip,
          pi_sev:
            begin read(prd,p,q); storeop;
              storep; storeq
            end;

          pi_cup,pi_cuf: begin storeop; labelsearch; storeq end;

          pi_sfr: begin storeop; labelsearch; storeq end;
          pi_mst:
            begin read(prd,p); storeop; storep;
              labelsearch; storeq;
              labelsearch; storeq
            end;

          pi_suv: begin storeop; labelsearch; storeq;
                     while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
                     if prd^ = 'l' then begin getnxt; labelsearch end
                     else read(prd,q);
                     if q > exceptiontop then q := q+gbloff;
                     putgblfix; storeq end;

          pi_equm, pi_neqm, pi_geqm, pi_grtm, pi_leqm, pi_lesm,

          pi_ixa,pi_mov,pi_dmp,pi_swp,

          pi_indx, pi_indi, pi_inda, pi_indr,
          pi_inds, pi_indb, pi_indc, pi_inci,
          pi_inca, pi_incb, pi_incc, pi_deci,
          pi_decb, pi_decc, pi_ckvi, pi_cpc,
          pi_aps, pi_ckvb, pi_ckvc, pi_incx,
          pi_decx, pi_ckvx, pi_cxs, pi_max,
          pi_retm, pi_lsa, pi_vbs,
          pi_mdc:
            begin read(prd,q); storeop;
              storeq
            end;

          pi_ldoi, pi_ldox, pi_ldoa, pi_ldor, pi_ldos, pi_ldob, pi_ldoc,
          pi_sroi,pi_srox,pi_sroa,pi_sror,pi_sros,pi_srob,pi_sroc,
          pi_lao: begin while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
                   storeop;
                   if prd^ = 'l' then begin getnxt; labelsearch end
                   else read(prd,q);
                   if q > exceptiontop then q := q+gbloff;
                   putgblfix; storeq end;

          pi_cuv, pi_cvf:
            begin while not eoln(prd) and
              (prd^ = ' ') do read(prd,ch);
                   storeop;
                   if prd^ = 'l' then begin getnxt; labelsearch end
                   else read(prd,q);
                   if q > exceptiontop then q := q+gbloff;
                   putgblfix; storeq end;

          pi_ltci,pi_ltcr,pi_ltcs,pi_ltcb,pi_ltcc,pi_ltcx,
          pi_lto: begin while not eoln(prd) and (prd^ = ' ') do read(prd,ch);
                 case ins[op].mi of
                   pi_ltci: op := ord(pi_ldoi);
                   pi_ltcr: op := ord(pi_ldor);
                   pi_ltcs: op := ord(pi_ldos);
                   pi_ltcb: op := ord(pi_ldob);
                   pi_ltcc: op := ord(pi_ldoc);
                   pi_ltcx: op := ord(pi_ldox);
                   pi_lto: op := ord(pi_lao);
                 end;
                 storeop;
                 if prd^ <> 'l' then errorl('Instr must have label    ');
                 getnxt; labelsearch; putcstfix; storeq
               end;

          pi_pck, pi_upk, pi_vis, pi_vip,
          pi_apc, pi_cxc, pi_ccs, pi_vin,
          pi_stom, pi_ctb, pi_cpp, pi_cpr,
          pi_sfs:
            begin read(prd,q); read(prd,q1);
              storeop; storeq; storeq1
            end;

          pi_cta, pi_ivti, pi_cvbi, pi_ivtx,
          pi_ivtb, pi_ivtc, pi_cvbx, pi_cvbb,
          pi_cvbc: begin
            read(prd,q); read(prd,q1); storeop; storeq; storeq1; labelsearch;
            putcstfix; storeq
          end;

          pi_lft: begin storeop; labelsearch; putcstfix; storeq end;

          pi_ujp, pi_fjp, pi_xjp, pi_tjp,
          pi_bge, pi_cal:
            begin storeop; labelsearch;
              storeq
            end;

          pi_ipj, pi_lpa:
            begin read(prd,p); storeop; storep;
              labelsearch; storeq
            end;

          pi_csp: begin skpspc; getname;
                           while name<>sptable[q] do
                           begin q := q+1; if q > maxsp then
                                 errorl('std proc/func not found  ')
                           end; storeop;
                           if pc+1 > cp then
                             errorl('Program code overflow    ');
                           store[pc] := q; putdef(pc, true); pc := pc+1
                      end;

          pi_ldcs, pi_ldci, pi_ldcr,
          pi_ldcn, pi_ldcb,
          pi_ldcc:
            begin case ins[op].mi of  (*get q*)
                           pi_ldci: begin read(prd,i); storeop;
                                      if pc+intsize > cp then
                                         errorl('Program code overflow    ');
                                      putint(pc, i); pc := pc+intsize
                                end;

                           pi_ldcr: begin read(prd,r);
                                      aligndc(realal, cp);
                                      cp := cp-realsize;
                                      if cp <= 0 then
                                         errorl('constant table overflow  ');
                                      putrel(cp, r); q := cp;
                                      storeop; putcstfix; storeq
                                end;

                           pi_ldcn: storeop; (*p,q = 0*)

                           pi_ldcb: begin read(prd,q); storeop;
                                      if pc+1 > cp then
                                        errorl('Program code overflow    ');
                                      putbol(pc, q <> 0); pc := pc+1 end;

                           pi_ldcc: begin
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
                           pi_ldcs: begin skpspc;
                                   if ch <> '(' then
                                     errorl('ldcs() expected          ');
                                   s := [ ];  getnxt;
                                   while ch<>')' do
                                   begin read(prd,s1); getnxt; s := s + [s1]
                                   end;
                                   aligndc(setal, cp);
                                   cp := cp-setsize;
                                   if cp <= 0 then
                                      errorl('constant table overflow  ');
                                   putset(cp, s);
                                   q := cp;
                                   storeop; putcstfix; storeq
                                end
                           end (*case*)
                     end;

           pi_chki, pi_chka, pi_chks, pi_chkb,
           pi_chkc, pi_ckla, pi_chkx,
           pi_cjp: begin
                         read(prd,lb,ub); storeop;
                         if ins[op].mi = pi_cjp then
                           begin labelsearch; q1 := q end;
                         if (ins[op].mi = pi_chka) or
                            (ins[op].mi = pi_ckla) then begin
                           q := lb; storeq
                         end else begin
                           aligndc(setal, cp);
                           cp := cp-intsize;
                           if cp <= 0 then errorl('constant table overflow  ');
                           putint(cp, ub);
                           cp := cp-intsize;
                           if cp <= 0 then errorl('constant table overflow  ');
                           putint(cp, lb); q := cp;
                           putcstfix; storeq
                         end;
                         if ins[op].mi = pi_cjp then storeq1
                       end;

           pi_lca: begin read(prd,l); skpspc;
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
                         cp := cp-l;
                         if cp <= 0 then errorl('constant table overflow  ');
                         q := cp;
                         for x := 1 to l do putchr(q+x-1, str[x]);
                         { this should have worked, the for loop is faulty
                           because the calculation for end is done after the i
                           set
                         for i := 0 to i-1 do putchr(q+i, str[i+1]);
                         }
                         storeop; putcstfix; storeq
                       end;

          pi_retp, pi_reti, pi_retr, pi_retc, pi_retb, pi_reta, pi_retx,
          pi_rets: begin read(prd,q); storeop; storeq end;

          pi_rip: begin storeop; labelsearch; storeq end;

          pi_equa, pi_equi, pi_equr, pi_equb, pi_equs, pi_equc,
          pi_neqa, pi_neqi, pi_neqr, pi_neqb, pi_neqs, pi_neqc,
          pi_brk, pi_geqi, pi_geqr, pi_geqb, pi_geqs, pi_geqc,
          pi_grti, pi_grtr, pi_grtb, pi_grts, pi_grtc,
          pi_leqi, pi_leqr, pi_leqb, pi_leqs, pi_leqc,
          pi_lesi, pi_lesr, pi_lesb, pi_less, pi_lesc,

          pi_ordi, pi_ordb, pi_ordc, pi_ordx,

          pi_stoi, pi_stoa, pi_stor, pi_stos,
          pi_stob, pi_stoc, pi_stox,

          pi_adi, pi_adr, pi_sbi, pi_sbr,
          pi_sgs, pi_flt, pi_flo, pi_trc,
          pi_ngi, pi_ngr, pi_sqi, pi_sqr,
          pi_abi, pi_abr, pi_notb, pi_and,
          pi_ior, pi_dif, pi_int, pi_uni,
          pi_inn, pi_mod, pi_odd, pi_mpi,
          pi_mpr, pi_dvi, pi_dvr, pi_chr,
          pi_rnd, pi_rgs, pi_noti, pi_xor,
          pi_ede, pi_mse, pi_lcp, pi_cps,
          pi_equv, pi_neqv, pi_lesv, pi_grtv,
          pi_leqv, pi_geqv, pi_vdp, pi_spc,
          pi_scp, pi_ldp, pi_vdd,
          pi_wbs, pi_wbe,

          { dupi, dupa, dupr, dups, dupb, dupc, cks, cke, inv, cal, vbe, cip, 
            cif, cpl }
          pi_dupi, pi_dupa, pi_dupr, pi_dups,
          pi_dupb, pi_dupc, pi_cks, pi_cke,
          pi_inv, pi_ret, pi_vbe, pi_cip,
          pi_cif, pi_cpl: storeop;
         
          { lsp is same as ldp }
          pi_lsp: begin op := ord(pi_ldp); storeop end;


                      (*ujc must have same length as ujp, so we output a dummy
                        q argument*)
          pi_ujc: begin storeop; q := 0; storeq end;

          pi_mpc{mpc}:; { does nothing in the interpreter }

          pi_s2c: begin read(prd,q); storeop; storeq end;

      end; (*case*)

      getlin { next intermediate line }
   end; (*assemble*)

begin (*load*)
  init;
  extvecs := 0;
  extvecs := NumExternal;
  pc := 0;
  { insert start sequence:

    lnp
    call skip
    stp
    extvectors
    mstack:

  }
  op := 20; storeop; q := 0; npadr := pc; storeq; { lnp }
  op := 21; storeop; q := pc+intsize+1+extvecs; storeq; { call mstack }
  op := 58; storeop; { stp }
  { place the external vectors table }
  extvecbase := pc; op := 242;
  for i := 1 to extvecs do storeop;
  generate;
  if not gbset then errorl('global space not set     ');
  pctop := pc; { save top of code store }
  lsttop := pctop; { save as top of listing }
  alignuc(gbsal, pctop); { align end of code block }
  { relocate constants }
  ad2 := pctop; crf := pctop-cp; cststr := ad2;
  for ad := cp to maxstr do
    begin store[ad2] := store[ad]; putdef(ad2, getdef(ad)); ad2 := ad2+1 end;
  pctop := ad2; { move globals to the top of that }
  alignuc(gbsal, pctop); { align end of constants block }
  gbtop := pctop+gbsiz;
  alignu(gbsal, gbtop);
  { relocate constants deck }
  if cstfixi >= 1 then for ci := 1 to cstfixi do
    begin ad := cstfixtab[ci]; putadr(ad, getadr(ad)+crf) end;
  { relocate global references }
  if gblfixi >= 1 then for gi := 1 to gblfixi do
    begin
    ad := gblfixtab[gi]; putadr(ad, getadr(ad)+pctop) end;
  gblrlc(pctop, crf); { relocate symbols as well }
  { set heap bottom pointer }
  if npadr < 0 then errorl('Heap bottom not set      ');
  putadr(npadr, gbtop);
  flabrlc; { link far labels }
  if dodmplab then dmplabs { Debug: dump label definitions }
end; (*load*)

(*------------------------------------------------------------------------*)

{ runtime handlers }

procedure wrtnewline;
begin
  if not newline then begin writeln; newline := true end
end;

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

override procedure valfil(fa: address); { attach file to file entry }
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
      write('addr: '); wrthex(output, ad, maxdigh, true);
      write(': ', abs(l):6, ': ');
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
var ad, ad1, l, l1: address;
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

override procedure newspc(len: address; var blk: address);
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
     if np > ep then errore(SpaceAllocateFail);
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
   else if (blk < gbtop) or (blk > np) then errorv(BadPointerValue);
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
   var i, j, k, w, l, f: integer;
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
       rx: integer;

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

   procedure readi(fn: fileno; var i: integer; var w: integer; fld: boolean; 
                   r: integer);
   var s: integer; d: integer;
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
   function valchr: boolean;
   begin
     valchr := 
       ((chkbuf in ['0'..'9','_']) and (r = 10)) or
       ((lcase(chkbuf) in ['0'..'9','a'..'f','_']) and (r = 16)) or
       ((chkbuf in ['0'..'1','_']) and (r = 2)) or
       ((chkbuf in ['0'..'7','_']) and (r = 8))
   end;
   begin
      s := +1; { set sign }
      { skip leading spaces }
      while (chkbuf = ' ') and not chkend do getbuf;
      if not (chkbuf in ['+', '-', '0'..'9','$','%','&']) then
        errore(InvalidIntegerFormat);
      if chkbuf = '$' then begin getbuf; r := 16 end
      else if chkbuf = '%' then begin getbuf; r := 2 end
      else if chkbuf = '&' then begin getbuf; r := 8 end
      else if chkbuf = '+' then getbuf
      else if chkbuf = '-' then begin getbuf; s := -1 end;
      if not valchr then errore(InvalidIntegerFormat);
      i := 0; { clear initial value }
      while valchr do begin { parse digit }
        while (chkbuf = '_') and not chkend do getbuf;
        if valchr then begin
          if chkbuf in ['0'..'9'] then d := ord(chkbuf)-ord('0')
          else d := ord(lcase(chkbuf))-ord('a')+10;
          if (i > pmmaxint div r) or
             ((i = pmmaxint div r) and (d > pmmaxint mod r)) then
            errore(IntegerValueOverFlow);
          i := i*r+d; { add in new digit }
          getbuf
        end
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
            readi(fn, i, w, fld, 10); { get exponent }
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

   procedure readsc(fn: fileno; ad: address; l: integer);
   var c: char;
   begin
     while (l > 0) and not eolnfn(fn) do begin
       if eoffn(fn) then errore(EndOfFile);
       read(filtable[fn], c); 
       if c <> getchr(ad) then errorv(ReadCharacterMismatch);
       ad := ad+1; l := l-1
     end
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
         while (l > 1) and (getchr(ad1) = ' ') do
           begin ad1 := ad1-1; l := l-1 end;
         if getchr(ad1) <> ' ' then
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

      { trace routine executions }
      if dotrcrot then 
        begin wrtnewline; writeln(pc:6, '/', sp:6, '-> ', q:2) end;

      case q of
           0 (*get*): begin popadr(ad); valfil(ad); fn := store[ad];
                        if varinc(ad+fileidsize, ad+fileidsize) then
                          errorv(VarReferencedFileBufferModified);
                        getfn(fn)
                      end;
           1 (*put*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                              outputfn: begin putfile(output, ad, fn);
                                              newline := false end;
                              prrfn: putfile(prr, ad, fn);
                              errorfn: begin putfile(output, ad, fn);
                                             newline := false end;
                              listfn: begin putfile(output, ad, fn);
                                            newline := false end;
                              inputfn,prdfn,
                              commandfn: errore(WriteOnReadOnlyFile)
                           end else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                putfile(filtable[fn], ad, fn)
                           end
                      end;
           3 (*rln*): begin popadr(ad); valfil(ad); fn := store[ad];
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
           5 (*wln*): begin popadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                              outputfn: begin writeln(output);
                                              newline := true end;
                              prrfn: writeln(prr);
                              errorfn: begin writeln(output);
                                             newline := true end;
                              listfn: begin writeln(output);
                                            newline := true end;
                              prdfn,inputfn,
                              commandfn: errore(WriteOnReadOnlyFile)
                           end else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                writeln(filtable[fn])
                           end
                      end;
           6 (*wrs*): begin popint(w); popadr(ad1); popint(l);
                           popadr(ad); valfil(ad); fn := store[ad];
                           if (w < 1) and iso7185 then
                             errore(InvalidFieldSpecification);
                           if fn <= commandfn then case fn of
                              outputfn: begin writestr(output, ad1, w, l);
                                              newline := false end;
                              prrfn: writestr(prr, ad1, w, l);
                              errorfn: begin writestr(output, ad1, w, l);
                                             newline := false end;
                              listfn: begin writestr(output, ad1, w, l);
                                            newline := false end;
                              prdfn,inputfn,
                              commandfn: errore(WriteOnReadOnlyFile);
                           end else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                writestr(filtable[fn], ad1, w, l)
                           end;
                      end;
           65 (*wrsp*): begin popadr(ad1); popint(l);
                           popadr(ad); valfil(ad); fn := store[ad];
                           if fn <= commandfn then case fn of
                             outputfn: begin writestrp(output, ad1, l);
                                             newline := false end;
                             prrfn: writestrp(prr, ad1, l);
                             errorfn: begin writestrp(output, ad1, l);
                                            newline := false end;
                             listfn: begin writestrp(output, ad1, l);
                                           newline := false end;
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
           69 (*wizb*): begin popint(w); popint(i); popadr(ad);
                            rd := 10;
                            if (q = 62) or (q = 67) then rd := 16
                            else if (q = 63) or (q = 68) then rd := 8
                            else if (q = 64) or (q = 69) then rd := 2;
                            lz := (q >= 66) and (q <= 69);
                            valfil(ad); fn := store[ad];
                            if (w < 1) and iso7185 then
                              errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: begin writei(output, i, w, rd, lz);
                                                 newline := false end;
                                 prrfn: writei(prr, i, w, rd, lz);
                                 errorfn: begin writei(output, i, w, rd, lz);
                                                newline := false end;
                                 listfn: begin writei(output, i, w, rd, lz);
                                               newline := false end;
                                 prdfn,inputfn,
                                 commandfn: errore(WriteOnReadOnlyFile)
                              end
                            else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                writei(filtable[fn], i, w, rd, lz)
                            end
                      end;
           9 (*wrr*): begin popint(w); poprel(r); popadr(ad);
                            valfil(ad); fn := store[ad];
                            if w < 1 then errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: begin write(output, r: w);
                                                 newline := false end;
                                 prrfn: write(prr, r:w);
                                 errorfn: begin write(output, r: w);
                                                newline := false end;
                                 listfn: begin write(output, r: w);
                                               newline := false end;
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
                            valfil(ad); fn := store[ad];
                            if (w < 1) and iso7185 then
                              errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: begin writec(output, c, w);
                                                 newline := false end;
                                 prrfn: writec(prr, c, w);
                                 errorfn: begin writec(output, c, w);
                                                newline := false end;
                                 listfn: begin writec(output, c, w);
                                               newline := false end;
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
           72(*rdif*),
           82(*rdx*),
           83(*rdxf*),
           87(*rdih*),
           88(*rdio*),
           89(*rdib*),
           90(*rifh*),
           91(*rifo*),
           92(*rifb*),
           99(*rdxh*),
           100(*rdxo*),
           101(*rdxb*),
           102(*rxfh*),
           103(*rxfo*),
           104(*rxfb*): begin w := pmmaxint; 
                           fld := q in [72,83,90,91,92,102,103,104];
                           rx := 10;
                           if q in [87,90,99,102] then rx := 16
                           else if q in [88,91,100,103] then rx := 8
                           else if q in [89,92,101,104] then rx := 2;
                           if fld then popint(w);
                           popadr(ad1); popadr(ad);
                           valfil(ad); fn := store[ad]; readi(fn, i, w, fld, rx);
                           if q in [82,83,99,100,101,102,103,104] then begin
                             if (i < 0) or (i > 255) then errore(ValueOutOfRange);
                             putbyt(ad1, i)
                           end else putint(ad1, i)
                      end;
           37(*rib*),
           71(*ribf*),
           84(*rxb*),
           85(*rxbf*),
           93(*ribh*),
           94(*ribo*),
           95(*ribb*),
           96(*rbfh*),
           97(*rbfo*),
           98(*rbfb*),
           105(*rxbh*),
           106(*rxbo*),
           107(*rxbb*),
           108(*rbxh*),
           109(*rbxo*),
           110(*rbxb*): begin w := pmmaxint; 
                           fld := q in [71,85,96,97,98,108,109,110];
                           rx := 10;
                           if q in [93,96,105,108] then rx := 16
                           else if q in [94,97,106,109] then rx := 8
                           else if q in [95,98,107,110] then rx := 2;
                           popint(mx); popint(mn);
                           if fld then popint(w); popadr(ad1); popadr(ad);
                           valfil(ad); fn := store[ad];
                           readi(fn, i, w, fld, rx);
                           if (i < mn) or (i > mx) then
                             errore(ValueOutOfRange);
                           { note: value should be in byte range }
                           if q in [82,83,105,106,107,108,109,110] then putbyt(ad1, i)
                           else putint(ad1, i)
                      end;
           12(*rdr*),
           73(*rdrf*): begin w := pmmaxint; fld := q = 73; if fld then popint(w);
                           popadr(ad1); popadr(ad);
                           valfil(ad); fn := store[ad];
                           readr(fn, r, w, fld); putrel(ad1, r)
                      end;
           13(*rdc*),
           75(*rdcf*): begin w := pmmaxint; fld := q = 75; if fld then popint(w);
                           popadr(ad1); popadr(ad);
                           valfil(ad); fn := store[ad];
                           readc(fn, c, w, fld); putchr(ad1, c)
                      end;
           38(*rcb*),
           74(*rcbf*): begin w := pmmaxint; fld := q = 74; popint(mx); popint(mn);
                            if fld then popint(w); popadr(ad1); popadr(ad);
                            valfil(ad);
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
                                outputfn: begin page(output);
                                                newline := true end;
                                prrfn: page(prr);
                                errorfn: begin page(output);
                                               newline := true end;
                                listfn: begin page(output);
                                              newline := true end;
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
                            valfil(ad); fn := store[ad];
                            if w < 1 then errore(InvalidFieldSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: begin write(output, b:w);
                                                 newline := false end;
                                 prrfn: write(prr, b:w);
                                 errorfn: begin write(output, b:w);
                                                newline := false end;
                                 listfn: begin write(output, b:w);
                                               newline := false end;
                                 prdfn,inputfn,
                                 commandfn: errore(WriteOnReadOnlyFile)
                              end
                            else begin
                                if filstate[fn] <> fwrite then
                                   errore(FileModeIncorrect);
                                write(filtable[fn], b:w)
                            end
                      end;
           25(*wrf*): begin popint(f); popint(w); poprel(r); popadr(ad);
                            valfil(ad); fn := store[ad];
                            if (w < 1) and iso7185 then
                              errore(InvalidFieldSpecification);
                            if f < 1 then errore(InvalidFractionSpecification);
                            if fn <= commandfn then case fn of
                                 outputfn: begin writerf(output, r, w, f);
                                                 newline := false end;
                                 prrfn: writerf(prr, r, w, f);
                                 errorfn: begin writerf(output, r, w, f);
                                                newline := false end;
                                 listfn: begin writerf(output, r, w, f);
                                               newline := false end;
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
           27(*wbf*): begin popint(l); popadr(ad1); popadr(ad);
                           valfilwm(ad); fn := store[ad];
                           for i := 1 to l do begin
                              chkdef(ad1); write(bfiltable[fn], store[ad1]);
                              ad1 := ad1+1
                           end
                      end;
           28(*wbi*): begin popint(i); popadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to intsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            popint(i)
                      end;
           45(*wbx*): begin popint(i); popadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            write(bfiltable[fn], store[sp]);
                            popint(i)
                      end;
           29(*wbr*): begin poprel(r); popadr(ad); pshrel(r);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to realsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            poprel(r)
                      end;
           30(*wbc*): begin popint(i); c := chr(i); popadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to charsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            popint(i)
                      end;
           31(*wbb*): begin popint(i); popadr(ad); pshint(i);
                            valfilwm(ad); fn := store[ad];
                            for i := 1 to boolsize do
                               write(bfiltable[fn], store[sp+i-1]);
                            popint(i)
                      end;
           32(*rbf*): begin popint(l); popadr(ad1); popadr(ad);
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
           43 (*fbv*): begin popadr(ad); valfil(ad);
                          fn := store[ad];
                          if fn = inputfn then putchr(ad+fileidsize, input^)
                          else if fn = prdfn then putchr(ad+fileidsize, prd^)
                          else if fn = commandfn then 
                            putchr(ad+fileidsize, bufcommand)
                          else begin
                            if filstate[fn] = fread then
                            putchr(ad+fileidsize, filtable[fn]^)
                          end;
                          filbuff[fn] := true { validate buffer }
                        end;
           44 (*fvb*): begin popint(i); popadr(ad); valfil(ad);
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
                         assign(filtable[fn], fl1);
                         filanamtab[fn] := true
                       end;
           56 (*assb*): begin popint(i); popadr(ad1); popadr(ad); valfil(ad);
                         fn := store[ad]; clrfn(fl1);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         assign(bfiltable[fn], fl1);
                         filanamtab[fn] := true
                       end;
           47 (*clst*): begin popadr(ad); valfil(ad); fn := store[ad];
                         close(filtable[fn]); filstate[fn] := fclosed
                       end;
           57 (*clsb*): begin popadr(ad); valfil(ad); fn := store[ad];
                         close(bfiltable[fn]); filstate[fn] := fclosed
                       end;
           48 (*pos*): begin popint(i); popadr(ad); valfil(ad); fn := store[ad];
                         if i < 1 then errore(InvalidFilePosition);
                         position(bfiltable[fn], i)
                       end;
           49 (*upd*): begin popadr(ad); valfil(ad); fn := store[ad];
                         update(bfiltable[fn]); filstate[fn] := fwrite;
                         filbuff[fn] := false
                       end;
           50 (*appt*): begin popadr(ad); valfil(ad); fn := store[ad];
                         append(filtable[fn]); filstate[fn] := fwrite;
                         filbuff[fn] := false
                       end;
           58 (*appb*): begin popadr(ad); valfil(ad); fn := store[ad];
                         append(bfiltable[fn]); filstate[fn] := fwrite;
                         filbuff[fn] := false
                       end;
           51 (*del*): begin popint(i); popadr(ad1); clrfn(fl1);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         delete(fl1)
                       end;
           52 (*chg*): begin popint(i); popadr(ad1); popint(l); popadr(ad);
                         clrfn(fl1); clrfn(fl2);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         for j := 1 to l do fl2[j] := chr(store[ad+j-1]);
                         change(fl2, fl1)
                       end;
           53 (*len*): begin popadr(ad); valfil(ad); fn := store[ad];
                         pshint(length(bfiltable[fn]))
                       end;
           54 (*loc*): begin popadr(ad); valfil(ad); fn := store[ad];
                         pshint(location(bfiltable[fn]))
                       end;
           55 (*exs*): begin popint(i); popadr(ad1); clrfn(fl1);
                         for j := 1 to i do fl1[j] := chr(store[ad1+j-1]);
                         pshint(ord(exists(fl1)))
                       end;
           59 (*hlt*): goto 99;
           60 (*ast*): begin popint(i);
                         if i = 0 then errorv(ProgramCodeAssertion);
                       end;
           61 (*asts*): begin popint(i); popadr(ad); popint(j);
                         if j = 0 then errors(ad, i);
                       end;
           70(*rds*),
           76(*rdsf*): begin w := pmmaxint; fld := q = 76; 
                         if fld then popint(w); popadr(ad1); popint(i); 
                         popadr(ad); valfil(ad); fn := store[ad];
                         reads(fn, ad1, i, w, fld);
                       end;
           77(*rdsp*): begin popadr(ad1); popint(i); popadr(ad);
                         valfil(ad); fn := store[ad];
                         readsp(fn, ad1, i)
                       end;
           86(*rdsc*): begin popadr(ad1); popint(i); 
                         popadr(ad); valfil(ad); fn := store[ad];
                         readsc(fn, ad1, i)
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
           80(*rdie*): begin w := pmmaxint; popint(i); popadr(ad1); popadr(ad);
                         readi(commandfn, i, w, false, 10); putint(ad, i);
                       end;
           81(*rdre*): begin w := pmmaxint; popint(i); popadr(ad1); popadr(ad);
                         readr(commandfn, r, w, false); putrel(ad, r);
                       end;
           2(*thw*):   begin popadr(ad1); mp := expmrk; sp := expstk;
                         pc := expadr; popadr(ad2); pshadr(ad1);
                         ep := getadr(mp+market) { get the mark ep }
                         { release to search vectors }
                       end;
           111(*sete*): begin popint(i) end; { noop for now }

      end;(*case q*)
end;(*callsp*)

procedure lstinsa(var a: address);
begin
  if isbrk(a) then write('b')
  else if istrc(a) then write('t')
  else write(' ');
  if getcov(a) then write('c') else write(' ');
  if a = pc then write('*') else write(' ');
  write(' ');
  wrthex(output, a, maxdigh, true);
  lstins(a);
  writeln
end;

function fndblk(a: address): pblock;
var fbp, bp: pblock;
begin
  { search for location in blocks }
  fbp := nil; bp := blklst;
  while bp <> nil do begin { traverse blocks }
    if (a >= bp^.bstart) and (a < bp^.bend) then
      begin fbp := bp; bp := nil end { found }
    else bp := bp^.next
  end;
  fndblk := fbp
end;

function fndmod(a: address): pblock;
var fbp, bp: pblock;
begin
  { search for location in blocks }
  fbp := nil; bp := blklst;
  while bp <> nil do begin { traverse blocks }
    if (a >= bp^.bestart) and (a < bp^.bend) and
       (bp^.btyp in [btprog, btmod]) then begin fbp := bp; bp := nil end { found }
    else bp := bp^.next
  end;
  fndmod := fbp
end;

procedure setcur;
begin
  { check already active, and don't do a full search if so. This saves time. }
  if curmod <> nil then begin
    if (pc < curmod^.bestart) or (pc >= curmod^.bend) then curmod := fndmod(pc)
  end else curmod := fndmod(pc)
end;

{ find line from address in module }
function addr2line(modp: pblock; a: address): integer;
var i: integer;
begin
  i := 1;
  while (modp^.lintrk^[i] < a) and (i < maxsrc) do i := i+1;
  if i > 1 then i := i-1;
  if modp^.lintrk^[i] < 0 then i := 0;
  addr2line := i
end;

{ skip over line markers }
procedure skplmk(var ad: address);
var i: integer;
begin
  i := 1;
  while store[ad] = mrkins do begin
    if (ad > maxstr) or (i > 100) then begin
      writeln('*** Could not skip line markers');
      goto 99
    end;
    ad := ad+(mrkinsl+intsize);
    i := i+1
  end
end;

{ skip mst instruction }
procedure skpmst(var ad: address);
begin
  if store[ad] = mstins then ad := ad+1+1+intsize*2
end;

{ print source lines }
procedure prtsrc(bp: pblock; s, e: integer; comp: boolean);
var f: text; i: integer; c: char; nl: boolean; si,ei: address; fn: filnam;
begin
  if bp = nil then begin
    setcur;
    if curmod = nil then writeln('*** No active module');
    bp := curmod
  end;
  if bp <> nil then begin
    strassfv(fn, bp^.fname);
    if not exists(fn) then
      writeln('*** Source file ', fn:lenp(fn), ' not found')
    else begin
      assign(f, fn); reset(f); i := 1;
      nl := true;
      while (i < s) and not eof(f) do begin readln(f); i := i+1 end;
      while (i <= e) and not eof(f) and not chkbrk do begin
        if nl then begin { output line head }
          write(i:4, ': ');
          if dosrcprf then write(bp^.linprf^[i]:6, ': ');
          if isbrkl(i) and not istmpl(i) then write('b')
          else if istrcl(i) and not istmpl(i) then write('t')
          else write(' ');
          if i = srclin then write('*') else write(' ');
          write(' ');
          nl := false
        end;
        if not eoln(f) then begin read(f, c); write(c) end
        else begin
          readln(f); writeln;
          if comp then begin { coordinated listing mode }
            si := bp^.lintrk^[i]; ei := bp^.lintrk^[i+1];
            if ei < 0 then ei := bp^.bend-1;
            if (si >= 0) and (ei >= 0) then
              while si <= ei do lstinsa(si) { list machine instructions }
          end;
          i := i+1; nl := true
        end
      end;
      close(f)
    end
  end
end;

function iswatch(ad: address): boolean;
var wi: wthinx;
begin
  wi := 1;
  while (wi < maxwth) and (wthtbl[wi] <> ad) and (wthtbl[wi] > 0) do wi := wi+1;
  iswatch := wthtbl[wi] = ad
end;

{ watch table evict }
procedure evict(s, e: address);
var wi: wthinx;
begin
  wi := 1;
  while (wi < maxwth) and (wthtbl[wi] > 0) do begin
    if (wthtbl[wi] >= s) and (wthtbl[wi] <= e) then wthtbl[wi] := -1;
    wi := wi+1
  end
end;

procedure putani(a: address);
begin
  anitbl[aniptr] := a;
  if aniptr = maxana then aniptr := 1 else aniptr := aniptr+1
end;

function lstana(a: integer): integer;
begin if a > 1 then a := a-1 else a := maxana; lstana := a end;

procedure putans(mp: pblock; l: integer);
begin
  anstbl[ansptr] := l;
  ansmtbl[ansptr] := mp;
  if ansptr = maxana then ansptr := 1 else ansptr := ansptr+1
end;

procedure lstexp(a: address);
begin
   writeln('exception frame:');
   writeln('exception vector: @', a$:#16, ':', getint(a)$:#16);
   writeln('exception address: @', a+8$:#16, ':', getint(a+8)$:#16);
   writeln('exception address of sp: @', a+16$:#16, ':', getint(a+16)$:#16);
   writeln('exception address of mp: @', a+24$:#16, ':', getint(a+24)$:#16)
end;

procedure sinins;
var ad,ad1,ad2,ad3,ad4: address; b: boolean; i,j,i1,i2 : integer; c1: char;
    i3,i4: integer; r1,r2: real; b1: boolean; s1,s2: settype;
    a1,a2,a3: address;
begin
  if pc >= pctop then errorv(PCOutOfRange);

  if dochkcov then putcov(pc, true); { check coverage if enabled }

  pcs := pc; { save starting pc }
  getop;


  if dotrcins then begin wrtnewline; ad := pcs;
    if isbrk(ad) then write('b')
    else if istrc(ad) then write('t')
    else write(' ');
    if getcov(ad) then write('c') else write(' ');
    write('*');
    write(' ');
    wrthex(output, ad, maxdigh, true);
    write('/');
    wrthex(output, sp, maxdigh, true);
    lstins(ad);
    writeln
  end;

  if doanalys and (ins[op].mi <> pi_brk{brk}) then putani(pcs);

  case ins[op].mi of

    pi_lodi: begin getp; getq;
                    pshint(getint(getadr(mp-p*ptrsize)+q))
                  end;
    pi_lodx: begin getp; getq;
                    pshint(getbyt(getadr(mp-p*ptrsize)+q))
                  end;
    pi_loda: begin getp; getq;
                    pshadr(getadr(getadr(mp-p*ptrsize)+q))
                  end;
    pi_lodr: begin getp; getq;
                    pshrel(getrel(getadr(mp-p*ptrsize)+q))
                  end;
    pi_lods: begin getp; getq;
                    pshadr(getadr(mp-p*ptrsize)+q)
                  end;
    pi_lodb: begin getp; getq;
                    pshint(ord(getbol(getadr(mp-p*ptrsize)+q)))
                  end;
    pi_lodc: begin getp; getq;
                    pshint(ord(getchr(getadr(mp-p*ptrsize)+q)))
                  end;

    pi_ldoi: begin getq; pshint(getint(q)) end;
    pi_ldox: begin getq; pshint(getbyt(q)) end;
    pi_ldoa: begin getq; pshadr(getadr(q)) end;
    pi_ldor: begin getq; pshrel(getrel(q)) end;
    pi_ldos: begin getq; pshadr(q) end;
    pi_ldob: begin getq; pshint(ord(getbol(q))) end;
    pi_ldoc: begin getq; pshint(ord(getchr(q))) end;

    pi_stri: begin getp; getq; stoad := getadr(mp-p*ptrsize)+q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popint(i); putint(stoad, i) end
                  end;
    pi_strx: begin getp; getq; stoad := getadr(mp-p*ptrsize)+q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popint(i); putbyt(stoad, i) end
                  end;
    pi_stra: begin getp; getq; stoad := getadr(mp-p*ptrsize)+q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popadr(ad); putadr(stoad, ad) end
                  end;
    pi_strr: begin getp; getq; stoad := getadr(mp-p*ptrsize)+q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin poprel(r1); putrel(stoad, r1) end
                  end;
    pi_strs: begin getp; getq; stoad := getadr(mp-p*ptrsize)+q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popadr(ad); getset(ad, s1); putset(stoad, s1) end
                  end;
    pi_strb: begin getp; getq; stoad := getadr(mp-p*ptrsize)+q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else
                          begin popint(i1); b1 := i1 <> 0; putbol(stoad, b1) end
                  end;
    pi_strc: begin getp; getq; stoad := getadr(mp-p*ptrsize)+q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else
                          begin popint(i1); c1 := chr(i1); putchr(stoad, c1) end
                  end;

    pi_sroi: begin getq; stoad := q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popint(i); putint(stoad, i) end
                  end;
    pi_srox: begin getq; stoad := q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popint(i); putbyt(stoad, i) end
                  end;
    pi_sroa: begin getq; stoad := q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popadr(ad); putadr(stoad, ad) end
                  end;
    pi_sror: begin getq; stoad := q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin poprel(r1); putrel(stoad, r1) end
                  end;
    pi_sros: begin getq; stoad := q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else begin popadr(ad); getset(ad, s1); putset(stoad, s1) end
                  end;
    pi_srob: begin getq; stoad := q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else
                          begin popint(i1); b1 := i1 <> 0; putbol(stoad, b1) end
                  end;
    pi_sroc: begin getq; stoad := q;
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs end
                        else
                          begin popint(i1); c1 := chr(i1); putchr(stoad, c1) end
                  end;

    pi_lda: begin getp; getq; pshadr(getadr(mp-p*ptrsize)+q) end;
    pi_lao: begin getq; pshadr(q) end;

    pi_stoi: begin popint(i); popadr(stoad);
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs; pshadr(stoad);
                                pshint(i) end
                        else putint(stoad, i)
                  end;
    pi_stox: begin popint(i); popadr(stoad);
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs; pshadr(stoad);
                                pshint(i) end
                        else putbyt(stoad, i)
                  end;
    pi_stoa: begin popadr(ad1); popadr(stoad);
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs; pshadr(stoad);
                                pshint(i) end
                        else putadr(stoad, ad1)
                  end;
    pi_stor: begin poprel(r1); popadr(stoad);
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs; pshadr(stoad);
                                pshint(i) end
                        else putrel(stoad, r1)
                  end;
    pi_stos: begin popadr(ad); getset(ad, s1); popadr(stoad);
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs; pshadr(stoad);
                                pshadr(ad) end
                        else putset(stoad, s1)
                  end;
    pi_stob: begin popint(i1); b1 := i1 <> 0; popadr(stoad);
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs; pshadr(stoad);
                                pshint(i) end
                        else putbol(stoad, b1)
                  end;
    pi_stoc: begin popint(i1); c1 := chr(i1); popadr(stoad);
                        if iswatch(stoad) and stopwatch then
                          begin watchmatch := true; pc := pcs; pshadr(stoad);
                                pshint(i) end
                        else putchr(stoad, c1)
                  end;
    pi_stom: begin getq; getq1; ad1 := getadr(sp+q1); ad2 := sp;
                    for i := 0 to q-1 do begin
                      store[ad1+i] := store[ad2+i]; putdef(ad1+i, getdef(ad2+i))
                    end;
                    sp := sp+q1+adrsize
                  end;
    pi_ctb: begin getq; getq1; popadr(ad1); ad2 := sp;
                    for i := 0 to q-1 do begin
                      store[ad1+i] := store[ad2+i]; putdef(ad1+i, getdef(ad2+i))
                    end;
                    sp := sp+q1; pshadr(ad1)
                  end;

    pi_sfs: begin getq; getq1; popadr(ad1); ad2 := sp;
                    for i := 0 to q-1 do begin
                      store[ad1+i] := store[ad2+i]; putdef(ad1+i, getdef(ad2+i))
                    end;
                    sp := sp+q1
                  end;

    pi_ldcc: begin pshint(ord(getchr(pc))); pc := pc+1 end;
    pi_ldcb: begin pshint(ord(getbol(pc))); pc := pc+1 end;
    pi_ldci: begin i := getint(pc); pc := pc+intsize; pshint(i) end;
    pi_ldcn: pshadr(nilval) (* load nil *) ;
    pi_ldcr: begin getq; pshrel(getrel(q)) end;
    pi_ldcs: begin getq; pshadr(q) end;

    pi_indi: begin getq; popadr(ad); pshint(getint(ad+q)) end;
    pi_indx: begin getq; popadr(ad); pshint(getbyt(ad+q)) end;
    pi_inda: begin getq; popadr(ad); ad1 := getadr(ad+q);
                        pshadr(ad1) end;
    pi_indr: begin getq; popadr(ad); pshrel(getrel(ad+q)) end;
    pi_inds: begin getq; popadr(ad); pshadr(ad+q) end;
    pi_indb: begin getq; popadr(ad); pshint(ord(getbol(ad+q))) end;
    pi_indc: begin getq; popadr(ad); pshint(ord(getchr(ad+q))) end;

    pi_incb,
    pi_incc,
    pi_incx,
    pi_inci: begin getq; popint(i1);
                   if dochkovf then if (i1<0) = (q<0) then
                      if pmmaxint-abs(i1) < abs(q) then
                        errore(IntegerValueOverflow);
                   pshint(i1+q)
                   end;
    pi_inca: begin getq; popadr(a1); pshadr(a1+q) end;

    pi_sfr: begin getq;
                   for j := 1 to q div intsize do pshint(0);
                   for j := 1 to q do putdef(sp+j-1, false)
                 end;
    pi_mst: begin getp; getq; getq1;
                  pshadr(0); { place current ep }
                  pshadr(0); { place bottom of stack }
                  pshadr(ep); { previous ep }
                  pshadr(mp); { save old mp on stack }
                  ad1 := mp; { save old mp }
                  mp := sp; { set new mp }
                  for i := 1 to p do begin ad1 := ad1-ptrsize; pshadr(getadr(ad1)) end;
                  pshadr(mp); { push new mp to complete display } 
                  ad := sp-q; (*q = length of dataseg*)
                  if ad <= np then errorv(StoreOverflow);
                  while sp > ad do 
                    begin sp := sp-1; store[sp] := 0; putdef(sp, false) end;
                  putadr(mp+marksb, sp); { set bottom of stack }
                  ep := sp-q1; if ep <= np then errorv(StoreOverFlow);
                  putadr(mp+market, ep) { place current ep }
                end;

    pi_cup,
    pi_cuf: begin (*q=entry point*)
                 getq; pshadr(pc); pc := q
                end;

    pi_cuv,
    pi_cvf: begin (*q=vector entry point*)
                 getq; pshadr(pc); pc := getadr(q)
                end;

    pi_suv: begin getq; getq1; putadr(q1, q) end;

    { For characters and booleans, need to clean 8 bit results because
      only the lower 8 bits were stored to. }
    pi_retc: begin getq; evict(ep, mp);
                   ep := getadr(mp+markep);
                   sp := mp; { index old mark }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q; { remove parameters }
                   putint(sp, ord(getchr(sp)))
                 end;
    pi_retb: begin getq; evict(ep, mp);
                   ep := getadr(mp+markep);
                   sp := mp; { index old mp }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q; { remove parameters }
                   putint(sp, ord(getbol(sp)))
                 end;
    pi_retp,
    pi_reti,
    pi_retx,
    pi_retr,
    pi_reta: begin getq; evict(ep, mp);
                   ep := getadr(mp+markep);
                   sp := mp; { index old mp }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q { remove parameters and mark }
                 end;

    pi_rets: begin getq; evict(ep, mp);
                   ep := getadr(mp+markep);
                   sp := mp; { index old mp }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q { remove parameters, leave set result on stack }
                 end;

    pi_retm: begin getq; evict(ep, mp);
                   ep := getadr(mp+markep);
                   sp := mp; { index old mp }
                   popadr(mp); { restore old mp }
                   sp := sp+marksize; { skip mark }
                   popadr(pc); { load return address }
                   sp := sp+q { remove parameters }
                 end;

    pi_csp: begin q := store[pc]; pc := pc+1; callsp end;

    pi_ixa: begin getq; popint(i); popadr(a1); pshadr(q*i+a1) end;

    pi_equa: begin popadr(a2); popadr(a1); pshint(ord(a1=a2)) end;
    pi_equb,
    pi_equc,
    pi_equi: begin popint(i2); popint(i1); pshint(ord(i1=i2)) end;
    pi_equr: begin poprel(r2); poprel(r1); pshint(ord(r1=r2)) end;
    pi_equs: begin popadr(ad1);
                    getset(ad1, s2); popadr(ad);
                    getset(ad, s1);
                    pshint(ord(s1=s2)) end;
    pi_equm: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(b)) end;
    pi_equv: begin popadr(a2);
                    popint(i); q := i;
                    popadr(a1); popint(i1);
                    compare(b, a1, a2);
                    pshint(ord(b)) end;

    pi_neqa: begin popadr(a2); popadr(a1); pshint(ord(a1<>a2)) end;
    pi_neqb,
    pi_neqc,
    pi_neqi: begin popint(i2); popint(i1); pshint(ord(i1<>i2)) end;
    pi_neqr: begin poprel(r2); poprel(r1); pshint(ord(r1<>r2)) end;
    pi_neqs: begin popadr(ad1);
                    getset(ad1, s2); popadr(ad);
                    getset(ad, s1);
                    pshint(ord(s1<>s2)) end;
    pi_neqm: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b)) end;
    pi_neqv: begin popadr(a2);
                    popint(i); q := i;
                    popadr(a1); popint(i1);
                    compare(b, a1, a2);
                    pshint(ord(not b)) end;

    pi_geqb,
    pi_geqc,
    pi_geqi: begin popint(i2); popint(i1); pshint(ord(i1>=i2)) end;
    pi_geqr: begin poprel(r2); poprel(r1); pshint(ord(r1>=r2)) end;
    pi_geqs: begin popadr(ad1);
                    getset(ad1, s2); popadr(ad);
                    getset(ad, s1);
                    pshint(ord(s1>=s2)) end;
    pi_geqm: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(b or (store[a1] >= store[a2])))
                  end;
    pi_geqv: begin popadr(a2);
                    popint(i); q := i;
                    popadr(a1); popint(i1);
                    compare(b, a1, a2);
                    pshint(ord(b or
                      (store[a1] >= store[a2])))
                  end;

    pi_grtb,
    pi_grtc,
    pi_grti: begin popint(i2); popint(i1); pshint(ord(i1>i2)) end;
    pi_grtr: begin poprel(r2); poprel(r1); pshint(ord(r1>r2)) end;
    pi_grts: errorv(SetInclusion);
    pi_grtm: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] > store[a2])))
                  end;
    pi_grtv: begin popadr(a2);
                    popint(i); q := i;
                    popadr(a1); popint(i1);
                    compare(b, a1, a2);
                    pshint(ord(not b and
                      (store[a1] > store[a2])))
                  end;


    pi_leqb,
    pi_leqc,
    pi_leqi: begin popint(i2); popint(i1); pshint(ord(i1<=i2)) end;
    pi_leqr: begin poprel(r2); poprel(r1); pshint(ord(r1<=r2)) end;
    pi_leqs: begin popadr(ad1);
                    getset(ad1, s2); popadr(ad);
                    getset(ad, s1);
                    pshint(ord(s1<=s2)) end;
    pi_leqm: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(b or (store[a1] <= store[a2])))
                  end;
    pi_leqv: begin popadr(a2);
                    popint(i); q := i;
                    popadr(a1); popint(i1);
                    compare(b, a1, a2);
                    pshint(ord(b or
                      (store[a1] <= store[a2])))
                  end;

    pi_lesb,
    pi_lesc,
    pi_lesi: begin popint(i2); popint(i1); pshint(ord(i1<i2)) end;
    pi_lesr: begin poprel(r2); poprel(r1); pshint(ord(r1<r2)) end;
    pi_less: errorv(SetInclusion);
    pi_lesm: begin getq; popadr(a2); popadr(a1); compare(b, a1, a2);
                        pshint(ord(not b and (store[a1] < store[a2])))
                  end;
    pi_lesv: begin popadr(a2);
                    popint(i); q := i;
                    popadr(a1); popint(i1);
                    compare(b, a1, a2);
                    pshint(ord(not b and
                      (store[a1] < store[a2])))
                  end;

    pi_ujp: begin getq; pc := q end;
    pi_fjp: begin getq; popint(i); if i = 0 then pc := q end;
    pi_xjp: begin getq; popint(i1); pc := i1*ujplen+q end;

    pi_chka,
    pi_ckla: begin getq; popadr(a1); pshadr(a1);
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
                         if isfree(a1) then
                           { attempt to dereference or assign a freed
                             block }
                           errorv(PointerUsedAfterDispose)
                       end
                 end;
    pi_chks: begin getq; popadr(ad); getset(ad, s1); pshadr(ad);
                   for j := setlow to getint(q)-1 do
                     if j in s1 then errorv(SetElementOutOfRange);
                   for j := getint(q+intsize)+1 to sethigh do
                     if j in s1 then errorv(SetElementOutOfRange);
                 end;
    pi_chkb,
    pi_chkc,
    pi_chkx,
    pi_chki: begin getq; popint(i1); pshint(i1);
                  if (i1 < getint(q)) or (i1 > getint(q+intsize)) then
                  errore(ValueOutOfRange)
                end;

    pi_cks: pshint(0);
    pi_ckvi,
    pi_ckvx,
    pi_ckvb,
    pi_ckvc: begin getq; popint(i2); popint(i1);
                    pshint(i1); pshint(ord((i1 = q) or (i2 <> 0)));
                  end;
    pi_cke: begin popint(i2); popint(i1);
                    if i2 = 0 then errorv(VariantNotActive)
                  end;

    pi_dupb,
    pi_dupc,
    pi_dupi: begin popint(i1); pshint(i1); pshint(i1) end;
    pi_dupa: begin popadr(a1); pshadr(a1); pshadr(a1) end;
    pi_dupr: begin poprel(r1); pshrel(r1); pshrel(r1) end;
    pi_dups: begin popadr(ad); pshadr(ad); pshadr(ad) end;

    pi_inv: begin popadr(stoad);
                       if iswatch(stoad) and stopwatch then
                         begin pshadr(stoad); watchmatch := true end
                       else putdef(stoad, false)
                 end;

    pi_adi: begin popint(i2); popint(i1);
                  if dochkovf then if (i1<0) = (i2<0) then
                    if pmmaxint-abs(i1) < abs(i2) then
                      errore(IntegerValueOverflow);
                pshint(i1+i2) end;
    pi_adr: begin poprel(r2); poprel(r1); pshrel(r1+r2) end;
    pi_sbi: begin popint(i2); popint(i1);
                  if dochkovf then if (i1<0) <> (i2<0) then
                    if pmmaxint-abs(i1) < abs(i2) then
                      errore(IntegerValueOverflow);
                pshint(i1-i2) end;
    pi_sbr: begin poprel(r2); poprel(r1); pshrel(r1-r2) end;
    pi_sgs: begin popadr(ad); popint(i1);
                        putset(ad, [i1]);
                        pshadr(ad) end;
    pi_flt: begin popint(i1); pshrel(i1) end;

    pi_flo: begin poprel(r1); popint(i1); pshrel(i1); pshrel(r1) end;

    pi_trc: begin poprel(r1);
                  if dochkovf then if (r1 < -pmmaxint) or (r1 > pmmaxint) then
                    errore(RealArgumentTooLarge);
                  pshint(trunc(r1)) end;
    pi_ngi: begin popint(i1); pshint(-i1) end;
    pi_ngr: begin poprel(r1); pshrel(-r1) end;
    pi_sqi: begin popint(i1);
                if dochkovf then if i1 <> 0 then
                  if abs(i1) > pmmaxint div abs(i1) then
                    errore(IntegerValueOverflow);
                pshint(sqr(i1)) end;
    pi_sqr: begin poprel(r1); pshrel(sqr(r1)) end;
    pi_abi: begin popint(i1); pshint(abs(i1)) end;
    pi_abr: begin poprel(r1); pshrel(abs(r1)) end;
    pi_notb: begin popint(i1); b1 := i1 <> 0; pshint(ord(not b1)) end;
    pi_noti: begin popint(i1);
                      if i1 < 0 then errore(BooleanOperatorOfNegative);
                      pshint(bnot(i1)) end;
    pi_and: begin popint(i2);
                      if i2 < 0 then errore(BooleanOperatorOfNegative);
                      popint(i1);
                      if i1 < 0 then errore(BooleanOperatorOfNegative);
                      pshint(band(i1, i2)) end;
    pi_ior: begin popint(i2);
                      if i2 < 0 then errore(BooleanOperatorOfNegative);
                      popint(i1);
                      if i1 < 0 then errore(BooleanOperatorOfNegative);
                      pshint(bor(i1, i2)) end;
    pi_xor: begin popint(i2);
                      if i2 < 0 then errore(BooleanOperatorOfNegative);
                      popint(i1); b1 := i1 <> 0;
                      if i1 < 0 then errore(BooleanOperatorOfNegative);
                      pshint(bxor(i1, i2)) end;
    pi_dif: begin popadr(ad); popadr(ad1); getset(ad1, s2);
                        popadr(ad1); getset(ad1, s1);
                        putset(ad, s1-s2); pshadr(ad) end;
    pi_int: begin popadr(ad); popadr(ad1); getset(ad1, s2);
                        popadr(ad1); getset(ad1, s1);
                        putset(ad, s1*s2); pshadr(ad) end;
    pi_uni: begin popadr(ad); popadr(ad1); getset(ad1, s2);
                        popadr(ad1); getset(ad1, s1);
                        putset(ad, s1+s2); pshadr(ad) end;
    pi_inn: begin popadr(ad); getset(ad, s1); popint(i1);
                        pshint(ord(i1 in s1)) end;
    pi_mod: begin popint(i2); popint(i1);
                  if dochkovf then if i2 <= 0 then
                    errore(InvalidDivisorToMod);
                  pshint(i1 mod i2) end;
    pi_odd: begin popint(i1); pshint(ord(odd(i1))) end;
    pi_mpi: begin popint(i2); popint(i1);
                  if dochkovf then if (i1 <> 0) and (i2 <> 0) then
                    if abs(i1) > pmmaxint div abs(i2) then
                      errore(IntegerValueOverflow);
                  pshint(i1*i2) end;
    pi_mpr: begin poprel(r2); poprel(r1); pshrel(r1*r2) end;
    pi_dvi: begin popint(i2); popint(i1);
                      if dochkovf then if i2 = 0 then errore(ZeroDivide);
                      pshint(i1 div i2) end;
    pi_dvr: begin poprel(r2); poprel(r1);
                      if dochkovf then if r2 = 0.0 then errore(ZeroDivide);
                      pshrel(r1/r2) end;
    pi_mov: begin getq; popint(i2); popint(i1);
                 for i3 := 0 to q-1 do
                   begin store[i1+i3] := store[i2+i3];
                         putdef(i1+i3, getdef(i2+i3)) end;
                end;
    pi_lca: begin getq; pshadr(q) end;

    pi_decb,
    pi_decc,
    pi_decx,
    pi_deci: begin getq; popint(i1);
                    if dochkovf then if (i1<0) <> (q<0) then
                      if pmmaxint-abs(i1) < abs(q) then
                        errore(IntegerValueOverflow);
                    pshint(i1-q) end;

    pi_stp: begin stopins := true; pc := pcs end;

    pi_ordb,
    pi_ordc,
    pi_ordx,
    pi_ordi: ; { ord is a no-op }

    pi_chr: ; { chr is a no-op }

    pi_ujc: errorv(InvalidCase);
    pi_rnd: begin poprel(r1);
                  if dochkovf then if (r1 < -(pmmaxint+0.5)) or (r1 > pmmaxint+0.5) then
                    errore(RealArgumentTooLarge);
                  pshint(round(r1)) end;
    pi_pck: begin getq; getq1; popadr(a3); popadr(a2); popadr(a1);
                 if a2+q > q1 then errore(PackElementsOutOfBounds);
                 for i4 := 0 to q-1 do begin chkdef(a1+a2);
                    store[a3+i4] := store[a1+a2];
                    putdef(a3+i4, getdef(a1+a2));
                    a2 := a2+1
                 end
               end;
    pi_upk: begin getq; getq1; popadr(a3); popadr(a2); popadr(a1);
                 if a3+q > q1 then errore(UnpackElementsOutOfBounds);
                 for i4 := 0 to q-1 do begin chkdef(a1+i4);
                    store[a2+a3] := store[a1+i4];
                    putdef(a2+a3, getdef(a1+i4));
                    a3 := a3+1
                 end
               end;

    pi_rgs: begin popadr(ad); popint(i2); popint(i1);
                        putset(ad, [i1..i2]);
                        pshadr(ad) end;
    pi_ipj: begin getp; getq; pc := q;
                 mp := getadr(mp-p*ptrsize); { index the mark to restore }
                 sp := getadr(mp+marksb); { get the stack bottom }
                 ep := getadr(mp+market) { get the mark ep }
               end;
    pi_cip,
    pi_cif: begin popadr(ad); ad1 := mp;
                mp := getadr(ad+1*ptrsize); pshadr(pc); pc := getadr(ad)
              end;
    pi_rip: begin getq; mp := getadr(sp+q) end;
    pi_lpa: begin getp; getq; { place procedure address on stack }
                pshadr(getadr(mp-p*ptrsize));
                pshadr(q)
              end;
    pi_dmp: begin getq; sp := sp+q end; { remove top of stack }

    pi_swp: begin getq; swpstk(q) end;

    pi_tjp: begin getq; popint(i); if i <> 0 then pc := q end;

    pi_lip: begin getp; getq; ad := getadr(mp-p*ptrsize) + q;
                   ad1 := getadr(ad); ad2 := getadr(ad+1*ptrsize);
                   pshadr(ad2); pshadr(ad1);
                 end;

    pi_cta: begin getq; getq1; getq2; popint(i); popadr(ad); pshadr(ad);
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

    pi_ivti,
    pi_ivtx,
    pi_ivtb,
    pi_ivtc: begin getq; getq1; getq2; popint(i); popadr(ad);
                      pshadr(ad); pshint(i);
                      if (i < 0) or (i >= getint(q2)) then
                        errorv(ValueOutOfRange);
                      if dochkdef then begin
                        b := getdef(ad);
                        if b then begin
                          if ins[op].mi = pi_ivti then
                            j := getint(ad)
                          else j := getbyt(ad);
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

    pi_cvbi,
    pi_cvbx,
    pi_cvbb,
    pi_cvbc: begin getq; getq1; getq2; popint(i); popadr(ad);
                      pshadr(ad); pshint(i);
                      if (i < 0) or (i >= getint(q2)) then
                        errorv(ValueOutOfRange);
                      b := getdef(ad);
                      if b then begin
                        if ins[op].mi = pi_cvbi then
                          j := getint(ad)
                        else j := getbyt(ad);
                        b := getint(q2+(i+1)*intsize) <>
                             getint(q2+(j+1)*intsize)
                      end;
                      if b then begin
                        ad := ad+q;
                        if varinc(ad, ad+q1-1) then
                          errorv(ChangeToVarReferencedVariant);
                      end
                end;

    pi_wbs: begin popadr(ad); pshadr(ad); withenter(ad) end;
    pi_wbe: withexit;

    pi_mrkl: begin getq; srclin := q;
                    if doanalys then putans(curmod, srclin);
                        if dosrcprf then begin setcur;
                          if curmod <> nil then
                            if curmod^.linprf^[q] < pmmaxint then
                              curmod^.linprf^[q] := curmod^.linprf^[q]+1;
                        end;
                        if dotrcsrc then
                          if dodbgsrc then begin
                            writeln;
                            prtsrc(nil, srclin-1, srclin+1, false);
                            writeln
                          end else writeln('Source line executed: ', q:1);
                        sourcemark := true
                  end;

    pi_bge: begin getq;
                   pshadr(expadr); pshadr(expstk); pshadr(expmrk);
                   pshadr(0); { place dummy vector }
                   expadr := q; expstk := sp; expmrk := mp
                 end;
    pi_ede: begin popadr(a1); { dispose vector }
                   popadr(expmrk); popadr(expstk); popadr(expadr)
                 end;
    pi_mse: begin popadr(a1);
                   popadr(expmrk); popadr(expstk); popadr(expadr);
                   if expadr = 0 then errorm(a1)
                   else begin { throw to new frame }
                     mp := expmrk; sp := expstk; pc := expadr;
                     popadr(a2); pshadr(a1);
                     ep := getadr(mp+market) { get the mark ep }
                   end
                 end;
    pi_sev: begin getp; getq; a1 := getadr(mp-p*ptrsize)+q;
                        popadr(ad); pshadr(ad); putadr(a1, ad)
                  end;
    pi_cjp: begin getq; getq1; popint(i1); pshint(i1);
                  if (i1 >= getint(q)) and (i1 <= getint(q+intsize)) then
                    begin pc := q1; popint(i1) end
                end;
    pi_lnp: begin getq; np := q; gbtop := np; ad := pctop;
                  while np > ad do
                    begin store[ad] := 0; putdef(ad, false); ad := ad+1 end
                end;
    pi_cal: begin getq; pshadr(pc); pc := q end;
    pi_ret: popadr(pc);
    pi_vbs: begin getq; popadr(ad); varenter(ad, ad+q-1) end;
    pi_vbe: varexit;
    pi_brk: begin breakins := true; pc := pcs end;
    pi_vis: begin getq; getq1; popadr(ad); ad1 := ad+q*intsize;
                   for i := 1 to q do begin
                     popint(i1); putint(ad1, i1); ad1 := ad1-intsize; q1 := q1*i1;
                   end;
                   popadr(ad1); sp := sp-q1; putadr(ad, sp); pshadr(ad1)
                 end;
    pi_vip: begin getq; getq1; popadr(ad); ad1 := ad+q*intsize;
                   for i := 1 to q do begin
                     popint(i1); putint(ad1, i1); ad1 := ad1-intsize; q1 := q1*i1;
                   end;
                   newspc(q1, ad2); putadr(ad1, ad2)
                 end;
    pi_vin: begin getq; getq1; popadr(ad); ad2 := sp;
                   for i := 1 to q do
                     begin q1 := q1*getint(ad2); ad2 := ad2+intsize end;
                   newspc(q1+q*intsize, ad2); putadr(ad, ad2); 
                   ad2 := ad2+q*intsize;
                   for i := 1 to q do
                     begin popint(i1); ad2 := ad2-intsize; putint(ad2, i1) end
                 end;
    pi_lcp: begin popadr(ad);
                  pshadr(ad+ptrsize);
                  pshadr(getadr(ad)) end;
    pi_cps: begin popadr(ad1); popint(i1); popadr(ad2); popint(i2);
                       pshint(i2); pshadr(ad2); pshint(i1); pshadr(ad1);
                       if i1 <> i2 then errorv(ContainerMismatch)
                 end;
    pi_cpc: begin getq;
                  popadr(ad1); popadr(ad2);
                  popadr(ad3); popadr(ad4);
                       pshadr(ad4); pshadr(ad3); pshadr(ad2); pshadr(ad1);
                       for i := 1 to q do begin
                         if getint(ad2) <> getint(ad4) then
                           errorv(ContainerMismatch);
                         ad2 := ad2+ptrsize; ad4 := ad4+ptrsize
                       end
                 end;

    pi_aps: begin getq; popadr(ad1); popadr(ad); popadr(ad); popint(i1);
                       for i := 0 to i1*q-1 do begin
                         store[ad+i] := store[ad1+i]; putdef(ad+i, getdef(ad1+i))
                       end
                 end;
    pi_apc: begin getq; getq1; popadr(ad1); popadr(ad); popadr(ad); 
                       popadr(ad2);
                       for i := 1 to q do
                         begin q1 := q1*getint(ad2); ad2 := ad2+intsize end;
                       for i := 0 to q1-1 do begin
                         store[ad+i] := store[ad1+i]; putdef(ad+i, getdef(ad1+i))
                       end
                 end;
    pi_cxs: begin getq; popint(i); popadr(ad); popint(i1);
                       if (i < 1) or (i > i1) then errore(ValueOutOfRange);
                       pshadr(ad+(i-1)*q)
                 end;
    pi_cxc: begin getq; getq1; popint(i); popadr(ad); popadr(ad1);
                       ad2 := ad1+ptrsize;
                       for j := 1 to q-1 do
                         begin q1 := q1*getint(ad2); ad2 := ad2+intsize end;
                       if (i < 1) or (i > getint(ad1)) then
                         errore(ValueOutOfRange);
                       pshadr(ad1+ptrsize); pshadr(ad+(i-1)*q1)
                 end;
    pi_lft: begin getq; popadr(ad); pshadr(q); pshadr(ad) end;
    pi_max: begin getq; popint(i); popadr(ad1);
                       if q > 1 then popadr(ad) else popint(i1);
                       if (i < 1) or (i > q) then errorv(InvalidContainerLevel);
                       if q = 1 then i := i1
                       else i := getint(ad+(q-i)*intsize);
                       pshint(i)
                 end;
    pi_vdp,
    pi_vdd: begin popadr(ad); dspspc(0, ad) end;
    pi_spc: begin popadr(ad);
                  popadr(ad1);
                  pshint(getint(ad1));
                  pshadr(ad) end;
    pi_ccs: begin getq; getq1; popadr(ad); popadr(ad1); ad3 := ad1;
                       if q = 1 then q1 := q1*ad1
                       else for i := 1 to q do
                         begin q1 := q1*getint(ad3); ad3 := ad3+intsize end;
                       ad2 := sp-q1; alignd(stackelsize, ad2); sp := ad2;
                       for i := 0 to q1-1 do begin
                         store[ad2+i] := store[ad+i]; putdef(ad2+i, getdef(ad+i));
                       end;
                       pshadr(ad1); pshadr(ad2)
                 end;
    pi_scp: begin popadr(ad); popadr(ad1); popadr(ad2); putadr(ad2, ad);
                       putadr(ad2+ptrsize, ad1) end;
    pi_ldp: begin popadr(ad); pshadr(getadr(ad+ptrsize));
                       pshadr(getadr(ad)) end;
    pi_cpp: begin getq; getq1; ad := sp+q; sp := sp-q1; ad1 := sp;
                       for i := 1 to q1 do begin
                         store[ad1] := store[ad]; putdef(ad1, getdef(ad));
                         ad := ad+1; ad1 := ad1+1
                       end
                 end;
    pi_cpr: begin getq; getq1; ad := sp+q+q1; ad1 := sp+q;
                       for i := 1 to q do begin
                         ad := ad-1; ad1 := ad1-1;
                         store[ad] := store[ad1]; putdef(ad, getdef(ad1))
                       end;
                       sp := sp+q1
                 end;
    pi_lsa: begin getq; pshadr(sp+q) end;
    pi_cpl: begin popadr(ad1); popadr(ad2); pshadr(ad2); pshadr(ad1);
                       pshadr(ad2) end;
    pi_mdc: begin getq; popint(i1); pshint(i1); pshint(i1+q) end;

    pi_s2c: getq; { no-op in interpreter, skip operand }

    pi_eext: begin
                    i1 := sp+adrsize; { index parameters }
                    ExecuteExternal(pc-extvecbase, i1);
                    popadr(pc); { load return address }
                    sp := i1; { skip parameters }
                  end;

    { illegal instructions }
    pi_ph1, pi_ph2, pi_ph3, pi_ph4,
    pi_ph5, pi_ph6, pi_ph7, pi_ph8,
    pi_ph9, pi_mpc, pi_ph10:
      errorv(InvalidInstruction)

  end
end;

procedure debug;

label 2,3;

type errcod = (enumexp, edigbrx,elinnf,esyntax,eblknf,esymnam,esymntl,esnficc,
               etypnr,erecfnf,erecfna,etypna,einxoor,etypnp,eptrudf,eptrnas,
               eptrnil,eptrdis,eoprudf,estropr,efactor,erlni,eoprmbi,esetni,
               evalstr,einvsln,ecntpbk,ebktblf,enbpaad,ebadbv,ecntsct,ewtblf,
               einvwnm,ecmdni,ecmderr,etypmbus,einvcop,etypmir,eintexp,etypmat,
               etypset,eoprenm,erealrx,esetval,eutstrg,etyperr,etypmis,esystem,
               esrcudf,etypmirs,esymnfb,emssym,eblkmba,emodmba,enospcs,enomodss,
               echrrng);
     restyp = (rtint, rtreal, rtset, rtstrg);
     expres = record case t: restyp of
                       rtint:  (i: integer);
                       rtreal: (r: real);
                       rtset:  (s: settype);
                       rtstrg: (sc: strvsp; l: integer);
              end;

var dbc: parctl; cn: alfa; dbgend: boolean; x,p: integer;
    sn, sn2: filnam; snl: 1..fillen;
    ens: array [1..100] of integer; ad: address;
    fw: wthnum; undef: boolean; c: char; dc: dbgcmd; doanalyss: boolean;

procedure error(e: errcod);

begin
  wrtnewline; write('*** ');
  case e of { error }
    esyntax:  writeln('Syntax error');
    enumexp:  writeln('Number expected');
    edigbrx:  writeln('Digit beyond radix');
    elinnf:   writeln('Cannot find line');
    eblknf:   writeln('Block not found in symbol table');
    esymnam:  writeln('Symbol name expected');
    esymntl:  writeln('Symbol name too long');
    esnficc:  writeln('symbol not found in current context(s)');
    etypnr:   writeln('Type is not record');
    erecfnf:  writeln('Record field not found');
    erecfna:  writeln('Record field not active');
    etypna:   writeln('Type is not array');
    einxoor:  writeln('Index out of range');
    etypnp:   writeln('Type is not pointer');
    eptrudf:  writeln('Pointer undefined');
    eptrnas:  writeln('Pointer never assigned');
    eptrnil:  writeln('Pointer is nil');
    eptrdis:  writeln('Pointer has been disposed');
    eoprudf:  writeln('operand is undefined');
    estropr:  writeln('Structured operand to operator');
    efactor:  writeln('Error in factor');
    erlni:    writeln('Reals not implemented');
    eoprmbi:  writeln('Operand must be integer');
    esetni:   writeln('Sets not implemented');
    evalstr:  writeln('Value is structured');
    einvsln:  writeln('Invalid source line');
    ecntpbk:  writeln('Could not place breakpoint');
    ebktblf:  writeln('Breakpoint table full');
    enbpaad:  writeln('No breakpoint at address');
    ebadbv:   writeln('Bad byte value');
    ecntsct:  writeln('Cannot set complex type');
    ewtblf:   writeln('Watch table full');
    einvwnm:  writeln('Invalid watch number');
    ecmdni:   writeln('Command not implemented');
    ecmderr:  writeln('Command error');
    etypmbus: writeln('Operand type must be unstructured');
    einvcop:  writeln('Invalid combination of operands');
    etypmir:  writeln('Type must be integer or real');
    eintexp:  writeln('Integer result expected');
    etypmat:  writeln('Type of operands does not match');
    etypset:  writeln('Type must be set');
    eoprenm:  writeln('Operand cannot be enumerated type');
    erealrx:  writeln('Real number starts with radix specifier');
    esetval:  writeln('Value out of range for set');
    eutstrg:  writeln('Unterminated string constant');
    etyperr:  writeln('Error in type');
    etypmis:  writeln('Type mismatch');
    esrcudf:  writeln('Source is undefined');
    etypmirs: writeln('Type must be integer, real or set');
    esymnfb:  writeln('Symbol not found in block');
    emssym:   writeln('Must specify symbol in block');
    eblkmba:  writeln('Block containing symbol must be active');
    emodmba:  writeln('Module must be active');
    enospcs:  writeln('No special symbol found');
    enomodss: writeln('Cannot modify this special symbol');
    echrrng:  writeln('Character value out of range');
    esystem:  writeln('System error');
  end;
  goto 2
end;

procedure getlin(var pc: parctl);
var c: char;
begin
  with pc do begin
    b := nil; l := 0; write('debug> ');
    while not eoln do begin read(c); l := l+1; strchrass(b, l, c) end;
    readln; p := 1
  end;
  if doechlin then begin { echo the line for testing }
    writevp(output, pc.b);
    writeln
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

procedure texpect(var pc: parctl; c: char);
begin
  if chkchr(pc) <> c then error(etyperr);
  nxtchr(pc)
end;

procedure getnum(var pc: parctl; var n: integer);
var r: integer;
begin
   n := 0; r := 10; skpspc(pc);
   if not (chkchr(pc) in ['$', '&', '%', '0'..'9']) then error(enumexp);
   if chkchr(pc) = '$' then begin r := 16; nxtchr(pc) end
   else if chkchr(pc) = '&' then begin r := 8; nxtchr(pc) end
   else if chkchr(pc) = '%' then begin r := 2; nxtchr(pc) end;
   while (chkchr(pc) in ['0'..'9','_']) or
         ((chkchr(pc) in ['A'..'F','a'..'f']) and (r = 16)) do begin
     if chkchr(pc) <> '_' then begin
       if ((r = 2) and (chkchr(pc) > '1')) or
          ((r = 8) and (chkchr(pc) > '7')) then error(edigbrx);
       if chkchr(pc) in ['0'..'9'] then n := n*r+ord(chkchr(pc))-ord('0')
       else if chkchr(pc) in ['A'..'F'] then
         n := n*r+ord(chkchr(pc))-ord('A')+10
       else n := n*r+ord(chkchr(pc))-ord('a')+10
     end;
     nxtchr(pc)
   end
end;

procedure getnam(var pc: parctl);
var i: alfainx;
begin
  skpspc(pc);
  for i := 1 to maxalfa do cn[i] := ' '; i := 1;
  { note we abbreviate after 9 characters }
  while (chkchr(pc) <> ' ') and (chkchr(pc) <> ';') and not chkend(pc) do
    begin cn[i] := chkchr(pc); if i < maxalfa then i := i+1 else cn[i] := ' ';
    nxtchr(pc)
  end
end;

procedure getlab(var pc: parctl);
var i: alfainx;
begin
  skpspc(pc);
  for i := 1 to maxalfa do cn[i] := ' '; i := 1;
  { note we abbreviate after 9 characters }
  while chkchr(pc) in ['A'..'Z','a'..'z', '_'] do
    begin cn[i] := chkchr(pc); if i < maxalfa then i := i+1 else cn[i] := ' ';
    nxtchr(pc)
  end
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

procedure clrtmp;
var i: 1..maxbrk;
begin
  for i := 1 to maxbrk do
    if brktbl[i].temp then brktbl[i].sa := -1
end;

procedure prtrng(a, b: address);
var i: 1..maxdigh;
begin
  wrthex(output, a, maxdigh, true); write('-');
  if b+1 > a then wrthex(output, b, maxdigh, true)
  else for i := 1 to maxdigh do write('*');
  writeln(' (',b+1-a:1,')')
end;

procedure dmpmem(s, e: address; bl: integer; be: boolean);
   var i, x, xw, dl, rb: integer;
       bs: array [1..16] of ibyte; bd, br: array [1..16] of boolean;
       f, l: boolean;
       ba: address;
begin l := false; 
   for i := 1 to 16 do begin bs[i] := 0; bd[i] := false; br[i] := false end;
   dl := e-s+1; 
   if (dl mod bl) <> 0 then dl := dl-(dl mod bl)+bl else dl := dl-(dl mod bl);
   e := s+dl-1;
   while (s <= e) and not chkbrk do begin
     ba := s; i := 1; f := true;
     while (s <= e) and (i <= 16) do begin
       if ((bs[i] <> store[s]) and bd[i] and getdef(s)) or not br[i] then 
         f := false;
       if (bd[i] <> getdef(s)) and br[i] then f := false;
       bs[i] := store[s]; bd[i] := getdef(s); br[i] := true; s := s+1; i := i+1
     end;
     if not f or (i < 16) then begin
       if l then begin
         writeln;
         for x := 1 to maxdigh do write('*'); write(': ');
         for x := 1 to 16 do begin
           write('**');
           if (x mod bl) = 0 then write(' ')
         end;
         write('  ');
         for x := 1 to 16 do begin
           write('*');
           if (bl > 1) and ((x mod bl) = 0) then write(' ')
         end
       end;
       writeln; wrthex(output, ba, maxdigh, true); write(': ');
       if bl = 1 then begin
         for x := 1 to i-1 do begin
           if bd[x] then wrthex(output, bs[x], 2, true)
           else write('UU');
           write(' ') 
         end;
         for x := i to 16 do write('   ');
         write('  ');
         for x := 1 to i-1 do begin
           if bd[x] then begin
             if (bs[x] >= ord(' ')) and (bs[x] < 128) then write(chr(bs[x]))
             else write('.')
           end else write('U')
         end
       end else begin
         for x := 1 to i-1 do begin
           xw := x;
           if not be then 
             begin xw := x-1; xw := (xw-xw mod bl)+(bl-1-(xw mod bl))+1 end;
           if bd[xw] then wrthex(output, bs[xw], 2, true)
           else write('UU');
           if (x mod bl) = 0 then write(' ')
         end;
         rb := 16-i+1;
         if rb >= 1 then write(' ':rb div bl+rb*2);
         write('  ');
         for x := 1 to i-1 do begin
           if not be then 
             begin xw := x-1; xw := (xw-xw mod bl)+(bl-1-(xw mod bl))+1 end;
           if bd[xw] then begin
             if (bs[xw] >= ord(' ')) and (bs[xw] < 128) then write(chr(bs[xw]))
             else write('.')
           end else write('U');
           if (x mod bl) = 0 then write(' ')
         end
       end;
       l := false
     end else l := true
   end;
   writeln;
   writeln
end;

procedure prthdr;
var ad: address;
begin
  wrtnewline; writeln;
  if dodbgsrc and (curmod <> nil) then
    prtsrc(nil, srclin-1, srclin+1,false) { do source level }
  else begin { machine level }
    write('pc: '); wrthex(output, pc, maxdigh, true);
    write(' sp: '); wrthex(output, sp, maxdigh, true);
    write(' mp: '); wrthex(output, mp, maxdigh, true);
    write(' np: '); wrthex(output, np, maxdigh, true);
    writeln;
    ad := pc; lstinsa(ad)
  end;
  writeln
end;

procedure dmpdsp(mp: address);
begin
  wrtnewline; writeln;
  write('Mark @'); wrthex(output, mp, maxdigh, true); writeln;
  write('ep: '); wrthex(output, mp+markep, 8, true); write(': ');
  if getdef(mp+markep) then wrthex(output, getadr(mp+markep), 8, true)
  else write('********'); writeln;
  write('sb: '); wrthex(output, mp+marksb, 8, true); write(': ');
  if getdef(mp+marksb) then wrthex(output, getadr(mp+marksb), 8, true)
  else write('********'); writeln;
  write('et: '); wrthex(output, mp+market, 8, true); write(': ');
  if getdef(mp+market) then wrthex(output, getadr(mp+market), 8, true)
  else write('********'); writeln;
  writeln
end;

procedure dmpfrm(mp, ep: address; pc: address);
var bp: pblock; line: integer;
begin
  setcur;
  if curmod = nil then error(emodmba)
  else begin
    bp := fndblk(pc); { find address in block }
    if bp = nil then error(eblknf);
    wrtnewline; writeln;
    writev(output, bp^.name, lenpv(bp^.name)); write(': addr: ');
    wrthex(output, pc, 8, true);
    write(' locals/stack: '); wrthex(output, ep, 8, true); write('-');
    wrthex(output, mp-marksize, 8, true);
    write(' (', mp-marksize-ep:1, ')');
    writeln;
    if dodbgsrc and (curmod <> nil) then begin
      line := addr2line(curmod, pc); { get equivalent line }
      if line = 0 then error(elinnf);
      prtsrc(nil, line-1, line+1, false) { do source level }
    end else begin { machine level }
      ad := pc; lstinsa(ad)
    end;
    writeln
  end
end;

{ test if there are any frames }
function noframe: boolean;
begin
  noframe := mp  = maxtop { mp at top of memory }
end;

{ test if last frame }
function lastframe(ma: address): boolean;
begin
  lastframe := getadr(ma) = maxtop { ma at top of memory }
end;

{ find active symbol }
procedure symadr(var fs: psymbol; var ma: address);
var cpc: address;
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
        cpc := getadr(ma+adrsize+marksize); { get next frame pc }
        ma := getadr(ma)
      end
    until (fs <> nil) or (ma = maxtop); { found or no next frame }
  end
end;

procedure getsym(var pc: parctl);
var i: 1..fillen;
begin for i := 1 to fillen do sn[i] := ' '; snl := 1; skpspc(pc);
  if not (chkchr(pc) in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
    error(esymnam);
  while chkchr(pc) in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do begin
    if snl >= fillen then error(esymntl);
    sn[snl] := chkchr(pc); nxtchr(pc); snl := snl+1
  end;
  snl := snl-1
end;

procedure fndrot(var fbp: pblock);
var bp: pblock;
begin p := dbc.p; skpspc(dbc); fbp := nil;
  if chkchr(dbc) in ['a'..'z', 'A'..'Z', '_'] then begin
    getsym(dbc); bp := blklst;
    while bp <> nil do begin
      if strequvf(bp^.bname, sn) then begin fbp := bp; bp := nil end;
      if bp <> nil then bp := bp^.next
    end;
    if fbp = nil then dbc.p := p
  end
end;

procedure fndmnm(var fbp: pblock);
var bp: pblock;
begin p := dbc.p; skpspc(dbc); fbp := nil;
  if chkchr(dbc) in ['a'..'z', 'A'..'Z', '_'] then begin
    getsym(dbc); bp := blklst;
    while bp <> nil do begin
      if strequvf(bp^.name, sn) and (bp^.btyp in [btprog, btmod]) then
        begin fbp := bp; bp := nil end;
      if bp <> nil then bp := bp^.next
    end;
    if fbp = nil then dbc.p := p
  end
end;

procedure qualident(var bp: pblock; var fsp: psymbol; var ma: address);
var bp2, fbp: pblock; sp: psymbol;

begin
  fndrot(bp); { find head block }
  if bp <> nil then begin { found }
    fsp := nil; { symbol not found }
    while (chkchr(dbc) = '.') and (fsp = nil) do begin { parse qualifiers }
      nxtchr(dbc); bp2 := bp^.incnxt; getsym(dbc);
      fbp := nil; { search included blocks }
      while bp2 <> nil do begin
        if strequvf(bp2^.name, sn) then begin fbp := bp2; bp2 := nil end
        else bp2 := bp2^.incnxt
      end;
      if fbp <> nil then bp := fbp { advance to found block }
      else begin { not a sub-block name, search symbol }
        sp := bp^.symbols; fsp := nil;
        while sp <> nil do begin { traverse symbols list }
          if strequvf(sp^.name, sn) then begin fsp := sp; sp := nil end
          else sp := sp^.next
        end;
        if fsp = nil then error(esymnfb);
        if fsp^.styp <> stglobal then begin
          { search for active frame on block }
          ma := mp; ad := pc;
          while not ((ad >= bp^.bstart) and (ad < bp^.bend)) and
                (ma <> maxtop) do begin
            ad := getadr(ma+adrsize+marksize);
            ma := getadr(ma)
          end;
          if ma = maxtop then error(eblkmba)
        end
      end
    end;
    if fsp = nil then error(emssym);
  end
end;

function isbyte(v: integer): boolean;
begin isbyte := (v >= 0) and (v <= 255) end;

procedure getrng(var pc: parctl; var enum: boolean; var s, e: integer);
var si: integer; c: char;
begin enum := false; { not enumeration }
  { check for common types (yep, boolean and char can be a range) }
  if chkchr(pc) = 'b' then begin s := 0; e := 1 end
  else if chkchr(pc) = 'c' then begin s := ordminchar; e := ordmaxchar end
  { this is a cheat to make general integer sets work }
  else if chkchr(pc) = 'i' then begin s := setlow; e := sethigh end
  else begin
    texpect(pc, 'x'); { must be subrange or enum }
    texpect(pc, '('); if chkchr(pc) in ['0'..'9'] then begin { subrange }
      getnum(pc, s); texpect(pc, ','); getnum(pc, e); texpect(pc, ')')
    end else begin { enum }
      enum := true; s := 0; e := 0; si := 1;
      repeat ens[si] := pc.p; getsym(pc); e := e+1; c := chkchr(pc);
        if c = ',' then nxtchr(pc);
        if si < 100 then si := si+1
      until c <> ',';
      texpect(pc, ')'); e := e-1
     end
   end
end;

{ skip subrange if exists }
procedure skpsub(var pc: parctl);
var p, i: integer;
begin p := pc.p;
  if chkchr(pc) = 'x' then begin { possible subrange }
    nxtchr(pc); texpect(pc, '(');
    if chkchr(pc) in ['0'..'9'] then begin
      { we have a leader }
      getnum(pc, i); texpect(pc, ','); getnum(pc, i); texpect(pc, ')')
    end else pc.p := p
  end else pc.p := p
end;

procedure skptyp(var pc: parctl); forward;

{ skip fieldlist }
procedure skiplist(var pc: parctl);
var c: char; i: integer;
begin
  texpect(pc, '(');
  while chkchr(pc) <> ')' do begin
    getsym(pc); texpect(pc, ':'); getnum(pc, i); texpect(pc, ':'); skptyp(pc);
    if chkchr(pc) = '(' then begin nxtchr(pc);
      { tagfield, parse sublists }
      while chkchr(pc) <> ')' do begin getnum(pc, i); skiplist(pc) end;
      texpect(pc, ')')
    end;
    c := chkchr(pc); if c = ',' then nxtchr(pc)
  end;
  texpect(pc, ')')
end;

{ skip over, don't print, type }
procedure skptyp{(var pc: parctl)};
var s,e: integer; enum: boolean;
begin
  case chkchr(pc) of
    'i','b','c','n', 'p', 'e': nxtchr(pc);
    'x': begin getrng(pc, enum, s, e);
           if not enum then skptyp(pc)
         end;
    's': begin nxtchr(pc); getrng(pc, enum, s, e);
           if not enum then nxtchr(pc);
           skptyp(pc)
         end;
    'a': begin nxtchr(pc); getrng(pc, enum, s, e);
           if not enum then nxtchr(pc);
           skptyp(pc)
         end;
    'r': begin nxtchr(pc); skiplist(pc) end;
  end
end;

procedure setpar(var pc: parctl; td: strvsp; p: integer);
begin pc.b := td; pc.l := lenpv(td); pc.p := p end;

procedure wrtset(var st: settype; var tdc: parctl);
var s, e, ss, se: integer; enum: boolean; first: boolean;

procedure wrtval(i: integer);
begin
  if not enum then begin
    if chkchr(tdc) = 'c' then write('''', chr(i), '''')
    else if chkchr(tdc) = 'b' then begin
      if i = 0 then write('false(0)')
      else write('true(1)')
    end else write(i:1)
  end else if (e <= 100) and (i >= s) and (i <= e) then begin
    { output symbolic }
    x := ens[i]; { get start position }
    repeat
      c := strchr(tdc.b, x);
      if (c <> ',') and (c <> ')') then begin write(c); x := x+1 end
    until (c = ')') or (c = ',')
  end else write(i:1);
end;

begin
  getrng(tdc, enum, s, e);
  write('['); first := true;
  { we print the whole set even if the range does not cover it. This means we
    tell the user if bits are set, even if outside their range. }
  ss := setlow;
  repeat
    if ss in st then begin
      se := ss;
      while (se in st) and (se <= sethigh) do se := se+1; se := se-1;
      if not first then write(',');
      wrtval(ss);
      if se > ss then begin
        write('..'); wrtval(se); ss := se
      end;
      first := false
    end;
    ss := ss+1
  until ss > sethigh;
  write(']')
end;

{ crosscheck simple types }
procedure valsim(var v: expres; tdc: parctl);
var s, e: integer; enum: boolean;
begin
  case chkchr(tdc) of { type }
    'i','p','b','c','x': if v.t <> rtint then error(etypmis);
    'n': if v.t <> rtreal then error(etypmis);
    's': if v.t <> rtset then error(etypmis);
    'a': begin nxtchr(tdc); getrng(tdc, enum, s, e); { get range of index }
           if not enum then nxtchr(tdc); { discard index type, we don't need it }
           if (chkchr(tdc) = 'c') and (s = 1) then begin nxtchr(tdc);
             if v.t <> rtstrg then error(etypmis);
           end else error(esystem)
         end;
    'r','e','f': error(esystem); { should not happen }
  end
end;

{ print simple value }
procedure prtsim(var v: expres; var tdc: parctl; r: integer; fl: integer;
                 deffld: boolean; lz: boolean);
var s, e: integer; enum: boolean;
begin
  valsim(v, tdc); { validate value matches type }
  case chkchr(tdc) of { type }
    'i','p': begin nxtchr(tdc); wrtnum(output, v.i, r, fl, lz) end;
    'b': begin nxtchr(tdc);
           if v.i = 0 then write('false(0)') else write('true(1)')
         end;
    'c': begin nxtchr(tdc); write('''', chr(v.i), '''(', v.i:1, ')') end;
    'n': begin nxtchr(tdc); if deffld then write(v.r) else write(v.r:fl) end;
    'x': begin
           getrng(tdc, enum, s, e);
           if not enum then prtsim(v, tdc, r, fl, deffld, lz)
           else if (e < 100) and (v.i >= s) and (v.i <= e) then begin
             x := ens[v.i+1]; { get start position }
             repeat
               c := strchr(tdc.b, x);
               if (c <> ',') and (c <> ')') then begin write(c); x := x+1 end
             until (c = ')') or (c = ',');
             write('(', v.i:1, ')')
           end else write(v.i:1);
         end;
    's': begin nxtchr(tdc); wrtset(v.s, tdc) end;
    'a': begin nxtchr(tdc); getrng(tdc, enum, s, e); { get range of index }
           if not enum then nxtchr(tdc); { discard index type, we don't need it }
           if (chkchr(tdc) = 'c') and (s = 1) then begin nxtchr(tdc);
             { print as string constant }
             write(''''); writev(output, v.sc, v.l); write('''');
           end else error(esystem)
         end;
    'r','e','f': error(esystem); { should not happen }
  end
end;

{ print value by type }
procedure prttyp(var ad: address; td: strvsp; var p: integer; byt: boolean;
                 r: integer; fl: integer; deffld: boolean; lz: boolean;
                 indent: integer);
const ispc = 2;
var i: integer; s, e: integer;
    enum: boolean;
    ad2, ad3: address; tdc, stdc: parctl; ps: integer; v: expres;
    subc: boolean; fbyt: boolean;

procedure newline;
begin
  writeln; if indent > 0 then write(' ':indent)
end;

{ process fieldlist }
procedure fieldlist(var pc: parctl; line: boolean);
var i, x: integer; c: char; off: address;
begin
  texpect(pc, '(');
  while chkchr(pc) <> ')' do begin
    getsym(pc); texpect(pc, ':'); getnum(pc, i); off := i; texpect(pc, ':');
    ad2 := ad+off; ad3 := ad2;
    prttyp(ad2, td, pc.p, false, r, fl, deffld, lz, indent);
    if chkchr(pc) = '(' then begin nxtchr(pc);
      { tagfield, parse sublists }
      while chkchr(pc) <> ')' do begin
        { need to fetch and check if this case is active }
        getnum(pc, i);
        { get actual tagfield according to size }
        if ad2-ad3 = 1 then x := getbyt(ad+off) else x := getint(ad+off);
        if i = x then begin write('('); fieldlist(pc, line); write(')') end
        else skiplist(pc)
      end;
      texpect(pc, ')')
    end;
    c := chkchr(pc); if c = ',' then begin nxtchr(pc); write(', ') end;
    if line and (chkchr(pc) <> ')') then newline
  end;
  texpect(pc, ')')
end;

function complex(pc: parctl): boolean;
var cplx: boolean;

procedure skptyp(var pc: parctl); forward;

procedure skiplist(var pc: parctl);
var i: integer;
begin
  texpect(pc, '(');
  while chkchr(pc) <> ')' do begin
    getsym(pc); texpect(pc, ':'); getnum(pc, i); texpect(pc, ':');
    if chkchr(pc) in ['s','a','r'] then cplx := true;
    skptyp(pc);
    if chkchr(pc) = '(' then begin nxtchr(pc);
      { tagfield, parse sublists }
      while chkchr(pc) <> ')' do begin getnum(pc, i); skiplist(pc) end;
      texpect(pc, ')')
    end;
    c := chkchr(pc); if c = ',' then nxtchr(pc)
  end;
  texpect(pc, ')')
end;

procedure skptyp{(var pc: parctl)};
var s,e: integer; enum: boolean;
begin
  case chkchr(pc) of
    'i','b','c','n', 'p', 'e': nxtchr(pc);
    'x': begin getrng(pc, enum, s, e);
           if not enum then skptyp(pc)
         end;
    's': begin nxtchr(pc); getrng(pc, enum, s, e);
           if not enum then nxtchr(pc);
           skptyp(pc)
         end;
    'a': begin nxtchr(pc); getrng(pc, enum, s, e);
           if not enum then nxtchr(pc);
           skptyp(pc)
         end;
    'r': begin nxtchr(pc); skiplist(pc) end;
  end
end;

begin cplx := false;
  skiplist(pc);
  complex := cplx
end;

begin { prttyp }
  setpar(tdc, td, p); { set up type digest for parse }
  case chkchr(tdc) of
    'i': begin
           if getdef(ad) then begin v.t := rtint;
             if byt then begin v.i := getbyt(ad); ad := ad+1 end
             else begin v.i := getint(ad); ad := ad+intsize end;
             prtsim(v, tdc, r, fl, deffld, lz)
           end else begin nxtchr(tdc); write('Undefined') end;
         end;
    'b': begin
           if getdef(ad) then begin v.t := rtint;
             v.i := ord(getbol(ad));
             prtsim(v, tdc, r, fl, deffld, lz);
           end else begin nxtchr(tdc); write('Undefined') end;
           ad := ad+boolsize
         end;
    'c': begin
           if getdef(ad) then begin v.t := rtint;
             v.i := ord(getchr(ad));
             prtsim(v, tdc, r, fl, deffld, lz)
           end else begin nxtchr(tdc); write('Undefined') end;
           ad := ad+charsize
         end;
    'n': begin
           if getdef(ad) then begin v.t := rtreal;
             v.r := getrel(ad);
             prtsim(v, tdc, r, fl, deffld, lz)
           end else begin nxtchr(tdc); write('Undefined') end;
           ad := ad+realsize
         end;
    'x': begin
           { have to parse ahead to know type and size }
           stdc := tdc; getrng(tdc, enum, s, e);
           fbyt := isbyte(s) and isbyte(e);
           { It's subrange or enumerated. Subrange has a subtype. Note all
           subranges are reduced to numeric. }
           if not enum then begin { eval subtype }
             prttyp(ad, td, tdc.p, isbyte(s) and isbyte(e), r, fl, deffld, lz,
                    indent)
           end else begin { it's an enumeration, that's terminal }
             if getdef(ad) then begin v.t := rtint;
               { fetch according to size }
               if fbyt then v.i := getbyt(ad) else v.i := getint(ad);
               { skip address according to span }
               if byt then ad := ad+1 else ad := ad+intsize;
               tdc := stdc; prtsim(v, tdc, r, fl, deffld, lz)
             end else begin
               if byt then ad := ad+1 else ad := ad+intsize;
               write('Undefined')
             end
           end
         end;
    'p': begin nxtchr(tdc);
           { Pointers either give a full subtype or cycle back in the digest.
             Cycles have an offset number into the digest. }
           if chkchr(tdc) in ['0'..'9'] then getnum(tdc, tdc.p)
         end;
    's': begin
           if getdef(ad) then
             begin v.t := rtset; getset(ad, v.s);
                   prtsim(v, tdc, r, fl, deffld, lz) end
           else begin nxtchr(tdc); write('Undefined') end;
           ad := ad+setsize
         end;
    'a': begin nxtchr(tdc); getrng(tdc, enum, s, e);
           if not enum then nxtchr(tdc); { discard index type, we don't need it }
           if (chkchr(tdc) = 'c') and (s = 1) then begin
             { print as string constant }
             write('''');
             for i := s to e do
               if getdef(ad+i-s) then write(getchr(ad+i-s)) else write('*');
             write('''');
             nxtchr(tdc)
           end else begin subc := chkchr(tdc) in ['s','a','r'];
             write('array '); indent := indent+ispc;
             if subc then newline;
             { print whole array }
             ps := tdc.p;
             for i := s to e do begin
               prttyp(ad, td, tdc.p, false, r, fl, deffld, lz, indent);
               if i < e then begin tdc.p := ps; write(', ') end
               else indent := indent-ispc;
               if subc then newline
             end;
             if not subc then write(' ');
             write('end')
           end
         end;
    'r': begin nxtchr(tdc); subc := complex(tdc);
           write('record '); indent := indent+ispc;
           if subc then newline;
           fieldlist(tdc, subc);
           indent := indent-ispc;
           if subc then newline else write(' ');
           write('end')
         end;
    'e': writeln('Exception');
    'f': writeln('File');
  end;
  p := tdc.p { put back digest position (string does not change) }
end;

function siztyp(var tdc: parctl): integer;

var sz: integer; enum: boolean; s, e: integer;

function sizlst: integer;
var i: integer; c: char; sz, sz2, mxsz: integer;
begin
  sz := 0;
  texpect(tdc, '(');
  while chkchr(tdc) <> ')' do begin
    getsym(tdc); texpect(tdc, ':'); getnum(tdc, i); texpect(tdc, ':');
    { use field offset from digest plus field size to find record extent }
    sz2 := i+siztyp(tdc);
    if sz2 > sz then sz := sz2;
    if chkchr(tdc) = '(' then begin nxtchr(tdc); mxsz := 0;
      { tagfield, parse sublists }
      while chkchr(tdc) <> ')' do begin
        getnum(tdc, i); sz2 := sizlst; if sz2 > mxsz then mxsz := sz2
      end;
      texpect(tdc, ')');
      if mxsz > sz then sz := mxsz
    end;
    c := chkchr(tdc); if c = ',' then begin nxtchr(tdc) end
  end;
  texpect(tdc, ')');
  sizlst := sz
end;

begin
  case chkchr(tdc) of
    'i': begin nxtchr(tdc); sz := intsize end;
    'b': begin nxtchr(tdc); sz := boolsize end;
    'c': begin nxtchr(tdc); sz := charsize end;
    'n': begin nxtchr(tdc); sz := realsize end;
    'x': begin getrng(tdc, enum, s, e); if not enum then nxtchr(tdc);
           if isbyte(s) and isbyte(e) then sz := 1 else sz := intsize;
         end;
    'p': begin nxtchr(tdc); sz := ptrsize;
           if chkchr(tdc) in ['0'..'9'] then getnum(tdc, s)
           else s := siztyp(tdc)
         end;
    's': begin nxtchr(tdc); sz := setsize; s := siztyp(tdc) end;
    'a': begin nxtchr(tdc); getrng(tdc, enum, s, e); { get range of index }
           if not enum then nxtchr(tdc); sz := siztyp(tdc)*(e-s+1)
         end;
    'r': begin nxtchr(tdc); sz := sizlst end;
    'e': begin nxtchr(tdc); sz := exceptsize end;
    'f': begin nxtchr(tdc); sz := filesize; s := siztyp(tdc) end;
  end;
  siztyp := sz { return size }
end;

{ process variable reference }
procedure vartyp(var sp: psymbol; var ad: address; var p: integer);
var tdc: parctl; fnd, act: boolean; foff: address; ad2: address;
    enum: boolean; s, e: integer; sz: integer; ma: address; i: integer;
    fp: integer; ps: integer; bp: pblock;

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
  texpect(tdc, '(');
  while chkchr(tdc) <> ')' do begin
    getsym(tdc); texpect(tdc, ':'); getnum(tdc, i); off := i; texpect(tdc, ':');
    if strmat then
      begin fnd := true; foff := off; act := isact; fp := tdc.p end;
    sz := siztyp(tdc);
    if chkchr(tdc) = '(' then begin nxtchr(tdc);
      { tagfield, parse sublists }
      while chkchr(tdc) <> ')' do begin
        { need to fetch and check if this case is active }
        getnum(tdc, i);
        { get actual tagfield according to size }
        if sz = 1 then x := getbyt(ad+off) else x := getint(ad+off);
        matrec((i = x) and isact);
      end;
      texpect(tdc, ')')
    end;
    c := chkchr(tdc); if c = ',' then nxtchr(tdc);
  end;
  texpect(tdc, ')')
end;

begin { vartyp }
  qualident(bp, sp, ma); { process possible qualident }
  if bp = nil then begin { search all blocks in active frame order }
    getsym(dbc); symadr(sp, ma)
  end;
  p := 1;
  if sp = nil then error(esnficc);
  if (sp^.styp = stlocal) or (sp^.styp = stparam) then
    ad := ma+sp^.off { local }
  else ad := sp^.off; { global }
  setpar(tdc, sp^.digest, p); { set up type digest for parse }
  while chkchr(dbc) in ['.', '[', '^'] do begin
    if chkchr(dbc) = '.' then begin { record field }
      if chkchr(tdc) <> 'r' then error(etypnr);
      nxtchr(dbc); nxtchr(tdc); getsym(dbc); sn2 := sn; { get field and save }
      fnd := false; act := false; matrec(true);
      if not fnd then error(erecfnf);
      if not act then error(erecfna);
      ad := ad+foff; { set field address }
      tdc.p := fp { set type position }
    end else if chkchr(dbc) = '[' then begin { array index }
      if chkchr(tdc) <> 'a' then error(etypna);
      nxtchr(dbc); nxtchr(tdc); getrng(tdc, enum, s, e);
      if not enum then nxtchr(tdc);
      getnum(dbc, i); texpect(dbc, ']');
      if (i < s) or (i > e) then error(einxoor);
      ps := tdc.p; sz := siztyp(tdc); ad := ad+(i-s)*sz; { find element address }
      tdc.p := ps { back to base type }
    end else if chkchr(dbc) = '^' then begin { pointer dereference }
      if chkchr(tdc) <> 'p' then error(etypnp);
      nxtchr(dbc); nxtchr(tdc);
      { various pointer checks, see chka instruction }
      if not getdef(ad) then error(eptrudf);
      ad2 := getadr(ad);
      if ad2 = 0 then error(eptrnas);
      if ad2 = nilval then error(eptrnil);
      if (dochkrpt or donorecpar) and isfree(ad2) then error(eptrdis);
      ad := ad2;
      { Pointers either give a full subtype or cycle back in the digest.
        Cycles have an offset number into the digest. }
      if chkchr(tdc) in ['0'..'9'] then begin getnum(tdc, s); tdc.p := s end
    end
  end;
  p := tdc.p { put digest position }
end;

function mattyp(ldc, rdc: parctl): boolean;
begin
  while not chkend(ldc) and not chkend(rdc) and
        (chkchr(ldc) = chkchr(rdc)) do begin nxtchr(ldc); nxtchr(rdc) end;
  mattyp := chkend(ldc) and chkend(rdc)
end;

procedure gettmp(var sp: psymbol);
begin
  new(sp); sp^.next := tmpsym; tmpsym := sp; sp^.name := nil;
  sp^.styp := stglobal; sp^.off := 0; sp^.digest := nil
end;

procedure puttmps;
var sp: psymbol;
begin
  while tmpsym <> nil do begin
    sp := tmpsym; tmpsym := tmpsym^.next;
    putstrs(sp^.name); putstrs(sp^.digest);
    dispose(sp)
  end
end;

procedure float(var r: expres);
var f: real;
begin
  if r.t = rtint then begin f := r.i; r.t := rtreal; r.r := f end
end;

{ process expression or structured reference }
procedure exptyp(var syp: psymbol; var ad: address; var p: integer;
                 var r: expres; var simple: boolean; var undef: boolean);
type opstr = packed array [1..4] of char;

var l: expres; b: boolean; ldc: parctl; opin: boolean;

function matop(os: opstr; keyword: boolean): boolean;
var i,ps: integer;
begin
   i := 1; ps := dbc.p;
   while (os[i] = chkchr(dbc)) and (os[i] <> ' ') do
     begin i := i+1; nxtchr(dbc) end;
   matop := (os[i] = ' ') and
            not ((chkchr(dbc) in ['a'..'z', 'A'..'Z', '_','0'..'9']) and
                 keyword);
   dbc.p := ps
end;

procedure sexpr(var syp: psymbol; var ad: address; var p: integer;
                var r: expres; var simple: boolean; var undef: boolean);
var l: expres; c: char; f: real; ldc, rdc: parctl;

procedure term(var syp: psymbol; var ad: address; var p: integer;
               var r: expres; var simple: boolean; var undef: boolean);
var l: expres; f: real; ldc, rdc: parctl;

procedure factor(var syp: psymbol; var ad: address; var p: integer;
                 var r: expres; var simple: boolean; var undef: boolean);
var tdc: parctl; enum: boolean; s, e, i, ps, ev, sgn: integer; f: real; c: char;
    st: settype; first: boolean; ldc, rdc: parctl;

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

procedure pd(c: char);
begin strchrass(syp^.digest, p, c); p := p+1 end;

procedure pdn(i: integer);
var c: char; lz: boolean; p: integer;
begin
  p := maxpow10; lz := true;
  while p > 0 do begin
    c := chr(i div p mod 10+ord('0'));
    if not lz or (c <> '0') then begin pd(c); lz := false end;
    p := p div 10
  end
end;

begin syp := nil; undef := false; skpspc(dbc); c := chkchr(dbc);
  if chkchr(dbc) in ['$','&','%','0'..'9'] then begin
    getnum(dbc, i); simple := true; r.t := rtint; r.i := i; syp := intsym; p := 1;
    if ((chkchr(dbc) = '.') and not matop('..  ', false)) or
       (lcase(chkchr(dbc)) = 'e') then begin { real }
      f := i; ev := 0; if c in ['$','&','%'] then error(erealrx);
      if chkchr(dbc) = '.' then begin
        nxtchr(dbc); if not (chkchr(dbc) in ['0'..'9']) then error(enumexp);
        repeat
          f := f*10+ord(chkchr(dbc))-ord('0'); nxtchr(dbc); ev := ev-1
        until not (chkchr(dbc) in ['0'..'9'])
      end;
      if lcase(chkchr(dbc)) = 'e' then begin
        nxtchr(dbc); sgn := +1;
        if (chkchr(dbc) = '+') or (chkchr(dbc) = '-') then begin
          if chkchr(dbc) = '-' then sgn := -1;
          nxtchr(dbc)
        end;
        getnum(dbc, i); { get rest of exponent }
        ev := ev+i*sgn
      end;
      if ev < 0 then f := f/pwrten(ev) else f := f*pwrten(ev);
      r.t := rtreal; r.r := f; syp := realsym; p := 1
    end
  end else if matop('not ', true) then begin
    nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); factor(syp, ad, p, r, simple, undef);
    if undef then error(eoprudf);
    if not simple then error(estropr);
    if r.t <> rtint then error(etypmbus);
    setpar(tdc, syp^.digest, p);
    { if boolean treat as boolean, else whole integer }
    if chkchr(tdc) = 'b' then
      begin if r.i = 0 then r.i := 1 else r.i := 0 end
    else if chkchr(tdc) = 'x' then error(eoprenm)
    else r.i := bnot(r.i)
  end else if chkchr(dbc) in ['a'..'z', 'A'..'Z', '_'] then begin
    p := 1; vartyp(syp, ad, p);
    setpar(tdc, syp^.digest, p); { set up type digest for parse }
    if chkchr(tdc) in ['i', 'b','c','p','x','n','s'] then begin
      { scalar }
      simple := true;
      { note for simple loads we leave the digest pointing to type }
      if getdef(ad) then case chkchr(tdc) of
        'i','p': begin r.t := rtint; r.i := getint(ad) end;
        'b','c': begin r.t := rtint; r.i := getbyt(ad) end;
        'x': begin ps := tdc.p; r.t := rtint;
               getrng(tdc, enum, s, e);
               if isbyte(s) and isbyte(e) then r.i := getbyt(ad)
               else r.i := getint(ad);
               if enum then tdc.p := ps { return type as enum if so }
             end;
        'n': begin r.t := rtreal; r.r := getrel(ad) end;
        's': begin r.t := rtset; getset(ad, r.s) end;
        'a': begin ps := tdc.p; nxtchr(tdc); getrng(tdc, enum, s, e);
               if not enum then nxtchr(tdc);
               if (chkchr(tdc) = 'c') and (s = 1) then begin { string }
                 r.t := rtstrg; getstr(r.sc);
                 for i := 1 to e do
                   begin strchrass(r.sc, i, getchr(ad)); ad := ad+charsize end;
                 r.l := e
               end else simple := false;
               tdc.p := ps { back to original type }
             end
      end else undef := true
    end else simple := false;
    p := tdc.p
  end else if chkchr(dbc) = '(' then begin
    nxtchr(dbc); exptyp(syp, ad, p, r, simple, undef);
    texpect(dbc, ')')
  end else if chkchr(dbc) = '[' then begin { set constant }
    nxtchr(dbc); st := []; first := true;
    repeat { set elements }
      exptyp(syp, ad, p, r, simple, undef); setpar(rdc, syp^.digest, p);
      if undef then error(eoprudf);
      if not simple then error(estropr);
      if r.t <> rtint then error(etypmbus);
      if first then ldc := rdc;
      if not mattyp(ldc, rdc) then error(etypmat);
      if not isbyte(r.i) then error(esetval);
      first := false; s := r.i; e := s;
      if matop('..  ', false) then begin { range }
        nxtchr(dbc); nxtchr(dbc);
        exptyp(syp, ad, p, r, simple, undef);
        if undef then error(eoprudf);
        if not simple then error(estropr);
        if r.t <> rtint then error(etypmbus);
        if not mattyp(ldc, rdc) then error(etypmat);
        if not isbyte(r.i) then error(esetval);
        e := r.i
      end;
      for i := s to e do st := st+[i];
      c := chkchr(dbc);
      if c = ',' then nxtchr(dbc);
    until c <> ',';
    texpect(dbc, ']');
    r.t := rtset; r.s := st; { place result }
    { create set version of base type }
    gettmp(syp); strchrass(syp^.digest, 1, 's');
    for i := rdc.p to rdc.l do strchrass(syp^.digest, i+1, strchr(rdc.b, i));
    p := 1
  end else if chkchr(dbc) = '''' then begin { string }
    nxtchr(dbc); r.t := rtstrg; r.sc := nil; r.l := 0;
    repeat
      while not chkend(dbc) and (chkchr(dbc) <> '''') do begin
        r.l := r.l+1; strchrass(r.sc, r.l, chkchr(dbc)); nxtchr(dbc);
      end;
      if chkchr(dbc) <> '''' then error(eutstrg) else nxtchr(dbc);
      c := chkchr(dbc); 
      if chkchr(dbc) = '''' then begin
        r.l := r.l+1; strchrass(r.sc, r.l, chkchr(dbc)); nxtchr(dbc)
      end
    until c <> '''';
    if r.l = 1 then begin { single character }
      c := strchr(r.sc, 1); r.t := rtint; r.i := ord(c); syp := charsym
    end else begin
      { construct a type for fixed string }
      gettmp(syp); p := 1; pd('a'); pd('x'); pd('('); pd('1'); pd(','); pdn(r.l);
      pd(')'); pd('i'); pd('c');
    end;
    p := 1; simple := true
  end else if chkchr(dbc) = '*' then begin { address of }
    nxtchr(dbc); vartyp(syp, ad, p); syp := intsym; p := 1; r.t := rtint;
    r.i := ad; simple := true
  end else if chkchr(dbc) = '@' then begin { special symbol }
    nxtchr(dbc); getlab(dbc);p := 1; syp := intsym; r.t := rtint; simple := true;
    if cn = 'pc        ' then r.i := pc
    else if cn = 'sp        ' then r.i := sp
    else if cn = 'mp        ' then r.i := mp
    else if cn = 'np        ' then r.i := np
    else if cn = 'constants ' then r.i := cststr
    else if cn = 'globals   ' then r.i := pctop
    else if cn = 'heapbotto ' then r.i := gbtop
    else error(enospcs)
  end else error(efactor)
end;

procedure right;
begin factor(syp, ad, p, r, simple, undef);
  if undef then error(eoprudf);
  if not simple then error(estropr);
end;

begin { term }
  factor(syp, ad, p, r, simple, undef);
  skpspc(dbc);
  while (chkchr(dbc) in ['*', '/']) or matop('div ', true) or
        matop('mod ', true) or
     matop('and ', true) do begin { operator }
    if undef then error(eoprudf);
    if not simple then error(estropr);
    l := r; setpar(ldc, syp^.digest, p); c := chkchr(dbc);
    if chkchr(dbc) = '*' then begin nxtchr(dbc); right;
      if (l.t = rtstrg) or (r.t = rtstrg) then error(etypmirs);
      if (l.t = rtreal) or (r.t = rtreal) then
        begin  float(l); float(r); syp := realsym; p := 1 end;
      if (l.t = rtint) and (r.t = rtint) then r.i := l.i*r.i
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin f := l.r*r.r; r.t := rtreal; r.r := f end
      else if (l.t = rtset) and (r.t = rtset) then begin
        setpar(rdc, syp^.digest, p);
        if mattyp(ldc, rdc) then r.s := l.s*r.s
        else error(etypmat)
      end else error(einvcop)
    end else if chkchr(dbc) = '/' then begin nxtchr(dbc); right;
      if ((l.t <> rtint) and (l.t <> rtreal)) or
         ((r.t <> rtint) and (r.t <> rtreal)) then error(etypmir);
      float(l); float(r); syp := realsym; p := 1;
      f := l.r/r.r; r.t := rtreal; r.r := f
    end else if matop('div ', true) then begin
      nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); right;
      if (l.t <> rtint) or (r.t <> rtint) then error(eoprmbi);
      r.i := l.i div r.i
    end else if matop('mod ', true) then begin
      nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); right;
      if (l.t <> rtint) or (r.t <> rtint) then error(eoprmbi);
      r.i := l.i mod r.i
    end else if matop('and ', true) then begin
      nxtchr(dbc); nxtchr(dbc); nxtchr(dbc); right;
      if (l.t <> rtint) or (r.t <> rtint) then error(eoprmbi);
      r.i := band(l.i,r.i)
    end;
    skpspc(dbc)
  end
end;

procedure right;
begin term(syp, ad, p, r, simple, undef);
  if undef then error(eoprudf);
  if not simple then error(estropr)
end;

begin { sexpr }
  skpspc(dbc); c := chkchr(dbc); if c in ['+','-'] then nxtchr(dbc);
  term(syp, ad, p, r, simple, undef);
  if c in ['+','-'] then begin
    if undef then error(eoprudf);
    if not simple then error(eoprmbi);
    if (r.t <> rtint) and (r.t <> rtreal) then error(etypmir);
    if c = '-' then if r.t = rtint then r.i := -r.i else r.r := -r.r
  end;
  skpspc(dbc);
  while (chkchr(dbc) in ['+', '-']) or matop('or  ', true) or
        matop('xor ', true) do begin
    if undef then error(eoprudf);
    if not simple then error(estropr);
    l := r; setpar(ldc, syp^.digest, p);
    if chkchr(dbc) = '+' then begin nxtchr(dbc); right;
      if (l.t = rtstrg) or (r.t = rtstrg) then error(etypmirs);
      if (l.t = rtreal) or (r.t = rtreal) then
        begin float(l); float(r); syp := realsym; p := 1 end;
      if (l.t = rtint) and (r.t = rtint) then r.i := l.i+r.i
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin f := l.r+r.r; r.t := rtreal; r.r := f end
      else if (l.t = rtset) and (r.t = rtset) then begin
        setpar(rdc, syp^.digest, p);
        if mattyp(ldc, rdc) then r.s := l.s+r.s
        else error(etypmat)
      end else error(einvcop)
    end else if chkchr(dbc) = '-' then begin nxtchr(dbc); right;
      if (l.t = rtstrg) or (r.t = rtstrg) then error(etypmirs);
      if (l.t = rtreal) or (r.t = rtreal) then
        begin float(l); float(r); syp := realsym; p := 1 end;
      if (l.t = rtint) and (r.t = rtint) then r.i := l.i-r.i
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin f := l.r-r.r; r.t := rtreal; r.r := f end
      else if (l.t = rtset) and (r.t = rtset) then begin
        setpar(rdc, syp^.digest, p);
        if mattyp(ldc, rdc) then r.s := l.s-r.s
        else error(etypmat)
      end else error(einvcop)
    end else if matop('or  ', true) then begin nxtchr(dbc); nxtchr(dbc); right;
      if (l.t <> rtint) or (r.t <> rtint) then error(eoprmbi);
      r.i := bor(l.i, r.i)
    end else if matop('xor ', true) then begin nxtchr(dbc); nxtchr(dbc);
      if (l.t <> rtint) or (r.t <> rtint) then error(eoprmbi);
      nxtchr(dbc); right;
      r.i := bxor(l.i, r.i)
    end;
    skpspc(dbc)
  end
end;

procedure right;
var rdc: parctl;
begin sexpr(syp, ad, p, r, simple, undef);
  if undef then error(eoprudf);
  if not simple then error(estropr);
  setpar(rdc, syp^.digest, p);
  if opin then begin { a in b }
    if chkchr(rdc) <> 's' then error(etypset);
    nxtchr(rdc) { get base type }
  end;
  if not mattyp(ldc, rdc) then error(etypmat)
end;

begin { exptyp }
  sexpr(syp, ad, p, r, simple, undef); skpspc(dbc); opin := matop('in  ', true);
  if (chkchr(dbc) in ['=', '<', '>']) or opin then begin { operator }
    if undef then error(eoprudf);
    if not simple then error(estropr);
    l := r; setpar(ldc, syp^.digest, p);
    if chkchr(dbc) = '=' then begin nxtchr(dbc); right;
      if (l.t = rtreal) or (r.t = rtreal) then begin float(l); float(r) end;
      if (l.t = rtint) and (r.t = rtint) then r.i := ord(l.i = r.i)
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin b := l.r = r.r; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtset) and (r.t = rtset) then
        begin b := l.s = r.s; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtstrg) and (r.t = rtstrg) then
        begin b := strequvv(l.sc, r.sc); r.t := rtint; r.i := ord(b) end
      else error(einvcop)
    end else if matop('<>  ', false) then begin nxtchr(dbc); nxtchr(dbc); right;
      if (l.t = rtreal) or (r.t = rtreal) then begin float(l); float(r) end;
      if (l.t = rtint) and (r.t = rtint) then r.i := ord(l.i <> r.i)
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin b := l.r <> r.r; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtset) and (r.t = rtset) then begin
        begin b := l.s <> r.s; r.t := rtint; r.i := ord(b) end
      end else if (l.t = rtstrg) and (r.t = rtstrg) then
        begin b := not strequvv(l.sc, r.sc); r.t := rtint; r.i := ord(b) end
      else error(einvcop)
    end else if matop('<=  ', false) then begin nxtchr(dbc); nxtchr(dbc); right;
      if (l.t = rtreal) or (r.t = rtreal) then begin float(l); float(r) end;
      if (l.t = rtint) and (r.t = rtint) then r.i := ord(l.i <= r.i)
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin b := l.r <= r.r; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtset) and (r.t = rtset) then
        begin b := l.s <= r.s; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtstrg) and (r.t = rtstrg) then
        begin b := not strltnvv(r.sc, l.sc); r.t := rtint; r.i := ord(b) end
      else error(einvcop)
    end else if matop('>=  ', false) then begin nxtchr(dbc); nxtchr(dbc); right;
      if (l.t = rtreal) or (r.t = rtreal) then begin float(l); float(r) end;
      if (l.t = rtint) and (r.t = rtint) then r.i := ord(l.i >= r.i)
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin b := l.r >= r.r; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtset) and (r.t = rtset) then
        begin b := l.s >= r.s; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtstrg) and (r.t = rtstrg) then
        begin b := not strltnvv(l.sc, r.sc); r.t := rtint; r.i := ord(b) end
      else error(einvcop)
    end else if chkchr(dbc) = '<' then begin nxtchr(dbc); right;
      if (l.t = rtreal) or (r.t = rtreal) then begin float(l); float(r) end;
      if (l.t = rtint) and (r.t = rtint) then r.i := ord(l.i < r.i)
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin b := l.r < r.r; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtstrg) and (r.t = rtstrg) then
        begin b := strltnvv(l.sc, r.sc); r.t := rtint; r.i := ord(b) end
      else error(einvcop)
    end else if chkchr(dbc) = '>' then begin nxtchr(dbc); right;
      if (l.t = rtreal) or (r.t = rtreal) then begin float(l); float(r) end;
      if (l.t = rtint) and (r.t = rtint) then r.i := ord(l.i > r.i)
      else if (l.t = rtreal) and (r.t = rtreal) then
        begin b := l.r > r.r; r.t := rtint; r.i := ord(b) end
      else if (l.t = rtstrg) and (r.t = rtstrg) then
        begin b := strltnvv(r.sc, l.sc); r.t := rtint; r.i := ord(b) end
      else error(einvcop)
    end else if opin then begin nxtchr(dbc); nxtchr(dbc); right;
      if (l.t = rtint) and (r.t = rtset) then
        begin b := l.i in r.s; r.t := rtint; r.i := ord(b) end
      else error(einvcop)
    end;
    syp := boolsym; p := 1 { retype for boolean result }
  end
end;

{ process expression }
procedure expr(var i: integer);
var ad: address; sp: psymbol; p: integer; simple: boolean; undef: boolean;
    r: expres;
begin
  { get complex or simple reference }
  exptyp(sp, ad, p, r, simple, undef);
  if undef then error(eoprudf);
  if not simple then error(evalstr);
  if r.t <> rtint then error(eintexp);
  i := r.i
end;

function watchno(ad: address): wthnum;
var wn: wthnum; wi: wthinx;
begin
  wn := 0; for wi := 1 to maxwth do if wthtbl[wi] = ad then wn := wi;
  watchno := wn
end;

{ print watch }
procedure prtwth;
var dotrcinss: boolean;
begin
  dotrcinss := dotrcins; dotrcins := false; { have to turn off during this }
  wrtnewline; write('Watch variable: @'); wrthex(output, pc, 8, true);
  write(': '); writev(output, wthsym[fw].sp^.name, lenpv(wthsym[fw].sp^.name));
  write('@'); wrthex(output, wthtbl[fw], 8, true); write(': ');
  ad := wthtbl[fw]; p := wthsym[fw].p;
  if not getdef(ad) then write('*')
  else prttyp(ad, wthsym[fw].sp^.digest, p, false, 10, 1, true, false, 0);
  stopwatch := false; { let instruction run }
  sinins;
  stopwatch := true;
  write(' -> ');
  ad := wthtbl[fw]; p := wthsym[fw].p;
  if not getdef(ad) then write('*')
  else prttyp(ad, wthsym[fw].sp^.digest, p, false, 10, 1, true, false, 0);
  writeln;
  dotrcins := dotrcinss
end;

{ set breakpoint/tracepoint at address }
procedure setbrk(ad: address; line: integer; trace, temp: boolean);
var i, x: integer;
begin
  x := 0; for i := maxbrk downto 1 do if brktbl[i].sa < 0 then x := i;
  if x = 0 then error(ebktblf);
  brktbl[x].sa := ad; brktbl[x].line := line;
  brktbl[x].trace := trace;
  brktbl[x].temp := temp
end;

{ skip over call instruction }
procedure skpovr;
var ra: address; stop: boolean;
begin
  if store[pc] in [21{cal},12{cup},246{cuf},27{cuv},113{cip}] then begin
    sinins; { let the call place return on stack }
    ra := getadr(sp); { get the return address }
    stop := false;
    while (pc <> ra) and not chkbrk and not stop do begin
      stopins := false; { set no stop flag }
      watchmatch := false; { set no watch was matched }
      sinins;
      if watchmatch then begin watchmatch := false; prtwth end;
      { if we hit break or stop, just stay on that instruction }
      if breakins then begin
        writeln('*** Break instruction hit');
        pc := pc-1; stop := true
      end else if stopins then begin
        writeln('*** Stop instruction hit');
        pc := pc-1; stop := true
      end
    end
  end else sinins
end;

{ execute debug command }
procedure dbgins(dc: dbgcmd);
var i, x, p: integer; wi: wthinx; tdc, stdc: parctl; bp, bp2: pblock;
    syp: psymbol; si,ei: integer; sim: boolean; enum: boolean;
    s,e,pcs,eps: address; r: integer; fl: integer; lz: boolean; l: integer;
    eres: expres; deffld: boolean; brk: boolean; sls: integer;
    bl: integer; be: boolean;
begin
  case dc of { command }
    dcli: begin { list instructions }
      s := 0; e := lsttop-1; l := 10;
      skpspc(dbc);
      if not chkend(dbc) then begin expr(i); s := i end;
      skpspc(dbc);
      if chkchr(dbc) = ':' then
        begin nxtchr(dbc); expr(i); l := i end
      else if not chkend(dbc) then begin expr(i); e := i; l := maxint end;
      if e > lsttop-1 then e := lsttop-1;
      wrtnewline; writeln;
      writeln('    Addr    Op Ins         P  Q');
      writeln('----------------------------------');
      while (s <= e) and (l > 0) and not chkbrk do begin
        if isbrk(s) then write('b')
        else if istrc(s) then write('t')
        else write(' ');
        if pc = s then write('*') else write(' ');
        if getcov(s) then write('c') else write(' ');
        write(' ');
        wrthex(output, s, maxdigh, true);
        lstins(s);
        writeln;
        l := l-1
      end
    end;
    dcd, dcd8, dcd16, dcd32, dcd64, dcdb16, dcdb32, dcdb64, dcdl16, dcdl32, 
    dcdl64: begin { dump memory }
      s := 0; e := 255;
      skpspc(dbc); 
      if not chkend(dbc) then begin 
        expr(i); s := i; 
        if s+255 > maxstr then e := maxstr else e := s+255 
      end;
      skpspc(dbc);
      if chkchr(dbc) = ':' then begin
        nxtchr(dbc); expr(i); 
        if s+i-1 > maxstr then e := maxstr else e := s+i-1
      end else if not chkend(dbc) then begin expr(i); e := i end;
      case dc of { format }
        dcd:    begin bl := 1; be := false end;
        dcd8:   begin bl := 1; be := false end;
        dcd16:  begin bl := 2; be := false end;
        dcd32:  begin bl := 4; be := false end;
        dcd64:  begin bl := 8; be := false end;
        dcdb16: begin bl := 2; be := true end;
        dcdb32: begin bl := 4; be := true end;
        dcdb64: begin bl := 8; be := true end;
        dcdl16: begin bl := 2; be := false end;
        dcdl32: begin bl := 4; be := false end;
        dcdl64: begin bl := 8; be := false end
      end;
      wrtnewline; writeln;
      dmpmem(s, e, bl, be);
      writeln
    end;
    dcds: begin { dump storage specs }
      wrtnewline; writeln;
      writeln('Storage areas occupied');
      writeln;
      write('Program     '); prtrng(0, cststr-1);
      write('Constants   '); prtrng(cststr, pctop-1);
      write('Globals     '); prtrng(pctop, gbtop-1);
      write('Stack/Heap  '); prtrng(gbtop, maxstr);
      writeln
    end;
    dcdd: begin { dump displays }
      if noframe then
        begin wrtnewline; writeln; writeln('No displays active'); writeln end
      else begin
        i := maxint; skpspc(dbc); if not chkend(dbc) then expr(i);
        s := mp;
        repeat dmpdsp(s); e := s; s := getadr(s); i := i-1
        until (i = 0) or lastframe(e) or chkbrk
      end
    end;
    dcdf: begin
      if noframe then
        begin wrtnewline; writeln; writeln('No displays active'); writeln end
      else begin
        i := maxint; skpspc(dbc); if not chkend(dbc) then expr(i);
        s := mp; pcs := pc; eps := getadr(s+market);
        repeat dmpfrm(s, eps, pcs); pcs := getadr(s+adrsize+marksize);
          e := s; s := getadr(s);
          if not lastframe(e) then begin eps := getadr(s+market); i := i-1 end
        until (i = 0) or lastframe(e) or chkbrk
      end
    end;
    dcdst: begin { dump stack }
      i := 10; skpspc(dbc); if not chkend(dbc) then expr(i);
      s := sp;
      while (i > 0) and (s <= maxstr) and not chkbrk do begin
        wrthex(output, s, 8, true); write(': '); 
        if not getdef(s) then writeln('UUUUUUUUUUUUUUUU: (U)')
        else begin
          wrthex(output, getint(s), intsize*2, true); 
          writeln(' (', getint(s):1, ')')
        end;
        s := s+intsize; i := i-1
      end
    end;
    dcb,dctp: begin
      { place breakpoint/tracepoint source }
      fndmnm(bp); { find if module specified }
      if bp = nil then begin { none found }
        setcur; if curmod = nil then error(emodmba);
        bp := curmod; { set module as current }
      end;
      fndrot(bp2); { see if routine name }
      if bp2 <> nil then s := bp2^.bstart
      else begin { not a routine, get line no }
        expr(l); if l > maxsrc then error(einvsln);
        if bp^.lintrk^[l] < 0 then error(einvsln);
        s := bp^.lintrk^[l]
      end;
      skplmk(s); { skip preceeding line markers }
      skpmst(s); { skip mst instruction to start frame }
      skplmk(s); { skip trailing line markers }
      l := addr2line(bp, s);
      setbrk(s, l, dc = dctp, false)
    end;
    dcbi, dctpi: begin
      { place breakpoint/tracepoint instruction }
      expr(i); s := i;
      l := 0;
      if curmod <> nil then l := addr2line(curmod, s);
      setbrk(s, l, dc = dctpi, false)
    end;
    dcc: begin { clear breakpoint }
      skpspc(dbc); if not chkend(dbc) then begin
        expr(i); s := i; i := 0;
        for i := 1 to maxbrk do if brktbl[i].sa = s then x := i;
        if i = 0 then error(enbpaad);
        brktbl[x].sa := -1
      end else for i := 1 to maxbrk do brktbl[i].sa := -1
    end;
    dclb: begin { list breakpoints }
      wrtnewline; 
      lstbrk
    end;
    dcsi, dcsis, dcsio, dcsiso: begin { step instruction }
      i := 1; skpspc(dbc); if not chkend(dbc) then expr(i);
      while (i > 0) and not chkbrk do begin
        stopins := false; { set no stop flag }
        watchmatch := false; { set no watch was matched }
        if (dc = dcsio) or (dc = dcsiso) then skpovr else sinins;
        if watchmatch then begin watchmatch := false; prtwth end;
        if (dc = dcsi) or (dc = dcsio) then prthdr; i := i-1;
        { if we hit break or stop, just stay on that instruction }
        if breakins then begin
          writeln('*** Break instruction hit');
          pc := pc-1; i := 0
        end else if stopins then begin
          writeln('*** Stop instruction hit');
          pc := pc-1; i := 0
        end
      end
    end;
    dcl, dclc: begin { list source }
      fndmnm(bp); { find if module specified }
      if bp = nil then { none found }
        begin setcur; if curmod = nil then error(emodmba); bp := curmod end;
      s := 0; e := 10;
      skpspc(dbc); if not chkend(dbc) then begin expr(i); s := i; e := s+10-1 end;
      skpspc(dbc);
      if chkchr(dbc) = ':' then
        begin nxtchr(dbc); expr(i); e := s+i-1 end
      else if not chkend(dbc) then begin expr(i); e := i end;
      wrtnewline; writeln;
      prtsrc(bp, s, e, cn = 'lc        ');
      writeln
    end;
    dcs, dcss, dcso, dcsso: begin { step source line }
      i := 1; skpspc(dbc); if not chkend(dbc) then expr(i);
      sls := srclin; { save current source line }
      while i > 0 do begin
        repeat
          stopins := false; { set no stop flag }
          sourcemark := false; { set no source line instruction }
          watchmatch := false; { set no watch was matched }
          if (dc = dcso) or (dc = dcsso) then skpovr else sinins;
          brk := chkbrk;
          if watchmatch then begin watchmatch := false; prtwth end
        until stopins or (sourcemark and (srclin <> sls)) or brk;
        { advance over any other source markers }
        while store[pc] = mrkins do sinins;
        { advance over mst if present }
        if store[pc] = mstins then sinins;
        { advance over any source markers }
        while store[pc] = mrkins do sinins;
        if (dc = dcs) or (dc = dcso) then prthdr; i := i-1;
        if brk then begin
          writeln('*** Program stopped by user break');
          i := 0
        { if we hit break or stop, just stay on that instruction }
        end else if breakins then begin
          writeln('*** Break instruction hit');
          i := 0
        end else if stopins then begin
          writeln('*** Stop instruction hit');
          i := 0
        end
      end
    end;
    dcret: begin { return subroutine }
      if lastframe(mp) then 
        begin wrtnewline; writeln('*** Nothing to return to') end
      else begin
        setcur; if curmod = nil then error(emodmba);
        bp := curmod; { set module as current }
        s := getadr(mp+adrsize+marksize);
        { skip any source markers }
        while store[s] = mrkins do skplmk(s);
        l := addr2line(bp, s);
        setbrk(s, l, false, true);
        dbgend := true
      end
    end;
    dcp: begin { print (various) }
      { process variable/expression reference }
      exptyp(syp, s, p, eres, sim, undef);
      skpspc(dbc); r := 10; fl := 1; lz := false; deffld := true;
      { get any print radix }
      if chkchr(dbc) = '$' then begin r := 16; nxtchr(dbc) end
      else if chkchr(dbc) = '&' then begin r := 8; nxtchr(dbc) end
      else if chkchr(dbc) = '%' then begin r := 2; nxtchr(dbc) end;
      skpspc(dbc);
      if chkchr(dbc) = ':' then begin { there is a field }
        nxtchr(dbc); skpspc(dbc);
        if chkchr(dbc) = '#' then begin nxtchr(dbc); lz := true end;
        getnum(dbc, fl);
        deffld := false
      end;
      wrtnewline; writeln;
      if undef then write('*') { can't print, undefined }
      else if sim then begin { write simple result }
        setpar(tdc, syp^.digest, p);
        prtsim(eres, tdc, r, fl, deffld, lz)
      end else
        { print the resulting tail }
        prttyp(s, syp^.digest, p, false, r, fl, deffld, lz, 0);
      writeln;
      writeln
    end;
    dce: begin { enter (hex) }
      expr(i); s := i; { get address }
      repeat
        expr(i);
        if (i > 255) or (i < 0) then error(ebadbv);
        store[s] := i;
        s := s+1
      until chkend(dbc) or (s < 0)
    end;
    dcst: begin { set (variable) }
      skpspc(dbc); if chkchr(dbc) = '@' then begin nxtchr(dbc); getlab(dbc);
        if cn = 'pc        ' then begin expr(i); pc := i end
        else if cn = 'sp        ' then begin expr(i); sp := i end
        else if cn = 'mp        ' then begin expr(i); mp := i end
        else if cn = 'np        ' then begin expr(i); np := i end
        else if (cn = 'constants ') or (cn = 'globals   ') or 
                (cn = 'heapbotto ') then error(enomodss)
        else error(enospcs)
      end else begin
        vartyp(syp, ad, p); setpar(tdc, syp^.digest, p);
        exptyp(syp, s, p, eres, sim, undef); setpar(stdc, syp^.digest, p);
        if undef then error(esrcudf);
        if sim then begin { simple }
          if chkchr(tdc) in ['i', 'b','c','p','x','n','s','a'] then begin
            case chkchr(tdc) of
              'i','p': begin if eres.t <> rtint then error(etypmis);
                         putint(ad, eres.i)
                       end;
              'b','c': begin if eres.t <> rtint then error(etypmis);
                         if (eres.i < 0) or (eres.i > 255) then error(echrrng);
                         putbyt(ad, eres.i mod 256)
                       end;
              'x': begin if eres.t <> rtint then error(etypmis);
                     getrng(tdc, enum, si, ei);
                     if not enum then nxtchr(tdc);
                     if isbyte(si) and isbyte(ei) then putbyt(ad, eres.i)
                     else putint(ad, eres.i)
                   end;
              'n': begin float(eres); if eres.t <> rtreal then error(etypmis);
                     putrel(ad, eres.r)
                   end;
              's': begin if eres.t <> rtreal then error(etypmis);
                     skpsub(tdc); skpsub(stdc);
                     if not mattyp(tdc, stdc) then error(etypmat);
                     putset(ad, eres.s)
                   end;
              'a': begin if eres.t <> rtstrg then error(etypmis);
                     if not mattyp(tdc, stdc) then error(etypmat);
                     nxtchr(stdc); getrng(stdc, enum, si, ei);
                     if not enum then nxtchr(stdc);
                     if (chkchr(stdc) = 'c') and (si = 1) then begin { string }
                       for i := 1 to ei do
                         begin putbyt(ad, ord(strchr(eres.sc, i)));
                               ad := ad+charsize end;
                     end else error(etypmis)
                   end;
            end
          end else error(etypmis)
        end else begin { set complex }
          if not mattyp(tdc, stdc) then error(etypmat);
          case chkchr(stdc) of
            'i','b','c','n','x','p','s','e','f': error(esystem);
            'a','r': begin x := siztyp(stdc);
                       for i := 1 to x do
                         begin store[ad] := store[s]; putdef(ad, getdef(s));
                               ad := ad+1; s := s+1 end;
                     end
          end
        end
      end
    end;
    dcw: begin { watch (variable) }
      vartyp(syp, ad, p); setpar(tdc, syp^.digest, p);
      { find free watch entry }
      fw := 0; for wi := maxwth downto 1 do if wthtbl[wi] < 0 then fw := wi;
      if fw = 0 then error(ewtblf);
      wthtbl[fw] := ad; wthsym[fw].sp := syp; wthsym[fw].p := p
    end;
    dclw: begin { list watch table }
      wrtnewline; writeln;
      writeln('Watch table:');
      writeln;
      for wi := 1 to maxwth do if wthtbl[wi] >= 0 then
        begin write(wi:1, ': '); wrthex(output, wthtbl[wi], 8, true); writeln end;
      writeln
    end;
    dccw: begin { clear watch table }
      if not chkend(dbc) then begin expr(i);
        if (i < 1) or (i > maxwth) then error(einvwnm);
        { move entries down to gap }
        for x := 1 to maxwth-1 do wthtbl[x] := wthtbl[x+1];
        wthtbl[maxwth] := -1
      end else for wi := 1 to maxwth do wthtbl[wi] := -1
    end;
    dclia: begin { list instruction analysis }
      i := lstana(aniptr); writeln; writeln('last instructions executed:');
      wrtnewline; writeln;
      while (i > 0) and not chkbrk do begin
        if anitbl[i] < 0 then i := 0
        else begin
          s := anitbl[i]; lstinsa(s); i := lstana(i);
          if i = lstana(aniptr) then i := 0
        end
      end;
      writeln
    end;
    dclsa: begin { list source analysis }
      i := lstana(ansptr); writeln; writeln('last source lines executed:');
      wrtnewline; writeln;
      while (i > 0) and not chkbrk do begin
        if anstbl[i] <= 0 then i := 0
        else begin
          prtsrc(ansmtbl[i], anstbl[i], anstbl[i], false); i := lstana(i);
          if i = lstana(ansptr) then i := 0
        end
      end;
      writeln
    end;
    dcpg: begin { print globals }
      wrtnewline; writeln; writeln('Globals:'); writeln;
      bp := blklst; { index top of block list }
      brk := false; { set no break }
      while (bp <> nil) and not brk do begin
        syp := bp^.symbols;
        while (syp <> nil) and not brk do begin { traverse symbols list }
          if syp^.styp = stglobal then begin
            writev(output, syp^.name, 20); write(' ');
            s := syp^.off; p := 1;
            prttyp(s, syp^.digest, p, false, 10, 1, true, false, 0);
            writeln;
          end;
          syp := syp^.next;
          brk := chkbrk
        end;
        bp := bp^.next
      end;
      writeln
    end;
    dcpl, dcpp: begin { print locals }
      if noframe then
        begin wrtnewline; writeln; writeln('No displays active'); writeln end
      else begin
        i := 1; skpspc(dbc); if not chkend(dbc) then expr(i);
        s := mp; pcs := pc;
        wrtnewline;
        brk := false;
        repeat
          bp := fndblk(pcs);
          if bp = nil then error(eblknf);
          if (bp^.btyp = btproc) or (bp^.btyp = btfunc) then begin
            writeln;
            if (cn = 'pl        ') then write('Locals for block: ')
            else write('Parameters for block: ');
            writev(output, bp^.name, lenpv(bp^.name));
            writeln; writeln;
            syp := bp^.symbols;
            while (syp <> nil) and not brk do begin { traverse symbols list }
              if ((syp^.styp = stlocal) and (cn = 'pl        ')) or
                 ((syp^.styp = stparam) and (cn = 'pp        ')) then begin
                writev(output, syp^.name, 20); write(' ');
                e := s+syp^.off; p := 1;
                prttyp(e, syp^.digest, p, false, 10, 1, true, false, 0);
                writeln
              end;
              syp := syp^.next;
              brk := chkbrk
            end;
            writeln
          end;
          pcs := getadr(s+adrsize+marksize); e := s; s := getadr(s); i := i-1
        until (i = 0) or lastframe(e) or brk
      end
    end;
    dchs: repspc; { report heap space }
    dcti: dotrcins := true; { trace instructions }
    dcnti: dotrcins := false; { no trace instructions }
    dctr: dotrcrot := true; { trace routine executions }
    dcntr: dotrcrot := false; { no trace routine executions }
    dcts: dotrcsrc := true; { trace source lines }
    dcnts: dotrcsrc := false; { no trace source lines }
    dcspf: dosrcprf := true; { source level profiling }
    dcnspf: dosrcprf := false; { no source level profiling }
    dcic: dochkcov := true; { instruction level coverage }
    dcnic: dochkcov := false; { no instruction level coverage }
    dcan: doanalys := true; { do analyze }
    dcnan: doanalys := false; { no analyze }
    dcps: prthdr; { print status }
    dcr: dbgend := true; { run }
    dcq: goto 99; { quit }
    dch, dchelp: begin
      wrtnewline; writeln;
      writeln('Commands:');
      writeln;
      writeln('h|help                             Help (this command)');
      writeln('l                   [m] [s[ e|:l]  List source lines');
      writeln('lc                  [m] [s[ e|:l]  List source and machine lines coordinated');
      writeln('li                  [s[ e|:l]      List machine instructions');
      writeln('p                   v              Print expression');
      writeln('d[b|l][8|16|32|64]  [s[ e|:l]      Dump memory');
      writeln('e                   a v[ v]...     Enter byte values to memory address');
      writeln('st                  d v            Set program variable');
      writeln('pg                                 Print all globals');
      writeln('pl                  [n]            print locals for current/number of enclosing');
      writeln('                                   blocks');
      writeln('pp                  [n]            print parameters for current/number of');
      writeln('                                   enclosing blocks');
      writeln('ds                                 Dump storage parameters');
      writeln('dd                  [n]            Dump display frames');
      writeln('df                  [n]            Dump frames formatted (call trace)');
      writeln('dst                 [n]            Dump stack words');
      writeln('b                   [m] a          Place breakpoint at source line');
      writeln('                                   number/routine');
      writeln('tp                  [m] a          Place tracepoint at source line');
      writeln('                                   number/routine');
      writeln('bi                  a              Place breakpoint at instruction');
      writeln('tpi                 a              Place tracepoint at instruction');
      writeln('c                   [a]            Clear breakpoint/all breakpoints');
      writeln('lb                                 List active breakpoints');
      writeln('w                   a              Watch variable');
      writeln('lw                                 List watch table');
      writeln('cw                  [n]            Clear watch table entry/all watch entries');
      writeln('lia                                List instruction analyzer buffer');
      writeln('lsa                                List source analyzer buffer');
      writeln('s                   [n]            Step next source line execution');
      writeln('ss                  [n]            Step next source line execution silently');
      writeln('si                  [n]            Step instructions');
      writeln('sis                 [n]            Step instructions silently');
      writeln('so                  [n]            Step over next source line execution');
      writeln('sso                 [n]            Step over next source line execution silently');
      writeln('sio                 [n]            Step over instructions');
      writeln('siso                [n]            Step over instructions silently');
      writeln('ret                                Return from subroutine');
      writeln('hs                                 Report heap space');
      writeln('ti                                 Turn instruction tracing on');
      writeln('nti                                Turn instruction tracing off');
      writeln('tr                                 Turn system routine tracing on');
      writeln('ntr                                Turn system routine tracing off');
      writeln('ts                                 Turn source line tracing on');
      writeln('nts                                Turn source line tracing off');
      writeln('spf                                Turn on source level profiling');
      writeln('nspf                               Turn off source level profiling');
      writeln('an                                 Turn on analyzer mode');
      writeln('nan                                Turn off analyzer mode');
      writeln('r                                  Run program from current pc');
      writeln('ps                                 Print current registers and instruction');
      writeln('q                                  Quit interpreter');
      writeln;
      writeln('!                                  Anywhere in line starts a comment');
      writeln
    end;
  { these are internal debugger commands }
    dclistline: begin
      setcur; if curmod = nil then error(emodmba);
      wrtnewline; writeln;
      writeln('Defined line to address entries:');
      writeln;
      for i := 1 to maxsrc do if curmod^.lintrk^[i] >= 0 then begin
        write(i:4, ':'); wrthex(output, curmod^.lintrk^[i], 8, true); writeln
      end;
      writeln
    end;
    dcdumpsymbo: begin
      wrtnewline; writeln;
      writeln('Symbols:');
      writeln;
      bp := blklst;
      brk := false;
      while (bp <> nil) and not brk do begin
        write('Block: '); writev(output, bp^.name, 20);
        write(' ');
        case bp^.btyp of
          btprog: write('program');
          btmod:  write('module');
          btproc: write('procedure');
          btfunc: write('function')
        end;
        write(' ');
        wrthex(output, bp^.bestart, 8, true); write(' ');
        wrthex(output, bp^.bstart, 8, true); write(' ');
        wrthex(output, bp^.bend, 8, true);
        writeln;
        writeln;
        syp := bp^.symbols;
        while (syp <> nil) and not brk do begin
          write('   Symbol: '); writev(output, syp^.name, 40);
          write(' ');
          case syp^.styp of
            stglobal: write('global');
            stlocal: write('local ');
            stparam:  write('param ')
          end;
          write(' '); prthex(syp^.off); write(' ');
          writev(output, syp^.digest, lenpv(syp^.digest));
          writeln; syp := syp^.next;
          brk := chkbrk
        end;
        writeln;
        bp := bp^.next
      end;
      writeln
    end;
    dcnone: error(ecmderr)
  end;
  puttmps { clear any temps }
end;

{ find debug command }
procedure fndcmd(var dc: dbgcmd);
var dci: dbgcmd;
begin
  dc := dcnone;
  for dci := dcnone to dcdumpsymbo do if cn = dbgcmds[dci] then dc := dci;
end;

begin { debug }
  if watchmatch then begin { a variable watch matched, handle special }
    watchmatch := false;
    fw := watchno(stoad); { get the watch number }
    if fw > 0 then
      { obviously system error if we don't find the watch, but just ignore }
      prtwth;
    goto 3 { skip main section }
  end;
  if not debugstart then begin
    wrtnewline; writeln;
    writeln('P6 debug mode');
    writeln
  end;
  getbrk;
  { if we broke on line, fix the line to point }
  if isbrk(pc) then begin { standing on a breakpoint }
      { get source line from breakpoint table if exists }
      x := fndbrk(pc);
      if x >= 1 then if brktbl[x].line > 0 then srclin := brktbl[x].line
  end;
  { if we broke on a source marker, execute it then back up.
    This is because break on source line always will do this, and we need the
    source line from the instruction. }
  if store[pc] = mrkins then begin 
    doanalyss := doanalys; { supress analysis on this }
    doanalys := false;
    sinins; 
    doanalys := doanalyss;
    pc := pc-(mrkinsl+intsize) 
  end;
  dbgend := false;
  debugstart := true; { set we started }
  clrtmp; { clear temp breakpoints }
  prthdr;

  2: { error reenter interpreter }
  puttmps; { clear any temps }
  if not istrc(pc) then begin { not tracepoint, enter debugger cli }
    wrtnewline;
    repeat
      getlin(dbc);
      repeat { statements }
        skpspc(dbc);
        if chkchr(dbc) <> '!' then begin
          if not chkend(dbc) then begin
            getnam(dbc);
            fndcmd(dc);
            dbgins(dc)
          end;
          c := chkchr(dbc);
          if c = ';' then nxtchr(dbc)
        end
      until (c <> ';') or (c = '!')
    until dbgend
  end;
  { single step past entry breakpoint (if it exists) }
  if isbrk(pc) then begin
    sinins;
    if watchmatch then begin watchmatch := false; prtwth end
  end;
  putbrk; { put back breakpoints }
  3: { exit from watch }
end;

procedure maktyp(var sp: psymbol; c: char);
begin
  new(sp);
  with sp^ do begin
    next := nil; name := nil; styp := stglobal; off := 0; digest := nil;
    strchrass(digest, 1, c);
  end
end;

procedure wrtdck;
var ad,ad2: address; l, cs: integer;
begin
  ad := 0;
  while ad < pctop do begin { output deck }
    { output header }
    l := pctop-ad; if l > 16 then l := 16;
    write(prr, ':'); wrthex(prr, l, 2, true); wrthex(prr, ad, 16, true);
    cs := 0;
    for ad2 := ad to ad+l-1 do begin
      cs := (cs+store[ad2]) mod 256;
      wrthex(prr, store[ad2], 2, true)
    end;
    wrthex(prr, cs, 2, true);
    writeln(prr);
    ad := ad+l
  end;
  writeln(prr, ':00000000000000000000')
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
      29: begin amd64_sysv := option[oi];
            if amd64_sysv then begin
              writeln('*** Calling convention mismatch');
              goto 99
            end
          end;
      2:; 3:; 4:; 12:; 20:; 21:; 22:;
      24:; 25:; 26:; 10:; 18:; 27:; 28:;
    end
end;

begin (* main *)

  { suppress unreferenced errors. }
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

  write('P6 Pascal interpreter vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  writeln;

  { capture user breaks, if possible }
  breakflag := false;
  capture;

  { supress errors on breakflag, only used in extention packages }
  if breakflag = true then;

  { find integer parameters }
  i := maxint;  maxdig := 0;
  while i > 0 do begin maxdig := maxdig+1; i := i div 10 end;

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
  amd64_sysv := false; { don't use SYS V AMD64 ABI calling convention }
  doechlin := false; { don't echo command lines }

  strcnt := 0; { clear string quanta allocation count }
  blkstk := nil; { clear symbols block stack }
  blklst := nil; { clear symbols block discard list }
  errsinprg := 0; { set no source errors }
  aniptr := 1; { clear analysis }
  ansptr := 1;
  newline := true; { set on new line }
  gbsiz := 0;
  curmod := nil; { set no module active }
  varlst := nil; { set no VAR block entries }
  varfre := nil;
  wthlst := nil; { set no with block entries }
  wthcnt := 0;
  wthfre := nil;

  { initialize command table }
  dbgcmds[dcnone]      := '          ';
  dbgcmds[dcli]        := 'li        '; 
  dbgcmds[dcd]         := 'd         '; 
  dbgcmds[dcd8]        := 'd8        '; 
  dbgcmds[dcd16]       := 'd16       '; 
  dbgcmds[dcd32]       := 'd32       '; 
  dbgcmds[dcd64]       := 'd64       '; 
  dbgcmds[dcdb16]      := 'db16      '; 
  dbgcmds[dcdb32]      := 'db32      '; 
  dbgcmds[dcdb64]      := 'db64      '; 
  dbgcmds[dcdl16]      := 'dl16      '; 
  dbgcmds[dcdl32]      := 'dl32      '; 
  dbgcmds[dcdl64]      := 'dl64      '; 
  dbgcmds[dcds]        := 'ds        '; 
  dbgcmds[dcdd]        := 'dd        '; 
  dbgcmds[dcdf]        := 'df        '; 
  dbgcmds[dcdst]       := 'dst       '; 
  dbgcmds[dcb]         := 'b         '; 
  dbgcmds[dctp]        := 'tp        '; 
  dbgcmds[dcbi]        := 'bi        '; 
  dbgcmds[dctpi]       := 'tpi       ';
  dbgcmds[dcc]         := 'c         '; 
  dbgcmds[dclb]        := 'lb        ';
  dbgcmds[dcsi]        := 'si        '; 
  dbgcmds[dcsis]       := 'sis       '; 
  dbgcmds[dcl]         := 'l         '; 
  dbgcmds[dclc]        := 'lc        '; 
  dbgcmds[dcs]         := 's         '; 
  dbgcmds[dcss]        := 'ss        '; 
  dbgcmds[dcp]         := 'p         '; 
  dbgcmds[dce]         := 'e         '; 
  dbgcmds[dcst]        := 'st        '; 
  dbgcmds[dcw]         := 'w         '; 
  dbgcmds[dclw]        := 'lw        ';
  dbgcmds[dccw]        := 'cw        '; 
  dbgcmds[dclia]       := 'lia       '; 
  dbgcmds[dclsa]       := 'lsa       '; 
  dbgcmds[dcpg]        := 'pg        '; 
  dbgcmds[dcpl]        := 'pl        '; 
  dbgcmds[dcpp]        := 'pp        '; 
  dbgcmds[dchs]        := 'hs        '; 
  dbgcmds[dcti]        := 'ti        '; 
  dbgcmds[dcnti]       := 'ti        '; 
  dbgcmds[dctr]        := 'tr        ';
  dbgcmds[dcntr]       := 'ntr       '; 
  dbgcmds[dcts]        := 'ts        '; 
  dbgcmds[dcnts]       := 'nts       '; 
  dbgcmds[dcspf]       := 'spf       '; 
  dbgcmds[dcnspf]      := 'nspf      ';
  dbgcmds[dcic]        := 'ic        ';
  dbgcmds[dcnic]       := 'nic       '; 
  dbgcmds[dcan]        := 'an        '; 
  dbgcmds[dcnan]       := 'nan       '; 
  dbgcmds[dcps]        := 'ps        '; 
  dbgcmds[dcr]         := 'r         '; 
  dbgcmds[dcq]         := 'q         '; 
  dbgcmds[dch]         := 'h         ';
  dbgcmds[dcso]        := 'so        ';
  dbgcmds[dcsso]       := 'sso       ';
  dbgcmds[dcsio]       := 'sio       ';
  dbgcmds[dcsiso]      := 'siso      ';
  dbgcmds[dcret]       := 'ret       ';
  dbgcmds[dchelp]      := 'help      '; 
  dbgcmds[dclistline]  := 'listline  '; 
  dbgcmds[dcdumpsymbo] := 'dumpsymbo ';

  exitcode:= 0; { clear program exit code }
  { endian flip status is set if the host processor and the target disagree on
    endian mode }
  flipend := litend <> lendian;
  maktyp(boolsym, 'b'); { create standard types }
  maktyp(realsym, 'n');
  maktyp(intsym, 'i');
  maktyp(charsym, 'c');
  fndpow(maxpow10, 10, decdig);
  fndpow(maxpow16, 16, hexdig);
  fndpow(maxpow8, 8, octdig);
  { adding sign bit to bindig causes overflow }
  fndpow(maxpow2, 2, bindig); {bindig := bindig+1;} { add sign bit }

  { get the command line }
  getcommandline;
  cmdpos := 1;
  paroptions; { parse command line options }
  { parse header files }
  parhdrfil(prd, prdval, '.p6 ');
  if not prdval then begin
    writeln('*** Error: input filename not found');
    goto 99
  end;
  paroptions; { parse command line options }
  while not eolncommand and not eofcommand and (bufcommand = ' ') do getcommand;
  if not eolncommand then begin
    parhdrfil(prr, prrval, '.out ');
    if not prrval then begin
      writeln('*** Error: input filename not found');
      goto 99
    end;
    paroptions; { parse command line options }
  end;
  plcopt; { place options }

  for bi := 1 to maxbrk do brktbl[bi].sa := -1; { clear breakpoint table }
  for wi := 1 to maxwth do wthtbl[wi] := -1; { clear watch table }
  { clear instruction analyzer table }
  for ai := 1 to maxana do anitbl[ai] := -1;
  { clear source analyzer table }
  for ai := 1 to maxana do begin anstbl[ai] := 0; ansmtbl[ai] := nil end;

  reset(prd);
  if prrval then rewrite(prr);

  { construct bit equivalence table }
  i := 1;
  for bai := 0 to 7 do begin bitmsk[bai] := i; i := i*2 end;
  for sdi := 0 to maxdef do storedef[sdi] := 0; { clear storage defined flags }
  for sdi := 0 to maxdef do storecov[sdi] := 0; { clear coverage bits }

  writeln('Assembling/loading program');
  load; (* assembles and stores code *)

  { if we are to output a deck, write that and stop }
  if dodckout then begin wrtdck; goto 99 end;

  { check and abort if source errors: this indicates a bad intermediate }
  if errsinprg > 0 then begin
    writeln;
    writeln('*** Source program contains errors: ', errsinprg:1);
    goto 99
  end;

  { initialize file state }
  for i := 1 to maxfil do
    begin filstate[i] := fnone; filanamtab[i] := false end;

  pc := 0; sp := maxtop; np := -1; mp := maxtop; ep := 5; srclin := 0;
  expadr := 0; expstk := 0; expmrk := 0;

  { set breakpoint at 0 to kick off debugger }
  if dodebug then begin
    ad := 0; { break at 0 }
    lno := 0;
    if dodbgsrc then begin { source level, find start of program }
      bp := blklst;
      pbp := nil;
      while bp <> nil do begin
        if bp^.btyp = btprog then pbp := bp; bp := bp^.next
      end;
      if pbp = nil then 
        begin writeln('*** Program block not found'); goto 99 end;
      curmod := pbp; ad := pbp^.bstart; 
      skplmk(ad); { skip preceeding line markers }
      skpmst(ad); { skip mst instruction to start frame }
      skplmk(ad); { skip trailing line markers }
      lno := addr2line(curmod, ad)
    end;
    brktbl[1].sa := ad; brktbl[1].line := lno;
    brktbl[1].trace := false; brktbl[1].temp := true;
    brktbl[1].ss := store[ad]; store[ad] := brkins
  end;

  debugstart := false; setcur;
  writeln('Running program');
  writeln;
  repeat
    stopins := false; { set no stop flag }
    breakins := false; { set no break instruction }
    sourcemark := false; { set no source line instruction }
    stopwatch := true; { set stop on watch match }
    watchmatch := false; { set no watch was matched }
    sinins;
    if chkbrk then begin
      wrtnewline;
      writeln('*** Program stopped by user break');
      if dodebug or dodbgflt then debug else goto 99
    end else
      { if breakpoint hit, go debugger }
      if breakins or (stopins and dodebug) or watchmatch then begin
        if stopins then
          begin wrtnewline; writeln; writeln('*** Stop instruction hit') end;
        breakins := false; stopins := false;
        if not watchmatch and not istrc(pc) and not istmp(pc) then
          begin wrtnewline; writeln; writeln('=== break ===') end;
        debug
      end
  until stopins; { until stop instruction is seen }

  99: { abort run }

  writeln;
  writeln('program complete');
  
  { return error code }
  seterr(exitcode)  

end.
