{*******************************************************************************
*                                                                              *
*                     AMD64 code generator for Pascal-P6                       *
*                                                                              *
* Generates assembly code for AMD64 and gcc.                                   *
*                                                                              *
* This software is covered by the 3-clause BSD license.                        *
*                                                                              *
* Copyright 2025 Scott A. Franco                                               *
*                                                                              *
* Redistribution and use in source and binary forms, with or without           *
* modification, are permitted provided that the following conditions are met:  *
*                                                                              *
* 1. Redistributions of source code must retain the above copyright notice,    *
* this list of conditions and the following disclaimer.                        *
*                                                                              *
* 2. Redistributions in binary form must reproduce the above copyright notice, *
* this list of conditions and the following disclaimer in the documentation    *
* and/or other materials provided with the distribution.                       *
*                                                                              *
* 3. Neither the name of the copyright holder nor the names of its             *
* contributors may be used to endorse or promote products derived from this    *
* software without specific prior written permission.                          *
*                                                                              *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS”  *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   *
* POSSIBILITY OF SUCH DAMAGE.                                                  *
*                                                                              *
*******************************************************************************}

program pgen(output);

joins services;

uses endian,      { endian mode }
     mpb,         { machine parameter block }
     version,     { current version number }
     parcmd,      { command line parsing }
     registers,   { cpu specific registers }
     independent; { cpu independent module }

label 99;

override procedure abort;

begin

  goto 99

end;

override procedure assemble; (*translate symbolic code into machine code and store*)

  var name :alfa; r :real; s :settype;
      i,s1,lb,ub,l:integer; c: char;
      str: strbuf; { buffer for string constants }
      cstp: cstptr;
      ep, ep2, ep3, ep4, ep5: expptr;
      r1: reg; sp, sp2: pstring; def, def2: boolean; val, val2: integer;
      stkadr: integer; { stack address tracking }
      blk: pblock; { block reference }

  procedure getreg(var r: reg; var rf: regset);
  var i: 1..maxintreg;
  begin
    i := 1;
    r := intassord[i];
    while not (r in rf) and (i < maxintreg) do 
      begin i := i+1; r := intassord[i] end;
    if not (r in rf) then error('Out of registers');
    rf := rf-[r];
  end;

  procedure getfreg(var r: reg; var rf: regset);
  var i: 1..maxfltreg;
  begin
    i := 1;
    r := fltassord[i];
    while not (r in rf) and (i < maxfltreg) do 
      begin i := i+1; r := fltassord[i] end;
    if not (r in rf) then error('Out of registers');
    rf := rf-[r];
  end;

  function isfltres(ep: expptr): boolean;
  var isf: boolean;
  begin
    isf := false; 
    if instab[ep^.op].insf then isf := true
    else if (ep^.op in [247{cif}, 246{cuf}]) and (ep^.rc = 1) then isf := true
    else if ep^.op = 15{csp} then
      if ep^.q in [19{atn},15{cos},16{exp},17{log},14{sin},18{sqt}] then 
        isf := true;
    isfltres := isf
  end;

  procedure assreg(ep: expptr; rf: regset; r1, r2: reg);
  var rfs: regset;

  procedure resreg(r: reg);
  begin
    if r <> rgnull then begin
      if not (r in rf) and (r <> r1) and (r <> r2) then ep^.rs := ep^.rs+[r];
      rf := rf-[r]
    end
  end;

  procedure dstreg(r: reg);
  begin
    if r <> rgnull then begin
      if not (r in rfs) and (r <> r1) and (r <> r2) then ep^.rs := ep^.rs+[r];
      rfs := rfs-[r]
    end
  end;

  { assign registers to parameters in call }
  procedure asspar(ep: expptr; mi: integer);
  var pp: expptr; pc: integer; fpc: integer; r1, r2: reg; fr: reg;
  begin
    refer(mi);
    pp := ep^.pl; pc := 1; fpc := 1;
    while pp <> nil do begin
      if isfltres(pp) then begin { floating result }
        if fpc <= maxfltparreg then begin
          resreg(parregf[fpc]); assreg(pp, rf, parregf[fpc], rgnull)
        end else begin
          getfreg(fr, rf); assreg(pp, rf, rgnull, rgnull)
        end;
        fpc := fpc+1
      end else if instab[pp^.op].insr = 2 then begin { double register }
        if pc <= maxintparreg-1 then begin
          resreg(parreg[pc]); resreg(parreg[pc+1]); 
          assreg(pp, rf, parreg[pc], parreg[pc+1])
        end else begin
          getreg(r1, rf); getreg(r2, rf); assreg(pp, rf, r1, r2)
        end;
        pc := pc+2
      end else begin { single register }
        if pc <= maxintparreg then begin
          resreg(parreg[pc]); assreg(pp, rf, parreg[pc], rgnull)
        end else begin 
          getreg(r1, rf); assreg(pp, rf, r1, rgnull) 
        end;
        pc := pc+1
      end;            
      pp := pp^.next
    end
  end;

  procedure asscall;
  begin
    { calling convention says can trash these }
    dstreg(rgrax); dstreg(rgrcx); dstreg(rgrdx); dstreg(rgrsi); 
    dstreg(rgrdi); dstreg(rgr8); dstreg(rgr9); dstreg(rgr10); 
    dstreg(rgr11); dstreg(rgxmm0); dstreg(rgxmm1); dstreg(rgxmm2); 
    dstreg(rgxmm3); dstreg(rgxmm4); dstreg(rgxmm5); dstreg(rgxmm6); 
    dstreg(rgxmm7); dstreg(rgxmm8); dstreg(rgxmm9); dstreg(rgxmm10);
    dstreg(rgxmm11); dstreg(rgxmm12); dstreg(rgxmm13); dstreg(rgxmm14); 
    dstreg(rgxmm15)
  end;

  begin { assreg }
    write(prr, '# assigning:  '); dmpety(prr, ep, r1, r2); 
    write(prr, ' ~rf: '); wrtregs(prr, rf, false); writeln(prr);
    rfs := rf;
    if ep^.al <> nil then assreg(ep^.al, rf, rgnull, rgnull);
    case ep^.op of

      {lodi,lodx,loda,lodb,lodc,lda}
      0,193,105,108,109,4: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
      end;

      {lodr}
      106: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
        if ep^.p <> blkstk^.lvl then getreg(ep^.t1, rf)
      end;

      {lods}
      107: begin 
        resreg(rgrsi); resreg(rgrdi); 
        ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        gettmp(ep^.r1a, setsize)
      end;

      {adr,sbr}
      29, 31: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) 
      end;

      {equr,neqr,geqr,grtr,leqr,lesr}
      138,144,150,156,162,168: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, rgnull, rgnull); resreg(ep^.l^.r1);
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {grts,less}
      158,170: ; { are invalid }

      {equs,neqs,geqs,leqs}
      140,146,152,164: begin 
        asscall;
        assreg(ep^.l, rf, rgrdi, rgnull); 
        assreg(ep^.r, rf, rgrsi, rgnull);
        if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
        else ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf)
      end;

      {adi,sbi,equ,neq,geq,grt,leq,les}
      28, 30, 17, 137, 139, 141, 18, 143, 145, 
      147, 19, 149, 151, 153, 20, 155, 157, 159, 21, 
      161, 163, 165, 167, 169, 171: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, r2); assreg(ep^.r, rf, rgnull, rgnull) 
      end;

      120{lip}: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        ep^.r2 := r2; if ep^.r2 = rgnull then getreg(ep^.r2, rf);
        getreg(ep^.t1, rf) 
      end; 

      {equm,neqm,geqm,grtm,leqm,lesm}
      142, 148, 154, 160, 166, 172: begin 
        asscall;
        ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, rgrdi, rgnull); 
        assreg(ep^.r, rf, rgrsi, rgnull)
      end;

      5{lao},234{lto}: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

      16{ixa}: begin 
        dstreg(rgrax); dstreg(rgrdx); resreg(rgrax);
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        ep^.t1 := ep^.r1;
        if (ep^.r1 = rgrax) or (ep^.r1 = rgrdx) then getreg(ep^.t1, rf);
        assreg(ep^.l, rf, ep^.r1, rgnull); assreg(ep^.r, rf, rgnull, rgnull)
      end;

      118{swp}: ; { done at top level }

      {ldoi,ldoa,ldob,ldoc,ldox,ltci,ltcb,ltcc,ltcx}
      1,65,68,69,194,228,231,232,233:begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf)
      end;

      {ldor,ltcr}
      66,229:begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) 
      end;

      {ldos,ltcs}
      67,230: begin 
        resreg(rgrsi); resreg(rgrdi); 
        ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        gettmp(ep^.r1a, setsize)
      end;

      {ind,inda,indb,indc,indx}
      9, 85,88,89,198: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf); 
        assreg(ep^.l, rf, ep^.r1, rgnull) 
      end;

      {indr}
      86: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf); getreg(ep^.t1, rf);
        assreg(ep^.l, rf, ep^.t1, rgnull) 
      end;

      {inds}
      87: begin 
        dstreg(rgrsi); dstreg(rgrdi);
        assreg(ep^.l, rf, rgrsi, rgnull);
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        gettmp(ep^.r1a, setsize)
      end;

      {inc,dec}
      10, 90, 93, 94, 57, 103, 104, 201, 202: begin 
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {mdc}
      254: begin 
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {suv}
      91: ;

      {ckvi,ckvb,ckvc,ckvx}
      175, 179, 180, 203: ;

      {cvbi,cvbx,cvbb,cvbc}
      100, 115, 116, 121: begin 
        asscall;
        assreg(ep^.l, rf, rgr9, rgnull); assreg(ep^.r, rf, rgrcx, rgnull);
        ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgrax
      end;

      {ivti,ivtx,ivtb,ivtc}
      192,101,102,111: begin 
        asscall;
        assreg(ep^.l, rf, rgr9, rgnull); assreg(ep^.r, rf, rgrcx, rgnull);
        ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgrax
      end;

      {cta}
      191: begin
        asscall;
        assreg(ep^.l, rf, rgr8, rgnull); assreg(ep^.r, rf, rgrcx, rgnull)
      end;

      {cps}
      176: begin 
        assreg(ep^.l, rf, rgnull, rgnull); 
        resreg(ep^.l^.r1); resreg(ep^.l^.r2);
        assreg(ep^.r, rf, rgnull, rgnull);
      end;

      {cpc}
      177: begin
        asscall; 
        assreg(ep^.l, rf, rgnull, rgrsi); assreg(ep^.r, rf, rgnull, rgrdx);
      end;

      {lpa}
      114: begin ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1); 
        if ep^.r2 = rgnull then getreg(ep^.r2, rf)
      end;

      {ldci,ldcc,ldcb}
      123,127,126: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
      end;

      {ldcn}
      125: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

      {ldcr}
      124: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) end;

      {ldcs}
      7: begin 
        dstreg(rgrsi); dstreg(rgrdi);
        ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        gettmp(ep^.r1a, setsize) 
      end;

      {chki,chkb,chkc,chkx}
      26, 98, 99, 199: begin 
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1); 
        getreg(ep^.t1, rf);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {chka}
      95: begin 
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1); 
        assreg(ep^.l, rf, ep^.r1, rgnull);
        { this gets copied back because sometimes chka is applied to fat 
          pointer }
        ep^.r2 := ep^.l^.r2
      end;

      {chks}
      97: begin 
        asscall;
        assreg(ep^.l, rf, rgrdx, rgnull);
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        ep^.r1a := ep^.l^.r1a
      end;

      {ckla}
      190: begin 
        dstreg(rgrax);
        ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      56 {lca}: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) 
      end;

      {ordi,ordb,ordc,ordx}
      59, 134, 136, 200: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, ep^.r1, r2)
      end;

      {lcp}
      135: begin ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.r2 = rgnull then getreg(ep^.r2, rf);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {sgs}
      32: begin 
        asscall; assreg(ep^.l, rf, rgrdi, rgnull);
        ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        gettmp(ep^.r1a, setsize)
      end;
 
      {flt,flo}
      33,34: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, rgnull, rgnull)  
      end;

      {trc}
      35: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if dochkovf then begin getfreg(ep^.t1, rf); getreg(ep^.t2, rf) end;
        assreg(ep^.l, rf, rgnull, rgnull)  
      end;

      {ngi}
      36: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, ep^.r1, rgnull)  
      end;

      {ngr}
      37: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
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
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, r2);
        getreg(ep^.t1, rf); getfreg(ep^.t2, rf) 
      end;

      {noti}
      205: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        getreg(ep^.t1, rf);
        assreg(ep^.l, rf, ep^.r1, r2)
      end;

      {notb,odd,chr}
      42,50,60: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, ep^.r1, r2) 
      end;

      {rnd}
      62: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if dochkovf then begin getfreg(ep^.t1, rf); getreg(ep^.t2, rf) end;
        assreg(ep^.l, rf, rgnull, rgnull) 
      end;

      {and,ior,xor}
      43,44,206: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, rgnull); 
        assreg(ep^.r, rf, rgnull, rgnull);
      end;

      {dif,int,uni}
      45,46,47: begin 
        asscall; 
        assreg(ep^.l, rf, rgrdi, rgnull); resreg(rgrdi);
        assreg(ep^.r, rf, rgrsi, rgnull);
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        ep^.r1a := ep^.l^.r1a
      end;

      {inn}
      48: begin 
        asscall;
        assreg(ep^.l, rf, rgrdi, rgnull); resreg(rgrdi);
        assreg(ep^.r, rf, rgrsi, rgnull);
        if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax else 
        ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf)
      end;

      {mod}
      49: begin 
        dstreg(rgrax); dstreg(rgrdx);
        if (r1 = rgnull) and (rgrdx in rf) then ep^.r1 := rgrdx
        else ep^.r1 := r1;
        assreg(ep^.l, rf, rgrax, rgnull); resreg(rgrax); 
        assreg(ep^.r, rf, rgnull, rgnull);
        if ep^.r1 = rgnull then getreg(ep^.r1, rf)
      end;

      {dvi}
      53: begin 
        dstreg(rgrax); dstreg(rgrdx);
        if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
        else ep^.r1 := r1;
        assreg(ep^.l, rf, rgrax, rgnull); resreg(rgrax); 
        assreg(ep^.r, rf, rgnull, rgnull);
        if ep^.r1 = rgnull then getreg(ep^.r1, rf)
      end;

      {mpi}
      51: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, rgnull);
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {mpr}
      52: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, rgnull); 
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {dvr}
      54: begin ep^.r1 := r1; 
        if ep^.r1 = rgnull then getfreg(ep^.r1, rf) else resreg(ep^.r1);
        if dodbgchk then begin getfreg(ep^.t1, rf); getreg(ep^.t2, rf) end;
        assreg(ep^.l, rf, ep^.r1, rgnull); 
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {rgs}
      110: begin 
        asscall; 
        assreg(ep^.l, rf, rgrdi, rgnull); assreg(ep^.r, rf, rgrsi, rgnull);
        ep^.r1 := r1; if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        gettmp(ep^.r1a, setsize)
      end;

      { dupi, dupa, dupr, dups, dupb, dupc }
      181, 182, 183, 184, 185, 186: ;

      {cks}
      187:;

      {csp}
      15: begin
        asscall; asspar(ep, sfptab[ep^.q].sppar);
        if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then resreg(rgrcx);
        if sfptab[ep^.q].spfunc then begin { function }
          if isfltres(ep) then begin
            if (r1 = rgnull) and (rgxmm0 in rf) then ep^.r1 := rgxmm0
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            if ep^.r1 <> rgxmm0 then dstreg(rgxmm0)
          end else begin
            if (r1 = rgnull) and (rgrax in rf) then ep^.r1 := rgrax
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            if ep^.r1 <> rgrax then dstreg(rgrax)
          end
        end
      end;

      {cuf}
      246: begin 
        asscall;
        if ep^.rc = 1 then begin
          if r1 = rgnull then begin
            if rgxmm0 in rf then ep^.r1 := rgxmm0 else getfreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end else if (ep^.rc = 2) or (ep^.rc = 3) then begin 
          ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
          gettmp(ep^.r1a, setsize)
        end else begin
          if r1 = rgnull then begin
            if rgrax in rf then ep^.r1 := rgrax else getreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end;
        asspar(ep, ep^.pn)
      end;

      {cup}
      12: begin
        asscall; asspar(ep, ep^.pn)
      end;

      {cip}
      113: begin
        asscall; asspar(ep, ep^.pn); assreg(ep^.l, rf, rgnull, rgnull)
      end;

      {cif}
      247: begin
        asscall; resreg(rgr15);
        if ep^.rc = 1 then begin
          if r1 = rgnull then begin
            if rgxmm0 in rf then ep^.r1 := rgxmm0 else getfreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end else if (ep^.rc = 2) or (ep^.rc = 3) then begin 
          ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
          gettmp(ep^.r1a, setsize)
        end else begin
          if r1 = rgnull then begin 
            if rgrax in rf then ep^.r1 := rgrax else getreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end;
        asspar(ep, ep^.pn); assreg(ep^.l, rf, rgnull, rgnull)
      end;

      {cuv}
      27: begin
        asscall; asspar(ep, ep^.pn)
      end;

      {cvf}
      249: begin 
        asscall;
        if ep^.rc = 1 then begin
          if r1 = rgnull then begin
            if rgxmm0 in rf then ep^.r1 := rgxmm0 else getfreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end else if (ep^.rc = 2) or (ep^.rc = 3) then begin 
          ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
          gettmp(ep^.r1a, setsize)
        end else begin
          if r1 = rgnull then begin
            if rgrax in rf then ep^.r1 := rgrax else getreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end;
        asspar(ep, ep^.pn)
      end;

      {cke}
      188: begin
        getreg(ep^.r1, rf); getreg(ep^.r2, rf); 
        getreg(ep^.t1, rf); assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {wbs}
      243: begin 
        asscall;
        ep^.r1 := r1; 
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {cxs}
      211: begin
        dstreg(rgrax); dstreg(rgrdx); resreg(rgrax); resreg(rgrdx);
        ep^.r1 := r1;
        if ep^.r1 = rgnull then ep^.r1 := rgrax;
        resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, rgnull); resreg(ep^.l^.r2);
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {cxc}
      212: begin
        dstreg(rgrax); dstreg(rgrdx); resreg(rgrax); resreg(rgrdx);
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then ep^.r1 := rgrax;
        resreg(ep^.r1); 
        if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2); 
        getreg(ep^.t1, rf);
        assreg(ep^.l, rf, ep^.r1, ep^.r2);
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {lft} 
      213: begin
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {max} 
      214: begin
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.q <> 1 then getreg(ep^.t1, rf);
        { left is complex pointer, right is lvl, result is left }
        assreg(ep^.l, rf, rgnull, ep^.r1);
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {equv,neqv,lesv,grtv,leqv,geqv} 
      215,216,217,218,219,220: begin
        asscall;
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, rgrdi, rgrdx); 
        assreg(ep^.r, rf, rgrsi, rgnull)
      end;

      {spc} 
      222: begin
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
        assreg(ep^.l, rf, ep^.r1, ep^.r2)
      end;

      {ccs} 
      223: begin
        dstreg(rgrsi); dstreg(rgrdi); dstreg(rgrcx); dstreg(rgrax); dstreg(rgrdx);
        resreg(rgrsi); resreg(rgrdi); resreg(rgrcx); resreg(rgrax); resreg(rgrdx);
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
        assreg(ep^.l, rf, ep^.r1, ep^.r2);
        getreg(ep^.t1, rf); getreg(ep^.t2, rf); getreg(ep^.t3, rf)
      end;

      {ldp} 
      225: begin
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
        assreg(ep^.l, rf, ep^.r1, rgnull)
      end;

      {mpc} 
      248: begin
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        if ep^.r2 = rgnull then getreg(ep^.r2, rf) else resreg(ep^.r2);
        assreg(ep^.l, rf, ep^.r1, rgnull);
        assreg(ep^.r, rf, ep^.r2, rgnull)
      end;

      {cpl} 
      251: begin
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        assreg(ep^.l, rf, rgnull, ep^.r1)
      end;

    end;
    write(prr, '# assigning~: '); dmpety(prr, ep); write(prr, ' ~rf: ');
    wrtregs(prr, rf, false); writeln(prr)
  end;

  procedure genexp(ep: expptr);
  var r: reg; ep2: expptr; stkadrs: integer; fl: integer;

  { push parameters in order }
  procedure pshpar(pp: expptr);
  begin
    while pp <> nil do begin
      genexp(pp);
      if pp^.r2 <> rgnull then begin
        wrtins(' pushq %1 # place 2nd register on stack', pp^.r2); 
        stkadr := stkadr-intsize
      end;
      if instab[pp^.op].inss then begin
        wrtins(' subq $0,%rsp # allocate set', setsize);
        wrtins(' pushq %rsi # save source');
        wrtins(' pushq %rdi # save destination');
        if pp^.r1 <> rgrsi then
          wrtins(' movq %1,%rsi # place source', pp^.r1);
        wrtins(' movq %rsp,%rdi # destination is stack');
        wrtins(' addq $0,%rdi # index over saved', ptrsize*2);
        wrtins(' movsq # move set');
        wrtins(' movsq');
        wrtins(' movsq');
        wrtins(' movsq');  
        wrtins(' popq %rdi # restore');          
        wrtins(' popq %rsi');
        stkadr := stkadr-setsize
      end else if pp^.r1 in [rgrax..rgr15] then begin
        wrtins(' pushq %1 # save parameter', pp^.r1); 
        stkadr := stkadr-intsize
      end else if pp^.r1 in [rgxmm0..rgxmm15] then begin
        wrtins(' subq $0,%rsp # allocate real on stack', realsize); 
        stkadr := stkadr-realsize;
        wrtins(' movsd %1,(%rsp) # place real on stack', pp^.r1)
      end;
      pp := pp^.next
    end
  end;

  { call system procedure/function }
  procedure callsp(ep: expptr; var sc: alfa; r: boolean);
  var si: packed array [1..60] of char; i: integer; pp: expptr; aln: boolean;
  begin
    { evaluate all parameters }
    pp := ep^.pl;
    while pp <> nil do begin genexp(pp); pp := pp^.next end;
    aln := false;
    if stkadr mod 16 <> 0 then begin
      wrtins(' pushq %rbx # align');
      aln := true
    end;
    si := ' call psystem_     # call system procedure/function         ';
    for i := 1 to maxalfa do if sc[i] <> ' ' then si[15+i-1] := sc[i];
    wrtins(si);
    if aln then
      wrtins(' popq %rbx # drop alignment');
    if r then begin
      if isfltres(ep) then begin
        if ep^.r1 <> rgxmm0 then 
          wrtins(' movsd %xmm0,%1 # place result', ep^.p, ep^.r1)
      end else begin 
        if ep^.r1 <> rgrax then 
          wrtins(' movq %rax,%1 # place result', ep^.p, ep^.r1);
        if (ep^.r2 <> rgnull) and (ep^.r2 <> rgrdx) then
          wrtins(' movq %rdx,%1', ep^.p, ep^.r2)
      end
    end
  end;

  { call nwl/dsl }
  procedure callnwldsl(ep: expptr);
  var aln: boolean; pp, ep2: expptr; i: integer; stkadrs: integer;
  begin
    stkadrs := stkadr; { save because we don't know tag count }
    ep2 := ep^.pl;
    for i := 1 to 3 do begin
      if ep2 = nil then error('system error');
      ep2 := ep2^.next
    end;
    pshpar(ep2);
    pp := ep^.pl; genexp(pp); { addr rdi }
    pp := pp^.next; genexp(pp); { size rsi }
    pp := pp^.next; genexp(pp); { tagcnt rdx }
    wrtins(' pushq %1 # save tag count', pp^.r1);
    wrtins(' movq %rsp,%rcx # index tag list');
    wrtins(' addq $0,%rcx', intsize);
    stkadr := stkadr-adrsize;
    aln := false;
    if stkadr mod 16 <> 0 then begin
      wrtins(' pushq %rbx # align'); 
      stkadr := stkadr-adrsize; aln := true
    end;
    if ep^.q = 39 then
      wrtins(' call psystem_nwl # call new')
    else
      wrtins(' call psystem_dsl # call dispose');
    if aln then begin
      wrtins(' popq %rbx # dump align');
      stkadr := stkadr+adrsize
    end;
    wrtins(' popq %rcx # restore tag count');
    stkadr := stkadr+adrsize;
    wrtins(' movq $0,%rax # find *integer', intsize);
    wrtins(' mulq %rcx');
    wrtins(' addq %rax,%rsp # dump taglist from stack');
    stkadr := stkadrs { restore to entry }
  end;

  begin { genexp }
    if ep <> nil then begin
      for r := rgrax to rgxmm15 do if r in ep^.rs then begin
          if r in [rgrax..rgr15] then begin
            wrtins(' pushq %1 # save used register', r); 
            stkadr := stkadr-intsize
          end else begin
            wrtins(' subq $0,%rsp # allocate real on stack', realsize);
            wrtins(' movsd %1,(%rsp) # save used register', r);
            stkadr := stkadr-realsize
          end
      end;
      genexp(ep^.al);
      if (ep^.op <> 113{cip}) and (ep^.op <> 247{cif}) then genexp(ep^.l);
      genexp(ep^.r); genexp(ep^.x1);
      write(prr, '# generating:  '); dmpety(prr, ep); writeln(prr);
      case ep^.op of

        {lodi,loda}
        0,105: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.r1);
            wrtins(' movq @l(%1),%1 # fetch local qword', ep^.q, ep^.p, ep^.r1)
          end else
            wrtins(' movq @l(%rbp),%1 # fetch local qword', ep^.q, ep^.p, ep^.r1)
        end;

        {lodx,lodb,lodc}
        193,108,109: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.r1);
            wrtins(' movzx @l(%1),%1 # fetch local byte', ep^.q, ep^.p, ep^.r1)
          end else
            wrtins(' movzx @l(%rbp),%1 # fetch local byte', ep^.q, ep^.p, ep^.r1)
        end;

        {lodr}
        106: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.t1);
            wrtins(' movsd @l(%1),%2 # fetch local real', ep^.q, ep^.p, ep^.t1, ep^.r1)
          end else
            wrtins(' movsd @l(%rbp),%1 # fetch local real', ep^.q, ep^.p, ep^.r1)
        end;

        {lods}
        107: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%rsi # get display pointer', ep^.q1);
            wrtins(' lea @l(%rsi),%rsi # index local set', ep^.q, ep^.p)
          end else
            wrtins(' lea @l(%rbp),%rsi # index local set', ep^.q, ep^.p);
          wrtins(' leaq ^-@s^0(%rbp),%rdi # index destination temp', ep^.r1a, lclspc^);
          wrtins(' movsq # move');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' leaq ^-@s^0(%rbp),%1 # index temp again', ep^.r1a, ep^.r1, lclspc^);
        end;

        {lda}
        4: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.r1);
            wrtins(' lea @l(%1),%1 # index local', ep^.q, ep^.r1)
          end else
            wrtins(' lea @l(%rbp),%1 # index local', ep^.q, ep^.r1)
        end;

        {adi}
        28: begin
          wrtins(' add %1,%2 # add integers', ep^.r^.r1, ep^.l^.r1);
          if dochkovf then begin
            wrtins(' jno 1f # skip no overflow');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # set line number', sline);
            wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end
        end;

        {adr}
        29: 
          wrtins(' addsd %1,%2 # add reals', ep^.r^.r1, ep^.l^.r1);

        {sbi}
        30: begin
          wrtins(' sub %1,%2 # subtract integers', ep^.r^.r1, ep^.l^.r1);
          if dochkovf then begin
            wrtins(' jno 1f # skip no overflow');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # set line number', sline);
            wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end
        end;

        {sbr}
        31: 
          wrtins(' subsd %1,%2 # subtract reals', ep^.r^.r1, ep^.l^.r1);

        {equr,neqr,geqr,grtr,leqr,lesr}
        138,144,150,156,162,168: begin 
          case ep^.op of
            138{equr}: wrtins(' cmpeqsd %1,%2 # compare real equal', ep^.r^.r1, ep^.l^.r1);
            144{neqr}: wrtins(' cmpneqsd %1,%2 # compare real not equal', ep^.r^.r1, ep^.l^.r1);
            150{geqr}: wrtins(' cmplesd %2,%1 # compare real greater or equal', ep^.r^.r1, ep^.l^.r1);
            156{grtr}: wrtins(' cmpltsd %2,%1 # compare real greater', ep^.r^.r1, ep^.l^.r1);
            162{leqr}: wrtins(' cmplesd %1,%2 # compare real less or equal', ep^.r^.r1, ep^.l^.r1);
            168{lesr}: wrtins(' cmpltsd %1,%2 # compare real less', ep^.r^.r1, ep^.l^.r1);
          end;
          if ep^.op in [150{geqr},156{grtr}] then 
            wrtins(' movq %1,%2 # move to result', ep^.r^.r1, ep^.r1)
          else
            wrtins(' movq %1,%2 # move to result', ep^.l^.r1, ep^.r1);
          wrtins(' andq $0,%1 # mask boolean', 1, ep^.r1) 
        end;

        120{lip}: begin 
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' movq ^0(%rbp),%1 # get display pointer', ep^.q1, ep^.t1);
            wrtins(' movq @l+ptrsize(%2),%1 # load frame pointer', ep^.q, ep^.p, ep^.r2, ep^.t1);
            wrtins(' movq @l(%2),%1 # load procedure address', ep^.q, ep^.p, ep^.r1, ep^.t1)
          end else begin
            wrtins(' movq @l+ptrsize(%rbp),%1 # load frame pointer', ep^.q, ep^.p, ep^.r2);
            wrtins(' movq @l(%rbp),%1 # load procedure address', ep^.q, ep^.p, ep^.r1)
          end
        end;  

        {equm,neqm,geqm,gtrm,leqm,lesm}
        142,148,154,160,166,172: begin 
          wrtins(' movq $0,%rdx # get string length', ep^.q);
          wrtins(' call psystem_strcmp # compare strings'); 
          wrtins(' cmpq $0,%rax # compare -0+ result');
          case ep^.op of
            142{equm}: wrtins(' sete %1l # set equal', ep^.r1);
            148{neqm}: wrtins(' setne %1l # set not equal', ep^.r1);
            154{geqm}: wrtins(' setge %1l # set greater or equal', ep^.r1);
            160{grtm}: wrtins(' setg %1l # set greater', ep^.r1);
            166{leqm}: wrtins(' setle %1l # set less or equal', ep^.r1);
            172{lesm}: wrtins(' setl %1l # set less', ep^.r1);
          end;
          wrtins(' movsx %1l,%1 # sign extend boolean', ep^.r1)
        end;

        5{lao},234{lto}:
          if ep^.fl <> nil then 
            wrtins(' leaq @s(%rip),%1 # load address of global', ep^.r1, ep^.fl^)
          else
            wrtins(' leaq @g(%rip),%1 # load address of global', ep^.q, ep^.r1);

        16{ixa}: begin 
          { left is address right is index, size is q }
          if ep^.r1 <> ep^.t1 then
            wrtins(' movq %1,%2 # save index', ep^.r1, ep^.t1);
          wrtins(' movq $0,%rax # get element size', ep^.q);
          wrtins(' mul %1 # find index*size', ep^.r^.r1);
          wrtins(' add %rax,%1 # add to base', ep^.t1);
          if ep^.r1 <> ep^.t1 then
            wrtins(' movq %1,%2 # move to final register', ep^.t1, ep^.r1)
        end;

        118{swp}: ; { done at top level }

        {ldoi,ltci}
        1,65,228:
          if ep^.fl <> nil then
            wrtins(' movq @s(%rip),%1 # load global quad', ep^.r1, ep^.fl^)
          else
            wrtins(' movq @g(%rip),%1 # load global quad', ep^.q, ep^.r1);

        {ldob,ldoc,ldox,ltcb,ltcc,ltcx}
        68,69,194,231,232,233:
          if ep^.fl <> nil then
            wrtins(' movzx @s(%rip),%1 # load and zero extend global byte', ep^.r1, ep^.fl^)
          else
            wrtins(' movzx @g(%rip),%1 # load and zero extend global byte', ep^.q, ep^.r1);

        {ldor,ltcr}
        66,229: 
          if ep^.fl <> nil then
            wrtins(' movsd @s(%rip),%1 # load global real', ep^.r1, ep^.fl^)
          else
            wrtins(' movsd @g(%rip),%1 # load global real', ep^.q, ep^.r1);

        {ldos,ltcs}
        67,230: begin
          if ep^.fl <> nil then
            wrtins(' leaq @s(%rip),%rsi # load address of global set', ep^.fl^)
          else
            wrtins(' leaq @g(%rip),%rsi # load address of global set', ep^.q);
          wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
          wrtins(' movsq # move');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)
        end;

        {indi,inda}
        9,85: 
          wrtins(' movq ^0(%1),%1 # load qword from address', ep^.q, ep^.l^.r1);

        {indr}
        86: 
          wrtins(' movsd ^0(%2),%1 # load real from address', ep^.q, ep^.r1, ep^.t1);

        {indb,indc,indx}
        88,89,198: wrtins(' movzx ^0(%1),%1 # load byte from address', ep^.q, ep^.l^.r1);

        {inds}
        87: begin 
          wrtins(' addq $0,%rsi # offset', ep^.q);
          wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
          wrtins(' movsq # move');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)
        end;

        {inci,incb,incc,incx}
        10, 93, 94, 201: begin
          wrtins(' addq $0,%1 # increment by n', ep^.q, ep^.r1);
          if dochkovf then begin
            wrtins(' jno 1f # skip no overflow');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # set line number', sline);
            wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end
        end;

        {inca}
        90: 
          wrtins(' addq $0,%1 # increment by n', ep^.q, ep^.r1);

        {deci,decb,decc,decx}
        57, 103, 104, 202: begin
          wrtins(' subq $0,%1 # decrement by n', ep^.q, ep^.r1);
          if dochkovf then begin
            wrtins(' jno 1f # skip no overflow');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # set line number', sline);
            wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end
        end;

        {mdc}
        254: begin
          wrtins(' movq %1,%2 # copy template pointer to data', ep^.r1, ep^.r2);
          wrtins(' addq $0,%1 # skip template to data', ep^.q, ep^.r1)
        end;

        {ckvi,ckvb,ckvc,ckvx}
        175, 179, 180, 203: begin 
          wrtins(' cmpq $0,%1 # check this tag value', ep^.q, ep^.r1);
          wrtins(' sete %1l # set boolean equal', ep^.t1);
          wrtins(' orq %1,%2 # or with running total', ep^.t1, ep^.r2);
        end;

        {cvbi,cvbx,cvbb,cvbc}
        100, 115, 116, 121: begin
          wrtins(' movq $0,%rdi # load tagfield offset', ep^.q);
          wrtins(' movq $0,%rsi # load size of variant', ep^.q1);
          wrtins(' leaq @s(%rip),%rdx # load logical variant table', ep^.lt^);
          if ep^.op = 100 then
            wrtins(' movq (%1),%r8 # get existing tag value', ep^.l^.r1)
          else
            wrtins(' movzx (%1),%r8 # get existing tag value', ep^.q, ep^.l^.r1);
          wrtins(' call psystem_tagchgvar # check valid tag change')
        end;

        {ivti,ivtx,ivtb,ivtc}
        192,101,102,111: begin
          wrtins(' movq $0,%rdi # load tagfield offset', ep^.q);
          wrtins(' movq $0,%rsi # load size of variant', ep^.q1);
          wrtins(' leaq @s(%rip),%rdx # load logical variant table', ep^.lt^);
          if ep^.op = 100 then
            wrtins(' movq (%1),%r8 # get existing tag value ', ep^.q, ep^.l^.r1)
          else
            wrtins(' movzx (%1),%r8 # get existing tag value', ep^.q, ep^.l^.r1);
          wrtins(' call psystem_tagchginv # invalidate tag changes')
        end;

        {cps}
        176: begin 
          wrtins(' cmpq %1,%2 # compare container lengths', ep^.r^.r2, ep^.l^.r2);
          wrtins(' je 1f # skip equal', ep^.q, ep^.r^.r1);
          wrtins(' leaq modnam(%rip),%rdi # load module name');
          wrtins(' movq $0,%rsi # load line number', sline);
          wrtins(' movq $ContainerMismatch,%rdx # load error code');
          wrtins(' call psystem_errore # process error');
          wrtins('1:');
        end;

        {cpc}
        177: begin
          wrtins(' movq $0,%rdi # get level number', ep^.q);
          wrtins(' call psystem_cmptmp # compare templates')
        end;

        {cta}
        191: begin
          wrtins(' movq $0,%rdi # get tag offset', ep^.q);
          wrtins(' movq $0,%rsi # get tag nesting level', ep^.q1);
          wrtins(' leaq @s(%rip),%rdx # index logical variant table', ep^.lt^);
          wrtins(' call psystem_tagchkass # check tag assignment')
        end;

        {lpa}
        114: begin 
          wrtins(' leaq @s(%rip),%1 # load procedure/function address', ep^.r1, ep^.fn^);
          wrtins(' movq @l(%rbp),%1 # load display pointer', ep^.q1, ep^.p, ep^.r2)
        end;

        {ldci,ldcc,ldcb}
        123,127,126:
          wrtins(' movq $0,%1 # load quad constant', ep^.vi, ep^.r1); 

        {ldcn}
        125:
          wrtins(' movq $0,%1 # load nil value', ep^.r1);

        {ldcr}
        124:
          wrtins(' movsd real^0(%rip),%1 # load real constant', ep^.realn, ep^.r1);

        {ldcs}
        7: begin
          wrtins(' leaq set^0(%rip),%rsi # index constant set', ep^.setn);
          wrtins(' leaq ^-@s^0(%rbp),%rdi # index temp', ep^.r1a, lclspc^);
          wrtins(' movsq # move');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' movsq');
          wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp    ', ep^.r1a, ep^.r1, lclspc^);
        end;

        {chki,chkb,chkc,chkx}
        26, 98, 99, 199: begin 
          wrtins(' movq $0,%1 # load low bound', ep^.vi, ep^.t1);
          wrtins(' cmpq %1,%2 # compare', ep^.t1, ep^.r1);
          wrtins(' jge 1f # skip if greater or equal');
          wrtins(' leaq modnam(%rip),%rdi # load module name');
          wrtins(' movq $0,%rsi # load line number', sline);
          wrtins(' movq $ValueOutOfRange,%rdx # load error code');
          wrtins(' call psystem_errore # process error');
          wrtins('1:        ');
          wrtins(' movq $0,%1 # load high bound', ep^.vi2, ep^.t1);
          wrtins(' cmpq %1,%2 # compare', ep^.t1, ep^.r1);
          wrtins(' jle 1f # skip if less or equal');
          wrtins(' leaq modnam(%rip),%rdi # load module name');
          wrtins(' movq $0,%rsi # load line number', sline);
          wrtins(' movq $ValueOutOfRange,%rdx # load error code');
          wrtins(' call psystem_errore # process error');
          wrtins('1:')
        end;

        {chka}
        95: begin 
          wrtins(' orq %1,%1 # check nil', ep^.r1);
          wrtins(' jnz 1f # skip if not');
          wrtins(' leaq modnam(%rip),%rdi # load module name');
          wrtins(' movq $0,%rsi # load line number', sline);
          wrtins(' movq $DereferenceOfNilPointer,%rdx # load error code');
          wrtins(' call psystem_errore # process error');
          wrtins('1:')
        end;

        {chks}
        97: begin
          wrtins(' movq $0,%rdi # load low bound', ep^.vi);
          wrtins(' movq $0,%rsi # load high bound', ep^.vi2);
          wrtins(' call psystem_chksetbnd # check set in bounds');
          wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp set', ep^.r1a, ep^.r1, lclspc^)
        end;

        {ckla}
        190: begin
          if ep^.q <> 0 then begin
            wrtins(' orq %1,%1 # check nil', ep^.r1);
            wrtins(' jge 1f # skip greater or equal', ep^.r2);
            wrtins(' leaq modnam(%rip),%rdi # load module name');
            wrtins(' movq $0,%rsi # load line number', sline);
            wrtins(' movq $DereferenceOfNilPointer,%rdx # load error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end
        end;

        56 {lca}: wrtins(' leaq string^0(%rip),%1 # load string constant address', ep^.strn, ep^.r1); 

        {grts,less}
        158,170: ; { are invalid }

        {equs,neqs,geqs,leqs}
        140,146,152,164: begin 
          case ep^.op of
            140: wrtins(' call psystem_setequ # check set equal');
            146: begin
              wrtins(' call psystem_setequ # check set equal');
              wrtins(' xor $0,%rax # invert equal status', 1);
            end;
            152,164: wrtins(' call psystem_setinc # check set inclusion');
          end;
          if ep^.r1 <> rgrax then
            wrtins(' movq %rax,%1 # move result to final register', ep^.r1);
          puttmp(ep^.l^.r1a); puttmp(ep^.r^.r1a)
        end;

        {equa,equi,equb,equc}
        17, 137, 139, 141,
        {neqa,neqi,neqb,neqc}
        18, 143, 145, 147,
        {geqi,geqb,geqc}
        149, 151, 153,
        {grti,grtb,grtc}
        155, 157, 159,
        {leqi,leqb,leqc}
        161, 163, 165,
        {lesi,lesb,lesc}
        167, 169, 171: begin 
          wrtins(' cmp %1,%2 # compare', ep^.r^.r1, ep^.l^.r1);
          case ep^.op of
            17,137,138,139,141: wrtins(' sete %1l # set equal', ep^.r1);
            18,143,144,145,147: wrtins(' setne %1l # set not equal', ep^.l^.r1);
            149,150,151,153: wrtins(' setge %1l # set greater or equal', ep^.l^.r1);
            155,156,157,159: wrtins(' setg %1l # set greater', ep^.l^.r1);
            161,162,163,165: wrtins(' setle %1l # set less or equal', ep^.l^.r1);
            167,168,169,171: wrtins(' setl %1l # set less', ep^.l^.r1)
          end;
          wrtins(' movsx %1l,%1 # sign extend boolean', ep^.l^.r1)
        end;

        {ordi,ordb,ordc,ordx}
        59, 134, 136, 200: ; { ord is a no-op }

        {lcp}
        135: begin 
          wrtins(' leaq ^0(%1),%2 # get length/template', ptrsize, ep^.l^.r1, ep^.r2);
          wrtins(' movq (%1),%1 # get pointer', ep^.l^.r1)
        end;

        {sgs}
        32: begin
          wrtins(' leaq ^-@s^0(%rbp),%rsi # index temp', ep^.r1a, lclspc^);
          wrtins(' call psystem_setsgl # make singleton set');
          wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^);
        end;

        {flt,flo}
        33,34: wrtins(' cvtsi2sd %1,%2 # convert integer to real', ep^.l^.r1, ep^.r1);

        {trc}
        35: begin
          if dochkovf then begin
            wrtins(' movsd real_int_max(%rip),%1 # load maximum int val', ep^.t1);
            wrtins(' cmpnltsd %1,%2 # compare real less or equal', ep^.l^.r1, ep^.t1);
            wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
            wrtins(' orq %1,%1 # check zero', ep^.t2);
            wrtins(' jz 2f # skip zero');
            wrtins(' movsd real_int_min(%rip),%1 # load minimum int val', ep^.t1);
            wrtins(' cmplesd %1,%2 # compare real greater or equal', ep^.l^.r1, ep^.t1);
            wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
            wrtins(' orq %1,%1 # check zero', ep^.t2);
            wrtins(' jnz 1f # skip not zero');
            wrtins('2:');
            wrtins(' leaq modnam(%rip),%rdi # load module name');
            wrtins(' movq $0,%rsi # load line number', sline);
            wrtins(' movq $RealArgumentTooLarge,%rdx # load error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end;                            
          wrtins(' cvttsd2si %1,%2 # trucate real to integer', ep^.l^.r1, ep^.r1);
        end;

        {ngi}
        36: wrtins(' negq %1 # negate integer', ep^.r1);

        {ngr}
        37: begin
          wrtins(' xorpd %1,%1  # clear register', ep^.r1, ep^.r1);
          wrtins(' subsd %1,%2  # find 0-real', ep^.l^.r1, ep^.r1)
        end;

        {sqi}
        38: begin 
          wrtins(' imulq %1,%1 # square integer', ep^.r1);
          if dochkovf then begin
            wrtins(' jno 1f # skip no overflow');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # set line number', sline);
            wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end
        end;

        {sqr}
        39: begin 
          wrtins(' mulsd %1,%1 # square real', ep^.r1);
        end;

        {abi}
        40: begin
          wrtins(' orq %1,%1 # check positive', ep^.r1);
          wrtins(' jns 1f # skip if so', ep^.r1);
          wrtins(' negq %1 # negate integer', ep^.r1);
          wrtins('1:');

        end;

        {abr}
        41: begin 
          wrtins(' movq $0x7fffffffffffffff,%1 # set mask', ep^.t1);
          wrtins(' movq %1,%2 # move to real', ep^.t1, ep^.t2);
          wrtins(' andpd %1,%2 # mask off sign bit', ep^.t2, ep^.r1)
        end;

        {notb}
        42: begin
          wrtins(' orq %1,%1 # test boolean', ep^.r1);
          wrtins(' movq $0,%1 # set true', 1, ep^.r1);
          wrtins(' jz 1f # skip if so', ep^.r2);
          wrtins(' movq $0,%1 # otherwise set false', ep^.r1);
          wrtins('1:')
        end;

        {noti}
        205: begin 
          if dodbgchk then begin
            wrtins(' orq %1,%1 # test signed', ep^.r1);
            wrtins(' jns 1f # skip if not');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # set line number', sline);
            wrtins(' movq $BooleanOperatorOfNegative,%rdx # set error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:');
          end;
          wrtins(' notq %1 # not integer', ep^.r1);
          wrtins(' movq $0,%1 # clear sign bit', pmmaxint, ep^.t1);
          wrtins(' andq %1,%2', pmmaxint, ep^.t1, ep^.r1)
        end;

        {odd}
        50: begin 
          wrtins(' andq $0,%1 # mask bit 0', 1, ep^.r1);
        end;

        {rnd}
        62: begin
          if dochkovf then begin
            wrtins(' movsd real_int_max(%rip),%1 # load maximum int val', ep^.t1);
            wrtins(' cmpnltsd %1,%2 # compare real less or equal', ep^.l^.r1, ep^.t1);
            wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
            wrtins(' orq %1,%1 # check zero', ep^.t2);
            wrtins(' jz 2f # skip zero');
            wrtins(' movsd real_int_min(%rip),%1 # load minimum int val', ep^.t1);
            wrtins(' cmplesd %1,%2 # compare real greater or equal', ep^.l^.r1, ep^.t1);
            wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
            wrtins(' orq %1,%1 # check zero', ep^.t2);
            wrtins(' jnz 1f # skip not zero');
            wrtins('2:');
            wrtins(' leaq modnam(%rip),%rdi # load module name');
            wrtins(' movq $0,%rsi # load line number', sline);
            wrtins(' movq $RealArgumentTooLarge,%rdx # load error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end;
          wrtins(' cvtsd2si %1,%2 # round to integer', ep^.l^.r1, ep^.r1)
        end;

        {chr}
        60: ; { chr is no-op }

        {and,ior,xor}
        43,44,206: begin 
          if dodbgchk then begin
            wrtins(' orq %1,%1 # check signed', ep^.l^.r1);
            wrtins(' jns 1f # skip if not');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # get line number', sline);
            wrtins(' movq $BooleanOperatorOfNegative,%rdx # get error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:');
            wrtins(' orq %1,%1 # check signed', ep^.r^.r1);
            wrtins(' jns 1f # skip if not');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # get line number', sline);
            wrtins(' movq $BooleanOperatorOfNegative,%rdx # get error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:');
          end;
          case ep^.op of
            43: wrtins(' andq %1,%2 # and integers', ep^.r^.r1, ep^.l^.r1);
            44: wrtins(' orq %1,%2 # or integers', ep^.r^.r1, ep^.l^.r1);
            206: wrtins(' xorq %1,%2 # xor integers', ep^.r^.r1, ep^.l^.r1)
          end
        end;

        {dif,int,uni}
        45,46,47: begin
          case ep^.op of 
            45: wrtins(' call psystem_setdif # find set difference');
            46: wrtins(' call psystem_setint # find set intersection');
            47: wrtins(' call psystem_setuni # find set union');
          end;
          wrtins(' leaq ^-@s^0(%rbp),%1 # reindex the temp', ep^.r1a, ep^.r1, lclspc^);
          puttmp(ep^.r^.r1a)
        end;

        {inn}
        48: begin
          wrtins(' call psystem_setsin # find set membership');
          if ep^.r1 <> rgrax then
            wrtins(' movq %rax,%1 # move result to target reg', ep^.r1);
          puttmp(ep^.r^.r1a)
        end;

        {mod}
        49: begin 
          wrtins(' cmpq $0,%1 # check zero divide', ep^.r^.r1);
          wrtins(' jg 1f # skip <= 0');
          wrtins(' leaq modnam(%rip),%rdi # index module name');
          wrtins(' movq $0,%rsi # set line number', sline);
          wrtins(' movq $InvalidDivisorToMod,%rdx # set error code');
          wrtins(' call psystem_errore # process error');
          wrtins('1:');
          wrtins(' xorq %rdx,%rdx # clear upper dividend');
          wrtins(' subq $0,%rax # find sign of dividend', ep^.r^.r1);
          wrtins(' jns 1f # skip positive', ep^.r^.r1);
          wrtins(' decq %rdx # set sign of upper dividend');
          wrtins('1:');
          wrtins(' idivq %1 # divide integer', ep^.r^.r1);
          if ep^.r1 <> rgrdx then
            wrtins(' movq %rdx,%1 # place result', ep^.r1)
        end;

        {dvi}
        53: begin 
          wrtins(' cmpq $0,%1 # check zero divide', ep^.r^.r1);
          wrtins(' jne 1f # skip no overflow');
          wrtins(' leaq modnam(%rip),%rdi # index module name');
          wrtins(' movq $0,%rsi # set line number', sline);
          wrtins(' movq $ZeroDivide,%rdx # set error code');
          wrtins(' call psystem_errore # process error');
          wrtins('1:');
          wrtins(' xorq %rdx,%rdx # clear upper dividend');
          wrtins(' subq $0,%rax # find sign of dividend', ep^.r^.r1);
          wrtins(' jns 1f # skip positive', ep^.r^.r1);
          wrtins(' decq %rdx # set sign of upper dividend ');
          wrtins('1:');
          wrtins(' idivq %1 # divide integer', ep^.r^.r1);
          if ep^.r1 <> rgrax then
            wrtins(' movq %rax,%1 # move result into place', ep^.r1)
        end;

        {mpi}
        51: begin
          wrtins(' imulq %1,%2 # multiply integers', ep^.r^.r1, ep^.l^.r1);
          if dochkovf then begin
            wrtins(' jno 1f # skip no overflow');
            wrtins(' leaq modnam(%rip),%rdi # index module name');
            wrtins(' movq $0,%rsi # set line number', sline);
            wrtins(' movq $IntegerValueOverflow,%rdx # set error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end
        end;

        {mpr}
        52: wrtins(' mulsd %1,%2 # multiply reals', ep^.r^.r1, ep^.l^.r1);

        {dvr}
        54: begin
          if dodbgchk then begin
            wrtins(' movsd real_zero(%rip),%1 # load real zero', ep^.t1);
            wrtins(' cmpeqsd %1,%2 # compare real equal', ep^.r^.r1, ep^.t1);
            wrtins(' movq %1,%2 # move result to temp', ep^.t1, ep^.t2);
            wrtins(' orq %1,%1 # check zero', 1, ep^.t2);
            wrtins(' jz 1f # skip not zero', ep^.r^.r1);
            wrtins(' leaq modnam(%rip),%rdi # load module name');
            wrtins(' movq $0,%rsi # load line number', sline);
            wrtins(' movq $ZeroDivide,%rdx # load error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:');
          end;
          wrtins(' divsd %1,%2 # divide reals   ', ep^.r^.r1, ep^.l^.r1)
        end;

        {rgs}
        110: begin 
          wrtins(' leaq ^-@s^0(%rbp),%rdx # index temp', ep^.r1a, lclspc^);
          wrtins(' call psystem_setrgs # set range of values');
          wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^);
        end;

        { dupi, dupa, dupr, dups, dupb, dupc }
        181, 182, 183, 184, 185, 186: ;

        {cks}
        187: ;

        {csp}
        15: begin
          { need irregular handling for nwl and dsl }
          if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then callnwldsl(ep)
          else callsp(ep, sfptab[ep^.q].sptable, sfptab[ep^.q].spfunc)
        end;

        {sfr}
        245:
          if ep^.lb <> nil then begin
            if dodbgchk then begin
              wrtins(' movq %rsp,%rax # copy stack pointer');
              wrtins(' subq $s,%rax # set new stack depth', ep^.lb^);
              wrtins('1:');
              wrtins(' cmpq %rax,%rsp # check done');
              wrtins(' jbe 2f # skip if below stack');
              wrtins(' pushq $0 # clear stack'); 
              wrtins(' jmp 1b # loop');
              wrtins('2:')
            end else
              wrtins(' subq $s,%rsp # set new stack depth', ep^.lb^);
          end;

        {cup,cuf}
        12, 246: begin
          genexp(ep^.sl); { process sfr start link }
          stkadrs := stkadr; { save stack track here }
          pshpar(ep^.pl); { process parameters first }
          if ep^.blk <> nil then begin
            write(prr, ' ':opcspc, 'call'); lftjst(parspc-(4+opcspc)); fl := parspc; 
            wrtblks(ep^.blk^.parent, true, fl); wrtblksht(ep^.blk, fl); 
            lftjst(cmtspc-fl); writeln(prr, '# call user procedure')
          end else wrtins(' call @s # call user procedure', ep^.fn^);
          if ep^.op = 246{cuf} then begin
            if ep^.rc = 1 then begin
              if ep^.r1 <> rgxmm0 then
                wrtins(' movq %xmm0,%1 # place result', ep^.r1)
            end else if ep^.rc = 2 then begin { move set from stack to temp }
                wrtins(' movq %rsp,%rsi # index set on stack');
                wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                wrtins(' movsq # move');
                wrtins(' movsq');
                wrtins(' movsq');
                wrtins(' movsq');
                wrtins(' addq $0,%rsp # remove set from stack', setsize);
                wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
            end else if ep^.rc = 3 then begin { move structure from stack to temp }
                wrtins(' movq %rsp,%rsi # index structure on stack');
                wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                wrtins(' movq $0,%rcx # load size', ep^.q2, lclspc^);
                wrtins(' repnz # move');
                wrtins(' movsb');
                wrtins(' addq $0,%rsp # remove structure from stack', ep^.q3);
                wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)                
            end else begin
              if ep^.r1 <> rgrax then
                wrtins(' movq %rax,%1 # place result', ep^.r1);
            end
          end;
          stkadr := stkadrs { restore stack position }
        end;

        {cip,cif}
        113,247: begin
          genexp(ep^.sl); { process sfr start link }
          stkadrs := stkadr; { save stack track here }
          pshpar(ep^.pl); { process parameters first }
          genexp(ep^.l); { load procedure address }
          wrtins(' movq %rbp,%r15 # move our frame pointer to preserved register');
          wrtins(' movq ^0(%1),%rbp # set callee frame pointer', 1*ptrsize, ep^.l^.r1);
          wrtins(' call *(%1) # call indirect', ep^.l^.r1);
          if ep^.op = 247{cif} then begin 
            if ep^.rc = 1 then begin
              if ep^.r1 <> rgxmm0 then
                wrtins(' movq %xmm0,%1 # place result', ep^.r1)
            end else if ep^.rc = 2 then begin { move set from stack to temp }
                wrtins(' movq %rsp,%rsi # index set on stack');
                wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                wrtins(' movsq # move');
                wrtins(' movsq');
                wrtins(' movsq');
                wrtins(' movsq');
                wrtins(' addq $0,%rsp # remove set from stack', setsize);
                wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
            end else if ep^.rc = 3 then begin { move structure from stack to temp }
                wrtins(' movq %rsp,%rsi # index structure on stack');
                wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                wrtins(' movq $0,%rcx # load size', ep^.q2, lclspc^);
                wrtins(' repnz # move');
                wrtins(' movsb');
                wrtins(' addq $0,%rsp # remove structure from stack', ep^.q3);
                wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
            end else begin
              if ep^.r1 <> rgrax then
                wrtins(' movq %rax,%1 # place result', ep^.r1)
            end
          end;
          wrtins(' movq %r15,%rbp # restore our frame pointer');
          stkadr := stkadrs { restore stack position }
        end;

        {cuv,cvf}
        27,249: begin
          genexp(ep^.sl); { process sfr start link }
          stkadrs := stkadr; { save stack track here }
          pshpar(ep^.pl); { process parameters first }
          if ep^.qs <> nil then wrtins(' call *@s(%rip) # call vectored', ep^.qs^)
          else wrtins(' call *@g(%rip) # call vectored', q);
          if ep^.op = 249{cvf} then begin
            if ep^.rc = 1 then begin
              if ep^.r1 <> rgxmm0 then
              wrtins(' movq %xmm0,%1 # place result', ep^.r1)
            end else if ep^.rc = 2 then begin { move set from stack to temp }
                wrtins(' movq %rsp,%rsi # index set on stack');
                wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                wrtins(' movsq # move');
                wrtins(' movsq');
                wrtins(' movsq');
                wrtins(' movsq');
                wrtins(' addq $0,%rsp # remove set from stack', setsize);
                wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^) 
            end else if ep^.rc = 3 then begin { move structure from stack to temp }
                wrtins(' movq %rsp,%rsi # index structure on stack');
                wrtins(' leaq ^-@s^0(%rbp),%rdi # load temp destination', ep^.r1a, lclspc^);
                wrtins(' movq $0,%rcx # load size', ep^.q2, lclspc^);
                wrtins(' repnz # move');
                wrtins(' movsb');
                wrtins(' addq $0,%rsp # remove structure from stack', ep^.q3);
                wrtins(' leaq ^-@s^0(%rbp),%1 # reindex temp', ep^.r1a, ep^.r1, lclspc^)                
            end else begin
              if ep^.r1 <> rgrax then
                wrtins(' movq %rax,%1 # place result', ep^.r1);
            end
          end;
          stkadr := stkadrs { restore stack position }
        end;

        {cke}
        188: begin
          wrtins(' movq $0,%1 # start running boolean', ep^.r2);
          ep2 := ep^.cl; 
          while ep2 <> nil do begin 
            ep2^.r1 := ep^.r1; ep2^.r2 := ep^.r2; ep2^.t1 := ep^.t1; 
            genexp(ep2); ep2 := ep2^.next 
          end;   
          wrtins(' jnz 1f # skip any variant active');
          wrtins(' leaq modnam(%rip),%rdi # set module name');
          wrtins(' movq $0,%rsi # set line number', sline);
          wrtins(' movq $VariantNotActive,%rdx # set error code');
          wrtins(' call psystem_errore # process error');
          wrtins('1:');
        end;

        {wbs}
        243: begin
          wrtins(' call psystem_withenter # establish with reference');
        end;

        {cxs}
        211: begin           
          wrtins(' decq %1 # 0 base index', ep^.r^.r1);
          if dodbgchk then begin
            wrtins(' cmpq %1,%2 # check index < length', ep^.l^.r2, ep^.r^.r1);
            wrtins(' jb 1f # skip below');
            wrtins(' leaq modnam(%rip),%rdi # load module name');
            wrtins(' movq $0,%rsi # load line number', sline);
            wrtins(' movq $ValueOutOfRange,%rdx # load error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:');
          end;
          wrtins(' movq $0,%rax # get element size', ep^.q);
          wrtins(' mulq %1 # find index*size', ep^.r^.r1);
          wrtins(' addq %rax,%1 # add to base', ep^.l^.r1);
          if ep^.r1 <> ep^.l^.r1 then
            wrtins(' movq %rax,%1 # move to result', ep^.r1)
        end;

        {cxc}
        212:begin
          { ep^.l^.r1: base addr, ep^.l^.r2: template addr, ep^.t1: temp reg}
          wrtins(' decq %1 # 0 base index', ep^.r^.r1);
          if dodbgchk then begin
            wrtins(' cmpq (%1),%2 # check index < length', ep^.l^.r2, ep^.r^.r1);
            wrtins(' jb 1f # skip below ');
            wrtins(' leaq modnam(%rip),%rdi # load module name');
            wrtins(' movq $0,%rsi # load line number', sline);
            wrtins(' movq $ValueOutOfRange,%rdx # load error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:        ');
          end;
          wrtins(' movq $0,%1 # get # levels-1', ep^.q-1, ep^.t1);
          wrtins(' movq $0,%rax # get base element size', ep^.q1);
          wrtins(' movq %1,%rdx # copy template address', ep^.l^.r2);
          wrtins('1:');
          wrtins(' addq $0,%rdx # next template location', intsize);
          wrtins(' mulq (%rdx) # add template to size');
          wrtins(' subq $0,%1 # count down levels', 1, ep^.t1);
          wrtins(' jnz 1b # loop over templates');     
          wrtins(' addq $0,%1 # advance template slot', intsize, ep^.l^.r2);  
          wrtins(' mulq %1 # find index*size', ep^.r^.r1);
          wrtins(' addq %rax,%1 # add to base', ep^.l^.r1);   
        end;

        {lft} 
        213: wrtins(' leaq @s(%rip),%1 # index template', q, ep^.r2, ep^.lt^);

        {max} 
        214: begin
          if dodbgchk then begin
            wrtins(' cmpq $0,%1 # chk lvl < 1', 1, ep^.r^.r1);
            wrtins(' jb 2f # skip if below');
            wrtins(' cmpq $0,%1 # compare', ep^.q, ep^.r^.r1);
            wrtins(' jbe 1f # skip if less or equal');
            wrtins('2:');
            wrtins(' leaq modnam(%rip),%rdi # load module name');
            wrtins(' movq $0,%rsi # load line number', sline);
            wrtins(' movq $InvalidContainerLevel,%rdx # load error code');
            wrtins(' call psystem_errore # process error');
            wrtins('1:')
          end;
          if ep^.q <> 1 then begin
            wrtins(' movq $0,%1 # get total lvl', ep^.q, ep^.t1);
            wrtins(' subq %1,%2 # find tl-al', ep^.r^.r1, ep^.t1);
            wrtins(' salq $4,%1 # *16 (long)', ep^.t1);
            wrtins(' addq %1,%2 # add to base template', ep^.l^.r2, ep^.t1);
            wrtins(' movq (%1),%2 # add to base template', ep^.t1, ep^.r1)
          end
        end;

        {equv,neqv,lesv,grtv,leqv,geqv} 
        215,216,217,218,219,220: begin
          wrtins(' call psystem_strcmp # compare strings'); 
          wrtins(' cmpq $0,%rax # compare -0+ result');
          case ep^.op of
            215{equv}: wrtins(' sete %1l # set equal', ep^.r1);
            216{neqv}: wrtins(' setne %1l # set not equal', ep^.r1);
            220{geqv}: wrtins(' setge %1l # set greater or equal', ep^.r1);
            218{grtv}: wrtins(' setg %1l # set greater', ep^.r1);
            219{leqv}: wrtins(' setle %1l # set less or equal', ep^.r1);
            217{lesv}: wrtins(' setl %1l # set less', ep^.r1);
          end;
          wrtins(' movsx %1l,%1 # sign extend boolean', ep^.r1)
        end;

        {spc} 
        222: begin
          wrtins(' movq (%1),%1 # fetch template length', ep^.l^.r2)
        end;

        {ccs} 
        223: begin
          if ep^.q = 1 then begin
            wrtins(' movq $0,%rax # get base element size', ep^.q1);
            wrtins(' mulq %1 # find base size*len', ep^.l^.r2);
            wrtins(' movq %rax,%1 # move to total length', ep^.t2);
          end else begin
            wrtins(' movq $0,%1 # get # levels', ep^.q, ep^.t1);
            wrtins(' movq $0,%1 # get base element size', ep^.q1, ep^.t2);
            wrtins(' movq %1,%2 # copy template address', ep^.l^.r2, ep^.t3);
            wrtins('1:');
            wrtins(' movq (%1),%rax # get size from template', ep^.t3);
            wrtins(' mulq %1 # multiply by size', ep^.t2);
            wrtins(' movq %rax,%1 # add template to size', ep^.t2);
            wrtins(' addq $0,%1 # next template location', intsize, ep^.t3);
            wrtins(' decq %1 # count down levels', ep^.t1);
            wrtins(' jnz 1b # loop over templates'); 
          end;
          wrtins(' subq %1,%rsp # allocate on stack', ep^.t2);  
          wrtins(' andq $0xfffffffffffffff0,%rsp # align stack');
          wrtins(' movq %1,%rsi # move source', ep^.l^.r1, ep^.t3);
          wrtins(' movq %rsp,%rdi # move dest', ep^.l^.r1, ep^.t3);
          wrtins(' movq %1,%rcx # move source', ep^.t2);
          wrtins(' repnz # move');
          wrtins(' movsb');
          wrtins(' movq %rsp,%1 # index copy', ep^.r1);
          wrtins(' movq %1,%2 # index template', ep^.l^.r2, ep^.r2)
        end;

        {ldp} 
        225: begin
          wrtins(' movq ^0(%1),%2 # get template adr', intsize, ep^.l^.r1, ep^.r2);
          wrtins(' movq (%1),%2 # get data adr', ep^.l^.r1, ep^.r1);
        end;

        {mpc}
        248: ; { registers are all assigned }

        {cpl}
        251: ; { registers are all assigned }

      end;
      for r := rgxmm15 downto rgrax do if r in ep^.rs then begin
        if r in [rgrax..rgr15] then begin
          wrtins(' popq %1 # restore used quad register', r);
          stkadr := stkadr-intsize
        end else begin
          wrtins(' movsd (%rsp),%1 # restore used real register', r);
          wrtins(' addq $0,%rsp # remove from stack', realsize);
          stkadr := stkadr-intsize
        end
      end;
      write(prr, '# generating~: '); dmpety(prr, ep); writeln(prr)
    end
  end;

  { get number of parameters of procedure/function/system call to parameters
    list }
  procedure getparn(ep: expptr; pn: integer);
  var pp: expptr;
  begin
      { pull parameters into list in reverse }
      while (pn > 0) and (estack <> nil) do 
        begin popstk(pp); pp^.next := ep^.pl; ep^.pl := pp; pn := pn-1 end
  end;

  { get parameters of procedure/function/system call to parameters list }
  procedure getpar(ep: expptr);
  begin
      ep^.sl := nil;
      { for function overloads, sfr can be on top }
      if estack^.op = 245{sfr} then popstk(ep^.sl); { get sfr start }
      getparn(ep, ep^.pn); { get those parameters into list }
      if ep^.sl = nil then popstk(ep^.sl); { get sfr start }
      if ep^.sl^.op <> 245{sfr} then error('system error');
  end;

  { reverse parameters list }
  procedure revpar(ep: expptr);
  var pl, pp: expptr;
  begin
    pl := nil;
    while ep^.pl <> nil do begin 
      pp := ep^.pl; ep^.pl := ep^.pl^.next;
      pp^.next := pl; pl := pp
    end;
    ep^.pl := pl
  end;

  { reorder last parameter }
  procedure ordpar(ep: expptr);
  var lp, pp: expptr;
  begin
    pp := ep^.pl;
    lp := nil;
    if pp <> nil then 
      while pp^.next <> nil do begin lp := pp; pp := pp^.next end;
    if (pp = nil) or (lp = nil) then error('system error');
    lp^.next := nil;
    pp^.next := ep^.pl;
    ep^.pl := pp
  end;

  { attach tag chk sequence to leaf }
  procedure attach(ep: expptr);
  begin
    if estack <> nil then
      if estack^.op = 188{cke} then popstk(ep^.al)
  end;

  { duplicate subtree }
  procedure duptre(s: expptr; var d: expptr);
  begin
    if s = nil then d := nil else begin { only copy tree components }
      getexp(d); d^ := s^; d^.next := nil; d^.sl := nil; d^.al := nil; 
      d^.pl := nil;
      duptre(s^.l, d^.l); duptre(s^.r, d^.r); duptre(s^.x1, d^.x1);
      duptre(s^.cl, d^.cl)
    end
  end;

  function alflen(var s: alfa): integer;
  var i: alfainx;
  begin
    i := 1; while (i < maxalfa) and (s[i] <> ' ') do i := i+1;
    if s[i] = ' ' then alflen := i-1 else alflen := i
  end;
 
  procedure getlvl(var p: lvltyp);
  var i: integer;
  begin
    getint(i); p := i
  end;

  procedure getadr(var a: address);
  var i: integer;
  begin
    getint(i); a := i
  end;

  procedure parp;
  begin
    getlvl(p)
  end;

  procedure parq;
  begin
    getadr(q)
  end;

  procedure parpq;
  begin
    getlvl(p); getadr(q)
  end;

  procedure parqq;
  begin
    getadr(q); getadr(q1)
  end;

  { evaluate and push n arguments depth first }
  procedure pshexps(n: integer);
  var ep: expptr; frereg: regset;
  begin
    if n > 0 then begin
      pshexps(n-1);
      frereg := allreg; popstk(ep); assreg(ep, frereg, rgnull, rgnull);
      dmptre(ep); genexp(ep);
      wrtins(' pushq %1 # place on stack', ep^.r1)
    end
  end;

begin { assemble } 
  refer(dmplst); { diagnostics }
  refer(dmptmp);
  refer(parp); { variation not used at present }
  p := 0;  q := 0;  q1 := 0; q2 := 0; q3 := 0; q4 := 0; op := 0; stkadr := 0;
  getname(name);
  { note this search removes the top instruction from use }
  while (instab[op].instr<>name) and (op < maxins) do op := op+1;
  if op = maxins then error('illegal instruction');
  case op of

    { *** non-terminals *** }

    {lodi,lodx,loda,lodr,lods,lodb,lodc,lda}
    0,193,105,106,107,108,109,4: begin parpq;
      q1 := -p*ptrsize; getexp(ep); attach(ep); pshstk(ep) 
    end;

    {adi,adr,sbi,sbr}
    28, 29, 30, 31: begin
      getexp(ep); popstk(ep^.r);
      popstk(ep^.l); pshstk(ep)
    end;

    {lip} 
    120: begin parpq;
      q1 := -p*ptrsize; getexp(ep); pshstk(ep) 
    end;

    { equm,neqm,geqm,grtm,leqm,lesm take a parameter }
    142, 148, 154, 160, 166, 172: begin parq;
      getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
    end;

    {lao} 
    5: begin skpspc;
      sp := nil;
      if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
      getexp(ep); ep^.fl := sp; attach(ep); pshstk(ep)
    end;

    {lto} 
    234: begin labelsearch(def, val, sp, blk);
      getexp(ep); ep^.qs := sp; ep^.fl := sp; attach(ep); pshstk(ep)
    end;

    {ixa}
    16: begin parq;
      getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
    end;

    {swp}
    118: begin parq;
      popstk(ep); popstk(ep2); pshstk(ep); pshstk(ep2) 
    end;

    {ldoi,ldoa,ldor,ldos,ldob,ldoc,ldox,ltci,ltcr,ltcs,ltcb,ltcc,ltcx}
    1, 65, 66, 67, 68, 69, 194,228,229,230,231,232,233: begin
      skpspc;
      sp := nil;
      if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
      getexp(ep); ep^.fl := sp; attach(ep); pshstk(ep) 
    end;

    {indi,inda,indr,inds,indb,indc,indx}
    9, 85, 86, 87, 88, 89, 198: begin parq;
      getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep)
    end;

    {inci,inca,incb,incc,incx,deci,deca,decb,decc,decx}
    10, 90, 93, 94, 201, 57, 103, 104, 202: begin parq; 
      getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep) 
    end;

    {mdc}
    254: begin parq; 
      getexp(ep); attach(ep); popstk(ep^.l); pshstk(ep) 
    end;

    {ckvi,ckvb,ckvc,ckvx}
    175, 179, 180, 203: begin parq;
      getexp(ep); pshstk(ep) 
    end;

    {cpc}
    177: begin
      getexp(ep); popstk(ep2); popstk(ep3);
      duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
      frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
      dmptre(ep); genexp(ep); deltre(ep)
    end;

    {lpa}
    114: begin getlvl(p); labelsearch(def, val, sp, blk); 
      q1 := -p*ptrsize; getexp(ep); ep^.fn := sp; pshstk(ep);
    end;

    {ldcs,ldci,ldcr,ldcn,ldcb,ldcc}
    7, 123, 124, 125, 126, 127: begin case op of

      123: begin getint(i); 
        getexp(ep); attach(ep); ep^.vi := i; pshstk(ep) 
      end;

      124: begin getreal(r); 
        getexp(ep); attach(ep);
        pshstk(ep); new(cstp); cstp^.ct := creal; 
        cstp^.r := r; realnum := realnum+1; 
        cstp^.realn := realnum; cstp^.next := csttbl; 
        csttbl := cstp; ep^.realn := realnum 
      end;

      125: begin
        getexp(ep); pshstk(ep) 
      end;

      126: begin getint(i); 
        getexp(ep); attach(ep); ep^.vi := i; pshstk(ep) 
      end;

      127: begin
        skpspc;
        if ch in ['0'..'9'] then begin i := 0;
          while ch in ['0'..'9'] do
            begin i := i*10+ord(ch)-ord('0'); getnxt end;
          c := chr(i)
        end else begin
          if ch <> '''' then error('illegal character');
          getnxt;  c := ch;
          getnxt;
          if ch <> '''' then error('illegal character')
        end;
        getexp(ep); attach(ep); ep^.vi := ord(c); pshstk(ep)
      end;

      7: begin skpspc;
        if ch <> '(' then error('ldcs() expected');
        s := [ ];  getnxt; 
        while ch<>')' do
          begin getint(s1); skpspc; s := s + [s1] end;
        getexp(ep); attach(ep); pshstk(ep);
        new(cstp); cstp^.ct := cset; cstp^.s := s;
        setnum := setnum+1; cstp^.setn := setnum;
        cstp^.next := csttbl; csttbl := cstp; ep^.setn := setnum
      end

      end (*case*)
    end;

    {chki,chks,chkb,chkc,ckla,chkx}
    26, 97, 98, 99, 190, 199: begin getint(lb); getint(ub);
      getexp(ep); popstk(ep^.l); 
      pshstk(ep); ep^.vi := lb; ep^.vi2 := ub
    end;

    {chka}
    95: begin getint(lb); getint(ub);
      if lb <> 0 then begin getexp(ep); popstk(ep^.l); pshstk(ep) end
    end;

    {lca}
    56: begin getint(l); skpspc;
      for i := 1 to strlen do str[i] := ' ';
      if ch <> '''' then error('bad string format');
      i := 0;
      repeat
        if eolinp then error('unterminated string');
        getnxt;
        c := ch; if (ch = '''') and (chla = '''') then 
          begin getnxt; c := ' ' end;
        if c <> '''' then begin
          if i >= strlen then error('string overflow');
          str[i+1] := ch; { accumulate string }
          i := i+1
        end
      until c = '''';
      getexp(ep); attach(ep); pshstk(ep);
      new(cstp); cstp^.ct := cstr; cstp^.str := strp(str); 
      cstp^.strl := l; strnum := strnum+1; cstp^.strn := strnum;
      cstp^.next := csttbl; csttbl := cstp; ep^.strn := strnum
    end;

    {grts,less}
    158,170: error('Invalid operand');

    {equa,equi,equr,equb,equs,equc}
    17, 137, 138, 139, 140, 141,
    {neqa,neqi,neqr,neqb,neqs,neqc}
    18, 143, 144, 145, 146, 147,
    {geqi,geqr,geqb,geqs,geqc}
    149, 150, 151, 152, 153,
    {grti,grtr,grtb,grtc}
    20, 155, 156, 157, 159,
    {leqi,leqr,leqb,leqs,leqc}
    161, 162, 163, 164, 165,
    {lesi,lesr,lesb,lesc}
    167, 168, 169, 171: begin
      getexp(ep); 
      { reverse order for leqs }
      if op = 164 then begin popstk(ep^.l); popstk(ep^.r) end
      else begin popstk(ep^.r); popstk(ep^.l) end;
      pshstk(ep)
    end;

    {brk}
    19: ; { unused }

    {ord}
    59, 134, 136, 200: begin
      getexp(ep); popstk(ep^.l); pshstk(ep);
    end;

    {lcp}
    135: begin
      getexp(ep); popstk(ep^.l); pshstk(ep) 
    end;

    {sgs}
    32: begin
      getexp(ep); popstk(ep^.l); pshstk(ep) 
    end;

    {flt}
    33: begin
      getexp(ep); popstk(ep^.l); pshstk(ep) 
    end;

    {flo}
    34: begin
      getexp(ep); popstk(ep2); popstk(ep^.l);
      pshstk(ep); pshstk(ep2)
    end;

    {trc}
    35: begin
      getexp(ep); popstk(ep^.l); pshstk(ep); 
    end;

    {ngi,ngr}
    36,37: begin
      getexp(ep); popstk(ep^.l); pshstk(ep) 
    end;

    {sqi,sqr}
    38,39: begin
      getexp(ep); popstk(ep^.l); pshstk(ep)
    end;

    {abi,abr}
    40,41: begin
      getexp(ep); popstk(ep^.l); pshstk(ep) 
    end;

    {notb,odd,chr,rnd,noti}
    42,50,60,62,205: begin
      getexp(ep); popstk(ep^.l); pshstk(ep)
    end;

    {and,ior,xor,dif,int,uni,inn,mod,mpi,mpr,dvi,dvr,rgs}
    43,44,45,46,47,48,49,51,52,53,54,110,206: begin
      getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep) 
    end;

    { At this level we just duplicate the tree. At lower levels we can
      optimize this. }

    { dupi, dupa, dupr, dups, dupb, dupc }
    181, 182, 183, 184, 185, 186: begin
      ep2 := nil;
      if estack <> nil then if estack^.op = 188{cke} then popstk(ep2);
      if estack = nil then error('Expression underflow');
      duptre(estack, ep); pshstk(ep);
      if ep2 <> nil then pshstk(ep2)
    end;

    {cks}
    187: begin
      getexp(ep); pshstk(ep)
    end;

    {sfr}
    245: begin labelsearch(def, val, sp, blk);
      getexp(ep); pshstk(ep);
      ep^.lb := nil;
      if (def and (val <> 0)) or not def then ep^.lb := sp
    end;


    {cuf}
    246: begin labelsearch(def, val, sp, blk);
      getadr(q); getadr(q1); getadr(q2); getadr(q3);
      getexp(ep); ep^.fn := sp; ep^.pn := q; ep^.rc := q1; ep^.blk := blk;
      getpar(ep); pshstk(ep)
    end;

    {cif}
    247: begin 
      getadr(q); getadr(q1); getadr(q2); getadr(q3);
      getexp(ep); ep^.pn := q; ep^.rc := q1; popstk(ep^.l); getpar(ep);
      pshstk(ep)
    end;

    {cvf}
    249: begin 
      skpspc;
      sp := nil;
      if ch = 'l' then begin 
        labelsearch(def, val, sp, blk);
        getadr(q1); getadr(q2); getadr(q3); getadr(q4)
      end else begin getadr(q1); getadr(q2); getadr(q3); getadr(q4) end;
      getexp(ep); ep^.qs := sp; ep^.pn := q1; getpar(ep);
      pshstk(ep)
    end;

    {cke}
    188: begin
      getexp(ep);
      ep4 := estack;
      while ep4 <> nil do begin
        if estack^.op in [{cks}187,{ckvb}179,{ckvc}180,{ckvi}175,{ckvx}203] then begin
          popstk(ep5); 
          if ep5^.op <> 187 then begin ep5^.next := ep^.cl; ep^.cl := ep5 end
          else putexp(ep5);
          ep4 := estack
        end else ep4 := nil
      end;
      popstk(ep^.l); pshstk(ep)
    end;

    {wbs}
    243: begin
      getexp(ep);
      popstk(ep^.l);
      pshstk(ep)
    end;

    {cxs}
    211: begin parq;
      getexp(ep);
      popstk(ep^.r); popstk(ep^.l);
      pshstk(ep)
    end;

    {cxc}
    212: begin parqq;
      getexp(ep);
      popstk(ep^.r); popstk(ep^.l);
      pshstk(ep)
    end;

    {lft} 
    213: begin labelsearch(def, val, sp, blk);
      getexp(ep);
      popstk(ep^.l); ep^.lt := sp;
      pshstk(ep)
    end;

    {max} 
    214: begin parq;
      getexp(ep);
      popstk(ep^.r); popstk(ep^.l);
      pshstk(ep)
    end;

    {equv,neqv,lesv,grtv,leqv,geqv} 
    215,216,217,218,219,220: begin
      getexp(ep); popstk(ep^.r); popstk(ep^.l); pshstk(ep)
    end;

    {spc} 
    222: begin
      getexp(ep);
      popstk(ep^.l);
      pshstk(ep)
    end;

    {ccs} 
    223: begin parqq;
      getexp(ep);
      popstk(ep^.l);
      pshstk(ep)
    end;

    {ldp} 
    225: begin
      getexp(ep);
      popstk(ep^.l);
      pshstk(ep)
    end;

    {mpc}
    248: begin parqq;
      ep4 := nil; popstk(ep2); i := q;
      while i > 0 do begin
        ep2^.next := ep4;
        ep4 := ep2;
        popstk(ep2);
        i := i-1
      end;
      getexp(ep); popstk(ep3);
      if q1 = 0 then begin ep^.l := ep2; ep^.r := ep3 end
      else begin ep^.l := ep3; ep^.r := ep2 end;
      pshstk(ep);
      while ep4 <> nil do begin ep := ep4; ep4 := ep4^.next; pshstk(ep) end;
    end;

    { cpl }
    251: begin
      getexp(ep);
      popstk(ep2); duptre(ep2, ep3); pshstk(ep2);
      ep^.l := ep3; pshstk(ep)
    end;

    { *** calls can be terminal or non-terminal *** }

    {csp} 
    15: begin skpspc; getname(name);
      while name<>sfptab[q].sptable do begin 
        q := q+1; if q > maxsp then error('std proc/func not found')
      end; 
      getexp(ep); 
      if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then 
        begin getparn(ep, maxint); revpar(ep); ordpar(ep) end
      else getparn(ep, sfptab[q].sppar);
      if sfptab[q].spfunc then pshstk(ep) { non-terminal, stack it }
      else begin { terminal, execute here }
        if sfptab[ep^.q].spkeep then begin
          if ep^.pl = nil then error('System error');
          duptre(ep^.pl, ep2); pshstk(ep2)
        end;
        frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
        dmptre(ep); genexp(ep);
        deltre(ep)
      end
    end;

    {cuv}
    27: begin 
      skpspc;
      sp := nil;
      if ch = 'l' then begin 
        labelsearch(def, val, sp, blk);
        getadr(q1)
      end else begin getadr(q); getadr(q1) end;
      getexp(ep); ep^.qs := sp; ep^.pn := q1; getpar(ep);
      frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
      genexp(ep); deltre(ep)
    end;

    { *** terminals *** }

    {cvbi,cvbx,cvbb,cvbc}
    100, 115, 116, 121,
    {ivti,ivtx,ivtb,ivtc,cta}
    192,101,102,111,191: begin getadr(q); getadr(q1);
      labelsearch(def, val, sp, blk); 
      getexp(ep); ep^.lt := sp; popstk(ep2); popstk(ep3); 
      duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
      frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
      dmptre(ep); genexp(ep); deltre(ep)
    end;

    {cup}
    12: begin labelsearch(def, val, sp, blk); getadr(q1);
      getexp(ep); ep^.fn := sp; ep^.pn := q1; ep^.blk := blk; getpar(ep);
      frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
      genexp(ep); deltre(ep);
    end;

    {cip}
    113: begin parq;
      getexp(ep); ep^.pn := q; popstk(ep^.l); getpar(ep);
      frereg := allreg; assreg(ep, frereg, rgnull, rgnull); dmptre(ep);
      genexp(ep); deltre(ep);
    end;

    {rip}
    13: parq;

    {stri,stra}
    2,70: begin parpq;
      frereg := allreg;
      popstk(ep); attach(ep); if p <> blkstk^.lvl then getreg(r1, frereg); 
      assreg(ep, frereg, rgnull, rgnull); 
      dmptre(ep); genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then begin
        wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
        wrtins(' movq %1,@l(%2) # store qword', q, p, ep^.r1, r1)
      end else 
        wrtins(' movq %1,@l(%rbp) # store qword', q, p, ep^.r1);
      deltre(ep)
    end;

    {strx,strb,strc} 
    195,73,74: begin parpq;
      frereg := allreg; if p <> blkstk^.lvl then getreg(r1, frereg);
      popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); 
      dmptre(ep); genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then begin
        wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
        wrtins(' movb %1l,@l(%2) # store byte', q, p, ep^.r1, r1)
      end else
        wrtins(' movb %1l,@l(%rbp) # store byte', q, p, ep^.r1);
      deltre(ep)
    end;

    {strr}
    71: begin parpq;
      frereg := allreg; if p <> blkstk^.lvl then getreg(r1, frereg);
      popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); 
      dmptre(ep); genexp(ep); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then begin
        wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
        wrtins(' movsd %1,@l(%2) # store real', q, p, ep^.r1, r1)
      end else
        wrtins(' movsd %1,@l(%rbp) # store real', q, p, ep^.r1);
      deltre(ep)
    end;

    {strs} 
    72:begin parpq;
      frereg := allreg; popstk(ep); attach(ep); assreg(ep, frereg, rgrsi, rgnull); 
      dmptre(ep); genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then
        wrtins(' movq ^0(%rbp),%rdi # get display pointer', -p*ptrsize)
      else
        wrtins(' movq %rbp,%rdi # get display pointer', -p*ptrsize);
      wrtins(' leaq @l(%rdi),%rdi # index destination', q, p);
      wrtins(' movsq # move set');
      wrtins(' movsq');
      wrtins(' movsq');
      wrtins(' movsq');
      puttmp(ep^.r1a); deltre(ep)
    end;

    {sev}
    253: begin parpq;
      frereg := allreg-[rgrax];
      getreg(r1, frereg); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' popq %rax # get exception vector'); 
      wrtins(' pushq %rax # replace'); 
      if p <> blkstk^.lvl then begin
        wrtins(' movq ^0(%rbp),%1 # get display pointer', -p*ptrsize, r1);
        wrtins(' movq %rax,@l(%1) # store qword', q, p, r1)
      end else
        wrtins(' movq %rax,@l(%rbp) # store qword', q, p)
    end;

    {mst}
    11: begin getlvl(p); labelsearch(def, val, lclspc, blk); 
      labelsearch(def2, val2, sp2, blk);
      if blkstk <> nil then
        if blkstk^.btyp in [btproc, btfunc] then begin
          write(prr, '        .globl   '); wrtblklng(blkstk); writeln(prr);
          write(prr, '        .type    '); wrtblklng(blkstk); writeln(prr, ', @function');
          wrtblklabs(blkstk);
        end;
      frereg := allreg;
      { We limit to the enter instruction }
      if p >= 32 then error('Too many nested levels');
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' pushq $0 # place current ep');
      wrtins(' pushq $0 # place bottom of stack');
      wrtins(' pushq $0 # place previous ep');
      wrtins(' enterq $1,$0 # enter frame', p+1);
      wrtins(' movq %rsp,%rax # copy sp');
      { find sp-locals }
      write(prr, '        subq    $'); write(prr, lclspc^); write(prr, '+'); 
      write(prr, blkstk^.tmpnam^); writeln(prr, ',%rax # find sp-locals');
      wrtins('1:', lclspc^);
      wrtins(' cmpq %rax,%rsp # check have reached stack');
      wrtins(' je 2f # skip if so');
      wrtins(' pushq $0 # push 0 word for locals');
      wrtins(' jmp 1b # loop', 7);
      wrtins('2:', lclspc^);
      wrtins(' movq %rsp,^0(%rbp) # set bottom of stack', marksb);
      { note there is no way to know locals space in advance }
      wrtins(' andq $0xfffffffffffffff0,%rsp # align stack');
      { save protected registers and keep aligned }
      wrtins(' pushq %rbx # save protected registers and keep aligned');
      wrtins(' pushq %r12');
      wrtins(' pushq %r13');
      wrtins(' pushq %r14');
      wrtins(' pushq %r15');
      wrtins(' pushq %r15 # second push aligns');
      tmpoff := -(p+1)*ptrsize;
      tmpspc := 0; { clear temps }
      stkadr := 0;
      { note ep is unused at this time }
      botstk
    end;

    {mov}
    55: begin parq;
      frereg := allreg; popstk(ep); popstk(ep2); dmptre(ep); dmptre(ep2);
      assreg(ep2, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
      assreg(ep, frereg, rgrsi, rgnull);
      genexp(ep2); genexp(ep);
      wrtins(' movq $0,%rcx # load the length of move', q);
      wrtins(' repnz # move/copy');
      wrtins(' movsb');
      deltre(ep); deltre(ep2);
      botstk
    end;

    {dmp}
    117: begin parq;
      { unfortunately case statements can jump to an expected dump }
      if estack <> nil then begin popstk(ep); deltre(ep) end
    end;

    {sroi,sroa,sror,srob,sroc,srox}
    3, 75, 76, 78, 79, 196: begin
      skpspc;
      sp := nil;
      if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
      frereg := allreg;
      popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); dmptre(ep); 
      genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      if (op = 78{srob}) or (op = 79){sroc} or (op = 196){srox} then begin
        if sp <> nil then
          wrtins(' movb %1l,@s(%rip) # store byte to global', ep^.r1, sp^)
        else
          wrtins(' movb %1l,@g(%rip) # store byte to global', q, ep^.r1)
      end else if op = 76{sror} then begin
        if sp <> nil then
          wrtins(' movsd %1l,@s(%rip) # store real to global', ep^.r1, sp^)
        else
          wrtins(' movsd %1l,@g(%rip) # store real to global', q, ep^.r1)
      end else begin {sroi, sroa}
        if sp <> nil then
          wrtins(' movq %1,@s(%rip) # store quad to global', q, ep^.r1, sp^)
        else
          wrtins(' movq %1,@g(%rip) # store quad to global', q, ep^.r1)
      end;
      deltre(ep)
    end;

    {sros}
    77: begin
      skpspc;
      sp := nil;
      if ch = 'l' then labelsearch(def, val, sp, blk) else parq;
      frereg := allreg;
      popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull); dmptre(ep); 
      genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' leaq ^-@s^0(%rbp),%rsi # index temp set', ep^.r1a, lclspc^);
      if sp <> nil then
        wrtins(' leaq @s(%rip),%rdi # index global destination', ep^.r1, sp^)
      else
        wrtins(' leaq @g(%rip),%rdi # index global destination', q, ep^.r1);
      wrtins(' movsq # move');
      wrtins(' movsq');
      wrtins(' movsq');
      wrtins(' movsq');
      puttmp(ep^.r1a); deltre(ep)
    end;

    {aps}
    178: begin parq;
      frereg := allreg; popstk(ep2); popstk(ep);
      assreg(ep, frereg, rgrdi, rgrcx); frereg := frereg-[rgrdi, rgrcx];
      assreg(ep2, frereg, rgrsi, rgnull);
      dmptre(ep); genexp(ep);
      dmptre(ep2); genexp(ep2);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq $0,%rax # get size', q);
      wrtins(' mulq %rcx # find len*size');
      wrtins(' movq %rax,%rcx # place size');
      wrtins(' repnz # move data  ');
      wrtins(' movsb    ');
      dmptre(ep); deltre(ep2); deltre(ep); 
      botstk  
    end; 

    {pck}
    63: begin parqq;
      frereg := allreg; popstk(ep);
      popstk(ep2); popstk(ep3); dmptre(ep3); dmptre(ep2); dmptre(ep);
      assreg(ep, frereg, rgrdx, rgnull); frereg := frereg-[rgrdx];
      assreg(ep2, frereg, rgrcx, rgnull); frereg := frereg-[rgrcx];
      assreg(ep3, frereg, rgr8, rgnull);
      genexp(ep); genexp(ep2); genexp(ep3);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq $0,%rdi # get size of packed array', q);
      wrtins(' movq $0,%rsi # get size of unpacked array', q1);
      wrtins(' call psystem_pack # pack the array');
      deltre(ep); deltre(ep2); deltre(ep3); 
      botstk 
    end;

    {upk}
    64: begin parqq;
      frereg := allreg; popstk(ep);
      popstk(ep2); popstk(ep3); dmptre(ep3); dmptre(ep2); dmptre(ep); 
      assreg(ep, frereg, rgrdx, rgnull); frereg := frereg-[rgrdx];
      assreg(ep2, frereg, rgrcx, rgnull); frereg := frereg-[rgrcx];
      assreg(ep3, frereg, rgr8, rgnull);
      genexp(ep); genexp(ep2); genexp(ep3);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq $0,%rdi # load size of packed array', q);
      wrtins(' movq $0,%rsi # load size of unpacked array', q1);
      wrtins(' call psystem_unpack # unpack the array');
      deltre(ep); deltre(ep2); deltre(ep3); 
      botstk 
    end;

    {ujp}
    23: begin labelsearch(def, val, sp, blk);
      wrtins(' jmp @s', sp^);
      if estack <> nil then begin { put in unresolved cache }
        getexp(ep); ep^.qs := sp;
        ep^.l := estack; estack := nil; ep^.next := jmpstr; jmpstr := ep;
      end
    end;

    {fjp,tjp}
    24,119: begin labelsearch(def, val, sp, blk);
      frereg := allreg; popstk(ep); 
      assreg(ep, frereg, rgnull, rgnull); dmptre(ep); genexp(ep); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' orb %1l,%1l # move boolean to flags', ep^.r1);
      if op = 24{fjp} then wrtins(' jz @s # go if false', sp^)
      else {tjp} wrtins(' jnz @s # go if true', sp^);
      deltre(ep)
    end;

    {xjp}
    25: begin labelsearch(def, val, sp, blk);
      frereg := allreg; popstk(ep); getreg(r1, frereg);
      assreg(ep, frereg, rgnull, rgnull); 
      dmptre(ep); genexp(ep); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq %1,%2 # make factoring copy of index', ep^.r1, r1);
      wrtins(' salq $2,%1 # *4', ep^.r1);
      wrtins(' addq %2,%1 # *5', ep^.r1, r1);
      wrtins(' leaq @s(%rip),%1 # index case jump table', r1, sp^);
      wrtins(' addq %2,%1 # add scaled index to base', ep^.r1, r1);
      wrtins(' jmp *%1', ep^.r1);
      deltre(ep); 
      botstk 
    end;

    {ipj}
    112: begin getlvl(p); labelsearch(def, val, sp, blk);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq ^0(%rbp),%rbp # get frame pointer for target', -p*ptrsize);
      wrtins(' movq ^0(%rbp),%rsp # get stack for target', marksb);
      wrtins(' andq $0xfffffffffffffff0,%rsp # align stack');
      wrtins(' jmp @s # goto jump target', sp^);
      botstk 
    end;

    {vbs}
    92: begin parq;
      frereg := allreg; popstk(ep); 
      assreg(ep, frereg, rgrdi, rgnull); dmptrel(ep, 19); genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq %rdi,%rsi # set start of variable block');
      wrtins(' addq $0,%rsi # set end of variable block', ep^.q-1);
      wrtins(' call psystem_varenter # establish variable reference block');
      deltre(ep);
      botstk
    end;

    {vbe}
    96: begin
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' call psystem_varexit # remove variable reference block');
      botstk
    end;

    {ret}
    22: begin
      frereg := allreg;
      wrtins(' ret      ');
      botstk
    end;

    {retp,retm}
    14,237: begin parq;
      frereg := allreg;
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' popq %r15 # undo alignment push');
      wrtins(' popq %r15 # restore protected registers');
      wrtins(' popq %r14');
      wrtins(' popq %r13');
      wrtins(' popq %r12');
      wrtins(' popq %rbx');
      wrtins(' leave # undo frame');
      wrtins(' addq $0,%rsp # remove frame data', marksize);
      wrtins(' popq %rcx # get return address');
      wrtins(' addq $0,%rsp # remove caller parameters', q);
      if op = 237{retm} then
        wrtins(' movq %rsp,%rax # index result in rax');
      wrtins(' pushq %rcx # replace return address');
      wrtins(' ret # return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    {reti,reta,retx,retc,retb}
    128,132,204,130,131: begin parq;
      frereg := allreg;
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' popq %r15 # undo alignment push');
      wrtins(' popq %r15 # restore protected registers');
      wrtins(' popq %r14');
      wrtins(' popq %r13');
      wrtins(' popq %r12');
      wrtins(' popq %rbx');
      wrtins(' leave # undo frame');
      wrtins(' addq $0,%rsp # remove frame data', marksize);
      wrtins(' popq %rcx # get return address');
      wrtins(' addq $0,%rsp # remove caller parameters', q);
      wrtins(' popq %rax # get qword result');
      if op in [204{retx},130{retc},131{retb}] then
        wrtins(' andq $0,%rax # mask byte result', 255);
      wrtins(' pushq %rcx # replace return address');
      wrtins(' ret # return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    {retr}
    129: begin parq;
      frereg := allreg;
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      { restore protected registers }
      wrtins(' popq %r15 # undo alignment push');
      wrtins(' popq %r15 # restore protected registers');
      wrtins(' popq %r14');
      wrtins(' popq %r13');
      wrtins(' popq %r12');
      wrtins(' popq %rbx');
      wrtins(' leave # undo frame');
      wrtins(' addq $0,%rsp # remove frame data', marksize);
      wrtins(' popq %rcx # get return address');
      wrtins(' addq $0,%rsp # remove caller parameters', q);
      wrtins(' movsd (%rsp),%xmm0 # move real from stack to xmm0');
      wrtins(' addq $0,%rsp # remove real from stack', realsize);
      wrtins(' pushq %rcx # restore return address');
      wrtins(' ret # return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    {rets}
    236: begin parq;
      frereg := allreg;
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      { restore protected registers }
      wrtins(' popq %r15 # undo alignment push');
      wrtins(' popq %r15 # restore protected registers');
      wrtins(' popq %r14');
      wrtins(' popq %r13');
      wrtins(' popq %r12');
      wrtins(' popq %rbx');
      wrtins(' leave # undo frame');
      wrtins(' addq $0,%rsp # remove frame data', marksize);
      wrtins(' popq %rcx # get return address');
      wrtins(' addq $0,%rsp # remove caller parameters', q);
      wrtins(' pushq %rcx # restore return address');
      wrtins(' ret # return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    {stoi,stoa,stor,stob,stoc,stox}
    6, 80, 81, 83, 84, 197: begin
      frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
      getreg(ep^.r1, frereg);
      assreg(ep, frereg, ep^.r1, rgnull);
      assreg(ep2, frereg, rgnull,  rgnull);
      dmptre(ep); dmptre(ep2);
      genexp(ep); genexp(ep2);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      case op of
        6{stoi},80{stoa}: wrtins(' movq %1,(%2) # store quad to address', q, ep2^.r1, ep^.r1);
        81{stor}: wrtins(' movsd %1,(%2) # store real to address', q, ep2^.r1, ep^.r1);
        83{stob},84{stoc},197{stox}:
          wrtins(' movb %1l,(%2) # store byte to address', q, ep2^.r1, ep^.r1)
      end;
      deltre(ep); deltre(ep2)
    end;

    {stos}
    82: begin
      frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
      assreg(ep, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
      assreg(ep2, frereg, rgrsi,  rgnull);
      dmptre(ep); dmptre(ep2);
      genexp(ep); genexp(ep2);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movsq # store set to address');
      wrtins(' movsq');
      wrtins(' movsq');
      wrtins(' movsq');
      puttmp(ep2^.r1a);
      deltre(ep); deltre(ep2)
    end;

    {stom} 
    235: begin parqq; 
      frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
      assreg(ep, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
      assreg(ep2, frereg, rgrsi,  rgnull);
      dmptre(ep); dmptre(ep2);
      genexp(ep); genexp(ep2);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq $0,%rcx # set length', q);
      wrtins(' repnz # move structure to address');
      wrtins(' movsb');
      puttmp(ep2^.r1a);
      deltre(ep); deltre(ep2)
    end;

    {stp}
    58: ; { unused }

    {inv} { a no-op in pgen }
    189: begin 
      frereg := allreg; popstk(ep); dmptre(ep); deltre(ep); 
      botstk
    end;

    61 {ujc}: begin
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' call psystem_caseerror');
      botstk
    end;

    {cjp}
    8: begin getadr(q); getadr(q1); labelsearch(def, val, sp, blk); 
      frereg := allreg; popstk(ep); 
      assreg(ep, frereg, rgnull, rgnull);
      dmptre(ep); genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' cmpq $0,%1 # check against low bound', q, ep^.r1);
      wrtins(' jl 1f # skip if lower');
      wrtins(' cmpq $0,%1 # check against high bound', q1, ep^.r1);
      wrtins(' jle @s # if less or equal, jump to target', sp^);
      wrtins('1:');
      pshstk(ep)
    end;

    {wbe}
    244: begin
      frereg := allreg;
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' call psystem_withexit # remove last with');
      botstk
    end;

    {vip}
    133: begin parqq;
      frereg := allreg;
      popstk(ep); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      pshexps(q); 
      assreg(ep, frereg, rgrdx, rgnull); dmptre(ep); genexp(ep);
      wrtins(' movq $0,%rdi # load # levels', q);
      wrtins(' movq $0,%rsi # base element size', q1);
      wrtins(' movq %rsp,%rcx # load array dimension list');
      wrtins(' call psystem_vip # fill template and allocate variable');
      wrtins(' addq $0,%rsp # dump dimensions from stack', q*intsize);
      deltre(ep);
      botstk
    end;

    {vis}
    122: begin parqq;
      frereg := allreg;
      popstk(ep); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      pshexps(q); 
      assreg(ep, frereg, rgrdx, rgnull); dmptre(ep); genexp(ep);
      wrtins(' movq $0,%rdi # index level', q, r1);
      wrtins(' movq $0,%rsi # base element size', q1);
      wrtins(' movq %rsp,%rcx # load array dimension list');
      wrtins(' pushq %rdx # save variable address');
      wrtins(' call psystem_vis # fill template and allocate variable');
      wrtins(' popq %rdx # restore variable address');
      wrtins(' addq $0,%rsp # dump dimensions from stack', q*intsize);
      wrtins(' popq %rbx # get return address');
      wrtins(' subq %rax,%rsp # allocate vector on stack', q*intsize);
      wrtins(' movq %rsp,(%rdx) # set variable address');
      wrtins(' andq $0xfffffffffffffff0,%rsp # align stack');
      wrtins(' pushq %rbx # replace return address');
      deltre(ep);
      botstk
    end;

    {vin}
    226: begin parqq;
      frereg := allreg;
      popstk(ep); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      pshexps(q); 
      assreg(ep, frereg, rgrdx, rgnull); dmptre(ep); genexp(ep);
      wrtins(' movq $0,%rdi # load # levels', q, r1);
      wrtins(' movq $0,%rsi # base element size', q1);
      wrtins(' movq %rsp,%rcx # load array dimension list');
      wrtins(' call psystem_vin # fill template and allocate variable');
      wrtins(' addq $0,%rsp # dump dimensions from stack', q*intsize);
      deltre(ep);
      botstk
    end;

    {suv}
    91: begin labelsearch(def, val, sp, blk); 
      skpspc;
      sp2 := nil;
      if ch = 'l' then labelsearch(def, val, sp2, blk) else getadr(q1); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' leaq @s(%rip),%rax # get new vector address', sp^);

      if sp2 <> nil then wrtins(' movq %rax,@s(%rip) #  place new vector', sp2^)
      else wrtins(' movq %rax,@g(%rip) # place new vector', q1);
    end;

    {cal}
    21: begin labelsearch(def, val, sp, blk);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' call @s # call routine/initializer', sp^);
    end;

    {bge}
    207: begin labelsearch(def, val, sp, blk);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' pushq psystem_expadr(%rip) # save current exception frame');
      wrtins(' pushq psystem_expstk(%rip)');
      wrtins(' pushq psystem_expmrk(%rip)');          
      wrtins(' pushq $0 # place dummy vector');
      wrtins(' leaq @s(%rip),%rax # place new exception frame', sp^);
      wrtins(' movq %rax,psystem_expadr(%rip)');
      wrtins(' movq %rsp,psystem_expstk(%rip)');
      wrtins(' movq %rbp,psystem_expmrk(%rip)');
      botstk
    end;        

    {ede}
    208: begin
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' popq %rax # Dispose vector');
      wrtins(' popq psystem_expmrk(%rip) # restore previous exception frame');
      wrtins(' popq psystem_expstk(%rip)');
      wrtins(' popq psystem_expadr(%rip)');
      botstk
    end;

    {mse}
    209: begin
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' popq %rdx # get error vector');
      wrtins(' popq psystem_expmrk(%rip) # restore previous exception frame');
      wrtins(' popq psystem_expstk(%rip)');
      wrtins(' popq psystem_expadr(%rip)');
      wrtins(' movq psystem_expadr(%rip),%rax');

      wrtins(' orq %rax,%rax');
      wrtins(' jnz 1f # skip if less or equal');
      wrtins('1:');
      wrtins(' leaq modnam(%rip),%rdi # load module name');
      wrtins(' movq $0,%rsi # load line number', sline);
{??? Why didn't this stop unhandled exceptions ???}
      wrtins(' call psystem_errorv # process error');

      wrtins(' movq psystem_expmrk(%rip),%rbp # throw to new frame');
      wrtins(' popq psystem_expstk(%rip)');
      wrtins(' popq psystem_expadr(%rip)');
      wrtins(' popq %rax # dump dummy vector for this frame');
      wrtins(' pushq %rdx # set new vector');
      botstk
    end;

    {apc}
    210: begin parqq;
      frereg := allreg;
      popstk(ep); popstk(ep2);
      assreg(ep, frereg, rgrdx, rgnull); frereg := frereg-[rgrdx];
      assreg(ep2, frereg, rgrcx, rgr8); 
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      dmptre(ep2); genexp(ep2);
      dmptre(ep); genexp(ep);
      wrtins(' movq $0,%rdi # load # levels ', q, r1);
      wrtins(' movq $0,%rsi # base element size       ', q1);
      wrtins(' call psystem_apc # assign containers   ');
      deltre(ep); deltre(ep2);
      botstk
    end;

    {vdp,vdd} 
    221,227: begin
      frereg := allreg;
      popstk(ep);
      assreg(ep, frereg, rgrdi, rgnull);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      dmptre(ep); genexp(ep);
      wrtins(' movq $0,%rsi # load size', 1);
      wrtins(' call psystem_dsp # dispose of vector');
      deltre(ep); 
      botstk
    end;

    {scp} 
    224: begin
      frereg := allreg;
      { complex pointer, store address }
      popstk(ep2); popstk(ep);
      getreg(r1, frereg);
      assreg(ep, frereg, r1, rgnull);
      assreg(ep2, frereg, rgnull, rgnull);
      dmptre(ep); dmptre(ep2);
      genexp(ep); genexp(ep2);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq %1,(%2) # store array address', ep2^.r1, ep^.r1);
      wrtins(' addq $0,%1 # skip to template', intsize, ep^.r1);
      wrtins(' movq %1,(%2) # store template', ep2^.r2, ep^.r1)
    end;

    {ctb} 
    238: begin parqq;
      frereg := allreg;
      popstk(ep);
      frereg := frereg-[rgrdi,rgrsi,rgrcx];
      assreg(ep, frereg, rgnull, rgnull);
      dmptre(ep); genexp(ep);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq %1,%rdi # index destination', q, ep^.r1);
      wrtins(' movq %rsp,%rsi # index stack data', q);
      wrtins(' movq $0,%rcx # set length', q);
      wrtins(' repnz # copy to buffer');
      wrtins(' movsb    ');
      wrtins(' addq $0,%rsp # remove from stack', q1)
    end;

    {cps}
    176: begin
      getexp(ep); popstk(ep2); popstk(ep3);
      duptre(ep2, ep^.r); duptre(ep3, ep^.l); pshstk(ep3); pshstk(ep2);
      frereg := allreg; assreg(ep, frereg, rgnull, rgnull); 
      dmptre(ep); genexp(ep); deltre(ep)
    end;

    {cpp} 
    239: parqq; { this is a no-op to us }

    {cpr} 
    240: parqq; { this is a no-op to us }

    {sfs}
    252: begin parqq; 
      frereg := allreg; popstk(ep2); popstk(ep); attach(ep);
      assreg(ep2, frereg, rgrdi, rgnull); frereg := frereg-[rgrdi];
      assreg(ep, frereg, rgrsi,  rgnull);
      dmptre(ep); dmptre(ep2);
      genexp(ep); genexp(ep2);
      writeln(prr, '# generating: ', op:3, ': ', instab[op].instr);
      wrtins(' movq $0,%rcx # set length', q);
      wrtins(' repnz # move structure to address');
      wrtins(' movsb');
      puttmp(ep^.r1a);
      deltre(ep); deltre(ep2)
    end;

    {*** These instructions are stack arrangers in the interpreter and are ignored here *** }

    {lsa} 
    241: parq;

    {lsp}
    250: ;

  end (*case*)

end; (*assemble*)

begin (* main *)

  proginit; { perform independent init }

  write('P6 Pascal AMD64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  writeln;

  parcmdlin; { parse command line }

  rewrite(prr);

  writeln('Generating program');

  writeln(prr, '#');
  write(prr, '# File generated by P6 Pascal AMD64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write(prr, '.x');
  writeln(prr);
  writeln(prr, '#');
  writeln(prr);

  xlate; (* assembles and stores code *)

  99 : { abort run }

  writeln;
  writeln('Program generation complete');

  { return 0 on no error, 1 on error }
  seterr(ord(errret));

end.
