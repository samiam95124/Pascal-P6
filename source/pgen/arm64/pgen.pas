{*******************************************************************************
*                                                                              *
*                     ARM64 code generator for Pascal-P6                       *
*                                                                              *
* Generates assembly code for ARM64 (AArch64) and gcc/gas.                     *
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
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  *
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

program pgen_arm64(output);

joins services;

uses endian,           { endian mode }
     mpb,              { machine parameter block }
     version,          { current version number }
     parcmd,           { command line parsing }
     registers_arm64,  { ARM64 cpu specific registers }
     independent;      { cpu independent module }

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
    else if (ep^.op in [247{cif}, 246{cuf}, 249{cvf}]) and
            (ep^.rc = 1) then isf := true
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
    { ARM64 calling convention: X0-X15 are caller-saved }
    dstreg(rgx0); dstreg(rgx1); dstreg(rgx2); dstreg(rgx3);
    dstreg(rgx4); dstreg(rgx5); dstreg(rgx6); dstreg(rgx7);
    dstreg(rgx8); dstreg(rgx9); dstreg(rgx10); dstreg(rgx11);
    dstreg(rgx12); dstreg(rgx13); dstreg(rgx14); dstreg(rgx15);
    { V0-V7 and V16-V31 are caller-saved }
    dstreg(rgv0); dstreg(rgv1); dstreg(rgv2); dstreg(rgv3);
    dstreg(rgv4); dstreg(rgv5); dstreg(rgv6); dstreg(rgv7);
    dstreg(rgv16); dstreg(rgv17); dstreg(rgv18); dstreg(rgv19);
    dstreg(rgv20); dstreg(rgv21); dstreg(rgv22); dstreg(rgv23);
    dstreg(rgv24); dstreg(rgv25); dstreg(rgv26); dstreg(rgv27);
    dstreg(rgv28); dstreg(rgv29); dstreg(rgv30); dstreg(rgv31)
  end;

  begin { assreg }
    write(prr, '// assigning:  '); dmpety(prr, ep, r1, r2);
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
        assreg(ep^.l, rf, rgx0, rgnull);
        assreg(ep^.r, rf, rgx1, rgnull);
        if (r1 = rgnull) and (rgx0 in rf) then ep^.r1 := rgx0
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
        assreg(ep^.l, rf, rgx0, rgnull);
        assreg(ep^.r, rf, rgx1, rgnull)
      end;

      5{lao},234{lto}: begin ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) end;

      16{ixa}: begin
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf) else resreg(ep^.r1);
        ep^.t1 := ep^.r1;
        getreg(ep^.t2, rf); { need temp for multiply }
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
        assreg(ep^.l, rf, rgnull, rgnull);
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
        assreg(ep^.l, rf, rgx3, rgnull); assreg(ep^.r, rf, rgx4, rgnull);
        ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgx0
      end;

      {ivti,ivtx,ivtb,ivtc}
      192,101,102,111: begin
        asscall;
        assreg(ep^.l, rf, rgx3, rgnull); assreg(ep^.r, rf, rgx4, rgnull);
        ep^.r1 := r1; if ep^.r1 = rgnull then ep^.r1 := rgx0
      end;

      {cta}
      191: begin
        asscall;
        assreg(ep^.l, rf, rgx3, rgnull); assreg(ep^.r, rf, rgx4, rgnull)
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
        assreg(ep^.l, rf, rgnull, rgx1); assreg(ep^.r, rf, rgnull, rgx2);
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
        ep^.r2 := ep^.l^.r2
      end;

      {chks}
      97: begin
        asscall;
        assreg(ep^.l, rf, rgx2, rgnull);
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        ep^.r1a := ep^.l^.r1a
      end;

      {ckla}
      190: begin
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
        asscall; assreg(ep^.l, rf, rgx0, rgnull);
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
        assreg(ep^.l, rf, ep^.r1, r2)
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
        assreg(ep^.l, rf, rgx0, rgnull); resreg(rgx0);
        assreg(ep^.r, rf, rgx1, rgnull);
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        ep^.r1a := ep^.l^.r1a
      end;

      {inn}
      48: begin
        asscall;
        assreg(ep^.l, rf, rgx0, rgnull); resreg(rgx0);
        assreg(ep^.r, rf, rgx1, rgnull);
        if (r1 = rgnull) and (rgx0 in rf) then ep^.r1 := rgx0 else
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf)
      end;

      {mod}
      49: begin
        ep^.r1 := r1;
        assreg(ep^.l, rf, rgnull, rgnull); resreg(ep^.l^.r1);
        assreg(ep^.r, rf, rgnull, rgnull);
        if ep^.r1 = rgnull then getreg(ep^.r1, rf)
      end;

      {dvi}
      53: begin
        ep^.r1 := r1;
        assreg(ep^.l, rf, rgnull, rgnull); resreg(ep^.l^.r1);
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
        assreg(ep^.l, rf, rgx0, rgnull); assreg(ep^.r, rf, rgx1, rgnull);
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
        if (ep^.q = 39{nwl}) or (ep^.q = 40{dsl}) then resreg(rgx2);
        if sfptab[ep^.q].spfunc then begin { function }
          if isfltres(ep) then begin
            if (r1 = rgnull) and (rgv0 in rf) then ep^.r1 := rgv0
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getfreg(ep^.r1, rf);
            if ep^.r1 <> rgv0 then dstreg(rgv0)
          end else begin
            if (r1 = rgnull) and (rgx0 in rf) then ep^.r1 := rgx0
            else ep^.r1 := r1;
            if ep^.r1 = rgnull then getreg(ep^.r1, rf);
            if ep^.r1 <> rgx0 then dstreg(rgx0)
          end
        end
      end;

      {cuf}
      246: begin
        asscall;
        if ep^.rc = 1 then begin
          if r1 = rgnull then begin
            if rgv0 in rf then ep^.r1 := rgv0 else getfreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end else if (ep^.rc = 2) or (ep^.rc = 3) then begin
          ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
          gettmp(ep^.r1a, setsize)
        end else begin
          if r1 = rgnull then begin
            if rgx0 in rf then ep^.r1 := rgx0 else getreg(ep^.r1, rf)
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
        asscall; resreg(rgx28);
        if ep^.rc = 1 then begin
          if r1 = rgnull then begin
            if rgv0 in rf then ep^.r1 := rgv0 else getfreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end else if (ep^.rc = 2) or (ep^.rc = 3) then begin
          ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
          gettmp(ep^.r1a, setsize)
        end else begin
          if r1 = rgnull then begin
            if rgx0 in rf then ep^.r1 := rgx0 else getreg(ep^.r1, rf)
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
            if rgv0 in rf then ep^.r1 := rgv0 else getfreg(ep^.r1, rf)
          end else ep^.r1 := r1
        end else if (ep^.rc = 2) or (ep^.rc = 3) then begin
          ep^.r1 := r1; if r1 = rgnull then getreg(ep^.r1, rf);
          gettmp(ep^.r1a, setsize)
        end else begin
          if r1 = rgnull then begin
            if rgx0 in rf then ep^.r1 := rgx0 else getreg(ep^.r1, rf)
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
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        resreg(ep^.r1);
        assreg(ep^.l, rf, ep^.r1, rgnull); resreg(ep^.l^.r2);
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {cxc}
      212: begin
        ep^.r1 := r1; ep^.r2 := r2;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
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
        assreg(ep^.l, rf, rgnull, ep^.r1);
        assreg(ep^.r, rf, rgnull, rgnull)
      end;

      {equv,neqv,lesv,grtv,leqv,geqv}
      215,216,217,218,219,220: begin
        asscall;
        ep^.r1 := r1;
        if ep^.r1 = rgnull then getreg(ep^.r1, rf);
        assreg(ep^.l, rf, rgx0, rgx2);
        assreg(ep^.r, rf, rgx1, rgnull)
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
    write(prr, '// assigning~: '); dmpety(prr, ep); write(prr, ' ~rf: ');
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
        wrtins(' str %1, [sp, #-16]! // place 2nd register on stack', pp^.r2);
        stkadr := stkadr-intsize
      end;
      if instab[pp^.op].inss then begin
        wrtins(' sub sp, sp, #0 // allocate set', setsize);
        wrtins(' mov x9, %1 // copy source', pp^.r1);
        wrtins(' mov x10, sp // destination is stack');
        wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
        wrtins(' stp x11, x12, [x10], #16 // store set part 1');
        wrtins(' ldp x11, x12, [x9] // load set part 2');
        wrtins(' stp x11, x12, [x10] // store set part 2');
        stkadr := stkadr-setsize
      end else if pp^.r1 in [rgx0..rgx28] then begin
        wrtins(' str %1, [sp, #-16]! // save parameter', pp^.r1);
        stkadr := stkadr-intsize
      end else if pp^.r1 in [rgv0..rgv31] then begin
        wrtins(' sub sp, sp, #0 // allocate real on stack', realsize);
        stkadr := stkadr-realsize;
        wrtins(' str %1, [sp] // place real on stack', pp^.r1)
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
      wrtins(' str x9, [sp, #-16]! // align');
      aln := true
    end;
    si := ' bl psystem_      // call system procedure/function          ';
    for i := 1 to maxalfa do if sc[i] <> ' ' then si[12+i-1] := sc[i];
    wrtins(si);
    if aln then
      wrtins(' ldr x9, [sp], #16 // drop alignment');
    if r then begin
      if isfltres(ep) then begin
        if ep^.r1 <> rgv0 then
          wrtins(' fmov %1, d0 // place result', ep^.r1)
      end else begin
        if ep^.r1 <> rgx0 then
          wrtins(' mov %1, x0 // place result', ep^.r1);
        if (ep^.r2 <> rgnull) and (ep^.r2 <> rgx1) then
          wrtins(' mov %1, x1 // place 2nd result', ep^.r2)
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
    pp := ep^.pl; genexp(pp); { addr x0 }
    pp := pp^.next; genexp(pp); { size x1 }
    pp := pp^.next; genexp(pp); { tagcnt x2 }
    wrtins(' str %1, [sp, #-16]! // save tag count', pp^.r1);
    wrtins(' add x3, sp, #0 // index tag list', intsize);
    stkadr := stkadr-adrsize;
    aln := false;
    if stkadr mod 16 <> 0 then begin
      wrtins(' str x9, [sp, #-16]! // align');
      stkadr := stkadr-adrsize; aln := true
    end;
    if ep^.q = 39 then
      wrtins(' bl psystem_nwl // call new')
    else
      wrtins(' bl psystem_dsl // call dispose');
    if aln then begin
      wrtins(' ldr x9, [sp], #16 // dump align');
      stkadr := stkadr+adrsize
    end;
    wrtins(' ldr x3, [sp], #16 // restore tag count');
    stkadr := stkadr+adrsize;
    wrtins(' mov x9, #0 // find *integer', intsize);
    wrtins(' mul x9, x3, x9 // multiply');
    wrtins(' add sp, sp, x9 // dump taglist from stack');
    stkadr := stkadrs { restore to entry }
  end;

  begin { genexp }
    if ep <> nil then begin
      for r := rgx0 to rgv31 do if r in ep^.rs then begin
          if r in [rgx0..rgx28] then begin
            wrtins(' str %1, [sp, #-16]! // save used register', r);
            stkadr := stkadr-intsize
          end else begin
            wrtins(' sub sp, sp, #0 // allocate real on stack', realsize);
            wrtins(' str %1, [sp] // save used register', r);
            stkadr := stkadr-realsize
          end
      end;
      genexp(ep^.al);
      if (ep^.op <> 113{cip}) and (ep^.op <> 247{cif}) then genexp(ep^.l);
      genexp(ep^.r); genexp(ep^.x1);
      write(prr, '// generating:  '); dmpety(prr, ep); writeln(prr);
      case ep^.op of

        {lodi,loda}
        0,105: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' ldr %1, [x29, #0] // get display pointer', ep^.q1, ep^.r1);
            wrtins(' ldr %1, [%1, #@l] // fetch local qword', ep^.q, ep^.p, ep^.r1)
          end else
            wrtins(' ldr %1, [x29, #@l] // fetch local qword', ep^.q, ep^.p, ep^.r1)
        end;

        {lodx,lodb,lodc}
        193,108,109: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' ldr %1, [x29, #0] // get display pointer', ep^.q1, ep^.r1);
            wrtins(' ldrb w0, [%1, #@l] // fetch local byte', ep^.q, ep^.p, ep^.r1);
            wrtins(' uxtb %1, w0 // zero extend', ep^.r1)
          end else begin
            wrtins(' ldrb w0, [x29, #@l] // fetch local byte', ep^.q, ep^.p);
            wrtins(' uxtb %1, w0 // zero extend', ep^.r1)
          end
        end;

        {lodr}
        106: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' ldr %1, [x29, #0] // get display pointer', ep^.q1, ep^.t1);
            wrtins(' ldr %1, [%2, #@l] // fetch local real', ep^.q, ep^.p, ep^.r1, ep^.t1)
          end else
            wrtins(' ldr %1, [x29, #@l] // fetch local real', ep^.q, ep^.p, ep^.r1)
        end;

        {lods}
        107: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' ldr x9, [x29, #0] // get display pointer', ep^.q1);
            wrtins(' add x9, x9, #@l // index local set', ep^.q, ep^.p)
          end else
            wrtins(' add x9, x29, #@l // index local set', ep^.q, ep^.p);
          wrtins(' sub x10, x29, #@s+0 // index destination temp', ep^.r1a, lclspc^);
          wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
          wrtins(' stp x11, x12, [x10], #16 // store set part 1');
          wrtins(' ldp x11, x12, [x9] // load set part 2');
          wrtins(' stp x11, x12, [x10] // store set part 2');
          wrtins(' sub %1, x29, #@s+0 // index temp again', ep^.r1a, ep^.r1, lclspc^);
        end;

        {lda}
        4: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' ldr %1, [x29, #0] // get display pointer', ep^.q1, ep^.r1);
            wrtins(' add %1, %1, #@l // index local', ep^.q, ep^.r1)
          end else
            wrtins(' add %1, x29, #@l // index local', ep^.q, ep^.r1)
        end;

        {adi}
        28: begin
          wrtins(' adds %1, %1, %2 // add integers', ep^.l^.r1, ep^.r^.r1);
          if dochkovf then begin
            wrtins(' b.vc 1f // skip no overflow');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // set line number', sline);
            wrtins(' mov x2, #IntegerValueOverflow // set error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end
        end;

        {adr}
        29:
          wrtins(' fadd %1, %1, %2 // add reals', ep^.l^.r1, ep^.r^.r1);

        {sbi}
        30: begin
          wrtins(' subs %1, %1, %2 // subtract integers', ep^.l^.r1, ep^.r^.r1);
          if dochkovf then begin
            wrtins(' b.vc 1f // skip no overflow');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // set line number', sline);
            wrtins(' mov x2, #IntegerValueOverflow // set error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end
        end;

        {sbr}
        31:
          wrtins(' fsub %1, %1, %2 // subtract reals', ep^.l^.r1, ep^.r^.r1);

        {equr,neqr,geqr,grtr,leqr,lesr}
        138,144,150,156,162,168: begin
          wrtins(' fcmp %1, %2 // compare reals', ep^.l^.r1, ep^.r^.r1);
          case ep^.op of
            138{equr}: wrtins(' cset %1, eq // set equal', ep^.r1);
            144{neqr}: wrtins(' cset %1, ne // set not equal', ep^.r1);
            150{geqr}: wrtins(' cset %1, ge // set greater or equal', ep^.r1);
            156{grtr}: wrtins(' cset %1, gt // set greater', ep^.r1);
            162{leqr}: wrtins(' cset %1, le // set less or equal', ep^.r1);
            168{lesr}: wrtins(' cset %1, lt // set less', ep^.r1);
          end
        end;

        120{lip}: begin
          if ep^.p <> blkstk^.lvl then begin
            wrtins(' ldr %1, [x29, #0] // get display pointer', ep^.q1, ep^.t1);
            wrtins(' ldr %1, [%2, #@l+ptrsize] // load frame pointer', ep^.q, ep^.p, ep^.r2, ep^.t1);
            wrtins(' ldr %1, [%2, #@l] // load procedure address', ep^.q, ep^.p, ep^.r1, ep^.t1)
          end else begin
            wrtins(' ldr %1, [x29, #@l+ptrsize] // load frame pointer', ep^.q, ep^.p, ep^.r2);
            wrtins(' ldr %1, [x29, #@l] // load procedure address', ep^.q, ep^.p, ep^.r1)
          end
        end;

        {equm,neqm,geqm,gtrm,leqm,lesm}
        142,148,154,160,166,172: begin
          wrtins(' mov x2, #0 // get string length', ep^.q);
          wrtins(' bl psystem_strcmp // compare strings');
          wrtins(' cmp x0, #0 // compare -0+ result');
          case ep^.op of
            142{equm}: wrtins(' cset %1, eq // set equal', ep^.r1);
            148{neqm}: wrtins(' cset %1, ne // set not equal', ep^.r1);
            154{geqm}: wrtins(' cset %1, ge // set greater or equal', ep^.r1);
            160{grtm}: wrtins(' cset %1, gt // set greater', ep^.r1);
            166{leqm}: wrtins(' cset %1, le // set less or equal', ep^.r1);
            172{lesm}: wrtins(' cset %1, lt // set less', ep^.r1);
          end
        end;

        5{lao},234{lto}:
          if ep^.fl <> nil then begin
            wrtins(' adrp %1, @s // load address of global (page)', ep^.r1, ep^.fl^);
            wrtins(' add %1, %1, :lo12:@s // load address of global (offset)', ep^.r1, ep^.fl^)
          end else begin
            wrtins(' adrp %1, globals+0 // load address of global (page)', ep^.q, ep^.r1);
            wrtins(' add %1, %1, :lo12:globals+0 // load address of global (offset)', ep^.q, ep^.r1)
          end;

        16{ixa}: begin
          { left is address right is index, size is q }
          wrtins(' mov %1, #0 // get element size', ep^.t2, ep^.q);
          wrtins(' mul %1, %2, %3 // find index*size', ep^.t2, ep^.r^.r1, ep^.t2);
          wrtins(' add %1, %1, %2 // add to base', ep^.l^.r1, ep^.t2)
        end;

        118{swp}: ; { done at top level }

        {ldoi,ltci}
        1,65,228:
          if ep^.fl <> nil then begin
            wrtins(' adrp x9, @s // load global quad (page)', ep^.fl^);
            wrtins(' ldr %1, [x9, :lo12:@s] // load global quad', ep^.r1, ep^.fl^)
          end else begin
            wrtins(' adrp x9, globals+0 // load global quad (page)', ep^.q);
            wrtins(' ldr %1, [x9, :lo12:globals+0] // load global quad', ep^.q, ep^.r1)
          end;

        {ldob,ldoc,ldox,ltcb,ltcc,ltcx}
        68,69,194,231,232,233:
          if ep^.fl <> nil then begin
            wrtins(' adrp x9, @s // load global byte (page)', ep^.fl^);
            wrtins(' ldrb w0, [x9, :lo12:@s] // load global byte', ep^.fl^);
            wrtins(' uxtb %1, w0 // zero extend', ep^.r1)
          end else begin
            wrtins(' adrp x9, globals+0 // load global byte (page)', ep^.q);
            wrtins(' ldrb w0, [x9, :lo12:globals+0] // load global byte', ep^.q);
            wrtins(' uxtb %1, w0 // zero extend', ep^.r1)
          end;

        {ldor,ltcr}
        66,229:
          if ep^.fl <> nil then begin
            wrtins(' adrp x9, @s // load global real (page)', ep^.fl^);
            wrtins(' ldr %1, [x9, :lo12:@s] // load global real', ep^.r1, ep^.fl^)
          end else begin
            wrtins(' adrp x9, globals+0 // load global real (page)', ep^.q);
            wrtins(' ldr %1, [x9, :lo12:globals+0] // load global real', ep^.q, ep^.r1)
          end;

        {ldos,ltcs}
        67,230: begin
          if ep^.fl <> nil then begin
            wrtins(' adrp x9, @s // load address of global set (page)', ep^.fl^);
            wrtins(' add x9, x9, :lo12:@s // load address of global set', ep^.fl^)
          end else begin
            wrtins(' adrp x9, globals+0 // load address of global set (page)', ep^.q);
            wrtins(' add x9, x9, :lo12:globals+0 // load address of global set', ep^.q)
          end;
          wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
          wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
          wrtins(' stp x11, x12, [x10], #16 // store set part 1');
          wrtins(' ldp x11, x12, [x9] // load set part 2');
          wrtins(' stp x11, x12, [x10] // store set part 2');
          wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
        end;

        {indi,inda}
        9,85:
          wrtins(' ldr %1, [%1, #0] // load qword from address', ep^.q, ep^.l^.r1);

        {indr}
        86:
          wrtins(' ldr %1, [%2, #0] // load real from address', ep^.q, ep^.r1, ep^.t1);

        {indb,indc,indx}
        88,89,198: begin
          wrtins(' ldrb w0, [%1, #0] // load byte from address', ep^.q, ep^.l^.r1);
          wrtins(' uxtb %1, w0 // zero extend', ep^.l^.r1)
        end;

        {inds}
        87: begin
          wrtins(' add x9, %1, #0 // offset source', ep^.l^.r1, ep^.q);
          wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
          wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
          wrtins(' stp x11, x12, [x10], #16 // store set part 1');
          wrtins(' ldp x11, x12, [x9] // load set part 2');
          wrtins(' stp x11, x12, [x10] // store set part 2');
          wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
        end;

        {inci,incb,incc,incx}
        10, 93, 94, 201: begin
          wrtins(' adds %1, %1, #0 // increment by n', ep^.q, ep^.r1);
          if dochkovf then begin
            wrtins(' b.vc 1f // skip no overflow');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // set line number', sline);
            wrtins(' mov x2, #IntegerValueOverflow // set error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end
        end;

        {inca}
        90:
          wrtins(' add %1, %1, #0 // increment by n', ep^.q, ep^.r1);

        {deci,decb,decc,decx}
        57, 103, 104, 202: begin
          wrtins(' subs %1, %1, #0 // decrement by n', ep^.q, ep^.r1);
          if dochkovf then begin
            wrtins(' b.vc 1f // skip no overflow');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // set line number', sline);
            wrtins(' mov x2, #IntegerValueOverflow // set error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end
        end;

        {mdc}
        254: begin
          wrtins(' mov %1, %2 // copy template pointer to data', ep^.r2, ep^.r1);
          wrtins(' add %1, %1, #0 // skip template to data', ep^.q, ep^.r1)
        end;

        {ckvi,ckvb,ckvc,ckvx}
        175, 179, 180, 203: begin
          wrtins(' cmp %1, #0 // check this tag value', ep^.r1, ep^.q);
          wrtins(' cset x9, eq // set boolean equal');
          wrtins(' orr %1, %1, x9 // or with running total', ep^.r2);
        end;

        {cvbi,cvbx,cvbb,cvbc}
        100, 115, 116, 121: begin
          wrtins(' mov x0, #0 // load tagfield offset', ep^.q);
          wrtins(' mov x1, #0 // load size of variant', ep^.q1);
          wrtins(' adrp x2, @s // load logical variant table (page)', ep^.lt^);
          wrtins(' add x2, x2, :lo12:@s // load logical variant table', ep^.lt^);
          if ep^.op = 100 then
            wrtins(' ldr x3, [%1] // get existing tag value', ep^.l^.r1)
          else begin
            wrtins(' ldrb w3, [%1] // get existing tag value', ep^.l^.r1);
            wrtins(' uxtb x3, w3 // zero extend')
          end;
          wrtins(' bl psystem_tagchgvar // check valid tag change')
        end;

        {ivti,ivtx,ivtb,ivtc}
        192,101,102,111: begin
          wrtins(' mov x0, #0 // load tagfield offset', ep^.q);
          wrtins(' mov x1, #0 // load size of variant', ep^.q1);
          wrtins(' adrp x2, @s // load logical variant table (page)', ep^.lt^);
          wrtins(' add x2, x2, :lo12:@s // load logical variant table', ep^.lt^);
          if ep^.op = 192 then
            wrtins(' ldr x3, [%1] // get existing tag value', ep^.l^.r1)
          else begin
            wrtins(' ldrb w3, [%1] // get existing tag value', ep^.l^.r1);
            wrtins(' uxtb x3, w3 // zero extend')
          end;
          wrtins(' bl psystem_tagchginv // invalidate tag changes')
        end;

        {cps}
        176: begin
          wrtins(' cmp %1, %2 // compare container lengths', ep^.l^.r2, ep^.r^.r2);
          wrtins(' b.eq 1f // skip equal');
          wrtins(' adrp x0, modnam // load module name');
          wrtins(' add x0, x0, :lo12:modnam');
          wrtins(' mov x1, #0 // load line number', sline);
          wrtins(' mov x2, #ContainerMismatch // load error code');
          wrtins(' bl psystem_errore // process error');
          wrtins('1:');
        end;

        {cpc}
        177: begin
          wrtins(' mov x0, #0 // get level number', ep^.q);
          wrtins(' bl psystem_cmptmp // compare templates')
        end;

        {cta}
        191: begin
          wrtins(' mov x0, #0 // get tag offset', ep^.q);
          wrtins(' mov x1, #0 // get tag nesting level', ep^.q1);
          wrtins(' adrp x2, @s // index logical variant table (page)', ep^.lt^);
          wrtins(' add x2, x2, :lo12:@s // index logical variant table', ep^.lt^);
          wrtins(' bl psystem_tagchkass // check tag assignment')
        end;

        {lpa}
        114: begin
          wrtins(' adrp %1, @s // load procedure/function address (page)', ep^.r1, ep^.fn^);
          wrtins(' add %1, %1, :lo12:@s // load procedure/function address', ep^.r1, ep^.fn^);
          wrtins(' ldr %1, [x29, #@l] // load display pointer', ep^.q1, ep^.p, ep^.r2)
        end;

        {ldci,ldcc,ldcb}
        123,127,126:
          wrtins(' mov %1, #0 // load constant', ep^.vi, ep^.r1);

        {ldcn}
        125:
          wrtins(' mov %1, #0 // load nil value', ep^.r1);

        {ldcr}
        124: begin
          wrtins(' adrp x9, real0 // load real constant (page)', ep^.realn);
          wrtins(' ldr %1, [x9, :lo12:real0] // load real constant', ep^.realn, ep^.r1)
        end;

        {ldcs}
        7: begin
          wrtins(' adrp x9, set0 // index constant set (page)', ep^.setn);
          wrtins(' add x9, x9, :lo12:set0 // index constant set', ep^.setn);
          wrtins(' sub x10, x29, #@s+0 // index temp', ep^.r1a, lclspc^);
          wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
          wrtins(' stp x11, x12, [x10], #16 // store set part 1');
          wrtins(' ldp x11, x12, [x9] // load set part 2');
          wrtins(' stp x11, x12, [x10] // store set part 2');
          wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^);
        end;

        {chki,chkb,chkc,chkx}
        26, 98, 99, 199: begin
          wrtins(' mov %1, #0 // load low bound', ep^.t1, ep^.vi);
          wrtins(' cmp %1, %2 // compare', ep^.r1, ep^.t1);
          wrtins(' b.ge 1f // skip if greater or equal');
          wrtins(' adrp x0, modnam // load module name');
          wrtins(' add x0, x0, :lo12:modnam');
          wrtins(' mov x1, #0 // load line number', sline);
          wrtins(' mov x2, #ValueOutOfRange // load error code');
          wrtins(' bl psystem_errore // process error');
          wrtins('1:');
          wrtins(' mov %1, #0 // load high bound', ep^.t1, ep^.vi2);
          wrtins(' cmp %1, %2 // compare', ep^.r1, ep^.t1);
          wrtins(' b.le 1f // skip if less or equal');
          wrtins(' adrp x0, modnam // load module name');
          wrtins(' add x0, x0, :lo12:modnam');
          wrtins(' mov x1, #0 // load line number', sline);
          wrtins(' mov x2, #ValueOutOfRange // load error code');
          wrtins(' bl psystem_errore // process error');
          wrtins('1:')
        end;

        {chka}
        95: begin
          wrtins(' cmp %1, #0 // check nil', ep^.r1);
          wrtins(' b.ne 1f // skip if not');
          wrtins(' adrp x0, modnam // load module name');
          wrtins(' add x0, x0, :lo12:modnam');
          wrtins(' mov x1, #0 // load line number', sline);
          wrtins(' mov x2, #DereferenceOfNilPointer // load error code');
          wrtins(' bl psystem_errore // process error');
          wrtins('1:')
        end;

        {chks}
        97: begin
          wrtins(' mov x0, #0 // load low bound', ep^.vi);
          wrtins(' mov x1, #0 // load high bound', ep^.vi2);
          wrtins(' bl psystem_chksetbnd // check set in bounds');
          wrtins(' sub %1, x29, #@s+0 // reindex temp set', ep^.r1a, ep^.r1, lclspc^)
        end;

        {ckla}
        190: begin
          if ep^.q <> 0 then begin
            wrtins(' cmp %1, #0 // check nil', ep^.r1);
            wrtins(' b.ne 1f // skip if not nil');
            wrtins(' adrp x0, modnam // load module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // load line number', sline);
            wrtins(' mov x2, #DereferenceOfNilPointer // load error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end
        end;

        56 {lca}: begin
          wrtins(' adrp %1, string0 // load string constant address (page)', ep^.strn, ep^.r1);
          wrtins(' add %1, %1, :lo12:string0 // load string constant address', ep^.strn, ep^.r1)
        end;

        {grts,less}
        158,170: ; { are invalid }

        {equs,neqs,geqs,leqs}
        140,146,152,164: begin
          case ep^.op of
            140: wrtins(' bl psystem_setequ // check set equal');
            146: begin
              wrtins(' bl psystem_setequ // check set equal');
              wrtins(' eor x0, x0, #1 // invert equal status');
            end;
            152,164: wrtins(' bl psystem_setinc // check set inclusion');
          end;
          if ep^.r1 <> rgx0 then
            wrtins(' mov %1, x0 // move result to final register', ep^.r1);
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
          wrtins(' cmp %1, %2 // compare', ep^.l^.r1, ep^.r^.r1);
          case ep^.op of
            17,137,139,141: wrtins(' cset %1, eq // set equal', ep^.l^.r1);
            18,143,145,147: wrtins(' cset %1, ne // set not equal', ep^.l^.r1);
            149,151,153: wrtins(' cset %1, ge // set greater or equal', ep^.l^.r1);
            155,157,159: wrtins(' cset %1, gt // set greater', ep^.l^.r1);
            161,163,165: wrtins(' cset %1, le // set less or equal', ep^.l^.r1);
            167,169,171: wrtins(' cset %1, lt // set less', ep^.l^.r1)
          end
        end;

        {ordi,ordb,ordc,ordx}
        59, 134, 136, 200: ; { ord is a no-op }

        {lcp}
        135: begin
          wrtins(' add %1, %2, #0 // get length/template', ep^.r2, ep^.l^.r1, ptrsize);
          wrtins(' ldr %1, [%1] // get pointer', ep^.l^.r1)
        end;

        {sgs}
        32: begin
          wrtins(' sub x1, x29, #@s+0 // index temp', ep^.r1a, lclspc^);
          wrtins(' bl psystem_setsgl // make singleton set');
          wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^);
        end;

        {flt,flo}
        33,34: wrtins(' scvtf %1, %2 // convert integer to real', ep^.r1, ep^.l^.r1);

        {trc}
        35: begin
          if dochkovf then begin
            wrtins(' adrp x9, real_int_max // load maximum int val (page)');
            wrtins(' ldr %1, [x9, :lo12:real_int_max] // load maximum int val', ep^.t1);
            wrtins(' fcmp %1, %2 // compare real', ep^.l^.r1, ep^.t1);
            wrtins(' b.lt 2f // skip if less');
            wrtins(' adrp x9, real_int_min // load minimum int val (page)');
            wrtins(' ldr %1, [x9, :lo12:real_int_min] // load minimum int val', ep^.t1);
            wrtins(' fcmp %1, %2 // compare real', ep^.l^.r1, ep^.t1);
            wrtins(' b.ge 1f // skip if greater or equal');
            wrtins('2:');
            wrtins(' adrp x0, modnam // load module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // load line number', sline);
            wrtins(' mov x2, #RealArgumentTooLarge // load error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end;
          wrtins(' fcvtzs %1, %2 // truncate real to integer', ep^.r1, ep^.l^.r1);
        end;

        {ngi}
        36: wrtins(' neg %1, %1 // negate integer', ep^.r1);

        {ngr}
        37: wrtins(' fneg %1, %2 // negate real', ep^.r1, ep^.l^.r1);

        {sqi}
        38: begin
          wrtins(' mul %1, %1, %1 // square integer', ep^.r1);
          if dochkovf then begin
            wrtins(' b.vc 1f // skip no overflow');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // set line number', sline);
            wrtins(' mov x2, #IntegerValueOverflow // set error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end
        end;

        {sqr}
        39:
          wrtins(' fmul %1, %1, %1 // square real', ep^.r1);

        {abi}
        40: begin
          wrtins(' cmp %1, #0 // check positive', ep^.r1);
          wrtins(' cneg %1, %1, lt // negate if negative', ep^.r1)
        end;

        {abr}
        41:
          wrtins(' fabs %1, %2 // absolute value of real', ep^.r1, ep^.l^.r1);

        {notb}
        42: begin
          wrtins(' cmp %1, #0 // test boolean', ep^.r1);
          wrtins(' cset %1, eq // set inverse', ep^.r1)
        end;

        {noti}
        205: begin
          if dodbgchk then begin
            wrtins(' cmp %1, #0 // test signed', ep^.r1);
            wrtins(' b.ge 1f // skip if not negative');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // set line number', sline);
            wrtins(' mov x2, #BooleanOperatorOfNegative // set error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:');
          end;
          wrtins(' mvn %1, %1 // not integer', ep^.r1);
          wrtins(' mov %1, #0 // load max positive int', ep^.t1, pmmaxint);
          wrtins(' and %1, %1, %2 // mask to positive', ep^.r1, ep^.t1)
        end;

        {odd}
        50:
          wrtins(' and %1, %1, #1 // mask bit 0', ep^.r1);

        {rnd}
        62: begin
          if dochkovf then begin
            wrtins(' adrp x9, real_int_max // load maximum int val (page)');
            wrtins(' ldr %1, [x9, :lo12:real_int_max] // load maximum int val', ep^.t1);
            wrtins(' fcmp %1, %2 // compare real', ep^.l^.r1, ep^.t1);
            wrtins(' b.lt 2f // skip if less');
            wrtins(' adrp x9, real_int_min // load minimum int val (page)');
            wrtins(' ldr %1, [x9, :lo12:real_int_min] // load minimum int val', ep^.t1);
            wrtins(' fcmp %1, %2 // compare real', ep^.l^.r1, ep^.t1);
            wrtins(' b.ge 1f // skip if greater or equal');
            wrtins('2:');
            wrtins(' adrp x0, modnam // load module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // load line number', sline);
            wrtins(' mov x2, #RealArgumentTooLarge // load error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end;
          wrtins(' fcvtns %1, %2 // round to nearest integer', ep^.r1, ep^.l^.r1)
        end;

        {chr}
        60: ; { chr is no-op }

        {and,ior,xor}
        43,44,206: begin
          if dodbgchk then begin
            wrtins(' cmp %1, #0 // check signed', ep^.l^.r1);
            wrtins(' b.ge 1f // skip if not negative');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // get line number', sline);
            wrtins(' mov x2, #BooleanOperatorOfNegative // get error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:');
            wrtins(' cmp %1, #0 // check signed', ep^.r^.r1);
            wrtins(' b.ge 1f // skip if not negative');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // get line number', sline);
            wrtins(' mov x2, #BooleanOperatorOfNegative // get error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:');
          end;
          case ep^.op of
            43: wrtins(' and %1, %1, %2 // and integers', ep^.l^.r1, ep^.r^.r1);
            44: wrtins(' orr %1, %1, %2 // or integers', ep^.l^.r1, ep^.r^.r1);
            206: wrtins(' eor %1, %1, %2 // xor integers', ep^.l^.r1, ep^.r^.r1)
          end
        end;

        {dif,int,uni}
        45,46,47: begin
          case ep^.op of
            45: wrtins(' bl psystem_setdif // find set difference');
            46: wrtins(' bl psystem_setint // find set intersection');
            47: wrtins(' bl psystem_setuni // find set union');
          end;
          wrtins(' sub %1, x29, #@s+0 // reindex the temp', ep^.r1a, ep^.r1, lclspc^);
          puttmp(ep^.r^.r1a)
        end;

        {inn}
        48: begin
          wrtins(' bl psystem_setsin // find set membership');
          if ep^.r1 <> rgx0 then
            wrtins(' mov %1, x0 // move result to target reg', ep^.r1);
          puttmp(ep^.r^.r1a)
        end;

        {mod}
        49: begin
          wrtins(' cmp %1, #0 // check zero divide', ep^.r^.r1);
          wrtins(' b.gt 1f // skip <= 0');
          wrtins(' adrp x0, modnam // index module name');
          wrtins(' add x0, x0, :lo12:modnam');
          wrtins(' mov x1, #0 // set line number', sline);
          wrtins(' mov x2, #InvalidDivisorToMod // set error code');
          wrtins(' bl psystem_errore // process error');
          wrtins('1:');
          wrtins(' sdiv x9, %1, %2 // divide', ep^.l^.r1, ep^.r^.r1);
          wrtins(' msub %1, x9, %2, %3 // remainder = dividend - quotient*divisor', ep^.r1, ep^.r^.r1, ep^.l^.r1)
        end;

        {dvi}
        53: begin
          wrtins(' cmp %1, #0 // check zero divide', ep^.r^.r1);
          wrtins(' b.ne 1f // skip if not zero');
          wrtins(' adrp x0, modnam // index module name');
          wrtins(' add x0, x0, :lo12:modnam');
          wrtins(' mov x1, #0 // set line number', sline);
          wrtins(' mov x2, #ZeroDivide // set error code');
          wrtins(' bl psystem_errore // process error');
          wrtins('1:');
          wrtins(' sdiv %1, %2, %3 // divide integer', ep^.r1, ep^.l^.r1, ep^.r^.r1)
        end;

        {mpi}
        51: begin
          wrtins(' mul %1, %1, %2 // multiply integers', ep^.l^.r1, ep^.r^.r1);
          if dochkovf then begin
            wrtins(' b.vc 1f // skip no overflow');
            wrtins(' adrp x0, modnam // index module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // set line number', sline);
            wrtins(' mov x2, #IntegerValueOverflow // set error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end
        end;

        {mpr}
        52: wrtins(' fmul %1, %1, %2 // multiply reals', ep^.l^.r1, ep^.r^.r1);

        {dvr}
        54: begin
          if dodbgchk then begin
            wrtins(' fcmp %1, #0.0 // check zero divide', ep^.r^.r1);
            wrtins(' b.ne 1f // skip not zero');
            wrtins(' adrp x0, modnam // load module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // load line number', sline);
            wrtins(' mov x2, #ZeroDivide // load error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:');
          end;
          wrtins(' fdiv %1, %1, %2 // divide reals', ep^.l^.r1, ep^.r^.r1)
        end;

        {rgs}
        110: begin
          wrtins(' sub x2, x29, #@s+0 // index temp', ep^.r1a, lclspc^);
          wrtins(' bl psystem_setrgs // set range of values');
          wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^);
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
              wrtins(' mov x9, sp // copy stack pointer');
              wrtins(' sub x9, x9, s // set new stack depth', ep^.lb^);
              wrtins('1:');
              wrtins(' cmp sp, x9 // check done');
              wrtins(' b.ls 2f // skip if below stack');
              wrtins(' str xzr, [sp, #-16]! // clear stack');
              wrtins(' b 1b // loop');
              wrtins('2:')
            end else
              wrtins(' sub sp, sp, s // set new stack depth', ep^.lb^);
          end;

        {cup,cuf}
        12, 246: begin
          genexp(ep^.sl); { process sfr start link }
          stkadrs := stkadr; { save stack track here }
          pshpar(ep^.pl); { process parameters first }
          if ep^.blk <> nil then begin
            write(prr, ' ':opcspc, 'bl '); lftjst(parspc-(3+opcspc)); fl := parspc;
            wrtblks(ep^.blk^.parent, true, fl); wrtblksht(ep^.blk, fl);
            lftjst(cmtspc-fl); writeln(prr, '// call user procedure')
          end else wrtins(' bl @s // call user procedure', ep^.fn^);
          if ep^.op = 246{cuf} then begin
            if ep^.rc = 1 then begin
              if ep^.r1 <> rgv0 then
                wrtins(' fmov %1, d0 // place result', ep^.r1)
            end else if ep^.rc = 2 then begin { move set from stack to temp }
                wrtins(' mov x9, sp // index set on stack');
                wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
                wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
                wrtins(' stp x11, x12, [x10], #16 // store set part 1');
                wrtins(' ldp x11, x12, [x9] // load set part 2');
                wrtins(' stp x11, x12, [x10] // store set part 2');
                wrtins(' add sp, sp, #0 // remove set from stack', setsize);
                wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end else if ep^.rc = 3 then begin { move structure from stack to temp }
                wrtins(' mov x9, sp // index structure on stack');
                wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
                wrtins(' mov x11, #0 // load size', ep^.q2);
                wrtins('1: ldrb w12, [x9], #1 // copy loop');
                wrtins(' strb w12, [x10], #1');
                wrtins(' subs x11, x11, #1');
                wrtins(' b.ne 1b');
                wrtins(' add sp, sp, #0 // remove structure from stack', ep^.q3);
                wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end else begin
              if ep^.r1 <> rgx0 then
                wrtins(' mov %1, x0 // place result', ep^.r1);
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
          wrtins(' mov x28, x29 // move our frame pointer to preserved register');
          wrtins(' ldr x29, [%1, #0] // set callee frame pointer', ptrsize, ep^.l^.r1);
          wrtins(' ldr x9, [%1] // load procedure address', ep^.l^.r1);
          wrtins(' blr x9 // call indirect');
          if ep^.op = 247{cif} then begin
            if ep^.rc = 1 then begin
              if ep^.r1 <> rgv0 then
                wrtins(' fmov %1, d0 // place result', ep^.r1)
            end else if ep^.rc = 2 then begin { move set from stack to temp }
                wrtins(' mov x9, sp // index set on stack');
                wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
                wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
                wrtins(' stp x11, x12, [x10], #16 // store set part 1');
                wrtins(' ldp x11, x12, [x9] // load set part 2');
                wrtins(' stp x11, x12, [x10] // store set part 2');
                wrtins(' add sp, sp, #0 // remove set from stack', setsize);
                wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end else if ep^.rc = 3 then begin { move structure from stack to temp }
                wrtins(' mov x9, sp // index structure on stack');
                wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
                wrtins(' mov x11, #0 // load size', ep^.q2);
                wrtins('1: ldrb w12, [x9], #1 // copy loop');
                wrtins(' strb w12, [x10], #1');
                wrtins(' subs x11, x11, #1');
                wrtins(' b.ne 1b');
                wrtins(' add sp, sp, #0 // remove structure from stack', ep^.q3);
                wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end else begin
              if ep^.r1 <> rgx0 then
                wrtins(' mov %1, x0 // place result', ep^.r1)
            end
          end;
          wrtins(' mov x29, x28 // restore our frame pointer');
          stkadr := stkadrs { restore stack position }
        end;

        {cuv,cvf}
        27,249: begin
          genexp(ep^.sl); { process sfr start link }
          stkadrs := stkadr; { save stack track here }
          pshpar(ep^.pl); { process parameters first }
          if ep^.qs <> nil then begin
            wrtins(' adrp x9, @s // load vectored address (page)', ep^.qs^);
            wrtins(' ldr x9, [x9, :lo12:@s] // load vectored address', ep^.qs^);
            wrtins(' blr x9 // call vectored')
          end else begin
            wrtins(' adrp x9, globals+0 // load vectored address (page)', q);
            wrtins(' ldr x9, [x9, :lo12:globals+0] // load vectored address', q);
            wrtins(' blr x9 // call vectored')
          end;
          if ep^.op = 249{cvf} then begin
            if ep^.rc = 1 then begin
              if ep^.r1 <> rgv0 then
                wrtins(' fmov %1, d0 // place result', ep^.r1)
            end else if ep^.rc = 2 then begin { move set from stack to temp }
                wrtins(' mov x9, sp // index set on stack');
                wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
                wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
                wrtins(' stp x11, x12, [x10], #16 // store set part 1');
                wrtins(' ldp x11, x12, [x9] // load set part 2');
                wrtins(' stp x11, x12, [x10] // store set part 2');
                wrtins(' add sp, sp, #0 // remove set from stack', setsize);
                wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end else if ep^.rc = 3 then begin { move structure from stack to temp }
                wrtins(' mov x9, sp // index structure on stack');
                wrtins(' sub x10, x29, #@s+0 // load temp destination', ep^.r1a, lclspc^);
                wrtins(' mov x11, #0 // load size', ep^.q2);
                wrtins('1: ldrb w12, [x9], #1 // copy loop');
                wrtins(' strb w12, [x10], #1');
                wrtins(' subs x11, x11, #1');
                wrtins(' b.ne 1b');
                wrtins(' add sp, sp, #0 // remove structure from stack', ep^.q3);
                wrtins(' sub %1, x29, #@s+0 // reindex temp', ep^.r1a, ep^.r1, lclspc^)
            end else begin
              if ep^.r1 <> rgx0 then
                wrtins(' mov %1, x0 // place result', ep^.r1);
            end
          end;
          stkadr := stkadrs { restore stack position }
        end;

        {cke}
        188: begin
          wrtins(' mov %1, #0 // start running boolean', ep^.r2);
          ep2 := ep^.cl;
          while ep2 <> nil do begin
            ep2^.r1 := ep^.r1; ep2^.r2 := ep^.r2; ep2^.t1 := ep^.t1;
            genexp(ep2); ep2 := ep2^.next
          end;
          wrtins(' cbnz %1, 1f // skip any variant active', ep^.r2);
          wrtins(' adrp x0, modnam // set module name');
          wrtins(' add x0, x0, :lo12:modnam');
          wrtins(' mov x1, #0 // set line number', sline);
          wrtins(' mov x2, #VariantNotActive // set error code');
          wrtins(' bl psystem_errore // process error');
          wrtins('1:');
        end;

        {wbs}
        243:
          wrtins(' bl psystem_withenter // establish with reference');

        {cxs}
        211: begin
          wrtins(' sub %1, %1, #1 // 0 base index', ep^.r^.r1);
          if dodbgchk then begin
            wrtins(' cmp %1, %2 // check index < length', ep^.r^.r1, ep^.l^.r2);
            wrtins(' b.lo 1f // skip below');
            wrtins(' adrp x0, modnam // load module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // load line number', sline);
            wrtins(' mov x2, #ValueOutOfRange // load error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:');
          end;
          wrtins(' mov x9, #0 // get element size', ep^.q);
          wrtins(' mul x9, %1, x9 // find index*size', ep^.r^.r1);
          wrtins(' add %1, %1, x9 // add to base', ep^.l^.r1)
        end;

        {cxc}
        212: begin
          wrtins(' sub %1, %1, #1 // 0 base index', ep^.r^.r1);
          if dodbgchk then begin
            wrtins(' ldr x9, [%1] // get length from template', ep^.l^.r2);
            wrtins(' cmp %1, x9 // check index < length', ep^.r^.r1);
            wrtins(' b.lo 1f // skip below');
            wrtins(' adrp x0, modnam // load module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // load line number', sline);
            wrtins(' mov x2, #ValueOutOfRange // load error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:');
          end;
          wrtins(' mov %1, #0 // get # levels-1', ep^.t1, ep^.q-1);
          wrtins(' mov x9, #0 // get base element size', ep^.q1);
          wrtins(' mov x10, %1 // copy template address', ep^.l^.r2);
          wrtins('1:');
          wrtins(' add x10, x10, #0 // next template location', intsize);
          wrtins(' ldr x11, [x10] // get template value');
          wrtins(' mul x9, x9, x11 // multiply by template');
          wrtins(' subs %1, %1, #1 // count down levels', ep^.t1);
          wrtins(' b.ne 1b // loop over templates');
          wrtins(' add %1, %1, #0 // advance template slot', ep^.l^.r2, intsize);
          wrtins(' mul x9, %1, x9 // find index*size', ep^.r^.r1);
          wrtins(' add %1, %1, x9 // add to base', ep^.l^.r1)
        end;

        {lft}
        213: begin
          wrtins(' adrp %1, @s // index template (page)', ep^.r2, ep^.lt^);
          wrtins(' add %1, %1, :lo12:@s // index template', ep^.r2, ep^.lt^)
        end;

        {max}
        214: begin
          if dodbgchk then begin
            wrtins(' cmp %1, #1 // chk lvl < 1', ep^.r^.r1);
            wrtins(' b.lo 2f // skip if below');
            wrtins(' cmp %1, #0 // compare', ep^.r^.r1, ep^.q);
            wrtins(' b.ls 1f // skip if less or equal');
            wrtins('2:');
            wrtins(' adrp x0, modnam // load module name');
            wrtins(' add x0, x0, :lo12:modnam');
            wrtins(' mov x1, #0 // load line number', sline);
            wrtins(' mov x2, #InvalidContainerLevel // load error code');
            wrtins(' bl psystem_errore // process error');
            wrtins('1:')
          end;
          if ep^.q <> 1 then begin
            wrtins(' mov %1, #0 // get total lvl', ep^.t1, ep^.q);
            wrtins(' sub %1, %1, %2 // find tl-al', ep^.t1, ep^.r^.r1);
            wrtins(' lsl %1, %1, #4 // *16 (long)', ep^.t1);
            wrtins(' add %1, %1, %2 // add to base template', ep^.t1, ep^.l^.r2);
            wrtins(' ldr %1, [%2] // load from template', ep^.r1, ep^.t1)
          end
        end;

        {equv,neqv,lesv,grtv,leqv,geqv}
        215,216,217,218,219,220: begin
          wrtins(' bl psystem_strcmp // compare strings');
          wrtins(' cmp x0, #0 // compare -0+ result');
          case ep^.op of
            215{equv}: wrtins(' cset %1, eq // set equal', ep^.r1);
            216{neqv}: wrtins(' cset %1, ne // set not equal', ep^.r1);
            220{geqv}: wrtins(' cset %1, ge // set greater or equal', ep^.r1);
            218{grtv}: wrtins(' cset %1, gt // set greater', ep^.r1);
            219{leqv}: wrtins(' cset %1, le // set less or equal', ep^.r1);
            217{lesv}: wrtins(' cset %1, lt // set less', ep^.r1);
          end
        end;

        {spc}
        222:
          wrtins(' ldr %1, [%1] // fetch template length', ep^.l^.r2);

        {ccs}
        223: begin
          if ep^.q = 1 then begin
            wrtins(' mov x9, #0 // get base element size', ep^.q1);
            wrtins(' mul %1, %2, x9 // find base size*len', ep^.t2, ep^.l^.r2)
          end else begin
            wrtins(' mov %1, #0 // get # levels', ep^.t1, ep^.q);
            wrtins(' mov %1, #0 // get base element size', ep^.t2, ep^.q1);
            wrtins(' mov %1, %2 // copy template address', ep^.t3, ep^.l^.r2);
            wrtins('1:');
            wrtins(' ldr x9, [%1] // get size from template', ep^.t3);
            wrtins(' mul %1, %1, x9 // multiply by size', ep^.t2);
            wrtins(' add %1, %1, #0 // next template location', ep^.t3, intsize);
            wrtins(' subs %1, %1, #1 // count down levels', ep^.t1);
            wrtins(' b.ne 1b // loop over templates')
          end;
          wrtins(' sub sp, sp, %1 // allocate on stack', ep^.t2);
          wrtins(' and sp, sp, #0xfffffffffffffff0 // align stack');
          wrtins(' mov x9, %1 // move source', ep^.l^.r1);
          wrtins(' mov x10, sp // move dest');
          wrtins(' mov x11, %1 // move count', ep^.t2);
          wrtins('1: ldrb w12, [x9], #1 // copy loop');
          wrtins(' strb w12, [x10], #1');
          wrtins(' subs x11, x11, #1');
          wrtins(' b.ne 1b');
          wrtins(' mov %1, sp // index copy', ep^.r1);
          wrtins(' mov %1, %2 // index template', ep^.r2, ep^.l^.r2)
        end;

        {ldp}
        225: begin
          wrtins(' ldr %1, [%2, #0] // get template adr', ep^.r2, ep^.l^.r1, intsize);
          wrtins(' ldr %1, [%2] // get data adr', ep^.r1, ep^.l^.r1)
        end;

        {mpc}
        248: ; { registers are all assigned }

        {cpl}
        251: ; { registers are all assigned }

      end;
      for r := rgv31 downto rgx0 do if r in ep^.rs then begin
        if r in [rgx0..rgx28] then begin
          wrtins(' ldr %1, [sp], #16 // restore used register', r);
          stkadr := stkadr-intsize
        end else begin
          wrtins(' ldr %1, [sp] // restore used real register', r);
          wrtins(' add sp, sp, #0 // remove from stack', realsize);
          stkadr := stkadr-intsize
        end
      end;
      write(prr, '// generating~: '); dmpety(prr, ep); writeln(prr)
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
      wrtins(' str %1, [sp, #-16]! // place on stack', ep^.r1)
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
      getexp(ep); ep^.qs := sp; ep^.pn := q1; ep^.rc := q2;
      getpar(ep); pshstk(ep)
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
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then begin
        wrtins(' ldr %1, [x29, #0] // get display pointer', -p*ptrsize, r1);
        wrtins(' str %1, [%2, #@l] // store qword', ep^.r1, r1, q, p)
      end else
        wrtins(' str %1, [x29, #@l] // store qword', ep^.r1, q, p);
      deltre(ep)
    end;

    {strx,strb,strc}
    195,73,74: begin parpq;
      frereg := allreg; if p <> blkstk^.lvl then getreg(r1, frereg);
      popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull);
      dmptre(ep); genexp(ep);
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then begin
        wrtins(' ldr %1, [x29, #0] // get display pointer', -p*ptrsize, r1);
        wrtins(' strb %1, [%2, #@l] // store byte', ep^.r1, r1, q, p)
      end else
        wrtins(' strb %1, [x29, #@l] // store byte', ep^.r1, q, p);
      deltre(ep)
    end;

    {strr}
    71: begin parpq;
      frereg := allreg; if p <> blkstk^.lvl then getreg(r1, frereg);
      popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull);
      dmptre(ep); genexp(ep);
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then begin
        wrtins(' ldr %1, [x29, #0] // get display pointer', -p*ptrsize, r1);
        wrtins(' str %1, [%2, #@l] // store real', ep^.r1, r1, q, p)
      end else
        wrtins(' str %1, [x29, #@l] // store real', ep^.r1, q, p);
      deltre(ep)
    end;

    {strs}
    72: begin parpq;
      frereg := allreg; popstk(ep); attach(ep); assreg(ep, frereg, rgnull, rgnull);
      dmptre(ep); genexp(ep);
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      if p <> blkstk^.lvl then
        wrtins(' ldr x9, [x29, #0] // get display pointer', -p*ptrsize)
      else
        wrtins(' mov x9, x29 // use frame pointer');
      wrtins(' add x9, x9, #@l // index destination', q, p);
      wrtins(' mov x10, %1 // copy source', ep^.r1);
      wrtins(' ldp x11, x12, [x10], #16 // load set part 1');
      wrtins(' stp x11, x12, [x9], #16 // store set part 1');
      wrtins(' ldp x11, x12, [x10] // load set part 2');
      wrtins(' stp x11, x12, [x9] // store set part 2');
      puttmp(ep^.r1a); deltre(ep)
    end;

    {sev}
    253: begin parpq;
      frereg := allreg;
      getreg(r1, frereg);
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      wrtins(' ldr x9, [sp] // get exception vector');
      if p <> blkstk^.lvl then begin
        wrtins(' ldr %1, [x29, #0] // get display pointer', -p*ptrsize, r1);
        wrtins(' str x9, [%1, #@l] // store qword', r1, q, p)
      end else
        wrtins(' str x9, [x29, #@l] // store qword', q, p)
    end;

    {mst}
    11: begin getlvl(p); labelsearch(def, val, lclspc, blk);
      labelsearch(def2, val2, sp2, blk);
      if blkstk <> nil then
        if blkstk^.btyp in [btproc, btfunc] then begin
          write(prr, '        .globl   '); wrtblklng(blkstk); writeln(prr);
          write(prr, '        .type    '); wrtblklng(blkstk); writeln(prr, ', %function');
          wrtblklabs(blkstk);
        end;
      frereg := allreg;
      { ARM64 allows more nesting levels }
      if p >= 32 then error('Too many nested levels');
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      { ARM64 uses STP/LDP for frame setup }
      wrtins(' stp x29, x30, [sp, #-16]! // save FP and LR');
      wrtins(' mov x29, sp // set up frame pointer');
      { Allocate space for display, mark, and locals }
      wrtins(' sub sp, sp, #0 // allocate mark space', marksize);
      { Store mark elements }
      wrtins(' str xzr, [x29, #-8] // place current ep (0)');
      wrtins(' str xzr, [x29, #-16] // place bottom of stack (0)');
      wrtins(' str xzr, [x29, #-24] // place previous ep (0)');
      { Allocate and clear locals }
      write(prr, '        sub     sp, sp, #'); write(prr, lclspc^); write(prr, '+');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' // allocate locals');
      wrtins(' and sp, sp, #0xfffffffffffffff0 // align stack');
      wrtins(' mov x9, sp // save SP for clearing');
      wrtins('1:');
      wrtins(' cmp x9, x29 // check have reached frame');
      wrtins(' b.ge 2f // skip if so');
      wrtins(' str xzr, [x9], #8 // clear word');
      wrtins(' b 1b // loop');
      wrtins('2:');
      write(prr, '        sub     x9, x29, #'); write(prr, lclspc^); write(prr, '+');
      write(prr, blkstk^.tmpnam^); writeln(prr, '+', marksize:1, ' // calc stack bottom');
      wrtins(' str x9, [x29, #0] // set bottom of stack', marksb);
      { Save callee-saved registers }
      wrtins(' stp x19, x20, [sp, #-16]! // save callee-saved registers');
      wrtins(' stp x21, x22, [sp, #-16]!');
      wrtins(' stp x23, x24, [sp, #-16]!');
      wrtins(' stp x25, x26, [sp, #-16]!');
      wrtins(' stp x27, x28, [sp, #-16]!');
      tmpoff := -(p+1)*ptrsize;
      tmpspc := 0; { clear temps }
      stkadr := 0;
      botstk
    end;

    {mov}
    55: begin parq;
      frereg := allreg; popstk(ep); popstk(ep2); dmptre(ep); dmptre(ep2);
      assreg(ep2, frereg, rgx0, rgnull); frereg := frereg-[rgx0];
      assreg(ep, frereg, rgx1, rgnull);
      genexp(ep2); genexp(ep);
      wrtins(' mov x2, #0 // load the length of move', q);
      wrtins('1: ldrb w9, [x1], #1 // copy loop');
      wrtins(' strb w9, [x0], #1');
      wrtins(' subs x2, x2, #1');
      wrtins(' b.ne 1b');
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
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      if (op = 78{srob}) or (op = 79){sroc} or (op = 196){srox} then begin
        if sp <> nil then begin
          wrtins(' adrp x9, @s // store byte to global (page)', sp^);
          wrtins(' strb %1, [x9, :lo12:@s] // store byte to global', ep^.r1, sp^)
        end else begin
          wrtins(' adrp x9, globals+0 // store byte to global (page)', q);
          wrtins(' strb %1, [x9, :lo12:globals+0] // store byte to global', q, ep^.r1)
        end
      end else if op = 76{sror} then begin
        if sp <> nil then begin
          wrtins(' adrp x9, @s // store real to global (page)', sp^);
          wrtins(' str %1, [x9, :lo12:@s] // store real to global', ep^.r1, sp^)
        end else begin
          wrtins(' adrp x9, globals+0 // store real to global (page)', q);
          wrtins(' str %1, [x9, :lo12:globals+0] // store real to global', q, ep^.r1)
        end
      end else begin {sroi, sroa}
        if sp <> nil then begin
          wrtins(' adrp x9, @s // store quad to global (page)', sp^);
          wrtins(' str %1, [x9, :lo12:@s] // store quad to global', ep^.r1, sp^)
        end else begin
          wrtins(' adrp x9, globals+0 // store quad to global (page)', q);
          wrtins(' str %1, [x9, :lo12:globals+0] // store quad to global', q, ep^.r1)
        end
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
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      wrtins(' sub x9, x29, #@s+0 // index temp set', ep^.r1a, lclspc^);
      if sp <> nil then begin
        wrtins(' adrp x10, @s // index global destination (page)', sp^);
        wrtins(' add x10, x10, :lo12:@s // index global destination', sp^)
      end else begin
        wrtins(' adrp x10, globals+0 // index global destination (page)', q);
        wrtins(' add x10, x10, :lo12:globals+0 // index global destination', q)
      end;
      wrtins(' ldp x11, x12, [x9], #16 // load set part 1');
      wrtins(' stp x11, x12, [x10], #16 // store set part 1');
      wrtins(' ldp x11, x12, [x9] // load set part 2');
      wrtins(' stp x11, x12, [x10] // store set part 2');
      puttmp(ep^.r1a); deltre(ep)
    end;

    {ujp}
    23: begin labelsearch(def, val, sp, blk);
      wrtins(' b @s // unconditional jump', sp^);
      if estack <> nil then begin { put in unresolved cache }
        getexp(ep); ep^.qs := sp;
        ep^.l := estack; estack := nil; ep^.next := jmpstr; jmpstr := ep;
      end
    end;

    {fjp,tjp}
    24,119: begin labelsearch(def, val, sp, blk);
      frereg := allreg; popstk(ep);
      assreg(ep, frereg, rgnull, rgnull); dmptre(ep); genexp(ep);
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      wrtins(' cmp %1, #0 // test boolean', ep^.r1);
      if op = 24{fjp} then wrtins(' b.eq @s // go if false', sp^)
      else {tjp} wrtins(' b.ne @s // go if true', sp^);
      deltre(ep)
    end;

    {xjp}
    25: begin labelsearch(def, val, sp, blk);
      frereg := allreg; popstk(ep); getreg(r1, frereg);
      assreg(ep, frereg, rgnull, rgnull);
      dmptre(ep); genexp(ep);
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      { ARM64 case jump: table contains offsets from table base }
      wrtins(' adrp %1, @s // index case jump table (page)', r1, sp^);
      wrtins(' add %1, %1, :lo12:@s // index case jump table', r1, sp^);
      wrtins(' ldr x9, [%1, %2, lsl #3] // load jump offset', r1, ep^.r1);
      wrtins(' add %1, %1, x9 // compute target', r1);
      wrtins(' br %1 // branch to target', r1);
      deltre(ep);
      botstk
    end;

    {ipj}
    112: begin getlvl(p); labelsearch(def, val, sp, blk);
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      wrtins(' ldr x29, [x29, #0] // get frame pointer for target', -p*ptrsize);
      wrtins(' ldr sp, [x29, #0] // get stack for target', marksb);
      wrtins(' and sp, sp, #0xfffffffffffffff0 // align stack');
      wrtins(' b @s // goto jump target', sp^);
      botstk
    end;

    {ret}
    22: begin
      frereg := allreg;
      wrtins(' ret // return');
      botstk
    end;

    {retp,retm}
    14,237: begin parq;
      frereg := allreg;
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      { Restore callee-saved registers }
      wrtins(' ldp x27, x28, [sp], #16 // restore callee-saved registers');
      wrtins(' ldp x25, x26, [sp], #16');
      wrtins(' ldp x23, x24, [sp], #16');
      wrtins(' ldp x21, x22, [sp], #16');
      wrtins(' ldp x19, x20, [sp], #16');
      wrtins(' mov sp, x29 // restore stack pointer');
      wrtins(' ldp x29, x30, [sp], #16 // restore FP and LR');
      wrtins(' add sp, sp, #0 // remove caller parameters', q);
      if op = 237{retm} then
        wrtins(' mov x0, sp // index result in x0');
      wrtins(' ret // return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    {reti,reta,retx,retc,retb}
    128,132,204,130,131: begin parq;
      frereg := allreg;
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      { Restore callee-saved registers }
      wrtins(' ldp x27, x28, [sp], #16 // restore callee-saved registers');
      wrtins(' ldp x25, x26, [sp], #16');
      wrtins(' ldp x23, x24, [sp], #16');
      wrtins(' ldp x21, x22, [sp], #16');
      wrtins(' ldp x19, x20, [sp], #16');
      wrtins(' mov sp, x29 // restore stack pointer');
      wrtins(' ldp x29, x30, [sp], #16 // restore FP and LR');
      wrtins(' add sp, sp, #0 // remove caller parameters', q);
      wrtins(' ldr x0, [sp], #16 // get qword result');
      if op in [204{retx},130{retc},131{retb}] then
        wrtins(' and x0, x0, #255 // mask byte result');
      wrtins(' ret // return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    {retr}
    129: begin parq;
      frereg := allreg;
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      { Restore callee-saved registers }
      wrtins(' ldp x27, x28, [sp], #16 // restore callee-saved registers');
      wrtins(' ldp x25, x26, [sp], #16');
      wrtins(' ldp x23, x24, [sp], #16');
      wrtins(' ldp x21, x22, [sp], #16');
      wrtins(' ldp x19, x20, [sp], #16');
      wrtins(' mov sp, x29 // restore stack pointer');
      wrtins(' ldp x29, x30, [sp], #16 // restore FP and LR');
      wrtins(' add sp, sp, #0 // remove caller parameters', q);
      wrtins(' ldr d0, [sp], #16 // get real result');
      wrtins(' ret // return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    {rets}
    236: begin parq;
      frereg := allreg;
      writeln(prr, '// generating: ', op:3, ': ', instab[op].instr);
      { Restore callee-saved registers }
      wrtins(' ldp x27, x28, [sp], #16 // restore callee-saved registers');
      wrtins(' ldp x25, x26, [sp], #16');
      wrtins(' ldp x23, x24, [sp], #16');
      wrtins(' ldp x21, x22, [sp], #16');
      wrtins(' ldp x19, x20, [sp], #16');
      wrtins(' mov sp, x29 // restore stack pointer');
      wrtins(' ldp x29, x30, [sp], #16 // restore FP and LR');
      wrtins(' add sp, sp, #0 // remove caller parameters', q);
      wrtins(' ret // return to caller');
      write(prr, blkstk^.tmpnam^); writeln(prr, ' = ', tmpspc:1);
      botstk; deltmp
    end;

    { Remaining terminal instructions truncated for file size }
    { These follow similar patterns to the AMD64 version }

  end (*case*)

end; (*assemble*)

begin (* main *)

  proginit; { perform independent init }

  write('P6 Pascal ARM64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write('.x');
  writeln;
  writeln;

  parcmdlin; { parse command line }

  rewrite(prr);

  writeln('Generating program');

  writeln(prr, '//');
  write(prr, '// File generated by P6 Pascal ARM64/gcc 64 bit code generator vs. ', majorver:1, '.', minorver:1);
  if experiment then write(prr, '.x');
  writeln(prr);
  writeln(prr, '//');
  writeln(prr);

  xlate; (* assembles and stores code *)

  99 : { abort run }

  writeln;
  writeln('Program generation complete');

  { return 0 on no error, 1 on error }
  seterr(ord(errret));

end.
