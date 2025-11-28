{*******************************************************************************

Contains the declaration of registers in the AMD64 CPU target

This includes the register type, and fixed tables classifying registers by type.

*******************************************************************************}

module registers;

const

maxintreg = 14; { maximum number of assignable integer reg }
maxfltreg = 16; { maximum number of assignable float reg }
maxintparreg = 6; { maximum number of integer/pointer parameter regs }
maxfltparreg = 8; { maximum number of floating parameter regs }

type 

{ registers in AMD64 target }
reg = (rgnull, rgrax, rgrbx, rgrcx, rgrdx, rgrsi, rgrdi, rgrbp, rgrsp, 
       rgr8, rgr9, rgr10, rgr11, rgr12, rgr13, rgr14, rgr15, 
       rgxmm0, rgxmm1, rgxmm2, rgxmm3, rgxmm4, rgxmm5, rgxmm6, rgxmm7,
       rgxmm8, rgxmm9, rgxmm10, rgxmm11, rgxmm12, rgxmm13, rgxmm14, rgxmm15);
regset = set of reg;

procedure wrtreg(var f: text; r: reg);
begin
  case r of
    rgnull:  write(f, '<null>');
    rgrax:   write(f, 'rax');
    rgrbx:   write(f, 'rbx');
    rgrcx:   write(f, 'rcx');
    rgrdx:   write(f, 'rdx');
    rgrsi:   write(f, 'rsi');
    rgrdi:   write(f, 'rdi');
    rgrbp:   write(f, 'rbp');
    rgrsp:   write(f, 'rsp');
    rgr8:    write(f, 'r8');
    rgr9:    write(f, 'r9');
    rgr10:   write(f, 'r10');
    rgr11:   write(f, 'r11');
    rgr12:   write(f, 'r12');
    rgr13:   write(f, 'r13');
    rgr14:   write(f, 'r14');
    rgr15:   write(f, 'r15');
    rgxmm0:  write(f, 'xmm0'); 
    rgxmm1:  write(f, 'xmm1'); 
    rgxmm2:  write(f, 'xmm2'); 
    rgxmm3:  write(f, 'xmm3'); 
    rgxmm4:  write(f, 'xmm4'); 
    rgxmm5:  write(f, 'xmm5'); 
    rgxmm6:  write(f, 'xmm6'); 
    rgxmm7:  write(f, 'xmm7');
    rgxmm8:  write(f, 'xmm8');
    rgxmm9:  write(f, 'xmm9'); 
    rgxmm10: write(f, 'xmm10'); 
    rgxmm11: write(f, 'xmm11'); 
    rgxmm12: write(f, 'xmm12'); 
    rgxmm13: write(f, 'xmm13'); 
    rgxmm14: write(f, 'xmm14'); 
    rgxmm15: write(f, 'xmm15');
  end
end;

procedure wrtbreg(var f: text; r: reg);
begin
  case r of
    rgnull:  write(f, '<null>');
    rgrax:   write(f, 'al');
    rgrbx:   write(f, 'bl');
    rgrcx:   write(f, 'cl');
    rgrdx:   write(f, 'dl');
    rgrsi:   write(f, 'sil');
    rgrdi:   write(f, 'dil');
    rgrbp:   write(f, 'bpl');
    rgrsp:   write(f, 'spl');
    rgr8:    write(f, 'r8b');
    rgr9:    write(f, 'r9b');
    rgr10:   write(f, 'r10b');
    rgr11:   write(f, 'r11b');
    rgr12:   write(f, 'r12b');
    rgr13:   write(f, 'r13b');
    rgr14:   write(f, 'r14b');
    rgr15:   write(f, 'r15b');
    rgxmm0:  write(f, 'xmm0'); 
    rgxmm1:  write(f, 'xmm1'); 
    rgxmm2:  write(f, 'xmm2'); 
    rgxmm3:  write(f, 'xmm3'); 
    rgxmm4:  write(f, 'xmm4'); 
    rgxmm5:  write(f, 'xmm5'); 
    rgxmm6:  write(f, 'xmm6'); 
    rgxmm7:  write(f, 'xmm7');
    rgxmm8:  write(f, 'xmm8');
    rgxmm9:  write(f, 'xmm9'); 
    rgxmm10: write(f, 'xmm10'); 
    rgxmm11: write(f, 'xmm11'); 
    rgxmm12: write(f, 'xmm12'); 
    rgxmm13: write(f, 'xmm13'); 
    rgxmm14: write(f, 'xmm14'); 
    rgxmm15: write(f, 'xmm15');
  end
end;

begin

end.