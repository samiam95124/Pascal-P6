{*******************************************************************************

Contains the declaration of registers in the ARM64 (AArch64) CPU target

This includes the register type, and fixed tables classifying registers by type.

ARM64 Register Conventions (AAPCS64):
  X0-X7:   Parameter/result registers (caller-saved)
  X8:      Indirect result location register
  X9-X15:  Temporary registers (caller-saved)
  X16-X17: Intra-procedure-call scratch registers (IP0, IP1)
  X18:     Platform register (reserved)
  X19-X28: Callee-saved registers
  X29:     Frame pointer (FP)
  X30:     Link register (LR)
  SP:      Stack pointer

  V0-V7:   Parameter/result FP/SIMD registers (caller-saved)
  V8-V15:  Callee-saved (lower 64 bits only)
  V16-V31: Temporary FP/SIMD registers (caller-saved)

*******************************************************************************}

module registers_arm64;

const

maxintreg = 25; { maximum number of assignable integer reg (X0-X15, X19-X28) }
maxfltreg = 32; { maximum number of assignable float reg (V0-V31) }
maxintparreg = 8; { maximum number of integer/pointer parameter regs (X0-X7) }
maxfltparreg = 8; { maximum number of floating parameter regs (V0-V7) }

type

{ registers in ARM64 target }
{ Note: We use 'rg' prefix for compatibility with existing code }
reg = (rgnull,
       { General purpose registers X0-X30 }
       rgx0, rgx1, rgx2, rgx3, rgx4, rgx5, rgx6, rgx7,
       rgx8, rgx9, rgx10, rgx11, rgx12, rgx13, rgx14, rgx15,
       rgx16, rgx17, rgx18, rgx19, rgx20, rgx21, rgx22, rgx23,
       rgx24, rgx25, rgx26, rgx27, rgx28, rgx29, rgx30,
       rgsp,
       { SIMD/FP registers V0-V31 (used as D0-D31 for doubles) }
       rgv0, rgv1, rgv2, rgv3, rgv4, rgv5, rgv6, rgv7,
       rgv8, rgv9, rgv10, rgv11, rgv12, rgv13, rgv14, rgv15,
       rgv16, rgv17, rgv18, rgv19, rgv20, rgv21, rgv22, rgv23,
       rgv24, rgv25, rgv26, rgv27, rgv28, rgv29, rgv30, rgv31);
regset = set of reg;

{ Convenient aliases }
const
  rgfp = rgx29;  { frame pointer }
  rglr = rgx30;  { link register }

{ set assignment preference registers }
fixed

{ integer register assignment order }
{ Prefer callee-saved first (X19-X28), then caller-saved (X9-X15, X0-X7) }
{ Avoid X8 (indirect result), X16-X18 (special), X29 (FP), X30 (LR) }
intassord: array [1..maxintreg] of reg = array
    rgx19, rgx20, rgx21, rgx22, rgx23, rgx24, rgx25, rgx26, rgx27, rgx28,
    rgx9, rgx10, rgx11, rgx12, rgx13, rgx14, rgx15,
    rgx0, rgx1, rgx2, rgx3, rgx4, rgx5, rgx6, rgx7
end;

{ floating point register assignment order }
{ Prefer callee-saved first (V8-V15), then others }
fltassord: array [1..maxfltreg] of reg = array
    rgv8, rgv9, rgv10, rgv11, rgv12, rgv13, rgv14, rgv15,
    rgv16, rgv17, rgv18, rgv19, rgv20, rgv21, rgv22, rgv23,
    rgv24, rgv25, rgv26, rgv27, rgv28, rgv29, rgv30, rgv31,
    rgv0, rgv1, rgv2, rgv3, rgv4, rgv5, rgv6, rgv7
end;

{ parameter registers (X0-X7) }
parreg: array [1..9] of reg = array
    rgx0, rgx1, rgx2, rgx3, rgx4, rgx5, rgx6, rgx7, rgnull
end;

{ floating point parameter registers (V0-V7) }
parregf: array [1..8] of reg = array
    rgv0, rgv1, rgv2, rgv3, rgv4, rgv5, rgv6, rgv7
end;

{ Write full 64-bit register name (X registers) }
procedure wrtreg(var f: text; r: reg);
begin
  case r of
    rgnull:  write(f, '<null>');
    rgx0:    write(f, 'x0');
    rgx1:    write(f, 'x1');
    rgx2:    write(f, 'x2');
    rgx3:    write(f, 'x3');
    rgx4:    write(f, 'x4');
    rgx5:    write(f, 'x5');
    rgx6:    write(f, 'x6');
    rgx7:    write(f, 'x7');
    rgx8:    write(f, 'x8');
    rgx9:    write(f, 'x9');
    rgx10:   write(f, 'x10');
    rgx11:   write(f, 'x11');
    rgx12:   write(f, 'x12');
    rgx13:   write(f, 'x13');
    rgx14:   write(f, 'x14');
    rgx15:   write(f, 'x15');
    rgx16:   write(f, 'x16');
    rgx17:   write(f, 'x17');
    rgx18:   write(f, 'x18');
    rgx19:   write(f, 'x19');
    rgx20:   write(f, 'x20');
    rgx21:   write(f, 'x21');
    rgx22:   write(f, 'x22');
    rgx23:   write(f, 'x23');
    rgx24:   write(f, 'x24');
    rgx25:   write(f, 'x25');
    rgx26:   write(f, 'x26');
    rgx27:   write(f, 'x27');
    rgx28:   write(f, 'x28');
    rgx29:   write(f, 'x29');
    rgx30:   write(f, 'x30');
    rgsp:    write(f, 'sp');
    rgv0:    write(f, 'd0');
    rgv1:    write(f, 'd1');
    rgv2:    write(f, 'd2');
    rgv3:    write(f, 'd3');
    rgv4:    write(f, 'd4');
    rgv5:    write(f, 'd5');
    rgv6:    write(f, 'd6');
    rgv7:    write(f, 'd7');
    rgv8:    write(f, 'd8');
    rgv9:    write(f, 'd9');
    rgv10:   write(f, 'd10');
    rgv11:   write(f, 'd11');
    rgv12:   write(f, 'd12');
    rgv13:   write(f, 'd13');
    rgv14:   write(f, 'd14');
    rgv15:   write(f, 'd15');
    rgv16:   write(f, 'd16');
    rgv17:   write(f, 'd17');
    rgv18:   write(f, 'd18');
    rgv19:   write(f, 'd19');
    rgv20:   write(f, 'd20');
    rgv21:   write(f, 'd21');
    rgv22:   write(f, 'd22');
    rgv23:   write(f, 'd23');
    rgv24:   write(f, 'd24');
    rgv25:   write(f, 'd25');
    rgv26:   write(f, 'd26');
    rgv27:   write(f, 'd27');
    rgv28:   write(f, 'd28');
    rgv29:   write(f, 'd29');
    rgv30:   write(f, 'd30');
    rgv31:   write(f, 'd31');
  end
end;

{ Write 32-bit register name (W registers for integers, S registers for floats) }
procedure wrtbreg(var f: text; r: reg);
begin
  case r of
    rgnull:  write(f, '<null>');
    rgx0:    write(f, 'w0');
    rgx1:    write(f, 'w1');
    rgx2:    write(f, 'w2');
    rgx3:    write(f, 'w3');
    rgx4:    write(f, 'w4');
    rgx5:    write(f, 'w5');
    rgx6:    write(f, 'w6');
    rgx7:    write(f, 'w7');
    rgx8:    write(f, 'w8');
    rgx9:    write(f, 'w9');
    rgx10:   write(f, 'w10');
    rgx11:   write(f, 'w11');
    rgx12:   write(f, 'w12');
    rgx13:   write(f, 'w13');
    rgx14:   write(f, 'w14');
    rgx15:   write(f, 'w15');
    rgx16:   write(f, 'w16');
    rgx17:   write(f, 'w17');
    rgx18:   write(f, 'w18');
    rgx19:   write(f, 'w19');
    rgx20:   write(f, 'w20');
    rgx21:   write(f, 'w21');
    rgx22:   write(f, 'w22');
    rgx23:   write(f, 'w23');
    rgx24:   write(f, 'w24');
    rgx25:   write(f, 'w25');
    rgx26:   write(f, 'w26');
    rgx27:   write(f, 'w27');
    rgx28:   write(f, 'w28');
    rgx29:   write(f, 'w29');
    rgx30:   write(f, 'w30');
    rgsp:    write(f, 'wsp');
    rgv0:    write(f, 's0');
    rgv1:    write(f, 's1');
    rgv2:    write(f, 's2');
    rgv3:    write(f, 's3');
    rgv4:    write(f, 's4');
    rgv5:    write(f, 's5');
    rgv6:    write(f, 's6');
    rgv7:    write(f, 's7');
    rgv8:    write(f, 's8');
    rgv9:    write(f, 's9');
    rgv10:   write(f, 's10');
    rgv11:   write(f, 's11');
    rgv12:   write(f, 's12');
    rgv13:   write(f, 's13');
    rgv14:   write(f, 's14');
    rgv15:   write(f, 's15');
    rgv16:   write(f, 's16');
    rgv17:   write(f, 's17');
    rgv18:   write(f, 's18');
    rgv19:   write(f, 's19');
    rgv20:   write(f, 's20');
    rgv21:   write(f, 's21');
    rgv22:   write(f, 's22');
    rgv23:   write(f, 's23');
    rgv24:   write(f, 's24');
    rgv25:   write(f, 's25');
    rgv26:   write(f, 's26');
    rgv27:   write(f, 's27');
    rgv28:   write(f, 's28');
    rgv29:   write(f, 's29');
    rgv30:   write(f, 's30');
    rgv31:   write(f, 's31');
  end
end;

{ Get register name length for formatting }
function regl(r: reg): integer;
begin
  if r = rgnull then regl := 6
  else if r in [rgx0..rgx9, rgsp] then regl := 2
  else if r in [rgx10..rgx30] then regl := 3
  else if r in [rgv0..rgv9] then regl := 2
  else regl := 3
end;

{ Get byte register name length for formatting }
function bregl(r: reg): integer;
begin
  if r = rgnull then bregl := 6
  else if r in [rgx0..rgx9] then bregl := 2
  else if r in [rgx10..rgx30, rgsp] then bregl := 3
  else if r in [rgv0..rgv9] then bregl := 2
  else bregl := 3
end;

{ Write a register set }
procedure wrtregs(var f: text; rs: regset; n: boolean);
var first: boolean; r: reg;
begin
  first := true; write(f, '[');
  for r := rgx0 to rgv31 do
    if ((r in rs) = n) and not (r in [rgx29, rgsp]) then begin
      if not first then write(f, ', ');
      wrtreg(f, r); first := false
    end;
  write(f, ']')
end;

begin
end.
