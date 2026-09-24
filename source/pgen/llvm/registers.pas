{*******************************************************************************
*                                                                              *
*                        LLVM IR TARGET REGISTER MODULE                        *
*                                                                              *
* The LLVM IR target has no physical registers: LLVM allocates them. The       *
* shared module (independent.pas) carries register fields in its expression    *
* nodes and diagnostic writers for them, so this module supplies the minimum   *
* the shared module names: a register type with the null register, the         *
* register set, and the writers, which print nothing useful.                   *
*                                                                              *
*******************************************************************************}

module registers;

const
maxintreg = 1;
maxfltreg = 1;
maxintparreg = 1;
maxfltparreg = 1;
regprefix = false;

type
reg = (rgnull, rgvirt);
regset = set of reg;

fixed
allreg: regset = [rgvirt];

procedure wrtreg(var f: text; r: reg);
begin
   if r = rgnull then write(f, 'null') else write(f, 'virt')
end;

procedure wrtbreg(var f: text; r: reg);
begin
   wrtreg(f, r)
end;

function regl(r: reg): integer;
begin
   regl := 4
end;

function bregl(r: reg): integer;
begin
   bregl := 4
end;

procedure wrtregs(var f: text; rs: regset; sep: boolean);
begin
   if rgvirt in rs then write(f, 'virt')
end;

begin
end.
