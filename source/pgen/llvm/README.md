# The LLVM IR code generator

The third pgen target. It translates the Pascal-P6 intermediate (`.p6`) to
textual LLVM IR (`.ll`), which clang compiles and links:

    source.pas -> pcom -> source.p6 -> pgen_llvm -> source.ll -> clang -> executable

It consumes the same intermediate as the AMD64 generator, the `amd64_sysv`
calling convention deck, and reproduces each routine's frame as pcom laid it
out for that convention, so every frame offset in the deck is honored as is.
LLVM does the rest: instruction selection, register allocation, the platform
calling convention, and optimization.

## Using it

    pc <program> -llvm          build through the LLVM target
    regress --llvm              the regression in llvm mode
    testprog --llvm <program>   one test program

`-llvm` is the executable (pgen) mode with the LLVM code generator and clang
in place of pgen and gcc. It is the default: the instruction file
(`bin/pc.ins`) selects it with the `llvm` instruction, and `-pgen` on the
command line selects the native generator instead. The tools for the mode
are set under the `llvm` tag:

    llvm
    ...
    begin llvm
       cc "clang"
       codegen "pgen_llvm"
       modulepath "../libs/llvm"
       exclude "../libs/llvm"
    end

Manual invocation:

    pcom program.pas program.p6 -amd64_sysv
    pgen_llvm program.p6 program.ll -amd64_sysv+
    clang -g -o program libs/llvm/main.o program.ll libs/psystem.a -lm -lpthread

A program's Pascal units must all come from one generator: an object built by
pgen and one built here do not interoperate (the frame layout and the call
protocol differ). pc checks the origin of each object it would reuse (the
generator leaves its `.ll` or `.s` beside it) and rebuilds one from the other
generator. The Pascal library modules therefore exist per generator: `strings.o`
and `parse.o` for this target are in `libs/llvm`, built by `bin/build`
(`llvmlib`). The C archives (`psystem.a`, `services.a`, `terminal.a`, ...) are
shared; their wrappers are called with the plain Pascal signature.

## Files

    pgen.pas          the generator (overrides the shared module's hooks)
    registers.pas     the register module: no registers, LLVM allocates them
    mpb.pas           machine parameter block (as amd64)
    endian.pas        endian mode (as amd64)
    pgen.ins          module path for the build

The runtime pieces the target needs in C are shared with the amd64 target on
linux and live in `source/pgen`: `main.c` (the program entry), `psystem_exc.c`
(the exception frames), `psystem_goto.c` (non-local goto) and
`psystem_mods.c` (the module chain), all in `psystem.a` except the entry,
which builds to `libs/main.o` and `libs/llvm/main.o` (Makefile `main`). The
generator builds to `bin/pgen_llvm` (`bin/build`).

## How it differs from the AMD64 generator

**Frames.** One `alloca` per routine holds the frame pcom laid out: the
display, the integer and real register pads, the function result slot,
locals, temporaries. Overflow parameters (beyond the six integer and six
real registers) are plain arguments spilled into the frame at the offsets
pcom assigned above the mark. The frame is zeroed at entry (file variables
must start closed).

**Scalar variables.** The frame's address escapes (into the display, and to
every callee), so LLVM can promote nothing in it to a register. Each frame
offset the routine's code accesses is therefore a named slot, `%lvN` for
offset -N, defined in the prologue as either an `alloca` of its own, which
mem2reg promotes, or the address of the frame bytes. The prologue is written
when the routine is complete, after its nested routines, so the choice is
made with everything known. A slot stays in the frame when a nested routine
reaches it through the display, when it lies in a structure (array, record,
set, file, container), when it is accessed in more than one way, when its
address is taken and no scalar symbol names it, or when the routine has a try
block or is the target of a non-local goto (a longjmp back into the routine
must find its variables in memory). Scalar locals, value and reference
parameters, the two words of a container parameter, the function result and
the for-loop and with temporaries are what leaves the frame. A scalar whose
address is taken (passed by reference) keeps its own alloca, which LLVM
treats conservatively.

**Addresses.** An address is an `i64` in the expression trees. The pointer
it was made from is remembered, and address arithmetic (`ixa`, `inc`, `cxs`)
is done on that pointer with `getelementptr`, so that LLVM knows which object
a store lands in; an `inttoptr` would let it touch any variable.

**Static links and structured results.** Every Pascal routine takes a
leading `ptr nest` parameter, the static link: the frame base of its caller,
which LLVM passes in the static chain register (r10 on x86-64). The prologue
rebuilds the x86 ENTER display from it, copying the entries of the outer
levels. r10 carries no argument and is caller-saved, so the C thunks of the
library modules, which the deck cannot tell apart from Pascal externals, are
called the same way and ignore it. C code that enters a Pascal procedure
(`pacall` in `libs/source/support.c`, the event callbacks) loads r10 with the
display of the procedure value.

Set and structure function results live in the caller's result frame, an
`alloca`. The caller leaves its address at offset `sfoslot` of the frame the
static link names, just before the call, and the callee's prologue fetches it
from there; it then maps the positive frame offsets pcom uses for the result
onto it. Only a routine whose result frame is accessed fetches it, so the
program block, entered from the module entry with no static link, does not
follow a null one. Nothing on the call path is shared between threads.

**Values.** Expression trees become SSA values, one per node result (two for
fat pointers), all `i64` or `double`. Duplicated call nodes (`duptre`) share
one call result.

**Control.** Labels are basic blocks; case tables become `switch`. Exceptions
are setjmp/longjmp frames through the shared shim (`psystem_exc.c`): `bge`
reserves a frame in the routine, links it as the innermost of the thread and
sets its jump buffer with `psystem_setjmp`, `thw` longjmps to the innermost
frame with the vector, `mse` rethrows to the enclosing one. The pointer to
the innermost frame is thread-local in the shim; the generated code never
touches it. Non-local goto: a routine that is the target of one keeps a table
of (label, jump buffer) in its frame, and the goto finds it through the
target's display entry (`psystem_llvm_ipj`), dropping the exception frames
it leaves. Checks use the
`llvm.*.with.overflow` intrinsics and branch to `psystem_errore`, declared
`noreturn`: the check then costs a compare and a branch, and does not make
LLVM reload every global after it.

**Module chain.** An object cannot fall through into the next one. Each module
registers its entry in the `psystem_llvm_mods` section; the linker
concatenates the entries in link order, which is the initialization order pc
established, and `psystem_llvm_nextmod` walks them. The walker is
`source/pgen/psystem_mods.c`, in `psystem.a`: the amd64 target chains its
modules the same way on linux. Module initializer strips
(`cal` between routines) are spliced into their owning routine with a local
return dispatch.

**Constants and globals.** Strings, sets, templates and constant tables are
private globals. A module's scalar globals (integer, real, boolean, char,
enumeration, subrange, pointer) are each a global of their own under the
exported name `module.symbol`, so LLVM can tell them apart from each other
and from the arrays. The rest live in one zero-initialized byte array at the
offsets pcom assigned, each symbol an alias at its offset.

## Status

Passes the regression in llvm mode: the sample programs, the ISO 7185
compliance test, Pascal-P2 and P4, the PRT rejection tests, Pascaline, and
the strings, services and network library tests.

Limits, as of this writing:

- amd64_sysv deck only, linux only (the section bounds `__start_`/`__stop_`
  are GNU ld's; a `-static` link works as for gcc).
- No debug metadata yet: `emitline` writes line comments, not `!DILocation`.
- `ctb` is not implemented.
- The IR carries no target triple; pc passes `-Wno-override-module`.
  Built and tested against clang 18 (Ubuntu 20.04) and clang 21 (Ubuntu 26.04).
- The IR uses opaque pointers, so clang 15 or later is needed; an older
  clang rejects every module ("expected type"). configure checks the
  default clang and offers to upgrade one that is too old.
