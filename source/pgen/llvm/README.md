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
in place of pgen and gcc. The instruction file (`bin/pc.ins`) selects them
under the `llvm` tag:

    llvm begin
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
    psystem_llvm.c    the runtime pieces the amd64 target keeps in assembly
                      (main.asm, psystem.asm), in C: main, the exception
                      frames, non-local goto, the module chain

`psystem_llvm.c` builds to `libs/llvm/main.o` (Makefile `$(LIBS)/llvm/main.o`,
part of `all`). The generator builds to `bin/pgen_llvm` (`bin/build`).

## How it differs from the AMD64 generator

**Frames.** One `alloca` per routine holds the frame pcom laid out: the
display, the integer and real register pads, the function result slot,
locals, temporaries. Overflow parameters (beyond the six integer and six
real registers) are plain arguments spilled into the frame at the offsets
pcom assigned above the mark. The frame is zeroed at entry (file variables
must start closed).

**Static links and structured results.** The x86 ENTER display is rebuilt
from the caller's frame base, which the caller stores in the runtime global
`psystem_llvm_sl` just before each call; the callee loads it in its prologue
and copies the display entries it needs. Set and structure function results
live in the caller's result frame, an `alloca` whose address goes in
`psystem_llvm_sfr` the same way; the callee maps the positive frame offsets
pcom uses for the result onto it. Globals rather than hidden arguments: the
generated routines then have the same signature as the C thunks of the library
modules, which the deck cannot tell apart from Pascal externals.

**Values.** Expression trees become SSA values, one per node result (two for
fat pointers), all `i64` or `double`. Duplicated call nodes (`duptre`) share
one call result.

**Control.** Labels are basic blocks; case tables become `switch`. Exceptions
are setjmp/longjmp frames in `psystem_llvm.c`: `bge` registers a frame and
`_setjmp`s, `thw` longjmps to the innermost frame with the vector, `mse`
rethrows to the enclosing one. Non-local goto: a routine that is the target of
one keeps a table of (label, jmp_buf) in its frame, and the goto finds it
through the target's display entry (`psystem_llvm_ipj`). Checks use the
`llvm.*.with.overflow` intrinsics and branch to `psystem_errore`.

**Module chain.** An object cannot fall through into the next one. Each module
registers its entry in the `psystem_llvm_mods` section; the linker
concatenates the entries in link order, which is the initialization order pc
established, and `psystem_llvm_nextmod` walks them. Module initializer strips
(`cal` between routines) are spliced into their owning routine with a local
return dispatch.

**Constants.** Strings, sets, templates and constant tables are private
globals; the module's globals are one zero-initialized byte array, with each
global symbol an alias at its offset.

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
