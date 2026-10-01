# Unifying the AMD64 and LLVM native targets

A plan to make objects built by the AMD64 generator (`pgen`) and by the LLVM
IR generator (`pgen_llvm`) link and call each other freely, so that there is
one native ABI, one runtime shim and one copy of each Pascal library module.

## Where things stand

At the instruction level the two targets already agree. Both use the SysV
register and stack argument order, both honor the frame offsets pcom assigns,
overflow parameters land in the same stack slots, and both call the C library
thunks with the plain Pascal signature. A routine in one object can be called
from the other and its arguments arrive correctly.

What forbids mixing is five protocols that live outside the call signature.
The AMD64 generator implements each by manipulating rbp and rsp directly.
LLVM code cannot do that, so the LLVM target replaced each with a small
runtime contract in the C shim (`source/pgen/psystem_exc.c`,
`psystem_goto.c`, `psystem_mods.c`, `main.c`, shared by both targets on
linux):

| Protocol | AMD64 generator | LLVM target |
|---|---|---|
| Static link and display | caller passes its frame base in r10; the prologue copies entries 1..level-1 from it and pushes its own base as entry level (was: `enterq` out of the dynamic caller's rbp frame) | caller passes its frame base in r10 (the `nest` parameter); the callee's prologue copies entries 1..level-1 from it and stores its own base as entry level |
| Structured function result | caller pushes result space on the stack; callee addresses it at positive rbp offsets | caller allocates the result and leaves its address at offset `sfoslot` of the frame r10 names; callee fetches it from there and keeps it at frame offset `sfrslot` |
| Exceptions | linux: as the LLVM target (windows: three words pushed on the stack plus the globals `psystem_expadr`, `psystem_expstk`, `psystem_expmrk`; a throw reloads rsp and rbp and jumps) | a frame of 80 bytes reserved at `bge`, linked as the innermost of the thread by the shim and set with `psystem_setjmp`; `thw` longjmps to the innermost, `mse` rethrows to the enclosing; the innermost pointer is thread-local |
| Non-local goto | linux: as the LLVM target (windows: load rbp from the display entry, reload rsp from the mark, jump) | table of (label, jmp_buf) in the target frame, pointer at frame offset 8; `psystem_llvm_ipj` (psystem_goto.c, in psystem.a) finds it through the display entry |
| Module chain | linux: as the LLVM target, through the section (windows: each object still falls through the end of its text into the next, `main.asm` runs into the first) | each object registers its entry in the `psystem_llvm_mods` section; `psystem_llvm_nextmod` (psystem_mods.c, in psystem.a) walks the section in link order |

The first two rows are as of #650, which moved the LLVM target off the
process globals `psystem_llvm_sl` and `psystem_llvm_sfr` onto rules 2 and 3
below, and of work items 1, 2, 3 and 5, which moved the AMD64 generator
onto rules 6, 4, 2 and 5 on linux. Only the structured result row still
differs there; windows is on the older mechanisms throughout.

Any one of these is enough to forbid mixing, which is why pc records the
generator of every object (`llvmobj`), rebuilds across generators, and keeps a
second copy of `strings.o` and `parse.o` in `libs/llvm`.

## Direction

clang owns its prologue, its frame pointer and its stack. Nothing pgen emits
can make LLVM code keep a pcom display at rbp offsets, accept a throw that
rewrites rsp underneath it, or fall through into the next object. Those are
exactly the things the LLVM target had to give up. So compatibility means the
AMD64 generator stops relying on them and adopts the five contracts. The LLVM
generator already proves each one against the full regression, so the AMD64
side mirrors decisions already made.

Two of the LLVM target's contracts are not good enough to become the ABI as
they stand, and are changed as part of the work.

## The ABI rules

These are the rules both generators follow afterwards, and that the win64 and
arm64 generators inherit.

1. **No shared mutable state on any call, throw or goto path.** Every value
   that crosses a call boundary travels in a register, on the stack or in the
   caller's own frame. Anything the runtime keeps between calls is
   thread-local. This is what makes the ABI hold in a multithreaded program,
   whether Pascaline grows threads or only receives callbacks on threads the
   sound and network libraries create.

2. **The static link travels in r10.** r10 is the SysV static chain register:
   GCC passes the static link of nested functions in it, and LLVM's `nest`
   parameter attribute lowers to it on x86-64. It is caller-saved and never
   carries an argument, so loading it before a call is harmless to a C callee,
   and every call keeps the same shape whether the callee is Pascal or a C
   thunk. The deck cannot tell the two apart, so this property is required.
   The caller loads r10 with its frame base immediately before the call; the
   callee's prologue copies display entries 1..level-1 from the frame r10
   names and stores its own base as entry level, exactly the copy `enterq`
   and the LLVM prologue perform today. Only the program or module block
   itself (level 1) copies nothing: a routine declared at the outer level is
   level 2 and copies entry 1, so every entry into Pascal code needs a valid
   static link. C code enters a Pascal procedure through `pacall`
   (`libs/source/support.c`), which loads r10 with the display of the
   procedure value alongside rbp, and so serves both generators.
   The Windows x64 convention also uses r10 for the static chain.

3. **The structured result address lives in the caller's frame.** Only one
   `nest` parameter exists, so the result pointer does not get a register of
   its own. The caller stores the address of the result area into a fixed
   slot of the frame r10 names immediately before the call (its own frame,
   or for a call through a procedure value the frame that value carries);
   the callee reads it at that offset from r10. The slot is frame offset 16
   (`sfoslot`), in the frame header the generators own, where the AMD64 frame
   has the saved frame pointer, mark and return address, so pcom's layout
   does not change. A callee fetches it only if its result frame is accessed:
   the program block is entered with no static link.

4. **Exceptions are setjmp/longjmp frames in the shared C shim, and the
   chain head is thread-local.** `bge` reserves a frame in the routine's
   temporaries, registers it and `_setjmp`s; `ede` drops it; `thw` longjmps
   to the innermost frame with the vector; `mse` drops and rethrows.
   `curexp`, `errmod` and `errline` become `_Thread_local`. A thread the
   runtime did not start has no master frame; a throw on it with an empty
   chain reports through the master handler as today. Whether such threads
   should be given a root frame (a `psystem_thread_init` entry) is decided
   when Pascal code first runs on one.
   The AMD64 generator's `psystem_expadr`, `psystem_expstk` and
   `psystem_expmrk` are process-wide globals with the same race; retiring
   them fixes a thread-safety hole the existing native target already has.

5. **Non-local goto through a per-frame table.** A routine that owns a target
   label keeps a table of (label key, jmp_buf) in its frame, pointer at frame
   base + 8, and `_setjmp`s at each target label. `ipj` becomes a call to
   `psystem_llvm_ipj(display entry, key)`. Rare in real code, so the cost
   lands only where the feature is used.

6. **Module chain through the linker section.** Each object places its entry
   address in `psystem_llvm_mods`; the initializer ends with a call to
   `psystem_llvm_nextmod` and a return, never a fall-through. Between-routine
   initializer strips are spliced into their owning routine with a local
   return dispatch, as the LLVM generator does. The C `main` replaces
   `main.asm`.

## Work items, in order

Each step is converted in the AMD64 generator and regressed on its own in
pgen mode before the next. Interoperation is testable only once all agree.

1. **Module chain and main.** Done on linux: the AMD64 generator emits the
   section entry, and the module end label calls `psystem_llvm_nextmod`
   (now `psystem_mods.c` in `psystem.a`, shared) and returns instead of
   falling through; `main.asm` calls `nextmod` instead of running into the
   first object. Windows keeps the fall through until a PE equivalent of
   the section bounds is chosen (`$`-sorted sections are the usual one).
   `main.asm` now serves windows only; `main.c` is the linux entry of
   both targets (item 2).

2. **Exceptions.** Done on linux. `bge` reserves the 80 byte frame on the
   stack, calls `psystem_bge` to link it and `psystem_setjmp` to arm it;
   `ede` calls `psystem_ede`; `mse` calls `psystem_mse`, which rethrows to
   the enclosing frame (the AMD64 sequence used to report an unhandled
   exception there); `sev` reads `psystem_curvec`. The innermost pointer,
   and the module and line of the last system error, are `_Thread_local` in
   `psystem_exc.c`, so each thread has its own chain. The jump buffer is the
   shim's own, `psystem_setjmp`/`psystem_longjmp` in `psystem.asm`, eight
   words, used by the exception frames and the non-local goto targets of
   both generators; no C library jump buffer or unwinder is involved. The
   non-local goto drops the frames of the activations it leaves
   (`psystem_popto`). `main.c` holds the master frame for both targets;
   `main.asm` and the throw and unwind of `psystem.asm` serve windows only.
   `psystem_caseerror` stays in assembly (the case table jump needs its
   fixed length), passing the error number now. `ExceptionBase` is defined
   once, in the shim.

3. **Static link in r10.** Done on both sides. AMD64: `movq %rbp,%r10`
   before each direct and vectored call, the procedure value's frame into
   r10 for an indirect one (rbp is no longer swapped for the call);
   `enterq` replaced by `pushq %rbp; movq %rsp,%rbp`, a `pushq -8k(%r10)`
   per outer level, and `pushq %rbp` for the level's own entry. LLVM
   (#650): a leading `ptr nest` parameter on every Pascal routine, the frame
   base passed in it at every call. `pacall` loads r10, and still rbp for
   objects from before this step; that goes when none is linked any more.

4. **Result pointer in the caller's frame.** Both generators store the
   result address to the header slot before the call and read it through
   r10 in the prologue (LLVM: done, #650). The AMD64 generator may keep
   allocating the result area on the stack; only how its address reaches the
   callee changes.

5. **Non-local goto.** Done on linux. AMD64: the prologue of a routine that
   owns a target keeps the table and the jump buffers between the aligned
   frame and the saved registers, pointer at frame offset 8, and arms each
   target with `_setjmp` after the pushes, where the stack is as the body
   leaves it at a label; `ipj` loads the owner's frame from the display and
   calls `psystem_llvm_ipj` (`psystem_goto.c`, shared). The stack mark at
   offset 16, which only the old `ipj` read, is no longer stored on linux,
   so offset 16 is free for item 4 and the LLVM target's `sfoslot` can stay
   where it is. Windows keeps the frame pointer and stack mark reload.

6. **Tooling.** pc drops `llvmobj` and the cross-generator rebuild; the
   `llvm` block in `bin/pc.ins` loses its module path and exclude;
   `libs/llvm` folds into `libs` (one `strings.o`, one `parse.o`, one
   `main.o`); `bin/build` loses `llvmlib`; the Makefile builds one runtime
   shim; `hostinstall` and `configure` lose the `libs/llvm` special case.
   Rename `psystem_llvm.c` to reflect that it serves both targets.

7. **Other targets.** win64 follows the same steps (r10 is the static chain
   there too; mingw's setjmp is the plain one). arm64 needs its own static
   link register, since AAPCS64 reserves the platform register on some
   systems; choose one and record it here.

## Acceptance

- Full regression in pgen and llvm modes, unchanged results.
- A pgen-built program linked against the LLVM-built `strings.o` and
  `parse.o`, and the reverse, through the whole library test set.
- A throw raised in a routine from one generator and caught in a routine
  from the other, and a non-local goto across the same boundary.
- drystone and fbench timings before and after, in both modes. The expected
  cost is one store per call and a setjmp per try-block entry; neither should
  show. If setjmp does show in exception-heavy code, the frame can be
  registered lazily, but measure first.
- gdb backtraces through mixed frames. The rbp chain is kept in the AMD64
  generator, and clang emits frame pointers when asked, so this should hold.

## Consequences

- One native ABI and one runtime shim; the assembly shims reduce to
  `psystem_caseerror`.
- The Pascal library modules exist once; the hosts tree loses `libs/llvm`.
- The existing native target becomes thread-safe on its exception path.
- pc's per-generator object tracking goes away, and with it the rule that a
  program's Pascal units must all come from one generator.

## Open questions

- Register for the arm64 static link.
- Whether threads the runtime did not start get a root exception frame, and
  what a throw on one with an empty chain should do.
- Whether the display copy stays per call (as `enterq` and the LLVM prologue
  do now) or moves to the static-ancestor walk that only needs the immediate
  static link. The per-call copy keeps every display reference a single load
  and is what pcom's layout assumes; it stays unless measurement says
  otherwise.
