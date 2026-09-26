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
runtime contract in `psystem_llvm.c`:

| Protocol | AMD64 generator | LLVM target |
|---|---|---|
| Static link and display | `enterq` copies the display out of the dynamic caller's rbp frame | caller stores its frame base in the global `psystem_llvm_sl`; the callee's prologue copies entries 1..level-1 from it and stores its own base as entry level |
| Structured function result | caller pushes result space on the stack; callee addresses it at positive rbp offsets | caller allocates the result and publishes its address in the global `psystem_llvm_sfr`; callee keeps it at frame offset `sfrslot` |
| Exceptions | three words pushed on the stack plus the globals `psystem_expadr`, `psystem_expstk`, `psystem_expmrk`; a throw reloads rsp and rbp and jumps | chain of C frames, `bge` registers one and `_setjmp`s, `thw` longjmps to the innermost, `mse` rethrows to the enclosing |
| Non-local goto | load rbp from the display entry, reload rsp from the mark, jump | table of (label, jmp_buf) in the target frame; `psystem_llvm_ipj` finds it through the display entry |
| Module chain | each object falls through the end of its text into the next; `main.asm` calls the first | each object registers its entry in the `psystem_llvm_mods` section; `psystem_llvm_nextmod` walks the section in link order |

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
   and the LLVM prologue perform today. Level-1 routines copy nothing, which
   is why C callbacks into level-1 Pascal routines need no static link.
   The Windows x64 convention also uses r10 for the static chain.

3. **The structured result address lives in the caller's frame.** Only one
   `nest` parameter exists, so the result pointer does not get a register of
   its own. The caller stores the address of the result area into a fixed
   slot of its own frame immediately before the call; the callee reads it at
   that offset from the frame base r10 names. The slot is in the frame header
   the generators own (the LLVM target already reserves `sfrslot` there), so
   pcom's layout does not change. The globals `psystem_llvm_sl` and
   `psystem_llvm_sfr` disappear.

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

1. **Module chain and main.** Emit the section entry and the `nextmod` call;
   splice the initializer strips; drop `main.asm` and build the C main for
   both targets. This step alone removes the duplicated startup code and the
   `psystem_thw`/`psystem_unwind` symbol collision class.

2. **Exceptions.** Replace the pushes in `bge`/`ede`/`mse` with the shim
   calls and `_setjmp`; make the chain head thread-local in the shim; remove
   the throw and unwind routines from `psystem.asm`. `psystem_caseerror` stays
   in assembly (the case table jump needs its fixed length) and is not
   duplicated anywhere. `ExceptionBase` is defined once, in the shim.

3. **Static link in r10.** AMD64: `movq %rbp,%r10` before each call; replace
   `enterq` with the explicit display copy through r10 (`enterq` with a
   nesting level is microcoded, so expect a small win, not a cost). LLVM:
   add a leading `ptr nest` parameter to every Pascal routine and pass the
   frame base in it at every call; drop the `psystem_llvm_sl` load and
   store.

4. **Result pointer in the caller's frame.** Both generators store the
   result address to the header slot before the call and read it through
   r10 in the prologue; drop `psystem_llvm_sfr`. The AMD64 generator may keep
   allocating the result area on the stack; only how its address reaches the
   callee changes.

5. **Non-local goto.** AMD64: build the table and `_setjmp` at target labels
   in routines that own them; `ipj` becomes the shim call.

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
