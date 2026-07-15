# Win64 native bring-up findings

Handoff notes from the Windows-side debugging session (2026-07-15) that produced
PR #538 (`win64-modpath-llp64-eoln-fixes`). Written for whoever (human or
Claude) continues the work on the Linux side. The machine used was Scott's
Windows 10 box, msys64/mingw64 gcc 15.2, repo at `C:\projects\PASCAL\pascal-p6`.

The starting symptom: `pc hello` on native Windows printed a cascade of
`Invalid command` errors against `bin/pc.ins` and then `*** pc: Error: System
fault`. Three independent root causes were found. All Windows host executables
in the hosts tree are affected and need a cross-rebuild from these sources.

## Root cause 1: LLP64 in the Pascaline/C binding layer

`libs/source` (support.h, services_wrapper.h/.c, and the graphics/terminal/
network wrappers) was written assuming `long` is 64 bits. True on LP64 Linux;
on win64 (LLP64, mingw) `long` is 32 bits. Every struct shared between the C
wrappers and compiled Pascal then has the wrong layout on Windows:

- `filrec` (services_wrapper.h): C builds a 112-byte struct with `next` at
  offset 104; the Pascal side reads the Pascaline layout — 192 bytes, `next`
  at offset 184 (integer = 64 bits, sets = 256 bits). Pascal reads 80 bytes
  past the end of the C allocation. `pc` faulted in `dolist` ("System fault",
  `l^.next` garbage non-nil); pgen segfaulted; pint's extlink threw
  "Value out of range"; the shipped cmach.exe segfaulted.
- `pstring_header.len` (support.h): 4-byte len written, 8-byte len read.
- `PEVTL` event macro: casts to `long*` while the comment says 8 bytes.

**Why the wine regression never caught it:** wine's heap happened to hold
zeros past the allocation, so the garbage `next` read back as nil. Native
Windows heaps don't.

**Fix (in the PR):** per Scott, do NOT change declarations to `long long`
(that would break future 32-bit targets). psystem.c already established the
convention: `#define long long long` under `_WIN32`, after all system
includes. The same block is now in `support.h`, positioned so that CRT
prototypes and the Ami headers (`services.h` etc., included before support.h
in every binding source) keep their true `long`. This fixes services,
graphics, terminal and sound wrappers wholesale on rebuild. Note
`filrec.size/alloc` had literal `long long`, which the macro would expand to
`long long long long` (a hard error) — they are now plain `long`.

**Verified:** rebuilt win64 `services.a` natively; a Pascal test program
calling `services.list('*.p6')` and printing `l^.name^` runs correctly
(crashes with the old library). The pc scanner stack (scanner/restbl/spctbl/
tolkens/strings) also verified working.

## Root cause 2: line endings

`bin/pc.ins` checked out CRLF (`core.autocrlf=true`), and the parse module saw
the `\r` before each newline as a stray character — every error column in the
original spew pointed exactly one past the last real character. The same
problem applies to any text file the tools read (decks written by pcom on
Windows are CRLF too, via mingw text-mode stdio).

**Fix (in the PR), per Scott:** never convert files; recognize endings
universally in the runtime. `source/pgen/psystem.c` now implements the
flip.c / IP Pascal algorithm: two per-file flags (`filecr[]`, `filelf[]`
beside `fileoln[]`) set on the current character being cr/lf before the next
is read; an lf directly after cr (or cr after lf) is the second half of the
same line ending and is skipped. Endings crlf, lfcr, cr, lf are all valid.
Consumption goes through `getcunv()`, peeks through `chkcunv()` (both new);
`eoffile/eolnfile/chkfile/getfneoln` route through them and now take the
logical file number. Binary reads are untouched. The flags reset at
`resetfn` and the attach path.

**Note:** pint.pas and cmach.c have their own deck readers; they coped with
CRLF decks in testing, but were not audited for the same tolerance.

## Root cause 3: module path separator vs drive letters

pc concatenates its module path with `:` and splits it with
`indexp(pt, ':')`. On Windows the components themselves contain `:`
(`C:\...`), so the first split yields a bare `C`; `fndfil`/`makpth` then built
`C\psystem.pas`, `ami_fulnam` → `SetCurrentDirectory("C\")` failed, and
`winerr()` exited with status 1 — **silently**, because its stderr message is
lost when output is piped. Found with gdb (`b exit`, backtrace through
`pc.fndfil → makpth → wrapper_fulnam("C\psystem.pas") → ami_setcur("C\") →
winerr`).

**Fix (in the PR), per Scott's decision:** `;` is Pascal-P6's path-list
separator **on all hosts** — not per-host switching, which would create
linux/windows misunderstandings. These concatenated strings are internal to
the toolset. All seven build/split sites in pc.pas now use `;`.

**Migration:** `MODULEPATH` environment variables written `a:b` must become
`a;b` (quote it in a Unix shell). Docs still describe the path as
`:`-separated (docs/pc, and the `usespath` comment block in pc.pas ~line
2863) — not yet updated.

## What the Linux side should do

1. `git checkout win64-modpath-llp64-eoln-fixes` (PR #538).
2. Cross-rebuild the win64 runtime libs (`libs/win64/*` targets → hosts tree)
   and all Windows host executables (`bin/build` / `compwin` flow), then run
   the regression under wine. Note wine's zeroed heap masks LLP64 layout bugs
   — a native Windows smoke test of at least `pc hello` is the real proof.
3. Update the docs for the `;` separator and migrate any MODULEPATH settings.
4. The PR deliberately contains **no binaries**; the hosts tree binaries in
   the repo are still the broken builds.

## Known remaining issues (not fixed in the PR)

- `network_support.c`: `(unsigned long long)` casts will expand badly under
  the redefined `long` if that file ever includes support.h behind the macro
  (currently it includes network.h only); its private
  `typedef struct { long len; ... } pstrrec` still has 32-bit len on win64
  unless compiled behind the macro. Needs the same include-order treatment.
- `source/cmach/extern.inc` `putlongset(address, long v)` shifts `v` by up to
  56 bits — UB/garbage for the high 4 set bytes when `long` is 32 bits. (The
  file is generated by tools/extgen/gencexec.py; fix the generator.)
- `winerr()` (amitk windows/services.c) exits after an fprintf(stderr) that
  can vanish; consider fflush + routing through the normal error machinery.
  The drive-letter failure was completely invisible because of this.
- The committed win64 hosts `psystem.a` predated the bundled bypass-stdio
  member (no `stdio_printf` etc.). Rebuilt during this session. Related: the
  Ami bypass stdio omits `rename()` on Windows (relies on the CRT), but a
  `-DSTDIO_BYPASS` consumer's `rename()` call is coined to `stdio_rename` —
  cmach on Windows needs a shim (one exists in the session scratchpad).
- pint's command line parsing consumes `<deck> [<outfile>]` and any options
  adjacent to them, so an interpreted program cannot receive a leading option
  argument; workaround below. cmach passes args cleanly but its VM faulted
  "Value out of range [3777]" running the pgen deck where pint (with
  `--chkundef-`) succeeded — undiagnosed, possibly a cmach check or VM bug
  worth a look.
- pgen requires `--win64` explicitly on its command line even when the input
  deck carries `win64+` (deliberate per independent.pas:1965, but the error
  surfaces only after preamble emission, and stdout buffering hides it if the
  run dies).
- DWARF debug info emitted by pgen uses 32-bit relocations that overflow at
  static link (`relocation truncated to fit ... .debug_info`) with mingw
  binutils when linking `-g3`; the win64 exes here were linked with debug
  stripped. May affect the `pgen-dwarf-types` work.

## Bootstrap technique used (reference)

A working native pc.exe was produced on Windows without any working pc, pgen,
pint-externals or cmach, by:

1. Native `pcom.exe` works — compile every unit to a `.p6` deck (`--win64`).
2. Run **pgen as an interpreted deck** under pint:
   `pint --chkundef- pgen_all.p6 junk.out <input_lf.p6> --win64 <out.s>`
   where `pgen_all.p6` = concatenated decks of strings, version, endian, mpb,
   parcmd, registers, independent, pgen (strings must be included or the
   loader fails "Module not present"; `junk.out` absorbs pint's own output
   slot so the remaining args reach the interpreted pgen; `--win64` sits
   between pgen's input and output filenames so pint doesn't eat it).
   LF-convert decks first when the interpreter lacks the psystem eoln fix.
3. `gcc -static -g3 -c` each generated `.s`; link
   `main.o <objects leaves-first, program last> services.a psystem.a -lm
   -lpthread -Wl,--stack,8388608`, matching pc's own dolink order.
4. A fixed native cmach was also built directly from `source/cmach/cmach.c`
   (pure C, `-DEXTERNALS -DSTDIO_BYPASS -I amitk/libc -I amitk/include`,
   bypass stdio compiled `-DSTDIO_BYPASS`, plus a `stdio_rename` shim, link
   services.a sound.a network.a, NOT psystem.a) — it validated the wrapper
   fix by running `services.list` decks correctly.

Interpreted pgen throughput: small modules take minutes; pc.pas took ~40 min;
scanner ~40 min; restbl ~15 min. msys64 make invocation quirks on the Windows
box: pass `OS=Windows_NT PASCALP6=<abs path> TMP= TEMP=` as make command-line
variables (env doesn't propagate) and use absolute target paths.

## State of the Windows working tree (not in the PR)

On Scott's Windows box (branch `win64-modpath-llp64-eoln-fixes` checked out):
rebuilt `hosts/windows/x86/bit64/libs/{services.a,psystem.a}` and
`libs/win64/*` from the fixed sources; `bin/pc_fixed.exe` / `bin/pc_dbg.exe`
are bootstrap builds that still predate the `;`-separator recompile (their
regeneration was running in background when the session moved to Linux);
`bin/pc.ins` converted to LF (harmless; redundant once the psystem fix is in
the binaries); `amitk` submodule initialized fresh. Scratch pipeline artifacts
live under the session scratchpad `pcbuild/` directory, including win64 decks
for all pgen modules (`w64_*.p6`), ready to finish a native pgen if ever
needed.
