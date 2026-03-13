# Debug Test Maintenance Notes

## Fixing instruction offset breakpoints in debug_test.inp

### Background

The file `debug_test.inp` is a debugger command script that drives the debugger
through a series of tests. The output goes to `debug_test.lst`, and
`debug_test.cmp` is the known-good expected output.

Several debugger commands in the script use hardcoded byte offsets to set
breakpoints or tracepoints at specific instructions relative to the current
program counter (`@pc`). These look like:

```
bi @pc+28
bi @pc+82
tpi @pc+86
```

### Why offsets break

When the compiler's code generation changes (e.g., the write/read refactoring
that converted file parameters from stack-keeping to temp variables), the
generated P-code instruction sequences change. Instructions may be added,
removed, or reordered, which shifts the byte addresses. The hardcoded `@pc+N`
offsets then point to the wrong location — often landing in the middle of an
instruction — causing the debugger to misbehave (typically running past the
intended breakpoint to program termination).

### How to diagnose

1. Diff `debug_test.lst` against `debug_test.cmp` to find where they diverge.
2. The first major divergence after a `bi @pc+N` or `tpi @pc+N` command is
   likely a broken offset.
3. Look at the `li @pc` listing that appears **just before** the offset command
   in the `.lst` file. The comment in the `.inp` file tells you which
   instruction in that listing is the target (e.g., "the csp instruction 5th
   from the top" or "the fjp, 11th instruction from the top").
4. Calculate the new offset: subtract the `@pc` address from the target
   instruction's address (both in hex), then convert to decimal.

### How to fix

1. Fix offsets **one at a time from the top**, because a bad offset causes the
   test to go off the rails — everything after it is invalid.
2. After fixing an offset in `debug_test.inp`, rerun the test.
3. Check the new `.lst` output to see if the next offset command now has valid
   listings, and calculate its correct offset.
4. Repeat until all offsets are fixed.
5. Also update the comments in the `.inp` file if the instruction position
   changed (e.g., "5th from the top" → "7th from the top").

### Example (2026-02 write refactoring)

The write refactoring added `loda` (load temp variable) instructions around
writeln calls, replacing the old `lao` (load address of output) pattern. This
made the instruction sequences longer, shifting offsets:

- Line 255: `bi @pc+28` → `bi @pc+47` (csp instruction moved from 5th to 7th)
- Line 269: `bi @pc+82` → `bi @pc+75` (mrkl instruction, still 11th)
- Line 279: `tpi @pc+86` — unchanged (the for-loop code was not affected)
