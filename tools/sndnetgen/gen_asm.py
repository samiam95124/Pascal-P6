#!/usr/bin/env python3
# Generate <module>_wrapper.asm from the mangled names emitted by compiling
# the module's stub program. Usage:
#   pc tools/sndnetgen/<module>stub      # compile (link fails - fine)
#   nm <module>stub.o | python3 gen_asm.py <module> > <module>_wrapper.asm
# Unlike graphics, these modules have no file-selector forms: every
# `<module>.NAME$SIG` symbol maps to the single C wrapper `wrapper_NAME`.
import sys, re

mod = sys.argv[1]
globls=[]; tramps=[]; seen=set()
for line in sys.stdin:
    m=re.search(r'\bU\s+(%s\.([a-z0-9]+)\$(\S+))' % mod, line)
    if not m: continue
    full,name = m.group(1), m.group(2)
    if full in seen: continue
    seen.add(full)
    globls.append('    .globl  %s' % full)
    tramps.append('%s:\n    jmp     wrapper_%s' % (full, name))

out=['# Generated %s trampolines. Do not edit by hand.' % mod, '    .text', '']
out += globls + ['']
out += [t+'\n' for t in tramps]
sys.stdout.write('\n'.join(out)+'\n')
sys.stderr.write('generated %d trampolines\n' % len(tramps))
