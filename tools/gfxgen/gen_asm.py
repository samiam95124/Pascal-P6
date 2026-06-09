#!/usr/bin/env python3
# Generate graphics_wrapper.asm from the mangled names emitted by compiling
# gstub.pas. Usage:
#   pc gstub            # compile (link fails - fine); produces gstub.o
#   nm gstub.o | python3 gen_asm.py > graphics_wrapper.asm
# Each undefined `graphics.NAME$SIG` symbol becomes a trampoline that jumps to
# the C wrapper (wrapper_NAMEf for the file form, wrapper_NAME otherwise).
import sys, re

# functions that have a file-selector form (first param FILE*) -> have both
# wrapper_Xf and wrapper_X; others have a single wrapper_X.
sel=set()
for line in open('/tmp/gfxgen/funcs.txt'):
    m=re.match(r'^.*?\bami_([a-z0-9]+)\s*\((.*)\)$',line.strip())
    if not m: continue
    name,args=m.group(1),m.group(2).strip()
    first=args.split(',')[0].replace(' ','') if args and args not in('void','') else ''
    if first.startswith('FILE*') and not first.startswith('FILE**'): sel.add(name)

def param_tokens(sig):
    # sig like 'p_fc_i_i' or 'p_i_i' or 'f...'; tokens after the kind marker
    parts = sig.split('_')
    return parts[1:] if parts else []

globls=[]; tramps=[]
seen=set()
for line in sys.stdin:
    m=re.search(r'\bU\s+(graphics\.([a-z0-9]+)\$(\S+))', line)
    if not m: continue
    full,name,sig = m.group(1),m.group(2),m.group(3)
    if full in seen: continue
    seen.add(full)
    toks=param_tokens(sig)
    is_file = name in sel and toks and toks[0]=='fc'
    wrap = ('wrapper_%sf' % name) if is_file else ('wrapper_%s' % name)
    globls.append('    .globl  %s' % full)
    tramps.append('%s:\n    jmp     %s' % (full, wrap))

out=['# Generated graphics trampolines. Do not edit by hand.', '    .text', '']
out += globls + ['']
out += [t+'\n' for t in tramps]
sys.stdout.write('\n'.join(out)+'\n')
sys.stderr.write('generated %d trampolines\n' % len(tramps))
