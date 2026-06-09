#!/usr/bin/env python3
# Phase 2: generate graphics_wrapper.c (C wrappers) from the prototypes.
import re

def parse_proto(line):
    m = re.match(r'^(.*?\bami_([a-z0-9]+))\s*\((.*)\)$', line)
    if not m: return None
    head, name = m.group(1), m.group(2)
    ret = head[:m.start(2)-len('ami_')].strip()
    args = m.group(3).strip()
    params = [] if args in ('', 'void') else [p.strip() for p in args.split(',')]
    return ret, name, params

def split_param(p):
    p = p.replace('const ', '')
    m = re.match(r'^(.*?)\s*([A-Za-z_]\w*)$', p)
    return (m.group(1).strip(), m.group(2)) if m else (p.strip(), '')

# functions that need a hand-written wrapper (callbacks/records/marshalled types)
SPECIAL_TYPES = ('ami_pevthan', 'ami_pevthan*', 'ami_evtrec*', 'ami_menuptr',
                 'ami_menuptr*', 'ami_strptr', 'ami_strptr*', 'FILE**',
                 'ami_qfnopts*', 'ami_qfropts*', 'ami_qfteffects*')
NVARIANT = {'wrtstr'}  # has an ami_Xn length variant

def emit(ret, name, params):
    """Return (c_text or None). None => special, hand-write."""
    has_file = bool(params) and split_param(params[0])[0].replace(' ','')=='FILE*'
    rest = params[1:] if has_file else params
    for p in rest:
        ct = split_param(p)[0].replace(' ','')
        if ct in SPECIAL_TYPES: return None  # hand-write

    cret = {'void':'void','int':'int','float':'double'}.get(ret,'void')
    # build wrapper param list + call-arg list + pre/post statements
    cparams=[]; callargs=[]; pre=[]; post=[]
    i=0
    while i<len(rest):
        ct,pn = split_param(rest[i]); cn=ct.replace(' ','')
        if cn=='char*' and i+1<len(rest):
            nt,nn=split_param(rest[i+1])
            if nt.replace(' ','')=='int' and nn==pn+'l':  # output string buffer
                cparams += [f'string {pn}', f'int {nn}']
                callargs += [pn, nn]; i+=2; continue
        if cn=='char*':                                    # input string
            cparams += [f'string {pn}', f'int {pn}l']
            if name in NVARIANT:
                callargs.append(f'{pn}, {pn}l')            # use n-variant
            else:
                callargs.append(f'cstrz({pn}, {pn}l)')     # null-terminate
            i+=1; continue
        if cn=='int*':                                     # var integer out
            cparams.append(f'long* {pn}')
            pre.append(f'int t{pn};')
            callargs.append(f'&t{pn}')
            post.append(f'*{pn} = t{pn};')
            i+=1; continue
        if cn=='FILE*':                                    # explicit file param
            cparams.append(f'pfile p{pn}')
            pre.append(f'FILE* {pn} = psystem_libcwrfil(p{pn});')
            callargs.append(pn); i+=1; continue
        if cn=='float':
            cparams.append(f'double {pn}'); callargs.append(f'(float){pn}'); i+=1; continue
        # default: scalar (int / enum / winmodset / bool) -> int
        cparams.append(f'int {pn}'); callargs.append(pn); i+=1
    nvar = name if name in NVARIANT else name
    callee = ('ami_'+name+'n') if name in NVARIANT else ('ami_'+name)
    rkw = 'return ' if cret!='void' else ''

    def body(fileexpr):
        lines=[]
        if has_file and fileexpr == 'f':
            lines.append('    FILE* f = psystem_libcwrfil(pfp);')
        lines += ['    '+s for s in pre]
        args = [fileexpr] + callargs if (has_file or fileexpr) else callargs
        lines.append(f'    {rkw}{callee}({", ".join(a for a in args if a)});')
        lines += ['    '+s for s in post]
        return '\n'.join(lines)

    txt=[]
    if has_file:
        ps = ', '.join(['pfile pfp']+cparams)
        txt.append(f'{cret} wrapper_{name}f({ps})\n{{\n{body("f")}\n}}')
        ps2 = ', '.join(cparams) if cparams else 'void'
        txt.append(f'{cret} wrapper_{name}({ps2})\n{{\n{body("stdout")}\n}}')
    else:
        ps = ', '.join(cparams) if cparams else 'void'
        txt.append(f'{cret} wrapper_{name}({ps})\n{{\n{body(None)}\n}}')
    return '\n\n'.join(txt)

specials=[]
out=['/* Generated graphics wrappers. Do not edit by hand. */','',
      '#include <string.h>','#include <graphics.h>','#include <graphics_wrapper.h>','',
      'extern char* cstrz(char* s, int l);','']
for line in open('/tmp/gfxgen/funcs.txt'):
    line=line.strip()
    if not line: continue
    pr=parse_proto(line)
    if not pr: continue
    ret,name,params=pr
    t=emit(ret,name,params)
    if t is None: specials.append(name); continue
    out.append(t); out.append('')

open('/tmp/gfxgen/graphics_wrapper.c','w').write('\n'.join(out)+'\n')
gen=sum(1 for l in out if l.startswith('void wrapper_') or l.startswith('int wrapper_') or l.startswith('double wrapper_'))
print(f"generated {gen} wrapper functions -> graphics_wrapper.c")
print(f"SPECIAL (hand-write {len(specials)}): {' '.join(sorted(set(specials)))}")
