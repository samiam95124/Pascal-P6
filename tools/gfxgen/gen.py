#!/usr/bin/env python3
# Generate Pascaline `external` declarations for graphics.pas from the
# extracted ami_ prototypes. Phase 1: interface decls only.
import re, sys

# C type -> Pascaline type (for scalar/simple params and returns)
TYPEMAP = {
    'int': 'integer',
    'float': 'real',
    'ami_color': 'color',
    'ami_lstyle': 'lstyle',
    'ami_tabori': 'tabori',
    'ami_winmodset': 'winmodset',
    'ami_stdmenusel': 'stdmenusel',
    'ami_qfnopts': 'qfnopts',
    'ami_qfropts': 'qfropts',
    'ami_qfteffects': 'qfteffects',
    'ami_menuptr': 'menuptr',
    'ami_strptr': 'strptr',
    'ami_evtcod': 'evtcod',
    'long': 'integer',
}
# pointer params that become Pascaline `var` (out) parameters
POINTEROUT = {
    'int*': 'integer', 'ami_menuptr*': 'menuptr', 'ami_strptr*': 'strptr',
    'ami_qfnopts*': 'qfnopts', 'ami_qfropts*': 'qfropts',
    'ami_qfteffects*': 'qfteffects',
}
RETMAP = {'void': None, 'int': 'integer', 'float': 'real'}

def parse_proto(line):
    m = re.match(r'^(.*?\bami_([a-z0-9]+))\s*\((.*)\)$', line)
    if not m: return None
    head, name, args = m.group(1), m.group(2), m.group(3).strip()
    ret = head[:m.start(2)-len('ami_')].strip()
    params = [] if args in ('', 'void') else [p.strip() for p in args.split(',')]
    return ret, name, params

def split_param(p):
    # returns (ctype, pname); ctype keeps '*'
    p = p.replace('const ', '')
    m = re.match(r'^(.*?)\s*([A-Za-z_]\w*)$', p)
    if not m:  # e.g. "char* s" already handled; fallback
        return p.strip(), ''
    ctype = m.group(1).strip()
    return ctype, m.group(2)

def gen_decl(ret, name, params):
    """Return (full_decl_lines). Emits file form + default form when first
    param is FILE*; flags anything unmapped with a trailing { ?? } note."""
    notes = []
    has_file = bool(params) and split_param(params[0])[0].replace(' ','') == 'FILE*'
    rest = params[1:] if has_file else params

    # build Pascaline param fragments from rest, with string/len and callback fusion
    def build(rest, withfile):
        frags = []
        i = 0
        while i < len(rest):
            ctype, pn = split_param(rest[i])
            cn = ctype.replace(' ','')
            # callback: ami_pevthan eh, ami_pevthan* oeh
            if cn == 'ami_pevthan':
                frags.append(f'procedure {pn or "eh"}(var er: evtrec)')
                i += 1; continue
            if cn == 'ami_pevthan*':
                frags.append(f'out {pn or "oeh"}: integer')
                i += 1; continue
            # event record
            if cn == 'ami_evtrec*':
                frags.append(f'var {pn or "er"}: evtrec')
                i += 1; continue
            # output string buffer:  char* NAME, int NAMEl  (length named name+'l')
            if cn == 'char*' and i+1 < len(rest):
                nt, nn = split_param(rest[i+1])
                if nt.replace(' ','') == 'int' and nn == pn + 'l':
                    frags.append(f'var {pn}: string')
                    i += 2; continue
            # input string (null-terminated): char* / const char* with no length
            if cn == 'char*':
                frags.append(f'view {pn}: string')
                i += 1; continue
            # file handles (FILE** out, or a non-selector FILE*) -> var f: text
            if cn in ('FILE**', 'FILE*'):
                frags.append(f'var {pn}: text')
                i += 1; continue
            # pointer-out scalars/structs:  int* x, ami_qfnopts* opt, ...
            if cn in POINTEROUT:
                frags.append(f'var {pn}: {POINTEROUT[cn]}')
                i += 1; continue
            # simple mapped types
            if cn in TYPEMAP:
                frags.append(f'{pn}: {TYPEMAP[cn]}')
                i += 1; continue
            notes.append(f'?{ctype}')
            frags.append(f'{pn}: {{?{ctype}}}')
            i += 1
        return frags

    pasret = RETMAP.get(ret, f'{{?{ret}}}')
    kw = 'function' if pasret else 'procedure'
    suffix = f': {pasret}' if pasret else ''

    frags_nofile = build(rest, False)
    paramstr = '; '.join(frags_nofile)

    lines = []
    if has_file:
        fileparams = ('var f: text' + ('; ' + paramstr if paramstr else ''))
        lines.append(f'{kw} {name}({fileparams}){suffix}; external;')
        deftparams = f'({paramstr})' if paramstr else ''
        lines.append(f'overload {kw} {name}{deftparams}{suffix}; external;')
    else:
        p = f'({paramstr})' if paramstr else ''
        lines.append(f'{kw} {name}{p}{suffix}; external;')
    return lines, notes

allnotes = {}
out = []
for line in open('/tmp/gfxgen/funcs.txt'):
    line = line.strip()
    if not line: continue
    pr = parse_proto(line)
    if not pr:
        out.append(f'{{ UNPARSED: {line} }}'); continue
    ret, name, params = pr
    lines, notes = gen_decl(ret, name, params)
    out.extend(lines)
    for n in notes: allnotes[n] = allnotes.get(n,0)+1

open('/tmp/gfxgen/decls.pas','w').write('\n'.join(out)+'\n')
print(f"wrote {len(out)} decl lines -> /tmp/gfxgen/decls.pas")
if allnotes:
    print("UNMAPPED C types (need handling):")
    for t,c in sorted(allnotes.items(), key=lambda x:-x[1]): print(f"  {c:3} {t}")
else:
    print("all types mapped.")
