#!/usr/bin/env python3
# Emit gstub.pas: a Pascaline program that calls every graphics routine (file
# form + default form) so the compiler emits the mangled `graphics.X$sig`
# external references. Compile it (link will fail - that is fine), then `nm` the
# undefined graphics.* symbols and feed them to gen_asm.py.
import re

def parse(line):
    m=re.match(r'^(.*?\bami_([a-z0-9]+))\s*\((.*)\)$',line)
    if not m: return None
    ret=m.group(1)[:m.start(2)-4].strip(); name=m.group(2)
    a=m.group(3).strip(); ps=[] if a in('','void') else [p.strip() for p in a.split(',')]
    return ret,name,ps

def sp(p):
    p=p.replace('const ','')
    m=re.match(r'^(.*?)\s*([A-Za-z_]\w*)$',p); return (m.group(1).strip(),m.group(2)) if m else (p,'')

# dummy argument for each Pascaline param type
def dummy(rest):
    args=[]; i=0
    while i<len(rest):
        ct,pn=sp(rest[i]); cn=ct.replace(' ','')
        if cn=='char*' and i+1<len(rest) and sp(rest[i+1])[0].replace(' ','')=='int' and sp(rest[i+1])[1]==pn+'l':
            args.append('ss'); i+=2; continue
        if cn=='char*': args.append('ss'); i+=1; continue
        if cn=='int*' or cn in('ami_qfnopts*','ami_qfropts*','ami_qfteffects*'): args.append('ii'); i+=1; continue
        if cn=='FILE**' or cn=='FILE*': args.append('ff'); i+=1; continue
        if cn=='float': args.append('rr'); i+=1; continue
        if cn=='ami_pevthan': args.append('dummyeh'); i+=1; continue
        if cn=='ami_pevthan*': args.append('oo'); i+=1; continue
        if cn=='ami_evtrec*': args.append('ee'); i+=1; continue
        if cn=='ami_menuptr' or cn=='ami_menuptr*': args.append('mm'); i+=1; continue
        if cn=='ami_strptr' or cn=='ami_strptr*': args.append('spp'); i+=1; continue
        if cn=='ami_color': args.append('black'); i+=1; continue
        if cn=='ami_lstyle': args.append('lssolid'); i+=1; continue
        if cn=='ami_tabori': args.append('totop'); i+=1; continue
        if cn=='ami_winmodset': args.append('wms'); i+=1; continue
        if cn=='ami_evtcod': args.append('etframe'); i+=1; continue
        args.append('ii'); i+=1   # int / enum-as-int / long / stdmenusel
    return args

calls=[]
for line in open('/tmp/gfxgen/funcs.txt'):
    line=line.strip()
    if not line: continue
    pr=parse(line)
    if not pr: continue
    ret,name,ps=pr
    hasfile = bool(ps) and sp(ps[0])[0].replace(' ','')=='FILE*'
    rest = ps[1:] if hasfile else ps
    args=dummy(rest)
    asn = 'ii := ' if ret=='int' else ('rr := ' if ret=='float' else '   ')
    def call(arglist):
        return f'{asn}{name}({", ".join(arglist)});'
    if hasfile:
        calls.append('   '+call(['output']+args))
        calls.append('   '+call(args))
    else:
        calls.append('   '+call(args))

hdr='''program gstub(output);

uses graphics;

var ii, oo: integer;
    rr:     real;
    ss:     packed array [1..100] of char;
    ee:     evtrec;
    mm:     menuptr;
    spp:    strptr;
    ff:     text;
    wms:    winmodset;

procedure dummyeh(var er: evtrec); begin end;

begin
'''
open('/tmp/gfxgen/gstub.pas','w').write(hdr+'\n'.join(calls)+'\n\nend.\n')
print(f"wrote gstub.pas with {len(calls)} calls")
