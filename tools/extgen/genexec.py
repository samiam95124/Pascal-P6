#!/usr/bin/env python3
# Generate the execterminal procedure body for source/term/extterm.pas from the
# terminal symbol list. Each symbol's signature digest drives the parameter
# marshalling; the case index matches the exttables terminal table order.
#
# Marshalling model (mirrors the hand-written services cases):
# - Parameters sit on the VM stack from `params` upward in reverse
#   declaration order (the last parameter at `params`); each scalar slot is
#   intsize, a view string is strparsiz (pointer+length), the file and var
#   parameters are addresses (adrsize).
# - A function's return slot is at `params` after it is advanced past all
#   parameters.
# - The file parameter maps the VM logical file to the interpreter's own
#   console files: the event routine takes input, everything else output;
#   general files are an error (the terminal model manages the console).
# - Colors convert between the VM ordinal and the native enumeration.
# - The eventover/eventsover forms take procedure parameters (callbacks into
#   interpreted code); they error as unimplemented.
import os, re

HERE = os.path.dirname(os.path.abspath(__file__))
OUT  = os.path.join(HERE, 'execterminal.frag')

def toks(sig):
    """split a signature digest into top-level tokens after the kind"""
    out=[]; i=0; depth=0; cur=''
    for ch in sig:
        if ch=='_' and depth==0:
            if cur: out.append(cur)
            cur=''
        else:
            if ch=='(': depth+=1
            if ch==')': depth-=1
            cur+=ch
    if cur: out.append(cur)
    return out

syms=[l.rstrip('\n') for l in open(os.path.join(HERE,'terminal.syms')) if l.strip()]

body=[]
for idx, sym in enumerate(syms, start=1):
    name, sig = sym.split('@', 1)
    tl = toks(sig)
    kind, params = tl[0], tl[1:]
    isfunc = kind == 'f'

    if name in ('eventover', 'eventsover'):
        body.append('       { %s takes procedure parameters (callbacks into'
                    % name)
        body.append('         interpreted code) and is not implemented }')
        body.append('       %d: errore(NotImplemented);' % idx)
        body.append('')
        continue

    # declared-order parameter list with stack slot sizes
    sizes=[]
    for p in params:
        if p=='fc': sizes.append(('file','adrsize'))
        elif p=='vc': sizes.append(('vstr','strparsiz'))
        elif p=='b': sizes.append(('bool','intsize'))
        elif p=='c': sizes.append(('char','intsize'))
        elif p.startswith('x('): sizes.append(('color','intsize'))
        elif p.startswith('r('): sizes.append(('evtrec','adrsize'))
        elif p=='i': sizes.append(('int','intsize'))
        else: raise SystemExit('unhandled token %r in %s' % (p, sym))

    # stack offsets: last declared parameter at params+0
    offs=[]; acc='params'
    for kindp, size in reversed(sizes):
        offs.append(acc)
        acc = acc + '+' + size
    offs.reverse()  # offs[i] = address expression of declared param i
    total = '+'.join(s for _, s in sizes)

    # emit
    args=[]; pre=[]; post=[]
    vi=0
    filearg = 'input' if name=='event' else 'output'
    for (kindp, size), off in zip(sizes, offs):
        if kindp=='file':
            pre.append('           ad := getadr(%s); valfil(ad); fn := getbyt(ad);' % off)
            if name=='event':
                pre.append('           if (fn <> inputfn) and (fn > commandfn) then')
                pre.append('              errore(FileModeIncorrect);')
            else:
                pre.append('           if fn > commandfn then errore(FileModeIncorrect);')
            args.append(filearg)
        elif kindp=='int':
            vi+=1; v='a' if vi==1 else 'a%d' % vi
            pre.append('           %s := getint(%s);' % (v, off))
            args.append(v)
        elif kindp=='bool':
            vi+=1; v='a' if vi==1 else 'a%d' % vi
            pre.append('           %s := getint(%s);' % (v, off))
            args.append('%s <> 0' % v)
        elif kindp=='char':
            vi+=1; v='a' if vi==1 else 'a%d' % vi
            pre.append('           %s := getint(%s);' % (v, off))
            args.append('chr(%s)' % v)
        elif kindp=='color':
            vi+=1; v='a' if vi==1 else 'a%d' % vi
            pre.append('           %s := getint(%s);' % (v, off))
            args.append('int2color(%s)' % v)
        elif kindp=='vstr':
            pre.append('           getstr(%s, s);' % off)
            args.append('s')
        elif kindp=='evtrec':
            pre.append('           ad2 := getadr(%s);' % off)
            args.append('er')
            post.append('           putevt(er, ad2);')

    call = 'terminal.%s(%s)' % (name, ', '.join(args)) if args \
           else 'terminal.%s' % name
    body.append('       %d: begin { %s@%s }' % (idx, name, sig[:40]))
    body.append('')
    body += pre
    if isfunc:
        # all terminal function results fit the integer return slot
        # (ints, booleans via ord, chars via ord)
        rname = name
        body.append('           a9 := ord(%s);' % call)
        if total: body.append('           params := params+%s;' % total)
        body.append('           putint(params, a9);')
    else:
        body.append('           %s;' % call)
        body += post
        if total: body.append('           params := params+%s;' % total)
    body.append('')
    body.append('       end;')
    body.append('')

open(OUT,'w').write('\n'.join(body)+'\n')
print('execterminal.frag: %d cases' % len(syms))
