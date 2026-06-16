#!/usr/bin/env python3
# C emitter: generate the external executor for the cmach interpreter
# (source/cmach/extern.inc) as C, the counterpart to genpexec.py which emits the
# same marshalling as Pascal for the pint/pmach interpreters.
#
# The spec parsing is shared: this imports genpexec's Gen (and parse_interface,
# toks, parse_evtrec) and subclasses it, replacing only the per-routine emission
# so the offset model -- params on the VM stack from `params` upward in reverse
# declaration order, scalar/address slots 8 bytes, a string 16 -- is identical to
# the Pascal generator. cmach exposes the same accessor names (getint, getadr,
# getbyt, putint, putadr, putbyt, getrel, putrel) and size macros (INTSIZE,
# ADRSIZE, REALSIZE, SETSIZE), so the translation is close to one-for-one. The
# native binding is called directly as ami_<name>(...) (the ami_* C API in
# amitk/include/<module>.h), where the Pascal side called <module>.<name>.
#
# Status: FOUNDATION. The offset model, flat routine numbering (matching
# source/exttables.pas via MODORDER), LookupExternal table, executor scaffold and
# the stub staging (struct/list/set/byte-array/event/menu/cert kinds error and
# are reported, like the Pascal generator) are in place and the generator runs.
#
# NEXT, and the substance of the work: the native ami_<name> ABI is NOT a 1:1
# image of the Pascal <module>.<name> signature, so the per-routine argument
# marshalling here is provisional. The native binding (amitk/include/<module>.h)
# passes explicit string lengths the Pascal interface carries implicitly -- e.g.
# the Pascal times(var s: string; t: integer) maps to
# ami_times(char* s, int sl, int t): a var/out string contributes (buffer,
# length) and a view string contributes (buffer) alone. The emitter must be
# driven by the parsed native header signatures rather than the Pascal parameter
# list to get the call shapes (and the length arguments) right. Until then the
# emitted extern.inc is not wired into cmach's build (EXTERNALS stays undefined).
import os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, HERE)

from genpexec import Gen, toks  # shared spec parsing

# Module flat-numbering order, matching source/exttables.pas: a symbol's routine
# number is its position in this concatenation (services 1..101, then terminal,
# graphics, sound, network). cmach's LookupExternal returns these numbers and the
# executor dispatches on them, so plain cmach numbers identically to pint/pmach
# and the terminal/graphics flavors slot in without renumbering.
MODORDER = ['services', 'terminal', 'graphics', 'sound', 'network']

# C size expressions for the slot names the Pascal generator uses.
SLOTC = {'strparsiz': '(PTRSIZE+INTSIZE)', 'adrsize': 'ADRSIZE',
         'intsize': 'INTSIZE'}

# parameter kinds whose C marshalling is not yet generated; the routine is
# stubbed and reported, exactly as genpexec stubs HARDTYPES.
CSTUBKINDS = {'string-list', 'byte-array', 'event-record', 'set', 'menu',
              'cert', 'list-struct', 'callback'}


class CGen(Gen):
    """Emit the per-routine executor cases as C for cmach."""

    def slotc(self, mode, typ):
        return SLOTC[self.slot(mode, typ)]

    def cgencase(self, idx, sym):
        """Return C lines for one routine, or a stub if it uses a kind not yet
           marshalled in C. Mirrors genpexec.Gen.gencase but emits C."""
        name, sig = sym.split('@', 1)
        tl = toks(sig); kind, ptoks = tl[0], tl[1:]
        isfunc = kind == 'f'
        hasfile = len(ptoks) > 0 and ptoks[0] == 'fc'
        # kinds not yet ported to C: callbacks and the special graphics/network
        # struct routines genpexec special-cases.
        if name in ('eventover', 'eventsover') or \
           (self.module == 'graphics' and name in ('menu', 'stdmenu')) or \
           (self.module == 'network' and
            name in ('certlistnet', 'certlistmsg', 'certlistfree')):
            return self._stub(idx, name, 'callback/struct routine')
        d = self.match_decl(name, hasfile, len(ptoks))
        if d is None:
            self.stubs.append((idx, name, 'no interface match'))
            return ['        case %d: errore(FunctionNotImplemented); break;' % idx]
        # reject kinds not yet handled in C
        for mode, pn, typ in d['params']:
            base = typ.split()[0]
            k = self._kind(mode, typ, base)
            if k in CSTUBKINDS:
                self.stubs.append((idx, name, '%s (%s)' % (typ, k)))
                return self._stub(idx, name, '%s parameter' % typ)
        offs = []; acc = '(*params)'
        for mode, pn, typ in reversed(d['params']):
            offs.append(acc)
            acc = acc + '+' + self.slotc(mode, typ)
        offs.reverse()
        total = '+'.join(self.slotc(m, t) for m, _, t in d['params'])
        pre = []; post = []; args = []; ni = 0; nr = 0; nstr = 0
        for (mode, pn, typ), off in zip(d['params'], offs):
            base = typ.split()[0]
            if typ == 'text':
                ni += 1; v = 'a%d' % ni
                pre.append('            ad = getadr(%s); valfil(ad); fn = getbyt(ad);' % off)
                pre.append('            if (fn <= commandfn) errore(FileModeIncorrect);')
                pre.append('            %s = fn;' % v)
                args.append('filtable[%s]' % v)
            elif typ == 'string':
                nstr += 1
                sv = 's' if nstr == 1 else 's%d' % nstr
                self.maxstr = max(self.maxstr, nstr)
                if mode == 'view':
                    pre.append('            getstr(%s, %s);' % (off, sv))
                else:
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = %s; /* var string base */' % (v, off))
                    pre.append('            %s[0] = 0;' % sv)
                    post.append('            putstr(%s, %s);' % (sv, v))
                args.append(sv)
            elif typ == 'real':
                nr += 1; v = 'r%d' % nr
                if mode in ('var', 'out'):
                    ni += 1; av = 'a%d' % ni
                    pre.append('            %s = getadr(%s);' % (av, off))
                    if mode == 'var':
                        pre.append('            %s = getrel(%s);' % (v, av))
                    args.append('&%s' % v)
                    post.append('            putrel(%s, %s);' % (av, v))
                else:
                    pre.append('            %s = getrel(%s);' % (v, off))
                    args.append(v)
            elif typ == 'boolean':
                ni += 1; v = 'a%d' % ni
                pre.append('            %s = getint(%s);' % (v, off))
                args.append(v)
            elif typ == 'char':
                ni += 1; v = 'a%d' % ni
                pre.append('            %s = getint(%s);' % (v, off))
                args.append('(char)%s' % v)
            elif base in self.module_enums():
                ni += 1; v = 'a%d' % ni
                pre.append('            %s = getint(%s); /* %s ordinal */' % (v, off, base))
                args.append(v)
            else:  # integer family
                if mode in ('var', 'out'):
                    ni += 1; av = 'a%d' % ni
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = getadr(%s);' % (av, off))
                    pre.append('            %s = 0;' % v)
                    args.append('&%s' % v)
                    post.append('            putint(%s, %s);' % (av, v))
                else:
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = getint(%s);' % (v, off))
                    args.append(v)
        self.maxint = max(self.maxint, ni); self.maxrel = max(self.maxrel, nr)
        call = 'ami_%s' % name
        call += '(' + ', '.join(args) + ')' if args else '()'
        out = ['        case %d: { /* %s@%s */' % (idx, name, sig[:36])]
        out += pre
        if isfunc:
            if d['ret'] == 'real':
                out.append('            r1 = %s;' % call)
                out += post
                if total:
                    out.append('            *params += %s;' % total)
                out.append('            putrel(*params, r1);')
            else:
                out.append('            rv = %s;' % call)
                out += post
                if total:
                    out.append('            *params += %s;' % total)
                out.append('            putint(*params, rv);')
        else:
            out.append('            %s;' % call)
            out += post
            if total:
                out.append('            *params += %s;' % total)
        out.append('        } break;')
        return out

    def _kind(self, mode, typ, base):
        """Classify a parameter so cgencase can stub the ones not yet ported."""
        if typ in ('string', 'text', 'real', 'boolean', 'char'):
            return 'scalar'
        if base == 'strptr':
            return 'string-list'
        if base == 'bytarr':
            return 'byte-array'
        if typ == 'evtrec':
            return 'event-record'
        if base in self.module_sets():
            return 'set'
        if base in HARDC:
            return {'menuptr': 'menu', 'imageptr': 'menu',
                    'widget': 'menu'}.get(base, 'list-struct')
        if base in self.module_enums():
            return 'scalar'
        # filrec/envrec list outputs come through as an integer-family address but
        # the routine (list/allenv/exece) needs struct marshalling
        if base in ('filptr', 'envptr') or typ in ('filptr', 'envptr'):
            return 'list-struct'
        return 'scalar'

    def _stub(self, idx, name, why):
        return ['        /* %s: %s not yet marshalled in C */' % (name, why),
                '        case %d: errore(FunctionNotImplemented); break;' % idx]

    def cexecutor(self):
        """Emit the C cases for this module's routines, numbered from base."""
        body = []
        for i, sym in enumerate(self.syms, start=1):
            body += self.cgencase(self.base + i - 1, sym)
        return body


HARDC = {'menuptr', 'imageptr', 'widget'}


def number(gens):
    """Assign each module a contiguous routine-number base in MODORDER, so the
       flat numbering matches source/exttables.pas."""
    n = 0
    for m in MODORDER:
        g = gens[m]
        g.base = n + 1
        n += len(g.syms)
    return n


def lookup_table(gens):
    """Emit LookupExternal: module+symbol name -> flat routine number."""
    out = ['void LookupExternal(symnam* module, symnam* symbol, int* routine)',
           '{', '', '    *routine = 0;', '']
    first = True
    for m in MODORDER:
        g = gens[m]
        kw = 'if' if first else 'else if'; first = False
        out.append('    %s (!strcmp(module, "%s")) {' % (kw, m))
        out.append('')
        for i, sym in enumerate(g.syms, start=g.base):
            nm = sym.split('@', 1)[0]
            out.append('        if (!strcmp(symbol, "%s")) *routine = %d;' % (nm, i))
        out.append('')
        out.append('    }')
    out += ['', '}', '']
    return out


def main():
    gens = {m: CGen(m) for m in MODORDER}
    total = number(gens)

    out = ['/* Generated by tools/extgen/gencexec.py; do not edit by hand. The',
           '   external executor and symbol table for the cmach interpreter. */',
           '']
    out += lookup_table(gens)
    out.append('int NumExternal(void) { return %d; }' % total)
    out.append('')
    out.append('void ExecuteExternal(int routine, address* params)')
    out.append('{')
    out.append('    long a1, a2, a3, a4, a5, a6, a7, a8, rv;')
    out.append('    double r1, r2;')
    out.append('    address ad, ad2;')
    out.append('    int fn;')
    # services, sound, network are hosted in plain cmach; terminal and graphics
    # are stubbed here (the cmacht/cmachg flavors will replace those cases).
    maxstr = max(gens[m].maxstr for m in ('services', 'sound', 'network'))
    out.append('    char %s;' % ', '.join('s%s[STRMAX]' % ('' if i == 1 else str(i))
                                          for i in range(1, maxstr + 1)))
    out.append('')
    out.append('    switch (routine) {')
    out.append('')
    for m in ('services', 'sound', 'network'):
        out.append('    /* ---- %s ---- */' % m)
        out += gens[m].cexecutor()
    out.append('')
    out.append('    default: errore(FunctionNotImplemented);')
    out.append('')
    out.append('    }')
    out.append('}')
    out.append('')

    dst = os.path.join(ROOT, 'source/cmach/extern.inc')
    open(dst, 'w').write('\n'.join(out) + '\n')

    for m in MODORDER:
        g = gens[m]
        print('%s: %d cases (base %d), %d stubs' %
              (m, len(g.syms), g.base, len(g.stubs)))
        for i, n, r in g.stubs:
            print('   stub %d %s (%s)' % (i, n, r))


if __name__ == '__main__':
    main()
