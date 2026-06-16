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
import os, sys, re

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, HERE)

from genpexec import Gen, toks  # shared spec parsing


def void_natives(module):
    """Names (without the ami_ prefix) of native functions that return void, read
       from amitk/include/<module>.h. A Pascal routine declared as a function but
       backed by a void ami_* is a binding mismatch (the Pascal form synthesizes
       the result through a wrapper); such routines are stubbed here for now."""
    path = os.path.join(ROOT, 'amitk', 'include', module + '.h')
    if not os.path.exists(path):
        return set()
    txt = open(path).read()
    return set(re.findall(r'\bvoid\s+ami_([a-z0-9]+)\s*\(', txt))

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
              'cert', 'list-struct', 'callback', 'pstring'}

# function return types that are not a scalar the executor can store with
# putint/putrel: a pstring/string return needs VM-heap allocation (the native
# binding has no ami_* for it -- these are Pascal-side overloads), so the routine
# is stubbed until that marshalling is added.
NONSCALARRET = {'pstring', 'string'}

# routines whose native call is the wrapper_<name> binding (a returned FILE*
# bound to a pair of var text files) rather than ami_<name>.
WRAPPED = {'opennet', 'opennetv6', 'waitnet', 'openwin'}


class CGen(Gen):
    """Emit the per-routine executor cases as C for cmach."""

    def __init__(self, module):
        super().__init__(module)
        self.voidnat = void_natives(module)

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
        # the no-file writetime/writedate overloads map to a native that requires
        # a FILE* the Pascal form supplies internally (default output) -- not yet
        # marshalled here
        if name in ('writetime', 'writedate') and not hasfile:
            self.stubs.append((idx, name, 'no-file form needs default output'))
            return self._stub(idx, name, 'no-file %s' % name)
        # the pstring-returning getenv overload: fetch into a buffer with the
        # native getenv, then allocate a vm heap string and return its address
        if self.module == 'services' and name == 'getenv' and isfunc:
            self.maxstr = max(self.maxstr, 2)
            return self._getenv(idx)
        d = self.match_decl(name, hasfile, len(ptoks))
        if d is None:
            self.stubs.append((idx, name, 'no interface match'))
            return ['        case %d: errore(FUNCTIONNOTIMPLEMENTED); break;' % idx]
        # a non-scalar (pstring/string) function return needs VM-heap marshalling
        if isfunc and d['ret'] in NONSCALARRET:
            self.stubs.append((idx, name, 'returns %s' % d['ret']))
            return self._stub(idx, name, 'non-scalar return %s' % d['ret'])
        # a Pascal function backed by a void native is a wrapper-synthesized
        # result the C side does not reproduce yet
        if isfunc and name in self.voidnat:
            self.stubs.append((idx, name, 'native ami_%s is void' % name))
            return self._stub(idx, name, 'function maps to void ami_%s' % name)
        # reject parameter kinds not yet handled in C
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
        pre = []; post = []; args = []; ni = 0; nr = 0; nstr = 0; filevars = []
        for (mode, pn, typ), off in zip(d['params'], offs):
            base = typ.split()[0]
            if typ == 'text':
                ni += 1; v = 'a%d' % ni
                if name in WRAPPED:
                    # the binding wrapper takes a pasfil* (pointer to the file
                    # variable in store) and attaches the opened connection into
                    # the file table itself, via the STDIO_BYPASS bridge
                    pre.append('            %s = getadr(%s); /* file var address */' % (v, off))
                    args.append('(unsigned char*)(store+%s)' % v)
                else:
                    pre.append('            ad = getadr(%s); valfil(ad); fn = getbyt(ad);' % off)
                    pre.append('            if (fn <= COMMANDFN) errore(FILEMODEINCORRECT);')
                    pre.append('            %s = fn;' % v)
                    args.append('filtable[%s]' % v)
                filevars.append(v)
            elif typ == 'string':
                # native ABI: a view string passes the buffer alone; a var/out
                # string passes (buffer, length) -- the length is the VM string's
                # logical size, the buffer the native may fill.
                nstr += 1
                sv = 's' if nstr == 1 else 's%d' % nstr
                self.maxstr = max(self.maxstr, nstr)
                if mode == 'view':
                    pre.append('            getstr(%s, %s);' % (off, sv))
                    args.append(sv)
                else:
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = %s; /* var string descriptor */' % (v, off))
                    pre.append('            %s[0] = 0;' % sv)
                    args.append(sv)
                    args.append('getint(%s+PTRSIZE)' % off)
                    post.append('            putstr(%s, %s);' % (sv, v))
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
        # The file-binding routines (a native FILE* returned and bound to a pair
        # of var text files) go through a C wrapper with the Pascal-shaped
        # signature -- wrapper_<name>(infile, outfile, ...) in network_support.c --
        # not the raw ami_<name>. Everything else calls ami_<name> directly.
        prefix = 'wrapper_' if name in WRAPPED else 'ami_'
        call = '%s%s' % (prefix, name)
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
            # the wrapper's bindnet attaches the connection files and sets their
            # read/write state through the bridge, so nothing more is needed here
            out += post
            if total:
                out.append('            *params += %s;' % total)
        out.append('        } break;')
        return out

    def _kind(self, mode, typ, base):
        """Classify a parameter so cgencase can stub the ones not yet ported."""
        if typ in ('string', 'text', 'real', 'boolean', 'char'):
            return 'scalar'
        if base == 'pstring':
            return 'pstring'
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

    def _getenv(self, idx):
        """getenv(view ls): pstring -- native ami_getenv(ls, ds, dsl) into a
           buffer, then a vm heap string [len][chars] whose address is the
           returned pstring."""
        return [
            '        case %d: { /* getenv@f_vc -> pstring */' % idx,
            '            long l, i; address sa;',
            '            getstr((*params), s);',
            '            ami_getenv(s, s2, STRMAX);',
            '            l = 0; while (s2[l]) l++; /* strlen */',
            '            newspc(INTSIZE+l, &sa); /* heap string */',
            '            putint(sa, l);',
            '            for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, s2[i]);',
            '            *params += (PTRSIZE+INTSIZE);',
            '            putadr(*params, sa);',
            '        } break;',
        ]

    def _stub(self, idx, name, why):
        return ['        /* %s: %s not yet marshalled in C */' % (name, why),
                '        case %d: errore(FUNCTIONNOTIMPLEMENTED); break;' % idx]

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


# C marshalling helpers emitted into extern.inc. STRMAX matches the Pascal
# generator's strmax (large enough for a PEM certificate returned through a var
# string). getstr loads a VM string parameter into a zero-terminated C buffer
# (inputs zero-terminate); putstr stores a C buffer back into a var string,
# space-padded (outputs space-pad, no terminator) -- the Ami string convention.
C_HELPERS = '''#define STRMAX 10000 /* C string buffer size for marshalling */

/* load a VM string parameter (descriptor at sa) into a zero-terminated buffer */
static void getstr(address sa, char* s)
{
    address ad; long l, i;
    ad = getadr(sa);          /* base address of string */
    l = getint(sa+PTRSIZE);   /* logical length */
    if (l > STRMAX-1) l = STRMAX-1;
    for (i = 0; i < l; i++) s[i] = getchr(ad+i);
    s[l] = 0;                 /* terminate */
}

/* store a zero-terminated buffer back into a VM var string (descriptor at sa),
   space-padded to the string's logical length */
static void putstr(char* s, address sa)
{
    address ad; long l, i;
    ad = getadr(sa);          /* base address of string */
    l = getint(sa+PTRSIZE);   /* logical length */
    for (i = 0; i < l; i++) putchr(ad+i, ' ');
    for (i = 0; i < l && s[i]; i++) putchr(ad+i, s[i]);
}'''


def main():
    gens = {m: CGen(m) for m in MODORDER}
    total = number(gens)

    # services, sound, network are hosted in plain cmach; terminal and graphics
    # are stubbed here (the cmacht/cmachg flavors will replace those cases).
    hosted = ('services', 'sound', 'network')
    bodies = {m: gens[m].cexecutor() for m in hosted}  # populates max counts
    maxint = max(gens[m].maxint for m in hosted)
    maxrel = max(gens[m].maxrel for m in hosted)
    maxstr = max(gens[m].maxstr for m in hosted)

    out = ['/* Generated by tools/extgen/gencexec.py; do not edit by hand. The',
           '   external executor for the cmach interpreter.',
           '',
           '   cmach runs a machine deck pre-assembled by pint --machdeck, in which',
           '   external references are already resolved to the external vector',
           '   (extvecbase+routine-1), so there is no load-time LookupExternal --',
           '   only ExecuteExternal, dispatched on the flat routine number whose',
           '   order matches source/exttables.pas (MODORDER). */',
           '', C_HELPERS, '']
    out.append('int NumExternal(void) { return %d; }' % total)
    out.append('')
    out.append('void ExecuteExternal(int routine, address* params)')
    out.append('{')
    out.append('    long %s, rv;' % ', '.join('a%d' % i for i in range(1, maxint + 1)))
    out.append('    double %s;' % ', '.join('r%d' % i for i in range(1, maxrel + 1)))
    out.append('    address ad, ad2;')
    out.append('    int fn;')
    out.append('    char %s;' % ', '.join('s%s[STRMAX]' % ('' if i == 1 else str(i))
                                          for i in range(1, maxstr + 1)))
    out.append('')
    out.append('    switch (routine) {')
    out.append('')
    for m in hosted:
        out.append('    /* ---- %s ---- */' % m)
        out += bodies[m]
    out.append('')
    out.append('    default: errore(FUNCTIONNOTIMPLEMENTED);')
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
