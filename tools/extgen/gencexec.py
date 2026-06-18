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


def file_natives(module):
    """Names (without ami_) of native functions taking a FILE* as their first
       argument, from amitk/include/<module>.h. Model (terminal/graphics) routines
       operate on a terminal/window FILE*; their no-file Pascal overload defaults
       it to the console, so the C call must prepend the default FILE* -- but only
       for routines that actually take one (global settings like autohold do not)."""
    path = os.path.join(ROOT, 'amitk', 'include', module + '.h')
    if not os.path.exists(path):
        return set()
    txt = open(path).read()
    return set(re.findall(r'\bami_([a-z0-9]+)\s*\(\s*FILE', txt))

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
CSTUBKINDS = {'menu', 'cert', 'list-struct', 'callback'}

# function return types that are not a scalar the executor can store with
# putint/putrel: a pstring/string return needs VM-heap allocation (the native
# binding has no ami_* for it -- these are Pascal-side overloads), so the routine
# is stubbed until that marshalling is added.
NONSCALARRET = {'pstring', 'string'}

# routines whose native call is the wrapper_<name> binding (a returned FILE*
# bound to a pair of var text files) rather than ami_<name>.
WRAPPED = {'opennet', 'opennetv6', 'waitnet', 'openwin'}

# Routines whose Pascaline (and .syms) name differs from the native ami_ name:
# the Pascal binding bridges them through a wrapper (graphics_wrapper.c) but the
# underlying native is ami_<value>. cmach calls the native directly, so it must
# use the real name. (graphics blor/flor -> ami_bor/ami_for.)
NAME_REMAP = {('graphics', 'blor'): 'bor', ('graphics', 'flor'): 'for'}


class CGen(Gen):
    """Emit the per-routine executor cases as C for cmach."""

    def __init__(self, module):
        super().__init__(module)
        self.voidnat = void_natives(module)
        self.filenat = file_natives(module)
        self.maxby = 0  # max open-byte-array params in any one routine
        self.usesevt = False  # module has an event-record (event/sendevent)

    def emit_evtprocs(self):
        """Emit C putevt/getevt for this module's event record, from the digest
           (self.evcodes/self.evvars), mirroring genpexec.Gen.evtprocs. The native
           ami_evtrec field names match the digest; the native enum is ami_<code>;
           the vm record is winid@0 handled@8 etype@9 + variant fields at the
           digest offsets."""
        self.collect_types()
        if not self.evcodes:
            return []
        def body(getmode):
            out = []
            for tag in sorted(self.evvars):
                fields = self.evvars[tag]
                if not fields:
                    continue
                code = self.evcodes[tag]
                sets = []
                for fn, off, ft in fields:
                    acc = 'getbyt' if ft in ('c', 'b') else 'getint'
                    put = 'putbyt' if ft in ('c', 'b') else 'putint'
                    if getmode:
                        sets.append('er->%s = %s(ad+%d);' % (fn, acc, off))
                    else:
                        sets.append('%s(ad+%d, er->%s);' % (put, off, fn))
                out.append('    case ami_%s: %s break;' % (code, ' '.join(sets)))
            return out
        out = ['/* event record marshalling: native ami_evtrec <-> vm event record',
               '   (winid@0, handled@8, etype@9 tag, variant fields per the digest) */',
               'static void putevt(ami_evtrec* er, address ad)',
               '{',
               '    putint(ad, er->winid);',
               '    putbyt(ad+8, er->handled);',
               '    putbyt(ad+9, er->etype);',
               '    switch (er->etype) {']
        out += body(False)
        out += ['    default: break;',
                '    }',
                '}',
                '',
                'static void getevt(address ad, ami_evtrec* er)',
                '{',
                '    er->winid = getint(ad);',
                '    er->handled = getbyt(ad+8);',
                '    er->etype = getbyt(ad+9);',
                '    switch (er->etype) {']
        out += body(True)
        out += ['    default: break;',
                '    }',
                '}']
        return out

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
        if name in ('eventover', 'eventsover'):
            return self._stub(idx, name, 'callback routine')
        if self.module == 'graphics':
            # menu(file, list) and stdmenu marshal a native menu list (getmenu/
            # putmenu); the bare menu(list) with no file stays stubbed, matching
            # pmachg's single stub.
            if name == 'menu' and hasfile:
                return self._menu(idx)
            if name == 'stdmenu':
                return self._stdmenu(idx)
            if name == 'menu':
                return self._stub(idx, name, 'no-file menu overload')
        # the no-file writetime/writedate overloads write the time/date to the
        # interpreter's default output (the ami stdout cmach writes through)
        if name in ('writetime', 'writedate') and not hasfile:
            return ['        case %d: { /* %s(t) -- default output */' % (idx, name),
                    '            ami_%s(stdout, getint(*params));' % name,
                    '            *params += INTSIZE;',
                    '        } break;']
        # the pstring-returning getenv overload: fetch into a buffer with the
        # native getenv, then allocate a vm heap string and return its address
        if self.module == 'services' and name == 'getenv' and isfunc:
            self.maxstr = max(self.maxstr, 2)
            return self._getenv(idx)
        # services routines whose C marshalling is hand-written (struct lists,
        # attr/perm sets, pstring returns, environment lists). Each mirrors the
        # corresponding case in source/extlink.pas (the Pascal executor pint and
        # pmach run), so cmach is byte-identical to them. Dispatched here, before
        # the generic path stubs the set/list-struct/pstring kinds.
        if self.module == 'services':
            self.maxstr = max(self.maxstr, 1)
            if name in ('writetime', 'writedate') and hasfile:
                return self._writetf(idx, name)
            if name == 'list':
                return self._list(idx, ptoks)
            if name in ('times', 'dates') and isfunc:
                return self._pstrret(idx, name, hasint=True)
            if name == 'getcur' and isfunc:
                return self._pstrret(idx, name, hasint=False)
            if name in ('setatr', 'resatr', 'setuper', 'resuper', 'setgper',
                        'resgper', 'setoper', 'resoper'):
                return self._setperm(idx, name, ptoks)
            if name == 'filchr':
                return self._filchr(idx)
            if name == 'allenv':
                return self._allenv(idx)
            if name in ('exece', 'execew'):
                return self._exece(idx, name, ptoks)
            # fulnam(var fn: string) is in-out: the native reads the name to
            # expand (the generic var-string handler would blank it, and
            # ami_fulnam errors on an empty spec). Read, expand in place, write
            # back -- mirrors extlink case 54.
            if name == 'fulnam' and not isfunc:
                return ['        case %d: { /* fulnam(var fn) -- expand in place */' % idx,
                        '            ad = (*params);',
                        '            getstr(ad, s);',
                        '            ami_fulnam(s, STRMAX);',
                        '            putstr(s, ad);',
                        '            *params += (PTRSIZE+INTSIZE);',
                        '        } break;']
            # pstring-returning function overloads backed by void in-place natives
            if name == 'fulnam' and isfunc:
                return self._funpstr(idx, 'fulnam', ptoks)
            if name in ('getpgm', 'getusr') and isfunc:
                return self._funpstr(idx, name, [])
            if name == 'maknam' and isfunc:
                return self._funpstr(idx, 'maknam', ptoks)
            # brknam with pstring out parameters (path/name/ext)
            if name == 'brknam' and 'pvc' in ptoks:
                return self._brknam(idx, ptoks)
        # network certificate-list routines: native ami_certlist* build a certptr
        # tree, marshalled to vm memory (mirrors genpexec certlist handling)
        if self.module == 'network':
            if name == 'certlistnet':
                return self._certlistnet(idx)
            if name == 'certlistmsg':
                return self._certlistmsg(idx)
            if name == 'certlistfree':
                return self._certlistfree(idx)
        d = self.match_decl(name, hasfile, ptoks)
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
        pre = []; post = []; args = []
        ni = 0; nr = 0; nstr = 0; nby = 0; filevars = []
        for (mode, pn, typ), off in zip(d['params'], offs):
            base = typ.split()[0]
            if typ == 'text':
                if self.module == 'terminal' and not self.graphfiles:
                    # terminal console: event reads the interpreter input, every
                    # other terminal routine writes the interpreter output (the
                    # ami stdin/stdout cmach is built through). Mirrors genpexec's
                    # terminal file handling (input/output), not filtable[fn].
                    pre.append('            ad = getadr(%s); valfil(ad); fn = getbyt(ad);' % off)
                    if name == 'event':
                        pre.append('            if (fn != INPUTFN && fn > COMMANDFN) errore(FILEMODEINCORRECT);')
                        args.append('stdin')
                    else:
                        pre.append('            if (fn > COMMANDFN) errore(FILEMODEINCORRECT);')
                        args.append('stdout')
                elif name in WRAPPED:
                    # the binding wrapper takes a pasfil* (pointer to the file
                    # variable in store) and attaches the opened connection into
                    # the file table itself, via the STDIO_BYPASS bridge
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = getadr(%s); /* file var address */' % (v, off))
                    args.append('(unsigned char*)(store+%s)' % v)
                    filevars.append(v)
                else:
                    # a general file (services/sound/network files, and the hosted
                    # window files for terminal/graphics under the graphics flavor)
                    ni += 1; v = 'a%d' % ni
                    pre.append('            ad = getadr(%s); valfil(ad); fn = getbyt(ad);' % off)
                    pre.append('            if (fn <= COMMANDFN) errore(FILEMODEINCORRECT);')
                    pre.append('            %s = fn;' % v)
                    args.append('filtable[%s]' % v)
                    filevars.append(v)
            elif typ == 'evtrec':
                # the event record: event() fills the native record then putevt
                # marshals it to the vm record; sendevent() reads the vm record
                # into the native record with getevt then sends it. The vm record
                # address is the by-reference param slot.
                pre.append('            ad2 = getadr(%s);' % off)
                args.append('&er')
                if name == 'sendevent':
                    pre.append('            getevt(ad2, &er);')
                else:
                    post.append('            putevt(&er, ad2);')
                self.usesevt = True
            elif typ == 'string':
                # native ABI: a view string passes the buffer alone; a var/out
                # string passes (buffer, length). The length is the capacity of
                # the C buffer the native fills (STRMAX) -- the same convention the
                # Pascal executor uses, where the buffer is a strmax-sized str. It
                # is NOT the VM string's own length: the native must be told how
                # big the scratch buffer is, and putstr clamps the copy-back to the
                # VM string's capacity afterward.
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
                    args.append('STRMAX')
                    post.append('            putstr(%s, %s);' % (sv, v))
            elif base == 'pstring':
                # a pstring view/value parameter: load its inline [len][base]
                # descriptor with getpstr and pass the buffer as the native char*
                # (the ami_* pstring forms take a bare char*). Shares the string
                # buffer counter so multi-string overloads get distinct buffers.
                # The slot is adrsize (Gen.slot), so the offset and final advance
                # follow automatically.
                nstr += 1
                sv = 's' if nstr == 1 else 's%d' % nstr
                self.maxstr = max(self.maxstr, nstr)
                pre.append('            getpstr(%s, %s);' % (off, sv))
                args.append(sv)
            elif base == 'bytarr':
                # open byte array: descriptor (base address, logical length) in a
                # strparsiz slot. view copies the VM bytes into a malloc'd buffer
                # and passes (ptr, len); var allocates an unread buffer the native
                # fills, then copies it back. Mirrors genpexec getabyte/newabyte+
                # putabyte. The native arg shapes differ by module: sound is
                # (byte*, int), network (void*, unsigned long).
                nby += 1
                bv = 'b%d' % nby; lv = 'bl%d' % nby
                if self.module == 'network':
                    ptrc = '(void*)%s' % bv; lenc = '(unsigned long)%s' % lv
                else:
                    ptrc = bv; lenc = '(int)%s' % lv
                if mode == 'view':
                    pre.append('            %s = getabyte(%s, &%s);' % (lv, off, bv))
                    args.append(ptrc); args.append(lenc)
                    post.append('            free(%s);' % bv)
                else:
                    ni += 1; av = 'a%d' % ni
                    pre.append('            %s = %s; /* bytarr descriptor */' % (av, off))
                    pre.append('            %s = newabyte(%s, &%s);' % (lv, off, bv))
                    args.append(ptrc); args.append(lenc)
                    post.append('            putabyte(%s, %s, %s);' % (av, bv, lv))
            elif base in self.module_sets():
                # a module set (graphics: winmodset/qf*opts -- int bitmasks). A
                # view/value set sits INLINE at the param slot; getlongset folds
                # its bytes to the native int (elements < 8 -> byte 0 is the
                # bitmask). A var/out set is passed BY REFERENCE: getadr -> the
                # set's address, the native fills an int there, putlongset writes
                # it back. Mirrors genpexec's set handling (services sets are
                # special-cased earlier and do not reach here).
                if mode in ('var', 'out'):
                    ni += 1; av = 'a%d' % ni
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = getadr(%s);' % (av, off))
                    if mode == 'var':
                        pre.append('            %s = getlongset(%s);' % (v, av))
                    else:
                        pre.append('            %s = 0;' % v)
                    args.append('(void*)&%s' % v)
                    post.append('            putlongset(%s, %s);' % (av, v))
                else:
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = getlongset(%s);' % (v, off))
                    args.append('(int)%s' % v)
            elif base == 'strptr':
                # an interpreted string list (next:0, str pstring:8) converts to a
                # native ami_strptr list for the list/drop/tab widgets. getstrlst
                # builds it; the native keeps it (genpexec does not free), so it is
                # passed inline. Mirrors genpexec getstrlst.
                ni += 1; v = 'a%d' % ni
                pre.append('            %s = getadr(%s); /* string list */' % (v, off))
                args.append('getstrlst(%s)' % v)
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
                    # the out-integer slot is a long, but the native takes the
                    # exact width (int*, unsigned long long*, ...): pass it as
                    # void* (a long, pre-zeroed, holds the result on a little-
                    # endian target) so the call is not a pointer-type mismatch.
                    args.append('(void*)&%s' % v)
                    post.append('            putint(%s, %s);' % (av, v))
                else:
                    ni += 1; v = 'a%d' % ni
                    pre.append('            %s = getint(%s);' % (v, off))
                    args.append(v)
        self.maxint = max(self.maxint, ni); self.maxrel = max(self.maxrel, nr)
        self.maxby = max(self.maxby, nby)
        # The terminal/graphics model routines always take a FILE* first in the
        # native ami_* ABI (ami_cursor(FILE*,...), ami_up(FILE*), ami_event(FILE*,
        # ...)), even though the Pascal no-file overload (e.g. cursor(x,y)) carries
        # no file -- the Pascal module defaults it internally. So for a no-file
        # form whose native takes a FILE* (name in self.filenat; global routines
        # like autohold/getwinid/query* take none and are excluded), prepend the
        # default: event reads the standard input, every other writer the standard
        # output. Plain terminal flavor -> the console (stdin/stdout cmach is built
        # through); graphics flavor (graphics module, and terminal hosted under it
        # via graphfiles) -> the hosted window files vmhost bound as the standard
        # files, filtable[vmstdin]/filtable[vmstdout].
        if not hasfile and NAME_REMAP.get((self.module, name), name) in self.filenat:
            ising = name == 'event'
            if self.module == 'graphics' or \
               (self.module == 'terminal' and self.graphfiles):
                args.insert(0, 'filtable[vmstdin]' if ising
                            else 'filtable[vmstdout]')
            elif self.module == 'terminal':
                args.insert(0, 'stdin' if ising else 'stdout')
        # The file-binding routines (a native FILE* returned and bound to a pair
        # of var text files) go through a C wrapper with the Pascal-shaped
        # signature -- wrapper_<name>(infile, outfile, ...) in network_support.c --
        # not the raw ami_<name>. Everything else calls ami_<name> directly.
        prefix = 'wrapper_' if name in WRAPPED else 'ami_'
        cname = NAME_REMAP.get((self.module, name), name)
        call = '%s%s' % (prefix, cname)
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

    def _writetf(self, idx, name):
        """writetime/writedate(var f: text; t): the file form. Standard files
           route to the interpreter output (output/error/list) or prr; general
           files to filtable[fn]. Read-only files error. Mirrors extlink case 7/9.
           output/error/list go to the ami stdout cmach already writes through."""
        return [
            '        case %d: { /* %s(var f: text; t) */' % (idx, name),
            '            long t;',
            '            ad = getadr((*params)+INTSIZE); valfil(ad); fn = getbyt(ad);',
            '            t = getint(*params);',
            '            if (fn <= COMMANDFN) {',
            '                if (fn == OUTPUTFN || fn == ERRORFN || fn == LISTFN)',
            '                    ami_%s(stdout, t);' % name,
            '                else if (fn == PRRFN) ami_%s(filtable[PRRFN], t);' % name,
            '                else errore(WRITEONREADONLYFILE);',
            '            } else {',
            '                if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);',
            '                ami_%s(filtable[fn], t);' % name,
            '            }',
            '            *params += INTSIZE+ADRSIZE;',
            '        } break;',
        ]

    def _list(self, idx, ptoks):
        """list(view f: string/pstring; var l: filptr): native ami_list fills a
           filrec linked list, cvtflist marshals it to vm memory. extlink cases
           1 (string) and 2 (pstring); the var l cell is the low param, f above."""
        isview = ptoks[0] == 'vc'
        load = ('getstr((*params)+ADRSIZE, s);' if isview
                else 'getpstr((*params)+ADRSIZE, s);')
        adv = 'PTRSIZE+(PTRSIZE+INTSIZE)' if isview else 'PTRSIZE+PTRSIZE'
        return [
            '        case %d: { /* list -- directory list into a filrec list */' % idx,
            '            ami_filrec* fp;',
            '            %s' % load,
            '            ami_list(s, &fp);',
            '            putadr(getadr(*params), cvtflist(fp)); /* into the var l cell */',
            '            *params += %s;' % adv,
            '        } break;',
        ]

    def _pstrret(self, idx, name, hasint):
        """times/dates/getcur function overloads: the native fills a buffer, then
           a vm heap string [len][chars] is built and its address returned as the
           pstring result. extlink cases 4 (times), 6 (dates), 39 (getcur)."""
        if hasint:
            return [
                '        case %d: { /* %s(t): pstring */' % (idx, name),
                '            long t;',
                '            t = getint(*params);',
                '            ami_%s(s, STRMAX, t);' % name,
                '            *params += INTSIZE; /* consume the int input */',
                '            putadr(*params, mkpstr(s));',
                '        } break;',
            ]
        return [
            '        case %d: { /* %s: pstring (no input) */' % (idx, name),
            '            ami_%s(s, STRMAX);' % name,
            '            putadr(*params, mkpstr(s)); /* no param advance */',
            '        } break;',
        ]

    def _setperm(self, idx, name, ptoks):
        """setatr/resatr/setuper.../resoper(view fn; a: attr/perm set): the set is
           the low param (passed by reference), folded to the native long bitmask;
           the filename follows. extlink cases 60..77."""
        isview = ptoks[0] == 'vc'
        load = ('getstr((*params)+PTRSIZE, s);' if isview
                else 'getpstr((*params)+PTRSIZE, s);')
        adv = 'PTRSIZE+(PTRSIZE+INTSIZE)' if isview else 'PTRSIZE+PTRSIZE'
        return [
            '        case %d: { /* %s -- filename + attr/perm set */' % (idx, name),
            '            long m;',
            '            %s' % load,
            '            m = getlongset(getadr(*params)); /* set by ref at +0 */',
            '            ami_%s(s, m);' % name,
            '            *params += %s;' % adv,
            '        } break;',
        ]

    def _filchr(self, idx):
        """filchr(out fc: schar): the native fills a 256-bit character set; it is
           layout-identical to a vm set, copied straight back. extlink case 82."""
        return [
            '        case %d: { /* filchr -- filename character set (out) */' % idx,
            '            ami_chrset cs; settype st; long i;',
            '            ami_filchr(cs);',
            '            for (i = 0; i < SETSIZE; i++) st[i] = cs[i];',
            '            putset(getadr(*params), st);',
            '            *params += PTRSIZE;',
            '        } break;',
        ]

    def _allenv(self, idx):
        """allenv(var el: envptr): native ami_allenv fills an envrec list,
           cvtenvlist marshals it to vm memory. extlink case 27."""
        return [
            '        case %d: { /* allenv -- environment list (out) */' % idx,
            '            ami_envrec* ep;',
            '            ami_allenv(&ep);',
            '            putadr(getadr(*params), cvtenvlist(ep)); /* into the var el cell */',
            '            *params += PTRSIZE;',
            '        } break;',
        ]

    def _exece(self, idx, name, ptoks):
        """exece/execew(view cmd; el: envptr [; out e: integer]): the vm env list
           is rebuilt as a native envrec list and passed to the native exec.
           extlink cases 32/33 (exece) and 36/37 (execew)."""
        isview = ptoks[0] == 'vc'
        iew = name == 'execew'
        if iew:  # params: e@0, el@ADRSIZE, cmd@ADRSIZE*2
            cmd_off, el_off = '(*params)+ADRSIZE*2', '(*params)+ADRSIZE'
            adv = ('PTRSIZE+PTRSIZE+(PTRSIZE+INTSIZE)' if isview
                   else 'PTRSIZE+PTRSIZE+PTRSIZE')
        else:    # params: el@0, cmd@ADRSIZE
            cmd_off, el_off = '(*params)+ADRSIZE', '(*params)'
            adv = 'PTRSIZE+(PTRSIZE+INTSIZE)' if isview else 'PTRSIZE+PTRSIZE'
        load = ('getstr(%s, s);' if isview else 'getpstr(%s, s);') % cmd_off
        out = ['        case %d: { /* %s -- exec with environment list */' % (idx, name),
               '            ami_envrec* ep;']
        if iew:
            out.append('            int rc;')
        out.append('            %s' % load)
        out.append('            ep = buildenvlist(getadr(%s));' % el_off)
        if iew:
            out.append('            ami_execew(s, ep, &rc);')
        else:
            out.append('            ami_exece(s, ep);')
        out.append('            freeenvlist(ep);')
        if iew:
            out.append('            putint(getadr(*params), rc); /* out status */')
        out.append('            *params += %s;' % adv)
        out.append('        } break;')
        return out

    def _funpstr(self, idx, name, ptoks):
        """pstring-returning function overloads backed by a void in-place native:
           ami_fulnam/ami_maknam expand/fill a STRMAX buffer, ami_getpgm/ami_getusr
           fill one. Load each input at its slot, advance past the inputs, then
           return mkpstr(result-buffer). Mirrors extlink cases 46-53/55/57/59 and
           the _pstrret/_getenv pattern. vc input slot = strparsiz, pvc = adrsize."""
        def slot(tok):
            return '(PTRSIZE+INTSIZE)' if tok == 'vc' else 'ADRSIZE'
        offs = []; acc = '(*params)'
        for tok in reversed(ptoks):
            offs.append(acc); acc = acc + '+' + slot(tok)
        offs.reverse()
        total = '+'.join(slot(t) for t in ptoks)
        out = ['        case %d: { /* %s -> pstring (void-native in-place) */'
               % (idx, name)]
        if name == 'maknam':
            self.maxstr = max(self.maxstr, 4)
            for tok, off, buf in zip(ptoks, offs, ['s2', 's3', 's4']):
                ld = 'getstr' if tok == 'vc' else 'getpstr'
                out.append('            %s(%s, %s);' % (ld, off, buf))
            out.append('            ami_maknam(s, STRMAX, s2, s3, s4);')
        elif name == 'fulnam':
            out.append('            getstr(%s, s);' % offs[0])
            out.append('            ami_fulnam(s, STRMAX);')
        else:  # getpgm / getusr -- no input, native fills s
            out.append('            ami_%s(s, STRMAX);' % name)
        if total:
            out.append('            *params += %s; /* past inputs */' % total)
        out.append('            putadr(*params, mkpstr(s));')
        out.append('        } break;')
        return out

    def _brknam(self, idx, ptoks):
        """brknam(view/pstring fn; out p, n, e: pstring): the native fills three
           buffers, each copied back as a VM heap pstring [len][chars] whose
           address is stored into the by-reference out cell. extlink cases 43/44.
           The input fn sits at the top slot, the three out-pstring cells (PTRSIZE
           each) below it: p@PTRSIZE*2, n@PTRSIZE, e@0. The advance is the true
           frame; for the pstring-fn overload that is PTRSIZE*4 (extlink case 44
           over-advances by INTSIZE -- a latent bug; cmach sets sp from this
           advance so it must be exact)."""
        self.maxstr = max(self.maxstr, 4)
        isview = ptoks[0] == 'vc'
        fnload = ('getstr((*params)+PTRSIZE*3, s);' if isview
                  else 'getpstr((*params)+PTRSIZE*3, s);')
        adv = 'PTRSIZE*3+(PTRSIZE+INTSIZE)' if isview else 'PTRSIZE*4'
        return [
            '        case %d: { /* brknam -- break filename into path/name/ext (out pstrings) */' % idx,
            '            %s' % fnload,
            '            s2[0] = 0; s3[0] = 0; s4[0] = 0;',
            '            ami_brknam(s, s2, STRMAX, s3, STRMAX, s4, STRMAX);',
            '            putpstr(s2, getadr((*params)+PTRSIZE*2)); /* path */',
            '            putpstr(s3, getadr((*params)+PTRSIZE));   /* name */',
            '            putpstr(s4, getadr((*params)));           /* ext  */',
            '            *params += %s;' % adv,
            '        } break;',
        ]

    def _certlistnet(self, idx):
        """certlistnet(var f: text; which: integer; var list: certptr): native
           ami_certlistnet fills an ami_certptr tree, putcertlist marshals it into
           vm memory (freeing the native side). params (low->high): var list
           cell@0, which@ADRSIZE, file var@ADRSIZE+INTSIZE."""
        self.maxint = max(self.maxint, 2)
        return [
            '        case %d: { /* certlistnet -- TLS net cert list */' % idx,
            '            ami_certptr cp;',
            '            ad = getadr((*params)+ADRSIZE+INTSIZE); valfil(ad); fn = getbyt(ad);',
            '            if (fn <= COMMANDFN) errore(FILEMODEINCORRECT);',
            '            a1 = fn;',
            '            a2 = getint((*params)+ADRSIZE); /* which */',
            '            ad2 = getadr(*params);          /* var list result cell */',
            '            cp = NULL;',
            '            ami_certlistnet(filtable[a1], a2, &cp);',
            '            putadr(ad2, putcertlist(cp));',
            '            *params += ADRSIZE+INTSIZE+ADRSIZE;',
            '        } break;',
        ]

    def _certlistmsg(self, idx):
        """certlistmsg(fn: integer; which: integer; var list: certptr): the message
           (datagram) connection form. params: var list cell@0, which@ADRSIZE,
           connection@ADRSIZE+INTSIZE."""
        self.maxint = max(self.maxint, 2)
        return [
            '        case %d: { /* certlistmsg -- TLS msg cert list */' % idx,
            '            ami_certptr cp;',
            '            a1 = getint((*params)+ADRSIZE+INTSIZE); /* connection */',
            '            a2 = getint((*params)+ADRSIZE);         /* which */',
            '            ad2 = getadr(*params);                  /* var list result cell */',
            '            cp = NULL;',
            '            ami_certlistmsg(a1, a2, &cp);',
            '            putadr(ad2, putcertlist(cp));',
            '            *params += ADRSIZE+INTSIZE+INTSIZE;',
            '        } break;',
        ]

    def _certlistfree(self, idx):
        """certlistfree(var list: certptr): free the vm cert tree and nil the cell."""
        return [
            '        case %d: { /* certlistfree -- free a VM cert list */' % idx,
            '            ad2 = getadr(*params);  /* var list cell */',
            '            freecertlist(getadr(ad2));',
            '            putadr(ad2, NILVAL);',
            '            *params += ADRSIZE;',
            '        } break;',
        ]

    def _menu(self, idx):
        """menu(var f: text; m: menuptr): attach a menu to a window. The VM menu
           list head is the low param, the file above it. Mirrors genpexec
           menu@p_fc_pr."""
        self.maxint = max(self.maxint, 1)
        return [
            '        case %d: { /* menu -- window menu */' % idx,
            '            a1 = getadr(*params); /* VM menu list head */',
            '            ad = getadr((*params)+ADRSIZE); valfil(ad); fn = getbyt(ad);',
            '            if (fn <= COMMANDFN) errore(FILEMODEINCORRECT);',
            '            ami_menu(filtable[fn], getmenu(a1));',
            '            *params += ADRSIZE+ADRSIZE;',
            '        } break;',
        ]

    def _stdmenu(self, idx):
        """stdmenu(sms: integer; var sm: menuptr; pm: menuptr): merge a standard
           menu set into pm, returning the combined list in sm. Mirrors genpexec
           stdmenu@p_i_pr_pr."""
        self.maxint = max(self.maxint, 2)
        return [
            '        case %d: { /* stdmenu -- standard menu */' % idx,
            '            ami_menuptr mnu;',
            '            a1 = getadr(*params);             /* parent (pm) list head */',
            '            ad2 = getadr((*params)+ADRSIZE);  /* var sm result cell */',
            '            a2 = getint((*params)+ADRSIZE*2); /* selection set */',
            '            mnu = NULL;',
            '            ami_stdmenu((ami_stdmenusel)a2, &mnu, getmenu(a1));',
            '            putadr(ad2, putmenu(mnu));',
            '            *params += ADRSIZE*2+INTSIZE;',
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
    while (l && s[l-1] == ' ') l--; /* trim trailing space padding, like the
                                       native binding's cstrz: a Pascaline view
                                       string (e.g. a packed array[1..100] of
                                       char whose descriptor length is the full
                                       capacity) is space-padded, and the Ami
                                       ami_* APIs strlen/strcat the buffer, so
                                       an untrimmed pad reaches maknam/open()
                                       and is taken literally. */
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
}

/* load an open byte-array parameter (descriptor at sa) into a freshly malloc'd
   buffer of the array's logical length; returns the length and sets *bufp. The
   caller frees after the native call. Mirrors genpexec getabyte. */
static long getabyte(address sa, byte** bufp)
{
    address ad; long l, i; byte* b;
    ad = getadr(sa);          /* base address of the array */
    l  = getint(sa+PTRSIZE);  /* logical length (array bound) */
    b  = malloc(l ? l : 1);   /* never malloc(0) */
    for (i = 0; i < l; i++) b[i] = getbyt(ad+i);
    *bufp = b;
    return l;
}

/* allocate an output byte-array buffer of the array's logical length WITHOUT
   reading the (possibly undefined) VM contents; zero-filled so the native sees
   defined bytes. Returns length, sets *bufp. Mirrors genpexec newabyte. */
static long newabyte(address sa, byte** bufp)
{
    long l; byte* b;
    l = getint(sa+PTRSIZE);   /* logical length (array bound) */
    b = malloc(l ? l : 1);
    memset(b, 0, l ? l : 1);
    *bufp = b;
    return l;
}

/* store an output byte-array buffer back into the VM array (descriptor at sa)
   and free it. Mirrors genpexec putabyte. */
static void putabyte(address sa, byte* buf, long l)
{
    address ad; long i;
    ad = getadr(sa);          /* base address of the array */
    for (i = 0; i < l; i++) putbyt(ad+i, buf[i]);
    free(buf);
}'''


# Services-specific marshalling helpers: structured parameter and return
# translation for the directory list, environment list, set and pstring
# routines. These mirror the Pascal helpers in source/extlink.pas (getpstr,
# cvtflist, cvtenv, getenv, set conversion) so cmach reproduces what pint and
# pmach do. They use the ami_filrec/ami_envrec/ami_chrset native types from
# services.h (always included before extern.inc in cmach).
C_SVC_HELPERS = '''/* load a pstring parameter into a zero-terminated buffer. A pstring parameter
   slot is the inline [length][base] descriptor extlink's getpstr reads (length
   at +0, base address at +INTSIZE) -- distinct from a pstring field inside a
   record, which points at a [length][chars] heap block. */
static void getpstr(address sa, char* s)
{
    address ad; long l, i;
    l = getadr(sa);            /* length */
    ad = getadr(sa+INTSIZE);   /* base address of chars */
    if (l > STRMAX-1) l = STRMAX-1;
    while (l > 0 && getchr(ad+l-1) == ' ') l--; /* trim trailing pad (cstrz), as
                                                   the wrappers do for pstring view
                                                   inputs too */
    for (i = 0; i < l; i++) s[i] = getchr(ad+i);
    s[l] = 0;
}

/* build a VM heap string [length:INTSIZE][chars] from a zero-terminated buffer,
   returning its address (the pstring value the caller stores or links). */
static address mkpstr(char* s)
{
    address sa; long l, i;
    l = 0; while (s[l]) l++;
    newspc(INTSIZE+l, &sa);
    putint(sa, l);
    for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, s[i]);
    return sa;
}

/* fold a VM set (at address a) to the native long bitmask: element b lives at
   byte b/8, bit b%8, so the low SETSIZE bytes little-endian form the long. The
   attr/perm elements are all < 8 (one byte), but the full 8 bytes are folded so
   the mapping is exact and width-faithful to extlink's union overlay. */
static long getlongset(address a)
{
    long v, i;
    v = 0;
    for (i = 0; i < 8; i++) v |= ((long)getbyt(a+i)) << (8*i);
    return v;
}

/* write a native long bitmask into a VM set (at address a): clear all SETSIZE
   bytes, then the low 8 bytes little-endian (the inverse of getlongset). */
static void putlongset(address a, long v)
{
    long i;
    for (i = 0; i < SETSIZE; i++) putbyt(a+i, 0);
    for (i = 0; i < 8; i++) putbyt(a+i, (v >> (8*i)) & 0xff);
}

/* convert a native ami_filrec list to a VM filrec linked list, freeing the
   native side; returns the VM head address (NILVAL if empty). Walks the record
   with a field cursor so the offsets follow the size constants. Mirrors
   extlink cvtflist. */
static address cvtflist(ami_filrec* fp)
{
    ami_filrec* tmp;
    address reca, ad, namea, topa, lasta, nexta;
    long nl, i;
    topa = 0; lasta = 0;
    while (fp != NULL) {
        newspc(PTRSIZE+INTSIZE+INTSIZE+SETSIZE+INTSIZE+INTSIZE+INTSIZE+INTSIZE+
               SETSIZE+SETSIZE+SETSIZE+PTRSIZE, &reca);
        if (topa == 0) topa = reca;            /* first record is the head */
        if (lasta != 0) putadr(lasta, reca);   /* link previous record's next */
        ad = reca;
        nl = 0; while (fp->name[nl]) nl++;      /* name string */
        newspc(INTSIZE+nl, &namea);
        putadr(ad, namea); ad += PTRSIZE;
        putint(namea, nl);
        for (i = 0; i < nl; i++) putbyt(namea+INTSIZE+i, fp->name[i]);
        putint(ad, fp->size); ad += INTSIZE;
        putint(ad, fp->alloc); ad += INTSIZE;
        putlongset(ad, fp->attr); ad += SETSIZE;
        putint(ad, fp->create); ad += INTSIZE;
        putint(ad, fp->modify); ad += INTSIZE;
        putint(ad, fp->access); ad += INTSIZE;
        putint(ad, fp->backup); ad += INTSIZE;
        putlongset(ad, fp->user); ad += SETSIZE;
        putlongset(ad, fp->group); ad += SETSIZE;
        putlongset(ad, fp->other); ad += SETSIZE;
        nexta = ad;                             /* the next field */
        putadr(nexta, NILVAL);
        lasta = nexta;
        tmp = fp; fp = fp->next;                /* free the native entry */
        free(tmp->name); free(tmp);
    }
    if (topa == 0) topa = NILVAL;
    return topa;
}

/* convert a native ami_envrec list to a VM envrec linked list, freeing the
   native side; returns the VM head address (NILVAL if empty). The envrec is
   name@0 data@PTRSIZE next@PTRSIZE*2, name/data pstring fields pointing at
   [length][chars] heap blocks. Mirrors extlink cvtenv. */
static address cvtenvlist(ami_envrec* ep)
{
    ami_envrec* tmp;
    address reca, ad, sa, topa, lasta, nexta;
    long l, i;
    topa = 0; lasta = 0;
    while (ep != NULL) {
        newspc(PTRSIZE*3, &reca);
        if (topa == 0) topa = reca;
        if (lasta != 0) putadr(lasta, reca);
        ad = reca;
        l = 0; while (ep->name[l]) l++;          /* name */
        newspc(INTSIZE+l, &sa);
        putadr(ad, sa); ad += PTRSIZE;
        putint(sa, l);
        for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, ep->name[i]);
        l = 0; while (ep->data[l]) l++;          /* data */
        newspc(INTSIZE+l, &sa);
        putadr(ad, sa); ad += PTRSIZE;
        putint(sa, l);
        for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, ep->data[i]);
        nexta = ad;
        putadr(nexta, NILVAL);
        lasta = nexta;
        tmp = ep; ep = ep->next;
        free(tmp->name); free(tmp->data); free(tmp);
    }
    if (topa == 0) topa = NILVAL;
    return topa;
}

/* build a native ami_envrec list from a VM envrec list (the head pointer in
   ad). The marshaller owns the native list and frees it after the call.
   Mirrors extlink's local getenv helper. */
static ami_envrec* buildenvlist(address ad)
{
    ami_envrec* ep; ami_envrec* lp; ami_envrec* np;
    address sa; long l, i;
    ep = NULL; lp = NULL;
    while (ad != NILVAL) {
        np = malloc(sizeof(ami_envrec));
        np->next = NULL;
        if (ep == NULL) ep = np;                 /* head */
        if (lp != NULL) lp->next = np;           /* link */
        sa = getadr(ad); ad += ADRSIZE;          /* name field */
        l = getint(sa); sa += INTSIZE;
        np->name = malloc(l+1);
        for (i = 0; i < l; i++) np->name[i] = getchr(sa+i);
        np->name[l] = 0;
        sa = getadr(ad); ad += ADRSIZE;          /* data field */
        l = getint(sa); sa += INTSIZE;
        np->data = malloc(l+1);
        for (i = 0; i < l; i++) np->data[i] = getchr(sa+i);
        np->data[l] = 0;
        lp = np;
        ad = getadr(ad);                         /* next field */
    }
    return ep;
}

/* free a native ami_envrec list built by buildenvlist */
static void freeenvlist(ami_envrec* ep)
{
    ami_envrec* tmp;
    while (ep != NULL) {
        tmp = ep; ep = ep->next;
        free(tmp->name); free(tmp->data); free(tmp);
    }
}

/* store a VM heap pstring [length:INTSIZE][chars] built from a zero-terminated
   buffer into the by-reference out cell da (da holds the address of the caller's
   pstring variable). Mirrors extlink putpstr. */
static void putpstr(char* s, address da)
{
    address sa; long l, i;
    l = 0; while (s[l]) l++;
    newspc(INTSIZE+l, &sa);
    putadr(da, sa);
    putint(sa, l);
    for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, s[i]);
}'''


# Network certificate-list translation. ami_certlistnet/msg build a native
# ami_certptr tree of ami_certfield records; putcertlist rebuilds it in VM memory
# (freeing the native side) and freecertlist frees the VM structure. The VM
# certfield record is the compiler's layout: name pstring@0, data pstring@8,
# critical@16, fork sublist@20, next@28, size 36; name/data are pstrings pointing
# at [length][chars] heap blocks. Mirrors genpexec CERTHELP. ami_certfield's
# name/data are char* (typedef string), zero-terminated and malloc'd by the
# binding.
C_NET_HELPERS = '''static address putcertlist(ami_certptr cp)
{
    address ad, sa; long l, i; ami_certptr lp;
    if (cp == NULL) return NILVAL;
    newspc(36, &ad);                       /* a certfield record */
    if (cp->name == NULL) putadr(ad, NILVAL);     /* name pstring at +0 */
    else {
        l = 0; while (cp->name[l]) l++;
        newspc(INTSIZE+l, &sa);
        putint(sa, l);
        for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, cp->name[i]);
        putadr(ad, sa);
    }
    if (cp->data == NULL) putadr(ad+8, NILVAL);   /* data pstring at +8 */
    else {
        l = 0; while (cp->data[l]) l++;
        newspc(INTSIZE+l, &sa);
        putint(sa, l);
        for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, cp->data[i]);
        putadr(ad+8, sa);
    }
    putbyt(ad+16, cp->critical ? 1 : 0);   /* critical at +16 */
    putadr(ad+20, putcertlist(cp->fork));  /* fork sublist at +20 */
    putadr(ad+28, putcertlist(cp->next));  /* next at +28 */
    lp = cp;                               /* release the native entry */
    free(lp->name); free(lp->data); free(lp);
    return ad;
}

static void freecertlist(address ad)
{
    address sa;
    if (ad != 0 && ad != NILVAL) {
        sa = getadr(ad);                   /* name string */
        if (sa != 0 && sa != NILVAL) dspspc(0, sa);
        sa = getadr(ad+8);                 /* data string */
        if (sa != 0 && sa != NILVAL) dspspc(0, sa);
        freecertlist(getadr(ad+20));       /* fork sublist */
        freecertlist(getadr(ad+28));       /* next entry */
        dspspc(0, ad);                     /* the record */
    }
}'''


# Graphics-specific C helpers: string-list and menu marshalling for the cmachg
# flavor. The interpreted list/menu records convert to/from the native
# ami_strrec/ami_menurec structures (graphics.h). Mirrors genpexec getstrlst /
# MENUHELP.
C_GFX_HELPERS = '''/* build a native ami_strptr list from an interpreted string list (next:0,
   str pstring:8). The native widget keeps the list, so it is not freed here
   (matching genpexec getstrlst). */
static ami_strptr getstrlst(address ad)
{
    ami_strptr sp; address sa; long l, i;
    if (ad == 0 || ad == NILVAL) return NULL;
    sp = malloc(sizeof(ami_strrec));
    sp->next = getstrlst(getadr(ad));
    sa = getadr(ad+8);                 /* str pstring */
    if (sa == 0 || sa == NILVAL) sp->str = NULL;
    else {
        l = getint(sa);
        sp->str = malloc(l+1);
        for (i = 0; i < l; i++) sp->str[i] = getchr(sa+INTSIZE+i);
        sp->str[l] = 0;
    }
    return sp;
}

/* build a native ami_menurec list from the VM menu record at ad (next@0,
   branch@8, onoff@16, oneof@17, bar@18 bytes, id@20 int, face pstring@28,
   size 36). Recurses next/branch; the face pstring becomes a malloc'd
   zero-terminated native string. Mirrors genpexec getmenu. */
static ami_menuptr getmenu(address ad)
{
    ami_menuptr mp; address sa; long l, i;
    if (ad == 0 || ad == NILVAL) return NULL;
    mp = malloc(sizeof(ami_menurec));
    mp->next = getmenu(getadr(ad));        /* next@0 */
    mp->branch = getmenu(getadr(ad+8));    /* branch@8 */
    mp->onoff = getbyt(ad+16);             /* onoff byte@16 */
    mp->oneof = getbyt(ad+17);             /* oneof byte@17 */
    mp->bar = getbyt(ad+18);               /* bar byte@18 */
    mp->id = getint(ad+20);                /* id@20 */
    sa = getadr(ad+28);                    /* face pstring@28 */
    if (sa == 0 || sa == NILVAL) mp->face = NULL;
    else {
        l = getint(sa);
        mp->face = malloc(l+1);
        for (i = 0; i < l; i++) mp->face[i] = getchr(sa+INTSIZE+i);
        mp->face[l] = 0;
    }
    return mp;
}

/* build a VM menu record (newspc 36) from a native ami_menurec list, recursing
   next/branch; the native face string becomes a VM heap pstring at face@28.
   Returns the VM record address (NILVAL if mp is NULL). Mirrors genpexec
   putmenu. The native list is not freed here -- ami_stdmenu owns it. */
static address putmenu(ami_menuptr mp)
{
    address ad, sa; long l, i;
    if (mp == NULL) return NILVAL;
    newspc(36, &ad);
    putadr(ad, putmenu(mp->next));         /* next@0 */
    putadr(ad+8, putmenu(mp->branch));     /* branch@8 */
    putbyt(ad+16, mp->onoff ? 1 : 0);
    putbyt(ad+17, mp->oneof ? 1 : 0);
    putbyt(ad+18, mp->bar ? 1 : 0);
    putint(ad+20, mp->id);                 /* id@20 */
    if (mp->face == NULL) putadr(ad+28, NILVAL);
    else {
        l = 0; while (mp->face[l]) l++;
        newspc(INTSIZE+l, &sa);
        putint(sa, l);
        for (i = 0; i < l; i++) putbyt(sa+INTSIZE+i, mp->face[i]);
        putadr(ad+28, sa);                 /* face pstring@28 */
    }
    return ad;
}'''


HEADER = '''/* Generated by tools/extgen/gencexec.py; do not edit by hand. The external
   executor for the cmach interpreter (the %s flavor).

   cmach runs a machine deck pre-assembled by pint --machdeck, in which external
   references are already resolved to the external vector (extvecbase+routine-1),
   so there is no load-time LookupExternal -- only ExecuteExternal, dispatched on
   the flat routine number whose order matches source/exttables.pas (MODORDER). */'''


def gen_file(dst, flavor, modlist, evtmod, extra_helpers, graphterm):
    """Generate one extern executor file hosting the given modules. modlist is in
       switch order; evtmod (a module name or None) gets putevt/getevt emitted;
       extra_helpers are extra C helper blocks (graphics menu/strptr); graphterm
       routes the terminal executor's files to the hosted window files (the
       graphics flavor) rather than the console."""
    gens = {m: CGen(m) for m in MODORDER}
    total = number(gens)
    if graphterm:
        gens['terminal'].graphfiles = True
    bodies = [(m, gens[m].cexecutor()) for m in modlist]  # populates max counts
    maxint = max(gens[m].maxint for m in modlist)
    maxrel = max(gens[m].maxrel for m in modlist)
    maxstr = max(gens[m].maxstr for m in modlist)
    maxby = max(gens[m].maxby for m in modlist)
    usesevt = any(gens[m].usesevt for m in modlist)

    out = [HEADER % flavor, '', C_HELPERS, '', C_SVC_HELPERS, '', C_NET_HELPERS, '']
    if evtmod:
        out += gens[evtmod].emit_evtprocs() + ['']
    for h in extra_helpers:
        out += [h, '']
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
    if maxby:
        out.append('    byte* %s;' % ', '.join('b%d' % i for i in range(1, maxby + 1)))
        out.append('    long %s;' % ', '.join('bl%d' % i for i in range(1, maxby + 1)))
    if usesevt:
        out.append('    ami_evtrec er;')
    out.append('')
    out.append('    switch (routine) {')
    out.append('')
    for m, body in bodies:
        out.append('    /* ---- %s ---- */' % m)
        out += body
    out.append('')
    out.append('    default: errore(FUNCTIONNOTIMPLEMENTED);')
    out.append('')
    out.append('    }')
    out.append('}')
    out.append('')
    open(dst, 'w').write('\n'.join(out) + '\n')

    print('%s flavor (%s):' % (flavor, os.path.basename(dst)))
    for m in modlist:
        g = gens[m]
        if g.stubs:
            print('  %s: %d stubs' % (m, len(g.stubs)))
            for i, n, r in g.stubs:
                print('     stub %d %s (%s)' % (i, n, r))
    return gens


def main():
    src = os.path.join(ROOT, 'source/cmach')
    # plain: services/sound/network (matches plain pint/pmach)
    gen_file(os.path.join(src, 'extern.inc'), 'plain',
             ['services', 'sound', 'network'], None, [], False)
    # terminal flavor (cmacht): + terminal, on the console
    gen_file(os.path.join(src, 'extern_term.inc'), 'terminal',
             ['services', 'terminal', 'sound', 'network'], 'terminal', [], False)
    # graphics flavor (cmachg): + graphics, and terminal hosted on the window
    # files (graphics is upward-compatible with terminal); graphics carries the
    # menu/string-list helpers
    gen_file(os.path.join(src, 'extern_graph.inc'), 'graphics',
             ['services', 'graphics', 'terminal', 'sound', 'network'], 'graphics',
             [C_GFX_HELPERS], True)


if __name__ == '__main__':
    main()
