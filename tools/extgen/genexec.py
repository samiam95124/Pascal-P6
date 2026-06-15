#!/usr/bin/env python3
# Generate the per-module external executors for the interpreter's flavored
# extexec modules (source/{plain,term,graph}/extexec.pas).
#
# Ground truth is twofold: the module's symbol list (<module>.syms, in .p6
# declaration order -- the table index is the routine number) carries the
# mangled signature digests, and the module's Pascaline interface
# (libs/<module>.pas) carries what the digest cannot: parameter modes
# (value/var/out/view) and type names. The two are matched by name and form
# (presence of the leading file parameter).
#
# Marshalling model (the services cases are the original exemplars):
# - Parameters sit on the VM stack from `params` upward in reverse
#   declaration order; scalar and address slots are 8 bytes, a string
#   (view or var) is 16 (pointer+length).
# - A function's return slot is at `params` after it is advanced past all
#   parameters; integer-family results store with putint, reals with putrel.
# - File parameters: terminal maps them to the interpreter console (event
#   reads the input side); graphics requires general files (the hosted
#   window files live in the file table) and passes filtable[fn].
# - Enumerations convert by succ-loop from the type's first value; sets
#   convert elementwise; both use the declared type names.
# - The event record converts to/from VM memory with offsets parsed from
#   the digest (putevt for event, getevt for sendevent).
# - eventover/eventsover register global-level interpreted handlers; a
#   native thunk re-enters the interpreter through callvm. Nested-level
#   handlers error (they need the planned thunk securing).
# - Parameters with types the interpreter does not yet marshal (window
#   menus, widget string lists) make the routine error as not implemented;
#   the generator lists them at the end of the run.
import os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))

# ----------------------------------------------------------------------------
# parse the module's Pascaline interface for external declarations
# ----------------------------------------------------------------------------

def parse_interface(path):
    """return list of decls: {name, isfunc, ret, params:[(mode,name,type)]}"""
    src = open(path).read()
    decls = []
    for m in re.finditer(
        r'(?:overload\s+)?(procedure|function)\s+([a-z0-9]+)\s*(\(([^()]*)\))?\s*(:\s*([a-z0-9]+))?\s*;\s*\n?\s*external\s*;',
        src, re.S):
        kind, name, _, plist, _, ret = m.groups()
        params = []
        if plist:
            for grp in plist.split(';'):
                grp = ' '.join(grp.split())
                if not grp: continue
                gm = re.match(r'(var|out|view)?\s*([a-z0-9, ]+):\s*(.+)', grp)
                if not gm: continue
                mode, names, typ = gm.groups()
                mode = mode or 'val'
                typ = ' '.join(typ.split())
                for nm in names.split(','):
                    params.append((mode, nm.strip(), typ))
        decls.append({'name': name, 'isfunc': kind == 'function',
                      'ret': ret, 'params': params})
    return decls

# ----------------------------------------------------------------------------
# signature digest helpers
# ----------------------------------------------------------------------------

def toks(sig):
    out=[]; depth=0; cur=''
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

def parse_evtrec(digest):
    """parse r(winid:0:i,handled:8:b,etype:9:x(names)(variants))"""
    m = re.match(r'r\(winid:0:i,handled:8:b,etype:9:x\(([a-z,]+)\)\((.*)\)\)$',
                 digest)
    assert m, 'unexpected event record digest'
    codes = m.group(1).split(',')
    variants = {}
    for vm in re.finditer(r'(\d+)\(([^)]*)\)', m.group(2)):
        tag = int(vm.group(1)); fields = []
        if vm.group(2):
            for f in vm.group(2).split(','):
                fn, off, ft = f.split(':')
                fields.append((fn, int(off), ft))
        variants[tag] = fields
    return codes, variants

# ----------------------------------------------------------------------------
# per-module generation
# ----------------------------------------------------------------------------

# Types whose parameter marshalling is not yet generated; routines using them
# are stubbed to errore(FunctionNotImplemented).
HARDTYPES = {'menuptr', 'imageptr', 'widget'}

class Gen:

    def __init__(self, module):
        self.module = module
        self.syms = [l.rstrip('\n') for l in
                     open(os.path.join(HERE, module + '.syms')) if l.strip()]
        self.decls = parse_interface(os.path.join(ROOT, 'libs',
                                                  module + '.pas'))
        self.enums = None
        self.sets = None
        self.stubs = []
        self.nsvars = set()
        self.tvarmax = {}  # typed scalar var-param pools: typename -> max count
        self.usesbyte = False  # module has a bytarr (open byte array) parameter
        self.usescert = False  # module has a certptr (certificate list) parameter
        self.maxint = 3; self.maxrel = 1; self.maxstr = 1
        ev = [s for s in self.syms if s.startswith('event@p_fc_r')]
        if ev:
            self.evcodes, self.evvars = parse_evtrec(
                ev[0].split('@',1)[1].split('_',2)[2])
        else:
            self.evcodes, self.evvars = None, None

    def match_decl(self, name, hasfile, nparams):
        for d in self.decls:
            if d['name'] != name: continue
            p = d['params']
            df = len(p) > 0 and p[0][2] == 'text' and p[0][0] == 'var'
            if df != hasfile: continue
            if len(p) != nparams: continue
            return d
        return None

    def slot(self, mode, typ):
        # open arrays (string of char, bytarr of byte) pass as a descriptor of
        # base address plus logical length, view or var alike
        if typ in ('string', 'bytarr'): return 'strparsiz'
        if mode in ('var','out') or typ in ('text','evtrec'): return 'adrsize'
        return 'intsize'

    def gencase(self, idx, sym):
        name, sig = sym.split('@', 1)
        tl = toks(sig); kind, ptoks = tl[0], tl[1:]
        isfunc = kind == 'f'
        hasfile = len(ptoks) > 0 and ptoks[0] == 'fc'
        if name in ('eventover', 'eventsover'):
            return self.gencallback(idx, name)
        if self.module == 'graphics' and name == 'menu' and hasfile:
            self.maxint = max(self.maxint, 1)
            return ['       %d: begin { menu@p_fc_pr }' % idx, '',
                    '           a1 := getadr(params); { interpreted menu list }',
                    '           ad := getadr(params+adrsize); valfil(ad); fn := getbyt(ad);',
                    '           if fn <= commandfn then errore(FileModeIncorrect);',
                    '           graphics.menu(filtable[fn], getmenu(a1));',
                    '           params := params+adrsize+adrsize', '',
                    '       end;', '']
        if self.module == 'graphics' and name == 'stdmenu':
            self.maxint = max(self.maxint, 2)
            return ['       %d: begin { stdmenu@p_i_pr_pr }' % idx, '',
                    '           a1 := getadr(params); { append list }',
                    '           ad2 := getadr(params+adrsize); { var sm result cell }',
                    '           a2 := getint(params+adrsize*2); { selections }',
                    '           mnu := nil;',
                    '           graphics.stdmenu(a2, mnu, getmenu(a1));',
                    '           putadr(ad2, putmenu(mnu));',
                    '           params := params+adrsize*2+intsize', '',
                    '       end;', '']
        # The certificate-list routines return a native linked list/tree of
        # certfield records (certptr). putcertlist translates that to vm memory
        # (and frees the native side); freecertlist frees the vm structure.
        if self.module == 'network' and name == 'certlistnet':
            self.maxint = max(self.maxint, 2); self.usescert = True
            return ['       %d: begin { certlistnet@p_fc_i_pr }' % idx, '',
                    '           ad := getadr(params+adrsize+intsize); valfil(ad); fn := getbyt(ad);',
                    '           if fn <= commandfn then errore(FileModeIncorrect);',
                    '           a1 := fn;',
                    '           a2 := getint(params+adrsize); { which }',
                    '           ad2 := getadr(params); { var list result cell }',
                    '           cp := nil;',
                    '           network.certlistnet(filtable[a1], a2, cp);',
                    '           putadr(ad2, putcertlist(cp));',
                    '           params := params+adrsize+intsize+adrsize', '',
                    '       end;', '']
        if self.module == 'network' and name == 'certlistmsg':
            self.maxint = max(self.maxint, 2); self.usescert = True
            return ['       %d: begin { certlistmsg@p_i_i_pr }' % idx, '',
                    '           a1 := getint(params+adrsize+intsize); { connection }',
                    '           a2 := getint(params+adrsize); { which }',
                    '           ad2 := getadr(params); { var list result cell }',
                    '           cp := nil;',
                    '           network.certlistmsg(a1, a2, cp);',
                    '           putadr(ad2, putcertlist(cp));',
                    '           params := params+intsize+intsize+adrsize', '',
                    '       end;', '']
        if self.module == 'network' and name == 'certlistfree':
            self.usescert = True
            return ['       %d: begin { certlistfree@p_pr }' % idx, '',
                    '           ad2 := getadr(params); { var list cell }',
                    '           freecertlist(getadr(ad2));',
                    '           putadr(ad2, nilval);',
                    '           params := params+adrsize', '',
                    '       end;', '']
        d = self.match_decl(name, hasfile, len(ptoks))
        if d is None:
            self.stubs.append((idx, name, 'no interface match'))
            return ['       %d: errore(FunctionNotImplemented);' % idx, '']
        for mode, pn, typ in d['params']:
            if typ.split()[0] in HARDTYPES:
                self.stubs.append((idx, name, typ))
                return ['       { %s: %s parameters are not yet marshalled }'
                        % (name, typ),
                        '       %d: errore(FunctionNotImplemented);' % idx, '']
        offs = []; acc = 'params'
        for mode, pn, typ in reversed(d['params']):
            offs.append(acc)
            acc = acc + '+' + self.slot(mode, typ)
        offs.reverse()
        total = '+'.join(self.slot(m,t) for m,_,t in d['params'])
        pre=[]; post=[]; args=[]; ni=0; nr=0; nstr=0; filevars=[]; tvn={}
        for (mode, pn, typ), off in zip(d['params'], offs):
            base = typ.split()[0]
            if typ == 'text':
                pre.append('           ad := getadr(%s); valfil(ad); fn := getbyt(ad);' % off)
                if self.module == 'terminal':
                    if name == 'event':
                        pre.append('           if (fn <> inputfn) and (fn > commandfn) then')
                        pre.append('              errore(FileModeIncorrect);')
                        args.append('input')
                    else:
                        pre.append('           if fn > commandfn then errore(FileModeIncorrect);')
                        args.append('output')
                else:
                    ni += 1; v = 'a%d' % ni
                    pre.append('           if fn <= commandfn then errore(FileModeIncorrect);')
                    pre.append('           %s := fn;' % v)
                    args.append('filtable[%s]' % v)
                    filevars.append(v)
            elif base == 'strptr':
                ni += 1; v = 'a%d' % ni
                pre.append('           %s := getadr(%s); { string list }' % (v, off))
                args.append('getstrlst(%s)' % v)
            elif typ == 'evtrec':
                pre.append('           ad2 := getadr(%s);' % off)
                args.append('er')
                if name == 'sendevent':
                    pre.append('           getevt(er, ad2);')
                else:
                    post.append('           putevt(er, ad2);')
            elif typ == 'string':
                # each string parameter needs its own buffer (s, s2, s3 ...);
                # sharing one would make every string argument the last value
                nstr += 1
                sv = 's' if nstr == 1 else 's%d' % nstr
                self.maxstr = max(self.maxstr, nstr)
                if mode == 'view':
                    pre.append('           getstr(%s, %s);' % (off, sv))
                    args.append(sv)
                else:
                    # var string: output only -- do not read the possibly
                    # undefined content, blank the buffer instead
                    ni += 1; v = 'a%d' % ni
                    pre.append('           %s := %s; { var string base }' % (v, off))
                    pre.append('           for rv := 1 to strmax do %s[rv] := \' \';' % sv)
                    args.append(sv)
                    post.append('           putstr(%s, %s);' % (sv, v))
            elif base == 'bytarr':
                # open array of byte: a wide pointer of (base address, length).
                # Allocate a container array of the runtime length so the call
                # passes that exact length (max(bp^)); copy bytes in, and for a
                # var parameter copy the result back. getabyte/putabyte free it.
                self.usesbyte = True
                if mode == 'view':
                    pre.append('           getabyte(%s, bp);' % off)
                    post.append('           dispose(bp);')
                else:
                    # var (output): allocate without reading the undefined vm
                    # buffer; the native routine fills it, then copy it back
                    pre.append('           newabyte(%s, bp);' % off)
                    post.append('           putabyte(%s, bp);' % off)
                args.append('bp^')
            elif base in self.module_enums():
                ni += 1; v = 'a%d' % ni
                pre.append('           %s := getint(%s);' % (v, off))
                args.append('cnv%s(%s)' % (base, v))
            elif base in self.module_sets():
                if mode in ('var','out'):
                    ni += 1; av = 'a%d' % ni
                    nstv = 'ns%s' % base
                    self.nsvars.add((nstv, base))
                    pre.append('           %s := getadr(%s);' % (av, off))
                    if mode == 'var':
                        pre.append('           getset(%s, st);' % av)
                        pre.append('           %s := cnv%s(st);' % (nstv, base))
                    else:
                        pre.append('           %s := [];' % nstv)
                    args.append(nstv)
                    post.append('           putset(%s, unc%s(%s));' % (av, base, nstv))
                else:
                    pre.append('           getset(%s, st);' % off)
                    args.append('cnv%s(st)' % base)
            elif typ == 'real':
                nr += 1; v = 'r%d' % nr
                if mode in ('var','out'):
                    ni += 1; av = 'a%d' % ni
                    pre.append('           %s := getadr(%s);' % (av, off))
                    if mode == 'var':
                        pre.append('           %s := getrel(%s);' % (v, av))
                    args.append(v)
                    post.append('           putrel(%s, %s);' % (av, v))
                else:
                    pre.append('           %s := getrel(%s);' % (v, off))
                    args.append(v)
            elif typ == 'boolean':
                ni += 1; v = 'a%d' % ni
                pre.append('           %s := getint(%s);' % (v, off))
                args.append('%s <> 0' % v)
            elif typ == 'char':
                ni += 1; v = 'a%d' % ni
                pre.append('           %s := getint(%s);' % (v, off))
                args.append('chr(%s)' % v)
            else: # integer family
                if mode in ('var','out'):
                    # the model's var scalars are outputs: the content may be
                    # undefined, so it is not read before the call
                    ni += 1; av = 'a%d' % ni
                    pre.append('           %s := getadr(%s);' % (av, off))
                    if base == 'integer':
                        ni += 1; v = 'a%d' % ni
                    else:
                        # a named integer-subrange type (e.g. lcardinal): the
                        # var parameter requires a variable of that exact type,
                        # not a plain integer, so draw one from a typed pool
                        tvn[base] = tvn.get(base, 0) + 1
                        v = '%s%d' % (base, tvn[base])
                    pre.append('           %s := 0;' % v)
                    args.append(v)
                    post.append('           putint(%s, %s);' % (av, v))
                else:
                    ni += 1; v = 'a%d' % ni
                    pre.append('           %s := getint(%s);' % (v, off))
                    args.append(v)
        self.maxint = max(self.maxint, ni); self.maxrel = max(self.maxrel, nr)
        for t, c in tvn.items():
            self.tvarmax[t] = max(self.tvarmax.get(t, 0), c)
        call = '%s.%s' % (self.module, name)
        if args: call += '(' + ', '.join(args) + ')'
        out = ['       %d: begin { %s@%s }' % (idx, name, sig[:40]), '']
        out += pre
        if isfunc:
            # post (var/out copy-back, container free) runs after the call but
            # before params advances: it writes to the original parameter slots
            if d['ret'] == 'real':
                out.append('           r1 := %s;' % call)
                out += post
                if total: out.append('           params := params+%s;' % total)
                out.append('           putrel(params, r1);')
            else:
                out.append('           rv := ord(%s);' % call)
                out += post
                if total: out.append('           params := params+%s;' % total)
                out.append('           putint(params, rv);')
        else:
            out.append('           %s;' % call)
            # Routines that open a connection into a pair of var text files
            # (a window, a tcp/secure socket): mark the bound files readable and
            # writable so the program's reads and writes on them are permitted.
            if name in ('openwin', 'opennet', 'opennetv6', 'waitnet') \
               and len(filevars) >= 2:
                out.append('           { the bound connection files become readable and writable }')
                out.append('           filstate[%s] := fread;' % filevars[0])
                out.append('           filstate[%s] := fwrite;' % filevars[1])
            out += post
            if total: out.append('           params := params+%s;' % total)
        out += ['', '       end;', '']
        return out

    def gencallback(self, idx, name):
        out = ['       %d: begin { %s }' % (idx, name), '']
        out.append('           evtsetup;')
        out.append('           ad := getadr(params); { out oeh }')
        out.append('           ad2 := getadr(params+adrsize); { handler address }')
        out.append('           a1 := getadr(params+adrsize+ptrsize); { handler frame }')
        out.append('           { only global-level handlers: nested ones need the')
        out.append('             planned thunk securing }')
        out.append('           if ad2 <> 0 then if a1 <> globalframe then')
        out.append('              errore(FunctionNotImplemented);')
        if name == 'eventover':
            out.append('           a2 := getint(params+adrsize+ptrsize*2); { event code }')
            out.append('           putint(ad, vmhand[a2+1]);')
            out.append('           vmhand[a2+1] := ad2;')
            out.append('           if not hooked[a2+1] then begin')
            out.append('')
            out.append('              %s.eventover(cnvevtcod(a2), evtthunk, a3);' % self.module)
            out.append('              hooked[a2+1] := true')
            out.append('')
            out.append('           end;')
            out.append('           params := params+adrsize+ptrsize*2+intsize')
        else:
            out.append('           putint(ad, vmhandall);')
            out.append('           vmhandall := ad2;')
            out.append('           if not hookedall then begin')
            out.append('')
            out.append('              %s.eventsover(evtsthunk, a3);' % self.module)
            out.append('              hookedall := true')
            out.append('')
            out.append('           end;')
            out.append('           params := params+adrsize+ptrsize*2')
        out += ['', '       end;', '']
        return out

    def collect_types(self):
        if self.enums is not None: return
        self.enums = {}; self.sets = {}
        if self.evcodes:
            self.enums['evtcod'] = self.evcodes[0]
        for sym in self.syms:
            name, sig = sym.split('@',1)
            tl = toks(sig); ptoks = tl[1:]
            hasfile = len(ptoks)>0 and ptoks[0]=='fc'
            d = self.match_decl(name, hasfile, len(ptoks))
            if not d: continue
            for (mode,pn,typ), tok in zip(d['params'], ptoks):
                base = typ.split()[0]
                # A true enumeration digest is x(elem,elem,...) with named
                # elements and no base-type suffix, so the token ends with ')'.
                # An integer subrange is x(low,high)<base> (e.g. x(0,255)i) and
                # ends with the base-type char; those pass as plain integers and
                # must not be mistaken for enums needing a value converter.
                if tok.startswith('x(') and tok.endswith(')') \
                   and base not in self.enums:
                    self.enums[base] = tok[2:-1].split(',')[0]
                if tok.startswith('sx(') and base not in self.sets:
                    self.sets[base] = tok[3:-1].split(',')

    def module_enums(self):
        self.collect_types()
        return self.enums

    def module_sets(self):
        self.collect_types()
        return self.sets

    def converters(self):
        out=[]
        for typ, first in sorted(self.module_enums().items()):
            out += ['{ convert ordinal to %s }' % typ, '',
                    'function cnv%s(i: integer): %s.%s;' % (typ, self.module, typ),
                    '', 'var c: %s.%s;' % (self.module, typ),
                    '    j: integer;', '', 'begin', '',
                    '   c := %s.%s;' % (self.module, first),
                    '   for j := 1 to i do c := succ(c);',
                    '   cnv%s := c' % typ, '', 'end;', '']
        for typ, elems in sorted(self.module_sets().items()):
            out += ['{ convert set to %s }' % typ, '',
                    'function cnv%s(view st: settype): %s.%s;' % (typ, self.module, typ),
                    '', 'var ns: %s.%s;' % (self.module, typ), '', 'begin', '',
                    '   ns := [];']
            for i, e in enumerate(elems):
                out.append('   if %d in st then ns := ns + [%s.%s];' % (i, self.module, e))
            out += ['   cnv%s := ns' % typ, '', 'end;', '']
            out += ['{ convert %s to set }' % typ, '',
                    'function unc%s(view ns: %s.%s): settype;' % (typ, self.module, typ),
                    '', 'var st: settype;', '', 'begin', '',
                    '   st := [];']
            for i, e in enumerate(elems):
                out.append('   if %s.%s in ns then st := st + [%d];' % (self.module, e, i))
            out += ['   unc%s := st' % typ, '', 'end;', '']
        return out

    def evtprocs(self):
        if not self.evcodes: return []
        out = ['{ place event record to client memory }', '',
               'procedure putevt(view er: %s.evtrec; ad: address);' % self.module,
               '', 'begin', '',
               '   putint(ad, er.winid); { winid at 0 }',
               '   putbyt(ad+8, ord(er.handled)); { handled at 8 }',
               '   putbyt(ad+9, ord(er.etype)); { etype tag at 9 }',
               '   case er.etype of { variant data, reverse order from 12 }', '']
        for tag in sorted(self.evvars):
            fields = self.evvars[tag]
            if not fields: continue
            code = self.evcodes[tag]
            sets=[]
            for fn, off, ft in fields:
                if ft in ('c','b'):
                    sets.append('putbyt(ad+%d, ord(er.%s))' % (off, fn))
                else:
                    sets.append('putint(ad+%d, er.%s)' % (off, fn))
            pre = '      %s.%s: ' % (self.module, code)
            if len(sets) == 1:
                out.append(pre + sets[0] + ';')
            else:
                out.append(pre + 'begin ' + ';\n         '.join(sets) + ' end;')
        out += ['      else { events without parameter data }', '   end', '',
                'end;', '',
                '{ get event record from client memory }', '',
                'procedure getevt(out er: %s.evtrec; ad: address);' % self.module,
                '', 'var i: integer;', '    c: %s.evtcod;' % self.module, '',
                'begin', '',
                '   i := getbyt(ad+9); { etype tag }',
                '   c := %s.etchar;' % self.module,
                '   while i > 0 do begin c := succ(c); i := i-1 end;',
                '   case c of { construct against the variant }', '']
        for tag in sorted(self.evvars):
            fields = self.evvars[tag]
            code = self.evcodes[tag]
            sets=['er.etype := %s.%s' % (self.module, code)]
            for fn, off, ft in fields:
                if ft == 'c':
                    sets.append('er.%s := chr(getbyt(ad+%d))' % (fn, off))
                elif ft == 'b':
                    sets.append('er.%s := getbyt(ad+%d) <> 0' % (fn, off))
                else:
                    sets.append('er.%s := getint(ad+%d)' % (fn, off))
            out.append('      %s.%s: begin ' % (self.module, code) +
                       ';\n         '.join(sets) + ' end;')
        out += ['      else er.etype := c', '   end;',
                '   er.winid := getint(ad);',
                '   er.handled := getbyt(ad+8) <> 0', '', 'end;', '']
        return out

    def callbacks(self):
        if not self.evcodes: return []
        return ['''{{ Event callback support: interpreted programs register global-level
  handlers through eventover/eventsover; a native thunk converts the event
  record into interpreted memory, runs the handler through the interpreter
  (callvm), and carries the handled flag back. Old vectors are opaque
  integers by the interface with no reinstall facility, so registration
  replaces the previous interpreted handler. }}

const nevtcod = {n}; {{ number of event codes }}
      evtrecvmsiz = 96; {{ interpreted event record scratch size }}

var vmhand:    array [1..nevtcod] of address; {{ per-event handlers }}
    vmhandall: address; {{ master handler }}
    hooked:    array [1..nevtcod] of boolean; {{ native vector hooked }}
    hookedall: boolean;
    evtinit:   boolean; {{ tables initialized }}

{{ initialize the callback tables on first use }}

procedure evtsetup;

var i: integer;

begin

   if not evtinit then begin

      for i := 1 to nevtcod do begin vmhand[i] := 0; hooked[i] := false end;
      vmhandall := 0;
      hookedall := false;
      evtinit := true

   end

end;

{{ run an interpreted handler against an event record }}

procedure callhand(h: address; var er: {m}.evtrec);

var ad: address;

begin

   ad := resvm(evtrecvmsiz); {{ scratch on the interpreted stack }}
   putevt(er, ad);
   callvm(globalframe, h, ad);
   er.handled := getbyt(ad+8) <> 0; {{ carry the handled flag back }}
   relvm(evtrecvmsiz)

end;

{{ native thunk for per-event vectors }}

procedure evtthunk(var er: {m}.evtrec);

var h: address;

begin

   h := vmhand[ord(er.etype)+1];
   if h <> 0 then callhand(h, er)

end;

{{ native thunk for the master vector }}

procedure evtsthunk(var er: {m}.evtrec);

begin

   if vmhandall <> 0 then callhand(vmhandall, er)

end;
'''.format(n=len(self.evcodes), m=self.module)]

    def executor(self, procname):
        body=[]
        for i, sym in enumerate(self.syms, start=1):
            body += self.gencase(i, sym)
        ints = ', '.join('a%d' % i for i in range(1, self.maxint+1))
        rels = ', '.join('r%d' % i for i in range(1, self.maxrel+1))
        head = ['procedure %s(routine: integer; var params: integer);' % procname,
                '', 'var %s, rv: integer;' % ints,
                '    %s: real;' % rels,
                '    %s: str;' % ', '.join(['s'] + ['s%d' % i for i in range(2, self.maxstr+1)]),
                '    ad, ad2:  address;',
                '    fn:       fileno;',
                '    st:       settype;']
        if self.evcodes:
            head.append('    er:       %s.evtrec;' % self.module)
        if self.usesbyte:
            head.append('    bp:       bytconp;')
        if self.usescert:
            head.append('    cp:       network.certptr;')
        if self.module == 'graphics':
            head.append('    mnu:      graphics.menuptr;')
        for nm, base in sorted(self.nsvars):
            head.append('    %s: %s.%s;' % (nm, self.module, base))
        for t in sorted(self.tvarmax):
            # these are built-in integer-family scalar types (e.g. lcardinal),
            # named but not module-qualified
            names = ', '.join('%s%d' % (t, i) for i in range(1, self.tvarmax[t]+1))
            head.append('    %s: %s;' % (names, t))
        head += ['', 'begin', '', '    case routine of', '']
        tail = ['    else errore(FunctionNotImplemented)', '', '    end', '',
                'end;', '']
        return head + body + tail

# ----------------------------------------------------------------------------
# flavor file assembly
# ----------------------------------------------------------------------------

HELPERS = '''
const strmax = 10000; { size of string buffers; large enough for a PEM
                        certificate (network certnet/certmsg return one through
                        a var string), matching the binding's certificate buffer
                        convention }
      strparsiz = ptrsize+intsize; { size of string parameter }
      FileModeIncorrect = 28; { file mode incorrect }
      FunctionNotImplemented = 90; { external function not implemented }

type str = packed array [1..strmax] of char;
     bytcon = packed array of byte; { container for open byte-array params }
     bytconp = ^bytcon;

{ load string from vm }

procedure getstr(sa: address; var s: string);

var ad: address;
    l:  integer;
    i:  integer;

begin

   for i := 1 to max(s) do s[i] := ' '; { clear result }
   ad := getadr(sa); { get base address of string }
   l := getint(sa+ptrsize); { get logical length }
   if l > max(s) then l := max(s); { limit to buffer }
   for i := 1 to l do begin { transfer string }

      s[i] := chr(getbyt(ad)); { get character }
      ad := ad+1 { next }

   end

end;

{ store string to vm }

procedure putstr(var s: str; sa: address);

var ad: address;
    l:  integer;
    i:  integer;

begin

   ad := getadr(sa); { get base address of string }
   l := getint(sa+ptrsize); { get logical length }
   if l > strmax then l := strmax;
   for i := 1 to l do begin

      putbyt(ad, ord(s[i]));
      ad := ad+1

   end

end;

{ allocate an output byte-array container of the vm array's length, without
  reading the (possibly undefined) vm contents -- for a var parameter the native
  routine fills the buffer, so the vm side need not be defined first }

procedure newabyte(sa: address; var bp: bytconp);

var l: integer;
    i: integer;

begin

   l := getint(sa+ptrsize); { logical length (the array bound) }
   new(bp, l);
   for i := 1 to l do bp^[i] := 0 { defined, native overwrites what it returns }

end;

{ load open byte array from vm into a freshly allocated container }

procedure getabyte(sa: address; var bp: bytconp);

var ad: address;
    l:  integer;
    i:  integer;

begin

   ad := getadr(sa); { base address of the array }
   l := getint(sa+ptrsize); { logical length (the array bound) }
   new(bp, l); { container of the runtime length, so max(bp^) = l }
   for i := 1 to l do begin { transfer bytes in }

      bp^[i] := getbyt(ad);
      ad := ad+1

   end

end;

{ store open byte array container back to vm and free it }

procedure putabyte(sa: address; bp: bytconp);

var ad: address;
    l:  integer;
    i:  integer;

begin

   ad := getadr(sa); { base address of the array }
   l := getint(sa+ptrsize); { logical length }
   for i := 1 to l do begin { transfer bytes back }

      putbyt(ad, bp^[i]);
      ad := ad+1

   end;
   dispose(bp)

end;
'''

STUB = '''procedure %s(routine: integer; var params: integer);

begin

   refer(routine);
   refer(params);

   errore(FunctionNotImplemented)

end;
'''

MENUHELP = '''{ Menu conversion: the interpreted menu list (next:0, branch:8, onoff:16,
  oneof:17, bar:18, id:20, face pstring:28 -- offsets from the signature
  digest) converts to a native list for menu, and a native list converts
  back into interpreted heap for the stdmenu result. }

function getmenu(ad: address): graphics.menuptr;

var mp: graphics.menuptr;
    sa: address;
    l, i: integer;

begin

   if (ad = 0) or (ad = nilval) then getmenu := nil
   else begin

      new(mp);
      mp^.next := getmenu(getadr(ad));
      mp^.branch := getmenu(getadr(ad+8));
      mp^.onoff := getbyt(ad+16) <> 0;
      mp^.oneof := getbyt(ad+17) <> 0;
      mp^.bar := getbyt(ad+18) <> 0;
      mp^.id := getint(ad+20);
      sa := getadr(ad+28); { face string }
      if (sa = 0) or (sa = nilval) then mp^.face := nil
      else begin

         l := getint(sa);
         new(mp^.face, l);
         for i := 1 to l do mp^.face^[i] := chr(getbyt(sa+intsize+i-1))

      end;
      getmenu := mp

   end

end;

function putmenu(mp: graphics.menuptr): address;

var ad, sa: address;
    l, i: integer;

begin

   if mp = nil then putmenu := nilval
   else begin

      newspc(36, ad); { a menu record in interpreted layout }
      putadr(ad, putmenu(mp^.next));
      putadr(ad+8, putmenu(mp^.branch));
      putbyt(ad+16, ord(mp^.onoff));
      putbyt(ad+17, ord(mp^.oneof));
      putbyt(ad+18, ord(mp^.bar));
      putint(ad+20, mp^.id);
      if mp^.face = nil then putadr(ad+28, nilval)
      else begin

         l := max(mp^.face^);
         newspc(intsize+l, sa);
         putint(sa, l);
         for i := 1 to l do putbyt(sa+intsize+i-1, ord(mp^.face^[i]));
         putadr(ad+28, sa)

      end;
      putmenu := ad

   end

end;

{ String list conversion: the interpreted list (next:0, str pstring:8 --
  offsets from the signature digest) converts to a native list for the
  list and drop box widgets. }

function getstrlst(ad: address): graphics.strptr;

var sp: graphics.strptr;
    sa: address;
    l, i: integer;

begin

   if (ad = 0) or (ad = nilval) then getstrlst := nil
   else begin

      new(sp);
      sp^.next := getstrlst(getadr(ad));
      sa := getadr(ad+8); { string }
      if (sa = 0) or (sa = nilval) then sp^.str := nil
      else begin

         l := getint(sa);
         new(sp^.str, l);
         for i := 1 to l do sp^.str^[i] := chr(getbyt(sa+intsize+i-1))

      end;
      getstrlst := sp

   end

end;
'''

VMHOST = '''
var nilwin: text; { never-opened file: openwin takes it as no parent }

{ find a free general file slot }

function frefil: fileno;

var i, ff: integer;

begin

   ff := 0;
   for i := commandfn+1 to maxfil do
      if (filstate[i] = fnone) and (ff = 0) then ff := i;
   if ff = 0 then errore(FunctionNotImplemented);
   frefil := ff

end;

{ host the interpreted program in its own window }

override procedure vmhost;

var fi, fo: fileno;

begin

   fi := frefil; filstate[fi] := fread; { reserve the input side }
   fo := frefil; filstate[fo] := fwrite; { reserve the output side }
   { open the window against the file table entries }
   graphics.openwin(filtable[fi], filtable[fo], nilwin, 1);
   { the interpreted standard files attach to the window }
   vmstdin := fi;
   vmstdout := fo

end;
'''

def writeflavor(path, header, joins, body):
    out = [header, '', 'module extexec;', '']
    if joins:
        out.append(joins)
        out.append('')
    out.append('uses pint_mem; { low level vm access for pint }')
    out.append('')
    out.append('procedure execterminal(routine: integer; var params: integer); forward;')
    out.append('procedure execgraph(routine: integer; var params: integer); forward;')
    out.append('procedure execsound(routine: integer; var params: integer); forward;')
    out.append('procedure execnetwork(routine: integer; var params: integer); forward;')
    out.append('')
    out.append('private')
    out.append('')
    out.append(body)
    out.append('begin')
    out.append('end.')
    open(path, 'w').write('\n'.join(out) + '\n')

CERTHELP = '''{ Certificate-list translation. The native network.certlist* routines build a
  linked list/tree of certfield records (name@0 data@8 critical@16 fork@20
  next@28, record size 36, with name/data as pstrings). putcertlist rebuilds it
  in vm memory, freeing the native side; freecertlist frees the vm structure. }

function putcertlist(cp: network.certptr): address;

var ad, sa: address;
    l, i:   integer;
    lp:     network.certptr;

begin

   if cp = nil then putcertlist := nilval
   else begin

      newspc(36, ad); { a certfield record in interpreted layout }
      { name pstring at +0 }
      if cp^.name = nil then putadr(ad, nilval)
      else begin
         l := max(cp^.name^);
         newspc(intsize+l, sa);
         putint(sa, l);
         for i := 1 to l do putbyt(sa+intsize+i-1, ord(cp^.name^[i]));
         putadr(ad, sa)
      end;
      { data pstring at +8 }
      if cp^.data = nil then putadr(ad+8, nilval)
      else begin
         l := max(cp^.data^);
         newspc(intsize+l, sa);
         putint(sa, l);
         for i := 1 to l do putbyt(sa+intsize+i-1, ord(cp^.data^[i]));
         putadr(ad+8, sa)
      end;
      putbyt(ad+16, ord(cp^.critical)); { critical at +16 }
      putadr(ad+20, putcertlist(cp^.fork)); { fork sublist at +20 }
      putadr(ad+28, putcertlist(cp^.next)); { next at +28 }
      { release the native entry; the sublist and next are freed by recursion }
      lp := cp;
      if lp^.name <> nil then dispose(lp^.name);
      if lp^.data <> nil then dispose(lp^.data);
      dispose(lp);
      putcertlist := ad

   end

end;

procedure freecertlist(ad: address);

var sa: address;

begin

   if (ad <> 0) and (ad <> nilval) then begin

      sa := getadr(ad); { name string }
      if (sa <> 0) and (sa <> nilval) then dspspc(0, sa);
      sa := getadr(ad+8); { data string }
      if (sa <> 0) and (sa <> nilval) then dspspc(0, sa);
      freecertlist(getadr(ad+20)); { fork sublist }
      freecertlist(getadr(ad+28)); { next entry }
      dspspc(0, ad) { the record }

   end

end;
'''

def addmods(body, mods):
    """Append the converters, event procs, callbacks and executor of each
       module in mods to body. Sound and network carry no events or callbacks,
       so those contribute nothing; their executor name is exec<module>. The
       network module also gets the certificate-list translators before its
       executor."""
    for g in mods:
        body += g.converters()
        body += g.evtprocs()
        body += g.callbacks()
        if g.module == 'network':
            body.append(CERTHELP)
        body += g.executor('exec'+g.module)
    return body

def main():
    term = Gen('terminal')
    graph = Gen('graphics')
    sound = Gen('sound')
    network = Gen('network')

    # Sound and network are standard in every flavor (plain, term, graph): the
    # interpreter is no longer a portability path, so all variants host them.

    tbody = [HELPERS]
    tbody += term.converters()
    tbody += term.evtprocs()
    tbody += term.callbacks()
    tbody += term.executor('execterminal')
    addmods(tbody, [sound, network])
    tbody.append(STUB % 'execgraph')
    writeflavor(os.path.join(ROOT, 'source/term/extexec.pas'),
'''{*******************************************************************************
*                                                                              *
*                       External model execution                               *
*                                                                              *
* The terminal flavor of the external execution module, selected for the      *
* pintt interpreter by module path: terminal, sound and network externals     *
* execute against their native models; graphics externals error.              *
*                                                                              *
* Generated by tools/extgen/genexec.py; do not edit by hand.                  *
*                                                                              *
*******************************************************************************}''',
        'joins terminal, sound, network; { terminal, sound and network model I/O }',
        '\n'.join(tbody))

    gbody = [HELPERS, VMHOST, MENUHELP]
    gbody += graph.converters()
    gbody += graph.evtprocs()
    gbody += graph.callbacks()
    gbody += graph.executor('execgraph')
    addmods(gbody, [sound, network])
    gbody.append(STUB % 'execterminal')
    writeflavor(os.path.join(ROOT, 'source/graph/extexec.pas'),
'''{*******************************************************************************
*                                                                              *
*                       External model execution                               *
*                                                                              *
* The graphics flavor of the external execution module, selected for the      *
* pintg interpreter by module path. Joining graphics links the "blonde"       *
* graphics archive (also placed in this directory): the model installs no     *
* automatic window over the interpreter's standard files. The vmhost hook     *
* opens a window through openwin into two general file table entries and the  *
* interpreted standard files attach to them, so the program's standard I/O    *
* flows to the window; further windows the interpreted program opens bind to  *
* general files the same way. Graphics externals execute against the native   *
* graphics model with the window files passed from the file table; sound and  *
* network externals execute against their native models; terminal externals   *
* error.                                                                       *
*                                                                              *
* Generated by tools/extgen/genexec.py; do not edit by hand.                  *
*                                                                              *
*******************************************************************************}''',
        'joins graphics, sound, network; { graphics (blonde archive), sound and network model I/O }',
        '\n'.join(gbody))

    pbody = [HELPERS]
    addmods(pbody, [sound, network])
    pbody.append(STUB % 'execterminal')
    pbody.append(STUB % 'execgraph')
    writeflavor(os.path.join(ROOT, 'source/plain/extexec.pas'),
'''{*******************************************************************************
*                                                                              *
*                       External model execution                               *
*                                                                              *
* The plain flavor of the external execution module: the plain interpreter    *
* hosts no terminal or graphics model, so those externals error. Sound and    *
* network are standard in every flavor and execute against their native       *
* models. The pintt and pintg interpreters add the terminal and graphics      *
* models by module path.                                                      *
*                                                                              *
* Generated by tools/extgen/genexec.py; do not edit by hand.                  *
*                                                                              *
*******************************************************************************}''',
        'joins sound, network; { sound and network model I/O }',
        '\n'.join(pbody))

    print('terminal: %d cases, %d stubs' % (len(term.syms), len(term.stubs)))
    for i, n, r in term.stubs: print('   stub %d %s (%s)' % (i, n, r))
    print('graphics: %d cases, %d stubs' % (len(graph.syms), len(graph.stubs)))
    for i, n, r in graph.stubs: print('   stub %d %s (%s)' % (i, n, r))
    print('sound: %d cases, %d stubs' % (len(sound.syms), len(sound.stubs)))
    for i, n, r in sound.stubs: print('   stub %d %s (%s)' % (i, n, r))
    print('network: %d cases, %d stubs' % (len(network.syms), len(network.stubs)))
    for i, n, r in network.stubs: print('   stub %d %s (%s)' % (i, n, r))

main()
