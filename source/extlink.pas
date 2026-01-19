{*******************************************************************************

Pascaline externals interface

This module connects internal interpreter calls to external routines in 
Pascaline. This involves picking up the parameters from the vm and calling the
underlying interface, then copying and reformatting the results back to the vm.

The externals interface determines what routines outside the vm implementation
are available to call. This is primarily routines in the Pascaline 
specification, but it could be any set of routines defined by the user.

*******************************************************************************}

module extlink;

joins services; { system services }

uses strings,  { string functions }
     pint_mem; { low level vm access for pint }

{*******************************************************************************

Lookup symbol/module name

Given a module name and symbol name, returns a number used to execute the given
module call. If no routine is found, a zero is returned. If a routine is found,
this can be used as a key to execute the correct routine.

Note that overloads are accounted for in the type signature. Thus the symbol for
each overload is different.

Note that the numbering system for routines is flat, that is, all modules use
the same incrementing numbers. This system many have to change if we end up
renumbering them too much.

*******************************************************************************}

procedure LookupExternal(
    { name of module }           view modulen: string;
    { symbol within module }     view symbol:  string;
    { resulting routine number } var routine: integer
    );

begin

   routine := 0; { set no routine found }

   if compp(modulen, 'services') then begin

       if compp(symbol, 'list@p_vc_pr(name:0:pvc,size:8:i,alloc:16:i,attr:24:sx(atexec,atarc,atsys,atdir,atloop),create:56:i,modify:64:i,access:72:i,backup:80:i,user:88:sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren),group:120:sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren),other:152:sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren),next:184:p2)') then routine := 1
       else if compp(symbol, 'list@p_pvc_pr(name:0:pvc,size:8:i,alloc:16:i,attr:24:sx(atexec,atarc,atsys,atdir,atloop),create:56:i,modify:64:i,access:72:i,backup:80:i,user:88:sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren),group:120:sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren),other:152:sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren),next:184:p2)') then routine := 2
       else if compp(symbol, 'times@p_vc_i') then routine := 3
       else if compp(symbol, 'times@f_i') then routine := 4
       else if compp(symbol, 'dates@p_vc_i') then routine := 5
       else if compp(symbol, 'dates@f_i') then routine := 6
       else if compp(symbol, 'writetime@p_fc_i') then routine := 7
       else if compp(symbol, 'writetime@p_i') then routine := 8
       else if compp(symbol, 'writedate@p_fc_i') then routine := 9
       else if compp(symbol, 'writedate@p_i') then routine := 10
       else if compp(symbol, 'time@f') then routine := 11
       else if compp(symbol, 'local@f_i') then routine := 12
       else if compp(symbol, 'clock@f') then routine := 13
       else if compp(symbol, 'elapsed@f_i') then routine := 14
       else if compp(symbol, 'validfile@f_vc') then routine := 15
       else if compp(symbol, 'validfile@f_pvc') then routine := 16
       else if compp(symbol, 'validpath@f_vc') then routine := 17
       else if compp(symbol, 'validpath@f_pvc') then routine := 18
       else if compp(symbol, 'wild@f_vc') then routine := 19
       else if compp(symbol, 'wild@f_pvc') then routine := 20
       else if compp(symbol, 'getenv@p_vc_vc') then routine := 21
       else if compp(symbol, 'getenv@f_vc') then routine := 22
       else if compp(symbol, 'setenv@p_vc_vc') then routine := 23
       else if compp(symbol, 'setenv@p_pvc_vc') then routine := 24
       else if compp(symbol, 'setenv@p_vc_pvc') then routine := 25
       else if compp(symbol, 'setenv@p_pvc_pvc') then routine := 26
       else if compp(symbol, 'allenv@p_pr(name:0:pvc,data:8:p12,next:16:p2)') then routine := 27
       else if compp(symbol, 'remenv@p_vc') then routine := 28
       else if compp(symbol, 'remenv@p_pvc') then routine := 29
       else if compp(symbol, 'exec@p_vc') then routine := 30
       else if compp(symbol, 'exec@p_pvc') then routine := 31
       else if compp(symbol, 'exece@p_vc_pr(name:0:pvc,data:8:p12,next:16:p2)') then routine := 32
       else if compp(symbol, 'exece@p_pvc_pr(name:0:pvc,data:8:p12,next:16:p2)') then routine := 33
       else if compp(symbol, 'execw@p_vc_i ') then routine := 34
       else if compp(symbol, 'execw@p_pvc_i ') then routine := 35
       else if compp(symbol, 'execew@p_vc_pr(name:0:pvc,data:8:p12,next:16:p2)_i') then routine := 36
       else if compp(symbol, 'execew@p_pvc_pr(name:0:pvc,data:8:p12,next:16:p2)_i') then routine := 37
       else if compp(symbol, 'getcur@p_vc') then routine := 38
       else if compp(symbol, 'getcur@f') then routine := 39
       else if compp(symbol, 'setcur@p_vc') then routine := 40
       else if compp(symbol, 'setcur@p_pvc') then routine := 41
       else if compp(symbol, 'brknam@p_vc_vc_vc_vc') then routine := 42
       else if compp(symbol, 'brknam@p_vc_pvc_pvc_pvc') then routine := 43
       else if compp(symbol, 'brknam@p_pvc_pvc_pvc_pvc') then routine := 44
       else if compp(symbol, 'maknam@p_vc_vc_vc_vc') then routine := 45
       else if compp(symbol, 'maknam@f_vc_vc_pvc') then routine := 46
       else if compp(symbol, 'maknam@f_vc_pvc_vc') then routine := 47
       else if compp(symbol, 'maknam@f_vc_pvc_pvc') then routine := 48
       else if compp(symbol, 'maknam@f_pvc_vc_vc') then routine := 49
       else if compp(symbol, 'maknam@f_pvc_vc_pvc') then routine := 50
       else if compp(symbol, 'maknam@f_pvc_pvc_vc') then routine := 51
       else if compp(symbol, 'maknam@f_pvc_pvc_pvc') then routine := 52
       else if compp(symbol, 'fulnam@p_vc') then routine := 53
       else if compp(symbol, 'fulnam@f_vc') then routine := 54
       else if compp(symbol, 'getpgm@p_vc') then routine := 55
       else if compp(symbol, 'getpgm@f') then routine := 56
       else if compp(symbol, 'getusr@p_vc') then routine := 57
       else if compp(symbol, 'getusr@f') then routine := 58
       else if compp(symbol, 'setatr@p_vc_sx(atexec,atarc,atsys,atdir,atloop)') then routine := 59
       else if compp(symbol, 'setatr@p_pvc_sx(atexec,atarc,atsys,atdir,atloop)') then routine := 60
       else if compp(symbol, 'resatr@p_vc_sx(atexec,atarc,atsys,atdir,atloop)') then routine := 61
       else if compp(symbol, 'resatr@p_pvc_sx(atexec,atarc,atsys,atdir,atloop)') then routine := 62
       else if compp(symbol, 'bakupd@p_vc') then routine := 63
       else if compp(symbol, 'bakupd@p_pvc') then routine := 64
       else if compp(symbol, 'setuper@p_vc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 65
       else if compp(symbol, 'setuper@p_pvc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 66
       else if compp(symbol, 'resuper@p_vc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 67
       else if compp(symbol, 'resuper@p_pvc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 68
       else if compp(symbol, 'setgper@p_vc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 69
       else if compp(symbol, 'setgper@p_pvc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 70
       else if compp(symbol, 'resgper@p_vc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 71
       else if compp(symbol, 'resgper@p_pvc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 72
       else if compp(symbol, 'setoper@p_vc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 73
       else if compp(symbol, 'setoper@p_pvc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 74
       else if compp(symbol, 'resoper@p_vc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 75
       else if compp(symbol, 'resoper@p_pvc_sx(pmread,pmwrite,pmexec,pmdel,pmvis,pmcopy,pmren)') then routine := 76
       else if compp(symbol, 'makpth@p_vc') then routine := 77
       else if compp(symbol, 'makpth@p_pvc') then routine := 78
       else if compp(symbol, 'rempth@p_vc') then routine := 79
       else if compp(symbol, 'rempth@p_pvc') then routine := 80
       else if compp(symbol, 'filchr@p_sc') then routine := 81
       else if compp(symbol, 'optchr@f') then routine := 82
       else if compp(symbol, 'pthchr@f') then routine := 83
       else if compp(symbol, 'latitude@f') then routine := 84
       else if compp(symbol, 'longitude@f') then routine := 85
       else if compp(symbol, 'altitude@f') then routine := 86
       else if compp(symbol, 'country@f') then routine := 87
       else if compp(symbol, 'countrys@p_vc_i') then routine := 88
       else if compp(symbol, 'timezone@f') then routine := 89
       else if compp(symbol, 'daysave@f') then routine := 90
       else if compp(symbol, 'time24hour@f') then routine := 91
       else if compp(symbol, 'language@f') then routine := 92
       else if compp(symbol, 'languages@p_vc_i') then routine := 93
       else if compp(symbol, 'decimal@f') then routine := 94
       else if compp(symbol, 'numbersep@f') then routine := 95
       else if compp(symbol, 'timeorder@f') then routine := 96
       else if compp(symbol, 'dateorder@f') then routine := 97
       else if compp(symbol, 'datesep@f') then routine := 98
       else if compp(symbol, 'timesep@f') then routine := 99
       else if compp(symbol, 'currchr@f') then routine := 100

   { placeholders }

   { terminal is complete }
   end else if compp(modulen, 'terminal') then begin

   { graph is unfinished }
   end else if compp(modulen, 'graph') then begin

   { terminal is complete }
   end else if compp(modulen, 'sound') then begin

   { network is fairly complete }
   end else if compp(modulen, 'network') then begin

   end

end;

{*******************************************************************************

Execute routine by number

Given a routine number, executes that routine. All of the input parameters are
passed on the stack, and the result, if any, also returned on the stack.

All of the input parameters are removed from the stack, leaving just the result
(if any).

The load of parameters is fairly ad-hoc. Value parameters are simply fetched.
VAR and VIEW pameters have to be loaded into a buffer to transfer.

The layout of the parameters and return are:

params+intsize*2: return value
params+intsize:   param1
params:           param2
...

ie., the parameters start at the params address, then climb upwards until the
return value (if any). The params address should be incremented past all
parameters, but left pointing at the return value, if any. Otherwise params
is just left above all parameters.

*******************************************************************************}

procedure ExecuteExternal(
    { number of routine to execute}             routine: integer;
    { address of parameters bottom/result } var params:  integer
);

const 

    strmax = 1000; { size of string buffers }

    WriteOnReadOnlyFile = 73;
    FileModeIncorrect   = 28;

    strparsiz = ptrsize+intsize; { size of string parameter }

type str = packed array [1..strmax] of char;

var a:             integer;
    s, s2, s3, s4: str;
    st:            settype;
    at:            services.attrset;
    ps:            services.permset;
    cs:            schar;
    ch:            char;
    fp:            services.filptr;
    ad, ad2:       address;
    fn:            fileno;
    bl:            boolean;
    ep:            services.envptr;
    sp:            pstring;

 
procedure getstr(sa: address; var s: string);

var a1:  integer;
    l,i: integer;

begin

    clears(s);
    a1 := getadr(sa); { get base address of string }
    l := getadr(sa+adrsize); { get length of string }
    if l > strmax then begin

       writeln('*** String too long for buffer');
       halt

    end;
    { transfer string data }
    for i := 1 to l do s[i] := getchr(a1+i-1)

end;

procedure putstr(var s: str; sa: address);

var i, m: integer;
   da: address;
   sl: integer;

begin

   da := getadr(sa); { get base address of string }
   sl := getadr(sa+adrsize); { get length of string }
   for i := 1 to sl do putchr(da+i-1, ' ');
   m := len(s);
   if m > sl then begin

      writeln('*** String too long for desintation');
      halt

   end;
   for i := 1 to m do begin putchr(da, s[i]); da := da+1 end

end;

procedure getpstr(sa: address; var s: string);

var a1:  integer;
    l,i: integer;

begin

    clears(s);
    a1 := getadr(sa+intsize); { get base address of string }
    l := getadr(sa); { get length of string }
    if l > strmax then begin

       writeln('*** String too long for buffer');
       halt

    end;
    { transfer string data }
    for i := 1 to l do s[i] := getchr(a1+i-1)

end;

procedure putpstr(var s: str; sl: integer; da: address);

var i:  integer;
    ad: address;

begin

   newspc(sl+intsize, ad);
   putadr(da, ad);
   putint(ad, sl);
   ad := ad+intsize;
   for i := 1 to sl do begin putchr(ad, s[i]); ad := ad+1 end

end;

procedure set2atr(view st: settype; out at: services.attrset);

var r: record case boolean of
         false: (st: settype);
         true:  (at: services.attrset)
       end;

begin

    r.st := st;
    at := r.at

end;

procedure atr2set(view at: services.attrset; out st: settype);

var r: record case boolean of
         false: (st: settype);
         true:  (at: services.attrset)
       end;

begin

    r.at := at;
    st := r.st

end;

procedure set2prm(view st: settype; out ps: services.permset);

var r: record case boolean of
         false: (st: settype);
         true:  (ps: services.permset)
       end;

begin

    r.st := st;
    ps := r.ps

end;

procedure prm2set(view ps: services.permset; out st: settype);

var r: record case boolean of
         false: (st: settype);
         true:  (ps: services.permset)
       end;

begin

    r.ps := ps;
    st := r.st

end;

procedure cst2set(view cs: schar; out st: settype);

var r: record case boolean of
         false: (st: settype);
         true:  (cs: schar)
       end;

begin

    r.cs := cs;
    st := r.st

end;

procedure movstr2vm(view s: string; ad: address);

var i: integer;

begin

    for i := 1 to max(s) do begin putchr(ad, s[i]); ad := ad+1 end

end;

procedure movstrfvm(var s: string; ad: address);

var i: integer;

begin

   for i := 1 to max(s) do begin s[i] := getchr(ad); ad := ad+1 end;

end;

{ convert file list fp to memory address la }
procedure cvtflist(fp: services.filptr; var la: address);

var lp: services.filptr; ad, ad2, ad3: address; l: integer;

begin

   ad3 := 0; { set no last entry }
   la := 0; { set no top entry }
   while fp <> nil do begin

      { transfer each record to storage }
      { get space for file record }
      newspc(ptrsize+intsize+intsize+setsize+intsize+intsize+
             intsize+intsize+setsize+setsize+setsize+ptrsize, ad);
      if la = 0 then la := ad; { set top entry }
      { link last to this if last exists }
      if ad3 <> 0 then putadr(ad3, ad);
      l := max(fp^.name^); { get length of name }
      newspc(l+intsize, ad2); { get space for filename }
      putadr(ad, ad2); { place name pointer }
      ad := ad+ptrsize;
      putint(ad2, l); { put string size }
      ad2 := ad2+intsize;
      { move the name string into place }
      movstr2vm(fp^.name^, ad2); { move into place }
      putint(ad, fp^.size);
      ad := ad+intsize;
      putint(ad, fp^.alloc);
      ad := ad+intsize;
      atr2set(fp^.attr, st);
      putset(ad, st);
      ad := ad+setsize;
      putint(ad, fp^.create);
      ad := ad+intsize;
      putint(ad, fp^.modify);
      ad := ad+intsize;
      putint(ad, fp^.access);
      ad := ad+intsize;
      putint(ad, fp^.backup);
      ad := ad+intsize;
      prm2set(fp^.user, st);
      putset(ad, st);
      ad := ad+setsize;
      prm2set(fp^.group, st);
      putset(ad, st);
      ad := ad+setsize;
      prm2set(fp^.other, st);
      putset(ad, st);
      ad := ad+setsize;
      ad3 := ad; { set last entry link }
      putadr(ad, nilval); { clear next (with nil value) }
      lp := fp; { save last entry }
      fp := fp^.next; { link next entry }
      dispose(lp^.name); { release all storage from entry }
      dispose(lp)

   end;
   { replace with our value for nil }
   if la = 0 then la := nilval

end;

procedure cvtenv(ep: services.envptr; var la: address);

var lp: services.envptr; ad, ad2, ad3: address; l: integer;

begin

   ad3 := 0; { set no last entry }
   la := 0; { set no top entry }
   while ep <> nil do begin { translate list }

      { transfer each record to storage }
      { get space for environment record }
      newspc(ptrsize*3, ad);
      if la = 0 then la := ad; { set top entry }
      { link last to this if last exists }
      if ad3 <> 0 then putadr(ad3, ad);
      l := max(ep^.name^); { get length of name }
      newspc(l+intsize, ad2); { get space for name }
      putadr(ad, ad2); { place name }
      ad := ad+ptrsize;
      putint(ad2, l); { put string size }
      ad2 := ad2+intsize;
      movstr2vm(ep^.name^, ad2); { move the name string into place }
      l := max(ep^.data^); { get length of data }
      newspc(l+intsize, ad2); { get space for name }
      putadr(ad, ad2); { place name }
      ad := ad+ptrsize;
      putint(ad2, l); { put string size }
      ad2 := ad2+intsize;
      movstr2vm(ep^.data^, ad2); { move the name string into place }
      ad3 := ad; { set last entry link }
      putadr(ad, nilval); { clear next (with nil value) }
      lp := ep; { save last entry }
      ep := ep^.next; { link next entry }
      dispose(lp^.name); { release all storage from entry }
      dispose(lp^.data);
      dispose(lp)

   end;
   { replace with our value for nil }
   if la = 0 then la := nilval

   { need to free up original C side entries we copied }

end;

procedure getenv(ad: address; out ep: services.envptr);

var ep2, lp: services.envptr; l: integer; ad2: address;

begin

   ep := nil; { set no top }
   lp := nil; { set no last }
   while ad <> nilval do begin

      new(ep2); { get new entry }
      ep2^.next := nil; { set no next }
      if ep = nil then ep := ep2; { set top entry if first }
      if lp <> nil then lp^.next := ep2; { link last to this if last exists }
      { pick up name }
      ad2 := getadr(ad); { index the string }
      ad := ad+adrsize;
      l := getint(ad2); { get the length }
      ad2 := ad2+intsize; { index base of string }
      new(ep2^.name, l+1); { allocate space for that }
      movstrfvm(ep2^.name^, ad2); { move string into place }
      { pick up data }
      ad2 := getadr(ad);
      ad := ad+adrsize;
      l := getint(ad2); { get the length }
      ad2 := ad2+intsize; { index base of string }
      new(ep2^.data, l+1); { allocate space for that }
      movstrfvm(ep2^.data^, ad2); { move string into place }
      lp := ep2; { set last entry }
      ad := getadr(ad) { link next }

   end

end;

begin

    case routine of

       1: begin { procedure list(view f: string; var l: filptr) }

           getstr(params+adrsize, s); { load string }
           services.list(s, fp); { get files list }
           cvtflist(fp, ad2); { convert list to memory }
           ad := getadr(params); { get address of list pointer }
           putadr(ad, ad2); { place to vm client }
           { return past parameters }
           params := params+ptrsize+strparsiz

       end;

       2: begin { procedure list(view f: pstring; var  l: filptr) }

           getpstr(params+adrsize, s); { load string }
           services.list(s, fp); { get files list }
           cvtflist(fp, ad2); { convert list to memory }
           ad := getadr(params); { get address of list pointer }
           putadr(ad, ad2); { place to vm client }
           { return past parameters }
           params := params+ptrsize+ptrsize

       end;

       3:  begin { procedure times(out s: string; t: integer) }

           services.times(s, getint(params));
           putstr(s, params+intsize);
           params := params+intsize+strparsiz

       end;

       4:  begin { function times(t: integer): pstring }

           sp := services.times(getint(params));
           newspc(max(sp^)+intsize, ad); { get dynamic space }
           putint(ad, max(sp^)); { place template }
           movstr2vm(sp^, ad+intsize); { move string into place }
           dispose(sp); { release string }
           params := params+intsize; { return past parameters }
           putadr(params, ad)

       end;

       5:  begin { procedure dates(out s: string; t: integer) }

           services.dates(s, getint(params));
           putstr(s, params+intsize);
           params := params+intsize+strparsiz

       end;

       6:  begin { function dates(t: integer): pstring;}

           sp := services.dates(getint(params));
           newspc(max(sp^)+intsize, ad); { get dynamic space }
           putint(ad, max(sp^)); { place template }
           movstr2vm(sp^, ad+intsize); { move string into place }
           dispose(sp); { release string }
           params := params+intsize; { return past parameters }
           putadr(params, ad)

       end;


       7:  begin { procedure writetime(var f: text; t: integer) }

           a := getint(params);
           ad := getadr(params+intsize); valfil(ad);
           if fn <= commandfn then case fn of
              outputfn,
              errorfn,
              listfn: begin services.writetime(output, a); newline := false end;
              prrfn: services.writetime(prr, a);
              prdfn,inputfn,
              commandfn: errore(WriteOnReadOnlyFile);
           end else begin
                if filstate[fn] <> fwrite then errore(FileModeIncorrect);
                services.writetime(filtable[fn], a)
           end;
           params := params+intsize+adrsize

       end;

       8:  begin { procedure writetime(t: integer) }

           a := getint(params);
           services.writetime(output, a); 
           newline := false;
           params := params+intsize

       end;

       9:  begin { procedure writedate(var f: text; t: integer) }

           a := getint(params);
           ad := getadr(params+intsize); valfil(ad);
           if fn <= commandfn then case fn of
              outputfn,
              errorfn,
              listfn: begin services.writedate(output, a); newline := false end;
              prrfn: services.writedate(prr, a);
              prdfn,inputfn,
              commandfn: errore(WriteOnReadOnlyFile);
           end else begin
                if filstate[fn] <> fwrite then errore(FileModeIncorrect);
                services.writedate(filtable[fn], a)
           end;
           params := params+intsize+adrsize

       end;

       10:  begin { procedure writedate(t: integer) }

           a := getint(params);
           services.writedate(output, a); 
           newline := false;
           params := params+intsize

       end;

       11: begin { function time: integer }

           a := services.time;
           putint(params, a)

       end;

       12: begin { function local(t: integer) }

           a := getint(params);
           a := services.local(a);
           params := params+intsize;
           putint(params, a)

       end;

       13: begin { function clock: integer }

           a := services.clock;
           putint(params, a)

       end;

       14: begin { function elapsed(r: integer): integer }

           a := getint(params);
           a := services.elapsed(a);
           params := params+intsize;
           putint(params, a)

       end;

       15: begin { function validfile(view s: string): boolean }

           getstr(params, s);
           bl := services.validfile(s);
           params := params+strparsiz;
           putint(params, ord(bl))

       end;

       16: begin { function validfile(view s: pstring): boolean }

           getpstr(params, s);
           bl := services.validfile(s);
           params := params+ptrsize;
           putint(params, ord(bl))

       end;

       17: begin { function validpath(view s: string): boolean }

           getstr(params, s);
           bl := services.validpath(s);
           params := params+strparsiz;
           putint(params, ord(bl))

       end;

       18: begin { function validpath(view s: pstring): boolean }

           getpstr(params, s);
           bl := services.validpath(s);
           params := params+ptrsize;
           putint(params, ord(bl))

       end;

       19: begin { function wild(view s: string): boolean }

           getstr(params, s);
           bl := services.wild(s);
           params := params+strparsiz;
           putint(params, ord(bl))

       end;

       20: begin { function wild(view s: pstring): boolean }

           getpstr(params, s);
           bl := services.wild(s);
           params := params+ptrsize;
           putint(params, ord(bl))

       end;

       21: begin { procedure getenv(view ls: string; out ds: string) }

          getstr(params+strparsiz, s);
          services.getenv(s, s2);
          putstr(s2, params);
          params := params+strparsiz*2

       end;

       22: begin { function getenv(view ls: string): pstring }

          getstr(params, s);
          sp := services.getenv(s);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+strparsiz; { return past parameters }
          putadr(params, ad)

       end;

       23: begin { procedure setenv(view sn, sd: string) }

          getstr(params+strparsiz, s);
          getstr(params, s2);
          services.setenv(s, s2);
          params := params+strparsiz*2

       end;

       24: begin { procedure setenv(sn: pstring; view sd: string) }

          getpstr(params+strparsiz, s);
          getstr(params, s2);
          services.setenv(s, s2);
          params := params+strparsiz+ptrsize

       end;

       25: begin { procedure setenv(view sn: string; sd: pstring) }

          getstr(params+ptrsize, s);
          getpstr(params, s2);
          services.setenv(s, s2);
          params := params+ptrsize+strparsiz

       end;

       26: begin { procedure setenv(sn, sd: pstring) }

          getpstr(params+ptrsize, s);
          getpstr(params, s2);
          services.setenv(s, s2);
          params := params+ptrsize*2

       end;

       27: begin { procedure allenv(var el: envptr) }

          services.allenv(ep); { get environment list }
          cvtenv(ep, ad2); { convert list to store }
          ad := getadr(params); { get address of list pointer }
          putadr(ad, ad2); { place to vm client }
          params := params+ptrsize

       end;

       28: begin { procedure remenv(view sn: string) }

          getstr(params, s);
          services.remenv(s);
          params := params+strparsiz

       end;

       29: begin { procedure remenv(view sn: pstring) }

          getpstr(params, s);
          services.remenv(s);
          params := params+ptrsize

       end;

       30: begin { procedure exec(view cmd: string) }

          getstr(params, s);
          services.exec(s);
          params := params+strparsiz

       end;

       31: begin { procedure exec(view cmd: pstring) }

          getpstr(params, s);
          services.exec(s);
          params := params+ptrsize

       end;

       32: begin { procedure exece(view cmd: string; el: envptr) }

          getstr(params+adrsize, s);
          getenv(getadr(params), ep);
          services.exece(s, ep);
          params := params+ptrsize+strparsiz

       end;

       33: begin { procedure exece(view cmd: pstring; el: envptr) }

          getpstr(params+adrsize, s);
          getenv(getadr(params), ep);
          services.exece(s, ep);
          params := params+ptrsize+ptrsize

       end;

       34: begin { procedure execw(view cmd: string; out e: integer) }

          getstr(params+adrsize, s);
          services.execw(s, a);
          putint(getadr(params), a);
          params := params+ptrsize+strparsiz

       end;

       35: begin { procedure execw(view cmd: pstring; out e: integer) }

          getpstr(params+adrsize, s);
          services.execw(s, a);
          putint(getadr(params), a);
          params := params+ptrsize+ptrsize

       end;

       36: begin { procedure execew(view cmd: string; el: envptr; out e: integer) }

          getstr(params+adrsize*2, s);
          getenv(getadr(params+adrsize), ep);
          services.execew(s, ep, a);
          putint(getadr(params), a);
          params := params+ptrsize+ptrsize+strparsiz

       end;

       37: begin { procedure execew(view cmd: pstring; el: envptr; out e: integer) }

          getpstr(params+adrsize*2, s);
          getenv(getadr(params+adrsize), ep);
          services.execew(s, ep, a);
          putint(getadr(params), a);
          params := params+ptrsize+ptrsize+ptrsize

       end;

       38: begin { procedure getcur(out fn: string) }

           services.getcur(s);
           putstr(s, params);
           params := params+strparsiz

       end;

       39:  begin { function getcur: pstring }

           sp := services.getcur;
           newspc(max(sp^)+intsize, ad); { get dynamic space }
           putint(ad, max(sp^)); { place template }
           movstr2vm(sp^, ad+intsize); { move string into place }
           dispose(sp); { release string }
           putadr(params, ad)

       end;

       40: begin { procedure setcur(view fn: string) }

          getstr(params, s);
          services.setcur(s);
          params := params+strparsiz

       end;

       41: begin { procedure setcur(view fn: pstring) }

          getpstr(params, s);
          services.setcur(s);
          params := params+ptrsize

       end;

       42: begin { procedure brknam(view fn: string; out p, n, e: string) }

          getstr(params+strparsiz*3, s);
          services.brknam(s, s2, s3, s4);
          putstr(s2, params+strparsiz*2);
          putstr(s3, params+strparsiz);
          putstr(s4, params);
          params := params+strparsiz*4

       end;

       43: begin { procedure brknam(view fn: string; out p, n, e: pstring) }

          getstr(params+ptrsize*3, s);
          services.brknam(s, s2, s3, s4);
          putpstr(s2, len(s2), getadr(params+ptrsize*2));
          putpstr(s3, len(s3), getadr(params+ptrsize));
          putpstr(s4, len(s4), getadr(params));
          params := params+ptrsize*3+strparsiz

       end;



       44: begin { procedure brknam(view fn: pstring; out p, n, e: pstring) }

          getpstr(params+ptrsize*3, s);
          services.brknam(s, s2, s3, s4);
          putpstr(s2, len(s2), getadr(params+ptrsize*2));
          putpstr(s3, len(s3), getadr(params+ptrsize));
          putpstr(s4, len(s4), getadr(params));
          params := params+ptrsize*3+strparsiz

       end;


       45: begin { procedure maknam(out fn: string; view p, n, e: string) }

          getstr(params+strparsiz+strparsiz, s2);
          getstr(params+strparsiz, s3);
          getstr(params, s4);
          services.maknam(s, s2, s3, s4);
          putstr(s, params+strparsiz*3);
          params := params+strparsiz*4

       end;

       46: begin { function maknam(view p, n, e: string): pstring }

          getstr(params+strparsiz*2, s2);
          getstr(params+strparsiz, s3);
          getstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+strparsiz*3;
          putadr(params, ad)

       end;

       47: begin { function maknam(view p: string; view n: string; e: pstring): pstring }

          getstr(params+ptrsize+strparsiz, s2);
          getstr(params+ptrsize, s3);
          getpstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+ptrsize+strparsiz+strparsiz;
          putadr(params, ad)

       end;

       48: begin { function maknam(view p: string; view n: pstring; e: string): pstring }

          getstr(params+strparsiz+ptrsize, s2);
          getpstr(params+strparsiz, s3);
          getstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+strparsiz+ptrsize+strparsiz;
          putadr(params, ad)

       end;

       49: begin { function maknam(view p: string; view n: pstring; e: pstring): pstring }

          getstr(params+ptrsize+ptrsize, s2);
          getpstr(params+ptrsize, s3);
          getpstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+ptrsize+ptrsize+strparsiz;
          putadr(params, ad)

       end;

       50: begin { function maknam(view p: pstring; view n: string; e: string): pstring }

          getpstr(params+strparsiz*2, s2);
          getstr(params+strparsiz, s3);
          getstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+strparsiz*2+ptrsize;
          putadr(params, ad)

       end;

       51: begin { function maknam(view p: pstring; view n: string; e: pstring): pstring }

          getpstr(params+ptrsize+strparsiz, s2);
          getstr(params+ptrsize, s3);
          getpstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+ptrsize+strparsiz+ptrsize;
          putadr(params, ad)

       end;

       52: begin { function maknam(view p: pstring; view n: pstring; e: string): pstring }

          getpstr(params+strparsiz+ptrsize, s2);
          getpstr(params+strparsiz, s3);
          getstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+strparsiz+ptrsize*2;
          putadr(params, ad)

       end;

       53: begin { function maknam(view p: pstring; view n: pstring; e: pstring): pstring }

          getpstr(params+ptrsize+ptrsize, s2);
          getpstr(params+ptrsize, s3);
          getstr(params, s4);
          sp := services.maknam(s2, s3, s4);
          newspc(max(sp^)+intsize, ad); { get dynamic space }
          putint(ad, max(sp^)); { place template }
          movstr2vm(sp^, ad+intsize); { move string into place }
          dispose(sp); { release string }
          params := params+ptrsize*3;
          putadr(params, ad)

       end;

       54: begin { procedure fulnam(var fn: string) }

           getstr(params, s);
           services.fulnam(s);
           putstr(s, params);
           params := params+strparsiz

       end;

       55: begin { function fulnam(view fn: string): pstring }

           getstr(params, s);
           sp := services.fulnam(s);
           newspc(max(sp^)+intsize, ad); { get dynamic space }
           putint(ad, max(sp^)); { place template }
           movstr2vm(sp^, ad+intsize); { move string into place }
           dispose(sp); { release string }
           params := params+strparsiz; { return past parameters }
           putadr(params, ad)

       end;

       56: begin { procedure getpgm(out p: string) }

           services.getpgm(s);
           putstr(s, params);
           params := params+strparsiz

       end;

       57: begin { function getpgm: pstring }

           sp := services.getpgm;
           newspc(max(sp^)+intsize, ad); { get dynamic space }
           putint(ad, max(sp^)); { place template }
           movstr2vm(sp^, ad+intsize); { move string into place }
           dispose(sp); { release string }
           params := params; { return past parameters }
           putadr(params, ad)

       end;


       58: begin { procedure getusr(out fn: string) }

           services.getusr(s);
           putstr(s, params);
           params := params+strparsiz

       end;

       59: begin { function getusr: pstring }

           sp := services.getusr;
           newspc(max(sp^)+intsize, ad); { get dynamic space }
           putint(ad, max(sp^)); { place template }
           movstr2vm(sp^, ad+intsize); { move string into place }
           dispose(sp); { release string }
           params := params; { return past parameters }
           putadr(params, ad)

       end;

       60: begin { procedure setatr(view fn: string; a: attrset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2atr(st, at);
           services.setatr(s, at);
           params := params+setsize+strparsiz

       end;

       61: begin { procedure setatr(view fn: pstring; a: attrset) }

           getpstr(params+setsize, s);
           getset(params, st);
           set2atr(st, at);
           services.setatr(s, at);
           params := params+setsize+ptrsize

       end;

       62: begin { procedure resatr(view fn: string; a: attrset)}

           getstr(params+setsize, s);
           getset(params, st);
           set2atr(st, at);
           services.resatr(s, at);
           params := params+setsize+strparsiz

       end;

       63: begin { procedure resatr(view fn: sstring; a: attrset)}

           getpstr(params+setsize, s);
           getset(params, st);
           set2atr(st, at);
           services.resatr(s, at);
           params := params+setsize+strparsiz

       end;

       64: begin { procedure bakupd(view fn: string) }

          getstr(params, s);
          services.bakupd(s);
          params := params+strparsiz

       end;

       65: begin { procedure setuper(view fn: string; p: permset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setuper(s, ps);
           params := params+setsize+strparsiz

       end;

       66: begin { procedure setuper(view fn: pstring; p: permset) }

           getpstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setuper(s, ps);
           params := params+setsize+ptrsize

       end;

       67: begin { procedure resuper(view fn: string; p: permset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resuper(s, ps);
           params := params+setsize+strparsiz

       end;

       68: begin { procedure resuper(view fn: pstring; p: permset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resuper(s, ps);
           params := params+setsize+ptrsize

       end;

       69: begin { procedure setgper(view fn: string; p: permset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setgper(s, ps);
           params := params+setsize+strparsiz

       end;

       70: begin { procedure setgper(view fn: pstring; p: permset) }

           getpstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setgper(s, ps);
           params := params+setsize+ptrsize

       end;

       71: begin { procedure resgper(view fn: string; p: permset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resgper(s, ps);
           params := params+setsize+strparsiz

       end;

       72: begin { procedure resgper(view fn: pstring; p: permset) }

           getpstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resgper(s, ps);
           params := params+setsize+ptrsize

       end;

       73: begin { procedure setoper(view fn: string; p: permset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setoper(s, ps);
           params := params+setsize+strparsiz

       end;

       74: begin { procedure setoper(view fn: pstring; p: permset) }

           getpstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setoper(s, ps);
           params := params+setsize+ptrsize

       end;

       75: begin { procedure resoper(view fn: string; p: permset) }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resoper(s, ps);
           params := params+setsize+strparsiz

       end;

       76: begin { procedure resoper(view fn: pstring; p: permset) }

           getpstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resoper(s, ps);
           params := params+setsize+ptrsize

       end;

       77: begin { procedure makpth(view fn: string) }

          getstr(params, s);
          services.makpth(s);
          params := params+strparsiz

       end;

       78: begin { procedure makpth(view fn: pstring) }

          getpstr(params, s);
          services.makpth(s);
          params := params+ptrsize

       end;

       79: begin { procedure rempth(view fn: string) }

          getstr(params, s);
          services.rempth(s);
          params := params+strparsiz

       end;

       80: begin { procedure rempth(view fn: pstring) }

          getpstr(params, s);
          services.rempth(s);
          params := params+ptrsize

       end;

       81: begin { procedure filchr(out fc: schar) }

           services.filchr(cs);
           cst2set(cs, st);
           putset(getadr(params), st);
           params := params+ptrsize

       end;

       82: begin { function optchr: char }

           ch := services.optchr;
           putint(params, ord(ch))

       end;

       83: begin { function pthchr: char }

           ch := services.pthchr;
           putint(params, ord(ch))

       end;

       84: begin { function latitude: integer }

           a := services.latitude;
           putint(params, a)

       end;

       85: begin { function longitude: integer }

           a := services.longitude;
           putint(params, a)

       end;

       86: begin { function altitude: integer }

           a := services.altitude;
           putint(params, a)

       end;

       87: begin { function country: integer }

           a := services.country;
           putint(params, a)

       end;

       88: begin { procedure countrys(out s: string; c: integer) }

           services.countrys(s, getint(params));
           putstr(s, params+intsize);
           params := params+intsize+strparsiz

       end;

       89: begin { function timezone: integer }

           a := services.timezone;
           putint(params, a)

       end;

       90: begin { function daysave: boolean }

           bl := services.daysave;
           putint(params, ord(bl))

       end;

       91: begin { function time24hour: boolean }

           bl := services.time24hour;
           putint(params, ord(bl))

       end;

       92: begin { function language: integer}

           a := services.language;
           params := params+intsize;
           putint(params, a)

       end;

       93: begin { procedure languages(out s: string; l: integer) }

           services.languages(s, getint(params));
           putstr(s, params+intsize);
           params := params+intsize+strparsiz

       end;

       94: begin { function decimal: char; external }

           ch := services.decimal;
           putint(params, ord(ch))

       end;

       95: begin { function numbersep: char }

           ch := services.numbersep;
           putint(params, ord(ch))

       end;

       96: begin { function timeorder: integer }

           a := services.timeorder;
           putint(params, a)

       end;

       97: begin { function dateorder: integer }

           a := services.dateorder;
           putint(params, a)

       end;

       98: begin { function datesep: char }

           ch := services.datesep;
           putint(params, ord(ch))

       end;

       99: begin { function timesep: char }

           ch := services.timesep;
           putint(params, ord(ch))

       end;

       100: begin { function currchr: char }

           ch := services.currchr;
           putint(params, ord(ch))

       end;

    end

end;

{*******************************************************************************

Get number of routines

Retrieves the total number of routines available.

*******************************************************************************}

function NumExternal: integer;

begin

    NumExternal := 58

end;

begin
end.