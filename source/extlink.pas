{*******************************************************************************

Pascaline externals interface

This module just stubs off the externals interface in pint.pas. The Pascaline
interface is not defined at this time.

*******************************************************************************}

module extlink;

joins services; { system services }

uses strings,  { string functions }
     pint_mem; { low level vm access for pint }

{*******************************************************************************

External routine definitions

Each of these brings an externally defined routine into Pascal callable form.

*******************************************************************************}

{ Place external routine declarations here }

{*******************************************************************************

Lookup symbol/module name

Given a module name and symbol name, returns a number used to execute the given
module call. If no routine is found, a zero is returned. If a routine is found,
this can be used as a key to execute the correct routine.

*******************************************************************************}

procedure LookupExternal(
    { name of module }           view modulen: string;
    { symbol within module }     view symbol:  string;
    { resulting routine number } var routine: integer
    );

begin

   routine := 0; { set no routine found }

   if compp(modulen, 'services') then begin

       if compp(symbol, 'list') then routine := 1
       else if compp(symbol, 'times') then routine := 2
       else if compp(symbol, 'dates') then routine := 3
       else if compp(symbol, 'writetime') then routine := 4
       else if compp(symbol, 'writedate') then routine := 5
       else if compp(symbol, 'time') then routine := 6
       else if compp(symbol, 'local') then routine := 7
       else if compp(symbol, 'clock') then routine := 8
       else if compp(symbol, 'elapsed') then routine := 9
       else if compp(symbol, 'validfile') then routine := 10
       else if compp(symbol, 'validpath') then routine := 11
       else if compp(symbol, 'wild') then routine := 12
       else if compp(symbol, 'getenv') then routine := 13
       else if compp(symbol, 'setenv') then routine := 14
       else if compp(symbol, 'allenv') then routine := 15
       else if compp(symbol, 'remenv') then routine := 16
       else if compp(symbol, 'exec') then routine := 17
       else if compp(symbol, 'exece ') then routine := 18
       else if compp(symbol, 'execw ') then routine := 19
       else if compp(symbol, 'execew') then routine := 20
       else if compp(symbol, 'getcur') then routine := 21
       else if compp(symbol, 'setcur') then routine := 22
       else if compp(symbol, 'brknam') then routine := 23
       else if compp(symbol, 'maknam') then routine := 24
       else if compp(symbol, 'fulnam') then routine := 25
       else if compp(symbol, 'getpgm') then routine := 26
       else if compp(symbol, 'getusr') then routine := 27
       else if compp(symbol, 'setatr') then routine := 28
       else if compp(symbol, 'resatr') then routine := 29
       else if compp(symbol, 'bakupd') then routine := 30
       else if compp(symbol, 'setuper') then routine := 31
       else if compp(symbol, 'resuper') then routine := 32
       else if compp(symbol, 'setgper') then routine := 33
       else if compp(symbol, 'resgper') then routine := 34
       else if compp(symbol, 'setoper') then routine := 35
       else if compp(symbol, 'resoper') then routine := 36
       else if compp(symbol, 'makpth') then routine := 37
       else if compp(symbol, 'rempth') then routine := 38
       else if compp(symbol, 'filchr') then routine := 39
       else if compp(symbol, 'optchr') then routine := 40
       else if compp(symbol, 'pthchr') then routine := 41
       else if compp(symbol, 'latitude ') then routine := 42
       else if compp(symbol, 'longitude') then routine := 43
       else if compp(symbol, 'altitude ') then routine := 44
       else if compp(symbol, 'country') then routine := 45
       else if compp(symbol, 'countrys') then routine := 46
       else if compp(symbol, 'timezone ') then routine := 47
       else if compp(symbol, 'daysave') then routine := 48
       else if compp(symbol, 'time24hour') then routine := 49
       else if compp(symbol, 'language') then routine := 50
       else if compp(symbol, 'languages') then routine := 51
       else if compp(symbol, 'decimal') then routine := 52
       else if compp(symbol, 'numbersep') then routine := 53
       else if compp(symbol, 'timeorder') then routine := 54
       else if compp(symbol, 'dateorder') then routine := 55
       else if compp(symbol, 'datesep') then routine := 56
       else if compp(symbol, 'timesep') then routine := 57
       else if compp(symbol, 'currchr') then routine := 58

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

procedure putstr(var s: str; sl: integer; da: address);

var i, m: integer;

begin

   for i := 1 to sl do putchr(da+i-1, ' ');
   m := len(s);
   if m > sl then begin

      writeln('*** String too long for desintation');
      halt

   end;
   for i := 1 to m do begin putchr(da, s[i]); da := da+1 end

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
      prm2set(fp^.group, st);
      putset(ad, st);
      prm2set(fp^.other, st);
      putset(ad, st);
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

       1: begin { list() }

           getstr(params+adrsize, s); { load string }
           services.list(s, fp); { get files list }
           cvtflist(fp, ad2); { convert list to memory }
           ad := getadr(params); { get address of list pointer }
           putadr(ad, ad2); { place to vm client }
           { return past parameters }
           params := params+ptrsize+ptrsize+intsize

       end;


       2:  begin { times() }

           services.times(s, getint(params));
           putstr(s, getint(params+intsize+ptrsize), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;

       3:  begin { dates() }

           services.dates(s, getint(params));
           putstr(s, getint(params+intsize+ptrsize), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;

       4:  begin { writetime() }

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
           end

       end;

       5:  begin { writedate() }

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
           end

       end;

       6: begin { time() }

           a := services.time;
           putint(params, a)

       end;

       7: begin { local() }

           a := getint(params);
           a := services.local(a);
           params := params+intsize;
           putint(params, a)

       end;

       8: begin { clock() }

           a := services.clock;
           putint(params, a)

       end;

       9: begin { elapsed() }

           a := getint(params);
           a := services.elapsed(a);
           params := params+intsize;
           putint(params, a)

       end;

       10: begin { validfile() }

           getstr(params, s);
           bl := services.validfile(s);
           params := params+ptrsize+intsize;
           putint(params, ord(bl))

       end;

       11: begin { validpath() }

           getstr(params, s);
           bl := services.validpath(s);
           params := params+ptrsize+intsize;
           putint(params, ord(bl))

       end;

       12: begin { wild() }

           getstr(params, s);
           bl := services.wild(s);
           params := params+ptrsize+intsize;
           putint(params, ord(bl))

       end;

       13: begin { getenv() }

          getstr(params+ptrsize+intsize, s);
          services.getenv(s, s2);
          putstr(s2, getint(params+ptrsize), getadr(params));
          params := params+ptrsize+intsize+ptrsize+intsize

       end;

       14: begin { setenv() }

          getstr(params+ptrsize+intsize, s);
          getstr(params, s2);
          services.setenv(s, s2);
          params := params+ptrsize+intsize+ptrsize+intsize

       end;

       15: begin { allenv() }

          services.allenv(ep); { get environment list }
          cvtenv(ep, ad2); { convert list to store }
          ad := getadr(params); { get address of list pointer }
          putadr(ad, ad2) { place to vm client }

       end;

       16: begin { remenv() }

          getstr(params, s);
          services.remenv(s);
          params := params+ptrsize+intsize

       end;


       17: begin { exec() }

          getstr(params, s);
          services.exec(s);
          params := params+ptrsize+intsize

       end;

       18: begin { exece() }

          getstr(params+adrsize, s);
          getenv(getadr(params), ep);
          services.exece(s, ep);
          params := params+ptrsize+ptrsize+intsize

       end;

       19: begin { execw() }

          getstr(params+adrsize, s);
          services.execw(s, a);
          putint(getadr(params), a);
          params := params+ptrsize+ptrsize+intsize

       end;

       20: begin { execew }

          getstr(params+adrsize*2, s);
          getenv(getadr(params+adrsize), ep);
          services.execew(s, ep, a);
          putint(getadr(params), a);
          params := params+ptrsize+ptrsize+ptrsize+intsize

       end;

       21: begin { getcur() }

           services.getcur(s);
           putstr(s, getint(params+ptrsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       22: begin { setcur() }

          getstr(params, s);
          services.setcur(s);
          params := params+ptrsize+intsize

       end;

       23: begin { brknam() }

          getstr(params+ptrsize*3+intsize*3, s);
          services.brknam(s, s2, s3, s4);
          putstr(s2, getint(params+ptrsize*3+intsize*2), getadr(params+ptrsize*2+intsize*2));
          putstr(s3, getint(params+ptrsize*2+intsize), getadr(params+ptrsize+intsize));
          putstr(s4, getint(params+ptrsize), getadr(params));
          params := params+(ptrsize+intsize)*4

       end;

       24: begin { maknam() }

          getstr(params+ptrsize+intsize+ptrsize+intsize, s2);
          getstr(params+ptrsize+intsize, s3);
          getstr(params, s4);
          services.maknam(s, s2, s3, s4);
          putstr(s, getint(params+ptrsize+intsize+ptrsize+intsize+ptrsize+intsize+ptrsize),
                    getadr(params+ptrsize+intsize+ptrsize+intsize+ptrsize+intsize));
          params := params+ptrsize+intsize+ptrsize+intsize+ptrsize+intsize+ptrsize+intsize

       end;

       25: begin { fulnam() }

           getstr(params, s);
           services.fulnam(s);
           putstr(s, getint(params+ptrsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       26: begin { getpgm() }

           services.getpgm(s);
           putstr(s, getint(params+ptrsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       27: begin { getusr() }

           services.getusr(s);
           putstr(s, getint(params+ptrsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       28: begin { setatr() }

           getstr(params+setsize, s);
           getset(params, st);
           set2atr(st, at);
           services.setatr(s, at);
           params := params+setsize+ptrsize+intsize

       end;

       29: begin { resatr() }

           getstr(params+setsize, s);
           getset(params, st);
           set2atr(st, at);
           services.resatr(s, at);
           params := params+setsize+ptrsize+intsize

       end;

       30: begin { bakupd() }

          getstr(params, s);
          services.bakupd(s);
          params := params+ptrsize+intsize

       end;

       31: begin { setuper() }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setuper(s, ps);
           params := params+setsize+ptrsize+intsize

       end;

       32: begin { resuper() }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resuper(s, ps);
           params := params+setsize+ptrsize+intsize

       end;

       33: begin { setgper() }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setgper(s, ps);
           params := params+setsize+ptrsize+intsize

       end;

       34: begin { resgper() }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resgper(s, ps);
           params := params+setsize+ptrsize+intsize

       end;

       35: begin { setoper() }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.setoper(s, ps);
           params := params+setsize+ptrsize+intsize

       end;

       36: begin { resoper() }

           getstr(params+setsize, s);
           getset(params, st);
           set2prm(st, ps);
           services.resoper(s, ps);
           params := params+setsize+ptrsize+intsize

       end;


       37: begin { makpth() }

          getstr(params, s);
          services.makpth(s);
          params := params+ptrsize+intsize

       end;

       38: begin { rempth() }

          getstr(params, s);
          services.rempth(s);
          params := params+ptrsize+intsize

       end;

       39: begin { filchr() }

           services.filchr(cs);
           cst2set(cs, st);
           putset(getadr(params), st);
           params := params+ptrsize

       end;

       40: begin { optchr() }

           ch := services.optchr;
           putint(params, ord(ch))

       end;

       41: begin { pthchr() }

           ch := services.pthchr;
           putint(params, ord(ch))

       end;

       42: begin { latitude() }

           a := services.latitude;
           putint(params, a)

       end;

       43: begin { longitude() }

           a := services.longitude;
           putint(params, a)

       end;

       44: begin { altitude() }

           a := services.altitude;
           putint(params, a)

       end;

       45: begin { country() }

           a := services.country;
           putint(params, a)

       end;

       46: begin { countrys() }

           services.countrys(s, getint(params));
           putstr(s, getint(params+intsize+ptrsize), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;

       47: begin { timezone() }

           a := services.timezone;
           putint(params, a)

       end;

       48: begin { daysave() }

           bl := services.daysave;
           putint(params, ord(bl))

       end;

       49: begin { time24hour() }

           bl := services.time24hour;
           putint(params, ord(bl))

       end;

       50: begin { language() }

           a := services.language;
           params := params+intsize;
           putint(params, a)

       end;

       51: begin { languages() }

           services.languages(s, getint(params));
           putstr(s, getint(params+intsize+ptrsize), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;

       52: begin { decimal() }

           ch := services.decimal;
           putint(params, ord(ch))

       end;

       53: begin { numbersep() }

           ch := services.numbersep;
           putint(params, ord(ch))

       end;

       54: begin { timeorder() }

           a := services.timeorder;
           putint(params, a)

       end;

       55: begin { dateorder() }

           a := services.dateorder;
           putint(params, a)

       end;

       56: begin { datesep() }

           ch := services.datesep;
           putint(params, ord(ch))

       end;

       57: begin { timesep() }

           ch := services.timesep;
           putint(params, ord(ch))

       end;

       58: begin { currchr() }

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