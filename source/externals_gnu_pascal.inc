{*******************************************************************************

Petit-ami interface

Defines the calls that make up the Petit-ami support library for Pascaline.

Petit-ami is the collection of Pascaline support library modules written in
C.

*******************************************************************************}

{*******************************************************************************

External routine definitions

Each of these brings an externally defined routine into Pascaline callable form.

*******************************************************************************}

{ Allow GNU Pascal extensions }
{$gnu-pascal}

{ petit-ami declares }
procedure pa_list(f: charptr; l: integer); external name 'pa_list';
procedure pa_times(s: charptr; sl: integer; t: integer); external name 'pa_times';
procedure pa_dates(s: charptr; sl: integer; t: integer); external name 'pa_dates';
function pa_time: integer; external name 'pa_time';
function pa_local(t: integer): integer; external name 'pa_local';
function pa_clock: integer; external name 'pa_clock';
function pa_elapsed(t: integer): integer; external name 'pa_elapsed';
function pa_validfile(s: charptr): integer; external name 'pa_validfile';
function pa_validpath(s: charptr): integer; external name 'pa_validpath';
function pa_wild(s: charptr): integer; external name 'pa_wild';
procedure pa_getenv(ls,ds: charptr; dsl: integer); external name 'pa_getenv';
procedure pa_setenv(ls,ds: charptr); external name 'pa_setenv';
procedure pa_allenv(l: integer); external name 'pa_allenv';
procedure pa_remenv(sn: charptr); external name 'pa_remenv';
procedure pa_exec(cmd: charptr); external name 'pa_exec';

{ not implemented }
procedure pa_exece(cmd: charptr; el: integer); external name 'pa_exece';

procedure pa_execw(cmd: charptr; var e: CInteger); external name 'pa_execw';

{ not implemented }
procedure pa_execew(cmd: charptr; el: integer; e: integer); external name 'pa_execew';

procedure pa_getcur(fn: charptr; l: integer); external name 'pa_getcur';
procedure pa_setcur(fn: charptr); external name 'pa_setcur';
procedure pa_brknam(fn: charptr; p: charptr; pl: integer; n: charptr; nl: integer;
                    e: charptr; el: integer); external name 'pa_brknam';
procedure pa_maknam(fn: charptr; fnl: integer; p, n, e: charptr); external name 'pa_maknam';
procedure pa_fulnam(fn: charptr; fnl: integer); external name 'pa_fulnam';
procedure pa_getpgm(p: charptr; pl: integer); external name 'pa_getpgm';
procedure pa_getusr(fn: charptr; fnl: integer); external name 'pa_getusr';
procedure pa_setatr(fn: charptr; a: integer); external name 'pa_setatr';
procedure pa_resatr(fn: charptr; a: integer); external name 'pa_resatr';
procedure pa_bakupd(fn: charptr); external name 'pa_bakupd';
procedure pa_setuper(fn: charptr; p: integer); external name 'pa_setuper';
procedure pa_resuper(fn: charptr; p: integer); external name 'pa_resuper';
procedure pa_setgper(fn: charptr; p: integer); external name 'pa_setgper';
procedure pa_resgper(fn: charptr; p: integer); external name 'pa_resgper';
procedure pa_setoper(fn: charptr; p: integer); external name 'pa_setoper';
procedure pa_resoper(fn: charptr; p: integer); external name 'pa_resoper';
procedure pa_makpth(fn: charptr); external name 'pa_makpth';
procedure pa_rempth(fn: charptr); external name 'pa_rempth';
procedure pa_filchr(fc: integer); external name 'pa_filchr';
function pa_optchr: char; external name 'pa_optchr';
function pa_pthchr: char; external name 'pa_pthchr';
function pa_latitude: integer; external name 'pa_latitude';
function pa_longitude: integer; external name 'pa_longitude';
function pa_altitude: integer; external name 'pa_altitude';
function pa_country: integer; external name 'pa_country';
procedure pa_countrys(s: charptr; sl: integer; c: integer); external name 'pa_countrys';
function pa_timezone: integer; external name 'pa_timezone';
function pa_daysave: integer; external name 'pa_daysave';
function pa_time24hour: integer; external name 'pa_time24hour';
function pa_language: integer; external name 'pa_language';
procedure pa_languages(s: charptr; sl: integer; l: integer); external name 'pa_languages';
function pa_decimal: char; external name 'pa_decimal';
function pa_numbersep: char; external name 'pa_numbersep';
function pa_timeorder: integer; external name 'pa_timeorder';
function pa_dateorder: integer; external name 'pa_dateorder';
function pa_datesep: char; external name 'pa_datesep';
function pa_timesep: char; external name 'pa_timesep';
function pa_currchr: char; external name 'pa_currchr';

{ external C support routines }
function malloc(s: integer): charptr; external name 'malloc';

{ Restore to ISO 7185 Pascal language }
{$classic-pascal-level-0}

{*******************************************************************************

Lookup symbol/module name

Given a module name and symbol name, returns a number used to execute the given
module call. If no routine is found, a zero is returned. If a routine is found,
this can be used as a key to execute the correct routine.

*******************************************************************************}

procedure LookupExternal(
    { name of module }           var module: symnam;
    { symbol within module }     var symbol: symnam;
    { resulting routine number } var routine: integer
    );

begin

   routine := 0; { set no routine found }

   if module = 'services            ' then begin

       if symbol = 'list                ' then routine := 1
       else if symbol = 'times               ' then routine := 2
       else if symbol = 'dates               ' then routine := 3
       else if symbol = 'writetime           ' then routine := 4
       else if symbol = 'writedate           ' then routine := 5
       else if symbol = 'time                ' then routine := 6
       else if symbol = 'local               ' then routine := 7
       else if symbol = 'clock               ' then routine := 8
       else if symbol = 'elapsed             ' then routine := 9
       else if symbol = 'validfile           ' then routine := 10
       else if symbol = 'validpath           ' then routine := 11
       else if symbol = 'wild                ' then routine := 12
       else if symbol = 'getenv              ' then routine := 13
       else if symbol = 'setenv              ' then routine := 14
       else if symbol = 'allenv              ' then routine := 15
       else if symbol = 'remenv              ' then routine := 16
       else if symbol = 'exec                ' then routine := 17
       else if symbol = 'exece               ' then routine := 18
       else if symbol = 'execw               ' then routine := 19
       else if symbol = 'execew              ' then routine := 20
       else if symbol = 'getcur              ' then routine := 21
       else if symbol = 'setcur              ' then routine := 22
       else if symbol = 'brknam              ' then routine := 23
       else if symbol = 'maknam              ' then routine := 24
       else if symbol = 'fulnam              ' then routine := 25
       else if symbol = 'getpgm              ' then routine := 26
       else if symbol = 'getusr              ' then routine := 27
       else if symbol = 'setatr              ' then routine := 28
       else if symbol = 'resatr              ' then routine := 29
       else if symbol = 'bakupd              ' then routine := 30
       else if symbol = 'setuper             ' then routine := 31
       else if symbol = 'resuper             ' then routine := 32
       else if symbol = 'setgper             ' then routine := 33
       else if symbol = 'resgper             ' then routine := 34
       else if symbol = 'setoper             ' then routine := 35
       else if symbol = 'resoper             ' then routine := 36
       else if symbol = 'makpth              ' then routine := 37
       else if symbol = 'rempth              ' then routine := 38
       else if symbol = 'filchr              ' then routine := 39
       else if symbol = 'optchr              ' then routine := 40
       else if symbol = 'pthchr              ' then routine := 41
       else if symbol = 'latitude            ' then routine := 42
       else if symbol = 'longitude           ' then routine := 43
       else if symbol = 'altitude            ' then routine := 44
       else if symbol = 'country             ' then routine := 45
       else if symbol = 'countrys            ' then routine := 46
       else if symbol = 'timezone            ' then routine := 47
       else if symbol = 'daysave             ' then routine := 48
       else if symbol = 'time24hour          ' then routine := 49
       else if symbol = 'language            ' then routine := 50
       else if symbol = 'languages           ' then routine := 51
       else if symbol = 'decimal             ' then routine := 52
       else if symbol = 'numbersep           ' then routine := 53
       else if symbol = 'timeorder           ' then routine := 54
       else if symbol = 'dateorder           ' then routine := 55
       else if symbol = 'datesep             ' then routine := 56
       else if symbol = 'timesep             ' then routine := 57
       else if symbol = 'currchr             ' then routine := 58
       else if symbol = 'seterr              ' then routine := 59

   { placeholders }

   { terminal is complete }
   end else if module = 'terminal            ' then begin

   { graph is unfinished }
   end else if module = 'graph               ' then begin

   { terminal is complete }
   end else if module = 'sound               ' then begin

   { network is fairly complete }
   end else if module = 'network             ' then begin

   end

end;

{*******************************************************************************

Execute routine by number

Given a routine number, executes that routine. The bottom of the input 
parameters is given, and the result above that. On return, the parameters are
skipped and the address of the return value is given.

The load of parameters is fairly ad-hoc. Value parameters are simply fetched.
VAR and VIEW pameters have to be loaded into a buffer to transfer.

*******************************************************************************}

{ Allow GNU Pascal extensions }
{$gnu-pascal}

procedure ExecuteExternal(
    { number of routine to execute}             routine: integer;
    { address of parameters bottom/result } var params:  address
    );

const strmax = 1000;

type str = packed array [1..strmax] of char;
     charptr = ^char;
     { attributes }
     attribute = (atexec,  { is an executable file type }
                  atarc,   { has been archived since last modification }
                  atsys,   { is a system special file }
                  atdir,   { is a directory special file }
                  atloop); { contains heriarchy loop }
     attrset = set of attribute; { attributes in a set }
     { permissions }
     permission = (pmread,  { may be read }
                   pmwrite, { may be written }
                   pmexec,  { may be executed }
                   pmdel,   { may be deleted }
                   pmvis,   { may be seen in directory listings }
                   pmcopy,  { may be copied }
                   pmren);  { may be renamed/moved }
     permset = set of permission; { permissions in a set }
     { standard directory format }
     filptr = ^filrec; { pointer to file records }
     filrec = record

       { note sets must be cinteger for spacing and pickup vs. the C version }
       name:    charptr;  { name of file }
       size:    integer;  { size of file }
       alloc:   integer;  { allocation of file }
       attr:    cinteger{attrset};  { attributes }
       create:  integer; { time of creation }
       modify:  integer; { time of last modification }
       access:  integer; { time of last access }
       backup:  integer; { time of last backup }
       user:    cinteger{permset};  { user permissions }
       group:   cinteger{permset};  { group permissions }
       other:   cinteger{permset};  { other permissions }
       next:    filptr    { next entry in list }

     end;
     { environment strings }
     envptr = ^envrec; { pointer to environment record }
     envrec = packed record

      name: charptr;    { name of string }
      data: charptr;    { data in string }
      next: envptr      { next entry in list }

     end;

     setchr = packed array [1..setsize] of byte;

var a, b, c:           integer;
    s, s2, s3, s4:     str;
    fp, lfp:           filptr;
    ad, ad2, ad3, ad4: address;
    cp:                charptr;
    i:                 integer;
    l:                 integer;
    bl:                boolean;
    st:                setchr;
    ch:                char;
    ep:                envptr;
    aci:               CInteger;

procedure getstr(sa: address; var s: str);

var a1:  integer;
    l,i: integer;

begin

    a1 := getadr(sa); { get base address of string }
    l := getadr(sa+adrsize); { get length of string }
    { trim off right padding }
    while (getchr(a1+l-1) = ' ') and(l > 1) do l := l-1;
    if getchr(a1+l-1) = ' ' then l := 0;
    { transfer string data }
    for i := 1 to l do s[i] := getchr(a1+i-1);
    s[l+1] := chr(0) { terminate }

end;

procedure putstr(var s: str; sl: integer; da: address);

var i: integer;

begin

   for i := 1 to sl do putchr(da+i-1, ' ');
   i := 1;
   while s[i] <> chr(0) do begin putchr(da, s[i]); da := da+1; i := i+1 end

end;

procedure prtcstr(var f: text; cp: charptr);

begin

   {$X+}
   while cp^ <> chr(0) do begin write(f, cp^); cp := succ(cp) end
   {$X-}

end;

procedure prtcstrfil(ad: address; cp: charptr);

var fn: fileno;

begin

   valfil(ad); fn := store[ad];
   if fn <= commandfn then case fn of

      outputfn,errorfn,listfn: begin prtcstr(output, cp); newline := false end;
      prrfn: prtcstr(prr, cp);
      prdfn,inputfn,
      commandfn: errore(WriteOnReadOnlyFile)

   end else begin

      if filstate[fn] <> fwrite then errore(FileModeIncorrect);
      prtcstr(filtable[fn], cp)

   end

end;

procedure putsetfil(var a: address; s: integer);

var i: integer;

begin

   putint(a, s);
   a := a+intsize;
   for i := 1 to setsize-intsize do begin putbyt(a, 0); a := a+1 end

end;

procedure getsetfil(var a: address; var s: integer);

var i: integer;

begin

   s := getint(a);
   a := a+intsize;

end;

procedure putsetchr(a: address; var st: setchr);

var i: integer;

begin

   for i := 1 to setsize do begin putbyt(a, st[i]); a := a+1 end

end;


function strlen(cp: charptr): integer;

var c: integer;

begin

   c := 0;
   {$X+}
   while cp^ <> chr(0) do begin c := c+1; cp := cp+1 end;
   {$X-}

   strlen := c

end;

procedure movstr2vm(cp: charptr; l: integer; ad: address);

var i: integer;

begin

   {$X+}
   for i := 1 to l do begin putchr(ad, cp^); ad := ad+1; cp := cp+1 end;
   {$X-}

end;

procedure movstrfvm(cp: charptr; l: integer; ad: address);

var i: integer;

begin

   {$X+}
   for i := 1 to l do begin cp^ := getchr(ad); ad := ad+1; cp := cp+1 end;
   {$X-}
   cp^ := chr(0) { terminate C string }

end;

procedure getenv(ad: address; var ep: envptr);

var ep2, lp: envptr;
    l: integer;

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
      ep2^.name := malloc(l+1); { allocate C space for that }
      movstrfvm(ep2^.name, l, ad2); { move string into place }
      { pick up data }
      ad2 := getadr(ad);
      ad := ad+adrsize;
      l := getint(ad2); { get the length }
      ad2 := ad2+intsize; { index base of string }
      ep2^.data := malloc(l+1); { allocate C space for that }
      movstrfvm(ep2^.data, l, ad2); { move string into place }
      lp := ep2; { set last entry }
      ad := getadr(ad) { link next }

   end

end;

begin

    case routine of

       1: begin { list() }

           getstr(params+adrsize, s); { load string }
           pa_list(@s, integer(@fp)); { get files list }
           ad3 := 0; { set no last entry }
           ad4 := 0; { set no top entry }
           while fp <> nil do begin

              { transfer each record to storage }
              { get space for file record }
              newspc(ptrsize+intsize+intsize+setsize+intsize+intsize+
                     intsize+intsize+setsize+setsize+setsize+ptrsize, ad);
              if ad4 = 0 then ad4 := ad; { set top entry }
              { link last to this if last exists }
              if ad3 <> 0 then putadr(ad3, ad);
              l := strlen(fp^.name); { get length of name }
              newspc(l+intsize, ad2); { get space for filename }
              putadr(ad, ad2); { place name pointer }
              ad := ad+ptrsize;
              putint(ad2, l); { put string size }
              ad2 := ad2+intsize;
              { move the name string into place }
              cp := fp^.name; { index string }
              movstr2vm(cp, l, ad2); { move into place }
              putint(ad, fp^.size);
              ad := ad+intsize;
              putint(ad, fp^.alloc);
              ad := ad+intsize;
              { sets need to be expanded }
              putsetfil(ad, fp^.attr);
              putint(ad, fp^.create);
              ad := ad+intsize;
              putint(ad, fp^.modify);
              ad := ad+intsize;
              putint(ad, fp^.access);
              ad := ad+intsize;
              putint(ad, fp^.backup);
              ad := ad+intsize;
              putsetfil(ad, fp^.user);
              putsetfil(ad, fp^.group);
              putsetfil(ad, fp^.other);
              ad3 := ad; { set last entry link }
              putadr(ad, nilval); { clear next (with nil value) }
              fp := fp^.next { link next entry }

           end;
           ad := getadr(params); { get address of list pointer }
           putadr(ad, ad4); { place to vm client }
           { return past parameters }
           params := params+ptrsize+ptrsize+intsize

           { need to free up original C side entries we copied }

       end;

       2:  begin { times() }

           pa_times(@s, getint(params+intsize*2), getint(params));
           putstr(s, getint(params+intsize*2), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;

       3:  begin { dates() }

           pa_dates(@s, getint(params+intsize*2), getint(params));
           putstr(s, getint(params+intsize*2), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;
       4:  begin { writetime() }

           pa_times(@s, strmax, getint(params));
           ad := getadr(params+intsize);
           prtcstrfil(ad, @s);
           params := params+intsize+ptrsize

       end;

       5:  begin { writedate() }

           pa_dates(@s, strmax, getint(params));
           ad := getadr(params+intsize);
           prtcstrfil(ad, @s);
           params := params+intsize+ptrsize

       end;

       6: begin { time() }

           a := pa_time;
           putint(params, a)

       end;

       7: begin { local() }

           a := getint(params);
           a := pa_local(a);
           putint(mp, a);
           params := params+intsize

       end;

       8: begin { clock() }

           a := pa_clock;
           putint(mp, a)

       end;

       9: begin { elapsed() }

           a := getint(params);
           a := pa_elapsed(a);
           putint(mp, a);
           params := params+intsize

       end;

       10: begin { validfile() }

           getstr(params, s);
           bl := pa_validfile(@s) <> 0;
           putint(mp, ord(bl));
           params := params+ptrsize+intsize

       end;

       11: begin { validpath() }

           getstr(params, s);
           bl := pa_validpath(@s) <> 0;
           putint(mp, ord(bl));
           params := params+ptrsize+intsize

       end;

       12: begin { wild() }

           getstr(params, s);
           bl := pa_wild(@s) <> 0;
           putint(mp, ord(bl));
           params := params+ptrsize+intsize

       end;

       13: begin { getenv() }

          getstr(params+ptrsize+intsize, s);
          pa_getenv(@s, @s2, getint(params+ptrsize));
          putstr(s2, getint(params+ptrsize), getadr(params));
          params := params+ptrsize+intsize+ptrsize+intsize

       end;

       14: begin { setenv() }

          getstr(params+ptrsize+intsize, s);
          getstr(params, s2);
          pa_setenv(@s, @s2);
          params := params+ptrsize+intsize

       end;

       15: begin { allenv() }

          pa_allenv(integer(@ep)); { get environment list }
          ad3 := 0; { set no last entry }
          ad4 := 0; { set no top entry }
          while ep <> nil do begin { translate list }

              { transfer each record to storage }
              { get space for environment record }
              newspc(ptrsize*3, ad);
              if ad4 = 0 then ad4 := ad; { set top entry }
              { link last to this if last exists }
              if ad3 <> 0 then putadr(ad3, ad);
              l := strlen(ep^.name); { get length of name }
              newspc(l+intsize, ad2); { get space for name }
              putadr(ad, ad2); { place name }
              ad := ad+ptrsize;
              putint(ad2, l); { put string size }
              ad2 := ad2+intsize;
              cp := ep^.name; { index string }
              movstr2vm(cp, l, ad2); { move the name string into place }
              l := strlen(ep^.data); { get length of data }
              newspc(l+intsize, ad2); { get space for name }
              putadr(ad, ad2); { place name }
              ad := ad+ptrsize;
              putint(ad2, l); { put string size }
              ad2 := ad2+intsize;
              cp := ep^.data; { index string }
              movstr2vm(cp, l, ad2); { move the name string into place }
              ad3 := ad; { set last entry link }
              putadr(ad, nilval); { clear next (with nil value) }
              ep := ep^.next { link next entry }

          end;
          ad := getadr(params); { get address of list pointer }
          putadr(ad, ad4); { place to vm client }

          { need to free up original C side entries we copied }

       end;

       16: begin { remenv() }

          getstr(params, s);
          pa_remenv(@s);
          params := params+ptrsize+intsize

       end;

       17: begin { exec() }

          getstr(params, s);
          pa_exec(@s);
          params := params+ptrsize+intsize

       end;

       18: begin { exece() }

          getstr(params+adrsize, s);
          getenv(getadr(params), ep);
          pa_exece(@s, integer(ep));
          params := params+ptrsize+ptrsize+intsize

       end;

       19: begin { execw() }

          getstr(params+adrsize, s);
          pa_execw(@s, aci);
          putint(getadr(params), aci);
          params := params+ptrsize+ptrsize+intsize

       end;

       20: begin { execew }

          getstr(params+adrsize*2, s);
          getenv(getadr(params+adrsize), ep);
          pa_execew(@s, integer(ep), aci);
          putint(getadr(params), aci);
          params := params+ptrsize+ptrsize+ptrsize+intsize

       end;

       21: begin { getcur() }

           pa_getcur(@s, getint(params+intsize+ptrsize));
           putstr(s, getint(params+intsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       22: begin { setcur() }

          getstr(params, s);
          pa_setcur(@s);
          params := params+ptrsize+intsize

       end;

       23: begin { brknam() }

          getstr(params+ptrsize*3+intsize*3, s);
          pa_brknam(@s, @s2, getint(params+ptrsize*3+intsize*2),
                    @s3, getint(params+ptrsize*2+intsize),
                    @s4, getint(params+ptrsize));
          putstr(s2, getint(params+ptrsize*3+intsize*2), getadr(params+ptrsize*2+intsize*2));
          putstr(s3, getint(params+ptrsize*2+intsize), getadr(params+ptrsize+intsize));
          putstr(s4, getint(params+ptrsize), getadr(params));
          params := params+(ptrsize+intsize)*4

       end;

       24: begin { maknam() }

          getstr(params+ptrsize*2+intsize*2, s2);
          getstr(params+ptrsize+intsize, s3);
          getstr(params, s4);
          pa_maknam(@s, getint(ptrsize*4+intsize*3), @s2, @s3, @s4);
          putstr(s, getint(params+ptrsize*4+intsize*3), getadr(params+ptrsize*3+intsize*3));
          params := params+(ptrsize+intsize)*4

       end;

       25: begin { fulnam() }

           getstr(params, s);
           pa_fulnam(@s, getint(params+ptrsize));
           putstr(s, getint(params+ptrsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       26: begin { getpgm() }

           pa_getpgm(@s, getint(params+ptrsize));
           putstr(s, getint(params+ptrsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       27: begin { getusr() }

           pa_getusr(@s, getint(params+ptrsize));
           putstr(s, getint(params+ptrsize), getadr(params));
           params := params+ptrsize+intsize

       end;

       28: begin { setatr() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_setatr(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       29: begin { resatr() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_resatr(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       30: begin { bakupd() }

          getstr(params, s);
          pa_bakupd(@s);
          params := params+ptrsize+intsize

       end;

       31: begin { setuper() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_setuper(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       32: begin { resuper() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_resuper(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       33: begin { setgper() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_setgper(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       34: begin { resgper() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_resgper(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       35: begin { setoper() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_setoper(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       36: begin { resoper() }

           getstr(params+setsize, s);
           getsetfil(params, a);
           pa_resoper(@s, a);
           params := params+setsize+ptrsize+intsize

       end;

       37: begin { makpth() }

          getstr(params, s);
          pa_makpth(@s);
          params := params+ptrsize+intsize

       end;

       38: begin { rempth() }

          getstr(params, s);
          pa_rempth(@s);
          params := params+ptrsize+intsize

       end;

       39: begin { filchr() }

           pa_filchr(integer(@st));
           putsetchr(getadr(params), st);
           params := params+setsize

       end;

       40: begin { optchr() }

           ch := pa_optchr();
           putchr(mp, ch);
           params := params+intsize

       end;

       41: begin { pthchr() }

           ch := pa_pthchr();
           putchr(mp, ch);
           params := params+intsize

       end;

       42: begin { latitude() }

           a := pa_latitude();
           putint(mp, a);
           params := params+intsize

       end;

       43: begin { longitude() }

           a := pa_longitude();
           putint(mp, a);
           params := params+intsize

       end;

       44: begin { altitude() }

           a := pa_altitude();
           putint(mp, a);
           params := params+intsize

       end;

       45: begin { country() }

           a := pa_country();
           putint(mp, a);
           params := params+intsize

       end;

       46: begin { countrys() }

           pa_countrys(@s, getint(params+intsize*2), getint(params));
           putstr(s, getint(params+intsize*2), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;

       47: begin { timezone() }

           a := pa_timezone();
           putint(mp, a);
           params := params+intsize

       end;

       48: begin { daysave() }

           a := pa_daysave();
           putint(mp, a);
           params := params+intsize

       end;

       49: begin { time24hour() }

           a := pa_time24hour();
           putint(mp, a);
           params := params+intsize

       end;

       50: begin { language() }

           a := pa_language();
           putint(mp, a);
           params := params+intsize

       end;

       51: begin { languages() }

           pa_languages(@s, getint(params+intsize*2), getint(params));
           putstr(s, getint(params+intsize*2), getadr(params+intsize));
           params := params+intsize+ptrsize+intsize

       end;

       52: begin { decimal() }

           ch := pa_decimal();
           putchr(mp, ch);
           params := params+intsize

       end;

       53: begin { numbersep() }

           ch := pa_numbersep();
           putchr(mp, ch);
           params := params+intsize

       end;

       54: begin { timeorder() }

           a := pa_timeorder();
           putint(mp, a);
           params := params+intsize

       end;

       55: begin { dateorder() }

           a := pa_dateorder();
           putint(mp, a);
           params := params+intsize

       end;

       56: begin { datesep() }

           ch := pa_datesep();
           putchr(mp, ch);
           params := params+intsize

       end;

       57: begin { timesep() }

           ch := pa_timesep();
           putchr(mp, ch);
           params := params+intsize

       end;

       58: begin { currchr() }

           ch := pa_currchr();
           putchr(mp, ch);
           params := params+intsize

       end;

       59: begin { seterr() }

           exitcode := getint(params)

       end;

    end

end;

{ Restore to ISO 7185 Pascal language }
{$classic-pascal-level-0}

{*******************************************************************************

Get number of routines

Retrieves the total number of routines available.

*******************************************************************************}

function NumExternal: integer;

begin

    NumExternal := 59

end;
