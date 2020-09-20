{*******************************************************************************

Petit-ami interface

Defines the calls that make up the Petit-ami support library for Pascaline.

Petit-ami is the collection of Pascaline support library modules written in
C.

*******************************************************************************}

{*******************************************************************************

External routine definitions

Each of these brings an externally defined routine into Pascal callable form.

*******************************************************************************}

{ Allow GNU Pascal extensions }
{$gnu-pascal}

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

{ not implemented }
procedure pa_allenv(l: integer); external name 'pa_allenv';

procedure pa_remenv(sn: charptr); external name 'pa_remenv';
procedure pa_exec(cmd: charptr); external name 'pa_exec';

{ not implemented }
procedure pa_exece(cmd: charptr; el: integer); external name 'pa_exece';

{ not implemented }
procedure pa_execw(cmd: charptr; e: integer); external name 'pa_execw';

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

Given a routine number, executes that routine. All of the input parameters are
passed on the stack, and the result, if any, also returned on the stack.

All of the input parameters are removed from the stack, leaving just the result
(if any).

The load of parameters is fairly ad-hoc. Value parameters are simply fetched.
VAR and VIEW pameters have to be loaded into a buffer to transfer.

*******************************************************************************}

{ Allow GNU Pascal extensions }
{$gnu-pascal}

procedure ExecuteExternal(
    { number of routine to execute} routine: integer
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
    fn:                fileno;

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

procedure prtcstr(cp: charptr);

begin

   {$X+}
   while cp^ <> chr(0) do begin write(cp^); cp := succ(cp) end
   {$X-}

end;

procedure prtchrfil(fn: integer; c: char);

begin

   if fn <= commandfn then case fn of

      outputfn,errorfn,listfn: begin write(output, c); newline := false end;
      prrfn: write(prr, c);
      prdfn,inputfn,
      commandfn: errore(WriteOnReadOnlyFile)

   end else begin

      if filstate[fn] <> fwrite then errore(FileModeIncorrect);
      write(filtable[fn], c)

   end

end;

procedure prtcstrfil(fn: integer; cp: charptr);

begin

   {$X+}
   while cp^ <> chr(0) do begin prtchrfil(fn, cp^); cp := succ(cp) end
   {$X-}

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

begin

    case routine of

       1: begin { list() }

           getstr(sp+adrsize, s); { load string }
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
              {$X+}
              for i := 1 to l do
                begin putbyt(ad2, ord(cp^)); ad2 := ad2+1; cp := cp+1 end;
              {$X-}
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
              putadr(ad, 1); { clear next }
              fp := fp^.next { link next entry }

           end;
           ad := getadr(sp); { get address of list pointer }
           putadr(ad, ad4); { place to vm client }

       end;

       2:  begin { times() }

           pa_times(@s, getint(sp+intsize*2), getint(sp));
           putstr(s, getint(sp+intsize*2), getadr(sp+intsize))

       end;

       3:  begin { dates() }

           pa_dates(@s, getint(sp+intsize*2), getint(sp));
           putstr(s, getint(sp+intsize*2), getadr(sp+intsize))

       end;
       4:  begin { writetime() }

           pa_times(@s, strmax, getint(sp));
           ad := getadr(sp+intsize);
           valfil(ad); fn := store[ad];
           prtcstrfil(fn, @s)

       end;

       5:  begin { writedate() }

           pa_dates(@s, strmax, getint(sp));
           ad := getadr(sp+intsize);
           valfil(ad); fn := store[ad];
           prtcstrfil(fn, @s)

       end;

       6: begin { time() }

           a := pa_time;
           putint(mp, a)

       end;

       7: begin { local() }

           a := getint(sp);
           a := pa_local(a);
           putint(mp, a)

       end;

       8: begin { clock() }

           a := pa_clock;
           putint(mp, a)

       end;

       9: begin { elapsed() }

           a := getint(sp);
           a := pa_elapsed(a);
           putint(mp, a)

       end;

       10: begin { validfile() }

           getstr(sp, s);
           bl := pa_validfile(@s) <> 0;
           putint(mp, ord(bl))

       end;

       11: begin { validpath() }

           getstr(sp, s);
           bl := pa_validpath(@s) <> 0;
           putint(mp, ord(bl))

       end;

       12: begin { wild() }

           getstr(sp, s);
           bl := pa_wild(@s) <> 0;
           putint(mp, ord(bl))

       end;

       13: begin { getenv() }

          getstr(sp+ptrsize+intsize, s);
          pa_getenv(@s, @s2, getint(sp+ptrsize));
          putstr(s2, getint(sp+ptrsize), getadr(sp))

       end;

       14: begin { setenv() }

          getstr(sp+ptrsize+intsize, s);
          getstr(sp, s2);
          pa_setenv(@s, @s2)

       end;

       {
       15: else if symbol = 'allenv              ' then routine :=
       }

       16: begin { remenv() }

          getstr(sp, s);
          pa_remenv(@s)

       end;

       17: begin { exec() }

          getstr(sp, s);
          pa_exec(@s)

       end;

       {
       18: else if symbol = 'exece               ' then routine :=
       19: else if symbol = 'execw               ' then routine :=
       20: else if symbol = 'execew              ' then routine :=
       }

       21: begin { getcur() }

           pa_getcur(@s, getint(sp+intsize+ptrsize));
           putstr(s, getint(sp+intsize), getadr(sp))

       end;

       22: begin { setcur() }

          getstr(sp, s);
          pa_setcur(@s)

       end;

       23: begin { brknam() }

          getstr(sp+ptrsize*3+intsize*3, s);
          pa_brknam(@s, @s2, getint(sp+ptrsize*3+intsize*2),
                    @s3, getint(sp+ptrsize*2+intsize),
                    @s4, getint(sp+ptrsize));
          putstr(s2, getint(sp+ptrsize*3+intsize*2), getadr(sp+ptrsize*2+intsize*2));
          putstr(s3, getint(sp+ptrsize*2+intsize), getadr(sp+ptrsize+intsize));
          putstr(s4, getint(sp+ptrsize), getadr(sp));

       end;

       24: begin { maknam() }

          getstr(sp+ptrsize*2+intsize*2, s2);
          getstr(sp+ptrsize+intsize, s3);
          getstr(sp, s4);
          pa_maknam(@s, getint(ptrsize*4+intsize*3), @s2, @s3, @s4);
          putstr(s, getint(sp+ptrsize*4+intsize*3), getadr(sp+ptrsize*3+intsize*3))

       end;

       25: begin { fulnam() }

           getstr(sp, s);
           pa_fulnam(@s, getint(sp+ptrsize));
           putstr(s, getint(sp+ptrsize), getadr(sp))

       end;

       26: begin { getpgm() }

           pa_getpgm(@s, getint(sp+ptrsize));
           putstr(s, getint(sp+ptrsize), getadr(sp))

       end;

       27: begin { getusr() }

           pa_getusr(@s, getint(sp+ptrsize));
           putstr(s, getint(sp+ptrsize), getadr(sp))

       end;

       28: begin { setatr() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_setatr(@s, a)

       end;

       29: begin { resatr() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_resatr(@s, a)

       end;

       30: begin { bakupd() }

          getstr(sp, s);
          pa_bakupd(@s)

       end;

       31: begin { setuper() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_setuper(@s, a)

       end;

       32: begin { resuper() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_resuper(@s, a)

       end;

       33: begin { setgper() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_setgper(@s, a)

       end;

       34: begin { resgper() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_resgper(@s, a)

       end;

       35: begin { setoper() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_setoper(@s, a)

       end;

       36: begin { resoper() }

           getstr(sp+setsize, s);
           getsetfil(sp, a);
           pa_resoper(@s, a)

       end;

       37: begin { makpth() }

          getstr(sp, s);
          pa_makpth(@s)

       end;

       38: begin { rempth() }

          getstr(sp, s);
          pa_rempth(@s)

       end;

       39: begin { filchr() }

           pa_filchr(integer(@st));
           putsetchr(getadr(sp), st)

       end;

       40: begin { optchr() }

           ch := pa_optchr();
           putchr(mp, ch);

       end;

       41: begin { pthchr() }

           ch := pa_pthchr();
           putchr(mp, ch);

       end;

       42: begin { latitude() }

           a := pa_latitude();
           putint(mp, a);

       end;

       43: begin { longitude() }

           a := pa_longitude();
           putint(mp, a);

       end;

       44: begin { altitude() }

           a := pa_altitude();
           putint(mp, a);

       end;

       45: begin { country() }

           a := pa_country();
           putint(mp, a);

       end;

       46: begin { countrys() }

           pa_countrys(@s, getint(sp+intsize*2), getint(sp));
           putstr(s, getint(sp+intsize*2), getadr(sp+intsize))

       end;

       47: begin { timezone() }

           a := pa_timezone();
           putint(mp, a);

       end;

       48: begin { daysave() }

           a := pa_daysave();
           putint(mp, a);

       end;

       49: begin { time24hour() }

           a := pa_time24hour();
           putint(mp, a);

       end;

       50: begin { language() }

           a := pa_language();
           putint(mp, a);

       end;

       51: begin { languages() }

           pa_languages(@s, getint(sp+intsize*2), getint(sp));
           putstr(s, getint(sp+intsize*2), getadr(sp+intsize))

       end;

       52: begin { decimal() }

           ch := pa_decimal();
           putchr(mp, ch);

       end;

       53: begin { numbersep() }

           ch := pa_numbersep();
           putchr(mp, ch);

       end;

       54: begin { timeorder() }

           a := pa_timeorder();
           putint(mp, a);

       end;

       55: begin { dateorder() }

           a := pa_dateorder();
           putint(mp, a);

       end;

       56: begin { datesep() }

           ch := pa_datesep();
           putchr(mp, ch);

       end;

       57: begin { timesep() }

           ch := pa_timesep();
           putchr(mp, ch);

       end;

       58: begin { currchr() }

           ch := pa_currchr();
           putchr(mp, ch);

       end;

       59: begin { seterr() }

           exitcode := getint(sp)

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
