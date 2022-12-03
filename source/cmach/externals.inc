/*******************************************************************************

Petit-ami interface

Defines the calls that make up the Petit-ami support library for Pascaline.

Petit-ami is the collection of Pascaline support library modules written in C.

At this implementation (a test implementation), only services.c is implemented.

*******************************************************************************/

/********************************************************************************

External routine definitions

Each of these brings an externally defined routine into Pascal callable form.

*******************************************************************************/

#include "services.h"

/*******************************************************************************

Lookup symbol/module name

Given a module name and symbol name, returns a number used to execute the given
module call. If no routine is found, a zero is returned. If a routine is found,
this can be used as a key to execute the correct routine.

*******************************************************************************/

void LookupExternal(
    /* name of module */           symnam* module,
    /* symbol within module */     symnam* symbol,
    /* resulting routine number */ int* routine
)

{

   routine = 0; /* set no routine found */

   if (!strcmp(module, "services") {

       if (!strcmp(symbol, "list") routine = 1;
       else if (!strcmp(symbol, "times") routine = 2;
       else if (!strcmp(symbol, "dates") routine = 3;
       else if (!strcmp(symbol, "writetime") routine = 4;
       else if (!strcmp(symbol, "writedate") routine = 5;
       else if (!strcmp(symbol, "time") routine = 6;
       else if (!strcmp(symbol, "local") routine = 7;
       else if (!strcmp(symbol, "clock") routine = 8;
       else if (!strcmp(symbol, "elapsed") routine = 9;
       else if (!strcmp(symbol, "validfile") routine = 10;
       else if (!strcmp(symbol, "validpath") routine = 11;
       else if (!strcmp(symbol, "wild") routine = 12;
       else if (!strcmp(symbol, "getenv") routine = 13;
       else if (!strcmp(symbol, "setenv") routine = 14;
       else if (!strcmp(symbol, "allenv") routine = 15;
       else if (!strcmp(symbol, "remenv") routine = 16;
       else if (!strcmp(symbol, "exec") routine = 17;
       else if (!strcmp(symbol, "exece") routine = 18;
       else if (!strcmp(symbol, "execw") routine = 19;
       else if (!strcmp(symbol, "execew") routine = 20;
       else if (!strcmp(symbol, "getcur") routine = 21;
       else if (!strcmp(symbol, "setcur") routine = 22;
       else if (!strcmp(symbol, "brknam") routine = 23;
       else if (!strcmp(symbol, "maknam") routine = 24;
       else if (!strcmp(symbol, "fulnam") routine = 25;
       else if (!strcmp(symbol, "getpgm") routine = 26;
       else if (!strcmp(symbol, "getusr") routine = 27;
       else if (!strcmp(symbol, "setatr") routine = 28;
       else if (!strcmp(symbol, "resatr") routine = 29;
       else if (!strcmp(symbol, "bakupd") routine = 30;
       else if (!strcmp(symbol, "setuper") routine = 31;
       else if (!strcmp(symbol, "resuper") routine = 32;
       else if (!strcmp(symbol, "setgper") routine = 33;
       else if (!strcmp(symbol, "resgper") routine = 34;
       else if (!strcmp(symbol, "setoper") routine = 35;
       else if (!strcmp(symbol, "resoper") routine = 36;
       else if (!strcmp(symbol, "makpth") routine = 37;
       else if (!strcmp(symbol, "rempth") routine = 38;
       else if (!strcmp(symbol, "filchr") routine = 39;
       else if (!strcmp(symbol, "optchr") routine = 40;
       else if (!strcmp(symbol, "pthchr") routine = 41;
       else if (!strcmp(symbol, "latitude") routine = 42;
       else if (!strcmp(symbol, "longitude") routine = 43;
       else if (!strcmp(symbol, "altitude") routine = 44;
       else if (!strcmp(symbol, "country") routine = 45;
       else if (!strcmp(symbol, "countrys") routine = 46;
       else if (!strcmp(symbol, "timezone") routine = 47;
       else if (!strcmp(symbol, "daysave") routine = 48;
       else if (!strcmp(symbol, "time24hour") routine = 49;
       else if (!strcmp(symbol, "language") routine = 50;
       else if (!strcmp(symbol, "languages") routine = 51;
       else if (!strcmp(symbol, "decimal") routine = 52;
       else if (!strcmp(symbol, "numbersep") routine = 53;
       else if (!strcmp(symbol, "timeorder") routine = 54;
       else if (!strcmp(symbol, "dateorder") routine = 55;
       else if (!strcmp(symbol, "datesep") routine = 56;
       else if (!strcmp(symbol, "timesep") routine = 57;
       else if (!strcmp(symbol, "currchr") routine = 58;
       else if (!strcmp(symbol, "seterr") routine = 59;

   /* placeholders */

   /* terminal */
   } else if (!strcmp(module, "terminal") {

   /* graph */
   } else if (!strcmp(module, "graph") {

   /* terminal is complete */
   } else if (!strcmp(module, "sound") {

   /* network is fairly complete */
   } else if (!strcmp(module, "network") {

   }

}

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
    ep:                envptr;
    aci:               CInteger;

procedure getstr(sa: address; var s: str);

var a1:  integer;
    l,i: integer;

begin

    a1 = getadr(sa); { get base address of string }
    l = getadr(sa+adrsize); { get length of string }
    { trim off right padding }
    while (getchr(a1+l-1) = ' ') and(l > 1) do l = l-1;
    if getchr(a1+l-1) = ' ' then l = 0;
    { transfer string data }
    for i = 1 to l do s[i] = getchr(a1+i-1);
    s[l+1] = chr(0) { terminate }

end;

procedure putstr(var s: str; sl: integer; da: address);

var i: integer;

begin

   for i = 1 to sl do putchr(da+i-1, ' ');
   i = 1;
   while s[i] <> chr(0) do begin putchr(da, s[i]); da = da+1; i = i+1 end

end;

procedure prtcstr(var f: text; cp: charptr);

begin

   {$X+}
   while cp^ <> chr(0) do begin write(f, cp^); cp = succ(cp) end
   {$X-}

end;

procedure prtcstrfil(ad: address; cp: charptr);

var fn: fileno;

begin

   valfil(ad); fn = store[ad];
   if fn <= commandfn then case fn of

      outputfn,errorfn,listfn: begin prtcstr(output, cp); newline = false end;
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
   a = a+intsize;
   for i = 1 to setsize-intsize do begin putbyt(a, 0); a = a+1 end

end;

procedure getsetfil(var a: address; var s: integer);

var i: integer;

begin

   s = getint(a);
   a = a+intsize;

end;

procedure putsetchr(a: address; var st: setchr);

var i: integer;

begin

   for i = 1 to setsize do begin putbyt(a, st[i]); a = a+1 end

end;


function strlen(cp: charptr): integer;

var c: integer;

begin

   c = 0;
   {$X+}
   while cp^ <> chr(0) do begin c = c+1; cp = cp+1 end;
   {$X-}

   strlen = c

end;

procedure movstr2vm(cp: charptr; l: integer; ad: address);

var i: integer;

begin

   {$X+}
   for i = 1 to l do begin putchr(ad, cp^); ad = ad+1; cp = cp+1 end;
   {$X-}

end;

procedure movstrfvm(cp: charptr; l: integer; ad: address);

var i: integer;

begin

   {$X+}
   for i = 1 to l do begin cp^ = getchr(ad); ad = ad+1; cp = cp+1 end;
   {$X-}
   cp^ = chr(0) { terminate C string }

end;

procedure getenv(ad: address; var ep: envptr);

var ep2, lp: envptr;
    l: integer;

begin

   ep = nil; { set no top }
   lp = nil; { set no last }
   while ad <> nilval do begin

      new(ep2); { get new entry }
      ep2^.next = nil; { set no next }
      if ep = nil then ep = ep2; { set top entry if first }
      if lp <> nil then lp^.next = ep2; { link last to this if last exists }
      { pick up name }
      ad2 = getadr(ad); { index the string }
      ad = ad+adrsize;
      l = getint(ad2); { get the length }
      ad2 = ad2+intsize; { index base of string }
      ep2^.name = malloc(l+1); { allocate C space for that }
      movstrfvm(ep2^.name, l, ad2); { move string into place }
      { pick up data }
      ad2 = getadr(ad);
      ad = ad+adrsize;
      l = getint(ad2); { get the length }
      ad2 = ad2+intsize; { index base of string }
      ep2^.data = malloc(l+1); { allocate C space for that }
      movstrfvm(ep2^.data, l, ad2); { move string into place }
      lp = ep2; { set last entry }
      ad = getadr(ad) { link next }

   end

end;

begin

    case routine of

       1: begin { list() }

           getstr(sp+adrsize, s); { load string }
           pa_list(@s, integer(@fp)); { get files list }
           ad3 = 0; { set no last entry }
           ad4 = 0; { set no top entry }
           while fp <> nil do begin

              { transfer each record to storage }
              { get space for file record }
              newspc(ptrsize+intsize+intsize+setsize+intsize+intsize+
                     intsize+intsize+setsize+setsize+setsize+ptrsize, ad);
              if ad4 = 0 then ad4 = ad; { set top entry }
              { link last to this if last exists }
              if ad3 <> 0 then putadr(ad3, ad);
              l = strlen(fp^.name); { get length of name }
              newspc(l+intsize, ad2); { get space for filename }
              putadr(ad, ad2); { place name pointer }
              ad = ad+ptrsize;
              putint(ad2, l); { put string size }
              ad2 = ad2+intsize;
              { move the name string into place }
              cp = fp^.name; { index string }
              movstr2vm(cp, l, ad2); { move into place }
              putint(ad, fp^.size);
              ad = ad+intsize;
              putint(ad, fp^.alloc);
              ad = ad+intsize;
              { sets need to be expanded }
              putsetfil(ad, fp^.attr);
              putint(ad, fp^.create);
              ad = ad+intsize;
              putint(ad, fp^.modify);
              ad = ad+intsize;
              putint(ad, fp^.access);
              ad = ad+intsize;
              putint(ad, fp^.backup);
              ad = ad+intsize;
              putsetfil(ad, fp^.user);
              putsetfil(ad, fp^.group);
              putsetfil(ad, fp^.other);
              ad3 = ad; { set last entry link }
              putadr(ad, nilval); { clear next (with nil value) }
              fp = fp^.next { link next entry }

           end;
           ad = getadr(sp); { get address of list pointer }
           putadr(ad, ad4); { place to vm client }

           { need to free up original C side entries we copied }

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
           ad = getadr(sp+intsize);
           prtcstrfil(ad, @s)

       end;

       5:  begin { writedate() }

           pa_dates(@s, strmax, getint(sp));
           ad = getadr(sp+intsize);
           prtcstrfil(ad, @s)

       end;

       6: begin { time() }

           a = pa_time;
           putint(mp, a)

       end;

       7: begin { local() }

           a = getint(sp);
           a = pa_local(a);
           putint(mp, a)

       end;

       8: begin { clock() }

           a = pa_clock;
           putint(mp, a)

       end;

       9: begin { elapsed() }

           a = getint(sp);
           a = pa_elapsed(a);
           putint(mp, a)

       end;

       10: begin { validfile() }

           getstr(sp, s);
           bl = pa_validfile(@s) <> 0;
           putint(mp, ord(bl))

       end;

       11: begin { validpath() }

           getstr(sp, s);
           bl = pa_validpath(@s) <> 0;
           putint(mp, ord(bl))

       end;

       12: begin { wild() }

           getstr(sp, s);
           bl = pa_wild(@s) <> 0;
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

       15: begin { allenv() }

          pa_allenv(integer(@ep)); { get environment list }
          ad3 = 0; { set no last entry }
          ad4 = 0; { set no top entry }
          while ep <> nil do begin { translate list }

              { transfer each record to storage }
              { get space for environment record }
              newspc(ptrsize*3, ad);
              if ad4 = 0 then ad4 = ad; { set top entry }
              { link last to this if last exists }
              if ad3 <> 0 then putadr(ad3, ad);
              l = strlen(ep^.name); { get length of name }
              newspc(l+intsize, ad2); { get space for name }
              putadr(ad, ad2); { place name }
              ad = ad+ptrsize;
              putint(ad2, l); { put string size }
              ad2 = ad2+intsize;
              cp = ep^.name; { index string }
              movstr2vm(cp, l, ad2); { move the name string into place }
              l = strlen(ep^.data); { get length of data }
              newspc(l+intsize, ad2); { get space for name }
              putadr(ad, ad2); { place name }
              ad = ad+ptrsize;
              putint(ad2, l); { put string size }
              ad2 = ad2+intsize;
              cp = ep^.data; { index string }
              movstr2vm(cp, l, ad2); { move the name string into place }
              ad3 = ad; { set last entry link }
              putadr(ad, nilval); { clear next (with nil value) }
              ep = ep^.next { link next entry }

          end;
          ad = getadr(sp); { get address of list pointer }
          putadr(ad, ad4); { place to vm client }

          { need to free up original C side entries we copied }

       end;

       16: begin { remenv() }

          getstr(sp, s);
          pa_remenv(@s)

       end;

       17: begin { exec() }

          getstr(sp, s);
          pa_exec(@s)

       end;

       18: begin { exece() }

          getstr(sp+adrsize, s);
          getenv(getadr(sp), ep);
          pa_exece(@s, integer(ep))

       end;

       19: begin { execw() }

          getstr(sp+adrsize, s);
          pa_execw(@s, aci);
          putint(getadr(sp), aci)

       end;

       20: begin { execew }

          getstr(sp+adrsize*2, s);
          getenv(getadr(sp+adrsize), ep);
          pa_execew(@s, integer(ep), aci);
          putint(getadr(sp), aci)

       end;

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

           ch = pa_optchr();
           putchr(mp, ch);

       end;

       41: begin { pthchr() }

           ch = pa_pthchr();
           putchr(mp, ch);

       end;

       42: begin { latitude() }

           a = pa_latitude();
           putint(mp, a);

       end;

       43: begin { longitude() }

           a = pa_longitude();
           putint(mp, a);

       end;

       44: begin { altitude() }

           a = pa_altitude();
           putint(mp, a);

       end;

       45: begin { country() }

           a = pa_country();
           putint(mp, a);

       end;

       46: begin { countrys() }

           pa_countrys(@s, getint(sp+intsize*2), getint(sp));
           putstr(s, getint(sp+intsize*2), getadr(sp+intsize))

       end;

       47: begin { timezone() }

           a = pa_timezone();
           putint(mp, a);

       end;

       48: begin { daysave() }

           a = pa_daysave();
           putint(mp, a);

       end;

       49: begin { time24hour() }

           a = pa_time24hour();
           putint(mp, a);

       end;

       50: begin { language() }

           a = pa_language();
           putint(mp, a);

       end;

       51: begin { languages() }

           pa_languages(@s, getint(sp+intsize*2), getint(sp));
           putstr(s, getint(sp+intsize*2), getadr(sp+intsize))

       end;

       52: begin { decimal() }

           ch = pa_decimal();
           putchr(mp, ch);

       end;

       53: begin { numbersep() }

           ch = pa_numbersep();
           putchr(mp, ch);

       end;

       54: begin { timeorder() }

           a = pa_timeorder();
           putint(mp, a);

       end;

       55: begin { dateorder() }

           a = pa_dateorder();
           putint(mp, a);

       end;

       56: begin { datesep() }

           ch = pa_datesep();
           putchr(mp, ch);

       end;

       57: begin { timesep() }

           ch = pa_timesep();
           putchr(mp, ch);

       end;

       58: begin { currchr() }

           ch = pa_currchr();
           putchr(mp, ch);

       end;

       59: begin { seterr() }

           exitcode = getint(sp)

       end;

    end

end;

{*******************************************************************************

Get number of routines

Retrieves the total number of routines available.

*******************************************************************************}

function NumExternal: integer;

begin

    NumExternal = 59

end;
