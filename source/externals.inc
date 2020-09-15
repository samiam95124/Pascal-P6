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
function pa_time: integer; external name 'pa_time';

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

var a, b, c:           integer;
    s:                 str;
    fp, lfp:           filptr;
    ad, ad2, ad3, ad4: address;
    cp:                charptr;
    i:                 integer;
    l:                 integer;

procedure getstr(sa: address; var s: str);

var a1:  integer;
    l,i: integer;

begin

    a1 := getadr(sa); { get base address of string }
    l := getadr(sa+adrsize); { get length of string }
    { transfer string data }
    for i := 1 to l do s[i] := chr(store[a1+i-1]);
    s[l+1] := chr(0) { terminate }

end;

procedure prtcstr(cp: charptr);

begin

   {$X+}
   while cp^ <> chr(0) do begin write(cp^); cp := succ(cp) end
   {$X-}

end;

procedure putsetfil(var a: address; s: integer);

var i: integer;

begin

   putint(a, s);
   ad := ad+intsize;
   for i := 1 to setsize-intsize do begin putbyt(ad, 0); ad := ad+1 end

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

       1: begin { list }

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

       {
       if symbol = 'list                ' then routine := 1
       2:  else if symbol = 'times               ' then routine :=
       3:  else if symbol = 'dates               ' then routine :=
       4:  else if symbol = 'writetime           ' then routine :=
       5:  else if symbol = 'writedate           ' then routine :=
       }

       6: begin

           a := pa_time;
           putint(mp, a)

       end;

       {
       6:  else if symbol = 'time                ' then routine :=
       7:  else if symbol = 'local               ' then routine :=
       8:  else if symbol = 'clock               ' then routine :=
       9:  else if symbol = 'elapsed             ' then routine :=
       10: else if symbol = 'validfile           ' then routine :=
       11: else if symbol = 'validpath           ' then routine :=
       12: else if symbol = 'wild                ' then routine :=
       13: else if symbol = 'getenv              ' then routine :=
       14: else if symbol = 'setenv              ' then routine :=
       15: else if symbol = 'allenv              ' then routine :=
       16: else if symbol = 'remenv              ' then routine :=
       17: else if symbol = 'exec                ' then routine :=
       18: else if symbol = 'exece               ' then routine :=
       19: else if symbol = 'execw               ' then routine :=
       20: else if symbol = 'execew              ' then routine :=
       21: else if symbol = 'getcur              ' then routine :=
       22 else if symbol = 'setcur              ' then routine :=
       23 else if symbol = 'brknam              ' then routine :=
       24 else if symbol = 'maknam              ' then routine :=
       25 else if symbol = 'fulnam              ' then routine :=
       26 else if symbol = 'getpgm              ' then routine :=
       27 else if symbol = 'getusr              ' then routine :=
       28 else if symbol = 'setatr              ' then routine :=
       29 else if symbol = 'resatr              ' then routine :=
       30 else if symbol = 'bakupd              ' then routine :=
       31 else if symbol = 'setuper             ' then routine :=
       32 else if symbol = 'resuper             ' then routine :=
       33 else if symbol = 'setgper             ' then routine :=
       34 else if symbol = 'resgper             ' then routine :=
       35 else if symbol = 'setoper             ' then routine :=
       36 else if symbol = 'resoper             ' then routine :=
       37 else if symbol = 'makpth              ' then routine :=
       38 else if symbol = 'rempth              ' then routine :=
       39 else if symbol = 'filchr              ' then routine :=
       40 else if symbol = 'optchr              ' then routine :=
       41 else if symbol = 'pthchr              ' then routine :=
       42 else if symbol = 'latitude            ' then routine :=
       43 else if symbol = 'longitude           ' then routine :=
       44 else if symbol = 'altitude            ' then routine :=
       45 else if symbol = 'country             ' then routine :=
       46 else if symbol = 'countrys            ' then routine :=
       47 else if symbol = 'timezone            ' then routine :=
       48 else if symbol = 'daysave             ' then routine :=
       49 else if symbol = 'time24hour          ' then routine :=
       50 else if symbol = 'language            ' then routine :=
       51 else if symbol = 'languages           ' then routine :=
       52 else if symbol = 'decimal             ' then routine :=
       53 else if symbol = 'numbersep           ' then routine :=
       54 else if symbol = 'timeorder           ' then routine :=
       55 else if symbol = 'dateorder           ' then routine :=
       56 else if symbol = 'datesep             ' then routine :=
       57 else if symbol = 'timesep             ' then routine :=
       58 else if symbol = 'currchr             ' then routine :=
       }

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

    NumExternal := 1

end;
