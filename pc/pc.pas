{******************************************************************************
*                                                                             *
*                        PC - PASCAL COMPILER SHELL                           *
*                                                                             *
*                       Copyright (C) 2001 S. A. Moore                        *
*                                                                             *
*                              Written 4/01                                   *
*                                                                             *
* PC is a Pascal compilation shell. It examines the file given it, and forms  *
* a dependency tree by looking at all the files that appear in "uses"         *
* statements. Then, the dates and times are checked against the object files, *
* and components rebuilt as required.                                         *
* The command line is:                                                        *
*                                                                             *
* pc program [#option]...                                                     *
*                                                                             *
* Options:                                                                    *
*                                                                             *
* #t or #tree - List the dependency tree.                                     *
*                                                                             *
* #a or #action - List the actions taken (commands performed)                 *
*                                                                             *
* #d or #dry - Do not actually perform anything. Used with #action to get a   *
* list of what is going to happen before actually running the operation.      *
*                                                                             *
* #r or #rebuild - Treat all files as needing to be rebuilt.                  *
*                                                                             *
* #el or #errorlimit=n - Passthrough option to limit errors.                  *
*                                                                             *
* #nrf or #noreference - Passthrough option to remove reference checking.     *
*                                                                             *
* #u or #uses=path - Direct specification of uses path.                       *
*                                                                             *
* Currently contains some Windows dependencies, which need to be removed:     *
*                                                                             *
* 1. Relys on directly outputting windows commands.                           *
*                                                                             *
* 2. Relys on windows mode paths.                                             *
*                                                                             *
* Remaining "to do" items:                                                    *
*                                                                             *
* 1. Implement packages.                                                      *
*                                                                             *
* 2. Have pc check what type the target file is, and if a module, perform     *
* a compile without a link.                                                   *
*                                                                             *
******************************************************************************}

program pc(output);

uses stddef,  { standard defines }
     strlib,  { string library }
     extlib,  { os extentions }
     parlib,  { parser library }
     scanner; { pascal scanner }

label 99; { abort program }

const serlib = 'serlib'; { name of serial libary }
      trmlib = 'trmlib'; { name of terminal library }
      gralib = 'gralib'; { name of graphical library }
      { libs that can be substituted for standard serial library }
      iolibs = 'serlib trmlib gralib tmmlib gmnlib';
      { libs that run in a separate window }
      gwlibs  = 'gralib gmnlib';
      cmdmax = 250;      { maximum length of command line }
      filmax = 1000;     { maximum length of filename }

type

   filinx = 1..filmax; { index for filename }
   filnam = packed array [filinx] of char; { filename }
   filept = ^filety; { pointer to file entry }
   fllptr = ^fillet; { pointer to file linkage entry }
   filety = record { file information entry }

      name:   filnam;  { filename }
      modify: integer; { time of last modification }
      rebld:  boolean; { requires rebuilding }
      excl:   boolean; { file is excluded or exists in an excluded directory }
      code:   boolean; { file contains code }
      stack:  filept;  { next stack entry }
      link:   fllptr;  { linkage list }
      list:   boolean; { placed in link list }
      asme:   filept;  { assembly file entry }
      obje:   filept;  { object file entry }
      syme:   filept;  { symbol file entry }
      pkg:    filept   { packaged within this module }

   end;
   fillet = record { file linkage entry }

      ref:  filept; { link to used entry }
      next: fllptr  { next entry link }

   end;
   lstptr = ^lstety; { pointer to file list entry }
   lstety = record { file list entry }

      name: pstring; { filename }
      next: lstptr  { next link }

   end;
   pkgptr = ^pkgety; { packaging entry }
   pkgety = record { library packing spec }

      name: pstring; { name of root package }
      lst:  lstptr; { list of contained packages }
      next: pkgptr { next entry in list }

   end;
   ext    = packed array [1..3] of char; { filename extention }

var

   cmdhan:  parhan;  { handle for command parsing }
   err:     boolean; { error holder }
   valfch:  chrset;  { valid file characters }
   filstk:  filept;  { file information stack }
   prgnam:  filnam;  { target program name }
   p, n, e: filnam;  { path components }
   usepth:  filnam;  { path of uses files }
   fverb:   boolean; { verbose flag (also gets passed through) }
   ftree:   boolean; { list dependency tree }
   fact:    boolean; { list actions }
   fdry:    boolean; { do not perform actions }
   frebld:  boolean; { rebuild all }
   fngwin:  boolean; { no graphical windows mode }
   fdeftrm: boolean; { default to terminal mode }
   fdefgra: boolean; { default to graphical mode }
   { these are "pass through" options, options meant for programs we execute }
   fovf:    boolean; { overflow checks in parser, used for unsigned numbers }
   fref:    boolean; { check for non-references in parser }
   fsymcof: boolean; { generate coff symbols }
   hp:      filept;  { head entry pointer }
   lnklst:  filnam;  { link order list }
   filcnt:  integer; { files to process count }
   filact:  fllptr;  { file actions list }
   actcnt:  integer; { actions count }
   excrbl:  boolean; { executive needs rebuilding }
   pgmpath: filnam;  { program path }
   usrpath: filnam;  { user path }
   curpath: filnam;  { current path }
   tmpnam:  filnam;  { temp filename holding }
   exclude: lstptr;  { exclude list }
   package: pkgptr;  { packaging list }
   grawin:  boolean; { a graphical window library exists }
   siolib:  boolean; { an alternate standard I/O library exists }

procedure logfil(view fn: string; var hp: filept); forward;

{******************************************************************************

Check options

Checks if a sequence of options is present in the input, and if so, parses and
processes them. An option is a '#' (or '/'), followed by the option identifier.
The identifier must be one of the valid options. Further processing may occur,
on input after the option, depending on the option specified (see the
handlers). Consult the operator's manual for full option details.

******************************************************************************}

procedure paropt;

var w:      filnam; { word holder }
    err:    boolean; { error flag holding }
    optfnd: boolean; { option found }

{ set true/false flag }

procedure setflg(view a, n: string; var f: boolean);

var ts: packed array [1..40] of char; { string holder }

begin

   if compp(w, n) or compp(w, a) then begin

      f := true; { perform true }
      optfnd := true { set option found }

   end else begin { try false cases }

      copy(ts, 'n'); { form negative }
      cat(ts, n);
      if compp(w, ts) then begin

         f := false; { perform false }
         optfnd := true { set option found }

      end else begin

         copy(ts, 'n'); { form negative }
         cat(ts, a);
         if compp(w, ts) then begin

            f := false; { perform false }
            optfnd := true { set option found }

         end

      end

   end

end;

begin

   skpspc(cmdhan); { skip spaces }
   while chkchr(cmdhan) = optchr do begin { parse option }

      optfnd := false; { set no option found }
      getchr(cmdhan); { skip option marker }
      parlab(cmdhan, w, err); { parse option label }
      if err then begin

         writeln('*** pc: Invalid option');
         goto 99

      end;
      setflg('v',  'verbose',  fverb); { verbose mode }
      setflg('t',  'tree',     ftree); { list dependency tree }
      setflg('a',  'action',   fact); { list actions }
      setflg('d',  'dry',      fdry); { don't perform actions }
      setflg('r',  'rebuild',  frebld); { rebuild everything }
      setflg('s',  'standard', fansi); { ansi standard mode }
      setflg('o',  'overflow', fovf); { check input numeric overflows }
      setflg('rf', 'refer',    fref); { check references }
      { keep terminal window for graphical window application }
      setflg('ktw', 'keepterminalwindow', fngwin);
      setflg('sc', 'symcoff', fsymcof); { generate coff symbols }
      setflg('dt', 'defaultterminal', fdeftrm); { default to terminal mode }
      setflg('dg', 'defaultgraphical', fdefgra); { default to graphical mode }
      if compp(w, 'usespath') or
         compp(w, 'up') then begin

         optfnd := true;
         skpspc(cmdhan); { skip spaces }
         if chkchr(cmdhan) <> '=' then begin { should have '=' }

            writeln('*** pc: Error: missing ''=''');
            goto 99

         end;
         getchr(cmdhan); { skip '=' }
         parwrd(cmdhan, usepth, err); { get path }
         if err then begin

            writeln('*** pc: Invalid uses path');
            goto 99

         end
         
      end;
      if not optfnd then begin { no option found }

         writeln('*** pc: Error: no option found');
         goto 99

      end;
      skpspc(cmdhan) { skip spaces }

   end

end;

{******************************************************************************

Search file

Searches for an existing file in the files stack. The extentions and path
are stripped for the comparision, which means that the first of duplicates is
used. This rule is based on the idea that like name files must be either
duplicates or errors, which with the requirement that file bodies match
their filenames is true.

Returns the head entry pointer that is found.

******************************************************************************}

function schfil(view fn: string) { filename }
                : filept;       { found status }

var fnd:     boolean; { return status holder }
    fp:      filept;  { stack pointer }
    p, n, e: filnam;  { filespec components }
    ns:      filnam;  { filename save }
    hp:      filept;  { head entry pointer }

begin

   fnd := false; { set not found }
   brknam(fn, p, n, e); { break out name only }
   copy(ns, n); { save }
   fp := filstk; { index top of stack }
   hp := nil; { set head not found }
   while fp <> nil do begin { search stack }

      brknam(fp^.name, p, n, e); { break name }
      if compp(ns, n) then hp := fp; { set filename found }
      fp := fp^.stack { next stack entry }

   end;

   schfil := hp { return result }

end;

{******************************************************************************

Check file excluded

Checks if the given file is a file in the exclude list, or is in an excluded
directory.

******************************************************************************}

function chkexcl(view fn: string): boolean;

var f: boolean; { match flag }
    tn: filnam; { filename holder }

function schexc(view fn: string): boolean;

var fp: lstptr; { pointer for exclude list }
    f:  boolean; { found flag }

begin

   f := false; { set not found }
   fp := exclude; { index top of exclude list }
   while fp <> nil do begin

      if compp(fp^.name^, fn) then begin { found name }

         f := true; { set found }
         fp := nil { indicate stop }

      end else fp := fp^.next { next in list }

   end;

   schexc := f { return result }
                                
end;

begin

   brknam(fn, p, n, e); { break down }
   maknam(tn, p, n, ''); { remove extention }
   f := schexc(tn); { search for the filename }
   if not f then f := schexc(p); { try search directory }

   chkexcl := f { return result }

end;

{******************************************************************************

Do list file

Gets a file entry corresponding to the filename. Gets a filptr entry for the
indicated file, then reformats it into our entry type. Does not handle
wildcards. If there is no such file, nil is returned.

******************************************************************************}

procedure dolist(view fn: string;  { filename to look up }
                 var  fp: filept); { entry to return }

var l:       filptr; { file entry pointer }

begin

   fp := nil; { set no file }
   list(fn, l); { get files list }
   if l <> nil then begin { there is a file }

      if l^.next <> nil then begin { should not be more than one entry }

         writeln('*** pc: Error: system fault: call S. A. Moore software');
         halt

      end;
      { translate entry }
      new(fp); { get a new entry }
      copy(fp^.name, fn); { place name }
      fp^.modify := l^.modify; { place date }
      fp^.rebld := false; { set no rebuild }
      fp^.excl := false; { clear exclude }
      fp^.code := false; { set no code }
      fp^.stack := nil; { clear stack link }
      fp^.link := nil; { clear link list }
      fp^.list := false; { set not in link list }
      fp^.asme := nil; { set no assembly file }
      fp^.obje := nil; { set no object file }
      fp^.syme := nil; { set no symbol file }
      fp^.pkg := nil { set no package file }

   end

end;

{******************************************************************************

Find file

Finds the given file by the uses path. If a uses path name is found, that name
is returned with full path. If it is not found, or there is no uses path,
the original name is returned.

******************************************************************************}

procedure fndfil(var fn: string);

var p, n, e: filnam; { path components }
    pt:      filnam; { uses path holder }
    w:       filnam; { single path holder }
    fns:     filnam; { trial filespec }
    m:       boolean; { match flag }
    i:       integer;

begin

   if usepth[1] <> ' ' then begin { uses path is not empty }

      brknam(fn, p, n, e); { break down filespec }
      copy(pt, usepth); { copy uses path }
      m := false; { set no match }
      repeat { try path components }

         { extract a single path from the uses path }
         i := indexp(pt, ','); { find location of ',' }
         if i = 0 then begin { only one path left, use the whole thing }

            copy(w, pt); { place }
            clears(pt) { clear out the rest }

         end else begin { extract single path }

            extract(w, pt, 1, i-1); { get the path }
            extract(pt, pt, i+1, len(pt)) { remove from uses path }

         end;
         maknam(fns, w, n, 'pas'); { construct a name }
         if exists(fns) then begin

            copy(fn, fns); { copy winning spec }
            m := true { set found }

         end

      until (pt[1] = ' ') or m { until path is empty or matched }

   end

end;

{******************************************************************************

Do uses file

Accepts the name of a Pascal file, complete with extention. searches for a
uses statement in the source, then processes a log operation on each file.
Since this is a recursive call, all of the uses files downwards from the file
will be registered. We perform a search for each file being already in the
tree in order to prevent loops.

The entries are chained to the current head entry.

Also now checks if the file contains code. Code is classified as any variables,
fixed, procedures or functions. If a file contains none of these, then it can
be assumed to be a define only file, and can be skipped from the link order.

******************************************************************************}

procedure douses(view fn: string;  { filename to process }
                      fp: filept); { head entry }

var f:  fcbptr; { file control block }
    t:  tolken; { next tolken save }
    w:  filnam; { path holder }
    hp: filept; { head entry pointer }
    p:  fllptr; { file linkage pointer }

{ skip forward to interesting tolken, while tracking in-code status }

procedure skpsrc(view s: tlkset);

var blkcnt: integer; { block nesting tracker }

begin

   blkcnt := 0; { set no block level }
   { skip }
   while not (f^.nxttlk in s) do begin

      { perform block nest/unnest }
      if f^.nxttlk = cbegin then blkcnt := blkcnt+1 { nest blocks }
      { 'begin' is unambiguous, but 'end' has multiple uses, it can also appear
        in a record. However, these are never declared inside a block. So we
        just ignore attempts to go negative on block nesting. }
      else if (f^.nxttlk = cend) and (blkcnt > 0) then 
         blkcnt := blkcnt-1 { denest }
      { now, ANYTHING within an active begin..end block can be considered code.
        We only have to do this because an empty block is required on a 
        module. }
      else if blkcnt > 0 then fp^.code := true;
      { Note that "var" can appear in two places, but they both imply code is
        generated. }
      if f^.nxttlk in [cvar, cfixed, cprocedure, cfunction] then 
         fp^.code := true; { coding structure found }
      gettlk(f) { get next tolken }

   end

end;

begin

   opnscn(f, fn); { open scan instance }
   skpsrc([cuses, ceof]); { skip until file end or "uses" }
   if f^.nxttlk = cuses then begin { we found it }

      gettlk(f); { skip 'uses' }
      repeat { process 'uses' files }

         if f^.nxttlk <> cidentifier then begin { bad syntax }

            write('Bad ''uses'' syntax in ');
            write(output, fn:0);
            writeln;
            goto 99

         end;
         copy(w, f^.nxtlab); { copy name }
         fndfil(w); { find it }
         logfil(w, hp); { log that }
         { chain it to caller head entry }
         new(p); { get a new link entry  }
         p^.ref := hp; { place link }
         p^.next := fp^.link; { insert to link list }
         fp^.link := p;
         gettlk(f); { get next tolken }
         t := f^.nxttlk; { save next tolken }
         if f^.nxttlk = ccma then gettlk(f) { skip ',' }

      until t <> ccma; { until no more }
      if f^.nxttlk <> cscn then begin { bad syntax }

         write('*** pc: Error: Bad ''uses'' syntax in ');
         write(output, fn:0);
         writeln;
         goto 99

      end
      { thats all we verify, the start and the end }
     
   end;
   skpsrc([ceof]); { skip until file end }
   clsscn(f) { close scan instance }

end;

{******************************************************************************

Log source file

Creates a file entry for the given file, and stacks that. The following
components are found and logged for the given filename:

file.pas - The Pascal source.
file.asm - The assembly language source.
file.obj - The object.
file.sym - The symbols.

The Pascal source must be found, or an error results. This is because even an
assembly file requires a "front end" description file.
This becomes the "head" file. Then, the other components are serached for,
and chained to the head entry as found.
Finally, the Pascal source file is examined for any "uses" files (using our
portable scanner), and each of the uses files are logged as well.

******************************************************************************}

procedure logfil(view fn: string;  { filename to process }
                 var  hp: filept); { head entry found }

var p, n, e: filnam; { path components }
    fp:      filept; { file entry pointer }
    fns:     filnam; { holder for filename }

begin

   copy(fns, fn); { create modifable copy of name }
   { check we have already logged this file }
   hp := schfil(fn);
   if hp = nil then begin { nope, process }

      { do the four component files }
      brknam(fns, p, n, e); { add Pascal extention }
      maknam(fns, p, n, 'pas');
      dolist(fns, hp);
      if hp = nil then begin { missing source }
      
         write('*** pc: Error: missing source file ''');
         write(output, fns:0);
         writeln('''');
         goto 99

      end;
      hp^.excl := chkexcl(fn); { check exists in exclude }
      brknam(fns, p, n, e); { add symbols extention }
      maknam(fns, p, n, 'sym');
      dolist(fns, fp);
      hp^.syme := fp; { place link }
      brknam(fns, p, n, e); { add object extention }
      maknam(fns, p, n, 'obj');
      dolist(fns, fp);
      hp^.obje := fp; { place link }
      brknam(fns, p, n, e); { add assembly extention }
      maknam(fns, p, n, 'asm');
      dolist(fns, fp);
      hp^.asme := fp; { place link }
      { place head on stack }
      hp^.stack := filstk;
      filstk := hp;
      brknam(fns, p, n, e); { add Pascal extention }
      maknam(fns, p, n, 'pas');
      filcnt := filcnt+1; { count head files }
      douses(fns, hp) { process any uses files under }

   end

end;

{******************************************************************************

Write file entry

Writes out the file name entry, followed by the time of modification and 
rebuild status. 

******************************************************************************}

procedure wrtfil(fp: filept); { entry to write }

begin

   { should find max length of filenames }
   write(fp^.name:0); { output filename }
   write(' '); { separate }
   writedate(output, local(fp^.modify)); { output date/time of modification }
   write('  '); { separate }
   writetime(output, local(fp^.modify));
   write('  '); { separate }
   if fp^.rebld then write('R');
   if fp^.excl then write('E');
   if fp^.code then write('C');
   writeln

end;

{******************************************************************************

Print discovered file tree

Prints the complete and formatted contents of the dependency tree.

******************************************************************************}

procedure prtree;

var fp:      filept; { pointers for file elements }
    rp:      fllptr; { reference pointer }
    p, n, e: filnam; { path components }
    fn:      filnam; { filename holder }

begin

   writeln;
   write('Dependency tree for ');
   write(output, prgnam:0);
   writeln;
   fp := filstk; { index top of stack }
   while fp <> nil do begin { print entries }

      writeln;
      writeln('Pascal module:');
      writeln;
      wrtfil(fp); { output head entry }
      if (fp^.asme <> nil) or (fp^.obje <> nil) or (fp^.syme <> nil) then begin

         writeln;
         writeln('   Components:');
         writeln;
         if fp^.asme <> nil then begin

            write('   '); { tab out }
            wrtfil(fp^.asme) { print entry }

         end;
         if fp^.obje <> nil then begin

            write('   '); { tab out }
            wrtfil(fp^.obje) { print entry }

         end;
         if fp^.syme <> nil then begin

            write('   '); { tab out }
            wrtfil(fp^.syme) { print entry }

         end

      end;
      rp := fp^.link; { index top linkage }
      if rp <> nil then begin { announce }

         writeln;
         writeln('   References:');
         writeln;
         write('   ');
         while rp <> nil do begin { traverse references }

            copy(fn, rp^.ref^.name); { copy name }
            brknam(fn, p, n, e); { remake without path or extention }
            maknam(fn, '', n, '');
            write(output, fn:0);
            if rp^.next <> nil then write(',');
            rp := rp^.next { next entry }
         
         end;
         writeln

      end;
      if fp^.pkg <> nil then begin { its a packed file }

         writeln;
         writeln('   Is a component of package: ');
         writeln;
         write('   ');
         wrtfil(fp^.pkg)

      end;
      fp := fp^.stack { next in stack }

   end;
   writeln

end;

{******************************************************************************

Dump package list

Dumps the contents of the package list. A diagnostic.

******************************************************************************}

procedure dmppkg;

var pp: pkgptr; { pointer for packages }
    lp: lstptr; { pointer for file entries }

begin

   writeln('Packages:');
   writeln;
   pp := package; { index top of package list }
   while pp <> nil do begin { traverse package list }

      writeln('Package: ', pp^.name^, ' Contents: ');
      lp := pp^.lst; { index top of file list }
      while lp <> nil do begin { traverse files list }

         writeln('   ', lp^.name^);
         lp := lp^.next { link next item in list }

      end;
      pp := pp^.next { next item in package list }

   end

end;

{******************************************************************************

Find or insert standard library

Because every completed binary must have a standard library at the front,
we check if one exists. If not, then we choose a default, as determined by the
options set.

******************************************************************************}

procedure stdlib;

var defnam: filnam; { name for default library }
    fp:     filept; { file pointer }

{ find if files list contains any i/o standard lib }

procedure schsio;

var w:  integer; { total number of libraries }
    l:  filnam;  { library name }
    i:  integer; { index }

begin

   siolib := false; { set no standard I/O libraries }
   w := words(iolibs); { find the number of standard I/O libraries to find }
   for i := 1 to w do begin { search libraries }

      extwords(l, iolibs, i, i); { get library name }
      { search for that and set true if found }
      if schfil(l) <> nil then siolib := true

   end

end;

{ find if files list contains any graphics windowing lib }

procedure schgwn;

var w:  integer; { total number of libraries }
    l:  filnam;  { library name }
    i:  integer; { index }

begin

   grawin := false; { set no graphical windowed I/O libraries }
   { find the number of graphical windowed I/O libraries to find }
   w := words(gwlibs);
   for i := 1 to w do begin { search libraries }

      extwords(l, gwlibs, i, i); { get library name }
      { search for that and set true if found }
      if schfil(l) <> nil then grawin := true

   end

end;

begin

   schsio; { find if target already specifies a standard library }
   schgwn; { find if target specifies a graphical windowed library }
   if not siolib then begin { no standard library specified }

      { find default library }
      if fdefgra then copy(defnam, gralib)
      else if fdeftrm then copy(defnam, trmlib)
      else copy(defnam, serlib);
      fndfil(defnam);
      if not exists(defnam) then begin { not found }

         write('*** pc: Error: support module ''');
         write(output, defnam:0);
         writeln(''' not found');
         goto 99

      end;
      logfil(defnam, fp) { log the library }

   end

end;

{******************************************************************************

Find package lists

Finds the package links in the file list. Each file in the files stack is
checked against the package list. If a referenced file is found inside a
package, then then we check if the package file exists in the files list.
If not, we then copy the package file from the package list to the files
list. Then, we place a package link from the referencing file to the package
file in the list.

******************************************************************************}

procedure fndpkg;

{ check if name is contained in a package in the files list }

function inpkg(view s: string): boolean;

var m: boolean; { found flag }
    pp: pkgptr; { pointer for packages }

function schpkg(lp: lstptr; view s: string): boolean;

var m:  boolean; { match flag }

begin

   m := false; { set no match }
   while lp <> nil do begin

      if compp(lp^.name^, s) then m := true; { found }
      lp := lp^.next { next entry }

   end;

   schpkg := m { return result }

end;

begin

   m := false; { set no match }
   pp := package; { index top of package list }
   while pp <> nil do begin { traverse package list }

      if schpkg(pp^.lst, s) then if schfil(pp^.name^) <> nil then m := true;
      pp := pp^.next { next package }

   end;

   inpkg := m { return match }

end;
   
{ add any required missing packages by reference }

procedure addpkg;

var pp:      pkgptr;  { pointer for packages }
    lp:      lstptr;  { pointer for file entries }
    fp:      filept;  { pointer to found entry }
    pf:      filept;  { package file found }
    p, n, e: filnam;  { path components }
    fn:      filnam;  { extended name }
    logged:  boolean; { a new file was logged in this relaxation pass }

begin

   repeat { relaxation }

      logged := false; { set no log occurred }
      pp := package; { index top of package list }
      while pp <> nil do begin { traverse package list }

         lp := pp^.lst; { index top of file list }
         while lp <> nil do begin { traverse files list }

            fp := schfil(lp^.name^); { find matching entry in files list }
            if fp <> nil then begin { found a match }

               if not inpkg(lp^.name^) then 
                  begin { File does not already exist in another, or the same
                          package. }

                  { We have extend this with .pas. I don't think that any other
                    extention would work downstream in any case. }
                  brknam(pp^.name^, p, n, e); { break down filespec }
                  maknam(fn, p, n, 'pas');
                  logfil(fn, pf); { log that }
                  logged := true { set new log }

               end

            end;
            lp := lp^.next { link next item in list }

         end;
         pp := pp^.next { next item in package list }

      end

   until not logged { until no new entries }

end;

{ link package references to their packages }

procedure lnkpkg;

var pp:            pkgptr; { pointer for packages }
    lp:            lstptr; { pointer for file entries }
    fp:            filept; { pointer to found entry }
    pnp, pnn, pne: filnam; { path components for package element }
    fnp, fnn, fne: filnam; { path components for package element }
    cp:            filept; { containing package in files list }

begin

   fp := filstk; { index top of files list }
   while fp <> nil do begin { traverse }

      brknam(fp^.name, fnp, fnn, fne); { break filename to components }
      pp := package; { index top of package list }
      while pp <> nil do begin { traverse package list }

         cp := schfil(pp^.name^); { find this package name in files list }
         if cp <> nil then begin { package is in files list }

            lp := pp^.lst; { index top of file list }
            while lp <> nil do begin { traverse files list }

               { break filename to components }
               brknam(lp^.name^, pnp, pnn, pne);
               if compp(pnn, fnn) then { file contained in package }
                  fp^.pkg := cp; { link file to containing package file }
               lp := lp^.next { link next item in list }

            end

         end;
         pp := pp^.next { next item in package list }

      end;
      fp := fp^.stack { next in stack }

   end

end;

begin { fndpkg }

   addpkg; { add required packages }
   lnkpkg { link to used packages }

end;

{******************************************************************************

Find link order

Discovers the link order. First, we search for all entries that don't reference
other modules (the leaf modules). These are placed in the link list, in stack
order, and marked as in the list. The stack is upside down from the root
program, so this tends to, but does not allways, result in the bottom most
references appearing first.

Second, we successively sweep for entries whose referenced modules are
themselves listed. These are also marked listed, so N passes can output the
entire tree.

When a pass is performed that does not result in any new files becoming listed,
then either the stack is empty, or the entries remaining form a cycle, or 
loop of mutual references. Since this is not at the present an error, we output
an information message, then we output the top of the list and continue the
entire algorithim until the stack is empty.

******************************************************************************}

procedure fndlnk;

var li:      integer; { output index }
    rescnt:  integer; { resolved file count }
    ressav:  integer; { resolved count save }
    cycwrn:  boolean; { cyclic warning output }
    filacts: fllptr; { file action list save }
    p:       fllptr; { file action pointer }

{ place output character }

procedure putchr(c: char);

begin

   if li > filmax then begin { overflow }

      writeln('*** pc: Error: link list too long');
      goto 99

   end;
   lnklst[li] := c; { place character }
   li := li+1

end;

{ place filename in list }

procedure plcfil(view fn: string);

var p, n, e: filnam;  { path components }
    fns:     filnam;  { save for name }
    i:       integer; { index for name }

begin

   { get rid of extention }
   brknam(fn, p, n, e); { break down name }
   maknam(fns, p, n, ''); { remake }
   putchr('"');
   { place name in link list }
   for i := 1 to len(fns) do putchr(fns[i]);
   putchr('"');
   putchr(' ') { place separator }

end;

{ place file entry to list }

procedure plcety(fp: filept); { file entry to place }

var p: fllptr; 

begin

   if fp^.code and (fp^.pkg = nil) then { contains code, not in a package }
      plcfil(fp^.name); { place name in list }
   fp^.list := true; { set listed }
   new(p); { get list entry }
   p^.ref := fp; { index head file }
   p^.next := filact; { push to list }
   filact := p;
   rescnt := rescnt+1 { count listed entry }

end;

{ search for standard I/O modules }

procedure schstdio;

var fp: filept; { pointer for files stack }

{ find name is in list }

function inlist(view m, l: string): boolean;

var i:       integer; { list index }
    f:       boolean; { match flag }
    w:       filnam;  { word from list }
    p, n, e: filnam; { filename components }

begin

   brknam(m, p, n, e); { strip name off }
   f := false; { set no match }
   for i := 1 to words(l) do begin { for each list word }

      extwords(w, l, i, i); { get the current word }
      if compp(n, w) then f := true { set found }

   end;

   inlist := f { return result }

end;

begin

   fp := filstk; { index the top of stack }
   while fp <> nil do begin { traverse }

      { look for entries with no references and not listed }
      if inlist(fp^.name, iolibs) and not fp^.list then plcety(fp); { found }
      fp := fp^.stack { next on stack }

   end

end;

{ search for leaves }

procedure schleaf;

var fp: filept; { pointer for files stack }

begin

   fp := filstk; { index the top of stack }
   while fp <> nil do begin { traverse }

      { look for entries with no references and not listed }
      if (fp^.link = nil) and not fp^.list then plcety(fp); { found }
      fp := fp^.stack { next on stack }

   end

end;

{ check entry is resolved }

function resvd(p: fllptr) { entry to check resolved }
               : boolean; { resolved status }

var r: boolean; { resolved flag }

begin

   r := true; { set resolved }
   while p <> nil do begin { traverse }

      if not p^.ref^.list then r := false; { not resolved }
      p := p^.next { link next }

   end;
   resvd := r { return result }

end;

{ search for resolved entries }

procedure schres; { entry was listed }

var fp: filept; { pointer for files stack }

begin

   fp := filstk; { index the top of stack }
   while fp <> nil do begin { traverse }

      { look for entries that are resolved and not listed }
      if resvd(fp^.link) and not fp^.list then plcety(fp); { found }
      fp := fp^.stack { next on stack }

   end

end;

{ search for any unlisted entry }

procedure schnls; { entry was listed }

var fp: filept; { pointer for files stack }

begin

   fp := filstk; { index the top of stack }
   while fp <> nil do begin { traverse }

      { look for entries that are resolved and not listed }
      if not fp^.list then plcety(fp); { found }
      fp := fp^.stack { next on stack }

   end

end;

begin

   clears(lnklst); { clear the output list }
   li := 1; { index 1st character }
   rescnt := 0; { set no files resolved }
   cycwrn := false; { set no cyclic warning output }
   schstdio; { put standard I/O at the head of the list }
   schleaf; { toss in the leaves }
   while rescnt < filcnt do begin { while files remain unresolved }

      ressav := rescnt; { save counter for change check }
      schres; { search for resolved entries }
      if ressav = rescnt then begin { stuck on cyclic entries }

         if fverb and not cycwrn then begin { announce }

            writeln;
            writeln('Cycle was found in ''uses'' declarations');
            writeln;
            cycwrn := true { set we warned of cyclic }

         end;
         schnls { throw out the first unlisted entry }

      end

   end;
   { now we need to reverse the order of the action list }
   filacts := filact; { get the list }
   filact := nil; { clear }
   while filacts <> nil do begin { entire list }

      p := filacts; { save top }
      filacts := filacts^.next; { gap }
      p^.next := filact; { push to destination }
      filact := p

   end

end;

{******************************************************************************

Execute build action

Executes a command line for building. If the list actions flag is true, the
action will be printed. If the dry flag is true, then no execution is done.

******************************************************************************}

procedure excact(view cs: string); { command string }

var r: integer; { command result code }

begin

   { print command if requested }
   if fact then begin

      write(output, cs:0);
      writeln

   end;
   if not fdry then begin { execute command }

      execw(cs, r); { execute }
      if r <> 0 then begin { error }

         writeln('Build has errors, terminating');
         goto 99

      end
      
   end

end;

{******************************************************************************

Perform file action

Performs the required action on each file entry. We check if the rebuild flag
is on, which indicates the file is to be rebuilt. If that flag is not on, we
do nothing.
Then, we recompile the Pascal source. If there is an assembly file, this is
simply done for error checking purposes.
Then, if there is an assembly source, this is compiled, replacing the results
of the Pascal compile.

******************************************************************************}

procedure doact(fp: filept);

var p, n, e: filnam; { path components }
    fns:     filnam; { save for name }
    cmdbuf:  linbuf; { command buffer }
    i:       lininx; { index for that }

{ place output character }

procedure putchr(c: char);

begin

   if i > maxlin then begin { overflow }

      writeln('*** Error: action command too long');
      goto 99

   end;
   cmdbuf[i] := c; { place character }
   i := i+1

end;

{ place string in output }

procedure putstr(view s: string);

var i: integer; { index for name }

begin

   { place name in link list }
   for i := 1 to len(s) do putchr(s[i])

end;

begin

   if fp^.rebld then begin { this section is to be rebuilt }

      i := 1; { set 1st command filename }
      clears(cmdbuf); { clear command buffer }
      copy(fns, fp^.name); { make a copy of the name }
      brknam(fns, p, n, e); { remove the extention }
      maknam(fns, p, n, '');
      fulnam(fns); { normalize it }
      { do information }
      if fverb then begin

         write('Building ');
         write(output, fns:0);
         writeln;
         if not fact then writeln

      end;
      { build parse x=x command }
      putstr('parse "');
      putstr(fns);
      putstr('"="');
      putstr(fns);
      putstr('"');
      { place uses path, if defined here }
      if len(usepth) > 0 then begin

         putstr('/uses="');
         putstr(usepth);
         putchr('"')
       
      end;
      { place pass through options }
      if fverb then putstr('/v'); { verbose }
      if fansi then putstr('/s') { standard }
               else putstr('/ns'); { not standard }
      if not fovf then putstr('/nooverflow'); { no overflow checks }
      if fref then putstr('/rf') { reference checks }
              else putstr('/nrf'); { no references checks }
      excact(cmdbuf); { execute command buffer action }
      { build ce x=x command }
      i := 1; { set 1st command filename }
      clears(cmdbuf); { clear command buffer }
      putstr('ec "');
      putstr(fns);
      putstr('"="');
      putstr(fns);
      putchr('"');
      excact(cmdbuf); { execute command buffer action }
      if not fact then writeln;
      actcnt := actcnt+1 { count actions }

   end

end;

{******************************************************************************

Perform file actions on list

Performs actions on each of the files in the action list.

******************************************************************************}

procedure doacts;

begin

   while filact <> nil do begin { until list is empty }

      doact(filact^.ref); { perform file action }
      filact := filact^.next { gap list top }

   end

end;

{******************************************************************************

Perform linkage and generate pass

Links and generates the final executable.

******************************************************************************}

procedure dolink;

var p, n, e: filnam;  { path components }
    fns:     filnam;  { save for name }
    cmdbuf:  linbuf;  { command buffer }
    i:       lininx;  { index for that }
    cap:     filnam;  { name for cap module }

{ place output character }

procedure putchr(c: char);

begin

   if i > maxlin then begin { overflow }

      writeln('*** pc: Error: action command too long');
      goto 99

   end;
   cmdbuf[i] := c; { place character }
   i := i+1

end;

{ place string in output }

procedure putstr(view s: string);

var i: integer; { index for name }

begin

   { place name in link list }
   for i := 1 to len(s) do putchr(s[i])

end;

begin

   { remove extention from target }
   brknam(prgnam, p, n, e);
   maknam(fns, p, n, '');
   i := 1; { set 1st command filename }
   clears(cmdbuf); { clear command buffer }
   { find cap }
   copy(cap, 'cap');
   fndfil(cap);
   if not exists(cap) then begin { not found }

      write('*** pc: Error: support module ''');
      write(output, cap:0);
      writeln(''' not found');
      goto 99

   end;
   { remove extention }
   brknam(cap, p, n, e);
   maknam(cap, p, n, '');
   if fverb then begin

      write('Building executable');
      writeln

   end;
   { build ln command }
   putstr('ln temp=');
   putstr(lnklst);
   putstr(' "');
   putstr(cap);
   putstr('"');
   if fverb then putstr('/v'); { verbose }
   putstr('/nu');
   excact(cmdbuf); { execute command buffer action }
   i := 1; { set 1st command filename }
   clears(cmdbuf); { clear command buffer }
   putstr('genpe');
   putchr(' ');
   putstr(fns);
   putstr('=temp');
   if fverb then putstr('/v'); { add verbose if set }
   { check graphical window specificially selected, or default graphical
     selected and not another type of standard I/O library, and not the
     "no graphical" (autodetach) flag }
   if (grawin or (fdefgra and not siolib)) and not fngwin then 
      putstr('/wg'); { add windows graphical }
   if fsymcof then putstr('/sc'); { add symbols generation }
   excact(cmdbuf);
   if fact then writeln('del temp.obj');
   delete('temp.obj');
   if fact then writeln('del temp.sym');
   delete('temp.sym')

end;

{******************************************************************************

Register file

Performs various registration tasks on the file stack. We check if both .obj 
and .sym files are both present, and flag rebuild if not. Then, we check
the time/date of both .obj and .sym files against the .pas and .asm file
(if it exists), and see if the .pas or .asm file is newer than the output
files. If so, it is marked rebuild.
We don't compare the .obj and .sym files against each other. Although they
should be nearly identical, I don't want to get into the situations (like
pausing the linker) that could cause them to be radically different.

******************************************************************************}

procedure regfil(fp: filept); { file head }

begin

   { check both .obj and .sym files exist }
   if (fp^.obje = nil) or (fp^.syme = nil) then fp^.rebld := true { set rebuild }
   else begin { both outputs exist }
  
      { check times on source }
      if (fp^.modify > fp^.obje^.modify) or
         (fp^.modify > fp^.syme^.modify) then
            fp^.rebld := true; { old, set rebuild }
      if fp^.asme <> nil then { assembly exists }
         if (fp^.asme^.modify > fp^.obje^.modify) or
            (fp^.asme^.modify > fp^.syme^.modify) then
               fp^.rebld := true { old, set rebuild }

   end;
   { if global rebuild is set, set this rebuild flag }
   if frebld then fp^.rebld := true;
   { If the exclusion flag is on, then no rebuild is ever done. This can go
     ahead and cause an error if an essential file is missing. }
   if fp^.excl then fp^.rebld := false;
   { if the file contains no code, then also exclude from a rebuild }
   if not fp^.code and not compp(fp^.name, prgnam) then fp^.rebld := false
   
end;

{******************************************************************************

Check executive rebuild

Checks if the executive needs to be rebuilt. This would occur if the .obj or
.sym files for the target are newer than the .exe file, or the .exe file does
not exist. Both of these files must have been rebuilt by this point in the
code.
Its possible that the .obj or .sym files could get deleted by another task, so
we output a special error message for that.

******************************************************************************}

procedure chkexc;

var op, sp, ep: filept; { file pointers }
    p, n, e:    filnam; { path components }
    fn:         filnam; { file name save }

begin

   brknam(prgnam, p, n, e); { break program name to components }
   { find each of .obj, .sym and .exe files }
   maknam(fn, p, n, 'obj');
   dolist(fn, op);
   maknam(fn, p, n, 'sym');
   dolist(fn, sp);
   maknam(fn, p, n, 'exe');
   dolist(fn, ep);
   if (op = nil) or (sp = nil) then begin { should not be missing }

      writeln('*** pc: Error: Sequence error, missing file: check other tasks');
      halt

   end;
   if ep = nil then excrbl := true { does not exist }
   else if (ep^.modify < op^.modify) or 
           (ep^.modify < sp^.modify) then
      { .exe date/time is older than .obj or .sym, execute rebuild }
      excrbl := true;
   dispose(ep); { release objects }
   dispose(op);
   dispose(sp)

end;

{******************************************************************************

Register files

Registers all of the files in the stack.

******************************************************************************}

procedure regfils;

var fp: filept; { pointer for files stack }

begin

   fp := filstk; { index top of stack }
   while fp <> nil do begin { traverse }

      regfil(fp); { register single file }
      fp := fp^.stack { next }

   end

end;

{*******************************************************************************

Parse and load instruction file

Parses and loads a list of instructions from the instruction file. The format
of the instruction file is:

! comment

command param param...param

Implemented instructions

exclude <path>/<file>

Causes the given path or file to be excluded from actions. Instead of adding a
missing file there to a compile list, for example, an error will be produced
instead. This command is used to protect library areas from recompilation.

package <name> [<name>]...

Indicates the given interface names are contained within the package file,
which is a standard object that was multiply linked.

usespath <path>

Sets the uses path. This can also come from the environment, and if it appears
here, will override the environment setting. This is the normal method used to
create an "environment free" setup.

*******************************************************************************}

procedure parinst(view ifn: string);

label nextline; { go to next line }

const cmdmax = 250;

var inshan:  parhan;  { handle for instruction parsing }
    fn:      filnam;  { filename holder }
    cmd:     filnam;  { command verb }
    err:     boolean; { parsing error }
    lp:      lstptr;  { pointer to file list entry }
    pp:      pkgptr;  { pointer to package list entry }

procedure inserr(view es: string);

begin

   prterr(inshan, output, es, true); { print error }
   getlin(inshan); { skip to new line }
   goto nextline

end;

{ skip spaces allowing '\' to bridge lines }

procedure lskpspc(inshan: parhan);

begin

   while not endfil(inshan) and not endlin(inshan) and 
         ((chkchr(inshan) = ' ') or (chkchr(inshan) = '\\')) do begin

      skpspc(inshan); { skip any spaces }
      if chkchr(inshan) = '\\' then getlin(inshan) { get next line }

   end

end;

{ parse filename or string }

procedure parfilstr(var fn: string);

begin

   if chkchr(inshan) = '"' then { check quoted }
      parstr(inshan, fn, err) { get string parameter }
   else
      parfil(inshan, fn, false, err); { get file parameter }
   if err then inserr('Invalid filename');

end;

begin

   if fverb then writeln('Reading instruction file ', ifn:0);
   openpar(inshan); { open parser }
   openfil(inshan, ifn, cmdmax); { open file to parse }

   nextline: { start new line }

   while not endfil(inshan) do begin { process instructions }
   
      skpspc(inshan); { skip leading spaces }
      if chkchr(inshan) = '!' then { skip comment line }
         while not endlin(inshan) do getchr(inshan)
      else if not endlin(inshan) then begin { command line }

         parlab(inshan, cmd, err); { get command word }
         if err then inserr('Invalid command');
         { find command }
         if compp(cmd, 'exclude') then 
            while not endlin(inshan) do begin

            lskpspc(inshan); { skip trailing ing spaces }
            parfilstr(fn); { get filename }
            fulnam(fn); { expand it }
            new(lp); { get new exclude list entry }
            copy(lp^.name, fn); { place name as filename }
            lp^.next := exclude; { push onto list }
            exclude := lp;
            lskpspc(inshan) { skip trailing spaces }

         end else if compp(cmd, 'package') then begin 

            lskpspc(inshan); { skip trailing spaces }
            parfilstr(fn); { get filename }
            fulnam(fn); { expand it }
            new(pp); { get new packaging entry }
            copy(pp^.name, fn); { place package root name }
            pp^.next := package; { push onto package list }
            package := pp;
            pp^.lst := nil; { clear contents list }
            skpspc(inshan); { skip spaces }
            if chkchr(inshan) <> '=' then inserr('''='' expected');
            getchr(inshan); { skip '=' }
            repeat { parse components }

               lskpspc(inshan); { skip trailing spaces }
               parfilstr(fn); { get filename }
               fulnam(fn); { expand it }
               new(lp); { get new list entry }
               copy(lp^.name, fn); { place package root name }
               lp^.next := pp^.lst; { push onto package list }
               pp^.lst := lp;
               lskpspc(inshan) { skip trailing spaces }

            until endlin(inshan) { until line end }

         end else if compp(cmd, 'usespath') then begin

            lskpspc(inshan); { skip trailing spaces }
            if chkchr(inshan) = '"' then { check quoted }
               parstr(inshan, usepth, err) { get string parameter }
            else
               parwrd(inshan, usepth, err); { get uses path }
            if err then inserr('Uses path too long or invalid')

         end else 
            { standard mode }
            if compp(cmd, 'standard') then fansi := true
         else
            { no standard mode }
            if compp(cmd, 'nstandard') then fansi := false
         else
            { check input overflow }
            if compp(cmd, 'overflow') then fovf := true
         else
            { no check input overflow }
            if compp(cmd, 'noverflow') then fovf := false
         else
            { check references }
            if compp(cmd, 'refer') then fref := true
         else
            { no check references }
            if compp(cmd, 'nrefer') then fref := false
         else
            { keep terminal window }
            if compp(cmd, 'keepterminalwindow') then fngwin := true
         else
            { no keep terminal window }
            if compp(cmd, 'nkeepterminalwindow') then fngwin := false
         else
            { generate coff symbols in binary }
            if compp(cmd, 'symcoff') then fsymcof := true
         else
            { no generate coff symbols in binary }
            if compp(cmd, 'nosymcoff') then fsymcof := false
         else inserr('No such instruction');
         skpspc(inshan); { skip trailing spaces }
         if chkchr(inshan) = '!' then { skip comment line }
            while not endlin(inshan) do getchr(inshan);
         if not endlin(inshan) then inserr('Invalid command')
         
      end;
      getlin(inshan) { skip to new line }

   end;
   closefil(inshan); { close the file }
   closepar(inshan) { close the parser instance }

end;

begin

   writeln;
   write('PC compiler shell vs. 1.13.01 Copyright (C) 2005 S. A. Moore');
   writeln;

   filstk := nil; { clear the files stack }
   filchr(valfch); { get the filename valid characters }
   getenv('usespath', usepth); { get the uses path }
   filcnt := 0; { clear files counter }
   filact := nil; { clear actions list }
   actcnt := 0; { set no actions performed }
   excrbl := false; { set executive does not need rebuild }
   exclude := nil; { clear exclude list }
   package := nil; { clear package list }

   { set flags }
   fverb := true; { verbose flag }
   ftree := false; { list dependency tree }
   fact := false; { list actions }
   fdry := false; { do not perform actions }
   frebld := false; { rebuild all }
   fdeftrm := false; { set no default to terminal mode }
   fdefgra := false; { set no default to graphical mode }
   { passthrough }
   fovf := true; { overflow checking on }
   fref := true; { reference checking on }
   fngwin := false; { do not override graphical windows switch }
   fsymcof := false; { do not generate coff symbols }
   siolib := false; { set no serial library found }
   grawin := false; { set no graphical windowing library found }

   { find any instruction files for us }
   getpgm(pgmpath); { get the program path }
   getusr(usrpath); { get the user path }
   getcur(curpath); { get the current path }
   maknam(tmpnam, pgmpath, 'pc', 'ins'); { create instruction file name }
   if exists(tmpnam) then parinst(tmpnam);
   if not comp(usrpath, pgmpath) then begin

      { program and user paths are not identical }
      maknam(tmpnam, usrpath, 'pc', 'ins'); { create instruction file name }
      if exists(tmpnam) then parinst(tmpnam);
      if not comp(curpath, pgmpath) and not comp(curpath, usrpath) then begin

         { current path not equal to any of program or user paths }
         maknam(tmpnam, curpath, 'pc', 'ins'); { create instruction file name }
         if exists(tmpnam) then parinst(tmpnam)

      end

   end;

   { process command line }
   openpar(cmdhan); { open parser }
   openfil(cmdhan, '_command', cmdmax); { open command line level }
   filchr(valfch); { get the filename valid characters }
   valfch := valfch-['=']; { remove parsing characters }
   setfch(cmdhan, valfch); { set that for active parsing }
   paropt; { parse command options }
   if endlin(cmdhan) then begin

      writeln('*** pc: Filename expected');
      goto 99

   end;
   skpspc(cmdhan); { skip spaces }
   if chkchr(cmdhan) = '"' then { parse string }
      parstr(cmdhan, prgnam, err) { get string parameter }
   else 
      parfil(cmdhan, prgnam, false, err); { parse filename }
   if err then begin

      writeln('*** pc: Invalid filename');
      goto 99

   end;
   paropt; { parse command options }
   skpspc(cmdhan); { skip to end }
   if not endlin(cmdhan) then begin

      writeln('*** pc: Invalid command line');
      goto 99

   end;
   brknam(prgnam, p, n, e); { add Pascal extention }
   maknam(prgnam, p, n, 'pas');
   { see if there is an instruction file to go with it }
   maknam(tmpnam, p, n, 'ins');
   if exists(tmpnam) then parinst(tmpnam);
   { now check the file itself exists }
   if not exists(prgnam) then begin { file not found }

      writeln('*** pc: Error: target file not found');
      goto 99

   end;
   logfil(prgnam, hp); { form tree from file }
   stdlib; { place standard libary }
   fndpkg; { find any included packages }
   regfils; { register files }
   if ftree then prtree; { print out the dependency tree }
   fndlnk; { find linking order }
   doacts; { perform per file actions }
   chkexc; { check executable needs rebuild }
   if (actcnt = 0) and not excrbl then
      writeln('No action required, files up to date')
   else
      dolink; { perform link }
   if fverb and (actcnt > 0) then begin

      writeln;
      writeln('Build complete');

   end;

   99: { terminate program }

end.
