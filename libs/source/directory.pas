{*******************************************************************************
*                                                                              *
*                          Directory handling library                          *
*                                                                              *
*                              2002/08/25                                      *
*                                                                              *
* Implements several directory list management functions. dirlib models        *
* directories as tree structured data, so all of the functions here are about  *
* creating a directory tree from the supplied specification, and managing one  *
* or more directory trees.                                                     *
*                                                                              *
* order        Sorts a file list for the specified ordering function.          *
* orderdirs    Sorts a directory list for the specified ordering function.     *
* chkdir       Returns true if the indicated file is a directory.              *
* merge        Merges two directory lists into one using names as key.         *
* filelist     Extends "list" with attribute and permission matching.          *
* treelist     Forms a full tree structured directory/file listing with        *
*              attribute and permission matching.                              *
*                                                                              *
*******************************************************************************}

module dirlib(output);

uses strlib,
     extlib;

type  dirptr = ^dirent; { pointer to directory record }
      dirent = record { directory head chains }
      
         name:  pstring; { name of directory (path) }
         files: filptr;  { files in directory }
         cnt:   integer; { number of files in list }
         tsize: integer; { total size of contained files }
         nmax:  integer; { maximum length in names }
         drop:  filptr;  { file entry directory was dropped from }
         next:  dirptr   { next entry in list }

      end;
      dirord = (onone, oalpha, osize, otime); { ordering types }

procedure order(var l: filptr; ordtyp: dirord; rev: boolean); forward;
procedure orderdirs(var l: dirptr; ordtyp: dirord; rev: boolean); forward;
function chkdir(view n: string): boolean; forward;
procedure merge(var dlst: dirptr; slst: dirptr); forward;
procedure join(var dlst: dirptr; slst: dirptr); forward;
procedure filelist(view n: string; atrmsk, atrcmp: attrset; usrmsk, usrcmp,
                   grpmsk, grpcmp, othmsk, othcmp: permset;
                   var fl: filptr); forward;
procedure treelist(view fn: string; expand, recurse: integer; marker: boolean;
                   atrmsk, atrcmp: attrset; usrmsk, usrcmp, grpmsk, grpcmp,
                   othmsk, othcmp: permset; var tree: dirptr); forward;

private

const filmax = 1000; { number of characters in a filename }

type  filinx = 1..filmax; { index for filename }
      filnam = packed array [filinx] of char; { a filename }

{*******************************************************************************

Make files list

This is our wrapper to the basic function. Accepts a wildcarded name, attribute
mask and compare value, user, group and other mask and compare, and the list
pointer to place the result in.

*******************************************************************************}

procedure filelist(view n: string; atrmsk, atrcmp: attrset; usrmsk, usrcmp,
                   grpmsk, grpcmp, othmsk, othcmp: permset;
                   var fl: filptr); { list }

var l, p: filptr; { list pointers }

begin

   list(n, fl); { make files list }
   { delete unmatched entries from list }
   l := nil; { clear destination }
   while fl <> nil do begin { reverse list }

      p := fl; { index top }
      fl := fl^.next; { gap out }
      if (p^.attr*atrmsk = atrcmp) and (p^.user*usrmsk = usrcmp) and
         (p^.group*grpmsk = grpcmp) and (p^.other*othmsk = othcmp) then begin

         { entry compares, include in listing }
         p^.next := l; { push onto new list }
         l := p

      end

   end;
   { reverse list back to order }
   fl := nil; { clear desitination }
   while l <> nil do begin { reverse list }

      p := l; { index top }
      l := l^.next; { gap out }
      p^.next := fl; { push onto new list }
      fl := p

   end;

end;

{*******************************************************************************

Check file is directory

Checks if the given file exists, and is a directory.

*******************************************************************************}

function chkdir(view n: string): boolean;

var fp: filptr;
    r:  boolean;

begin

   r := false; { set file does not exist }
   list(n, fp); { get file entry }
   if fp <> nil then { file exists }
      r := atdir in fp^.attr; { set directory status of file }
   chkdir := r { pass result }

end;

{*******************************************************************************

Order list 

Reorders the given files list using the specified ordering function. If the
reverse flag is set, the order is reversed, such as sort from z to a, etc.

*******************************************************************************}

procedure order(var l: filptr; ordtyp: dirord; rev: boolean);

var p1, p2, p3, p4: filptr; { temporary list root }
    f: boolean;

begin

   p1 := l; { save list }
   l := nil; { clear result list }
   while p1 <> nil do begin { order list }

      { remove from source list }
      p2 := p1;
      p1 := p1^.next;
      p3 := l; { index destination root }
      p4 := nil; { clear last label }
      f := false; { set no match }
      while not f and (p3 <> nil) do begin

         { check insert label < inpection label }
         { use ordering function }
         case ordtyp of { order }

            osize:  if (p2^.size < p3^.size) <> rev then f := true;

            otime:  if (p2^.modify < p3^.modify) then f := true; 

            oalpha: if gtrp(p2^.name^, p3^.name^) <> rev then f := true;

            onone:  f := true { this will reverse the list }

         end;
         if not f then begin { go next }

            p4 := p3; { save last }
            p3 := p3^.next { next entry }

         end

      end;
      if p4 = nil then begin { insert at top }

         p2^.next := l;
         l := p2

      end else begin { insert in middle }

         p2^.next := p3;
         p4^.next := p2

      end

   end

end;

{*******************************************************************************

Order directories list 

Reorders all file lists in a directory list.

*******************************************************************************}

procedure orderdirs(var l: dirptr; ordtyp: dirord; rev: boolean);

var dp: dirptr;

begin

   dp := l; { index top of list }
   while dp <> nil do begin { traverse list }

      order(dp^.files, ordtyp, rev); { order files list }
      dp := dp^.next { next directory }

   end

end;

{*******************************************************************************

Find size total

Totals the sizes of all files in the given list.

*******************************************************************************}

function fndsiz(l: filptr): integer;

var ts: integer; { size holder }

begin

   ts := 0; { clear result } 
   while l <> nil do begin { traverse }

      ts := ts + l^.size; { add size }
      l := l^.next { next entry }

   end;
   fndsiz := ts { return result }

end;

{*******************************************************************************

Calculate maximum name length

Finds the maximum length of names in the given files list.

*******************************************************************************}

function fndmax(dp: dirptr): integer;

var maxnam: integer; { maximum holder }
    l:      filptr;  { pointer to files }

begin

   maxnam := 0; { clear maximum name length }
   l := dp^.files; { index top of files list }
   while l <> nil do begin { traverse }

      { set new maximum if greater }
      if max(l^.name^) > maxnam then maxnam := max(l^.name^);
      l := l^.next { next entry }

   end;
   fndmax := maxnam { return result }

end;

{*******************************************************************************

Update list count and max

Updates the list count, max and tsize parameters for a directory.

*******************************************************************************}

procedure upddir(dp: dirptr);

var lp: filptr; { pointer to files }

begin

   { count files }
   dp^.cnt := 0; { initalize files count }
   lp := dp^.files; { index top of list }
   while lp <> nil do begin { count files }

      dp^.cnt := dp^.cnt+1; { count file }
      lp := lp^.next { next entry }

   end;
   { find file maximum name length }
   dp^.nmax := fndmax(dp); { find maximum names length }
   dp^.tsize := fndsiz(dp^.files) { find total sizes }
   
end;

{*******************************************************************************

Update list count and max on directory list

Updates the list count, max and tsize parameters for a directory list.

*******************************************************************************}

procedure upddirs(dp: dirptr);

begin

   while dp <> nil do begin { traverse }

      upddir(dp); { update entry }
      dp := dp^.next { next entry }

   end

end;

{*******************************************************************************

Merge new directory

Checks if the directory name matches an existing directory in the list, and
merges the two if so.
Releases the head directory entry upon merge.

*******************************************************************************}

procedure merged(var dlst: dirptr; slst: dirptr);

var dp, dfp: dirptr; { pointers to directory list }
    fp:      filptr; { pointer for file entry }
    

begin

   dp := dlst; { index top of directory list }
   dfp := nil; { set no directory found }
   while dp <> nil do begin { traverse list }

      if compp(dp^.name^, slst^.name^) and (dfp = nil) then
         dfp := dp; { set directory if found }
      dp := dp^.next { next directory }

   end;
   if dfp <> nil then begin { merge directory lists }

      { check null case }
      if dfp^.files = nil then dfp^.files := slst^.files { just place files }
      else begin { find end of list }

         fp := dfp^.files; { index top of list }
         while fp^.next <> nil do fp := fp^.next; { find end of list }
         fp^.next := slst^.files { link lists }

      end;
      dispose(slst^.name); { release redundant entry }
      dispose(slst);
      
   end else begin { just dump into list }

      slst^.next := dlst; { push onto list }
      dlst := slst

   end

end;

{*******************************************************************************

Merge directory lists

Merges two directory lists together by name.

*******************************************************************************}

procedure merge(var dlst: dirptr; slst: dirptr);

var dp: dirptr; { pointer to directory entries }

begin

   while slst <> nil do begin { merge directories }

      dp := slst; { index top entry }
      slst := slst^.next; { gap }
      merged(dlst, dp) { merge with destination list }
     
   end;
   upddirs(dlst) { update parameters }

end;

{*******************************************************************************

Join directory lists

Joins one directory list to the end of another, without merging.

*******************************************************************************}

procedure join(var dlst: dirptr; slst: dirptr);

var dp: dirptr; { pointer to directory entries }

begin

   if dlst = nil then dlst := slst { empty destination, just equate }
   else begin { full join }

      { find end of first list }
      dp := dlst; { index top entry }
      while dp^.next <> nil do dp := dp^.next; { find last }
      dp^.next := slst { link lists }

   end

end;

{*******************************************************************************

Make directory listing

Given a possibly wildcarded filename specification, creates a directory entry
for that, sets the directory name (path), and generates a list of files for
the directory entry.

*******************************************************************************}

procedure makdir(view s: string; expand: integer; dfp: filptr;
                 var ff: boolean; var dp: dirptr; atrmsk, atrcmp: attrset;
                 usrmsk, usrcmp, grpmsk, grpcmp, othmsk, othcmp: permset;
                 var dirlst: dirptr);

var p, n, e:     filnam; { filename components }
    name, name1: filnam; { name holder }
    fp:          filptr; { file entry pointer }
    ph:          filnam; { path holding }
    dp1:         dirptr; { directory pointer }

begin

   copy(name, s); { copy name }
   brknam(name, ph, n, e); { get specified path first }
   fulnam(name); { form full path }
   brknam(name, p, n, e); { extract path }
   new(dp); { get a new directory entry }
   { check filename ends with dangling path, and expand if so }
   if name[len(name)] = '\\' {pthchr} then begin

      copy(p, name); { set directory as path }
      copy(n, '*'); { set name wild }
      copy(e, ''); { clear extention }
      maknam(name, p, n, e) { remake name }
      
   end;
   dp^.name := copy(p); { place the path }
   dp^.drop := dfp; { place drop pointer }
   filelist(name, atrmsk, atrcmp, usrmsk, usrcmp, grpmsk, grpcmp,
            othmsk, othcmp, dp^.files); { make file list }
   if dp^.files <> nil then begin

      ff := true;
      { process expand or recurse, but not both }
      if expand > 0 then begin { process expands on list }

         fp := dp^.files; { index top of files list }
         while fp <> nil do begin { traverse }

            { check directory, but not looping special entry }
            if (atdir in fp^.attr) and not (atloop in fp^.attr) then begin

               { create path from directory name and filename }
               maknam(name1, dp^.name^, fp^.name^, ''); { create a name }
               maknam(name, name1, '*', ''); { add wildcard ending }
               { and reprocess with one less level }
               makdir(name, expand-1, fp, ff, dp1, atrmsk, atrcmp, usrmsk,
                      usrcmp, grpmsk, grpcmp, othmsk, othcmp, dirlst)

            end;
            fp := fp^.next { next entry }

         end

      end;
      { merge with existing directories }
      merged(dirlst, dp)

   end else begin { tear down entry }

      dispose(dp^.name); { dispose of name }
      dispose(dp); { dispose of entry }
      dp := nil { clear it }

   end
   
end;

{*******************************************************************************

Make recursive listing

Creates a recursive listing using the given filespec and path. The path is
typically started as blank (current directory), then the filespec is applied to
the current directory, then to each subdirectory within the recursion level.
The filespec must not contain a path.

*******************************************************************************}

procedure makrecur(view pn, fn: string; recurse: integer; marker: boolean;
                   dfp: filptr; var ff: boolean; atrmsk, atrcmp: attrset;
                   usrmsk, usrcmp, grpmsk, grpcmp, othmsk, othcmp: permset;
                   var dirlst: dirptr);

var p, n, e:     filnam;  { filename components }
    name:        filnam;  { name holder }
    fp, fl:      filptr;  { file entry pointer }
    ffnd:        boolean; { found file(s) flag }
    dp:          dirptr;  { directory pointer }

procedure mrgdir(view pn: string; var dp: dirptr; fp: filptr);

var fp1, ffp, lfp: filptr; { file entry pointers }

begin

   if dp = nil then begin

      new(dp); { get a new directory entry }
      dp^.name := copy(pn); { place the path }
      dp^.drop := dfp; { place drop pointer }
      dp^.files := nil; { clear files list }
      dp^.next := dirlst; { push onto list }
      dirlst := dp

   end;
   { find existing entry on list }
   fp1 := dp^.files; { index top of list }
   ffp := nil; { clear found pointer }
   lfp := nil; { clear last }
   while fp1 <> nil do begin { traverse }

      if compp(fp^.name^, fp1^.name^) then begin

         ffp := fp1; { set found }
         fp1 := nil { terminate }

      end else begin { next entry }

         lfp := fp1; { set last }
         fp1 := fp1^.next { next entry }   

      end

   end;
   if ffp <> nil then begin { found, remove }

      if lfp = nil then dp^.files := ffp^.next { gap }
      else lfp^.next := ffp^.next; { remove mid list }
      dispose(ffp^.name); { release name }
      dispose(ffp) { release entry }

   end;
   fp^.next := dp^.files; { push onto list }
   dp^.files := fp

end;

begin

   brknam(fn, p, n, e); { break down file }
   maknam(name, pn, n, e); { create full path specification }
   { create a listing at this level }
   makdir(name, 0, dfp, ff, dp, atrmsk, atrcmp, usrmsk, usrcmp,
          grpmsk, grpcmp, othmsk, othcmp, dirlst);
   { the above created a specified list, now we need all directories. if a
     directory is found that has entries we need below it, we must merge
     that with the listing obtained in the last step, which could be nil }
   maknam(name, pn, '*', ''); { create full list }
   filelist(name, atrmsk, atrcmp, usrmsk, usrcmp, grpmsk, grpcmp,
            othmsk, othcmp, fl); { make file list }
   while fl <> nil do begin { process files in list }

      fp := fl; { index top of list }
      fl := fl^.next; { gap }
      { check directory, but not loop special }
      if (atdir in fp^.attr) and not (atloop in fp^.attr) then begin

         { now we reapply the base wildcard spec to the subdirectory }
         maknam(name, pn, fp^.name^, '');
         { and reprocess with one less level }
         ffnd := false; { set no file found }
         if marker then { generate with marker references }
            makrecur(name, fn, recurse-1, marker, fp, ffnd, atrmsk, atrcmp,
                     usrmsk, usrcmp, grpmsk, grpcmp, othmsk, othcmp, dirlst)
         else { generate without markers }
            makrecur(name, fn, recurse-1, marker, nil, ffnd, atrmsk, atrcmp,
                     usrmsk, usrcmp, grpmsk, grpcmp, othmsk, othcmp, dirlst);
         { if the file is somewhere under this tree, and we are in drop
           directory mode, we must merge it so it shows up in the tree }
         if ffnd and marker then mrgdir(name, dp, fp) else begin { dispose }

            dispose(fp^.name); { release name }
            dispose(fp) { release entry }

         end

      end else begin

         dispose(fp^.name); { release name }
         dispose(fp) { release entry }

      end

   end

end;

{*******************************************************************************

Make directory tree

Processes a single file reference into a directory tree. The following
parameters are accepted:

fn    Contains the wildcarded specification. If there is a path, that is used,
otherwise, the default path is used. If a single, non-pathed filename with no
wildcards appears and it is a directory, that entire subdirectory is assumed.

expand   If greater than zero, indicates that each directory is to be expanded
to the expand number of levels. Ie., expand = 1 means to expand all directories
in the specified directory, expand = 2 means to expand all of the directories
under that, etc. Setting expand = maxint effectively means "expand all levels".
If recurse is also specified, expand takes precidence over it.

recurse  If greater than zero, indicates that each directory will have the
file specification applied to it, to the number of levels specified.

atrmsk,  Indicates the attribute bits that are to be compared, and what state
atrcmp   they will be when compared. Correct files will be included in the list.

usrmsk,
usrcmp   Indicates the user permission compare status.

grpmsk,
grpcmp   Indicates the user permission compare status.

othmsk,
othcmp   Indicates the user permission compare status.

order    Indicates what type of ordering to perform on the list.

rev      Indicates the normal sort order is to be performed backwards.

tree     Gives the result directory tree.

*******************************************************************************}

procedure treelist(view fn: string; expand, recurse: integer; marker: boolean;
                   atrmsk, atrcmp: attrset; usrmsk, usrcmp, grpmsk, grpcmp,
                   othmsk, othcmp: permset; var tree: dirptr);

var p, n, e: filnam;  { filename components }
    ffnd:    boolean; { file found flag }
    dp:      dirptr;  { directory pointer }

begin

   ffnd := false; { set no file found (we don't use the result ) }
   { give priority to expand }
   if (expand > 0) or (recurse = 0) then
      makdir(fn, expand, nil, ffnd, dp, atrmsk, atrcmp, usrmsk, usrcmp,
             grpmsk, grpcmp, othmsk, othcmp, tree)
   else begin

      brknam(fn, p, n, e); { find possible path }
      { check path exists }
      if p[1] <> ' ' then { process normal mode }
         makdir(fn, expand, nil, ffnd, dp, atrmsk, atrcmp, usrmsk, usrcmp,
                grpmsk, grpcmp, othmsk, othcmp, tree)
      else { process recursion mode }
         makrecur('', fn, recurse, marker, nil, ffnd, atrmsk, atrcmp, usrmsk,
                  usrcmp, grpmsk, grpcmp, othmsk, othcmp, tree)

   end;
   upddirs(tree) { perform parameter updates on directory tree }

end;

begin

   refer(output)

end.
