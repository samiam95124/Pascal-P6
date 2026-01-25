{******************************************************************************

TREDIR - Tree Directory Listing Utility

Recursively lists files and directories in a tree structure using box-drawing
characters to show the hierarchy. Directories are listed before files, and
entries are sorted alphabetically.

Usage:

tredir [path]

Lists all files and directories starting from the specified path, or the
current directory if no path is given.

******************************************************************************}

program tredir(command, output);

uses services, strings;

{******************************************************************************

Check if name is . or ..

Returns true if the given name is the special "." or ".." directory entry.

******************************************************************************}

function isdots(
    { name to check } n: pstring):
    { true if . or .. } boolean;

begin

    isdots := compc(n, '.') or compc(n, '..')

end;

{******************************************************************************

Check if entry is directory

Returns true if the entry is a directory (excluding . and ..).

******************************************************************************}

function isdir(
    { file entry } p: filptr):
    { true if directory } boolean;

begin

    isdir := (atdir in p^.attr) and not isdots(p^.name)

end;

{******************************************************************************

Compare strings

Compares two pstrings lexicographically. Returns true if a < b.

******************************************************************************}

function strless(
    { first string } a: pstring;
    { second string } b: pstring):
    { true if a < b } boolean;

var { index } i: integer;
    { length of a } lena: integer;
    { length of b } lenb: integer;
    { minimum length } minlen: integer;
    { result } r: boolean;
    { done flag } done: boolean;

begin

    lena := len(a);
    lenb := len(b);
    if lena < lenb then minlen := lena
    else minlen := lenb;
    i := 1;
    done := false;
    r := false;
    while (i <= minlen) and not done do begin

        if a^[i] < b^[i] then begin

            r := true;
            done := true

        end else if a^[i] > b^[i] then begin

            r := false;
            done := true

        end else
            i := i + 1

    end;
    if not done then r := lena < lenb;

    strless := r

end;

{******************************************************************************

Sort file list

Sorts a file list alphabetically using insertion sort.

******************************************************************************}

procedure sortlist(
    { file list to sort } var fp: filptr);

var { current pointer } p: filptr;
    { sorted list head } sorted: filptr;
    { sorted list tail } stail: filptr;
    { insertion point } ins: filptr;
    { previous for insertion } prev: filptr;
    { next to process } next: filptr;
    { found flag } found: boolean;

begin

    sorted := nil;
    stail := nil;
    p := fp;
    while p <> nil do begin

        next := p^.next;
        p^.next := nil;
        if sorted = nil then begin

            sorted := p;
            stail := p

        end else if strless(p^.name, sorted^.name) then begin

            { insert at head }
            p^.next := sorted;
            sorted := p

        end else begin

            { find insertion point }
            prev := sorted;
            ins := sorted^.next;
            found := false;
            while (ins <> nil) and not found do begin

                if strless(p^.name, ins^.name) then
                    found := true
                else begin

                    prev := ins;
                    ins := ins^.next

                end

            end;
            p^.next := ins;
            prev^.next := p;
            if ins = nil then stail := p

        end;
        p := next

    end;
    fp := sorted

end;

{******************************************************************************

Count entries

Counts the number of non-dot entries in a file list.

******************************************************************************}

function countentries(
    { file list } fp: filptr):
    { number of entries } integer;

var { list pointer } p: filptr;
    { count } n: integer;

begin

    n := 0;
    p := fp;
    while p <> nil do begin

        if not isdots(p^.name) then n := n + 1;
        p := p^.next

    end;

    countentries := n

end;

{******************************************************************************

Print entry

Prints a single entry with proper tree characters and recurses for directories.

******************************************************************************}

procedure printentry(
    { file entry } p: filptr;
    { path to entry } path: pstring;
    { prefix string } prefix: pstring;
    { is last entry } islast: boolean);

forward;

{******************************************************************************

List directory

Recursively lists directory contents with tree-drawing characters.
Uses box-drawing characters to show hierarchy. Lists directories first,
then files, both sorted alphabetically.

******************************************************************************}

procedure listdir(
    { path to list } path: pstring;
    { prefix string for indentation } prefix: pstring);

var { file list head } fp: filptr;
    { file list pointer } p: filptr;
    { wildcard path } wildpath: pstring;
    { entry count } total: integer;
    { current entry } current: integer;
    { is last entry } islast: boolean;

begin

    { build wildcard path: path/* }
    wildpath := cat(cat(path, '/'), '*');
    list(wildpath, fp);
    sortlist(fp);
    total := countentries(fp);
    current := 0;

    { first pass: directories }
    p := fp;
    while p <> nil do begin

        if isdir(p) then begin

            current := current + 1;
            islast := current = total;
            printentry(p, path, prefix, islast)

        end;
        p := p^.next

    end;

    { second pass: files }
    p := fp;
    while p <> nil do begin

        if not isdots(p^.name) and not (atdir in p^.attr) then begin

            current := current + 1;
            islast := current = total;
            printentry(p, path, prefix, islast)

        end;
        p := p^.next

    end

end;

{******************************************************************************

Print entry

Prints a single entry with proper tree characters and recurses for directories.

******************************************************************************}

procedure printentry(
    { file entry } p: filptr;
    { path to entry } path: pstring;
    { prefix string } prefix: pstring;
    { is last entry } islast: boolean);

var { subdirectory path } subpath: pstring;
    { new prefix for children } newprefix: pstring;

begin

    { print prefix and branch character }
    write(prefix^);
    if islast then write('└── ')
    else write('├── ');
    { print filename }
    writeln(p^.name^);
    { if directory, recurse into it }
    if atdir in p^.attr then begin

        { build subpath: path + '/' + name }
        subpath := cat(cat(path, '/'), p^.name);
        { build new prefix: add vertical line or spaces }
        if islast then newprefix := cat(prefix, '    ')
        else newprefix := cat(prefix, '│   ');
        listdir(subpath, newprefix)

    end

end;

var { starting path } start: pstring;
    { empty prefix } prefix: pstring;
    { command line argument } arg: pstring;

begin

    { check for path argument }
    arg := argp;
    if arg = nil then begin

        { no argument, use current directory }
        new(start, 1);
        start^ := '.'

    end else
        start := arg;
    writeln(start^);
    new(prefix, 0);
    listdir(start, prefix)

end.
