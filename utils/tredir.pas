{******************************************************************************

TREDIR - Tree Directory Listing Utility

Recursively lists files and directories in a tree structure.
Each level of nesting is indented by 4 spaces.

Usage:

tredir

Lists all files and directories starting from the current directory.

******************************************************************************}

program tredir(output);

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

List directory

Recursively lists directory contents with indentation. Each level of nesting
adds 4 spaces of indentation.

******************************************************************************}

procedure listdir(
    { path to list } path: pstring;
    { current indentation level } indent: integer);

var { file list head } fp: filptr;
    { file list pointer } p: filptr;
    { loop counter } i: integer;
    { subdirectory path } subpath: pstring;
    { wildcard path } wildpath: pstring;

begin

    { build wildcard path: path/* }
    wildpath := cat(cat(path, '/'), '*');
    list(wildpath, fp);
    p := fp;
    while p <> nil do begin

        { skip . and .. entries }
        if not isdots(p^.name) then begin

            { print indentation }
            for i := 1 to indent do write(' ');
            { print filename }
            writeln(p^.name^);
            { if directory, recurse into it }
            if atdir in p^.attr then begin

                { build subpath: path + '/' + name }
                subpath := cat(cat(path, '/'), p^.name);
                listdir(subpath, indent + 4)

            end

        end;
        p := p^.next

    end

end;

var { starting path } start: pstring;

begin

    new(start, 1);
    start^ := '.';
    listdir(start, 0)

end.
