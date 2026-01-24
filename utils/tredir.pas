{******************************************************************************
*
* TREDIR - Tree Directory Listing Utility
*
* Recursively lists files and directories in a tree structure.
* Each level of nesting is indented by 4 spaces.
*
******************************************************************************}

program tredir(output);

uses services, strings;

{ check if name is . or .. }
function isdots(n: pstring): boolean;
begin
   isdots := compc(n, '.') or compc(n, '..')
end;

{ recursively list directory contents with indentation }
procedure listdir(path: pstring; indent: integer);
var fp, p: filptr;
    i: integer;
    subpath, wildpath: pstring;
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

var start: pstring;

begin
   new(start, 1);
   start^ := '.';
   listdir(start, 0)
end.
