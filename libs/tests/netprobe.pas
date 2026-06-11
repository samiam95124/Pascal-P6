program netprobe(output);
uses network;
var a: lcardinal;
    fi, fo: text;
    c: char;
begin
   addrnet('localhost', a);
   writeln('addr: ', a:1);
   opennet(fi, fo, a, 12399, 0);
   writeln(fo, 'hello from pascaline');
   close(fo);
   write('received: ');
   while not eoln(fi) do begin read(fi, c); write(c) end;
   writeln;
   close(fi)
end.
