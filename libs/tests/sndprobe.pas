program sndprobe(output);
uses sound;
var n: integer;
begin
   n := synthout;
   writeln('output synthesizers: ', n:1);
   n := waveout;
   writeln('output wave devices: ', n:1)
end.
