{ Stub program: references every sound binding form so the mangled
  names appear in the object for trampoline generation. Compile with
  pc (the link fails; only the .o is needed). }
program soundstub(output);

uses sound;

var ii: integer;
    lc: lcardinal;
    ss: packed array [1..100] of char;
    bb: packed array [1..100] of byte;
    tf, tg: text;
    ch: chan; nt: note; it: instrument;

begin
   starttimeout;
   stoptimeout;
   ii := curtimeout;
   starttimein;
   stoptimein;
   ii := curtimein;
   ii := synthout;
   ii := synthin;
   opensynthout(1);
   closesynthout(1);
   opensynthin(1);
   closesynthin(1);
   noteon(1, 1, ch, nt, 1);
   noteoff(1, 1, ch, nt, 1);
   instchange(1, 1, ch, it);
   attack(1, 1, ch, 1);
   release(1, 1, ch, 1);
   legato(1, 1, ch, 1);
   portamento(1, 1, ch, 1);
   vibrato(1, 1, ch, 1);
   volsynthchan(1, 1, ch, 1);
   porttime(1, 1, ch, 1);
   balance(1, 1, ch, 1);
   pan(1, 1, ch, 1);
   timbre(1, 1, ch, 1);
   brightness(1, 1, ch, 1);
   reverb(1, 1, ch, 1);
   tremulo(1, 1, ch, 1);
   chorus(1, 1, ch, 1);
   celeste(1, 1, ch, 1);
   phaser(1, 1, ch, 1);
   aftertouch(1, 1, ch, nt, 1);
   pressure(1, 1, ch, 1);
   pitch(1, 1, ch, 1);
   pitchrange(1, 1, ch, 1);
   mono(1, 1, ch, 1);
   poly(1, 1, ch);
   loadsynth(1, ss);
   playsynth(1, 1, 1);
   delsynth(1);
   waitsynth(1);
   ii := waveout;
   ii := wavein;
   openwaveout(1);
   closewaveout(1);
   loadwave(1, ss);
   playwave(1, 1, 1);
   delwave(1);
   volwave(1, 1, 1);
   waitwave(1);
   chanwaveout(1, 1);
   ratewaveout(1, 1);
   lenwaveout(1, 1);
   sgnwaveout(1, 1);
   fltwaveout(1, 1);
   endwaveout(1, 1);
   wrwave(1, bb);
   openwavein(1);
   closewavein(1);
   ii := chanwavein(1);
   ii := ratewavein(1);
   ii := lenwavein(1);
   ii := sgnwavein(1);
   ii := endwavein(1);
   ii := fltwavein(1);
   ii := rdwave(1, bb);
   synthoutname(1, ss);
   synthinname(1, ss);
   waveoutname(1, ss);
   waveinname(1, ss);
   ii := setparamsynthin(1, ss, ss);
   ii := setparamsynthout(1, ss, ss);
   ii := setparamwavein(1, ss, ss);
   ii := setparamwaveout(1, ss, ss);
   getparamsynthin(1, ss, ss);
   getparamsynthout(1, ss, ss);
   getparamwavein(1, ss, ss);
   getparamwaveout(1, ss, ss)
end.
