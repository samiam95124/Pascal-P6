{******************************************************************************
*                                                                             *
*                          SOUND LIBRARY TEST PROGRAM                         *
*                                                                             *
* Goes through various test cases on the sound library. It is the Pascaline  *
* equivalent of the Ami sound_test.c reference test.                          *
*                                                                             *
* Notes:                                                                      *
*                                                                             *
* 1. The MIDI tests not only test sound.c, but also the synthesizer           *
* implementation.                                                             *
*                                                                             *
* 2. The output port defaults to synth_out (the first fluidsynth instance)    *
* and can be given as a number on the command line, the equivalent of the C  *
* original's --port option.                                                  *
*                                                                             *
* The terminate event (etterm) is handled by polling in the wait routines,    *
* which perform a non-local goto to the terminate label.                      *
*                                                                             *
******************************************************************************}

program sound_test(input, output, command);

uses terminal, { terminal level functions }
     sound;    { sound library }

label 99; { terminate }

const

   second = 10000;      { one second in 100 microsecond ticks }
   i32max = 2147483647; { 32 bit INT_MAX, the parameter scale used by sound }

var

   n:         note;       { note }
   ins:       instrument; { instrument }
   i, x, j:   integer;
   dport:     integer;    { output port }
   randstate: integer;    { random number generator state }

{ find random number between 0 and N (linear congruential generator, since
  the runtime exposes no rand primitive; scaled with mod to avoid 32-bit
  multiply overflow) }

function randn(limit: integer): integer;

begin

   { advance LCG: state := (state*1103515245+12345) mod 2^31, kept positive }
   randstate := (randstate*1103515245+12345) mod i32max;
   if randstate < 0 then randstate := randstate+i32max;
   if limit <= 0 then randn := 0
   else randn := randstate mod (limit+1)

end;

{******************************************************************************

Wait time

wait time in 100 microseconds.

******************************************************************************}

procedure waittime(t: integer);

var er: evtrec; { event record }

begin

   timer(output, 1, t, false);
   repeat event(input, er) until (er.etype = ettim) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{******************************************************************************

Wait user interaction

Wait return to be pressed, or handle terminate.

******************************************************************************}

procedure waitnext;

var er: evtrec; { event record }

begin

   repeat event(input, er) until (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{******************************************************************************

Wait return

Prints a message and waits for return to be pressed, or handle terminate.

******************************************************************************}

procedure waitret;

begin

   writeln('Hit return to continue');
   waitnext

end;

{******************************************************************************

Play random notes

Plays random notes on the current instrument, for a given number of notes, to
the given port.

******************************************************************************}

procedure playrand(port: integer; notes: integer);

var key: note;
    i:   integer;

begin

   randstate := 42; { seed random number generator }
   for i := 1 to notes do begin

      { Generate a random key }
      key := 60+randn(12)-1;
      { Play a note }
      noteon(port, 0, 1, key, i32max);
      { Sleep for 1/10 second }
      waittime(second div 20);
      { Stop the note }
      noteoff(port, 0, 1, key, 0);
      { Sleep for 1/10 second }
      waittime(second div 20)

   end

end;

{******************************************************************************

Play note

Just plays a test note, with 1/4 on and off times. Plays middle C.

******************************************************************************}

procedure playnote(port: integer; n: note);

begin

   noteon(port, 0, 1, n, i32max); { play middle C }
   waittime(second div 4);
   noteoff(port, 0, 1, n, 0);
   waittime(second div 4)

end;

{******************************************************************************

Play scale

Plays a simple scale with on time.

******************************************************************************}

procedure playscale(port: integer; t: integer);

begin

   noteon(port, 0, 1, note_c+octave_6, i32max);
   waittime(t);
   noteoff(port, 0, 1, note_c+octave_6, 0);
   waittime(second div 4);
   noteon(port, 0, 1, note_d+octave_6, i32max);
   waittime(t);
   noteoff(port, 0, 1, note_d+octave_6, 0);
   waittime(second div 4);
   noteon(port, 0, 1, note_e+octave_6, i32max);
   waittime(t);
   noteoff(port, 0, 1, note_e+octave_6, 0);
   waittime(second div 4);
   noteon(port, 0, 1, note_f+octave_6, i32max);
   waittime(t);
   noteoff(port, 0, 1, note_f+octave_6, 0);
   waittime(second div 4);
   noteon(port, 0, 1, note_g+octave_6, i32max);
   waittime(t);
   noteoff(port, 0, 1, note_g+octave_6, 0);
   waittime(second div 4);
   noteon(port, 0, 1, note_a+octave_6, i32max);
   waittime(t);
   noteoff(port, 0, 1, note_a+octave_6, 0);
   waittime(second div 4);
   noteon(port, 0, 1, note_b+octave_6, i32max);
   waittime(t);
   noteoff(port, 0, 1, note_b+octave_6, 0);
   waittime(second div 4)

end;

begin

   { The output port: the default synthesizer (the first fluidsynth
     instance), or the port number given on the command line (the C
     original's --port option). }
   dport := synth_out;
   if not eof(command) then read(command, dport);

   {***************************************************************************

   MIDI TESTS

   ***************************************************************************}

   opensynthout(dport);

   instchange(dport, 0, 1, inst_acoustic_grand);

   writeln('Sound library test');
   writeln;
   writeln('Runs through various sound tests and gives you a chance to');
   writeln('evaluate if the sound produced matches the description.');
   writeln;
   writeln;
   writeln('Note that this test can also serve as a test of the output synthesizer.');
   writeln('Not all synths implement all modes or instruments. In fact, it is common');
   writeln('to leave many features unimplemented.');
   waitret;

   writeln('Run through the entire scale of notes available');
   for n := note_c+octave_1 to note_g+octave_11 do begin

      write(n:1, ' ');
      noteon(dport, 0, 1, n, i32max);
      waittime(second div 10);
      noteoff(dport, 0, 1, n, 0)

   end;
   writeln;
   writeln('Complete');
   waitret;

   writeln('Run through all instruments with middle C');
   writeln('Note that not all syths implement all instruments');
   write('Instruments: ');
   for ins := inst_acoustic_grand to inst_gunshot do begin

      write(ins:1, ' ');
      instchange(dport, 0, 1, ins);
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 10);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 10)

   end;
   writeln;
   instchange(dport, 0, 1, inst_acoustic_grand);
   writeln('Complete');
   waitret;

   writeln('Run though all percussive instruments');
   writeln('Note that not all syths implement all instruments');
   write('Instruments: ');
   for n := note_acoustic_bass_drum to note_open_triangle do begin

      write(n:1, ' ');
      noteon(dport, 0, 10, n, i32max);
      waittime(second div 10);
      noteoff(dport, 0, 10, n, 0);
      waittime(second div 10)

   end;
   writeln;
   writeln('Complete');
   waitret;

   writeln('Chop test, play note series and repeat with the envelope time');
   writeln('limited by noteoff');
   writeln('First piano, then organ');
   writeln('Note that some synths appear to set a minimum on note length');
   instchange(dport, 0, 1, inst_acoustic_grand);
   for i := 10 downto 1 do playscale(dport, i*(second div 30));
   instchange(dport, 0, 1, inst_drawbar_organ);
   for i := 10 downto 1 do playscale(dport, i*(second div 30));
   writeln('Complete');
   waitret;

   writeln('Note volume test');
   instchange(dport, 0, 1, inst_acoustic_grand);
   for i := 0 to 19 do begin

      noteon(dport, 0, 1, note_c+octave_6, i*(i32max div 20));
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   writeln('Complete');
   waitret;

   writeln('Random note programming piano:');
   waitret;
   playrand(dport, 100);
   writeln('Complete');
   waitret;

   writeln('Random note programming harpsichord:');
   waitret;
   instchange(dport, 0, 1, inst_harpsichord);
   playrand(dport, 100);
   writeln('Complete');
   waitret;

   writeln('Random note programming organ:');
   waitret;
   instchange(dport, 0, 1, inst_drawbar_organ);
   playrand(dport, 100);
   writeln('Complete');
   waitret;

   writeln('Random note programming soprando sax:');
   waitret;
   instchange(dport, 0, 1, inst_soprano_sax);
   playrand(dport, 100);
   writeln('Complete');
   waitret;

   writeln('Random note programming telephone:');
   waitret;
   instchange(dport, 0, 1, inst_telephone_ring);
   playrand(dport, 100);
   writeln('Complete');
   waitret;

   { restore piano }
   instchange(dport, 0, 1, inst_acoustic_grand);

   { set attack times }
   writeln('Set step attack times on piano');
   waitret;
   for i := 0 to 10 do begin

      writeln('Attack: ', i*(i32max div 10):1);
      attack(dport, 0, 1, i*(i32max div 10));
      playnote(dport, note_c+octave_6)

   end;
   attack(dport, 0, 1, i32max div 2); { reset normal }
   writeln('Complete');
   waitret;

   writeln('Set step attack times on organ');
   waitret;
   instchange(dport, 0, 1, inst_drawbar_organ);
   for i := 0 to 10 do begin

      writeln('Attack: ', i*(i32max div 10):1);
      attack(dport, 0, 1, i*(i32max div 10));
      playnote(dport, note_c+octave_6)

   end;
   attack(dport, 0, 1, i32max div 2); { reset normal }
   writeln('Complete');
   waitret;

   { set release times }
   writeln('Set step release times on piano');
   waitret;
   instchange(dport, 0, 1, inst_acoustic_grand);
   for i := 0 to 10 do begin

      writeln('Release: ', i*(i32max div 10):1);
      release(dport, 0, 1, i*(i32max div 10));
      playnote(dport, note_c+octave_6)

   end;
   release(dport, 0, 1, i32max div 2); { reset normal }
   writeln('Complete');
   waitret;

   writeln('Set step release times on organ');
   waitret;
   instchange(dport, 0, 1, inst_drawbar_organ);
   for i := 0 to 10 do begin

      writeln('Release: ', i*(i32max div 10):1);
      release(dport, 0, 1, i*(i32max div 10));
      playnote(dport, note_c+octave_6)

   end;
   release(dport, 0, 1, i32max div 2); { reset normal }
   writeln('Complete');
   waitret;

   { set legato }
   writeln('Set legato on piano, first normal, then legato');
   waitret;
   instchange(dport, 0, 1, inst_acoustic_grand);
   legato(dport, 0, 1, 0);
   noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
   waittime(second div 4);
   noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
   waittime(second div 4);
   { turn off both }
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   noteoff(dport, 0, 1, note_d+octave_6, i32max);
   { now repeat with legato on }
   legato(dport, 0, 1, 1);
   noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
   waittime(second div 4);
   noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
   waittime(second div 4);
   { turn off both }
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   noteoff(dport, 0, 1, note_d+octave_6, i32max);
   legato(dport, 0, 1, 0); { reset normal }
   writeln('Complete');
   waitret;

   writeln('Set legato on organ, first normal, then legato');
   waitret;
   instchange(dport, 0, 1, inst_drawbar_organ);
   legato(dport, 0, 1, 0);
   noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
   waittime(second div 4);
   noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
   waittime(second div 4);
   { turn off both }
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   noteoff(dport, 0, 1, note_d+octave_6, i32max);
   { now repeat with legato on }
   legato(dport, 0, 1, 1);
   noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
   waittime(second div 4);
   noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
   waittime(second div 4);
   { turn off both }
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   noteoff(dport, 0, 1, note_d+octave_6, i32max);
   legato(dport, 0, 1, 0); { reset normal }
   writeln('Complete');
   waitret;

   { set portamento }
   writeln('Set portamento on piano, first normal, then portamento, through');
   writeln('various portamento times');
   waitret;
   instchange(dport, 0, 1, inst_acoustic_grand);
   for i := 0 to 9 do begin

      writeln('Portamento time: ', i*(i32max div 10):1);
      porttime(dport, 0, 1, i*(i32max div 10));
      portamento(dport, 0, 1, 0);
      noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
      waittime(second div 4);
      noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
      waittime(second div 4);
      { turn off both }
      noteoff(dport, 0, 1, note_c+octave_6, i32max);
      noteoff(dport, 0, 1, note_d+octave_6, i32max);
      { now repeat with portamento on }
      portamento(dport, 0, 1, 1);
      noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
      waittime(second div 4);
      noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
      waittime(second div 4);
      { turn off both }
      noteoff(dport, 0, 1, note_c+octave_6, i32max);
      noteoff(dport, 0, 1, note_d+octave_6, i32max)

   end;
   portamento(dport, 0, 1, 0); { reset normal }
   writeln('Complete');
   waitret;

   writeln('Set portamento on organ, first normal, then portamento');
   waitret;
   instchange(dport, 0, 1, inst_drawbar_organ);
   for i := 0 to 9 do begin

      writeln('Portamento time: ', i*(i32max div 10):1);
      portamento(dport, 0, 1, 0);
      noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
      waittime(second div 4);
      noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
      waittime(second div 4);
      { turn off both }
      noteoff(dport, 0, 1, note_c+octave_6, i32max);
      noteoff(dport, 0, 1, note_d+octave_6, i32max);
      { now repeat with portamento on }
      portamento(dport, 0, 1, 1);
      noteon(dport, 0, 1, note_c+octave_6, i32max); { play middle C }
      waittime(second div 4);
      noteon(dport, 0, 1, note_d+octave_6, i32max); { play D }
      waittime(second div 4);
      { turn off both }
      noteoff(dport, 0, 1, note_c+octave_6, i32max);
      noteoff(dport, 0, 1, note_d+octave_6, i32max)

   end;
   portamento(dport, 0, 1, 0); { reset normal }
   writeln('Complete');
   waitret;

   writeln('Channel volume test. Play note continuously while advancing volume');
   instchange(dport, 0, 1, inst_drawbar_organ);
   noteon(dport, 0, 1, note_c+octave_6, i32max);
   { advance volume sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Volume: ', i*(i32max div 20):1);
      volsynthchan(dport, 0, 1, i*(i32max div 20));
      waittime(second div 4)

   end;
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   { reset channel vol to midline }
   volsynthchan(dport, 0, 1, i32max div 2);
   writeln('Complete');
   waitret;

   writeln('Channel balance test. Play note continuously while changing');
   writeln('from to right');
   instchange(dport, 0, 1, inst_drawbar_organ);
   noteon(dport, 0, 1, note_c+octave_6, i32max);
   { advance volume sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Balance: ', -i32max+(i*((i32max div 20)*2)):1);
      balance(dport, 0, 1, -i32max+(i*((i32max div 20)*2)));
      waittime(second div 4)

   end;
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   { reset channel balance to midline }
   balance(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel vibrato test. Play note continuously while advancing vibrato');
   instchange(dport, 0, 1, inst_drawbar_organ);
   noteon(dport, 0, 1, note_c+octave_6, i32max);
   { advance vibrato sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Vibrato: ', i*(i32max div 20):1);
      vibrato(dport, 0, 1, i*(i32max div 20));
      waittime(second)

   end;
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   { reset channel vibrato to midline }
   vibrato(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel pan test. Play note continuously while changing');
   writeln('pan from to right');
   instchange(dport, 0, 1, inst_drawbar_organ);
   noteon(dport, 0, 1, note_c+octave_6, i32max);
   { advance pan sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Pan: ', -i32max+(i*((i32max div 20)*2)):1);
      pan(dport, 0, 1, -i32max+(i*((i32max div 20)*2)));
      waittime(second div 4)

   end;
   noteoff(dport, 0, 1, note_c+octave_6, i32max);
   { reset channel pan to midline }
   pan(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel timbre test. Play notes while advancing timbre');
   instchange(dport, 0, 1, inst_acoustic_grand);
   { advance timbre sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Timbre: ', i*(i32max div 20):1);
      timbre(dport, 0, 1, i*(i32max div 20));
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   { reset channel timbre }
   timbre(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel brightness test. Play notes while advancing brightness');
   instchange(dport, 0, 1, inst_acoustic_grand);
   { advance brightness sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Brightness: ', i*(i32max div 20):1);
      brightness(dport, 0, 1, i*(i32max div 20));
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   { reset channel brightness }
   brightness(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel reverb test. Play notes while advancing reverb');
   instchange(dport, 0, 1, inst_acoustic_grand);
   { advance reverb sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Reverb: ', i*(i32max div 20):1);
      reverb(dport, 0, 1, i*(i32max div 20));
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   { reset channel reverb }
   reverb(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel tremulo test. Play notes while advancing tremulo');
   instchange(dport, 0, 1, inst_acoustic_grand);

   { advance tremulo sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Tremulo: ', i*(i32max div 20):1);
      tremulo(dport, 0, 1, i*(i32max div 20));
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   { reset channel tremulo }
   tremulo(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel chorus test. Play notes while advancing chorus');
   instchange(dport, 0, 1, inst_acoustic_grand);

   { advance chorus sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Chorus: ', i*(i32max div 20):1);
      chorus(dport, 0, 1, i*(i32max div 20));
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   { reset channel chorus }
   chorus(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel celeste test. Play notes while advancing celeste');
   instchange(dport, 0, 1, inst_acoustic_grand);
   { advance celeste sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Celeste: ', i*(i32max div 20):1);
      celeste(dport, 0, 1, i*(i32max div 20));
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   { reset channel celeste }
   celeste(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   writeln('Channel phaser test. Play notes while advancing phaser');
   instchange(dport, 0, 1, inst_acoustic_grand);
   { advance phaser sets on channel while playing }
   for i := 0 to 19 do begin

      writeln('Phaser: ', i*(i32max div 20):1);
      phaser(dport, 0, 1, i*(i32max div 20));
      noteon(dport, 0, 1, note_c+octave_6, i32max);
      waittime(second div 4);
      noteoff(dport, 0, 1, note_c+octave_6, 0);
      waittime(second div 4)

   end;
   { reset channel phaser }
   phaser(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   { don't know about this test, it seems to limit the total pitch wheel range,
     which is not right }
   writeln('pitch wheel. Vary pitch wheel while playing continuously');
   instchange(dport, 0, 1, inst_lead_1_square);
   noteon(dport, 0, 1, note_c+octave_6, i32max);
   for j := 0 to 9 do begin

      writeln('Pitchrange: ', j*(i32max div 10):1);
      pitchrange(dport, 0, 1, j*(i32max div 10));
      for x := 0 to 9 do
         for i := 0 to 9 do begin

         writeln('Pitch: ', -i32max+(i*((i32max div 10)*2)):1);
         pitch(dport, 0, 1, -i32max+(i*((i32max div 10)*2)));
         waittime(second div 100)

      end

   end;
   noteoff(dport, 0, 1, note_c+octave_6, 0);
   pitch(dport, 0, 1, 0);
   writeln('Complete');
   waitret;

   99: { terminate }
   closesynthout(dport);
   writeln

end.
