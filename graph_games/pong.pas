{******************************************************************************
*                                                                             *
*                                 PONG GAME                                   *
*                                                                             *
*                       COPYRIGHT (C) 1997 S. A. FRANCO                       *
*                                                                             *
* Plays pong in graphical mode.                                               *
*                                                                             *
* This is the Pascaline equivalent of the Ami graph_games/pong.c reference   *
* game.                                                                       *
*                                                                             *
******************************************************************************}

program pong(input, output);

uses graphics,
     sound;

label 1,  { newgame }
      99; { endgame }

const

   balmov   = 50;               { ball move timer }
   newbal   = 100*2;            { wait for new ball time, 1 sec (in ball units) }
   ballclr  = blue;             { ball color }
   wallclr  = cyan;             { wall color }
   padclr   = green;            { paddle color }
   bncenote = 5;                { time to play bounce note }
   wallnote = note_d+octave_6;  { note to play off wall }
   failtime = 30;               { note to play on failure }
   failnote = note_c+octave_4;  { note to play on fail }
   i32max   = 2147483647;       { 32 bit INT_MAX, joystick/velocity scale }
   ftsign   = 3;                { AMI_FONT_SIGN }

type

   rectangle = record { rectangle }

      x1, y1, x2, y2: integer

   end;

var

   wall:         integer;   { wall thickness }
   balls:        integer;   { ball size }
   hballs:       integer;   { half ball size }
   padh:         integer;   { height of paddle }
   pwdis:        integer;   { distance of paddle from bottom wall }
   padw:         integer;   { paddle width }
   hpadw:        integer;   { half paddle width }
   padx:         integer;   { paddle position x }
   bdx:          integer;   { ball direction x }
   bdy:          integer;   { ball direction y }
   baltim:       integer;   { ball start timer }
   er:           evtrec;    { event record }
   jchr:         integer;   { number of pixels to joystick movement }
   score:        integer;   { score }
   scrsiz:       integer;   { score size }
   scrchg:       boolean;   { score has changed }
   nottim:       integer;   { bounce note timer }
   failtimer:    integer;   { fail note timer }
   paddle:       rectangle; { paddle rectangle }
   ball, balsav: rectangle; { ball rectangle }
   wallt, walll, wallr, wallb: rectangle; { wall rectangles }

{******************************************************************************

Write string to screen

Writes a string to the indicated position on the screen.

******************************************************************************}

procedure writexy(x, y: integer;  { position to write to }
                  view s: string); { string to write }

begin

   cursorg(output, x, y); { position cursor }
   writeln(s) { output string }

end;

{******************************************************************************

Write centered string

Writes a string that is centered on the line given. Returns the
starting position of the string.

******************************************************************************}

procedure wrtcen(y: integer;      { y position of string }
                 view s: string); { string to write }

var off: integer; { string offset }

begin

   off := maxxg(output) div 2-strsiz(output, s) div 2;
   writexy(off, y, s) { write out contents }

end;

{******************************************************************************

Draw rectangle

Draws a filled rectangle, in the given color.

******************************************************************************}

procedure drwrect(var r: rectangle; c: color);

begin

   fcolor(output, c); { set color }
   frect(output, r.x1, r.y1, r.x2, r.y2)

end;

{******************************************************************************

Offset rectangle

Offsets a rectangle by an x and y difference.

******************************************************************************}

procedure offrect(var r: rectangle; x, y: integer);

begin

   r.x1 := r.x1+x;
   r.y1 := r.y1+y;
   r.x2 := r.x2+x;
   r.y2 := r.y2+y

end;

{******************************************************************************

Rationalize a rectangle

Rationalizes a rectangle, that is, arranges the points so that the 1st point
is lower in x and y than the second.

******************************************************************************}

procedure ratrect(var r: rectangle);

var t: integer; { swap temp }

begin

   if r.x1 > r.x2 then begin { swap x }

      t := r.x1;
      r.x1 := r.x2;
      r.x2 := t

   end;
   if r.y1 > r.y2 then begin { swap y }

      t := r.y1;
      r.y1 := r.y2;
      r.y2 := t

   end

end;

{******************************************************************************

Find intersection of rectangles

Checks if two rectangles intersect. Returns true if so.

******************************************************************************}

function intrect(var r1, r2: rectangle): boolean;

begin

   { rationalize the rectangles }
   ratrect(r1);
   ratrect(r2);

   intrect := (r1.x2 >= r2.x1) and (r1.x1 <= r2.x2) and
              (r1.y2 >= r2.y1) and (r1.y1 <= r2.y2)

end;

{******************************************************************************

Set rectangle

Sets the rectangle to the given values.

******************************************************************************}

procedure setrct(var r: rectangle; x1, y1, x2, y2: integer);

begin

   r.x1 := x1;
   r.y1 := y1;
   r.x2 := x2;
   r.y2 := y2

end;

{******************************************************************************

Clear rectangle

Clear rectangle points to zero. Usually used to flag the rectangle invalid.

******************************************************************************}

procedure clrrect(var r: rectangle);

begin

   r.x1 := 0;
   r.y1 := 0;
   r.x2 := 0;
   r.y2 := 0

end;

{******************************************************************************

Draw screen

Draws a new screen, with borders.

******************************************************************************}

procedure drwscn;

begin

   writeln; { clear screen }
   { draw walls }
   drwrect(wallt, wallclr); { top }
   drwrect(walll, wallclr); { left }
   drwrect(wallr, wallclr); { right }
   drwrect(wallb, wallclr); { bottom }
   fcolor(output, black);
   wrtcen(maxyg(output)-wall+1, 'PONG VS. 1.0')

end;

{******************************************************************************

Set new paddle position

Places the paddle at the given position.

******************************************************************************}

procedure padpos(x: integer);

begin

   if x-hpadw <= walll.x2 then x := walll.x2+hpadw+1; { clip to ends }
   if x+hpadw >= wallr.x1 then x := wallr.x1-hpadw-1;
   { erase old location }
   fcolor(output, white);
   frect(output, padx-hpadw, maxyg(output)-wall-padh-pwdis,
                 padx+hpadw, maxyg(output)-wall-pwdis);
   padx := x; { set new location }
   setrct(paddle, x-hpadw, maxyg(output)-wall-padh-pwdis,
                  x+hpadw, maxyg(output)-wall-pwdis);
   drwrect(paddle, padclr) { draw paddle }

end;

{******************************************************************************

Main program

******************************************************************************}

begin

   nottim := 0; { clear bounce note timer }
   failtimer := 0; { clear fail timer }
   opensynthout(synth_out); { open synthesizer }
   instchange(synth_out, 0, 1, inst_lead_1_square);
   { find basic joystick increment }
   jchr := i32max div ((maxxg(output)-2) div 2);
   curvis(output, 0); { remove drawing cursor }
   auto(output, 0); { turn off scrolling }
   font(output, ftsign); { sign font }
   wall := maxyg(output) div 20; { set wall thickness }
   balls := maxyg(output) div 20; { set ball size }
   hballs := balls div 2; { set half ball size }
   padh := maxyg(output) div 22; { set paddle thickness }
   pwdis := padh div 4; { set distance of paddle to wall }
   padw := maxxg(output) div 8; { set paddle width }
   hpadw := padw div 2; { half paddle width }
   bold(output, 1);
   fontsiz(output, wall-2); { font fits in the wall }
   binvis(output); { no background writes }
   timer(output, 1, balmov, 1); { enable timer }

   1: { newgame: start new game }

   padx := maxxg(output) div 2; { find initial paddle position }
   padpos(padx); { display paddle }
   clrrect(ball); { set ball not on screen }
   baltim := 0; { set ball ready to start }
   { set up wall rectangles }
   setrct(wallt, 1, 1, maxxg(output), wall); { top }
   setrct(walll, 1, 1, wall, maxyg(output)); { left }
   { right }
   setrct(wallr, maxxg(output)-wall, 1, maxxg(output), maxyg(output));
   { bottom }
   setrct(wallb, 1, maxyg(output)-wall, maxxg(output), maxyg(output));
   scrsiz := strsiz(output, 'SCORE 0000'); { set nominal size of score string }
   scrchg := true; { set score changed }
   drwscn; { draw game screen }
   repeat { game loop }

      if (ball.x1 = 0) and (baltim = 0) then begin

         { ball not on screen, and time to wait expired, send out ball }
         setrct(ball, wall+1, maxyg(output)-4*wall-balls,
                      wall+1+balls, maxyg(output)-4*wall);
         bdx := +maxxg(output) div 300; { set direction of travel }
         bdy := -(maxyg(output) div 150);
         { draw the ball }
         fcolor(output, ballclr);
         drwrect(ball, ballclr);
         score := 0; { clear score }
         scrchg := true { set changed }

      end;
      if scrchg then begin { process score change }

         { erase score }
         fcolor(output, wallclr);
         frect(output, maxxg(output) div 2-scrsiz div 2, 1,
                       maxxg(output) div 2+scrsiz div 2, wall);
         { place updated score on screen }
         fcolor(output, black);
         cursorg(output, maxxg(output) div 2-scrsiz div 2, 2);
         writeln('SCORE ', score:5);
         scrchg := false { reset score change flag }

      end;
      repeat event(input, er) { wait relevant events }
      until (er.etype = etterm) or (er.etype = etleft) or
            (er.etype = etright) or (er.etype = etfun) or
            (er.etype = ettim) or (er.etype = etjoymov);
      if er.etype = etterm then goto 99; { game exits }
      if er.etype = etfun then goto 1; { restart game }
      { process paddle movements }
      if er.etype = etleft then padpos(padx-5) { move left }
      else if er.etype = etright then padpos(padx+5) { move right }
      else if er.etype = etjoymov then { move joystick }
         padpos(maxxg(output) div 2+er.joypx div jchr)
      else if er.etype = ettim then begin { move timer }

         if er.timnum = 1 then begin { ball timer }

            { if the note timer is running, decrement it }
            if nottim > 0 then begin

               nottim := nottim-1; { decrement }
               if nottim = 0 then { times up, turn note off }
                  noteoff(synth_out, 0, 1, wallnote, i32max)

            end;
            { if the fail note timer is running, decrement it }
            if failtimer > 0 then begin

               failtimer := failtimer-1; { decrement }
               if failtimer = 0 then { times up, turn note off }
                  noteoff(synth_out, 0, 1, failnote, i32max)

            end;
            if ball.x1 > 0 then begin { ball on screen }

               balsav := ball; { save ball position }
               offrect(ball, bdx, bdy); { move the ball }
               { check off screen motions }
               if intrect(ball, walll) or intrect(ball, wallr) then begin

                  { hit left or right wall }
                  ball := balsav; { restore }
                  bdx := -bdx; { change direction }
                  offrect(ball, bdx, bdy); { recalculate }
                  { start bounce note }
                  noteon(synth_out, 0, 1, wallnote, i32max);
                  nottim := bncenote { set timer }

               end else if intrect(ball, wallt) then begin { hits top }

                  ball := balsav; { restore }
                  bdy := -bdy; { change direction }
                  offrect(ball, bdx, bdy); { recalculate }
                  { start bounce note }
                  noteon(synth_out, 0, 1, wallnote, i32max);
                  nottim := bncenote { set timer }

               end else if intrect(ball, paddle) then begin

                  { hits paddle. now the ball can hit left, center or right.
                    left goes left, right goes right, and center reflects }
                  ball := balsav; { restore }
                  if ball.x1+hballs < padx-padh div 2 then bdx := -1 { left }
                  else if ball.x1+hballs > padx+padh div 2 then bdx := +1 { right }
                  else if bdx < 0 then bdx := -1 else bdx := +1; { center }
                  bdy := -bdy; { reflect y }
                  offrect(ball, bdx, bdy); { recalculate }
                  score := score+1; { count hits }
                  scrchg := true; { set changed }
                  { start bounce note }
                  noteon(synth_out, 0, 1, wallnote, i32max);
                  nottim := bncenote { set timer }

               end;
               if intrect(ball, wallb) then begin { ball out of bounds }

                  drwrect(balsav, white);
                  clrrect(ball); { set ball not on screen }
                  baltim := newbal; { start time on new ball wait }
                  { start fail note }
                  noteon(synth_out, 0, 1, failnote, i32max);
                  failtimer := failtime { set timer }

               end else begin { ball in play }

                  { erase only the leftover part of the old ball }
                  fcolor(output, white);
                  if bdx < 0 then { ball move left }
                     frect(output, ball.x2+1, balsav.y1,
                                   balsav.x2, balsav.y2)
                  else { ball move right }
                     frect(output, balsav.x1, balsav.y1,
                                   ball.x1-1, balsav.y2);
                  if bdy < 0 then { ball move up }
                     frect(output, balsav.x1, ball.y2+1,
                                   balsav.x2, balsav.y2)
                  else { ball move down }
                     frect(output, balsav.x1, balsav.y1,
                                   balsav.x2, ball.y1-1);
                  drwrect(ball, ballclr) { redraw the ball }

               end

            end;
            { if the ball timer is running, decrement it }
            if baltim > 0 then baltim := baltim-1

         end

      end

   until false; { forever }

   99: { endgame: exit game }

   closesynthout(synth_out) { close synthesizer }

end.
