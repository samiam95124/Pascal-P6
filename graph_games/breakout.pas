{******************************************************************************
*                                                                             *
*                                BREAKOUT GAME                                *
*                                                                             *
*                       COPYRIGHT (C) 2002 S. A. FRANCO                       *
*                                                                             *
* Plays breakout in graphical mode.                                           *
*                                                                             *
* This is the Pascaline port of the Ami breakout.c reference game.            *
*                                                                             *
******************************************************************************}

program breakout(input, output);

uses graphics,
     sound;

label 88, { start new game }
      99; { terminate }

const

   second     = 10000;          { one second }
   osec       = second div 8;   { 1/8 second }
   balmov     = 50;             { ball move timer }
   newbal     = second;         { wait for new ball time }
   ballclr    = blue;           { ball color }
   wallclr    = cyan;           { wall color }
   padclr     = green;          { paddle color }
   bouncetime = 250;            { time to play bounce note }
   wallnote   = note_d+octave_6; { note to play off wall }
   bricknote  = note_e+octave_7; { note to play off brick }
   failtime   = 1500;           { note to play on failure }
   failnote   = note_c+octave_4; { note to play on fail }
   brkrow     = 6;              { number of brick rows }
   brkcol     = 10;             { number of brick columns }
   i32max     = 2147483647;     { 32 bit INT_MAX }
   ftsign     = 3;              { AMI_FONT_SIGN }

type

   rectangle = record { rectangle }

      x1, y1, x2, y2: integer

   end;

var

   padx:    integer;   { paddle position x }
   bdx:     integer;   { ball direction x }
   bdy:     integer;   { ball direction y }
   baltim:  integer;   { ball start timer }
   er:      evtrec;    { event record }
   jchr:    integer;   { number of pixels to joystick movement }
   score:   integer;   { score }
   scrsiz:  integer;   { score size }
   scrchg:  boolean;   { score has changed }
   paddle:  rectangle; { paddle rectangle }
   ball, balsav: rectangle; { ball rectangle }
   wallt, walll, wallr, wallb: rectangle; { wall rectangles }
   bricks:  array [0..brkrow-1, 0..brkcol-1] of rectangle; { brick array }
   brki:    boolean;   { brick was intersected }
   fldbrk:  integer;   { bricks hit this field }
   bip:     integer;   { middle of ball intersection with paddle }
   wall:    integer;   { wall thickness }
   brkh:    integer;   { brick height }
   brkbrd:  integer;   { brick border }
   balls:   integer;   { ball size }
   hballs:  integer;   { half ball size }
   padh:    integer;   { height of paddle }
   pwdis:   integer;   { distance of paddle from bottom wall }
   padw:    integer;   { paddle width }
   hpadw:   integer;   { half paddle width }

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

   off := maxxg(output) div 2 - strsiz(output, s) div 2;
   writexy(off, y, s) { write out contents }

end;

{******************************************************************************

Translate color code

Translates a logical color to an RGB color. Returns the RGB color in three
variables.

******************************************************************************}

procedure log2rgb(c: color; var r, g, b: integer);

begin

   { translate color number }
   r := 0; { default to black }
   g := 0;
   b := 0;
   if c = white then begin r := i32max; g := i32max; b := i32max end
   else if c = red then begin r := i32max; g := 0; b := 0 end
   else if c = green then begin r := 0; g := i32max; b := 0 end
   else if c = blue then begin r := 0; g := 0; b := i32max end
   else if c = cyan then begin r := 0; g := i32max; b := i32max end
   else if c = yellow then begin r := i32max; g := i32max; b := 0 end
   else if c = magenta then begin r := i32max; g := 0; b := i32max end

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

Draw bordered rectangle

Draws a filled rectangle with border, in the given color.

******************************************************************************}

procedure dim(dv: real; var r, g, b: integer);

begin

   r := trunc(r*dv);
   g := trunc(g*dv);
   b := trunc(b*dv)

end;

procedure drwbrect(var r: rectangle; c: color);

var i:          integer;
    hr, hg, hb: integer; { rgb value of highlight }
    mr, mg, mb: integer; { rbg value of midlight }
    lr, lg, lb: integer; { rbg value of lowlight }

begin

   log2rgb(c, hr, hg, hb); { find actual color }
   mr := hr; { copy }
   mg := hg;
   mb := hb;
   lr := hr;
   lg := hg;
   lb := hb;
   dim(0.80, mr, mg, mb); { dim midlight to %75 }
   dim(0.60, lr, lg, lb); { dim lowlight to %50 }
   fcolorg(output, mr, mg, mb); { set brick body to midlight }
   frect(output, r.x1, r.y1, r.x2, r.y2); { draw brick }
   fcolorg(output, hr, hg, hb); { set hilight }
   frect(output, r.x1, r.y1, r.x1+brkbrd-1, r.y2); { border left }
   frect(output, r.x1, r.y1, r.x2, r.y1+brkbrd-1); { top }
   { set lowlight border color }
   fcolorg(output, lr, lg, lb);
   { border right }
   for i := 1 to brkbrd do
      frect(output, r.x2-i+1, r.y1+i-1, r.x2, r.y2);
   { border bottom }
   for i := 1 to brkbrd do
      frect(output, r.x1+i-1, r.y2-i+1, r.x2, r.y2)

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

function intsec(var r1, r2: rectangle): boolean;

begin

   { rationalize the rectangles }
   ratrect(r1);
   ratrect(r2);

   intsec := (r1.x2 >= r2.x1) and (r1.x1 <= r2.x2) and
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
   wrtcen(maxyg(output)-wall+1, 'BREAKOUT VS. 1.0')

end;

{******************************************************************************

Draw wall

Redraws the brick wall.

******************************************************************************}

procedure drwwall;

var r, c: integer; { brick array indexes }
    clr:  color;   { brick color }

begin

   clr := red; { set 1st pure color }
   for r := 0 to brkrow-1 do
      for c := 0 to brkcol-1 do begin

      drwbrect(bricks[r][c], clr);
      if clr < magenta then clr := succ(clr)
      else clr := red

   end

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

Set brick wall

Initializes the bricks in the wall coordinates.

******************************************************************************}

procedure setwall;

var r, c:   integer; { brick array indexes }
    brkw:   integer; { brick width }
    brkr:   integer; { brick remainder }
    brkoff: integer; { brick wall offset }
    co:     integer; { collumn offset }
    rd:     integer; { remainder distributor }

begin

   brkw := (maxxg(output)-2*wall) div brkcol; { find brick width }
   brkr := (maxxg(output)-2*wall) mod brkcol - 1; { find brick remainder }
   brkoff := maxyg(output) div 4; { find brick wall offset }
   for r := 0 to brkrow-1 do begin

      co := 0; { clear collumn offset }
      rd := brkr; { set remainder distributor }
      for c := 0 to brkcol-1 do begin

         setrct(bricks[r][c], 1+co+wall, 1+(r-1)*brkh+brkoff,
                              1+co+brkw-1+wall+ord(rd > 0),
                              1+(r-1)*brkh+brkh-1+brkoff);
         co := co+brkw+ord(rd > 0); { offset to next brick }
         if brkr > 0 then rd := rd-1 { reduce remainder }

      end

   end

end;

{******************************************************************************

Find brick intersection

Searches for a brick that intersects with the ball, and if found, erases the
brick and returns true. Note that if more than one brick intersects, they all
disappear.

******************************************************************************}

procedure interbrick;

var r, c: integer; { brick array indexes }

begin

   brki := false; { set no brick intersection }
   for r := 0 to brkrow-1 do
      for c := 0 to brkcol-1 do if intsec(ball, bricks[r][c]) then begin

      brki := true; { set intersected }
      drwrect(bricks[r][c], white); { erase from screen }
      clrrect(bricks[r][c]); { clear brick data }
      score := score+1; { count hits }
      scrchg := true; { set changed }
      fldbrk := fldbrk+1 { add to bricks this field }

   end

end;

{******************************************************************************

Main program

******************************************************************************}

begin

   opensynthout(synth_out); { open synthesizer }
   instchange(synth_out, 0, 1, inst_lead_1_square);
   starttimeout; { start sequencer running }
   jchr := i32max div ((maxxg(output)-2) div 2); { find basic joystick
                                                   increment }
   curvis(output, false); { remove drawing cursor }
   auto(output, false); { turn off scrolling }
   font(output, ftsign); { sign font }
   wall := maxyg(output) div 20; { set wall thickness }
   brkh := maxyg(output) div 25; { set brick thickness }
   brkbrd := brkh div 5; { set brick border }
   balls := maxyg(output) div 20; { set ball size }
   hballs := balls div 2; { set half ball size }
   padh := maxyg(output) div 22; { set paddle thickness }
   pwdis := padh div 4; { set distance of paddle to wall }
   padw := maxxg(output) div 8; { set paddle width }
   hpadw := padw div 2; { half paddle width }
   bold(output, true);
   fontsiz(output, wall-2); { font fits in the wall }
   binvis(output); { no background writes }
   timer(output, 1, balmov, true); { enable timer }

   88: { start new game }

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
   scrsiz := strsiz(output, 'SCORE 0000'); { set nominal size of score
                                             string }
   scrchg := true; { set score changed }
   drwscn; { draw game screen }
   score := 0; { clear score }
   baltim := newbal div balmov; { set starting ball time }
   repeat { game loop }

      setwall; { initialize bricks }
      drwwall; { redraw the wall }
      fldbrk := 0; { clear bricks hit this field }
      repeat { fields }

         if (ball.x1 = 0) and (baltim = 0) then begin

            { ball not on screen, and time to wait expired, send out ball }
            setrct(ball, wall+1, maxyg(output)-4*wall-balls,
                         wall+1+balls, maxyg(output)-4*wall);
            bdx := +(maxxg(output) div 300); { set direction of travel }
            bdy := -(maxyg(output) div 150);
            { draw the ball }
            fcolor(output, ballclr);
            drwrect(ball, ballclr);
            scrchg := true { set changed }

         end;
         if scrchg then begin { process score change }

            { erase score }
            fcolor(output, wallclr);
            frect(output, maxxg(output) div 2 - scrsiz div 2, 1,
                          maxxg(output) div 2 + scrsiz div 2, wall);
            { place updated score on screen }
            fcolor(output, black);
            cursorg(output, maxxg(output) div 2 - scrsiz div 2, 2);
            writeln('SCORE ', score:5);
            scrchg := false { reset score change flag }

         end;
         repeat event(input, er) { wait relevant events }
         until er.etype in [etterm, etleft, etright, etfun, ettim, etjoymov];
         if er.etype = etterm then goto 99; { game exits }
         if er.etype = etfun then goto 88; { restart game }
         { process paddle movements }
         if er.etype = etleft then padpos(padx-5) { move left }
         else if er.etype = etright then padpos(padx+5) { move right }
         else if er.etype = etjoymov then { move joystick }
            padpos(maxxg(output) div 2 + er.joypx div jchr)
         else if er.etype = ettim then begin { move timer }

            if er.timnum = 1 then begin { ball timer }

               if ball.x1 > 0 then begin { ball on screen }

                  balsav := ball; { save ball position }
                  offrect(ball, bdx, bdy); { move the ball }
                  { check off screen motions }
                  if intsec(ball, walll) or intsec(ball, wallr) then begin

                     { hit left or right wall }
                     ball := balsav; { restore }
                     bdx := -bdx; { change direction }
                     offrect(ball, bdx, bdy); { recalculate }
                     { start bounce note }
                     noteon(synth_out, 0, 1, wallnote, i32max);
                     noteoff(synth_out, curtimeout+bouncetime, 1, wallnote,
                             i32max)

                  end else if intsec(ball, wallt) then begin { hits top }

                     ball := balsav; { restore }
                     bdy := -bdy; { change direction }
                     offrect(ball, bdx, bdy); { recalculate }
                     { start bounce note }
                     noteon(synth_out, 0, 1, wallnote, i32max);
                     noteoff(synth_out, curtimeout+bouncetime, 1, wallnote,
                             i32max)

                  end else if intsec(ball, paddle) then begin

                     ball := balsav; { restore }
                     { find which 5th of the paddle was struck }
                     bip := (ball.x1+hballs-paddle.x1) div (padw div 5);
                     { clip to 5th }
                     if bip < 0 then bip := 0;
                     if bip > 5 then bip := 5;
                     case bip of

                        0: bdx := -2; { left hard }
                        1: bdx := -1; { soft soft }
                        2: ;          { center reflects }
                        3: bdx := +1; { right soft }
                        4: bdx := +2; { right hard }
                        5: bdx := +2  { right hard }

                     end;
                     bdy := -bdy; { reflect y }
                     offrect(ball, bdx, bdy); { recalculate }
                     { if the ball is still below the paddle plane, move
                       it up until it is not }
                     if ball.y2 >= paddle.y1 then
                        offrect(ball, 0, -(ball.y2-paddle.y1+1));
                     { start bounce note }
                     noteon(synth_out, 0, 1, wallnote, i32max);
                     noteoff(synth_out, curtimeout+bouncetime, 1, wallnote,
                             i32max)

                  end else begin { check brick hits }

                     interbrick; { check brick intersection }
                     if brki then begin { there was a brick hit }

                        ball := balsav; { restore }
                        bdy := -bdy; { change direction }
                        offrect(ball, bdx, bdy); { recalculate }
                        { start bounce note }
                        noteon(synth_out, 0, 1, bricknote, i32max);
                        noteoff(synth_out, curtimeout+bouncetime, 1,
                                bricknote, i32max)

                     end

                  end;
                  if intsec(ball, wallb) then begin { ball out of bounds }

                     drwrect(balsav, white);
                     clrrect(ball); { set ball not on screen }
                     { start time on new ball wait }
                     baltim := newbal div balmov;
                     { start fail note }
                     noteon(synth_out, 0, 1, failnote, i32max);
                     noteoff(synth_out, curtimeout+failtime, 1, failnote,
                             i32max)

                  end else begin { ball in play }

                     { erase only the leftover part of the old ball }
                     fcolor(output, white);
                     if bdx < 0 then { ball move left }
                        frect(output, ball.x2+1, balsav.y1,
                                      balsav.x2, balsav.y2)
                     else { move move right }
                        frect(output, balsav.x1, balsav.y1,
                                      ball.x1-1, balsav.y2);
                     if bdy < 0 then { ball move up }
                        frect(output, balsav.x1, ball.y2+1,
                                      balsav.x2, balsav.y2)
                     else { move move down }
                        frect(output, balsav.x1, balsav.y1,
                                      balsav.x2, ball.y1-1);
                     drwrect(ball, ballclr) { redraw the ball }

                  end

               end;
               { if the ball timer is running, decrement it }
               if baltim > 0 then baltim := baltim-1

            end

         end

      until fldbrk = brkrow*brkcol; { until bricks are cleared }
      { play the field clear fanfare }
      noteon(synth_out,  0,                   1, note_c+octave_6, i32max);
      noteoff(synth_out, curtimeout+osec*2,   1, note_c+octave_6, i32max);
      noteon(synth_out,  curtimeout+osec*3,   1, note_d+octave_6, i32max);
      noteoff(synth_out, curtimeout+osec*4,   1, note_d+octave_6, i32max);
      noteon(synth_out,  curtimeout+osec*5,   1, note_e+octave_6, i32max);
      noteoff(synth_out, curtimeout+osec*6,   1, note_e+octave_6, i32max);
      noteon(synth_out,  curtimeout+osec*7,   1, note_f+octave_6, i32max);
      noteoff(synth_out, curtimeout+osec*8,   1, note_f+octave_6, i32max);
      noteon(synth_out,  curtimeout+osec*9,   1, note_e+octave_6, i32max);
      noteoff(synth_out, curtimeout+osec*10,  1, note_e+octave_6, i32max);
      noteon(synth_out,  curtimeout+osec*11,  1, note_d+octave_6, i32max);
      noteoff(synth_out, curtimeout+osec*13,  1, note_d+octave_6, i32max);
      baltim := (osec*13+newbal) div balmov; { wait fanfare }
      drwrect(ball, white); { clear ball }
      clrrect(ball) { set ball not on screen }

   until false; { forever }

   99: ; { exit game }

   closesynthout(synth_out) { close synthesizer }

end.
