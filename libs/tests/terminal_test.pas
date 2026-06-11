{******************************************************************************
*                                                                             *
*                           SCREEN TEST PROGRAM                               *
*                                                                             *
*                    Copyright (C) 2024 Scott A. Franco                       *
*                                                                             *
* This program performs a reasonably complete test of common features in the  *
* terminal level standard. It is the Pascaline equivalent of the Ami          *
* terminal_test.c reference test.                                             *
*                                                                             *
* Tests performed:                                                            *
*                                                                             *
* 1.  Title set         2.  Screen parameters  3.  Timers                     *
* 4.  Cursor visible    5.  Text entry         6.  Text entry with offset     *
* 7.  Last line         8.  Cursor movements   9.  Scroll cursor              *
* 10. Row id            11. Column id          12. Fill                       *
* 13. Sidewinder        14. Bouncing ball      15. Attributes and colors      *
* 16. RGB colors        17. Scrolling          18. Tab                        *
* 19. Offscreen write   20. Offscreen scroll   21. Buffer switching           *
* 22. Writethrough      23. Buffer follow      24. Focus and hover            *
* 25. Threading         26. Joystick           27. Mouse                      *
* 28. Event vector      29. Character speed     30. Scrolling speed            *
* 31. Buffer flip speed                                                       *
*                                                                             *
* The terminate event (etterm) is handled by polling in the wait routines,    *
* which perform a non-local goto to the terminate label.                      *
*                                                                             *
******************************************************************************}

program terminal_test(input, output);

uses terminal,
     services;

label 99; { terminate }

const

   second = 10000;          { one second in 100 microsecond ticks }
   i32max = 2147483647;     { 32 bit INT_MAX, the RGB ratio used by terminal }

type

   bench = (bncharw, bnscroll, bnbuffer); { benchmark types }
   benchrec = record iter, time: integer end;

var

   x, y, lx, ly, tx, ty, dx, dy: integer;
   c:                            char;
   top, bottom, lside, rside:    integer; { borders }
   direction:                    (dup, ddown, dleft, dright); { writing direction }
   count, t1, t2, minlen:        integer;
   er:                           evtrec; { event record }
   i, j, b, tc, cnt:             integer;
   clk:                          integer;
   tf:                           text; { test file }
   eventflag1, eventflag2:       boolean;
   oeh1, oeh2:                   integer; { old event handlers }
   line:                         packed array [1..250] of char;
   bi:                           bench;
   benchtab:                     array [bench] of benchrec;

fixed

   { color table, packed 24 bit RGB values }
   colormap: array [1..117] of integer = array
      $330000, $331900, $333300, $193300, $003300, $003319, $003333, $001933,
      $000033, $190033, $330033, $330019, $000000, $660000, $663300, $666600,
      $336600, $006600, $006633, $006666, $003366, $000066, $330066, $660066,
      $660033, $202020, $990000, $994c00, $999900, $4c9900, $009900, $00994c,
      $009999, $004c99, $000099, $4c0099, $990099, $99004c, $404040, $cc0000,
      $cc6600, $cccc00, $66cc00, $00cc00, $00cc66, $00cccc, $0066cc, $0000cc,
      $6600cc, $cc00cc, $cc0066, $606060, $ff0000, $ff8000, $ffff00, $80ff00,
      $00ff00, $00ff80, $00ffff, $0080ff, $0000ff, $7f00ff, $ff00ff, $ff007f,
      $808080, $ff3333, $ff9933, $ffff33, $99ff33, $33ff33, $33ff99, $33ffff,
      $3399ff, $3333ff, $9933ff, $ff33ff, $ff3399, $a0a0a0, $ff6666, $ffb266,
      $ffff66, $b2ff66, $66ff66, $66ffb2, $66ffff, $66b2ff, $6666ff, $b266ff,
      $ff66ff, $ff66b2, $c0c0c0, $ff9999, $ffcc99, $ffff99, $ccff99, $99ff99,
      $99ffcc, $99ffff, $99ccff, $9999ff, $cc99ff, $ff99ff, $ff99cc, $e0e0e0,
      $ffcccc, $ffe5cc, $ffffcc, $e5ffcc, $ccffcc, $ccffe5, $ccffff, $cce5ff,
      $ccccff, $e5ccff, $ffccff, $ffcce5, $ffffff
   end;

{ draw box }

procedure box(sx, sy, ex, ey: integer; c: char);

var x, y: integer;

begin

   cursor(output, sx, sy); { top }
   for x := sx to ex do write(c);
   cursor(output, sx, ey); { bottom }
   for x := sx to ex do write(c);
   for y := sy to ey do begin cursor(output, sx, y); write(c) end; { left }
   for y := sy to ey do begin cursor(output, ex, y); write(c) end  { right }

end;

{ wait time in 100 microseconds }

procedure wait(t: integer);

begin

   timer(output, 1, t, false);
   repeat event(input, er) until (er.etype = ettim) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{ wait return to be pressed, or handle terminate }

procedure waitnext;

begin

   repeat event(input, er) until (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{ fill screen with row order data (one digit per row) }

procedure fillrow;

var x, y: integer; c: char;

begin

   c := '1';
   for y := 1 to maxy(output) do begin

      cursor(output, 1, y);
      for x := 1 to maxx(output) do write(c);
      if c <> '9' then c := succ(c) else c := '0'

   end

end;

{ fill screen with column order data (digits increment along each row) }

procedure fillcol;

var x, y: integer; c: char;

begin

   for y := 1 to maxy(output) do begin

      cursor(output, 1, y);
      c := '1';
      for x := 1 to maxx(output) do begin

         write(c);
         if c <> '9' then c := succ(c) else c := '0'

      end

   end

end;

{ timer resolution test }

procedure timetest;

var i, t, et, total, max, min: integer;
    er: evtrec;

begin

   writeln('Timer test, measuring minimum timer resolution, 100 samples');
   writeln;
   max := 0;
   min := maxint;
   total := 0;
   for i := 1 to 100 do begin

      t := clock;
      timer(output, 1, 1, false);
      repeat write('*'); event(input, er) until er.etype = ettim;
      et := elapsed(t);
      total := total+elapsed(t);
      if et > max then max := et;
      if et < min then min := et

   end;
   writeln;
   writeln;
   writeln('Average time was: ', total div 100:1, '00 Microseconds');
   writeln('Minimum time was: ', min:1, '00 Microseconds');
   writeln('Maximum time was: ', max:1, '00 Microseconds');
   write('This timer supports frame rates up to ', 10000 div (total div 100):1);
   writeln(' frames per second');
   t := clock;
   timer(output, 1, 10000, false);
   repeat event(input, er) until er.etype = ettim;
   writeln('1 second time, was: ', elapsed(t):1, '00 Microseconds');
   writeln;
   writeln('30 seconds of 1 second ticks:');
   writeln;
   for i := 1 to 30 do begin

      timer(output, 1, 10000, false);
      repeat event(input, er) until (er.etype = ettim) or (er.etype = etterm);
      if er.etype = etterm then goto 99;
      write('.')

   end

end;

{ framing timer test }

procedure frametest;

var i, t, et, total, max, min: integer;
    er: evtrec;

begin

   writeln('Framing timer test, measuring 10 occurances of the framing timer');
   writeln;
   frametimer(output, true);
   max := 0;
   min := maxint;
   total := 0;
   for i := 1 to 10 do begin

      t := clock;
      repeat write('.'); event(input, er) until er.etype = etframe;
      et := elapsed(t);
      total := total+elapsed(t);
      if et > max then max := et;
      if et < min then min := et

   end;
   frametimer(output, false);
   writeln;
   writeln;
   writeln('Average time was: ', total div 10:1, '00 Microseconds');
   writeln('Minimum time was: ', min:1, '00 Microseconds');
   writeln('Maximum time was: ', max:1, '00 Microseconds')

end;

{ plot joystick on screen }

procedure plotjoy(line, joy: integer);

var i, x: integer;
    r:    real;

begin

   cursor(output, 1, line);
   for i := 1 to maxx(output) do write(' '); { clear line }
   if joy < 0 then begin { plot left }

      r := abs(joy);
      x := (maxx(output) div 2)-round(r*(maxx(output) div 2)/i32max);
      cursor(output, x, line);
      while x <= maxx(output) div 2 do begin write('*'); x := x+1 end

   end else begin { plot right }

      r := joy;
      x := round(r*(maxx(output) div 2)/i32max+(maxx(output) div 2));
      i := maxx(output) div 2;
      cursor(output, i, line);
      while i <= x do begin write('*'); i := i+1 end

   end

end;

{ print centered string }

procedure prtcen(y: integer; view s: string);

begin

   cursor(output, (maxx(output) div 2)-(max(s) div 2), y);
   write(s)

end;

{ print center banner string }

procedure prtban(view s: string);

var i: integer;

begin

   cursor(output, (maxx(output) div 2)-(max(s) div 2)-1, maxy(output) div 2-1);
   for i := 1 to max(s)+2 do write(' ');
   cursor(output, (maxx(output) div 2)-(max(s) div 2)-1, maxy(output) div 2);
   write(' ');
   prtcen(maxy(output) div 2, s);
   write(' ');
   cursor(output, (maxx(output) div 2)-(max(s) div 2)-1, maxy(output) div 2+1);
   for i := 1 to max(s)+2 do write(' ')

end;

{ set foreground color from a packed 24 bit RGB value }

procedure fcolorp(c: integer);

begin

   fcolorc(output, i32max div 256 * (c div 65536 mod 256),
                   i32max div 256 * (c div 256 mod 256),
                   i32max div 256 * (c mod 256))

end;

{ set background color from a packed 24 bit RGB value }

procedure bcolorp(c: integer);

begin

   bcolorc(output, i32max div 256 * (c div 65536 mod 256),
                   i32max div 256 * (c div 256 mod 256),
                   i32max div 256 * (c mod 256))

end;

{ event handler vector 1, installed via eventover }

procedure event_vector_1(var er: evtrec);

begin

   if er.etype <> etframe then er.handled := false;
   eventflag1 := true

end;

{ event handler vector 2, installed via eventsover }

procedure event_vector_2(var er: evtrec);

begin

   if er.etype <> etframe then er.handled := false;
   eventflag2 := true

end;

begin

   select(output, 2, 2); { move off the display buffer }
   { set black on white text }
   fcolor(output, black);
   bcolor(output, white);
   page;
   curvis(output, false);
   prtban('Terminal mode screen test vs. 1.0');
   prtcen(maxy(output), 'Press return to continue');
   waitnext;

   { *********************** Title set test ********************* }

   page;
   title(output, 'Terminal test');
   writeln('Terminal window title set test.');
   writeln;
   writeln('See if the title of the terminal window above has changed.');
   writeln;
   writeln('Note that this will do nothing if the terminal is not windowed.');
   writeln('Note also that changing the terminal title may not be implemented.');
   prtcen(maxy(output), 'Press return to continue');
   waitnext;

   { *********************** Display screen parameters ********************* }

   page; { clear screen }
   writeln('Screen size: x -> ', maxx(output):1, ' y -> ', maxy(output):1);
   writeln;
   writeln('Number of joysticks: ', joystick(output):1);
   for i := 1 to joystick(output) do begin

      writeln;
      writeln('Number of axes on joystick: ', i:1, ' is: ', joyaxis(output, i):1);
      writeln('Number of buttons on joystick: ', i:1, ' is: ',
              joybutton(output, i):1)

   end;
   writeln;
   writeln('Number of mice: ', mouse(output):1);
   for i := 1 to mouse(output) do begin

      writeln;
      writeln('Number of buttons on mouse: ', i:1, ' is: ', mousebutton(output, i):1)

   end;
   prtcen(maxy(output), 'Press return to continue');
   waitnext;

   { ***************************** Timers test **************************** }

   page;
   timetest;
   writeln;
   frametest;
   prtcen(maxy(output), 'Press return to continue');
   waitnext;

   { ********************* Cursor visible/invisible test ****************** }

   page;
   curvis(output, true);
   write('Cursor should be [on ], press return ->');
   waitnext;
   curvis(output, false);
   write('\crCursor should be [off], press return ->');
   waitnext;
   curvis(output, true);
   write('\crCursor should be [on ], press return ->');
   waitnext;
   write('\cr                                       ');
   curvis(output, false);
   writeln;
   writeln;
   prtcen(maxy(output), 'Press return to continue');
   waitnext;

   { *********************** Console standard text entry ****************** }

   page;
   curvis(output, true);
   writeln('Standard input line enter test');
   writeln;
   writeln('Enter text below. The line editor may have common line edit features');
   writeln('installed, such as back up cursor, delete backwards/forwards, start');
   writeln('and end of line, etc. Read the local system manual and try them.');
   writeln;
   i := 0;
   while not eoln and (i < 250) do begin i := i+1; read(line[i]) end;
   readln;
   writeln;
   writeln('You typed:');
   writeln;
   for j := 1 to i do write(line[j]);
   writeln;
   prtcen(maxy(output), 'Press return to continue');
   waitnext;

   { **************** Console standard text entry with offset ************* }

   page;
   curvis(output, true);
   writeln('Standard input line enter with offset test');
   writeln;
   writeln('Enter text below. The line editor may have common line edit features');
   writeln('installed, such as back up cursor, delete backwards/forwards, start');
   writeln('and end of line, etc. Read the local system manual and try them.');
   writeln;
   write('===========>');
   i := 0;
   while not eoln and (i < 250) do begin i := i+1; read(line[i]) end;
   readln;
   writeln;
   writeln('You typed:');
   writeln;
   for j := 1 to i do write(line[j]);
   writeln;
   prtcen(maxy(output), 'Press return to continue');
   waitnext;

   { ************************* Test last line problem ************************ }

   page;
   curvis(output, false); { remove cursor }
   auto(output, false); { turn off auto scroll }
   prtcen(1, 'Last line blank out test');
   cursor(output, 1, 3);
   writeln('If this terminal is not capable of showing the last character on');
   writeln('the last line, the "*" character pointed to by the arrow below');
   writeln('will not appear (probally blank). This should be noted for each');
   writeln('of the following test patterns.');
   cursor(output, 1, maxy(output));
   for i := 1 to maxx(output)-2 do write('-');
   write('>*');
   waitnext;

   { ************************** Cursor movements test ************************ }

   { First, do it with automatic scrolling on. The pattern will rely on scroll
     up, down, left wrap and right wrap working correctly. }

   page;
   auto(output, true); { set auto on }
   curvis(output, false); { remove cursor }
   { top of left lower }
   cursor(output, 1, maxy(output));
   write('\\/');
   { top of right lower, bottom of left lower, and move it all up }
   cursor(output, maxx(output)-1, maxy(output));
   write('\\//\\');
   { finish right lower }
   up(output); left(output); left(output); left(output); left(output);
   down(output); down(output);
   write('/\\');
   { now move it back down }
   home(output);
   left(output);
   { upper left hand cross }
   cursor(output, 1, 1);
   write('\\/');
   cursor(output, maxx(output), 1);
   right(output);
   write('/\\');
   { upper right hand cross }
   cursor(output, maxx(output)-1, 2);
   write('/\\');
   cursor(output, 1, 2);
   left(output); left(output);
   write('\\/');
   { test delete works }
   prtcen(1, 'BARK!');
   del(output); del(output); del(output); del(output); del(output);
   prtcen(maxy(output) div 2-1, 'Cursor movements test, automatic scroll ON');
   prtcen(maxy(output) div 2+1, 'Should be a double line X in each corner');
   waitnext;

   { Now do it with automatic scrolling off. The pattern will rely on the
     ability of the cursor to go into "negative" space. }

   page;
   auto(output, false); { disable automatic screen scroll/wrap }
   { upper left }
   home(output);
   write('\\/');
   up(output); left(output); left(output); left(output); left(output);
   down(output); down(output); right(output); right(output);
   write('/\\');
   { upper right }
   cursor(output, maxx(output)-1, 1);
   write('\\/');
   down(output); del(output); del(output);
   write('/\\');
   { lower left }
   cursor(output, 1, maxy(output));
   write('/\\');
   down(output); left(output); left(output); left(output);
   up(output); up(output); right(output);
   write('\\/');
   { lower right }
   cursor(output, maxx(output), maxy(output)-1);
   write('/');
   left(output); left(output);
   write('\\');
   down(output); del(output);
   write('/\\');
   prtcen(maxy(output) div 2-1, 'Cursor movements test, automatic scroll OFF');
   prtcen(maxy(output) div 2+1, 'Should be a double line X in each corner');
   waitnext;

   { **************************** Scroll cursor test ************************* }

   page;
   curvis(output, true);
   prtcen(maxy(output) div 2, 'Scroll cursor test, cursor should be here ->');
   up(output);
   scroll(output, 0, 1);
   waitnext;
   curvis(output, false);

   { ******************************* Row ID test ***************************** }

   page;
   c := '1';
   for y := 1 to maxy(output) do begin

      cursor(output, 1, y); { index start of line }
      for x := 1 to maxx(output) do write(c); { output characters }
      if c <> '9' then c := succ(c) else c := '0'

   end;
   prtban('Row ID test, all rows should be numbered');
   waitnext;

   { *************************** Column ID test ***************************** }

   page;
   fillcol;
   prtban('Column ID test, all columns should be numbered');
   waitnext;

   { ****************************** Fill test ******************************** }

   page;
   c := chr(0); { initalize character value }
   for y := 1 to maxy(output) do begin

      cursor(output, 1, y); { index start of line }
      for x := 1 to maxx(output) do begin

         if (c >= ' ') and (c <> chr($7f)) then write(c)
         else write('\\');
         if c <> chr($7f) then c := succ(c) else c := chr(0)

      end

   end;
   prtban('Fill test, all printable characters should appear');
   waitnext;

   { **************************** Sidewinder test **************************** }

   page;
   x := 1; { set origin }
   y := 1;
   top := 1; { set borders }
   bottom := maxy(output);
   lside := 2;
   rside := maxx(output);
   direction := ddown; { start down }
   t1 := maxx(output);
   t2 := maxy(output);
   tc := 0;
   for count := 1 to t1 * t2 do begin

      cursor(output, x, y); { place character }
      write('*');
      tc := tc+1;
      if tc >= 10 then begin wait(50); tc := 0 end;
      case direction of

         ddown:  begin
                    y := y+1;
                    if y = bottom then begin direction := dright; bottom := bottom-1 end
                 end;
         dright: begin
                    x := x+1;
                    if x = rside then begin direction := dup; rside := rside-1 end
                 end;
         dup:    begin
                    y := y-1;
                    if y = top then begin direction := dleft; top := top+1 end
                 end;
         dleft:  begin
                    x := x-1;
                    if x = lside then begin direction := ddown; lside := lside+1 end
                 end

      end

   end;
   prtcen(maxy(output)-1, '                 ');
   prtcen(maxy(output), ' Sidewinder test ');
   waitnext;

   { *************************** Bouncing ball test ************************** }

   page;
   x := 10; y := 20; { set origin }
   lx := 10; ly := 20; { set last }
   dx := -1; dy := -1; { set initial directions }
   for count := 1 to 1000 do begin

      cursor(output, x, y); { place character }
      write('*');
      wait(100); { wait for display, otherwise cannot see }
      cursor(output, lx, ly);
      write(' ');
      lx := x; ly := y; { set last }
      x := x+dx; y := y+dy; { find next }
      tx := x; ty := y;
      if (x = 1) or (tx = maxx(output)) then dx := -dx;
      if (y = 1) or (ty = maxy(output)) then dy := -dy;
      wait(100) { slow this down }

   end;
   prtcen(maxy(output)-1, '                    ');
   prtcen(maxy(output), ' Bouncing ball test ');
   waitnext;

   { ************************ Attributes and colors test ******************** }

   page;
   if maxy(output) < 20 then write('Not enough lines for attributes test')
   else begin

      blink(output, true);      writeln('Blinking text');     blink(output, false);
      reverse(output, true);    writeln('Reversed text');     reverse(output, false);
      underline(output, true);  writeln('Underlined text');   underline(output, false);
      write('Superscript '); superscript(output, true); writeln('text');
      superscript(output, false);
      write('Subscript ');   subscript(output, true);   writeln('text');
      subscript(output, false);
      italic(output, true);     writeln('Italic text');      italic(output, false);
      bold(output, true);       writeln('Bold text');        bold(output, false);
      strikeout(output, true);  writeln('Strikeout text');   strikeout(output, false);
      standout(output, true);   writeln('Standout text');    standout(output, false);
      fcolor(output, red);     writeln('Red text');
      fcolor(output, green);   writeln('Green text');
      fcolor(output, blue);    writeln('Blue text');
      fcolor(output, cyan);    writeln('Cyan text');
      fcolor(output, yellow);  writeln('Yellow text');
      fcolor(output, magenta); writeln('Magenta text');
      fcolor(output, black);
      bcolor(output, red);     writeln('Red background text');
      bcolor(output, green);   writeln('Green background text');
      bcolor(output, blue);    writeln('Blue background text');
      bcolor(output, cyan);    writeln('Cyan background text');
      bcolor(output, yellow);  writeln('Yellow background text');
      bcolor(output, magenta); writeln('Magenta background text');
      bcolor(output, black); fcolor(output, white);
      writeln('White on black text');
      bcolor(output, white); fcolor(output, black);
      writeln('Black on white text');
      bcolor(output, white); fcolor(output, black);
      prtcen(maxy(output), 'Attributes and colors test')

   end;
   waitnext;

   { **************************** RGB colors test ************************* }

   page;
   auto(output, true);
   prtcen(maxy(output), 'RGB colors test');
   home(output);
   writeln('The terminal may not implement 24 bit RGB colors for characters.');
   writeln;
   writeln('In this case the colors will be the nearest primaries to the RGB');
   writeln('Colors.');
   writeln;
   writeln('Foreground      Background');
   j := 0;
   for i := 1 to 117 do begin

      bcolor(output, white);
      fcolorp(colormap[i]);
      write('*');
      j := j+1;
      if j >= 13 then begin

         write('   ');
         j := 0;
         while j < 13 do begin

            fcolor(output, white);
            bcolorp(colormap[i-13+j+1]);
            write('*');
            j := j+1

         end;
         j := 0;
         writeln

      end

   end;
   waitnext;
   bcolor(output, white);
   fcolor(output, black);

   { ***************************** Scrolling test **************************** }

   page; fillrow;
   for y := 1 to maxy(output) do begin wait(200); scroll(output, 0, 1) end;
   prtcen(maxy(output), 'Scroll up'); waitnext;
   page; fillrow;
   for y := 1 to maxy(output) do begin wait(200); scroll(output, 0, -1) end;
   prtcen(maxy(output), 'Scroll down'); waitnext;
   page; fillcol;
   for x := 1 to maxx(output) do begin wait(200); scroll(output, 1, 0) end;
   prtcen(maxy(output), 'Scroll left'); waitnext;
   page; fillcol;
   for x := 1 to maxx(output) do begin wait(200); scroll(output, -1, 0) end;
   { find minimum screen dimension }
   if maxx(output) < maxy(output) then minlen := maxx(output)
   else minlen := maxy(output);
   prtcen(maxy(output), 'Scroll right'); waitnext;
   page; fillcol;
   for i := 1 to minlen do begin wait(200); scroll(output, 1, 1) end;
   prtcen(maxy(output), 'Scroll up/left'); waitnext;
   page; fillcol;
   for i := 1 to minlen do begin wait(200); scroll(output, 1, -1) end;
   prtcen(maxy(output), 'Scroll down/left'); waitnext;
   page; fillcol;
   for i := 1 to minlen do begin wait(200); scroll(output, -1, 1) end;
   prtcen(maxy(output), 'Scroll up/right'); waitnext;
   page; fillcol;
   for i := 1 to minlen do begin wait(200); scroll(output, -1, -1) end;
   prtcen(maxy(output), 'Scroll down/right'); waitnext;

   { ******************************** Tab test ******************************* }

   { Note tab test, besides testing tabbing, also tests offscreen draws
     (clipping). }

   page;
   auto(output, false); { turn off auto }
   { fill top with column order data }
   c := '1';
   for x := 1 to maxx(output) do begin

      write(c);
      if c <> '9' then c := succ(c) else c := '0'

   end;
   { run tabbing }
   for y := 1 to maxy(output) do begin

      for i := 1 to y-1 do write('\ht');
      writeln('>Tab ', y-1:3)

   end;
   prtcen(maxy(output), 'Tabbing test');
   waitnext;

   { ************************** Offscreen write test ************************* }

   page;
   auto(output, false);
   x := maxx(output) div 2; { find center screen }
   y := maxy(output) div 2;
   for i := 0 to maxx(output) div 2+200 do begin cursor(output, x+i, y); write('*') end;
   for i := 0 to maxy(output) div 2+200 do begin cursor(output, x, y+i); write('*') end;
   for i := 0 to maxx(output) div 2+200 do begin cursor(output, x-i, y); write('*') end;
   for i := 0 to maxy(output) div 2+200 do begin cursor(output, x, y-i); write('*') end;
   home(output);
   writeln('Offscreen write test');
   writeln;
   writeln('There should be a cross centered onscreen.');
   writeln('The display should not scroll.');
   waitnext;

   { ************************** Offscreen scroll test ********************* }

   page;
   auto(output, false);
   writeln('Offscreen scroll test');
   writeln;
   writeln('The line numbers will count screen lines.');
   writeln('The display should not scroll.');
   writeln;
   for y := 6 to maxy(output)+200 do writeln('Line ', y:1);
   waitnext;

   { ************************** Buffer switching test ************************ }

   page;
   curvis(output, false);
   for b := 2 to 10 do begin { prepare buffers }

      select(output, b, 2); { select buffer }
      box(b-1, b-1, maxx(output)-(b-2), maxy(output)-(b-2), '*');
      prtcen(maxy(output), 'Buffer switching test')

   end;
   for i := 1 to 30 do { flip buffers }
      for b := 2 to 10 do begin wait(300); select(output, 2, b) end;
   select(output, 2, 2); { restore buffer select }

   { **************************** Writethrough test ************************** }

   page;
   prtcen(maxy(output), 'File writethrough test');
   home(output);
   rewrite(tf);
   writeln(tf, 'This is a test file');
   reset(tf);
   while not eoln(tf) do begin read(tf, c); write(c) end;
   readln(tf);
   writeln;
   writeln;
   writeln('s/b');
   writeln;
   writeln('This is a test file');
   waitnext;

   { **************************** buffer follow test ************************* }

   page;
   auto(output, false);
   curvis(output, false);
   box(1, 1, maxx(output), maxy(output), '*');
   prtcen(maxy(output), ' Buffer follow test ');
   cursor(output, 3, 3);
   writeln('Resize the window, the frame should stay at the original size');
   waitnext;
   page;
   box(1, 1, maxx(output), maxy(output), '*');
   prtcen(maxy(output), ' Buffer follow test ');
   cursor(output, 3, 3);
   writeln('Resize the window, the frame should follow the window');
   x := maxx(output);
   y := maxy(output);
   repeat

      event(input, er);
      if er.etype = etresize then begin

         box(1, 1, x, y, ' ');
         sizbuf(output, er.rszx, er.rszy);
         box(1, 1, maxx(output), maxy(output), '*');
         x := maxx(output);
         y := maxy(output);
         prtcen(maxy(output), ' Buffer follow test ');
         cursor(output, 3, 3);
         writeln('Resize the window, the frame should follow the window')

      end

   until (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99;
   auto(output, true);
   curvis(output, false);

   { **************************** Focus and hover test *********************** }

   page;
   curvis(output, false);
   prtcen(maxy(output), 'Focus and hover test');
   home(output);
   writeln('Click the window, then other windows and watch the focus box.');
   writeln;
   writeln('Roll over the window, then outside the window, and watch the hover box.');
   writeln;
   writeln('If focus is not supported, it is always on');
   writeln;
   writeln('Note with simulated hover, assert is immedate, but deassert is');
   writeln('after about 5 seconds.');
   box(10, 10, 30, 14, '#');
   cursor(output, 17, 12);
   write('Focus');
   box(40, 10, 60, 14, '#');
   cursor(output, 47, 12);
   write('hover');
   repeat

      event(input, er);
      if er.etype = etfocus then box(10, 10, 30, 14, '#')
      else if er.etype = etnofocus then box(10, 10, 30, 14, '*');
      if er.etype = ethover then box(40, 10, 60, 14, '#')
      else if er.etype = etnohover then box(40, 10, 60, 14, '*')

   until (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99;
   curvis(output, true);

   { ****************************** Joystick test **************************** }

   if joystick(output) > 0 then begin { joystick test }

      page;
      curvis(output, false);
      prtcen(1, 'Move the joystick(s) X, Y and Z, and hit buttons');
      prtcen(maxy(output), 'Joystick test');
      repeat { gather joystick events }

         event(input, er);
         if er.etype = etjoymov then begin { joystick movement }

            cursor(output, 1, 3);
            writeln('joystick: ', er.mjoyn:3, ' x: ', er.joypx:11, ' y: ',
                    er.joypy:11, ' z: ', er.joypz:11);
            writeln('              4: ', er.joyp4:11, ' 5: ', er.joyp5:11,
                    ' 6: ', er.joyp6:11);
            plotjoy(5, er.joypx);
            plotjoy(6, er.joypy);
            plotjoy(7, er.joypz);
            plotjoy(8, er.joyp4);
            plotjoy(9, er.joyp5);
            plotjoy(10, er.joyp6)

         end else if er.etype = etjoyba then begin { joystick button assert }

            cursor(output, 1, 17+er.ajoyn);
            write('joystick: ', er.ajoyn:1, ' button assert:   ', er.ajoybn:2)

         end else if er.etype = etjoybd then begin { joystick button deassert }

            cursor(output, 1, 17+er.djoyn);
            write('joystick: ', er.djoyn:1, ' button deassert: ', er.djoybn:2)

         end

      until (er.etype = etenter) or (er.etype = etterm);
      if er.etype = etterm then goto 99;
      curvis(output, true)

   end;

   { **************************** Mouse test ********************************* }

   if mouse(output) > 0 then begin { mouse test }

      x := 1; y := 1;
      page;
      auto(output, false);
      curvis(output, false);
      prtcen(1, 'Move the mouse, and hit buttons');
      prtcen(maxy(output), 'Mouse test');
      repeat { gather mouse events }

         event(input, er);
         if er.etype = etmoumov then begin

            cursor(output, x, y);
            writeln('          ');
            cursor(output, er.moupx, er.moupy);
            x := curx(output);
            y := cury(output);
            writeln('<- Mouse ', er.mmoun:1);
            prtcen(1, 'Move the mouse, and hit buttons');
            prtcen(maxy(output), 'Mouse test')

         end;
         { blank out button status line }
         cursor(output, 1, maxy(output)-2);
         for i := 1 to maxx(output) do write(' ');
         if er.etype = etmouba then begin { mouse button assert }

            cursor(output, 1, maxy(output)-2);
            writeln('Mouse button assert, mouse: ', er.amoun:1,
                    ' button: ', er.amoubn:1);
            prtcen(1, 'Move the mouse, and hit buttons');
            prtcen(maxy(output), 'Mouse test')

         end;
         if er.etype = etmoubd then begin { mouse button deassert }

            cursor(output, 1, maxy(output)-2);
            writeln('Mouse button deassert, mouse: ', er.dmoun:1,
                    ' button: ', er.dmoubn:1);
            prtcen(1, 'Move the mouse, and hit buttons');
            prtcen(maxy(output), 'Mouse test')

         end

      until (er.etype = etenter) or (er.etype = etterm);
      if er.etype = etterm then goto 99;
      home(output);
      auto(output, true);
      curvis(output, true)

   end;

   { ************************* Event vector test  **************************** }

   page;
   prtcen(maxy(output), 'Event vector test');
   home(output);
   { since there is no facility to remove vectors, these tests have to be done
     in order. }

   eventflag1 := false;
   eventover(etframe, event_vector_1, oeh1);
   frametimer(output, true);
   writeln('Waiting for frame event, hit return to continue');
   repeat event(input, er)
   until (er.etype = etframe) or (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99;
   if er.etype = etframe then writeln('*** Event bled through! ***');
   if eventflag1 then writeln('Fanout event passes')
   else writeln('*** Fanout event fails! ***');
   eventflag2 := false;
   eventsover(event_vector_2, oeh1);
   writeln('Waiting for frame event, hit return to continue');
   repeat event(input, er)
   until (er.etype = etframe) or (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99;
   if er.etype = etframe then writeln('*** Event bled through! ***');
   if eventflag2 then writeln('Master event passes')
   else writeln('*** Master event fails! ***');
   frametimer(output, false);
   waitnext;

   { ********************** Character write speed test *********************** }

   page;
   curvis(output, false);
   clk := clock; { get reference time }
   c := chr(0); { initalize character value }
   cnt := 0; { clear character count }
   for y := 1 to maxy(output) do begin

      cursor(output, 1, y); { index start of line }
      for x := 1 to maxx(output) do begin

         if (c >= ' ') and (c <> chr($7f)) then write(c) else write('\\');
         if c <> chr($7f) then c := succ(c) else c := chr(0);
         cnt := cnt+1 { count characters }

      end

   end;
   clk := elapsed(clk); { find elapsed time }
   benchtab[bncharw].iter := cnt;
   benchtab[bncharw].time := clk;
   page;
   writeln('Character write speed: ', clk*0.0001:1:6, ' seconds, per character ',
           clk/cnt*0.0001:1:8);
   waitnext;

   { ************************** Scrolling speed test ************************* }

   page;
   fillrow; { fill screen so we aren't moving blanks }
   prtban('Scrolling speed test');
   clk := clock; { get reference time }
   cnt := 0; { clear count }
   for i := 1 to 100 do begin { scroll various directions }

      scroll(output, 0, -1); scroll(output, -1, 0); scroll(output, 0, 1);
      scroll(output, 0, 1); scroll(output, 1, 0); scroll(output, 1, 0);
      scroll(output, 0, -1); scroll(output, 0, -1); scroll(output, -1, 0);
      scroll(output, 0, 1); scroll(output, -1, -1); scroll(output, 1, 1);
      scroll(output, 1, 1); scroll(output, -1, -1); scroll(output, 1, -1);
      scroll(output, -1, 1); scroll(output, -1, 1); scroll(output, 1, -1);
      cnt := cnt+19 { count all scrolls }

   end;
   clk := elapsed(clk); { find elapsed time }
   benchtab[bnscroll].iter := cnt;
   benchtab[bnscroll].time := clk;
   page;
   writeln('Scrolling speed: ', clk*0.0001:1:6, ' seconds, per scroll ',
           clk/cnt*0.0001:1:8);
   waitnext;

   { ************************** Buffer flip speed test ************************* }

   page;
   cnt := 0; { clear count }
   for b := 2 to 10 do begin { prepare buffers }

      select(output, b, 2); { select buffer }
      box(b-1, b-1, maxx(output)-b+2, maxy(output)-b+2, '*')

   end;
   clk := clock; { get reference time }
   for i := 1 to 100 do { flip buffers }
      for b := 2 to 10 do begin select(output, 2, b); cnt := cnt+1 end;
   clk := elapsed(clk); { find elapsed time }
   benchtab[bnbuffer].iter := cnt;
   benchtab[bnbuffer].time := clk;
   select(output, 2, 2); { restore buffer select }
   page;
   writeln('Buffer switch speed: ', clk*0.0001:1:6, ' average seconds per switch ',
           clk/cnt*0.0001:1:8);
   waitnext;

   99: ; { terminate }

   { test complete }
   select(output, 1, 1); { back to display buffer }
   curvis(output, true); { restore cursor }
   auto(output, true); { enable automatic screen wrap }
   writeln;
   writeln('Test complete');
   writeln;

   { output benchmark table }

   writeln;
   writeln('Benchmark table');
   writeln;
   writeln('Type                   Seconds  Per fig');
   writeln('--------------------------------------------');
   for bi := bncharw to bnbuffer do begin

      case bi of

         bncharw:  write('character write speed ');
         bnscroll: write('Scroll speed          ');
         bnbuffer: write('Buffer flip speed     ')

      end;
      write(benchtab[bi].time*0.0001:6:2, '    ');
      writeln(benchtab[bi].time*0.0001/benchtab[bi].iter:1:8)

   end;
   writeln

end.
