{******************************************************************************
*                                                                             *
*                           WIDGET TEST PROGRAM                               *
*                                                                             *
*                    Copyright (C) 2005 Scott A. Franco                       *
*                                                                             *
* Tests the widgets and dialogs available. It is the Pascaline equivalent of  *
* the Ami widget_test.c reference test.                                       *
*                                                                             *
* The terminate event (etterm) is handled by polling in the wait routines and *
* event loops, which perform a non-local goto to the terminate label (99).    *
*                                                                             *
******************************************************************************}

program widget_test(input, output);

uses graphics,
     services,
     version,
     strings;

label 99; { terminate }

const

   second = 10000;      { one second in 100 microsecond ticks }
   i32max = 2147483647; { 32 bit INT_MAX, used for scroll/progress ratios }
   { font codes (from amitk/include/graphics.h: AMI_FONT_TERM..AMI_FONT_TECH) }
   ftbook = 2;          { AMI_FONT_BOOK }

var

   framenum:           integer; { current frame number }
   er:                 evtrec;  { ami_evtrec er, event record }
   chk, chk2, chk3:    integer; { checkbox/radio states (C int booleans) }
   s:                  packed array [1..100] of char; { widget text buffer }
   ss, rs:             packed array [1..100] of char; { search/replace }
   prog:               integer; { progress bar position }
   sp, lp:             strptr;  { string list pointers }
   x, y, lm, xs, ys:   integer;
   bx, by, ix, iy:     integer;
   r, g, b:            integer;
   optf:               qfnopts;  { find query options }
   optfr:              qfropts;  { find/replace query options }
   fc:                 integer;  { font code }
   fs:                 integer;  { font size }
   fr, fg, fb:         integer;  { foreground color }
   br, bg, bb:         integer;  { background color }
   fe:                 qfteffects; { font effects }
   cx, cy:             integer;
   ox, oy:             integer;
   cox, coy:           integer;
   csx, csy:           integer;
   i:                  integer;

{ ************************************************************************

   Helper routines

   ************************************************************************ }

{ wait return to be pressed, or handle terminate }

{ set the window title with the chapter frame number, as graphics_test does;
  called at the start of each chapter so every screen is numbered, including
  the interactive chapters that wait on their own event loop instead of
  waitnext }

procedure setframe;

var ts: pstring; { title string }

begin

   framenum := framenum+1;
   openstring;
   ts := cat('widget_test: frame ', ints(framenum));
   title(output, ts^);
   closestring

end;

procedure waitnext;

var er: evtrec;

begin

   repeat event(input, er) until (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{ draw a character grid }

procedure chrgrid;

var x, y: integer;

begin

   fcolor(output, yellow);
   y := 1;
   while y < maxyg(output) do begin

      line(output, 1, y, maxxg(output), y);
      y := y+chrsizy(output)

   end;
   x := 1;
   while x < maxxg(output) do begin

      line(output, x, 1, x, maxyg(output));
      x := x+chrsizx(output)

   end;
   fcolor(output, black)

end;

{ ************************************************************************

   Main program

   ************************************************************************ }

begin

   framenum := 0;

   curvis(output, false);
   write('Widget test vs. ', majorver:1, '.', minorver:1);
   if experiment then write('.x');
   writeln;
   writeln;
   writeln('Hit return in any window to continue for each test');
   waitnext;

   { ************************** Background test ************************* }

   setframe;

   bcolor(output, backcolor); { the system widget face color }
   page;
   writeln('Background pa_color test');
   writeln;
   writeln('The background color should match widgets now.');
   waitnext;
   bcolor(output, white);

   { ********************** Terminal Button test ************************ }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal buttons test');
   writeln;
   buttonsiz(output, 'Hello, there', x, y);
   button(output, 10, 7, 10+x-1, 7+y-1, 'Hello, there', 1);
   buttonsiz(output, 'Bark!', x, y);
   button(output, 10, 10, 10+x-1, 10+y-1, 'Bark!', 2);
   buttonsiz(output, 'Sniff', x, y);
   button(output, 10, 13, 10+x-1, 13+y-1, 'Sniff', 3);
   writeln('Hit the buttons, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etbutton then begin

         if er.butid = 1 then writeln('Hello to you, too')
         else if er.butid = 2 then writeln('Bark bark')
         else if er.butid = 3 then writeln('Sniff sniff')
         else writeln('!!! No button with id: ', er.butid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   enablewidget(output, 2, 0);
   writeln('Now the middle button is disabled, and should not be able to');
   writeln('be pressed.');
   writeln('Hit the buttons, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etbutton then begin

         if er.butid = 1 then writeln('Hello to you, too')
         else if er.butid = 2 then writeln('Bark bark')
         else if er.butid = 3 then writeln('Sniff sniff')
         else writeln('!!! No button with id: ', er.butid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);

   { ********************* Graphical Buttons test *********************** }

   setframe;

   page;
   writeln('Graphical buttons test');
   writeln;
   writeln('Hit the buttons, or return to continue');
   writeln;
   lm := maxxg(output) div 20; { set left margin }
   i := curyg(output); { set y position buttons }
   buttonsizg(output, 'Hello, there', x, y);
   buttong(output, lm, i, lm+x, i+y, 'Hello, there', 1);
   i := i+y+(y div 2); { set increment between buttons }
   buttonsizg(output, 'Bark!', x, y);
   buttong(output, lm, i, lm+x, i+y, 'Bark!', 2);
   i := i+y+(y div 2); { set increment between buttons }
   buttonsizg(output, 'Sniff', x, y);
   buttong(output, lm, i, lm+x, i+y, 'Sniff', 3);
   repeat

      event(input, er);
      if er.etype = etbutton then begin

         if er.butid = 1 then writeln('Hello to you, too')
         else if er.butid = 2 then writeln('Bark bark')
         else if er.butid = 3 then writeln('Sniff sniff')
         else begin

            writeln('!!! No button with id: ', er.butid:1);
            write(' !!!')

         end

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   enablewidget(output, 2, 0);
   writeln('Now the middle button is disabled, and should not be able to');
   writeln('be pressed.');
   writeln('Hit the buttons, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etbutton then begin

         if er.butid = 1 then writeln('Hello to you, too')
         else if er.butid = 2 then writeln('Bark bark')
         else if er.butid = 3 then writeln('Sniff sniff')
         else writeln('!!! No button with id: ', er.butid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);

   { ********************* Terminal Checkbox test *********************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal checkbox test');
   writeln;
   chk := 0;
   chk2 := 0;
   chk3 := 0;
   checkboxsiz(output, 'Pick me', x, y);
   checkbox(output, 10, 7, 10+x-1, 7+y-1, 'Pick me', 1);
   checkboxsiz(output, 'Or me', x, y);
   checkbox(output, 10, 10, 10+x-1, 10+y-1, 'Or me', 2);
   checkboxsiz(output, 'No, me', x, y);
   checkbox(output, 10, 13, 10+x-1, 13+y-1, 'No, me', 3);
   writeln('Hit the checkbox, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etchkbox then begin

         if er.ckbxid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.ckbxid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.ckbxid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else writeln('!!! No button with id: ', er.ckbxid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   enablewidget(output, 2, 0);
   writeln('Now the middle checkbox is disabled, and should not be able to');
   writeln('be pressed.');
   writeln('Hit the checkbox, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etchkbox then begin

         if er.ckbxid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.ckbxid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.ckbxid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else writeln('!!! No button with id: ', er.ckbxid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);

   { ********************* Graphical Checkbox test ********************** }

   setframe;

   page;
   writeln('Graphical checkbox test');
   writeln;
   writeln('Hit the checkbox, or return to continue');
   writeln;
   lm := maxxg(output) div 20; { set left margin }
   i := curyg(output); { set y position buttons }
   chk := 0;
   chk2 := 0;
   chk3 := 0;
   checkboxsizg(output, 'Pick me', x, y);
   checkboxg(output, lm, i, lm+x, i+y, 'Pick me', 1);
   i := i+y+(y div 2); { set increment between checkboxes }
   checkboxsizg(output, 'Or me', x, y);
   checkboxg(output, lm, i, lm+x, i+y, 'Or me', 2);
   i := i+y+(y div 2); { set increment between checkboxes }
   checkboxsizg(output, 'No, me', x, y);
   checkboxg(output, lm, i, lm+x, i+y, 'No, me', 3);

   repeat

      event(input, er);
      if er.etype = etchkbox then begin

         if er.ckbxid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.ckbxid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.ckbxid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else writeln('!!! No button with id: ', er.ckbxid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   enablewidget(output, 2, 0);
   writeln('Now the middle checkbox is disabled, and should not be able to');
   writeln('be pressed.');
   writeln('Hit the checkbox, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etchkbox then begin

         if er.ckbxid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.ckbxid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.ckbxid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else writeln('!!! No button with id: ', er.ckbxid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);

   { ******************* Terminal radio button test ********************* }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal radio button test');
   writeln;
   chk := 0;
   chk2 := 0;
   chk3 := 0;
   radiobuttonsiz(output, 'Station 1', x, y);
   radiobutton(output, 10, 7, 10+x-1, 7+y-1, 'Station 1', 1);
   radiobuttonsiz(output, 'Station 2', x, y);
   radiobutton(output, 10, 10, 10+x-1, 10+y-1, 'Station 2', 2);
   radiobuttonsiz(output, 'Station 3', x, y);
   radiobutton(output, 10, 13, 10+x-1, 13+y-1, 'Station 3', 3);
   writeln('Hit the radio button, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etradbut then begin

         if er.radbid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.radbid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.radbid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else write('!!! No button with id: ', er.radbid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   enablewidget(output, 2, 0);
   writeln('Now the middle radio button is disabled, and should not be able');
   writeln('to be pressed.');
   writeln('Hit the radio button, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etradbut then begin

         if er.radbid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.radbid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.radbid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else writeln('!!! No button with id: ', er.radbid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);

   { ******************* Graphical radio button test ******************** }

   setframe;

   page;
   writeln('Graphical radio button test');
   writeln;
   writeln('Hit the radio button, or return to continue');
   writeln;
   lm := maxxg(output) div 20; { set left margin }
   i := curyg(output); { set y position buttons }
   chk := 0;
   chk2 := 0;
   chk3 := 0;
   radiobuttonsizg(output, 'Station 1', x, y);
   radiobuttong(output, lm, i, lm+x, i+y, 'Station 1', 1);
   i := i+y+(y div 2); { set increment between buttons }
   radiobuttonsizg(output, 'Station 2', x, y);
   radiobuttong(output, lm, i, lm+x, i+y, 'Station 2', 2);
   i := i+y+(y div 2); { set increment between buttons }
   radiobuttonsizg(output, 'Station 3', x, y);
   radiobuttong(output, lm, i, lm+x, i+y, 'Station 3', 3);

   repeat

      event(input, er);
      if er.etype = etradbut then begin

         if er.radbid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.radbid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.radbid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else writeln('!!! No button with id: ', er.radbid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   enablewidget(output, 2, 0);
   writeln('Now the middle radio button is disabled, and should not be able');
   writeln('to be pressed.');
   writeln('Hit the radio button, or return to continue');
   writeln;
   repeat

      event(input, er);
      if er.etype = etradbut then begin

         if er.radbid = 1 then begin

            writeln('You selected the top checkbox');
            chk := 1-chk;
            selectwidget(output, 1, chk)

         end else if er.radbid = 2 then begin

            writeln('You selected the middle checkbox');
            chk2 := 1-chk2;
            selectwidget(output, 2, chk2)

         end else if er.radbid = 3 then begin

            writeln('You selected the bottom checkbox');
            chk3 := 1-chk3;
            selectwidget(output, 3, chk3)

         end else writeln('!!! No button with id: ', er.radbid:1, ' !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);

   { ******************** Terminal Group box test *********************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal group box test');
   writeln;
   groupsiz(output, 'Hello there', 0, 0, x, y, ox, oy);
   group(output, 10, 10, 10+x, 10+y, 'Hello there', 1);
   writeln('This is a group box with a null client area');
   writeln('Hit return to continue');
   waitnext;
   killwidget(output, 1);
   groupsiz(output, 'Hello there', 20, 10, x, y, ox, oy);
   group(output, 10, 10, 10+x, 10+y, 'Hello there', 1);
   writeln('This is a group box with a 20,10 client area');
   writeln('Hit return to continue');
   waitnext;
   killwidget(output, 1);
   groupsiz(output, 'Hello there', 20, 10, x, y, ox, oy);
   group(output, 10, 10, 10+x, 10+y, 'Hello there', 1);
   button(output, 10+ox, 10+oy, 10+ox+20-1, 10+oy+10-1, 'Bark, bark!', 2);
   writeln('This is a group box with a 20,10 layered button');
   writeln('Hit return to continue');
   waitnext;
   killwidget(output, 1);
   killwidget(output, 2);

   { ******************** Graphical Group box test ********************** }

   setframe;

   page;
   writeln('Graphical group box test');
   writeln;
   writeln('This is a group box with a null client area');
   writeln('Hit return to continue');
   writeln;
   xs := maxxg(output) div 10; { set size of group client }
   ys := xs;
   lm := maxxg(output) div 20; { set left margin }
   i := curyg(output)+7*chrsizy(output); { set y position buttons }
   groupsizg(output, 'Hello there', 0, 0, x, y, ox, oy);
   groupg(output, lm, i, lm+x, i+y, 'Hello there', 1);
   waitnext;
   killwidget(output, 1);
   writeln('This is a group box with a ', xs:1, ',', ys:1, ' client area');
   writeln('Hit return to continue');
   writeln;
   groupsizg(output, 'Hello there', xs, ys, x, y, ox, oy);
   groupg(output, lm, i, lm+x, i+y, 'Hello there', 1);
   waitnext;
   killwidget(output, 1);
   writeln('This is a group box with a ', xs:1, ',', ys:1, ' layered button');
   write('Hit return to continue');
   writeln;
   groupsizg(output, 'Hello there', xs, ys, x, y, ox, oy);
   groupg(output, lm, i, lm+x, i+y, 'Hello there', 1);
   buttong(output, lm+ox, i+oy, lm+ox+xs, i+oy+ys, 'Bark, bark!', 2);
   waitnext;
   killwidget(output, 1);
   killwidget(output, 2);

   { ******************** Terminal background test ********************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal background test');
   writeln;
   background(output, 10, 10, 40, 20, 1);
   writeln('Hit return to continue');
   waitnext;
   button(output, 11, 11, 39, 19, 'Bark, bark!', 2);
   writeln('This is a background with a layered button');
   writeln('Hit return to continue');
   waitnext;
   killwidget(output, 1);
   killwidget(output, 2);

   { ******************** Graphical background test ********************* }

   setframe;

   page;
   writeln('Graphical background test');
   writeln;
   writeln('Hit return to continue');
   writeln;
   xs := maxxg(output) div 5; { set size of group client }
   ys := xs;
   bx := xs div 10;
   by := bx;
   lm := maxxg(output) div 20; { set left margin }
   i := curyg(output)+chrsizy(output)*3; { set y position buttons }
   backgroundg(output, lm, i, lm+xs, i+ys, 1);
   waitnext;
   buttong(output, lm+bx, i+by, lm+xs-bx, i+ys-by, 'Bark, bark!', 2);
   writeln('This is a background with a layered button');
   writeln('Hit return to continue');
   waitnext;
   killwidget(output, 1);
   killwidget(output, 2);

   { ******************** Terminal scroll bar test ********************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal scroll bar test');
   writeln;
   xs := maxxg(output) div 5; { set size of group client }
   ys := xs;
   bx := xs div 10;
   by := bx;
   lm := maxxg(output) div 20; { set left margin }
   scrollvertsiz(output, x, y);
   scrollvert(output, 10, 10, 10+x-1, 20, 1);
   scrollhorizsiz(output, x, y);
   scrollhoriz(output, 15, 10, 35, 10+y-1, 2);
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);

   { **************** Terminal scroll bar sizing test ******************* }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal scroll bar sizing test');
   writeln;
   scrollvert(output, 10, 10, 12, 20, 1);
   scrollsiz(output, 1, (i32max div 4)*3);
   scrollvert(output, 10+5, 10, 12+5, 20, 2);
   scrollsiz(output, 2, i32max div 2);
   scrollvert(output, 10+10, 10, 12+10, 20, 3);
   scrollsiz(output, 3, i32max div 4);
   scrollvert(output, 10+15, 10, 12+15, 20, 4);
   scrollsiz(output, 4, i32max div 8);
   writeln('Now should be four scrollbars, decending in size to the right.');
   writeln('All of the scrollbars can be manipulated.');
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { *************** Terminal scroll bar minimums test ****************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal scroll bar minimums test');
   writeln;
   scrollvertsiz(output, x, y);
   scrollvert(output, 10, 10, 10+x-1, 10+y-1, 1);
   scrollhorizsiz(output, x, y);
   scrollhoriz(output, 15, 10, 15+x-1, 10+y-1, 2);
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);

   { ********** Terminal scroll bar fat and skinny bars test ************ }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal scroll bar fat and skinny bars test');
   writeln;
   scrollvertsiz(output, x, y);
   scrollvert(output, 10, 10, 10, 10+10, 1);
   scrollvert(output, 12, 10, 20, 10+10, 3);
   scrollhorizsiz(output, x, y);
   scrollhoriz(output, 30, 10, 30+20, 10, 2);
   scrollhoriz(output, 30, 12, 30+20, 20, 4);
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { ******************** Graphical scroll bar test ********************* }

   setframe;

   page;
   writeln('Graphical scroll bar test');
   writeln;
   lm := maxxg(output) div 20; { set left margin }
   iy := curyg(output); { set y increment }
   ys := maxyg(output) div 4;
   xs := ys;
   scrollvertsizg(output, x, y);
   scrollvertg(output, lm, iy, lm+x, iy+ys, 1);
   scrollhorizsizg(output, x, y);
   scrollhorizg(output, lm+x+chrsizx(output), iy,
                        lm+x+chrsizx(output)+xs, iy+y, 2);
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/pa_left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);

   { **************** Graphical scroll bar sizing test ****************** }

   setframe;

   page;
   writeln('Graphical scroll bar sizing test');
   writeln;
   writeln('Now should be four scrollbars, decending in size to the right.');
   writeln('All of the scrollbars can be manipulated.');
   writeln;
   lm := maxxg(output) div 20; { set left margin }
   iy := curyg(output); { set y increment }
   ys := maxyg(output) div 4;
   xs := maxxg(output) div 30;
   scrollvertsizg(output, x, y);
   scrollvertg(output, lm, iy, lm+x, iy+ys, 1);
   scrollsiz(output, 1, (i32max div 4)*3);
   scrollvertg(output, lm+xs, iy, lm+xs+x, iy+ys, 2);
   scrollsiz(output, 2, i32max div 2);
   scrollvertg(output, lm+xs*2, iy, lm+xs*2+x, iy+ys, 3);
   scrollsiz(output, 3, i32max div 4);
   scrollvertg(output, lm+xs*3, iy, lm+xs*3+x, iy+ys, 4);
   scrollsiz(output, 4, i32max div 8);
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/pa_left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { *************** Graphical scroll bar minimums test ***************** }

   setframe;

   page;
   writeln('Graphical scroll bar minimums test');
   writeln;
   lm := maxxg(output) div 20; { set left margin }
   iy := curyg(output); { set y increment }
   xs := maxxg(output) div 30;
   scrollvertsizg(output, x, y);
   scrollvertg(output, lm, iy, lm+x, iy+y, 1);
   scrollsiz(output, 1, i32max div 2);
   scrollhorizsizg(output, x, y);
   scrollhorizg(output, lm+xs, iy, lm+xs+x, iy+y, 2);
   scrollsiz(output, 2, i32max div 2);
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);

   { ********** Graphical scroll bar fat and skinny bars test *********** }

   setframe;

   page;
   writeln('Graphical scroll bar fat and skinny bars test');
   writeln;
   lm := maxxg(output) div 20; { set left margin }
   iy := curyg(output); { set y increment }
   ix := maxxg(output) div 30; { set x increment }
   xs := maxxg(output) div 4;
   ys := xs;
   scrollvertsizg(output, x, y);
   scrollvertg(output, lm, iy, lm+x, iy+ys, 1);
   scrollvertg(output, lm+ix, iy, lm+ix+maxxg(output) div 10, iy+ys, 3);
   lm := lm+ix+maxxg(output) div 10+maxxg(output) div 20;
   scrollhorizsizg(output, x, y);
   scrollhorizg(output, lm, iy, lm+xs, iy+y, 2);
   scrollhorizg(output, lm, iy+ix, lm+xs, iy+y+ix+maxxg(output) div 10, 4);
   repeat

      event(input, er);
      if er.etype = etsclull then
         writeln('Scrollbar: ', er.sclulid:1, ' up/left line');
      if er.etype = etscldrl then
         writeln('Scrollbar: ', er.scldrid:1, ' down/right line');
      if er.etype = etsclulp then
         writeln('Scrollbar: ', er.sclupid:1, ' up/left page');
      if er.etype = etscldrp then
         writeln('Scrollbar: ', er.scldpid:1, ' down/right page');
      if er.etype = etsclpos then begin

         scrollpos(output, er.sclpid, er.sclpos); { set new position for scrollbar }
         writeln('Scrollbar: ', er.sclpid:1, ' position set: ', er.sclpos:1)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { *************** Terminal number select box test ******************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal number select box test');
   writeln;
   numselboxsiz(output, 1, 10, x, y);
   numselbox(output, 10, 10, 10+x-1, 10+y-1, 1, 10, 1);
   repeat

      event(input, er);
      if er.etype = etnumbox then writeln('You selected: ', er.numbsl:1);
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { *************** Graphical number select box test ******************* }

   setframe;

   page;
   writeln('Graphical number select box test');
   writeln;
   numselboxsizg(output, 1, 10, x, y);
   numselboxg(output, 100, 100, 100+x, 100+y, 1, 10, 1);
   repeat

      event(input, er);
      if er.etype = etnumbox then writeln('You selected: ', er.numbsl:1);
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ********************* Terminal edit box test *********************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal edit box test');
   writeln;
   editboxsiz(output, 'Hi there, george', x, y);
   editbox(output, 10, 10, 10+x-1, 10+y-1, 1);
   putwidgettext(output, 1, 'Hi there, george');
   repeat

      event(input, er);
      if er.etype = etedtbox then begin

         getwidgettext(output, 1, s);
         writeln('You entered: ', s:*)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ********************* Graphical edit box test ********************** }

   setframe;

   page;
   writeln('Graphical edit box test');
   writeln;
   editboxsizg(output, 'Hi there, george', x, y);
   editboxg(output, 100, 100, 100+x-1, 100+y-1, 1);
   putwidgettext(output, 1, 'Hi there, george');
   repeat

      event(input, er);
      if er.etype = etedtbox then begin

         getwidgettext(output, 1, s);
         writeln('You entered: ', s:*)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ******************* Terminal progress bar test ********************* }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal progress bar test');
   writeln;
   progbarsiz(output, x, y);
   progbar(output, 10, 10, 10+x-1, 10+y-1, 1);
   timer(output, 1, second, true);
   prog := 1;
   repeat

      event(input, er);
      if er.etype = ettim then begin

         if prog < 20 then begin

            progbarpos(output, 1, i32max-((20-prog)*(i32max div 20)));
            prog := prog+1 { next progress value }

         end else if prog = 20 then begin

            progbarpos(output, 1, i32max);
            writeln('Done !');
            prog := 11;
            killtimer(output, 1)

         end

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ******************* Graphical progress bar test ******************** }

   setframe;

   page;
   writeln('Graphical progress bar test');
   writeln;
   progbarsizg(output, x, y);
   progbarg(output, 100, 100, 100+x-1, 100+y-1, 1);
   timer(output, 1, second, true);
   prog := 1;
   repeat

      event(input, er);
      if er.etype = ettim then begin

         if prog < 20 then begin

            progbarpos(output, 1, i32max-((20-prog)*(i32max div 20)));
            prog := prog+1 { next progress value }

         end else if prog = 20 then begin

            progbarpos(output, 1, i32max);
            writeln('Done !');
            prog := 11;
            killtimer(output, 1)

         end

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ********************* Terminal list box test *********************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal list box test');
   writeln;
   writeln('Note that it is normal for this box to not fill to exact');
   writeln('character cells.');
   writeln;
   new(lp);
   lp^.str := copy('Blue');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Red');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Green');
   sp^.next := lp;
   lp := sp;
   listboxsiz(output, lp, x, y);
   listbox(output, 10, 10, 10+x-1, 10+y-1, lp, 1);
   repeat

      event(input, er);
      if er.etype = etlstbox then begin

         if er.lstbsl = 1 then writeln('You selected pa_green')
         else if er.lstbsl = 2 then writeln('You selected pa_red')
         else if er.lstbsl = 3 then writeln('You selected pa_blue')
         else writeln('!!! Bad select number !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ********************* Graphical list box test ********************** }

   setframe;

   page;
   writeln('Graphical list box test');
   writeln;
   new(lp);
   lp^.str := copy('Blue');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Red');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Green');
   sp^.next := lp;
   lp := sp;
   listboxsizg(output, lp, x, y);
   listboxg(output, 100, 100, 100+x-1, 100+y-1, lp, 1);
   repeat

      event(input, er);
      if er.etype = etlstbox then begin

         if er.lstbsl = 1 then writeln('You selected pa_green')
         else if er.lstbsl = 2 then writeln('You selected pa_red')
         else if er.lstbsl = 3 then writeln('You selected pa_blue')
         else writeln('!!! Bad select number !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ******************* Terminal dropdown box test ********************* }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal dropdown box test');
   writeln;
   writeln('Note that it is normal for this box to not fill to exact');
   writeln('character cells.');
   writeln;
   new(lp);
   lp^.str := copy('dog');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('cat');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('bird');
   sp^.next := lp;
   lp := sp;
   dropboxsiz(output, lp, cx, cy, ox, oy);
   dropbox(output, 10, 10, 10+ox-1, 10+oy-1, lp, 1);
   repeat

      event(input, er);
      if er.etype = etdrpbox then begin

         if er.drpbsl = 1 then writeln('You selected Bird')
         else if er.drpbsl = 2 then writeln('You selected Cat')
         else if er.drpbsl = 3 then writeln('You selected Dog')
         else writeln('!!! Bad select number !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ******************* Graphical dropdown box test ******************** }

   setframe;

   page;
   writeln('Graphical dropdown box test');
   writeln;
   new(lp);
   lp^.str := copy('dog');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('cat');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('bird');
   sp^.next := lp;
   lp := sp;
   dropboxsizg(output, lp, cx, cy, ox, oy);
   dropboxg(output, 100, 100, 100+ox-1, 100+oy-1, lp, 1);
   repeat

      event(input, er);
      if er.etype = etdrpbox then begin

         if er.drpbsl = 1 then writeln('You selected Bird')
         else if er.drpbsl = 2 then writeln('You selected Cat')
         else if er.drpbsl = 3 then writeln('You selected Dog')
         else writeln('!!! Bad select number !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { **************** Terminal dropdown edit box test ******************* }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal dropdown edit box test');
   writeln;
   writeln('Note that it is normal for this box to not fill to exact');
   writeln('character cells.');
   writeln;
   new(lp);
   lp^.str := copy('corn');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('flower');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Tortillas');
   sp^.next := lp;
   lp := sp;
   dropeditboxsiz(output, lp, cx, cy, ox, oy);
   dropeditbox(output, 10, 10, 10+ox-1, 10+oy-1, lp, 1);
   repeat

      event(input, er);
      if er.etype = etdrebox then begin

         getwidgettext(output, 1, s);
         writeln('You selected: ', s:*)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { **************** Graphical dropdown edit box test ****************** }

   setframe;

   page;
   writeln('Graphical dropdown edit box test');
   writeln;
   new(lp);
   lp^.str := copy('corn');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('flower');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Tortillas');
   sp^.next := lp;
   lp := sp;
   dropeditboxsizg(output, lp, cx, cy, ox, oy);
   dropeditboxg(output, 100, 100, 100+ox-1, 100+oy-1, lp, 1);
   repeat

      event(input, er);
      if er.etype = etdrebox then begin

         getwidgettext(output, 1, s);
         writeln('You selected: ', s:*)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);

   { ********************** Terminal slider test ************************ }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal slider test');
   slidehorizsiz(output, x, y);
   x := 20;
   slidehoriz(output, 10, 10, 10+x-1, 10+y-1, 10, 1);
   slidehoriz(output, 10, 20, 10+x-1, 20+y-1, 0, 2);
   slidevertsiz(output, x, y);
   y := 10;
   slidevert(output, 40, 10, 40+x-1, 10+y-1, 10, 3);
   slidevert(output, 50, 10, 50+x-1, 10+y-1, 0, 4);
   writeln('Bottom and right sliders should not have tick marks');
   repeat

      event(input, er);
      if er.etype = etsldpos then
         writeln('Slider id: ', er.sldpid:1, ' position: ', er.sldpos:1);
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { ********************** Graphical slider test *********************** }

   setframe;

   page;
   writeln('Graphical slider test');
   writeln;
   writeln('Bottom and right sliders should not have tick marks');
   writeln;
   ox := trunc(maxyg(output)*0.125);
   oy := curyg(output);
   slidehorizsizg(output, xs, ys);
   xs := trunc(maxxg(output)*0.25);
   slidehorizg(output, ox, oy, ox+xs-1, oy+ys-1, 10, 1);
   oy := oy+trunc(maxyg(output)*0.25);
   slidehorizg(output, ox, oy, ox+xs-1, oy+ys-1, 0, 2);
   x := xs; { save slider size x }
   slidevertsizg(output, xs, ys);

   ox := ox+x+ox; { offset past horizontals }
   oy := curyg(output); { reset to top }
   ys := trunc(maxxg(output)*0.25);

   slidevertg(output, ox, oy, ox+xs-1, oy+ys-1, 10, 3);
   ox := ox+trunc(maxxg(output)*0.125);
   slidevertg(output, ox, oy, ox+xs-1, oy+ys-1, 0, 4);
   repeat

      event(input, er);
      if er.etype = etsldpos then
         writeln('Slider id: ', er.sldpid:1, ' position: ', er.sldpos:1);
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { ********************** Terminal tab bar test *********************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal tab bar test');
   writeln;

   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, totop, 20, 2, x, y, ox, oy);
   tabbar(output, 15, 3, 15+x-1, 3+y-1, lp, totop, 1);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, toright, 4, 12, x, y, ox, oy);
   tabbar(output, 37, 7, 37+x-1, 7+y-1, lp, toright, 2);

   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, tobottom, 20, 2, x, y, ox, oy);
   tabbar(output, 15, 19, 15+x-1, 19+y-1, lp, tobottom, 3);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, toleft, 4, 12, x, y, ox, oy);
   tabbar(output, 5, 7, 5+x-1, 7+y-1, lp, toleft, 4);

   repeat

      event(input, er);
      if er.etype = ettabbar then begin

         if er.tabid = 1 then begin

            if er.tabsel = 1 then writeln('Top bar: You selected Left')
            else if er.tabsel = 2 then writeln('Top bar: You selected Center')
            else if er.tabsel = 3 then writeln('Top bar: You selected Right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 2 then begin

            if er.tabsel = 1 then writeln('Right bar: You selected Top')
            else if er.tabsel = 2 then writeln('Right bar: You selected Center')
            else if er.tabsel = 3 then writeln('Right bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 3 then begin

            if er.tabsel = 1 then writeln('Bottom bar: You selected Left')
            else if er.tabsel = 2 then writeln('Bottom bar: You selected Center')
            else if er.tabsel = 3 then writeln('Bottom bar: You selected right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 4 then begin

            if er.tabsel = 1 then writeln('Left bar: You selected Top')
            else if er.tabsel = 2 then writeln('Left bar: You selected Center')
            else if er.tabsel = 3 then writeln('Left bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else writeln('!!! Bad tab id !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { ********************** Graphical tab bar test ********************** }

   setframe;

   page;
   writeln('Graphical tab bar test');
   writeln;

   ox := trunc(maxyg(output)*0.3);
   oy := curyg(output);
   csx := trunc(maxyg(output)*0.5);
   csy := trunc(maxyg(output)*0.1);

   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, totop, csx, csy, xs, ys, cox, coy);
   tabbarg(output, ox, oy, ox+xs-1, oy+ys-1, lp, totop, 1);
   y := oy+ys-1;

   ox := trunc(maxxg(output)*0.5);
   oy := y;
   csx := trunc(maxyg(output)*0.1);
   csy := trunc(maxyg(output)*0.5);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, toright, csx, csy, xs, ys, cox, coy);
   tabbarg(output, ox, oy, ox+xs-1, oy+ys-1, lp, toright, 2);

   ox := trunc(maxyg(output)*0.3);
   oy := oy+ys-1;
   csx := trunc(maxyg(output)*0.5);
   csy := trunc(maxyg(output)*0.1);

   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, tobottom, csx, csy, xs, ys, cox, coy);
   tabbarg(output, ox, oy, ox+xs-1, oy+ys-1, lp, tobottom, 3);

   ox := trunc(maxxg(output)*0.05);
   oy := y;
   csx := trunc(maxyg(output)*0.1);
   csy := trunc(maxyg(output)*0.5);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, toleft, csx, csy, xs, ys, cox, coy);
   tabbarg(output, ox, oy, ox+xs-1, oy+ys-1, lp, toleft, 4);

   repeat

      event(input, er);
      if er.etype = ettabbar then begin

         if er.tabid = 1 then begin

            if er.tabsel = 1 then writeln('Top bar: You selected Left')
            else if er.tabsel = 2 then writeln('Top bar: You selected Center')
            else if er.tabsel = 3 then writeln('Top bar: You selected Right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 2 then begin

            if er.tabsel = 1 then writeln('Right bar: You selected Top')
            else if er.tabsel = 2 then writeln('Right bar: You selected Center')
            else if er.tabsel = 3 then writeln('Right bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 3 then begin

            if er.tabsel = 1 then writeln('Bottom bar: You selected Left')
            else if er.tabsel = 2 then writeln('Bottom bar: You selected Center')
            else if er.tabsel = 3 then writeln('Bottom bar: You selected right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 4 then begin

            if er.tabsel = 1 then writeln('Left bar: You selected Top')
            else if er.tabsel = 2 then writeln('Left bar: You selected Center')
            else if er.tabsel = 3 then writeln('Left bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else writeln('!!! Bad tab id !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { ****************** Terminal overlaid tab bar test ****************** }

   setframe;

   page;
   chrgrid;
   binvis(output);
   writeln('Terminal overlaid tab bar test');
   writeln;

   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, totop, 30, 12, x, y, ox, oy);
   tabbar(output, 20-ox, 7-oy, 20+x-ox-1, 7+y-oy-1, lp, totop, 1);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, toright, 30, 12, x, y, ox, oy);
   tabbar(output, 20-ox, 7-oy, 20+x-ox-1, 7+y-oy-1, lp, toright, 2);

   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, tobottom, 30, 12, x, y, ox, oy);
   tabbar(output, 20-ox, 7-oy, 20+x-ox-1, 7+y-oy-1, lp, tobottom, 3);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsiz(output, toleft, 30, 12, x, y, ox, oy);
   tabbar(output, 20-ox, 7-oy, 20+x-ox-1, 7+y-oy-1, lp, toleft, 4);

   repeat

      event(input, er);
      if er.etype = ettabbar then begin

         if er.tabid = 1 then begin

            if er.tabsel = 1 then writeln('Top bar: You selected Left')
            else if er.tabsel = 2 then writeln('Top bar: You selected Center')
            else if er.tabsel = 3 then writeln('Top bar: You selected Right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 2 then begin

            if er.tabsel = 1 then writeln('Right bar: You selected Top')
            else if er.tabsel = 2 then writeln('Right bar: You selected Center')
            else if er.tabsel = 3 then writeln('Right bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 3 then begin

            if er.tabsel = 1 then writeln('Bottom bar: You selected Left')
            else if er.tabsel = 2 then writeln('Bottom bar: You selected Center')
            else if er.tabsel = 3 then writeln('Bottom bar: You selected right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 4 then begin

            if er.tabsel = 1 then writeln('Left bar: You selected Top')
            else if er.tabsel = 2 then writeln('Left bar: You selected Center')
            else if er.tabsel = 3 then writeln('Left bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else writeln('!!! Bad tab id !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { ***************** Graphical overlaid tab bar test ****************** }

   setframe;

   page;
   writeln('Graphical overlaid tab bar test');
   writeln;
   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, totop, 200, 200, x, y, ox, oy);
   tabbarg(output, 200-ox, 100-oy, 200+x-ox, 100+y-oy, lp, totop, 1);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, toright, 200, 200, x, y, ox, oy);
   tabbarg(output, 200-ox, 100-oy, 200+x-ox, 100+y-oy, lp, toright, 2);

   new(lp);
   lp^.str := copy('Right');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Left');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, tobottom, 200, 200, x, y, ox, oy);
   tabbarg(output, 200-ox, 100-oy, 200+x-ox, 100+y-oy, lp, tobottom, 3);

   new(lp);
   lp^.str := copy('Bottom');
   lp^.next := nil;
   new(sp);
   sp^.str := copy('Center');
   sp^.next := lp;
   lp := sp;
   new(sp);
   sp^.str := copy('Top');
   sp^.next := lp;
   lp := sp;
   tabbarsizg(output, toleft, 200, 200, x, y, ox, oy);
   tabbarg(output, 200-ox, 100-oy, 200+x-ox, 100+y-oy, lp, toleft, 4);

   repeat

      event(input, er);
      if er.etype = ettabbar then begin

         if er.tabid = 1 then begin

            if er.tabsel = 1 then writeln('Top bar: You selected Left')
            else if er.tabsel = 2 then writeln('Top bar: You selected Center')
            else if er.tabsel = 3 then writeln('Top bar: You selected Right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 2 then begin

            if er.tabsel = 1 then writeln('Right bar: You selected Top')
            else if er.tabsel = 2 then writeln('Right bar: You selected Center')
            else if er.tabsel = 3 then writeln('Right bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 3 then begin

            if er.tabsel = 1 then writeln('Bottom bar: You selected Left')
            else if er.tabsel = 2 then writeln('Bottom bar: You selected Center')
            else if er.tabsel = 3 then writeln('Bottom bar: You selected right')
            else writeln('!!! Bad select number !!!')

         end else if er.tabid = 4 then begin

            if er.tabsel = 1 then writeln('Left bar: You selected Top')
            else if er.tabsel = 2 then writeln('Left bar: You selected Center')
            else if er.tabsel = 3 then writeln('Left bar: You selected Bottom')
            else writeln('!!! Bad select number !!!')

         end else writeln('!!! Bad tab id !!!')

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   killwidget(output, 1);
   killwidget(output, 2);
   killwidget(output, 3);
   killwidget(output, 4);

   { ************************** Alert test ****************************** }

   setframe;

   page;
   writeln('Alert test');
   writeln;
   writeln('There should be an pa_alert dialog');
   writeln('Both the dialog and this window should be fully reactive');
   alert('This is an important message', 'There has been an event !');
   writeln;
   writeln('Alert dialog should have completed now');
   waitnext;

   { *********************** Color query test *************************** }

   setframe;

   page;
   writeln('Color query test');
   writeln;
   writeln('There should be an pa_color query dialog');
   writeln('Both the dialog and this window should be fully reactive');
   writeln('The pa_color pa_white should be the default selection');
   r := i32max;
   g := i32max;
   b := i32max;
   querycolor(r, g, b);
   writeln;
   writeln('Dialog should have completed now');
   writeln('Colors are: red: ', r:1, ' green: ', g:1, ' blue: ', b:1);
   waitnext;

   { ********************* Open file query test ************************* }

   setframe;

   page;
   writeln('Open file query test');
   writeln;
   writeln('There should be an open file query dialog');
   writeln('Both the dialog and this window should be fully reactive');
   writeln('The dialog should have "myfile.txt" as the default filename');
   copy(s, 'myfile.txt');
   queryopen(s);
   writeln;
   writeln('Dialog should have completed now');
   writeln('Filename is: ', s:*);
   waitnext;

   { ********************* Save file query test ************************* }

   setframe;

   page;
   writeln('Save file query test');
   writeln;
   writeln('There should be an save file query dialog');
   writeln('Both the dialog and this window should be fully reactive');
   writeln('The dialog should have "myfile.txt" as the default filename');
   copy(s, 'myfile.txt');
   querysave(s);
   writeln;
   writeln('Dialog should have completed now');
   writeln('Filename is: ', s:*);
   waitnext;

   { *********************** Find query test **************************** }

   setframe;

   page;
   writeln('Find query test');
   writeln;
   writeln('There should be a find query dialog');
   writeln('Both the dialog and this window should be fully reactive');
   writeln('The dialog should have "mystuff" as the default search string');
   copy(s, 'mystuff');
   optf := [];
   queryfind(s, optf);
   writeln;
   writeln('Dialog should have completed now');
   writeln('Search string is: "', s:*, '"');
   if qfncase in optf then writeln('Case sensitive is on')
   else writeln('Case sensitive is off');
   if qfnup in optf then writeln('Search up')
   else writeln('Search down');
   if qfnre in optf then writeln('Use regular expression')
   else writeln('Use literal expression');
   waitnext;

   { ******************** Find/replace query test *********************** }

   setframe;

   page;
   writeln('Find/replace query test');
   writeln;
   writeln('There should be a find/replace query dialog');
   writeln('Both the dialog and this window should be fully reactive');
   writeln('The dialog should have "bark" as the default search string');
   writeln('and should have "sniff" as the default replacement string');
   copy(ss, 'bark');
   copy(rs, 'sniff');
   optfr := [];
   queryfindrep(ss, rs, optfr);
   writeln;
   writeln('Dialog should have completed now');
   writeln('Search string is: "', ss:*, '"');
   writeln('Replace string is: "', rs:*, '"');
   if qfrcase in optfr then writeln('Case sensitive is on')
   else writeln('Case sensitive is off');
   if qfrup in optfr then writeln('Search/replace up')
   else writeln('Search/replace down');
   if qfrre in optfr then writeln('Regular expressions are on')
   else writeln('Regular expressions are off');
   if qfrfind in optfr then writeln('Mode is find')
   else writeln('Mode is find/replace');
   if qfrallfil in optfr then writeln('Mode is find/replace all in file')
   else writeln('Mode is find/replace first in file');
   if qfralllin in optfr then writeln('Mode is find/replace all on line(s)')
   else writeln('Mode is find/replace first on line(s)');
   waitnext;

   { ************************ Font query test *************************** }

   setframe;

   page;
   writeln('Font query test');
   writeln;
   writeln('There should be a font query dialog');
   writeln('Both the dialog and this window should be fully reactive');
   fc := ftbook;
   fs := chrsizy(output);
   fr := 0; { set foreground to black }
   fg := 0;
   fb := 0;
   br := i32max; { set background to white }
   bg := i32max;
   bb := i32max;
   fe := [];
   queryfont(output, fc, fs, fr, fg, fb, br, bg, bb, fe);
   clears(s);
   fontnam(output, fc, s);
   writeln;
   writeln('Dialog should have completed now');
   writeln('Font code: ', fc:1, '(', s:*, ')');
   writeln('Font size: ', fs:1);
   writeln('Foreground pa_color: Red: ', fr:1, ' Green: ', fg:1,
           ' Blue: ', fb:1);
   writeln('Background pa_color: Red: ', br:1, ' Green: ', bg:1,
           ' Blue: ', bb:1);
   if qfteblink in fe then writeln('Blink');
   if qftereverse in fe then writeln('Reverse');
   if qfteunderline in fe then writeln('Underline');
   if qftesuperscript in fe then writeln('Superscript');
   if qftesubscript in fe then writeln('Subscript');
   if qfteitalic in fe then writeln('Italic');
   if qftebold in fe then writeln('Bold');
   if qftestrikeout in fe then writeln('Strikeout');
   if qftestandout in fe then writeln('Standout');
   if qftecondensed in fe then writeln('Condensed');
   if qfteextended in fe then writeln('Extended');
   if qftexlight in fe then writeln('Xlight');
   if qftelight in fe then writeln('Light');
   if qftexbold in fe then writeln('Xbold');
   if qftehollow in fe then writeln('Hollow');
   if qfteraised in fe then writeln('Raised');
   waitnext;

   99: { terminate }

   page;
   writeln('Test complete')

end.
