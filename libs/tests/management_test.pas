{******************************************************************************
*                                                                             *
*                     WINDOW MANAGEMENT TEST PROGRAM                          *
*                                                                             *
*                    Copyright (C) 2005 Scott A. Moore                        *
*                                                                             *
* Tests text and graphical windows management calls. It is the Pascaline      *
* equivalent of the Ami management_test.c reference test.                     *
*                                                                             *
* The terminate event (etterm) is handled by polling in the wait routines,    *
* which perform a non-local goto to the terminate label (99).                 *
*                                                                             *
******************************************************************************}

program management_test(input, output);

uses graphics,
     services,
     version,
     strings;

label 99; { terminate }

const

   { font codes (from amitk/include/graphics.h: AMI_FONT_TERM..AMI_FONT_TECH) }
   ftterm = 1;          { AMI_FONT_TERM }
   ftbook = 2;          { AMI_FONT_BOOK }
   ftsign = 3;          { AMI_FONT_SIGN }
   fttech = 4;          { AMI_FONT_TECH }
   { standard menu ids (from amitk/include/graphics.h: AMI_SMNEW..AMI_SMMAX) }
   smnew       = 1;     { new file }
   smopen      = 2;     { open file }
   smclose     = 3;     { close file }
   smsave      = 4;     { save file }
   smsaveas    = 5;     { save file as name }
   smpageset   = 6;     { page setup }
   smprint     = 7;     { print }
   smexit      = 8;     { exit program }
   smundo      = 9;     { undo edit }
   smcut       = 10;    { cut selection }
   smpaste     = 11;    { paste selection }
   smdelete    = 12;    { delete selection }
   smfind      = 13;    { find text }
   smfindnext  = 14;    { find next }
   smreplace   = 15;    { replace text }
   smgoto      = 16;    { goto line }
   smselectall = 17;    { select all text }
   smnewwindow = 18;    { new window }
   smtilehoriz = 19;    { tile child windows horizontally }
   smtilevert  = 20;    { tile child windows vertically }
   smcascade   = 21;    { cascade windows }
   smcloseall  = 22;    { close all windows }
   smhelptopic = 23;    { help topics }
   smabout     = 24;    { about this program }
   smmax       = 24;    { maximum defined standard menu entries }
   { program defined menu entries after the standard set }
   smone       = 25;    { smmax+1 }
   smtwo       = 26;    { smmax+2 }
   smthree     = 27;    { smmax+3 }

var

   win2:           text;    { window 2 file }
   win3:           text;    { window 3 file }
   win4:           text;    { window 4 file }
   nilwin:         text;    { never opened, stands in for C NULL parent }
   x, x2, y, y2:   integer;
   ox, oy:         integer; { original size of window }
   fb:             boolean; { front/back flipper }
   er:             evtrec;  { event record }
   mp:             menuptr; { menu pointer }
   ml:             menuptr; { menu list }
   framenum:       integer; { current frame number }
   sm:             menuptr; { submenu list }
   sred:           integer; { state variables }
   sgreen:         integer;
   sblue:          integer;
   mincnt:         integer; { minimize counter }
   maxcnt:         integer; { maximize counter }
   nrmcnt:         integer; { normalize counter }
   i:              integer;
   xs, ys:         integer;
   cs:             integer;
   t, et:          integer;
   c1, c2, c3:     color;
   xr:             integer; { ratio window but parent }

{ ************************************************************************

   Helper routines

   ************************************************************************ }

{ wait return to be pressed, or handle terminate }

procedure waitnext;

var er: evtrec;
    ts: pstring; { title string }

begin

   { set title with frame number (recycled dynamic string) }
   framenum := framenum+1;
   openstring;
   ts := cat('management_test: frame ', ints(framenum));
   title(output, ts^);
   closestring;
   repeat event(input, er) until (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{ wait return to be pressed, or handle terminate, while printing characters }

procedure waitnextprint;

var er: evtrec;

begin

   repeat

      event(input, er);
      if er.etype = etchar then
         writeln('Window: ', er.winid:1, ' char: ', er.echar)

   until (er.etype = etenter) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{ print centered string }

procedure prtcen(y: integer; view s: string);

begin

   cursor(output, (maxx(output) div 2)-(max(s) div 2), y);
   write(s)

end;

{ print centered string graphical }

procedure prtceng(y: integer; view s: string);

begin

   cursorg(output, (maxxg(output) div 2)-(strsiz(output, s) div 2), y);
   write(s)

end;

{ wait time in 100 microseconds }

procedure waittime(t: integer);

var er: evtrec;

begin

   timer(output, 1, t, false);
   repeat event(input, er)
   until (er.etype = ettim) or (er.etype = etterm);
   if er.etype = etterm then goto 99

end;

{ find bit mask value for given bit number (C BIT macro, 1 << b) }

function bitm(b: integer): integer;

var i, r: integer;

begin

   r := 1;
   for i := 1 to b do r := r*2;
   bitm := r

end;

{ append a new menu entry to the given list }

procedure appendmenu(var list: menuptr; m: menuptr);

var lp: menuptr;

begin

   { clear these links for insurance }
   m^.next := nil; { clear next }
   m^.branch := nil; { clear branch }
   if list = nil then list := m { list empty, set as first entry }
   else begin { list non-empty }

      { find last entry in list }
      lp := list; { index 1st on list }
      while lp^.next <> nil do lp := lp^.next;
      lp^.next := m { append at end }

   end

end;

{ create menu entry }

procedure newmenu(var mp: menuptr; onoff, oneof, bar: boolean;
                  id: integer; view face: string);

begin

   new(mp);
   mp^.onoff := onoff;
   mp^.oneof := oneof;
   mp^.bar := bar;
   mp^.id := id;
   mp^.face := copy(face)

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

{ display frame test }

procedure frameinside(view s: string; x, y: integer);

begin

   page;
   fcolor(output, cyan);
   rect(output, 1, 1, x, y);
   line(output, 1, 1, x, y);
   line(output, 1, y, x, 1);
   fcolor(output, black);
   binvis(output);
   writeln(s);
   bover(output)

end;

procedure frametest(view s: string);

var er:   evtrec;
    x, y: integer;

begin

   x := maxxg(output); { set size }
   y := maxyg(output);
   frameinside(s, x, y);
   repeat

      event(input, er); { get next event }
      if er.etype = etredraw then frameinside(s, x, y);
      if er.etype = etresize then begin

         { Save the new dimensions, even if not required. This way we must
           get a resize notification for this test to work. }
         x := maxxg(output);
         y := maxyg(output)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter

end;

{ Finds the largest square that fits into the screen, then applies a ratio to
  that. Used to determine a relative size that fits the screen. }

procedure sqrrat(var xs, ys: integer; rat: real);

begin

   { ratio by screen smallest x-y, then square it up }
   getsizg(output, xs, ys);
   if xs > ys then begin ys := trunc(ys/rat); xs := ys end { square }
   else begin xs := trunc(xs/rat); ys := xs end

end;

function nextcolor(c: color): color;

begin

   if c = magenta then nextcolor := red
   else if c < red then nextcolor := red
   else nextcolor := succ(c)

end;

{ ************************************************************************

   Main program

   ************************************************************************ }

begin

   framenum := 0;

   auto(output, false);
   curvis(output, false);
   write('Managed screen test vs. ', majorver:1, '.', minorver:1);
   if experiment then write('.x');
   writeln;
   writeln;
   scnsiz(output, x, y);
   writeln('Screen size character: x: ', x:1, ' y: ', y:1);
   scnsizg(output, x, y);
   writeln('Screen size pixel: x: ', x:1, ' y: ', y:1);
   writeln;
   getsiz(output, x, y);
   writeln('Window size character: x: ', x:1, ' y: ', y:1);
   getsizg(output, ox, oy);
   writeln('Window size graphical: x: ', ox:1, ' y: ', oy:1);
   writeln;
   writeln('Client size character: x: ', maxx(output):1, ' y: ',
           maxy(output):1);
   writeln('Client size graphical: x: ', maxxg(output):1, ' y: ',
           maxyg(output):1);
   writeln;
   writeln('Hit return in any window to continue for each test');
   waitnext;

   { ************************** Window titling test ************************** }

   title(output, 'This is a mangement test window');
   writeln('The title bar of this window should read: This is a mangement test window');
   prtceng(maxyg(output)-chrsizy(output), 'Window title test');
   waitnext;

   { ************************** Multiple windows ************************** }

   page;
   curvis(output, true);
   prtceng(maxyg(output)-chrsizy(output), 'Multiple window test');
   home(output);
   auto(output, true);
   write('This is the main window');
   writeln;
   writeln('Select back and forth between each window, and make sure the');
   writeln('cursor follows');
   writeln;
   write('Here is the cursor->');
   openwin(input, win2, nilwin, 2);
   writeln(win2, 'This is the second window');
   writeln(win2);
   write(win2, 'Here is the cursor->');
   waitnext;
   writeln;
   writeln('Now enter characters to each window, then end with return');
   waitnextprint;
   close(win2);
   page;
   writeln('Second window now closed');
   waitnext;
   curvis(output, false);
   auto(output, false);

   { ********************* Resize buffer window character ******************** }

   ox := maxx(output);
   oy := maxy(output);
   bcolor(output, white);
   sizbuf(output, 50, 50);
   bcolor(output, cyan);
   page;
   for x := 1 to maxx(output) do write('*');
   cursor(output, 1, maxy(output));
   for x := 1 to maxx(output) do write('*');
   for y := 1 to maxy(output) do begin cursor(output, 1, y); write('*') end;
   for y := 1 to maxy(output) do
      begin cursor(output, maxx(output), y); write('*') end;
   home(output);
   writeln('Buffer should now be 50 by 50 characters, and');
   writeln('painted blue');
   writeln('maxx: ', maxx(output):1, ' maxy: ', maxy(output):1);
   writeln('Open up window to verify this');
   prtcen(maxy(output), 'Buffer resize character test');
   bcolor(output, white);
   waitnext;
   sizbuf(output, ox, oy);

   { *********************** Resize buffer window pixel ********************** }

   ox := maxxg(output);
   oy := maxyg(output);
   sqrrat(xs, ys, 1.3); { find square ratio }
   bcolor(output, white);
   sizbufg(output, xs, ys);
   bcolor(output, cyan);
   page;
   linewidth(output, 20);
   line(output, 1, 1, maxxg(output), 1);
   line(output, 1, 1, 1, maxyg(output));
   line(output, 1, maxyg(output), maxxg(output), maxyg(output));
   line(output, maxxg(output), 1, maxxg(output), maxyg(output));
   writeln('Buffer should now be ', xs:1, ' by ', ys:1, ' pixels, and');
   writeln('painted blue');
   writeln('maxxg: ', maxxg(output):1, ' maxyg: ', maxyg(output):1);
   writeln('Open up window to verify this');
   prtcen(maxy(output), 'Buffer resize graphical test');
   bcolor(output, white);
   waitnext;
   sizbufg(output, ox, oy);

   { ****************** Resize screen with buffer on character *************** }

   ox := maxxg(output);
   oy := maxyg(output);
   for x := 20 to 80 do begin

      setsiz(output, x, 25);
      getsiz(output, x2, y2);
      if (x2 <> x) or (y2 <> 25) then begin

         setsiz(output, 80, 25);
         page;
         writeln('*** Getsiz does not match setsiz, x: ', x2:1, ' y: ', y2:1,
                 ' vs. x: ', x:1, ' y: ', 25:1);
         waitnext;
         goto 99

      end;
      page;
      writeln('Resize screen buffered character');
      writeln;
      writeln('Moving in x');
      waittime(1000)

   end;
   writeln;
   write('Complete');
   waitnext;
   for y := 10 to 50 do begin

      setsiz(output, 80, y);
      getsiz(output, x2, y2);
      if (x2 <> 80) or (y2 <> y) then begin

         setsiz(output, 80, 25);
         page;
         writeln('*** Getsiz does not match setsiz, x: ', x2:1, ' y: ', y2:1,
                 ' vs. x: ', 80:1, ' y: ', y:1);
         writeln('*** Getsiz does not match setsiz');
         waitnext;
         goto 99

      end;
      page;
      writeln('Resize screen buffered character');
      writeln;
      writeln('Moving in y');
      waittime(1000)

   end;
   writeln;
   writeln('Complete');
   waitnext;
   winclientg(output, ox, oy, ox, oy, [wmframe, wmsize, wmsysbar]);
   setsizg(output, ox, oy);

   { ******************** Resize screen with buffer on pixel ***************** }

   ox := maxxg(output);
   oy := maxyg(output);
   sqrrat(xs, ys, 1.5); { find square ratio }
   x := xs;
   while x <= xs*4 do begin

      setsizg(output, x, ys);
      getsizg(output, x2, y2);
      if (x2 <> x) or (y2 <> ys) then begin

         setsiz(output, 80, 25);
         page;
         writeln('*** Getsiz does not match setsiz, x: ', x2:1, ' y: ', y2:1,
                 ' vs. x: ', x:1, ' y: ', ys:1);
         writeln('*** Getsiz does ! match setsiz');
         waitnext;
         goto 99

      end;
      page;
      writeln('Resize screen buffered graphical');
      writeln;
      writeln('Moving in x');
      waittime(100);
      x := x+xs div 64

   end;
   writeln;
   writeln('Complete');
   waitnext;
   y := ys;
   while y <= ys*4 do begin

      setsizg(output, xs, y);
      getsizg(output, x2, y2);
      if (x2 <> xs) or (y2 <> y) then begin

         setsiz(output, 80, 25);
         page;
         writeln('*** Getsiz does not match setsiz, x: ', x2:1, ' y: ', y2:1,
                 ' vs. x: ', 300:1, ' y: ', y:1);
         writeln('*** Getsiz does ! match setsiz');
         waitnext;
         goto 99

      end;
      page;
      writeln('Resize screen buffered graphical');
      writeln;
      writeln('Moving in y');
      waittime(100);
      y := y+ys div 64

   end;
   writeln;
   writeln('Complete');
   waitnext;
   winclientg(output, ox, oy, ox, oy, [wmframe, wmsize, wmsysbar]);
   setsizg(output, ox, oy);

   { ********************************* Front/back test *********************** }

   sqrrat(xs, ys, 8); { find square ratio }
   cs := chrsizy(output); { save the character size }
   page;
   auto(output, false);
   writeln('Position window for font/back test');
   writeln('Then hit space to flip font/back status, or return to stop');
   fb := false; { clear front/back status }
   font(output, ftsign);
   fontsiz(output, ys);

   repeat

      event(input, er);
      if er.etype = etchar then if er.echar = ' ' then begin { flip front/back }

         fb := not fb;
         if fb then begin

            front(output);
            fcolor(output, white);
            prtceng(maxyg(output) div 2-chrsizy(output) div 2, 'Back');
            fcolor(output, black);
            prtceng(maxyg(output) div 2-chrsizy(output) div 2, 'Front')

         end else begin

            back(output);
            fcolor(output, white);
            prtceng(maxyg(output) div 2-chrsizy(output) div 2, 'Front');
            fcolor(output, black);
            prtceng(maxyg(output) div 2-chrsizy(output) div 2, 'Back')

         end

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   home(output);
   fontsiz(output, cs);
   font(output, ftterm);
   auto(output, true);

   { ************************* Frame controls test buffered ****************** }

   page;
   fcolor(output, cyan);
   rect(output, 1, 1, maxxg(output), maxyg(output));
   line(output, 1, 1, maxxg(output), maxyg(output));
   line(output, 1, maxyg(output), maxxg(output), 1);
   fcolor(output, black);
   binvis(output);
   writeln('Ready for frame controls buffered');
   writeln('(Note system may not implement all -- or any frame controls)');
   waitnext;
   frame(output, 0);
   writeln('Entire frame off');
   waitnext;
   frame(output, 1);
   writeln('Entire frame on');
   waitnext;
   sysbar(output, 0);
   writeln('System bar off');
   waitnext;
   sysbar(output, 1);
   writeln('System bar on');
   waitnext;
   sizable(output, 0);
   writeln('Size bars off');
   waitnext;
   sizable(output, 1);
   writeln('Size bars on');
   waitnext;
   bover(output);

   { ************************* Frame controls test unbuffered ****************** }

   buffer(output, 0);
   frametest('Ready for frame controls unbuffered - Resize me!');
   writeln('(Note system may not implement all -- or any frame controls)');
   frame(output, 0);
   frametest('Entire frame off');
   frame(output, 1);
   frametest('Entire frame on');
   sysbar(output, 0);
   frametest('System bar off');
   sysbar(output, 1);
   frametest('System bar on');
   sizable(output, 0);
   frametest('Size bars off');
   sizable(output, 1);
   frametest('Size bars on');
   buffer(output, 1);

   { ********************************* Menu test ***************************** }

   auto(output, true);
   page;
   fcolor(output, cyan);
   rect(output, 1, 1, maxxg(output), maxyg(output));
   line(output, 1, 1, maxxg(output), maxyg(output));
   line(output, 1, maxyg(output), maxxg(output), 1);
   fcolor(output, black);
   ml := nil; { clear menu list }
   newmenu(mp, false, false, false, 1, 'Say hello');
   appendmenu(ml, mp);
   newmenu(mp, true, false, true, 2, 'Bark');
   appendmenu(ml, mp);
   newmenu(mp, false, false, false, 3, 'Walk');
   appendmenu(ml, mp);
   newmenu(sm, false, false, false, 4, 'Sublist');
   appendmenu(ml, sm);
   { these are one/of buttons }
   newmenu(mp, false, true, false, 5, 'slow');
   appendmenu(sm^.branch, mp);
   newmenu(mp, false, true, false, 6, 'medium');
   appendmenu(sm^.branch, mp);
   newmenu(mp, false, false, true, 7, 'fast');
   appendmenu(sm^.branch, mp);
   { these are on/off buttons }
   newmenu(mp, true, false, false, 8, 'red');
   appendmenu(sm^.branch, mp);
   newmenu(mp, true, false, false, 9, 'green');
   appendmenu(sm^.branch, mp);
   newmenu(mp, true, false, false, 10, 'blue');
   appendmenu(sm^.branch, mp);
   menu(output, ml);
   menuena(output, 3, 0); { disable "Walk" }
   menusel(output, 5, 1); { turn on "slow" }
   menusel(output, 8, 1); { turn on "red" }

   home(output);
   writeln('Use sample menu above');
   writeln('''Walk'' is disabled');
   writeln('''Sublist'' is a dropdown');
   writeln('''slow'', ''medium'' and ''fast'' are a one/of list');
   writeln('''red'', ''green'' and ''blue'' are on/off');
   writeln('There should be a bar between slow-medium-fast groups and');
   writeln('red-green-blue groups.');
   sred := 1; { set states }
   sgreen := 0;
   sblue := 0;
   repeat

      event(input, er);
      if er.etype = etterm then goto 99;
      if er.etype = etmenus then begin

         write('Menu select: ');
         case er.menuid of

            1:  writeln('Say hello');
            2:  writeln('Bark');
            3:  writeln('Walk');
            4:  writeln('Sublist');
            5:  begin writeln('slow'); menusel(output, 5, 1) end;
            6:  begin writeln('medium'); menusel(output, 6, 1) end;
            7:  begin writeln('fast'); menusel(output, 7, 1) end;
            8:  begin writeln('red'); sred := 1-sred;
                      menusel(output, 8, sred) end;
            9:  begin writeln('green'); sgreen := 1-sgreen;
                      menusel(output, 9, sgreen) end;
            10: begin writeln('blue'); sblue := 1-sblue;
                      menusel(output, 10, sblue) end

         else writeln

         end

      end

   until (er.etype = etenter) or (er.etype = etterm);
   menu(output, nil);

   { ****************************** Standard menu test ******************** }

   page;
   auto(output, true);
   ml := nil; { clear menu list }
   newmenu(mp, false, false, false, smone, 'one');
   appendmenu(ml, mp);
   newmenu(mp, true, false, true, smtwo, 'two');
   appendmenu(ml, mp);
   newmenu(mp, false, false, false, smthree, 'three');
   appendmenu(ml, mp);
   stdmenu(bitm(smnew)+bitm(smopen)+bitm(smclose)+
           bitm(smsave)+bitm(smsaveas)+bitm(smpageset)+
           bitm(smprint)+bitm(smexit)+bitm(smundo)+
           bitm(smcut)+bitm(smpaste)+bitm(smdelete)+
           bitm(smfind)+bitm(smfindnext)+bitm(smreplace)+
           bitm(smgoto)+bitm(smselectall)+bitm(smnewwindow)+
           bitm(smtilehoriz)+bitm(smtilevert)+bitm(smcascade)+
           bitm(smcloseall)+bitm(smhelptopic)+bitm(smabout),
           mp, ml);
   menu(output, mp);
   writeln('Standard menu appears above');
   writeln('Check our ''one'', ''two'', ''three'' buttons are in the program');
   writeln('defined position');
   repeat

      event(input, er);
      if er.etype = etterm then goto 99;
      if er.etype = etmenus then begin

         write('Menu select: ');
         case er.menuid of

            smnew:       writeln('new');
            smopen:      writeln('open');
            smclose:     writeln('close');
            smsave:      writeln('save');
            smsaveas:    writeln('saveas');
            smpageset:   writeln('pageset');
            smprint:     writeln('print');
            smexit:      writeln('exit');
            smundo:      writeln('undo');
            smcut:       writeln('cut');
            smpaste:     writeln('paste');
            smdelete:    writeln('delete');
            smfind:      writeln('find');
            smfindnext:  writeln('findnext');
            smreplace:   writeln('replace');
            smgoto:      writeln('goto');
            smselectall: writeln('selectall');
            smnewwindow: writeln('newwindow');
            smtilehoriz: writeln('tilehoriz');
            smtilevert:  writeln('tilevert');
            smcascade:   writeln('cascade');
            smcloseall:  writeln('closeall');
            smhelptopic: writeln('helptopic');
            smabout:     writeln('about');
            smone:       writeln('one');
            smtwo:       writeln('two');
            smthree:     writeln('three')

         else writeln

         end

      end

   until (er.etype = etenter) or (er.etype = etterm);
   menu(output, nil);

   { ************************* Child windows test character ****************** }

   page;
   chrgrid;
   prtcen(maxy(output), 'Child windows test character');
   openwin(input, win2, output, 2);
   curvis(win2, false);
   setpos(win2, 1, 10);
   sizbuf(win2, 20, 10);
   setsiz(win2, 20, 10);
   openwin(input, win3, output, 3);
   curvis(win3, false);
   setpos(win3, 21, 10);
   sizbuf(win3, 20, 10);
   setsiz(win3, 20, 10);
   openwin(input, win4, output, 4);
   curvis(win4, false);
   setpos(win4, 41, 10);
   sizbuf(win4, 20, 10);
   setsiz(win4, 20, 10);
   bcolor(win2, cyan);
   page(win2);
   writeln(win2, 'I am child window 1');
   bcolor(win3, yellow);
   page(win3);
   writeln(win3, 'I am child window 2');
   bcolor(win4, magenta);
   page(win4);
   writeln(win4, 'I am child window 3');
   home(output);
   writeln('There should be 3 labeled child windows below, with frames   ');
   writeln('(the system may not implement frames on child windows)      ');
   waitnext;
   frame(win2, 0);
   frame(win3, 0);
   frame(win4, 0);
   home(output);
   writeln('There should be 3 labeled child windows below, without frames');
   writeln('                                                            ');
   waitnext;
   close(win2);
   close(win3);
   close(win4);
   home(output);
   writeln('Child windows should all be closed                           ');
   waitnext;

   { *************************** Child windows test pixel ******************** }

   page;
   sqrrat(xs, ys, 2.5); { find square ratio }
   prtcen(maxy(output), 'Child windows test pixel');
   openwin(input, win2, output, 2);
   curvis(win2, false);
   setposg(win2, xs*0+1, trunc(ys/2.5));
   sizbufg(win2, xs, ys);
   setsizg(win2, xs, ys);
   openwin(input, win3, output, 3);
   curvis(win3, false);
   setposg(win3, xs*1+1, trunc(ys/2.5));
   sizbufg(win3, xs, ys);
   setsizg(win3, xs, ys);
   openwin(input, win4, output, 4);
   curvis(win4, false);
   setposg(win4, xs*2+1, trunc(ys/2.5));
   sizbufg(win4, xs, ys);
   setsizg(win4, xs, ys);
   bcolor(win2, cyan);
   page(win2);
   writeln(win2, 'I am child window 1');
   bcolor(win3, yellow);
   page(win3);
   writeln(win3, 'I am child window 2');
   bcolor(win4, magenta);
   page(win4);
   writeln(win4, 'I am child window 3');
   home(output);
   writeln('There should be 3 labled child windows below, with frames   ');
   writeln('(the system may not implement frames on child windows)      ');
   waitnext;
   frame(win2, 0);
   frame(win3, 0);
   frame(win4, 0);
   home(output);
   writeln('There should be 3 labled child windows below, without frames');
   writeln('                                                            ');
   waitnext;
   close(win2);
   close(win3);
   close(win4);
   home(output);
   writeln('Child windows should all be closed                          ');
   writeln('                                                            ');
   waitnext;

   { *************** Child windows independent test character ************ }

   curvis(output, true);
   page;
   chrgrid;
   prtcen(maxy(output), 'Child windows independent test character');
   openwin(input, win2, output, 2);
   setpos(win2, 11, 10);
   sizbuf(win2, 30, 10);
   setsiz(win2, 30, 10);
   openwin(input, win3, output, 3);
   setpos(win3, 41, 10);
   sizbuf(win3, 30, 10);
   setsiz(win3, 30, 10);
   bcolor(win2, cyan);
   page(win2);
   writeln(win2, 'I am child window 1');
   bcolor(win3, yellow);
   page(win3);
   writeln(win3, 'I am child window 2');
   home(output);
   writeln('There should be 2 labeled child windows below, with frames   ');
   writeln('(the system may not implement frames on child windows)       ');
   writeln('Test focus can be moved between windows, including the main  ');
   writeln('window. Test windows can be minimized and maximized          ');
   writeln('(if framed), test entering characters to windows.            ');
   repeat

      event(input, er); { get next event }
      if er.etype = etchar then begin

         if er.winid = 2 then write(win2, er.echar)
         else if er.winid = 3 then write(win3, er.echar)

      end else if er.etype = etenter then begin

         { translate the crs so we can test scrolling }
         if er.winid = 2 then writeln(win2)
         else if er.winid = 3 then writeln(win3)

      end else if er.etype = etterm then
         { only take terminations from main window }
         if er.winid = 1 then goto 99

   { terminate on cr to the main window only }
   until (er.etype = etenter) and (er.winid = 1);
   close(win2);
   close(win3);
   home(output);
   writeln('Child windows should all be closed                           ');
   writeln('                                                             ');
   writeln('                                                             ');
   writeln('                                                             ');
   writeln('                                                             ');
   curvis(output, false);
   waitnext;

   { ******************** Child windows independent test pixel ************** }

   page;
   sqrrat(xs, ys, 2); { find square ratio }
   prtcen(maxy(output), 'Child windows test pixel');
   openwin(input, win2, output, 2);
   setposg(win2, xs*0+xs div 5, ys div 2);
   sizbufg(win2, xs, ys);
   setsizg(win2, xs, ys);
   openwin(input, win3, output, 3);
   setposg(win3, xs*1+xs div 5, ys div 2);
   sizbufg(win3, xs, ys);
   setsizg(win3, xs, ys);
   bcolor(win2, cyan);
   page(win2);
   writeln(win2, 'I am child window 1');
   bcolor(win3, yellow);
   page(win3);
   writeln(win3, 'I am child window 2');
   home(output);
   writeln('There should be 2 labeled child windows below, with frames   ');
   writeln('(the system may not implement frames on child windows)      ');
   writeln('Test focus can be moved between windows, test windows can be ');
   writeln('minimized and maximized (if framed), test entering           ');
   writeln('characters to windows.                                       ');
   repeat

      event(input, er); { get next event }
      if er.etype = etchar then begin

         if er.winid = 2 then write(win2, er.echar)
         else if er.winid = 3 then write(win3, er.echar)

      end else if er.etype = etenter then begin

         { translate the crs so we can test scrolling }
         if er.winid = 2 then writeln(win2)
         else if er.winid = 3 then writeln(win3)

      end else if er.etype = etterm then
         { only take terminations from main window }
         if er.winid = 1 then goto 99

   { terminate on cr to the main window only }
   until (er.etype = etenter) and (er.winid = 1);
   close(win2);
   close(win3);
   home(output);
   writeln('Child windows should all be closed                          ');
   writeln('                                                            ');
   writeln('                                                            ');
   writeln('                                                            ');
   writeln('                                                            ');
   waitnext;

   { ******************* Child windows stacking test pixel ******************* }

   page;
   sqrrat(xs, ys, 2.5); { find square ratio }
   prtcen(maxy(output), 'Child windows stacking test pixel');
   openwin(input, win2, output, 2);
   curvis(win2, false);
   setposg(win2, (xs div 2)*0+xs div 5, trunc(ys/2.5)+(ys*0) div 4);
   sizbufg(win2, xs, ys);
   setsizg(win2, xs, ys);
   openwin(input, win3, output, 3);
   curvis(win3, false);
   setposg(win3, (xs div 2)*1+xs div 5, trunc(ys/2.5)+(ys*1) div 4);
   sizbufg(win3, xs, ys);
   setsizg(win3, xs, ys);
   openwin(input, win4, output, 4);
   curvis(win4, false);
   setposg(win4, (xs div 2)*2+xs div 5, trunc(ys/2.5)+(ys*2) div 4);
   sizbufg(win4, xs, ys);
   setsizg(win4, xs, ys);
   bcolor(win2, cyan);
   page(win2);
   writeln(win2, 'I am child window 1');
   bcolor(win3, yellow);
   page(win3);
   writeln(win3, 'I am child window 2');
   bcolor(win4, magenta);
   page(win4);
   writeln(win4, 'I am child window 3');
   home(output);
   writeln('There should be 3 labled child windows below, overlapped,   ');
   writeln('with child 1 on the bottom, child 2 middle, and child 3 top.');
   waitnext;
   back(win2);
   back(win3);
   back(win4);
   home(output);
   writeln('Now the windows are reordered, with child 1 on top, child 2 ');
   writeln('below that, and child 3 on the bottom.                      ');
   waitnext;
   front(win2);
   front(win3);
   front(win4);
   home(output);
   writeln('Now the windows are reordered, with child 3 on top, child 2 ');
   writeln('below that, and child 1 on the bottom.                      ');
   waitnext;
   close(win2);
   close(win3);
   close(win4);
   page;
   writeln('Child windows should all be closed                          ');
   waitnext;

   { ************** Child windows stacking resize test pixel 1 *************** }

   sqrrat(xs, ys, 5); { find square ratio }
   buffer(output, 0);
   auto(output, false);
   openwin(input, win2, output, 2);
   setposg(win2, (xs div 2)*1, (ys div 2)*1);
   sizbufg(win2, maxxg(output)-xs*2, maxyg(output)-ys*2);
   setsizg(win2, maxxg(output)-xs*2, maxyg(output)-ys*2);
   openwin(input, win3, output, 3);
   setposg(win3, (xs div 2)*2, (ys div 2)*2);
   sizbufg(win3, maxxg(output)-xs*2, maxyg(output)-ys*2);
   setsizg(win3, maxxg(output)-xs*2, maxyg(output)-ys*2);
   openwin(input, win4, output, 4);
   setposg(win4, (xs div 2)*3, (ys div 2)*3);
   sizbufg(win4, maxxg(output)-xs*2, maxyg(output)-ys*2);
   setsizg(win4, maxxg(output)-xs*2, maxyg(output)-ys*2);
   curvis(win2, false);
   bcolor(win2, cyan);
   page(win2);
   writeln(win2, 'I am child window 1');
   curvis(win3, false);
   bcolor(win3, yellow);
   page(win3);
   writeln(win3, 'I am child window 2');
   curvis(win4, false);
   bcolor(win4, magenta);
   page(win4);
   writeln(win4, 'I am child window 3');
   repeat

      event(input, er);
      if (er.etype = etredraw) or (er.etype = etresize) then begin

         page;
         prtceng(maxyg(output)-chrsizy(output),
                 'Child windows stacking resize test pixel 1');
         prtceng(1, 'move and resize');
         setsizg(win3, maxxg(output)-xs*2, maxyg(output)-ys*2);
         setsizg(win4, maxxg(output)-xs*2, maxyg(output)-ys*2);
         setsizg(win2, maxxg(output)-xs*2, maxyg(output)-ys*2)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   close(win2);
   close(win3);
   close(win4);
   buffer(output, 1);
   page;
   writeln('Child windows should all be closed                          ');
   waitnext;

   { ************** Child windows stacking resize test pixel 2 *************** }

   sqrrat(xs, ys, 20); { find square ratio }
   buffer(output, 0);
   openwin(input, win2, output, 2);
   auto(win2, false);
   curvis(win2, false);
   setposg(win2, xs*1, ys*1);
   sizbufg(win2, strsiz(win2, 'I am child window 1'), chrsizy(win2));
   setsizg(win2, maxxg(output)-xs*1*2, maxyg(output)-ys*1*2);
   openwin(input, win3, output, 3);
   auto(win3, false);
   curvis(win3, false);
   setposg(win3, xs*2, ys*2);
   sizbufg(win2, strsiz(win3, 'I am child window 2'), chrsizy(win3));
   setsizg(win3, maxxg(output)-xs*2*2, maxyg(output)-ys*2*2);
   openwin(input, win4, output, 4);
   auto(win4, false);
   curvis(win4, false);
   setposg(win4, xs*3, ys*3);
   sizbufg(win2, strsiz(win4, 'I am child window 3'), chrsizy(win4));
   setsizg(win4, maxxg(output)-xs*3*2, maxyg(output)-ys*3*2);
   bcolor(win2, cyan);
   page(win2);
   write(win2, 'I am child window 1');
   bcolor(win3, yellow);
   page(win3);
   write(win3, 'I am child window 2');
   bcolor(win4, magenta);
   page(win4);
   write(win4, 'I am child window 3');
   repeat

      event(input, er);
      if (er.etype = etredraw) or (er.etype = etresize) then begin

         page;
         prtceng(maxyg(output)-chrsizy(output),
                 'Child windows stacking resize test pixel 2');
         prtceng(1, 'move and resize');
         setsizg(win2, maxxg(output)-xs*1*2, maxyg(output)-ys*1*2);
         setsizg(win3, maxxg(output)-xs*2*2, maxyg(output)-ys*2*2);
         setsizg(win4, maxxg(output)-xs*3*2, maxyg(output)-ys*3*2)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   close(win2);
   close(win3);
   close(win4);
   buffer(output, 1);
   page;
   writeln('Child windows should all be closed                          ');
   waitnext;

   { ******************************* Buffer off test *********************** }

   page;
   cs := chrsizy(output); { save the character size }
   auto(output, false);
   buffer(output, 0);
   { initialize prime size information }
   x := maxxg(output);
   y := maxyg(output);
   linewidth(output, 5); { set large lines }
   font(output, ftsign);
   binvis(output);
   repeat

      event(input, er); { get next event }
      if (er.etype = etredraw) or (er.etype = etresize) then begin

         { clear screen without overwriting frame }
         fcolor(output, white);
         frect(output, 1+5, 1+5, x-5, y-5);
         fcolor(output, black);
         fontsiz(output, y div 10);
         prtceng(maxyg(output) div 2-chrsizy(output) div 2,
                 'SIZE AND COVER ME !');
         rect(output, 1+2, 1+2, x-2, y-2) { frame the window }

      end;
      if er.etype = etresize then begin

         { Save the new demensions, even if not required. This way we must
           get a resize notification for this test to work. }
         x := maxxg(output);
         y := maxyg(output)

      end;
      if er.etype = etterm then goto 99

   until er.etype = etenter;
   buffer(output, 1);
   fontsiz(output, cs);
   font(output, ftterm);
   home(output);
   auto(output, true);

   { ****************************** min/max/norm test ********************* }

   page;
   auto(output, false);
   buffer(output, 0);
   font(output, ftterm);
   mincnt := 0; { clear minimize counter }
   maxcnt := 0; { clear maximize counter }
   nrmcnt := 0; { clear normalize counter }
   repeat

      event(input, er); { get next event }
      { count minimize, maximize, normalize }
      if er.etype = etmax then maxcnt := maxcnt+1;
      if er.etype = etmin then mincnt := mincnt+1;
      if er.etype = etnorm then nrmcnt := nrmcnt+1;
      if (er.etype = etredraw) or (er.etype = etmax) or
         (er.etype = etmin) or (er.etype = etnorm) then begin

         page;
         writeln('Minimize, maximize and restore this window');
         writeln;
         writeln('Minimize count:  ', mincnt:1);
         writeln('Maximize count:  ', maxcnt:1);
         writeln('Normalize count: ', nrmcnt:1)

      end;

      if er.etype = etterm then goto 99

   until er.etype = etenter;
   buffer(output, 1);

   { ******************** Window size calculate character ***************** }

   page;
   prtceng(maxyg(output)-chrsizy(output), 'Window size calculate character');
   home(output);
   openwin(input, win2, nilwin, 2);
   linewidth(output, 1);

   winclient(output, 20, 10, x, y, [wmframe, wmsize, wmsysbar]);
   writeln('For (20, 10) client, full frame, window size is: ', x:1, ',', y:1);
   setsiz(win2, x, y);
   page(win2);
   fcolor(win2, black);
   writeln(win2, '12345678901234567890');
   writeln(win2, '2');
   writeln(win2, '3');
   writeln(win2, '4');
   writeln(win2, '5');
   writeln(win2, '6');
   writeln(win2, '7');
   writeln(win2, '8');
   writeln(win2, '9');
   writeln(win2, '0');
   fcolor(win2, cyan);
   rect(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 10*chrsizy(win2), 20*chrsizx(win2), 1);
   curvis(win2, false);
   writeln('Check client window has (20, 10) surface');
   waitnext;

   writeln('System bar off');
   sysbar(win2, 0);
   winclient(output, 20, 10, x, y, [wmframe, wmsize]);
   writeln('For (20, 10) client, no system bar, window size is: ', x:1, ',',
           y:1);
   setsiz(win2, x, y);
   page(win2);
   fcolor(win2, black);
   writeln(win2, '12345678901234567890');
   writeln(win2, '2');
   writeln(win2, '3');
   writeln(win2, '4');
   writeln(win2, '5');
   writeln(win2, '6');
   writeln(win2, '7');
   writeln(win2, '8');
   writeln(win2, '9');
   writeln(win2, '0');
   fcolor(win2, cyan);
   rect(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 10*chrsizy(win2), 20*chrsizx(win2), 1);
   curvis(win2, false);
   writeln('Check client window has (20, 10) surface');
   waitnext;

   writeln('Sizing bars off');
   sysbar(win2, 1);
   sizable(win2, 0);
   winclient(output, 20, 10, x, y, [wmframe, wmsysbar]);
   writeln('For (20, 10) client, no size bars, window size is: ', x:1, ',',
           y:1);
   setsiz(win2, x, y);
   page(win2);
   fcolor(win2, black);
   writeln(win2, '12345678901234567890');
   writeln(win2, '2');
   writeln(win2, '3');
   writeln(win2, '4');
   writeln(win2, '5');
   writeln(win2, '6');
   writeln(win2, '7');
   writeln(win2, '8');
   writeln(win2, '9');
   writeln(win2, '0');
   fcolor(win2, cyan);
   rect(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 10*chrsizy(win2), 20*chrsizx(win2), 1);
   curvis(win2, false);
   writeln('Check client window has (20, 10) surface');
   waitnext;

   writeln('frame off');
   sysbar(win2, 1);
   sizable(win2, 1);
   frame(win2, 0);
   winclient(output, 20, 10, x, y, [wmsize, wmsysbar]);
   writeln('For (20, 10) client, no frame, window size is: ', x:1, ',', y:1);
   setsiz(win2, x, y);
   page(win2);
   fcolor(win2, black);
   writeln(win2, '12345678901234567890');
   writeln(win2, '2');
   writeln(win2, '3');
   writeln(win2, '4');
   writeln(win2, '5');
   writeln(win2, '6');
   writeln(win2, '7');
   writeln(win2, '8');
   writeln(win2, '9');
   writeln(win2, '0');
   fcolor(win2, cyan);
   rect(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 1, 20*chrsizx(win2), 10*chrsizy(win2));
   line(win2, 1, 10*chrsizy(win2), 20*chrsizx(win2), 1);
   curvis(win2, false);
   writeln('Check client window has (20, 10) surface');
   waitnext;

   close(win2);

   { ************************ Window size calculate pixel ******************** }

   page;
   xr := maxxg(output) div 3; { ratio window but parent }
   prtceng(maxyg(output)-chrsizy(output), 'Window size calculate pixel');
   home(output);
   openwin(input, win2, nilwin, 2);
   linewidth(output, 1);
   fcolor(win2, cyan);
   winclientg(output, xr, xr, x, y, [wmframe, wmsize, wmsysbar]);
   writeln('For (', xr:1, ', ', xr:1,
           ') client, full frame, window size is: ', x:1, ',', y:1);
   setsizg(win2, x, y);
   rect(win2, 1, 1, xr, xr);
   line(win2, 1, 1, xr, xr);
   line(win2, 1, xr, xr, 1);
   curvis(win2, false);
   writeln('Check client window has (', xr:1, ', ', xr:1, ') surface');
   waitnext;

   writeln('System bar off');
   sysbar(win2, 0);
   winclientg(output, xr, xr, x, y, [wmframe, wmsize]);
   writeln('For (', xr:1, ', ', xr:1,
           ') client, no system bar, window size is: ', x:1, ',', y:1);
   setsizg(win2, x, y);
   page(win2);
   rect(win2, 1, 1, xr, xr);
   line(win2, 1, 1, xr, xr);
   line(win2, 1, xr, xr, 1);
   writeln('Check client window has (', xr:1, ', ', xr:1, ') surface');
   waitnext;

   writeln('Sizing bars off');
   sysbar(win2, 1);
   sizable(win2, 0);
   winclientg(output, xr, xr, x, y, [wmframe, wmsysbar]);
   writeln('For (', xr:1, ', ', xr:1,
           ') client, no sizing, window size is: ', x:1, ',', y:1);
   setsizg(win2, x, y);
   page(win2);
   rect(win2, 1, 1, xr, xr);
   line(win2, 1, 1, xr, xr);
   line(win2, 1, xr, xr, 1);
   writeln('Check client window has (', xr:1, ', ', xr:1, ') surface');
   waitnext;

   writeln('frame off');
   sysbar(win2, 1);
   sizable(win2, 1);
   frame(win2, 0);
   winclientg(output, xr, xr, x, y, [wmsize, wmsysbar]);
   writeln('For (', xr:1, ', ', xr:1,
           ') client, no frame, window size is: ', x:1, ',', y:1);
   setsizg(win2, x, y);
   page(win2);
   rect(win2, 1, 1, xr, xr);
   line(win2, 1, 1, xr, xr);
   line(win2, 1, xr, xr, 1);
   writeln('Check client window has (', xr:1, ', ', xr:1, ') surface');
   waitnext;

   close(win2);

   { ******************* Window size calculate minimums pixel *************** }

   { this test does not work, winclient needs to return the minimums;
     it is disabled in the C original as well }

   { ********************** Child windows torture test pixel ***************** }

   getsizg(output, xs, ys); { get window size }
   if xs > ys then begin xs := trunc(xs/3.5); ys := xs end
   else begin ys := trunc(ys/3.5); xs := ys end;
   c1 := red;
   c2 := green;
   c3 := blue;
   page;
   writeln('Child windows torture test pixel');
   t := clock; { get base time }
   for i := 1 to 100 do begin

      openwin(input, win2, output, 2);
      setposg(win2, xs div 10, ys div 5);
      sizbufg(win2, xs, ys);
      setsizg(win2, xs, ys);
      openwin(input, win3, output, 3);
      setposg(win3, xs div 10+xs, ys div 5);
      sizbufg(win3, xs, ys);
      setsizg(win3, xs, ys);
      openwin(input, win4, output, 4);
      setposg(win4, xs div 10+xs*2, ys div 5);
      sizbufg(win4, xs, ys);
      setsizg(win4, xs, ys);
      bcolor(win2, c1);
      c1 := nextcolor(c1);
      page(win2);
      writeln(win2, 'I am child window 1');
      bcolor(win3, c2);
      c2 := nextcolor(c2);
      page(win3);
      writeln(win3, 'I am child window 2');
      bcolor(win4, c3);
      c3 := nextcolor(c3);
      page(win4);
      writeln(win4, 'I am child window 3');
      close(win2);
      close(win3);
      close(win4)

   end;
   et := elapsed(t);
   home(output);
   bover(output);
   writeln('Child windows should all be closed');
   writeln;
   writeln('Child windows place and remove ', 100:1, ' iterations ',
           et*0.0001:1:6, ' seconds');
   writeln(et*0.0001/100:1:6, ' per iteration');
   waitnext;

   99: ; { terminate }

   page;
   auto(output, false);
   font(output, ftsign);
   fontsiz(output, 50);
   prtceng(maxyg(output) div 2-chrsizy(output) div 2, 'Test complete')

end.
