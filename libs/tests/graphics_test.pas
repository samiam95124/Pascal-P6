{******************************************************************************
*                                                                             *
*                          GRAPHICS TEST PROGRAM                              *
*                                                                             *
*                    Copyright (C) 2024 Scott A. Franco                       *
*                                                                             *
* This program performs a reasonably complete test of common features in the  *
* graphics level standard. It is the Pascaline equivalent of the Ami          *
* graphics_test.c reference test.                                            *
*                                                                             *
* The terminate event (etterm) is handled by polling in the wait routines,    *
* which perform a non-local goto to the terminate label (99).                 *
*                                                                             *
******************************************************************************}

program graphics_test(input, output);

uses graphics,
     services,
     version,
     strings;

label 99; { terminate }

const

   s1     = 'Moving string';        { S1 }
   s2     = 'Variable size string'; { S2 }
   s3     = 'Sizing test string';   { S3 }
   s4     = 'Justify test string';  { S4 }
   s5     = 'Invisible body text';  { S5 }
   s6     = 'Example text';         { S6 }
   coldiv = 6;          { number of color divisions }
   colsqr = 20;         { size of color square }
   second = 10000;      { one second in 100 microsecond ticks }
   i32max = 2147483647; { 32 bit INT_MAX, base for angle/RGB ratios }
   degree = 5965232;    { INT_MAX div 360, ticks per degree }
   maxsquare = 10;      { number of animation squares }
   reprate   = 2;       { number of moves per frame }
   { font codes (from amitk/include/graphics.h: AMI_FONT_TERM..AMI_FONT_TECH) }
   ftterm = 1;          { AMI_FONT_TERM }
   ftbook = 2;          { AMI_FONT_BOOK }
   ftsign = 3;          { AMI_FONT_SIGN }
   fttech = 4;          { AMI_FONT_TECH }

type

   { benchmark types, in C 'bench' enum order bnline1..bnpictns }
   bench = (bnline1, bnline10, bnrect1, bnrect10, bnrrect1, bnrrect10,
            bnfrect, bnfrrect, bnellipse1, bnellipse10, bnfellipse,
            bnarc1, bnarc10, bnfarc, bnfchord, bnftriangle,
            bntext, bntextbi, bnpict, bnpictns);
   benchrec = record iter, time: integer end; { benchmark stats record }
   balrec   = record { square animation data record }
                 x, y:   integer; { current position }
                 lx, ly: integer; { last position }
                 xd, yd: integer; { deltas }
                 c:      color     { color }
              end;

var

   framenum:               integer; { current frame number }
   x, y:                   integer;
   xs, ys:                 integer;
   xsize, ysize:           integer;
   xspace, yspace:         integer;
   i:                      integer;
   dx, dy:                 integer;
   ln:                     integer;
   term:                   boolean; { terminate flag for waitchar }
   w:                      integer;
   l:                      integer;
   a:                      integer;
   r, g, b:                integer;
   c, c1, c2:              color;   { ami_color c, c1, c2 }
   lx, ly:                 integer;
   x1, y1, x2, y2:         integer; { rectangle corner work vars }
   tx1, ty1, tx2, ty2, tx3, ty3: integer;
   h:                      integer;
   cnt:                    integer;
   er:                     evtrec;  { ami_evtrec er, event record }
   fsiz:                   integer;
   aa, ab:                 integer;
   s:                      integer;
   bi:                     bench;
   benchtab:               array [bench] of benchrec;
   fns:                    packed array [1..100] of char; { font name buffer }
   fn:                     packed array [1..100] of char; { picture file name }
   ps:                     real;    { font point size accumulator }
   f:                      real;    { radian work value }
   { viewport scaling test work vars }
   cx, cy, ww, wh, gs:     integer;
   vox, voy:               integer;
   vsx, vsy:               real;
   done:                   boolean;
   { random number generator state }
   randstate:              integer;
   { square animation globals (C: squaresize, halfsquare, baltbl) }
   squaresize:             integer;
   halfsquare:             integer;
   baltbl:                 array [1..maxsquare] of balrec;

{ ************************************************************************

   Helper routines

   ************************************************************************ }

{ find the character length of a string (C strlen). max() cannot be applied
  to a const string identifier directly, but works on a view string
  parameter, so this wrapper provides the C strlen idiom for const strings. }

function strlen(view s: string): integer;

begin

   strlen := max(s)

end;

{ True if a right-padded string is blank/empty. Scan from the last position
  left while blank, stopping at the first position; the string is empty only if
  the scan reaches position 1 and that position is also blank. A leading blank
  alone does NOT mean blank: " rst: rasa" is a real name. }

function blank(view s: string): boolean;

var i: integer;

begin

   i := max(s);
   while (i > 1) and (s[i] = ' ') do i := i-1;
   blank := (i = 1) and (s[1] = ' ')

end;

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

{ find random number in given range }

function randr(s, e: integer): integer;

begin

   randr := randn(e-s)+s

end;

{ convert a color ordinal (2..7, red..magenta) to a color value }

function intcolor(o: integer): color;

var cl: color;

begin

   cl := black;
   case o of
      0: cl := black;  1: cl := white;  2: cl := red;
      3: cl := green;  4: cl := blue;   5: cl := cyan;
      6: cl := yellow; 7: cl := magenta
   end;
   intcolor := cl

end;

{ swap two integers }

procedure swap(var a, b: integer);

var t: integer;

begin

   t := a;
   a := b;
   b := t

end;

{ wait time in 100 microseconds, with return to terminate }

procedure waitchar(t: integer; var st: boolean);

var er: evtrec;

begin

   st := false; { set no space terminate }
   timer(output, 1, t, false);
   repeat event(input, er)
   until (er.etype = ettim) or (er.etype = etterm) or
         (er.etype = etchar) or (er.etype = etenter);
   { no short-circuit: nest the char test }
   if er.etype = etchar then if er.echar = ' ' then st := true;
   if er.etype = etenter then st := true;
   if er.etype = etterm then goto 99

end;

{ wait return to be pressed, or handle terminate }

procedure waitnext;

var er: evtrec;
    ts: pstring; { title string }

begin

   { set title with frame number (recycled dynamic string) }
   framenum := framenum+1;
   openstring;
   ts := cat('graphics_test: frame ', ints(framenum));
   title(output, ts^);
   closestring;
   repeat event(input, er) until (er.etype = etenter) or (er.etype = etterm);
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

{ print all printable characters }

procedure prtall;

var c: char;
    s: packed array [1..1] of char;

begin

   for c := ' ' to '}' do begin

      s[1] := 'c';
      if curxg(output)+strsiz(output, s) > maxxg(output) then
         cursorg(output, 1, curyg(output)+chrsizy(output));
      write(c)

   end;
   writeln

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

{ find rectangular coordinates from polar, relative to center of circle,
  with given diameter }

procedure rectcord(a, { angle, 0-359 }
                   r: integer; { radius of circle }
                   var x, y: integer); { returns rectangular coordinate }

var angle: real; { angle in radian measure }

begin

   angle := a*0.01745329; { find radian measure }
   x := round(sin(angle)*r); { find distance x }
   y := round(cos(angle)*r)  { find distance y }

end;

{ draw polar coordinate line }

procedure pline(a, { angle of line }
                o, { length of line }
                cx, { center of circle in x and y }
                cy,
                w: integer); { width of line }

var ex, ey: integer; { line start and end }

begin

   rectcord(a, o, ex, ey); { find endpoint of line }
   linewidth(output, w); { set width }
   line(output, cx, cy, cx+ex, cy-ey) { draw line }

end;

{ draw centered justified text }

procedure justcenter(view s: string; l: integer);

var i, x: integer;

begin

   x := maxxg(output) div 2-l div 2;
   cursorg(output, x, curyg(output));
   writejust(output, s, l);
   writeln;
   fcolor(output, white);
   frect(output, x, curyg(output), x+l-1, curyg(output)+chrsizy(output)-1);
   fcolor(output, black);
   rect(output, x, curyg(output), x+l-1, curyg(output)+chrsizy(output)-1);
   for i := 0 to max(s)-1 do
      line(output, x+justpos(output, s, i, l), curyg(output),
           x+justpos(output, s, i, l), curyg(output)+chrsizy(output)-1);
   writeln

end;

{ draw graphics grid }

procedure grid;

var x, y:    integer;
    xspace:  integer;
    yspace:  integer;
    yrat:    real;

begin

   yrat := dpmy(output)/dpmx(output); { find aspect ratio }
   xspace := maxxg(output) div 60;
   yspace := round(xspace*yrat);
   linewidth(output, 1);
   fcolor(output, cyan);
   x := 10;
   while x <= maxxg(output) do begin

      line(output, x, 1, x, maxyg(output));
      x := x+xspace

   end;
   y := 10;
   while y <= maxyg(output) do begin

      line(output, 1, y, maxxg(output), y);
      y := y+yspace

   end;
   fcolor(output, black)

end;

{ draw screen edge }

procedure edge;

begin

   { draw boundary lines showing valid coordinate space }
   fcolor(output, red);
   line(output, 1, 1, maxxg(output), 1);
   line(output, 1, maxyg(output), maxxg(output), maxyg(output));
   line(output, 1, 1, 1, maxyg(output));
   line(output, maxxg(output), 1, maxxg(output), maxyg(output));
   fcolor(output, black)

end;

{ check for break (frame tick, terminate, char or enter) }

function chkbrk: boolean;

var er:   evtrec; { event record }
    done: boolean;

begin

   done := false;
   repeat event(input, er)
   until (er.etype = etframe) or (er.etype = etterm) or
         (er.etype = etchar) or (er.etype = etenter);
   if er.etype = etterm then goto 99;
   { NO short-circuit: test the two terminating types separately }
   if er.etype = etchar then done := true; { terminate }
   if er.etype = etenter then done := true; { terminate }

   chkbrk := done

end;

{ draw a single animation square }

procedure drawsquare(c: color; x, y: integer);

begin

   fcolor(output, c); { set color }
   frect(output, x-halfsquare+1, y-halfsquare+1, x+halfsquare-1, y+halfsquare-1)

end;

{ move a single square, bouncing off the edges }

procedure movesquare(s: integer);

var nx, ny: integer; { temp coordinate holders }

begin

   nx := baltbl[s].x+baltbl[s].xd; { trial move square }
   ny := baltbl[s].y+baltbl[s].yd;
   { check out of bounds and reverse direction }
   if (nx < halfsquare) or (nx > maxxg(output)-halfsquare+1) then
      baltbl[s].xd := -baltbl[s].xd;
   if (ny < halfsquare) or (ny > maxyg(output)-halfsquare+1) then
      baltbl[s].yd := -baltbl[s].yd;
   baltbl[s].x := baltbl[s].x+baltbl[s].xd; { move square }
   baltbl[s].y := baltbl[s].y+baltbl[s].yd

end;

{ square animation test }

procedure squares;

var cd:   integer; { current display flip select (0/1) }
    i:    integer; { index for table }
    rc:   integer; { repetition counter }
    done: boolean; { done flag }

begin

   squaresize := maxyg(output) div 5;
   halfsquare := squaresize div 2;
   { initialize square data }
   for i := 1 to maxsquare do begin

      baltbl[i].x := randn(maxxg(output)-squaresize)+halfsquare;
      baltbl[i].y := randn(maxyg(output)-squaresize)+halfsquare;
      if randn(1) = 0 then baltbl[i].xd := +1 else baltbl[i].xd := -1;
      if randn(1) = 0 then baltbl[i].yd := +1 else baltbl[i].yd := -1;
      baltbl[i].lx := baltbl[i].x; { set last position to same }
      baltbl[i].ly := baltbl[i].y;
      baltbl[i].c := intcolor(randr(ord(red), ord(magenta))) { random color }

   end;
   curvis(output, false); { turn off cursor }
   cd := 0; { set 1st display }
   { place squares on display }
   for i := 1 to maxsquare do
      drawsquare(baltbl[i].c, baltbl[i].x, baltbl[i].y);
   done := false; { set ! done }
   while not done do begin

      { select display and update surfaces }
      select(output, (1-cd)+1, cd+1);
      page;
      fover(output);
      fcolor(output, black);
      prtcen(maxy(output), 'Animation test');
      fxor(output);
      { save old positions }
      for i := 1 to maxsquare do begin

         baltbl[i].lx := baltbl[i].x; { save last position }
         baltbl[i].ly := baltbl[i].y

      end;
      { move squares }
      for rc := 1 to reprate do { repeats per frame }
         for i := 1 to maxsquare do movesquare(i); { process squares }
      { draw squares }
      for i := 1 to maxsquare do
         drawsquare(baltbl[i].c, baltbl[i].x, baltbl[i].y);
      cd := 1-cd; { flip display and update surfaces }
      done := chkbrk { check complete }

   end;
   select(output, 1, 1); { restore buffer surfaces }
   fover(output) { restore foreground overwrite }

end;

{ draw standard graphical test, which is all the figures possible
  arranged on the screen }

procedure graphtest(lw: integer); { line width }

var fsiz:    integer;
    x, y:    integer;
    xsize:   integer; { total space x for figures }
    xspace:  integer; { x space between figures }
    yspace:  integer; { y space between figures }
    xfigsiz: integer; { size of figure x }
    yfigsiz: integer; { size of figure y }
    yrat:    real;    { aspect ratio of y to x }

begin

   auto(output, false);
   font(output, ftsign);
   fsiz := chrsizy(output); { save character size to restore }
   fontsiz(output, maxyg(output) div 20);
   bcolor(output, yellow);
   cursorg(output, maxxg(output) div 2-strsiz(output, s6) div 2, curyg(output));
   writeln(s6);
   writeln;
   { note: after this we can't use text spacing }

   { we assume x > y }
   yrat := dpmy(output)/dpmx(output); { find aspect ratio }
   xsize := maxxg(output) div 5; { set spacing of figures }
   xspace := xsize div 5; { set x space between figures }
   xfigsiz := xsize-xspace; { net x size of figure }
   yfigsiz := round(xfigsiz*yrat); { net y size of figure }
   yspace := round(xspace*yrat); { set y space between figures }

   { first row of figures }
   fcolor(output, magenta);
   linewidth(output, lw);
   y := curyg(output); { set y location to current }
   x := xspace div 2; { set x location to after space left }
   rect(output, x, y, x+xfigsiz-1, y+yfigsiz-1);
   fcolor(output, green);
   x := x+xfigsiz+xspace;
   frect(output, x, y, x+xfigsiz-1, y+yfigsiz-1);
   fcolor(output, yellow);
   x := x+xfigsiz+xspace;
   ftriangle(output, x, y+yfigsiz-1, x+xfigsiz div 2-1, y,
                     x+xfigsiz-1, y+yfigsiz-1);
   fcolor(output, red);
   x := x+xfigsiz+xspace;
   rrect(output, x, y, x+xfigsiz-1, y+yfigsiz-1, 20, 20);
   fcolor(output, magenta);
   x := x+xfigsiz+xspace;
   arc(output, x, y, x+xfigsiz-1, y+yfigsiz-1, 0, i32max div 4);
   fcolor(output, green);
   farc(output, x, y, x+xfigsiz-1, y+xfigsiz-1,
                      i32max div 2, i32max div 2+i32max div 4);
   y := y+yfigsiz+yspace;
   x := xspace div 2;

   { second row of figures }
   fcolor(output, blue);
   frect(output, x, y, x+xfigsiz-1, y+yfigsiz-1);
   x := x+xfigsiz+xspace;
   fcolor(output, magenta);
   frrect(output, x, y, x+xfigsiz-1, y+yfigsiz-1, 20, 20);
   x := x+xfigsiz+xspace;
   fcolor(output, green);
   ellipse(output, x, y, x+xfigsiz-1, y+yfigsiz-1);
   x := x+xfigsiz+xspace;
   fcolor(output, yellow);
   fellipse(output, x, y, x+xfigsiz-1, y+yfigsiz-1);
   x := x+xfigsiz+xspace;
   fcolor(output, blue);
   fchord(output, x, y, x+xfigsiz-1, y+yfigsiz-1, 0, i32max div 2);
   y := y+xfigsiz+xspace;

   { third row of figures (lines) }
   fcolor(output, red);
   linewidth(output, 1);
   line(output, 20, y, maxxg(output)-20, y);
   y := y+10;
   fcolor(output, green);
   linewidth(output, 3);
   line(output, 20, y, maxxg(output)-20, y);
   y := y+10;
   fcolor(output, blue);
   linewidth(output, 7);
   line(output, 20, y, maxxg(output)-20, y);
   y := y+20;
   fcolor(output, magenta);
   linewidth(output, 15);
   line(output, 20, y, maxxg(output)-20, y);
   linewidth(output, 1);
   fontsiz(output, fsiz); { restore font size }
   fcolor(output, black);
   bcolor(output, white);
   font(output, ftterm)

end;

{ ************************************************************************

   Benchmark speed routines

   ************************************************************************ }

{ test line speed }

procedure linespeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   auto(output, false);
   curvis(output, false);
   page;
   linewidth(output, w);
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      line(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                   randr(1, maxxg(output)), randr(1, maxyg(output)))

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test rectangle speed }

procedure rectspeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   auto(output, false);
   curvis(output, false);
   page;
   linewidth(output, w);
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      rect(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                   randr(1, maxxg(output)), randr(1, maxyg(output)))

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test rounded rectangle speed }

procedure rrectspeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   auto(output, false);
   curvis(output, false);
   page;
   linewidth(output, w);
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      rrect(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                    randr(1, maxxg(output)), randr(1, maxyg(output)),
                    randn(100-1)+1, randn(100-1)+1)

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test filled rectangle speed }

procedure frectspeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      frect(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                    randr(1, maxxg(output)), randr(1, maxyg(output)))

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test filled rounded rectangle speed }

procedure frrectspeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      frrect(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                     randr(1, maxxg(output)), randr(1, maxyg(output)),
                     randn(100-1)+1, randn(100-1)+1)

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test ellipse speed }

procedure ellipsespeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   auto(output, false);
   curvis(output, false);
   page;
   linewidth(output, w);
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      ellipse(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                      randr(1, maxxg(output)), randr(1, maxyg(output)))

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test filled ellipse speed }

procedure fellipsespeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      fellipse(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                       randr(1, maxxg(output)), randr(1, maxyg(output)))

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test arc speed }

procedure arcspeed(w, t: integer; var s: integer);

var
   i:      integer;
   c:      integer;
   sa, ea: integer;
   cl:     color;

begin

   auto(output, false);
   curvis(output, false);
   page;
   linewidth(output, w);
   c := clock;
   for i := 1 to t do begin

      repeat

         sa := randn(i32max);
         ea := randn(i32max)

      until not (ea <= sa);
      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      arc(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                  randr(1, maxxg(output)), randr(1, maxyg(output)),
                  sa, ea)

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test filled arc speed }

procedure farcspeed(w, t: integer; var s: integer);

var
   i:      integer;
   c:      integer;
   sa, ea: integer;
   cl:     color;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   c := clock;
   for i := 1 to t do begin

      repeat

         sa := randn(i32max);
         ea := randn(i32max)

      until not (ea <= sa);
      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      farc(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                   randr(1, maxxg(output)), randr(1, maxyg(output)),
                   sa, ea)

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test filled chord speed }

procedure fchordspeed(w, t: integer; var s: integer);

var
   i:      integer;
   c:      integer;
   sa, ea: integer;
   cl:     color;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   c := clock;
   for i := 1 to t do begin

      repeat

         sa := randn(i32max);
         ea := randn(i32max)

      until not (ea <= sa);
      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      fchord(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                     randr(1, maxxg(output)), randr(1, maxyg(output)),
                     sa, ea)

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test filled triangle speed }

procedure ftrianglespeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      ftriangle(output, randr(1, maxxg(output)), randr(1, maxyg(output)),
                        randr(1, maxxg(output)), randr(1, maxyg(output)),
                        randr(1, maxxg(output)), randr(1, maxyg(output)))

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test text speed }

procedure ftextspeed(w, t: integer; var s: integer);

var
   i:  integer;
   c:  integer;
   cl: color;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   c := clock;
   for i := 1 to t do begin

      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      fcolor(output, cl);
      case randr(ord(red), ord(magenta)) of
         2: cl := red;   3: cl := green;  4: cl := blue;
         5: cl := cyan;  6: cl := yellow; 7: cl := magenta
      end;
      bcolor(output, cl);
      cursorg(output, randr(1, maxxg(output)), randr(1, maxyg(output)));
      write('Test text')

   end;
   s := elapsed(c);
   fcolor(output, black);
   bcolor(output, white)

end;

{ test picture draw speed }

procedure fpictspeed(w, t: integer; var s: integer);

var
   i:              integer;
   c:              integer;
   x1, y1, x2, y2: integer;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   loadpict(output, 1, 'mypic');
   c := clock;
   for i := 1 to t do begin

      x1 := randr(1, maxxg(output));
      x2 := randr(1, maxxg(output));
      if x1 > x2 then swap(x1, x2);
      y1 := randr(1, maxyg(output));
      y2 := randr(1, maxyg(output));
      if y1 > y2 then swap(y1, y2);
      picture(output, 1, x1, y1, x2, y2)

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ test picture draw speed, no scaling }

procedure fpictnsspeed(w, t: integer; var s: integer);

var
   i:      integer;
   c:      integer;
   x, y:   integer;
   xs, ys: integer;

begin

   refer(w); { unused by filled shapes; required by the benchtest fp signature }
   auto(output, false);
   curvis(output, false);
   page;
   loadpict(output, 1, 'mypic');
   xs := pictsizx(output, 1);
   ys := pictsizy(output, 1);
   c := clock;
   for i := 1 to t do begin

      x := randr(1, maxxg(output));
      y := randr(1, maxyg(output));
      picture(output, 1, x, y, x+xs-1, y+ys-1)

   end;
   s := elapsed(c);
   fcolor(output, black)

end;

{ run benchmark test

  Since different processors take different amounts of time to run the tests,
  we normalize them to gather as much data as needed to run for 5 seconds per
  test. To find out how many tests we need to run for that time, we measure a
  small number of tests, then extrapolate that time to 5 seconds. Since the
  test could iterate in a very short (unmeasurable) time, we run the tests at
  progressive orders of magnitude until we run at least 10ms of test time and
  at least 10 iterations. }

procedure benchtest(procedure fp(w, t: integer; var s: integer);
                    bn: bench; w: integer);

var
   i:  integer;
   t:  integer;
   et: integer;
   s:  integer;

begin

   { test how many iterations we need to get a measurable timebase }
   i := 10;
   repeat

      t := clock;            { get base time }
      fp(w, i, s);           { perform test }
      et := elapsed(t);      { find time to execute }
      i := i*2               { scale for next pass }

   until not (et < second);  { set minimum time to measure for stability }
   i := i div 2;             { remove last scale }
   { find iterations for 5 second run }
   i := round((second*5*0.0001)/(et*0.0001/i));
   fp(w, i, s);              { run final test }
   benchtab[bn].iter := i;   { place iterations }
   benchtab[bn].time := s    { place time to run }

end;

{ ************************************************************************

   Main program

   ************************************************************************ }

begin

   { initialize the random generator state }
   randstate := 1;
   framenum := 0;

   { start frame timer }
   frametimer(output, true);
   curvis(output, false);
   binvis(output);
   write('Graphics screen test vs. ', majorver:1, '.', minorver:1);
   if experiment then write('.x');
   writeln;
   writeln;
   writeln('Screen size in characters: x -> ', maxx(output):1,
           ' y -> ', maxy(output):1);
   writeln('            in pixels:     x -> ', maxxg(output):1,
           ' y -> ', maxyg(output):1);
   writeln('Size of character in default font: x -> ', chrsizx(output):1,
           ' y -> ', chrsizy(output):1);
   writeln('Dots per meter: dpmx: ', dpmx(output):1, ' dpmy: ', dpmy(output):1);
   writeln('Aspect ratio: ', dpmx(output)/dpmy(output):1:6);
   prtcen(maxy(output),
          'Press return to start test (and to pass each pattern)');
   waitnext;

   { ******************** Graphical figures test ******************** }

   page;

   grid;
   writeln;
   bover(output);
   graphtest(1);
   binvis(output);
   prtcen(maxy(output), 'Graphical figures test, linewidth == 1');
   waitnext;

   page;
   grid;
   writeln;
   graphtest(2);
   prtcen(maxy(output), 'Graphical figures test, linewidth == 2');
   waitnext;

   page;
   grid;
   writeln;
   graphtest(3);
   prtcen(maxy(output), 'Graphical figures test, linewidth == 3');
   waitnext;

   page;
   grid;
   writeln;
   graphtest(5);
   prtcen(maxy(output), 'Graphical figures test, linewidth == 5');
   waitnext;

   page;
   grid;
   writeln;
   graphtest(11);
   prtcen(maxy(output), 'Graphical figures test, linewidth == 11');
   waitnext;

   { ******************** Standard Fonts test ******************** }

   page;
   chrgrid;
   prtcen(maxy(output), 'Standard fonts test');
   auto(output, false);
   home(output);
   binvis(output);
   fontnam(output, ftterm, fns);
   if not blank(fns) then begin

      font(output, ftterm);
      writeln('This is the terminal font: System name: "', fns:*, '"');
      writeln('Size x -> ', chrsizx(output):1, ' y -> ', chrsizy(output):1);
      prtall;
      writeln

   end else begin

      writeln('There is no terminal font');
      writeln

   end;
   fontnam(output, ftbook, fns);
   if not blank(fns) then begin

      font(output, ftbook);
      writeln('This is the book font: System name: "', fns:*, '"');
      writeln('Size x -> ', chrsizx(output):1, ' y -> ', chrsizy(output):1);
      prtall;
      writeln

   end else begin

      writeln('There is no book font');
      writeln

   end;
   fontnam(output, ftsign, fns);
   if not blank(fns) then begin

      font(output, ftsign);
      writeln('This is the sign font: System name: "', fns:*, '"');
      writeln('Size x -> ', chrsizx(output):1, ' y -> ', chrsizy(output):1);
      prtall;
      writeln

   end else begin

      writeln('There is no sign font');
      writeln

   end;
   fontnam(output, fttech, fns);
   if not blank(fns) then begin

      font(output, fttech);
      writeln('This is the technical font: System name: "', fns:*, '"');
      writeln('Size x -> ', chrsizx(output):1, ' y -> ', chrsizy(output):1);
      prtall;
      writeln

   end else begin

      writeln('There is no technical font');
      writeln

   end;
   font(output, ftterm);
   writeln('Complete');
   waitnext;

   { ********************** Graphical cursor movement test ******************* }

   page;
   auto(output, false);
   prtcen(maxy(output), 'Graphical cursor movement test');
   x := 1;
   y := 1;
   i := 10000;
   dx := +1;
   dy := +1;
   ln := strsiz(output, s1);
   term := false;
   while term = false do begin

      cursorg(output, x, y);
      write(s1);
      xs := x;
      ys := y;
      x := x+dx;
      y := y+dy;
      if (x < 1) or (x+ln-1 > maxxg(output)) then begin

         x := xs;
         dx := -dx

      end;
      if (y < 1) or (y+chrsizy(output)*2 > maxyg(output)) then begin

         y := ys;
         dy := -dy

      end;
      waitchar(100, term);
      cursorg(output, xs, ys);
      fcolor(output, white);
      write(s1);
      fcolor(output, black)

   end;

   { *************************** Vertical lines test ************************* }

   page;
   grid;
   prtcen(maxy(output), 'Vertical lines test');
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 50;
   y := yspace;
   w := 1;
   while y+w div 2 < maxyg(output)-chrsizy(output) do begin

      linewidth(output, w);
      line(output, xspace, y, maxxg(output)-xspace, y);
      y := y+yspace;
      w := w+1

   end;
   linewidth(output, 1);
   waitnext;

   { ************************* Horizontal lines test ************************* }

   page;
   grid;
   prtcen(maxy(output), 'Horizontal lines test');
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 20;
   x := xspace;
   w := 1;
   while x+w div 2 < maxxg(output)-20 do begin

      linewidth(output, w);
      line(output, x, yspace, x, maxyg(output)-chrsizy(output));
      x := x+xspace;
      w := w+1

   end;
   linewidth(output, 1);
   waitnext;

   { ************************** 45 degree lines test ************************* }

   page;
   grid;
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 20;
   { Lines have slope 1 (y = x + y_intercept). Draw them long enough to
     exit the window on both ends. Stepping the y-intercept from below the
     top-right corner down to above the bottom-left fills the whole usable
     window with parallel diagonals. }
   ysize := maxxg(output)+maxyg(output);
   y := -(maxxg(output)-xspace);
   w := 1;
   while y+w div 2 < maxyg(output)-chrsizy(output)-yspace do begin

      linewidth(output, w);
      line(output, 0, y, ysize, y+ysize);
      y := y+xspace;
      w := w+1

   end;
   linewidth(output, 1);
   prtcen(maxy(output), '45 degree lines test');
   waitnext;

   { ********************** Vertical lines test - dashed ******************** }

   page;
   grid;
   linestyle(output, lsdash);
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 50;
   y := yspace;
   w := 1;
   while y+w div 2 < maxyg(output)-chrsizy(output) do begin

      linewidth(output, w);
      line(output, xspace, y, maxxg(output)-xspace, y);
      y := y+yspace;
      w := w+1

   end;
   linewidth(output, 1);
   linestyle(output, lssolid);
   prtcen(maxy(output), 'Vertical lines test (dashed)');
   waitnext;

   { ********************** Vertical lines test - dotted ******************** }

   page;
   grid;
   linestyle(output, lsdot);
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 50;
   y := yspace;
   w := 1;
   while y+w div 2 < maxyg(output)-chrsizy(output) do begin

      linewidth(output, w);
      line(output, xspace, y, maxxg(output)-xspace, y);
      y := y+yspace;
      w := w+1

   end;
   linewidth(output, 1);
   linestyle(output, lssolid);
   prtcen(maxy(output), 'Vertical lines test (dotted)');
   waitnext;

   { ********************* Horizontal lines test - dashed ******************* }

   page;
   grid;
   linestyle(output, lsdash);
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 20;
   x := xspace;
   w := 1;
   while x+w div 2 < maxxg(output)-20 do begin

      linewidth(output, w);
      line(output, x, yspace, x, maxyg(output)-chrsizy(output));
      x := x+xspace;
      w := w+1

   end;
   linewidth(output, 1);
   linestyle(output, lssolid);
   prtcen(maxy(output), 'Horizontal lines test (dashed)');
   waitnext;

   { ********************* Horizontal lines test - dotted ******************* }

   page;
   grid;
   linestyle(output, lsdot);
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 20;
   x := xspace;
   w := 1;
   while x+w div 2 < maxxg(output)-20 do begin

      linewidth(output, w);
      line(output, x, yspace, x, maxyg(output)-chrsizy(output));
      x := x+xspace;
      w := w+1

   end;
   linewidth(output, 1);
   linestyle(output, lssolid);
   prtcen(maxy(output), 'Horizontal lines test (dotted)');
   waitnext;

   { ********************* 45 degree lines test - dashed ******************** }

   page;
   grid;
   linestyle(output, lsdash);
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 20;
   ysize := maxxg(output)+maxyg(output);
   y := -(maxxg(output)-xspace);
   w := 1;
   while y+w div 2 < maxyg(output)-chrsizy(output)-yspace do begin

      linewidth(output, w);
      line(output, 0, y, ysize, y+ysize);
      y := y+xspace;
      w := w+1

   end;
   linewidth(output, 1);
   linestyle(output, lssolid);
   prtcen(maxy(output), '45 degree lines test (dashed)');
   waitnext;

   { ********************* 45 degree lines test - dotted ******************** }

   page;
   grid;
   linestyle(output, lsdot);
   yspace := maxyg(output) div 20;
   xspace := maxxg(output) div 20;
   ysize := maxxg(output)+maxyg(output);
   y := -(maxxg(output)-xspace);
   w := 1;
   while y+w div 2 < maxyg(output)-chrsizy(output)-yspace do begin

      linewidth(output, w);
      line(output, 0, y, ysize, y+ysize);
      y := y+xspace;
      w := w+1

   end;
   linewidth(output, 1);
   linestyle(output, lssolid);
   prtcen(maxy(output), '45 degree lines test (dotted)');
   waitnext;

   { **************************** Polar lines test ************************** }

   page;
   grid;
   prtcen(maxy(output), 'Polar lines test');
   x := maxxg(output) div 2;
   y := maxyg(output) div 2;
   if maxxg(output) > maxyg(output) then l := maxyg(output) div 2-chrsizy(output)
   else l := maxxg(output) div 2-chrsizy(output);
   w := 1;
   fcolor(output, blue);
   ellipse(output, x-l, y-l, x+l, y+l);
   fcolor(output, black);
   bover(output);
   while w < 10 do begin

      a := 0; { set angle }
      while a < 360 do begin

         pline(a, l, x, y, w);
         a := a+10

      end;
      home(output);
      writeln('Line width: ', w:1);
      w := w+1;
      waitnext

   end;
   binvis(output);
   linewidth(output, 1);

   { ******************************* Color test 1 ****************************** }

   page;
   y := 1; { set 1st row }
   r := 0; { set colors }
   g := 0;
   b := 0;
   while y < maxyg(output) do begin

      x := 1;
      while x < maxxg(output) do begin

         fcolorg(output, r, g, b);
         frect(output, x, y, x+colsqr-1, y+colsqr-1);
         x := x+colsqr;
         if r <= i32max-i32max div coldiv then r := r+i32max div coldiv
         else begin

            r := 0;
            if g <= i32max-i32max div coldiv then g := g+i32max div coldiv
            else begin

               g := 0;
               if b <= i32max-i32max div coldiv then b := b+i32max div coldiv
               else b := 0

            end

         end

      end;
      y := y+colsqr

   end;
   fcolor(output, black);
   bcolor(output, white);
   bover(output);
   prtcen(maxy(output), 'Color test 1');
   binvis(output);
   waitnext;

   { ******************************* Color test 2 ****************************** }

   page;
   x := 1; { set 2st collumn }
   while x < maxxg(output) do begin

      fcolorg(output, i32max div maxxg(output)*x, 0, 0);
      line(output, x, 1, x, maxyg(output));
      x := x+1

   end;
   binvis(output);
   fcolor(output, black);
   bcolor(output, white);
   bover(output);
   prtcen(maxy(output), 'Color test 2');
   binvis(output);
   waitnext;

   { ******************************* Color test 3 ****************************** }

   page;
   x := 1; { set 2st collumn }
   while x < maxxg(output) do begin

      fcolorg(output, 0, i32max div maxxg(output)*x, 0);
      line(output, x, 1, x, maxyg(output));
      x := x+1

   end;
   binvis(output);
   fcolor(output, black);
   bcolor(output, white);
   bover(output);
   prtcen(maxy(output), 'Color test 3');
   binvis(output);
   waitnext;

   { ******************************* Color test 4 ****************************** }

   page;
   x := 1; { set 2st collumn }
   while x < maxxg(output) do begin

      fcolorg(output, 0, 0, i32max div maxxg(output)*x);
      line(output, x, 1, x, maxyg(output));
      x := x+1

   end;
   binvis(output);
   fcolor(output, black);
   bcolor(output, white);
   bover(output);
   prtcen(maxy(output), 'Color test 4');
   binvis(output);
   waitnext;

   { ***************************** Rectangle test **************************** }

   page;
   grid;
   l := 10;
   x := maxxg(output) div 2; { find center }
   y := maxyg(output) div 2;
   w := 1;
   c := black;
   while (l < maxxg(output) div 2) and (l < maxyg(output) div 2-chrsizy(output)) do begin

      fcolor(output, c);
      linewidth(output, w);
      rect(output, x-l, y-l, x+l, y+l);
      l := l+20;
      w := w+1;
      if c < magenta then c := succ(c) else c := black;
      if c = white then c := succ(c)

   end;
   linewidth(output, 1);
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Rectangle test');
   waitnext;

   { ************************ Filled rectangle test 1 ************************ }

   page;
   grid;
   if maxxg(output) > maxyg(output) then l := maxyg(output) div 2-chrsizy(output)
   else l := maxxg(output) div 2-chrsizy(output);
   x := maxxg(output) div 2; { find center }
   y := maxyg(output) div 2;
   c := black;
   while l >= 10 do begin

      fcolor(output, c);
      frect(output, x-l, y-l, x+l, y+l);
      l := l-20;
      if c < magenta then c := succ(c) else c := black;
      if c = white then c := succ(c)

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled rectangle test 1');
   waitnext;

   { ************************ Filled rectangle test 2 ************************ }

   page;
   grid;
   l := 10;
   x := 20;
   y := 20;
   c := black;
   while y+l*2 < maxyg(output)-chrsizy(output) do begin

      while x+l*2 < maxxg(output)-chrsizy(output) do begin

         fcolor(output, c);
         frect(output, x, y, x+l*2, y+l*2);
         x := x+l*2+20;
         l := l+5;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := 10;
      y := y+l*2+10

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled rectangle test 2');
   waitnext;

   { ************************* Rounded rectangle test ************************ }

   binvis(output);
   r := 1;
   while r < 100 do begin

      page;
      grid;
      l := 10;
      x := maxxg(output) div 2; { find center }
      y := maxyg(output) div 2;
      w := 1;
      c := black;
      writeln('r: ', r:1);
      while (l+w div 2 < maxxg(output) div 2) and (l < maxyg(output) div 2-chrsizy(output)) do begin

         fcolor(output, c);
         linewidth(output, w);
         rrect(output, x-l, y-l, x+l, y+l, r, r);
         l := l+w;
         w := w+1;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      linewidth(output, 1);
      fcolor(output, black);
      prtcen(maxy(output), 'Rounded rectangle test');
      waitnext;
      r := r+10

   end;

   { ******** Rounded rectangle minimums test ******** }

   page;
   xs := maxxg(output) div 20;
   ys := maxyg(output) div 20;

   { paint the grid }
   fcolor(output, cyan);
   x1 := 1;
   while x1 < maxxg(output) do begin

      line(output, x1, 1, x1, maxyg(output));
      x1 := x1+xs

   end;
   y1 := 1;
   while y1 < maxyg(output) do begin

      line(output, 1, y1, maxxg(output), y1);
      y1 := y1+ys

   end;
   fcolor(output, black);

   { draw vertical }
   x1 := 1+xs;
   y1 := 1+ys;
   x2 := x1+xs*2;
   y2 := y1;
   while y2+ys < maxyg(output) do begin

      rrect(output, x1, y1, x2, y2, 10, 10);
      y1 := y1+ys;
      y2 := y2+ys+1

   end;

   { draw horizontal }
   x1 := 1+xs*4;
   y1 := 1+ys;
   x2 := x1;
   y2 := ys*4;
   while x2+xs < maxxg(output) do begin

      rrect(output, x1, y1, x2, y2, 10, 10);
      x1 := x1+xs;
      x2 := x2+xs+1

   end;

   { draw boxes }
   x1 := 1+xs*4;
   y1 := 1+ys*6;
   x2 := x1;
   y2 := y1;
   while x2 < maxxg(output) do begin

      rrect(output, x1, y1, x2, y2, 10, 10);
      x1 := x1+xs;
      x2 := x2+xs+1;
      y2 := y2+1

   end;
   prtcen(maxy(output), 'Rounded Rectangle Minimums Test');
   waitnext;

   { ******** Filled rounded rectangle test 1 ******** }

   binvis(output);
   r := 1;
   while r < 100 do begin

      page;
      grid;
      if maxxg(output) > maxyg(output) then l := maxyg(output) div 2-chrsizy(output)
      else l := maxxg(output) div 2-chrsizy(output);
      x := maxxg(output) div 2; { find center }
      y := maxyg(output) div 2;
      c := black;
      writeln('r: ', r:1);
      while l >= 10 do begin

         fcolor(output, c);
         frrect(output, x-l, y-l, x+l, y+l, r, r);
         l := l-20;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      fcolor(output, black);
      prtcen(maxy(output), 'Filled rounded rectangle test 1');
      waitnext;
      r := r+10

   end;

   { ******** Filled rounded rectangle test 2 ******** }

   binvis(output);
   r := 1;
   while r < 100 do begin

      page;
      grid;
      writeln('r: ', r:1);
      l := 10;
      x := 20;
      y := curyg(output);
      c := black;
      while y+l*2 < maxyg(output)-20 do begin

         while x+l*2 < maxxg(output)-20 do begin

            fcolor(output, c);
            frrect(output, x, y, x+l*2, y+l*2, r, r);
            x := x+l*2+20;
            l := l+5;
            if c < magenta then c := succ(c) else c := black;
            if c = white then c := succ(c)

         end;
         x := 10;
         y := y+l*2+10

      end;
      fcolor(output, black);
      binvis(output);
      prtcen(maxy(output), 'Filled rounded rectangle test 2');
      waitnext;
      r := r+10

   end;

   { ******** Filled rounded rectangle minimums test ******** }

   page;
   xs := maxxg(output) div 20;
   ys := maxyg(output) div 20;

   { paint the grid }
   fcolor(output, cyan);
   x1 := 1;
   while x1 < maxxg(output) do begin

      line(output, x1, 1, x1, maxyg(output));
      x1 := x1+xs

   end;
   y1 := 1;
   while y1 < maxyg(output) do begin

      line(output, 1, y1, maxxg(output), y1);
      y1 := y1+ys

   end;
   fcolor(output, black);

   { draw vertical }
   x1 := 1+xs;
   y1 := 1+ys;
   x2 := x1+xs*2;
   y2 := y1;
   while y2+ys < maxyg(output) do begin

      frrect(output, x1, y1, x2, y2, 10, 10);
      y1 := y1+ys;
      y2 := y2+ys+1

   end;

   { draw horizontal }
   x1 := 1+xs*4;
   y1 := 1+ys;
   x2 := x1;
   y2 := ys*4;
   while x2+xs < maxxg(output) do begin

      frrect(output, x1, y1, x2, y2, 10, 10);
      x1 := x1+xs;
      x2 := x2+xs+1

   end;

   { draw boxes }
   x1 := 1+xs*4;
   y1 := 1+ys*6;
   x2 := x1;
   y2 := y1;
   while x2 < maxxg(output) do begin

      frrect(output, x1, y1, x2, y2, 10, 10);
      x1 := x1+xs;
      x2 := x2+xs+1;
      y2 := y2+1

   end;
   prtcen(maxy(output), 'Filled Rectangle Minimums Test');
   waitnext;

   { ******** Ellipse test ******** }

   binvis(output);
   w := 1;
   while w < 10 do begin

      page;
      grid;
      lx := maxxg(output) div 2-10;
      lx := lx-lx mod 10;
      ly := maxyg(output) div 2-10-chrsizy(output);
      ly := ly-ly mod 10;
      x := maxxg(output) div 2; { find center }
      y := maxyg(output) div 2;
      x := x-x mod 10;
      y := y-y mod 10;
      c := black;
      writeln('width: ', w:1);
      while (lx >= 10) and (ly >= 10) do begin

         fcolor(output, c);
         linewidth(output, w);
         ellipse(output, x-lx, y-ly, x+lx, y+ly);
         lx := lx-20;
         ly := ly-20;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      fcolor(output, black);
      prtcen(maxy(output), 'Ellipse test');
      waitnext;
      w := w+1

   end;
   linewidth(output, 1);

   { ******** Filled ellipse test 1 ******** }

   page;
   grid;
   lx := maxxg(output) div 2-10;
   lx := lx-lx mod 10;
   ly := maxyg(output) div 2-10-chrsizy(output);
   ly := ly-ly mod 10;
   x := maxxg(output) div 2; { find center }
   y := maxyg(output) div 2;
   x := x-x mod 10;
   y := y-y mod 10;
   c := black;
   while (lx >= 10) and (ly >= 10) do begin

      fcolor(output, c);
      fellipse(output, x-lx, y-ly, x+lx, y+ly);
      lx := lx-20;
      ly := ly-20;
      if c < magenta then c := succ(c) else c := black;
      if c = white then c := succ(c)

   end;
   fcolor(output, black);
   prtcen(maxy(output), 'Filled ellipse test 1');
   waitnext;

   { ******** Filled ellipse test 2 ******** }

   page;
   grid;
   l := 10;
   x := 20;
   y := 20;
   c := black;
   while y+l*2 < maxyg(output)-20 do begin

      while x+l*2 < maxxg(output)-20 do begin

         fcolor(output, c);
         fellipse(output, x, y, x+l*2, y+l*2);
         x := x+l*2+20;
         l := l+5;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := 10;
      y := y+l*2+10

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled ellipse test 2');
   waitnext;

   { ******************************* Arc test 1 ******************************** }

   binvis(output);
   w := 1;
   while w < 10 do begin

      page;
      grid;
      a := 0;
      c := black;
      i := 10;
      write('Linewidth: ', w:1);
      while (i < maxxg(output) div 2) and
            (i < (maxyg(output)-chrsizy(output)) div 2) do begin

         a := 0;
         while a <= i32max-i32max div 10 do begin

            fcolor(output, c);
            linewidth(output, w);
            arc(output, i, i, maxxg(output)-i,
                maxyg(output)-chrsizy(output)-i, a, a+i32max div 10);
            a := a+i32max div 5;
            if c < magenta then c := succ(c) else c := black;
            if c = white then c := succ(c)

         end;
         i := i+20

      end;
      fcolor(output, black);
      prtcen(maxy(output), 'Arc test 1');
      waitnext;
      w := w+1

   end;

   { ************************ Arc test 2 ************************ }

   binvis(output);
   w := 1;
   xspace := maxxg(output) div 40;
   yspace := maxyg(output) div 40;
   while w < 10 do begin

      page;
      grid;
      writeln('Linewidth: ', w:1);
      l := maxxg(output) div 40;
      x := xspace;
      y := curyg(output);
      c := black;
      aa := 0;
      ab := i32max div 360*90;
      while y+l*2 < maxyg(output)-yspace do begin

         while x+l*2 < maxxg(output)-xspace do begin

            fcolor(output, red);
            linewidth(output, 1);
            rect(output, x, y, x+l*2, y+l*2);
            fcolor(output, black);
            linewidth(output, w);
            arc(output, x, y, x+l*2, y+l*2, aa, ab);
            x := x+l*2+xspace;
            l := l+maxxg(output) div 60

         end;
         x := xspace;
         y := y+l*2+yspace

      end;
      binvis(output);
      prtcen(maxy(output), 'Arc test 2');
      waitnext;
      w := w+1

   end;

   { ************************ Arc test 3 ************************ }

   binvis(output);
   w := 1;
   xspace := maxxg(output) div 25;
   yspace := xspace;
   while w < 10 do begin

      page;
      grid;
      writeln('Linewidth: ', w:1);
      l := xspace;
      x := xspace;
      y := curyg(output);
      c := black;
      aa := 0;
      ab := 10;
      while (y+l*2 < maxyg(output)-yspace) and (ab <= 360) do begin

         while (x+l*2 < maxxg(output)-xspace) and (ab <= 360) do begin

            fcolor(output, red);
            linewidth(output, 1);
            rect(output, x, y, x+l*2, y+l*2);
            fcolor(output, black);
            linewidth(output, w);
            arc(output, x, y, x+l*2, y+l*2, aa*degree, ab*degree);
            x := x+l*2+xspace;
            ab := ab+10

         end;
         x := xspace;
         y := y+l*2+yspace

      end;
      binvis(output);
      prtcen(maxy(output), 'Arc test 3');
      waitnext;
      w := w+1

   end;

   { ************************ Arc test 4 ************************ }

   binvis(output);
   w := 1;
   xspace := maxxg(output) div 25;
   yspace := xspace;
   while w < 10 do begin

      page;
      grid;
      writeln('Linewidth: ', w:1);
      l := xspace;
      x := xspace;
      y := curyg(output);
      c := black;
      aa := 0;
      ab := 360;
      while (y+l*2 < maxyg(output)-yspace) and (aa < 360) do begin

         while (x+l*2 < maxxg(output)-xspace) and (aa < 360) do begin

            fcolor(output, red);
            linewidth(output, 1);
            rect(output, x, y, x+l*2, y+l*2);
            fcolor(output, black);
            linewidth(output, w);
            arc(output, x, y, x+l*2, y+l*2, aa*degree, ab*degree);
            x := x+l*2+xspace;
            aa := aa+10

         end;
         x := xspace;
         y := y+l*2+yspace

      end;
      binvis(output);
      prtcen(maxy(output), 'Arc test 4');
      waitnext;
      w := w+1

   end;

   { **************************** Filled arc test 1 **************************** }

   page;
   grid;
   a := 0;
   c := black;
   a := 0;
   x := maxxg(output)-10;
   x := x-x mod 10;
   y := maxyg(output)-chrsizy(output)-10;
   y := y-y mod 10;
   while a <= i32max-i32max div 10 do begin

      fcolor(output, c);
      farc(output, 10, 10, x, y, a, a+i32max div 10);
      a := a+i32max div 5;
      if c < magenta then c := succ(c) else c := black;
      if c = white then c := succ(c)

   end;
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Filled arc test 1');
   waitnext;

   { ************************ filled arc test 2 ************************ }

   page;
   xspace := maxxg(output) div 40;
   yspace := maxyg(output) div 40;
   grid;
   l := maxxg(output) div 50;
   x := xspace;
   y := yspace;
   c := black;
   aa := 0;
   ab := i32max div 360*90;
   while y+l*2 < maxyg(output)-yspace do begin

      while x+l*2 < maxxg(output)-xspace do begin

         fcolor(output, red);
         linewidth(output, 1);
         rect(output, x, y, x+l*2, y+l*2);
         fcolor(output, c);
         farc(output, x, y, x+l*2, y+l*2, aa, ab);
         x := x+l*2+xspace;
         l := l+maxxg(output) div 40;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := xspace;
      y := y+l*2+yspace

   end;
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Filled arc test 2');
   waitnext;

   { ************************ Filled arc test 3 ************************ }

   page;
   xspace := maxxg(output) div 40;
   yspace := maxyg(output) div 40;
   grid;
   l := maxxg(output) div 21;
   x := xspace;
   y := yspace;
   c := black;
   aa := 0;
   ab := 10;
   while (y+l*2 < maxyg(output)-yspace) and (ab <= 360) do begin

      while (x+l*2 < maxxg(output)-xspace) and (ab <= 360) do begin

         fcolor(output, c);
         farc(output, x, y, x+l*2, y+l*2, aa*degree, ab*degree);
         x := x+l*2+xspace;
         ab := ab+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := xspace;
      y := y+l*2+yspace

   end;
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Arc test 3');
   waitnext;

   { ************************ Filled arc test 4 ************************ }

   page;
   xspace := maxxg(output) div 40;
   yspace := maxyg(output) div 40;
   grid;
   l := maxxg(output) div 21;
   x := xspace;
   y := yspace;
   c := black;
   aa := 0;
   ab := 360;
   while (y+l*2 < maxyg(output)-yspace) and (aa < 360) do begin

      while (x+l*2 < maxxg(output)-xspace) and (aa < 360) do begin

         fcolor(output, c);
         farc(output, x, y, x+l*2, y+l*2, aa*degree, ab*degree);
         x := x+l*2+xspace;
         aa := aa+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := xspace;
      y := y+l*2+yspace

   end;
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Arc test 4');
   waitnext;

   { *************************** Filled chord test 1 *************************** }

   page;
   grid;
   a := 0;
   c := black;
   a := 0;
   i := 8;
   x := maxxg(output)-10;
   x := x-x mod 10;
   y := maxyg(output)-chrsizy(output)-10;
   y := y-y mod 10;
   while a <= i32max-i32max div i do begin

      fcolor(output, c);
      fchord(output, 10, 10, x, y, a, a+i32max div i);
      a := a+i32max div (i div 2);
      if c < magenta then c := succ(c) else c := black;
      if c = white then c := succ(c)

   end;
   fcolor(output, black);
   prtcen(maxy(output), 'Filled chord test 1');
   waitnext;

   { ************************ Filled chord test 2 ************************ }

   page;
   xspace := maxxg(output) div 50;
   yspace := xspace;
   grid;
   l := maxxg(output) div 100;
   x := xspace;
   y := yspace;
   c := black;
   aa := 0;
   ab := i32max div 360*90;
   while y+l*2 < maxyg(output)-yspace do begin

      while x+l*2 < maxxg(output)-xspace do begin

         fcolor(output, c);
         fchord(output, x, y, x+l*2, y+l*2, aa, ab);
         x := x+l*2+xspace;
         l := l+maxxg(output) div 100;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := xspace;
      y := y+l*2+yspace

   end;
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Filled chord test 2');
   waitnext;

   { ************************ Filled chord test 3 ************************ }

   page;
   xspace := maxxg(output) div 50;
   yspace := xspace;
   grid;
   l := maxxg(output) div 20;
   x := xspace;
   y := yspace;
   c := black;
   aa := 0;
   ab := 10;
   while (y+l*2 < maxyg(output)-yspace) and (ab <= 360) do begin

      while (x+l*2 < maxxg(output)-xspace) and (ab <= 360) do begin

         fcolor(output, c);
         fchord(output, x, y, x+l*2, y+l*2, aa*degree, ab*degree);
         x := x+l*2+xspace;
         ab := ab+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := xspace;
      y := y+l*2+yspace

   end;
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Filled chord test 3');
   waitnext;

   { ************************ Filled chord test 4 ************************ }

   page;
   xspace := maxxg(output) div 50;
   yspace := xspace;
   grid;
   l := maxxg(output) div 20;
   x := xspace;
   y := yspace;
   c := black;
   aa := 0;
   ab := 360;
   while (y+l*2 < maxyg(output)-yspace) and (aa < 360) do begin

      while (x+l*2 < maxxg(output)-xspace) and (aa < 360) do begin

         fcolor(output, c);
         fchord(output, x, y, x+l*2, y+l*2, aa*degree, ab*degree);
         x := x+l*2+xspace;
         aa := aa+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := xspace;
      y := y+l*2+yspace

   end;
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Filled chord test 4');
   waitnext;

   { ************************** Filled triangle test 1 ************************* }

   page;
   grid;
   tx1 := 10;
   ty1 := maxyg(output)-chrsizy(output)-10;
   ty1 := ty1-ty1 mod 10;
   tx2 := maxxg(output) div 2;
   ty2 := 10;
   tx3 := maxxg(output)-10;
   tx3 := tx3-tx3 mod 10;
   ty3 := maxyg(output)-chrsizy(output)-10;
   ty3 := ty3-ty3 mod 10;
   c := black;
   i := 40;
   while (tx1 <= tx3-10) and (ty2 <= ty3-10) do begin

      fcolor(output, c);
      ftriangle(output, tx1, ty1, tx2, ty2, tx3, ty3);
      tx1 := tx1+i;
      ty1 := ty1-i div 2;
      ty2 := ty2+i;
      tx3 := tx3-i;
      ty3 := ty3-i div 2;
      if c < magenta then c := succ(c) else c := black;
      if c = white then c := succ(c)

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled triangle test 1');
   waitnext;

   { ************************** Filled triangle test 2 ************************* }

   page;
   grid;
   x := 20;
   y := 20;
   l := 20;
   while y < maxyg(output)-20-l do begin

      while (y < maxyg(output)-20-l) and (x < maxxg(output)-20-l) do begin

         fcolor(output, c);
         ftriangle(output, x, y+l, x+l div 2, y, x+l, y+l);
         x := x+l+20;
         l := l+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := 20;
      y := y+l+20

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled triangle test 2');
   waitnext;

   { ************************** Filled triangle test 3 ************************* }

   page;
   grid;
   x := 20;
   y := 20;
   l := 20;
   while y < maxyg(output)-20-l do begin

      while (y < maxyg(output)-20-l) and (x < maxxg(output)-20-l) do begin

         fcolor(output, c);
         ftriangle(output, x, y+l, x, y, x+l, y+l);
         x := x+l+20;
         l := l+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := 20;
      y := y+l+20

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled triangle test 3');
   waitnext;

   { ************************** Filled triangle test 4 ************************* }

   page;
   grid;
   x := 20;
   y := 20;
   l := 20;
   while y < maxyg(output)-20-l do begin

      while (y < maxyg(output)-20-l) and (x < maxxg(output)-20-l) do begin

         fcolor(output, c);
         ftriangle(output, x, y+l, x, y, x+l, y);
         x := x+l+20;
         l := l+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := 20;
      y := y+l+20

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled triangle test 4');
   waitnext;

   { ************************** Filled triangle test 5 ************************* }

   page;
   grid;
   x := 20;
   y := 20;
   l := 20;
   while y < maxyg(output)-20-l do begin

      while (y < maxyg(output)-20-l) and (x < maxxg(output)-20-l) do begin

         fcolor(output, c);
         ftriangle(output, x+l div 2, y+l, x, y, x+l, y);
         x := x+l+20;
         l := l+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := 20;
      y := y+l+20

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled triangle test 5');
   waitnext;

   { ************************** Filled triangle test 6 ************************* }

   page;
   grid;
   x := 20;
   y := 20;
   l := 20;
   c := black;
   while y < maxyg(output)-20-l do begin

      while (y < maxyg(output)-20-l) and (x < maxxg(output)-20-l) do begin

         fcolor(output, c);
         ftriangle(output, x+l, y+l, x, y, x+l, y);
         x := x+l+20;
         l := l+10;
         if c < magenta then c := succ(c) else c := black;
         if c = white then c := succ(c)

      end;
      x := 20;
      y := y+l+20

   end;
   fcolor(output, black);
   binvis(output);
   prtcen(maxy(output), 'Filled triangle test 6');
   waitnext;

   { ************************** Filled triangle test 7 ************************* }

   page;
   grid;
   c := black;
   fcolor(output, c);
   ftriangle(output, 50, 50, 50, 100, 200, 50);
   if c < magenta then c := succ(c) else c := black;
   if c = white then c := succ(c);
   fcolor(output, c);
   ftriangle(output, 50, 100, 300, 200, 200, 50);
   if c < magenta then c := succ(c) else c := black;
   if c = white then c := succ(c);
   fcolor(output, c);
   ftriangle(output, 200, 50, 300, 200, 350, 100);
   if c < magenta then c := succ(c) else c := black;
   if c = white then c := succ(c);
   fcolor(output, c);
   ftriangle(output, 350, 100, 400, 300, 300, 200);
   if c < magenta then c := succ(c) else c := black;
   if c = white then c := succ(c);
   binvis(output);
   fcolor(output, black);
   prtcen(maxy(output), 'Filled triangle test 7');
   waitnext;

   { ************************** Filled triangle test 8 ************************* }

   page;
   grid;
   fcolor(output, black);
   ftriangle(output, 50, 50, 50, 100, 200, 50);
   ftriangle(output, 50, 100, 300, 200, 200, 50);
   ftriangle(output, 200, 50, 300, 200, 350, 100);
   ftriangle(output, 350, 100, 400, 300, 300, 200);
   binvis(output);
   prtcen(maxy(output), 'Filled triangle test 8');
   waitnext;

   { ******** Font sizing test ******** }

   page;
   grid;
   fsiz := chrsizy(output); { save character size to restore }
   h := 10;
   auto(output, false);
   font(output, ftsign);
   c1 := black;
   c2 := blue;
   bover(output);
   while curyg(output)+chrsizy(output) <= maxyg(output)-20 do begin

      fcolor(output, c1);
      bcolor(output, c2);
      fontsiz(output, h);
      writeln(chrsizy(output):1, ':', points(output):1:1, ': ', s2);
      h := h+5;
      if c1 < magenta then c1 := succ(c1) else c1 := black;
      if c1 = white then c1 := succ(c1);
      if c2 < magenta then c2 := succ(c2) else c2 := black;
      if c2 = white then c2 := succ(c2)

   end;
   fontsiz(output, fsiz); { restore font size }
   fcolor(output, black);
   bcolor(output, white);
   font(output, ftterm);
   binvis(output);
   prtcen(maxy(output), 'Font sizing test');
   waitnext;

   { ******** Font point sizing test ******** }

   page;
   grid;
   fsiz := chrsizy(output); { save character size to restore }
   ps := 6.0;
   auto(output, false);
   font(output, ftsign);
   c1 := black;
   c2 := blue;
   bover(output);
   while curyg(output)+chrsizy(output) <= maxyg(output)-20 do begin

      fcolor(output, c1);
      bcolor(output, c2);
      setpoints(output, ps);
      writeln(chrsizy(output):1, ':', points(output):1:1, ': ', s2);
      ps := ps+3.0;
      if c1 < magenta then c1 := succ(c1) else c1 := black;
      if c1 = white then c1 := succ(c1);
      if c2 < magenta then c2 := succ(c2) else c2 := black;
      if c2 = white then c2 := succ(c2)

   end;
   fontsiz(output, fsiz); { restore font size }
   fcolor(output, black);
   bcolor(output, white);
   font(output, ftterm);
   binvis(output);
   prtcen(maxy(output), 'Font point sizing test');
   waitnext;

   { ******** Font list test ******** }

   page;
   grid;
   writeln('Number of fonts: ', fonts(output):1);
   writeln;
   { Iterate font codes 1..fonts() directly and skip only genuinely blank names
     (blank() tests right-padded strings, so leading-space names like
     " rst: rasa" are kept). Bounding the loop by the code count also keeps the
     index from ever running past the last font. }
   cnt := fonts(output);
   for i := 1 to cnt do begin

      fontnam(output, i, fns);
      if not blank(fns) then begin { defined font }

         writeln(i:1, ': ', fns:*);
         if cury(output) >= maxy(output) then begin { screen overflows }

            writeln('Press return to continue');
            waitnext;
            page;
            grid

         end

      end

   end;
   writeln;
   writeln('List complete');
   waitnext;

   { ******** Font examples test ******** }

   page;
   grid;
   auto(output, false);
   bcolor(output, cyan);
   bover(output);
   cnt := fonts(output);
   for i := 1 to cnt do begin

      fontnam(output, i, fns);
      if not blank(fns) then begin { defined font }

         font(output, i);
         writeln(i:1, ': ', fns:*);
         if cury(output) >= maxy(output) then begin { screen overflows }

            font(output, ftterm);
            writeln('Press return to continue');
            waitnext;
            bcolor(output, white);
            page;
            grid;
            bcolor(output, cyan)

         end

      end

   end;
   bcolor(output, white);
   font(output, ftterm);
   binvis(output);
   writeln;
   writeln('List complete');
   waitnext;

   { ******** Extended effects test ******** }

   page;
   grid;
   auto(output, false);
   font(output, ftsign);
   condensed(output, 1);
   writeln('Condensed');
   condensed(output, 0);
   extended(output, 1);
   writeln('Extended');
   extended(output, 0);
   xlight(output, 1);
   writeln('Extra light');
   xlight(output, 0);
   light(output, 1);
   writeln('Light');
   light(output, 0);
   xbold(output, 1);
   writeln('Extra bold');
   xbold(output, 0);
   hollow(output, 1);
   writeln('Hollow');
   hollow(output, 0);
   raised(output, 1);
   writeln('Raised');
   raised(output, 0);
   font(output, ftterm);
   prtcen(maxy(output), 'Extended effects test');
   waitnext;

   { ******** Character sizes and positions test ******** }

   page;
   grid;
   auto(output, false);
   fsiz := chrsizy(output); { save character size to restore }
   font(output, ftsign);
   fontsiz(output, maxyg(output) div 12);
   writeln('Size of test string: ', strsiz(output, s3):1);
   writeln;
   x := (maxxg(output) div 2)-(strsiz(output, s3) div 2);
   cursorg(output, x, curyg(output)); { go to centered }
   bcolor(output, cyan);
   bover(output);
   writeln(s3);
   fcolor(output, white);
   frect(output, x, curyg(output), x+strsiz(output, s3)-1,
         curyg(output)+chrsizy(output)-1);
   fcolor(output, black);
   rect(output, x, curyg(output), x+strsiz(output, s3)-1,
        curyg(output)+chrsizy(output)-1);
   for i := 0 to strlen(s3)-1 do
      line(output, x+chrpos(output, s3, i), curyg(output),
           x+chrpos(output, s3, i),
           curyg(output)+chrsizy(output)-1);
   writeln;

   l := strsiz(output, s4); { get minimum sizing for string }
   justcenter(s4, l);
   justcenter(s4, l+40);
   justcenter(s4, l+80);

   fontsiz(output, fsiz); { restore font size }
   font(output, ftterm);
   binvis(output);
   prtcen(maxy(output), 'Character sizes and positions');
   waitnext;
   bcolor(output, white);

   { ******** Polar text lines test ******** }

   page;
   grid;
   auto(output, false); { rotated text is incompatible with the text grid }
   x := maxxg(output) div 2; { window center }
   y := maxyg(output) div 2;
   l := chrsizx(output)*5; { start radius - 5 char widths from center }
   fcolor(output, black);
   bover(output);
   a := 0;
   while a < 360 do begin

      { Endpoint of radial at angle a, radius l. }
      rectcord(a, l, tx1, ty1);
      { Rotate text so it reads OUTWARD along the radial. path uses
        the same compass convention as rectcord (0 = north, 90 = east),
        ratioed to INT_MAX = 360 degrees. }
      path(output, a*degree);
      { Shift origin perpendicular to the drawing direction by half the
        character height, so the radial passes through the vertical
        center of the text (not the default top-left origin). }
      f := a*0.01745329; { degrees to radians (matches rectcord) }
      cursorg(output,
              x+tx1 - trunc(chrsizy(output)/2.0 * cos(f)),
              y-ty1 - trunc(chrsizy(output)/2.0 * sin(f)));
      write('this is a test string');
      a := a+10

   end;
   path(output, i32max div 4); { restore default (90 deg / east-reading) }
   binvis(output);
   prtcen(maxy(output), 'Polar text lines');
   waitnext;
   auto(output, true); { re-enable the text grid }

   { ******** Graphical tabbing test ******** }

   page;
   grid;
   auto(output, false);
   font(output, ftterm);
   for i := 1 to 5 do begin

      for x := 1 to i do write(chr(9));
      writeln('Terminal tab: ', i:1)

   end;
   clrtab(output);
   for i := 1 to 5 do settabg(output, i*43);
   for i := 1 to 5 do begin

      for x := 1 to i do write(chr(9));
      writeln('Graphical tab number: ', i:1, ' position: ', i*43:1)

   end;
   restabg(output, 2*43);
   restabg(output, 4*43);
   writeln;
   writeln('After removing tabs ', 2*43:1, ' and ', 4*43:1);
   writeln;
   for i := 1 to 5 do begin

      for x := 1 to i do write(chr(9));
      writeln('Graphical tab number: ', i:1)

   end;
   prtcen(maxy(output), 'Graphical tabbing test');
   waitnext;

   { ************************** Picture draw test **************************** }

   page;
   grid;
   maknam(fn, '', 'mypic', '');
   loadpict(output, 1, fn);
   writeln('Picture size for 1: x: ', pictsizx(output, 1):1, ' y: ',
           pictsizy(output, 1):1);
   maknam(fn, '', 'mypic1', 'bmp');
   loadpict(output, 2, fn);
   writeln('Picture size for 2: x: ', pictsizx(output, 2):1, ' y: ',
           pictsizy(output, 2):1);
   writeln;
   y := curyg(output);
   xspace := maxxg(output) div 20;
   xsize := maxxg(output) div 6;
   yspace := xspace;
   ysize := xsize;
   picture(output, 1, xspace, y, xspace+xsize, y+ysize);
   picture(output, 1, xspace+xsize, y+ysize, xspace+xsize*2, y+ysize*2);
   picture(output, 1, xspace, y+ysize*2, xspace+xsize, y+ysize*3);
   picture(output, 2, xspace+maxxg(output) div 2, y,
                      xspace+xsize+maxxg(output) div 2, y+ysize);
   picture(output, 2, xspace+xsize+maxxg(output) div 2, y+ysize,
                      xspace+xsize*2+maxxg(output) div 2, y+ysize+ysize div 2);
   picture(output, 2, xspace+maxxg(output) div 2, y+ysize*2,
                      xspace+xsize div 2+maxxg(output) div 2, y+ysize*3);
   delpict(output, 1);
   delpict(output, 2);
   prtcen(maxy(output), 'Picture draw test');
   waitnext;

   { ********************** Invisible foreground test ************************ }

   page;
   grid;
   writeln;
   bover(output);
   finvis(output);
   graphtest(1);
   binvis(output);
   fover(output);
   prtcen(maxy(output), 'Invisible foreground test');
   waitnext;
   fover(output);

   { ********************** Invisible background test ************************ }

   page;
   grid;
   writeln;
   binvis(output);
   fover(output);
   graphtest(1);
   binvis(output);
   fover(output);
   prtcen(maxy(output), 'Invisible background test');
   waitnext;
   bover(output);

   { ************************** Xor foreground test ************************** }

   page;
   grid;
   writeln;
   bover(output);
   fxor(output);
   graphtest(1);
   binvis(output);
   fover(output);
   prtcen(maxy(output), 'Xor foreground test');
   waitnext;
   fover(output);

   { ************************* Xor background test *************************** }

   page;
   grid;
   writeln;
   bxor(output);
   fover(output);
   graphtest(1);
   binvis(output);
   fover(output);
   prtcen(maxy(output), 'Xor background test');
   waitnext;
   bover(output);

   { ********************** Graphical scrolling test *********************** }

   page;
   grid;
   binvis(output);
   prtcen(1, 'Use up, down, right and left keys to scroll by pixel');
   prtcen(2, 'Hit enter to continue');
   prtcen(3, 'Note that edges will clear to green as screen moves');
   prtcen(maxy(output), 'Graphical scrolling test');
   bcolor(output, green);
   repeat
      event(input, er);
      if er.etype = etup then scrollg(output, 0, -1);
      if er.etype = etdown then scrollg(output, 0, 1);
      if er.etype = etright then scrollg(output, 1, 0);
      if er.etype = etleft then scrollg(output, -1, 0);
      if er.etype = etterm then goto 99
   until er.etype = etenter;
   bover(output);
   bcolor(output, white);

   { ********************** Graphical mouse movement test ***************** }

   page;
   prtcen(1, 'Move the mouse around');
   prtcen(3, 'Hit Enter to continue');
   prtcen(maxy(output), 'Graphical mouse movement test');
   x := -1;
   y := -1;
   repeat
      event(input, er);
      if er.etype = etmoumovg then begin
         { NO short-circuit: nest the position test }
         if x > 0 then if y > 0 then line(output, x, y, er.moupxg, er.moupyg);
         x := er.moupxg;
         y := er.moupyg
      end;
      if er.etype = etterm then goto 99
   until er.etype = etenter;

   { ************************** Animation test ***************************** }

   squares;

   { ************************** View offset test ************************** }

   page;
   auto(output, false); { turn off autoscroll }
   curvis(output, false); { turn off cursor }
   edge;
   { move the origin right and down }
   viewoffg(output, maxxg(output) div 2, maxyg(output) div 2);
   grid;
   fcolor(output, green);
   frect(output, 0, 0, 100, 100);
   cursorg(output, 1, -(maxyg(output) div 2));
   { reset origin }
   viewoffg(output, 0, 0);
   fcolor(output, black);
   prtcen(maxy(output), 'View offset test');
   prtcen(1, 'The 1,1 origin is now at screen center');
   waitnext;
   auto(output, true); { back to normal character }
   curvis(output, true); { turn on cursor }

   { ************************** View scale test ************************** }

   page;
   auto(output, false);
   curvis(output, false); { turn off cursor }
   edge;
   viewscale(output, 0.5, 0.5);
   grid;
   fcolor(output, green);
   frect(output, 0, 0, 100, 100);
   fcolor(output, black);
   { reset origin and scale }
   viewscale(output, 1.0, 1.0);
   viewoffg(output, 0, 0);
   prtcen(1, 'Logical coordinates are now 1/2 size');
   prtcen(maxy(output), 'View scale test');
   waitnext;
   auto(output, true); { back to normal character }
   curvis(output, true); { turn on cursor }

   { ************************ Viewport scaling test ************************** }

   page;
   auto(output, false);
   curvis(output, false); { turn off cursor }
   fsiz := chrsizy(output); { save default font size }
   font(output, ftsign);
   cx := maxxg(output) div 2; { center x }
   cy := maxyg(output) div 2; { center y }
   ww := maxxg(output);       { window width }
   wh := maxyg(output);       { window height }
   gs := 80;                  { gate size }
   vsx := 1.0; vsy := 1.0;
   vox := 0; voy := 0;
   done := false;
   { initial offset: center the drawing }
   vox := trunc(ww/2 - (cx-1) * vsx);
   voy := trunc(wh/2 - (cy-1) * vsy);
   viewoffg(output, vox, voy);
   while not done do begin
      page;
      { draw boundary lines showing valid coordinate space }
      fcolor(output, cyan);
      line(output, 1, 1, maxxg(output), 1);
      line(output, 1, maxyg(output), maxxg(output), maxyg(output));
      line(output, 1, 1, 1, maxyg(output));
      line(output, maxxg(output), 1, maxxg(output), maxyg(output));
      { draw a simple CMOS inverter schematic }
      fcolor(output, black);
      { VDD and VSS rails }
      line(output, cx-gs*2, cy-gs*2, cx+gs*2, cy-gs*2); { VDD }
      line(output, cx-gs*2, cy+gs*2, cx+gs*2, cy+gs*2); { VSS }
      { PMOS: gate on left, source/drain vertical }
      rect(output, cx-gs div 2, cy-gs*3 div 2, cx+gs div 2, cy-gs div 2); { body }
      line(output, cx-gs, cy-gs, cx-gs div 2, cy-gs); { gate }
      line(output, cx, cy-gs*2, cx, cy-gs*3 div 2); { source to VDD }
      line(output, cx, cy-gs div 2, cx, cy); { drain to mid }
      { NMOS: gate on left, source/drain vertical }
      rect(output, cx-gs div 2, cy+gs div 2, cx+gs div 2, cy+gs*3 div 2); { body }
      line(output, cx-gs, cy+gs, cx-gs div 2, cy+gs); { gate }
      line(output, cx, cy+gs*3 div 2, cx, cy+gs*2); { source to VSS }
      line(output, cx, cy, cx, cy+gs div 2); { drain from mid }
      { input line }
      line(output, cx-gs*2, cy, cx-gs, cy);
      line(output, cx-gs, cy-gs, cx-gs, cy+gs);
      { output line }
      line(output, cx, cy, cx+gs*2, cy);
      { output dot }
      fellipse(output, cx+gs div 4-5, cy-5, cx+gs div 4+5, cy+5);
      { labels }
      fontsiz(output, 20);
      cursorg(output, cx-gs div 4, cy-gs*2-5);
      write('VDD');
      cursorg(output, cx-gs div 4, cy+gs*2+20);
      write('VSS');
      cursorg(output, cx-gs*2-30, cy+6);
      write('IN');
      cursorg(output, cx+gs*2+5, cy+6);
      write('OUT');
      { status at top, caption at bottom - draw at identity scale
        so UI text stays the same physical size regardless of zoom }
      viewscale(output, 1.0, 1.0);
      viewoffg(output, 0, 0);
      fontsiz(output, fsiz);
      font(output, ftterm);
      { status line: build directly with write/cursor rather than sprintf }
      cursorg(output, 1, 1);
      write('Sx:', vsx:1:2, ' Sy:', vsy:1:2, ' Off:', vox:1, ',', voy:1,
            ' PgUp/Dn=zoom Arrows=pan Home/End=Yzoom Enter=next');
      prtcen(maxy(output), 'View drawing scale test');
      { restore the current scale for next redraw }
      viewscale(output, vsx, vsy);
      viewoffg(output, vox, voy);
      { wait for key }
      repeat event(input, er) until (er.etype = etenter) or
         (er.etype = etterm) or (er.etype = etpagu) or
         (er.etype = etpagd) or (er.etype = etup) or
         (er.etype = etdown) or (er.etype = etleft) or
         (er.etype = etright) or (er.etype = ethomel) or
         (er.etype = etendl);
      if er.etype = etterm then goto 99
      else if er.etype = etenter then done := true
      else if er.etype = etpagu then begin
         vsx := vsx*1.25;
         vsy := vsy*1.25;
         viewscale(output, vsx, vsy);
         vox := trunc(ww/2 - (cx-1) * vsx);
         voy := trunc(wh/2 - (cy-1) * vsy);
         viewoffg(output, vox, voy)
      end
      else if er.etype = etpagd then begin
         vsx := vsx/1.25;
         vsy := vsy/1.25;
         if vsx < 0.01 then vsx := 0.01;
         if vsy < 0.01 then vsy := 0.01;
         viewscale(output, vsx, vsy);
         vox := trunc(ww/2 - (cx-1) * vsx);
         voy := trunc(wh/2 - (cy-1) * vsy);
         viewoffg(output, vox, voy)
      end
      else if er.etype = ethomel then begin
         vsy := vsy*1.25;
         viewscale(output, vsx, vsy);
         voy := trunc(wh/2 - (cy-1) * vsy);
         viewoffg(output, vox, voy)
      end
      else if er.etype = etendl then begin
         vsy := vsy/1.25;
         if vsy < 0.01 then vsy := 0.01;
         viewscale(output, vsx, vsy);
         voy := trunc(wh/2 - (cy-1) * vsy);
         viewoffg(output, vox, voy)
      end
      else if er.etype = etup then begin
         voy := voy-20;
         viewoffg(output, vox, voy)
      end
      else if er.etype = etdown then begin
         voy := voy+20;
         viewoffg(output, vox, voy)
      end
      else if er.etype = etleft then begin
         vox := vox-20;
         viewoffg(output, vox, voy)
      end
      else if er.etype = etright then begin
         vox := vox+20;
         viewoffg(output, vox, voy)
      end
   end;
   { reset to identity }
   viewscale(output, 1.0, 1.0);
   viewoffg(output, 0, 0);
   curvis(output, false); { turn off cursor }

   { ************************** Benchmarks **************************** }

   bover(output);

   benchtest(linespeed, bnline1, 1);
   i := benchtab[bnline1].iter;
   s := benchtab[bnline1].time;
   writeln('Line speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per line ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(linespeed, bnline10, 10);
   i := benchtab[bnline10].iter;
   s := benchtab[bnline10].time;
   writeln('Line speed for width: 10, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per line ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(rectspeed, bnrect1, 1);
   i := benchtab[bnrect1].iter;
   s := benchtab[bnrect1].time;
   writeln('Rectangle speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per rectangle ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(rectspeed, bnrect10, 10);
   i := benchtab[bnrect10].iter;
   s := benchtab[bnrect10].time;
   writeln('Rectangle speed for width: 10, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per rectangle ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(rrectspeed, bnrrect1, 1);
   i := benchtab[bnrrect1].iter;
   s := benchtab[bnrrect1].time;
   writeln('Rounded rectangle speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per rounded rectangle ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(rrectspeed, bnrrect10, 10);
   i := benchtab[bnrrect10].iter;
   s := benchtab[bnrrect10].time;
   writeln('Rounded rectangle speed for width: 10, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per rounded rectangle ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(frectspeed, bnfrect, 1);
   i := benchtab[bnfrect].iter;
   s := benchtab[bnfrect].time;
   writeln('Filled rectangle speed, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per filled rectangle ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(frrectspeed, bnfrrect, 1);
   i := benchtab[bnfrrect].iter;
   s := benchtab[bnfrrect].time;
   writeln('Filled rounded rectangle speed, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per filled rounded rectangle ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(ellipsespeed, bnellipse1, 1);
   i := benchtab[bnellipse1].iter;
   s := benchtab[bnellipse1].time;
   writeln('Ellipse speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per ellipse ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(ellipsespeed, bnellipse10, 10);
   i := benchtab[bnellipse10].iter;
   s := benchtab[bnellipse10].time;
   writeln('Ellipse speed for width: 10, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per ellipse ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(fellipsespeed, bnfellipse, 1);
   i := benchtab[bnfellipse].iter;
   s := benchtab[bnfellipse].time;
   writeln('Filled ellipse speed, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per filled ellipse ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(arcspeed, bnarc1, 1);
   i := benchtab[bnarc1].iter;
   s := benchtab[bnarc1].time;
   writeln('Arc speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per arc ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(arcspeed, bnarc10, 1);
   i := benchtab[bnarc10].iter;
   s := benchtab[bnarc10].time;
   writeln('Arc speed for width: 10, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per arc ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(farcspeed, bnfarc, 1);
   i := benchtab[bnfarc].iter;
   s := benchtab[bnfarc].time;
   writeln('Filled arc speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per filled arc ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(fchordspeed, bnfchord, 1);
   i := benchtab[bnfchord].iter;
   s := benchtab[bnfchord].time;
   writeln('Filled chord speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per filled chord ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(ftrianglespeed, bnftriangle, 1);
   i := benchtab[bnftriangle].iter;
   s := benchtab[bnftriangle].time;
   writeln('Filled triangle speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per filled triangle ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   bover(output);
   fover(output);
   benchtest(ftextspeed, bntext, 1);
   i := benchtab[bntext].iter;
   s := benchtab[bntext].time;
   home(output);
   writeln('Text speed, with overwrite, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per write ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   binvis(output);
   fover(output);
   benchtest(ftextspeed, bntextbi, 1);
   i := benchtab[bntextbi].iter;
   s := benchtab[bntextbi].time;
   home(output);
   bover(output);
   writeln('Text speed, invisible background, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per write ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(fpictspeed, bnpict, 1);
   i := benchtab[bnpict].iter;
   s := benchtab[bnpict].time;
   writeln('Picture draw speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per picture ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   benchtest(fpictnsspeed, bnpictns, 1);
   i := benchtab[bnpictns].iter;
   s := benchtab[bnpictns].time;
   writeln('No scale picture draw speed for width: 1, ', i:1, ' iterations ', s*0.0001:1:6, ' seconds');
   writeln('Seconds per picture ', s*0.0001/i:1:6);
   if chkbrk then; { check user break }

   { output table }

   writeln;
   writeln('Benchmark table');
   writeln;
   writeln('Type                        Seconds   Per fig');
   writeln('--------------------------------------------------');
   for bi := bnline1 to bnpictns do begin

      case bi of { benchmark type }

         bnline1:     write('line width 1                ');
         bnline10:    write('line width 10               ');
         bnrect1:     write('rectangle width 1           ');
         bnrect10:    write('rectangle width 10          ');
         bnrrect1:    write('rounded rectangle width 1   ');
         bnrrect10:   write('rounded rectangle width 10  ');
         bnfrect:     write('filled rectangle            ');
         bnfrrect:    write('filled rounded rectangle    ');
         bnellipse1:  write('ellipse width 1             ');
         bnellipse10: write('ellipse width 10            ');
         bnfellipse:  write('filled ellipse              ');
         bnarc1:      write('arc width 1                 ');
         bnarc10:     write('arc width 10                ');
         bnfarc:      write('filled arc                  ');
         bnfchord:    write('filled chord                ');
         bnftriangle: write('filled triangle             ');
         bntext:      write('text                        ');
         bntextbi:    write('background invisible text   ');
         bnpict:      write('Picture draw                ');
         bnpictns:    write('No scaling picture draw     ')

      end;
      write(benchtab[bi].time*0.0001:6:2, '    ');
      writeln(benchtab[bi].time*0.0001/benchtab[bi].iter:1:6)

   end;

   99: ; { terminate }

   page;
   auto(output, false);
   font(output, ftsign);
   fontsiz(output, 50);
   prtceng(maxy(output) div 2, 'Test complete')

end.
