{******************************************************************************
*                                                                             *
*                             BACKGAMMON GAME                                 *
*                                                                             *
*                       COPYRIGHT (C) 2026 S. A. FRANCO                       *
*                                                                             *
* A graphical backgammon game with resizable window and menus. Implements    *
* full backgammon rules including hitting, bearing off, bar re-entry, and    *
* doubles. Supports human vs human and human vs computer play.               *
* Computer uses evaluation-based search over all legal move combinations.    *
*                                                                             *
* This is the Pascaline port of the Ami reference game backgammon.c.         *
*                                                                             *
******************************************************************************}

program backgammon(input, output);

uses graphics,
     sound,
     services,
     strings;

label 99; { terminate }

const

   { sound defines (precomputed from sound.pas note/octave constants) }
   dicenote    = 56;    { note_g+octave_5 }
   movenote    = 53;    { note_e+octave_5 }
   hitnote     = 61;    { note_c+octave_6 }
   bearoffnote = 58;    { note_a+octave_5 }
   winnote     = 37;    { note_c+octave_4 }
   dicedur     = 200;
   movedur     = 100;
   hitdur      = 200;
   bearoffdur  = 250;
   windur      = 800;

   i32max = 2147483647; { 32 bit INT_MAX, base for RGB ratios }

   { game constants }
   numpoints   = 24;
   numcheckers = 15;
   maxstack    = 15;

   { players: p1 = white, moves 24->1; p2 = black, moves 1->24 }
   p1 = 0;
   p2 = 1;

   { menu ids }
   menunew    = 100;
   menuexit   = 101;
   menuabout  = 102;
   menupvp    = 103;
   menupvcw   = 104;
   menupvcb   = 105;
   menuundo   = 106;
   menudouble = 107;

   { game modes }
   modepvp  = 0;
   modepvcw = 1;   { player is white (p1), computer is black (p2) }
   modepvcb = 2;   { player is black (p2), computer is white (p1) }

   { AI timer }
   timerai   = 2;
   timeranim = 3;
   animtick  = 800;   { ~80ms per animation frame }

   { game states }
   gsroll     = 0;   { waiting to roll dice }
   gsmove     = 1;   { selecting moves }
   gsgameover = 2;   { game finished }

   { max move sequences for AI evaluation }
   maxcombos = 5000;

   { undo history }
   maxundo = 20;

   { timed status message overlay }
   timermsg    = 4;
   msgduration = 20000;  { 2 seconds }

   { animation }
   animframes = 15;

   { destination list }
   maxdests = 8;

   { wave table ids }
   waveslide = 1;
   wavedice  = 2;

   { font codes (from amitk/include/graphics.h) }
   ftsign = 3;          { AMI_FONT_SIGN }

type

   boardarr = array [0..23] of integer;
   pairarr  = array [0..1] of integer;
   dicearr  = array [0..3] of integer;
   usedarr  = array [0..3] of boolean;
   seenarr  = array [0..29] of boolean;
   comboarr = array [0..3, 0..29] of boolean;
   dset     = set of 0..3;

   destinfo = record

      dest: integer; { -1 = bear off, 0-23 = point }
      dv:   integer  { die value used }

   end;
   destarr  = array [0..7] of destinfo;

   undostate = record

      board:         boardarr;
      bar:           pairarr;
      off:           pairarr;
      dice:          pairarr;
      used:          usedarr;
      ndice:         integer;
      turn:          integer;
      selectedpoint: integer

   end;

   { save/restore state for AI }
   aistate = record

      board: boardarr;
      bar:   pairarr;
      off:   pairarr;
      used:  usedarr

   end;

var

   astf:          pstring; { scratch asset-file path (getpgm-resolved) }

   { board state: board[0..23] = number of checkers on each point
     (board[0] = point 1, board[23] = point 24)
     positive = p1 (white), negative = p2 (black) }
   board:         boardarr;
   bar:           pairarr; { checkers on the bar }
   off:           pairarr; { checkers borne off }
   turn:          integer; { p1 or p2 }
   gamestate:     integer; { gsroll, gsmove, gsgameover }
   gamemode:      integer; { modepvp, modepvcw, modepvcb }
   aipending:     boolean;

   dice:          pairarr; { current dice values }
   used:          usedarr; { which dice are used (up to 4 for doubles) }
   ndice:         integer; { 2 normally, 4 for doubles }
   selectedpoint: integer; { currently selected source point, -1 = none,
                             24 = bar for p1, 25 = bar for p2 }
   mousex, mousey: integer;
   soundenabled:  boolean;

   { timed status message overlay }
   overlaymsg:    pstring;
   overlayactive: boolean;

   { doubling cube }
   cubevalue:     integer; { current cube value: 1, 2, 4, 8, 16, 32, 64 }
   cubeowner:     integer; { -1 = center (either can double), p1 or p2 }
   score:         pairarr; { cumulative match score }

   { animation state }
   animating:     boolean; { true during move animation }
   animframe:     integer; { current frame 0..animframes }
   animsx, animsy: integer; { source pixel position }
   animdx, animdy: integer; { destination pixel position }
   animplayer:    integer; { which player's checker is moving }

   { undo stack }
   undostack:     array [0..19] of undostate;
   undotop:       integer; { index of next free slot }

   { board coordinate helpers }
   scrw, scrh:    integer;
   brdx, brdy, brdw, brdh: integer; { board rectangle }
   barw:          integer; { central bar width }
   pointw:        integer; { width of each point/triangle }
   bearw:         integer; { bearing off tray width }
   checkerr:      integer; { checker radius }

   { AI search state }
   aibestscore:   integer;
   aicombocount:  integer;

   { random number generator state }
   randstate:     integer;

   { main program work variables }
   er:            evtrec;  { event record }
   pf:            text;    { position save file }
   pw, ph:        integer; { saved window size }
   wx, wy:        integer; { current window size }
   okw, okh:      boolean; { position file parse flags }
   bdummy:        boolean; { discard for undopop result }

{ ****************************************************************************

   Helper routines

   **************************************************************************** }

{ find random number between 0 and N (linear congruential generator, since
  the runtime exposes no rand primitive) }

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

{ color helper (C CLR macro: v*(INT_MAX/255)) }

function clr(v: integer): integer;

begin

   clr := v * (i32max div 255)

end;

{ make a single character dynamic string }

function chstr(c: char): pstring;

var a: packed array [1..1] of char;

begin

   a[1] := c;
   chstr := copy(a)

end;

{ concatenate a single character to a dynamic string }

function catc(s: pstring; c: char): pstring;

var a: packed array [1..1] of char;

begin

   a[1] := c;
   catc := cat(s, a)

end;

{ player name string }

function plname(p: integer): pstring;

begin

   if p = p1 then plname := copy('White')
   else plname := copy('Black')

end;

{ read an integer from a text file line, skipping any non-digit characters
  (replaces the C sscanf of the position file) }

procedure rdint(var f: text; var v: integer; var ok: boolean);

var dn: boolean;

begin

   v := 0;
   ok := false;
   { skip to next digit on the line }
   dn := false;
   while not dn do
      if eoln(f) then dn := true
      else if (f^ >= '0') and (f^ <= '9') then dn := true
      else get(f);
   { accumulate digits }
   dn := false;
   while not dn do
      if eoln(f) then dn := true
      else if (f^ >= '0') and (f^ <= '9') then begin

         v := v*10+(ord(f^)-ord('0'));
         ok := true;
         get(f)

      end else dn := true

end;

{ ****************************************************************************

   Initialization

   **************************************************************************** }

procedure initboard;

var i: integer;

begin

   for i := 0 to 23 do board[i] := 0;
   bar[p1] := 0; bar[p2] := 0;
   off[p1] := 0; off[p2] := 0;

   { standard backgammon starting position.
     p1 (white, positive) moves from point 24 toward point 1.
     p2 (black, negative) moves from point 1 toward point 24.
     board[i] represents point (i+1). }

   { p1: 2 on point 24, 5 on point 13, 3 on point 8, 5 on point 6 }
   board[23] := 2;   { point 24 }
   board[12] := 5;   { point 13 }
   board[7]  := 3;   { point 8 }
   board[5]  := 5;   { point 6 }

   { p2: 2 on point 1, 5 on point 12, 3 on point 17, 5 on point 19 }
   board[0]  := -2;  { point 1 }
   board[11] := -5;  { point 12 }
   board[16] := -3;  { point 17 }
   board[18] := -5;  { point 19 }

   turn := p1;
   gamestate := gsroll;
   aipending := false;
   selectedpoint := -1;
   dice[0] := 0; dice[1] := 0;
   ndice := 0;
   undotop := 0;
   cubevalue := 1;
   cubeowner := -1; { center }
   for i := 0 to 3 do used[i] := false

end;

{ ****************************************************************************

   Dice

   **************************************************************************** }

procedure rolldice;

begin

   dice[0] := randr(1, 6);
   dice[1] := randr(1, 6);
   if dice[0] = dice[1] then ndice := 4
   else ndice := 2;
   used[0] := false; used[1] := false;
   used[2] := false; used[3] := false

end;

{ get the value of die slot i }

function dieval(i: integer): integer;

var r: integer;

begin

   r := 0;
   if ndice = 4 then r := dice[0] { all four are same value }
   else if i < 2 then r := dice[i];
   dieval := r

end;

{ get available (unused) dice values into array, return count }

procedure getavaildice(var vals: dicearr; var n: integer);

var i: integer;

begin

   n := 0;
   for i := 0 to ndice-1 do
      if not used[i] then begin

         vals[n] := dieval(i);
         n := n+1

      end

end;

{ count remaining unused dice }

function diceremaining: integer;

var i, n: integer;

begin

   n := 0;
   for i := 0 to ndice-1 do
      if not used[i] then n := n+1;
   diceremaining := n

end;

{ mark a die with given value as used. Returns index used, or -1 }

function usedie(v: integer): integer;

var i, r: integer;

begin

   r := -1;
   for i := 0 to ndice-1 do
      if r < 0 then
         if not used[i] then
            if dieval(i) = v then begin

               used[i] := true;
               r := i

            end;
   usedie := r

end;

{ unuse a die at index }

procedure unusedie(idx: integer);

begin

   if (idx >= 0) and (idx <= 3) then used[idx] := false

end;

{ ****************************************************************************

   Move legality

   **************************************************************************** }

{ check if all of player's checkers are in their home board }

function allinhome(player: integer): boolean;

var i: integer;
    r: boolean;

begin

   r := true;
   if player = p1 then begin

      if bar[p1] > 0 then r := false;
      for i := 6 to 23 do
         if board[i] > 0 then r := false

   end else begin

      if bar[p2] > 0 then r := false;
      for i := 0 to 17 do
         if board[i] < 0 then r := false

   end;
   allinhome := r

end;

{ check if a point is occupied by opponent (more than 1 checker) }

function pointblocked(pt, player: integer): boolean;

var r: boolean;

begin

   r := false;
   if (pt >= 0) and (pt <= 23) then begin

      if player = p1 then r := board[pt] < -1
      else r := board[pt] > 1

   end;
   pointblocked := r

end;

{ check if a point has a single opponent checker (blot) }

function pointisblot(pt, player: integer): boolean;

var r: boolean;

begin

   r := false;
   if (pt >= 0) and (pt <= 23) then begin

      if player = p1 then r := board[pt] = -1
      else r := board[pt] = 1

   end;
   pointisblot := r

end;

{ can player move a checker from the bar with die value dv? }

function canenter(player, dv: integer): boolean;

var dest: integer;
    r:    boolean;

begin

   if player = p1 then dest := 24-dv  { entering p2's home (points 19-24) }
   else dest := dv-1;                 { entering p1's home (points 1-6) }
   if (dest < 0) or (dest > 23) then r := false
   else r := not pointblocked(dest, player);
   canenter := r

end;

{ destination point when moving from src with die value dv.
  Returns -1 for bear off, -2 for invalid. }

function movedest(src, dv, player: integer): integer;

var dest, i, r: integer;
    fin:        boolean;

begin

   r := -2;
   fin := false;
   if player = p1 then begin

      dest := src-dv;
      if dest < 0 then begin

         { bearing off }
         if not allinhome(p1) then r := -2
         else if dest = -1 then r := -1 { exact bear off from point (dv) }
         else begin

            { can only bear off with higher die if no checker on higher point }
            r := -1;
            for i := src+1 to 5 do
               if board[i] > 0 then r := -2

         end;
         fin := true

      end

   end else begin

      dest := src+dv;
      if dest > 23 then begin

         if not allinhome(p2) then r := -2
         else if dest = 24 then r := -1
         else begin

            r := -1;
            for i := src-1 downto 18 do
               if board[i] < 0 then r := -2

         end;
         fin := true

      end

   end;
   if not fin then begin

      if pointblocked(dest, player) then r := -2
      else r := dest

   end;
   movedest := r

end;

{ can player move from src with die value dv? src is board index 0-23 }

function canmovefrom(src, dv, player: integer): boolean;

var r: boolean;

begin

   r := true;
   if bar[player] > 0 then r := false { must enter from bar first }
   else if (player = p1) and (board[src] <= 0) then r := false
   else if (player = p2) and (board[src] >= 0) then r := false
   else r := movedest(src, dv, player) >= -1;
   canmovefrom := r

end;

{ check if player has ANY legal move with remaining dice }

function hasanymove(player: integer): boolean;

var vals:  dicearr;
    nv, i, j: integer;
    r, skip:  boolean;

begin

   r := false;
   getavaildice(vals, nv);
   if nv > 0 then begin

      if bar[player] > 0 then begin

         for i := 0 to nv-1 do
            if canenter(player, vals[i]) then r := true

      end else
         for j := 0 to 23 do begin

            skip := false;
            if (player = p1) and (board[j] <= 0) then skip := true;
            if (player = p2) and (board[j] >= 0) then skip := true;
            if not skip then
               for i := 0 to nv-1 do
                  if canmovefrom(j, vals[i], player) then r := true

         end

   end;
   hasanymove := r

end;

{ ****************************************************************************

   Execute a move

   **************************************************************************** }

procedure showmessage(view msg: string);

begin

   if overlaymsg <> nil then dispose(overlaymsg);
   overlaymsg := copy(msg);
   overlayactive := true;
   timer(output, timermsg, msgduration, false)

end;

procedure playsound(n, dur: integer);

begin

   if soundenabled then begin

      noteon(synth_out, 0, 1, n, i32max);
      noteoff(synth_out, curtimeout+dur, 1, n, i32max)

   end

end;

procedure playslidesound;

begin

   if soundenabled then playwave(wave_out, 0, waveslide)

end;

{ push current state onto undo stack }

procedure undopush;

begin

   if undotop < maxundo then begin

      undostack[undotop].board := board;
      undostack[undotop].bar := bar;
      undostack[undotop].off := off;
      undostack[undotop].dice := dice;
      undostack[undotop].used := used;
      undostack[undotop].ndice := ndice;
      undostack[undotop].turn := turn;
      undostack[undotop].selectedpoint := selectedpoint;
      undotop := undotop+1

   end

end;

{ pop and restore state from undo stack }

function undopop: boolean;

begin

   if undotop <= 0 then undopop := false
   else begin

      undotop := undotop-1;
      board := undostack[undotop].board;
      bar := undostack[undotop].bar;
      off := undostack[undotop].off;
      dice := undostack[undotop].dice;
      used := undostack[undotop].used;
      ndice := undostack[undotop].ndice;
      turn := undostack[undotop].turn;
      selectedpoint := undostack[undotop].selectedpoint;
      undopop := true

   end

end;

{ ****************************************************************************

   Board coordinate helpers

   **************************************************************************** }

procedure calcmetrics;

begin

   scrw := maxxg(output);
   scrh := maxyg(output);

   brdw := scrw*78 div 100;
   brdh := scrh*88 div 100;
   barw := brdw div 18;
   bearw := brdw div 14;
   pointw := (brdw-barw-bearw) div 12;

   { recalculate brdw to be exact }
   brdw := pointw*12+barw+bearw;
   brdx := (scrw-brdw) div 2;
   brdy := scrh div 15;
   brdh := scrh-brdy*2;

   checkerr := pointw*2 div 5;
   if checkerr < 4 then checkerr := 4

end;

{ get pixel x for center of a point (0-23).
  Top row: points 12-23 left to right (index 12 at left, 23 at right)
  Bottom row: points 11-0 left to right (index 11 at left, 0 at right) }

function pointcx(pt: integer): integer;

var col, x: integer;

begin

   if pt >= 12 then col := pt-12 { top row: point 12 at left }
   else col := 11-pt;            { bottom row: point 11 at left }
   x := brdx+col*pointw+pointw div 2;
   { account for bar in center (after first 6 columns) }
   if col >= 6 then x := x+barw;
   pointcx := x

end;

{ get pixel y for a checker at position 'pos' (0 = base) on a point }

function checkercy(pt, pos: integer): integer;

var spacing: integer;

begin

   spacing := checkerr*2+2;
   if pos > 4 then pos := 4; { stack limit visually }
   if pt >= 12 then
      { top row: triangles point down }
      checkercy := brdy+checkerr+4+pos*spacing
   else
      { bottom row: triangles point up }
      checkercy := brdy+brdh-checkerr-4-pos*spacing

end;

{ get source position for animation - where the checker is now }

procedure getcheckersrcpos(src, player: integer; var px, py: integer);

var cnt: integer;

begin

   if (src = 24) or (src = 25) then begin

      { from bar }
      px := brdx+pointw*6+barw div 2;
      if player = p1 then
         py := brdy+brdh div 2+checkerr+4+(bar[p1]-1)*(checkerr*2+2)
      else
         py := brdy+brdh div 2-checkerr-4-(bar[p2]-1)*(checkerr*2+2)

   end else begin

      if board[src] > 0 then cnt := board[src]
      else cnt := -board[src];
      px := pointcx(src);
      py := checkercy(src, cnt-1)

   end

end;

{ get destination position for animation - where the checker will land }

procedure getcheckerdestpos(dest, player: integer; bearoff: boolean;
                            var px, py: integer);

var cnt, spacing: integer;

begin

   if bearoff then begin

      { bear off tray }
      px := brdx+pointw*12+barw+bearw div 2;
      spacing := checkerr;
      if spacing < 4 then spacing := 4;
      if player = p1 then py := brdy+brdh-8-off[p1]*spacing
      else py := brdy+8+off[p2]*spacing

   end else begin

      if board[dest] > 0 then cnt := board[dest]
      else cnt := -board[dest];
      { if landing on opponent single, it'll be replaced, so count = 0 }
      if board[dest] <> 0 then
         if ((player = p1) and (board[dest] < 0)) or
            ((player = p2) and (board[dest] > 0)) then cnt := 0;
      px := pointcx(dest);
      py := checkercy(dest, cnt)

   end

end;

{ start a visual animation from src to dest position }

procedure startmoveanim(sx, sy, dx, dy, player: integer);

begin

   animsx := sx; animsy := sy;
   animdx := dx; animdy := dy;
   animplayer := player;
   animframe := 0;
   animating := true;
   timer(output, timeranim, animtick, true)

end;

procedure finishanimation;

begin

   animating := false;
   killtimer(output, timeranim)

end;

{ record positions and start animation for a move about to happen.
  Call BEFORE the board update, then do the board update after this. }

procedure animatemove(src, dest, player: integer; frombar, bearoff: boolean);

var sx, sy, dx, dy: integer;

begin

   getcheckersrcpos(src, player, sx, sy);
   getcheckerdestpos(dest, player, bearoff, dx, dy);
   startmoveanim(sx, sy, dx, dy, player);
   playslidesound

end;

{ execute move: from bar if frombar, else from src. dv is die value.
  Returns: 0=normal, 1=hit, 2=bear off }

function domoveinternal(src, dv, player: integer; frombar: boolean): integer;

var dest, res, dmy: integer;

begin

   res := 0;
   if frombar then begin

      bar[player] := bar[player]-1;
      if player = p1 then dest := 24-dv
      else dest := dv-1

   end else begin

      dest := movedest(src, dv, player);
      { remove from source }
      if player = p1 then board[src] := board[src]-1
      else board[src] := board[src]+1

   end;

   if dest = -1 then begin

      { bearing off }
      off[player] := off[player]+1;
      res := 2

   end else begin

      { check for hit }
      if pointisblot(dest, player) then begin

         if player = p1 then begin

            board[dest] := 0;
            bar[p2] := bar[p2]+1

         end else begin

            board[dest] := 0;
            bar[p1] := bar[p1]+1

         end;
         res := 1

      end;
      { place checker }
      if player = p1 then board[dest] := board[dest]+1
      else board[dest] := board[dest]-1

   end;

   dmy := usedie(dv);
   domoveinternal := res

end;

{ animated move: record positions, then execute }

function domove(src, dv, player: integer; frombar: boolean): integer;

var dest, srcpt: integer;
    bearoff:     boolean;

begin

   { save state for undo }
   undopush;

   { figure out destination for animation }
   if frombar then begin

      srcpt := 24; { bar }
      if player = p1 then dest := 24-dv
      else dest := dv-1;
      bearoff := false

   end else begin

      srcpt := src;
      dest := movedest(src, dv, player);
      bearoff := dest = -1

   end;

   { start animation BEFORE changing board state }
   animatemove(srcpt, dest, player, frombar, bearoff);

   { now execute the actual move }
   domove := domoveinternal(src, dv, player, frombar)

end;

{ undo a move (for AI search) }

procedure undomove(src, dest, dv, player: integer; frombar, washit: boolean;
                   dieidx: integer);

begin

   if dest = -1 then begin

      { undo bear off }
      off[player] := off[player]-1;
      if frombar then bar[player] := bar[player]+1
      else begin

         if player = p1 then board[src] := board[src]+1
         else board[src] := board[src]-1

      end

   end else begin

      { remove checker from dest }
      if player = p1 then board[dest] := board[dest]-1
      else board[dest] := board[dest]+1;

      { undo hit }
      if washit then begin

         if player = p1 then begin

            board[dest] := -1;
            bar[p2] := bar[p2]-1

         end else begin

            board[dest] := 1;
            bar[p1] := bar[p1]-1

         end

      end;

      if frombar then bar[player] := bar[player]+1
      else begin

         if player = p1 then board[src] := board[src]+1
         else board[src] := board[src]-1

      end

   end;

   unusedie(dieidx)

end;

{ ****************************************************************************

   End of turn / game over check

   **************************************************************************** }

procedure endturn;

begin

   if off[turn] >= numcheckers then begin

      gamestate := gsgameover;
      playsound(winnote, windur)

   end else begin

      turn := 1-turn;
      gamestate := gsroll;
      selectedpoint := -1

   end

end;

{ ****************************************************************************

   Pip count calculation

   **************************************************************************** }

function pipcount(player: integer): integer;

var i, pips: integer;

begin

   pips := 0;
   if player = p1 then begin

      pips := pips+bar[p1]*25;
      for i := 0 to 23 do
         if board[i] > 0 then pips := pips+board[i]*(i+1)

   end else begin

      pips := pips+bar[p2]*25;
      for i := 0 to 23 do
         if board[i] < 0 then pips := pips+(-board[i])*(24-i)

   end;
   pipcount := pips

end;

{ convert mouse position to a point index, or special values.
  Returns: 0-23 for board points, 24 for bar area, 25 for bear off area,
  -1 for nothing. }

function mousetopoint(mx, my: integer): integer;

var col, pt, halfh, barleft, barright, r: integer;

begin

   halfh := brdh div 2;
   barleft := brdx+pointw*6;
   barright := barleft+barw;

   r := -1;
   if (mx >= brdx) and (mx <= brdx+brdw) and
      (my >= brdy) and (my <= brdy+brdh) then begin

      { check bear off area }
      if mx > brdx+pointw*12+barw then r := 25
      { check bar area }
      else if (mx >= barleft) and (mx <= barright) then r := 24
      else begin

         { determine column (0-11) }
         if mx >= barright then col := (mx-barright) div pointw+6
         else col := (mx-brdx) div pointw;
         if col < 0 then col := 0;
         if col > 11 then col := 11;

         { determine top or bottom half }
         if my < brdy+halfh then pt := col+12 { top row }
         else pt := 11-col;                   { bottom row }
         r := pt

      end

   end;
   mousetopoint := r

end;

{ ****************************************************************************

   Get valid destinations for selected source and available dice

   **************************************************************************** }

function getvaliddests(src: integer; frombar: boolean;
                       var dests: destarr): integer;

var vals:         dicearr;
    nv, i, nd, dest: integer;
    seen:         seenarr; { track unique destinations }
    skip:         boolean;

begin

   for i := 0 to 29 do seen[i] := false;
   getavaildice(vals, nv);
   nd := 0;

   for i := 0 to nv-1 do begin

      skip := false;
      dest := 0;
      if frombar then begin

         if not canenter(turn, vals[i]) then skip := true
         else begin

            if turn = p1 then dest := 24-vals[i]
            else dest := vals[i]-1

         end

      end else begin

         if not canmovefrom(src, vals[i], turn) then skip := true
         else dest := movedest(src, vals[i], turn)

      end;
      { use dest+1 as index (dest can be -1 for bear off) }
      if not skip then
         if not seen[dest+1] then begin

            seen[dest+1] := true;
            if nd < maxdests then begin

               dests[nd].dest := dest;
               dests[nd].dv := vals[i];
               nd := nd+1

            end

         end

   end;
   getvaliddests := nd

end;

{ ****************************************************************************

   Drawing

   **************************************************************************** }

procedure drawboardbg;

var i, x1, y1, x2, y2, cx, trih:  integer;
    midx, row, lx, rx, halfw:     integer;
    coloridx:                     integer;

begin

   { background }
   fcolorg(output, clr(50), clr(50), clr(50));
   frect(output, 1, 1, scrw, scrh);

   { board border (wood frame) }
   fcolorg(output, clr(101), clr(67), clr(33));
   frect(output, brdx-6, brdy-6, brdx+brdw+6, brdy+brdh+6);

   { board felt background (dark green) }
   fcolorg(output, clr(0), clr(80), clr(40));
   frect(output, brdx, brdy, brdx+brdw, brdy+brdh);

   { central bar (dark wood) }
   midx := brdx+pointw*6;
   fcolorg(output, clr(90), clr(55), clr(25));
   frect(output, midx, brdy, midx+barw, brdy+brdh);

   { bearing off tray }
   x1 := brdx+pointw*12+barw;
   fcolorg(output, clr(80), clr(50), clr(20));
   frect(output, x1, brdy, x1+bearw, brdy+brdh);
   { dividing line }
   fcolorg(output, clr(60), clr(35), clr(10));
   line(output, x1, brdy+brdh div 2, x1+bearw, brdy+brdh div 2);

   { draw triangles (points) }
   trih := brdh*2 div 5;

   for i := 0 to 23 do begin

      coloridx := i mod 2;
      cx := pointcx(i);
      x1 := cx-pointw div 2+1;
      x2 := cx+pointw div 2-1;

      if coloridx = 0 then
         fcolorg(output, clr(139), clr(90), clr(43))   { dark brown }
      else
         fcolorg(output, clr(222), clr(198), clr(158)); { cream/tan }

      if i >= 12 then begin

         { top row: triangle points down }
         y1 := brdy+1;
         { draw triangle as series of horizontal lines }
         for row := 0 to trih do begin

            halfw := (x2-x1)*(trih-row) div (2*trih);
            lx := cx-halfw;
            rx := cx+halfw;
            frect(output, lx, y1+row, rx, y1+row)

         end

      end else begin

         { bottom row: triangle points up }
         y2 := brdy+brdh-1;
         for row := 0 to trih do begin

            halfw := (x2-x1)*(trih-row) div (2*trih);
            lx := cx-halfw;
            rx := cx+halfw;
            frect(output, lx, y2-row, rx, y2-row)

         end

      end

   end

end;

procedure drawchecker(cx, cy, player: integer; highlight: boolean);

var r: integer;

begin

   r := checkerr;

   { outline }
   fcolorg(output, clr(30), clr(30), clr(30));
   fellipse(output, cx-r, cy-r, cx+r, cy+r);

   { body }
   r := r-2;
   if r < 2 then r := 2;
   if player = p1 then begin

      if highlight then fcolorg(output, clr(200), clr(255), clr(200))
      else fcolorg(output, clr(240), clr(235), clr(220))

   end else begin

      if highlight then fcolorg(output, clr(150), clr(100), clr(100))
      else fcolorg(output, clr(50), clr(40), clr(35))

   end;
   fellipse(output, cx-r, cy-r, cx+r, cy+r);

   { inner ring for 3D effect }
   r := r*2 div 3;
   if r < 1 then r := 1;
   if player = p1 then fcolorg(output, clr(255), clr(250), clr(240))
   else fcolorg(output, clr(70), clr(55), clr(50));
   fellipse(output, cx-r, cy-r, cx+r, cy+r);

   { center dot }
   r := r div 3;
   if r < 1 then r := 1;
   if player = p1 then fcolorg(output, clr(220), clr(215), clr(200))
   else fcolorg(output, clr(45), clr(35), clr(30));
   fellipse(output, cx-r, cy-r, cx+r, cy+r)

end;

procedure drawcheckers;

var i, j, cnt, player, cx, cy:  integer;
    barcx, spacing, trayx:      integer;
    issel:                      boolean;

begin

   for i := 0 to 23 do
      if board[i] <> 0 then begin

         if board[i] > 0 then player := p1 else player := p2;
         if board[i] > 0 then cnt := board[i] else cnt := -board[i];
         issel := selectedpoint = i;

         for j := 0 to cnt-1 do begin

            cx := pointcx(i);
            cy := checkercy(i, j);
            { if last checker in selected stack, highlight }
            drawchecker(cx, cy, player, issel and (j = cnt-1))

         end

      end;

   { draw bar checkers }
   barcx := brdx+pointw*6+barw div 2;
   spacing := checkerr*2+2;

   { p1 bar (bottom half of bar) }
   for j := 0 to bar[p1]-1 do begin

      cy := brdy+brdh div 2+checkerr+4+j*spacing;
      issel := (selectedpoint = 24) and (turn = p1);
      drawchecker(barcx, cy, p1, issel and (j = bar[p1]-1))

   end;

   { p2 bar (top half of bar) }
   for j := 0 to bar[p2]-1 do begin

      cy := brdy+brdh div 2-checkerr-4-j*spacing;
      issel := (selectedpoint = 24) and (turn = p2);
      drawchecker(barcx, cy, p2, issel and (j = bar[p2]-1))

   end;

   { draw borne off checkers in tray }
   trayx := brdx+pointw*12+barw+bearw div 2;
   spacing := checkerr;
   if spacing < 4 then spacing := 4;

   { p1 borne off (bottom half) }
   for j := 0 to off[p1]-1 do begin

      cy := brdy+brdh-8-j*spacing;
      drawchecker(trayx, cy, p1, false)

   end;

   { p2 borne off (top half) }
   for j := 0 to off[p2]-1 do begin

      cy := brdy+8+j*spacing;
      drawchecker(trayx, cy, p2, false)

   end

end;

{ get the landing stack position for a checker arriving at dest.
  Accounts for existing checkers and potential hits. }

function landingpos(dest, player: integer): integer;

var cnt, r: integer;

begin

   r := 0;
   if (dest >= 0) and (dest < 24) then begin

      cnt := board[dest];
      if player = p1 then begin

         if cnt >= 0 then r := cnt { hit: replaces the blot }

      end else begin

         if cnt <= 0 then r := -cnt

      end

   end;
   landingpos := r

end;

{ draw a highlight circle at a destination point }

procedure drawdestcircle(dest, stackpos, cr, cg, cb, lw: integer);

var cx, cy, tx: integer;

begin

   if dest = -1 then begin

      { bear off tray }
      tx := brdx+pointw*12+barw;
      fcolorg(output, clr(cr), clr(cg), clr(cb));
      linewidth(output, lw);
      rect(output, tx+2, brdy+2, tx+bearw-2, brdy+brdh-2);
      linewidth(output, 1)

   end else begin

      cx := pointcx(dest);
      cy := checkercy(dest, stackpos);
      fcolorg(output, clr(cr), clr(cg), clr(cb));
      linewidth(output, lw);
      ellipse(output, cx-checkerr, cy-checkerr, cx+checkerr, cy+checkerr);
      linewidth(output, 1)

   end

end;

{ recursively find combo destinations reachable by using multiple dice.
  depth = number of dice used so far (1 = single, 2+ = combo).
  curpt = current simulated position.
  diceused = set of which dice slots are used in this path. }

procedure findcombodests(src, curpt, depth, maxdepth: integer; diceused: dset;
                         var singleseen: seenarr; var comboseen: comboarr);

var vals:                 dicearr;
    nv, i, k, k2, ib:     integer;
    usedcount, avail:     integer;
    middest, savecur:     integer;
    savemid, idx, sp:     integer;
    skip, found:          boolean;

begin

   getavaildice(vals, nv);

   for i := 0 to nv-1 do begin

      ib := i;
      skip := false;

      { for non-doubles, each die used once }
      if ndice < 4 then begin

         if i in diceused then skip := true

      end else begin

         { doubles: count how many we've used vs available }
         avail := 0;
         for k := 0 to ndice-1 do
            if not used[k] then avail := avail+1;
         { count members of diceused }
         usedcount := 0;
         for k := 0 to 3 do
            if k in diceused then usedcount := usedcount+1;
         if usedcount >= avail then skip := true;
         { for doubles, don't revisit same combo path }
         if not skip then
            if i in diceused then begin

               { find next unused slot }
               found := false;
               for k2 := i+1 to 3 do
                  if not found then
                     if not (k2 in diceused) then begin

                        ib := k2;
                        found := true

                     end;
               if not found then skip := true

            end

      end;

      if not skip then
         if not canmovefrom(curpt, vals[i], turn) then skip := true;

      if not skip then begin

         { simulate move }
         savecur := board[curpt];
         if turn = p1 then board[curpt] := board[curpt]-1
         else board[curpt] := board[curpt]+1;

         middest := movedest(curpt, vals[i], turn);

         if (middest >= 0) and (middest < 24) then begin

            { check if blocked }
            if ((turn = p1) and (board[middest] < -1)) or
               ((turn = p2) and (board[middest] > 1)) then
               board[curpt] := savecur
            else begin

               savemid := board[middest];
               { simulate landing }
               if ((turn = p1) and (board[middest] = -1)) or
                  ((turn = p2) and (board[middest] = 1)) then
                  board[middest] := 0; { hit }
               if turn = p1 then board[middest] := board[middest]+1
               else board[middest] := board[middest]-1;

               idx := middest+1;
               if depth >= 1 then
                  if not singleseen[idx] then
                     if not comboseen[depth, idx] then begin

                        comboseen[depth, idx] := true;
                        sp := landingpos(middest, turn);
                        drawdestcircle(middest, sp, 0, 220, 255, 5)

                     end;

               { recurse for deeper combos }
               if depth+1 < maxdepth then
                  findcombodests(src, middest, depth+1, maxdepth,
                                 diceused+[ib], singleseen, comboseen);

               board[middest] := savemid;
               board[curpt] := savecur

            end

         end else begin

            { bear off combo }
            if middest = -1 then
               if depth >= 1 then
                  if not comboseen[depth, 0] then begin

                     comboseen[depth, 0] := true;
                     drawdestcircle(-1, 0, 0, 220, 255, 5)

                  end;
            board[curpt] := savecur

         end

      end

   end

end;

procedure drawhighlightdests;

var dests:        destarr;
    nd, i, j, sp: integer;
    s, avail:     integer;
    frombar:      boolean;
    singleseen:   seenarr;
    comboseen:    comboarr;

begin

   if selectedpoint >= 0 then begin

      frombar := selectedpoint = 24;
      if frombar then s := -1
      else s := selectedpoint;
      nd := getvaliddests(s, frombar, dests);

      for i := 0 to 29 do singleseen[i] := false;
      for i := 0 to 3 do
         for j := 0 to 29 do comboseen[i, j] := false;

      { draw single-die destinations in yellow }
      for i := 0 to nd-1 do begin

         if dests[i].dest >= 0 then sp := landingpos(dests[i].dest, turn)
         else sp := 0;
         drawdestcircle(dests[i].dest, sp, 255, 255, 0, 5);
         if dests[i].dest >= -1 then singleseen[dests[i].dest+1] := true

      end;

      { draw combo destinations in cyan }
      if not frombar then begin

         { count available dice }
         avail := 0;
         for i := 0 to ndice-1 do
            if not used[i] then avail := avail+1;
         if avail >= 2 then
            findcombodests(selectedpoint, selectedpoint, 1, avail, [],
                           singleseen, comboseen)

      end

   end

end;

procedure drawdicedisplay;

var ddx, ddy, dsz, i, v, dotr:      integer;
    x1, y1, x2, y2, cx, cy:         integer;
    k, dx1, dx2, dx3, dy1, dy2, dy3: integer;
    remn, fsz:                      integer;
    isused:                         boolean;
    m:                              pstring;

begin

   if (gamestate <> gsroll) or (dice[0] <> 0) then begin

      dsz := brdh div 10;
      if dsz < 20 then dsz := 20;
      if dsz > 60 then dsz := 60;
      dotr := dsz div 8;
      if dotr < 2 then dotr := 2;

      { position dice in center of board }
      ddx := brdx+brdw div 2-dsz-dsz div 4;
      ddy := brdy+brdh div 2-dsz div 2;

      for i := 0 to 1 do begin

         v := dice[i];
         if v <> 0 then begin

            { check if any die with this value is still available }
            isused := true;
            for k := 0 to ndice-1 do
               if isused then
                  if not used[k] then
                     if dieval(k) = v then isused := false;

            x1 := ddx+i*(dsz+dsz div 2);
            y1 := ddy;
            x2 := x1+dsz;
            y2 := y1+dsz;
            cx := (x1+x2) div 2;
            cy := (y1+y2) div 2;

            { die background }
            if isused then fcolorg(output, clr(120), clr(120), clr(120))
            else fcolorg(output, clr(240), clr(230), clr(210));
            frect(output, x1, y1, x2, y2);

            { die border }
            fcolorg(output, clr(60), clr(50), clr(40));
            rect(output, x1, y1, x2, y2);

            { dots }
            if isused then fcolorg(output, clr(80), clr(80), clr(80))
            else fcolorg(output, clr(20), clr(20), clr(20));

            dx1 := x1+dsz div 4;
            dx2 := cx;
            dx3 := x2-dsz div 4;
            dy1 := y1+dsz div 4;
            dy2 := cy;
            dy3 := y2-dsz div 4;

            if (v = 1) or (v = 3) or (v = 5) then
               fellipse(output, dx2-dotr, dy2-dotr, dx2+dotr, dy2+dotr);
            if v >= 2 then begin

               fellipse(output, dx1-dotr, dy3-dotr, dx1+dotr, dy3+dotr);
               fellipse(output, dx3-dotr, dy1-dotr, dx3+dotr, dy1+dotr)

            end;
            if v >= 4 then begin

               fellipse(output, dx1-dotr, dy1-dotr, dx1+dotr, dy1+dotr);
               fellipse(output, dx3-dotr, dy3-dotr, dx3+dotr, dy3+dotr)

            end;
            if v = 6 then begin

               fellipse(output, dx1-dotr, dy2-dotr, dx1+dotr, dy2+dotr);
               fellipse(output, dx3-dotr, dy2-dotr, dx3+dotr, dy2+dotr)

            end

         end

      end;

      { show remaining dice count for doubles }
      if ndice = 4 then begin

         remn := diceremaining;
         fsz := dsz div 3;
         if fsz < 10 then fsz := 10;
         fontsiz(output, fsz);
         fcolorg(output, clr(255), clr(255), clr(200));
         openstring;
         m := cat(chstr('x'), ints(remn));
         m := cat(m, ' left');
         cursorg(output, ddx, ddy+dsz+4);
         write(m^);
         closestring

      end

   end

end;

procedure drawpointlabels;

var i, cx, fsz: integer;
    b:          pstring;

begin

   fsz := pointw div 3;
   if fsz < 8 then fsz := 8;
   fontsiz(output, fsz);
   fcolorg(output, clr(180), clr(170), clr(150));

   openstring;
   for i := 0 to 23 do begin

      cx := pointcx(i);
      b := ints(i+1);
      if i >= 12 then
         { top: label above }
         cursorg(output, cx-strsiz(output, b^) div 2, brdy-fsz-2)
      else
         { bottom: label below }
         cursorg(output, cx-strsiz(output, b^) div 2, brdy+brdh+4);
      write(b^)

   end;
   closestring

end;

function iscomputerturn: boolean;

var r: boolean;

begin

   r := false;
   if gamemode = modepvcw then if turn = p2 then r := true;
   if gamemode = modepvcb then if turn = p1 then r := true;
   iscomputerturn := r

end;

procedure drawstatus;

var fsz, sy, sy2, csz, ccx, ccy: integer;
    loser, mult, pts, k2:        integer;
    m, b:                        pstring;

begin

   fsz := scrh div 30;
   if fsz < 12 then fsz := 12;
   sy := brdy+brdh+fsz+8;

   fontsiz(output, fsz);
   openstring;

   { pip counts on sides }
   fcolorg(output, clr(240), clr(235), clr(220));
   m := cat('White: ', ints(pipcount(p1)));
   m := cat(m, ' pips');
   cursorg(output, brdx, sy);
   write(m^);

   fcolorg(output, clr(160), clr(150), clr(140));
   m := cat('Black: ', ints(pipcount(p2)));
   m := cat(m, ' pips');
   cursorg(output, brdx+brdw-strsiz(output, m^), sy);
   write(m^);

   { center status message }
   fcolorg(output, clr(255), clr(255), clr(200));

   if gamestate = gsgameover then begin

      { check for gammon/backgammon multiplier }
      loser := 1-turn;
      mult := 1;
      if off[loser] = 0 then begin

         mult := 2; { gammon }
         if bar[loser] > 0 then mult := 3 { backgammon }
         else
            { check if loser has checkers in winner's home }
            for k2 := 0 to 23 do begin

               if loser = p1 then
                  if board[k2] > 0 then
                     if k2 >= 18 then mult := 3;
               if loser = p2 then
                  if board[k2] < 0 then
                     if k2 <= 5 then mult := 3

            end

      end;
      pts := cubevalue*mult;
      score[turn] := score[turn]+pts;
      m := plname(turn);
      if mult = 3 then m := cat(m, ' wins BACKGAMMON! (+')
      else if mult = 2 then m := cat(m, ' wins GAMMON! (+')
      else m := cat(m, ' wins! (+');
      m := cat(m, ints(pts));
      m := cat(m, ' pts)')

   end else if gamestate = gsroll then begin

      m := plname(turn);
      if iscomputerturn then m := cat(m, ' (computer) rolling...')
      else m := cat(m, ' - Click to roll dice')

   end else begin

      m := plname(turn);
      if iscomputerturn then m := cat(m, ' (computer) thinking...')
      else if selectedpoint >= 0 then m := cat(m, ' - Click destination')
      else if bar[turn] > 0 then m := cat(m, ' - Click bar checker to enter')
      else m := cat(m, ' - Select a checker to move')

   end;
   cursorg(output, scrw div 2-strsiz(output, m^) div 2, sy);
   write(m^);

   { score and doubling cube }
   sy2 := sy+fsz+4;
   csz := fsz;

   { draw scores }
   fcolorg(output, clr(240), clr(235), clr(220));
   m := cat('Score: ', ints(score[p1]));
   cursorg(output, brdx, sy2);
   write(m^);

   fcolorg(output, clr(160), clr(150), clr(140));
   m := cat('Score: ', ints(score[p2]));
   cursorg(output, brdx+brdw-strsiz(output, m^), sy2);
   write(m^);

   { draw doubling cube }
   b := ints(cubevalue);
   ccx := scrw div 2;
   ccy := sy2;

   { cube background }
   fcolorg(output, clr(220), clr(220), clr(200));
   frect(output, ccx-csz, ccy-csz div 4, ccx+csz, ccy+csz);
   fcolorg(output, clr(60), clr(60), clr(60));
   rect(output, ccx-csz, ccy-csz div 4, ccx+csz, ccy+csz);

   { cube value }
   fcolorg(output, clr(30), clr(30), clr(30));
   fontsiz(output, csz*3 div 4);
   cursorg(output, ccx-strsiz(output, b^) div 2, ccy+csz div 8);
   write(b^);
   fontsiz(output, fsz); { restore }

   { owner indicator }
   if cubeowner = p1 then begin

      fcolorg(output, clr(240), clr(235), clr(220));
      b := chstr('W');
      cursorg(output, ccx-csz-strsiz(output, b^)-4, ccy);
      write(b^)

   end else if cubeowner = p2 then begin

      fcolorg(output, clr(100), clr(90), clr(80));
      b := chstr('B');
      cursorg(output, ccx+csz+4, ccy);
      write(b^)

   end;
   closestring

end;

procedure drawall;

var cx, cy:           integer;
    fsz, mw, mh, mx, my: integer;

begin

   calcmetrics;
   drawboardbg;
   drawcheckers;

   { draw the animated checker if animation is in progress }
   if animating then begin

      cx := animsx+(animdx-animsx)*animframe div animframes;
      cy := animsy+(animdy-animsy)*animframe div animframes;
      drawchecker(cx, cy, animplayer, true)

   end;

   drawhighlightdests;
   drawdicedisplay;
   drawpointlabels;
   drawstatus;

   { draw overlay message if active }
   if overlayactive then
      if overlaymsg <> nil then begin

         fsz := scrh div 20;
         if fsz < 14 then fsz := 14;
         fontsiz(output, fsz);
         mw := strsiz(output, overlaymsg^)+fsz*2;
         mh := fsz*3;
         mx := scrw div 2-mw div 2;
         my := scrh div 2-mh div 2;

         { dark background box }
         fcolorg(output, clr(20), clr(20), clr(40));
         frect(output, mx, my, mx+mw, my+mh);
         fcolorg(output, clr(200), clr(180), clr(80));
         rect(output, mx, my, mx+mw, my+mh);

         { text }
         fcolorg(output, clr(255), clr(255), clr(200));
         cursorg(output, mx+fsz, my+fsz);
         write(overlaymsg^)

      end

end;

{ ****************************************************************************

   Computer AI

   **************************************************************************** }

{ count how many opponent checkers can hit a blot at point pt }

function blotexposure(pt, opponent: integer): integer;

var hits, i, dist: integer;
    skip:          boolean;

begin

   hits := 0;
   for i := 0 to 23 do begin

      skip := false;
      if (opponent = p1) and (board[i] <= 0) then skip := true;
      if (opponent = p2) and (board[i] >= 0) then skip := true;
      if not skip then begin

         if opponent = p1 then dist := i-pt
         else dist := pt-i;
         if (dist > 0) and (dist <= 12) then hits := hits+1; { combo range }
         if (dist > 0) and (dist <= 6) then hits := hits+1   { direct shot }

      end

   end;
   { bar checkers can also hit }
   if bar[opponent] > 0 then begin

      if opponent = p1 then
         if pt >= 18 then if pt <= 23 then hits := hits+2;
      if opponent = p2 then
         if pt >= 0 then if pt <= 5 then hits := hits+2

   end;
   blotexposure := hits

end;

{ evaluate for a specific player (positive = good for that player) }

function evalplayer(player: integer): integer;

var i, sc, opp:           integer;
    homepts:              integer; { number of home board points held }
    primelen:             integer; { longest consecutive run of held points }
    curprime:             integer;
    anchorcount:          integer; { points held in opponent's home board }
    totalpip:             integer;
    blots:                integer;
    cnt, dist, exposure:  integer;

begin

   opp := 1-player;
   sc := 0;
   homepts := 0;
   primelen := 0;
   curprime := 0;
   anchorcount := 0;
   totalpip := 0;
   blots := 0;

   { borne off is the ultimate goal }
   sc := sc+off[player]*600;

   { bar is devastating }
   sc := sc-bar[player]*500;

   { opponent on bar is great for us }
   sc := sc+bar[opp]*300;

   for i := 0 to 23 do begin

      if player = p1 then begin

         if board[i] > 0 then cnt := board[i] else cnt := 0;
         dist := i { pip distance for p1 (lower = closer to home) }

      end else begin

         if board[i] < 0 then cnt := -board[i] else cnt := 0;
         dist := 23-i

      end;

      if cnt = 0 then curprime := 0
      else begin

         { pip count }
         totalpip := totalpip+cnt*(dist+1);

         { advancement: reward checkers closer to home }
         sc := sc+cnt*(24-dist)*3;

         if cnt >= 2 then begin

            { holding a point }
            sc := sc+40;
            curprime := curprime+1;
            if curprime > primelen then primelen := curprime;

            { home board points are very valuable }
            if dist < 6 then begin

               homepts := homepts+1;
               sc := sc+50;
               { inner home points (1-3) are especially valuable }
               if dist < 3 then sc := sc+25

            end;

            { anchors in opponent's home are strategic }
            if dist >= 18 then begin

               anchorcount := anchorcount+1;
               sc := sc+35

            end;

            { stacking more than 5 on one point is wasteful }
            if cnt > 5 then sc := sc-(cnt-5)*15;
            { 3 is ideal for blocking }
            if cnt = 3 then sc := sc+10

         end else begin

            { blot - penalize based on exposure to being hit }
            exposure := blotexposure(i, opp);
            sc := sc-(25+exposure*15);
            blots := blots+1;

            { blots in opponent's home board are extra dangerous }
            if dist >= 18 then sc := sc-40;
            { blots near our home are less risky }
            if dist < 6 then sc := sc+15

         end

      end

   end;

   { prime bonus: consecutive blocked points are very strong }
   if primelen >= 3 then sc := sc+primelen*40;
   if primelen >= 5 then sc := sc+150;  { 5-prime is very strong }
   if primelen >= 6 then sc := sc+300;  { 6-prime is nearly unpassable }

   { home board coverage bonus }
   if homepts >= 3 then sc := sc+60;
   if homepts >= 5 then sc := sc+120;
   if homepts >= 6 then sc := sc+200; { closed home board }

   { bearing off readiness }
   if allinhome(player) then begin

      sc := sc+250;
      { when bearing off, low pip count is key }
      sc := sc-totalpip*2

   end else
      { general pip count matters but less }
      sc := sc-totalpip;

   { avoid too many blots }
   if blots >= 3 then sc := sc-blots*30;

   evalplayer := sc

end;

{ board evaluation from p1's perspective (higher = better for p1) }

function evaluate: integer;

begin

   evaluate := evalplayer(p1)-evalplayer(p2)

end;

procedure saveai(var s: aistate);

begin

   s.board := board;
   s.bar := bar;
   s.off := off;
   s.used := used

end;

procedure restoreai(var s: aistate);

begin

   board := s.board;
   bar := s.bar;
   off := s.off;
   used := s.used

end;

{ recursively find best sequence of moves for current dice.
  Tries all orderings and picks the one with best evaluation. }

procedure aisearch(player, depth: integer);

label 1; { exit }

var vals:            dicearr;
    nv, i, j, sc:    integer;
    dest, dmy:       integer;
    anymove, washit: boolean;
    frombar, skip:   boolean;
    saved:           aistate;

begin

   aicombocount := aicombocount+1;
   if aicombocount > maxcombos then goto 1;

   getavaildice(vals, nv);
   if nv = 0 then begin

      sc := evaluate;
      if player = p1 then begin

         if sc > aibestscore then aibestscore := sc

      end else begin

         if sc < aibestscore then aibestscore := sc

      end;
      goto 1

   end;

   anymove := false;
   frombar := bar[player] > 0;

   if frombar then begin

      for i := 0 to nv-1 do
         if canenter(player, vals[i]) then begin

            if player = p1 then dest := 24-vals[i]
            else dest := vals[i]-1;
            washit := pointisblot(dest, player);

            saveai(saved);
            dmy := usedie(vals[i]);
            bar[player] := bar[player]-1;
            if washit then begin

               if player = p1 then begin

                  board[dest] := 0;
                  bar[p2] := bar[p2]+1

               end else begin

                  board[dest] := 0;
                  bar[p1] := bar[p1]+1

               end

            end;
            if player = p1 then board[dest] := board[dest]+1
            else board[dest] := board[dest]-1;

            aisearch(player, depth+1);
            anymove := true;

            restoreai(saved)

         end

   end else
      for j := 0 to 23 do begin

         skip := false;
         if (player = p1) and (board[j] <= 0) then skip := true;
         if (player = p2) and (board[j] >= 0) then skip := true;
         if not skip then
            for i := 0 to nv-1 do
               if aicombocount <= maxcombos then
                  if canmovefrom(j, vals[i], player) then begin

                     dest := movedest(j, vals[i], player);
                     washit := false;
                     if dest >= 0 then washit := pointisblot(dest, player);

                     saveai(saved);
                     dmy := usedie(vals[i]);
                     if player = p1 then board[j] := board[j]-1
                     else board[j] := board[j]+1;

                     if dest = -1 then off[player] := off[player]+1
                     else begin

                        if washit then begin

                           if player = p1 then begin

                              board[dest] := 0;
                              bar[p2] := bar[p2]+1

                           end else begin

                              board[dest] := 0;
                              bar[p1] := bar[p1]+1

                           end

                        end;
                        if player = p1 then board[dest] := board[dest]+1
                        else board[dest] := board[dest]-1

                     end;

                     aisearch(player, depth+1);
                     anymove := true;

                     restoreai(saved)

                  end

      end;

   if not anymove then begin

      { no moves possible with remaining dice }
      sc := evaluate;
      if player = p1 then begin

         if sc > aibestscore then aibestscore := sc

      end else begin

         if sc < aibestscore then aibestscore := sc

      end

   end;

   1: { exit }

end;

{ AI top level: try each first move, pick the one leading to best score }

procedure aimove;

label 1; { exit }

var vals:               dicearr;
    nv, i, j, dest:     integer;
    sc, savebest:       integer;
    res, dmy:           integer;
    bestsrc, bestdv:    integer;
    bestfrombar:        boolean;
    frombar, washit:    boolean;
    skip:               boolean;
    saved:              aistate;

begin

   soundenabled := false;

   getavaildice(vals, nv);
   if nv = 0 then begin

      soundenabled := true;
      endturn;
      goto 1

   end;

   frombar := bar[turn] > 0;

   bestsrc := -1;
   bestdv := 0;
   bestfrombar := false;
   if turn = p1 then aibestscore := -1000000
   else aibestscore := 1000000;

   if frombar then begin

      for i := 0 to nv-1 do
         if canenter(turn, vals[i]) then begin

            if turn = p1 then dest := 24-vals[i]
            else dest := vals[i]-1;
            washit := pointisblot(dest, turn);

            saveai(saved);
            dmy := usedie(vals[i]);
            bar[turn] := bar[turn]-1;
            if washit then begin

               if turn = p1 then begin

                  board[dest] := 0;
                  bar[p2] := bar[p2]+1

               end else begin

                  board[dest] := 0;
                  bar[p1] := bar[p1]+1

               end

            end;
            if turn = p1 then board[dest] := board[dest]+1
            else board[dest] := board[dest]-1;

            aicombocount := 0;
            savebest := aibestscore;
            if turn = p1 then aibestscore := -1000000
            else aibestscore := 1000000;
            aisearch(turn, 1);
            sc := aibestscore;
            aibestscore := savebest;

            if ((turn = p1) and (sc > aibestscore)) or
               ((turn = p2) and (sc < aibestscore)) then begin

               aibestscore := sc;
               bestsrc := -1;
               bestdv := vals[i];
               bestfrombar := true

            end;

            restoreai(saved)

         end

   end else
      for j := 0 to 23 do begin

         skip := false;
         if (turn = p1) and (board[j] <= 0) then skip := true;
         if (turn = p2) and (board[j] >= 0) then skip := true;
         if not skip then
            for i := 0 to nv-1 do
               if canmovefrom(j, vals[i], turn) then begin

                  dest := movedest(j, vals[i], turn);
                  washit := false;
                  if dest >= 0 then washit := pointisblot(dest, turn);

                  saveai(saved);
                  dmy := usedie(vals[i]);
                  if turn = p1 then board[j] := board[j]-1
                  else board[j] := board[j]+1;

                  if dest = -1 then off[turn] := off[turn]+1
                  else begin

                     if washit then begin

                        if turn = p1 then begin

                           board[dest] := 0;
                           bar[p2] := bar[p2]+1

                        end else begin

                           board[dest] := 0;
                           bar[p1] := bar[p1]+1

                        end

                     end;
                     if turn = p1 then board[dest] := board[dest]+1
                     else board[dest] := board[dest]-1

                  end;

                  aicombocount := 0;
                  savebest := aibestscore;
                  if turn = p1 then aibestscore := -1000000
                  else aibestscore := 1000000;
                  aisearch(turn, 1);
                  sc := aibestscore;
                  aibestscore := savebest;

                  if ((turn = p1) and (sc > aibestscore)) or
                     ((turn = p2) and (sc < aibestscore)) then begin

                     aibestscore := sc;
                     bestsrc := j;
                     bestdv := vals[i];
                     bestfrombar := false

                  end;

                  restoreai(saved)

               end

      end;

   soundenabled := true;

   if (bestdv = 0) or ((bestsrc < 0) and (not bestfrombar)) then begin

      { no legal move }
      endturn;
      goto 1

   end;

   { execute the chosen move }
   res := domove(bestsrc, bestdv, turn, bestfrombar);
   if res = 1 then playsound(hitnote, hitdur)
   else if res = 2 then playsound(bearoffnote, bearoffdur);

   { check if more moves remain }
   if off[turn] >= numcheckers then begin

      gamestate := gsgameover;
      playsound(winnote, windur);
      goto 1

   end;

   if (diceremaining > 0) and hasanymove(turn) then begin

      { schedule another AI move }
      timer(output, timerai, 300, false);
      aipending := true

   end else endturn;

   1: { exit }

end;

{ ****************************************************************************

   Try to execute a combo move (using two dice) from src to finaldest.
   Returns true if successful.

   **************************************************************************** }

function trycombomove(src, finaldest: integer; frombar: boolean): boolean;

var vals:                  dicearr;
    nv, i, j, k, avail:    integer;
    mid, middest, player:  integer;
    savesrc, savemid:      integer;
    savebar, res, dmy:     integer;
    r, skipi, skipj, can:  boolean;

begin

   r := false;
   player := turn;

   getavaildice(vals, nv);
   if nv >= 2 then
      for i := 0 to nv-1 do
         if not r then begin

            skipi := false;
            mid := 0;

            if frombar then begin

               if not canenter(player, vals[i]) then skipi := true
               else begin

                  if player = p1 then mid := 24-vals[i]
                  else mid := vals[i]-1

               end

            end else begin

               if not canmovefrom(src, vals[i], player) then skipi := true
               else mid := movedest(src, vals[i], player)

            end;
            if not skipi then
               if mid < 0 then skipi := true;

            { check intermediate is landable }
            if not skipi then
               if board[mid] <> 0 then
                  if ((player = p1) and (board[mid] < -1)) or
                     ((player = p2) and (board[mid] > 1)) then skipi := true;

            if not skipi then
               for j := 0 to nv-1 do
                  if not r then begin

                     skipj := false;
                     if (j = i) and (ndice < 4) then skipj := true;
                     if not skipj then
                        if (j = i) and (ndice >= 4) then begin

                           avail := 0;
                           for k := 0 to ndice-1 do
                              if not used[k] then avail := avail+1;
                           if avail < 2 then skipj := true

                        end;

                     if not skipj then begin

                        { simulate intermediate }
                        if frombar then savesrc := 0
                        else savesrc := board[src];
                        savemid := board[mid];
                        savebar := bar[player];
                        if frombar then bar[player] := bar[player]-1
                        else begin

                           if player = p1 then board[src] := board[src]-1
                           else board[src] := board[src]+1

                        end;
                        { handle hit at mid }
                        if ((player = p1) and (board[mid] = -1)) or
                           ((player = p2) and (board[mid] = 1)) then
                           board[mid] := 0;
                        if player = p1 then board[mid] := board[mid]+1
                        else board[mid] := board[mid]-1;

                        can := canmovefrom(mid, vals[j], player);
                        if can then middest := movedest(mid, vals[j], player)
                        else middest := -2;

                        { restore }
                        if frombar then bar[player] := savebar
                        else board[src] := savesrc;
                        board[mid] := savemid;

                        if middest = finaldest then begin

                           { found the combo - execute both moves }
                           if frombar then
                              dmy := domove(-1, vals[i], player, true)
                           else
                              dmy := domove(src, vals[i], player, false);
                           res := domove(mid, vals[j], player, false);
                           if res = 1 then playsound(hitnote, hitdur)
                           else if res = 2 then
                              playsound(bearoffnote, bearoffdur);
                           r := true

                        end

                     end

                  end

         end;
   trycombomove := r

end;

{ ****************************************************************************

   Handle player click

   **************************************************************************** }

{ common bookkeeping after a successful player move }

procedure finishmove;

begin

   selectedpoint := -1;
   if off[turn] >= numcheckers then begin

      gamestate := gsgameover;
      playsound(winnote, windur)

   end else begin

      if (diceremaining = 0) or (not hasanymove(turn)) then endturn

   end

end;

procedure handleclick;

label 1; { exit }

var pt, nd, i, res: integer;
    dests:          destarr;

begin

   if gamestate = gsgameover then goto 1;
   if iscomputerturn then goto 1;

   if gamestate = gsroll then begin

      { roll dice }
      rolldice;
      if soundenabled then playwave(wave_out, 0, wavedice);
      gamestate := gsmove;
      selectedpoint := -1;

      { check if any moves possible }
      if not hasanymove(turn) then begin

         { no legal moves, end turn after brief display }
         timer(output, timerai, 500, false);
         aipending := true

      end;
      goto 1

   end;

   { gsmove: selecting checker or destination }
   pt := mousetopoint(mousex, mousey);
   if pt < 0 then begin

      selectedpoint := -1;
      goto 1

   end;

   { if must enter from bar }
   if bar[turn] > 0 then begin

      if pt = 24 then begin

         { clicked bar - select it }
         selectedpoint := 24;
         goto 1

      end;
      if selectedpoint = 24 then
         if (pt >= 0) and (pt <= 23) then begin

            { try to enter at clicked point }
            nd := getvaliddests(-1, true, dests);
            for i := 0 to nd-1 do
               if dests[i].dest = pt then begin

                  res := domove(-1, dests[i].dv, turn, true);
                  if res = 1 then playsound(hitnote, hitdur);
                  finishmove;
                  goto 1

               end;
            { try combo from bar }
            if trycombomove(-1, pt, true) then begin

               finishmove;
               goto 1

            end

         end;
      { auto-select bar if turn player has bar checkers }
      selectedpoint := 24;
      goto 1

   end;

   { normal move (not from bar) }
   if (selectedpoint >= 0) and (selectedpoint <= 23) then begin

      { a piece is selected, check if clicking a valid destination }
      nd := getvaliddests(selectedpoint, false, dests);

      { check if clicked the bear off tray }
      if pt = 25 then
         for i := 0 to nd-1 do
            if dests[i].dest = -1 then begin

               res := domove(selectedpoint, dests[i].dv, turn, false);
               playsound(bearoffnote, bearoffdur);
               finishmove;
               goto 1

            end;

      { check if clicked a valid board destination }
      if (pt >= 0) and (pt <= 23) then
         for i := 0 to nd-1 do
            if dests[i].dest = pt then begin

               res := domove(selectedpoint, dests[i].dv, turn, false);
               if res = 1 then playsound(hitnote, hitdur)
               else if res = 2 then playsound(bearoffnote, bearoffdur);
               finishmove;
               goto 1

            end;

      { not a single-die dest - try combo move }
      if (pt >= 0) and (pt <= 23) then
         if trycombomove(selectedpoint, pt, false) then begin

            finishmove;
            goto 1

         end;
      { try combo bear off }
      if pt = 25 then
         if trycombomove(selectedpoint, -1, false) then begin

            finishmove;
            goto 1

         end;

      { not a valid dest - try to select a different piece }
      selectedpoint := -1

   end;

   { try to select a checker }
   if (pt >= 0) and (pt <= 23) then begin

      if (turn = p1) and (board[pt] > 0) then begin

         { check if this piece has any valid move }
         nd := getvaliddests(pt, false, dests);
         if nd > 0 then selectedpoint := pt
         else selectedpoint := -1

      end else if (turn = p2) and (board[pt] < 0) then begin

         nd := getvaliddests(pt, false, dests);
         if nd > 0 then selectedpoint := pt
         else selectedpoint := -1

      end else selectedpoint := -1

   end;

   1: { exit }

end;

{ ****************************************************************************

   Menu setup

   **************************************************************************** }

{ create menu entry }

procedure newmenu(var mp: menuptr; onoff, oneof, isbar: boolean;
                  id: integer; view face: string);

begin

   new(mp);
   mp^.next := nil;
   mp^.branch := nil;
   mp^.onoff := onoff;
   mp^.oneof := oneof;
   mp^.bar := isbar;
   mp^.id := id;
   mp^.face := copy(face)

end;

{ append a new menu entry to the given list }

procedure appendmenu(var list: menuptr; item: menuptr);

var p: menuptr;

begin

   if list = nil then list := item
   else begin

      p := list;
      while p^.next <> nil do p := p^.next;
      p^.next := item

   end

end;

procedure setupmenu;

var menulist:             menuptr;
    gamemenu, gameitems:  menuptr;
    modemenu, modeitems:  menuptr;
    helpmenu, helpitems:  menuptr;
    mp:                   menuptr;

begin

   menulist := nil;

   { game menu }
   newmenu(gamemenu, false, false, false, 0, 'Game');
   appendmenu(menulist, gamemenu);
   gameitems := nil;
   newmenu(mp, false, false, false, menunew, 'New Game');
   appendmenu(gameitems, mp);
   newmenu(mp, false, false, false, menuundo, 'Undo Move');
   appendmenu(gameitems, mp);
   newmenu(mp, false, false, true, menudouble, 'Double');
   appendmenu(gameitems, mp);
   newmenu(mp, false, false, false, menuexit, 'Exit');
   appendmenu(gameitems, mp);
   gamemenu^.branch := gameitems;

   { mode menu }
   newmenu(modemenu, false, false, false, 0, 'Mode');
   appendmenu(menulist, modemenu);
   modeitems := nil;
   newmenu(mp, false, true, false, menupvp, 'Player vs Player');
   appendmenu(modeitems, mp);
   newmenu(mp, false, true, false, menupvcw, 'Play White vs Computer');
   appendmenu(modeitems, mp);
   newmenu(mp, false, true, false, menupvcb, 'Play Black vs Computer');
   appendmenu(modeitems, mp);
   modemenu^.branch := modeitems;

   { help menu }
   newmenu(helpmenu, false, false, false, 0, 'Help');
   appendmenu(menulist, helpmenu);
   helpitems := nil;
   newmenu(mp, false, false, false, menuabout, 'About Backgammon');
   appendmenu(helpitems, mp);
   helpmenu^.branch := helpitems;

   menu(output, menulist)

end;

{ ****************************************************************************

   Menu actions

   **************************************************************************** }

{ player offers to double - only before rolling }

procedure handledouble;

var opp, mypip, theirpip: integer;
    aiaccepts:            boolean;
    m:                    pstring;

begin

   if gamestate <> gsroll then begin

      alert('Double', 'You can only double before rolling.');
      drawall

   end else if iscomputerturn then begin

      { not your turn }

   end else if (cubeowner <> -1) and (cubeowner <> turn) then begin

      alert('Double',
            'Only the player who was last doubled can redouble.');
      drawall

   end else begin

      opp := 1-turn;
      openstring;
      if gamemode = modepvp then begin

         { PvP: show offer to opponent }
         m := plname(turn);
         m := cat(m, ' doubles to ');
         m := cat(m, ints(cubevalue*2));
         m := catc(m, '!');
         m := catc(m, chr(10));
         m := cat(m, 'Does ');
         m := cat(m, plname(opp));
         m := cat(m, ' accept?');
         alert('Double', m^);
         { in PvP we auto-accept for now (no decline UI) }
         cubevalue := cubevalue*2;
         cubeowner := opp;
         drawall

      end else begin

         { vs computer: AI decides. AI uses simple heuristic: accept if
           pip count difference is not too large }
         mypip := pipcount(opp);
         theirpip := pipcount(turn);
         aiaccepts := (cubevalue < 16) and (mypip <= theirpip*3 div 2);
         if aiaccepts then begin

            cubevalue := cubevalue*2;
            cubeowner := opp;
            m := copy('Computer accepts the double.');
            m := catc(m, chr(10));
            m := cat(m, 'Cube is now at ');
            m := cat(m, ints(cubevalue));
            m := catc(m, '.');
            alert('Double', m^);
            drawall

         end else begin

            m := copy('Computer declines the double.');
            m := catc(m, chr(10));
            m := cat(m, plname(turn));
            m := cat(m, ' wins ');
            m := cat(m, ints(cubevalue));
            if cubevalue > 1 then m := cat(m, ' points!')
            else m := cat(m, ' point!');
            score[turn] := score[turn]+cubevalue;
            alert('Double', m^);
            initboard;
            drawall

         end

      end;
      closestring

   end

end;

{ show the about dialog }

procedure showabout;

var m: pstring;

begin

   openstring;
   m := copy('Backgammon for Amitk');
   m := catc(m, chr(10));
   m := cat(m, 'Human vs human or vs computer');
   m := catc(m, chr(10));
   m := cat(m, 'Click to roll, then select moves');
   m := catc(m, chr(10));
   m := cat(m, 'Copyright (C) 2026 S. A. Franco');
   alert('About Backgammon', m^);
   closestring;
   drawall

end;

{ ****************************************************************************

   Main program

   **************************************************************************** }

begin

   randstate := clock mod i32max; { seed random generator from system clock }

   mousex := 0;
   mousey := 0;
   animating := false;
   aipending := false;
   overlayactive := false;
   overlaymsg := nil;
   soundenabled := false;
   undotop := 0;

   title(output, 'Backgammon');
   curvis(output, false);
   auto(output, false);
   autohold(false); { override automatic hold }
   buffer(output, 0);
   font(output, ftsign);
   bold(output, true);
   binvis(output);

   { restore saved window size/position if available }
   if exists('backgammon.pos') then begin

      assign(pf, 'backgammon.pos');
      reset(pf);
      okw := false;
      okh := false;
      pw := 0;
      ph := 0;
      if not eof(pf) then begin

         readln(pf); { skip header line }
         if not eof(pf) then begin

            readln(pf); { skip blank line }
            if not eof(pf) then begin

               rdint(pf, pw, okw);
               rdint(pf, ph, okh)

            end

         end

      end;
      if okw and okh then
         if (pw > 100) and (ph > 100) then setsizg(output, pw, ph);
      close(pf)

   end;

   opensynthout(synth_out);
   instchange(synth_out, 0, 1, inst_woodblock);
   starttimeout;
   { the sound files accompany the program (getpgm), else the current dir }
   astf := maknam(getpgm, 'slide', 'wav');
   if not exists(astf^) then astf := copy('slide.wav');
   loadwave(waveslide, astf^);
   astf := maknam(getpgm, 'dice', 'wav');
   if not exists(astf^) then astf := copy('dice.wav');
   loadwave(wavedice, astf^);
   openwaveout(wave_out);
   soundenabled := true;

   gamemode := modepvcw;
   score[p1] := 0;
   score[p2] := 0;
   setupmenu;
   initboard;
   calcmetrics;
   drawall;

   repeat

      event(input, er);

      case er.etype of

         etredraw: drawall;

         etresize: drawall;

         etmoumovg: begin

            mousex := er.moupxg;
            mousey := er.moupyg

         end;

         etmouba:
            if er.amoubn = 1 then
               if not animating then begin

                  handleclick;
                  drawall;

                  { if it became computer's turn after roll state }
                  if gamestate = gsroll then
                     if iscomputerturn then
                        if not aipending then begin

                           timer(output, timerai, 300, false);
                           aipending := true

                        end

               end;

         ettim:
            if er.timnum = timeranim then begin

               if animating then begin

                  animframe := animframe+1;
                  if animframe >= animframes then finishanimation;
                  drawall

               end

            end else if er.timnum = timermsg then begin

               overlayactive := false;
               drawall

            end else if er.timnum = timerai then begin

               aipending := false;

               if (gamestate = gsmove) and (not hasanymove(turn)) then begin

                  { no moves available after roll - just end turn }
                  endturn;
                  drawall;
                  if gamestate = gsroll then
                     if iscomputerturn then begin

                        timer(output, timerai, 300, false);
                        aipending := true

                     end

               end else if (gamestate = gsroll) and iscomputerturn then begin

                  { computer rolls }
                  rolldice;
                  if soundenabled then playwave(wave_out, 0, wavedice);
                  gamestate := gsmove;
                  drawall;

                  if hasanymove(turn) then begin

                     timer(output, timerai, 400, false);
                     aipending := true

                  end else begin

                     endturn;
                     drawall;
                     if gamestate = gsroll then
                        if iscomputerturn then begin

                           timer(output, timerai, 300, false);
                           aipending := true

                        end

                  end

               end else if (gamestate = gsmove) and iscomputerturn then begin

                  { computer makes a move }
                  aimove;
                  drawall;

                  { aimove schedules another timer if more moves remain,
                    or calls endturn. If turn ended, check if still
                    computer's turn. }
                  if gamestate = gsroll then
                     if iscomputerturn then
                        if not aipending then begin

                           timer(output, timerai, 500, false);
                           aipending := true

                        end

               end

            end;

         etmenus:
            case er.menuid of

               menunew: begin

                  initboard;
                  drawall;
                  if iscomputerturn then begin

                     timer(output, timerai, 300, false);
                     aipending := true

                  end

               end;

               menuundo:
                  if undotop > 0 then
                     if gamestate = gsmove then
                        if not iscomputerturn then
                           if not animating then begin

                              bdummy := undopop;
                              gamestate := gsmove;
                              drawall

                           end;

               menudouble: handledouble;

               menuexit: goto 99;

               menupvp: begin

                  gamemode := modepvp;
                  initboard;
                  drawall

               end;

               menupvcw: begin

                  gamemode := modepvcw;
                  initboard;
                  drawall;
                  if iscomputerturn then begin

                     timer(output, timerai, 300, false);
                     aipending := true

                  end

               end;

               menupvcb: begin

                  gamemode := modepvcb;
                  initboard;
                  drawall;
                  if iscomputerturn then begin

                     timer(output, timerai, 300, false);
                     aipending := true

                  end

               end;

               menuabout: showabout

            else

            end

      else

      end

   until er.etype = etterm;

   99: ; { terminate }

   { save window position/size }
   getsizg(output, wx, wy);
   assign(pf, 'backgammon.pos');
   rewrite(pf);
   writeln(pf, 'Backgammon position');
   writeln(pf);
   writeln(pf, 'width: ', wx:1, ' height: ', wy:1,
               ' position x: 0 position y: 0');
   close(pf);

   closewaveout(wave_out);
   closesynthout(synth_out)

end.
