{******************************************************************************
*                                                                             *
*                               CHECKERS GAME                                 *
*                                                                             *
*                       COPYRIGHT (C) 2026 S. A. FRANCO                       *
*                                                                             *
* A graphical checkers game with resizable window and menus. Implements full  *
* checkers rules including mandatory jumps, multi-jumps, and king promotion.  *
* Supports human vs human and human vs computer play.                         *
* Computer uses minimax search with alpha-beta pruning.                       *
*                                                                             *
* Pascaline port of the Ami checkers.c reference game.                        *
*                                                                             *
******************************************************************************}

program checkers(input, output);

uses graphics,
     sound,
     strings;

label 99; { terminate }

const

   i32max = 2147483647; { 32 bit INT_MAX, base for RGB ratios/velocity }

   { sound defines }
   move_note    = 53;  { note_e+octave_5 }
   capture_note = 56;  { note_g+octave_5 }
   king_note    = 61;  { note_c+octave_6 }
   win_note     = 37;  { note_c+octave_4 }
   move_dur     = 150;
   king_dur     = 300;
   win_dur      = 800;

   { piece and board definitions }
   empty = 0;
   man   = 1;
   king  = 2;

   red_side   = 0;
   black_side = 1;

   { menu ids }
   menu_new   = 100;
   menu_exit  = 101;
   menu_about = 102;
   menu_pvp   = 103;
   menu_pvc_r = 104;
   menu_pvc_b = 105;

   { game mode }
   mode_pvp   = 0; { player vs player }
   mode_pvc_r = 1; { player is red, computer is black }
   mode_pvc_b = 2; { player is black, computer is red }

   { AI search depth }
   ai_depth = 6;

   { AI timer id }
   timer_ai = 2;

   maxmoves = 128;

   { font code (from amitk/include/graphics.h: AMI_FONT_SIGN) }
   font_sign = 3;

type

   checkmove = record

      fr, fc, tr, tc: integer; { from and to square }
      isjump:         boolean  { move is a jump }

   end;
   movearr  = array [1..maxmoves] of checkmove;
   boardarr = array [0..7, 0..7] of integer;

   { save/restore board state for search }
   boardstate = record

      board:     boardarr; { board contents }
      turn:      integer;  { whose turn }
      gamestate: integer   { 0=playing, 1=game over }

   end;

var

   board:      boardarr; { the playing board }
   turn:       integer;  { red_side or black_side }
   selected:   boolean;  { a piece is currently selected }
   selr, selc: integer;  { selected piece row, col }
   gamestate:  integer;  { 0=playing, 1=game over }
   gamemode:   integer;  { mode_pvp, mode_pvc_r, mode_pvc_b }
   aipending:  boolean;  { AI move is pending (timer set) }
   mustjump:   boolean;  { jumps are available and mandatory }
   multijump:  boolean;  { in the middle of a multi-jump sequence }

   legalmoves: movearr;  { legal moves for selected piece }
   nmoves:     integer;  { number of legal moves }

   mousex, mousey: integer; { mouse tracking position }
   soundenabled:   boolean; { sounds are enabled }

   er:             evtrec;  { event record }
   mr, mc:         integer; { clicked square }
   found:          boolean; { clicked square is a move target }
   cancont:        boolean; { piece can continue jumping }
   i:              integer; { general index }
   aboutmsg:       packed array [1..80] of char; { about dialog message }

fixed

   { move direction tables, row and column deltas }
   drtab: array [1..4] of integer = array 1, 1, -1, -1 end;
   dctab: array [1..4] of integer = array 1, -1, 1, -1 end;

   { piece-square table for positional evaluation (center and advancement
     bonus), flattened 8x8, index r*8+c }
   manpst: array [0..63] of integer = array
      0,  0,  0,  0,  0,  0,  0,  0,
      1,  0,  1,  0,  1,  0,  1,  0,
      0,  2,  0,  2,  0,  2,  0,  2,
      3,  0,  4,  0,  4,  0,  3,  0,
      0,  4,  0,  5,  0,  5,  0,  4,
      5,  0,  6,  0,  6,  0,  5,  0,
      0,  6,  0,  7,  0,  7,  0,  6,
      8,  0,  8,  0,  8,  0,  8,  0
   end;

   kingpst: array [0..63] of integer = array
      0,  0,  0,  0,  0,  0,  0,  0,
      0,  2,  0,  2,  0,  2,  0,  2,
      0,  2,  0,  3,  0,  3,  0,  2,
      0,  2,  0,  4,  0,  4,  0,  2,
      0,  2,  0,  4,  0,  4,  0,  2,
      0,  2,  0,  3,  0,  3,  0,  2,
      0,  2,  0,  2,  0,  2,  0,  2,
      0,  0,  0,  0,  0,  0,  0,  0
   end;

{******************************************************************************

Piece encoding helpers

C macros MAKEPIECE/PTYPE/PCOLOR: piece = color shl 2 or type.

******************************************************************************}

function makepiece(clr, typ: integer): integer;

begin

   makepiece := clr*4+typ

end;

function ptype(p: integer): integer;

begin

   ptype := p mod 4

end;

function pcolor(p: integer): integer;

begin

   pcolor := (p div 4) mod 2

end;

{ scale a 0..255 RGB component to the 0..INT_MAX color range }

function crgb(v: integer): integer;

begin

   crgb := v*(i32max div 255)

end;

{******************************************************************************

Board initialization

******************************************************************************}

procedure init_board;

var r, c: integer;

begin

   for r := 0 to 7 do
      for c := 0 to 7 do board[r, c] := empty;

   { Red at bottom (rows 0-2), black at top (rows 5-7) }
   { Pieces only on dark squares: where (r+c) is even }
   for r := 0 to 2 do
      for c := 0 to 7 do
         if (r+c) mod 2 = 0 then board[r, c] := makepiece(red_side, man);

   for r := 5 to 7 do
      for c := 0 to 7 do
         if (r+c) mod 2 = 0 then board[r, c] := makepiece(black_side, man);

   turn := red_side;
   selected := false;
   gamestate := 0;
   aipending := false;
   mustjump := false;
   multijump := false;
   nmoves := 0

end;

{******************************************************************************

Board coordinate helpers

******************************************************************************}

function sqsize: integer;

var bw, bh, sz: integer;

begin

   bw := maxxg(output);
   bh := maxyg(output)-maxyg(output) div 12;
   if bw < bh then sz := bw else sz := bh;
   sqsize := sz div 8

end;

function boardx0: integer;

begin

   boardx0 := (maxxg(output)-sqsize*8) div 2

end;

function boardy0: integer;

begin

   boardy0 := (maxyg(output)-maxyg(output) div 12-sqsize*8) div 2

end;

procedure sq2px(r, c: integer; var px, py: integer);

var sz: integer;

begin

   sz := sqsize;
   px := boardx0+c*sz;
   py := boardy0+(7-r)*sz

end;

function px2sq(px, py: integer; var r, c: integer): boolean;

var sz, bx, by: integer;

begin

   sz := sqsize;
   bx := boardx0;
   by := boardy0;

   px2sq := false;
   if (px >= bx) and (py >= by) then begin

      c := (px-bx) div sz;
      r := 7-(py-by) div sz;
      if (c >= 0) and (c <= 7) and (r >= 0) and (r <= 7) then px2sq := true

   end

end;

function onboard(r, c: integer): boolean;

begin

   onboard := (r >= 0) and (r < 8) and (c >= 0) and (c < 8)

end;

{******************************************************************************

Move generation

******************************************************************************}

{ Check if a piece at (r,c) can jump in direction (dr,dc) }

function can_jump(r, c, dr, dc: integer): boolean;

var mr, mc, lr, lc, p: integer;

begin

   mr := r+dr; mc := c+dc;     { middle square }
   lr := r+2*dr; lc := c+2*dc; { landing square }

   can_jump := false;
   if onboard(lr, lc) then
      if board[lr, lc] = empty then begin

      p := board[mr, mc];
      if p <> empty then
         if pcolor(p) <> pcolor(board[r, c]) then can_jump := true

   end

end;

{ Check if a man at (r,c) of given color may move in direction index i.
  Men can only move forward; kings move any direction. }

function dirok(typ, clr, i: integer): boolean;

var ok: boolean;

begin

   ok := true;
   if typ = man then begin

      if clr = red_side then if drtab[i] < 0 then ok := false;
      if clr = black_side then if drtab[i] > 0 then ok := false

   end;
   dirok := ok

end;

{ Check if any piece of given color has a jump available }

function any_jump_available(clr: integer): boolean;

var r, c, p, typ, i: integer;
    jump: boolean;

begin

   jump := false;
   r := 0;
   while (r < 8) and not jump do begin

      c := 0;
      while (c < 8) and not jump do begin

         p := board[r, c];
         if p <> empty then if pcolor(p) = clr then begin

            typ := ptype(p);
            i := 1;
            while (i <= 4) and not jump do begin

               if dirok(typ, clr, i) then
                  if can_jump(r, c, drtab[i], dctab[i]) then jump := true;
               i := i+1

            end

         end;
         c := c+1

      end;
      r := r+1

   end;
   any_jump_available := jump

end;

{ Check if a specific piece at (r,c) has a jump available }

function piece_has_jump(r, c: integer): boolean;

var p, typ, clr, i: integer;
    jump: boolean;

begin

   jump := false;
   p := board[r, c];
   if p <> empty then begin

      clr := pcolor(p);
      typ := ptype(p);
      i := 1;
      while (i <= 4) and not jump do begin

         if dirok(typ, clr, i) then
            if can_jump(r, c, drtab[i], dctab[i]) then jump := true;
         i := i+1

      end

   end;
   piece_has_jump := jump

end;

{ Generate moves for a specific piece at (r,c).
  If jumpsonly is set, only generate jump moves. }

procedure gen_piece_moves(r, c: integer; jumpsonly: boolean);

var p, clr, typ, i, nr, nc: integer;

begin

   nmoves := 0;
   p := board[r, c];
   if p <> empty then begin

      clr := pcolor(p);
      typ := ptype(p);

      { Generate jump moves }
      for i := 1 to 4 do
         if dirok(typ, clr, i) then
            if can_jump(r, c, drtab[i], dctab[i]) then begin

         nr := r+2*drtab[i];
         nc := c+2*dctab[i];
         if nmoves < maxmoves then begin

            nmoves := nmoves+1;
            legalmoves[nmoves].fr := r;
            legalmoves[nmoves].fc := c;
            legalmoves[nmoves].tr := nr;
            legalmoves[nmoves].tc := nc;
            legalmoves[nmoves].isjump := true

         end

      end;

      { if jumps exist, simple moves not allowed }
      if not jumpsonly then if nmoves = 0 then

         { Generate simple (non-jump) moves }
         for i := 1 to 4 do
            if dirok(typ, clr, i) then begin

            nr := r+drtab[i];
            nc := c+dctab[i];
            if onboard(nr, nc) then
               if board[nr, nc] = empty then
                  if nmoves < maxmoves then begin

               nmoves := nmoves+1;
               legalmoves[nmoves].fr := r;
               legalmoves[nmoves].fc := c;
               legalmoves[nmoves].tr := nr;
               legalmoves[nmoves].tc := nc;
               legalmoves[nmoves].isjump := false

            end

         end

   end

end;

{ Generate all legal moves for a given color }

function gen_all_moves(clr: integer; var moves: movearr): integer;

var r, c, count, savenmoves, i: integer;
    jumpsexist: boolean;

begin

   jumpsexist := any_jump_available(clr);
   savenmoves := nmoves;
   count := 0;

   for r := 0 to 7 do
      for c := 0 to 7 do
         if board[r, c] <> empty then
            if pcolor(board[r, c]) = clr then begin

         gen_piece_moves(r, c, jumpsexist);
         for i := 1 to nmoves do
            if count < maxmoves then begin

            count := count+1;
            moves[count] := legalmoves[i]

         end

      end;

   nmoves := savenmoves;
   gen_all_moves := count

end;

function has_legal_moves(clr: integer): boolean;

var moves: movearr;

begin

   has_legal_moves := gen_all_moves(clr, moves) > 0

end;

{ Count pieces of a given color }

function count_pieces(clr: integer): integer;

var r, c, count: integer;

begin

   count := 0;
   for r := 0 to 7 do
      for c := 0 to 7 do
         if board[r, c] <> empty then
            if pcolor(board[r, c]) = clr then count := count+1;
   count_pieces := count

end;

{******************************************************************************

Execute a move on the board

******************************************************************************}

procedure play_sound(n, dur: integer);

begin

   noteon(synth_out, 0, 1, n, i32max);
   noteoff(synth_out, curtimeout+dur, 1, n, i32max)

end;

{ Execute a move. Returns true if the piece can continue jumping. }

function do_move(fr, fc, tr, tc: integer): boolean;

var p, clr, mr, mc: integer;
    isjump, promoted, cancontinue: boolean;

begin

   p := board[fr, fc];
   clr := pcolor(p);
   isjump := abs(tr-fr) = 2;
   promoted := false;
   cancontinue := false;

   if isjump then begin

      { remove jumped piece }
      mr := (fr+tr) div 2;
      mc := (fc+tc) div 2;
      board[mr, mc] := empty

   end;

   board[tr, tc] := p;
   board[fr, fc] := empty;

   { check for promotion }
   if ptype(board[tr, tc]) = man then begin

      if clr = red_side then if tr = 7 then begin

         board[tr, tc] := makepiece(red_side, king);
         promoted := true

      end;
      if clr = black_side then if tr = 0 then begin

         board[tr, tc] := makepiece(black_side, king);
         promoted := true

      end

   end;

   { Check if multi-jump is possible (only if this was a jump and
     the piece was not just promoted) }
   if isjump then if not promoted then
      if piece_has_jump(tr, tc) then cancontinue := true;

   if not cancontinue then begin

      { Turn is over }
      turn := 1-turn;

      { Check for game over }
      if not has_legal_moves(turn) or (count_pieces(turn) = 0) then
         gamestate := 1

   end;

   { Play sounds }
   if soundenabled then begin

      if gamestate = 1 then play_sound(win_note, win_dur)
      else if promoted then play_sound(king_note, king_dur)
      else if isjump then play_sound(capture_note, move_dur)
      else play_sound(move_note, move_dur)

   end;

   do_move := cancontinue

end;

{******************************************************************************

Computer AI - minimax with alpha-beta pruning

******************************************************************************}

function evaluate: integer;

var r, c, score, p, typ, clr: integer;

begin

   score := 0;
   for r := 0 to 7 do
      for c := 0 to 7 do begin

      p := board[r, c];
      if p <> empty then begin

         typ := ptype(p);
         clr := pcolor(p);

         if typ = man then begin

            if clr = red_side then score := score+100+manpst[r*8+c]
            else score := score-(100+manpst[(7-r)*8+c])

         end else begin { king }

            if clr = red_side then score := score+160+kingpst[r*8+c]
            else score := score-(160+kingpst[r*8+c])

         end

      end

   end;
   evaluate := score

end;

procedure save_state(var s: boardstate);

begin

   s.board := board;
   s.turn := turn;
   s.gamestate := gamestate

end;

procedure restore_state(var s: boardstate);

begin

   board := s.board;
   turn := s.turn;
   gamestate := s.gamestate

end;

{ Recursively execute a move, handling multi-jumps for AI.
  After the move, if a multi-jump is possible, pick the
  continuation greedily (first available jump). }

procedure do_move_full(fr, fc, tr, tc: integer);

var cancontinue: boolean;
    savenmoves: integer;

begin

   cancontinue := do_move(fr, fc, tr, tc);

   { If multi-jump, do a greedy continuation }
   while cancontinue do begin

      savenmoves := nmoves;
      gen_piece_moves(tr, tc, true); { jumps only }
      if nmoves = 0 then begin

         nmoves := savenmoves;
         { end the turn }
         turn := 1-turn;
         if not has_legal_moves(turn) or (count_pieces(turn) = 0) then
            gamestate := 1;
         cancontinue := false

      end else begin

         { pick first available jump }
         fr := legalmoves[1].fr;
         fc := legalmoves[1].fc;
         tr := legalmoves[1].tr;
         tc := legalmoves[1].tc;
         nmoves := savenmoves;

         cancontinue := do_move(fr, fc, tr, tc)

      end

   end

end;

function minimax(depth, alpha, beta: integer; maximizing: boolean): integer;

var moves: movearr;
    nmov, i, val, score: integer;
    saved: boardstate;
    clr: integer;

begin

   if (depth = 0) or (gamestate <> 0) then minimax := evaluate
   else begin

      if maximizing then clr := red_side else clr := black_side;
      nmov := gen_all_moves(clr, moves);

      if nmov = 0 then begin

         { no moves = loss }
         if maximizing then minimax := -100000+(ai_depth-depth)
         else minimax := 100000-(ai_depth-depth)

      end else begin

         if maximizing then begin

            val := -200000;
            i := 1;
            while (i <= nmov) and (beta > alpha) do begin

               save_state(saved);
               do_move_full(moves[i].fr, moves[i].fc,
                            moves[i].tr, moves[i].tc);
               score := minimax(depth-1, alpha, beta, false);
               restore_state(saved);
               if score > val then val := score;
               if val > alpha then alpha := val;
               i := i+1

            end

         end else begin

            val := 200000;
            i := 1;
            while (i <= nmov) and (beta > alpha) do begin

               save_state(saved);
               do_move_full(moves[i].fr, moves[i].fc,
                            moves[i].tr, moves[i].tc);
               score := minimax(depth-1, alpha, beta, true);
               restore_state(saved);
               if score < val then val := score;
               if val < beta then beta := val;
               i := i+1

            end

         end;
         minimax := val

      end

   end

end;

procedure ai_move;

var moves: movearr;
    nmov, i, bestidx, bestval, val: integer;
    saved: boardstate;
    aicolor: integer;
    maximizing: boolean;

begin

   aicolor := turn;
   maximizing := aicolor = red_side;

   nmov := gen_all_moves(aicolor, moves);
   if nmov > 0 then begin

      soundenabled := false; { silence during search }
      bestidx := 1;

      if maximizing then begin

         bestval := -200000;
         for i := 1 to nmov do begin

            save_state(saved);
            do_move_full(moves[i].fr, moves[i].fc,
                         moves[i].tr, moves[i].tc);
            val := minimax(ai_depth-1, -200000, 200000, false);
            restore_state(saved);
            if val > bestval then begin bestval := val; bestidx := i end

         end

      end else begin

         bestval := 200000;
         for i := 1 to nmov do begin

            save_state(saved);
            do_move_full(moves[i].fr, moves[i].fc,
                         moves[i].tr, moves[i].tc);
            val := minimax(ai_depth-1, -200000, 200000, true);
            restore_state(saved);
            if val < bestval then begin bestval := val; bestidx := i end

         end

      end;
      soundenabled := true; { re-enable for the actual move }

      { Execute the chosen move (with multi-jump handling) }
      do_move_full(moves[bestidx].fr, moves[bestidx].fc,
                   moves[bestidx].tr, moves[bestidx].tc);
      multijump := false

   end

end;

{ Check if it is the computer's turn }

function is_computer_turn: boolean;

var r: boolean;

begin

   r := false;
   if gamemode = mode_pvc_r then if turn = black_side then r := true;
   if gamemode = mode_pvc_b then if turn = red_side then r := true;
   is_computer_turn := r

end;

{******************************************************************************

Drawing

******************************************************************************}

{ check square (r,c) is a move target of the current selection }

function targmove(r, c: integer): boolean;

var i: integer;
    ismove: boolean;

begin

   ismove := false;
   i := 1;
   while (i <= nmoves) and not ismove do begin

      if (legalmoves[i].tr = r) and (legalmoves[i].tc = c) then ismove := true
      else i := i+1

   end;
   targmove := ismove

end;

procedure draw_piece(r, c, px, py, sz: integer);

var p, clr, typ: integer;
    cx, cy, rx, ry, margin: integer;
    krx, kry: integer;

begin

   p := board[r, c];
   clr := pcolor(p);
   typ := ptype(p);

   margin := sz div 8;
   cx := px+sz div 2;
   cy := py+sz div 2;
   rx := sz div 2-margin;
   ry := sz div 2-margin;

   { Draw dark outline circle (slightly larger) }
   fcolorg(output, crgb(20), crgb(20), crgb(20));
   fellipse(output, cx-rx, cy-ry, cx+rx, cy+ry);

   { Draw piece body (slightly smaller for outline effect) }
   rx := rx-sz div 16;
   ry := ry-sz div 16;
   if rx < 1 then rx := 1;
   if ry < 1 then ry := 1;

   if clr = red_side then fcolorg(output, crgb(200), crgb(30), crgb(30))
   else fcolorg(output, crgb(40), crgb(40), crgb(40));
   fellipse(output, cx-rx, cy-ry, cx+rx, cy+ry);

   { King marking: smaller inner circle in gold }
   if typ = king then begin

      krx := rx div 2;
      kry := ry div 2;

      if krx < 1 then krx := 1;
      if kry < 1 then kry := 1;
      fcolorg(output, crgb(255), crgb(215), crgb(0));
      fellipse(output, cx-krx, cy-kry, cx+krx, cy+kry)

   end

end;

procedure draw_square(r, c: integer);

var px, py, sz: integer;
    isdark, ismove: boolean;

begin

   sz := sqsize;
   sq2px(r, c, px, py);

   isdark := (r+c) mod 2 = 0;

   if selected and (r = selr) and (c = selc) then
      fcolorg(output, crgb(100), crgb(180), crgb(100)) { selected square }
   else begin

      ismove := false;
      if selected then ismove := targmove(r, c);
      if ismove then
         fcolorg(output, crgb(200), crgb(200), crgb(80)) { move target }
      else if isdark then
         fcolorg(output, crgb(181), crgb(136), crgb(99)) { dark square }
      else
         fcolorg(output, crgb(240), crgb(217), crgb(181)) { light square }

   end;
   frect(output, px, py, px+sz-1, py+sz-1);

   if board[r, c] <> empty then draw_piece(r, c, px, py, sz)

end;

procedure draw_board;

var r, c, sz, bx, by: integer;
    lbl: packed array [1..1] of char;

begin

   fcolor(output, white);
   frect(output, 1, 1, maxxg(output), maxyg(output));

   for r := 0 to 7 do
      for c := 0 to 7 do draw_square(r, c);

   { rank and file labels }
   sz := sqsize;
   bx := boardx0;
   by := boardy0;

   fontsiz(output, sz div 4);
   fcolorg(output, crgb(80), crgb(80), crgb(80));

   for c := 0 to 7 do begin

      lbl[1] := chr(ord('a')+c);
      cursorg(output, bx+c*sz+sz div 2-strsiz(output, lbl) div 2,
              by+8*sz+2);
      write(lbl[1])

   end;
   for r := 0 to 7 do begin

      lbl[1] := chr(ord('1')+r);
      cursorg(output, bx-sz div 3,
              by+(7-r)*sz+sz div 2-sz div 8);
      write(lbl[1])

   end

end;

{ print status message centered at given y }

procedure statmsg(sy: integer; view s: string);

begin

   cursorg(output, maxxg(output) div 2-strsiz(output, s) div 2, sy+4);
   write(s)

end;

procedure draw_status;

var sy, fsz: integer;

begin

   fsz := maxyg(output) div 25;
   if fsz < 14 then fsz := 14;
   sy := maxyg(output)-fsz*2;

   fcolor(output, white);
   frect(output, 1, sy, maxxg(output), maxyg(output));

   fontsiz(output, fsz);
   fcolorg(output, crgb(40), crgb(40), crgb(40));

   if gamestate = 1 then begin

      if turn = red_side then statmsg(sy, 'Game over! Black wins.')
      else statmsg(sy, 'Game over! Red wins.')

   end else if is_computer_turn then
      statmsg(sy, 'Computer is thinking...')
   else if multijump then begin

      if turn = red_side then statmsg(sy, 'Red must continue jumping!')
      else statmsg(sy, 'Black must continue jumping!')

   end else if mustjump then begin

      if turn = red_side then statmsg(sy, 'Red to move - jump required!')
      else statmsg(sy, 'Black to move - jump required!')

   end else begin

      if turn = red_side then statmsg(sy, 'Red to move.')
      else statmsg(sy, 'Black to move.')

   end

end;

procedure draw_all;

begin

   draw_board;
   draw_status

end;

{******************************************************************************

Menu setup

******************************************************************************}

function newmenuitem(onoff, oneof, bar: boolean; id: integer;
                     view face: string): menuptr;

var mp: menuptr;

begin

   new(mp);
   mp^.next := nil;
   mp^.branch := nil;
   mp^.onoff := onoff;
   mp^.oneof := oneof;
   mp^.bar := bar;
   mp^.id := id;
   mp^.face := copy(face);
   newmenuitem := mp

end;

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

procedure setup_menu;

var mlist:     menuptr;
    gamemenu:  menuptr;
    gameitems: menuptr;
    modemenu:  menuptr;
    modeitems: menuptr;
    helpmenu:  menuptr;
    helpitems: menuptr;

begin

   mlist := nil;

   { Game menu }
   gamemenu := newmenuitem(false, false, false, 0, 'Game');
   appendmenu(mlist, gamemenu);
   gameitems := nil;
   appendmenu(gameitems,
      newmenuitem(false, false, true, menu_new, 'New Game'));
   appendmenu(gameitems,
      newmenuitem(false, false, false, menu_exit, 'Exit'));
   gamemenu^.branch := gameitems;

   { Mode menu }
   modemenu := newmenuitem(false, false, false, 0, 'Mode');
   appendmenu(mlist, modemenu);
   modeitems := nil;
   appendmenu(modeitems,
      newmenuitem(false, true, false, menu_pvp, 'Player vs Player'));
   appendmenu(modeitems,
      newmenuitem(false, true, false, menu_pvc_r, 'Play Red vs Computer'));
   appendmenu(modeitems,
      newmenuitem(false, true, false, menu_pvc_b, 'Play Black vs Computer'));
   modemenu^.branch := modeitems;

   { Help menu }
   helpmenu := newmenuitem(false, false, false, 0, 'Help');
   appendmenu(mlist, helpmenu);
   helpitems := nil;
   appendmenu(helpitems,
      newmenuitem(false, false, false, menu_about, 'About Checkers'));
   helpmenu^.branch := helpitems;

   menu(output, mlist)

end;

{******************************************************************************

Main program

******************************************************************************}

begin

   title(output, 'Checkers');
   curvis(output, false);
   auto(output, false);
   buffer(output, 0);
   font(output, font_sign);
   bold(output, true);
   binvis(output);

   opensynthout(synth_out);
   instchange(synth_out, 0, 1, inst_acoustic_grand);
   starttimeout;
   soundenabled := true;

   { construct about dialog message with embedded newlines }
   aboutmsg := 'Checkers for Amitk Human vs human or vs computer Copyright (C) 2026 S. A. Franco';
   aboutmsg[19] := chr(10);
   aboutmsg[49] := chr(10);

   mousex := 0;
   mousey := 0;

   gamemode := mode_pvc_r;
   setup_menu;
   init_board;
   draw_all;

   { if computer goes first, schedule AI move }
   if is_computer_turn then begin

      timer(output, timer_ai, 100, false);
      aipending := true;
      draw_all

   end;

   repeat

      event(input, er);

      if (er.etype = etredraw) or (er.etype = etresize) then draw_all

      else if er.etype = etmoumovg then begin

         mousex := er.moupxg;
         mousey := er.moupyg

      end

      else if er.etype = etmouba then begin

         if er.amoubn = 1 then if gamestate = 0 then
            if not is_computer_turn then
               if px2sq(mousex, mousey, mr, mc) then begin

            { During multi-jump, only the jumping piece can be used }
            if multijump then begin

               if selected then begin

                  found := targmove(mr, mc);
                  if found then begin

                     cancont := do_move(selr, selc, mr, mc);
                     if cancont then begin

                        { continue multi-jump with same piece }
                        selr := mr; selc := mc;
                        gen_piece_moves(mr, mc, true);
                        multijump := true

                     end else begin

                        selected := false;
                        multijump := false;
                        nmoves := 0

                     end;
                     mustjump := any_jump_available(turn);
                     draw_all;
                     { schedule AI if turn changed }
                     if not multijump then if gamestate = 0 then
                        if is_computer_turn then begin

                        timer(output, timer_ai, 100, false);
                        aipending := true;
                        draw_all

                     end

                  end
                  { clicking elsewhere during multi-jump: ignore }

               end

            end else if selected then begin

               found := targmove(mr, mc);
               if found then begin

                  cancont := do_move(selr, selc, mr, mc);
                  if cancont then begin

                     { start multi-jump }
                     selected := true;
                     selr := mr; selc := mc;
                     gen_piece_moves(mr, mc, true);
                     multijump := true

                  end else begin

                     selected := false;
                     multijump := false;
                     nmoves := 0

                  end;
                  mustjump := any_jump_available(turn);
                  draw_all;
                  { schedule AI response }
                  if not multijump then if gamestate = 0 then
                     if is_computer_turn then begin

                     timer(output, timer_ai, 100, false);
                     aipending := true;
                     draw_all

                  end

               end else begin

                  { deselect and try to select new piece }
                  selected := false;
                  nmoves := 0;
                  if board[mr, mc] <> empty then
                     if pcolor(board[mr, mc]) = turn then begin

                     mustjump := any_jump_available(turn);
                     { if jumps are required, only allow selecting
                       pieces that can jump }
                     if mustjump and not piece_has_jump(mr, mc) then begin

                        { can't select this piece }

                     end else begin

                        gen_piece_moves(mr, mc, mustjump);
                        if nmoves > 0 then begin

                           selected := true;
                           selr := mr; selc := mc

                        end

                     end

                  end;
                  draw_all

               end

            end else begin

               { nothing selected yet }
               if board[mr, mc] <> empty then
                  if pcolor(board[mr, mc]) = turn then begin

                  mustjump := any_jump_available(turn);
                  if mustjump and not piece_has_jump(mr, mc) then begin

                     { can't select this piece - must jump }

                  end else begin

                     gen_piece_moves(mr, mc, mustjump);
                     if nmoves > 0 then begin

                        selected := true;
                        selr := mr; selc := mc;
                        draw_all

                     end

                  end

               end

            end

         end

      end

      else if er.etype = ettim then begin

         if er.timnum = timer_ai then
            if gamestate = 0 then if is_computer_turn then begin

            ai_move;
            aipending := false;
            draw_all

         end

      end

      else if er.etype = etmenus then begin

         if er.menuid = menu_new then begin

            init_board;
            selected := false; nmoves := 0;
            multijump := false;
            mustjump := false;
            draw_all;
            if is_computer_turn then begin

               timer(output, timer_ai, 100, false);
               aipending := true;
               draw_all

            end

         end else if er.menuid = menu_exit then goto 99

         else if er.menuid = menu_pvp then begin

            gamemode := mode_pvp;
            init_board;
            draw_all

         end else if er.menuid = menu_pvc_r then begin

            gamemode := mode_pvc_r;
            init_board;
            draw_all

         end else if er.menuid = menu_pvc_b then begin

            gamemode := mode_pvc_b;
            init_board;
            draw_all;
            if is_computer_turn then begin

               timer(output, timer_ai, 100, false);
               aipending := true;
               draw_all

            end

         end else if er.menuid = menu_about then begin

            alert('About Checkers', aboutmsg);
            draw_all

         end

      end

   until er.etype = etterm;

   99: { terminate }

   closesynthout(synth_out)

end.
