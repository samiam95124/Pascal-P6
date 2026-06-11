{******************************************************************************
*                                                                             *
*                                 CHESS GAME                                  *
*                                                                             *
*                       COPYRIGHT (C) 2026 S. A. FRANCO                       *
*                                                                             *
* A graphical chess game with resizable window and menus. Implements full     *
* chess rules including castling, en passant, and pawn promotion.             *
* Supports human vs human and human vs computer play.                         *
* Computer uses minimax search with alpha-beta pruning.                       *
*                                                                             *
* This is the Pascaline port of the Ami chess.c reference game.               *
*                                                                             *
******************************************************************************}

program chess(input, output);

uses graphics,
     sound,
     strings;

label 99; { terminate }

const

   i32max = 2147483647; { 32 bit INT_MAX, base for RGB ratios }

   { sound defines }
   move_note    = note_e+octave_5;
   capture_note = note_g+octave_5;
   check_note   = note_c+octave_6;
   mate_note    = note_c+octave_4;
   move_dur     = 150;
   check_dur    = 300;
   mate_dur     = 800;

   { piece and board definitions }
   empty   = 0;
   pawn    = 1;
   knight  = 2;
   bishop  = 3;
   rook    = 4;
   queen   = 5;
   king    = 6;

   white_side = 0;
   black_side = 1;

   { precomposed pieces (makepiece(color, type) = color*8+type) }
   wp = 1;  wn = 2;  wb = 3;  wr = 4;  wq = 5;  wk = 6;
   bp = 9;  bn = 10; bb = 11; br = 12; bq = 13; bk = 14;

   { menu ids }
   menu_new   = 100;
   menu_exit  = 101;
   menu_about = 102;
   menu_pvp   = 103;
   menu_pvc_w = 104;
   menu_pvc_b = 105;

   { game mode }
   mode_pvp   = 0; { player vs player }
   mode_pvc_w = 1; { player is white, computer is black }
   mode_pvc_b = 2; { player is black, computer is white }

   ai_depth = 4;   { AI search depth }
   timer_ai = 2;   { AI timer id }

   maxmoves = 256;

   { font code (from amitk/include/graphics.h: AMI_FONT_SIGN) }
   ftsign = 3;

   { board colors }
   light_r = 240*(i32max div 255);
   light_g = 217*(i32max div 255);
   light_b = 181*(i32max div 255);
   dark_r  = 181*(i32max div 255);
   dark_g  = 136*(i32max div 255);
   dark_b  = 99*(i32max div 255);
   sel_r   = 130*(i32max div 255);
   sel_g   = 151*(i32max div 255);
   sel_b   = 105*(i32max div 255);
   movc_r  = 170*(i32max div 255);
   movc_g  = 162*(i32max div 255);
   movc_b  = 58*(i32max div 255);

type

   chessmove = record fr, fc, tr, tc: integer end;
   movearr   = array [1..maxmoves] of chessmove;
   boardarr  = array [0..7, 0..7] of integer;

   { save/restore board state for search }
   boardstate = record

      bd:                 boardarr;
      trn, epc:           integer;
      wkc, wqc, bkc, bqc: boolean;
      gst:                integer

   end;

fixed

   { knight move/attack offsets }
   kdir: array [1..8, 1..2] of integer = array
      array -2, -1 end, array -2, 1 end, array -1, -2 end, array -1, 2 end,
      array 1, -2 end, array 1, 2 end, array 2, -1 end, array 2, 1 end
   end;

   { diagonal directions }
   ddir: array [1..4, 1..2] of integer = array
      array -1, -1 end, array -1, 1 end, array 1, -1 end, array 1, 1 end
   end;

   { straight directions }
   sdir: array [1..4, 1..2] of integer = array
      array -1, 0 end, array 1, 0 end, array 0, -1 end, array 0, 1 end
   end;

   { queen (all) directions }
   qdir: array [1..8, 1..2] of integer = array
      array -1, -1 end, array -1, 0 end, array -1, 1 end, array 0, -1 end,
      array 0, 1 end, array 1, -1 end, array 1, 0 end, array 1, 1 end
   end;

   { piece values for evaluation }
   pieceval: array [0..6] of integer =
      array 0, 100, 320, 330, 500, 900, 20000 end;

   { piece-square tables for positional evaluation (from white's perspective) }
   pawnpst: array [0..7, 0..7] of integer = array
      array  0,  0,  0,  0,  0,  0,  0,  0 end,
      array  5, 10, 10,-20,-20, 10, 10,  5 end,
      array  5, -5,-10,  0,  0,-10, -5,  5 end,
      array  0,  0,  0, 20, 20,  0,  0,  0 end,
      array  5,  5, 10, 25, 25, 10,  5,  5 end,
      array 10, 10, 20, 30, 30, 20, 10, 10 end,
      array 50, 50, 50, 50, 50, 50, 50, 50 end,
      array  0,  0,  0,  0,  0,  0,  0,  0 end
   end;

   knightpst: array [0..7, 0..7] of integer = array
      array -50,-40,-30,-30,-30,-30,-40,-50 end,
      array -40,-20,  0,  5,  5,  0,-20,-40 end,
      array -30,  5, 10, 15, 15, 10,  5,-30 end,
      array -30,  0, 15, 20, 20, 15,  0,-30 end,
      array -30,  5, 15, 20, 20, 15,  5,-30 end,
      array -30,  0, 10, 15, 15, 10,  0,-30 end,
      array -40,-20,  0,  0,  0,  0,-20,-40 end,
      array -50,-40,-30,-30,-30,-30,-40,-50 end
   end;

var

   { global state }
   board:      boardarr;
   turn:       integer;
   selected:   boolean;
   selr, selc: integer;
   gamestate:  integer; { 0=playing, 1=checkmate, 2=stalemate }
   gamemode:   integer; { mode_pvp, mode_pvc_w, mode_pvc_b }
   aipending:  boolean; { AI move is pending (timer set) }

   wkcastle, wqcastle: boolean;
   bkcastle, bqcastle: boolean;
   epcol:              integer;

   legalmoves: movearr;
   nmoves:     integer;

   mousex, mousey: integer;

   sndenable: boolean; { only play sounds for real moves, not AI search }

   er: evtrec; { event record }

{******************************************************************************

Piece composition helpers

******************************************************************************}

function makepiece(col, typ: integer): integer;

begin

   makepiece := col*8+typ

end;

function ptyp(p: integer): integer;

begin

   ptyp := p mod 8

end;

function pcol(p: integer): integer;

begin

   pcol := (p div 8) mod 2

end;

{ picture ids: white pieces light 1-6, white dark 7-12,
  black light 13-18, black dark 19-24 }

function picid(pc, pt: integer; light: boolean): integer;

var id: integer;

begin

   id := pc*12+pt;
   if not light then id := id+6;
   picid := id

end;

{******************************************************************************

Board initialization

******************************************************************************}

procedure init_board;

var r, c: integer;

begin

   for r := 0 to 7 do for c := 0 to 7 do board[r, c] := empty;

   board[0, 0] := wr; board[0, 1] := wn; board[0, 2] := wb; board[0, 3] := wq;
   board[0, 4] := wk; board[0, 5] := wb; board[0, 6] := wn; board[0, 7] := wr;
   for c := 0 to 7 do board[1, c] := wp;

   board[7, 0] := br; board[7, 1] := bn; board[7, 2] := bb; board[7, 3] := bq;
   board[7, 4] := bk; board[7, 5] := bb; board[7, 6] := bn; board[7, 7] := br;
   for c := 0 to 7 do board[6, c] := bp;

   turn := white_side;
   selected := false;
   gamestate := 0;
   aipending := false;
   wkcastle := true; wqcastle := true;
   bkcastle := true; bqcastle := true;
   epcol := -1;
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
    res:        boolean;

begin

   sz := sqsize;
   bx := boardx0;
   by := boardy0;

   if (px < bx) or (py < by) then res := false
   else begin

      c := (px-bx) div sz;
      r := 7-(py-by) div sz;
      res := true;
      if (c < 0) or (c > 7) or (r < 0) or (r > 7) then res := false

   end;
   px2sq := res

end;

{******************************************************************************

Move generation

******************************************************************************}

function onboard(r, c: integer): boolean;

begin

   onboard := (r >= 0) and (r < 8) and (c >= 0) and (c < 8)

end;

procedure findking(col: integer; var kr, kc: integer);

var r, c, kng: integer;

begin

   kng := makepiece(col, king);
   kr := -1; kc := -1;
   for r := 0 to 7 do
      for c := 0 to 7 do
         if board[r, c] = kng then begin kr := r; kc := c end

end;

{ scan a slider ray for an attacker of either given piece type }

function slide_attack(r, c, attacker, dr, dc, t1, t2: integer): boolean;

var j, nr, nc, p: integer;
    stop, atk:    boolean;

begin

   j := 1; stop := false; atk := false;
   while (j < 8) and not stop do begin

      nr := r+dr*j; nc := c+dc*j;
      if not onboard(nr, nc) then stop := true
      else begin

         p := board[nr, nc];
         if p <> empty then begin

            if pcol(p) = attacker then
               if (ptyp(p) = t1) or (ptyp(p) = t2) then atk := true;
            stop := true

         end

      end;
      j := j+1

   end;
   slide_attack := atk

end;

function is_attacked(r, c, attacker: integer): boolean;

var dr, dc, i, nr, nc: integer;
    atk:               boolean;

begin

   atk := false;

   { pawn attacks }
   if attacker = white_side then begin

      if onboard(r-1, c-1) then if board[r-1, c-1] = wp then atk := true;
      if onboard(r-1, c+1) then if board[r-1, c+1] = wp then atk := true

   end else begin

      if onboard(r+1, c-1) then if board[r+1, c-1] = bp then atk := true;
      if onboard(r+1, c+1) then if board[r+1, c+1] = bp then atk := true

   end;

   { knight attacks }
   if not atk then
      for i := 1 to 8 do begin

         nr := r+kdir[i, 1]; nc := c+kdir[i, 2];
         if onboard(nr, nc) then
            if board[nr, nc] = makepiece(attacker, knight) then atk := true

      end;

   { king attacks }
   if not atk then
      for dr := -1 to 1 do
         for dc := -1 to 1 do
            if (dr <> 0) or (dc <> 0) then begin

               nr := r+dr; nc := c+dc;
               if onboard(nr, nc) then
                  if board[nr, nc] = makepiece(attacker, king) then atk := true

            end;

   { diagonal sliders (bishop/queen) }
   if not atk then
      for i := 1 to 4 do
         if slide_attack(r, c, attacker, ddir[i, 1], ddir[i, 2],
                         bishop, queen) then atk := true;

   { straight sliders (rook/queen) }
   if not atk then
      for i := 1 to 4 do
         if slide_attack(r, c, attacker, sdir[i, 1], sdir[i, 2],
                         rook, queen) then atk := true;

   is_attacked := atk

end;

function in_check(col: integer): boolean;

var kr, kc: integer;

begin

   findking(col, kr, kc);
   if kr < 0 then in_check := false
   else in_check := is_attacked(kr, kc, 1-col)

end;

function move_legal(fr, fc, tr, tc: integer): boolean;

var captured: integer;

begin

   captured := board[tr, tc];
   board[tr, tc] := board[fr, fc];
   board[fr, fc] := empty;
   move_legal := not in_check(pcol(board[tr, tc]));
   board[fr, fc] := board[tr, tc];
   board[tr, tc] := captured

end;

procedure addmove(fr, fc, tr, tc: integer);

begin

   if nmoves < maxmoves then
      if move_legal(fr, fc, tr, tc) then begin

         nmoves := nmoves+1;
         legalmoves[nmoves].fr := fr; legalmoves[nmoves].fc := fc;
         legalmoves[nmoves].tr := tr; legalmoves[nmoves].tc := tc

      end

end;

{ generate slider moves along one ray }

procedure gen_slide(r, c, col, dr, dc: integer);

var j, nr, nc: integer;
    stop:      boolean;

begin

   j := 1; stop := false;
   while (j < 8) and not stop do begin

      nr := r+dr*j; nc := c+dc*j;
      if not onboard(nr, nc) then stop := true
      else if board[nr, nc] = empty then addmove(r, c, nr, nc)
      else begin

         if pcol(board[nr, nc]) <> col then addmove(r, c, nr, nc);
         stop := true

      end;
      j := j+1

   end

end;

procedure gen_piece_moves(r, c: integer);

var p, col, typ, nr, nc, i, dr, dc: integer;
    dir, startr, eprow:             integer;

begin

   nmoves := 0;
   p := board[r, c];
   if p <> empty then begin

      col := pcol(p);
      typ := ptyp(p);

      case typ of

         pawn: begin

            if col = white_side then begin dir := 1; startr := 1 end
            else begin dir := -1; startr := 6 end;

            nr := r+dir;
            if onboard(nr, c) then
               if board[nr, c] = empty then begin

                  addmove(r, c, nr, c);
                  if r = startr then
                     if board[r+2*dir, c] = empty then
                        addmove(r, c, r+2*dir, c)

               end;
            dc := -1;
            while dc <= 1 do begin

               nc := c+dc;
               if onboard(nr, nc) then begin

                  if board[nr, nc] <> empty then
                     if pcol(board[nr, nc]) <> col then addmove(r, c, nr, nc);
                  if col = white_side then eprow := 5 else eprow := 2;
                  if nr = eprow then
                     if epcol = nc then
                        if board[nr, nc] = empty then addmove(r, c, nr, nc)

               end;
               dc := dc+2

            end

         end;

         knight:
            for i := 1 to 8 do begin

               nr := r+kdir[i, 1]; nc := c+kdir[i, 2];
               if onboard(nr, nc) then
                  if (board[nr, nc] = empty) or
                     (pcol(board[nr, nc]) <> col) then addmove(r, c, nr, nc)

            end;

         bishop:
            for i := 1 to 4 do gen_slide(r, c, col, ddir[i, 1], ddir[i, 2]);

         rook:
            for i := 1 to 4 do gen_slide(r, c, col, sdir[i, 1], sdir[i, 2]);

         queen:
            for i := 1 to 8 do gen_slide(r, c, col, qdir[i, 1], qdir[i, 2]);

         king: begin

            for dr := -1 to 1 do
               for dc := -1 to 1 do
                  if (dr <> 0) or (dc <> 0) then begin

                     nr := r+dr; nc := c+dc;
                     if onboard(nr, nc) then
                        if (board[nr, nc] = empty) or
                           (pcol(board[nr, nc]) <> col) then
                           addmove(r, c, nr, nc)

                  end;
            if (col = white_side) and (r = 0) and (c = 4) then
               if not in_check(col) then begin

                  if wkcastle then
                     if (board[0, 5] = empty) and (board[0, 6] = empty) and
                        (board[0, 7] = wr) then
                        if not is_attacked(0, 5, black_side) then
                           if not is_attacked(0, 6, black_side) then
                              addmove(0, 4, 0, 6);
                  if wqcastle then
                     if (board[0, 3] = empty) and (board[0, 2] = empty) and
                        (board[0, 1] = empty) and (board[0, 0] = wr) then
                        if not is_attacked(0, 3, black_side) then
                           if not is_attacked(0, 2, black_side) then
                              addmove(0, 4, 0, 2)

               end;
            if (col = black_side) and (r = 7) and (c = 4) then
               if not in_check(col) then begin

                  if bkcastle then
                     if (board[7, 5] = empty) and (board[7, 6] = empty) and
                        (board[7, 7] = br) then
                        if not is_attacked(7, 5, white_side) then
                           if not is_attacked(7, 6, white_side) then
                              addmove(7, 4, 7, 6);
                  if bqcastle then
                     if (board[7, 3] = empty) and (board[7, 2] = empty) and
                        (board[7, 1] = empty) and (board[7, 0] = br) then
                        if not is_attacked(7, 3, white_side) then
                           if not is_attacked(7, 2, white_side) then
                              addmove(7, 4, 7, 2)

               end

         end

      end

   end

end;

{ generate all legal moves for a given color into the provided array }

function gen_all_moves(col: integer; var moves: movearr): integer;

var r, c, cnt, savenmoves, i: integer;

begin

   savenmoves := nmoves;
   cnt := 0;
   for r := 0 to 7 do
      for c := 0 to 7 do
         if board[r, c] <> empty then
            if pcol(board[r, c]) = col then begin

               gen_piece_moves(r, c);
               for i := 1 to nmoves do
                  if cnt < maxmoves then begin

                     cnt := cnt+1;
                     moves[cnt] := legalmoves[i]

                  end

            end;
   nmoves := savenmoves;
   gen_all_moves := cnt

end;

function has_legal_moves(col: integer): boolean;

var r, c, savenmoves: integer;
    found:            boolean;

begin

   savenmoves := nmoves;
   found := false;
   r := 0;
   while (r < 8) and not found do begin

      c := 0;
      while (c < 8) and not found do begin

         if board[r, c] <> empty then
            if pcol(board[r, c]) = col then begin

               gen_piece_moves(r, c);
               if nmoves > 0 then found := true

            end;
         c := c+1

      end;
      r := r+1

   end;
   nmoves := savenmoves;
   has_legal_moves := found

end;

{******************************************************************************

Execute a move on the board

******************************************************************************}

procedure play_sound(n, dur: integer);

begin

   noteon(synth_out, 0, 1, n, i32max);
   noteoff(synth_out, curtimeout+dur, 1, n, i32max)

end;

procedure do_move(fr, fc, tr, tc: integer);

var p, col, typ: integer;
    iscapture:   boolean;

begin

   p := board[fr, fc];
   col := pcol(p);
   typ := ptyp(p);
   iscapture := board[tr, tc] <> empty;

   { en passant is also a capture; remove the passed pawn }
   if typ = pawn then
      if tc <> fc then
         if board[tr, tc] = empty then begin

            iscapture := true;
            board[fr, tc] := empty

         end;

   if typ = king then begin

      if (fc = 4) and (tc = 6) then begin

         board[fr, 5] := board[fr, 7]; board[fr, 7] := empty

      end;
      if (fc = 4) and (tc = 2) then begin

         board[fr, 3] := board[fr, 0]; board[fr, 0] := empty

      end

   end;

   if typ = pawn then begin

      if (tr-fr = 2) or (fr-tr = 2) then epcol := fc
      else epcol := -1

   end else epcol := -1;

   if typ = king then begin

      if col = white_side then begin wkcastle := false; wqcastle := false end
      else begin bkcastle := false; bqcastle := false end

   end;
   if typ = rook then begin

      if col = white_side then begin

         if (fr = 0) and (fc = 0) then wqcastle := false;
         if (fr = 0) and (fc = 7) then wkcastle := false

      end else begin

         if (fr = 7) and (fc = 0) then bqcastle := false;
         if (fr = 7) and (fc = 7) then bkcastle := false

      end

   end;
   if (tr = 0) and (tc = 0) then wqcastle := false;
   if (tr = 0) and (tc = 7) then wkcastle := false;
   if (tr = 7) and (tc = 0) then bqcastle := false;
   if (tr = 7) and (tc = 7) then bkcastle := false;

   board[tr, tc] := p;
   board[fr, fc] := empty;

   if typ = pawn then
      if (tr = 7) or (tr = 0) then board[tr, tc] := makepiece(col, queen);

   turn := 1-turn;

   if not has_legal_moves(turn) then begin

      if in_check(turn) then gamestate := 1
      else gamestate := 2

   end else gamestate := 0;

   { play sound only for real moves }
   if sndenable then begin

      if gamestate = 1 then play_sound(mate_note, mate_dur)
      else if in_check(turn) then play_sound(check_note, check_dur)
      else if iscapture then play_sound(capture_note, move_dur)
      else play_sound(move_note, move_dur)

   end

end;

{******************************************************************************

Computer AI - minimax with alpha-beta pruning

******************************************************************************}

function evaluate: integer;

var r, c, score, p, typ, col, pstval: integer;

begin

   score := 0;
   for r := 0 to 7 do
      for c := 0 to 7 do begin

         p := board[r, c];
         if p <> empty then begin

            typ := ptyp(p);
            col := pcol(p);

            { material }
            pstval := pieceval[typ];

            { positional bonus from piece-square tables }
            if typ = pawn then begin

               if col = white_side then pstval := pstval+pawnpst[r, c]
               else pstval := pstval+pawnpst[7-r, c]

            end else if typ = knight then begin

               if col = white_side then pstval := pstval+knightpst[r, c]
               else pstval := pstval+knightpst[7-r, c]

            end;

            if col = white_side then score := score+pstval
            else score := score-pstval

         end

      end;
   evaluate := score

end;

procedure save_state(var s: boardstate);

begin

   s.bd := board;
   s.trn := turn; s.epc := epcol;
   s.wkc := wkcastle; s.wqc := wqcastle;
   s.bkc := bkcastle; s.bqc := bqcastle;
   s.gst := gamestate

end;

procedure restore_state(var s: boardstate);

begin

   board := s.bd;
   turn := s.trn; epcol := s.epc;
   wkcastle := s.wkc; wqcastle := s.wqc;
   bkcastle := s.bkc; bqcastle := s.bqc;
   gamestate := s.gst

end;

function minimax(depth, alpha, beta: integer; maximizing: boolean): integer;

var moves:               movearr;
    nmov, i, val, score: integer;
    saved:               boardstate;
    col:                 integer;
    prune:               boolean;

begin

   if depth = 0 then minimax := evaluate
   else begin

      if maximizing then col := white_side else col := black_side;
      nmov := gen_all_moves(col, moves);

      if nmov = 0 then begin

         if in_check(col) then begin

            if maximizing then minimax := -100000+(ai_depth-depth)
            else minimax := 100000-(ai_depth-depth)

         end else minimax := 0 { stalemate }

      end else begin

         prune := false;
         if maximizing then begin

            val := -200000;
            i := 1;
            while (i <= nmov) and not prune do begin

               save_state(saved);
               do_move(moves[i].fr, moves[i].fc, moves[i].tr, moves[i].tc);
               score := minimax(depth-1, alpha, beta, false);
               restore_state(saved);
               if score > val then val := score;
               if val > alpha then alpha := val;
               if beta <= alpha then prune := true;
               i := i+1

            end

         end else begin

            val := 200000;
            i := 1;
            while (i <= nmov) and not prune do begin

               save_state(saved);
               do_move(moves[i].fr, moves[i].fc, moves[i].tr, moves[i].tc);
               score := minimax(depth-1, alpha, beta, true);
               restore_state(saved);
               if score < val then val := score;
               if val < beta then beta := val;
               if beta <= alpha then prune := true;
               i := i+1

            end

         end;
         minimax := val

      end

   end

end;

procedure ai_move;

var moves:                  movearr;
    nmov, i, bestidx, val:  integer;
    bestval:                integer;
    saved:                  boardstate;
    aicolor:                integer;
    maximizing:             boolean;

begin

   aicolor := turn;
   maximizing := aicolor = white_side;

   nmov := gen_all_moves(aicolor, moves);
   if nmov > 0 then begin

      sndenable := false; { silence during search }
      bestidx := 1;
      if maximizing then begin

         bestval := -200000;
         for i := 1 to nmov do begin

            save_state(saved);
            do_move(moves[i].fr, moves[i].fc, moves[i].tr, moves[i].tc);
            val := minimax(ai_depth-1, -200000, 200000, false);
            restore_state(saved);
            if val > bestval then begin bestval := val; bestidx := i end

         end

      end else begin

         bestval := 200000;
         for i := 1 to nmov do begin

            save_state(saved);
            do_move(moves[i].fr, moves[i].fc, moves[i].tr, moves[i].tc);
            val := minimax(ai_depth-1, -200000, 200000, true);
            restore_state(saved);
            if val < bestval then begin bestval := val; bestidx := i end

         end

      end;
      sndenable := true; { re-enable for the actual move }

      do_move(moves[bestidx].fr, moves[bestidx].fc,
              moves[bestidx].tr, moves[bestidx].tc)

   end

end;

{ check if it is the computer's turn }

function is_computer_turn: boolean;

var r: boolean;

begin

   r := false;
   if gamemode = mode_pvc_w then if turn = black_side then r := true;
   if gamemode = mode_pvc_b then if turn = white_side then r := true;
   is_computer_turn := r

end;

{******************************************************************************

Drawing

******************************************************************************}

procedure load_pieces;

var col, typ, sq:           integer;
    np, cp, sp:             pstring;
    p1, p2, p3, p4, p5:     pstring;

begin

   for col := 0 to 1 do
      for sq := 0 to 1 do
         for typ := 1 to 6 do begin

            openstring;
            case typ of

               pawn:   np := copy('pawn');
               knight: np := copy('knight');
               bishop: np := copy('bishop');
               rook:   np := copy('rook');
               queen:  np := copy('queen');
               king:   np := copy('king')

            end;
            if col = white_side then cp := copy('w') else cp := copy('b');
            if sq = 0 then sp := copy('l') else sp := copy('d');
            p1 := cat('graph_games/chess_pieces/', cp);
            p2 := cat(p1, np);
            p3 := cat(p2, '_');
            p4 := cat(p3, sp);
            p5 := cat(p4, '.bmp');
            loadpict(output, picid(col, typ, sq = 0), p5^);
            closestring

         end

end;

procedure draw_piece(r, c, px, py, sz: integer);

var pc, pt, pid, margin: integer;
    light:               boolean;

begin

   pc := pcol(board[r, c]);
   pt := ptyp(board[r, c]);
   light := (r+c) mod 2 = 1;
   pid := picid(pc, pt, light);
   margin := sz div 10;

   picture(output, pid, px+margin, py+margin,
           px+sz-margin-1, py+sz-margin-1)

end;

procedure draw_square(r, c: integer);

var px, py, sz, i: integer;
    islight:       boolean;
    ismove:        boolean;

begin

   sz := sqsize;
   sq2px(r, c, px, py);

   islight := (r+c) mod 2 = 1;

   if selected and (r = selr) and (c = selc) then
      fcolorg(output, sel_r, sel_g, sel_b)
   else begin

      ismove := false;
      if selected then
         for i := 1 to nmoves do
            if (legalmoves[i].tr = r) and (legalmoves[i].tc = c) then
               ismove := true;
      if ismove then fcolorg(output, movc_r, movc_g, movc_b)
      else if islight then fcolorg(output, light_r, light_g, light_b)
      else fcolorg(output, dark_r, dark_g, dark_b)

   end;
   frect(output, px, py, px+sz-1, py+sz-1);

   if board[r, c] <> empty then draw_piece(r, c, px, py, sz)

end;

procedure draw_board;

var r, c, sz, bx, by: integer;
    lbl:              packed array [1..1] of char;

begin

   fcolor(output, white);
   frect(output, 1, 1, maxxg(output), maxyg(output));

   for r := 0 to 7 do
      for c := 0 to 7 do
         draw_square(r, c);

   { rank and file labels }
   sz := sqsize;
   bx := boardx0;
   by := boardy0;

   fontsiz(output, sz div 4);
   fcolorg(output, 80*(i32max div 255), 80*(i32max div 255),
           80*(i32max div 255));

   for c := 0 to 7 do begin

      lbl[1] := chr(ord('a')+c);
      cursorg(output, bx+c*sz+sz div 2-strsiz(output, lbl) div 2,
              by+8*sz+2);
      write(lbl[1])

   end;
   for r := 0 to 7 do begin

      lbl[1] := chr(ord('1')+r);
      cursorg(output, bx-sz div 3, by+(7-r)*sz+sz div 2-sz div 8);
      write(lbl[1])

   end

end;

{ print status message centered at given y }

procedure stmsg(sy: integer; view s: string);

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
   fcolorg(output, 40*(i32max div 255), 40*(i32max div 255),
           40*(i32max div 255));

   if gamestate = 1 then begin

      if turn = white_side then stmsg(sy, 'Checkmate! Black wins.')
      else stmsg(sy, 'Checkmate! White wins.')

   end else if gamestate = 2 then stmsg(sy, 'Stalemate - draw.')
   else if is_computer_turn then stmsg(sy, 'Computer is thinking...')
   else if in_check(turn) then begin

      if turn = white_side then stmsg(sy, 'White to move - CHECK!')
      else stmsg(sy, 'Black to move - CHECK!')

   end else begin

      if turn = white_side then stmsg(sy, 'White to move.')
      else stmsg(sy, 'Black to move.')

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

{ create menu entry }

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

{ append a menu entry to the given list }

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

var ml:         menuptr;
    game_menu:  menuptr;
    game_items: menuptr;
    mode_menu:  menuptr;
    mode_items: menuptr;
    help_menu:  menuptr;
    help_items: menuptr;

begin

   ml := nil;

   { Game menu }
   game_menu := newmenuitem(false, false, false, 0, 'Game');
   appendmenu(ml, game_menu);
   game_items := nil;
   appendmenu(game_items,
      newmenuitem(false, false, true, menu_new, 'New Game'));
   appendmenu(game_items,
      newmenuitem(false, false, false, menu_exit, 'Exit'));
   game_menu^.branch := game_items;

   { Mode menu }
   mode_menu := newmenuitem(false, false, false, 0, 'Mode');
   appendmenu(ml, mode_menu);
   mode_items := nil;
   appendmenu(mode_items,
      newmenuitem(false, true, false, menu_pvp, 'Player vs Player'));
   appendmenu(mode_items,
      newmenuitem(false, true, false, menu_pvc_w, 'Play White vs Computer'));
   appendmenu(mode_items,
      newmenuitem(false, true, false, menu_pvc_b, 'Play Black vs Computer'));
   mode_menu^.branch := mode_items;

   { Help menu }
   help_menu := newmenuitem(false, false, false, 0, 'Help');
   appendmenu(ml, help_menu);
   help_items := nil;
   appendmenu(help_items,
      newmenuitem(false, false, false, menu_about, 'About Chess'));
   help_menu^.branch := help_items;

   menu(output, ml)

end;

{******************************************************************************

Event handlers

******************************************************************************}

{ handle a player mouse click on the board }

procedure do_click;

var mr, mc, i: integer;
    found:     boolean;

begin

   if px2sq(mousex, mousey, mr, mc) then begin

      if selected then begin

         found := false;
         for i := 1 to nmoves do
            if (legalmoves[i].tr = mr) and (legalmoves[i].tc = mc) then
               found := true;
         if found then begin

            do_move(selr, selc, mr, mc);
            selected := false;
            nmoves := 0;
            draw_all;
            { schedule AI response }
            if gamestate = 0 then
               if is_computer_turn then begin

                  timer(output, timer_ai, 100, 0);
                  aipending := true;
                  draw_all

               end

         end else begin

            selected := false;
            nmoves := 0;
            if board[mr, mc] <> empty then
               if pcol(board[mr, mc]) = turn then begin

                  gen_piece_moves(mr, mc);
                  if nmoves > 0 then begin

                     selected := true;
                     selr := mr; selc := mc

                  end

               end;
            draw_all

         end

      end else begin

         if board[mr, mc] <> empty then
            if pcol(board[mr, mc]) = turn then begin

               gen_piece_moves(mr, mc);
               if nmoves > 0 then begin

                  selected := true;
                  selr := mr; selc := mc;
                  draw_all

               end

            end

      end

   end

end;

{ show the about dialog }

procedure do_about;

var nl:             packed array [1..1] of char;
    p1, p2, p3, p4: pstring;

begin

   nl[1] := chr(10);
   openstring;
   p1 := cat('Chess for Amitk', nl);
   p2 := cat(p1, 'Human vs human or vs computer');
   p3 := cat(p2, nl);
   p4 := cat(p3, 'Copyright (C) 2026 S. A. Franco');
   alert('About Chess', p4^);
   closestring

end;

{ handle menu selections }

procedure do_menu(var er: evtrec);

begin

   case er.menuid of

      menu_new: begin

         init_board;
         selected := false; nmoves := 0;
         draw_all;
         if is_computer_turn then begin

            timer(output, timer_ai, 100, 0);
            aipending := true;
            draw_all

         end

      end;

      menu_exit: er.etype := etterm;

      menu_pvp: begin

         gamemode := mode_pvp;
         init_board;
         draw_all

      end;

      menu_pvc_w: begin

         gamemode := mode_pvc_w;
         init_board;
         draw_all

      end;

      menu_pvc_b: begin

         gamemode := mode_pvc_b;
         init_board;
         draw_all;
         if is_computer_turn then begin

            timer(output, timer_ai, 100, 0);
            aipending := true;
            draw_all

         end

      end;

      menu_about: begin

         do_about;
         draw_all

      end

      else { ignore other menu ids }

   end

end;

{******************************************************************************

Main program

******************************************************************************}

begin

   title(output, 'Chess');
   curvis(output, 0);
   auto(output, 0);
   buffer(output, 0);
   font(output, ftsign);
   bold(output, 1);
   binvis(output);

   opensynthout(synth_out);
   instchange(synth_out, 0, 1, inst_acoustic_grand);
   starttimeout;
   sndenable := true;

   gamemode := mode_pvc_w;
   setup_menu;
   load_pieces;
   init_board;
   draw_all;

   { if computer goes first, schedule AI move }
   if is_computer_turn then begin

      timer(output, timer_ai, 100, 0);
      aipending := true;
      draw_all

   end;

   repeat

      event(input, er);

      if (er.etype = etredraw) or (er.etype = etresize) then draw_all
      else if er.etype = etmoumovg then begin

         mousex := er.moupxg;
         mousey := er.moupyg

      end else if er.etype = etmouba then begin

         if er.amoubn = 1 then
            if gamestate = 0 then
               if not is_computer_turn then do_click

      end else if er.etype = ettim then begin

         if er.timnum = timer_ai then
            if gamestate = 0 then
               if is_computer_turn then begin

                  ai_move;
                  aipending := false;
                  draw_all

               end

      end else if er.etype = etmenus then do_menu(er)

   until er.etype = etterm;

   99: { terminate }

   closesynthout(synth_out)

end.
