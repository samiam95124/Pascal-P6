{******************************************************************************
*                                                                             *
*                              DEFENDERS GAME                                 *
*                                                                             *
*                       COPYRIGHT (C) 2026 S. A. FRANCO                       *
*                                                                             *
* A Space Invaders style arcade game with original artwork. Player defends    *
* against waves of descending alien invaders using a mobile cannon.           *
*                                                                             *
* Pascaline port of the Ami defenders.c reference game.                       *
*                                                                             *
******************************************************************************}

program defenders(input, output);

uses graphics,
     sound,
     services,
     strings;

label 99; { terminate }

const

   second       = 10000;      { one second in timer units }

   { timer IDs }
   timer_afire  = 2;          { alien fire timer }
   timer_ufo    = 3;          { UFO spawn timer }

   { timer intervals }
   afire_min    = 5000;       { min alien fire interval }
   afire_max    = 15000;      { max alien fire interval }
   ufo_interval = 150000;     { UFO appears ~every 15s }

   { grid dimensions }
   alien_rows   = 5;
   alien_cols   = 8;
   num_shields  = 4;
   shield_segs  = 5;          { segments per shield axis }

   { menu IDs }
   menu_new     = 100;
   menu_exit    = 101;
   menu_about   = 102;

   { sound defines }
   shoot_note   = 61;         { note_c+octave_6 }
   hit_note     = 77;         { note_e+octave_7 }
   explode_note = 37;         { note_c+octave_4 }
   ufo_note     = 58;         { note_a+octave_5 }
   shoot_dur    = 100;
   hit_dur      = 150;
   explode_dur  = 500;
   ufo_dur      = 200;

   { 32 bit INT_MAX, base for color/joystick ratios }
   i32max       = 2147483647;
   clrunit      = 8421504;    { i32max div 255, color scale unit }

   { directions }
   dir_right    = 1;
   dir_left     = -1;

   { max bullets }
   max_pbullets = 3;
   max_abullets = 8;

   { input device control - last device to change takes over }
   ctl_mouse    = 0;
   ctl_keyboard = 1;
   ctl_joystick = 2;
   joy_threshold = 107374182; { i32max div 20, movement to take over }

   { font code (AMI_FONT_SIGN) }
   ftsign       = 3;

type

   bulletrec = record
      x, y:   integer; { center position }
      active: boolean
   end;

   alienrec = record
      alive:    boolean;
      row, col: integer;
      points:   integer { score value }
   end;

   uforec = record
      active: boolean;
      x, y:   integer;
      dx:     integer { direction: positive = right }
   end;

   shieldrec = record
      x, y: integer; { top-left of shield block }
      w, h: integer; { total size }
      { intact flags for each shield segment }
      segs: array [0..4, 0..4] of boolean
   end;

var

   { screen metrics }
   scr_w, scr_h:       integer;
   alien_w, alien_h:   integer; { size of one alien cell }
   player_w, player_h: integer; { player ship size }
   bullet_w, bullet_h: integer; { bullet dimensions }
   ufo_w, ufo_h:       integer; { UFO size }

   { game objects }
   aliens:             array [0..4, 0..7] of alienrec;
   alien_base_x, alien_base_y: integer; { top-left of alien grid }
   alien_dir:          integer; { dir_right or dir_left }
   alien_move_counter: integer; { ticks until next alien move }
   alien_move_rate:    integer; { ticks between alien moves }
   aliens_alive:       integer; { count of living aliens }

   player_x:           integer; { player center x }
   player_y:           integer; { player top y }
   mouse_x:            integer; { current mouse x }

   active_ctl:         integer; { which device is controlling }
   last_joyx:          integer; { last joystick x for change detection }

   pbullets:           array [0..2] of bulletrec; { player bullets }
   abullets:           array [0..7] of bulletrec; { alien bullets }

   shields:            array [0..3] of shieldrec;

   ufo:                uforec;

   score:              integer;
   lives:              integer;
   wave:               integer;
   game_over:          boolean;
   game_started:       boolean;
   flip:               integer; { double buffer flip state, 0 or 1 }

   randstate:          integer; { random number generator state }

   er:                 evtrec;  { event record }
   move_step:          integer;
   jdelta, jchr:       integer; { joystick work values }

{******************************************************************************

Random numbers

Linear congruential generator, since the runtime exposes no rand primitive.

******************************************************************************}

{ find random number between 0 and limit, inclusive }

function randn(limit: integer): integer;

begin

   { advance LCG: state := (state*1103515245+12345) mod 2^31, kept positive }
   randstate := (randstate*1103515245+12345) mod i32max;
   if randstate < 0 then randstate := randstate+i32max;
   if limit <= 0 then randn := 0
   else randn := randstate mod (limit+1)

end;

{ color helper: scale 0-255 to 0-INT_MAX }

function clr(v: integer): integer;

begin

   clr := v*clrunit

end;

{******************************************************************************

Menu setup

******************************************************************************}

{ create menu entry }

procedure newmenuitem(var mp: menuptr; onoff, oneof, bar: boolean;
                      id: integer; view face: string);

begin

   new(mp);
   mp^.next := nil;
   mp^.branch := nil;
   mp^.onoff := onoff;
   mp^.oneof := oneof;
   mp^.bar := bar;
   mp^.id := id;
   mp^.face := copy(face)

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

var ml:         menuptr; { menu list }
    game_menu:  menuptr;
    game_items: menuptr;
    help_menu:  menuptr;
    help_items: menuptr;
    mp:         menuptr;

begin

   ml := nil;

   { Game menu }
   newmenuitem(game_menu, false, false, false, 0, 'Game');
   appendmenu(ml, game_menu);
   game_items := nil;
   newmenuitem(mp, false, false, true, menu_new, 'New Game');
   appendmenu(game_items, mp);
   newmenuitem(mp, false, false, false, menu_exit, 'Exit');
   appendmenu(game_items, mp);
   game_menu^.branch := game_items;

   { Help menu }
   newmenuitem(help_menu, false, false, false, 0, 'Help');
   appendmenu(ml, help_menu);
   help_items := nil;
   newmenuitem(mp, false, false, false, menu_about, 'About Defenders');
   appendmenu(help_items, mp);
   help_menu^.branch := help_items;

   menu(output, ml)

end;

{******************************************************************************

Calculate screen metrics

******************************************************************************}

procedure calc_metrics;

begin

   scr_w := maxxg(output);
   scr_h := maxyg(output);
   alien_w := scr_w div 12;
   alien_h := scr_h div 18;
   player_w := scr_w div 14;
   player_h := scr_h div 22;
   bullet_w := scr_w div 200;
   if bullet_w < 2 then bullet_w := 2;
   bullet_h := scr_h div 40;
   if bullet_h < 4 then bullet_h := 4;
   ufo_w := alien_w*3 div 2;
   ufo_h := alien_h*2 div 3

end;

{******************************************************************************

Initialize shields

******************************************************************************}

procedure init_shields;

var i, r, c:                          integer;
    shld_w, shld_h, gap, start_x, shld_y: integer;

begin

   shld_w := scr_w div 10;
   shld_h := scr_h div 16;
   gap := scr_w div (num_shields+1);
   start_x := gap-shld_w div 2;
   shld_y := scr_h-scr_h div 6;

   for i := 0 to num_shields-1 do begin

      shields[i].x := start_x+i*gap;
      shields[i].y := shld_y;
      shields[i].w := shld_w;
      shields[i].h := shld_h;
      for r := 0 to shield_segs-1 do
         for c := 0 to shield_segs-1 do
            shields[i].segs[r, c] := true

   end

end;

{******************************************************************************

Initialize aliens for a new wave

******************************************************************************}

procedure init_aliens;

var r, c: integer;

begin

   alien_base_x := scr_w div 8;
   alien_base_y := scr_h div 8;
   alien_dir := dir_right;
   alien_move_counter := 0;
   { speed increases with wave number }
   alien_move_rate := 12-wave;
   if alien_move_rate < 2 then alien_move_rate := 2;
   aliens_alive := alien_rows*alien_cols;

   for r := 0 to alien_rows-1 do
      for c := 0 to alien_cols-1 do begin

      aliens[r, c].alive := true;
      aliens[r, c].row := r;
      aliens[r, c].col := c;
      if r = 0 then aliens[r, c].points := 30
      else if r <= 2 then aliens[r, c].points := 20
      else aliens[r, c].points := 10

   end

end;

{******************************************************************************

Initialize a new game

******************************************************************************}

procedure init_game;

var i: integer;

begin

   calc_metrics;
   score := 0;
   lives := 3;
   wave := 1;
   game_over := false;
   game_started := true;
   active_ctl := ctl_mouse;
   last_joyx := 0;
   player_x := scr_w div 2;
   player_y := scr_h-scr_h div 10;

   for i := 0 to max_pbullets-1 do pbullets[i].active := false;
   for i := 0 to max_abullets-1 do abullets[i].active := false;

   ufo.active := false;
   init_aliens;
   init_shields

end;

{******************************************************************************

Drawing helpers

******************************************************************************}

procedure set_color_black;

begin

   fcolorg(output, 0, 0, 0)

end;

procedure clear_screen;

begin

   set_color_black;
   frect(output, 1, 1, scr_w, scr_h)

end;

{ write an integer zero padded to the given width (C "%06d") }

procedure wrtzint(n, w: integer);

var d, t, j: integer;

begin

   d := 1; { count digits in n }
   t := n;
   while t >= 10 do begin t := t div 10; d := d+1 end;
   for j := 1 to w-d do write('0');
   write(n:1)

end;

{******************************************************************************

Draw player ship - trapezoid/wedge shape

******************************************************************************}

procedure draw_player;

var cx, cy, hw, hh, i, w: integer;

begin

   cx := player_x;
   cy := player_y;
   hw := player_w div 2;
   hh := player_h;

   { bright green ship }
   fcolorg(output, clr(50), clr(255), clr(50));

   { draw wedge shape: stacked rectangles getting narrower toward top }
   for i := 0 to hh-1 do begin

      w := hw*(hh-i) div hh;
      if w < 2 then w := 2;
      frect(output, cx-w, cy+i, cx+w, cy+i)

   end;

   { cannon turret on top }
   fcolorg(output, clr(100), clr(255), clr(100));
   frect(output, cx-2, cy-hh div 3, cx+2, cy);

   { cockpit highlight }
   fcolorg(output, clr(200), clr(255), clr(200));
   frect(output, cx-hw div 4, cy+hh div 3, cx+hw div 4, cy+hh div 2)

end;

{******************************************************************************

Draw an alien based on its row type

******************************************************************************}

procedure draw_alien_at(cx, cy, row: integer);

var aw, ah, i, w: integer;

begin

   aw := alien_w div 2-2;
   ah := alien_h div 2-2;

   if row = 0 then begin

      { Diamond shape (row 1, top) - magenta/pink }
      fcolorg(output, clr(255), clr(80), clr(220));
      { draw diamond using lines }
      for i := 0 to ah do begin

         w := aw*i div ah;
         frect(output, cx-w, cy-i, cx+w, cy-i);
         frect(output, cx-w, cy+i, cx+w, cy+i)

      end;
      { inner eye dot }
      fcolorg(output, clr(255), clr(255), clr(255));
      fellipse(output, cx-aw div 4, cy-ah div 4, cx+aw div 4, cy+ah div 4)

   end else if row <= 2 then begin

      { Triangle shapes (rows 2-3) - cyan }
      fcolorg(output, clr(0), clr(255), clr(255));
      { inverted triangle: wide at top, narrow at bottom }
      for i := 0 to ah*2-1 do begin

         w := aw*(ah*2-i) div (ah*2);
         frect(output, cx-w, cy-ah+i, cx+w, cy-ah+i)

      end;
      { two "eye" dots }
      fcolorg(output, clr(255), clr(0), clr(0));
      fellipse(output, cx-aw div 3-2, cy-ah div 3-2,
                       cx-aw div 3+2, cy-ah div 3+2);
      fellipse(output, cx+aw div 3-2, cy-ah div 3-2,
                       cx+aw div 3+2, cy-ah div 3+2)

   end else begin

      { Circle shapes (rows 4-5) - yellow-green }
      fcolorg(output, clr(180), clr(255), clr(50));
      fellipse(output, cx-aw, cy-ah, cx+aw, cy+ah);
      { inner face: two eyes and mouth }
      fcolorg(output, clr(40), clr(40), clr(0));
      fellipse(output, cx-aw div 3-2, cy-ah div 4-2,
                       cx-aw div 3+2, cy-ah div 4+2);
      fellipse(output, cx+aw div 3-2, cy-ah div 4-2,
                       cx+aw div 3+2, cy-ah div 4+2);
      frect(output, cx-aw div 3, cy+ah div 4, cx+aw div 3, cy+ah div 4+1)

   end

end;

{******************************************************************************

Get alien center position from grid coordinates

******************************************************************************}

procedure alien_pos(row, col: integer; var cx, cy: integer);

begin

   cx := alien_base_x+col*(alien_w+alien_w div 4)+alien_w div 2;
   cy := alien_base_y+row*(alien_h+alien_h div 4)+alien_h div 2

end;

{******************************************************************************

Draw all aliens

******************************************************************************}

procedure draw_aliens;

var r, c, cx, cy: integer;

begin

   for r := 0 to alien_rows-1 do
      for c := 0 to alien_cols-1 do
         if aliens[r, c].alive then begin

         alien_pos(r, c, cx, cy);
         draw_alien_at(cx, cy, r)

      end

end;

{******************************************************************************

Draw shields

******************************************************************************}

procedure draw_shields;

var i, r, c:        integer;
    sw, sh, sx, sy: integer;

begin

   fcolorg(output, clr(60), clr(60), clr(255));

   for i := 0 to num_shields-1 do begin

      sw := shields[i].w div shield_segs;
      sh := shields[i].h div shield_segs;
      for r := 0 to shield_segs-1 do
         for c := 0 to shield_segs-1 do
            if shields[i].segs[r, c] then begin

            sx := shields[i].x+c*sw;
            sy := shields[i].y+r*sh;
            frect(output, sx, sy, sx+sw-1, sy+sh-1)

         end

   end

end;

{******************************************************************************

Draw bullets

******************************************************************************}

procedure draw_bullets;

var i: integer;

begin

   { player bullets - white }
   fcolorg(output, clr(255), clr(255), clr(255));
   for i := 0 to max_pbullets-1 do
      if pbullets[i].active then
         frect(output, pbullets[i].x-bullet_w div 2,
                       pbullets[i].y-bullet_h div 2,
                       pbullets[i].x+bullet_w div 2,
                       pbullets[i].y+bullet_h div 2);

   { alien bullets - red/orange }
   fcolorg(output, clr(255), clr(120), clr(0));
   for i := 0 to max_abullets-1 do
      if abullets[i].active then
         frect(output, abullets[i].x-bullet_w div 2,
                       abullets[i].y-bullet_h div 2,
                       abullets[i].x+bullet_w div 2,
                       abullets[i].y+bullet_h div 2)

end;

{******************************************************************************

Draw UFO

******************************************************************************}

procedure draw_ufo;

var cx, cy, hw, hh: integer;

begin

   if ufo.active then begin

      cx := ufo.x;
      cy := ufo.y;
      hw := ufo_w div 2;
      hh := ufo_h div 2;

      { red saucer body }
      fcolorg(output, clr(255), clr(30), clr(30));
      fellipse(output, cx-hw, cy-hh div 2, cx+hw, cy+hh div 2);

      { dome on top }
      fcolorg(output, clr(255), clr(100), clr(100));
      fellipse(output, cx-hw div 3, cy-hh, cx+hw div 3, cy-hh div 3);

      { lights }
      fcolorg(output, clr(255), clr(255), clr(0));
      fellipse(output, cx-hw div 2-2, cy-2, cx-hw div 2+2, cy+2);
      fellipse(output, cx-2, cy-2, cx+2, cy+2);
      fellipse(output, cx+hw div 2-2, cy-2, cx+hw div 2+2, cy+2)

   end

end;

{******************************************************************************

Draw HUD - score, lives, wave

******************************************************************************}

procedure draw_hud;

var fsz, lx, i: integer;

begin

   fsz := scr_h div 30;
   if fsz < 10 then fsz := 10;
   fontsiz(output, fsz);

   { score on left }
   fcolorg(output, clr(255), clr(255), clr(255));
   cursorg(output, scr_w div 40, scr_h div 60);
   write('SCORE ');
   wrtzint(score, 6);
   writeln;

   { wave in center }
   cursorg(output, scr_w div 2-scr_w div 20, scr_h div 60);
   writeln('WAVE ', wave:1);

   { lives on right - draw small ship icons }
   cursorg(output, scr_w-scr_w div 5, scr_h div 60);
   writeln('LIVES');
   lx := scr_w-scr_w div 5+scr_w div 12;
   fcolorg(output, clr(50), clr(255), clr(50));
   for i := 0 to lives-1 do
      fellipse(output, lx+i*(player_w div 3+4), scr_h div 60+2,
                       lx+i*(player_w div 3+4)+player_w div 4,
                       scr_h div 60+fsz-2)

end;

{******************************************************************************

Draw game over overlay

******************************************************************************}

procedure draw_gameover;

var fsz: integer;

begin

   fsz := scr_h div 10;
   if fsz < 20 then fsz := 20;
   fontsiz(output, fsz);
   fcolorg(output, clr(255), clr(0), clr(0));
   cursorg(output, scr_w div 2-scr_w div 5, scr_h div 2-fsz div 2);
   writeln('GAME OVER');

   fsz := scr_h div 20;
   fontsiz(output, fsz);
   fcolorg(output, clr(255), clr(255), clr(255));
   cursorg(output, scr_w div 2-scr_w div 5, scr_h div 2+scr_h div 10);
   writeln('Select Game > New Game to play again')

end;

{******************************************************************************

Draw entire screen

******************************************************************************}

procedure draw_all;

var fsz: integer;

begin

   calc_metrics;
   clear_screen;
   if not game_started then begin

      fsz := scr_h div 10;
      if fsz < 20 then fsz := 20;
      fontsiz(output, fsz);
      fcolorg(output, clr(50), clr(255), clr(50));
      cursorg(output, scr_w div 2-scr_w div 4, scr_h div 3);
      writeln('DEFENDERS');
      fsz := scr_h div 20;
      fontsiz(output, fsz);
      fcolorg(output, clr(255), clr(255), clr(255));
      cursorg(output, scr_w div 2-scr_w div 4, scr_h div 2);
      writeln('Select Game > New Game to start')

   end else begin

      draw_hud;
      draw_aliens;
      draw_shields;
      draw_player;
      draw_bullets;
      draw_ufo;
      if game_over then draw_gameover

   end

end;

{******************************************************************************

Fire a player bullet

******************************************************************************}

procedure player_fire;

var i:     integer;
    fired: boolean;

begin

   if not game_over then begin

      fired := false;
      for i := 0 to max_pbullets-1 do
         if not fired then
            if not pbullets[i].active then begin

            pbullets[i].active := true;
            pbullets[i].x := player_x;
            pbullets[i].y := player_y-player_h div 3;
            { shoot sound }
            noteon(synth_out, 0, 1, shoot_note, i32max);
            noteoff(synth_out, curtimeout+shoot_dur, 1, shoot_note, i32max);
            fired := true

         end

   end

end;

{******************************************************************************

Alien fires a bullet downward

******************************************************************************}

procedure alien_fire;

var i, k, r, c, cx, cy: integer;
    candidates:         array [0..7] of integer;
    ncand:              integer;
    found, fired:       boolean;

begin

   if not game_over then if aliens_alive > 0 then begin

      { find bottom-most alive alien in each column }
      ncand := 0;
      for c := 0 to alien_cols-1 do begin

         found := false;
         for r := alien_rows-1 downto 0 do
            if not found then
               if aliens[r, c].alive then begin

               candidates[ncand] := r*alien_cols+c;
               ncand := ncand+1;
               found := true

            end

      end;

      if ncand > 0 then begin

         { pick random bottom alien }
         i := candidates[randn(ncand-1)];
         r := i div alien_cols;
         c := i mod alien_cols;

         { find free bullet slot }
         fired := false;
         for k := 0 to max_abullets-1 do
            if not fired then
               if not abullets[k].active then begin

               alien_pos(r, c, cx, cy);
               abullets[k].active := true;
               abullets[k].x := cx;
               abullets[k].y := cy+alien_h div 2;
               fired := true

            end

      end

   end

end;

{******************************************************************************

Check if a point hits a shield segment, and destroy it if so.
Returns true if hit.

******************************************************************************}

function hit_shield(bx, by: integer): boolean;

var i, r, c:        integer;
    sw, sh, sx, sy: integer;
    found:          boolean;

begin

   found := false;
   for i := 0 to num_shields-1 do
      if not found then
         { quick bounds check }
         if (bx >= shields[i].x) and (bx <= shields[i].x+shields[i].w) and
            (by >= shields[i].y) and (by <= shields[i].y+shields[i].h) then begin

         sw := shields[i].w div shield_segs;
         sh := shields[i].h div shield_segs;
         for r := 0 to shield_segs-1 do
            for c := 0 to shield_segs-1 do
               if not found then
                  if shields[i].segs[r, c] then begin

                  sx := shields[i].x+c*sw;
                  sy := shields[i].y+r*sh;
                  if (bx >= sx) and (bx <= sx+sw) and
                     (by >= sy) and (by <= sy+sh) then begin

                     shields[i].segs[r, c] := false;
                     found := true

                  end

               end

      end;
   hit_shield := found

end;

{******************************************************************************

Move player bullets and check collisions

******************************************************************************}

procedure update_pbullets;

var i, r, c, cx, cy: integer;
    speed:           integer;
    hit:             boolean;

begin

   speed := scr_h div 80;
   if speed < 3 then speed := 3;

   for i := 0 to max_pbullets-1 do
      if pbullets[i].active then begin

      pbullets[i].y := pbullets[i].y-speed;

      { off screen? }
      if pbullets[i].y < 0 then pbullets[i].active := false

      { hit shield? }
      else if hit_shield(pbullets[i].x, pbullets[i].y) then
         pbullets[i].active := false

      else begin

         hit := false;

         { hit UFO? }
         if ufo.active then
            if (pbullets[i].x >= ufo.x-ufo_w div 2) and
               (pbullets[i].x <= ufo.x+ufo_w div 2) and
               (pbullets[i].y >= ufo.y-ufo_h div 2) and
               (pbullets[i].y <= ufo.y+ufo_h div 2) then begin

               ufo.active := false;
               pbullets[i].active := false;
               score := score+100;
               noteon(synth_out, 0, 1, ufo_note, i32max);
               noteoff(synth_out, curtimeout+ufo_dur, 1, ufo_note, i32max);
               hit := true

            end;

         { hit alien? }
         if not hit then
            for r := 0 to alien_rows-1 do
               for c := 0 to alien_cols-1 do
                  if not hit then
                     if aliens[r, c].alive then begin

                     alien_pos(r, c, cx, cy);
                     if (pbullets[i].x >= cx-alien_w div 2) and
                        (pbullets[i].x <= cx+alien_w div 2) and
                        (pbullets[i].y >= cy-alien_h div 2) and
                        (pbullets[i].y <= cy+alien_h div 2) then begin

                        { hit! }
                        aliens[r, c].alive := false;
                        pbullets[i].active := false;
                        score := score+aliens[r, c].points;
                        aliens_alive := aliens_alive-1;

                        { speed up remaining aliens }
                        if aliens_alive > 0 then begin

                           alien_move_rate := 12-wave-
                              (alien_rows*alien_cols-aliens_alive) div 5;
                           if alien_move_rate < 1 then alien_move_rate := 1

                        end;

                        noteon(synth_out, 0, 1, hit_note, i32max);
                        noteoff(synth_out, curtimeout+hit_dur, 1, hit_note,
                                i32max);
                        hit := true

                     end

                  end

      end

   end

end;

{******************************************************************************

Move alien bullets and check collisions

******************************************************************************}

procedure update_abullets;

var i:     integer;
    speed: integer;

begin

   speed := scr_h div 120;
   if speed < 2 then speed := 2;

   for i := 0 to max_abullets-1 do
      if abullets[i].active then begin

      abullets[i].y := abullets[i].y+speed;

      { off screen? }
      if abullets[i].y > scr_h then abullets[i].active := false

      { hit shield? }
      else if hit_shield(abullets[i].x, abullets[i].y) then
         abullets[i].active := false

      { hit player? }
      else if (abullets[i].x >= player_x-player_w div 2) and
              (abullets[i].x <= player_x+player_w div 2) and
              (abullets[i].y >= player_y) and
              (abullets[i].y <= player_y+player_h) then begin

         abullets[i].active := false;
         lives := lives-1;
         noteon(synth_out, 0, 1, explode_note, i32max);
         noteoff(synth_out, curtimeout+explode_dur, 1, explode_note, i32max);
         if lives <= 0 then game_over := true

      end

   end

end;

{******************************************************************************

Move aliens

******************************************************************************}

procedure update_aliens;

var step_x, step_y: integer;
    need_drop:      boolean;
    r, c, cx, cy:   integer;

begin

   if aliens_alive > 0 then begin

      alien_move_counter := alien_move_counter+1;
      if alien_move_counter >= alien_move_rate then begin

         alien_move_counter := 0;

         step_x := scr_w div 80;
         if step_x < 2 then step_x := 2;
         step_y := alien_h div 2;

         { check if we need to change direction }
         need_drop := false;
         for r := 0 to alien_rows-1 do
            for c := 0 to alien_cols-1 do
               if aliens[r, c].alive then begin

               alien_pos(r, c, cx, cy);
               if alien_dir = dir_right then
                  if cx+alien_w div 2+step_x >= scr_w then need_drop := true;
               if alien_dir = dir_left then
                  if cx-alien_w div 2-step_x <= 0 then need_drop := true

            end;

         if need_drop then begin

            alien_dir := -alien_dir;
            alien_base_y := alien_base_y+step_y;

            { check if aliens reached player level }
            for r := 0 to alien_rows-1 do
               for c := 0 to alien_cols-1 do
                  if aliens[r, c].alive then begin

                  alien_pos(r, c, cx, cy);
                  if cy+alien_h div 2 >= player_y then game_over := true

               end

         end else alien_base_x := alien_base_x+step_x*alien_dir

      end

   end

end;

{******************************************************************************

Move UFO

******************************************************************************}

procedure update_ufo;

var speed: integer;

begin

   if ufo.active then begin

      speed := scr_w div 200;
      if speed < 1 then speed := 1;

      ufo.x := ufo.x+speed*ufo.dx;

      if (ufo.x < -ufo_w) or (ufo.x > scr_w+ufo_w) then ufo.active := false

   end

end;

{******************************************************************************

Spawn a UFO

******************************************************************************}

procedure spawn_ufo;

begin

   if not ufo.active then if not game_over then begin

      ufo.active := true;
      ufo.y := scr_h div 16;
      if randn(1) = 0 then begin

         ufo.x := -ufo_w;
         ufo.dx := 1

      end else begin

         ufo.x := scr_w+ufo_w;
         ufo.dx := -1

      end

   end

end;

{******************************************************************************

Check for wave completion

******************************************************************************}

procedure check_wave_complete;

begin

   if aliens_alive = 0 then if not game_over then begin

      wave := wave+1;
      init_aliens;
      { partially restore shields each wave }
      init_shields

   end

end;

{******************************************************************************

Show the about dialog

******************************************************************************}

procedure show_about;

var ms: pstring;
    nl: packed array [1..1] of char;

begin

   nl[1] := chr(10);
   openstring; { use recycled dynamic strings }
   ms := cat('Defenders - A Space Invaders style game', nl);
   ms := cat(ms, 'Use arrows or mouse to move, click to fire');
   ms := cat(ms, nl);
   ms := cat(ms, 'Copyright (C) 2026 S. A. Franco');
   alert('About Defenders', ms^);
   closestring

end;

{******************************************************************************

Main program

******************************************************************************}

begin

   { seed random number generator from the time of day }
   randstate := time mod i32max;
   if randstate < 0 then randstate := randstate+i32max;

   title(output, 'Defenders');
   curvis(output, false);
   auto(output, false);
   { stay in buffered mode for select double buffering }
   font(output, ftsign);
   bold(output, true);
   binvis(output);
   bcolorg(output, 0, 0, 0);
   frametimer(output, true);
   flip := 0;

   opensynthout(synth_out);
   instchange(synth_out, 0, 1, inst_lead_1_square);
   starttimeout;

   setup_menu;

   calc_metrics;
   init_game;
   draw_all;

   { start game timers }
   timer(output, timer_afire, afire_min+randn(afire_max-afire_min-1), false);
   timer(output, timer_ufo, ufo_interval, false);

   mouse_x := scr_w div 2;

   repeat

      event(input, er);

      if er.etype = etresize then begin

         sizbufg(output, er.rszxg, er.rszyg);
         calc_metrics;
         player_y := scr_h-scr_h div 10;
         if player_x > scr_w-player_w div 2 then
            player_x := scr_w-player_w div 2;
         init_shields;
         draw_all;
         select(output, (1-flip)+1, flip+1);
         flip := 1-flip

      end

      else if er.etype = etredraw then draw_all

      else if er.etype = etmoumovg then begin

         if mouse_x <> er.moupxg then active_ctl := ctl_mouse;
         mouse_x := er.moupxg;
         if active_ctl = ctl_mouse then
            if game_started and (not game_over) then begin

            player_x := mouse_x;
            if player_x < player_w div 2 then player_x := player_w div 2;
            if player_x > scr_w-player_w div 2 then
               player_x := scr_w-player_w div 2

         end

      end

      else if er.etype = etmouba then begin

         if er.amoubn = 1 then
            if game_started and (not game_over) then player_fire

      end

      else if er.etype = etenter then begin

         if game_started and (not game_over) then player_fire

      end

      else if er.etype = etchar then begin

         if er.echar = ' ' then
            if game_started and (not game_over) then player_fire

      end

      else if er.etype = etleft then begin

         active_ctl := ctl_keyboard;
         if game_started and (not game_over) then begin

            move_step := scr_w div 50;
            if move_step < 5 then move_step := 5;
            player_x := player_x-move_step;
            if player_x < player_w div 2 then player_x := player_w div 2

         end

      end

      else if er.etype = etright then begin

         active_ctl := ctl_keyboard;
         if game_started and (not game_over) then begin

            move_step := scr_w div 50;
            if move_step < 5 then move_step := 5;
            player_x := player_x+move_step;
            if player_x > scr_w-player_w div 2 then
               player_x := scr_w-player_w div 2

         end

      end

      else if er.etype = etjoymov then begin

         { only take over if joystick moved significantly }
         jdelta := er.joypx-last_joyx;
         if jdelta < 0 then jdelta := -jdelta;
         if jdelta > joy_threshold then active_ctl := ctl_joystick;
         last_joyx := er.joypx;
         if active_ctl = ctl_joystick then
            if game_started and (not game_over) then begin

            jchr := i32max div ((scr_w-2) div 2);
            player_x := scr_w div 2+er.joypx div jchr;
            if player_x < player_w div 2 then player_x := player_w div 2;
            if player_x > scr_w-player_w div 2 then
               player_x := scr_w-player_w div 2

         end

      end

      else if er.etype = etjoyba then begin

         if er.ajoybn = 1 then
            if game_started and (not game_over) then player_fire

      end

      else if er.etype = etframe then begin

         if game_started and (not game_over) then begin

            update_aliens;
            update_pbullets;
            update_abullets;
            update_ufo;
            check_wave_complete

         end;
         { double buffer: select offscreen, draw, flip }
         select(output, (1-flip)+1, flip+1);
         draw_all;
         flip := 1-flip

      end

      else if er.etype = ettim then begin

         if er.timnum = timer_afire then begin

            if game_started and (not game_over) then begin

               alien_fire;
               { restart alien fire timer with random interval }
               timer(output, timer_afire,
                     afire_min+randn(afire_max-afire_min-1), false)

            end

         end else if er.timnum = timer_ufo then
            if game_started and (not game_over) then begin

            spawn_ufo;
            timer(output, timer_ufo, ufo_interval, false)

         end

      end

      else if er.etype = etmenus then begin

         if er.menuid = menu_new then begin

            init_game;
            timer(output, timer_afire,
                  afire_min+randn(afire_max-afire_min-1), false);
            timer(output, timer_ufo, ufo_interval, false);
            draw_all

         end else if er.menuid = menu_exit then goto 99

         else if er.menuid = menu_about then begin

            show_about;
            draw_all

         end

      end

   until er.etype = etterm;

   99: ;

   closesynthout(synth_out)

end.
