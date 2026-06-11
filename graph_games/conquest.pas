{******************************************************************************
*                                                                             *
*                              CONQUEST GAME                                  *
*                                                                             *
*                       COPYRIGHT (C) 2026 S. A. FRANCO                       *
*                                                                             *
* A territory conquest strategy game with real world geography. Players take  *
* turns deploying armies, attacking adjacent territories with dice combat,    *
* and fortifying positions in pursuit of world domination.                    *
*                                                                             *
* This is the Pascaline equivalent of the Ami graph_games/conquest.c          *
* reference game.                                                             *
*                                                                             *
******************************************************************************}

program conquest(input, output);

uses graphics,
     sound,
     services,
     strings;

label 99; { terminate }

const

   second   = 10000; { one second in timer units }

   { timer IDs }
   timerai   = 1; { AI thinking delay }
   timerdice = 2; { dice display timer }
   timermsg  = 3; { message display timer }

   { timer intervals }
   aidelay   = 3000;  { ~300ms AI delay }
   dicedelay = 10000; { 1s dice display }
   msgdelay  = 15000; { 1.5s message display }

   { menu IDs }
   menunew   = 100;
   menuexit  = 101;
   menuabout = 102;
   menu2p    = 110;
   menu3p    = 111;
   menu4p    = 112;
   menu5p    = 113;
   menup1h   = 120;
   menup1c   = 121;
   menup2h   = 122;
   menup2c   = 123;
   menup3h   = 124;
   menup3c   = 125;
   menup4h   = 126;
   menup4c   = 127;
   menup5h   = 128;
   menup5c   = 129;

   { sound defines }
   dicenote    = note_e+octave_5;
   conquernote = note_c+octave_6;
   deploynote  = note_g+octave_5;
   losenote    = note_c+octave_3;
   winnote     = note_c+octave_7;
   dicedur     = 150;
   conquerdur  = 400;
   deploydur   = 100;
   losedur     = 800;
   windur      = 1000;

   i32max = 2147483647; { 32 bit INT_MAX, base for RGB ratios }
   ftsign = 3;          { AMI_FONT_SIGN }

   { territory count }
   numterr = 44;

   { max adjacencies per territory }
   maxadj = 8;

   { continents }
   contnamerica  = 0;
   contsamerica  = 1;
   conteurope    = 2;
   contafrica    = 3;
   contmideast   = 4;
   contasia      = 5;
   contoceania   = 6;
   numcontinents = 7;

   { max players }
   maxplayers = 5;

   { game phases }
   phasedeploy  = 0;
   phaseattack  = 1;
   phasefortify = 2;

   { game states }
   statesetup    = 0; { pre-game }
   stateplaying  = 1;
   stategameover = 2;

   { max dice }
   maxattdice = 3;
   maxdefdice = 2;

   { picture id for the map }
   picmap = 1;

   { territory indices }
   talaska      = 0;
   twcanada     = 1;
   tecanada     = 2;
   tpaccoast    = 3;
   tmtnwest     = 4;
   tcentralus   = 5;
   tsouthernus  = 6;
   tnortheastus = 7;
   tmexico      = 8;
   tcenamerica  = 9;
   tcolven      = 10;
   tbraziln     = 11;
   tbrazils     = 12;
   tperubol     = 13;
   targchile    = 14;
   tbritire     = 15;
   tscandinavia = 16;
   tweurope     = 17;
   tceneurope   = 18;
   tseurope     = 19;
   teeurope     = 20;
   tukrbalk     = 21;
   tnafrica     = 22;
   twafrica     = 23;
   teafrica     = 24;
   tcenafrica   = 25;
   tsafrica     = 26;
   tmadagascar  = 27;
   tturkeycauc  = 28;
   tmiddleeast  = 29;
   tarabpen     = 30;
   tcentralasia = 31;
   trussiaw     = 32;
   trussiacen   = 33;
   trussiae     = 34;
   tchinan      = 35;
   tchinas      = 36;
   tindia       = 37;
   tseasia      = 38;
   tkoreajapan  = 39;
   twaustralia  = 40;
   teaustralia  = 41;
   tnewzealand  = 42;
   tindopapua   = 43;

type

   territory = record

      name:      pstring; { territory name }
      continent: integer; { continent index }
      owner:     integer; { player index or -1 }
      armies:    integer; { armies on territory }
      nadj:      integer; { number of adjacencies }
      adj:       array [1..maxadj] of integer; { adjacent territories }
      mx, my:    real     { marker position, normalized 0.0-1.0 }

   end;

   playerrec = record

      active:      boolean; { is this player in the game? }
      human:       boolean; { true = human, false = computer }
      territories: integer; { count of owned territories }
      cards:       integer; { simplified: count of cards held }
      eliminated:  boolean  { true if knocked out }

   end;

   continentrec = record

      name:  pstring; { continent name }
      bonus: integer; { continent bonus armies }
      start: integer; { first territory index }
      count: integer  { number of territories }

   end;

   intarr  = array [0..numterr-1] of integer; { territory index work array }
   dicearr = array [0..maxattdice-1] of integer; { dice values }

var

   { static game data }
   terrs:        array [0..numterr-1] of territory;
   conts:        array [0..numcontinents-1] of continentrec;
   playercolors: array [0..maxplayers-1, 1..3] of integer;

   { screen metrics }
   scrw, scrh:               integer;
   mapx, mapy, mapw, maph:   integer; { map area }
   statush:                  integer; { status bar height }

   { game state }
   players:    array [0..maxplayers-1] of playerrec;
   numplayers: integer;
   curplayer:  integer;
   phase:      integer;
   gamestate:  integer;
   flip:       integer;

   { selection state }
   selsource: integer; { selected source territory }
   seltarget: integer; { selected target territory }
   hoverterr: integer; { territory under mouse }

   { dice results }
   attdice:    dicearr;
   defdice:    dicearr;
   numattdice: integer;
   numdefdice: integer;
   showdice:   boolean; { true while dice are displayed }
   attwins:    integer; { results of last battle }
   defwins:    integer;

   { card trade tracking }
   cardsetstraded:    integer; { global count of sets traded }
   conqueredthisturn: boolean; { did current player conquer a territory? }

   { message display }
   msg:     pstring; { persistent message text }
   showmsg: boolean;

   { mouse tracking }
   mousex, mousey: integer;

   { "Done" button area }
   donebx, doneby, donebw, donebh: integer;

   { AI timer pending }
   aipending: boolean;

   { reinforcements tracking for deploy phase }
   deployremaining: integer;

   { random number generator state }
   randstate: integer;

   { newline character for the about box (control chars cannot appear in
     const strings) }
   nls: packed array [1..1] of char;

   er: evtrec;  { event record }
   i:  integer; { main work index }
   ap: pstring; { about message build pointer }

{ ************************************************************************

   Helper routines

   ************************************************************************ }

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

{ color helper: scale 0-255 to 0-INT_MAX (C CLR macro) }

function clr(v: integer): integer;

begin

   clr := v*(i32max div 255)

end;

{ write a string container through a view binding (write cannot take an
  unbounded container directly) }

procedure wrstr(view s: string);

begin

   write(s)

end;

{ write a string centered on the given x position }

procedure writecen(cx, ypos: integer; view s: string);

begin

   cursorg(output, cx-strsiz(output, s) div 2, ypos);
   write(s)

end;

{ write an integer centered on the given x position }

procedure writeintcen(cx, ypos, v: integer);

var p: pstring;

begin

   openstring;
   p := ints(v);
   cursorg(output, cx-strsiz(output, p^) div 2, ypos);
   wrstr(p^);
   closestring

end;

{ set the persistent message from a (possibly recycled) dynamic string }

procedure setmsg(p: pstring);

var i: integer;

begin

   if msg <> nil then dispose(msg);
   new(msg, max(p^));
   for i := 1 to max(p^) do msg^[i] := p^[i];
   showmsg := true

end;

{ set message "Player N<tail>" }

procedure msgplayer(pl: integer; view tail: string);

var tp: pstring;

begin

   openstring;
   tp := cat('Player ', ints(pl+1));
   tp := cat(tp, tail);
   setmsg(tp);
   closestring

end;

{ set message "Player N: Deploy M<tail>" }

procedure msgdeploy(pl, n: integer; view tail: string);

var tp: pstring;

begin

   openstring;
   tp := cat('Player ', ints(pl+1));
   tp := cat(tp, ': Deploy ');
   tp := cat(tp, ints(n));
   tp := cat(tp, tail);
   setmsg(tp);
   closestring

end;

{ play a note of the given duration on the synthesizer }

procedure playnote(n, dur: integer);

begin

   noteon(synth_out, 0, 1, n, i32max);
   noteoff(synth_out, curtimeout+dur, 1, n, i32max)

end;

{ ************************************************************************

   Static territory initialization

   ************************************************************************ }

{ set up a single territory entry. Unused adjacencies are passed as -1. }

procedure setterr(ti: integer; view nm: string; cont: integer;
                  a1, a2, a3, a4, a5, a6, a7, a8: integer);

var n: integer;

begin

   terrs[ti].name := copy(nm);
   terrs[ti].continent := cont;
   terrs[ti].owner := -1;
   terrs[ti].armies := 0;
   terrs[ti].adj[1] := a1;
   terrs[ti].adj[2] := a2;
   terrs[ti].adj[3] := a3;
   terrs[ti].adj[4] := a4;
   terrs[ti].adj[5] := a5;
   terrs[ti].adj[6] := a6;
   terrs[ti].adj[7] := a7;
   terrs[ti].adj[8] := a8;
   n := 0;
   if a1 >= 0 then n := n+1;
   if a2 >= 0 then n := n+1;
   if a3 >= 0 then n := n+1;
   if a4 >= 0 then n := n+1;
   if a5 >= 0 then n := n+1;
   if a6 >= 0 then n := n+1;
   if a7 >= 0 then n := n+1;
   if a8 >= 0 then n := n+1;
   terrs[ti].nadj := n;
   terrs[ti].mx := 0.0;
   terrs[ti].my := 0.0

end;

procedure initterritories;

begin

   { North America (0-9) }
   setterr(talaska,      'Alaska',      contnamerica, twcanada, tpaccoast,
           trussiae, -1, -1, -1, -1, -1);
   setterr(twcanada,     'W Canada',    contnamerica, talaska, tecanada,
           tpaccoast, tmtnwest, -1, -1, -1, -1);
   setterr(tecanada,     'E Canada',    contnamerica, twcanada, tmtnwest,
           tcentralus, tnortheastus, -1, -1, -1, -1);
   setterr(tpaccoast,    'Pac Coast',   contnamerica, talaska, twcanada,
           tmtnwest, tmexico, -1, -1, -1, -1);
   setterr(tmtnwest,     'Mtn West',    contnamerica, twcanada, tecanada,
           tpaccoast, tcentralus, tmexico, -1, -1, -1);
   setterr(tcentralus,   'Central US',  contnamerica, tecanada, tmtnwest,
           tnortheastus, tsouthernus, tmexico, -1, -1, -1);
   setterr(tsouthernus,  'Southern US', contnamerica, tcentralus,
           tnortheastus, tmexico, tcenamerica, -1, -1, -1, -1);
   setterr(tnortheastus, 'NE US',       contnamerica, tecanada, tcentralus,
           tsouthernus, -1, -1, -1, -1, -1);
   setterr(tmexico,      'Mexico',      contnamerica, tpaccoast, tmtnwest,
           tcentralus, tsouthernus, tcenamerica, -1, -1, -1);
   setterr(tcenamerica,  'C America',   contnamerica, tmexico, tsouthernus,
           tcolven, -1, -1, -1, -1, -1);

   { South America (10-14) }
   setterr(tcolven,   'Col/Ven',   contsamerica, tcenamerica, tbraziln,
           tperubol, -1, -1, -1, -1, -1);
   setterr(tbraziln,  'Brazil N',  contsamerica, tcolven, tbrazils,
           tperubol, tnafrica, -1, -1, -1, -1);
   setterr(tbrazils,  'Brazil S',  contsamerica, tbraziln, tperubol,
           targchile, -1, -1, -1, -1, -1);
   setterr(tperubol,  'Peru/Bol',  contsamerica, tcolven, tbraziln,
           tbrazils, targchile, -1, -1, -1, -1);
   setterr(targchile, 'Arg/Chile', contsamerica, tbrazils, tperubol,
           -1, -1, -1, -1, -1, -1);

   { Europe (15-21) }
   setterr(tbritire,     'Brit/Ire',    conteurope, tscandinavia, tweurope,
           tceneurope, -1, -1, -1, -1, -1);
   setterr(tscandinavia, 'Scandinavia', conteurope, tbritire, tceneurope,
           trussiaw, -1, -1, -1, -1, -1);
   setterr(tweurope,     'W Europe',    conteurope, tbritire, tceneurope,
           tseurope, tnafrica, -1, -1, -1, -1);
   setterr(tceneurope,   'Cen Europe',  conteurope, tbritire, tscandinavia,
           tweurope, tseurope, teeurope, tukrbalk, -1, -1);
   setterr(tseurope,     'S Europe',    conteurope, tweurope, tceneurope,
           tukrbalk, tnafrica, tturkeycauc, -1, -1, -1);
   setterr(teeurope,     'E Europe',    conteurope, tceneurope, tukrbalk,
           trussiaw, tturkeycauc, -1, -1, -1, -1);
   setterr(tukrbalk,     'Ukr/Balk',    conteurope, tceneurope, tseurope,
           teeurope, tturkeycauc, tmiddleeast, -1, -1, -1);

   { Africa (22-27) }
   setterr(tnafrica,    'N Africa',   contafrica, tbraziln, tweurope,
           tseurope, twafrica, teafrica, -1, -1, -1);
   setterr(twafrica,    'W Africa',   contafrica, tnafrica, teafrica,
           tcenafrica, tsafrica, -1, -1, -1, -1);
   setterr(teafrica,    'E Africa',   contafrica, tnafrica, twafrica,
           tcenafrica, tarabpen, tmadagascar, -1, -1, -1);
   setterr(tcenafrica,  'Cen Africa', contafrica, twafrica, teafrica,
           tsafrica, tmadagascar, -1, -1, -1, -1);
   setterr(tsafrica,    'S Africa',   contafrica, twafrica, tcenafrica,
           tmadagascar, -1, -1, -1, -1, -1);
   setterr(tmadagascar, 'Madagascar', contafrica, teafrica, tcenafrica,
           tsafrica, -1, -1, -1, -1, -1);

   { Middle East & Central Asia (28-31) }
   setterr(tturkeycauc,  'Turkey',   contmideast, tseurope, teeurope,
           tukrbalk, tmiddleeast, tcentralasia, -1, -1, -1);
   setterr(tmiddleeast,  'Mid East', contmideast, tukrbalk, tturkeycauc,
           tarabpen, tcentralasia, tindia, -1, -1, -1);
   setterr(tarabpen,     'Arab Pen', contmideast, teafrica, tmiddleeast,
           tindia, -1, -1, -1, -1, -1);
   setterr(tcentralasia, 'Cen Asia', contmideast, tturkeycauc, tmiddleeast,
           trussiaw, trussiacen, tchinan, -1, -1, -1);

   { Asia (32-39) }
   setterr(trussiaw,    'Russia W',  contasia, tscandinavia, teeurope,
           trussiacen, tcentralasia, -1, -1, -1, -1);
   setterr(trussiacen,  'Russia C',  contasia, trussiaw, trussiae,
           tcentralasia, tchinan, -1, -1, -1, -1);
   setterr(trussiae,    'Russia E',  contasia, talaska, trussiacen,
           tchinan, tkoreajapan, -1, -1, -1, -1);
   setterr(tchinan,     'China N',   contasia, tcentralasia, trussiacen,
           trussiae, tchinas, tkoreajapan, tindia, -1, -1);
   setterr(tchinas,     'China S',   contasia, tchinan, tindia,
           tseasia, tkoreajapan, -1, -1, -1, -1);
   setterr(tindia,      'India',     contasia, tmiddleeast, tarabpen,
           tchinan, tchinas, tseasia, -1, -1, -1);
   setterr(tseasia,     'SE Asia',   contasia, tchinas, tindia,
           tindopapua, tkoreajapan, -1, -1, -1, -1);
   setterr(tkoreajapan, 'Korea/Jpn', contasia, trussiae, tchinan,
           tchinas, tseasia, -1, -1, -1, -1);

   { Oceania (40-43) }
   setterr(twaustralia, 'W Australia', contoceania, teaustralia,
           tindopapua, tnewzealand, -1, -1, -1, -1, -1);
   setterr(teaustralia, 'E Australia', contoceania, twaustralia,
           tindopapua, tnewzealand, -1, -1, -1, -1, -1);
   setterr(tnewzealand, 'New Zealand', contoceania, twaustralia,
           teaustralia, -1, -1, -1, -1, -1, -1);
   setterr(tindopapua,  'Indo/Papua',  contoceania, tseasia, twaustralia,
           teaustralia, -1, -1, -1, -1, -1)

end;

{ set up a single continent entry }

procedure setcont(ci: integer; view nm: string; bonus, start, cnt: integer);

begin

   conts[ci].name := copy(nm);
   conts[ci].bonus := bonus;
   conts[ci].start := start;
   conts[ci].count := cnt

end;

procedure initcontinents;

begin

   setcont(contnamerica, 'N America', 5, 0,  10);
   setcont(contsamerica, 'S America', 2, 10,  5);
   setcont(conteurope,   'Europe',    5, 15,  7);
   setcont(contafrica,   'Africa',    3, 22,  6);
   setcont(contmideast,  'Mid East',  3, 28,  4);
   setcont(contasia,     'Asia',      7, 32,  8);
   setcont(contoceania,  'Oceania',   2, 40,  4)

end;

{ set up player colors }

procedure initplayercolors;

begin

   playercolors[0, 1] := 220; playercolors[0, 2] :=  50; playercolors[0, 3] :=  50; { red }
   playercolors[1, 1] :=  50; playercolors[1, 2] := 100; playercolors[1, 3] := 220; { blue }
   playercolors[2, 1] :=  50; playercolors[2, 2] := 180; playercolors[2, 3] :=  50; { green }
   playercolors[3, 1] := 220; playercolors[3, 2] := 200; playercolors[3, 3] :=  50; { yellow }
   playercolors[4, 1] := 160; playercolors[4, 2] :=  50; playercolors[4, 3] := 200  { purple }

end;

{ ************************************************************************

   Menu setup

   ************************************************************************ }

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

procedure setupmenu;

var m:        menuptr;
    gamemenu: menuptr;
    plyrmenu: menuptr;
    helpmenu: menuptr;

begin

   m := nil;

   { Game menu }
   gamemenu := newmenuitem(false, false, false, 0, 'Game');
   appendmenu(m, gamemenu);
   appendmenu(gamemenu^.branch,
      newmenuitem(false, false, true, menunew, 'New Game'));
   appendmenu(gamemenu^.branch,
      newmenuitem(false, false, false, menuexit, 'Exit'));

   { Players menu }
   plyrmenu := newmenuitem(false, false, false, 0, 'Players');
   appendmenu(m, plyrmenu);
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, true, true, menu2p, '2 Players'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, true, false, menu3p, '3 Players'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, true, false, menu4p, '4 Players'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, true, false, menu5p, '5 Players'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, true, menup1h, 'P1: Human'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup1c, 'P1: Computer'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup2h, 'P2: Human'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup2c, 'P2: Computer'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup3h, 'P3: Human'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup3c, 'P3: Computer'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup4h, 'P4: Human'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup4c, 'P4: Computer'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup5h, 'P5: Human'));
   appendmenu(plyrmenu^.branch,
      newmenuitem(false, false, false, menup5c, 'P5: Computer'));

   { Help menu }
   helpmenu := newmenuitem(false, false, false, 0, 'Help');
   appendmenu(m, helpmenu);
   appendmenu(helpmenu^.branch,
      newmenuitem(false, false, false, menuabout, 'About Conquest'));

   menu(output, m)

end;

{ ************************************************************************

   Calculate screen metrics

   ************************************************************************ }

procedure calcmetrics;

begin

   scrw := maxxg(output);
   scrh := maxyg(output);
   statush := scrh div 12;
   mapx := scrw div 40;
   mapy := scrh div 40;
   mapw := scrw-mapx*2;
   maph := scrh-statush-mapy*2;

   { Done button in status bar }
   donebw := scrw div 8;
   donebh := statush*2 div 3;
   donebx := scrw-donebw-scrw div 40;
   doneby := scrh-statush+(statush-donebh) div 2

end;

{ ************************************************************************

   Initialize territory marker positions

   ************************************************************************ }

procedure setmarker(ti: integer; x, y: real);

begin

   terrs[ti].mx := x;
   terrs[ti].my := y

end;

procedure initmarkers;

begin

   { North America }
   setmarker(talaska,      0.055, 0.11);
   setmarker(twcanada,     0.12,  0.16);
   setmarker(tecanada,     0.22,  0.13);
   setmarker(tpaccoast,    0.09,  0.27);
   setmarker(tmtnwest,     0.13,  0.27);
   setmarker(tcentralus,   0.17,  0.27);
   setmarker(tsouthernus,  0.17,  0.35);
   setmarker(tnortheastus, 0.21,  0.27);
   setmarker(tmexico,      0.11,  0.42);
   setmarker(tcenamerica,  0.13,  0.49);

   { South America }
   setmarker(tcolven,      0.22,  0.55);
   setmarker(tbraziln,     0.27,  0.58);
   setmarker(tbrazils,     0.26,  0.68);
   setmarker(tperubol,     0.21,  0.68);
   setmarker(targchile,    0.22,  0.78);

   { Europe }
   setmarker(tbritire,     0.35,  0.22);
   setmarker(tscandinavia, 0.39,  0.12);
   setmarker(tweurope,     0.37,  0.32);
   setmarker(tceneurope,   0.39,  0.27);
   setmarker(tseurope,     0.39,  0.35);
   setmarker(teeurope,     0.42,  0.24);
   setmarker(tukrbalk,     0.44,  0.29);

   { Africa }
   setmarker(tnafrica,     0.43,  0.47);
   setmarker(twafrica,     0.42,  0.55);
   setmarker(teafrica,     0.49,  0.55);
   setmarker(tcenafrica,   0.47,  0.62);
   setmarker(tsafrica,     0.47,  0.72);
   setmarker(tmadagascar,  0.52,  0.68);

   { Middle East & Central Asia }
   setmarker(tturkeycauc,  0.45,  0.37);
   setmarker(tmiddleeast,  0.48,  0.42);
   setmarker(tarabpen,     0.49,  0.48);
   setmarker(tcentralasia, 0.52,  0.32);

   { Asia }
   setmarker(trussiaw,     0.48,  0.16);
   setmarker(trussiacen,   0.57,  0.12);
   setmarker(trussiae,     0.69,  0.11);
   setmarker(tchinan,      0.66,  0.32);
   setmarker(tchinas,      0.67,  0.42);
   setmarker(tindia,       0.58,  0.45);
   setmarker(tseasia,      0.65,  0.48);
   setmarker(tkoreajapan,  0.72,  0.28);

   { Oceania }
   setmarker(twaustralia,  0.78,  0.78);
   setmarker(teaustralia,  0.82,  0.78);
   setmarker(tnewzealand,  0.88,  0.82);
   setmarker(tindopapua,   0.74,  0.58)

end;

{ ************************************************************************

   Territory marker screen position and hit testing

   ************************************************************************ }

{ get marker screen position }

procedure terrmarkerpos(ti: integer; var px, py: integer);

begin

   px := mapx+trunc(terrs[ti].mx*mapw);
   py := mapy+trunc(terrs[ti].my*maph)

end;

{ marker hit radius in pixels }

function markerradius: integer;

begin

   markerradius := mapw div 60

end;

{ check if point is within marker radius of territory }

function pointinterr(px, py, ti: integer): boolean;

var cx, cy, dx, dy, r: integer;

begin

   terrmarkerpos(ti, cx, cy);
   r := markerradius;
   dx := px-cx;
   dy := py-cy;
   pointinterr := (dx*dx+dy*dy) <= (r*r)

end;

function findterrat(px, py: integer): integer;

var i, cx, cy, dx, dy, r, d: integer;
    best:     integer;
    bestdist: integer;

begin

   best := -1;
   bestdist := maxint;
   r := markerradius;
   for i := 0 to numterr-1 do begin

      terrmarkerpos(i, cx, cy);
      dx := px-cx;
      dy := py-cy;
      d := dx*dx+dy*dy;
      if d <= r*r then
         if d < bestdist then begin

            bestdist := d;
            best := i

         end

   end;
   findterrat := best

end;

{ ************************************************************************

   Adjacency check

   ************************************************************************ }

function areadjacent(a, b: integer): boolean;

var i: integer;
    r: boolean;

begin

   r := false;
   for i := 1 to terrs[a].nadj do
      if terrs[a].adj[i] = b then r := true;
   areadjacent := r

end;

{ ************************************************************************

   Connected territory check (for fortify - BFS)

   ************************************************************************ }

function areconnected(a, b, owner: integer): boolean;

var visited: array [0..numterr-1] of boolean;
    queue:   intarr;
    head, tail, cur, i, nb: integer;
    found:   boolean;

begin

   if a = b then found := true
   else begin

      found := false;
      for i := 0 to numterr-1 do visited[i] := false;
      head := 0;
      tail := 0;
      queue[tail] := a;
      tail := tail+1;
      visited[a] := true;

      while (head < tail) and not found do begin

         cur := queue[head];
         head := head+1;
         for i := 1 to terrs[cur].nadj do begin

            nb := terrs[cur].adj[i];
            if not visited[nb] then
               if terrs[nb].owner = owner then begin

                  if nb = b then found := true;
                  visited[nb] := true;
                  queue[tail] := nb;
                  tail := tail+1

               end

         end

      end

   end;
   areconnected := found

end;

{ ************************************************************************

   Player territory/army counting

   ************************************************************************ }

function countterritories(p: integer): integer;

var i, n: integer;

begin

   n := 0;
   for i := 0 to numterr-1 do
      if terrs[i].owner = p then n := n+1;
   countterritories := n

end;

function counttotalarmies(p: integer): integer;

var i, n: integer;

begin

   n := 0;
   for i := 0 to numterr-1 do
      if terrs[i].owner = p then n := n+terrs[i].armies;
   counttotalarmies := n

end;

function ownscontinent(p, c: integer): boolean;

var i:    integer;
    owns: boolean;

begin

   owns := true;
   for i := conts[c].start to conts[c].start+conts[c].count-1 do
      if terrs[i].owner <> p then owns := false;
   ownscontinent := owns

end;

function calcreinforcements(p: integer): integer;

var base, bonus, c: integer;

begin

   base := countterritories(p) div 3;
   if base < 3 then base := 3;

   bonus := 0;
   for c := 0 to numcontinents-1 do
      if ownscontinent(p, c) then bonus := bonus+conts[c].bonus;

   calcreinforcements := base+bonus

end;

{ ************************************************************************

   Card trading

   ************************************************************************ }

function cardtradevalue: integer;

begin

   { increasing value: 4, 6, 8, 10, 12, 15, 20, 25, ... }
   if cardsetstraded < 5 then cardtradevalue := 4+cardsetstraded*2
   else cardtradevalue := 5+cardsetstraded*5

end;

{ ************************************************************************

   Dice combat

   ************************************************************************ }

function rolldie: integer;

begin

   rolldie := randn(5)+1

end;

procedure sortdicedesc(var dice: dicearr; n: integer);

var i, j, tmp: integer;

begin

   for i := 0 to n-2 do
      for j := i+1 to n-1 do
         if dice[j] > dice[i] then begin

            tmp := dice[i];
            dice[i] := dice[j];
            dice[j] := tmp

         end

end;

procedure resolvecombat(attt, deft: integer);

var attarmy, defarmy, cmpn, i: integer;

begin

   attarmy := terrs[attt].armies-1;
   defarmy := terrs[deft].armies;

   { determine dice count }
   numattdice := attarmy;
   if numattdice > maxattdice then numattdice := maxattdice;
   numdefdice := defarmy;
   if numdefdice > maxdefdice then numdefdice := maxdefdice;

   { roll }
   for i := 0 to numattdice-1 do attdice[i] := rolldie;
   for i := 0 to numdefdice-1 do defdice[i] := rolldie;

   sortdicedesc(attdice, numattdice);
   sortdicedesc(defdice, numdefdice);

   { compare }
   if numattdice < numdefdice then cmpn := numattdice
   else cmpn := numdefdice;
   attwins := 0;
   defwins := 0;
   for i := 0 to cmpn-1 do
      if attdice[i] > defdice[i] then attwins := attwins+1
      else defwins := defwins+1;

   terrs[attt].armies := terrs[attt].armies-defwins;
   terrs[deft].armies := terrs[deft].armies-attwins

end;

{ ************************************************************************

   Check for player elimination and game over

   ************************************************************************ }

procedure checkeliminations;

var i, alivecount, winner: integer;

begin

   alivecount := 0;
   winner := -1;
   for i := 0 to numplayers-1 do
      if players[i].active then begin

         players[i].territories := countterritories(i);
         if players[i].territories = 0 then players[i].eliminated := true;
         if not players[i].eliminated then begin

            alivecount := alivecount+1;
            winner := i

         end

      end;

   if alivecount = 1 then
      if gamestate = stateplaying then begin

         gamestate := stategameover;
         msgplayer(winner, ' wins!')

      end

end;

{ ************************************************************************

   Initialize new game - random territory distribution

   ************************************************************************ }

procedure initgame;

var i, j, p, tmp:        integer;
    initarmies:          integer;
    placed, remaining:   integer;
    nc:                  integer;
    order, choices:      intarr;

begin

   gamestate := stateplaying;
   cardsetstraded := 0;
   showdice := false;
   showmsg := false;
   selsource := -1;
   seltarget := -1;
   aipending := false;

   { reset players }
   for i := 0 to maxplayers-1 do begin

      players[i].active := i < numplayers;
      players[i].territories := 0;
      players[i].cards := 0;
      players[i].eliminated := false

   end;

   { create random order of territories }
   for i := 0 to numterr-1 do order[i] := i;
   for i := numterr-1 downto 1 do begin

      j := randn(i);
      tmp := order[i];
      order[i] := order[j];
      order[j] := tmp

   end;

   { distribute territories round-robin }
   for i := 0 to numterr-1 do begin

      p := i mod numplayers;
      terrs[order[i]].owner := p;
      terrs[order[i]].armies := 1

   end;

   { distribute remaining initial armies (30 per player for 3p, scaled) }
   if numplayers = 2 then initarmies := 40
   else if numplayers = 3 then initarmies := 35
   else if numplayers = 4 then initarmies := 30
   else initarmies := 25;

   for p := 0 to numplayers-1 do begin

      placed := countterritories(p);
      remaining := initarmies-placed;
      for i := 1 to remaining do begin

         { place on random owned territory }
         nc := 0;
         for j := 0 to numterr-1 do
            if terrs[j].owner = p then begin

               choices[nc] := j;
               nc := nc+1

            end;
         if nc > 0 then begin

            j := choices[randn(nc-1)];
            terrs[j].armies := terrs[j].armies+1

         end

      end;
      players[p].territories := countterritories(p)

   end;

   curplayer := 0;
   phase := phasedeploy;
   conqueredthisturn := false;

   { calculate initial reinforcements - store in msg }
   msgdeploy(curplayer, calcreinforcements(curplayer), ' armies')

end;

{ ************************************************************************

   Reinforcements tracking for deploy phase

   ************************************************************************ }

procedure startdeployphase;

var bonus: integer;

begin

   phase := phasedeploy;
   deployremaining := calcreinforcements(curplayer);

   { auto-trade cards if player has 3+ }
   while (players[curplayer].cards >= 3) and (deployremaining < 50) do begin

      bonus := cardtradevalue;
      players[curplayer].cards := players[curplayer].cards-3;
      deployremaining := deployremaining+bonus;
      cardsetstraded := cardsetstraded+1

   end;

   msgdeploy(curplayer, deployremaining, ' armies')

end;

{ ************************************************************************

   Advance to next player

   ************************************************************************ }

procedure nextplayer;

var start: integer;

begin

   { award card if conquered territory this turn }
   if conqueredthisturn then
      players[curplayer].cards := players[curplayer].cards+1;

   start := curplayer;
   repeat
      curplayer := (curplayer+1) mod numplayers
   until (not players[curplayer].eliminated) or (curplayer = start);

   conqueredthisturn := false;
   selsource := -1;
   seltarget := -1;

   if gamestate = stateplaying then startdeployphase

end;

{ ************************************************************************

   Advance phase

   ************************************************************************ }

procedure advancephase;

begin

   if phase = phasedeploy then begin

      if deployremaining <= 0 then begin { must place all armies }

         phase := phaseattack;
         selsource := -1;
         seltarget := -1;
         msgplayer(curplayer, ': Attack or Done')

      end

   end else if phase = phaseattack then begin

      phase := phasefortify;
      selsource := -1;
      seltarget := -1;
      msgplayer(curplayer, ': Fortify or Done')

   end else
      nextplayer

end;

{ ************************************************************************

   Handle territory click during deploy phase

   ************************************************************************ }

procedure handledeployclick(ti: integer);

begin

   if ti >= 0 then
      if terrs[ti].owner = curplayer then
         if deployremaining > 0 then begin

            terrs[ti].armies := terrs[ti].armies+1;
            deployremaining := deployremaining-1;

            playnote(deploynote, deploydur);

            if deployremaining <= 0 then
               msgplayer(curplayer, ': Deploy done. Press Done to attack.')
            else
               msgdeploy(curplayer, deployremaining, ' remaining')

         end

end;

{ ************************************************************************

   Handle territory click during attack phase

   ************************************************************************ }

procedure handleattackclick(ti: integer);

var oldowner: integer;
    tp:       pstring;

begin

   if ti < 0 then begin

      selsource := -1;
      seltarget := -1

   end else if terrs[ti].owner = curplayer then begin

      { clicking own territory: select as source }
      if terrs[ti].armies > 1 then begin

         selsource := ti;
         seltarget := -1

      end

   end else if selsource >= 0 then
      { clicking enemy territory: attack if source selected and adjacent }
      if areadjacent(selsource, ti) then begin

         seltarget := ti;

         { play dice sound }
         playnote(dicenote, dicedur);

         { resolve combat }
         oldowner := terrs[ti].owner;
         resolvecombat(selsource, ti);
         showdice := true;

         { check if territory conquered }
         if terrs[ti].armies <= 0 then begin

            { conquer! }
            terrs[ti].owner := curplayer;
            { move armies in }
            terrs[ti].armies := terrs[selsource].armies-1;
            if terrs[ti].armies < 1 then terrs[ti].armies := 1;
            terrs[selsource].armies := 1;
            conqueredthisturn := true;

            playnote(conquernote, conquerdur);

            openstring;
            tp := cat('Player ', ints(curplayer+1));
            tp := cat(tp, ' conquered ');
            tp := cat(tp, terrs[ti].name);
            tp := cat(tp, '!');
            setmsg(tp);
            closestring;

            checkeliminations

         end else begin

            openstring;
            tp := cat('Battle: lost ', ints(defwins));
            tp := cat(tp, ', killed ');
            tp := cat(tp, ints(attwins));
            setmsg(tp);
            closestring

         end;

         { deselect if source can't attack anymore }
         if terrs[selsource].armies <= 1 then selsource := -1;
         seltarget := -1

      end

end;

{ ************************************************************************

   Handle territory click during fortify phase

   ************************************************************************ }

procedure handlefortifyclick(ti: integer);

var domove: boolean;
    tp:     pstring;

begin

   if ti < 0 then selsource := -1
   else if terrs[ti].owner = curplayer then begin

      if selsource < 0 then begin

         { select source }
         if terrs[ti].armies > 1 then selsource := ti

      end else begin

         { select target - must be connected through own territories }
         domove := false;
         if ti <> selsource then
            if areconnected(selsource, ti, curplayer) then domove := true;
         if domove then begin

            { move one army per click }
            if terrs[selsource].armies > 1 then begin

               terrs[selsource].armies := terrs[selsource].armies-1;
               terrs[ti].armies := terrs[ti].armies+1;
               openstring;
               tp := cat('Fortified ', terrs[ti].name);
               tp := cat(tp, ' (+1)');
               setmsg(tp);
               closestring;
               if terrs[selsource].armies <= 1 then selsource := -1

            end

         end else begin

            { clicked different own territory, reselect }
            if terrs[ti].armies > 1 then selsource := ti
            else selsource := -1

         end

      end

   end

end;

{ ************************************************************************

   Handle territory click (dispatch by phase)

   ************************************************************************ }

procedure handleclick(px, py: integer);

var ti: integer;

begin

   if gamestate = stateplaying then
      if players[curplayer].human then begin

         { check Done button }
         if (px >= donebx) and (px <= donebx+donebw) and
            (py >= doneby) and (py <= doneby+donebh) then
            advancephase
         else begin

            ti := findterrat(px, py);

            if phase = phasedeploy then handledeployclick(ti)
            else if phase = phaseattack then handleattackclick(ti)
            else handlefortifyclick(ti)

         end

      end

end;

{ ************************************************************************

   AI: Simple greedy computer player

   ************************************************************************ }

{ find border territories (own territory adjacent to enemy) }

function aifindborder(p: integer; var borders: intarr;
                      mx: integer): integer;

var i, j, n:  integer;
    isborder: boolean;

begin

   n := 0;
   for i := 0 to numterr-1 do
      if n < mx then
         if terrs[i].owner = p then begin

            isborder := false;
            for j := 1 to terrs[i].nadj do
               if terrs[terrs[i].adj[j]].owner <> p then isborder := true;
            if isborder then begin

               borders[n] := i;
               n := n+1

            end

         end;
   aifindborder := n

end;

procedure aideploy(p: integer);

var borders:    intarr;
    nb, i:      integer;
    target:     integer;

begin

   nb := aifindborder(p, borders, numterr);
   if nb = 0 then begin

      { no borders? just pick first owned }
      i := 0;
      while (i < numterr) and (nb = 0) do begin

         if terrs[i].owner = p then begin

            nb := 1;
            borders[0] := i

         end;
         i := i+1

      end

   end;

   while deployremaining > 0 do begin

      { deploy to random border territory }
      target := borders[randn(nb-1)];
      terrs[target].armies := terrs[target].armies+1;
      deployremaining := deployremaining-1

   end

end;

procedure aiattack(p: integer);

var attacked:           boolean;
    attempts, i, j, nb: integer;
    bestsrc, besttgt:   integer;
    bestratio, ratio:   integer;

begin

   attempts := 0;
   repeat

      attacked := false;
      bestsrc := -1;
      besttgt := -1;
      bestratio := 0;

      for i := 0 to numterr-1 do
         if terrs[i].owner = p then
            if terrs[i].armies > 1 then
               for j := 1 to terrs[i].nadj do begin

                  nb := terrs[i].adj[j];
                  if terrs[nb].owner <> p then begin

                     ratio := terrs[i].armies*100 div (terrs[nb].armies+1);
                     { add randomness }
                     ratio := ratio+randn(39)-20;

                     if ratio > bestratio then begin

                        bestratio := ratio;
                        bestsrc := i;
                        besttgt := nb

                     end

                  end

               end;

      { only attack if we have good odds (at least 1.5:1 ratio) }
      if bestsrc >= 0 then
         if bestratio >= 140 then begin

            selsource := bestsrc;
            resolvecombat(bestsrc, besttgt);

            if terrs[besttgt].armies <= 0 then begin

               terrs[besttgt].owner := p;
               terrs[besttgt].armies := terrs[bestsrc].armies-1;
               if terrs[besttgt].armies < 1 then terrs[besttgt].armies := 1;
               terrs[bestsrc].armies := 1;
               conqueredthisturn := true;
               checkeliminations

            end;
            attacked := true

         end;
      attempts := attempts+1

   until (not attacked) or (attempts >= 20) or
         (gamestate = stategameover);

   if gamestate <> stategameover then selsource := -1

end;

procedure aifortify(p: integer);

var borders:  intarr;
    nb, i, j: integer;
    target:   integer;
    mv:       integer;
    interior: boolean;
    donemove: boolean;

begin

   nb := aifindborder(p, borders, numterr);
   if nb > 0 then begin

      { find interior territories with armies and move to border }
      i := 0;
      donemove := false;
      while (i < numterr) and not donemove do begin

         if terrs[i].owner = p then
            if terrs[i].armies > 1 then begin

               { check if this is an interior territory }
               interior := true;
               for j := 1 to terrs[i].nadj do
                  if terrs[terrs[i].adj[j]].owner <> p then
                     interior := false;

               if interior then begin

                  { move armies to a random connected border territory }
                  target := borders[randn(nb-1)];
                  if areconnected(i, target, p) then begin

                     mv := terrs[i].armies-1;
                     terrs[i].armies := 1;
                     terrs[target].armies := terrs[target].armies+mv;
                     donemove := true { only one fortify move }

                  end

               end

            end;
         i := i+1

      end

   end

end;

procedure aidoturn(p: integer);

begin

   startdeployphase;
   aideploy(p);
   aiattack(p);
   if gamestate <> stategameover then aifortify(p)
   { conqueredthisturn flag is checked by nextplayer for card award }

end;

{ ************************************************************************

   Drawing functions

   ************************************************************************ }

procedure setplayercolor(p: integer);

begin

   if (p < 0) or (p >= maxplayers) then
      fcolorg(output, clr(150), clr(150), clr(150)) { gray }
   else
      fcolorg(output, clr(playercolors[p, 1]), clr(playercolors[p, 2]),
              clr(playercolors[p, 3]))

end;

procedure clearscreen;

begin

   fcolorg(output, clr(20), clr(30), clr(50));
   frect(output, 1, 1, scrw, scrh)

end;

procedure loadmap;

begin

   loadpict(output, picmap, 'graph_games/conquest_map.bmp')

end;

procedure drawocean;

begin

   { draw world map background image scaled to map area }
   picture(output, picmap, mapx, mapy, mapx+mapw, mapy+maph)

end;

procedure drawterritory(ti: integer);

var cx, cy, r, fsz: integer;
    isvalidtarget:  boolean;

begin

   terrmarkerpos(ti, cx, cy);
   r := markerradius;

   { check if this territory is a valid attack/fortify target }
   isvalidtarget := false;
   if selsource >= 0 then
      if ti <> selsource then begin

         if phase = phaseattack then begin

            if terrs[ti].owner <> curplayer then
               if areadjacent(selsource, ti) then isvalidtarget := true

         end else if phase = phasefortify then begin

            if terrs[ti].owner = curplayer then
               if areconnected(selsource, ti, curplayer) then
                  isvalidtarget := true

         end

      end;

   { filled circle in player color }
   setplayercolor(terrs[ti].owner);
   fellipse(output, cx-r, cy-r, cx+r, cy+r);

   { outline }
   if ti = selsource then begin

      { selected source: thick white outline }
      fcolorg(output, clr(255), clr(255), clr(255));
      linewidth(output, 3);
      ellipse(output, cx-r, cy-r, cx+r, cy+r);
      linewidth(output, 1)

   end else if isvalidtarget then begin

      { valid target: yellow outline }
      fcolorg(output, clr(255), clr(255), clr(0));
      linewidth(output, 2);
      ellipse(output, cx-r, cy-r, cx+r, cy+r);
      linewidth(output, 1)

   end else begin

      { default: thin black outline }
      fcolorg(output, clr(0), clr(0), clr(0));
      ellipse(output, cx-r, cy-r, cx+r, cy+r)

   end;

   { army count centered in circle }
   fsz := r;
   if fsz < 6 then fsz := 6;
   if fsz > 20 then fsz := 20;
   fontsiz(output, fsz);

   { black text for light player colors (yellow), white for dark }
   if terrs[ti].owner = 3 then
      fcolorg(output, clr(0), clr(0), clr(0))
   else
      fcolorg(output, clr(255), clr(255), clr(255));

   writeintcen(cx, cy+fsz div 3, terrs[ti].armies)

end;

procedure drawadjacencylines;

var i, j, nb:           integer;
    cxa, cya, cxb, cyb: integer;
    skip:               boolean;

begin

   fcolorg(output, clr(80), clr(80), clr(100));
   linewidth(output, 1);

   for i := 0 to numterr-1 do begin

      terrmarkerpos(i, cxa, cya);

      for j := 1 to terrs[i].nadj do begin

         nb := terrs[i].adj[j];
         if nb > i then begin { draw each line only once }

            skip := false;
            { skip Bering Strait line (wraps around) }
            if (i = talaska) and (nb = trussiae) then skip := true;
            { skip cross-Atlantic (Brazil N <-> N Africa) }
            if (i = tbraziln) and (nb = tnafrica) then skip := true;

            if not skip then begin

               terrmarkerpos(nb, cxb, cyb);
               line(output, cxa, cya, cxb, cyb)

            end

         end

      end

   end

end;

procedure drawstatusbar;

var bary, fsz, sw, i, bx: integer;
    candone:              boolean;
    sp:                   pstring;

begin

   bary := scrh-statush;

   { background }
   fcolorg(output, clr(30), clr(30), clr(40));
   frect(output, 1, bary, scrw, scrh);

   { separator line }
   fcolorg(output, clr(100), clr(100), clr(120));
   line(output, 1, bary, scrw, bary);

   if gamestate = statesetup then begin

      fsz := statush div 3;
      if fsz < 8 then fsz := 8;
      fontsiz(output, fsz);
      fcolorg(output, clr(200), clr(200), clr(200));
      cursorg(output, scrw div 20, bary+statush div 2+fsz div 4);
      write('Select Game -> New Game to start')

   end else if gamestate = stategameover then begin

      fsz := statush div 3;
      if fsz < 8 then fsz := 8;
      fontsiz(output, fsz);
      setplayercolor(curplayer);
      cursorg(output, scrw div 20, bary+statush div 2+fsz div 4);
      if msg <> nil then wrstr(msg^)

   end else begin

      { player indicator blocks }
      fsz := statush div 4;
      if fsz < 6 then fsz := 6;
      fontsiz(output, fsz);
      bx := scrw div 60;
      for i := 0 to numplayers-1 do
         if not players[i].eliminated then begin

            setplayercolor(i);
            frect(output, bx, bary+statush div 6,
                  bx+statush div 3, bary+statush div 2);
            if i = curplayer then begin

               fcolorg(output, clr(255), clr(255), clr(255));
               rect(output, bx, bary+statush div 6,
                    bx+statush div 3, bary+statush div 2)

            end;
            fcolorg(output, clr(200), clr(200), clr(200));
            cursorg(output, bx+statush div 3+4, bary+statush div 2-2);
            write(countterritories(i):1);
            bx := bx+scrw div 12

         end;

      { current info }
      fsz := statush div 3;
      if fsz < 8 then fsz := 8;
      fontsiz(output, fsz);

      openstring;
      sp := cat('P', ints(curplayer+1));
      sp := cat(sp, ' [');
      if phase = phasedeploy then sp := cat(sp, 'DEPLOY')
      else if phase = phaseattack then sp := cat(sp, 'ATTACK')
      else sp := cat(sp, 'FORTIFY');
      sp := cat(sp, ']');
      if showmsg then
         if msg <> nil then begin

            sp := cat(sp, ' ');
            sp := cat(sp, msg)

         end;

      setplayercolor(curplayer);
      sw := strsiz(output, sp^);
      cursorg(output, scrw div 2-sw div 2, bary+statush*4 div 5);
      wrstr(sp^);
      closestring;

      { draw "Done" button }
      if players[curplayer].human then begin

         candone := (phase <> phasedeploy) or (deployremaining <= 0);
         if candone then
            fcolorg(output, clr(60), clr(120), clr(60))
         else
            fcolorg(output, clr(60), clr(60), clr(60));
         frect(output, donebx, doneby, donebx+donebw, doneby+donebh);
         fcolorg(output, clr(200), clr(200), clr(200));
         rect(output, donebx, doneby, donebx+donebw, doneby+donebh);

         fsz := donebh div 2;
         if fsz < 6 then fsz := 6;
         fontsiz(output, fsz);
         fcolorg(output, clr(255), clr(255), clr(255));
         writecen(donebx+donebw div 2, doneby+donebh div 2+fsz div 4, 'Done')

      end

   end

end;

procedure drawdicedisplay;

var cx, cy, bw, bh, dsz, i, dx, fsz: integer;
    p: pstring;

begin

   if showdice then begin

      cx := scrw div 2;
      cy := scrh div 2-statush;
      bw := scrw div 5;
      bh := scrh div 8;
      dsz := bh div 2;

      { background panel }
      fcolorg(output, clr(0), clr(0), clr(0));
      frect(output, cx-bw, cy-bh, cx+bw, cy+bh);
      fcolorg(output, clr(180), clr(180), clr(180));
      rect(output, cx-bw, cy-bh, cx+bw, cy+bh);

      fsz := dsz*2 div 3;
      if fsz < 8 then fsz := 8;
      fontsiz(output, fsz);

      { attacker dice (red) }
      dx := cx-bw+dsz;
      for i := 0 to numattdice-1 do begin

         fcolorg(output, clr(200), clr(50), clr(50));
         frect(output, dx, cy-dsz div 2, dx+dsz, cy+dsz div 2);
         fcolorg(output, clr(255), clr(255), clr(255));
         writeintcen(dx+dsz div 2, cy+fsz div 4, attdice[i]);
         dx := dx+dsz+dsz div 3

      end;

      { defender dice (blue) }
      dx := cx+dsz div 2;
      for i := 0 to numdefdice-1 do begin

         fcolorg(output, clr(50), clr(100), clr(200));
         frect(output, dx, cy-dsz div 2, dx+dsz, cy+dsz div 2);
         fcolorg(output, clr(255), clr(255), clr(255));
         writeintcen(dx+dsz div 2, cy+fsz div 4, defdice[i]);
         dx := dx+dsz+dsz div 3

      end;

      { "vs" text }
      fcolorg(output, clr(200), clr(200), clr(200));
      writecen(cx, cy-bh+fsz+4, 'vs');

      { result text }
      openstring;
      p := cat('-', ints(defwins));
      p := cat(p, ' / -');
      p := cat(p, ints(attwins));
      cursorg(output, cx-strsiz(output, p^) div 2, cy+bh-4);
      wrstr(p^);
      closestring

   end

end;

procedure drawcontinentlabels;

var c, fsz, sw: integer;
    cx, cy:     integer;
    clx, cly:   array [0..numcontinents-1] of real;

begin

   { approximate center positions for continent labels (normalized coords) }
   clx[0] := 0.15; cly[0] := 0.08;
   clx[1] := 0.24; cly[1] := 0.85;
   clx[2] := 0.40; cly[2] := 0.08;
   clx[3] := 0.46; cly[3] := 0.75;
   clx[4] := 0.50; cly[4] := 0.38;
   clx[5] := 0.65; cly[5] := 0.05;
   clx[6] := 0.82; cly[6] := 0.90;

   fsz := maph div 50;
   if fsz < 4 then fsz := 4;
   fontsiz(output, fsz);
   fcolorg(output, clr(200), clr(200), clr(130));

   for c := 0 to numcontinents-1 do begin

      cx := mapx+trunc(clx[c]*mapw);
      cy := mapy+trunc(cly[c]*maph);
      sw := strsiz(output, conts[c].name^);
      cursorg(output, cx-sw div 2, cy);
      wrstr(conts[c].name^);
      write('(+', conts[c].bonus:1, ')')

   end

end;

{ draw a stick figure (human) icon centered at cx, cy, size s }

procedure drawhumanicon(cx, cy, s: integer);

var hr: integer; { head radius }

begin

   hr := s div 5;

   { head }
   fellipse(output, cx-hr, cy-s div 2, cx+hr, cy-s div 2+2*hr);
   { body }
   line(output, cx, cy-s div 2+2*hr, cx, cy+s div 6);
   { arms }
   line(output, cx-s div 3, cy-s div 6, cx+s div 3, cy-s div 6);
   { legs }
   line(output, cx, cy+s div 6, cx-s div 4, cy+s div 2);
   line(output, cx, cy+s div 6, cx+s div 4, cy+s div 2)

end;

{ draw a robot icon centered at cx, cy, size s, player color p }

procedure drawroboticon(cx, cy, s, p: integer);

var bw, bh, hw, hh: integer;

begin

   bw := s div 2; { body width }
   bh := s div 3; { body height }
   hw := s div 3; { head width }
   hh := s div 4; { head height }

   { antenna }
   line(output, cx, cy-s div 2, cx, cy-s div 2+s div 8);
   fellipse(output, cx-s div 12, cy-s div 2-s div 12,
            cx+s div 12, cy-s div 2+s div 12);
   { head }
   frect(output, cx-hw div 2, cy-s div 2+s div 8,
         cx+hw div 2, cy-s div 2+s div 8+hh);
   { eyes }
   fcolorg(output, clr(255), clr(50), clr(50));
   fellipse(output, cx-hw div 4-s div 16, cy-s div 2+s div 8+hh div 4,
            cx-hw div 4+s div 16, cy-s div 2+s div 8+hh*3 div 4);
   fellipse(output, cx+hw div 4-s div 16, cy-s div 2+s div 8+hh div 4,
            cx+hw div 4+s div 16, cy-s div 2+s div 8+hh*3 div 4);
   { body }
   setplayercolor(p);
   frect(output, cx-bw div 2, cy-s div 8, cx+bw div 2, cy-s div 8+bh);
   { legs }
   frect(output, cx-bw div 3, cy-s div 8+bh,
         cx-bw div 3+s div 8, cy+s div 2);
   frect(output, cx+bw div 3-s div 8, cy-s div 8+bh,
         cx+bw div 3, cy+s div 2)

end;

{ draw player setup icons showing human/computer status }

procedure drawplayericons;

var i, cx, cy:            integer;
    iconsz, gap:          integer;
    totalw, startx:       integer;
    fsz:                  integer;
    basey:                integer;
    lp:                   pstring;

begin

   iconsz := scrh div 8;
   if iconsz < 30 then iconsz := 30;
   gap := scrw div (maxplayers+1);
   totalw := numplayers*gap;
   startx := (scrw-totalw) div 2+gap div 2;

   { position below the subtitle }
   basey := scrh div 2+scrh div 12;

   fsz := scrh div 40;
   if fsz < 8 then fsz := 8;
   fontsiz(output, fsz);

   for i := 0 to numplayers-1 do begin

      cx := startx+i*gap;
      cy := basey;

      { draw player color background circle }
      setplayercolor(i);
      linewidth(output, 2);

      if players[i].human then drawhumanicon(cx, cy, iconsz)
      else drawroboticon(cx, cy, iconsz, i);
      linewidth(output, 1);

      { label }
      openstring;
      lp := cat('P', ints(i+1));
      if players[i].human then lp := cat(lp, ' Human')
      else lp := cat(lp, ' CPU');
      setplayercolor(i);
      writecen(cx, cy+iconsz div 2+fsz, lp^);
      closestring

   end

end;

procedure drawtitlescreen;

var fsz: integer;

begin

   fsz := scrh div 12;
   if fsz < 16 then fsz := 16;
   fontsiz(output, fsz);
   fcolorg(output, clr(220), clr(180), clr(50));
   writecen(scrw div 2, scrh div 4, 'CONQUEST');

   fsz := scrh div 30;
   if fsz < 8 then fsz := 8;
   fontsiz(output, fsz);
   fcolorg(output, clr(180), clr(180), clr(180));
   writecen(scrw div 2, scrh div 4+scrh div 10,
            'A game of world domination');

   { draw player setup icons }
   drawplayericons;

   fsz := scrh div 35;
   if fsz < 8 then fsz := 8;
   fontsiz(output, fsz);
   fcolorg(output, clr(140), clr(140), clr(140));
   writecen(scrw div 2, scrh*3 div 4,
            'Use Players menu to configure, then Game -> New Game')

end;

procedure drawall;

var i: integer;

begin

   calcmetrics;
   clearscreen;

   if gamestate = statesetup then begin

      drawtitlescreen;
      drawstatusbar

   end else begin

      drawocean;
      drawadjacencylines;
      drawcontinentlabels;

      for i := 0 to numterr-1 do drawterritory(i);

      drawdicedisplay;
      drawstatusbar

   end

end;

{ ************************************************************************

   Main program

   ************************************************************************ }

begin

   { seed the random generator }
   randstate := clock mod i32max;
   if randstate <= 0 then randstate := 1;

   { initialize game state }
   msg := nil;
   showmsg := false;
   showdice := false;
   gamestate := statesetup;
   curplayer := 0;
   phase := phasedeploy;
   selsource := -1;
   seltarget := -1;
   hoverterr := -1;
   cardsetstraded := 0;
   conqueredthisturn := false;
   aipending := false;
   deployremaining := 0;
   nls[1] := chr(10); { newline for the about box }

   initterritories;
   initcontinents;
   initplayercolors;

   title(output, 'Conquest');
   curvis(output, 0);
   auto(output, 0);
   { stay in buffered mode for select double buffering }
   font(output, ftsign);
   bold(output, 1);
   binvis(output);
   bcolorg(output, 0, 0, 0);
   frametimer(output, 1);
   flip := 0;

   opensynthout(synth_out);
   instchange(synth_out, 0, 1, inst_acoustic_grand);
   starttimeout;

   setupmenu;
   loadmap;
   initmarkers;

   { default: 3 players, P1 human, rest computer }
   numplayers := 3;
   for i := 0 to maxplayers-1 do begin

      players[i].active := false;
      players[i].human := false;
      players[i].territories := 0;
      players[i].cards := 0;
      players[i].eliminated := false

   end;
   players[0].human := true;

   calcmetrics;
   mousex := scrw div 2;
   mousey := scrh div 2;
   drawall;

   repeat

      event(input, er);

      if er.etype = etresize then begin

         sizbufg(output, er.rszxg, er.rszyg);
         calcmetrics;
         drawall;
         select(output, (1-flip)+1, flip+1);
         flip := 1-flip

      end else if er.etype = etredraw then drawall

      else if er.etype = etmoumovg then begin

         mousex := er.moupxg;
         mousey := er.moupyg;
         hoverterr := findterrat(mousex, mousey)

      end else if er.etype = etmouba then begin

         if er.amoubn = 1 then handleclick(mousex, mousey)

      end else if er.etype = etenter then begin

         { Enter key = Done }
         if gamestate = stateplaying then
            if players[curplayer].human then advancephase

      end else if er.etype = etframe then begin

         { handle AI turns during frame events - one per frame }
         if gamestate = stateplaying then
            if not players[curplayer].human then
               if not players[curplayer].eliminated then begin

                  aidoturn(curplayer);
                  if gamestate <> stategameover then nextplayer

               end;

         { double buffer: select offscreen, draw, flip }
         select(output, (1-flip)+1, flip+1);
         drawall;
         flip := 1-flip

      end else if er.etype = etmenus then begin

         if er.menuid = menunew then begin

            initgame;
            drawall

         end else if er.menuid = menuexit then goto 99
         else if er.menuid = menu2p then numplayers := 2
         else if er.menuid = menu3p then numplayers := 3
         else if er.menuid = menu4p then numplayers := 4
         else if er.menuid = menu5p then numplayers := 5
         else if er.menuid = menup1h then players[0].human := true
         else if er.menuid = menup1c then players[0].human := false
         else if er.menuid = menup2h then players[1].human := true
         else if er.menuid = menup2c then players[1].human := false
         else if er.menuid = menup3h then players[2].human := true
         else if er.menuid = menup3c then players[2].human := false
         else if er.menuid = menup4h then players[3].human := true
         else if er.menuid = menup4c then players[3].human := false
         else if er.menuid = menup5h then players[4].human := true
         else if er.menuid = menup5c then players[4].human := false
         else if er.menuid = menuabout then begin

            openstring;
            ap := cat('Conquest - A territory strategy game', nls);
            ap := cat(ap, 'Click territories to deploy, attack, fortify');
            ap := cat(ap, nls);
            ap := cat(ap, 'Copyright (C) 2026 S. A. Franco');
            alert('About Conquest', ap^);
            closestring;
            drawall

         end

      end

   until er.etype = etterm;

   99: { terminate }

   closesynthout(synth_out)

end.
