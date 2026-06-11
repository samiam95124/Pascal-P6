{******************************************************************************
*                                                                             *
*                         GRAPHICS MODEL LIBRARY                              *
*                                                                             *
*                           Copyright (C) 2024                                *
*                                                                             *
*                               S. A. FRANCO                                  *
*                                                                             *
* Defines the Pascaline interface to the Ami graphics module. Graphics        *
* is a superset of the terminal model: it provides the full character surface *
* of terminal plus a graphical surface (lines, rectangles, ellipses, arcs,    *
* fonts, pictures), windowing (frames, sizing, menus, openwin) and widgets    *
* (buttons, checkboxes, edit boxes, list boxes, sliders, tab bars). A terminal *
* program relinks against graphics unchanged.                                 *
*                                                                             *
* This is the Pascaline binding. The implementation lives in the C Ami        *
* graphics module (graphics.c), reached through graphics_wrapper.             *
*                                                                             *
******************************************************************************}

module graphics;

const

   maxtim = 10; { maximum number of timers available }

type

   { colors displayable in text mode }
   color = (black, white, red, green, blue, cyan, yellow, magenta, backcolor);

   { line styles for graphical drawing }
   lstyle = (lssolid, lsdash, lsdot);

   { tab bar orientation }
   tabori = (totop, toright, tobottom, toleft);

   { events }
   evtcod = (etchar,    { ANSI character returned }
             etup,      { cursor up one line }
             etdown,    { down one line }
             etleft,    { left one character }
             etright,   { right one character }
             etleftw,   { left one word }
             etrightw,  { right one word }
             ethome,    { home of document }
             ethomes,   { home of screen }
             ethomel,   { home of line }
             etend,     { end of document }
             etends,    { end of screen }
             etendl,    { end of line }
             etscrl,    { scroll left one character }
             etscrr,    { scroll right one character }
             etscru,    { scroll up one line }
             etscrd,    { scroll down one line }
             etpagd,    { page down }
             etpagu,    { page up }
             ettab,     { tab }
             etenter,   { enter line }
             etinsert,  { insert block }
             etinsertl, { insert line }
             etinsertt, { insert toggle }
             etdel,     { delete block }
             etdell,    { delete line }
             etdelcf,   { delete character forward }
             etdelcb,   { delete character backward }
             etcopy,    { copy block }
             etcopyl,   { copy line }
             etcan,     { cancel current operation }
             etstop,    { stop current operation }
             etcont,    { continue current operation }
             etprint,   { print document }
             etprintb,  { print block }
             etprints,  { print screen }
             etfun,     { function key }
             etmenu,    { display menu }
             etmouba,   { mouse button assertion }
             etmoubd,   { mouse button deassertion }
             etmoumov,  { mouse move }
             ettim,     { timer matures }
             etjoyba,   { joystick button assertion }
             etjoybd,   { joystick button deassertion }
             etjoymov,  { joystick move }
             etresize,  { window was resized }
             etfocus,   { window has focus }
             etnofocus, { window lost focus }
             ethover,   { window being hovered }
             etnohover, { window stopped being hovered }
             etterm,    { terminate program }
             etframe,   { frame sync }
             etmoumovg, { mouse move, graphical coordinates }
             etredraw,  { window redraw }
             etmin,     { window minimized }
             etmax,     { window maximized }
             etnorm,    { window normalized }
             etmenus,   { menu item selected }
             etbutton,  { button assert }
             etchkbox,  { checkbox click }
             etradbut,  { radio button click }
             etsclull,  { scroll up/left line }
             etscldrl,  { scroll down/right line }
             etsclulp,  { scroll up/left page }
             etscldrp,  { scroll down/right page }
             etsclpos,  { scroll bar position }
             etedtbox,  { edit box complete }
             etnumbox,  { number select box complete }
             etlstbox,  { list box select }
             etdrpbox,  { drop box select }
             etdrebox,  { drop edit box select }
             etsldpos,  { slider position }
             ettabbar,  { tab bar select }
             etsys,     { reserved: system }
             etman,     { reserved: management }
             etwidget,  { reserved: widget }
             etuser);   { reserved: user }

   { event record }
   evtptr = ^evtrec;
   evtrec = record

      winid:   integer; { identifier of window for event }
      handled: boolean; { event was handled }
      case etype: evtcod of { event type }

         { events that carry parameter data }
         etchar:   (echar: char);                  { ANSI character returned }
         ettim:    (timnum: integer);              { timer handle that matured }
         etmoumov: (mmoun, moupx, moupy: integer); { mouse number and movement }
         etmouba:  (amoun, amoubn: integer);       { mouse and button asserted }
         etmoubd:  (dmoun, dmoubn: integer);       { mouse and button deasserted }
         etjoyba:  (ajoyn, ajoybn: integer);       { joystick and button asserted }
         etjoybd:  (djoyn, djoybn: integer);       { joystick and button deasserted }
         etjoymov: (mjoyn, joypx, joypy, joypz,    { joystick and coordinates }
                    joyp4, joyp5, joyp6: integer);
         etfun:    (fkey: integer);                { function key number }
         etresize: (rszx, rszy, rszxg, rszyg: integer); { new size, char and graph }
         etmoumovg:(mmoung, moupxg, moupyg: integer);   { mouse move, graphical }
         etredraw: (rsx, rsy, rex, rey: integer);       { redraw region }
         etmenus:  (menuid: integer);              { menu item selected }
         etbutton: (butid: integer);               { button id }
         etchkbox: (ckbxid: integer);              { checkbox id }
         etradbut: (radbid: integer);              { radio button id }
         etsclull: (sclulid: integer);             { scroll up/left line id }
         etscldrl: (scldrid: integer);             { scroll down/right line id }
         etsclulp: (sclupid: integer);             { scroll up/left page id }
         etscldrp: (scldpid: integer);             { scroll down/right page id }
         etsclpos: (sclpid, sclpos: integer);      { scroll bar id and position }
         etedtbox: (edtbid: integer);              { edit box id }
         etnumbox: (numbid, numbsl: integer);      { number box id and value }
         etlstbox: (lstbid, lstbsl: integer);      { list box id and selection }
         etdrpbox: (drpbid, drpbsl: integer);      { drop box id and selection }
         etdrebox: (drebid: integer);              { drop edit box id }
         etsldpos: (sldpid, sldpos: integer);      { slider id and position }
         ettabbar: (tabid, tabsel: integer);       { tab bar id and selection }

         { events without parameter data carry no fields }
         etup, etdown, etleft, etright, etleftw, etrightw, ethome, ethomes,
         ethomel, etend, etends, etendl, etscrl, etscrr, etscru, etscrd, etpagd,
         etpagu, ettab, etenter, etinsert, etinsertl, etinsertt, etdel, etdell,
         etdelcf, etdelcb, etcopy, etcopyl, etcan, etstop, etcont, etprint,
         etprintb, etprints, etmenu, etfocus, etnofocus, ethover, etnohover,
         etterm, etframe, etmin, etmax, etnorm, etsys, etman, etwidget,
         etuser: ()

   end;

   { window mode set, used to select window components }
   winmod = (wmframe,   { frame on/off }
             wmsize,    { size bars on/off }
             wmsysbar); { system bar on/off }
   winmodset = set of winmod;

   { menu }
   menuptr = ^menurec;
   menurec = record

      next:   menuptr; { next menu item in list }
      branch: menuptr; { menu branch }
      onoff:  boolean; { on/off highlight }
      oneof:  boolean; { "one of" highlight }
      bar:    boolean; { place bar under }
      id:     integer; { id of menu item }
      face:   pstring  { text to place on button }

   end;

   { string list (for list boxes, drop boxes, etc.) }
   strptr = ^strrec;
   strrec = record

      next: strptr;  { next entry in list }
      str:  pstring  { string }

   end;

   { standard menu selector }
   stdmenusel = integer;

   { query find/replace and font effect option sets }
   qfnopt    = (qfncase, qfnup, qfnre);
   qfnopts   = set of qfnopt;
   qfropt    = (qfrcase, qfrup, qfrre, qfrfind, qfrallfil, qfralllin);
   qfropts   = set of qfropt;
   qfteffect = (qfteblink, qftereverse, qfteunderline, qftesuperscript,
                qftesubscript, qfteitalic, qftebold, qftestrikeout,
                qftestandout, qftecondensed, qfteextended, qftexlight,
                qftelight, qftexbold, qftehollow, qfteraised);
   qfteffects = set of qfteffect;

{
 * Routines at this level. Each surface routine has an explicit file form
 * (var f: text) and a default form that operates on the standard output
 * (or, for event, the standard input).
}
function points(var f: text): real; external;
overload function points: real; external;
function baseline(var f: text): integer; external;
overload function baseline: integer; external;
function chrpos(var f: text; view s: string; p: integer): integer; external;
overload function chrpos(view s: string; p: integer): integer; external;
function chrsizx(var f: text): integer; external;
overload function chrsizx: integer; external;
function chrsizy(var f: text): integer; external;
overload function chrsizy: integer; external;
function curbnd(var f: text): integer; external;
overload function curbnd: integer; external;
function curx(var f: text): integer; external;
overload function curx: integer; external;
function curxg(var f: text): integer; external;
overload function curxg: integer; external;
function cury(var f: text): integer; external;
overload function cury: integer; external;
function curyg(var f: text): integer; external;
overload function curyg: integer; external;
function dpmx(var f: text): integer; external;
overload function dpmx: integer; external;
function dpmy(var f: text): integer; external;
overload function dpmy: integer; external;
function fonts(var f: text): integer; external;
overload function fonts: integer; external;
function funkey(var f: text): integer; external;
overload function funkey: integer; external;
function getwigid(var f: text): integer; external;
overload function getwigid: integer; external;
function getwinid: integer; external;
function joyaxis(var f: text; j: integer): integer; external;
overload function joyaxis(j: integer): integer; external;
function joybutton(var f: text; j: integer): integer; external;
overload function joybutton(j: integer): integer; external;
function joystick(var f: text): integer; external;
overload function joystick: integer; external;
function justpos(var f: text; view s: string; p: integer; n: integer): integer; external;
overload function justpos(view s: string; p: integer; n: integer): integer; external;
function maxx(var f: text): integer; external;
overload function maxx: integer; external;
function maxxg(var f: text): integer; external;
overload function maxxg: integer; external;
function maxy(var f: text): integer; external;
overload function maxy: integer; external;
function maxyg(var f: text): integer; external;
overload function maxyg: integer; external;
function mousebutton(var f: text; m: integer): integer; external;
overload function mousebutton(m: integer): integer; external;
function mouse(var f: text): integer; external;
overload function mouse: integer; external;
function pictsizx(var f: text; p: integer): integer; external;
overload function pictsizx(p: integer): integer; external;
function pictsizy(var f: text; p: integer): integer; external;
overload function pictsizy(p: integer): integer; external;
function scalex(var f: text; x: integer): integer; external;
overload function scalex(x: integer): integer; external;
function scaley(var f: text; y: integer): integer; external;
overload function scaley(y: integer): integer; external;
function strsiz(var f: text; view s: string): integer; external;
overload function strsiz(view s: string): integer; external;
procedure alert(view title: string; view message: string); external;
procedure arc(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sa: integer; ea: integer); external;
overload procedure arc(x1: integer; y1: integer; x2: integer; y2: integer; sa: integer; ea: integer); external;
procedure auto(var f: text; e: integer); external;
overload procedure auto(e: integer); external;
procedure autohold(e: integer); external;
procedure back(var f: text); external;
overload procedure back; external;
procedure background(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure background(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure backgroundg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure backgroundg(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure backwidget(var f: text; id: integer); external;
overload procedure backwidget(id: integer); external;
procedure band(var f: text); external;
overload procedure band; external;
procedure bcolorc(var f: text; r: integer; g: integer; b: integer); external;
overload procedure bcolorc(r: integer; g: integer; b: integer); external;
procedure bcolor(var f: text; c: color); external;
overload procedure bcolor(c: color); external;
procedure bcolorg(var f: text; r: integer; g: integer; b: integer); external;
overload procedure bcolorg(r: integer; g: integer; b: integer); external;
procedure binvis(var f: text); external;
overload procedure binvis; external;
procedure blink(var f: text; e: integer); external;
overload procedure blink(e: integer); external;
procedure bold(var f: text; e: integer); external;
overload procedure bold(e: integer); external;
procedure blor(var f: text); external;
overload procedure blor; external;
procedure bover(var f: text); external;
overload procedure bover; external;
procedure buffer(var f: text; e: integer); external;
overload procedure buffer(e: integer); external;
procedure button(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure button(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure buttong(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure buttong(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure buttonsiz(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure buttonsiz(view s: string; var w: integer; var h: integer); external;
procedure buttonsizg(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure buttonsizg(view s: string; var w: integer; var h: integer); external;
procedure bxor(var f: text); external;
overload procedure bxor; external;
procedure checkbox(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure checkbox(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure checkboxg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure checkboxg(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure checkboxsiz(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure checkboxsiz(view s: string; var w: integer; var h: integer); external;
procedure checkboxsizg(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure checkboxsizg(view s: string; var w: integer; var h: integer); external;
procedure chrspcx(var f: text; s: integer); external;
overload procedure chrspcx(s: integer); external;
procedure chrspcy(var f: text; s: integer); external;
overload procedure chrspcy(s: integer); external;
procedure clrtab(var f: text); external;
overload procedure clrtab; external;
procedure condensed(var f: text; e: integer); external;
overload procedure condensed(e: integer); external;
procedure cursor(var f: text; x: integer; y: integer); external;
overload procedure cursor(x: integer; y: integer); external;
procedure cursorg(var f: text; x: integer; y: integer); external;
overload procedure cursorg(x: integer; y: integer); external;
procedure curvis(var f: text; e: integer); external;
overload procedure curvis(e: integer); external;
procedure del(var f: text); external;
overload procedure del; external;
procedure delpict(var f: text; p: integer); external;
overload procedure delpict(p: integer); external;
procedure down(var f: text); external;
overload procedure down; external;
procedure dropbox(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
overload procedure dropbox(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
procedure dropboxg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
overload procedure dropboxg(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
procedure dropboxsiz(var f: text; sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
overload procedure dropboxsiz(sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
procedure dropboxsizg(var f: text; sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
overload procedure dropboxsizg(sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
procedure dropeditbox(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
overload procedure dropeditbox(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
procedure dropeditboxg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
overload procedure dropeditboxg(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
procedure dropeditboxsiz(var f: text; sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
overload procedure dropeditboxsiz(sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
procedure dropeditboxsizg(var f: text; sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
overload procedure dropeditboxsizg(sp: strptr; var cw: integer; var ch: integer; var ow: integer; var oh: integer); external;
procedure editbox(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure editbox(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure editboxg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure editboxg(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure editboxsiz(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure editboxsiz(view s: string; var w: integer; var h: integer); external;
procedure editboxsizg(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure editboxsizg(view s: string; var w: integer; var h: integer); external;
procedure ellipse(var f: text; x1: integer; y1: integer; x2: integer; y2: integer); external;
overload procedure ellipse(x1: integer; y1: integer; x2: integer; y2: integer); external;
procedure enablewidget(var f: text; id: integer; e: integer); external;
overload procedure enablewidget(id: integer; e: integer); external;
procedure event(var f: text; var er: evtrec); external;
overload procedure event(var er: evtrec); external;
procedure eventover(e: evtcod; procedure eh(var er: evtrec); out oeh: integer); external;
procedure eventsover(procedure eh(var er: evtrec); out oeh: integer); external;
procedure extended(var f: text; e: integer); external;
overload procedure extended(e: integer); external;
procedure fand(var f: text); external;
overload procedure fand; external;
procedure farc(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sa: integer; ea: integer); external;
overload procedure farc(x1: integer; y1: integer; x2: integer; y2: integer; sa: integer; ea: integer); external;
procedure fchord(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sa: integer; ea: integer); external;
overload procedure fchord(x1: integer; y1: integer; x2: integer; y2: integer; sa: integer; ea: integer); external;
procedure fcolorc(var f: text; r: integer; g: integer; b: integer); external;
overload procedure fcolorc(r: integer; g: integer; b: integer); external;
procedure fcolor(var f: text; c: color); external;
overload procedure fcolor(c: color); external;
procedure fcolorg(var f: text; r: integer; g: integer; b: integer); external;
overload procedure fcolorg(r: integer; g: integer; b: integer); external;
procedure fellipse(var f: text; x1: integer; y1: integer; x2: integer; y2: integer); external;
overload procedure fellipse(x1: integer; y1: integer; x2: integer; y2: integer); external;
procedure finvis(var f: text); external;
overload procedure finvis; external;
procedure focus(var f: text); external;
overload procedure focus; external;
procedure focuswidget(var f: text; id: integer); external;
overload procedure focuswidget(id: integer); external;
procedure font(var f: text; fc: integer); external;
overload procedure font(fc: integer); external;
procedure fontnam(var f: text; fc: integer; var fns: string); external;
overload procedure fontnam(fc: integer; var fns: string); external;
procedure fontsiz(var f: text; s: integer); external;
overload procedure fontsiz(s: integer); external;
procedure flor(var f: text); external;
overload procedure flor; external;
procedure fover(var f: text); external;
overload procedure fover; external;
procedure frame(var f: text; e: integer); external;
overload procedure frame(e: integer); external;
procedure frametimer(var f: text; e: integer); external;
overload procedure frametimer(e: integer); external;
procedure frect(var f: text; x1: integer; y1: integer; x2: integer; y2: integer); external;
overload procedure frect(x1: integer; y1: integer; x2: integer; y2: integer); external;
procedure front(var f: text); external;
overload procedure front; external;
procedure frontwidget(var f: text; id: integer); external;
overload procedure frontwidget(id: integer); external;
procedure frrect(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; xs: integer; ys: integer); external;
overload procedure frrect(x1: integer; y1: integer; x2: integer; y2: integer; xs: integer; ys: integer); external;
procedure ftriangle(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; x3: integer; y3: integer); external;
overload procedure ftriangle(x1: integer; y1: integer; x2: integer; y2: integer; x3: integer; y3: integer); external;
procedure fxor(var f: text); external;
overload procedure fxor; external;
procedure getsiz(var f: text; var x: integer; var y: integer); external;
overload procedure getsiz(var x: integer; var y: integer); external;
procedure getsizg(var f: text; var x: integer; var y: integer); external;
overload procedure getsizg(var x: integer; var y: integer); external;
procedure getwidgettext(var f: text; id: integer; var s: string); external;
overload procedure getwidgettext(id: integer; var s: string); external;
procedure group(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure group(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure groupg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure groupg(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure groupsiz(var f: text; view s: string; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
overload procedure groupsiz(view s: string; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
procedure groupsizg(var f: text; view s: string; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
overload procedure groupsizg(view s: string; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
procedure hollow(var f: text; e: integer); external;
overload procedure hollow(e: integer); external;
procedure home(var f: text); external;
overload procedure home; external;
procedure italic(var f: text; e: integer); external;
overload procedure italic(e: integer); external;
procedure killtimer(var f: text; i: integer); external;
overload procedure killtimer(i: integer); external;
procedure killwidget(var f: text; id: integer); external;
overload procedure killwidget(id: integer); external;
procedure left(var f: text); external;
overload procedure left; external;
procedure light(var f: text; e: integer); external;
overload procedure light(e: integer); external;
procedure line(var f: text; x1: integer; y1: integer; x2: integer; y2: integer); external;
overload procedure line(x1: integer; y1: integer; x2: integer; y2: integer); external;
procedure linestyle(var f: text; style: lstyle); external;
overload procedure linestyle(style: lstyle); external;
procedure linewidth(var f: text; w: integer); external;
overload procedure linewidth(w: integer); external;
procedure listbox(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
overload procedure listbox(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
procedure listboxg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
overload procedure listboxg(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; id: integer); external;
procedure listboxsiz(var f: text; sp: strptr; var w: integer; var h: integer); external;
overload procedure listboxsiz(sp: strptr; var w: integer; var h: integer); external;
procedure listboxsizg(var f: text; sp: strptr; var w: integer; var h: integer); external;
overload procedure listboxsizg(sp: strptr; var w: integer; var h: integer); external;
procedure loadpict(var f: text; p: integer; view fn: string); external;
overload procedure loadpict(p: integer; view fn: string); external;
procedure menuena(var f: text; id: integer; onoff: integer); external;
overload procedure menuena(id: integer; onoff: integer); external;
procedure menu(var f: text; m: menuptr); external;
overload procedure menu(m: menuptr); external;
procedure menusel(var f: text; id: integer; select: integer); external;
overload procedure menusel(id: integer; select: integer); external;
procedure numselbox(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; l: integer; u: integer; id: integer); external;
overload procedure numselbox(x1: integer; y1: integer; x2: integer; y2: integer; l: integer; u: integer; id: integer); external;
procedure numselboxg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; l: integer; u: integer; id: integer); external;
overload procedure numselboxg(x1: integer; y1: integer; x2: integer; y2: integer; l: integer; u: integer; id: integer); external;
procedure numselboxsiz(var f: text; l: integer; u: integer; var w: integer; var h: integer); external;
overload procedure numselboxsiz(l: integer; u: integer; var w: integer; var h: integer); external;
procedure numselboxsizg(var f: text; l: integer; u: integer; var w: integer; var h: integer); external;
overload procedure numselboxsizg(l: integer; u: integer; var w: integer; var h: integer); external;
procedure openwin(var infile: text; var outfile: text; var parent: text; wid: integer); external;
procedure path(var f: text; a: integer); external;
overload procedure path(a: integer); external;
procedure picture(var f: text; p: integer; x1: integer; y1: integer; x2: integer; y2: integer); external;
overload procedure picture(p: integer; x1: integer; y1: integer; x2: integer; y2: integer); external;
procedure poswidget(var f: text; id: integer; x: integer; y: integer); external;
overload procedure poswidget(id: integer; x: integer; y: integer); external;
procedure poswidgetg(var f: text; id: integer; x: integer; y: integer); external;
overload procedure poswidgetg(id: integer; x: integer; y: integer); external;
procedure progbar(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure progbar(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure progbarg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure progbarg(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure progbarpos(var f: text; id: integer; pos: integer); external;
overload procedure progbarpos(id: integer; pos: integer); external;
procedure progbarsiz(var f: text; var w: integer; var h: integer); external;
overload procedure progbarsiz(var w: integer; var h: integer); external;
procedure progbarsizg(var f: text; var w: integer; var h: integer); external;
overload procedure progbarsizg(var w: integer; var h: integer); external;
procedure putwidgettext(var f: text; id: integer; view s: string); external;
overload procedure putwidgettext(id: integer; view s: string); external;
procedure querycolor(var r: integer; var g: integer; var b: integer); external;
procedure queryfind(var s: string; var opt: qfnopts); external;
procedure queryfindrep(var s: string; var r: string; var opt: qfropts); external;
procedure queryfont(var f: text; var fc: integer; var s: integer; var fr: integer; var fg: integer; var fb: integer; var br: integer; var bg: integer; var bb: integer; var effect: qfteffects); external;
overload procedure queryfont(var fc: integer; var s: integer; var fr: integer; var fg: integer; var fb: integer; var br: integer; var bg: integer; var bb: integer; var effect: qfteffects); external;
procedure queryopen(var s: string); external;
procedure querysave(var s: string); external;
procedure radiobutton(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure radiobutton(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure radiobuttong(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
overload procedure radiobuttong(x1: integer; y1: integer; x2: integer; y2: integer; view s: string; id: integer); external;
procedure radiobuttonsiz(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure radiobuttonsiz(view s: string; var w: integer; var h: integer); external;
procedure radiobuttonsizg(var f: text; view s: string; var w: integer; var h: integer); external;
overload procedure radiobuttonsizg(view s: string; var w: integer; var h: integer); external;
procedure raised(var f: text; e: integer); external;
overload procedure raised(e: integer); external;
procedure rect(var f: text; x1: integer; y1: integer; x2: integer; y2: integer); external;
overload procedure rect(x1: integer; y1: integer; x2: integer; y2: integer); external;
procedure restab(var f: text; t: integer); external;
overload procedure restab(t: integer); external;
procedure restabg(var f: text; t: integer); external;
overload procedure restabg(t: integer); external;
procedure reverse(var f: text; e: integer); external;
overload procedure reverse(e: integer); external;
procedure right(var f: text); external;
overload procedure right; external;
procedure rrect(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; xs: integer; ys: integer); external;
overload procedure rrect(x1: integer; y1: integer; x2: integer; y2: integer; xs: integer; ys: integer); external;
procedure scncen(var f: text; var x: integer; var y: integer); external;
overload procedure scncen(var x: integer; var y: integer); external;
procedure scnceng(var f: text; var x: integer; var y: integer); external;
overload procedure scnceng(var x: integer; var y: integer); external;
procedure scnsiz(var f: text; var x: integer; var y: integer); external;
overload procedure scnsiz(var x: integer; var y: integer); external;
procedure scnsizg(var f: text; var x: integer; var y: integer); external;
overload procedure scnsizg(var x: integer; var y: integer); external;
procedure scroll(var f: text; x: integer; y: integer); external;
overload procedure scroll(x: integer; y: integer); external;
procedure scrollg(var f: text; x: integer; y: integer); external;
overload procedure scrollg(x: integer; y: integer); external;
procedure scrollhoriz(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure scrollhoriz(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure scrollhorizg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure scrollhorizg(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure scrollhorizsiz(var f: text; var w: integer; var h: integer); external;
overload procedure scrollhorizsiz(var w: integer; var h: integer); external;
procedure scrollhorizsizg(var f: text; var w: integer; var h: integer); external;
overload procedure scrollhorizsizg(var w: integer; var h: integer); external;
procedure scrollpos(var f: text; id: integer; r: integer); external;
overload procedure scrollpos(id: integer; r: integer); external;
procedure scrollsiz(var f: text; id: integer; r: integer); external;
overload procedure scrollsiz(id: integer; r: integer); external;
procedure scrollvert(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure scrollvert(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure scrollvertg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
overload procedure scrollvertg(x1: integer; y1: integer; x2: integer; y2: integer; id: integer); external;
procedure scrollvertsiz(var f: text; var w: integer; var h: integer); external;
overload procedure scrollvertsiz(var w: integer; var h: integer); external;
procedure scrollvertsizg(var f: text; var w: integer; var h: integer); external;
overload procedure scrollvertsizg(var w: integer; var h: integer); external;
procedure select(var f: text; u: integer; d: integer); external;
overload procedure select(u: integer; d: integer); external;
procedure selectwidget(var f: text; id: integer; e: integer); external;
overload procedure selectwidget(id: integer; e: integer); external;
procedure sendevent(var f: text; var er: evtrec); external;
overload procedure sendevent(var er: evtrec); external;
procedure setpixel(var f: text; x: integer; y: integer); external;
overload procedure setpixel(x: integer; y: integer); external;
procedure setpoints(var f: text; ps: real); external;
overload procedure setpoints(ps: real); external;
procedure setpos(var f: text; x: integer; y: integer); external;
overload procedure setpos(x: integer; y: integer); external;
procedure setposg(var f: text; x: integer; y: integer); external;
overload procedure setposg(x: integer; y: integer); external;
procedure setsiz(var f: text; x: integer; y: integer); external;
overload procedure setsiz(x: integer; y: integer); external;
procedure setsizg(var f: text; x: integer; y: integer); external;
overload procedure setsizg(x: integer; y: integer); external;
procedure settab(var f: text; t: integer); external;
overload procedure settab(t: integer); external;
procedure settabg(var f: text; t: integer); external;
overload procedure settabg(t: integer); external;
procedure sizable(var f: text; e: integer); external;
overload procedure sizable(e: integer); external;
procedure sizbuf(var f: text; x: integer; y: integer); external;
overload procedure sizbuf(x: integer; y: integer); external;
procedure sizbufg(var f: text; x: integer; y: integer); external;
overload procedure sizbufg(x: integer; y: integer); external;
procedure sizwidget(var f: text; id: integer; x: integer; y: integer); external;
overload procedure sizwidget(id: integer; x: integer; y: integer); external;
procedure sizwidgetg(var f: text; id: integer; x: integer; y: integer); external;
overload procedure sizwidgetg(id: integer; x: integer; y: integer); external;
procedure slidehoriz(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
overload procedure slidehoriz(x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
procedure slidehorizg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
overload procedure slidehorizg(x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
procedure slidehorizsiz(var f: text; var w: integer; var h: integer); external;
overload procedure slidehorizsiz(var w: integer; var h: integer); external;
procedure slidehorizsizg(var f: text; var w: integer; var h: integer); external;
overload procedure slidehorizsizg(var w: integer; var h: integer); external;
procedure slidevert(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
overload procedure slidevert(x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
procedure slidevertg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
overload procedure slidevertg(x1: integer; y1: integer; x2: integer; y2: integer; mark: integer; id: integer); external;
procedure slidevertsiz(var f: text; var w: integer; var h: integer); external;
overload procedure slidevertsiz(var w: integer; var h: integer); external;
procedure slidevertsizg(var f: text; var w: integer; var h: integer); external;
overload procedure slidevertsizg(var w: integer; var h: integer); external;
procedure standout(var f: text; e: integer); external;
overload procedure standout(e: integer); external;
procedure stdmenu(sms: stdmenusel; var sm: menuptr; pm: menuptr); external;
procedure strikeout(var f: text; e: integer); external;
overload procedure strikeout(e: integer); external;
procedure subscript(var f: text; e: integer); external;
overload procedure subscript(e: integer); external;
procedure superscript(var f: text; e: integer); external;
overload procedure superscript(e: integer); external;
procedure sysbar(var f: text; e: integer); external;
overload procedure sysbar(e: integer); external;
procedure tabbarclient(var f: text; tor: tabori; w: integer; h: integer; var cw: integer; var ch: integer; var ox: integer; var oy: integer); external;
overload procedure tabbarclient(tor: tabori; w: integer; h: integer; var cw: integer; var ch: integer; var ox: integer; var oy: integer); external;
procedure tabbarclientg(var f: text; tor: tabori; w: integer; h: integer; var cw: integer; var ch: integer; var ox: integer; var oy: integer); external;
overload procedure tabbarclientg(tor: tabori; w: integer; h: integer; var cw: integer; var ch: integer; var ox: integer; var oy: integer); external;
procedure tabbar(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; tor: tabori; id: integer); external;
overload procedure tabbar(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; tor: tabori; id: integer); external;
procedure tabbarg(var f: text; x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; tor: tabori; id: integer); external;
overload procedure tabbarg(x1: integer; y1: integer; x2: integer; y2: integer; sp: strptr; tor: tabori; id: integer); external;
procedure tabbarsiz(var f: text; tor: tabori; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
overload procedure tabbarsiz(tor: tabori; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
procedure tabbarsizg(var f: text; tor: tabori; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
overload procedure tabbarsizg(tor: tabori; cw: integer; ch: integer; var w: integer; var h: integer; var ox: integer; var oy: integer); external;
procedure tabsel(var f: text; id: integer; tn: integer); external;
overload procedure tabsel(id: integer; tn: integer); external;
procedure timer(var f: text; i: integer; t: integer; r: integer); external;
overload procedure timer(i: integer; t: integer; r: integer); external;
procedure title(var f: text; view ts: string); external;
overload procedure title(view ts: string); external;
procedure underline(var f: text; e: integer); external;
overload procedure underline(e: integer); external;
procedure up(var f: text); external;
overload procedure up; external;
procedure viewoffg(var f: text; x: integer; y: integer); external;
overload procedure viewoffg(x: integer; y: integer); external;
procedure viewscale(var f: text; x: real; y: real); external;
overload procedure viewscale(x: real; y: real); external;
procedure winclient(var f: text; cx: integer; cy: integer; var wx: integer; var wy: integer; ms: winmodset); external;
overload procedure winclient(cx: integer; cy: integer; var wx: integer; var wy: integer; ms: winmodset); external;
procedure winclientg(var f: text; cx: integer; cy: integer; var wx: integer; var wy: integer; ms: winmodset); external;
overload procedure winclientg(cx: integer; cy: integer; var wx: integer; var wy: integer; ms: winmodset); external;
procedure writejust(var f: text; view s: string; n: integer); external;
overload procedure writejust(view s: string; n: integer); external;
procedure wrtstr(var f: text; view s: string); external;
overload procedure wrtstr(view s: string); external;
procedure wrtstrn(var f: text; view s: string; n: integer); external;
overload procedure wrtstrn(view s: string; n: integer); external;
procedure xbold(var f: text; e: integer); external;
overload procedure xbold(e: integer); external;
procedure xlight(var f: text; e: integer); external;
overload procedure xlight(e: integer); external;

begin
end.
