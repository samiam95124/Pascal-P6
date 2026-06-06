{******************************************************************************
*                                                                             *
*                         TERMINAL MODEL LIBRARY                              *
*                                                                             *
*                           Copyright (C) 2024                                *
*                                                                             *
*                               S. A. FRANCO                                  *
*                                                                             *
* Defines the Pascaline interface to the Petit-Ami terminal module. The       *
* terminal interface describes a 2 dimensional, fixed window on which         *
* characters are drawn. Each character can have colors or attributes. The     *
* size of the window can be determined, and timer, mouse, and joystick        *
* services are supported.                                                     *
*                                                                             *
* This is the Pascaline binding. The actual implementation lives in the C     *
* Petit-Ami terminal module (terminal.c), reached through terminal_wrapper.   *
*                                                                             *
******************************************************************************}

module terminal;

const

   maxtim = 10; { maximum number of timers available }

type

   { colors displayable in text mode }
   color = (black, white, red, green, blue, cyan, yellow, magenta);

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
             etredraw,  { window redraw }
             etmin,     { window minimized }
             etmax,     { window maximized }
             etnorm,    { window normalized }
             etmenus);  { menu item selected }

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
         etresize: (rszx, rszy: integer);          { new window size }
         etmenus:  (menuid: integer);              { menu item selected }

         { events without parameter data carry no fields }
         etup, etdown, etleft, etright, etleftw, etrightw, ethome, ethomes,
         ethomel, etend, etends, etendl, etscrl, etscrr, etscru, etscrd, etpagd,
         etpagu, ettab, etenter, etinsert, etinsertl, etinsertt, etdel, etdell,
         etdelcf, etdelcb, etcopy, etcopyl, etcan, etstop, etcont, etprint,
         etprintb, etprints, etmenu, etfocus, etnofocus, ethover, etnohover,
         etterm, etframe, etredraw, etmin, etmax, etnorm: ()

   end;

   { window mode set, used to select window components }
   winmod = (wmframe, { frame on/off }
             wmsize,  { size bars on/off }
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

   { standard menu selector }
   stdmenusel = integer;

{
 * Routines at this level. Each screen routine has an explicit file form
 * (var f: text) and a default form that operates on the standard output
 * (or, for event, the standard input).
}
procedure cursor(var f: text; x, y: integer); external;
overload procedure cursor(x, y: integer); external;
function maxx(var f: text): integer; external;
overload function maxx: integer; external;
function maxy(var f: text): integer; external;
overload function maxy: integer; external;
procedure home(var f: text); external;
overload procedure home; external;
procedure del(var f: text); external;
overload procedure del; external;
procedure up(var f: text); external;
overload procedure up; external;
procedure down(var f: text); external;
overload procedure down; external;
procedure left(var f: text); external;
overload procedure left; external;
procedure right(var f: text); external;
overload procedure right; external;
procedure blink(var f: text; e: boolean); external;
overload procedure blink(e: boolean); external;
procedure reverse(var f: text; e: boolean); external;
overload procedure reverse(e: boolean); external;
procedure underline(var f: text; e: boolean); external;
overload procedure underline(e: boolean); external;
procedure superscript(var f: text; e: boolean); external;
overload procedure superscript(e: boolean); external;
procedure subscript(var f: text; e: boolean); external;
overload procedure subscript(e: boolean); external;
procedure italic(var f: text; e: boolean); external;
overload procedure italic(e: boolean); external;
procedure bold(var f: text; e: boolean); external;
overload procedure bold(e: boolean); external;
procedure strikeout(var f: text; e: boolean); external;
overload procedure strikeout(e: boolean); external;
procedure standout(var f: text; e: boolean); external;
overload procedure standout(e: boolean); external;
procedure fcolor(var f: text; c: color); external;
overload procedure fcolor(c: color); external;
procedure bcolor(var f: text; c: color); external;
overload procedure bcolor(c: color); external;
procedure auto(var f: text; e: boolean); external;
overload procedure auto(e: boolean); external;
procedure curvis(var f: text; e: boolean); external;
overload procedure curvis(e: boolean); external;
procedure scroll(var f: text; x, y: integer); external;
overload procedure scroll(x, y: integer); external;
function curx(var f: text): integer; external;
overload function curx: integer; external;
function cury(var f: text): integer; external;
overload function cury: integer; external;
function curbnd(var f: text): boolean; external;
overload function curbnd: boolean; external;
procedure select(var f: text; u, d: integer); external;
overload procedure select(u, d: integer); external;
procedure event(var f: text; var er: evtrec); external;
overload procedure event(var er: evtrec); external;
procedure timer(var f: text; i: integer; t: integer; r: boolean); external;
overload procedure timer(i: integer; t: integer; r: boolean); external;
procedure killtimer(var f: text; i: integer); external;
overload procedure killtimer(i: integer); external;
function mouse(var f: text): integer; external;
overload function mouse: integer; external;
function mousebutton(var f: text; m: integer): integer; external;
overload function mousebutton(m: integer): integer; external;
function joystick(var f: text): integer; external;
overload function joystick: integer; external;
function joybutton(var f: text; j: integer): integer; external;
overload function joybutton(j: integer): integer; external;
function joyaxis(var f: text; j: integer): integer; external;
overload function joyaxis(j: integer): integer; external;
procedure settab(var f: text; t: integer); external;
overload procedure settab(t: integer); external;
procedure restab(var f: text; t: integer); external;
overload procedure restab(t: integer); external;
procedure clrtab(var f: text); external;
overload procedure clrtab; external;
function funkey(var f: text): integer; external;
overload function funkey: integer; external;
procedure frametimer(var f: text; e: boolean); external;
overload procedure frametimer(e: boolean); external;
procedure autohold(e: boolean); external;
procedure wrtstr(var f: text; view s: string); external;
overload procedure wrtstr(view s: string); external;
procedure sizbuf(var f: text; x, y: integer); external;
overload procedure sizbuf(x, y: integer); external;
procedure title(var f: text; view ts: string); external;
overload procedure title(view ts: string); external;
procedure fcolorc(var f: text; r, g, b: integer); external;
overload procedure fcolorc(r, g, b: integer); external;
procedure bcolorc(var f: text; r, g, b: integer); external;
overload procedure bcolorc(r, g, b: integer); external;
procedure sendevent(var f: text; var er: evtrec); external;
overload procedure sendevent(var er: evtrec); external;
procedure openwin(var infile, outfile, parent: text; wid: integer); external;
procedure buffer(var f: text; e: boolean); external;
overload procedure buffer(e: boolean); external;
procedure getsiz(var f: text; var x, y: integer); external;
overload procedure getsiz(var x, y: integer); external;
procedure setsiz(var f: text; x, y: integer); external;
overload procedure setsiz(x, y: integer); external;
procedure setpos(var f: text; x, y: integer); external;
overload procedure setpos(x, y: integer); external;
procedure scnsiz(var f: text; var x, y: integer); external;
overload procedure scnsiz(var x, y: integer); external;
procedure scncen(var f: text; var x, y: integer); external;
overload procedure scncen(var x, y: integer); external;
procedure winclient(var f: text; cx, cy: integer; var wx, wy: integer;
                    ms: winmodset); external;
overload procedure winclient(cx, cy: integer; var wx, wy: integer;
                            ms: winmodset); external;
procedure front(var f: text); external;
overload procedure front; external;
procedure back(var f: text); external;
overload procedure back; external;
procedure frame(var f: text; e: boolean); external;
overload procedure frame(e: boolean); external;
procedure sizable(var f: text; e: boolean); external;
overload procedure sizable(e: boolean); external;
procedure sysbar(var f: text; e: boolean); external;
overload procedure sysbar(e: boolean); external;
procedure menu(var f: text; m: menuptr); external;
overload procedure menu(m: menuptr); external;
procedure menuena(var f: text; id: integer; onoff: boolean); external;
overload procedure menuena(id: integer; onoff: boolean); external;
procedure menusel(var f: text; id: integer; en: boolean); external;
overload procedure menusel(id: integer; en: boolean); external;
procedure stdmenu(sms: stdmenusel; var sm: menuptr; pm: menuptr); external;
procedure focus(var f: text); external;
overload procedure focus; external;
function getwinid: integer; external;
procedure democall(procedure p); external;
procedure demoevent(procedure handler(var er: evtrec)); external;
procedure eventover(e: evtcod; procedure eh(var er: evtrec); out oeh: integer);
   external;
procedure eventsover(procedure eh(var er: evtrec); out oeh: integer); external;
function newthread(procedure threadmain): integer; external;
function initlock: integer; external;
procedure deinitlock(ln: integer); external;
procedure lock(ln: integer); external;
procedure unlock(ln: integer); external;
function initsig: integer; external;
procedure deinitsig(sn: integer); external;
procedure sendsig(sn: integer); external;
procedure sendsigone(sn: integer); external;
procedure waitsig(ln, sn: integer); external;

begin
end.
