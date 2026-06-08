/*******************************************************************************
*                     Terminal library wrapper for Pascaline                   *
*                              Created 2024                                    *
*                               S. A. FRANCO                                   *
*                                                                              *
* A wrapper function for each Pascaline terminal routine. Each screen routine  *
* has a file form (wrapper_Xf), from the "var f: text" overload, and a default *
* form (wrapper_X) operating on standard output (or standard input). The event *
* record conversion and callback/menu layer live in terminal_support.c.        *
*******************************************************************************/

#include <stdio.h>
#include <terminal.h>
#include <terminal_wrapper.h>

/* home */

void wrapper_homef(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_home(f);
}

void wrapper_home(void)
{
    ami_home(stdout);
}

/* del */

void wrapper_delf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_del(f);
}

void wrapper_del(void)
{
    ami_del(stdout);
}

/* up */

void wrapper_upf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_up(f);
}

void wrapper_up(void)
{
    ami_up(stdout);
}

/* down */

void wrapper_downf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_down(f);
}

void wrapper_down(void)
{
    ami_down(stdout);
}

/* left */

void wrapper_leftf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_left(f);
}

void wrapper_left(void)
{
    ami_left(stdout);
}

/* right */

void wrapper_rightf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_right(f);
}

void wrapper_right(void)
{
    ami_right(stdout);
}

/* clrtab */

void wrapper_clrtabf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_clrtab(f);
}

void wrapper_clrtab(void)
{
    ami_clrtab(stdout);
}

/* front */

/* back */

/* focus */

/* blink */

void wrapper_blinkf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_blink(f, e);
}

void wrapper_blink(int e)
{
    ami_blink(stdout, e);
}

/* reverse */

void wrapper_reversef(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_reverse(f, e);
}

void wrapper_reverse(int e)
{
    ami_reverse(stdout, e);
}

/* underline */

void wrapper_underlinef(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_underline(f, e);
}

void wrapper_underline(int e)
{
    ami_underline(stdout, e);
}

/* superscript */

void wrapper_superscriptf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_superscript(f, e);
}

void wrapper_superscript(int e)
{
    ami_superscript(stdout, e);
}

/* subscript */

void wrapper_subscriptf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_subscript(f, e);
}

void wrapper_subscript(int e)
{
    ami_subscript(stdout, e);
}

/* italic */

void wrapper_italicf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_italic(f, e);
}

void wrapper_italic(int e)
{
    ami_italic(stdout, e);
}

/* bold */

void wrapper_boldf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bold(f, e);
}

void wrapper_bold(int e)
{
    ami_bold(stdout, e);
}

/* strikeout */

void wrapper_strikeoutf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_strikeout(f, e);
}

void wrapper_strikeout(int e)
{
    ami_strikeout(stdout, e);
}

/* standout */

void wrapper_standoutf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_standout(f, e);
}

void wrapper_standout(int e)
{
    ami_standout(stdout, e);
}

/* auto */

void wrapper_autof(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_auto(f, e);
}

void wrapper_auto(int e)
{
    ami_auto(stdout, e);
}

/* curvis */

void wrapper_curvisf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_curvis(f, e);
}

void wrapper_curvis(int e)
{
    ami_curvis(stdout, e);
}

/* frametimer */

void wrapper_frametimerf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_frametimer(f, e);
}

void wrapper_frametimer(int e)
{
    ami_frametimer(stdout, e);
}

/* buffer */

/* frame */

/* sizable */

/* sysbar */

/* killtimer */

void wrapper_killtimerf(pfile pfp, int i)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_killtimer(f, i);
}

void wrapper_killtimer(int i)
{
    ami_killtimer(stdout, i);
}

/* settab */

void wrapper_settabf(pfile pfp, int t)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_settab(f, t);
}

void wrapper_settab(int t)
{
    ami_settab(stdout, t);
}

/* restab */

void wrapper_restabf(pfile pfp, int t)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_restab(f, t);
}

void wrapper_restab(int t)
{
    ami_restab(stdout, t);
}

/* cursor */

void wrapper_cursorf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_cursor(f, x, y);
}

void wrapper_cursor(int x, int y)
{
    ami_cursor(stdout, x, y);
}

/* scroll */

void wrapper_scrollf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scroll(f, x, y);
}

void wrapper_scroll(int x, int y)
{
    ami_scroll(stdout, x, y);
}

/* select */

void wrapper_selectf(pfile pfp, int u, int d)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_select(f, u, d);
}

void wrapper_select(int u, int d)
{
    ami_select(stdout, u, d);
}

/* sizbuf */

void wrapper_sizbuff(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_sizbuf(f, x, y);
}

void wrapper_sizbuf(int x, int y)
{
    ami_sizbuf(stdout, x, y);
}

/* setsiz */

/* setpos */

/* fcolorc */

void wrapper_fcolorcf(pfile pfp, int r, int g, int b)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fcolorc(f, r, g, b);
}

void wrapper_fcolorc(int r, int g, int b)
{
    ami_fcolorc(stdout, r, g, b);
}

/* bcolorc */

void wrapper_bcolorcf(pfile pfp, int r, int g, int b)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bcolorc(f, r, g, b);
}

void wrapper_bcolorc(int r, int g, int b)
{
    ami_bcolorc(stdout, r, g, b);
}

/* fcolor */

void wrapper_fcolorf(pfile pfp, int c)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fcolor(f, c);
}

void wrapper_fcolor(int c)
{
    ami_fcolor(stdout, c);
}

/* bcolor */

void wrapper_bcolorf(pfile pfp, int c)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bcolor(f, c);
}

void wrapper_bcolor(int c)
{
    ami_bcolor(stdout, c);
}

/* timer */

void wrapper_timerf(pfile pfp, int i, long t, int r)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_timer(f, i, t, r);
}

void wrapper_timer(int i, long t, int r)
{
    ami_timer(stdout, i, t, r);
}

/* maxx */

int wrapper_maxxf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_maxx(f));
}

int wrapper_maxx(void)
{
    return (ami_maxx(stdout));
}

/* maxy */

int wrapper_maxyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_maxy(f));
}

int wrapper_maxy(void)
{
    return (ami_maxy(stdout));
}

/* curx */

int wrapper_curxf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_curx(f));
}

int wrapper_curx(void)
{
    return (ami_curx(stdout));
}

/* cury */

int wrapper_curyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_cury(f));
}

int wrapper_cury(void)
{
    return (ami_cury(stdout));
}

/* curbnd */

int wrapper_curbndf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_curbnd(f));
}

int wrapper_curbnd(void)
{
    return (ami_curbnd(stdout));
}

/* mouse */

int wrapper_mousef(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_mouse(f));
}

int wrapper_mouse(void)
{
    return (ami_mouse(stdout));
}

/* joystick */

int wrapper_joystickf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_joystick(f));
}

int wrapper_joystick(void)
{
    return (ami_joystick(stdout));
}

/* funkey */

int wrapper_funkeyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_funkey(f));
}

int wrapper_funkey(void)
{
    return (ami_funkey(stdout));
}

/* mousebutton */

int wrapper_mousebuttonf(pfile pfp, int m)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_mousebutton(f, m));
}

int wrapper_mousebutton(int m)
{
    return (ami_mousebutton(stdout, m));
}

/* joybutton */

int wrapper_joybuttonf(pfile pfp, int j)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_joybutton(f, j));
}

int wrapper_joybutton(int j)
{
    return (ami_joybutton(stdout, j));
}

/* joyaxis */

int wrapper_joyaxisf(pfile pfp, int j)
{
    FILE* f = psystem_libcwrfil(pfp);
    return (ami_joyaxis(f, j));
}

int wrapper_joyaxis(int j)
{
    return (ami_joyaxis(stdout, j));
}

/* getsiz */

/* scnsiz */

/* scncen */

/* wrtstr */

void wrapper_wrtstrf(pfile pfp, string s, int sl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_wrtstrn(f, s, sl);
}

void wrapper_wrtstr(string s, int sl)
{
    ami_wrtstrn(stdout, s, sl);
}

/* title */

void wrapper_titlef(pfile pfp, string ts, int tsl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_titlen(f, ts, tsl);
}

void wrapper_title(string ts, int tsl)
{
    ami_titlen(stdout, ts, tsl);
}

/* autohold */

void wrapper_autohold(int e)
{
    ami_autohold(e);
}

/* getwinid */

/* initlock */

/* deinitlock */

/* lock */

/* unlock */

/* initsig */

/* deinitsig */

/* sendsig */

/* sendsigone */

/* waitsig */

