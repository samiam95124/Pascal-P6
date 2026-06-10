/* Generated graphics wrappers. Do not edit by hand. */

#include <graphics.h>
#include <support.h>

double wrapper_pointsf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_points(f);
}

double wrapper_points(void)
{
    return ami_points(stdout);
}

int wrapper_baselinef(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_baseline(f);
}

int wrapper_baseline(void)
{
    return ami_baseline(stdout);
}

int wrapper_chrposf(pfile pfp, string s, int sl, int p)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_chrpos(f, cstrz(s, sl), p);
}

int wrapper_chrpos(string s, int sl, int p)
{
    return ami_chrpos(stdout, cstrz(s, sl), p);
}

int wrapper_chrsizxf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_chrsizx(f);
}

int wrapper_chrsizx(void)
{
    return ami_chrsizx(stdout);
}

int wrapper_chrsizyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_chrsizy(f);
}

int wrapper_chrsizy(void)
{
    return ami_chrsizy(stdout);
}

int wrapper_curbndf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_curbnd(f);
}

int wrapper_curbnd(void)
{
    return ami_curbnd(stdout);
}

int wrapper_curxf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_curx(f);
}

int wrapper_curx(void)
{
    return ami_curx(stdout);
}

int wrapper_curxgf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_curxg(f);
}

int wrapper_curxg(void)
{
    return ami_curxg(stdout);
}

int wrapper_curyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_cury(f);
}

int wrapper_cury(void)
{
    return ami_cury(stdout);
}

int wrapper_curygf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_curyg(f);
}

int wrapper_curyg(void)
{
    return ami_curyg(stdout);
}

int wrapper_dpmxf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_dpmx(f);
}

int wrapper_dpmx(void)
{
    return ami_dpmx(stdout);
}

int wrapper_dpmyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_dpmy(f);
}

int wrapper_dpmy(void)
{
    return ami_dpmy(stdout);
}

int wrapper_fontsf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_fonts(f);
}

int wrapper_fonts(void)
{
    return ami_fonts(stdout);
}

int wrapper_funkeyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_funkey(f);
}

int wrapper_funkey(void)
{
    return ami_funkey(stdout);
}

int wrapper_getwigidf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_getwigid(f);
}

int wrapper_getwigid(void)
{
    return ami_getwigid(stdout);
}

int wrapper_getwinid(void)
{
    return ami_getwinid();
}

int wrapper_joyaxisf(pfile pfp, int j)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_joyaxis(f, j);
}

int wrapper_joyaxis(int j)
{
    return ami_joyaxis(stdout, j);
}

int wrapper_joybuttonf(pfile pfp, int j)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_joybutton(f, j);
}

int wrapper_joybutton(int j)
{
    return ami_joybutton(stdout, j);
}

int wrapper_joystickf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_joystick(f);
}

int wrapper_joystick(void)
{
    return ami_joystick(stdout);
}

int wrapper_justposf(pfile pfp, string s, int sl, int p, int n)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_justpos(f, cstrz(s, sl), p, n);
}

int wrapper_justpos(string s, int sl, int p, int n)
{
    return ami_justpos(stdout, cstrz(s, sl), p, n);
}

int wrapper_maxxf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_maxx(f);
}

int wrapper_maxx(void)
{
    return ami_maxx(stdout);
}

int wrapper_maxxgf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_maxxg(f);
}

int wrapper_maxxg(void)
{
    return ami_maxxg(stdout);
}

int wrapper_maxyf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_maxy(f);
}

int wrapper_maxy(void)
{
    return ami_maxy(stdout);
}

int wrapper_maxygf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_maxyg(f);
}

int wrapper_maxyg(void)
{
    return ami_maxyg(stdout);
}

int wrapper_mousebuttonf(pfile pfp, int m)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_mousebutton(f, m);
}

int wrapper_mousebutton(int m)
{
    return ami_mousebutton(stdout, m);
}

int wrapper_mousef(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_mouse(f);
}

int wrapper_mouse(void)
{
    return ami_mouse(stdout);
}

int wrapper_pictsizxf(pfile pfp, int p)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_pictsizx(f, p);
}

int wrapper_pictsizx(int p)
{
    return ami_pictsizx(stdout, p);
}

int wrapper_pictsizyf(pfile pfp, int p)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_pictsizy(f, p);
}

int wrapper_pictsizy(int p)
{
    return ami_pictsizy(stdout, p);
}

int wrapper_scalexf(pfile pfp, int x)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_scalex(f, x);
}

int wrapper_scalex(int x)
{
    return ami_scalex(stdout, x);
}

int wrapper_scaleyf(pfile pfp, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_scaley(f, y);
}

int wrapper_scaley(int y)
{
    return ami_scaley(stdout, y);
}

int wrapper_strsizf(pfile pfp, string s, int sl)
{
    FILE* f = psystem_libcwrfil(pfp);
    return ami_strsiz(f, cstrz(s, sl));
}

int wrapper_strsiz(string s, int sl)
{
    return ami_strsiz(stdout, cstrz(s, sl));
}

void wrapper_alert(string title, int titlel, string message, int messagel)
{
    ami_alert(cstrz(title, titlel), cstrz(message, messagel));
}

void wrapper_arcf(pfile pfp, int x1, int y1, int x2, int y2, int sa, int ea)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_arc(f, x1, y1, x2, y2, sa, ea);
}

void wrapper_arc(int x1, int y1, int x2, int y2, int sa, int ea)
{
    ami_arc(stdout, x1, y1, x2, y2, sa, ea);
}

void wrapper_autof(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_auto(f, e);
}

void wrapper_auto(int e)
{
    ami_auto(stdout, e);
}

void wrapper_autohold(int e)
{
    ami_autohold(e);
}

void wrapper_backf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_back(f);
}

void wrapper_back(void)
{
    ami_back(stdout);
}

void wrapper_backgroundf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_background(f, x1, y1, x2, y2, id);
}

void wrapper_background(int x1, int y1, int x2, int y2, int id)
{
    ami_background(stdout, x1, y1, x2, y2, id);
}

void wrapper_backgroundgf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_backgroundg(f, x1, y1, x2, y2, id);
}

void wrapper_backgroundg(int x1, int y1, int x2, int y2, int id)
{
    ami_backgroundg(stdout, x1, y1, x2, y2, id);
}

void wrapper_backwidgetf(pfile pfp, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_backwidget(f, id);
}

void wrapper_backwidget(int id)
{
    ami_backwidget(stdout, id);
}

void wrapper_bandf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_band(f);
}

void wrapper_band(void)
{
    ami_band(stdout);
}

void wrapper_bcolorcf(pfile pfp, int r, int g, int b)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bcolorc(f, r, g, b);
}

void wrapper_bcolorc(int r, int g, int b)
{
    ami_bcolorc(stdout, r, g, b);
}

void wrapper_bcolorf(pfile pfp, int c)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bcolor(f, c);
}

void wrapper_bcolor(int c)
{
    ami_bcolor(stdout, c);
}

void wrapper_bcolorgf(pfile pfp, int r, int g, int b)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bcolorg(f, r, g, b);
}

void wrapper_bcolorg(int r, int g, int b)
{
    ami_bcolorg(stdout, r, g, b);
}

void wrapper_binvisf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_binvis(f);
}

void wrapper_binvis(void)
{
    ami_binvis(stdout);
}

void wrapper_blinkf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_blink(f, e);
}

void wrapper_blink(int e)
{
    ami_blink(stdout, e);
}

void wrapper_boldf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bold(f, e);
}

void wrapper_bold(int e)
{
    ami_bold(stdout, e);
}

void wrapper_borf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bor(f);
}

void wrapper_bor(void)
{
    ami_bor(stdout);
}

void wrapper_boverf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bover(f);
}

void wrapper_bover(void)
{
    ami_bover(stdout);
}

void wrapper_bufferf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_buffer(f, e);
}

void wrapper_buffer(int e)
{
    ami_buffer(stdout, e);
}

void wrapper_buttonf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_button(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_button(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_button(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_buttongf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_buttong(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_buttong(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_buttong(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_buttonsizf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_buttonsiz(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_buttonsiz(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_buttonsiz(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_buttonsizgf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_buttonsizg(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_buttonsizg(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_buttonsizg(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_bxorf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_bxor(f);
}

void wrapper_bxor(void)
{
    ami_bxor(stdout);
}

void wrapper_checkboxf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_checkbox(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_checkbox(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_checkbox(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_checkboxgf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_checkboxg(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_checkboxg(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_checkboxg(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_checkboxsizf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_checkboxsiz(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_checkboxsiz(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_checkboxsiz(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_checkboxsizgf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_checkboxsizg(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_checkboxsizg(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_checkboxsizg(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_chrspcxf(pfile pfp, int s)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_chrspcx(f, s);
}

void wrapper_chrspcx(int s)
{
    ami_chrspcx(stdout, s);
}

void wrapper_chrspcyf(pfile pfp, int s)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_chrspcy(f, s);
}

void wrapper_chrspcy(int s)
{
    ami_chrspcy(stdout, s);
}

void wrapper_clrtabf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_clrtab(f);
}

void wrapper_clrtab(void)
{
    ami_clrtab(stdout);
}

void wrapper_condensedf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_condensed(f, e);
}

void wrapper_condensed(int e)
{
    ami_condensed(stdout, e);
}

void wrapper_cursorf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_cursor(f, x, y);
}

void wrapper_cursor(int x, int y)
{
    ami_cursor(stdout, x, y);
}

void wrapper_cursorgf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_cursorg(f, x, y);
}

void wrapper_cursorg(int x, int y)
{
    ami_cursorg(stdout, x, y);
}

void wrapper_curvisf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_curvis(f, e);
}

void wrapper_curvis(int e)
{
    ami_curvis(stdout, e);
}

void wrapper_delf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_del(f);
}

void wrapper_del(void)
{
    ami_del(stdout);
}

void wrapper_delpictf(pfile pfp, int p)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_delpict(f, p);
}

void wrapper_delpict(int p)
{
    ami_delpict(stdout, p);
}

void wrapper_downf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_down(f);
}

void wrapper_down(void)
{
    ami_down(stdout);
}

void wrapper_editboxf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_editbox(f, x1, y1, x2, y2, id);
}

void wrapper_editbox(int x1, int y1, int x2, int y2, int id)
{
    ami_editbox(stdout, x1, y1, x2, y2, id);
}

void wrapper_editboxgf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_editboxg(f, x1, y1, x2, y2, id);
}

void wrapper_editboxg(int x1, int y1, int x2, int y2, int id)
{
    ami_editboxg(stdout, x1, y1, x2, y2, id);
}

void wrapper_editboxsizf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_editboxsiz(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_editboxsiz(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_editboxsiz(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_editboxsizgf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_editboxsizg(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_editboxsizg(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_editboxsizg(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_ellipsef(pfile pfp, int x1, int y1, int x2, int y2)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_ellipse(f, x1, y1, x2, y2);
}

void wrapper_ellipse(int x1, int y1, int x2, int y2)
{
    ami_ellipse(stdout, x1, y1, x2, y2);
}

void wrapper_enablewidgetf(pfile pfp, int id, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_enablewidget(f, id, e);
}

void wrapper_enablewidget(int id, int e)
{
    ami_enablewidget(stdout, id, e);
}

void wrapper_extendedf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_extended(f, e);
}

void wrapper_extended(int e)
{
    ami_extended(stdout, e);
}

void wrapper_fandf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fand(f);
}

void wrapper_fand(void)
{
    ami_fand(stdout);
}

void wrapper_farcf(pfile pfp, int x1, int y1, int x2, int y2, int sa, int ea)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_farc(f, x1, y1, x2, y2, sa, ea);
}

void wrapper_farc(int x1, int y1, int x2, int y2, int sa, int ea)
{
    ami_farc(stdout, x1, y1, x2, y2, sa, ea);
}

void wrapper_fchordf(pfile pfp, int x1, int y1, int x2, int y2, int sa, int ea)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fchord(f, x1, y1, x2, y2, sa, ea);
}

void wrapper_fchord(int x1, int y1, int x2, int y2, int sa, int ea)
{
    ami_fchord(stdout, x1, y1, x2, y2, sa, ea);
}

void wrapper_fcolorcf(pfile pfp, int r, int g, int b)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fcolorc(f, r, g, b);
}

void wrapper_fcolorc(int r, int g, int b)
{
    ami_fcolorc(stdout, r, g, b);
}

void wrapper_fcolorf(pfile pfp, int c)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fcolor(f, c);
}

void wrapper_fcolor(int c)
{
    ami_fcolor(stdout, c);
}

void wrapper_fcolorgf(pfile pfp, int r, int g, int b)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fcolorg(f, r, g, b);
}

void wrapper_fcolorg(int r, int g, int b)
{
    ami_fcolorg(stdout, r, g, b);
}

void wrapper_fellipsef(pfile pfp, int x1, int y1, int x2, int y2)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fellipse(f, x1, y1, x2, y2);
}

void wrapper_fellipse(int x1, int y1, int x2, int y2)
{
    ami_fellipse(stdout, x1, y1, x2, y2);
}

void wrapper_finvisf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_finvis(f);
}

void wrapper_finvis(void)
{
    ami_finvis(stdout);
}

void wrapper_focusf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_focus(f);
}

void wrapper_focus(void)
{
    ami_focus(stdout);
}

void wrapper_focuswidgetf(pfile pfp, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_focuswidget(f, id);
}

void wrapper_focuswidget(int id)
{
    ami_focuswidget(stdout, id);
}

void wrapper_fontf(pfile pfp, int fc)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_font(f, fc);
}

void wrapper_font(int fc)
{
    ami_font(stdout, fc);
}

void wrapper_fontnamf(pfile pfp, int fc, string fns, int fnsl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fontnam(f, fc, fns, fnsl);
    { int _p = 0; while (_p < fnsl && fns[_p]) _p++; while (_p < fnsl) fns[_p++] = ' '; }
}

void wrapper_fontnam(int fc, string fns, int fnsl)
{
    ami_fontnam(stdout, fc, fns, fnsl);
    { int _p = 0; while (_p < fnsl && fns[_p]) _p++; while (_p < fnsl) fns[_p++] = ' '; }
}

void wrapper_fontsizf(pfile pfp, int s)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fontsiz(f, s);
}

void wrapper_fontsiz(int s)
{
    ami_fontsiz(stdout, s);
}

void wrapper_forf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_for(f);
}

void wrapper_for(void)
{
    ami_for(stdout);
}

void wrapper_foverf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fover(f);
}

void wrapper_fover(void)
{
    ami_fover(stdout);
}

void wrapper_framef(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_frame(f, e);
}

void wrapper_frame(int e)
{
    ami_frame(stdout, e);
}

void wrapper_frametimerf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_frametimer(f, e);
}

void wrapper_frametimer(int e)
{
    ami_frametimer(stdout, e);
}

void wrapper_frectf(pfile pfp, int x1, int y1, int x2, int y2)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_frect(f, x1, y1, x2, y2);
}

void wrapper_frect(int x1, int y1, int x2, int y2)
{
    ami_frect(stdout, x1, y1, x2, y2);
}

void wrapper_frontf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_front(f);
}

void wrapper_front(void)
{
    ami_front(stdout);
}

void wrapper_frontwidgetf(pfile pfp, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_frontwidget(f, id);
}

void wrapper_frontwidget(int id)
{
    ami_frontwidget(stdout, id);
}

void wrapper_frrectf(pfile pfp, int x1, int y1, int x2, int y2, int xs, int ys)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_frrect(f, x1, y1, x2, y2, xs, ys);
}

void wrapper_frrect(int x1, int y1, int x2, int y2, int xs, int ys)
{
    ami_frrect(stdout, x1, y1, x2, y2, xs, ys);
}

void wrapper_ftrianglef(pfile pfp, int x1, int y1, int x2, int y2, int x3, int y3)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_ftriangle(f, x1, y1, x2, y2, x3, y3);
}

void wrapper_ftriangle(int x1, int y1, int x2, int y2, int x3, int y3)
{
    ami_ftriangle(stdout, x1, y1, x2, y2, x3, y3);
}

void wrapper_fxorf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_fxor(f);
}

void wrapper_fxor(void)
{
    ami_fxor(stdout);
}

void wrapper_getsizf(pfile pfp, long* x, long* y)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tx;
    int ty;
    ami_getsiz(f, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_getsiz(long* x, long* y)
{
    int tx;
    int ty;
    ami_getsiz(stdout, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_getsizgf(pfile pfp, long* x, long* y)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tx;
    int ty;
    ami_getsizg(f, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_getsizg(long* x, long* y)
{
    int tx;
    int ty;
    ami_getsizg(stdout, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_getwidgettextf(pfile pfp, int id, string s, int sl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_getwidgettext(f, id, s, sl);
    { int _p = 0; while (_p < sl && s[_p]) _p++; while (_p < sl) s[_p++] = ' '; }
}

void wrapper_getwidgettext(int id, string s, int sl)
{
    ami_getwidgettext(stdout, id, s, sl);
    { int _p = 0; while (_p < sl && s[_p]) _p++; while (_p < sl) s[_p++] = ' '; }
}

void wrapper_groupf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_group(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_group(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_group(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_groupgf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_groupg(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_groupg(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_groupg(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_groupsizf(pfile pfp, string s, int sl, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    int tox;
    int toy;
    ami_groupsiz(f, cstrz(s, sl), cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_groupsiz(string s, int sl, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    int tw;
    int th;
    int tox;
    int toy;
    ami_groupsiz(stdout, cstrz(s, sl), cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_groupsizgf(pfile pfp, string s, int sl, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    int tox;
    int toy;
    ami_groupsizg(f, cstrz(s, sl), cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_groupsizg(string s, int sl, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    int tw;
    int th;
    int tox;
    int toy;
    ami_groupsizg(stdout, cstrz(s, sl), cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_hollowf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_hollow(f, e);
}

void wrapper_hollow(int e)
{
    ami_hollow(stdout, e);
}

void wrapper_homef(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_home(f);
}

void wrapper_home(void)
{
    ami_home(stdout);
}

void wrapper_italicf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_italic(f, e);
}

void wrapper_italic(int e)
{
    ami_italic(stdout, e);
}

void wrapper_killtimerf(pfile pfp, int i)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_killtimer(f, i);
}

void wrapper_killtimer(int i)
{
    ami_killtimer(stdout, i);
}

void wrapper_killwidgetf(pfile pfp, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_killwidget(f, id);
}

void wrapper_killwidget(int id)
{
    ami_killwidget(stdout, id);
}

void wrapper_leftf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_left(f);
}

void wrapper_left(void)
{
    ami_left(stdout);
}

void wrapper_lightf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_light(f, e);
}

void wrapper_light(int e)
{
    ami_light(stdout, e);
}

void wrapper_linef(pfile pfp, int x1, int y1, int x2, int y2)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_line(f, x1, y1, x2, y2);
}

void wrapper_line(int x1, int y1, int x2, int y2)
{
    ami_line(stdout, x1, y1, x2, y2);
}

void wrapper_linestylef(pfile pfp, int style)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_linestyle(f, style);
}

void wrapper_linestyle(int style)
{
    ami_linestyle(stdout, style);
}

void wrapper_linewidthf(pfile pfp, int w)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_linewidth(f, w);
}

void wrapper_linewidth(int w)
{
    ami_linewidth(stdout, w);
}

void wrapper_loadpictf(pfile pfp, int p, string fn, int fnl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_loadpict(f, p, cstrz(fn, fnl));
}

void wrapper_loadpict(int p, string fn, int fnl)
{
    ami_loadpict(stdout, p, cstrz(fn, fnl));
}

void wrapper_menuenaf(pfile pfp, int id, int onoff)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_menuena(f, id, onoff);
}

void wrapper_menuena(int id, int onoff)
{
    ami_menuena(stdout, id, onoff);
}

void wrapper_menuself(pfile pfp, int id, int select)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_menusel(f, id, select);
}

void wrapper_menusel(int id, int select)
{
    ami_menusel(stdout, id, select);
}

void wrapper_numselboxf(pfile pfp, int x1, int y1, int x2, int y2, int l, int u, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_numselbox(f, x1, y1, x2, y2, l, u, id);
}

void wrapper_numselbox(int x1, int y1, int x2, int y2, int l, int u, int id)
{
    ami_numselbox(stdout, x1, y1, x2, y2, l, u, id);
}

void wrapper_numselboxgf(pfile pfp, int x1, int y1, int x2, int y2, int l, int u, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_numselboxg(f, x1, y1, x2, y2, l, u, id);
}

void wrapper_numselboxg(int x1, int y1, int x2, int y2, int l, int u, int id)
{
    ami_numselboxg(stdout, x1, y1, x2, y2, l, u, id);
}

void wrapper_numselboxsizf(pfile pfp, int l, int u, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_numselboxsiz(f, l, u, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_numselboxsiz(int l, int u, long* w, long* h)
{
    int tw;
    int th;
    ami_numselboxsiz(stdout, l, u, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_numselboxsizgf(pfile pfp, int l, int u, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_numselboxsizg(f, l, u, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_numselboxsizg(int l, int u, long* w, long* h)
{
    int tw;
    int th;
    ami_numselboxsizg(stdout, l, u, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_pathf(pfile pfp, int a)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_path(f, a);
}

void wrapper_path(int a)
{
    ami_path(stdout, a);
}

void wrapper_picturef(pfile pfp, int p, int x1, int y1, int x2, int y2)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_picture(f, p, x1, y1, x2, y2);
}

void wrapper_picture(int p, int x1, int y1, int x2, int y2)
{
    ami_picture(stdout, p, x1, y1, x2, y2);
}

void wrapper_poswidgetf(pfile pfp, int id, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_poswidget(f, id, x, y);
}

void wrapper_poswidget(int id, int x, int y)
{
    ami_poswidget(stdout, id, x, y);
}

void wrapper_poswidgetgf(pfile pfp, int id, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_poswidgetg(f, id, x, y);
}

void wrapper_poswidgetg(int id, int x, int y)
{
    ami_poswidgetg(stdout, id, x, y);
}

void wrapper_progbarf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_progbar(f, x1, y1, x2, y2, id);
}

void wrapper_progbar(int x1, int y1, int x2, int y2, int id)
{
    ami_progbar(stdout, x1, y1, x2, y2, id);
}

void wrapper_progbargf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_progbarg(f, x1, y1, x2, y2, id);
}

void wrapper_progbarg(int x1, int y1, int x2, int y2, int id)
{
    ami_progbarg(stdout, x1, y1, x2, y2, id);
}

void wrapper_progbarposf(pfile pfp, int id, int pos)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_progbarpos(f, id, pos);
}

void wrapper_progbarpos(int id, int pos)
{
    ami_progbarpos(stdout, id, pos);
}

void wrapper_progbarsizf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_progbarsiz(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_progbarsiz(long* w, long* h)
{
    int tw;
    int th;
    ami_progbarsiz(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_progbarsizgf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_progbarsizg(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_progbarsizg(long* w, long* h)
{
    int tw;
    int th;
    ami_progbarsizg(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_putwidgettextf(pfile pfp, int id, string s, int sl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_putwidgettext(f, id, cstrz(s, sl));
}

void wrapper_putwidgettext(int id, string s, int sl)
{
    ami_putwidgettext(stdout, id, cstrz(s, sl));
}

void wrapper_querycolor(long* r, long* g, long* b)
{
    int tr;
    int tg;
    int tb;
    ami_querycolor(&tr, &tg, &tb);
    *r = tr;
    *g = tg;
    *b = tb;
}

void wrapper_queryopen(string s, int sl)
{
    ami_queryopen(s, sl);
    { int _p = 0; while (_p < sl && s[_p]) _p++; while (_p < sl) s[_p++] = ' '; }
}

void wrapper_querysave(string s, int sl)
{
    ami_querysave(s, sl);
    { int _p = 0; while (_p < sl && s[_p]) _p++; while (_p < sl) s[_p++] = ' '; }
}

void wrapper_radiobuttonf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_radiobutton(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_radiobutton(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_radiobutton(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_radiobuttongf(pfile pfp, int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_radiobuttong(f, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_radiobuttong(int x1, int y1, int x2, int y2, string s, int sl, int id)
{
    ami_radiobuttong(stdout, x1, y1, x2, y2, cstrz(s, sl), id);
}

void wrapper_radiobuttonsizf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_radiobuttonsiz(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_radiobuttonsiz(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_radiobuttonsiz(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_radiobuttonsizgf(pfile pfp, string s, int sl, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_radiobuttonsizg(f, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_radiobuttonsizg(string s, int sl, long* w, long* h)
{
    int tw;
    int th;
    ami_radiobuttonsizg(stdout, cstrz(s, sl), &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_raisedf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_raised(f, e);
}

void wrapper_raised(int e)
{
    ami_raised(stdout, e);
}

void wrapper_rectf(pfile pfp, int x1, int y1, int x2, int y2)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_rect(f, x1, y1, x2, y2);
}

void wrapper_rect(int x1, int y1, int x2, int y2)
{
    ami_rect(stdout, x1, y1, x2, y2);
}

void wrapper_restabf(pfile pfp, int t)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_restab(f, t);
}

void wrapper_restab(int t)
{
    ami_restab(stdout, t);
}

void wrapper_restabgf(pfile pfp, int t)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_restabg(f, t);
}

void wrapper_restabg(int t)
{
    ami_restabg(stdout, t);
}

void wrapper_reversef(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_reverse(f, e);
}

void wrapper_reverse(int e)
{
    ami_reverse(stdout, e);
}

void wrapper_rightf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_right(f);
}

void wrapper_right(void)
{
    ami_right(stdout);
}

void wrapper_rrectf(pfile pfp, int x1, int y1, int x2, int y2, int xs, int ys)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_rrect(f, x1, y1, x2, y2, xs, ys);
}

void wrapper_rrect(int x1, int y1, int x2, int y2, int xs, int ys)
{
    ami_rrect(stdout, x1, y1, x2, y2, xs, ys);
}

void wrapper_scncenf(pfile pfp, long* x, long* y)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tx;
    int ty;
    ami_scncen(f, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scncen(long* x, long* y)
{
    int tx;
    int ty;
    ami_scncen(stdout, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scncengf(pfile pfp, long* x, long* y)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tx;
    int ty;
    ami_scnceng(f, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scnceng(long* x, long* y)
{
    int tx;
    int ty;
    ami_scnceng(stdout, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scnsizf(pfile pfp, long* x, long* y)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tx;
    int ty;
    ami_scnsiz(f, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scnsiz(long* x, long* y)
{
    int tx;
    int ty;
    ami_scnsiz(stdout, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scnsizgf(pfile pfp, long* x, long* y)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tx;
    int ty;
    ami_scnsizg(f, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scnsizg(long* x, long* y)
{
    int tx;
    int ty;
    ami_scnsizg(stdout, &tx, &ty);
    *x = tx;
    *y = ty;
}

void wrapper_scrollf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scroll(f, x, y);
}

void wrapper_scroll(int x, int y)
{
    ami_scroll(stdout, x, y);
}

void wrapper_scrollgf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scrollg(f, x, y);
}

void wrapper_scrollg(int x, int y)
{
    ami_scrollg(stdout, x, y);
}

void wrapper_scrollhorizf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scrollhoriz(f, x1, y1, x2, y2, id);
}

void wrapper_scrollhoriz(int x1, int y1, int x2, int y2, int id)
{
    ami_scrollhoriz(stdout, x1, y1, x2, y2, id);
}

void wrapper_scrollhorizgf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scrollhorizg(f, x1, y1, x2, y2, id);
}

void wrapper_scrollhorizg(int x1, int y1, int x2, int y2, int id)
{
    ami_scrollhorizg(stdout, x1, y1, x2, y2, id);
}

void wrapper_scrollhorizsizf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_scrollhorizsiz(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_scrollhorizsiz(long* w, long* h)
{
    int tw;
    int th;
    ami_scrollhorizsiz(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_scrollhorizsizgf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_scrollhorizsizg(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_scrollhorizsizg(long* w, long* h)
{
    int tw;
    int th;
    ami_scrollhorizsizg(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_scrollposf(pfile pfp, int id, int r)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scrollpos(f, id, r);
}

void wrapper_scrollpos(int id, int r)
{
    ami_scrollpos(stdout, id, r);
}

void wrapper_scrollsizf(pfile pfp, int id, int r)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scrollsiz(f, id, r);
}

void wrapper_scrollsiz(int id, int r)
{
    ami_scrollsiz(stdout, id, r);
}

void wrapper_scrollvertf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scrollvert(f, x1, y1, x2, y2, id);
}

void wrapper_scrollvert(int x1, int y1, int x2, int y2, int id)
{
    ami_scrollvert(stdout, x1, y1, x2, y2, id);
}

void wrapper_scrollvertgf(pfile pfp, int x1, int y1, int x2, int y2, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_scrollvertg(f, x1, y1, x2, y2, id);
}

void wrapper_scrollvertg(int x1, int y1, int x2, int y2, int id)
{
    ami_scrollvertg(stdout, x1, y1, x2, y2, id);
}

void wrapper_scrollvertsizf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_scrollvertsiz(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_scrollvertsiz(long* w, long* h)
{
    int tw;
    int th;
    ami_scrollvertsiz(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_scrollvertsizgf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_scrollvertsizg(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_scrollvertsizg(long* w, long* h)
{
    int tw;
    int th;
    ami_scrollvertsizg(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_selectf(pfile pfp, int u, int d)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_select(f, u, d);
}

void wrapper_select(int u, int d)
{
    ami_select(stdout, u, d);
}

void wrapper_selectwidgetf(pfile pfp, int id, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_selectwidget(f, id, e);
}

void wrapper_selectwidget(int id, int e)
{
    ami_selectwidget(stdout, id, e);
}

void wrapper_setpixelf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_setpixel(f, x, y);
}

void wrapper_setpixel(int x, int y)
{
    ami_setpixel(stdout, x, y);
}

void wrapper_setpointsf(pfile pfp, double ps)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_setpoints(f, (float)ps);
}

void wrapper_setpoints(double ps)
{
    ami_setpoints(stdout, (float)ps);
}

void wrapper_setposf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_setpos(f, x, y);
}

void wrapper_setpos(int x, int y)
{
    ami_setpos(stdout, x, y);
}

void wrapper_setposgf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_setposg(f, x, y);
}

void wrapper_setposg(int x, int y)
{
    ami_setposg(stdout, x, y);
}

void wrapper_setsizf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_setsiz(f, x, y);
}

void wrapper_setsiz(int x, int y)
{
    ami_setsiz(stdout, x, y);
}

void wrapper_setsizgf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_setsizg(f, x, y);
}

void wrapper_setsizg(int x, int y)
{
    ami_setsizg(stdout, x, y);
}

void wrapper_settabf(pfile pfp, int t)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_settab(f, t);
}

void wrapper_settab(int t)
{
    ami_settab(stdout, t);
}

void wrapper_settabgf(pfile pfp, int t)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_settabg(f, t);
}

void wrapper_settabg(int t)
{
    ami_settabg(stdout, t);
}

void wrapper_sizablef(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_sizable(f, e);
}

void wrapper_sizable(int e)
{
    ami_sizable(stdout, e);
}

void wrapper_sizbuff(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_sizbuf(f, x, y);
}

void wrapper_sizbuf(int x, int y)
{
    ami_sizbuf(stdout, x, y);
}

void wrapper_sizbufgf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_sizbufg(f, x, y);
}

void wrapper_sizbufg(int x, int y)
{
    ami_sizbufg(stdout, x, y);
}

void wrapper_sizwidgetf(pfile pfp, int id, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_sizwidget(f, id, x, y);
}

void wrapper_sizwidget(int id, int x, int y)
{
    ami_sizwidget(stdout, id, x, y);
}

void wrapper_sizwidgetgf(pfile pfp, int id, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_sizwidgetg(f, id, x, y);
}

void wrapper_sizwidgetg(int id, int x, int y)
{
    ami_sizwidgetg(stdout, id, x, y);
}

void wrapper_slidehorizf(pfile pfp, int x1, int y1, int x2, int y2, int mark, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_slidehoriz(f, x1, y1, x2, y2, mark, id);
}

void wrapper_slidehoriz(int x1, int y1, int x2, int y2, int mark, int id)
{
    ami_slidehoriz(stdout, x1, y1, x2, y2, mark, id);
}

void wrapper_slidehorizgf(pfile pfp, int x1, int y1, int x2, int y2, int mark, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_slidehorizg(f, x1, y1, x2, y2, mark, id);
}

void wrapper_slidehorizg(int x1, int y1, int x2, int y2, int mark, int id)
{
    ami_slidehorizg(stdout, x1, y1, x2, y2, mark, id);
}

void wrapper_slidehorizsizf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_slidehorizsiz(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_slidehorizsiz(long* w, long* h)
{
    int tw;
    int th;
    ami_slidehorizsiz(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_slidehorizsizgf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_slidehorizsizg(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_slidehorizsizg(long* w, long* h)
{
    int tw;
    int th;
    ami_slidehorizsizg(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_slidevertf(pfile pfp, int x1, int y1, int x2, int y2, int mark, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_slidevert(f, x1, y1, x2, y2, mark, id);
}

void wrapper_slidevert(int x1, int y1, int x2, int y2, int mark, int id)
{
    ami_slidevert(stdout, x1, y1, x2, y2, mark, id);
}

void wrapper_slidevertgf(pfile pfp, int x1, int y1, int x2, int y2, int mark, int id)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_slidevertg(f, x1, y1, x2, y2, mark, id);
}

void wrapper_slidevertg(int x1, int y1, int x2, int y2, int mark, int id)
{
    ami_slidevertg(stdout, x1, y1, x2, y2, mark, id);
}

void wrapper_slidevertsizf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_slidevertsiz(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_slidevertsiz(long* w, long* h)
{
    int tw;
    int th;
    ami_slidevertsiz(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_slidevertsizgf(pfile pfp, long* w, long* h)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    ami_slidevertsizg(f, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_slidevertsizg(long* w, long* h)
{
    int tw;
    int th;
    ami_slidevertsizg(stdout, &tw, &th);
    *w = tw;
    *h = th;
}

void wrapper_standoutf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_standout(f, e);
}

void wrapper_standout(int e)
{
    ami_standout(stdout, e);
}

void wrapper_strikeoutf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_strikeout(f, e);
}

void wrapper_strikeout(int e)
{
    ami_strikeout(stdout, e);
}

void wrapper_subscriptf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_subscript(f, e);
}

void wrapper_subscript(int e)
{
    ami_subscript(stdout, e);
}

void wrapper_superscriptf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_superscript(f, e);
}

void wrapper_superscript(int e)
{
    ami_superscript(stdout, e);
}

void wrapper_sysbarf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_sysbar(f, e);
}

void wrapper_sysbar(int e)
{
    ami_sysbar(stdout, e);
}

void wrapper_tabbarclientf(pfile pfp, int tor, int w, int h, long* cw, long* ch, long* ox, long* oy)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tcw;
    int tch;
    int tox;
    int toy;
    ami_tabbarclient(f, tor, w, h, &tcw, &tch, &tox, &toy);
    *cw = tcw;
    *ch = tch;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabbarclient(int tor, int w, int h, long* cw, long* ch, long* ox, long* oy)
{
    int tcw;
    int tch;
    int tox;
    int toy;
    ami_tabbarclient(stdout, tor, w, h, &tcw, &tch, &tox, &toy);
    *cw = tcw;
    *ch = tch;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabbarclientgf(pfile pfp, int tor, int w, int h, long* cw, long* ch, long* ox, long* oy)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tcw;
    int tch;
    int tox;
    int toy;
    ami_tabbarclientg(f, tor, w, h, &tcw, &tch, &tox, &toy);
    *cw = tcw;
    *ch = tch;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabbarclientg(int tor, int w, int h, long* cw, long* ch, long* ox, long* oy)
{
    int tcw;
    int tch;
    int tox;
    int toy;
    ami_tabbarclientg(stdout, tor, w, h, &tcw, &tch, &tox, &toy);
    *cw = tcw;
    *ch = tch;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabbarsizf(pfile pfp, int tor, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    int tox;
    int toy;
    ami_tabbarsiz(f, tor, cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabbarsiz(int tor, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    int tw;
    int th;
    int tox;
    int toy;
    ami_tabbarsiz(stdout, tor, cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabbarsizgf(pfile pfp, int tor, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    FILE* f = psystem_libcwrfil(pfp);
    int tw;
    int th;
    int tox;
    int toy;
    ami_tabbarsizg(f, tor, cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabbarsizg(int tor, int cw, int ch, long* w, long* h, long* ox, long* oy)
{
    int tw;
    int th;
    int tox;
    int toy;
    ami_tabbarsizg(stdout, tor, cw, ch, &tw, &th, &tox, &toy);
    *w = tw;
    *h = th;
    *ox = tox;
    *oy = toy;
}

void wrapper_tabself(pfile pfp, int id, int tn)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_tabsel(f, id, tn);
}

void wrapper_tabsel(int id, int tn)
{
    ami_tabsel(stdout, id, tn);
}

void wrapper_timerf(pfile pfp, int i, int t, int r)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_timer(f, i, t, r);
}

void wrapper_timer(int i, int t, int r)
{
    ami_timer(stdout, i, t, r);
}

void wrapper_titlef(pfile pfp, string ts, int tsl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_title(f, cstrz(ts, tsl));
}

void wrapper_title(string ts, int tsl)
{
    ami_title(stdout, cstrz(ts, tsl));
}

void wrapper_underlinef(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_underline(f, e);
}

void wrapper_underline(int e)
{
    ami_underline(stdout, e);
}

void wrapper_upf(pfile pfp)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_up(f);
}

void wrapper_up(void)
{
    ami_up(stdout);
}

void wrapper_viewoffgf(pfile pfp, int x, int y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_viewoffg(f, x, y);
}

void wrapper_viewoffg(int x, int y)
{
    ami_viewoffg(stdout, x, y);
}

void wrapper_viewscalef(pfile pfp, double x, double y)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_viewscale(f, (float)x, (float)y);
}

void wrapper_viewscale(double x, double y)
{
    ami_viewscale(stdout, (float)x, (float)y);
}

void wrapper_winclientf(pfile pfp, int cx, int cy, long* wx, long* wy, int ms)
{
    FILE* f = psystem_libcwrfil(pfp);
    int twx;
    int twy;
    ami_winclient(f, cx, cy, &twx, &twy, ms);
    *wx = twx;
    *wy = twy;
}

void wrapper_winclient(int cx, int cy, long* wx, long* wy, int ms)
{
    int twx;
    int twy;
    ami_winclient(stdout, cx, cy, &twx, &twy, ms);
    *wx = twx;
    *wy = twy;
}

void wrapper_winclientgf(pfile pfp, int cx, int cy, long* wx, long* wy, int ms)
{
    FILE* f = psystem_libcwrfil(pfp);
    int twx;
    int twy;
    ami_winclientg(f, cx, cy, &twx, &twy, ms);
    *wx = twx;
    *wy = twy;
}

void wrapper_winclientg(int cx, int cy, long* wx, long* wy, int ms)
{
    int twx;
    int twy;
    ami_winclientg(stdout, cx, cy, &twx, &twy, ms);
    *wx = twx;
    *wy = twy;
}

void wrapper_writejustf(pfile pfp, string s, int sl, int n)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_writejust(f, cstrz(s, sl), n);
}

void wrapper_writejust(string s, int sl, int n)
{
    ami_writejust(stdout, cstrz(s, sl), n);
}

void wrapper_wrtstrf(pfile pfp, string s, int sl)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_wrtstrn(f, s, sl);
}

void wrapper_wrtstr(string s, int sl)
{
    ami_wrtstrn(stdout, s, sl);
}

void wrapper_wrtstrnf(pfile pfp, string s, int sl, int n)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_wrtstrn(f, cstrz(s, sl), n);
}

void wrapper_wrtstrn(string s, int sl, int n)
{
    ami_wrtstrn(stdout, cstrz(s, sl), n);
}

void wrapper_xboldf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_xbold(f, e);
}

void wrapper_xbold(int e)
{
    ami_xbold(stdout, e);
}

void wrapper_xlightf(pfile pfp, int e)
{
    FILE* f = psystem_libcwrfil(pfp);
    ami_xlight(f, e);
}

void wrapper_xlight(int e)
{
    ami_xlight(stdout, e);
}

