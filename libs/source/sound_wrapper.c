/* Generated sound wrappers. Do not edit by hand.
 *
 * Compiled against plain glibc stdio (no STDIO_BYPASS): the sound
 * library lives in the glibc world. Only the string conversion
 * helpers are shared with the bypass world; they touch no FILE.
 */

#include <sound.h>

extern char* cstrz(char* s, int l); /* support.o: trim pad + terminate */
/* sound.h declares this one with a stray s (getparamswaveout) */
void ami_getparamwaveout(int p, string name, string value, int len);

void wrapper_starttimeout(void)
{
    ami_starttimeout();
}

void wrapper_stoptimeout(void)
{
    ami_stoptimeout();
}

int wrapper_curtimeout(void)
{
    return ami_curtimeout();
}

void wrapper_starttimein(void)
{
    ami_starttimein();
}

void wrapper_stoptimein(void)
{
    ami_stoptimein();
}

int wrapper_curtimein(void)
{
    return ami_curtimein();
}

int wrapper_synthout(void)
{
    return ami_synthout();
}

int wrapper_synthin(void)
{
    return ami_synthin();
}

void wrapper_opensynthout(int p)
{
    ami_opensynthout(p);
}

void wrapper_closesynthout(int p)
{
    ami_closesynthout(p);
}

void wrapper_opensynthin(int p)
{
    ami_opensynthin(p);
}

void wrapper_closesynthin(int p)
{
    ami_closesynthin(p);
}

void wrapper_noteon(int p, int t, int c, int n, int v)
{
    ami_noteon(p, t, c, n, v);
}

void wrapper_noteoff(int p, int t, int c, int n, int v)
{
    ami_noteoff(p, t, c, n, v);
}

void wrapper_instchange(int p, int t, int c, int i)
{
    ami_instchange(p, t, c, i);
}

void wrapper_attack(int p, int t, int c, int at)
{
    ami_attack(p, t, c, at);
}

void wrapper_release(int p, int t, int c, int rt)
{
    ami_release(p, t, c, rt);
}

void wrapper_legato(int p, int t, int c, int b)
{
    ami_legato(p, t, c, b);
}

void wrapper_portamento(int p, int t, int c, int b)
{
    ami_portamento(p, t, c, b);
}

void wrapper_vibrato(int p, int t, int c, int v)
{
    ami_vibrato(p, t, c, v);
}

void wrapper_volsynthchan(int p, int t, int c, int v)
{
    ami_volsynthchan(p, t, c, v);
}

void wrapper_porttime(int p, int t, int c, int v)
{
    ami_porttime(p, t, c, v);
}

void wrapper_balance(int p, int t, int c, int b)
{
    ami_balance(p, t, c, b);
}

void wrapper_pan(int p, int t, int c, int b)
{
    ami_pan(p, t, c, b);
}

void wrapper_timbre(int p, int t, int c, int tb)
{
    ami_timbre(p, t, c, tb);
}

void wrapper_brightness(int p, int t, int c, int b)
{
    ami_brightness(p, t, c, b);
}

void wrapper_reverb(int p, int t, int c, int r)
{
    ami_reverb(p, t, c, r);
}

void wrapper_tremulo(int p, int t, int c, int tr)
{
    ami_tremulo(p, t, c, tr);
}

void wrapper_chorus(int p, int t, int c, int cr)
{
    ami_chorus(p, t, c, cr);
}

void wrapper_celeste(int p, int t, int c, int ce)
{
    ami_celeste(p, t, c, ce);
}

void wrapper_phaser(int p, int t, int c, int ph)
{
    ami_phaser(p, t, c, ph);
}

void wrapper_aftertouch(int p, int t, int c, int n, int at)
{
    ami_aftertouch(p, t, c, n, at);
}

void wrapper_pressure(int p, int t, int c, int pr)
{
    ami_pressure(p, t, c, pr);
}

void wrapper_pitch(int p, int t, int c, int pt)
{
    ami_pitch(p, t, c, pt);
}

void wrapper_pitchrange(int p, int t, int c, int v)
{
    ami_pitchrange(p, t, c, v);
}

void wrapper_mono(int p, int t, int c, int ch)
{
    ami_mono(p, t, c, ch);
}

void wrapper_poly(int p, int t, int c)
{
    ami_poly(p, t, c);
}

void wrapper_loadsynth(int s, char* sf, int sfl)
{
    ami_loadsynth(s, cstrz(sf, sfl));
}

void wrapper_playsynth(int p, int t, int s)
{
    ami_playsynth(p, t, s);
}

void wrapper_delsynth(int s)
{
    ami_delsynth(s);
}

void wrapper_waitsynth(int p)
{
    ami_waitsynth(p);
}

int wrapper_waveout(void)
{
    return ami_waveout();
}

int wrapper_wavein(void)
{
    return ami_wavein();
}

void wrapper_openwaveout(int p)
{
    ami_openwaveout(p);
}

void wrapper_closewaveout(int p)
{
    ami_closewaveout(p);
}

void wrapper_loadwave(int w, char* fn, int fnl)
{
    ami_loadwave(w, cstrz(fn, fnl));
}

void wrapper_playwave(int p, int t, int w)
{
    ami_playwave(p, t, w);
}

void wrapper_delwave(int w)
{
    ami_delwave(w);
}

void wrapper_volwave(int p, int t, int v)
{
    ami_volwave(p, t, v);
}

void wrapper_waitwave(int p)
{
    ami_waitwave(p);
}

void wrapper_chanwaveout(int p, int c)
{
    ami_chanwaveout(p, c);
}

void wrapper_ratewaveout(int p, int r)
{
    ami_ratewaveout(p, r);
}

void wrapper_lenwaveout(int p, int l)
{
    ami_lenwaveout(p, l);
}

void wrapper_sgnwaveout(int p, int s)
{
    ami_sgnwaveout(p, s);
}

void wrapper_fltwaveout(int p, int f)
{
    ami_fltwaveout(p, f);
}

void wrapper_endwaveout(int p, int e)
{
    ami_endwaveout(p, e);
}

void wrapper_wrwave(int p, char* buff, int buffl)
{
    ami_wrwave(p, (void*)buff, buffl);
}

void wrapper_openwavein(int p)
{
    ami_openwavein(p);
}

void wrapper_closewavein(int p)
{
    ami_closewavein(p);
}

int wrapper_chanwavein(int p)
{
    return ami_chanwavein(p);
}

int wrapper_ratewavein(int p)
{
    return ami_ratewavein(p);
}

int wrapper_lenwavein(int p)
{
    return ami_lenwavein(p);
}

int wrapper_sgnwavein(int p)
{
    return ami_sgnwavein(p);
}

int wrapper_endwavein(int p)
{
    return ami_endwavein(p);
}

int wrapper_fltwavein(int p)
{
    return ami_fltwavein(p);
}

int wrapper_rdwave(int p, char* buff, int buffl)
{
    return ami_rdwave(p, (void*)buff, buffl);
}

void wrapper_synthoutname(int p, char* name, int namel)
{
    ami_synthoutname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

void wrapper_synthinname(int p, char* name, int namel)
{
    ami_synthinname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

void wrapper_waveoutname(int p, char* name, int namel)
{
    ami_waveoutname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

void wrapper_waveinname(int p, char* name, int namel)
{
    ami_waveinname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

int wrapper_setparamsynthin(int p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamsynthin(p, cstrz(name, namel), cstrz(value, valuel));
}

int wrapper_setparamsynthout(int p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamsynthout(p, cstrz(name, namel), cstrz(value, valuel));
}

int wrapper_setparamwavein(int p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamwavein(p, cstrz(name, namel), cstrz(value, valuel));
}

int wrapper_setparamwaveout(int p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamwaveout(p, cstrz(name, namel), cstrz(value, valuel));
}

void wrapper_getparamsynthin(int p, char* name, int namel, char* value, int valuel)
{
    ami_getparamsynthin(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

void wrapper_getparamsynthout(int p, char* name, int namel, char* value, int valuel)
{
    ami_getparamsynthout(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

void wrapper_getparamwavein(int p, char* name, int namel, char* value, int valuel)
{
    ami_getparamwavein(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

void wrapper_getparamwaveout(int p, char* name, int namel, char* value, int valuel)
{
    ami_getparamwaveout(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

