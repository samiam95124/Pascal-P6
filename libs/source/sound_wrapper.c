/* Generated sound wrappers. Do not edit by hand.
 *
 * Compiled against plain glibc stdio (no STDIO_BYPASS): the sound
 * library lives in the glibc world. Only the string conversion
 * helpers are shared with the bypass world; they touch no FILE.
 */

#include <sound.h>

extern char* cstrz(char* s, int l); /* support.o: trim pad + terminate */

void wrapper_starttimeout(void)
{
    ami_starttimeout();
}

void wrapper_stoptimeout(void)
{
    ami_stoptimeout();
}

ami_long wrapper_curtimeout(void)
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

ami_long wrapper_curtimein(void)
{
    return ami_curtimein();
}

ami_long wrapper_synthout(void)
{
    return ami_synthout();
}

ami_long wrapper_synthin(void)
{
    return ami_synthin();
}

void wrapper_opensynthout(ami_long p)
{
    ami_opensynthout(p);
}

void wrapper_closesynthout(ami_long p)
{
    ami_closesynthout(p);
}

void wrapper_opensynthin(ami_long p)
{
    ami_opensynthin(p);
}

void wrapper_closesynthin(ami_long p)
{
    ami_closesynthin(p);
}

void wrapper_noteon(ami_long p, ami_long t, ami_long c, ami_long n, ami_long v)
{
    ami_noteon(p, t, c, n, v);
}

void wrapper_noteoff(ami_long p, ami_long t, ami_long c, ami_long n, ami_long v)
{
    ami_noteoff(p, t, c, n, v);
}

void wrapper_instchange(ami_long p, ami_long t, ami_long c, ami_long i)
{
    ami_instchange(p, t, c, i);
}

void wrapper_attack(ami_long p, ami_long t, ami_long c, ami_long at)
{
    ami_attack(p, t, c, at);
}

void wrapper_release(ami_long p, ami_long t, ami_long c, ami_long rt)
{
    ami_release(p, t, c, rt);
}

void wrapper_legato(ami_long p, ami_long t, ami_long c, ami_long b)
{
    ami_legato(p, t, c, b);
}

void wrapper_portamento(ami_long p, ami_long t, ami_long c, ami_long b)
{
    ami_portamento(p, t, c, b);
}

void wrapper_vibrato(ami_long p, ami_long t, ami_long c, ami_long v)
{
    ami_vibrato(p, t, c, v);
}

void wrapper_volsynthchan(ami_long p, ami_long t, ami_long c, ami_long v)
{
    ami_volsynthchan(p, t, c, v);
}

void wrapper_porttime(ami_long p, ami_long t, ami_long c, ami_long v)
{
    ami_porttime(p, t, c, v);
}

void wrapper_balance(ami_long p, ami_long t, ami_long c, ami_long b)
{
    ami_balance(p, t, c, b);
}

void wrapper_pan(ami_long p, ami_long t, ami_long c, ami_long b)
{
    ami_pan(p, t, c, b);
}

void wrapper_timbre(ami_long p, ami_long t, ami_long c, ami_long tb)
{
    ami_timbre(p, t, c, tb);
}

void wrapper_brightness(ami_long p, ami_long t, ami_long c, ami_long b)
{
    ami_brightness(p, t, c, b);
}

void wrapper_reverb(ami_long p, ami_long t, ami_long c, ami_long r)
{
    ami_reverb(p, t, c, r);
}

void wrapper_tremulo(ami_long p, ami_long t, ami_long c, ami_long tr)
{
    ami_tremulo(p, t, c, tr);
}

void wrapper_chorus(ami_long p, ami_long t, ami_long c, ami_long cr)
{
    ami_chorus(p, t, c, cr);
}

void wrapper_celeste(ami_long p, ami_long t, ami_long c, ami_long ce)
{
    ami_celeste(p, t, c, ce);
}

void wrapper_phaser(ami_long p, ami_long t, ami_long c, ami_long ph)
{
    ami_phaser(p, t, c, ph);
}

void wrapper_aftertouch(ami_long p, ami_long t, ami_long c, ami_long n, ami_long at)
{
    ami_aftertouch(p, t, c, n, at);
}

void wrapper_pressure(ami_long p, ami_long t, ami_long c, ami_long pr)
{
    ami_pressure(p, t, c, pr);
}

void wrapper_pitch(ami_long p, ami_long t, ami_long c, ami_long pt)
{
    ami_pitch(p, t, c, pt);
}

void wrapper_pitchrange(ami_long p, ami_long t, ami_long c, ami_long v)
{
    ami_pitchrange(p, t, c, v);
}

void wrapper_mono(ami_long p, ami_long t, ami_long c, ami_long ch)
{
    ami_mono(p, t, c, ch);
}

void wrapper_poly(ami_long p, ami_long t, ami_long c)
{
    ami_poly(p, t, c);
}

void wrapper_loadsynth(ami_long s, char* sf, int sfl)
{
    ami_loadsynth(s, cstrz(sf, sfl));
}

void wrapper_playsynth(ami_long p, ami_long t, ami_long s)
{
    ami_playsynth(p, t, s);
}

void wrapper_delsynth(ami_long s)
{
    ami_delsynth(s);
}

void wrapper_waitsynth(ami_long p)
{
    ami_waitsynth(p);
}

ami_long wrapper_waveout(void)
{
    return ami_waveout();
}

ami_long wrapper_wavein(void)
{
    return ami_wavein();
}

void wrapper_openwaveout(ami_long p)
{
    ami_openwaveout(p);
}

void wrapper_closewaveout(ami_long p)
{
    ami_closewaveout(p);
}

void wrapper_loadwave(ami_long w, char* fn, int fnl)
{
    ami_loadwave(w, cstrz(fn, fnl));
}

void wrapper_playwave(ami_long p, ami_long t, ami_long w)
{
    ami_playwave(p, t, w);
}

void wrapper_delwave(ami_long w)
{
    ami_delwave(w);
}

void wrapper_volwave(ami_long p, ami_long t, ami_long v)
{
    ami_volwave(p, t, v);
}

void wrapper_waitwave(ami_long p)
{
    ami_waitwave(p);
}

void wrapper_chanwaveout(ami_long p, ami_long c)
{
    ami_chanwaveout(p, c);
}

void wrapper_ratewaveout(ami_long p, ami_long r)
{
    ami_ratewaveout(p, r);
}

void wrapper_lenwaveout(ami_long p, ami_long l)
{
    ami_lenwaveout(p, l);
}

void wrapper_sgnwaveout(ami_long p, ami_long s)
{
    ami_sgnwaveout(p, s);
}

void wrapper_fltwaveout(ami_long p, ami_long f)
{
    ami_fltwaveout(p, f);
}

void wrapper_endwaveout(ami_long p, ami_long e)
{
    ami_endwaveout(p, e);
}

void wrapper_wrwave(ami_long p, char* buff, int buffl)
{
    ami_wrwave(p, (void*)buff, buffl);
}

void wrapper_openwavein(ami_long p)
{
    ami_openwavein(p);
}

void wrapper_closewavein(ami_long p)
{
    ami_closewavein(p);
}

ami_long wrapper_chanwavein(ami_long p)
{
    return ami_chanwavein(p);
}

ami_long wrapper_ratewavein(ami_long p)
{
    return ami_ratewavein(p);
}

ami_long wrapper_lenwavein(ami_long p)
{
    return ami_lenwavein(p);
}

ami_long wrapper_sgnwavein(ami_long p)
{
    return ami_sgnwavein(p);
}

ami_long wrapper_endwavein(ami_long p)
{
    return ami_endwavein(p);
}

ami_long wrapper_fltwavein(ami_long p)
{
    return ami_fltwavein(p);
}

ami_long wrapper_rdwave(ami_long p, char* buff, int buffl)
{
    return ami_rdwave(p, (void*)buff, buffl);
}

void wrapper_synthoutname(ami_long p, char* name, int namel)
{
    ami_synthoutname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

void wrapper_synthinname(ami_long p, char* name, int namel)
{
    ami_synthinname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

void wrapper_waveoutname(ami_long p, char* name, int namel)
{
    ami_waveoutname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

void wrapper_waveinname(ami_long p, char* name, int namel)
{
    ami_waveinname(p, name, namel);
    { int _p = 0; while (_p < namel && name[_p]) _p++;
      while (_p < namel) name[_p++] = ' '; }
}

ami_long wrapper_setparamsynthin(ami_long p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamsynthin(p, cstrz(name, namel), cstrz(value, valuel));
}

ami_long wrapper_setparamsynthout(ami_long p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamsynthout(p, cstrz(name, namel), cstrz(value, valuel));
}

ami_long wrapper_setparamwavein(ami_long p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamwavein(p, cstrz(name, namel), cstrz(value, valuel));
}

ami_long wrapper_setparamwaveout(ami_long p, char* name, int namel, char* value, int valuel)
{
    return ami_setparamwaveout(p, cstrz(name, namel), cstrz(value, valuel));
}

void wrapper_getparamsynthin(ami_long p, char* name, int namel, char* value, int valuel)
{
    ami_getparamsynthin(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

void wrapper_getparamsynthout(ami_long p, char* name, int namel, char* value, int valuel)
{
    ami_getparamsynthout(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

void wrapper_getparamwavein(ami_long p, char* name, int namel, char* value, int valuel)
{
    ami_getparamwavein(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

void wrapper_getparamwaveout(ami_long p, char* name, int namel, char* value, int valuel)
{
    ami_getparamwaveout(p, cstrz(name, namel), value, valuel);
    { int _p = 0; while (_p < valuel && value[_p]) _p++;
      while (_p < valuel) value[_p++] = ' '; }
}

