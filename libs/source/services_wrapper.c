/*******************************************************************************
*                                                                              *
*                     Services library wrapper for Pascaline                   *
*                                                                              *
*                              Created 2025                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Contains a wrapper function for each Pascaline function in the services      *
* library. Each wrapper takes the C equivalent of parameters in the Pascaline  *
* version of the library, Then these are translated to/from the C library.     *
*                                                                              *
* The Pascaline side calls are compatible with the operating system ABI, or    *
* are mde so by services_wrapper.asm. It also handles spreading of the         *
* overloads.                                                                   *
*                                                                              *
* There are many cases where no conversion is required. In this case, the      *
* wrapper contains a single call to the serices library. The C compiler        *
* redunces this to just a jump, even in the presence of parameters and/or      *
* return values. Thus the wrapper overhead is quite low.                       *
*                                                                              *
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <services.h>
#include <services_wrapper.h>

/********************************************************************************

procedure list(view f: string; var  l: filptr);
->
void pa_listl(char* f, int l, pa_filrec **lp);

********************************************************************************/

void wrapper_list(
    /** filename */  string fn, int fnl,
    /** file list */ filptr* fl
)

{

    /* get files list in C form */
    pa_listl(fn, fnl, fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline(fl);

}

/********************************************************************************

procedure list(view f: pstring; var  l: filptr);
->
extern void pa_listl(char* f, int l, pa_filrec **lp);

********************************************************************************/

void wrapper_listp(
    /** filename */  pstring fn,
    /** file list */ filptr* fl
)

{

    /** string pointer */ string s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */
    /* get files list in C form */
    pa_listl(s, l, fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline(fl);

}

/********************************************************************************

procedure times(out s: string; t: integer);
->
void pa_times(char* s, int sl, int t);

********************************************************************************/

void wrapper_times(
    /** string pointer */ string s,
    /** string length */  int l,
    /** time          */  long t
)

{

    pa_times(s, l, t); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function times(t: integer): pstring;
->
void pa_times(char* s, int sl, int t);

********************************************************************************/

pstring wrapper_timesp(
    /** time          */  long t
)

{

    char buff[BUFLEN];

    pa_times(buff, BUFLEN, t); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}


/********************************************************************************

procedure dates(var s: string; t: integer);
->
pa_dates(char* s, int sl, int t);

********************************************************************************/

void wrapper_dates(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** time          */  long t
)

{

    pa_dates(s, l, t); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function dates(t: integer): pstring;
->
pa_dates(char* s, int sl, int t);

********************************************************************************/

pstring wrapper_datesp(
    /** time          */  long t
)

{

    char buff[BUFLEN];

    pa_dates(buff, BUFLEN, t); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure writetime(var f: text; t: integer);
->
void pa_writetime(FILE *f, int t);

********************************************************************************/

void wrapper_writetimef(
    /** file to write */ pfile pfp,
    /** time */          long t
)

{

    FILE* fp; /* libc file pointer */

    fp = psystem_libcwrfil(pfp); /* find libc compatible file */
    pa_writetime(fp, t);

}

/********************************************************************************

procedure writetime(var f: text; t: integer);
->
void pa_writetime(FILE *f, int t);

********************************************************************************/

void wrapper_writetime(
    /** time */          long t
)

{

    pa_writetime(stdout, t);

}

/********************************************************************************

procedure writedatef(var f: text; t: integer);
->
void pa_writedate(FILE *f, int t);

********************************************************************************/

void wrapper_writedatef(
    /** file to write */ pfile pfp,
    /** time */          long t
)

{

    FILE* fp; /* libc file pointer */

    fp = psystem_libcwrfil(pfp); /* find libc compatible file */
    pa_writedate(fp, t);

}

/********************************************************************************

procedure writedate(var f: text; t: integer);
->
void pa_writedate(FILE *f, int t);

********************************************************************************/

void wrapper_writedate(
    /** time */ long t
)

{

    pa_writedate(stdout, t);

}

/********************************************************************************

function time: integer;
->
long pa_time(void);

********************************************************************************/

long wrapper_time(void)

{

    return (pa_time());

}

/********************************************************************************

function local(t: integer): integer;
->
long pa_local(long t);

********************************************************************************/

long wrapper_local(
    /** time */ long t
)

{

    return (pa_local(t));

}

/********************************************************************************

function clock: integer;
->
long pa_clock(void);

********************************************************************************/

long wrapper_clock(void)

{

    return (pa_clock());

}

/********************************************************************************

function elapsed(r: integer): integer;
->
long pa_elapsed(long r);

********************************************************************************/

long wrapper_elapsed(
    /** time */ long t
)

{

    return (pa_elapsed(t));

}

/********************************************************************************

function validfile(view s: string): boolean;
->
int  pa_validfilel(char* s, int l);

********************************************************************************/

int wrapper_validfile(
    /** string pointer */ char* s,
    /** string length */  int l,
)

{

    return (pa_validfilel(s, l));

}

/********************************************************************************

function validfile(view s: pstring): boolean;
->
int  pa_validfilel(char* s, int l);

********************************************************************************/

int wrapper_validfilep(
    /** filename */  pstring fn,
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    return(pa_validfilel(s, l));

}

/********************************************************************************

function validpath(view s: string): boolean;
->
int  pa_validpathl(char* s, int l);

********************************************************************************/

int wrapper_validpath(
    /** string pointer */ char* s,
    /** string length */  int l,
)

{

    return (pa_validpathl(s, l));

}

/********************************************************************************

function validpath(view s: pstring): boolean;
->
int  pa_validpathl(char* s, int l);

********************************************************************************/

int wrapper_validpathp(
    /** filename */  pstring fn,
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    return(pa_validpathl(s, l));

}

/********************************************************************************

function wild(view s: string): boolean;
->
int  pa_wildl(char* s, int l);

********************************************************************************/

int wrapper_wild(
    /** string pointer */ char* s,
    /** string length */  int l,
)

{

    return (pa_wildl(s, l));

}

/********************************************************************************

function wild(view s: pstring): boolean;
->
int  pa_wildl(char* s, int l);

********************************************************************************/

int wrapper_wildp(
    /** filename */  pstring fn,
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    return(pa_wildl(s, l));

}

/********************************************************************************

procedure getenv(view ls: string; out ds: string);
->
void pa_getenvl(char* ls, int lsl, char* ds, int dsl);

********************************************************************************/

void wrapper_getenv(
    /** variable name */ string  s, int sl,
    /** variable value */ string d, int dl
)

{

    pa_getenvl(s, sl, d, dl); /* find environment string */
    cstr2pad(d, dl); /* convert output to padded */

}

/********************************************************************************

function getenv(view ls: string): pstring;
->
void pa_getenvl(char* ls, int lsl, char* ds, int dsl);

********************************************************************************/

pstring wrapper_getenvp(
    /** variable name */ string  s, int sl
)

{

    char buff[BUFLEN];

    pa_getenvl(s, sl, buff, BUFLEN); /* find environment string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure setenv(view sn, sd: string);
->
void pa_setenvl(char* sn, int snl, char* sd, int sdl);

********************************************************************************/

void wrapper_setenv(
    /** name */ string n, int nl,
    /** variable */ string v, int vl
)

{

    pa_setenvl(n, nl, v, vl)

}

/********************************************************************************

procedure setenv(sn: pstring; view sd: string);
->
void pa_setenvl(char* sn, int snl, char* sd, int sdl);

********************************************************************************/

void wrapper_setenvps(
    /** name */ pstring n,
    /** variable */ string v, int vl
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(n, &s, &l); /* get cstr/len from pstring */

    pa_setenvl(s, l, v, vl);

}

/********************************************************************************

procedure setenv(view sn: string; sd: pstring);
->
void pa_setenvl(char* sn, int snl, char* sd, int sdl);

********************************************************************************/

void wrapper_setenvsp(
    /** name */     string n, int nl,
    /** variable */ pstring v
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(v, &s, &l); /* get cstr/len from pstring */

    pa_setenvl(n, nl, s, l);

}

/********************************************************************************

procedure setenv(sn, sd: pstring);
->
void pa_setenvl(char* sn, int snl, char* sd, int sdl);

********************************************************************************/

void wrapper_setenvpp(
    /** name */     pstring n,
    /** variable */ pstring v
)

{

    /** string pointer */ char* ns;
    /** string length */  int nl;
    /** string pointer */ char* vs;
    /** string length */  int vl;

    pstr2cstrl(n, &ns, &nl); /* get cstr/len from pstring */
    pstr2cstrl(v, &vs, &vl); /* get cstr/len from pstring */

    pa_setenvl(ns, nl, vs, vl);

}

/********************************************************************************

procedure allenv(var el: envptr);
->
void pa_allenv(pa_envrec **el);

********************************************************************************/

void allenv(pa_envrec** el)

{

    pa_allenv(el);
    cenvlist2pascaline(*el);

}

/********************************************************************************

procedure remenv(view sn: string);
->
void pa_remenvl(char* sn, int snl);

********************************************************************************/

void wrapper_remenv(
    /** string pointer */ char* s,
    /** string length */  int l,
)

{

    pa_remenvl(s, l);

}

/********************************************************************************

procedure remenv(view sn: pstring);
->
extern void pa_remenvl(char* sn, int snl);

********************************************************************************/

void wrapper_remenvp(
    /** name */  pstring n,
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(n, &s, &l); /* get cstr/len from pstring */

    pa_remenvl(s, l);

}

/********************************************************************************

procedure exec(view cmd: string);
->
void pa_execl(char* cmd, int cmdl);

********************************************************************************/

void wrapper_exec(
    /** string pointer */ char* s,
    /** string length */  int l,
)

{

    pa_execl(s, l);

}

/********************************************************************************

procedure exec(cmd: pstring);
->
void pa_execl(char* cmd, int cmdl);

********************************************************************************/

void wrapper_execp(
    /** filename */  pstring fn,
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    pa_execl(s, l);

}

/********************************************************************************

procedure exece(view cmd: string; el: envptr);
->
void pa_execel(char* cmd, int cmdl, pa_envrec *el);

********************************************************************************/

void wrapper_exece(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** environment */    pa_envptr el
)

{

    cenvlist2c(el); /* convert environment to C form */
    pa_execel(s, l, el); /* execute */

}

/********************************************************************************

procedure exece(cmd: pstring; el: envptr);
->
void pa_execel(char* cmd, int cmdl, pa_envrec *el);

********************************************************************************/

void wrapper_execep(
    /** filename */    pstring fn,
    /** environment */ pa_envptr el
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    cenvlist2c(el); /* convert environment to C form */
    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    pa_execel(s, l, el);

}

/********************************************************************************

procedure execw(view cmd: string; out e: integer);
->
void pa_execwl(char* cmd, int cmdl, int *e);

********************************************************************************/

void wrapper_execw(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** error */          int *e
)

{

    pa_execwl(s, l, e);

}

/********************************************************************************

procedure execw(cmd: pstring; out e: integer);
->
void pa_execwl(char* cmd, int cmdl, int *e);

********************************************************************************/

void wrapper_execwp(
    /** filename */    pstring fn,
    /** environment */ pa_envptr el,
    /** error */       int *e
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    pa_execwl(s, l, e);

}

/********************************************************************************

procedure execew(view cmd: string; el: envptr; out e: integer);
->
void pa_execewl(char* cmd, int cmdl, pa_envrec *el, int *e);

********************************************************************************/

void wrapper_execew(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** environment */    pa_envptr el,
    /** error */          int *e
)

{

    cenvlist2c(el); /* convert environment to C form */
    pa_execewl(s, l, el, e);

}

/********************************************************************************

procedure execew(cmd: pstring; el: envptr; out e: integer);
->
void pa_execewl(char* cmd, int cmdl, pa_envrec *el, int *e);

********************************************************************************/

void wrapper_execwp(
    /** filename */    pstring fn,
    /** environment */ pa_envptr el,
    /** error */       int *e
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    cenvlist2c(el); /* convert environment to C form */
    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    pa_execewl(s, l, el, e);

}

/********************************************************************************

procedure getcur(out fn: string);
->
void pa_getcur(char* fn, int l);

********************************************************************************/

void wrapper_getcur(
    /** string pointer */ string s,
    /** string length */  int l,
)

{

    pa_getcur(s, l); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function getcur: pstring;
->
void pa_getcur(char* fn, int l);

********************************************************************************/

pstring wrapper_getcurp(void)

{

    char buff[BUFLEN];

    pa_getcur(buff, BUFLEN); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure setcur(view fn: string);
->
void pa_setcurl(char* fn, int fnl);

********************************************************************************/

int wrapper_setcur(
    /** string pointer */ char* s,
    /** string length */  int l,
)

{

    return (pa_setcurl(s, l));

}

/********************************************************************************

procedure setcur(fn: pstring);
->
void pa_setcurl(char* fn, int fnl);

********************************************************************************/

int wrapper_setcurp(
    /** filename */  pstring fn,
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    return(pa_setcurl(s, l));

}

/********************************************************************************

procedure brknam(view fn: string; out p, n, e: string);
->
void pa_brknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

void wrapper_brknam(
    /* filename */  char* fn, int fnl,
    /* path */      char* p, int pl,
    /* name */      char* n, int nl,
    /* extension */ char*e, int el
)

{

    /* execute subfunction */
    pa_brknaml(fn, fnl, p, pl, n, nl, e, el);
    /* convert output cstrings to padded pascaline */
    cstr2pad(p, pl);
    cstr2pad(n, nl);
    cstr2pad(e, el);

}

/********************************************************************************

procedure brknam(view fn: string; out p, n, e: pstring);
->
void pa_brknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

void wrapper_brknamp(
    /* filename */  char* fn, int fnl,
    /* path */      pstring p,
    /* name */      pstring n,
    /* extension */ pstring e
)

{

    int pl, nl, el; /* lengths */

    cpstrp2cstrl(&p, &pl); /* convert pstring arguments to cstr/len */
    cpstrp2cstrl(&n, &nl);
    cpstrp2cstrl(&e, &el);
    /* execute subfunction */
    pa_brknaml(fn, fnl, p, pl, n, nl, e, el);
    cstr2pad(p, pl);
    cstr2pad(n, nl);
    cstr2pad(e, el);

}

/********************************************************************************

procedure brknam(fn: pstring; out p, n, e: pstring);
->
void pa_brknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

void wrapper_brknamp(
    /* filename */  pstring fn,
    /* path */      pstring p,
    /* name */      pstring n,
    /* extension */ pstring e
)

{

    int fnl, pl, nl, el; /* lengths */

    cpstrp2cstrl(&fn, &fnl); /* convert pstring arguments to cstr/len */
    cpstrp2cstrl(&p, &pl);
    cpstrp2cstrl(&n, &nl);
    cpstrp2cstrl(&e, &el);
    /* execute subfunction */
    pa_brknaml(fn, fnl, p, pl, n, nl, e, el);
    cstr2pad(p, pl);
    cstr2pad(n, nl);
    cstr2pad(e, el);

}

/********************************************************************************

procedure maknam(out fn: string; view p, n, e: string);
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

void wrapper_maknam(
    /** string pointer */ string fn,
    /** string length */  int fnl,
    /** string pointer */ string p,
    /** string length */  int pl,
    /** string pointer */ string n,
    /** string length */  int nl,
    /** string pointer */ string e,
    /** string length */  int el
)

{

    pa_maknaml(fn, fnl, p, pl, n, nl, e, el);
    cstr2pad(fn, fnl); /* convert output to padded */

}

/********************************************************************************

function maknam(view p, n, e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamp(
    /** string pointer */ string fn,
    /** string length */  int fnl,
    /** string pointer */ string p,
    /** string length */  int pl,
    /** string pointer */ string n,
    /** string length */  int nl,
    /** string pointer */ string e,
    /** string length */  int el
)

{

    char buff[BUFLEN];

    pa_maknaml(buff, BUFLEN, p, pl, n, nl, e, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; view n: string; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknampspp(
    /** string pointer */ pstring p,
    /** string pointer */ string n,
    /** string length */  int nl,
    /** string pointer */ pstring e
)

{

    string ps;
    int    pl;
    string es;
    int    el;
    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pstr2cstrl(e, &es, &el); /* get cstr/len from pstring */
    pa_maknaml(buff1, BUFLEN, p, pl, n, nl, es, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; n: pstring; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamppps(
    /** string pointer */ pstring p,
    /** string pointer */ pstring n,
    /** string pointer */ string  e,
    /** string length */  int     el
)

{

    string ps;
    int    pl;
    string ns;
    int    nl;
    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pstr2cstrl(n, &ns, &nl); /* get cstr/len from pstring */
    pa_maknaml(buff1, BUFLEN, ps, pl, ns, nl, e, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; n: pstring; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamppps(
    /** string pointer */ string  p,
    /** string length */  int     pl,
    /** string pointer */ pstring n,
    /** string pointer */ pstring e,
)

{

    string ns;
    int    nl;
    string es;
    int    el;
    char buff[BUFLEN];

    pstr2cstrl(n, &ns, &nl); /* get cstr/len from pstring */
    pstr2cstrl(e, &es, &el); /* get cstr/len from pstring */
    pa_maknaml(buff1, BUFLEN, p, pl, ns, nl, es, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; view n: string; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamppss(
    /** string pointer */ pstring  p,
    /** string pointer */ pstring n,
    /** string length */  int     nl,
    /** string pointer */ pstring e,
    /** string length */  int     el
)

{

    string ps;
    int    pl;
    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pa_maknaml(buff1, BUFLEN, ps, pl, n, nl, e, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; view n: string; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamppsp(
    /** string pointer */ pstring p,
    /** string pointer */ string  n,
    /** string length */  int     nl,
    /** string pointer */ pstring e
)

{

    string ps;
    int    ll;
    string es;
    int    el;
    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pstr2cstrl(e, &es, &el); /* get cstr/len from pstring */
    pa_maknaml(buff, BUFLEN, ps, pl, n, nl, es, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; n: pstring; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamppps(
    /** string pointer */ pstring p,
    /** string pointer */ pstring n,
    /** string pointer */ pstring e,
    /** string length */  int     el,
)

{

    string ps;
    int    pl;
    string ns;
    int    nl;
    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pstr2cstrl(n, &ns, &nl); /* get cstr/len from pstring */
    pa_maknaml(buff, BUFLEN, ps, pl, ns, nl, e, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; n: pstring; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamppps(
    /** string pointer */ pstring p,
    /** string pointer */ pstring n,
    /** string pointer */ pstring e
)

{

    string ps;
    int    pl;
    string ns;
    int    nl;
    string es;
    int    el;

    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pstr2cstrl(n, &ns, &nl); /* get cstr/len from pstring */
    pstr2cstrl(e, &es, &el); /* get cstr/len from pstring */
    pa_maknaml(buff, BUFLEN, ps, pl, ns, nl, es, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure fulnam(var fn: string);
->
void pa_fulnam(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

function fulnam(view fn: string): pstring;
->
void pa_fulnam(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure getpgm(out p: string);
->
extern void pa_getpgm(char* p, int pl);

********************************************************************************/

/********************************************************************************

function getpgm: pstring;
->
extern void pa_getpgm(char* p, int pl);

********************************************************************************/

/********************************************************************************

procedure getusr(out fn: string);
->
void pa_getusr(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

function getusr: pstring;
->
void pa_getusr(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure setatr(view fn: string; a: attrset);
->
pa_setatr(char* fn, pa_attrset a);

********************************************************************************/

/********************************************************************************

procedure setatr(fn: pstring; a: attrset);
->
void pa_setatrl(char* fn, int fnl, pa_attrset a);

********************************************************************************/

/********************************************************************************

procedure resatr(view fn: string; a: attrset);
->
void pa_resatrl(char* fn, int fnl, pa_attrset a);

********************************************************************************/

/********************************************************************************

procedure resatr(fn: pstring; a: attrset);
->
void pa_resatrl(char* fn, int fnl, pa_attrset a);

********************************************************************************/

/********************************************************************************

procedure bakupd(view fn: string);
->
void pa_bakupdl(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure bakupd(fn: pstring);
->
void pa_bakupdl(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure setuper(view fn: string; p: permset);
->
void pa_setuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure setuper(fn: pstring; p: permset);
->
void pa_setuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure resuper(view fn: string; p: permset);
->
void pa_resuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure resuper(fn: pstring; p: permset);
->
void pa_resuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure setgper(view fn: string; p: permset);
->
void pa_setgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure setgper(fn: pstring; p: permset);
->
void pa_setgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure resgper(view fn: string; p: permset);
->
void pa_resgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure resgper(fn: pstring; p: permset);
->
void pa_resgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure setoper(view fn: string; p: permset);
->
void pa_setoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure setoper(fn: pstring; p: permset);
->
void pa_setoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure resoper(view fn: string; p: permset);
->
void pa_resoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure resoper(fn: pstring; p: permset);
->
void pa_resoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

/********************************************************************************

procedure makpth(view fn: string);
->
void pa_makpthl(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure makpth(fn: pstring);
->
void pa_makpthl(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure rempth(view fn: string);
->
void pa_rempthl(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure rempth(fn: pstring);
->
void pa_rempthl(char* fn, int fnl);

********************************************************************************/

/********************************************************************************

procedure filchr(out fc: schar);
->
void pa_filchr(pa_chrset fc);

********************************************************************************/

/********************************************************************************

function optchr: char;
->
char pa_optchr(void);

********************************************************************************/

/********************************************************************************

function pthchr: char;
->
char pa_pthchr(void);

********************************************************************************/

/********************************************************************************

function latitude: integer;
->
int  pa_latitude(void);

********************************************************************************/

/********************************************************************************

function longitude: integer;
->
int  pa_longitude(void);

********************************************************************************/

/********************************************************************************

function altitude: integer;
->
int  pa_altitude(void);

********************************************************************************/

/********************************************************************************

function country: integer;
->
int  pa_country(void);

********************************************************************************/

/********************************************************************************

procedure countrys(view s: string; len: integer; c: integer);
->
void pa_countrys(char* s, int sl, int c);

********************************************************************************/

/********************************************************************************

function timezone: integer;
->
int  pa_timezone(void);

********************************************************************************/

/********************************************************************************

function daysave: integer;
->
int  pa_daysave(void);

********************************************************************************/

/********************************************************************************

function time24hour: integer;
->
int pa_time24hour(void);

********************************************************************************/

/********************************************************************************

function language: integer;
->
int pa_language(void);

********************************************************************************/

/********************************************************************************

procedure languages(view s: string; len: integer; l: integer);
->
void pa_languages(char* s, int sl, int l);

********************************************************************************/

/********************************************************************************

function decimal: char;
->
char pa_decimal(void);

********************************************************************************/

/********************************************************************************

function numbersep: char;
->
char pa_numbersep(void);

********************************************************************************/

/********************************************************************************

function timeorder: integer;
->
int  pa_timeorder(void);

********************************************************************************/

/********************************************************************************

function dateorder: integer;
->
int  pa_dateorder(void);

********************************************************************************/

/********************************************************************************

function datesep: char;
->
char pa_datesep(void);

********************************************************************************/

/********************************************************************************

function timesep: char;
->
char pa_timesep(void);

********************************************************************************/

/********************************************************************************

function currchr: char;
->
char pa_currchr(void);

********************************************************************************/
