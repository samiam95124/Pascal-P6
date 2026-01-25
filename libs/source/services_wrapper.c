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

    rempad(fn, &fnl);
    /* get files list in C form */
    pa_listl(fn, fnl, (pa_filptr*)fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline((pa_filptr*)fl);

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
    pa_listl(s, l, (pa_filptr*)fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline((pa_filptr*)fl);

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
    /** string length */  int l
)

{

    rempad(s, &l);
    return (pa_validfilel(s, l));

}

/********************************************************************************

function validfile(view s: pstring): boolean;
->
int  pa_validfilel(char* s, int l);

********************************************************************************/

int wrapper_validfilep(
    /** filename */  pstring fn
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
    /** string length */  int l
)

{

    rempad(s, &l);
    return (pa_validpathl(s, l));

}

/********************************************************************************

function validpath(view s: pstring): boolean;
->
int  pa_validpathl(char* s, int l);

********************************************************************************/

int wrapper_validpathp(
    /** filename */  pstring fn
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
    /** string length */  int l
)

{

    rempad(s, &l);
    return (pa_wildl(s, l));

}

/********************************************************************************

function wild(view s: pstring): boolean;
->
int  pa_wildl(char* s, int l);

********************************************************************************/

int wrapper_wildp(
    /** filename */  pstring fn
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

    rempad(s, &sl);
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

    rempad(s, &sl);
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

    rempad(n, &nl);
    rempad(v, &vl);
    pa_setenvl(n, nl, v, vl);

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
    rempad(v, &vl);

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

    rempad(n, &nl);
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

void wrapper_allenv(pa_envrec** el)

{

    pa_allenv(el);
    cenvlist2pascaline(el);

}

/********************************************************************************

procedure remenv(view sn: string);
->
void pa_remenvl(char* sn, int snl);

********************************************************************************/

void wrapper_remenv(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l);
    pa_remenvl(s, l);

}

/********************************************************************************

procedure remenv(view sn: pstring);
->
extern void pa_remenvl(char* sn, int snl);

********************************************************************************/

void wrapper_remenvp(
    /** name */  pstring n
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
    /** string length */  int l
)

{

    rempad(s, &l);
    pa_execl(s, l);

}

/********************************************************************************

procedure exec(cmd: pstring);
->
void pa_execl(char* cmd, int cmdl);

********************************************************************************/

void wrapper_execp(
    /** filename */  pstring fn
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
    /** environment */    envptr el
)

{

    /* C copy list */ pa_envptr cel;

    rempad(s, &l); /* remove right padding */
    cel = cenvlist2c(el); /* convert environment to C form */
    pa_execel(s, l, cel); /* execute */
    freenvl(cel); /* free the list */

}

/********************************************************************************

procedure exece(cmd: pstring; el: envptr);
->
void pa_execel(char* cmd, int cmdl, pa_envrec *el);

********************************************************************************/

void wrapper_execep(
    /** filename */    pstring fn,
    /** environment */ envptr el
)

{

    /** string pointer */ char* s;
    /** string length */  int l;
    /* C copy list */ pa_envptr cel;

    cel = cenvlist2c(el); /* convert environment to C form */
    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */
    pa_execel(s, l, cel);
    freenvl(cel); /* free the list */

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

    rempad(s, &l); /* remove right padding */
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
    /** environment */    envptr el,
    /** error */          int *e
)

{

    /* C copy list */ pa_envptr cel;

    rempad(s, &l); /* remove right padding */
    cel = cenvlist2c(el); /* convert environment to C form */
    pa_execewl(s, l, cel, e);
    freenvl(cel); /* free the list */

}

/********************************************************************************

procedure execew(cmd: pstring; el: envptr; out e: integer);
->
void pa_execewl(char* cmd, int cmdl, pa_envrec *el, int *e);

********************************************************************************/

void wrapper_execewp(
    /** filename */    pstring fn,
    /** environment */ envptr el,
    /** error */       int *e
)

{

    /** string pointer */ char* s;
    /** string length */  int l;
    /* C copy list */     pa_envptr cel;

    cel = cenvlist2c(el); /* convert environment to C form */
    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    pa_execewl(s, l, cel, e);

}

/********************************************************************************

procedure getcur(out fn: string);
->
void pa_getcur(char* fn, int l);

********************************************************************************/

void wrapper_getcur(
    /** string pointer */ string s,
    /** string length */  int l
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

void wrapper_setcur(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove right padding */
    pa_setcurl(s, l); /* set current path */

}

/********************************************************************************

procedure setcur(fn: pstring);
->
void pa_setcurl(char* fn, int fnl);

********************************************************************************/

void wrapper_setcurp(
    /** filename */  pstring fn
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    pa_setcurl(s, l);

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

    rempad(fn, &fnl); /* remove right padding */
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

void wrapper_brknamsp(
    /* filename */  char* fn, int fnl,
    /* path */      pstring* p,
    /* name */      pstring* n,
    /* extension */ pstring* e
)

{

    int pl, nl, el; /* lengths */
    char buff1[BUFLEN];
    char buff2[BUFLEN];
    char buff3[BUFLEN];

    rempad(fn, &fnl); /* remove right padding */
    /* execute subfunction */
    pa_brknaml(fn, fnl, buff1, BUFLEN, buff2, BUFLEN, buff3, BUFLEN);
    *p = cstr2pstr(buff1, BUFLEN); /* place results */
    *n = cstr2pstr(buff2, BUFLEN);
    *e = cstr2pstr(buff3, BUFLEN);

}

/********************************************************************************

procedure brknam(fn: pstring; out p, n, e: pstring);
->
void pa_brknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

void wrapper_brknampp(
    /* filename */  pstring fn,
    /* path */      pstring* p,
    /* name */      pstring* n,
    /* extension */ pstring* e
)

{

    int fnl, pl, nl, el; /* lengths */
    char buff1[BUFLEN];
    char buff2[BUFLEN];
    char buff3[BUFLEN];

    cpstrp2cstrl(&fn, &fnl); /* convert pstring arguments to cstr/len */
    /* execute subfunction */
    pa_brknaml((string)fn, fnl, buff1, BUFLEN, buff2, BUFLEN, buff3, BUFLEN);
    *p = cstr2pstr(buff1, BUFLEN); /* place results */
    *n = cstr2pstr(buff2, BUFLEN);
    *e = cstr2pstr(buff3, BUFLEN);

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

    rempad(p, &pl); /* remove padding */
    rempad(n, &nl);
    rempad(e, &el);
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
    /** string pointer */ string p,
    /** string length */  int pl,
    /** string pointer */ string n,
    /** string length */  int nl,
    /** string pointer */ string e,
    /** string length */  int el
)

{

    char buff[BUFLEN];

    rempad(p, &pl);
    rempad(n, &nl);
    rempad(e, &el);
    pa_maknaml(buff, BUFLEN, p, pl, n, nl, e, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; view n: string; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknampssp(
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
    rempad(n, &nl); /* remove padding */
    pstr2cstrl(e, &es, &el); /* get cstr/len from pstring */
    pa_maknaml(buff, BUFLEN, ps, pl, n, nl, es, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; n: pstring; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknampsps(
    /** string pointer */ string p, int pl,
    /** string pointer */ pstring n,
    /** string pointer */ string  e,
    /** string length */  int     el
)

{

    string ns;
    int    nl;
    char buff[BUFLEN];

    rempad(p, &pl); /* remove padding */
    pstr2cstrl(n, &ns, &nl); /* get cstr/len from pstring */
    rempad(e, &el); /* remove padding */
    pa_maknaml(buff, BUFLEN, p, pl, ns, nl, e, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; n: pstring; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknampspp(
    /** string pointer */ string  p,
    /** string length */  int     pl,
    /** string pointer */ pstring n,
    /** string pointer */ pstring e
)

{

    string ns;
    int    nl;
    string es;
    int    el;
    char buff[BUFLEN];

    rempad(p, &pl); /* remove padding */
    pstr2cstrl(n, &ns, &nl); /* get cstr/len from pstring */
    pstr2cstrl(e, &es, &el); /* get cstr/len from pstring */
    pa_maknaml(buff, BUFLEN, p, pl, ns, nl, es, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; view n: string; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknamppss(
    /** string pointer */ pstring p,
    /** string pointer */ string  n,
    /** string length */  int     nl,
    /** string pointer */ string  e,
    /** string length */  int     el
)

{

    string ps;
    int    pl;
    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    rempad(n, &nl); /* remove padding */
    rempad(e, &el); /* remove padding */
    pa_maknaml(buff, BUFLEN, ps, pl, n, nl, e, el);

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
    int    pl;
    string es;
    int    el;
    char buff[BUFLEN];

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    rempad(n, &nl); /* remove padding */
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
    rempad(e, &el); /* remove padding */
    pa_maknaml(buff, BUFLEN, ps, pl, ns, nl, e, el);

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; n: pstring; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

pstring wrapper_maknampppp(
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

void wrapper_fulnam(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    int pl = l;

    rempad(s, &pl);
    s[pl] = 0;
    pa_fulnam(s, l); /* find full name string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function fulnam(view fn: string): pstring;
->
void pa_fulnam(char* fn, int fnl);

********************************************************************************/

pstring wrapper_fulnamp(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    char buff[BUFLEN];

    rempad(s, &l);
    strncpy(buff, s, l); /* copy filename to buffer */
    if (l < BUFLEN) buff[l] = 0; /* ensure terminated */
    pa_fulnam(buff, BUFLEN); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure getpgm(out p: string);
->
extern void pa_getpgm(char* p, int pl);

********************************************************************************/

void wrapper_getpgm(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    pa_getpgm(s, l); /* find program path string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function getpgm: pstring;
->
extern void pa_getpgm(char* p, int pl);

********************************************************************************/

pstring wrapper_getpgmp(void)

{

    char buff[BUFLEN];

    pa_getpgm(buff, BUFLEN); /* find program path string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure getusr(out fn: string);
->
void pa_getusr(char* fn, int fnl);

********************************************************************************/

void wrapper_getusr(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    pa_getusr(s, l); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function getusr: pstring;
->
void pa_getusr(char* fn, int fnl);

********************************************************************************/

pstring wrapper_getusrp(void)

{

    char buff[BUFLEN];

    pa_getusr(buff, BUFLEN); /* find program path string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure setatr(view fn: string; a: attrset);
->
pa_setatr(char* fn, pa_attrset a);

********************************************************************************/

void wrapper_setatr(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_setatrl(s, l, a[0]); /* set attributes */

}

/********************************************************************************

procedure setatr(fn: pstring; a: attrset);
->
void pa_setatrl(char* fn, int fnl, pa_attrset a);

********************************************************************************/

void wrapper_setatrp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_setatrl(as, al, a[0]); /* set attributes */

}

/********************************************************************************

procedure resatr(view fn: string; a: attrset);
->
void pa_resatrl(char* fn, int fnl, pa_attrset a);

********************************************************************************/

void wrapper_resatr(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_resatrl(s, l, a[0]); /* reset attributes */

}

/********************************************************************************

procedure resatr(fn: pstring; a: attrset);
->
void pa_resatrl(char* fn, int fnl, pa_attrset a);

********************************************************************************/

void wrapper_resatrp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_resatrl(as, al, a[0]); /* set attributes */

}

/********************************************************************************

procedure bakupd(view fn: string);
->
void pa_bakupdl(char* fn, int fnl);

********************************************************************************/

void wrapper_bakupd(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove padding */
    pa_bakupdl(s, l); /* set backed up */

}

/********************************************************************************

procedure bakupd(fn: pstring);
->
void pa_bakupdl(char* fn, int fnl);

********************************************************************************/

void wrapper_bakupdp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_bakupdl(as, al); /* set attributes */

}

/********************************************************************************

procedure setuper(view fn: string; p: permset);
->
void pa_setuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_setuper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_setuperl(s, l, a[0]); /* set permissions */

}

/********************************************************************************

procedure setuper(fn: pstring; p: permset);
->
void pa_setuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_setuperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_setuperl(as, al, a[0]); /* set permissions */

}

/********************************************************************************

procedure resuper(view fn: string; p: permset);
->
void pa_resuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_resuper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_resuperl(s, l, a[0]); /* set permissions */

}

/********************************************************************************

procedure resuper(fn: pstring; p: permset);
->
void pa_resuperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_resuperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_resuperl(as, al, a[0]); /* set permissions */

}

/********************************************************************************

procedure setgper(view fn: string; p: permset);
->
void pa_setgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_setgper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_setgperl(s, l, a[0]); /* set permissions */

}

/********************************************************************************

procedure setgper(fn: pstring; p: permset);
->
void pa_setgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_setgperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_setgperl(as, al, a[0]); /* set permissions */

}

/********************************************************************************

procedure resgper(view fn: string; p: permset);
->
void pa_resgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_resgper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_resgperl(s, l, a[0]); /* set permissions */

}

/********************************************************************************

procedure resgper(fn: pstring; p: permset);
->
void pa_resgperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_resgperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_resgperl(as, al, a[0]); /* set permissions */

}

/********************************************************************************

procedure setoper(view fn: string; p: permset);
->
void pa_setoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_setoper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_setoperl(s, l, a[0]); /* set permissions */

}

/********************************************************************************

procedure setoper(fn: pstring; p: permset);
->
void pa_setoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_setoperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_setoperl(as, al, a[0]); /* set permissions */

}

/********************************************************************************

procedure resoper(view fn: string; p: permset);
->
void pa_resoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_resoper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    pa_resoperl(s, l, a[0]); /* set permissions */

}

/********************************************************************************

procedure resoper(fn: pstring; p: permset);
->
void pa_resoperl(char* fn, int fnl, pa_permset p);

********************************************************************************/

void wrapper_resoperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    pa_resoperl(as, al, a[0]); /* set permissions */

}

/********************************************************************************

procedure makpth(view fn: string);
->
void pa_makpthl(char* fn, int fnl);

********************************************************************************/

void wrapper_makpth(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove padding */
    pa_makpthl(s, l); /* make path */

}

/********************************************************************************

procedure makpth(fn: pstring);
->
void pa_makpthl(char* fn, int fnl);

********************************************************************************/

void wrapper_makpthp(
    /** string pointer */ pstring p
)

{

    string ps;
    int    pl;

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pa_makpthl(ps, pl); /* set permissions */

}

/********************************************************************************

procedure rempth(view fn: string);
->
void pa_rempthl(char* fn, int fnl);

********************************************************************************/

void wrapper_rempth(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove padding */
    pa_rempthl(s, l); /* make path */

}

/********************************************************************************

procedure rempth(fn: pstring);
->
void pa_rempthl(char* fn, int fnl);

********************************************************************************/

void wrapper_rempthp(
    /** string pointer */ pstring p
)

{

    string ps;
    int    pl;

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    pa_rempthl(ps, pl); /* set permissions */

}

/********************************************************************************

procedure filchr(out fc: schar);
->
void pa_filchr(pa_chrset fc);

********************************************************************************/

void wrapper_filchr(
    /** set of char */ pa_chrset fc
)

{

    pa_filchr(fc);

}
/********************************************************************************

function optchr: char;
->
char pa_optchr(void);

********************************************************************************/

char wrapper_optchr(void)

{

    return (pa_optchr());

}

/********************************************************************************

function pthchr: char;
->
char pa_pthchr(void);

********************************************************************************/

char wrapper_pthchr(void)

{

    return (pa_pthchr());

}

/********************************************************************************

function latitude: integer;
->
int  pa_latitude(void);

********************************************************************************/

int wrapper_latitude(void)

{

    return (pa_latitude());

}

/********************************************************************************

function longitude: integer;
->
int  pa_longitude(void);

********************************************************************************/

int wrapper_longitude(void)

{

    return (pa_longitude());

}

/********************************************************************************

function altitude: integer;
->
int  pa_altitude(void);

********************************************************************************/

int wrapper_altitude(void)

{

    return (pa_altitude());

}

/********************************************************************************

function country: integer;
->
int  pa_country(void);

********************************************************************************/

int wrapper_country(void)

{

    return (pa_country());

}

/********************************************************************************

procedure countrys(out s: string; c: integer);
->
void pa_countrys(char* s, int sl, int c);

********************************************************************************/

void wrapper_countrys(
    /** string pointer */ string s,
    /** string length */  int l,
    /** country code  */  int c
)

{

    pa_countrys(s, l, c); /* find country string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function timezone: integer;
->
int  pa_timezone(void);

********************************************************************************/

int wrapper_timezone(void)

{

    return (pa_timezone());

}

/********************************************************************************

function daysave: boolean;
->
int  pa_daysave(void);

********************************************************************************/

int wrapper_daysave(void)

{

    return (pa_daysave());

}

/********************************************************************************

function time24hour: boolean;
->
int pa_time24hour(void);

********************************************************************************/

int wrapper_time24hour(void)

{

    return (pa_time24hour());

}

/********************************************************************************

function language: integer;
->
int pa_language(void);

********************************************************************************/

int wrapper_language(void)

{

    return (pa_language());

}

/********************************************************************************

procedure languages(out s: string; len: integer; l: integer);
->
void pa_languages(char* s, int sl, int l);

********************************************************************************/

void wrapper_languages(
    /** string pointer */ string s,
    /** string length */  int l,
    /** country code  */  int c
)

{

    pa_languages(s, l, c); /* find country string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function decimal: char;
->
char pa_decimal(void);

********************************************************************************/

char wrapper_decimal(void)

{

    return (pa_decimal());

}

/********************************************************************************

function numbersep: char;
->
char pa_numbersep(void);

********************************************************************************/

char wrapper_numbersep(void)

{

    return (pa_decimal());

}

/********************************************************************************

function timeorder: integer;
->
int  pa_timeorder(void);

********************************************************************************/

int wrapper_timeorder(void)

{

    return (pa_timeorder());

}

/********************************************************************************

function dateorder: integer;
->
int  pa_dateorder(void);

********************************************************************************/

int wrapper_dateorder(void)

{

    return (pa_dateorder());

}

/********************************************************************************

function datesep: char;
->
char pa_datesep(void);

********************************************************************************/

char wrapper_datesep(void)

{

    return (pa_datesep());

}

/********************************************************************************

function timesep: char;
->
char pa_timesep(void);

********************************************************************************/

char wrapper_timesep(void)

{

    return (pa_timesep());

}

/********************************************************************************

function currchr: char;
->
char pa_currchr(void);

********************************************************************************/

char wrapper_currchr(void)

{

    return (pa_currchr());

}
