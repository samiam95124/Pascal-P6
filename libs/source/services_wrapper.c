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
void ami_list(char* f, ami_filrec **lp);

********************************************************************************/

void wrapper_list(
    /** filename */  string fn, int fnl,
    /** file list */ filptr* fl
)

{

    rempad(fn, &fnl);
    /* get files list in C form */
    ami_list(cstrz(fn, fnl), (ami_filptr*)fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline((ami_filptr*)fl);

}

/********************************************************************************

procedure list(view f: pstring; var  l: filptr);
->
extern void ami_list(char* f, ami_filrec **lp);

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
    ami_list(cstrz(s, l), (ami_filptr*)fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline((ami_filptr*)fl);

}

/********************************************************************************

procedure times(out s: string; t: integer);
->
void ami_times(char* s, int sl, int t);

********************************************************************************/

void wrapper_times(
    /** string pointer */ string s,
    /** string length */  int l,
    /** time          */  long t
)

{

    ami_times(s, l, t); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function times(t: integer): pstring;
->
void ami_times(char* s, int sl, int t);

********************************************************************************/

pstring wrapper_timesp(
    /** time          */  long t
)

{

    char buff[BUFLEN];

    ami_times(buff, BUFLEN, t); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}


/********************************************************************************

procedure dates(var s: string; t: integer);
->
ami_dates(char* s, int sl, int t);

********************************************************************************/

void wrapper_dates(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** time          */  long t
)

{

    ami_dates(s, l, t); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function dates(t: integer): pstring;
->
ami_dates(char* s, int sl, int t);

********************************************************************************/

pstring wrapper_datesp(
    /** time          */  long t
)

{

    char buff[BUFLEN];

    ami_dates(buff, BUFLEN, t); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure writetime(var f: text; t: integer);
->
void ami_writetime(FILE *f, int t);

********************************************************************************/

void wrapper_writetimef(
    /** file to write */ pfile pfp,
    /** time */          long t
)

{

    FILE* fp; /* libc file pointer */

    fp = psystem_libcwrfil(pfp); /* find libc compatible file */
    ami_writetime(fp, t);

}

/********************************************************************************

procedure writetime(var f: text; t: integer);
->
void ami_writetime(FILE *f, int t);

********************************************************************************/

void wrapper_writetime(
    /** time */          long t
)

{

    ami_writetime(stdout, t);

}

/********************************************************************************

procedure writedatef(var f: text; t: integer);
->
void ami_writedate(FILE *f, int t);

********************************************************************************/

void wrapper_writedatef(
    /** file to write */ pfile pfp,
    /** time */          long t
)

{

    FILE* fp; /* libc file pointer */

    fp = psystem_libcwrfil(pfp); /* find libc compatible file */
    ami_writedate(fp, t);

}

/********************************************************************************

procedure writedate(var f: text; t: integer);
->
void ami_writedate(FILE *f, int t);

********************************************************************************/

void wrapper_writedate(
    /** time */ long t
)

{

    ami_writedate(stdout, t);

}

/********************************************************************************

function validfile(view s: string): boolean;
->
int  ami_validfile(char* s);

********************************************************************************/

int wrapper_validfile(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l);
    return (ami_validfile(cstrz(s, l)));

}

/********************************************************************************

function validfile(view s: pstring): boolean;
->
int  ami_validfile(char* s);

********************************************************************************/

int wrapper_validfilep(
    /** filename */  pstring fn
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    return(ami_validfile(cstrz(s, l)));

}

/********************************************************************************

function validpath(view s: string): boolean;
->
int  ami_validpath(char* s);

********************************************************************************/

int wrapper_validpath(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l);
    return (ami_validpath(cstrz(s, l)));

}

/********************************************************************************

function validpath(view s: pstring): boolean;
->
int  ami_validpath(char* s);

********************************************************************************/

int wrapper_validpathp(
    /** filename */  pstring fn
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    return(ami_validpath(cstrz(s, l)));

}

/********************************************************************************

function wild(view s: string): boolean;
->
int  ami_wild(char* s);

********************************************************************************/

int wrapper_wild(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l);
    return (ami_wild(cstrz(s, l)));

}

/********************************************************************************

function wild(view s: pstring): boolean;
->
int  ami_wild(char* s);

********************************************************************************/

int wrapper_wildp(
    /** filename */  pstring fn
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    return(ami_wild(cstrz(s, l)));

}

/********************************************************************************

procedure getenv(view ls: string; out ds: string);
->
void ami_getenv(char* ls, char* ds, int dsl);

********************************************************************************/

void wrapper_getenv(
    /** variable name */ string  s, int sl,
    /** variable value */ string d, int dl
)

{

    rempad(s, &sl);
    ami_getenv(cstrz(s, sl), d, dl); /* find environment string */
    cstr2pad(d, dl); /* convert output to padded */

}

/********************************************************************************

function getenv(view ls: string): pstring;
->
void ami_getenv(char* ls, char* ds, int dsl);

********************************************************************************/

pstring wrapper_getenvp(
    /** variable name */ string  s, int sl
)

{

    char buff[BUFLEN];

    rempad(s, &sl);
    ami_getenv(cstrz(s, sl), buff, BUFLEN); /* find environment string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure setenv(view sn, sd: string);
->
void ami_setenv(char* sn, char* sd);

********************************************************************************/

void wrapper_setenv(
    /** name */ string n, int nl,
    /** variable */ string v, int vl
)

{

    rempad(n, &nl);
    rempad(v, &vl);
    ami_setenv(cstrz(n, nl), cstrz(v, vl));

}

/********************************************************************************

procedure setenv(sn: pstring; view sd: string);
->
void ami_setenv(char* sn, char* sd);

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

    ami_setenv(cstrz(s, l), cstrz(v, vl));

}

/********************************************************************************

procedure setenv(view sn: string; sd: pstring);
->
void ami_setenv(char* sn, char* sd);

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

    ami_setenv(cstrz(n, nl), cstrz(s, l));

}

/********************************************************************************

procedure setenv(sn, sd: pstring);
->
void ami_setenv(char* sn, char* sd);

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

    ami_setenv(cstrz(ns, nl), cstrz(vs, vl));

}

/********************************************************************************

procedure allenv(var el: envptr);
->
void ami_allenv(ami_envrec **el);

********************************************************************************/

void wrapper_allenv(ami_envrec** el)

{

    ami_allenv(el);
    cenvlist2pascaline(el);

}

/********************************************************************************

procedure remenv(view sn: string);
->
void ami_remenv(char* sn);

********************************************************************************/

void wrapper_remenv(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l);
    ami_remenv(cstrz(s, l));

}

/********************************************************************************

procedure remenv(view sn: pstring);
->
extern void ami_remenv(char* sn);

********************************************************************************/

void wrapper_remenvp(
    /** name */  pstring n
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(n, &s, &l); /* get cstr/len from pstring */

    ami_remenv(cstrz(s, l));

}

/********************************************************************************

procedure exec(view cmd: string);
->
void ami_exec(char* cmd);

********************************************************************************/

void wrapper_exec(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l);
    ami_exec(cstrz(s, l));

}

/********************************************************************************

procedure exec(cmd: pstring);
->
void ami_exec(char* cmd);

********************************************************************************/

void wrapper_execp(
    /** filename */  pstring fn
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    ami_exec(cstrz(s, l));

}

/********************************************************************************

procedure exece(view cmd: string; el: envptr);
->
void ami_exece(char* cmd, ami_envrec *el);

********************************************************************************/

void wrapper_exece(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** environment */    envptr el
)

{

    /* C copy list */ ami_envptr cel;

    rempad(s, &l); /* remove right padding */
    cel = cenvlist2c(el); /* convert environment to C form */
    ami_exece(cstrz(s, l), cel); /* execute */
    freenvl(cel); /* free the list */

}

/********************************************************************************

procedure exece(cmd: pstring; el: envptr);
->
void ami_exece(char* cmd, ami_envrec *el);

********************************************************************************/

void wrapper_execep(
    /** filename */    pstring fn,
    /** environment */ envptr el
)

{

    /** string pointer */ char* s;
    /** string length */  int l;
    /* C copy list */ ami_envptr cel;

    cel = cenvlist2c(el); /* convert environment to C form */
    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */
    ami_exece(cstrz(s, l), cel);
    freenvl(cel); /* free the list */

}

/********************************************************************************

procedure execw(view cmd: string; out e: integer);
->
void ami_execw(char* cmd, int *e);

********************************************************************************/

void wrapper_execw(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** error */          int *e
)

{

    rempad(s, &l); /* remove right padding */
    ami_execw(cstrz(s, l), e);

}

/********************************************************************************

procedure execw(cmd: pstring; out e: integer);
->
void ami_execw(char* cmd, int *e);

********************************************************************************/

void wrapper_execwp(
    /** filename */    pstring fn,
    /** environment */ ami_envptr el,
    /** error */       int *e
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    ami_execw(cstrz(s, l), e);

}

/********************************************************************************

procedure execew(view cmd: string; el: envptr; out e: integer);
->
void ami_execew(char* cmd, ami_envrec *el, int *e);

********************************************************************************/

void wrapper_execew(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** environment */    envptr el,
    /** error */          int *e
)

{

    /* C copy list */ ami_envptr cel;

    rempad(s, &l); /* remove right padding */
    cel = cenvlist2c(el); /* convert environment to C form */
    ami_execew(cstrz(s, l), cel, e);
    freenvl(cel); /* free the list */

}

/********************************************************************************

procedure execew(cmd: pstring; el: envptr; out e: integer);
->
void ami_execew(char* cmd, ami_envrec *el, int *e);

********************************************************************************/

void wrapper_execewp(
    /** filename */    pstring fn,
    /** environment */ envptr el,
    /** error */       int *e
)

{

    /** string pointer */ char* s;
    /** string length */  int l;
    /* C copy list */     ami_envptr cel;

    cel = cenvlist2c(el); /* convert environment to C form */
    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    ami_execew(cstrz(s, l), cel, e);

}

/********************************************************************************

procedure getcur(out fn: string);
->
void ami_getcur(char* fn, int l);

********************************************************************************/

void wrapper_getcur(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    ami_getcur(s, l); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function getcur: pstring;
->
void ami_getcur(char* fn, int l);

********************************************************************************/

pstring wrapper_getcurp(void)

{

    char buff[BUFLEN];

    ami_getcur(buff, BUFLEN); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure setcur(view fn: string);
->
void ami_setcur(char* fn);

********************************************************************************/

void wrapper_setcur(
    /** string pointer */ char* s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove right padding */
    ami_setcur(cstrz(s, l)); /* set current path */

}

/********************************************************************************

procedure setcur(fn: pstring);
->
void ami_setcur(char* fn);

********************************************************************************/

void wrapper_setcurp(
    /** filename */  pstring fn
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */

    ami_setcur(cstrz(s, l));

}

/********************************************************************************

procedure brknam(view fn: string; out p, n, e: string);
->
void ami_brknam(char* fn, char* p, int pl, char* n, int nl, char* e, int el);

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
    ami_brknam(cstrz(fn, fnl), p, pl, n, nl, e, el);
    /* convert output cstrings to padded pascaline */
    cstr2pad(p, pl);
    cstr2pad(n, nl);
    cstr2pad(e, el);

}

/********************************************************************************

procedure brknam(view fn: string; out p, n, e: pstring);
->
void ami_brknam(char* fn, char* p, int pl, char* n, int nl, char* e, int el);

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
    ami_brknam(cstrz(fn, fnl), buff1, BUFLEN, buff2, BUFLEN, buff3, BUFLEN);
    *p = cstr2pstr(buff1, BUFLEN); /* place results */
    *n = cstr2pstr(buff2, BUFLEN);
    *e = cstr2pstr(buff3, BUFLEN);

}

/********************************************************************************

procedure brknam(fn: pstring; out p, n, e: pstring);
->
void ami_brknam(char* fn, char* p, int pl, char* n, int nl, char* e, int el);

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
    ami_brknam(cstrz((string)fn, fnl), buff1, BUFLEN, buff2, BUFLEN, buff3, BUFLEN);
    *p = cstr2pstr(buff1, BUFLEN); /* place results */
    *n = cstr2pstr(buff2, BUFLEN);
    *e = cstr2pstr(buff3, BUFLEN);

}

/********************************************************************************

procedure maknam(out fn: string; view p, n, e: string);
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(fn, fnl, cstrz(p, pl), cstrz(n, nl), cstrz(e, el));
    cstr2pad(fn, fnl); /* convert output to padded */

}

/********************************************************************************

function maknam(view p, n, e: string): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(p, pl), cstrz(n, nl), cstrz(e, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; view n: string; e: pstring): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(ps, pl), cstrz(n, nl), cstrz(es, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; n: pstring; view e: string): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(p, pl), cstrz(ns, nl), cstrz(e, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(view p: string; n: pstring; e: pstring): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(p, pl), cstrz(ns, nl), cstrz(es, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; view n: string; view e: string): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(ps, pl), cstrz(n, nl), cstrz(e, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; view n: string; e: pstring): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(ps, pl), cstrz(n, nl), cstrz(es, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; n: pstring; view e: string): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(ps, pl), cstrz(ns, nl), cstrz(e, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

function maknam(p: pstring; n: pstring; e: pstring): pstring;
->
void ami_maknam(char* fn, int fnl, char* p, char* n, char* e);

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
    ami_maknam(buff, BUFLEN, cstrz(ps, pl), cstrz(ns, nl), cstrz(es, el));

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure fulnam(var fn: string);
->
void ami_fulnam(char* fn, int fnl);

********************************************************************************/

void wrapper_fulnam(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    int pl = l;

    rempad(s, &pl);
    s[pl] = 0;
    ami_fulnam(s, l); /* find full name string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function fulnam(view fn: string): pstring;
->
void ami_fulnam(char* fn, int fnl);

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
    ami_fulnam(buff, BUFLEN); /* find time string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure getpgm(out p: string);
->
extern void ami_getpgm(char* p, int pl);

********************************************************************************/

void wrapper_getpgm(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    ami_getpgm(s, l); /* find program path string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function getpgm: pstring;
->
extern void ami_getpgm(char* p, int pl);

********************************************************************************/

pstring wrapper_getpgmp(void)

{

    char buff[BUFLEN];

    ami_getpgm(buff, BUFLEN); /* find program path string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure getusr(out fn: string);
->
void ami_getusr(char* fn, int fnl);

********************************************************************************/

void wrapper_getusr(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    ami_getusr(s, l); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function getusr: pstring;
->
void ami_getusr(char* fn, int fnl);

********************************************************************************/

pstring wrapper_getusrp(void)

{

    char buff[BUFLEN];

    ami_getusr(buff, BUFLEN); /* find program path string */

    return (cstr2pstr(buff, BUFLEN)); /* return pstring */

}

/********************************************************************************

procedure setatr(view fn: string; a: attrset);
->
ami_setatr(char* fn, ami_attrset a);

********************************************************************************/

void wrapper_setatr(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_setatr(cstrz(s, l), a[0]); /* set attributes */

}

/********************************************************************************

procedure setatr(fn: pstring; a: attrset);
->
void ami_setatr(char* fn, ami_attrset a);

********************************************************************************/

void wrapper_setatrp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_setatr(cstrz(as, al), a[0]); /* set attributes */

}

/********************************************************************************

procedure resatr(view fn: string; a: attrset);
->
void ami_resatr(char* fn, ami_attrset a);

********************************************************************************/

void wrapper_resatr(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_resatr(cstrz(s, l), a[0]); /* reset attributes */

}

/********************************************************************************

procedure resatr(fn: pstring; a: attrset);
->
void ami_resatr(char* fn, ami_attrset a);

********************************************************************************/

void wrapper_resatrp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_resatr(cstrz(as, al), a[0]); /* set attributes */

}

/********************************************************************************

procedure bakupd(view fn: string);
->
void ami_bakupd(char* fn);

********************************************************************************/

void wrapper_bakupd(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove padding */
    ami_bakupd(cstrz(s, l)); /* set backed up */

}

/********************************************************************************

procedure bakupd(fn: pstring);
->
void ami_bakupd(char* fn);

********************************************************************************/

void wrapper_bakupdp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_bakupd(cstrz(as, al)); /* set attributes */

}

/********************************************************************************

procedure setuper(view fn: string; p: permset);
->
void ami_setuper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_setuper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_setuper(cstrz(s, l), a[0]); /* set permissions */

}

/********************************************************************************

procedure setuper(fn: pstring; p: permset);
->
void ami_setuper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_setuperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_setuper(cstrz(as, al), a[0]); /* set permissions */

}

/********************************************************************************

procedure resuper(view fn: string; p: permset);
->
void ami_resuper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_resuper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_resuper(cstrz(s, l), a[0]); /* set permissions */

}

/********************************************************************************

procedure resuper(fn: pstring; p: permset);
->
void ami_resuper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_resuperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_resuper(cstrz(as, al), a[0]); /* set permissions */

}

/********************************************************************************

procedure setgper(view fn: string; p: permset);
->
void ami_setgper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_setgper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_setgper(cstrz(s, l), a[0]); /* set permissions */

}

/********************************************************************************

procedure setgper(fn: pstring; p: permset);
->
void ami_setgper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_setgperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_setgper(cstrz(as, al), a[0]); /* set permissions */

}

/********************************************************************************

procedure resgper(view fn: string; p: permset);
->
void ami_resgper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_resgper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_resgper(cstrz(s, l), a[0]); /* set permissions */

}

/********************************************************************************

procedure resgper(fn: pstring; p: permset);
->
void ami_resgper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_resgperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_resgper(cstrz(as, al), a[0]); /* set permissions */

}

/********************************************************************************

procedure setoper(view fn: string; p: permset);
->
void ami_setoper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_setoper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_setoper(cstrz(s, l), a[0]); /* set permissions */

}

/********************************************************************************

procedure setoper(fn: pstring; p: permset);
->
void ami_setoper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_setoperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_setoper(cstrz(as, al), a[0]); /* set permissions */

}

/********************************************************************************

procedure resoper(view fn: string; p: permset);
->
void ami_resoper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_resoper(
    /** string pointer */ string s,
    /** string length */  int l,
    /** attribute set */  set a
)

{

    rempad(s, &l); /* remove padding */
    ami_resoper(cstrz(s, l), a[0]); /* set permissions */

}

/********************************************************************************

procedure resoper(fn: pstring; p: permset);
->
void ami_resoper(char* fn, ami_permset p);

********************************************************************************/

void wrapper_resoperp(
    /** string pointer */ pstring s,
    /** attribute set */  set a
)

{

    string as;
    int    al;

    pstr2cstrl(s, &as, &al); /* get cstr/len from pstring */
    ami_resoper(cstrz(as, al), a[0]); /* set permissions */

}

/********************************************************************************

procedure makpth(view fn: string);
->
void ami_makpth(char* fn);

********************************************************************************/

void wrapper_makpth(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove padding */
    ami_makpth(cstrz(s, l)); /* make path */

}

/********************************************************************************

procedure makpth(fn: pstring);
->
void ami_makpth(char* fn);

********************************************************************************/

void wrapper_makpthp(
    /** string pointer */ pstring p
)

{

    string ps;
    int    pl;

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    ami_makpth(cstrz(ps, pl)); /* set permissions */

}

/********************************************************************************

procedure rempth(view fn: string);
->
void ami_rempth(char* fn);

********************************************************************************/

void wrapper_rempth(
    /** string pointer */ string s,
    /** string length */  int l
)

{

    rempad(s, &l); /* remove padding */
    ami_rempth(cstrz(s, l)); /* make path */

}

/********************************************************************************

procedure rempth(fn: pstring);
->
void ami_rempth(char* fn);

********************************************************************************/

void wrapper_rempthp(
    /** string pointer */ pstring p
)

{

    string ps;
    int    pl;

    pstr2cstrl(p, &ps, &pl); /* get cstr/len from pstring */
    ami_rempth(cstrz(ps, pl)); /* set permissions */

}

/********************************************************************************

procedure countrys(out s: string; c: integer);
->
void ami_countrys(char* s, int sl, int c);

********************************************************************************/

void wrapper_countrys(
    /** string pointer */ string s,
    /** string length */  int l,
    /** country code  */  int c
)

{

    ami_countrys(s, l, c); /* find country string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

procedure languages(out s: string; len: integer; l: integer);
->
void ami_languages(char* s, int sl, int l);

********************************************************************************/

void wrapper_languages(
    /** string pointer */ string s,
    /** string length */  int l,
    /** country code  */  int c
)

{

    ami_languages(s, l, c); /* find country string */
    cstr2pad(s, l); /* convert output to padded */

}

