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
`    
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
`    
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

services.setenv$p_vc_vc:
    procedure pa_setenvl, 4

/********************************************************************************

procedure setenv(sn: pstring; view sd: string);
->
void pa_setenvl(char* sn, int snl, char* sd, int sdl);

********************************************************************************/

services.setenv$p_pvc_vc:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    setenvl             # call C routine
    postamble

/********************************************************************************

procedure setenv(view sn: string; sd: pstring);
->
void pa_setenvl(char* sn, int snl, char* sd, int sdl);

********************************************************************************/

services.setenv$p_vc_pvc:
    preamble    0, 3
    movq    (%rdx),%rcx         # move string len to 2nd
    addq    $8,%rdx             # index string data
    call    setenvl             # call C routine
    postamble


/********************************************************************************

procedure setenv(sn, sd: pstring);
->
void pa_setenvl(char* sn, int snl, char* sd, int sdl);

********************************************************************************/

services.setenv$p_pvc_pvc:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    movq    (%rdx),%rcx         # move string len to 2nd
    addq    $8,%rdx             # index string data
    call    setenvl             # call C routine
    postamble

/********************************************************************************

procedure allenv(var el: envptr);
->
void pa_allenv(pa_envrec **el);

********************************************************************************/

services.allenv$p_:
    preamble    0, 1
    push    %rdi                # save env*
    call    pa_allenv           # call C function
    call    cenvlist2pascaline  # convert env list
    postamble

/********************************************************************************

procedure remenv(view sn: string);
->
void pa_remenvl(char* sn, int snl);

********************************************************************************/

services.remenv$p_vc:
    procedure   pa_remenvl, 0, 2

/********************************************************************************

procedure remenv(view sn: pstring);
->
extern void pa_remenvl(char* sn, int snl);

********************************************************************************/

services.remenv$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_remenvl             # call C routine
    postamble

/********************************************************************************

procedure exec(view cmd: string);
->
void pa_execl(char* cmd, int cmdl);

********************************************************************************/

services.exec$p_vc:
    procedure   pa_execl, 0, 2

/********************************************************************************

procedure exec(cmd: pstring);
->
void pa_execl(char* cmd, int cmdl);

********************************************************************************/

services.exec$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execl             # call C routine
    postamble

/********************************************************************************

procedure exece(view cmd: string; el: envptr);
->
void pa_execel(char* cmd, int cmdl, pa_envrec *el);

********************************************************************************/

services.exece$p_vc:
    preamble    0, 3
    pushq       %rdi            # save cmd string
    pushq       %rsi            # save cmd len
    pushq       %rdx            # save el
    mov         %rdx,%rdi       # set el as 1st par
    call        cenvlist2pascaline # convert env list
    popq        %rdx            # restore el
    popq        %rsi            # restore cmd len
    popq`       %rdi            # restore cmd string
    call        pa_execel       # call C routine
    postamble

/********************************************************************************

procedure exece(cmd: pstring; el: envptr);
->
void pa_execel(char* cmd, int cmdl, pa_envrec *el);

********************************************************************************/

services.exece$p_pvc:
    preamble    0, 3
    movq    (%rdi),%rsi     # move string len to 2nd
    addq    $8,%rdi         # index string data
    pushq   %rdi            # save cmd string
    pushq   %rsi            # save cmd len
    pushq   %rdx            # save el
    mov     %rdx,%rdi       # set el as 1st par
    call    cenvlist2pascaline # convert env list
    popq    %rdx            # restore el
    popq    %rsi            # restore cmd len
    popq`   %rdi            # restore cmd string
    call    pa_execel       # call C routine
    postamble

/********************************************************************************

procedure execw(view cmd: string; var e: integer);
->
void pa_execwl(char* cmd, int cmdl, int *e);

********************************************************************************/

services.execw$p_vc_i:
    procedure   pa_execwl, 0, 3

/********************************************************************************

procedure execw(cmd: pstring; var e: integer);
->
void pa_execwl(char* cmd, int cmdl, int *e);

********************************************************************************/

services.execw$p_pvc_i:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execwl            # call C routine
    postamble

/********************************************************************************

procedure execew(view cmd: string; el: envptr; var e: integer);
->
void pa_execewl(char* cmd, int cmdl, pa_envrec *el, int *e);

********************************************************************************/

services.exece$p_vc_xx_i:
    preamble    0, 4
    pushq       %rdi            # save cmd string
    pushq       %rsi            # save cmd len
    pushq       %rdx            # save el
    pushq       %rcx            # save e
    mov         %rdx,%rdi       # set el as 1st par
    call        cenvlist2pascaline # convert env list
    popq        %rcx            # restore e
    popq        %rdx            # restore el
    popq        %rsi            # restore cmd len
    popq`       %rdi            # restore cmd string
    call        pa_execewl      # call C routine
    postamble

/********************************************************************************

procedure execew(cmd: pstring; el: envptr; var e: integer);
->
void pa_execewl(char* cmd, int cmdl, pa_envrec *el, int *e);

********************************************************************************/

services.exece$p_pvc_xx_i:
    preamble    0, 4
    movq        (%rdi),%rsi     # move string len to 2nd
    addq        $8,%rdi         # index string data
    pushq       %rdi            # save cmd string
    pushq       %rsi            # save cmd len
    pushq       %rdx            # save el
    pushq       %rcx            # save e
    mov         %rdx,%rdi       # set el as 1st par
    call        cenvlist2pascaline # convert env list
    popq        %rcx            # restore e
    popq        %rdx            # restore el
    popq        %rsi            # restore cmd len
    popq`       %rdi            # restore cmd string
    call        pa_execewl      # call C routine
    postamble

/********************************************************************************

procedure getcur(out fn: string);
->
void pa_getcur(char* fn, int l);

********************************************************************************/

services.getcur$p_vc_i:
    preamble    0, 2
    pushq   %rdi                # save string
    pushq   %rsi                # save length
    call    pa_getcur           # call C function
    popq    %rsi                # restore length
    pop     %rdi                # restore string
    call    cstr2pad            # convert to padded
    postamble

/********************************************************************************

function getcur: pstring;
->
void pa_getcur(char* fn, int l);

********************************************************************************/

services.getcur$f:
    preamble    1, 1
    movq    %rdi,%rdx           # place t
    subq    $1024,%rsp          # allocate stack buffer
    movq    %rsp,%rdi           # index that
    movq    $1024,%rsi          # set maximum length
    call    pa_getcur           # get time string in buffer
    movq    %rsp,%rdi           # index buffer
    movq    $1024,%rsi          # set maximum length
    call    cstr2pstr           # convert to pstring
    addq    $1024,%rsp          # deallocate stack buffer
    postamble

/********************************************************************************

procedure setcur(view fn: string);
->
void pa_setcurl(char* fn, int fnl);

********************************************************************************/

services.setcur$p_vc:
    procedure pa_setcurl, 0, 2

/********************************************************************************

procedure setcur(fn: pstring);
->
void pa_setcurl(char* fn, int fnl);

********************************************************************************/

services.setcur$f_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_setcurl          # call C routine
    postamble

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

/********************************************************************************

function maknam(view p, n, e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

/********************************************************************************

function maknam(view p: string; view n: string; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

/********************************************************************************

function maknam(view p: string; n: pstring; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

/********************************************************************************

function maknam(view p: string; n: pstring; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; view n: string; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; view n: string; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; n: pstring; view e: string): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; n: pstring; e: pstring): pstring;
->
void pa_maknaml(char* fn, int fnl, char* p, int pl, char* n, int nl, char* e, 
                int el);

********************************************************************************/

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
