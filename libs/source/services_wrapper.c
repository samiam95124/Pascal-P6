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

procedure list(view f: pstring; var  l: filptr);

********************************************************************************/

void wrapper_listp(
    /** filename */  pstring fn,
    /** file list */ filptr* fl
)

{

    /** string pointer */ char* s;
    /** string length */  int l;

    pstr2cstrl(fn, &s, &l); /* get cstr/len from pstring */
    /* get files list in C form */
    pa_listl(s, l, fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline(fl);

}

/********************************************************************************

procedure times(var s: string; t: integer);

********************************************************************************/

void wrapper_times(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** time          */  long t
)

{

    pa_times(s, l); /* find time string */
    cstr2pad(s, l); /* convert output to padded */



}

/********************************************************************************

function times(t: integer): pstring;

********************************************************************************/

pstring wrapper_timesp(
    /** time          */  long t
)

{

    char buff[1024];

    pa_times(buff, 1024); /* find time string */
`    
    return (cstr2pstr(buff, 1024); /* return pstring */

}


/********************************************************************************

procedure times(var s: string; t: integer);

********************************************************************************/

void wrapper_dates(
    /** string pointer */ char* s,
    /** string length */  int l,
    /** time          */  long t
)

{

    pa_dates(s, l); /* find time string */
    cstr2pad(s, l); /* convert output to padded */

}

/********************************************************************************

function times(t: integer): pstring;

********************************************************************************/

pstring wrapper_datesp(
    /** time          */  long t
)

{

    char buff[1024];

    pa_dates(buff, 1024); /* find time string */
`    
    return (cstr2pstr(buff, 1024); /* return pstring */

}

/********************************************************************************

procedure writetime(var f: text; t: integer);

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

********************************************************************************/

void wrapper_writetime(
    /** time */          long t
)

{

    pa_writetime(stdout, t);

}

/********************************************************************************

procedure writetime(var f: text; t: integer);

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

procedure writetime(var f: text; t: integer);

********************************************************************************/

void wrapper_writedate(
    /** time */ long t
)

{

    pa_writedate(stdout, t);

}

/********************************************************************************

function time: integer;

********************************************************************************/

long wrapper_time(void)

{

    return (pa_time());

}

/********************************************************************************

function local(t: integer): integer;

********************************************************************************/

long wrapper_local(
    /** time */ long t
)

{

    return (pa_local(t));

}

/********************************************************************************

function clock: integer;

********************************************************************************/

long wrapper_clock(void)

{

    return (pa_clock());

}

/********************************************************************************

function elapsed(r: integer): integer;

********************************************************************************/

long wrapper_elapsed(
    /** time */ long t
)

{

    return (pa_elapsed(t));

}

/********************************************************************************

function validfile(view s: string): boolean;

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

********************************************************************************/

services.wild$f_vc_i:
    function pa_wild, 2

/********************************************************************************

function wild(view s: pstring): boolean;

********************************************************************************/

services.wild$f_pvc:
    preamble    1, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_wild             # call C routine
    postamble

/********************************************************************************

procedure getenv(view ls: string; var ds: string);

********************************************************************************/

services.getenv$p_vc_vc:
    preamble    0, 4
    pushq   %rdx                # save string
    pushq   %rcx                # save length
    call    pa_getenvl          # call C routine
    pop     %rsi                # restore length
    popq    %rdi                # restore string
    call    cstr2pad            # convert result to padded
    postamble

/********************************************************************************

function getenv(view ls: string): pstring;

********************************************************************************/

services.getenv$f_vc:
    preamble    1, 2
    subq    $1024,%rsp          # allocate stack buffer
    movq    %rsp,%rdx           # index that
    movq    $1024,%rcx          # set maximum length
    call    pa_getenvl          # get string in buffer
    movq    %rsp,%rdi           # index buffer
    movq    $1024,%rsi          # set maximum length
    call    cstr2pstr           # convert to pstring
    addq    $1024,%rsp          # deallocate stack buffer
    postamble

/********************************************************************************

procedure setenv(view sn, sd: string);

********************************************************************************/

services.setenv$p_vc_vc:
    procedure pa_setenvl, 4

/********************************************************************************

procedure setenv(sn: pstring; view sd: string);

********************************************************************************/

services.setenv$p_pvc_vc:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    setenvl             # call C routine
    postamble

/********************************************************************************

procedure setenv(view sn: string; sd: pstring);

********************************************************************************/

services.setenv$p_vc_pvc:
    preamble    0, 3
    movq    (%rdx),%rcx         # move string len to 2nd
    addq    $8,%rdx             # index string data
    call    setenvl             # call C routine
    postamble


/********************************************************************************

procedure setenv(sn, sd: pstring);

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

********************************************************************************/

services.allenv$p_:
    preamble    0, 1
    push    %rdi                # save env*
    call    pa_allenv           # call C function
    call    cenvlist2pascaline  # convert env list
    postamble

/********************************************************************************

procedure remenv(view sn: string);

********************************************************************************/

services.remenv$p_vc:
    procedure   pa_remenvl, 0, 2

/********************************************************************************

procedure remenv(view sn: pstring);

********************************************************************************/

services.remenv$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_remenvl             # call C routine
    postamble

/********************************************************************************

procedure exec(view cmd: string);

********************************************************************************/

services.exec$p_vc:
    procedure   pa_execl, 0, 2

/********************************************************************************

procedure exec(cmd: pstring);

********************************************************************************/

services.exec$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execl             # call C routine
    postamble

/********************************************************************************

procedure exece(view cmd: string; el: envptr);

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

********************************************************************************/

services.execw$p_vc_i:
    procedure   pa_execwl, 0, 3

/********************************************************************************

procedure execw(cmd: pstring; var e: integer);

********************************************************************************/

services.execw$p_pvc_i:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execwl            # call C routine
    postamble

/********************************************************************************

procedure execew(view cmd: string; el: envptr; var e: integer);

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

procedure getcur(var fn: string);

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

********************************************************************************/

services.setcur$p_vc:
    procedure pa_setcurl, 0, 2

/********************************************************************************

procedure setcur(fn: pstring);

********************************************************************************/

services.setcur$f_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_setcurl          # call C routine
    postamble

/********************************************************************************

brknam wrapper function with stringl

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

brknam wrapper function with pstring

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

brknam wrapper function with pstring

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

procedure maknam(var fn: string; view p, n, e: string);

********************************************************************************/

/********************************************************************************

function maknam(view p, n, e: string): pstring;

********************************************************************************/

/********************************************************************************

function maknam(view p: string; view n: string; e: pstring): pstring;

********************************************************************************/

/********************************************************************************

function maknam(view p: string; n: pstring; view e: string): pstring;

********************************************************************************/

/********************************************************************************

function maknam(view p: string; n: pstring; e: pstring): pstring;

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; view n: string; view e: string): pstring;

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; view n: string; e: pstring): pstring;

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; n: pstring; view e: string): pstring;

********************************************************************************/

/********************************************************************************

function maknam(p: pstring; n: pstring; e: pstring): pstring;

********************************************************************************/

/********************************************************************************

procedure fulnam(var fn: string);

********************************************************************************/

/********************************************************************************

function fulnam(view fn: string): pstring;

********************************************************************************/

/********************************************************************************

procedure getpgm(var p: string);

********************************************************************************/

/********************************************************************************

function getpgm: pstring;

********************************************************************************/

/********************************************************************************

procedure getusr(var fn: string);

********************************************************************************/

/********************************************************************************

function getusr: pstring;

********************************************************************************/

/********************************************************************************

procedure setatr(view fn: string; a: attrset);

********************************************************************************/

/********************************************************************************

procedure setatr(fn: pstring; a: attrset);

********************************************************************************/

/********************************************************************************

procedure resatr(view fn: string; a: attrset);

********************************************************************************/

/********************************************************************************

procedure resatr(fn: pstring; a: attrset);

********************************************************************************/

/********************************************************************************

procedure bakupd(view fn: string);

********************************************************************************/

/********************************************************************************

procedure bakupd(fn: pstring);

********************************************************************************/

/********************************************************************************

procedure setuper(view fn: string; p: permset);

********************************************************************************/

/********************************************************************************

procedure setuper(fn: pstring; p: permset);

********************************************************************************/

/********************************************************************************

procedure resuper(view fn: string; p: permset);

********************************************************************************/

/********************************************************************************

procedure resuper(fn: pstring; p: permset);

********************************************************************************/

/********************************************************************************

procedure setgper(view fn: string; p: permset);

********************************************************************************/

/********************************************************************************

procedure setgper(fn: pstring; p: permset);

********************************************************************************/

/********************************************************************************

procedure resgper(view fn: string; p: permset);

********************************************************************************/

/********************************************************************************

procedure resgper(fn: pstring; p: permset);

********************************************************************************/

/********************************************************************************

procedure setoper(view fn: string; p: permset);

********************************************************************************/

/********************************************************************************

procedure setoper(fn: pstring; p: permset);

********************************************************************************/

/********************************************************************************

procedure resoper(view fn: string; p: permset);

********************************************************************************/

/********************************************************************************

procedure resoper(fn: pstring; p: permset);

********************************************************************************/

/********************************************************************************

procedure makpth(view fn: string);

********************************************************************************/

/********************************************************************************

procedure makpth(fn: pstring);

********************************************************************************/

/********************************************************************************

procedure rempth(view fn: string);

********************************************************************************/

/********************************************************************************

procedure rempth(fn: pstring);

********************************************************************************/

/********************************************************************************

procedure filchr(var fc: schar);

********************************************************************************/

/********************************************************************************

function optchr: char;

********************************************************************************/

/********************************************************************************

function pthchr: char;

********************************************************************************/

/********************************************************************************

function latitude: integer;

********************************************************************************/

/********************************************************************************

function longitude: integer;

********************************************************************************/

/********************************************************************************

function altitude: integer;

********************************************************************************/

/********************************************************************************

function country: integer;

********************************************************************************/

/********************************************************************************

procedure countrys(view s: string; len: integer; c: integer);

********************************************************************************/

/********************************************************************************

function timezone: integer;

********************************************************************************/

/********************************************************************************

function daysave: integer;

********************************************************************************/

/********************************************************************************

function time24hour: integer;

********************************************************************************/

/********************************************************************************

function language: integer;

********************************************************************************/

/********************************************************************************

procedure languages(view s: string; len: integer; l: integer);

********************************************************************************/

/********************************************************************************

function decimal: char;

********************************************************************************/

/********************************************************************************

function numbersep: char;

********************************************************************************/

/********************************************************************************

function timeorder: integer;

********************************************************************************/

/********************************************************************************

function dateorder: integer;

********************************************************************************/

/********************************************************************************

function datesep: char;

********************************************************************************/

/********************************************************************************

function timesep: char;

********************************************************************************/

/********************************************************************************

function currchr: char;

********************************************************************************/
