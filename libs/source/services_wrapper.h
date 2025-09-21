/*******************************************************************************
*                                                                              *
*                     Services Library Support for Pascaline                   *
*                                                                              *
*                              Created 2025                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Contains code to convert C structures from Petit-Ami services library to     *
* Pascaline form.                                                              *
*                                                                              *
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <services.h>

/*
* Header structure for pstring.
*
* The structure is:
*
* length
* string data
*
* The string data is any length, and is allocated at the end of the structure.
* It is based on a single character, but actually can be any length.
*/
typedef struct {

    long len;
    char data;

} pstring_header;
typedef pstring_header* pstring;

/* Pascaline sets are 256, or 4*64 bits */
typedef long set[4];

/* Pascaline directory format, has 256 bit sets */
typedef struct filrec {

    char*          name;    /* name of file (zero terminated) */
    long long      size;    /* size of file */
    long long      alloc;   /* allocation of file */
    set            attr;    /* attributes */
    long           create;  /* time of creation */
    long           modify;  /* time of last modification */
    long           access;  /* time of last access */
    long           backup;  /* time of last backup */
    set            user;    /* user permissions */
    set            group;   /* group permissions */
    set            other;   /* other permissions */
    struct filrec* next;    /* next entry in list */

} filrec;
typedef filrec* filptr; /* pointer to file records */

/* pascaline file pointer */
typedef pfile unsigned char*;

/********************************************************************************

Convert C zero terminated string to pstring

Accepts a C string in buffer with a buffer length. The string is allocated in
heap, and returned in pstring format.

********************************************************************************/

pstring cstr2pstr(
    /** C string buffer */ char* cs,
    /** length of buffer */ int l
)

{

    /* pointer for pascaline string */ pstring ps;
    /* pointer to string */            char* s;

    l = strnlen(cs, l); /* get length of string */
    /* allocate header+data */
    ps = malloc(sizeof(pstring_header)+l);
    ps->len = l; /* set length */
    /* point to string after the pstring header */
    s = (char*)&ps->data;
    strncpy(s, cs, l); /* copy name string to new pstring */

    return (ps);

}

/********************************************************************************

Convert pstring to C zero terminated string

Accepts a pstring and a C output buffer and length. The pstring is copied to the
output buffer and zero terminated.

********************************************************************************/

char* pstr2cstr(
    /** pstring */ pstring ps
)

{

    /* pointer to string */ char* cs;

    cs = malloc(ps->len+1);
    strncpy(cs, (char*)&ps->data, ps->len); /* copy string data */

    return ((char*) ps);

}

/********************************************************************************

pstring to C zero terminated string pointer with length

Accepts a pstring and converts that to a char* and length.

********************************************************************************/

void pstr2cstrl(
    /** pstring */ pstring ps,
    /** cstring */ char**   s,
    /** length */  int* l
)

{

    *l = (*ps)->len; /* get length */
    *s = (char*)&((*ps)->data); /* index data */

}

/********************************************************************************

Convert pstring to C zero terminated string pointer in place

Accepts a pointer to a pstring and converts that to a char* and length.

********************************************************************************/

void cpstrp2cstrl(
    /** pstring */ pstring* ps,
    /** length */  int* l
)

{

    *l = (*ps)->len; /* get length */
    *ps = (pstring)&((*ps)->data); /* index data */

}

/********************************************************************************

Convert file list to Pascaline form

Accepts a file list from the list()/listl() call in services.

The resulting file list must be converted to Pascaline form. It is translated 
into a new list, and the original list disposed of. The following translations
are performed:

1. C has different ideas about string pointers. In the original list, the 
filename field is a simple zero terminated string. In Pascaline, its a pstring
or pointer to string. These are designed to be allocated dynamically, and so the
heap allocation starts with a template for the string, then the string data 
follows like:

0: String address
4: Length of string.
8-N: Data in string.

2. Sets are 256 bits (32 bytes) in Pascaline.

This routine goes through each entry in a files list, converting each of the
filename entries to a new structure and disposing of the old.

********************************************************************************/

void cfilelist2pascaline(
    /** file record list head */ pa_filptr* fla
)

{

    /* pointer for pascaline string */ pstring ps;
    /* length */                       int l;
    /* pointer to string */            char* s;
    /* pointer to old list */          pa_filptr fl;
    /* pointer to last old entry */    pa_filptr np;
    /* new list */                     filptr nl;
    /* last entry */                   filptr lp;
    /* new list pointer */             filptr p;

    nl = NULL; /* create new list */
    lp = NULL; /* set no last */
    fl = *fla; /* get old list */
    while (fl) { /* for each list record */

        p = malloc(sizeof(filrec)); /* create new entry */
        if (!nl) nl = p; /* start new list if empty */
        /* copy name into pstring form in new entry */
        p->name = (char*)cstr2pstr(fl->name, strlen(fl->name));
        free(fl->name); /* free the old string */
        /* transfer other entries, converting sets */
        p->size = fl->size;
        p->alloc = fl->alloc;
        p->attr[0] = fl->attr;
        p->attr[1] = 0;
        p->attr[2] = 0;
        p->attr[3] = 0;
        p->create = fl->create;
        p->modify = fl->modify;
        p->access = fl->access;
        p->backup = fl->backup;
        p->user[0] = fl->user;
        p->user[1] = 0;
        p->user[2] = 0;
        p->user[3] = 0;
        p->group[0] = fl->group;
        p->group[1] = 0;
        p->group[2] = 0;
        p->group[3] = 0;
        p->other[0] = fl->other;
        p->other[1] = 0;
        p->other[2] = 0;
        p->other[3] = 0;
        p->next = NULL; /* terminate */
        np = fl->next; /* save next old entry */
        free(fl); /* release old entry */
        fl = np; /* go next entry */
        /* link last entry new */
        if (lp) lp->next = p;
        lp = p; /* set new last entry */

    }
    /* set address of converted list */
    *fla = (pa_filptr)nl;

}

/********************************************************************************

Convert C zero terminated string to padded

Accepts a C string in buffer with a buffer length. The buffer past the string is
cleared to spaces, including the terminating zero.

********************************************************************************/

void cstr2pad(
    /** C string buffer */ char* cs,
    /** length of buffer */ int l
)

{

    while (l && *cs) { l--; cs++; }
    while (l) { l--; *cs++ = ' '; }

}



/********************************************************************************

Convert environment list to Pascaline form

Accepts an environment list in Pascaline form, and converts all of the string
data constained into C zero terminated string form.

********************************************************************************/

void cenvlist2pascaline(
    /** file record list head */ pa_envptr* eva
)

{

    /* pointer to old list */ pa_envptr el;
    /* C sring pointer */      char*cs;

    el = *eva; /* index list */
    while (el) { /* traverse */

        /* convert name */
        cs = el->name; /* save old string */
        el->name = (char*)cstr2pstr(el->name, strlen(el->name));
        free(cs); /* free old string */
        /* convert data */
        cs = el->data; /* save old string */
        el->name = (char*)cstr2pstr(el->data, strlen(el->data));
        free(cs); /* free old string */
        el = el->next; /* go next entry */

    }

}

/********************************************************************************

Convert environment list to C form

********************************************************************************/

void cenvlist2c(
    /** file record list head */ pa_envptr* eva
)

{

    /* pointer to old list */ pa_envptr el;
    /* pstring */             pstring ps;

    el = *eva; /* index list */
    while (el) { /* traverse */

        /* convert name */
        ps = (pstring)el->name; /* save old string */
        el->name = pstr2cstr((pstring)el->name); /* convert */
        free(ps); /* free old string */
        /* convert data */
        ps = (pstring)el->data; /* save old string */
        el->name = pstr2cstr((pstring)el->data); /* convert */
        free(ps); /* free old string */
        el = el->next; /* go next entry */

    }

}

/********************************************************************************

list wrapper function

procedure list(view f: string; var l: filptr);

********************************************************************************/

void wrapper_list(
    /** filename */ char* fn, fnl,
    /** file list */ filptr* fl
)

{

    /* get files list in C form */
    pa_listl(fn, fnl, fl); 
    /* convert list to Pascaline form */
    cfilelist2pascaline(fl);

}

/********************************************************************************

list wrapper function

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

times wrapper function

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

times wrapper function with pstring

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

times wrapper function

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

times wrapper function with pstring

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

writetime wrapper function with file

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

writetime wrapper function

procedure writetime(var f: text; t: integer);

********************************************************************************/

void wrapper_writetime(
    /** time */          long t
)

{

    pa_writetime(stdout, t);

}

/********************************************************************************

writedate wrapper function with file

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

writedate wrapper function

procedure writetime(var f: text; t: integer);

********************************************************************************/

void wrapper_writedate(
    /** time */ long t
)

{

    pa_writedate(stdout, t);

}

/********************************************************************************

time wrapper function

function time: integer;

********************************************************************************/

long wrapper_time(void)

{

    return (pa_time());

}

/********************************************************************************

local wrapper function

function local(t: integer): integer;

********************************************************************************/

long wrapper_local(
    /** time */ long t
)

{

    return (pa_local(t));

}

/********************************************************************************

clock wrapper function

function clock: integer;

********************************************************************************/

long wrapper_clock(void)

{

    return (pa_clock());

}

/********************************************************************************

elapsed wrapper function

function elapsed(r: integer): integer;

********************************************************************************/

long wrapper_elapsed(
    /** time */ long t
)

{

    return (pa_elapsed(t));

}

/********************************************************************************

validfile wrapper function

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

validfile wrapper function

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

validpath wrapper function

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

validpath wrapper function

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

# function wild(view s: string): boolean; external;

services.wild$f_vc_i:
    function pa_wild, 2

# overload function wild(view s: pstring): boolean; external;

services.wild$f_pvc:
    preamble    1, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_wild             # call C routine
    postamble

# procedure getenv(view ls: string; var ds: string); external;

services.getenv$p_vc_vc:
    preamble    0, 4
    pushq   %rdx                # save string
    pushq   %rcx                # save length
    call    pa_getenvl          # call C routine
    pop     %rsi                # restore length
    popq    %rdi                # restore string
    call    cstr2pad            # convert result to padded
    postamble

# overload function getenv(view ls: string): pstring; external;

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

# procedure setenv(view sn, sd: string); external;

services.setenv$p_vc_vc:
    procedure pa_setenvl, 4

# overload procedure setenv(sn: pstring; view sd: string); external;

services.setenv$p_pvc_vc:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    setenvl             # call C routine
    postamble

# overload procedure setenv(view sn: string; sd: pstring); external;

services.setenv$p_vc_pvc:
    preamble    0, 3
    movq    (%rdx),%rcx         # move string len to 2nd
    addq    $8,%rdx             # index string data
    call    setenvl             # call C routine
    postamble

# overload procedure setenv(sn, sd: pstring); external;

services.setenv$p_pvc_pvc:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    movq    (%rdx),%rcx         # move string len to 2nd
    addq    $8,%rdx             # index string data
    call    setenvl             # call C routine
    postamble

# procedure allenv(var el: envptr); external;

services.allenv$p_:
    preamble    0, 1
    push    %rdi                # save env*
    call    pa_allenv           # call C function
    call    cenvlist2pascaline  # convert env list
    postamble

# procedure remenv(view sn: string); external;

services.remenv$p_vc:
    procedure   pa_remenvl, 0, 2

# overload procedure remenv(view sn: pstring); external;

services.remenv$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_remenvl             # call C routine
    postamble

# procedure exec(view cmd: string); external;

services.exec$p_vc:
    procedure   pa_execl, 0, 2

# overload procedure exec(cmd: pstring); external;

services.exec$p_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execl             # call C routine
    postamble

# procedure exece(view cmd: string; el: envptr); external;

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

# overload procedure exece(cmd: pstring; el: envptr); external;

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

# procedure execw(view cmd: string; var e: integer); external;

services.execw$p_vc_i:
    procedure   pa_execwl, 0, 3

# overload procedure execw(cmd: pstring; var e: integer); external;

services.execw$p_pvc_i:
    preamble    0, 3
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_execwl            # call C routine
    postamble

# procedure execew(view cmd: string; el: envptr; var e: integer); external;

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

# overload procedure execew(cmd: pstring; el: envptr; var e: integer); external;

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

# procedure getcur(var fn: string); external;

services.getcur$p_vc_i:
    preamble    0, 2
    pushq   %rdi                # save string
    pushq   %rsi                # save length
    call    pa_getcur           # call C function
    popq    %rsi                # restore length
    pop     %rdi                # restore string
    call    cstr2pad            # convert to padded
    postamble

# overload function getcur: pstring; external;

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

# procedure setcur(view fn: string); external;

services.setcur$p_vc:
    procedure pa_setcurl, 0, 2

# overload procedure setcur(fn: pstring); external;

services.setcur$f_pvc:
    preamble    0, 2
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    call    pa_setcurl          # call C routine
    postamble

# procedure brknam(view fn: string; var p, n, e: string); external;

services.brknam$p_vc_vc_vc_vc:
    preamble    0, 6
    swaptop
    call    wrapper_brknam      # execute wrapper
    popq    %r11                # dump 4th parameter
    popq    %r11
    postamble

# overload procedure brknam(view fn: string; var p, n, e: pstring); external;

services.brknam$p_vc_pvc_pvc_pvc:
    procedure wrapper_brknamp, 5

# overload procedure brknam(fn: pstring; var p, n, e: pstring); external;

services.brknam$p_pvc_pvc_pvc_pvc:
    preamble    0, 4
    call    wrapper_brknampp    # execute wrapper
    postamble

# procedure maknam(var fn: string; view p, n, e: string); external;

services.maknam$p_vc_vc_vc_vc:
    preamble    0, 8
    swaptop                     # swap the onstack parameters
    call    pa_maknaml          # call C function
    postamble

# overload function maknam(view p, n, e: string): pstring; external;
# overload function maknam(view p: string; view n: string; e: pstring): pstring; external;
# overload function maknam(view p: string; n: pstring; view e: string): pstring; external;
# overload function maknam(view p: string; n: pstring; e: pstring): pstring; external;
# overload function maknam(p: pstring; view n: string; view e: string): pstring; external;
# overload function maknam(p: pstring; view n: string; e: pstring): pstring; external;
# overload function maknam(p: pstring; n: pstring; view e: string): pstring; external;
# overload function maknam(p: pstring; n: pstring; e: pstring): pstring; external;


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

# procedure fulnam(var fn: string); external;
# overload function fulnam(view fn: string): pstring; external;
# procedure getpgm(var p: string); external;
# overload function getpgm: pstring; external;
# procedure getusr(var fn: string); external;
# overload function getusr: pstring; external;
# procedure setatr(view fn: string; a: attrset); external;
# overload procedure setatr(fn: pstring; a: attrset); external;
# procedure resatr(view fn: string; a: attrset); external;
# overload procedure resatr(fn: pstring; a: attrset); external;
# procedure bakupd(view fn: string); external;
# overload procedure bakupd(fn: pstring); external;
# procedure setuper(view fn: string; p: permset); external;
# overload procedure setuper(fn: pstring; p: permset); external;
# procedure resuper(view fn: string; p: permset); external;
# overload procedure resuper(fn: pstring; p: permset); external;
# procedure setgper(view fn: string; p: permset); external;
# overload procedure setgper(fn: pstring; p: permset); external;
# procedure resgper(view fn: string; p: permset); external;
# overload procedure resgper(fn: pstring; p: permset); external;
# procedure setoper(view fn: string; p: permset); external;
# overload procedure setoper(fn: pstring; p: permset); external;
# procedure resoper(view fn: string; p: permset); external;
# overload procedure resoper(fn: pstring; p: permset); external;
# procedure makpth(view fn: string); external;
# overload procedure makpth(fn: pstring); external;
# procedure rempth(view fn: string); external;
# overload procedure rempth(fn: pstring); external;
# procedure filchr(var fc: schar); external;
# function optchr: char; external;
# function pthchr: char; external;
# function latitude: integer; external;
# function pa_longitude: integer; external;
# function pa_altitude: integer; external;
# function pa_country: integer; external;
# procedure pa_countrys(view s: string; len: integer; c: integer); external;
# function pa_timezone: integer; external;
# function pa_daysave: integer; external;
# function pa_time24hour: integer; external;
# function pa_language: integer; external;
# procedure pa_languages(view s: string; len: integer; l: integer);  external;
# function pa_decimal: char; external;
# function pa_numbersep: char; external;
# function pa_timeorder: integer; external;
# function pa_dateorder: integer; external;
# function pa_datesep: char; external;
# function pa_timesep: char; external;
# function pa_currchr: char; external;
