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
#include <services_wrapper.h>

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

    return (cs);

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

    *l = ps->len; /* get length */
    *s = (char*)&ps->data; /* index data */

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

Remove right space padding from string with length

Removes space padding from the right side of a string/length pair by readjusting
the length.

********************************************************************************/

void rempad(
    /** C string buffer */ char* cs,
    /** length of buffer */ int* l
)

{

    cs += *l-1;
    while (l && *cs == ' ') { (*l)--; cs--; }

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
        el->data = (char*)cstr2pstr(el->data, strlen(el->data));
        free(cs); /* free old string */
        el = el->next; /* go next entry */

    }

}

/********************************************************************************

Convert environment list to C form

Preserves the original list. Returns C copy list.

********************************************************************************/

pa_envptr cenvlist2c(
    /** environment record list head */ envptr el
)

{

    /* pstring */ pstring ps;
    /* new copy list in C */ pa_envptr cl;
    /* new entry */ pa_envptr cp;
    /* last entry */ pa_envptr lp;

    lp = NULL; /* set no last entry */
    while (el) { /* traverse */

        cp = malloc(sizeof(pa_envrec)); /* get new C entry */
        cp->name = pstr2cstr((pstring)el->name); /* convert name */
        cp->data = pstr2cstr((pstring)el->data); /* convert data */
        cp->next = NULL;
        if (lp) lp->next = cp; /* link last to this */
        else cl = cp; /* set as root */
        lp = cp; /* set new last */
        el = el->next; /* go next entry */

    }

}

/********************************************************************************

Free environment list in C form

Frees both strins and the entry for the entire list.

********************************************************************************/

void freenvl(
    /** environment record list head */ pa_envptr el
)

{

    /* element pointer */ pa_envptr ep;

    while (el) { /* drain */

        ep = el; /* copy top pointer */
        el = el->next; /* next entry */
        free(ep->name); /* free strings */
        free(ep->data);
        free(ep); /* free the structure */

    }

}
