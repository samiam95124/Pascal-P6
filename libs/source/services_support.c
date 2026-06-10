/*******************************************************************************
*                                                                              *
*                     Services Library Support for Pascaline                   *
*                                                                              *
*                              Created 2025                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Contains code to convert C structures from Ami services library to           *
* Pascaline form.                                                              *
*                                                                              *
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <services.h>
#include <services_wrapper.h>
#include <support.h>

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
    /** file record list head */ ami_filptr* fla
)

{

    /* pointer for pascaline string */ pstring ps;
    /* length */                       int l;
    /* pointer to string */            char* s;
    /* pointer to old list */          ami_filptr fl;
    /* pointer to last old entry */    ami_filptr np;
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
    *fla = (ami_filptr)nl;

}

/********************************************************************************

Convert environment list to Pascaline form

Accepts an environment list in Pascaline form, and converts all of the string
data constained into C zero terminated string form.

********************************************************************************/

void cenvlist2pascaline(
    /** file record list head */ ami_envptr* eva
)

{

    /* pointer to old list */ ami_envptr el;
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

ami_envptr cenvlist2c(
    /** environment record list head */ envptr el
)

{

    /* pstring */ pstring ps;
    /* new copy list in C */ ami_envptr cl;
    /* new entry */ ami_envptr cp;
    /* last entry */ ami_envptr lp;

    lp = NULL; /* set no last entry */
    while (el) { /* traverse */

        cp = malloc(sizeof(ami_envrec)); /* get new C entry */
        cp->name = pstr2cstr((pstring)el->name); /* convert name */
        cp->data = pstr2cstr((pstring)el->data); /* convert data */
        cp->next = NULL;
        if (lp) lp->next = cp; /* link last to this */
        else cl = cp; /* set as root */
        lp = cp; /* set new last */
        el = el->next; /* go next entry */

    }

    return cl;

}

/********************************************************************************

Free environment list in C form

Frees both strins and the entry for the entire list.

********************************************************************************/

void freenvl(
    /** environment record list head */ ami_envptr el
)

{

    /* element pointer */ ami_envptr ep;

    while (el) { /* drain */

        ep = el; /* copy top pointer */
        el = el->next; /* next entry */
        free(ep->name); /* free strings */
        free(ep->data);
        free(ep); /* free the structure */

    }

}
