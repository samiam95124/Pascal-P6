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
*/
typedef struct {

    long len;

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

printf("cfilelist2pascaline: begin: next offset: %lu\n", offsetof(struct filrec, next));
    nl = NULL; /* create new list */
    lp = NULL; /* set no last */
    fl = *fla; /* get old list */
    while (fl) { /* for each list record */

        p = malloc(sizeof(filrec)); /* create new entry */
        if (!nl) nl = p; /* start new list if empty */
        /* copy fields */
        l = strlen(fl->name); /* get length of string */
printf("cfilelist2pascaline: length: %d allocation size: %ld\n", l, sizeof(pstring_header)+l);
        /* allocate header+data */
        ps = malloc(sizeof(pstring_header)+l);
        ps->len = l; /* set length */
        /* point to string after the pstring header */
        s = ((char*)ps)+sizeof(pstring_header);
        strncpy(s, fl->name, l); /* copy name string to new pstring */
        free(fl->name); /* free the old string */
        p->name = (char*)ps; /* place pstring pointer */
printf("cfilelist2pascaline: fl: %p fl->name: %p\n", p, p->name);
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