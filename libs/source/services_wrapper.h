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
