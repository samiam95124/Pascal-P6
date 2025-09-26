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

#ifndef __SERVICES_WRAPPER_H__
#define __SERVICES_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Pascaline string
 *
 * Passed as parameter is char* followed by length 
 */
typedef char* string;

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
typedef unsigned char* pfile;

/*
 * Length of string buffers
 */
#define BUFLEN 1024

/*
 * Services support routines
 */

pstring cstr2pstr(char* cs, int l); /* convert C string to pstring */
char* pstr2cstr(pstring ps); /* convert pstring to C string in place */
void pstr2cstrl(pstring ps, char** s, int* l); /* convert pstring to C string with length */
void cpstrp2cstrl(pstring* ps, int* l); /* convert pstring to C string in place with length */
void cstr2pad(char* cs, int l); /* convert C string to padded Pascaline */
void cfilelist2pascaline(pa_filptr* fla); /* convert C files list to Pascaline files list */
void cenvlist2pascaline(pa_envptr* eva); /* convert C environment string list to Pascaline */
void cenvlist2c(pa_envptr* eva); /* convert Pascaline environment string list to C */
FILE* psystem_libcwrfil(pfile f); /* Find libc write file equivalent */
FILE* psystem_libcrdfil(pfile f); /* Find libc read file equivalent */

#ifdef __cplusplus
}
#endif

#endif