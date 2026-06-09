/*******************************************************************************
*                                                                              *
*                     Services Library Support for Pascaline                   *
*                                                                              *
*                              Created 2025                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Contains code to convert C structures from Ami services library to           *
* Pascaline form. The common Pascaline/C types and string conversions are in   *
* support.h; this header adds the services-specific record formats.   *
*                                                                              *
*******************************************************************************/

#ifndef __SERVICES_WRAPPER_H__
#define __SERVICES_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <support.h>

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

/* environment strings */
typedef struct envrec {

    pstring name;    /* name of string (zero terminated) */
    pstring data;    /* data in string (zero terminated) */
    struct envrec *next; /* next entry in list */

} envrec;
typedef envrec* envptr; /* pointer to environment record */

/*
 * Length of string buffers
 */
#define BUFLEN 1024

/*
 * Services-specific support routines
 */
void cfilelist2pascaline(ami_filptr* fla); /* convert C files list to Pascaline files list */
void cenvlist2pascaline(ami_envptr* eva); /* convert C environment string list to Pascaline */
ami_envptr cenvlist2c(envptr el); /* convert Pascaline environment string list to C */
void freenvl(ami_envptr el); /* free environment list */

#ifdef __cplusplus
}
#endif

#endif
