/*******************************************************************************
*                                                                              *
*                         Services Library Header                              *
*                                                                              *
*                              Created 1996                                    *
*                                                                              *
*                               S. A. MOORE                                    *
*                                                                              *
*******************************************************************************/

#ifndef __SERVICES_H__
#define __SERVICES_H__

#include <stdio.h>

/*
 * Set manipulation operators for chrset.
 *
 * These are used to change the character set that defines what characters
 * are admissible in filenames.
 */
#define SETLEN 32 /* length of char set */
#define INSET(s, b) (s[b>>3] & 1<<b%8) /* test inclusion */
#define ADDSET(s, b) (s[b>>3] |= 1<<b%8) /* add set member */
#define SUBSET(s, b) (s[b>>3] &= ~(1<<b%8)) /* remove set member */
#define CLRSET(s) { int i; for (i = 0; i < SETLEN; i++) s[i] = 0; } /* clear set */

/* attributes */
typedef enum {

    pa_atexec, /* is an executable file type */
    pa_atarc,  /* has been archived since last modification */
    pa_atsys,  /* is a system special file */
    pa_atdir,  /* is a directory special file */
    pa_atloop  /* contains heriarchy loop */

} pa_attribute;
typedef int pa_attrset; /* attributes in a set */

/* permissions */
typedef enum {

    pa_pmread,  /* may be read */
    pa_pmwrite, /* may be written */
    pa_pmexec,  /* may be executed */
    pa_pmdel,   /* may be deleted */
    pa_pmvis,   /* may be seen in directory listings */
    pa_pmcopy,  /* may be copied */
    pa_pmren    /* may be renamed/moved */

} pa_permission;
typedef int pa_permset; /* permissions in a set */

/* standard directory format */
typedef struct pa_filrec {

    char*             name;    /* name of file (zero terminated) */
    long long         size;    /* size of file */
    long long         alloc;   /* allocation of file */
    pa_attrset        attr;    /* attributes */
    long              create;  /* time of creation */
    long              modify;  /* time of last modification */
    long              access;  /* time of last access */
    long              backup;  /* time of last backup */
    pa_permset        user;    /* user permissions */
    pa_permset        group;   /* group permissions */
    pa_permset        other;   /* other permissions */
    struct pa_filrec* next;    /* next entry in list */

} pa_filrec;
typedef pa_filrec* pa_filptr; /* pointer to file records */

/* environment strings */
typedef struct pa_envrec {

    char* name;    /* name of string (zero terminated) */
    char* data;    /* data in string (zero terminated) */
    struct pa_envrec *next; /* next entry in list */

} pa_envrec;
typedef pa_envrec* pa_envptr; /* pointer to environment record */

/* character set */
typedef unsigned char pa_chrset[SETLEN];

/*
 * Functions exposed in the services module
 */
extern void pa_list(char* f, pa_filrec **l);
extern void pa_times(char* s, int sl, int t);
extern void pa_dates(char* s, int sl, int t);
extern void pa_writetime(FILE *f, int t);
extern void pa_writedate(FILE *f, int t);
extern int  pa_time(void);
extern int  pa_local(int t);
extern int  pa_clock(void);
extern int  pa_elapsed(int r);
extern int  pa_validfile(char* s);
extern int  pa_validpath(char* s);
extern int  pa_wild(char* s);
extern void pa_getenv(char* ls, char* ds, int dsl);
extern void pa_setenv(char* sn, char* sd);
extern void pa_allenv(pa_envrec **el);
extern void pa_remenv(char* sn);
extern void pa_exec(char* cmd);
extern void pa_exece(char* cmd, pa_envrec *el);
extern void pa_execw(char* cmd, int *e);
extern void pa_execew(char* cmd, pa_envrec *el, int *e);
extern void pa_getcur(char* fn, int l);
extern void pa_setcur(char* fn);
extern void pa_brknam(char* fn, char* p, int pl, char* n, int nl, char* e, int el);
extern void pa_maknam(char* fn, int fnl, char* p, char* n, char* e);
extern void pa_fulnam(char* fn, int fnl);
extern void pa_getpgm(char* p, int pl);
extern void pa_getusr(char* fn, int fnl);
extern void pa_setatr(char* fn, pa_attrset a);
extern void pa_resatr(char* fn, pa_attrset a);
extern void pa_bakupd(char* fn);
extern void pa_setuper(char* fn, pa_permset p);
extern void pa_resuper(char* fn, pa_permset p);
extern void pa_setgper(char* fn, pa_permset p);
extern void pa_resgper(char* fn, pa_permset p);
extern void pa_setoper(char* fn, pa_permset p);
extern void pa_resoper(char* fn, pa_permset p);
extern void pa_makpth(char* fn);
extern void pa_rempth(char* fn);
extern void pa_filchr(pa_chrset fc);
extern char pa_optchr(void);
extern char pa_pthchr(void);
extern int  pa_latitude(void);
extern int  pa_longitude(void);
extern int  pa_altitude(void);
extern int  pa_country(void);
extern void pa_countrys(char* s, int sl, int c);
extern int  pa_timezone(void);
extern int  pa_daysave(void);
extern int  pa_time24hour(void);
extern int  pa_language(void);
extern void pa_languages(char* s, int sl, int l);
extern char pa_decimal(void);
extern char pa_numbersep(void);
extern int  pa_timeorder(void);
extern int  pa_dateorder(void);
extern char pa_datesep(void);
extern char pa_timesep(void);
extern char pa_currchr(void);

#endif
