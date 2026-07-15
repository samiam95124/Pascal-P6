/*******************************************************************************
*                                                                              *
*                  Common Pascaline/C Support for Ami Bindings                 *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Types, macros and routines shared by the Pascaline bindings (services,        *
* terminal, graphics, ...). These are the conversions and call mechanisms that  *
* do not depend on any one binding's module header: the Pascaline data types as *
* seen from C, the counted/heap string conversions, the event-record byte       *
* layout, and the machinery to call a Pascaline procedure (and build event      *
* callback thunks) from C. Each binding's own support file keeps only what is    *
* specific to its module (its event-record conversion and its wrappers).        *
*                                                                              *
*******************************************************************************/

#ifndef __SUPPORT_H__
#define __SUPPORT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/*
 * Windows x64 is LLP64: long is 32 bits, unlike every other 64 bit
 * platform. The Pascaline-facing types below use long as the machine
 * word/Pascal integer type, which scales with the target on all other
 * systems (64 bits on LP64 systems, 32 bits on 32 bit targets). For
 * Windows only, long is redefined to long long (64 bits), matching
 * psystem.c. This must appear after all system includes and after the
 * Ami model header (services.h, terminal.h, ...) so that the C library
 * prototypes and the Ami structures keep their true types -- every
 * binding source includes support.h last for this reason.
 */
#ifdef _WIN32
#define long long long
#define labs llabs
#undef LONG_MAX
#define LONG_MAX LLONG_MAX
#undef LONG_MIN
#define LONG_MIN LLONG_MIN
#endif

/* Pascaline string: passed as a char* followed by an int length. */
typedef char* string;

/* Header for a pstring (length-prefixed heap string); data follows the len. */
typedef struct {

    long len;  /* length of string */
    char data; /* first character of string data */

} pstring_header;
typedef pstring_header* pstring;

/* Pascaline file handle (a Pascaline file control block). */
typedef unsigned char* pfile;

/*
 * Pascaline event record byte layout (from the generated DWARF). The fixed
 * fields are forward (winid@0, handled@8, etype@9); the variant fields are laid
 * out in reverse declaration order from offset 12. Each binding's cevt2pascaline
 * fills these by exact offset.
 */
#define PA_EVT_WINID   0  /* long  winid            */
#define PA_EVT_HANDLED 8  /* char  handled          */
#define PA_EVT_ETYPE   9  /* char  etype (tag)      */
#define PA_EVT_DATA    12 /* base of variant fields */

#define PEVTL(p, off)  (*(long*)((char*)(p)+(off)))  /* 8 byte integer field */
#define PEVTC(p, off)  (*(char*)((char*)(p)+(off)))  /* 1 byte char/bool field */

/* File conversion helpers, supplied by the psystem runtime. */
FILE* psystem_libcwrfil(pfile f); /* libc write file equivalent */
FILE* psystem_libcrdfil(pfile f); /* libc read file equivalent */
void  psystem_libcatcfil(pfile f, FILE* fp, long wr); /* attach libc file */

/* String conversions between Pascaline and C forms. */
pstring cstr2pstr(char* cs, int l);       /* C string -> heap pstring */
char*   pstr2cstr(pstring ps);            /* pstring -> heap C string */
void    pstr2cstrl(pstring ps, char** s, int* l); /* pstring -> char*+len */
void    cpstrp2cstrl(pstring* ps, int* l);/* pstring -> char*+len in place */
void    cstr2pad(char* cs, int l);        /* C string -> space padded */
void    rempad(char* cs, int* l);         /* trim right space padding */
char*   cstrz(char* s, int l);            /* counted string -> zero-terminated */

/*
 * Call a Pascaline procedure from C. A Pascaline procedure value is a pair
 * (code address, display pointer); pacall sets %rbp to the display, passes arg
 * in %rdi, and enters code.
 */
void pacall(void* code, void* display, void* arg);

/*
 * Build an executable thunk presenting a single C function pointer that, when
 * called with one argument, tail-calls dispatch(code, display, arg). Used to
 * hand a Pascaline procedure to Ami as a plain callback. dispatch is the
 * binding's own event dispatcher (which knows that binding's event record).
 */
void* mkevtthunk(void* code, void* display, void* dispatch);

#ifdef __cplusplus
}
#endif

#endif /* __SUPPORT_H__ */
