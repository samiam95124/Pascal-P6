/*******************************************************************************
*                                                                              *
*                     Graphics Library Support for Pascaline                   *
*                                                                              *
*                              Created 2024                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Types and helper declarations shared by the Pascaline graphics wrappers.     *
* The wrappers bridge the Pascaline graphics module to the Petit-Ami C          *
* graphics module (ami_* in graphics.h). Graphics is a superset of terminal,    *
* so the event record and file conventions match the terminal binding.          *
*                                                                              *
*******************************************************************************/

#ifndef __GRAPHICS_WRAPPER_H__
#define __GRAPHICS_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/* Pascaline string: passed as a char* followed by an int length. */
typedef char* string;

/* Header structure for pstring (length prefixed heap string). */
typedef struct {

    long len;  /* length of string */
    char data; /* first character of string data */

} pstring_header;
typedef pstring_header* pstring;

/* Pascaline file pointer (handle to a Pascaline file control block) */
typedef unsigned char* pfile;

/*
 * Pascaline event record layout (from the generated DWARF for graphics.pas).
 * Fixed fields are forward (winid@0, handled@8, etype@9); the variant fields
 * are laid out in reverse declaration order from offset 12. The conversion
 * routines in graphics_support.c read/write these by exact offset.
 */
#define PA_EVT_WINID   0  /* long  winid            */
#define PA_EVT_HANDLED 8  /* char  handled          */
#define PA_EVT_ETYPE   9  /* char  etype (tag)      */
#define PA_EVT_DATA    12 /* base of variant fields */

/* helpers to access a Pascaline evtrec by byte offset */
#define PEVTL(p, off)  (*(long*)((char*)(p)+(off)))  /* 8 byte integer field */
#define PEVTC(p, off)  (*(char*)((char*)(p)+(off)))  /* 1 byte char/bool field */

/*
 * File conversion helpers, supplied by the psystem runtime. Convert a Pascaline
 * file handle to its libc FILE* equivalent.
 */
FILE* psystem_libcwrfil(pfile f); /* libc write file equivalent */
FILE* psystem_libcrdfil(pfile f); /* libc read file equivalent */

/* Null-terminate a counted Pascaline string into a rotating buffer. */
char* cstrz(char* s, int l);

#ifdef __cplusplus
}
#endif

#endif /* __GRAPHICS_WRAPPER_H__ */
