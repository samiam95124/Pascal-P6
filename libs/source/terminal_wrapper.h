/*******************************************************************************
*                                                                              *
*                     Terminal Library Support for Pascaline                   *
*                                                                              *
*                              Created 2024                                    *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Types and helper declarations shared by the Pascaline terminal wrappers.     *
* The wrappers bridge the Pascaline terminal module to the Petit-Ami C         *
* terminal module (ami_* in terminal.h).                                       *
*                                                                              *
*******************************************************************************/

#ifndef __TERMINAL_WRAPPER_H__
#define __TERMINAL_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

/*
 * Pascaline string: passed as a char* followed by an int length.
 */
typedef char* string;

/*
 * Header structure for pstring (length prefixed heap string).
 */
typedef struct {

    long len;  /* length of string */
    char data; /* first character of string data */

} pstring_header;
typedef pstring_header* pstring;

/* Pascaline file pointer (handle to a Pascaline file control block) */
typedef unsigned char* pfile;

/*
 * Pascaline event record.
 *
 * This mirrors the layout that the Pascal-P6 compiler assigns to the Pascaline
 * 'evtrec' variant record in terminal.pas, as determined from the generated
 * DWARF. The fixed fields are laid out forward (winid@0, handled@8, etype@9),
 * but the variant fields are laid out in reverse declaration order starting at
 * offset 12. The conversion routines in terminal_support.c read and write
 * these fields by their exact offsets, so this struct is documentation of the
 * layout; the multi-field variants are accessed through the offset macros in
 * terminal_support.c rather than through named members here.
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

#ifdef __cplusplus
}
#endif

#endif /* __TERMINAL_WRAPPER_H__ */
