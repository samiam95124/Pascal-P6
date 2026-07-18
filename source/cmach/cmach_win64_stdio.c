/*******************************************************************************
*                                                                              *
*              Win64 STDIO_BYPASS gap fillers for the C interpreter            *
*                                                                              *
* cmach is compiled with -DSTDIO_BYPASS so its file table and the Ami model    *
* bindings share one stdio world (see the cmach build notes in the Makefile).  *
* Under STDIO_BYPASS the Ami stdio.h coins the plain stdio calls to their      *
* stdio_* funnel equivalents. On Windows the Ami bypass stdio omits rename()   *
* -- it relies on the native C runtime rename() (amitk/libc/stdio.c) -- so the *
* coined stdio_rename() is left undefined and the link fails. Provide it here, *
* mapping straight to the CRT rename(). This is Windows-only; on other hosts   *
* the bypass stdio supplies stdio_rename itself.                               *
*                                                                              *
*******************************************************************************/

#include <stdio.h>

int stdio_rename(const char* oldname, const char* newname)

{

    return rename(oldname, newname);

}
