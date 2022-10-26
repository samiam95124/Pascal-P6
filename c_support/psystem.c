/*******************************************************************************

                           PSYSTEM SUPPORT LIBRARY

Contains the code to support system calls in Pascal-P6. Note that the system
library interface for Pascal-P6 can interface directly to C level calls, so no
wrapper is required.

The format of a Pascal file is:

Addr  Meaning
0:    Logical file number (0-100)
1-n:  Buffer variable.

Valid logical file numbers are 1 to 100. A file number of 0 means that the file
was never opened. All file stores are expected to be initialized to 0, usually
within the .bss, but this could also be from a calloc() call. As per Pascaline
specification, a reset() or rewrite() must be applied to the file to open it,
and if no name was assigned to the file, it is opened as a anonymous temporary
file.

The buffer variable will be the number of bytes of the base type of the file,
which could be any size. In the case of text files, it is one byte, and the
header files are preallocated as such. The buffer variable is used as part of
the "lazy I/O" scheme used to implement Pascal style I/O efficiently.

Note that a lot of this code comes from cmach.c, which has the same basic system
functions.

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

/* assigned logical channels for header files */
#define INPUTFN   1 /* 'input' file no. */
#define OUTPUTFN  2 /* 'output' file no. */
#define PRDFN     3 /* 'prd' file no. */
#define PRRFN     4 /* 'prr' file no. */
#define ERRORFN   5 /* 'error' file no. */
#define LISTFN    6 /* 'list' file no. */
#define COMMANDFN 7 /* 'command' file no. */

typedef long boolean; /* true/false */

static FILE* filtable[MAXFIL+1]; /* general file holders */
static filnam filnamtab[MAXFIL+1]; /* assigned name of files */
static boolean filanamtab[MAXFIL+1]; /* name has been assigned flags */
static filsts filstate[MAXFIL+1]; /* file state holding */
static boolean filbuff[MAXFIL+1]; /* file buffer full status */
static boolean fileoln[MAXFIL+1]; /* last file character read was eoln */
static boolean filbof[MAXFIL+1]; /* beginning of file */

typedef char pasfil; /* Pascal file */

/** ****************************************************************************

Output error

Prints the given error string and halts.

*******************************************************************************/

static error(char* es)

{

    fprintf(stderr, "*** psystem error: %s\n", es);

    exit(1);

}

/** ****************************************************************************

Write string

Accepts a Pascal file pointer, a string with length, and a field. The string is
printed to the field, which is either clipped or extended with blanks to match
the field specification.

*******************************************************************************/

void _psystem_wrs(
    /* Pascal file to write to */ pasfil* f,
    /* String to write */         char    s[],
    /* Length of string */        int     l,
    /* Width of field */          int     w
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (l > w) l = w; /* limit string to field */
    if (fn <= COMMANDFN) switch (fn) {
       case OUTPUTFN: fprintf(stdout, "%*.*s", w, l, s); break;
       case PRRFN: fprintf(filtable[PRRFN], "%*.*s", w, l, s); break;
       case ERRORFN: fprintf(stderr, "%*.*s", w, l, s); break;
       case LISTFN: fprintf(stdout, "%*.*s", w, l, s); break;
       case PRDFN: case INPUTFN:
       case COMMANDFN: errore(WRITEONREADONLYFILE); break;
    } else {
         if (filstate[fn] != fswrite) errore("File mode incorrect");
         fprintf(filtable[fn], "%*.*s", w, l, s);
    }

}

/** ****************************************************************************

Write end of line

Accepts a Pascal file pointer. An end of line sequence is output to the file.

*******************************************************************************/
/**
 * Write end of line
 */
void _psystem_wln(
    /* Pascal file to write to */ pasfil* fb 
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn != OUTPUTFN) {

        fprintf(stderr, "Invalid file\n");
        exit(1);

    }
    fprintf(stdout, "\n");

}