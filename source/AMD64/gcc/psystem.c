/** ****************************************************************************

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

There have been several psystem modules over the years. What makes this one
(and cmach) unique besides being written in C, is that it translates Pascal 
support calls to C ANSI calls.

LICENSING:                                                                 
                                                                           
Copyright (c) 1996, 2018, Scott A. Franco                                  
All rights reserved.                                                       
                                                                           
Redistribution and use in source and binary forms, with or without         
modification, are permitted provided that the following conditions are met:
                                                                           
1. Redistributions of source code must retain the above copyright notice,  
   this list of conditions and the following disclaimer.                   
2. Redistributions in binary form must reproduce the above copyright       
   notice, this list of conditions and the following disclaimer in the     
   documentation and/or other materials provided with the distribution.    
                                                                           
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE  
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE   
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR        
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF       
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS   
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN    
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)    
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.                                                
                                                                           
The views and conclusions contained in the software and documentation are  
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the Pascal-P6 project.           

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>

/* Set default configuration flags. This gives proper behavior even if no
  preprocessor flags are passed in.

  The defaults are:
  WRDSIZ32       - 32 bit compiler.
  ISO7185_PASCAL - uses ISO 7185 standard language only.
*/
#if !defined(WRDSIZ32) && !defined(WRDSIZ64)
#define WRDSIZ32 1
#endif

#ifndef ISO7185
#define ISO7185 FALSE /* iso7185 standard flag */
#endif

#ifndef DORECYCL
#define DORECYCL TRUE /* obey heap space recycle requests */
#endif

/*******************************************************************************

Program object sizes and characteristics, sync with pint. These define
the machine specific characteristics of the target.

The configurations are as follows:

type                  #bits 32  #bits 64
===========================================================
integer               32        64
real                  64        64
char                  8         8
boolean               8         8
set                   256       256
pointers              32        64
marks                 32        64
File logical number   8         8

Both endian types are supported. There is no alignment needed, but you
may wish to use alignment to tune the runtime speed.

The machine characteristics dep}ent on byte accessable machines. This
table is all you should need to adapt to any byte addressable machine.

*/

#ifdef WRDSIZ32
#define INTSIZE             4   /* size of integer */
#define INTAL               4   /* alignment of integer */
#define INTDIG              11  /* number of decimal digits in integer */
#define INTHEX              8   /* number of hex digits of integer */
#define REALSIZE            8   /* size of real */
#define REALAL              4   /* alignment of real */
#define CHARSIZE            1   /* size of char */
#define CHARAL              1   /* alignment of char */
#define CHARMAX             1
#define BOOLSIZE            1   /* size of boolean */
#define BOOLAL              1   /* alignment of boolean */
#define PTRSIZE             4   /* size of pointer */
#define ADRSIZE             4   /* size of address */
#define ADRAL               4   /* alignment of address */
#define SETSIZE            32   /* size of set */
#define SETAL               1   /* alignment of set */
#define FILESIZE            1   /* required runtime space for file (lfn) */
#define FILEIDSIZE          1   /* size of the lfn only */
#define EXCEPTSIZE          1   /* size of exception variable */
#define EXCEPTAL            1
#define STACKAL             4   /* alignment of stack */
#define STACKELSIZE         4   /* stack element size */
#define MAXSIZE            32   /* this is the largest type that can be on the
                                   stack */
/* Heap alignment should be either the natural word alignment of the
  machine, or the largest object needing alignment that will be allocated.
  It can also be used to enforce minimum block allocation policy. */
#define HEAPAL              4   /* alignment for each heap arena */
#define GBSAL               4   /* globals area alignment */
#define SETHIGH           255   /* Sets are 256 values */
#define SETLOW              0
#define ORDMAXCHAR        255   /* Characters are 8 bit ISO/IEC 8859-1 */
#define ORDMINCHAR          0
#define MAXRESULT    REALSIZE   /* maximum size of function result */
#define MARKSIZE           24   /* maxresult+6*ptrsize */
#define UJPLEN              5   /* length of ujp instruction (used for case
                                   jumps) */

/* Value of nil is 1 because this allows checks for pointers that were
  initialized, which would be zero (since we clear all space to zero).
  In the new unified code/data space scheme, 0 and 1 are always invalid
  addresses, since the startup code is at least that long. */
#define NILVAL              1  /* value of 'nil' */

/* Mark element offsets

  Mark format is:

  -8:  Function return value, 64 bits, enables a full real result.
  -12: Static link.
  -16: Dynamic link.
  -20: Saved EP from previous frame.
  -24: Stack bottom after locals allocate. Used for interprocdural gotos.
  -28: EP from current frame. Used for interprocedural gotos.
  -32: Return address

*/
#define MARKSL              -4  /* static link */
#define MARKDL              -8  /* dynamic link */
#define MARKEP              -12 /* (old) maximum frame size */
#define MARKSB              -16 /* stack bottom */
#define MARKET              -20 /* current ep */
#define MARKRA              -24 /* return address */
#endif

#ifdef WRDSIZ64
#define INTSIZE             8  /* size of integer */
#define INTAL               4  /* alignment of integer */
#define INTDIG              20 /* number of decimal digits in integer */
#define INTHEX              16 /* number of hex digits of integer */
#define REALSIZE            8  /* size of real */
#define REALAL              4  /* alignment of real */
#define CHARSIZE            1  /* size of char */
#define CHARAL              1  /* alignment of char */
#define CHARMAX             1
#define BOOLSIZE            1  /* size of boolean */
#define BOOLAL              1  /* alignment of boolean */
#define PTRSIZE             8  /* size of pointer */
#define ADRSIZE             8  /* size of address */
#define ADRAL               4  /* alignment of address */
#define SETSIZE            32  /* size of set */
#define SETAL               1  /* alignment of set */
#define FILESIZE            1  /* required runtime space for file (lfn) */
#define FILEIDSIZE          1  /* size of the lfn only */
#define EXCEPTSIZE          1  /* size of exception variable */
#define EXCEPTAL            1
#define STACKAL             8  /* alignment of stack */
#define STACKELSIZE         8  /* stack element size */
#define MAXSIZE            32  /* this is the largest type that can be on the
                                  stack */
/* Heap alignment should be either the natural word alignment of the
  machine, or the largest object needing alignment that will be allocated.
  It can also be used to enforce minimum block allocation policy. */
#define HEAPAL              4  /* alignment for each heap arena */
#define GBSAL               4  /* globals area alignment */
#define SETHIGH           255  /* Sets are 256 values */
#define SETLOW              0
#define ORDMAXCHAR        255  /* Characters are 8 bit ISO/IEC 8859-1 */
#define ORDMINCHAR          0
#define MAXRESULT    REALSIZE  /* maximum size of function result */
#define MARKSIZE           48  /* maxresult+6*ptrsize */
#define UJPLEN              9  /* length of ujp instruction (used for case
                                  jumps) */

/* Value of nil is 1 because this allows checks for pointers that were
  initialized, which would be zero (since we clear all space to zero).
  In the new unified code/data space scheme, 0 and 1 are always invalid
  addresses, since the startup code is at least that long. */
#define NILVAL              1  /* value of 'nil' */

/* Mark element offsets

  Mark format is:

  -8: Static link.
  -16: Dynamic link.
  -24: Saved EP from previous frame.
  -32: Stack bottom after locals allocate. Used for interprocdural gotos.
  -40: EP from current frame. Used for interprocedural gotos.
  -48: Return address

*/
#define MARKSL              -8  /* static link */
#define MARKDL              -16 /* dynamic link */
#define MARKEP              -24 /* (old) maximum frame size */
#define MARKSB              -32 /* stack bottom */
#define MARKET              -40 /* current ep */
#define MARKRA              -48 /* return address */
#endif

/* ******************* end of pcom and pint common parameters *********** */

/* internal constants */

#define TRUE  1               /* value of true */
#define FALSE 0               /* value of false */

/* assigned logical channels for header files */
#define INPUTFN   1 /* 'input' file no. */
#define OUTPUTFN  2 /* 'output' file no. */
#define ERRORFN   5 /* 'error' file no. */
#define LISTFN    6 /* 'list' file no. */
#define COMMANDFN 7 /* 'command' file no. */

#define MAXFIL       100  /* maximum number of general (temp) files */
#define FILLEN       2000 /* maximum length of filenames */
#define REALEF       9    /* real extra field in floating format -1.0e+000 */
#define ERRLEN       250  /* maximum length of error messages */
#define MAXCMD       250  /* size of command line buffer */
#define MAXDBF       30   /* size of numeric conversion buffer */

/* locations of standard exceptions */
#define EXCEPTIONBASE                       14
#define VALUEOUTOFRANGE                     14
#define ARRAYLENGTHMATCH                    15
#define CASEVALUENOTFOUND                   16
#define ZERODIVIDE                          17
#define INVALIDOPERAND                      18
#define NILPOINTERDEREFERENCE               19
#define REALOVERFLOW                        20
#define REALUNDERFLOW                       21
#define REALPROCESSINGFAULT                 22
#define TAGVALUENOTACTIVE                   23
#define TOOMANYFILES                        24
#define FILEISOPEN                          25
#define FILEALREADYNAMED                    26
#define FILENOTOPEN                         27
#define FILEMODEINCORRECT                   28
#define INVALIDFIELDSPECIFICATION           29
#define INVALIDREALNUMBER                   30
#define INVALIDFRACTIONSPECIFICATION        31
#define INVALIDINTEGERFORMAT                32
#define INTEGERVALUEOVERFLOW                33
#define INVALIDREALFORMAT                   34
#define ENDOFFILE                           35
#define INVALIDFILEPOSITION                 36
#define FILENAMETOOLONG                     37
#define FILEOPENFAIL                        38
#define FILESIZEFAIL                        39
#define FILECLOSEFAIL                       40
#define FILEREADFAIL                        41
#define FILEWRITEFAIL                       42
#define FILEPOSITIONFAIL                    43
#define FILEDELETEFAIL                      44
#define FILENAMECHANGEFAIL                  45
#define SPACEALLOCATEFAIL                   46
#define SPACERELEASEFAIL                    47
#define SPACEALLOCATENEGATIVE               48
#define CANNOTPERFORMSPECIAL                49
#define COMMANDLINETOOLONG                  50
#define READPASTEOF                         51
#define FILETRANSFERLENGTHZERO              52
#define FILESIZETOOLARGE                    53
#define FILENAMEEMPTY                       54
#define CANNOTOPENSTANDARD                  55
#define TOOMANYTEMPORARYFILES               56
#define INPUTBUFFEROVERFLOW                 57
#define TOOMANYTHREADS                      58
#define CANNOTSTARTTHREAD                   59
#define INVALIDTHREADHANDLE                 60
#define CANNOTSTOPTHREAD                    61
#define TOOMANYINTERTASKLOCKS               62
#define INVALIDLOCKHANDLE                   63
#define LOCKSEQUENCEFAIL                    64
#define TOOMANYSIGNALS                      65
#define CANNOTCREATESIGNAL                  66
#define INVALIDSIGNALHANDLE                 67
#define CANNOTDELETESIGNAL                  68
#define CANNOTSENDSIGNAL                    69
#define WAITFORSIGNALFAIL                   70
#define FIELDNOTBLANK                       71
#define READONWRITEONLYFILE                 72
#define WRITEONREADONLYFILE                 73
#define FILEBUFFERVARIABLEUNDEFINED         74
#define NONDECIMALRADIXOFNEGATIVE           75
#define INVALIDARGUMENTTOLN                 76
#define INVALIDARGUMENTTOSQRT               77
#define CANNOTRESETORREWRITESTANDARDFILE    78
#define CANNOTRESETWRITEONLYFILE            79
#define CANNOTREWRITEREADONLYFILE           80
#define SETELEMENTOUTOFRANGE                81
#define REALARGUMENTTOOLARGE                82
#define BOOLEANOPERATOROFNEGATIVE           83
#define INVALIDDIVISORTOMOD                 84
#define PACKELEMENTSOUTOFBOUNDS             85
#define UNPACKELEMENTSOUTOFBOUNDS           86
#define CANNOTRESETCLOSEDTEMPFILE           87
#define EXCEPTIONTOP                        87

/* Exceptions that can't be caught.
  Note that these don't have associated exception variables. */

#define UNDEFINEDLOCATIONACCESS             88
#define FUNCTIONNOTIMPLEMENTED              89
#define INVALIDINISO7185MODE                90
#define HEAPFORMATINVALID                   91
#define DISPOSEOFUNINITALIZEDPOINTER        92
#define DISPOSEOFNILPOINTER                 93
#define BADPOINTERVALUE                     94
#define BLOCKALREADYFREED                   95
#define INVALIDSTANDARDPROCEDUREORFUNCTION  96
#define INVALIDINSTRUCTION                  97
#define NEWDISPOSETAGSMISMATCH              98
#define PCOUTOFRANGE                        99
#define STOREOVERFLOW                       100
#define STACKBALANCE                        101
#define SETINCLUSION                        102
#define UNINITIALIZEDPOINTER                103
#define DEREFERENCEOFNILPOINTER             104
#define POINTERUSEDAFTERDISPOSE             105
#define VARIANTNOTACTIVE                    106
#define INVALIDCASE                         107
#define SYSTEMERROR                         108
#define CHANGETOALLOCATEDTAGFIELD           109
#define UNHANDLEDEXCEPTION                  110
#define PROGRAMCODEASSERTION                111
#define VARLISTEMPTY                        112
#define CHANGETOVARREFERENCEDVARIANT        113
#define DISPOSEOFVARREFERENCEDBLOCK         114
#define VARREFERENCEDFILEBUFFERMODIFIED     115
#define CONTAINERMISMATCH                   116
#define INVALIDCONTAINERLEVEL               117

typedef unsigned char byte;    /* 8-bit byte */
typedef long boolean; /* true/false */
typedef char pasfil;  /* Pascal file */
typedef long filnum;  /* logical file number */
typedef char filnam[FILLEN]; /* filename strings */
typedef enum {
  fsclosed,
  fsread,
  fswrite
} filsts;                      /* file states */
typedef long cmdinx;           /* index for command line buffer */
typedef long cmdnum;           /* length of command line buffer */
typedef char cmdbuf[MAXCMD];   /* buffer for command line */
typedef char errmsg[ERRLEN];   /* error string */
typedef byte settype[SETSIZE]; /* standard set */
typedef long address;          /* address */

/* VAR reference block */
typedef struct _varblk *varptr;
typedef struct _varblk {
    varptr next;  /* next entry */
    unsigned char* s; /* start and end address of block */
    unsigned char* e; 
} varblk;

static FILE* filtable[MAXFIL+1]; /* general file holders */
static filnam filnamtab[MAXFIL+1]; /* assigned name of files */
static boolean filanamtab[MAXFIL+1]; /* name has been assigned flags */
static filsts filstate[MAXFIL+1]; /* file state holding */
static boolean filbuff[MAXFIL+1]; /* file buffer full status */
static boolean fileoln[MAXFIL+1]; /* last file character read was eoln */
static boolean filbof[MAXFIL+1]; /* beginning of file */
static varptr varlst; /* active var block pushdown stack */
static varptr varfre; /* free var block entries */
static long srclin;  /* current source line executing */

static cmdbuf  cmdlin;  /* command line */
static cmdnum  cmdlen;  /* length of command line */
static cmdinx  cmdpos;  /* current position in command line */

/** ****************************************************************************

Output error

Prints the given error string and halts.

*******************************************************************************/

static void error(char* es)

{

    fprintf(stderr, "*** psystem error: %s\n", es);

    exit(1);

}

/* handle exception vector */
void psystem_errorv(address ea)
{ printf("\n*** Runtime error: ");
  if (srclin > 0) printf(" [%ld]: ", srclin);
  switch (ea) {

    /* Exceptions that can be intercepted */
    case VALUEOUTOFRANGE:                    printf("Value out of range\n"); break;
    case ARRAYLENGTHMATCH:                   printf("Array length match\n"); break;
    case CASEVALUENOTFOUND:                  printf("Case value not found\n"); break;
    case ZERODIVIDE:                         printf("Zero divide\n"); break;
    case INVALIDOPERAND:                     printf("Invalid operand\n"); break;
    case NILPOINTERDEREFERENCE:              printf("Nil pointer dereference\n"); break;
    case REALOVERFLOW:                       printf("Real overflow\n"); break;
    case REALUNDERFLOW:                      printf("Real underflow\n"); break;
    case REALPROCESSINGFAULT:                printf("Real processing fault\n"); break;
    case TAGVALUENOTACTIVE:                  printf("Tag value not active\n"); break;
    case TOOMANYFILES:                       printf("Too many files\n"); break;
    case FILEISOPEN:                         printf("File is open\n"); break;
    case FILEALREADYNAMED:                   printf("File already named\n"); break;
    case FILENOTOPEN:                        printf("File not open\n"); break;
    case FILEMODEINCORRECT:                  printf("File mode incorrect\n"); break;
    case INVALIDFIELDSPECIFICATION:          printf("Invalid field specification\n"); break;
    case INVALIDREALNUMBER:                  printf("Invalid real number\n"); break;
    case INVALIDFRACTIONSPECIFICATION:       printf("Invalid fraction specification\n"); break;
    case INVALIDINTEGERFORMAT:               printf("Invalid integer format\n"); break;
    case INTEGERVALUEOVERFLOW:               printf("Integer value overflow\n"); break;
    case INVALIDREALFORMAT:                  printf("Invalid real format\n"); break;
    case ENDOFFILE:                          printf("End of file\n"); break;
    case INVALIDFILEPOSITION:                printf("Invalid file position\n"); break;
    case FILENAMETOOLONG:                    printf("Filename too long\n"); break;
    case FILEOPENFAIL:                       printf("File open fail\n"); break;
    case FILESIZEFAIL:                       printf("File size fail\n"); break;
    case FILECLOSEFAIL:                      printf("File close fail\n"); break;
    case FILEREADFAIL:                       printf("File read fail\n"); break;
    case FILEWRITEFAIL:                      printf("File write fail\n"); break;
    case FILEPOSITIONFAIL:                   printf("File position fail\n"); break;
    case FILEDELETEFAIL:                     printf("File delete fail\n"); break;
    case FILENAMECHANGEFAIL:                 printf("File name change fail\n"); break;
    case SPACEALLOCATEFAIL:                  printf("Space allocate fail\n"); break;
    case SPACERELEASEFAIL:                   printf("Space release fail\n"); break;
    case SPACEALLOCATENEGATIVE:              printf("Space allocate negative\n"); break;
    case CANNOTPERFORMSPECIAL:               printf("Cannot perform special\n"); break;
    case COMMANDLINETOOLONG:                 printf("Command line too long\n"); break;
    case READPASTEOF:                        printf("Read past eof\n"); break;
    case FILETRANSFERLENGTHZERO:             printf("File transfer length zero\n"); break;
    case FILESIZETOOLARGE:                   printf("File size too large\n"); break;
    case FILENAMEEMPTY:                      printf("Filename empty\n"); break;
    case CANNOTOPENSTANDARD:                 printf("Cannot open standard\n"); break;
    case TOOMANYTEMPORARYFILES:              printf("Too many temporary files\n"); break;
    case INPUTBUFFEROVERFLOW:                printf("Input buffer overflow\n"); break;
    case TOOMANYTHREADS:                     printf("Too many threads\n"); break;
    case CANNOTSTARTTHREAD:                  printf("Cannot start thread\n"); break;
    case INVALIDTHREADHANDLE:                printf("Invalid thread handle\n"); break;
    case CANNOTSTOPTHREAD:                   printf("Cannot stop thread\n"); break;
    case TOOMANYINTERTASKLOCKS:              printf("Too many inter task locks\n"); break;
    case INVALIDLOCKHANDLE:                  printf("Invalid lock handle\n"); break;
    case LOCKSEQUENCEFAIL:                   printf("Lock sequence fail\n"); break;
    case TOOMANYSIGNALS:                     printf("Too many signals\n"); break;
    case CANNOTCREATESIGNAL:                 printf("Cannot create signal\n"); break;
    case INVALIDSIGNALHANDLE:                printf("Invalid signal handle\n"); break;
    case CANNOTDELETESIGNAL:                 printf("Cannot delete signal\n"); break;
    case CANNOTSENDSIGNAL:                   printf("Cannot send signal\n"); break;
    case WAITFORSIGNALFAIL:                  printf("Wait for signal fail\n"); break;
    case FIELDNOTBLANK:                      printf("Field not blank\n"); break;
    case READONWRITEONLYFILE:                printf("Read on write only file\n"); break;
    case WRITEONREADONLYFILE:                printf("Write on read only file\n"); break;
    case FILEBUFFERVARIABLEUNDEFINED:        printf("File buffer variable undefined\n"); break;
    case NONDECIMALRADIXOFNEGATIVE:          printf("Nondecimal radix of negative\n"); break;
    case INVALIDARGUMENTTOLN:                printf("Invalid argument to ln\n"); break;
    case INVALIDARGUMENTTOSQRT:              printf("Invalid argument to sqrt\n"); break;
    case CANNOTRESETORREWRITESTANDARDFILE:   printf("Cannot reset or rewrite standard file\n"); break;
    case CANNOTRESETWRITEONLYFILE:           printf("Cannot reset write only file\n"); break;
    case CANNOTREWRITEREADONLYFILE :         printf("Cannot rewrite read only file\n"); break;
    case SETELEMENTOUTOFRANGE:               printf("Set element out of range\n"); break;
    case REALARGUMENTTOOLARGE:               printf("Real argument too large\n"); break;
    case BOOLEANOPERATOROFNEGATIVE:          printf("Boolean operator of negative\n"); break;
    case INVALIDDIVISORTOMOD:                printf("Invalid divisor to mod\n"); break;
    case PACKELEMENTSOUTOFBOUNDS:            printf("Pack elements out of bounds\n"); break;
    case UNPACKELEMENTSOUTOFBOUNDS:          printf("Unpack elements out of bounds\n"); break;
    case CANNOTRESETCLOSEDTEMPFILE:          printf("Cannot reset closed temp file\n"); break;

    /* Exceptions that can't be intercepted */
    case UNDEFINEDLOCATIONACCESS:            printf("Undefined location access\n"); break;
    case FUNCTIONNOTIMPLEMENTED:             printf("Function not implemented\n"); break;
    case INVALIDINISO7185MODE:               printf("Invalid in ISO 7185 mode\n"); break;
    case HEAPFORMATINVALID:                  printf("Heap format invalid\n"); break;
    case DISPOSEOFUNINITALIZEDPOINTER:       printf("Dispose of uninitalized pointer\n"); break;
    case DISPOSEOFNILPOINTER:                printf("Dispose of nil pointer\n"); break;
    case BADPOINTERVALUE:                    printf("Bad pointer value\n"); break;
    case BLOCKALREADYFREED:                  printf("Block already freed\n"); break;
    case INVALIDSTANDARDPROCEDUREORFUNCTION: printf("Invalid standard procedure or function\n"); break;
    case INVALIDINSTRUCTION:                 printf("Invalid instruction\n"); break;
    case NEWDISPOSETAGSMISMATCH:             printf("New dispose tags mismatch\n"); break;
    case PCOUTOFRANGE:                       printf("Pc out of range\n"); break;
    case STOREOVERFLOW:                      printf("Store overflow\n"); break;
    case STACKBALANCE:                       printf("Stack balance\n"); break;
    case SETINCLUSION:                       printf("Set inclusion\n"); break;
    case UNINITIALIZEDPOINTER:               printf("Uninitialized pointer\n"); break;
    case DEREFERENCEOFNILPOINTER:            printf("Dereference of nil pointer\n"); break;
    case POINTERUSEDAFTERDISPOSE:            printf("Pointer used after dispose\n"); break;
    case VARIANTNOTACTIVE:                   printf("Variant not active\n"); break;
    case INVALIDCASE:                        printf("Invalid case\n"); break;
    case SYSTEMERROR:                        printf("System error\n"); break;
    case CHANGETOALLOCATEDTAGFIELD:          printf("Change to allocated tag field\n"); break;
    case UNHANDLEDEXCEPTION:                 printf("Unhandled exception\n"); break;
    case PROGRAMCODEASSERTION:               printf("Program code assertion\n"); break;
    case VARLISTEMPTY:                       printf("VAR block list empty\n"); break;
    case CHANGETOVARREFERENCEDVARIANT:       printf("Change to VAR referenced variant\n"); break;
    case DISPOSEOFVARREFERENCEDBLOCK:        printf("Dispose of VAR referenced block\n"); break;
    case VARREFERENCEDFILEBUFFERMODIFIED:    printf("VAR referenced file buffer modified\n"); break;
    case CONTAINERMISMATCH:                  printf("Container length(s) do not match\n"); break;
    case INVALIDCONTAINERLEVEL:              printf("InvalidContainerLevel\n"); break;
  }
  exit(1);
}

void psystem_errore(long ei)
{

    psystem_errorv(ei);

}

/** ****************************************************************************

Get command line

Gets the command line into the given command buffer. Unix does not technically
have a command buffer, but rather passes it as a series of command and argument
strings. We reconstruct the command line by concatenating the argument strings
together separated by spaces.

*******************************************************************************/

static void getcommandline(long argc, char* argv[], cmdbuf cb, cmdnum* l)
{

    cmdinx i;
    long x;
    char *p;

    for (i = 0; i < MAXCMD; i++) cb[i] = ' '; i = 0;
    while (argc) { /* walk input parameters and concatenate */

        p = *argv;
        while (*p) {

            if (i >= MAXCMD) {

                fprintf(stderr,
                        "*** Too many/too long command line parameters\n");
                exit(1);

            }
            cb[i++] = *p++;

        }
        argc--; argv++; /* advance */
        if (argc) { /* still more */

            if (i >= MAXCMD) {

                fprintf(stderr,
                        "*** Too many/too long command line parameters\n");
                exit(1);

            }
            cb[i++] = ' ';

        }

    }

}

/* "fileofy" routines for command line processing.

  These routines implement the command header file by reading from a
  buffer where the command line is stored. The assumption here is that
  there is a fairly simple call to retrieve the command line.

  If it is wanted, the command file primitives can be used to implement
  another type of interface, say, reading from an actual file.

  The command buffer is designed to never be completely full.
  The last two locations indicate:

  maxcmd: end of file
  maxcmd-1: end of line

  These locations are always left as space, thus eoln returns space as
  the standard specifies.
*/

static char bufcommand(void)
{ return (cmdlin[cmdpos]); }

static void getcommand(void)
{ if (cmdpos <= cmdlen) cmdpos = cmdpos+1; }

static boolean eofcommand(void)
{ return (cmdpos > cmdlen+1); }

static boolean eolncommand(void)
{ return (cmdpos >= cmdlen+1); }

static void readlncommand(void)
{ cmdpos = MAXCMD; }

/* The external assigns read a filename off the command line. The original name
  of the header file is also passed in, and can be used to process. However,
  this implementation ignores them and just reads the names in order off the
  command line (as provided for in Annex C of the Pascaline standard).

  The processing of command line filenames does not exclude the use of the
  command file. The command file simply starts reading after all filename
  parameters have been removed.

*/

void assignexternal(filnum fn, char hfn[])
{
    long i;

    for (i = 0; i < FILLEN; i++) { filnamtab[fn][i] = ' '; }
    /* skip leading spaces */
    while (!eolncommand() && !eofcommand() && bufcommand() == ' ') getcommand();
    i = 0;
    while (!eolncommand() && !eofcommand() &&
           (isalnum(bufcommand()) || bufcommand() == '_')) {
        if (i >= FILLEN) error("File name too long");
        filnamtab[fn][i] = bufcommand();
        getcommand();
        i = i+1;
    }
    if (i >= FILLEN) error("File name too long");
    filnamtab[fn][i] = 0; /* terminate */
}

/* set operations */

void sset(settype s, long b)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s[i] = 0;
    s[b/8] |= 1<<b%8;
}

void rset(settype s, long b1, long b2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s[i] = 0;
    if (b1 > b2) { i = b1; b1 = b2; b2 = i; }
    for (i = b1; i <= b2; i++) s[i/8] |= 1<<i%8;
}

void suni(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s1[i] = s1[i] | s2[i];
}

void sint(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s1[i] = s1[i] & s2[i];
}

void sdif(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) s1[i] = s1[i] & ~s2[i];
}

boolean sisin(long i, settype s)
{
    return (!!(s[i/8] & 1<<i%8));
}

boolean sequ(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++) if (s1[i] != s2[i]) return (FALSE);
    return (TRUE);
}

boolean sinc(settype s1, settype s2)
{
    long i;

    for (i = 0; i < SETSIZE; i++)
        if ((s1[i] & s2[i]) != s2[i]) return (FALSE);
    return (TRUE);
}

/** ****************************************************************************

Copy addr/length string to C zero terminated string

Copies from the source string to the destination string for length characters,
then places a zero termination at the end. This is like strncpy(), but always
terminates the destination.

*******************************************************************************/

void strcpyl(
    /* destination string */ char* ds,
    /* source string */      char* ss,
    /* source string length */ long sl
)

{

    while (sl) {

        *ds++ = *ss++;
        sl--;
    }
    *ds = 0;

}

/** ****************************************************************************

Enter variable reference block

Register a block of memory as having an outstanding variable reference.

*******************************************************************************/

void varenter(unsigned char* s, unsigned char* e)

{

    varptr vp;

    if (varfre) { vp = varfre; varfre = vp->next; }
    else vp = (varptr) malloc(sizeof(varblk));
    vp->s = s; vp->e = e; vp->next = varlst; varlst = vp;

}

/** ****************************************************************************

Remove variable reference block

Removes the last registered variable reference block.

*******************************************************************************/

void varexit(void)

{

    varptr vp;

    if (!varlst) error("Var list empty");
    vp = varlst; varlst = vp->next; vp->next = varfre; varfre = vp;

}

/** ****************************************************************************

Check variable reference overlap

Checks if the provided block overlaps any outstanding variable reference block.

*******************************************************************************/

boolean varlap(unsigned char* s, unsigned char* e)

{

    varptr vp;
    long f;

    vp = varlst; f = FALSE;
    while (vp && !f) {

        f = (vp->e >= s && vp->s <= e);
        vp = vp->next;

    }

    return (f);

}

/** ****************************************************************************

Validate Pascal file

Expects a Pascal file address. The file is checked for zero, and if so allocates
a file from the file table. 

*******************************************************************************/

void valfil(
    /** Pascal file to validate */ pasfil* f
)

{

    long i,ff;

    if (*f == 0) { /* no file */

        i = COMMANDFN+1; /* start search after the header files */
        ff = 0;
        while (i <= MAXFIL) {

            if (filstate[i] == fsclosed) { ff = i; i = MAXFIL; }
            i = i+1;

        }
        if (ff == 0) error("Too many files");
        *f = ff;

    }

}

/** ****************************************************************************

Validate file for writing

Expects a Pascal file address. Validates the file is in write mode.

*******************************************************************************/

void valfilwm(
    /** Pascal file to validate */ pasfil* f
)

{

    valfil(f); /* validate file address */
    if (filstate[*f] != fswrite) error("File mode incorrect");

}

/** ****************************************************************************

Validate file for reading

Expects a Pascal file address. Validates the file is in read mode.

*******************************************************************************/

void valfilrm(
    /** Pascal file to validate */ pasfil* f
)

{

    valfil(f); /* validate file address */
    if (filstate[*f] != fsread) error("File mode incorrect");

}

/** ****************************************************************************

Find if file is at EOF

Expects a C file pointer, and returns true if the file is at EOF.

Note: turns out this could be implemented by feof().

*******************************************************************************/

boolean eoffile(
    /** File to check */ FILE* fp
)

{ 

    long c; 

    c = fgetc(fp); 
    if (c != EOF) ungetc(c, fp); 

    return (c == EOF); 

}

/** ****************************************************************************

Find if file is at EOLN

Expects a C file pointer, and returns true if the file is at EOLN.

*******************************************************************************/

boolean eolnfile(
    /** File to check */ FILE* fp
)

{ 

    long c; 
    
    c = fgetc(fp); 
    if (c != EOF) ungetc(c, fp); 

    return (c == '\n'); 

}

/** ****************************************************************************

Check next character

Returns the next character without moving the file forward. If the next 
character is EOLN or EOF, returns space.

*******************************************************************************/

char chkfile(
    /** File to get character from */ FILE* fp
)

{

    long c;

    c = fgetc(fp); 
    if (c != EOF) ungetc(c, fp);

    return ((c=='\n'||c==EOF)?' ':c);

}


/** ****************************************************************************

Find file length

*******************************************************************************/

long lengthfile(
    /** File to get length */ FILE* fp
)

{

    long s, p;

    s = ftell(fp); fseek(fp, 0, SEEK_END);
    p = ftell(fp); fseek(fp, s, SEEK_SET);

    return (p);

}

/** ****************************************************************************

Check next character in file buffer variable

Returns the next character from Pascal file without moving the file forward. 
If the next character is EOLN or EOF, returns space. Performs the correct action
according to file type. Errors on read to incorrect file type.

*******************************************************************************/

char buffn(
    /** Logical file number */ filnum fn
)

{

    long c;

    if (fn <= COMMANDFN) switch(fn) {

        case INPUTFN:   c = chkfile(stdin); break;
        case OUTPUTFN: case ERRORFN:
        case LISTFN:    error("Read on write only file"); break;
        case COMMANDFN: c = bufcommand(); break;

    } else {

        if (filstate[fn] != fsread) error("File mode incorrect");
        c = chkfile(filtable[fn]);

    }

    return (c);

}

/** ****************************************************************************

Get next character with EOF/EOLN checking

Gets the next character from the given file and checks for EOF and EOLN. 
Discards the character (see get() semantics). If EOF is true for the file after
the character is discarded, and EOLN has not already been flagged for the file,
then EOLN is set. If EOLN is detected, the eoln status is set. Also resets the
beginning of file status if not EOF.

The net result is that if EOF is encountered without an EOLN first, EOLN status
is returned, followed by EOF status.

*******************************************************************************/

void getfneoln(
    /** C file to read */      FILE* fp,
    /** logical file number */ filnum fn
)

{

    long c;

    c = fgetc(fp);
    if (c == EOF && !fileoln[fn]) fileoln[fn] = TRUE;
    else fileoln[fn] = c == '\n';
    if (c != EOF) filbof[fn] = FALSE;

}

/** ****************************************************************************

Get next character text file

Advances the character position for a text file.

*******************************************************************************/

void getfn(filnum fn)
{

    if (fn <= COMMANDFN) switch (fn) {

        case INPUTFN:   getfneoln(stdin, INPUTFN); break;
        case OUTPUTFN: case ERRORFN:
        case LISTFN:    error("Read on write only file"); break;
        case COMMANDFN: getcommand(); break;

    } else {

        if (filstate[fn] != fsread) error("File mode incorrect");
        getfneoln(filtable[fn], fn);

    }

}

/** ****************************************************************************

Check file is at EOF.

Returns true if file is at EOF. Handles EOLN insertion.

*******************************************************************************/

boolean chkeoffn(FILE* fp, filnum fn)

{

    if (fn == INPUTFN) {

        if ((eoffile(fp) && fileoln[fn]) || filbof[fn]) return (TRUE);
        else return (FALSE);

    } else {

        if (filstate[fn] == fswrite)
            return ftell(filtable[fn]) >= lengthfile(filtable[fn]);
        else if (filstate[fn] == fsread) {

            if ((eoffile(filtable[fn]) && fileoln[fn]) || filbof[fn])
                return (TRUE);
            else return (FALSE);

        } else error("File not open");

    }

}

/** ****************************************************************************

Check file is at EOF

Returns true if file is at EOF.

*******************************************************************************/

boolean eoffn(filnum fn)

{

    boolean eof;

    if (fn <= COMMANDFN) switch (fn) {

        case INPUTFN:   eof = chkeoffn(stdin, INPUTFN); break;
        case OUTPUTFN:  eof = TRUE; break;
        case ERRORFN:   eof = TRUE; break;
        case LISTFN:    eof = TRUE; break;
        case COMMANDFN: eof = eofcommand(); break;

    } else eof = chkeoffn(filtable[fn], fn);

    return (eof);

}

/** ****************************************************************************

Check file is at EOLN

Checks if the file given is at EOLN.

*******************************************************************************/

boolean chkeolnfn(FILE* fp, filnum fn)

{

    if ((eoffile(fp) && !fileoln[fn]) && !filbof[fn]) return (TRUE);
    else return (eolnfile(fp));

}

/** ****************************************************************************

Check logical file by number is at EOLN

Checks if the logical file number is at EOLN.

*******************************************************************************/

boolean eolnfn(filnum fn)
{
    boolean eoln;

    if (fn <= COMMANDFN) switch (fn) {

        case INPUTFN:   eoln = chkeolnfn(stdin, INPUTFN); break;
        case ERRORFN: case OUTPUTFN:
        case LISTFN:    error("File mode incorrect"); break;
        case COMMANDFN: eoln = eolncommand(); break;

    } else {

        if (filstate[fn] == fsclosed) error("File not open");
        eoln = chkeolnfn(filtable[fn], fn);

    }

    return (eoln);
}

/** ****************************************************************************

Read to line end

Reads and discards characters until EOLN is reached.

*******************************************************************************/

void readline(filnum fn)

{
    while (!eolnfn(fn)) {

        if (eoffn(fn)) error("End of file");
        getfn(fn);

    }
    if (eolnfn(fn)) getfn(fn);

}

/** ****************************************************************************

Check next character with field

Returns the next character in the given file. Accepts a field width. Returns
space if past the field width.

*******************************************************************************/

char chkbuf(filnum fn, long w)

{ 

    if (w > 0) return buffn(fn); 
    else return(' '); 

}

/** ****************************************************************************

Check EOF file

Accepts a file number and a field width. Returns true if either the at the EOF
or end of the field.

*******************************************************************************/

boolean chkend(filnum fn, long w)

{ 
 
    return (w = 0 || eoffn(fn)); 

}

/** ****************************************************************************

Get next character file with field

Gets the next character in the file if the field is not at end. Treats EOLN as
an EOF.

*******************************************************************************/

void getbuf(filnum fn, long* w)

{

    if (*w > 0) {

        if (eoffn(fn)) error("End of file");
        getfn(fn); *w = *w-1;

    }

}

/** ****************************************************************************

Read integer fielded

Reads an integer from the given file with field. An integer is read from the
input file, but limited by the field width if provided. If the fielded flag is
true, will also verify that the rest of the field after the number is parsed
is blank.

*******************************************************************************/

void readi(filnum fn, long *i, long* w, boolean fld)
{

    long s;
    long d;

    s = +1; /* set sign */
    /* skip leading spaces */
    while (chkbuf(fn, *w) == ' ' && !chkend(fn, *w)) getbuf(fn, w);
    if (!(chkbuf(fn, *w) == '+' || chkbuf(fn, *w) == '-' ||
        isdigit(chkbuf(fn, *w)))) error("Invalid integer format");
    if (chkbuf(fn, *w) == '+') getbuf(fn, w);
    else if (chkbuf(fn, *w) == '-') { getbuf(fn, w); s = -1; }
    if (!(isdigit(chkbuf(fn, *w))))
        error("Invalid integer format");
    *i = 0; /* clear initial value */
    while (isdigit(chkbuf(fn, *w))) { /* parse digit */

        d = chkbuf(fn, *w)-'0';
        if (*i > INT_MAX/10 || *i == INT_MAX/10 && d > INT_MAX%10)
            error("Integer value overflow");
        *i = *i*10+d; /* add in new digit */
        getbuf(fn, w);

    }
    *i = *i*s; /* place sign */
    /* if fielded, validate the rest of the field is blank */
    if (fld) while (!chkend(fn, *w)) {

        if (chkbuf(fn, *w) != ' ') error("Field not blank");
        getbuf(fn, w);

    }

}

/** ****************************************************************************

Find power of ten

Finds the given power of 10.

*******************************************************************************/

double pwrten(long e)

{

    double t; /* accumulator */
    double p; /* current power */

    p = 1.0e+1; /* set 1st power */
    t = 1.0; /* initalize result */
    do {

        if (e&1) t = t*p; /* if bit set, add this power */
        e = e / 2; /* index next bit */
        p = p*p; /* find next power */

    } while (e != 0);

    return (t);

}

/** ****************************************************************************

Read real fielded

Reads a real from the given file with field. An real is read from the
input file, but limited by the field width if provided. If the fielded flag is
true, will also verify that the rest of the field after the number is parsed
is blank.

*******************************************************************************/

void readr(filnum fn, double* r, long w, boolean fld)

{

    long i; /* integer holding */
    long e; /* exponent */
    long d; /* digit */
    boolean s; /* sign */

    e = 0; /* clear exponent */
    s = FALSE; /* set sign */
    *r = 0.0; /* clear result */
    /* skip leading spaces */
    while (chkbuf(fn, w) == ' ' && !chkend(fn, w)) getbuf(fn, &w);
    /* get any sign from number */
    if (chkbuf(fn, w) == '-') { getbuf(fn, &w); s = TRUE; }
    else if (chkbuf(fn, w) == '+') getbuf(fn, &w);
    if (!(isdigit(chkbuf(fn, w)))) error("Invalid real number");
    while (isdigit(chkbuf(fn, w))) { /* parse digit */
      d = chkbuf(fn, w)-'0';
      *r = *r*10+d; /* add in new digit */
      getbuf(fn, &w);
    }
    if (chkbuf(fn, w) == '.' || tolower(chkbuf(fn, w)) == 'e') { /* it's a real */

      if (chkbuf(fn, w) == '.') { /* decimal point */

            getbuf(fn, &w); /* skip '.' */
            if (!(isdigit(chkbuf(fn, w)))) error("Invalid real number");
            while (isdigit(chkbuf(fn, w))) { /* parse digit */

                d = chkbuf(fn, w)-'0';
                *r = *r*10+d; /* add in new digit */
                getbuf(fn, &w);
                e = e-1; /* count off right of decimal */

            }

        }
        if (tolower(chkbuf(fn, w)) == 'e') { /* exponent */

            getbuf(fn, &w); /* skip 'e' */
            if (!(isdigit(chkbuf(fn, w)) || chkbuf(fn, w) == '+' || 
                chkbuf(fn, w) == '-')) error("Invalid real number");
            readi(fn, &i, &w, fld); /* get exponent */
            /* find with exponent */
            e = e+i;

        }
        if (e < 0) *r = *r/pwrten(e); else *r = *r*pwrten(e);

    }
    if (s) *r = -*r;
    /* if fielded, validate the rest of the field is blank */
    if (fld) while (!chkend(fn, w)) {

        if (chkbuf(fn, w) != ' ') error("Field not blank");
        getbuf(fn, &w);
    }

}

/** ****************************************************************************

Read character fielded

Reads a character from the given file with field. An character is read from the
input file, but limited by the field width if provided. If the fielded flag is
true, will also verify that the rest of the field after the character is read
is blank.

*******************************************************************************/

void readc(filnum fn, char* c, long w, boolean fld)

{

    *c = chkbuf(fn, w); getbuf(fn, &w);
    /* if fielded, validate the rest of the field is blank */
    if (fld) while (!chkend(fn, w)) {

        if (chkbuf(fn, w) != ' ') error("Field not blank");
        getbuf(fn, &w);

    }

}

/** ****************************************************************************

Read string fielded

Reads a string from the given file with field. A string is read from the
input file, but limited by the field width if provided. If the fielded flag is
true, will also verify that the rest of the field after the string is read
is blank.

*******************************************************************************/

void reads(filnum fn, char* s, long l, long w, boolean fld)

{

    long c;

    while (l > 0) {

        c = chkbuf(fn, w); getbuf(fn, &w); *s++ = c; l--;

    }
    /* if fielded, validate the rest of the field is blank */
    if (fld) while (!chkend(fn, w)) {

        if (chkbuf(fn, w) != ' ') error("Field not blank");
        getbuf(fn, &w);

    }

}

/** ****************************************************************************

Read string padded

Reads from the input file to a given string with lenth. The number of characters
in the string are read until EOLN is encountered. If EOF is encountered, it is 
an error. After the string is read, the remaining character positions in the 
string are cleared to blanks.

*******************************************************************************/

void readsp(filnum fn, char* s,  long l)
{

    char c;

    while (l > 0 && !eolnfn(fn)) {

        if (eoffn(fn)) error("End of file");
        c = fgetc(filtable[fn]); *s++ =  c; l--;

    }
    while (l > 0) { *s++ = ' '; l--; }

}

/** ****************************************************************************

Write string padded

Writes the given text file to the end of a padded string. The end of the padded
string is the last non-space character in the string.

*******************************************************************************/

void writestrp(FILE* f, char* s, long l)
{

    long i;
    char* p;

    p = s+l-1; /* find end */
    while (l > 0 && *p == ' ') { p--; l--; }
    for (i = 0; i < l; i++) fprintf(f, "%c", *s++);

}

/** ****************************************************************************

Write zero filler to file

Writes n '0's to the file.

*******************************************************************************/

void filllz(FILE* f, long n)

{ 

    while (n > 0) { fputc('0', f); n--; } 

}

/** ****************************************************************************

Write integer with radix, field and zero fill

Writes an integer to the output file. Accepts a field, a width, a radix and a
flag indicating to zero fill. The integer is written in the given radix. It is
placed in either the right of the field, or left if the field is negative. If
the field is smaller than the number of characters needed by the integer and
sign, then it will be extended to cover the required space. If the zero fill
flag is used, then '0's are used to fill the field to the left. If the number
is negative and a non-decimal radix is specified, an error results.

*******************************************************************************/

void writei(FILE* f, long w, long fl, long r, long lz)
{

    long i, d, ds;
    char digit[MAXDBF];
    boolean sgn;

    if (w < 0) {

        sgn = TRUE; w = abs(w);
        if (r != 10) error("Non-decimal radix of negative") ;

    } else sgn = FALSE;
    for (i = 0; i < MAXDBF; i++) digit[i] = ' ';
    i = MAXDBF-1; d = 0;
    do {

        if (w % r < 10) digit[i] = w % r+'0';
        else digit[i] = w % r -10 +'a';
        w = w / r; i = i-1; d = d+1;

    } while (w != 0);
    if (sgn) ds = d+1; else ds = d; /* add sign */
    if (ds > abs(fl)) if (fl < 0) fl = -ds; else fl = ds;
    if (fl > 0 && fl > ds)
      if (lz) filllz(f, fl-ds); else fprintf(f, "%*c", (int)(fl-ds), ' ');
    if (sgn) fputc('-', f);
    for (i = MAXDBF-d; i < MAXDBF; i++) fputc(digit[i], f);
    if (fl < 1 && abs(fl) > ds) fprintf(f, "%*c", (int)(abs(fl)-ds), ' ');

}

/** ****************************************************************************

Write integer in radix

Writes the given integer with give width, radix and zero fill, to the Pascal
file.

*******************************************************************************/

void writeipf(pasfil* f, long i, long w, long r, long lz)

{

    int fn;

    valfil(f);
    fn = *f;
    if (w < 1 && ISO7185) error("Invalid field specification");
    if (fn <= COMMANDFN) switch (fn) {

         case OUTPUTFN: writei(stdout, i, w, r, lz); break;
         case ERRORFN: writei(stderr, i, w, r, lz); break;
         case LISTFN: writei(stdout, i, w, r, lz); break;
         case INPUTFN:
         case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        writei(filtable[fn], i, w, r, lz);

    }

}


/** ****************************************************************************

Write boolean to text file

Writes a boolean to the given Pascal text file, with field width. The values
"true" or "false" are output, and either clipped or space padded to fit the
given field.

*******************************************************************************/

void writeb(FILE* f, boolean b, long w)
{

    long l;

    if (b) {

        l = 4; if (l > w) l = w; /* limit string to field */
        fprintf(f, "%*.*s", (int)w, (int)l, "true");

    } else {

        l = 5; if (l > w) l = w; /* limit string to field */
        fprintf(f, "%*.*s", (int)w, (int)l, "false");

    }

}

/** ****************************************************************************

*******************************************************************************/

void putfile(FILE* f, pasfil* pf, filnum fn)

{

    if (!filbuff[fn]) error("File buffer variable undefined");
    fputc(*(pf+FILEIDSIZE), f);
    filbuff[fn] = FALSE;

}

/** ****************************************************************************

Reset file by logical file number

Resets the given file. If the binary flag is true, the file will be opened as
a binary file, otherwise it is opened as a text file.

*******************************************************************************/

void resetfn(filnum fn, boolean bin)

{

    /* file was closed, no assigned name, give it a temp name */
    if (filstate[fn] == fsclosed && !filanamtab[fn]) tmpnam(filnamtab[fn]);
    if (filstate[fn] != fsclosed)
        if (fclose(filtable[fn])) error("File close fails");
    if (!(filtable[fn] = fopen(filnamtab[fn], bin?"rb":"r")))
        error("File open fails");
    filstate[fn] = fsread;
    filbuff[fn] = FALSE;
    fileoln[fn] = FALSE;
    filbof[fn] = FALSE;

}

/** ****************************************************************************

Rewrite file by logical file number

Rewrites the given file. If the binary flag is true, the file will be opened as
a binary file, otherwise it is opened as a text file.

*******************************************************************************/

void rewritefn(filnum fn, boolean bin)

{

    /* file was closed, no assigned name, give it a temp name */
    if (filstate[fn] == fsclosed && !filanamtab[fn]) tmpnam(filnamtab[fn]);
    if (filstate[fn] != fsclosed)
        if (fclose(filtable[fn])) error("File close fails");
    if (!(filtable[fn] = fopen(filnamtab[fn], bin?"wb":"w")))
        error("File open fails");
    filstate[fn] = fswrite;
    filbuff[fn] = FALSE;

}

/** ****************************************************************************

Start of support routines

What follows are the support routines for Pascal-P6.

*******************************************************************************/

/** ****************************************************************************

Get next file element

Get next file element to the file buffer variable. Accepts a Pascal file 
pointer.

*******************************************************************************/

void psystem_get(
    /** Pascal file to read from */ pasfil* f
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case INPUTFN:   getfneoln(stdin, INPUTFN); break;
        case OUTPUTFN:  case ERRORFN:
        case LISTFN:    error("Read on write file"); break;
        case COMMANDFN: getcommand(); break;

    } else {

        if (filstate[fn] != fsread) error("File mode incorrect");
        getfneoln(filtable[fn], fn);

    }

}

/** ****************************************************************************

Put next file element

Puts the next file element from the file buffer variable. Accepts a Pascal file 
pointer.

*******************************************************************************/

void psystem_put(
    /* Pascal file to write to */ pasfil* f
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: putfile(stdout, f, fn); break;
        case ERRORFN: putfile(stderr, f, fn); break;
        case LISTFN: putfile(stdout, f, fn); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        putfile(filtable[fn], f, fn);

    }

}

/** ****************************************************************************

Read line

Reads up to the next EOLN and discards any file contents before the EOLN.

*******************************************************************************/

void psystem_rln(
    /* Pascal file to write to */ pasfil* f
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case INPUTFN: readline(INPUTFN); break;
        case OUTPUTFN: case ERRORFN:
        case LISTFN: error("Read on write only file"); break;
        case COMMANDFN: readlncommand(); break;

    } else {

        if (filstate[fn] != fsread) error("File mode incorrect");
        readline(fn);

    }

}

/** ****************************************************************************

Allocate space

Allocates new space. Expects the address of the pointer to allocate, and the
length of allocation. The space required is allocated and cleared to zeros.

*******************************************************************************/

void psystem_new(
    /** Address of pointer */ unsigned char** p,
    /** length to allocate */ unsigned long l
)

{

    *p = calloc(l, 1); /* allocate */
    if (!*p) error("Could not allocate space");

}

/** ****************************************************************************

Allocate space for checked variant record

Allocates space for a variant record with tags. Expects the address of the
pointer to allocate, and the length of allocation, a pointer to the tag array,
and the length of the tag array.

Allocates space with a "buried" tag field list. This is done by allocating the
required space with the space required for the tag array added. Then the tag
array, and its length, are placed below the record space and a pointer returned
to the record. This looks like:

    p-> Record space
        Tag count
        Tag N
        ...
        Tag 2
        Tag 1

The tag table is found behind the record base pointer. The count of tags is
placed just below the record base pointer, so that the code can find both the
number of tags and where the base of the tag array is.

Having a table of tag values used to allocate the variant record allows the
code to verify access to the variants used.

*******************************************************************************/

void psystem_nwl(
    /** Address of pointer */  unsigned char** p,
    /** length to allocate */  unsigned long   l,
    /** number of tags */      unsigned long   tc,
    /** pointer to tag list */ long*           tl
)

{

    unsigned char* p2;
    long* lp;
    unsigned long* ulp;
    long i;

    p2 = calloc(l+tc+sizeof(unsigned long), 1); /* get whole allocation */
    lp = (long*)p2;
    for (i = 0; i < tc; i++) *lp++ = *tl++; /* place tags */
    ulp = (unsigned long*)lp;
    *ulp++ = tc; /* place tag count */
    *p = (unsigned char*)ulp; /* place record base */

}

/** ****************************************************************************

Write end of line

Accepts a Pascal file pointer. An end of line sequence is output to the file.

*******************************************************************************/

void psystem_wln(
    /* Pascal file to write to */ pasfil* f
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

       case OUTPUTFN: fprintf(stdout, "\n"); break;
       case ERRORFN: fprintf(stderr, "\n"); break;
       case LISTFN: fprintf(filtable[LISTFN], "\n"); break;
       case INPUTFN:
       case COMMANDFN: error("Write on read only file"); break;

    } else {

       if (filstate[fn] != fswrite) error("File mode incorrect");
       fprintf(filtable[fn], "\n");

    }

}

/** ****************************************************************************

Write string

Accepts a Pascal file pointer, a string with length, and a field. The string is
printed to the field, which is either clipped or extended with blanks to match
the field specification.

*******************************************************************************/

void psystem_wrs(
    /* Pascal file to write to */ pasfil* f,
    /* Length of string */        int     l,
    /* String to write */         char    s[],
    /* Width of field */          int     w
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (w < 1 && ISO7185) error("Invalid field specification");
    if (l > w) l = w; /* limit string to field */
    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: fprintf(stdout, "%*.*s", w, l, s); break;
        case ERRORFN: fprintf(stderr, "%*.*s", w, l, s); break;
        case LISTFN: fprintf(stdout, "%*.*s", w, l, s); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        fprintf(filtable[fn], "%*.*s", w, l, s);

    }

}

/** ****************************************************************************

Write string padded

Accepts a Pascal file pointer, a string with length, and a field. The string is
printed with a field determined by the padded length of the string. The padded
length of the string is the number of characters, starting with the left side of
the string, that are non-space.

*******************************************************************************/

void psystem_wrsp(
    /* Pascal file to write to */ pasfil* f,
    /* String to write */         char*   s,
    /* Length of string */        int     l
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: writestrp(stdout, s, l); break;
        case ERRORFN: writestrp(stderr, s, l); break;
        case LISTFN: writestrp(stdout, s, l); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        writestrp(filtable[fn], s, l);

    }

}

/** ****************************************************************************

Find EOF of text file

Checks if the given Pascal text file is at EOF, and returns true if so.

*******************************************************************************/

boolean psystem_eof(
    /* Pascal file to write to */ pasfil* f
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    return (eoffn(fn));

}

/** ****************************************************************************

Find EOF of binary file

Checks if the given Pascal binary file is at EOF, and returns true if so.

*******************************************************************************/

boolean psystem_efb(
    /* Pascal file to write to */ pasfil* f
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (filstate[fn] == fswrite) return(TRUE);
    else if (filstate[fn] == fsread) 
        return (eoffile(filtable[fn]) && !filbuff[fn]);

}

/** ****************************************************************************

Find EOLN of text file

Checks if the given Pascal text file is at EOLN, and returns true if so.

*******************************************************************************/

boolean psystem_eln(
    /* Pascal file to write to */ pasfil* f
)

{

    int fn;

    fn = *f; /* get logical file no. */

    return (eolnfn(fn));

}

/** ****************************************************************************

Write integer decimal

Writes out the given integer in decimal form, with the given field width.

*******************************************************************************/

void psystem_wri(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 10, FALSE); /* output decimal, not zero padded */

}

/** ****************************************************************************

Write integer hexadecimal

Writes out the given integer in hexadecimal form, with the given field width.

*******************************************************************************/

void psystem_wrih(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 16, FALSE); /* output hexadecimal, not zero padded */

}

/** ****************************************************************************

Write integer octal

Writes out the given integer in octal form, with the given field width.

*******************************************************************************/

void psystem_wrio(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 8, FALSE); /* output octal, not zero padded */

}

/** ****************************************************************************

Write integer binary

Writes out the given integer in binary form, with the given field width.

*******************************************************************************/

void psystem_wrib(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 2, FALSE); /* output binary, not zero padded */

}

/** ****************************************************************************

Write integer decimal with leading zero padding

Writes out the given integer in decimal form, with the given field width. The
field width is padded out with leading zeros.

*******************************************************************************/

void psystem_wiz(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 10, TRUE); /* output decimal, zero padded */

}

/** ****************************************************************************

Write integer hexadecimal with leading zero padding

Writes out the given integer in hexadecimal form, with the given field width. 
The field width is padded out with leading zeros.

*******************************************************************************/

void psystem_wizh(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 16, TRUE); /* output hexadecimal, zero padded */

}

/** ****************************************************************************

Write integer octal with leading zero padding

Writes out the given integer in octal form, with the given field width. 
The field width is padded out with leading zeros.

*******************************************************************************/

void psystem_wizo(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 8, TRUE); /* output octal, zero padded */

}

/** ****************************************************************************

Write integer binary with leading zero padding

Writes out the given integer in binary form, with the given field width. 
The field width is padded out with leading zeros.

*******************************************************************************/

void psystem_wizb(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w
)

{

    int fn;

    fn = *f; /* get logical file no. */

    writeipf(f, i, w, 2, TRUE); /* output binary, zero padded */

}

/** ****************************************************************************

Write real

Writes out the given real, with the given field width.

*******************************************************************************/

void psystem_wrr(
    /* Pascal file to write to */ pasfil* f,
    /* Real to write */           double  r,
    /* Field width */             long    w
)

{

    int fn;
    int l;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (w < 1) error("Invalid field specification");
    if (w < REALEF) w = REALEF; /* set minimum width */
    l = w-REALEF+1; /* assign leftover to fractional digits w/o sign */
    if (fn <= COMMANDFN) switch (fn) {

         case OUTPUTFN: fprintf(stdout, "%*.*e", (int)w, l, r); break;
         case ERRORFN: fprintf(stderr, "%*.*e", (int)w, l, r); break;
         case LISTFN: fprintf(stdout, "%*.*e", (int)w, l, r); break;
         case INPUTFN:
         case COMMANDFN: error("Write on read only file");

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        fprintf(filtable[fn], "%*.*e", (int)w, l, r);

    }

}

/** ****************************************************************************

Write character to text file

Writes out the given character, with the given field width.

*******************************************************************************/

void psystem_wrc(
    /* Pascal file to write to */ pasfil* f,
    /* Character to write */      char    c,
    /* Field width */             long    w
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (w < 1 && ISO7185) error("Invalid field specification");
    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: fprintf(stdout, "%*c", (int)w, c); break;
        case ERRORFN: fprintf(stderr, "%*c", (int)w, c); break;
        case LISTFN: fprintf(stdout, "%*c", (int)w, c); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        fprintf(filtable[fn], "%*c", (int)w, c);

    }

}

/** ****************************************************************************

Read integer from text file

Reads an integer from the given text file and returns it.

*******************************************************************************/

void psystem_rdi(
    /* Pascal file to write to */ pasfil* f,
    /* integer to read */         long*    i
)

{

    int fn;
    long w;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    w = INT_MAX;
    readi(fn, i, &w, FALSE);

}

/** ****************************************************************************

Read integer from text file with field

Reads an integer from the given text file with the given field and returns it.
See the Pascaline specification. Only the characters in the indicated field will
be used, even if followed by other digits.

*******************************************************************************/

void psystem_rdif(
    /* Pascal file to write to */ pasfil* f,
    /* integer to read */         long*    i,
    /* Field */                   long    w
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readi(fn, i, &w, TRUE);

}

/** ****************************************************************************

Read integer bounded from text file

Reads an integer with bounds from the given text file and returns it.

The bounds give the acceptable limits of the integer value, from minimum to 
maximum. Out of range integers will result in an error.

*******************************************************************************/

void psystem_rib(
    /* Pascal file to write to */ pasfil* f,
    /* integer to read */         long*    i,
    /* Bounds for integer */      long mn, long mx
)

{

    int fn;
    long w;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    w = INT_MAX; 
    readi(fn, i, &w, FALSE);
    if (*i < mn || *i > mx) error("Value out of range");

}

/** ****************************************************************************

Read integer bounded from text file with field

Reads an integer with bounds from the given text file with the given field and
returns it. See the Pascaline specification. Only the characters in the 
indicated field will be used, even if followed by other digits.

The bounds give the acceptable limits of the integer value, from minimum to 
maximum. Out of range integers will result in an error.

*******************************************************************************/

void psystem_ribf(
    /* Pascal file to write to */ pasfil* f,
    /* integer to read */         long* i,
    /* Bounds for integer */      long mn, long mx,
    /* Field to read */           long w
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readi(fn, i, &w, TRUE);
    if (*i < mn || *i > mx) error("Value out of range");

}

/** ****************************************************************************

Read real from text file

Reads a real in floating format from the given text file. Returns the real as
read.

*******************************************************************************/

void psystem_rdr(
    /* Pascal file to read from */ pasfil* f,
    /* real to read */             double* r
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readr(fn, r, INT_MAX, FALSE); /* read real */

}

/** ****************************************************************************

Read real from text file with field

Reads a real in floating format from the given text file from the given field.
Returns the real as read. See the Pascaline specification. Only the characters
in the indicated field will be used, even if followed by other digits.

*******************************************************************************/

void psystem_rdrf(
    /* Pascal file to read from */ pasfil* f,
    /* real to read */             double* r,
    /* Field */                    long    w
)

{

    int    fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readr(fn, r, w, TRUE); /* read real */

}

/** ****************************************************************************

Read character from text file

Reads a character from the given text file. Returns the character.

*******************************************************************************/

void psystem_rdc(
    /* Pascal file to read from */ pasfil* f,
    /* character to read */        char*   c
)

{

    int  fn;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readc(fn, c, INT_MAX, FALSE);

}

/** ****************************************************************************

Read character from text file with field

Reads a character from the given text file with the given field. Returns the 
character. See the Pascaline specification. Only the characters in the indicated
field will be used, even if followed by other digits.

*******************************************************************************/

void psystem_rdcf(
    /* Pascal file to read from */ pasfil* f,
    /* character to read */        char*   c,
    /* Field */                    long    w
)

{

    int  fn;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readc(fn, c, w, TRUE);

}

/** ****************************************************************************

Read character from text file with range check

Reads a character from the given text file with a range check. The minimum and 
and maximum are specified, and an error results if the character is not within
those two. Returns the character.

*******************************************************************************/

void psystem_rcb(
    /* Pascal file to read from */ pasfil* f,
    /* character to read */        char*   c,
    /* Range of values          */ long mn, long mx
)

{

    int  fn;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readc(fn, c, INT_MAX, FALSE);
    if (*c < mn || *c > mx) error("Value out of range");

}

/** ****************************************************************************

Read character from text file with range check and field

Reads a character from the given text file with a range check and field. The 
minimum and and maximum are specified, and an error results if the character is
not within those two. See the Pascaline specification. Only the characters in
the indicated field will be used, even if followed by other digits. Returns the
character.

*******************************************************************************/

char psystem_rcbf(
    /* Pascal file to read from */ pasfil* f,
    /* character to read */        char*   c,
    /* Range of values          */ long mn, long mx,
    /* Field                    */ long w
)

{

    int  fn;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readc(fn, c, w, TRUE);
    if (*c < mn || *c > mx) error("Value out of range");

}

/** ****************************************************************************

Find sine of real

Finds the sine of the given number and returns that.

*******************************************************************************/

double psystem_sin(
    /* Real to find sine of */ double r
)

{

    return (sin(r));

}

/** ****************************************************************************

Find cosine of real

Finds the cosine of the given number and returns that.

*******************************************************************************/

double psystem_cos(
    /* Real to find cosine of */ double r
)

{

    return (cos(r));

}

/** ****************************************************************************

Find exponential of real

Finds the exponential of the given number and returns that.

*******************************************************************************/

double psystem_exp(
    /* Real to find exponential of */ double r
)

{

    return (exp(r));

}

/** ****************************************************************************

Find logarithm of real

Finds the logarithm of the given number and returns that.

*******************************************************************************/

double psystem_log(
    /* Real to find exponential of */ double r
)

{

    return (log(r));

}

/** ****************************************************************************

Find square root of real

Finds the square root of the given number and returns that.

*******************************************************************************/

double psystem_sqt(
    /* Real to find square root of */ double r
)

{

    return (sqrt(r));

}

/** ****************************************************************************

Find arctangent of real

Finds the arctangent of the given number and returns that.

*******************************************************************************/

double psystem_atn(
    /* Real to find square root of */ double r
)

{

    return (atan(r));

}

/** ****************************************************************************

Page text file

Outputs a page or form-feed to the given Pascal text file.

*******************************************************************************/

void psystem_pag(
    /* Pascal file to page */ pasfil* f
)

{

    int  fn;
    char c;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: fprintf(stdout, "\f"); break;
        case ERRORFN: fprintf(stderr, "\f"); break;
        case LISTFN: fprintf(stdout, "\f"); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        fprintf(filtable[fn], "\f");

    }

}

/** ****************************************************************************

Reset file text

Resets the given Pascal text file to the beginning and sets it in read mode.

*******************************************************************************/

void psystem_rsf(
    /* Pascal file to reset */ pasfil* f
)

{

    int  fn;
    char c;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: case ERRORFN: case LISTFN: case INPUTFN:
        case COMMANDFN:
            error("Cannot reset or rewrite standardfile"); break;

    } else resetfn(fn, FALSE);

}

/** ****************************************************************************

Rewrite file text

Rewrites the given Pascal text file to the beginning and clears the file.

*******************************************************************************/

void psystem_rwf(
    /* Pascal file to rewrite */ pasfil* f
)

{

    int  fn;
    char c;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case ERRORFN: case LISTFN: case OUTPUTFN: case INPUTFN:
        case COMMANDFN:
            error("Cannot reset or rewrite standardfile"); break;

    } else rewritefn(fn, FALSE);

}

/** ****************************************************************************

Write boolean to text file

Writes the given boolean to the Pascal text file with width.

*******************************************************************************/

void psystem_wrb(
    /* Pascal file to rewrite */ pasfil* f,
    /* Boolean to write */       boolean b,
    /* Width */                  long    w
)

{

    int  fn;
    char c;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    b = b != 0;
    if (w < 1) error("Invalid field specification");
    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: writeb(stdout, b, w); break;
        case ERRORFN: writeb(stderr, b, w); break;
        case LISTFN: writeb(stdout, b, w); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        writeb(filtable[fn], b, w);

    }

}

/** ****************************************************************************

Write real to text file in fixed point notation

Writes a given real to a Pascal text file with field and fraction. 

*******************************************************************************/

void psystem_wrf(
    /* Pascal file to rewrite */ pasfil* f,
    /* Real to write */          double  r,
    /* Width */                  long    w,
    /* Fraction */               long    fr
)

{

    int  fn;
    char c;
    
    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (w < 1 && ISO7185) error("Invalid field specification");
    if (fr < 1) error("Invalid fraction specification");
    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: fprintf(stdout, "%*.*f", (int)w, (int)fr, r); break;
        case ERRORFN: fprintf(stderr, "%*.*f", (int)w, (int)fr, r); break;
        case LISTFN: fprintf(stdout, "%*.*f", (int)w, (int)fr, r); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        fprintf(filtable[fn], "%*.*f", (int)w, (int)fr, r);

    }

}

/** ****************************************************************************

Dispose of dynamic variable

Returns the indicated space with the given size to free store. The size is not
used. The block is checked for outstanding variable references.

*******************************************************************************/

void psystem_dsp(
    /** Block to dispose of */ unsigned char* p,
    /** Size of block */       unsigned long  s
)

{

    if (varlap(p, p+s-1)) error("Dispose of varaible referenced block");
    free(p);

}

/** ****************************************************************************

Deallocate space for checked variant record

Deallocates space for a variant record with tags. Expects the address of the
pointer to deallocate, and the length of allocation, a pointer to the tag array,
and the length of the tag array.

Deallocates space with a "buried" tag field list. Expects that the address given
will be of a variant record that was allocated with psystem_nwl, which places a
tag field list below the pointer. This looks like:

    p-> Record space
        Tag count
        Tag N
        ...
        Tag 2
        Tag 1

The tag table is found behind the record base pointer. The count of tags is
placed just below the record base pointer, so that the code can find both the
number of tags and where the base of the tag array is.

The tag list given is matched to the tag list of the allocated pointer, and an
error results if they don't match in number and value. This would indicate that
a different set of tags were used to dispose of the variant record than was
used to create it.

Once the tags are matched, the whole block is disposed of, both the tag list and
the variant record.

*******************************************************************************/

void psystem_dsl(
    /** Address of block */    unsigned char* p,
    /** length to allocate */  unsigned long  l,
    /** number of tags */      unsigned long  tc,
    /** pointer to tag list */ long*          tl
)

{

    unsigned long* ulp;
    long *         lp;
    unsigned char* bp;

    ulp = (unsigned long*) p; /* point to top */
    ulp--;
    if (*ulp != tc) error("New/dispose tags do not match in number");
    lp = (unsigned long*)ulp; /* point to bottom */
    bp = (unsigned char*) lp; /* save that */
    while (tc) { /* compare tags list */

        if (*lp != *tl) error("New/dispose tags do not match");
        lp++; /* next tag */
        tl++;
        tc--;

    }
    free(bp); /* free the net block */

}

/** ****************************************************************************

Write binary variable to binary file

Writes a binary variable to the given Pascal binary file. The binary variable
can be any size, and the size bytes from the variable address are written.

In Pascal-P6, files are typed text or binary, and binary files are structured
as bytes.

*******************************************************************************/

void psystem_wbf(
    /* Pascal file to rewrite */ pasfil*        f,
    /* Variable to write */      unsigned char* p,
    /* Length to write */        long           l
)

{

    int  fn;
    long i;
    
    valfilwm(f); /* validate file for writing */
    fn = *f;     /* get logical file no. */
    FILE* fp;    /* file pointer */

    fp = filtable[fn];
    for (i = 0; i < l; i++) fputc(*p++, fp);

}

/** ****************************************************************************

Write integer variable to binary file

Writes a integer variable to the given Pascal binary file. The integer variable
is written out as a series of bytes. 

In Pascal-P6, files are typed text or binary, and binary files are structured
as bytes.

*******************************************************************************/

void psystem_wbi(
    /* Pascal file to rewrite */ pasfil* f,
    /* Variable to write */      long*   p
)

{

    int  fn;
    long i;
    
    valfilwm(f); /* validate file for writing */
    fn = *f;     /* get logical file no. */
    FILE* fp;    /* file pointer */

    fp = filtable[fn];
    for (i = 0; i < INTSIZE; i++) fputc(*p++, fp);

}

/** ****************************************************************************

Write byte variable to binary file

Writes a byte variable to the given Pascal binary file. A single byte is 
written. 

In Pascal-P6, files are typed text or binary, and binary files are structured
as bytes.

*******************************************************************************/

void psystem_wbx(
    /* Pascal file to rewrite */ pasfil*        f,
    /* Variable to write */      unsigned char* p
)

{

    int  fn;
    long i;
    
    valfilwm(f); /* validate file for writing */
    fn = *f;     /* get logical file no. */

    fputc(*p++, filtable[fn]);

}

/** ****************************************************************************

Write real variable to binary file

Writes a real variable to the given Pascal binary file. Writes the real as a
series of bytes.

In Pascal-P6, files are typed text or binary, and binary files are structured
as bytes.

*******************************************************************************/

void psystem_wbr(
    /* Pascal file to rewrite */ pasfil* f,
    /* Variable to write */      double* p
)

{

    int  fn;
    long i;
    
    valfilwm(f); /* validate file for writing */
    fn = *f;     /* get logical file no. */
    FILE* fp;    /* file pointer */

    fp = filtable[fn];
    for (i = 0; i < REALSIZE; i++) fputc(*p++, fp);

}

/** ****************************************************************************

Write character variable to binary file

Writes a character variable to the given Pascal binary file. Writes the 
character as a series of bytes (one in the case of ASCII, more for Unicode).

In Pascal-P6, files are typed text or binary, and binary files are structured
as bytes.

*******************************************************************************/

void psystem_wbc(
    /* Pascal file to rewrite */ pasfil*        f,
    /* Variable to write */      unsigned char* p
)

{

    int  fn;
    long i;
    
    valfilwm(f); /* validate file for writing */
    fn = *f;     /* get logical file no. */
    FILE* fp;    /* file pointer */

    fp = filtable[fn];
    for (i = 0; i < CHARSIZE; i++) fputc(*p++, fp);

}

/** ****************************************************************************

Write boolean variable to binary file

Writes a boolean variable to the given Pascal binary file. Writes the boolean as
a single byte.

In Pascal-P6, files are typed text or binary, and binary files are structured
as bytes.

*******************************************************************************/

void psystem_wbb(
    /* Pascal file to rewrite */ pasfil*  f,
    /* Variable to write */      boolean* p
)

{

    int  fn;
    long i;
    
    valfilwm(f); /* validate file for writing */
    fn = *f;     /* get logical file no. */

    fputc(*p++, filtable[fn]);

}

/** ****************************************************************************

Read binary variable from binary file

Reads a binary variable from the given Pascal binary file. The binary variable
can be any size, and the size bytes from the variable address are read.

In Pascal-P6, files are typed text or binary, and binary files are structured
as bytes.

*******************************************************************************/

void psystem_rbf(
    /* Pascal file to rewrite */ pasfil*        f,
    /* Variable to read */       unsigned char* p,
    /* Length to read */         long           l
)

{

    int  fn;
    long i;
    
    valfilwm(f); /* validate file for writing */
    fn = *f;     /* get logical file no. */
    FILE* fp;    /* file pointer */

    fp = filtable[fn];
    for (i = 0; i < l; i++) *p++ = fgetc(fp);

}

/** ****************************************************************************

Reset file binary

Resets the given Pascal binary file.

*******************************************************************************/

void psystem_rsb(
    /* Pascal binary file */ pasfil* f
)

{

    int  fn;
    long i;
    
    valfil(f); /* validate file */
    fn = *f;   /* get logical file no. */

    resetfn(fn, TRUE);

}

/** ****************************************************************************

Rewrite file binary

Rewrites the given Pascal binary file.

*******************************************************************************/

void psystem_rwb(
    /* Pascal binary file */ pasfil* f
)

{

    int  fn;
    long i;
    
    valfil(f); /* validate file */
    fn = *f;   /* get logical file no. */

    rewritefn(fn, TRUE);

}

/** ****************************************************************************

Get file binary

Loads the next file element from a binary file. The Pascal binary file is
specified, with the size of each element.

*******************************************************************************/

void psystem_gbf(
    /* Pascal binary file */     pasfil*       f,
    /* Length of file element */ unsigned long l
)

{

    int            fn;
    long           i;
    unsigned char* bp;
    FILE*          fp;
    
    valfilrm(f); /* validate file */
    fn = *f;   /* get logical file no. */

    bp = f+FILEIDSIZE; /* index the file variable */
    fp = filtable[fn]; /* get file pointer */
    if (varlap(bp, bp+l-1))
        error("Variable referenced file buffer modified");
    if (filbuff[fn]) filbuff[fn] = FALSE; /* if buffer is full, just dump it */
    else /* fill buffer from file */
        for (i = 0; i < l; i++) *bp++ = fgetc(fp);

}

/** ****************************************************************************

Put file binary

Writes the next file element to a binary file. The Pascal binary file is
specified, with the size of each element.

*******************************************************************************/

void psystem_pbf(
    /* Pascal binary file */     pasfil*       f,
    /* Length of file element */ unsigned long l
)

{

    int            fn;
    long           i;
    unsigned char* bp;
    FILE*          fp;
    
    valfilwm(f); /* validate file */
    fn = *f;   /* get logical file no. */

    bp = f+FILEIDSIZE; /* index the file variable */
    fp = filtable[fn]; /* get file pointer */
    if (!filbuff[fn]) error("File buffer variable undefined");
    for (i = 0; i < l; i++) fputc(*bp++, fp);
    filbuff[fn] = FALSE; /* set buffer empty */

}

/** ****************************************************************************

Pascal text file buffer validate

Determines if the given Pascal text file has a valid buffer variable loaded. If
not, it is loaded from the input file. This satisfies the Lazy I/O concept.

*******************************************************************************/

void psystem_fbv(
    /* Pascal binary file */     pasfil*       f
)

{

    int            fn;
    long           i;
    unsigned char* bp;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    bp = f+FILEIDSIZE; /* index the file variable */
    if (fn == INPUTFN) *bp = buffn(INPUTFN);
    else {

        if (filstate[fn] == fsread)
        *bp = buffn(fn);

    }
    filbuff[fn] = TRUE;

}

/** ****************************************************************************

Pascal binary file buffer validate

Determines if the given Pascal binary file has a valid buffer variable loaded. 
If not, it is loaded from the input file. This satisfies the Lazy I/O concept.

*******************************************************************************/

void psystem_fvb(
    /* Pascal binary file */     pasfil*       f,
    /* Length of file element */ unsigned long l
)

{

    int            fn;
    long           i;
    unsigned char* bp;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    bp = f+FILEIDSIZE; /* index the file variable */
    /* load buffer only if in read mode, and buffer is
       empty */
    if (filstate[fn] == fsread && !filbuff[fn])
        for (i = 0; i < l; i++) *bp++ = fgetc(filtable[fn]);
    filbuff[fn] = TRUE;

}

/** ****************************************************************************

Assign filename for text file

Assigns the given filename and length to the Pascal text file. When Pascal files
are not assigned a name, they are opened as anonymous files, which are deleted
automatically when closed. Assigning Pascal files a name makes them a permanent
part of the filesystem.

*******************************************************************************/

void psystem_asst(
    /* Pascal file to assign name */ pasfil* f,
    /* Name string to assign */      char*   n,
    /* Length of name */             long    l
)

{

    int            fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (l >= FILLEN) error("File name too long");
    strcpyl(filnamtab[fn], n, l);
    filanamtab[fn] = TRUE; /* set name assigned */

}

/** ****************************************************************************

Assign filename for binary file

Assigns the given filename and length to the Pascal binary file. When Pascal 
files are not assigned a name, they are opened as anonymous files, which are 
deleted automatically when closed. Assigning Pascal files a name makes them a 
permanent part of the filesystem.

*******************************************************************************/

void psystem_assb(
    /* Pascal file to assign name */ pasfil* f,
    /* Name string to assign */      char*   n,
    /* Length of name */             long    l
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (l >= FILLEN) error("File name too long");
    strcpyl(filnamtab[fn], n, l);
    filanamtab[fn] = TRUE; /* set name assigned */

}

/** ****************************************************************************

Close Pascal text file

Closes the given Pascal text file.

*******************************************************************************/

void psystem_clst(
    /* Pascal text file to close */ pasfil* f
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (fclose(filtable[fn])) error("File close fails");
    /* if the file is temp, remove now */
    if (!filanamtab[fn]) remove(filnamtab[fn]);
    filanamtab[fn] = FALSE; /* break any name association */

}

/** ****************************************************************************

Close Pascal binary file

Closes the given Pascal text file.

*******************************************************************************/

void psystem_clsb(
    /* Pascal text file to close */ pasfil* f
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (fclose(filtable[fn])) error("File close fails");
    /* if the file is temp, remove now */
    if (!filanamtab[fn]) remove(filnamtab[fn]);
    filanamtab[fn] = FALSE; /* break any name association */

}

/** ****************************************************************************

Position Pascal binary file

Positions the given Pascal binary file to the element given. Note that only
binary files in Pascal can be positioned. File positions are 1 to n.

*******************************************************************************/

void psystem_pos(
    /* Pascal file */ pasfil* f,
    /* Position */    long i
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */
    if (i < 1) error("Invalid file position");
    if (fseek(filtable[fn], i-1, SEEK_SET)) error("File position fails");

}

/** ****************************************************************************

Update Pascal binary file

Resets the given file to the beginning, but does not clear the contents of the
file.

*******************************************************************************/

void psystem_upd(
    /* Pascal file */ pasfil* f
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (filstate[fn] == fsread) fseek(filtable[fn], 0, SEEK_SET);
    else {

      if (fclose(filtable[fn])) error("File close fails");
      if (!fopen(filnamtab[fn], "wb")) error("File open fails");

    }

}

/** ****************************************************************************

Append Pascal text file

Positions the given file to the end and changes to write mode, effectively 
setting up the file to append.

*******************************************************************************/

void psystem_appt(
    /* Pascal file */ pasfil* f
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (filstate[fn] == fswrite) fseek(filtable[fn], 0, SEEK_END);
    else {

      if (fclose(filtable[fn])) error("File close fails");
      if (!fopen(filnamtab[fn], "w")) error("File open fails");

    }

}

/** ****************************************************************************

Append Pascal binary file

Positions the given file to the end and changes to write mode, effectively 
setting up the file to append.

*******************************************************************************/

void psystem_appb(
    /* Pascal file */ pasfil* f
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (filstate[fn] == fswrite) fseek(filtable[fn], 0, SEEK_END);
    else {

      if (fclose(filtable[fn])) error("File close fails");
      if (!fopen(filnamtab[fn], "w")) error("File open fails");

    }

}

/** ****************************************************************************

Delete file by name

Expects a filename string and a length. Deletes the file from storage.

*******************************************************************************/

void psystem_del(
    /* Filename string */ char* n,
    /* Length */          long l
)

{

    filnam fn;
    int    r;

    strcpyl(fn, n, l); /* copy string to buffer */
    r = remove(fn); /* remove file */
    if (r) error("File delete fails");

}

/** ****************************************************************************

Change filename

Expects a old filename and length, and a new filename and length. The filename
is changed on disk. Note that the path must match between the names.

*******************************************************************************/

void psystem_chg(
    /* Old filename string */ char* on,
    /* Length */              long  ol,
    /* New filename string */ char* nn,
    /* Length */              long  nl
)

{

    filnam ofn, nfn;
    int    r;

    strcpyl(ofn, on, ol); /* copy strings to buffers */
    strcpyl(nfn, nn, nl);
    r = rename(ofn, nfn); /* rename file */
    if (r) error("File name change fails");

}

/** ****************************************************************************

Find length of binary file

Finds the length in elements of a given binary file.

??? doesn't this and other calls need to know the size of the element ???

*******************************************************************************/

long psystem_len(
    /* Pascal file */ pasfil* f
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    return (lengthfile(filtable[fn]));

}

/** ****************************************************************************

Find location of binary file

Finds the current read/write location in elements of a given binary file.

??? doesn't this and other calls need to know the size of the element ???

*******************************************************************************/

long psystem_loc(
    /* Pascal file */ pasfil* f
)

{

    int fn;
    long i;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    i = ftell(filtable[fn]);
    if (i < 0) error("File position fail");

    return (i+1);

}

/** ****************************************************************************

Find file exists

Finds out if the given string with length exists in storage by that filename.
Returns true if so, otherwise false.

*******************************************************************************/

boolean psystem_exs(
    /* Filename string */ char* n,
    /* String length */   long  l
)

{

    filnam fn;
    FILE* fp;

    strcpyl(fn, n, l); /* make a copy of the string */
    if (fp = fopen(fn, "r")) fclose(fp); /* test file exists */

    return (!!fp); /* exit with status */

}

/** ****************************************************************************

Halt program

Simply halts the running program.

*******************************************************************************/

void psystem_hlt(void)

{

    exit(0);

}

/** ****************************************************************************

Assert truth value

Checks if the given truth value is true, and continues if so. If not, the 
program is halted with an error.

*******************************************************************************/

void psystem_ast(
    /* Truth value */ long i
)

{

    if (i == 0) error("Program code assertion fails");

}

/** ****************************************************************************

Assert truth value with message

Checks if the given truth value is true, and continues if so. If not, the 
program is halted with an error message given by the call.

*******************************************************************************/

void psystem_asts(
    /* Truth value */          long  i,
    /* Error message */        char* e,
    /* Error message length */ long l
)

{

    errmsg em;
    
    strcpyl(em, e, l); /* copy error string */
    if (i == 0) error(em);

}

/** ****************************************************************************

Read string

Reads a string from the Pascal text file. Expects a string address and length,
The number of characters in the string are read.

*******************************************************************************/

long psystem_rds(
    /* Pascal file */ pasfil* f,
    /* String */      char*   s,
    /* Length */      long    l
)

{

    int fn;
    long i;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    reads(fn, s, l, INT_MAX, FALSE);

}

/** ****************************************************************************

Read string

Read string from Pascal text file. Expects a string address, length and field, 
The number of characters in the field are read. If the field is smaller than the
string, only the field number of characters are read, and rest of the string is
left uninitialized. If the field is longer than the string, the string is 
filled, then the rest of the field characters are expected to be blank.

*******************************************************************************/

long psystem_rdsf(
    /* Pascal file */ pasfil* f,
    /* String */      char*   s,
    /* Length */      long    l,
    /* Field */       long    w
)

{

    int fn;
    long i;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    reads(fn, s, l, w, TRUE);

}

/** ****************************************************************************

Read padded string

Reads the string from a Pascal text file. Expects the string address and length.
The string is read until either the string is full, or eoln occurs. If there are
more characters, and the string is full, an error results. If eoln occurs first,
the rest of the string is filled with blanks.

*******************************************************************************/

long psystem_rdsp(
    /* Pascal file */ pasfil* f,
    /* String */      char*   s,
    /* Length */      long    l,
    /* Field */       long    w
)

{

    int fn;
    long i;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    readsp(fn, s, l);

}

/** ****************************************************************************

Assign external file text

Assign text file to external header file. The given file is assigned to a file
read from the header file, or by name of file. The program name of the file
variable is passed. The standard Pascaline behavior is to read the filename from
the command line and use that for the assignment. However, other behaviors are
possible, like using the variable name.

*******************************************************************************/

long psystem_aeft(
    /* Pascal file */     pasfil* f,
    /* Filename String */ char*   s,
    /* Length */          long    l
)

{

    int fn;
    long i;
    filnam fs;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    strcpyl(fs, s, l); /* make copy of string */
    assignexternal(fn, fs);

}

/** ****************************************************************************

Assign external file binary

Assign binary file to external header file. The given file is assigned to a file
read from the header file, or by name of file. The program name of the file
variable is passed. The standard Pascaline behavior is to read the filename from
the command line and use that for the assignment. However, other behaviors are
possible, like using the variable name.

*******************************************************************************/

long psystem_aefb(
    /* Pascal file */     pasfil* f,
    /* Filename String */ char*   s,
    /* Length */          long    l
)

{

    int fn;
    long i;
    filnam fs;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    strcpyl(fs, s, l); /* make copy of string */
    assignexternal(fn, fs);

}

/** ****************************************************************************

Read integer external

Reads an external integer for a header parameter. The program name of the header
parameter and its length are passed as parameters, but the standard Pascaline
behavior is to read the integer from the command line. The integer is returned.

*******************************************************************************/

void psystem_rdie(
    /* integer to read */                long* i,
    /* Name of header variable */        char* n,
    /* Length of header variable name */ long  l
)

{

    long w;

    w = INT_MAX; 
    readi(COMMANDFN, i, &w, FALSE);

}

/** ****************************************************************************

Read real external

Reads an external real for a header parameter. The program name of the header
parameter and its length are passed as parameters, but the standard Pascaline
behavior is to read the real from the command line. The real is returned.

*******************************************************************************/

void psystem_rdre(
    /* real to read */                   double* r,
    /* Name of header variable */        char*   n,
    /* Length of header variable name */ long    l
)

{

    long w;

    w = INT_MAX; 
    readr(COMMANDFN, r, w, FALSE);

}

/** ****************************************************************************

Throw exception

*******************************************************************************/

void psystem_thw(void)

{

    error("Throw function not implemented");

}

/** ****************************************************************************

Compare strings

Compares two strings, and returns:

<0 - String 2 greater than string 1.
0  - String 1 and 2 equal.
1  - String 1 greater than string 2

ie., just like strcmp, but does not treat 0's specially. The length is 
specified, and both strings must be of the same length.

*******************************************************************************/

int psystem_strcmp(
    /** First string */  char* s1,
    /** Second string */ char* s2, 
    /** Length */        int l
)

{

    int     i;
    boolean b;
    int     r;

    i = 0;
    b = TRUE;
    while (b && i<l) {

        if (*s1++ == *s2++) i++;
        else b = FALSE;

    }
    if (i == l) i = i-1;
    s1--;
    s2--;

    if (b) r = 0;
    else if (*s1 < *s2) r = -1;
    else r = 1;

}

/** ****************************************************************************

Check change to tagfield in variable referenced variant

Checks if a new tagfield is different from the old one, and performs a VAR
reference overlap check on the variant if so.

*******************************************************************************/

void psystem_tagchgvar(
    /* offset to tagfield */               unsigned long off,
    /* size of variant */                  unsigned long size,
    /* address of logical variant table */ unsigned long lvt[],
    /* new tagfield content */             long ntag,
    /* old tagfield content */             long otag,
    /* address of tagfield */              unsigned char* taddr
)

{

    /* check valid tag. We don't allow negative tags for the check, even
       though that is valid ISO 7185 */
    if (ntag < 0 || ntag >= lvt[0]) psystem_errorv(VALUEOUTOFRANGE);
    taddr += size; /* offset to variant */
    /* translate both tags to a logical variant, then check has changed */
    if (lvt[ntag+1] != lvt[otag+1])
        /* if changed, see if the variant is in a VAR referenced region */
        if (varlap(taddr, taddr+size)) psystem_errorv(CHANGETOVARREFERENCEDVARIANT);

}

/** ****************************************************************************

Check change to tagfield invalidate

Checks if a new tagfield is different from the old one, and invalidates the
variant if so.

This is really a placeholder since we don't implement undefined checks, which
is the whole point of this routine.

*******************************************************************************/

void psystem_tagchginv(
    /* offset to tagfield */               unsigned long off,
    /* size of variant */                  unsigned long size,
    /* address of logical variant table */ unsigned long lvt[],
    /* new tagfield content */             long ntag,
    /* old tagfield content */             long otag,
    /* address of tagfield */              unsigned char* taddr
)

{

    /* check valid tag. We don't allow negative tags for the check, even
       though that is valid ISO 7185 */
    if (ntag < 0 || ntag >= lvt[0]) psystem_errorv(VALUEOUTOFRANGE);

}

/** ****************************************************************************

Check matching templates

Accepts to template addresses, and the number of levels they contain. The 
templates are matched, and if they don't match, an error is thrown.

*******************************************************************************/

void psystem_cmptmp(
    /* number of levels */ unsigned long lvl,
    /* first template */   unsigned long* tmp1,
    /* second template */  unsigned long* tmp2
)
{

    while (lvl--) if (*tmp1++ != *tmp2++) psystem_errorv(CONTAINERMISMATCH);

}

/** ****************************************************************************

Check tag assignment

*******************************************************************************/

void psystem_tagchkass(
    /* offset to tagfield from record start */ unsigned long  off,
    /* nesting level of tagfield */            unsigned long  lvl,
    /* address of logical variant table */     unsigned long  lvt[],
    /* new tagfield content */                 long           ntag,
    /* address of tagfield */                  unsigned char* taddr
)

{

    unsigned long* tcp;

    taddr = taddr-off-INTSIZE; /* index top of tag constant list on heap */
    /* fetch table count adjusted for system bias */
    taddr = taddr-ADRSIZE-1;
    tcp = (unsigned long*) taddr; /* put in word form */
    if (*tcp >= lvl) { /* if in tagfield constant list */

        /* check valid tag. We don't allow negative tags for the check, even
           though that is valid ISO 7185 */
        if (ntag < 0 || ntag >= lvt[0]) psystem_errorv(VALUEOUTOFRANGE);
        if (tcp[lvl-1] != lvt[lvl+ntag+1]) psystem_errorv(CHANGETOALLOCATEDTAGFIELD);

    }

}

/** ****************************************************************************

Check set bounds

Given a low and high bounds for the set base type, checks if the given set lies
only within the bounds. That is, there are no members of the set that lie 
outside the bounds.

*******************************************************************************/

void psystem_chksetbnd(
    /* low bound */  long    low,
    /* high bound */ long    high,
    /* set */        settype s
)

{

    long j;

    for (j = SETLOW; j < low; j++)
        if (sisin(j, s)) psystem_errorv(SETELEMENTOUTOFRANGE);
    for (j = high+1; j <= SETHIGH; j++)
        if (sisin(j, s)) psystem_errorv(SETELEMENTOUTOFRANGE);

}

/** ****************************************************************************

Set singleton set

Clears the given set and sets the single bit given.

*******************************************************************************/

void psystem_setsgl(
    /* bit to set */ long    b,
    /* set */        settype s
)

{

    long i;

    for (i = 0; i < SETSIZE; i++) s[i] = 0;
    s[b/8] |= 1<<b%8;

}

/** ****************************************************************************

Find set difference

Finds set difference, or s1-s2.

*******************************************************************************/

void psystem_setdif(
    /* a set */ settype d,
    /* set b */ settype b
)

{

    long i;

    for (i = 0; i < SETSIZE; i++) d[i] = d[i] & ~b[i];

}

/** ****************************************************************************

Find set intersection

Finds set intersection.

*******************************************************************************/

void psystem_setint(
    /* a set */ settype d,
    /* set b */ settype b
)

{

    long i;

    for (i = 0; i < SETSIZE; i++) d[i] = d[i] & b[i];

}

/** ****************************************************************************

Find set union

Finds set union.

*******************************************************************************/

void psystem_setuni(
    /* a set */ settype d,
    /* set b */ settype b
)

{

    long i;

    for (i = 0; i < SETSIZE; i++) d[i] = d[i] | b[i];

}

/** ****************************************************************************

Find set membership

Finds if the element of the set exists. Expects the element number and the set.
Returns true if the element is included, otherwise false.

*******************************************************************************/

boolean psystem_setsin(
    /* set element */ long i,
    /* set  */        settype s
)

{

    return (!!(s[i/8] & 1<<i%8));

}

/** ****************************************************************************

Find set equal

Finds if the sets are equal. Returns true if the sets are equal, otherwise 
false.

*******************************************************************************/

boolean psystem_setequ(
    /* a set */ settype a,
    /* set b */ settype b
)

{

    long i;

    for (i = 0; i < SETSIZE; i++) if (a[i] != b[i]) return (FALSE);
    return (TRUE);

}

/** ****************************************************************************

Find set inclusion

Finds if all of the elements of a are in b.

*******************************************************************************/

boolean psystem_setinc(
    /* a set */ settype a,
    /* set b */ settype b
)

{

    long i;

    for (i = 0; i < SETSIZE; i++)
        if ((a[i] & b[i]) != b[i]) return (FALSE);
    return (TRUE);

}

/** ****************************************************************************

Set element range

Sets a range of elements in a set, from low to high. Expects the low index and
high index, and the set to operate on.

*******************************************************************************/

boolean psystem_setrgs(
    /* set element low */  long l,
    /* set element high */ long h,
    /* set  */             settype s
)

{

    long i;

    for (i = 0; i < SETSIZE; i++) s[i] = 0;
    if (l > h) { i = l; l = h; h = i; }
    for (i = l; i <= h; i++) s[i/8] |= 1<<i%8;

}

/** ****************************************************************************

Initialize psystem support module

*******************************************************************************/

static void psystem_init (int argc, char* argv[]) __attribute__((constructor (110)));
static void psystem_init(int argc, char* argv[])

{

    int i;

    argc--; argv++; /* discard the program parameter */

    /* initialize file state */
    for (i = 1; i <= MAXFIL; i++) {
        filstate[i] = fsclosed; /* closed */
        filanamtab[i] = FALSE; /* no name assigned */
    }

    /* set status of standard files */
    filstate[INPUTFN] = fsread;
    filstate[OUTPUTFN] = fswrite;
    filstate[ERRORFN] = fswrite;
    filstate[LISTFN] = fswrite;
    filstate[COMMANDFN] = fsread;

    /* get the command line */
    getcommandline(argc, argv, cmdlin, &cmdlen);
    cmdpos = 1;

}

/** ****************************************************************************

Deinitialize psystem support module

*******************************************************************************/

static void psystem_deinit (void) __attribute__((destructor (110)));
static void psystem_deinit()

{

    int i;

    for (i = COMMANDFN+1; i <= MAXFIL; i++) if (filstate[i] != fsclosed) {
        fclose(filtable[i]);
        if (!filanamtab[i]) remove(filnamtab[i]);
    }

}