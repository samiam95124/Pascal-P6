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

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#ifndef DORECYCL
#define DORECYCL TRUE /* obey heap space recycle requests */
#endif

/* invoke a special recycle mode that creates single word entries on
  recycle of any object, breaking off and recycling the rest. Once
  allocated, each entry exists forever, and accesses to it can be
  checked. */
#ifndef DOCHKRPT
#define DOCHKRPT FALSE /* check reuse of freed entry (automatically
                           invokes dorecycl = false */
#endif

/*
 * Companion flag to DOCHKRPT: break returned blocks as occupied. not free.
 * This means that The entire disposed block will be kept out of circulation,
 * but the first word will be kept as a "disposed block" marker.
 *
 * This helps catch errors when references exist within blocks. This could
 * happen for several reasons, for example a with reference, a var reference,
 * or similar cached pointer. Thus this flag modifies DOCHKRPT to leave returned
 * space totally unused.
 */
#ifndef DONORECPAR
#define DONORECPAR FALSE /* do not recycle part of returned entries */
#endif

#ifndef DOCHKDEF
#define DOCHKDEF TRUE /* check undefined accesses */
#endif

#ifndef ISO7185
#define ISO7185 FALSE /* iso7185 standard flag */
#endif

/* assigned logical channels for header files */
#define INPUTFN   1 /* 'input' file no. */
#define OUTPUTFN  2 /* 'output' file no. */
#define ERRORFN   5 /* 'error' file no. */
#define LISTFN    6 /* 'list' file no. */
#define COMMANDFN 7 /* 'command' file no. */

#define MAXFIL       100  /* maximum number of general (temp) files */
#define FILLEN       2000 /* maximum length of filenames */
#define REALEF       9    /* real extra field in floating format -1.0e+000 */

typedef long boolean; /* true/false */
typedef char pasfil; /* Pascal file */
typedef enum {
  fsclosed,
  fsread,
  fswrite
} filsts;                      /* file states */
typedef long cmdinx;            /* index for command line buffer */
typedef long cmdnum;            /* length of command line buffer */
typedef char cmdbuf[MAXCMD];   /* buffer for command line */

static FILE* filtable[MAXFIL+1]; /* general file holders */
static filnam filnamtab[MAXFIL+1]; /* assigned name of files */
static boolean filanamtab[MAXFIL+1]; /* name has been assigned flags */
static filsts filstate[MAXFIL+1]; /* file state holding */
static boolean filbuff[MAXFIL+1]; /* file buffer full status */
static boolean fileoln[MAXFIL+1]; /* last file character read was eoln */
static boolean filbof[MAXFIL+1]; /* beginning of file */

cmdbuf  cmdlin;  /* command line */
cmdnum  cmdlen;  /* length of command line */
cmdinx  cmdpos;  /* current position in command line */

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
                finish(1);

            }
            cb[i++] = *p++;

        }
        argc--; argv++; /* advance */
        if (argc) { /* still more */

            if (i >= MAXCMD) {

                fprintf(stderr,
                        "*** Too many/too long command line parameters\n");
                finish(1);

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

char bufcommand(void)
{ return (cmdlin[cmdpos]); }

void getcommand(void)
{ if (cmdpos <= cmdlen) cmdpos = cmdpos+1; }

boolean eofcommand(void)
{ return (cmdpos > cmdlen+1); }

boolean eolncommand(void)
{ return (cmdpos >= cmdlen+1); }

void readlncommand(void)
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
        if (i >= FILLEN) errorv(FILENAMETOOLONG);
        filnamtab[fn][i] = bufcommand();
        getcommand();
        i = i+1;
    }
    if (i >= FILLEN) errorv(FILENAMETOOLONG);
    filnamtab[fn][i] = 0; /* terminate */
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
        if (ff == 0) errore(TOOMANYFILES);
        *f = ff;

    }

}

/** ****************************************************************************

Validate file for writing

Expects a Pascal file address. Validates the file is in write mode.

*******************************************************************************/

void valfilwm(
    /** Pascal file to validate */ pasfile f;
)

{

    valfil(f); /* validate file address */
    if (filstate[store[*f] != fswrite) error("File mode incorrect");

}

/** ****************************************************************************

Validate file for reading

Expects a Pascal file address. Validates the file is in read mode.

*******************************************************************************/

void valfilwm(
    /** Pascal file to validate */ pasfile f;
)

{

    valfil(f); /* validate file address */
    if (filstate[store[*f] != fsread) error("File mode incorrect");

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

        if (filstate[fn] != fsread) errore(FILEMODEINCORRECT);
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

*******************************************************************************/

void getfn(filnum fn)
{
    if (fn <= COMMANDFN) switch (fn) {
        case INPUTFN:   getfneoln(stdin, INPUTFN); break;
        case PRDFN:     getfneoln(filtable[PRDFN], PRDFN); break;
        case OUTPUTFN: case PRRFN: case ERRORFN:
        case LISTFN:    errore(READONWRITEONLYFILE); break;
        case COMMANDFN: getcommand(); break;
    } else {
        if (filstate[fn] != fsread) errore(FILEMODEINCORRECT);
        getfneoln(filtable[fn], fn);
    }
}

/** ****************************************************************************

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
        } else errore(FILENOTOPEN);
    }
}

/** ****************************************************************************

*******************************************************************************/

boolean eoffn(filnum fn)
{
    boolean eof;

    if (fn <= COMMANDFN) switch (fn) {
        case INPUTFN:   eof = chkeoffn(stdin, INPUTFN); break;
        case OUTPUTFN:  eof = TRUE; break;
        case PRDFN:     eof = chkeoffn(filtable[PRDFN], PRDFN); break;
        case PRRFN:     eof = chkeoffn(filtable[PRRFN], PRRFN); break;
        case ERRORFN:   eof = TRUE; break;
        case LISTFN:    eof = TRUE; break;
        case COMMANDFN: eof = eofcommand(); break;
    } else eof = chkeoffn(filtable[fn], fn);

    return (eof);
}

/** ****************************************************************************

*******************************************************************************/

boolean chkeolnfn(FILE* fp, filnum fn)
{
    if ((eoffile(fp) && !fileoln[fn]) && !filbof[fn]) return (TRUE);
    else return (eolnfile(fp));
}

/** ****************************************************************************

*******************************************************************************/

boolean eolnfn(filnum fn)
{
    boolean eoln;

    if (fn <= COMMANDFN) switch (fn) {
        case INPUTFN:   eoln = chkeolnfn(stdin, INPUTFN); break;
        case PRDFN:     eoln = chkeolnfn(filtable[PRDFN], PRDFN); break;
        case PRRFN:     eoln = chkeolnfn(filtable[PRRFN], PRRFN); break;
        case ERRORFN: case OUTPUTFN:
        case LISTFN:    errore(FILEMODEINCORRECT); break;
        case COMMANDFN: eoln = eolncommand(); break;
    } else {
        if (filstate[fn] == fsclosed) errore(FILENOTOPEN);
        eoln = chkeolnfn(filtable[fn], fn);
    }

    return (eoln);
}

/** ****************************************************************************

*******************************************************************************/

void readline(filnum fn)
{
    while (!eolnfn(fn)) {
        if (eoffn(fn)) errore(ENDOFFILE);
        getfn(fn);
    }
    if (eolnfn(fn)) getfn(fn);
}

/** ****************************************************************************

*******************************************************************************/

char chkbuf(filnum fn, long w)
{ if (w > 0) return buffn(fn); else return(' '); }

/** ****************************************************************************

*******************************************************************************/

boolean chkend(filnum fn, long w)
{ return (w = 0 || eoffn(fn)); }

/** ****************************************************************************

*******************************************************************************/

void getbuf(filnum fn, long* w)
{
  if (*w > 0) {
    if (eoffn(fn)) errore(ENDOFFILE);
    getfn(fn); *w = *w-1;
  }
}

/** ****************************************************************************

*******************************************************************************/

void readi(filnum fn, long *i, long* w, boolean fld)
{
    long s;
    long d;

   s = +1; /* set sign */
   /* skip leading spaces */
   while (chkbuf(fn, *w) == ' ' && !chkend(fn, *w)) getbuf(fn, w);
   if (!(chkbuf(fn, *w) == '+' || chkbuf(fn, *w) == '-' ||
         isdigit(chkbuf(fn, *w)))) errore(INVALIDINTEGERFORMAT);
   if (chkbuf(fn, *w) == '+') getbuf(fn, w);
   else if (chkbuf(fn, *w) == '-') { getbuf(fn, w); s = -1; }
   if (!(isdigit(chkbuf(fn, *w))))
     errore(INVALIDINTEGERFORMAT);
   *i = 0; /* clear initial value */
   while (isdigit(chkbuf(fn, *w))) { /* parse digit */
     d = chkbuf(fn, *w)-'0';
     if (*i > INT_MAX/10 ||
         *i == INT_MAX/10 && d > INT_MAX%10)
       errore(INTEGERVALUEOVERFLOW);
     *i = *i*10+d; /* add in new digit */
     getbuf(fn, w);
   }
   *i = *i*s; /* place sign */
   /* if fielded, validate the rest of the field is blank */
   if (fld) while (!chkend(fn, *w)) {
     if (chkbuf(fn, *w) != ' ') errore(FIELDNOTBLANK);
     getbuf(fn, w);
   }
}

/** ****************************************************************************

*******************************************************************************/

/* find power of ten */
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
   if (!(isdigit(chkbuf(fn, w)))) errore(INVALIDREALNUMBER);
   while (isdigit(chkbuf(fn, w))) { /* parse digit */
      d = chkbuf(fn, w)-'0';
      *r = *r*10+d; /* add in new digit */
      getbuf(fn, &w);
   }
   if (chkbuf(fn, w) == '.' || tolower(chkbuf(fn, w)) == 'e') { /* it's a real */
      if (chkbuf(fn, w) == '.') { /* decimal point */
         getbuf(fn, &w); /* skip '.' */
         if (!(isdigit(chkbuf(fn, w)))) errore(INVALIDREALNUMBER);
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
               chkbuf(fn, w) == '-'))
            errore(INVALIDREALNUMBER);
         readi(fn, &i, &w, fld); /* get exponent */
         /* find with exponent */
         e = e+i;
      }
      if (e < 0) *r = *r/pwrten(e); else *r = *r*pwrten(e);
   }
   if (s) *r = -*r;
   /* if fielded, validate the rest of the field is blank */
   if (fld) while (!chkend(fn, w)) {
     if (chkbuf(fn, w) != ' ') errore(FIELDNOTBLANK);
     getbuf(fn, &w);
   }
}

/** ****************************************************************************

*******************************************************************************/

void readc(filnum fn, char* c, long w, boolean fld)
{
   *c = chkbuf(fn, w); getbuf(fn, &w);
   /* if fielded, validate the rest of the field is blank */
   if (fld) while (!chkend(fn, w)) {
     if (chkbuf(fn, w) != ' ') errore(FIELDNOTBLANK);
     getbuf(fn, &w);
   }
} /*readc*/

/** ****************************************************************************

*******************************************************************************/

void reads(filnum fn, address ad, long l, long w, boolean fld)
{
  long c;
  while (l > 0) {
    c = chkbuf(fn, w); getbuf(fn, &w); putchr(ad, c); ad = ad+1; l = l-1;
  }
  /* if fielded, validate the rest of the field is blank */
  if (fld) while (!chkend(fn, w)) {
    if (chkbuf(fn, w) != ' ') errore(FIELDNOTBLANK);
    getbuf(fn, &w);
  }
} /*reads*/

/** ****************************************************************************

*******************************************************************************/

void readsp(filnum fn, address ad,  long l)
{
  char c;

  while (l > 0 && !eolnfn(fn)) {
    if (eoffn(fn)) errore(ENDOFFILE);
    c = fgetc(filtable[fn]); putchr(ad, c); ad = ad+1; l = l-1;
  }
  while (l > 0) { putchr(ad, ' '); ad = ad+1; l = l-1; }
}

/** ****************************************************************************

*******************************************************************************/

void writestrp(FILE* f, address ad, long l)
{
    long i;
    address ad1;

    ad1 = ad+l-1; /* find end */
    while (l > 0 && getchr(ad1) == ' ')
           { ad1 = ad1-1; l = l-1; }
    for (i = 0; i < l; i++) fprintf(f, "%c", getchr(ad+i));
}

/** ****************************************************************************

*******************************************************************************/

void filllz(FILE* f, long n)
{ while (n > 0) { fputc('0', f); n--; } }

/** ****************************************************************************

*******************************************************************************/

/* Write integer */
void writei(FILE* f, long w, long fl, long r, long lz)
{

    long i, d, ds;
    char digit[MAXDBF];
    boolean sgn;

    if (w < 0) {

        sgn = TRUE; w = abs(w);
        if (r != 10) errore(NONDECIMALRADIXOFNEGATIVE) ;

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

Writes the given integer with give width, radix and zero fill, from the Pascal
file.

*******************************************************************************/

void writeipf(pasfil f, long w, long fl, long r, long lz)

{

    int fn;

    valfil(f);
    fn = *f;
    if (w < 1 && ISO7185) error("Invalid field specification");
    if (fn <= COMMANDFN) switch (fn) {

         case OUTPUTFN: writei(stdout, i, w, rd, lz); break;
         case ERRORFN: writei(stderr, i, w, rd, lz); break;
         case LISTFN: writei(stdout, i, w, rd, lz); break;
         case INPUTFN:
         case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        writei(filtable[fn], i, w, rd, lz);

    }

}


/** ****************************************************************************

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

void putfile(FILE* f, address ad, filnum fn)
{
    if (!filbuff[fn]) errore(FILEBUFFERVARIABLEUNDEFINED);
    fputc(getchr(ad+FILEIDSIZE), f);
    filbuff[fn] = FALSE;
} /*putfile*/

/** ****************************************************************************

*******************************************************************************/

void resetfn(filnum fn, boolean bin)
{
    /* file was closed, no assigned name, give it a temp name */
    if (filstate[fn] == fsclosed && !filanamtab[fn]) tmpnam(filnamtab[fn]);
    if (filstate[fn] != fsclosed)
        if (fclose(filtable[fn])) errore(FILECLOSEFAIL);
    if (!(filtable[fn] = fopen(filnamtab[fn], bin?"rb":"r")))
        errore(FILEOPENFAIL);
    filstate[fn] = fsread;
    filbuff[fn] = FALSE;
    fileoln[fn] = FALSE;
    filbof[fn] = FALSE;
}

/** ****************************************************************************

*******************************************************************************/

void rewritefn(filnum fn, boolean bin)
{
    /* file was closed, no assigned name, give it a temp name */
    if (filstate[fn] == fsclosed && !filanamtab[fn]) tmpnam(filnamtab[fn]);
    if (filstate[fn] != fsclosed)
        if (fclose(filtable[fn])) errore(FILECLOSEFAIL);
    if (!(filtable[fn] = fopen(filnamtab[fn], bin?"wb":"w")))
        errore(FILEOPENFAIL);
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
    /** Pascal file to read from */ pasfil* f,
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
    /* Pascal file to write to */ pasfil* f,
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: putfile(stdout, pasfil, fn); break;
        case ERRORFN: putfile(stderr, pasfil, fn); break;
        case LISTFN: putfile(stdout, pasfil, fn); break;
        case INPUTFN: case PRDFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) errore("File mode incorrect");
        putfile(filtable[fn], pasfil, fn);

    }

}

/** ****************************************************************************

Read line

Reads up to the next EOLN and discards any file contents before the EOLN.

*******************************************************************************/

void psystem_put(
    /* Pascal file to write to */ pasfil* f,
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {

        case INPUTFN: readline(INPUTFN); break;
        case PRDFN: readline(PRDFN); break;
        case OUTPUTFN: prrfn: errorfn:
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
    /** Address of pointer */ unsigned char** p;
    /** length to allocate */ unsigned long l
)

{

    *p = calloc(l); /* allocate */
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
    /** Address of pointer */  unsigned char** p;
    /** length to allocate */  unsigned long l
    /** pointer to tag list */ long* tl,
    /** number of tags */      unsigned long tc
)

{

    unsigned char* p2;
    long* lp;
    unsigned long* ulp;

    p2 = calloc(l+tc+sizeof(unsigned long)); /* get whole allocation */
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
    /* Pascal file to write to */ pasfil* fb 
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (fn <= COMMANDFN) switch (fn) {
       case OUTPUTFN: fprintf(stdout, "\n"); break;
       case PRRFN: fprintf(filtable[PRRFN], "\n"); break;
       case ERRORFN: fprintf(stderr, "\n"); break;
       case LISTFN: fprintf(filtable[LISTFN], "\n"); break;
       case PRDFN: case INPUTFN:
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
    /* String to write */         char    s[],
    /* Length of string */        int     l,
    /* Width of field */          int     w
)

{

    int fn;
 
    fn = *f; /* get logical file no. */

    if (w < 1 && ISO7185) error("Invalid field specification");
    if (l > w) l = w; /* limit string to field */
    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: fprintf(stdout, "%*.*s", w, l, s); break;
        case PRRFN: fprintf(filtable[PRRFN], "%*.*s", w, l, s); break;
        case ERRORFN: fprintf(stderr, "%*.*s", w, l, s); break;
        case LISTFN: fprintf(stdout, "%*.*s", w, l, s); break;
        case PRDFN: case INPUTFN:
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

void psystem_wrs(
    /* Pascal file to write to */ pasfil* f,
    /* String to write */         char    s[],
    /* Length of string */        int     l,
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

        if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
        writestrp(filtable[fn], ad1, l);

    }

}

/** ****************************************************************************

Find EOF of text file

Checks if the given Pascal text file is at EOF, and returns true if so.

*******************************************************************************/

boolean psystem_eof(
    /* Pascal file to write to */ pasfil* f,
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
    /* Pascal file to write to */ pasfil* f,
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

boolean psystem_efb(
    /* Pascal file to write to */ pasfil* f,
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

boolean psystem_wri(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w,
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

boolean psystem_wrih(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w,
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

boolean psystem_wrio(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w,
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

boolean psystem_wrib(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w,
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

boolean psystem_wiz(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w,
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

boolean psystem_wizh(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w,
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

boolean psystem_wizo(
    /* Pascal file to write to */ pasfil* f,
    /* Integer to write */        long    i,
    /* Field width */             long    w,
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

boolean psystem_wizb(
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

boolean psystem_wrr(
    /* Pascal file to write to */ pasfil* f,
    /* Real to write */           long    r,
    /* Field width */             long    w
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (w < 1) error("Invalid field specification");
    if (w < REALEF) w = REALEF; /* set minimum width */
    l = w-REALEF+1; /* assign leftover to fractional digits w/o sign */
    if (fn <= COMMANDFN) switch (fn) {

         case OUTPUTFN: fprintf(stdout, "%*.*e", w, l, r); break;
         case ERRORFN: fprintf(stderr, "%*.*e", w, l, r); break;
         case LISTFN: fprintf(stdout, "%*.*e", w, l, r); break;
         case INPUTFN:
         case COMMANDFN: error("Write on read only file");

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        fprintf(filtable[fn], "%*.*e", w, l, r);

    }

}

/** ****************************************************************************

Write character to text file

Writes out the given character, with the given field width.

*******************************************************************************/

boolean psystem_wrc(
    /* Pascal file to write to */ pasfil* f,
    /* Character to write */      char    c,
    /* Field width */             long    w
)

{

    int fn;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    if (w < 1 && ISO7185) errore("Invalid field specification");
    if (fn <= COMMANDFN) switch (fn) {

        case OUTPUTFN: fprintf(stdout, "%*c", w, c); break;
        case ERRORFN: fprintf(stderr, "%*c", w, c); break;
        case LISTFN: fprintf(stdout, "%*c", w, c); break;
        case INPUTFN:
        case COMMANDFN: error("Write on read only file"); break;

    } else {

        if (filstate[fn] != fswrite) error("File mode incorrect");
        fprintf(filtable[fn], "%*c", w, c);

    }

/** ****************************************************************************

Read integer from text file

Reads an integer from the given text file and returns it.

*******************************************************************************/

long psystem_rdi(
    /* Pascal file to write to */ pasfil* f,
)

{

    int fn;
    long i;
    long w;

    valfil(f); /* validate file */
    fn = *f; /* get logical file no. */

    w = INT_MAX;
    readi(fn, &i, &w, fld);

    return (i);

}
    case 11/*rdi*/:
    case 72/*rdif*/: w = INT_MAX; fld = q == 72; if (fld) popint(w);
                     popadr(ad1); popadr(ad); pshadr(ad);
                     valfil(ad); fn = store[ad]; readi(fn, &i, &w, fld);
                     putint(ad1, i);
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 37/*rib*/:
    case 71/*ribf*/: w = INT_MAX; fld = q == 71; popint(mx); popint(mn);
                    if (fld) popint(w); popadr(ad1); popadr(ad);
                    pshadr(ad); valfil(ad); fn = store[ad];
                    readi(fn, &i, &w, fld);
                    if (i < mn || i > mx) errore(VALUEOUTOFRANGE);
                    putint(ad1, i);
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 12/*rdr*/:
    case 73/*rdrf*/: w = INT_MAX; fld = q == 73; if (fld) popint(w);
                    popadr(ad1); popadr(ad); pshadr(ad);
                    valfil(ad); fn = store[ad];
                    readr(fn, &r, w, fld); putrel(ad1, r);
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 13/*rdc*/:
    case 75/*rdcf*/: w = INT_MAX; fld = q == 75; if (fld) popint(w);
                    popadr(ad1); popadr(ad); pshadr(ad);
                    valfil(ad); fn = store[ad];
                    readc(fn, &c, w, fld); putchr(ad1, c);
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 38/*rcb*/:
    case 74/*rcbf*/: w = INT_MAX; fld = q == 74; popint(mx); popint(mn);
                     if (fld) popint(w); popadr(ad1); popadr(ad);
                     pshadr(ad); valfil(ad);
                     fn = store[ad];
                     readc(fn, &c, w, fld);
                     if (c < mn || c > mx) errore(VALUEOUTOFRANGE);
                     putchr(ad1, c);
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 14/*sin*/: poprel(r1); pshrel(sin(r1)); break;

/** ****************************************************************************

*******************************************************************************/

    case 15/*cos*/: poprel(r1); pshrel(cos(r1)); break;

/** ****************************************************************************

*******************************************************************************/

    case 16/*exp*/: poprel(r1); pshrel(exp(r1)); break;

/** ****************************************************************************

*******************************************************************************/

    case 17/*log*/: poprel(r1); if (r1 <= 0) errore(INVALIDARGUMENTTOLN);
                    pshrel(log(r1)); break;

/** ****************************************************************************

*******************************************************************************/

    case 18/*sqt*/: poprel(r1); if (r1 < 0) errore(INVALIDARGUMENTTOSQRT);
                    pshrel(sqrt(r1)); break;

/** ****************************************************************************

*******************************************************************************/

    case 19/*atn*/: poprel(r1); pshrel(atan(r1)); break;

/** ****************************************************************************

*******************************************************************************/

    /* placeholder for "mark" */
    case 20/*sav*/: errorv(INVALIDSTANDARDPROCEDUREORFUNCTION);

/** ****************************************************************************

*******************************************************************************/

    case 21/*pag*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                         case OUTPUTFN: fprintf(stdout, "\f"); break;
                         case PRRFN: fprintf(filtable[PRRFN], "\f"); break;
                         case ERRORFN: fprintf(stderr, "\f"); break;
                         case LISTFN: fprintf(stdout, "\f"); break;
                         case PRDFN: case INPUTFN:
                         case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                    } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         fprintf(filtable[fn], "\f");
                    }
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 22/*rsf*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                         case PRDFN: resetfn(PRDFN, FALSE); break;
                         case PRRFN: errore(CANNOTRESETWRITEONLYFILE); break;
                         case OUTPUTFN: case ERRORFN: case LISTFN: case INPUTFN:
                         case COMMANDFN:
                           errore(CANNOTRESETORREWRITESTANDARDFILE); break;
                    } else resetfn(fn, FALSE);
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 23/*rwf*/: popadr(ad); valfil(ad); fn = store[ad];
                    if (fn <= COMMANDFN) switch (fn) {
                         case PRRFN: rewritefn(PRRFN, FALSE); break;
                         case PRDFN: errore(CANNOTREWRITEREADONLYFILE); break;
                         case ERRORFN: case LISTFN: case OUTPUTFN: case INPUTFN:
                         case COMMANDFN:
                           errore(CANNOTRESETORREWRITESTANDARDFILE); break;
                    } else rewritefn(fn, FALSE);
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 24/*wrb*/: popint(w); popint(i); b = i != 0; popadr(ad);
                     pshadr(ad); valfil(ad); fn = store[ad];
                     if (w < 1) errore(INVALIDFIELDSPECIFICATION);
                     if (fn <= COMMANDFN) switch (fn) {
                          case OUTPUTFN: writeb(stdout, b, w); break;
                          case PRRFN: writeb(filtable[PRRFN], b, w); break;
                          case ERRORFN: writeb(stderr, b, w); break;
                          case LISTFN: writeb(stdout, b, w); break;
                          case PRDFN: case INPUTFN:
                          case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                     } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         writeb(filtable[fn], b, w);
                     }
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 25/*wrf*/: popint(f); popint(w); poprel(r); popadr(ad); pshadr(ad);
                     valfil(ad); fn = store[ad];
                     if (w < 1 && ISO7185) errore(INVALIDFIELDSPECIFICATION);
                     if (f < 1) errore(INVALIDFRACTIONSPECIFICATION);
                     if (fn <= COMMANDFN) switch (fn) {
                          case OUTPUTFN: fprintf(stdout, "%*.*f", (int)w, (int)f, r);
                                         break;
                          case PRRFN: fprintf(filtable[PRRFN], "%*.*f", (int)w, (int)f, r);
                                      break;
                          case ERRORFN: fprintf(stderr, "%*.*f", (int)w, (int)f, r);
                                        break;
                          case LISTFN: fprintf(stdout, "%*.*f", (int)w, (int)f, r); break;
                          case PRDFN: case INPUTFN:
                          case COMMANDFN: errore(WRITEONREADONLYFILE); break;
                     } else {
                         if (filstate[fn] != fswrite) errore(FILEMODEINCORRECT);
                         fprintf(filtable[fn], "%*.*f", (int)w, (int)f, r);
                     }
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 26/*dsp*/: popadr(ad1); popadr(ad);
                    if (varlap(ad, ad+ad1-1))
                      errorv(DISPOSEOFVARREFERENCEDBLOCK);
                    dspspc(ad1, ad); break;

/** ****************************************************************************

*******************************************************************************/

    case 40/*dsl*/: popadr(ad1); popint(i); /* get size of record and n tags */
                    /* add padding for zero tag case */
                    l = 0; if (i == 0 && DONORECPAR) l = 1;
                    ad = getadr(sp+i*INTSIZE); /* get rec addr */
                    j = i; /* save tag count */
                    /* under us is either the number of tags or the length of the block, if it
                      was freed. Either way, it is >= adrsize if not free */
                    if (getint(ad-INTSIZE) <= ADRSIZE)
                        errorv(BLOCKALREADYFREED);
                    if (i != getint(ad-INTSIZE)-ADRSIZE-1)
                        errorv(NEWDISPOSETAGSMISMATCH);
                    ad = ad-INTSIZE*2; ad2 = sp;
                    /* ad = top of tags in dynamic, ad2 = top of tags in
                      stack */
                    k = i;
                    while (k > 0)
                    {
                        if (getint(ad) != getint(ad2))
                            errorv(NEWDISPOSETAGSMISMATCH);
                        ad = ad-INTSIZE; ad2 = ad2+INTSIZE; k = k-1;
                    }
                    ad = ad+INTSIZE; ad1 = ad1+(i+1)*INTSIZE;
                    /* ajust for dummy */
                    ad = ad-(l*INTSIZE); ad1 = ad1+(l*INTSIZE);
                    if (varlap(ad, ad+ad1-1))
                      errorv(DISPOSEOFVARREFERENCEDBLOCK);
                    dspspc(ad1, ad);
                    while (i > 0) { popint(j); i = i-1; };
                    popadr(ad);
                    if (DONORECPAR) {
                        /* This flag means we are going to keep the entry, even
                         * after disposal. We place a dummy tag below the
                         * pointer just to indicate the space was freed.
                         */
                         putadr(ad-ADRSIZE, ADRSIZE);
                    }
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 27/*wbf*/: popint(l); popadr(ad1); popadr(ad); pshadr(ad);
                    valfilwm(ad); fn = store[ad];
                    for (i = 0; i < l; i++) {
                       chkdef(ad1); fputc(store[ad1], filtable[fn]);
                       ad1 = ad1+1;
                    }
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 28/*wbi*/: popint(i); popadr(ad); pshadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < INTSIZE; i++)
                        fputc(store[sp+i], filtable[fn]);
                     popint(i);
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 45/*wbx*/: popint(i); popadr(ad); pshadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     fputc(store[sp], filtable[fn]); popint(i);
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 29/*wbr*/: poprel(r); popadr(ad); pshadr(ad); pshrel(r);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < REALSIZE; i++)
                        fputc(store[sp+i], filtable[fn]);
                     poprel(r);
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 30/*wbc*/: popint(i); c = i; popadr(ad); pshadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < CHARSIZE; i++)
                        fputc(store[sp+i], filtable[fn]);
                     popint(i);
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 31/*wbb*/: popint(i); popadr(ad); pshadr(ad); pshint(i);
                     valfilwm(ad); fn = store[ad];
                     for (i = 0; i < BOOLSIZE; i++)
                         fputc(store[sp+i], filtable[fn]);
                     popint(i);
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 32/*rbf*/: popint(l); popadr(ad1); popadr(ad); pshadr(ad);
                     valfilrm(ad); fn = store[ad];
                     if (filbuff[fn]) /* buffer data exists */
                       for (i = 0; i < l; i++) {
                         store[ad1+i] = store[ad+FILEIDSIZE+i];
                         putdef(ad1+i, TRUE);
                       }
                     else
                       for (i = 0; i < l; i++) {
                         if (eoffile(filtable[fn])) errore(ENDOFFILE);
                         store[ad1] = fgetc(filtable[fn]);
                         putdef(ad1, TRUE);
                         ad1 = ad1+1;
                       }
                     break;

/** ****************************************************************************

*******************************************************************************/

    case 33/*rsb*/: popadr(ad); valfil(ad); fn = store[ad]; resetfn(fn, TRUE);
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 34/*rwb*/: popadr(ad); valfil(ad); fn = store[ad];
                    rewritefn(fn, TRUE); break;

/** ****************************************************************************

*******************************************************************************/

    case 35/*gbf*/: popint(i); popadr(ad); valfilrm(ad);
                    fn = store[ad];
                    if (varlap(ad+FILEIDSIZE, ad+FILEIDSIZE+i-1))
                        errorv(VARREFERENCEDFILEBUFFERMODIFIED);
                    if (filbuff[fn]) filbuff[fn] = FALSE;
                    else
                      for (j = 0; j < i; j++)
                         store[ad+FILEIDSIZE+j] = fgetc(filtable[fn]);
                    break;

/** ****************************************************************************

*******************************************************************************/

    case 36/*pbf*/: popint(i); popadr(ad); valfilwm(ad);
                 fn = store[ad];
                 if (!filbuff[fn]) errore(FILEBUFFERVARIABLEUNDEFINED);
                 for (j = 0; j < i; j++)
                   fputc(store[ad+FILEIDSIZE+j], filtable[fn]);
                 filbuff[fn] = FALSE;
                 break;

/** ****************************************************************************

*******************************************************************************/

    case 43 /*fbv*/: popadr(ad); pshadr(ad); valfil(ad);
                   fn = store[ad];
                   if (fn == INPUTFN) putchr(ad+FILEIDSIZE, buffn(INPUTFN));
                   else if (fn == PRDFN) putchr(ad+FILEIDSIZE, buffn(PRDFN));
                   else {
                     if (filstate[fn] == fsread)
                       putchr(ad+FILEIDSIZE, buffn(fn));
                   }
                   filbuff[fn] = TRUE;
                   break;

/** ****************************************************************************

*******************************************************************************/

    case 44 /*fvb*/: popint(i); popadr(ad); pshadr(ad); valfil(ad);
                   fn = store[ad];
                   /* load buffer only if in read mode, and buffer is
                     empty */
                   if (filstate[fn] == fsread && !filbuff[fn]) {
                       for (j = 0; j < i; j++) {
                         store[ad+FILEIDSIZE+j] = fgetc(filtable[fn]);
                         putdef(ad+FILEIDSIZE+j, TRUE);
                       }
                   }
                   filbuff[fn] = TRUE;
                   break;

/** ****************************************************************************

*******************************************************************************/

    case 46 /*asst*/:
    case 56 /*assb*/: popint(i); popadr(ad1); popadr(ad); valfil(ad);
                  fn = store[ad];
                  for (j = 0; j < i; j++) {
                    if (j >= FILLEN) errore(FILENAMETOOLONG);
                    filnamtab[fn][j] = store[ad1+j];
                  }
                  if (j >= FILLEN) errore(FILENAMETOOLONG);
                  filnamtab[fn][j] = 0;
                  filanamtab[fn] = TRUE; /* set name assigned */
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 47 /*clst*/:
    case 57 /*clsb*/: popadr(ad); valfil(ad); fn = store[ad];
                if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                /* if the file is temp, remove now */
                if (!filanamtab[fn]) remove(filnamtab[fn]);
                filanamtab[fn] = FALSE; /* break any name association */
                break;

/** ****************************************************************************

*******************************************************************************/

    case 48 /*pos*/: popint(i); popadr(ad); valfil(ad); fn = store[ad];
                if (i < 1) errore(INVALIDFILEPOSITION);
                if (fseek(filtable[fn], i-1, SEEK_SET)) errore(FILEPOSITIONFAIL);
                break;

/** ****************************************************************************

*******************************************************************************/

    case 49 /*upd*/: popadr(ad); valfil(ad); fn = store[ad];
                if (filstate[fn] == fsread) fseek(filtable[fn], 0, SEEK_SET);
                else {
                  if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                  if (!fopen(filnamtab[fn], "wb")) errore(FILEOPENFAIL);
                }
                break;

/** ****************************************************************************

*******************************************************************************/

    case 50 /*appt*/: popadr(ad); valfil(ad); fn = store[ad];
                if (filstate[fn] == fswrite) fseek(filtable[fn], 0, SEEK_END);
                else {
                  if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                  if (!fopen(filnamtab[fn], "w")) errore(FILEOPENFAIL);
                }
                break;

/** ****************************************************************************

*******************************************************************************/

    case 58 /*appb*/: popadr(ad); valfil(ad); fn = store[ad];
                if (filstate[fn] == fswrite) fseek(filtable[fn], 0, SEEK_END);
                else {
                  if (fclose(filtable[fn])) errorv(FILECLOSEFAIL);
                  if (!fopen(filnamtab[fn], "wb")) errore(FILEOPENFAIL);
                }
                break;

/** ****************************************************************************

*******************************************************************************/

    case 51 /*del*/: popint(i); popadr(ad1);
                  for (j = 0; j < i; i++) fl1[j] = store[ad1+j]; fl1[j] = 0;
                  i = remove(fl1); if (i) errorv(FILEDELETEFAIL);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 52 /*chg*/: popint(i); popadr(ad1); popint(l); popadr(ad);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j]; fl1[j] = 0;
                  for (j = 0; j < l; j++) fl2[j] = store[ad+j]; fl2[j] = 0;
                  if (rename(fl1, fl2)) errorv(FILENAMECHANGEFAIL);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 53 /*len*/: popadr(ad); valfil(ad); fn = store[ad];
                pshint(lengthfile(filtable[fn]));
                break;

/** ****************************************************************************

*******************************************************************************/

    case 54 /*loc*/: popadr(ad); valfil(ad); fn = store[ad];
                  if (i = ftell(filtable[fn]) < 0) errorv(FILEPOSITIONFAIL);
                  pshint(i+1);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 55 /*exs*/: popint(i); popadr(ad1);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j]; fl1[j] = 0;
                  if (fp = fopen(fl1, "r")) fclose(fp);
                  pshint(!!fp);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 59 /*hlt*/: finish(1); break;

/** ****************************************************************************

*******************************************************************************/

    case 60 /*ast*/: popint(i);
                  if (i == 0) errorv(PROGRAMCODEASSERTION);
                 break;

/** ****************************************************************************

*******************************************************************************/

    case 61 /*asts*/: popint(i); popadr(ad); popint(j);
                  if (j == 0) errors(ad, i);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 70/*rds*/:
    case 76/*rdsf*/: w = INT_MAX; fld = q == 76; popint(i);
                  if (fld) popint(w); popadr(ad1); popadr(ad);
                  pshadr(ad); valfil(ad); fn = store[ad];
                  reads(fn, ad1, i, w, fld);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 77/*rdsp*/: popint(i); popadr(ad1); popadr(ad); pshadr(ad);
                  valfil(ad); fn = store[ad];
                  readsp(fn, ad1, i);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 78/*aeft*/: popint(i); popadr(ad1); popadr(ad); valfil(ad);
                  fn = store[ad]; clrfn(fl1);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j];
                  assignexternal(fn, fl1);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 79/*aefb*/: popint(i); popadr(ad1); popadr(ad); valfil(ad);
                  fn = store[ad]; clrfn(fl1);
                  for (j = 0; j < i; j++) fl1[j] = store[ad1+j];
                  assignexternal(fn, fl1);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 80/*rdie*/: w = INT_MAX; popint(i); popadr(ad1); popadr(ad);
                  readi(COMMANDFN, &i, &w, FALSE); putint(ad, i);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 81/*rdre*/: w = INT_MAX; popint(i); popadr(ad1); popadr(ad);
                  readr(COMMANDFN, &r, w, FALSE); putrel(ad, r);
                  break;

/** ****************************************************************************

*******************************************************************************/

    case 2/*thw*/: popadr(ad1); mp = expmrk; sp = expstk;
                  pc = expadr; popadr(ad2); pshadr(ad1);
                  ep = getadr(mp+MARKET); /* get the mark ep */
                  /* release to search vectors */
                  break;

/** ****************************************************************************

Initialize psystem support module

*******************************************************************************/

static void psystem_init (int argc, char* argv[]) __attribute__((constructor (110)));
static void psystem_init(int argc, char* argv[])

{

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

static void psystem_deinit (void) __attribute__((destructor (110)));
static void psystem_deinit()

{

}