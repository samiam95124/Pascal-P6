/*******************************************************************************
*                                                                              *
*                                   SPEW TEST                                  *
*                                                                              *
*                                2005 S. A. Moore                              *
*                                                                              *
* Takes a file parameter. We walk though the given file, one character at a    *
* time, one line at a time, and knock out each character in the file with an   *
* alternate character, then parse that, placing the errors into a temp file.   *
* the error file is then inspected for error numbers and types, and the        *
* results tabulated, then output at the end of the program.                    *
*                                                                              *
* If parse returns "operator attention", then we abort, and print the line and *
* character which caused the error. The file "spewtest.pas" will have the      *
* file that caused the error. An operator attention error is either a problem  *
* with files, access or other run problem, or a system fault. All of these     *
* should be corrected immediately, so we stop.                                 *
*                                                                              *
* If the run completes, the top 10 errors are presented, with the highest      *
* count first to lowest last. These are the number of errors caused by just    *
* a one character source change, so its directly indicative of the quality of  *
* the parser's error recovery.                                                 *
*                                                                              *
* The top error entry is replaced back into the spewtest.pas file, this is     *
* for the convienence of being able to immediately test for the highest        *
* error count case.                                                            *
*                                                                              *
* Translated from Pascal 6/7/2018                                              *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>

#define ALTCHR '~' /* alternate test character */
#define ERRNO  10  /* number of top count errors to log (must be even) */
#define MAXLEN 250 /* maximum line length to process */

int lincnt; /* line we are testing on */
int chrcnt; /* character of testing line */
int lincnts; /* save for line */
int chrcnts; /* save for chr */
int  done; /* done with testing flag */
/* top error log, stores the top 10 errors by total spew or number of output
   errors */
typedef struct {

    int errcnt; /* number of errors */
    int errlin; /* line it occurred on */
    int errchr; /* character it occurred on */

} errrec;
errrec errlog[10];

/*******************************************************************************

Create temp file

Copies the source file to a temp file. When the line and character counts match
the one we are testing, we output the alternate character to the file, then
continue copying until we reach the end of the file. If we reach the end of the
file, and the error point is not encountered, then the done flag is set.

*******************************************************************************/

void createtemp(/* alternate character */ char ac,
                /* source filename */     char* sn)

{

    int c;     /* character buffer */
    int lc;    /* line counter */
    int cc;    /* character counter */
    int found; /* found replacement position */
    int fcc;   /* found line length */
    FILE* sfp; /* source file */
    FILE* dfp; /* destination file */

    lincnts = lincnt; /* save line and character */
    chrcnts = chrcnt;
    done = 0; /* set not done */
    found = 0; /* set not done */
    sfp = fopen(sn, "r");
    if (!sfp) {

        fprintf(stderr, "*** Cannot open source file %s\n", sn);
        exit(1);

    }
    dfp = fopen("spewtest.pas", "w");
    if (!dfp) {

        fprintf(stderr, "*** Cannot open output file\n");
        exit(1);

    }
    lc = 1; /* set line and character counters */
    cc = 1;
    fcc = 0; /* clear found line count */
    /* copy source file to temp file */
    do { /* until end of source */

        do { /* read source line */

            c = getc(sfp); /* get next source file character */
            if (c != EOF) {

                /* if we are at the test location, replace the character with the
                   alternate */
                if (lc == lincnt && cc == chrcnt && c != '\n') {

                    c = ac; /* replace character */
                    found = 1; /* flag replacement occurred */

                }
                putc(c, dfp); /* output to temp file */
                if (c != '\n') cc = cc+1; /* count characters */

            }

        } while (c != '\n' && c != EOF); /* until end of line */
        if (c != EOF) {

            /* if we found a line, then put the length of that line here */
            if (fcc == 0 && found) fcc = cc;
            lc = lc+1; /* count lines */
            cc = 1; /* reset characters */

        }

    } while (c != EOF);
    fclose(sfp); /* close files */
    fclose(dfp);
    /* advance character count */
    chrcnt = chrcnt+1;
    if (chrcnt >= fcc) { /* off end of line, go next line */

        lincnt = lincnt+1; /* count off lines */
        chrcnt = 1; /* reset character count */

    }
    /* check test counter off end of file */
    done = lincnt >= lc;

}

/********************************************************************************

Place error in error log

Finds the minimum count entry in the error log, and replaces that. If the new
error is not above that, it is discarded.

*******************************************************************************/

void logerr(/* Error count */      int err,
            /* Line number */      int lin,
            /* Character number */ int chr)

{

    /* error log index */         int ei;
    /* minimum error count */     int min;
    /* index for minimum entry */ int mi;

    min = INT_MAX; /* set no minimum */
    mi = 0;
    /* find minimum entry */
    for (ei = 0; ei < ERRNO; ei++) {

        if (errlog[ei].errcnt < min) {

            /* found an entry smaller than last, use it */
            min = errlog[ei].errcnt; /* save error count */
            mi = ei; /* save index */

        }

    }
    if (err > min) { /* if new error count > min error count */

        errlog[mi].errcnt = err; /* set error count */
        errlog[mi].errlin = lin; /* set line */
        errlog[mi].errchr = chr; /* set character */

    }

}

/********************************************************************************

Sort the error log

Just bubble sorts the error log. Speed is not a big issue here.

*******************************************************************************/

void srterr(void)

{

    errrec errsav; /* save for error log entry */
    int swap;      /* swap flag */
    int ei;        /* index for that */

    do { /* sort table */

        swap = 0; /* set no swap happened */
        ei = 0; /* set 1st entry */
        while (ei < ERRNO-1) { /* traverse the log */

            if (errlog[ei].errcnt < errlog[ei+1].errcnt) { /* swap */

                /* save this entry */
                memcpy(&errsav, &errlog[ei], sizeof(errrec));
                /* copy next to this */
                memcpy(&errlog[ei],  &errlog[ei+1], sizeof(errrec));
                /* place this to next */
                memcpy(&errlog[ei+1], &errsav, sizeof(errrec));
                swap = 1; /* set swap occurred */

            }
            ei = ei+1; /* skip to next pair */

        }

    } while (swap); /* until no swap occurred */

}

/*******************************************************************************

Analize error file

Reads the error output file, and analizes the errors it contains. The error
total line is found, and the number of errors taken from that. If no error total
line is found, then the compiler is assumed to have crashed.

Error totals are logged. Crashes terminate immediately.

*******************************************************************************/

void anaerr(void)

{

    FILE* fp; /* error file */
    char linbuf[MAXLEN]; /* line buffer */
    int ec; /* error count */
    int ef; /* error line found */
    char* r;

    fp = fopen("spewtest.err", "r");
    if (fp) {

        ec = 0; /* clear error count */
        ef = 0; /* set no error line found */
        do { /* get source lines */

            r = fgets(linbuf, MAXLEN, fp); /* get next line */
            if (r) { /* not EOF */

                /* see if this is the error count line */
                if (!strncmp(linbuf, "Errors in program: ", 19)) {

                    /* found, get error count and flag */
                    sscanf(linbuf, "Errors in program: %d", &ec);
                    ef = 1;

                }

            }

        } while (r); /* until EOF */
        fclose(fp);
        if (!ef) {

            printf("\n");
            printf("Compiler crashed: see spewtest.pas and spewtest.err for "
                   "details.\n");
            printf("Line: %d char: %d\n\n", lincnt, chrcnt);

        }

    }
    logerr(ec, lincnts, chrcnts); /* log the error stats */

}

/*******************************************************************************

Run test parse

We copy the source file to a temp file with the inserted error, then run a parse
on the temp file, and collect and tabulate the errors.

*******************************************************************************/

void testparse(/* Alternate character */ char ac,
               /* source filename */     char sn[])

{

    int err;


    createtemp(ac, sn); /* create temp file */
    if (!done) { /* not end of file */

        system("compile spewtest");
        anaerr(); /* do error analisys */

    }

}

/*******************************************************************************

Run program

Validate input, then run a series of tests by copying the source file into a
temp with an error inserted. The resulting error count is then logged, with the
top error producers by error count kept. We then print this list, and reproduce
the top error case in a compilable file.

*******************************************************************************/

void main(/* Input argument count */ int argc,
          /* Input argument array */ char *argv[])

{

    int ei;

    printf("\n");
    printf("Spew test vs. 1.0\n");
    printf("\n");
    /* clear error logging array */
    for (ei = 0; ei < ERRNO; ei++) {

        errlog[ei].errcnt = 0; /* clear error count */
        errlog[ei].errlin = 0; /* clear line number */
        errlog[ei].errchr = 0; /* clear character number */

    }
    if (argc != 2) {

        fprintf(stderr, "No source filename specified\n");
        exit(1);

    }
    printf("Testing with: %s\n", argv[1]);
    lincnt = 1; /* set 1st line and character */
    chrcnt = 1;
    done = 0; /* set not done */
    do { /* run test */

        printf("Testing: Line: %d Char: %d\n", lincnt, chrcnt);
        testparse(ALTCHR, argv[1]); /* with alternate character */

    } while (!done); /* until end of source file reached */

    srterr(); /* sort the error log */
    /* print out error log */
    printf("\n");
    printf("Error log (maximum first to minimum last)\n");
    printf("\n");
    for (ei = 0; ei < ERRNO; ei++) {

        if (errlog[ei].errcnt > 0)
            printf("Count: %d line: %d char: %d\n", errlog[ei].errcnt,
                   errlog[ei].errlin, errlog[ei].errchr);

    }
    printf("\n");
    /* reproduce the top error count for testing convience */
    if (errlog[0].errcnt > 0) {

        lincnt = errlog[0].errlin; /* set line */
        chrcnt = errlog[0].errchr; /* set character */
        createtemp(ALTCHR, argv[1]); /* reproduce the file */
        printf("The maximum error case has been reproduced in spewtest.pas\n");
        printf("\n");

    }

    printf("Function complete\n");

}
