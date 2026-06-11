/*******************************************************************************
*                                                                              *
*                    Network Library Support for Pascaline                     *
*                                                                              *
*                               S. A. FRANCO                                   *
*                                                                              *
* Hand-written wrappers for the network binding forms that carry C files or    *
* marshalled lists. The straight parameter wrappers are generated into         *
* network_wrapper.c.                                                           *
*                                                                              *
* This file, like the network library itself, is compiled against plain glibc  *
* stdio. The Pascaline runtime (psystem) runs in the Ami-stdio world, so a     *
* connection's glibc FILE cannot be planted in the Pascaline file table        *
* directly. The bridge crosses worlds at the file descriptor: the glibc        *
* FILE's descriptor is reopened as an Ami-stdio file (stdio_fdopen, the        *
* bypass-world fdopen), and that file is bound to the Pascaline file variable  *
* with psystem_libcatcfil. The glibc FILE is kept in a descriptor-indexed      *
* table so the certificate calls, which take the connection's C file, can     *
* find it again from the Pascaline file.                                       *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <network.h>

/* Pascaline file: a byte holding the logical file number */
typedef unsigned char* pfile;

/* pstring: length-prefixed Pascaline dynamic string (support.c layout) */
typedef struct { long len; char data[]; } pstrrec;

/* support.o helpers (string-only; safe across stdio worlds) */
extern void* cstr2pstr(char* cs, int l);

/* psystem runtime (Ami-stdio world) */
extern void  psystem_libcatcfil(pfile f, void* fp, long wr);
extern void* psystem_libcrdfil(pfile f);
extern void* psystem_libcwrfil(pfile f);

/* Ami-stdio world functions, by their real (bypass) names */
extern void* stdio_fdopen(int fd, const char* mode);
extern int   stdio_fileno(void* fp);

/* glibc FILE for a connection, indexed by its file descriptor, so the
   certificate calls can recover it from the Pascaline file */
#define MAXNETFIL 1024
static FILE* netfil[MAXNETFIL];

/* bind one connection (glibc FILE) to a Pascaline in/out file pair */
static void bindnet(pfile infile, pfile outfile, FILE* gf)
{
    int   fd;
    int   fd2;
    void* af;

    fd = fileno(gf); /* glibc descriptor of the connection */
    if (fd >= 0 && fd < MAXNETFIL) netfil[fd] = gf; /* save for cert calls */
    /* Reopen the descriptor in the Ami-stdio world, one file per side. The
       Ami stdio tracks one FILE per descriptor, so the write side gets a
       duplicate of the descriptor (which also gives each side independent
       close semantics). Both sides open in update mode: the socket
       descriptor is read-write, and the Ami fdopen verifies the requested
       mode against the descriptor's actual access mode. */
    af = stdio_fdopen(fd, "r+");
    psystem_libcatcfil(infile, af, 0);
    fd2 = dup(fd);
    if (fd2 >= 0 && fd2 < MAXNETFIL) netfil[fd2] = gf; /* both find the conn */
    af = stdio_fdopen(fd2, "r+");
    psystem_libcatcfil(outfile, af, 1);
}

/* recover the connection's glibc FILE from a Pascaline file */
static FILE* netfile(pfile f)
{
    void* af;
    int   fd;

    af = psystem_libcrdfil(f); /* Ami-stdio file bound to the variable */
    fd = stdio_fileno(af); /* shared descriptor */
    if (fd < 0 || fd >= MAXNETFIL || !netfil[fd]) return (NULL);

    return (netfil[fd]);
}

void wrapper_opennet(pfile infile, pfile outfile, long addr, int port,
                     int secure)
{
    bindnet(infile, outfile,
            ami_opennet((unsigned long)addr, port, secure));
}

void wrapper_opennetv6(pfile infile, pfile outfile, long addrh, long addrl,
                       int port, int secure)
{
    bindnet(infile, outfile,
            ami_opennetv6((unsigned long long)addrh,
                          (unsigned long long)addrl, port, secure));
}

void wrapper_waitnet(pfile infile, pfile outfile, int port, int secure)
{
    bindnet(infile, outfile, ami_waitnet(port, secure));
}

int wrapper_certnet(pfile f, int which, char* cert, int certl)
{
    int r;

    r = ami_certnet(netfile(f), which, cert, certl);
    { int _p = 0; while (_p < certl && cert[_p]) _p++;
      while (_p < certl) cert[_p++] = ' '; }

    return (r);
}

/*
 * Certificate field lists: convert the C ami_certfield list to the Pascaline
 * certfield record list. The Pascaline layout -- validated against the
 * mangled stub names (pr$name$0$pvc$data$8$p12$critical$16$b$fork$20$p2$
 * next$28$p2$) -- is: name (pstring) @0, data (pstring) @8, critical
 * (boolean byte) @16, fork @20, next @28; pointers after the boolean align
 * to 4, not 8.
 */
#define PCF_NAME     0
#define PCF_DATA     8
#define PCF_CRITICAL 16
#define PCF_FORK     20
#define PCF_NEXT     28
#define PCF_SIZE     36

#define PCFP(p,off) (*(void**)((char*)(p)+(off)))
#define PCFC(p,off) (*(char*) ((char*)(p)+(off)))

static void* cvtcert(ami_certptr cp)
{
    void* head = NULL;
    void** tail = &head;
    void* pc;

    while (cp) {

        pc = calloc(1, PCF_SIZE);
        PCFP(pc, PCF_NAME) = cp->name ? cstr2pstr(cp->name, strlen(cp->name))
                                      : NULL;
        PCFP(pc, PCF_DATA) = cp->data ? cstr2pstr(cp->data, strlen(cp->data))
                                      : NULL;
        PCFC(pc, PCF_CRITICAL) = cp->critical != 0;
        PCFP(pc, PCF_FORK) = cvtcert(cp->fork);
        PCFP(pc, PCF_NEXT) = NULL;
        *tail = pc;
        tail = (void**)((char*)pc+PCF_NEXT);
        cp = cp->next;

    }

    return (head);
}

void wrapper_certlistnet(pfile f, int which, void** list)
{
    ami_certptr cl = NULL;

    ami_certlistnet(netfile(f), which, &cl);
    *list = cvtcert(cl);
    /* network.h declares ami_certlistfree but network.c does not implement
       it; the C list is left to the library until that lands */
}

void wrapper_certlistmsg(int fn, int which, void** list)
{
    ami_certptr cl = NULL;

    ami_certlistmsg(fn, which, &cl);
    *list = cvtcert(cl);
    /* see above: ami_certlistfree is not implemented in network.c */
}

/* free a Pascaline-side certificate list (the records and pstrings are
   allocated here with calloc/cstr2pstr, so they are freed here) */
static void frecert(void* pc)
{
    void* nx;

    while (pc) {

        nx = PCFP(pc, PCF_NEXT);
        free(PCFP(pc, PCF_NAME));
        free(PCFP(pc, PCF_DATA));
        frecert(PCFP(pc, PCF_FORK));
        free(pc);
        pc = nx;

    }
}

void wrapper_certlistfree(void** list)
{
    frecert(*list);
    *list = NULL;
}
