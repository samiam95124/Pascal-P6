/*
 * Translated from Pascaline by p2c
 * Source: iso7185pat
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>
#include "pas2csup.h"
#include "iso7185pat.h"

/* Pascal runtime helpers */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#include <stdbool.h>
#else
#define bool int
#define true 1
#define false 0
#endif
#define p2c_eoln(f) ({int c=getc(f);ungetc(c,f);c==10||c==EOF;})
#define p2c_readc(f,v) (fscanf(f,"%c",&v),(v=='\n'?(v=' '):0))

#define ccst 'v'
#define cone 1
#define doptrtortst false
#define mmaxint -9223372036854775807
#define rcnst  4.333000000000000e+01
#define rscst -8.422000000000000e+01
#define rscst2 -4.333000000000000e+01
#define rscst3  8.422000000000000e+01
#define scst 7.905050333459945e-323
#define tcnst 768
#define testfile true
#define tsncst -52
#define tsncst2 -768
#define tsncst3 52

enum enum_ {
    one,
    two,
    three,
    four,
    five,
    six,
    seven,
    eight,
    nine,
    ten
};

enum vart {
    vti,
    vtb,
    vtc,
    vte,
    vtes,
    vts,
    vtr,
    vtst,
    vta,
    vtrc,
    vtstc,
    vtp
};

enum _enum_1 {
    mon,
    tue,
    wed,
    thur,
    fri,
    sat,
    sun
};

typedef struct recs recs;
typedef struct rec rec;
typedef struct prec prec;
typedef struct recv recv;
typedef struct recvb recvb;
typedef struct recvc recvc;
typedef struct r_t r_t;
typedef struct vra_t vra_t;
typedef struct vvrs_t vvrs_t;
typedef struct vvrb_t vvrb_t;
typedef struct vvre_t vvre_t;
typedef struct vvres_t vvres_t;
typedef struct _rec_1 _rec_1;
typedef struct _rec_2 _rec_2;
typedef struct _rec_3 _rec_3;
typedef struct _rec_4 _rec_4;
typedef struct _rec_5 _rec_5;
typedef struct _rec_6 _rec_6;
typedef struct _rec_7 _rec_7;
typedef struct _rec_8 _rec_8;
typedef struct _rec_9 _rec_9;
typedef struct nvr_t nvr_t;
typedef struct rcast_t rcast_t;
typedef long* iptr;

struct recs {
    long a;
    char b;
};

struct rec {
    long i;
    bool b;
    char c;
    int e;
    unsigned char es;
    unsigned char s;
    double r;
    string10 st;
    arri a;
    recs rc;
    p2c_settype stc;
    iptr p;
};

struct prec {
    long i;
    bool b;
    char c;
    int e;
    unsigned char es;
    unsigned char s;
    double r;
    string10 st;
    arri a;
    recs rc;
    p2c_settype stc;
    iptr p;
};

struct recv {
    long a;
    char b;
    bool c;
    union {
        int e;
        string10 d;
    };
};

struct recvb {
    long i;
    bool b;
    union {
        bool q;
        union {
            bool n;
            double r;
        };
        char c;
    };
};

struct recvc {
    unsigned char vt;
    union {
        bool vb;
        long vi;
    };
};

struct r_t {
    long rx;
    char rc;
    long ry;
    bool rb;
    char rs[12];
};

struct vra_t {
    long i;
    int vt;
    union {
        struct {
            iptr vdp;
            long m;
        };
        struct {
            p2c_settype vdstc;
            long l;
        };
        struct {
            recs vdrc;
            long k;
        };
        struct {
            arri vda;
            long j;
        };
        struct {
            string10 vdst;
            long h;
        };
        struct {
            double vdr;
            long g;
        };
        struct {
            unsigned char vds;
            long f;
        };
        struct {
            unsigned char vdes;
            long e;
        };
        struct {
            int vde;
            long d;
        };
        struct {
            char vdc;
            long c;
        };
        struct {
            bool vdb;
            long b;
        };
        struct {
            long vdi;
            long a;
        };
    };
};

struct vvrs_t {
    unsigned char vt;
    union {
        bool vb;
        long vi;
    };
};

struct vvrb_t {
    bool vt;
    union {
        bool vb;
        long vi;
    };
};

struct vvre_t {
    int vt;
    union {
        bool vb;
        long vi;
    };
};

struct vvres_t {
    unsigned char vt;
    union {
        bool vb;
        long vi;
    };
};

struct _rec_1 {
    long i;
};

struct _rec_2 {
    long i;
    _rec_1 r;
};

struct _rec_3 {
    long i;
    _rec_2 r;
};

struct _rec_4 {
    long i;
    _rec_3 r;
};

struct _rec_5 {
    long i;
    _rec_4 r;
};

struct _rec_6 {
    long i;
    _rec_5 r;
};

struct _rec_7 {
    long i;
    _rec_6 r;
};

struct _rec_8 {
    long i;
    _rec_7 r;
};

struct _rec_9 {
    long i;
    _rec_8 r;
};

struct nvr_t {
    long i;
    _rec_9 r;
};

struct rcast_t {
    bool rcastt;
    union {
    };
};

typedef char string10[12];
typedef long arri[11];
typedef long arrim[3][3][3][3][3][3];
typedef recs arrr[11];

long a[11];
arri ai;
arrr ara;
rec arec;
long as;
bool avb[11];
char avc[12];
char avcs[11];
int ave[11];
unsigned char aves[11];
FILE* avf[11];
arri avi;
arri avi2;
unsigned char avis[11];
iptr avp[11];
double avr[11];
recs avrc[11];
p2c_settype avs[11];
bool ba;
bool bb;
bool bc;
long bia[2];
long bs;
char ca;
long car[123];
char cb;
char cc;
char ci;
long cia[256];
long cnt;
long cnt2;
long cs;
long csia[123];
p2c_settype csta;
p2c_settype cstb;
p2c_settype cstc;
p2c_settype cstd;
p2c_settype cste;
p2c_settype cstf;
p2c_settype cstg;
long da[11][11];
long ds;
int ea;
int ei;
long eia[10];
long es;
long esia[6];
p2c_file fa;
p2c_file fb;
p2c_file fc;
p2c_file fe;
p2c_file fes;
p2c_file fi;
p2c_file fp;
p2c_file fr;
p2c_file frc;
p2c_file fs;
p2c_file fst;
p2c_file fstc;
FILE* ft;
long gs;
long hs;
long i;
long* iap[101];
long intaliasv;
iptr ip;
long* ipa;
long* ipb;
long* ipc;
long* ipd;
long* ipe;
long iso7185pat;
arrim mdar;
arrim mdar2;
long myowninteger;
long myvar;
long myvarmyvar;
long myvarmyvarmyvar;
long myvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar;
long myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar;
long n;
nvr_t nvr;
prec parec;
bool pavb[11];
char pavc[12];
char pavcs[11];
int pave[11];
unsigned char paves[11];
FILE* pavf[11];
long pavi[11];
unsigned char pavis[11];
iptr pavp[11];
double pavr[11];
recs pavrc[11];
p2c_settype pavs[11];
long pbia[2];
long pcia[256];
long pcsia[123];
long peia[10];
long pesia[6];
p2c_file pfa;
p2c_file pfb;
p2c_file pfc;
p2c_file pfe;
p2c_file pfes;
p2c_file pfi;
p2c_file pfp;
p2c_file pfr;
p2c_file pfrc;
p2c_file pfs;
p2c_file pfst;
p2c_file pfstc;
long* pi1;
long* pi2;
arri* pta;
bool* ptb;
char* ptc;
int* pte;
unsigned char* ptes;
long* pti;
long* pti1;
iptr pti2;
iptr* ptp;
double* ptr;
recs* ptrc;
unsigned char* pts;
string10* ptst;
p2c_settype* ptstc;
long q;
r_t r;
double ra;
double rb;
double rc;
rcast_t rcast;
long rcastt;
double rd;
double re;
long rn;
long rndseq;
rec* rpa;
recvb* rpb;
recvc* rpc;
string10 s;
char sa[12];
char sar[11][12];
char sb[12];
p2c_settype sba;
p2c_settype sbb;
p2c_settype sbc;
p2c_settype sbd;
p2c_settype sbe;
p2c_settype sbf;
p2c_settype sbg;
char sc[12];
p2c_settype sena;
p2c_settype senb;
p2c_settype senc;
p2c_settype send;
p2c_settype sene;
p2c_settype senf;
p2c_settype seng;
signed char sras;
signed char srbs;
signed char srcs;
signed char srds;
signed char sres;
unsigned char srx;
unsigned char sry;
unsigned char srz;
p2c_settype sta;
p2c_settype stb;
p2c_settype stc;
p2c_settype std;
p2c_settype ste;
p2c_settype stf;
p2c_settype stg;
int sva;
int svb;
int svc;
long t;
long vnum;
vra_t vra;
recv vrec;
vvrb_t vvrb;
vvre_t vvre;
vvres_t vvres;
vvrs_t vvrs;
long x;
long y;
long z;

jmp_buf _jmpenv_7;
jmp_buf _jmpenv_6;
jmp_buf _jmpenv_5;
jmp_buf _jmpenv_4;

void junk1(long z,long q)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    printf("%ld %ld", (long)(z), (long)(q));

}

void junk2(long* z)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    (*z) = (*z) + 1;

}

void junk3(string10 p)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    printf("%10s", &p[1]);

}

void junk4(string10 p)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    p[5] = '?';
    printf("%10s", &p[1]);

}

long junk5(long x)
{

    typedef long* iptr;

    long junk5__result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    junk5__result = x + 1;

    return junk5__result;
}

void junk6(void)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    longjmp(_jmpenv_6, 1);

}

long junk7(long a,long b,long c)
{

    typedef long* iptr;

    long x;
    long y;
    long z;
    long junk7__result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    x = 1;
    y = 2;
    z = 3;
    printf("%ld %ld %ld ", (long)(a), (long)(b), (long)(c));
    a = 4;
    b = 5;
    c = 6;
    printf("%ld %ld %ld %ld %ld %ld", (long)(c), (long)(b), (long)(a), (long)(z), (long)(y), (long)(x));
    junk7__result = 78;

    return junk7__result;
}

void junk8(long a,bool b,char c,int e,unsigned char es,unsigned char s,
           double r,string10 st,arri ar,rec rc,recv rv,p2c_settype stc,
           iptr p)
{

    typedef long* iptr;

    char ci;
    long i;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    printf("%ld %5s %c %ld %ld %ld %15g %10s\n", (long)(a), (b) ? "true" : "false", (int)(c), (long)(e), (long)(es), (long)(s), r, &st[1]);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        printf("%ld ", (long)(ar[i]));

    }
    }
    printf("\n");
    printf("%ld %5s %c %ld %ld %ld %15g %10s\n", (long)(rc.i), (rc.b) ? "true" : "false", (int)(rc.c), (long)(rc.e), (long)(rc.es), (long)(rc.s), rc.r, &rc.st[1]);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        printf("%ld ", (long)(rc.a[i]));

    }
    }
    printf("\n");
    printf("%ld %c\n", (long)(rc.rc.a), (int)(rc.rc.b));
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (p2c_sisin(ci, rc.stc)) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf("\n");
    printf("%ld\n", (long)(*rc.p));
    printf("%ld %c %5s\n", (long)(rv.a), (int)(rv.b), (rv.c) ? "true" : "false");
    if (rv.c) {

        printf("%ld\n", (long)(rv.e));

    } else {

        printf("%10s\n", &rv.d[1]);

    }
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (p2c_sisin(ci, stc)) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf("\n");
    printf("%ld\n", (long)(*p));

}

void junk9(void (*junk9)(long, long, char),long (*y)(long))
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    junk9(9834, 8383, 'j');
    printf(" %ld", (long)(y(743)));

}

void junk10(long x,long y,char junk10)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    printf("%ld %ld %c", (long)(x), (long)(y), (int)(junk10));

}

long junk11(long x)
{

    typedef long* iptr;

    long junk11__result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    junk11__result = (x + 1);

    return junk11__result;
}

void junk12(void (*xq)(long (*yq)(long)),long (*q)(long))
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    xq(q);

}

void junk13(long (*xz)(long))
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    printf("%ld", (long)(xz(941)));

}
static void junk14_junk15();
typedef long* iptr;


static void junk14_junk15(long* i__up, long* x__up)
{

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    printf("%ld %ld", (long)((*i__up)), (long)((*x__up)));

}

void junk14(void)
{

    typedef long* iptr;

    long i;
    long x;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    i = 62;
    x = 76;
    junk14_junk15(&i, &x);

}

void junk16(void)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

}
static void junk17_junk18();
typedef long* iptr;


static void junk17_junk18(long* i__up)
{

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    printf("%ld", (long)((*i__up)));

}

void junk17(void (*x)(void),long i)
{

    typedef long* iptr;

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    x();
    if (i == 52) {

        junk17((void (*)(void))junk17_junk18, 83);

    }

}

void junk19(void)
{

    typedef long* iptr;
    typedef char* junk19_pt;

    junk19_pt p;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    p = malloc(sizeof(char));
    *p = 'a';
    printf("%c", (int)(*p));
    free(p);

}
static long junk20_inner();
typedef long* iptr;
typedef char* junk19_pt;


static long junk20_inner(long* junk20__result__up)
{

    long junk20_inner__result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    junk20_inner__result = 12;
    (*junk20__result__up) = 37;

    return junk20_inner__result;
}

long junk20(void)
{

    typedef long* iptr;
    typedef char* junk19_pt;

    long i;
    long junk20__result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    i = junk20_inner(&junk20__result);

    return junk20__result;
}

iptr frp(void)
{

    typedef long* iptr;
    typedef char* junk19_pt;

    iptr frp__result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    frp__result = pti2;

    return frp__result;
}

long random_(long low,long hi)
{

    typedef long* iptr;
    typedef char* junk19_pt;

    long gamma;
    long random___result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    gamma = 16807 * (rndseq % (2147483647 / 16807)) - (2147483647 % 16807) * (rndseq / (2147483647 / 16807));
    if (gamma > 0) {

        rndseq = gamma;

    } else {

        rndseq = gamma + 2147483647;

    }
    random___result = rndseq / (9223372036854775807 / (hi - low + 1)) + low;

    return random___result;
} 

long junk21(void)
{

    typedef long* iptr;
    typedef char* junk19_pt;

    unsigned char abs;
    unsigned char arctan;
    unsigned char boolean;
    unsigned char chr;
    unsigned char cos;
    unsigned char dispose;
    unsigned char eof;
    unsigned char eoln;
    unsigned char exp;
    unsigned char false_;
    unsigned char get;
    unsigned char ln;
    unsigned char new;
    unsigned char odd;
    unsigned char ord;
    unsigned char pack;
    unsigned char page;
    unsigned char pred;
    unsigned char put;
    unsigned char read;
    unsigned char readln;
    unsigned char real;
    unsigned char reset;
    unsigned char rewrite;
    unsigned char round;
    unsigned char sin;
    unsigned char sqr;
    unsigned char sqrt;
    unsigned char succ;
    unsigned char text;
    unsigned char true_;
    unsigned char trunc;
    unsigned char unpack;
    unsigned char write;
    unsigned char writeln;
    long junk21__result;
    p2c_settype __attribute__((unused)) _stmp1, _stmp2;

    true_ = 1;
    false_ = 1;
    real = 1;
    boolean = 1;
    text = 1;
    abs = 1;
    sqr = 1;
    sqrt = 1;
    sin = 1;
    cos = 1;
    arctan = 1;
    ln = 1;
    exp = 1;
    trunc = 1;
    round = 1;
    ord = 1;
    chr = 1;
    succ = 1;
    pred = 1;
    odd = 1;
    eoln = 1;
    eof = 1;
    read = 1;
    readln = 1;
    write = 1;
    writeln = 1;
    rewrite = 1;
    reset = 1;
    put = 1;
    get = 1;
    page = 1;
    new = 1;
    dispose = 1;
    pack = 1;
    unpack = 1;
    junk21__result = true_ + false_ + real + boolean + text + abs + sqr + sqrt + sin + cos + arctan + ln + exp + trunc + round + ord + chr + succ + pred + odd + eoln + eof + read + readln + write + writeln + rewrite + reset + put + get + page + new + dispose + pack + unpack;

    return junk21__result;
}
int main(void)
{

    p2c_settype __attribute__((unused)) _stmp1, _stmp2;
    if (setjmp(_jmpenv_6)) goto _l9999;
    printf("****************************************************************");
    printf("***************\n");
    printf("\n");
    printf("                 TEST SUITE FOR ISO 7185 PASCAL\n");
    printf("\n");
    printf("                 Copyright (C) 1995 S. A. Moore - All rights ");
    printf("reserved\n");
    printf("\n");
    printf("****************************************************************");
    printf("***************\n");
    printf("\n");

    a[1] = 1;
    esia[two] = 1;
    pesia[two] = 1;
    p2c_frewrite(&fes, sizeof(unsigned char));
    p2c_frewrite(&pfes, sizeof(unsigned char));
    p2c_frewrite(&fs, sizeof(unsigned char));
    p2c_frewrite(&pfs, sizeof(unsigned char));
    p2c_frewrite(&fr, sizeof(double));
    p2c_frewrite(&pfr, sizeof(double));
    p2c_frewrite(&fst, sizeof(string10));
    p2c_frewrite(&pfst, sizeof(string10));
    p2c_frewrite(&fa, sizeof(arri));
    p2c_frewrite(&pfa, sizeof(arri));
    p2c_frewrite(&frc, sizeof(recs));
    p2c_frewrite(&pfrc, sizeof(recs));
    p2c_frewrite(&fstc, sizeof(p2c_settype));
    p2c_frewrite(&pfstc, sizeof(p2c_settype));
    p2c_frewrite(&fp, sizeof(iptr));
    p2c_frewrite(&pfp, sizeof(iptr));
    rcastt = 1;
    rcast.rcastt = true;
    intaliasv = 1;

    printf("The following are implementation defined characteristics\n");
    printf("\n");
    printf("Maxint: %ld\n", (long)(9223372036854775807));
    i = 9223372036854775807;
    x = 0;
    while (i > 0) {

        i = i / 2;
        x = x + 1;

    }
    printf("Bit length of integer without sign bit appears to be: %ld\n", (long)(x));
    printf("Integer default output field\n");
    printf("         1111111111222222222233333333334\n");
    printf("1234567890123456789012345678901234567890\n");
    printf("%11ld\n", (long)(1));
    printf("Real default output field\n");
    printf("         1111111111222222222233333333334\n");
    printf("1234567890123456789012345678901234567890\n");
    printf("%22g\n", 1.199999999999999);
    printf("Note that the exponent character 'e' or 'E' is implementation\n");
    printf("defined as well as the number of exponent digits\n");
    printf("Boolean default output field\n");
    printf("         1111111111222222222233333333334\n");
    printf("1234567890123456789012345678901234567890\n");
    printf("%5s\n", (false) ? "true" : "false");
    printf("%5s\n", (true) ? "true" : "false");
    printf("Note that the upper or lower case state of the characters in\n");
    printf("'true' and 'false' are implementation defined\n");
    printf("Char default output field\n");
    printf("         1111111111222222222233333333334\n");
    printf("1234567890123456789012345678901234567890\n");
    printf("a\n");
    if (('a' == 97) && ('(' == 40) && ('^' == 94)) {

        printf("Appears to be ASCII\n");

    } else {

        printf("Appears to not be ASCII\n");

    }

    printf("\n");
    printf("******************* Control structures tests *******************\n");
    printf("\n");
    printf("Control1: ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        printf("%ld ", (long)(i));

    }
    }
    printf("s/b 1 2 3 4 5 6 7 8 9 10\n");
    printf("Control2: ");
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(i));

    }
    }
    printf("s/b 10 9 8 7 6 5 4 3 2 1\n");
    printf("Control3: ");
    i = 1;
    while (i <= 10) {

        printf("%ld ", (long)(i));
        i = i + 1; 

    }
    printf("s/b 1 2 3 4 5 6 7 8 9 10\n");
    printf("Control4: ");
    i = 1;
    do {

        printf("%ld ", (long)(i));
        i = i + 1;

    } while (!(i > 10));
    printf("s/b 1 2 3 4 5 6 7 8 9 10\n");
    printf("Control5: ");
    i = 1;
    _l0: ;
    printf("%ld ", (long)(i));
    i = i + 1;
    if (i <= 10) {

        goto _l0;

    }
    printf("s/b 1 2 3 4 5 6 7 8 9 10\n");
    printf("Control6: ");

    if (true) {

        printf("yes");

    } else {

        printf("no");

    }

    printf(" s/b yes\n");
    printf("Control7: ");
    if (false) {

        printf("no");

    } else {

        printf("yes");

    }
    printf(" s/b yes\n");
    printf("Control8: ");
    if (true) {

        printf("yes ");

    }
    printf("stop");
    printf(" s/b yes stop\n");
    printf("Control9: ");
    if (false) {

        printf("no ");

    }
    printf("stop");
    printf(" s/b stop\n");

    printf("Control10: ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        switch (i) {

        case 1:
            printf("one ");
            break;

        case 2:
            printf("two ");
            break;

        case 3:
            printf("three ");
            break;

        case 4:
            printf("four ");
            break;

        case 5:
            printf("five ");
            break;

        case 6:
            printf("six ");
            break;

        case 7:
            printf("seven ");
            break;

        case 8:
            printf("eight ");
            break;

        case 9:
        case 10:
            printf("nine-ten ");
            break;

        }

    }
    }
    printf("\n");
    printf("Control10: s/b ");
    printf("one two three four five ");
    printf("six seven eight nine-ten nine-ten\n");
    printf("Control11: start ");
    junk6();
    printf("!! BAD !!");
    _l9999: ;
    printf("stop s/b start stop\n");
    printf("Control12: start ");
    goto _l3;
    printf("!! BAD !!");
    _l3: ;
    printf("stop s/b start stop\n");
    printf("Control13: start ");

    i = 10;
    { long _forlim = i;
    for (i = 1; i <= _forlim; i++) {

        printf("%3ld", (long)(i));

    }
    }
    printf(" s/b start  1  2  3  4  5  6  7  8  9 10\n");
    printf("Control14: start ");

    i = 10;
    { long _forlim = 1;
    for (i = i; i >= _forlim; i--) {

        printf("%3ld", (long)(i));

    }
    }
    printf(" s/b start 10  9  8  7  6  5  4  3  2  1\n");
    printf("Control15: start ");

    { long _forlim = 9;
    for (i = 0; i <= _forlim; i++) {

        printf("%2ld", (long)(i));

    }
    }
    printf(" s/b start 0 1 2 3 4 5 6 7 8 9\n");
    printf("Control16: start ");

    { long _forlim = 0;
    for (i = 9; i >= _forlim; i--) {

        printf("%2ld", (long)(i));

    }
    }
    printf(" s/b start 9 8 7 6 5 4 3 2 1 0\n");

    printf("Control17: start ");
    i = 10000;
    switch (i) {

    case 1:

        printf("*** bad ***");
        break;

    case 10000:
        printf("good");
        break;

    }
    printf(" s/b start good\n");
    printf("Control18: start ");
    do {

        goto _l4;
        printf("!! BAD !!");
        _l4: ;
        printf("stop s/b start stop\n");
        i = 0;
        if (i != 0) {

            goto _l4;

        }

    } while (!(true));

    printf("\n");
    printf("******************* Integers *******************\n");
    printf("\n");

    x = 43;
    y = 78;
    z = y;
    printf("Integer1:   %ld s/b 121\n", (long)(x + y));
    printf("Integer2:   %ld s/b 35\n", (long)(y - x));
    printf("Integer3:   %ld s/b 3354\n", (long)(x * y));
    printf("Integer4:   %ld s/b 1\n", (long)(y / x));
    printf("Integer5:   %ld s/b 35\n", (long)(y % x));
    printf("Integer6:   %ld s/b 44\n", (long)((x + 1)));
    printf("Integer7:   %ld s/b 42\n", (long)((x - 1)));
    printf("Integer8:   %ld s/b 1849\n", (long)(((x)*(x))));
    printf("Integer9:   %c s/b N\n", (int)(y));
    printf("Integer10:  %ld s/b 43\n", (long)(x));
    printf("Integer11:  %5s s/b true\n", (((x) & 1)) ? "true" : "false");
    printf("Integer12:  %5s s/b false\n", (((y) & 1)) ? "true" : "false");
    printf("Integer13:  %5s s/b true\n", (z == y) ? "true" : "false");
    printf("Integer14:  %5s s/b false\n", (x == y) ? "true" : "false");
    printf("Integer15:  %5s s/b true\n", (x < y) ? "true" : "false");
    printf("Integer16:  %5s s/b false\n", (y < x) ? "true" : "false");
    printf("Integer17:  %5s s/b true\n", (y > x) ? "true" : "false");
    printf("Integer18:  %5s s/b false\n", (x > y) ? "true" : "false");
    printf("Integer19:  %5s s/b true\n", (x != y) ? "true" : "false");
    printf("Integer20:  %5s s/b false\n", (y != z) ? "true" : "false");
    printf("Integer21:  %5s s/b true\n", (x <= y) ? "true" : "false");
    printf("Integer22:  %5s s/b true\n", (z <= y) ? "true" : "false");
    printf("Integer23:  %5s s/b false\n", (y <= x) ? "true" : "false");
    printf("Integer24:  %5s s/b true\n", (y >= x) ? "true" : "false");
    printf("Integer25:  %5s s/b true\n", (y >= z) ? "true" : "false");
    printf("Integer26:  %5s s/b false\n", (x >= y) ? "true" : "false");

    printf("Integer27:  ");
    i = 546;
    printf("%ld s/b 546\n", (long)(i));
    printf("Integer28:  %ld s/b 90\n", (long)(56 + 34));
    printf("Integer29:  %ld s/b 22\n", (long)(56 - 34));
    printf("Integer30:  %ld s/b 1904\n", (long)(56 * 34));
    printf("Integer31:  %ld s/b 1\n", (long)(56 / 34));
    printf("Integer32:  %ld s/b 22\n", (long)(56 % 34));
    printf("Integer33:  %ld s/b 6\n", (long)((5 + 1)));
    printf("Integer34:  %ld s/b 4\n", (long)((5 - 1)));
    printf("Integer35:  %ld s/b 49\n", (long)(((7)*(7))));
    printf("Integer36:  %c s/b A\n", (int)(65));
    printf("Integer37:  %ld s/b 65\n", (long)(65));
    printf("Integer38:  %ld s/b 768\n", (long)(768));
    printf("Integer39:  %5s s/b true\n", (((5) & 1)) ? "true" : "false");
    printf("Integer40:  %5s s/b false\n", (((8) & 1)) ? "true" : "false");
    printf("Integer41:  %5s s/b true\n", (56 == 56) ? "true" : "false");
    printf("Integer42:  %5s s/b false\n", (56 == 57) ? "true" : "false");
    printf("Integer43:  %5s s/b true\n", (56 < 57) ? "true" : "false");
    printf("Integer44:  %5s s/b false\n", (57 < 56) ? "true" : "false");
    printf("Integer45:  %5s s/b true\n", (57 > 56) ? "true" : "false");
    printf("Integer46:  %5s s/b false\n", (56 > 57) ? "true" : "false");
    printf("Integer47:  %5s s/b true\n", (56 != 57) ? "true" : "false");
    printf("Integer48:  %5s s/b false\n", (56 != 56) ? "true" : "false");
    printf("Integer49:  %5s s/b true\n", (55 <= 500) ? "true" : "false");
    printf("Integer50:  %5s s/b true\n", (67 <= 67) ? "true" : "false");
    printf("Integer51:  %5s s/b false\n", (56 <= 33) ? "true" : "false");
    printf("Integer52:  %5s s/b true\n", (645 >= 4) ? "true" : "false");
    printf("Integer53:  %5s s/b true\n", (23 >= 23) ? "true" : "false");
    printf("Integer54:  %5s s/b false\n", (45 >= 123) ? "true" : "false");

    as = - 14;
    bs = - 32;
    cs = - 14;
    ds = 20;
    es = - 15;
    gs = 9223372036854775807;
    hs = -9223372036854775807;
    vnum = - 9223372036854775807;
    printf("Integer55:  %ld s/b 6\n", (long)(as + ds));
    printf("Integer56:  %ld s/b 6\n", (long)(ds + as));
    printf("Integer57:  %ld s/b -12\n", (long)(bs + ds));
    printf("Integer58:  %ld s/b -46\n", (long)(as + bs));
    printf("Integer59:  %ld s/b 34\n", (long)(ds - as));
    printf("Integer60:  %ld s/b -52\n", (long)(bs - ds));
    printf("Integer61:  %ld s/b -18\n", (long)(bs - as));
    printf("Integer62:  %ld s/b -280\n", (long)(ds * as));
    printf("Integer63:  %ld s/b -280\n", (long)(as * ds));
    printf("Integer64:  %ld s/b 448\n", (long)(as * bs));
    printf("Integer65:  %ld s/b -1\n", (long)(ds / as));
    printf("Integer66:  %ld s/b -1\n", (long)(bs / ds));
    printf("Integer67:  %ld s/b 2\n", (long)(bs / as));
    printf("Integer68:  %ld s/b -13\n", (long)((as + 1)));
    printf("Integer69:  %ld s/b -33\n", (long)((bs - 1)));
    printf("Integer70: %ld s/b 196\n", (long)(((as)*(as))));
    printf("Integer71:  %5s s/b false\n", (((as) & 1)) ? "true" : "false");
    printf("Integer72:  %5s s/b true\n", (((es) & 1)) ? "true" : "false");
    printf("Integer73:  %5s s/b true\n", (as == cs) ? "true" : "false");
    printf("Integer74:  %5s s/b false\n", (as == bs) ? "true" : "false");
    printf("Integer75:  %5s s/b true\n", (as != bs) ? "true" : "false");
    printf("Integer76:  %5s s/b false\n", (as != cs) ? "true" : "false");
    printf("Integer77:  %5s s/b true\n", (as < ds) ? "true" : "false");
    printf("Integer78:  %5s s/b true\n", (bs < as) ? "true" : "false");
    printf("Integer79:  %5s s/b false\n", (ds < as) ? "true" : "false");
    printf("Integer80:  %5s s/b false\n", (as < bs) ? "true" : "false");
    printf("Integer81:  %5s s/b true\n", (ds > as) ? "true" : "false");
    printf("Integer82:  %5s s/b true\n", (as > bs) ? "true" : "false");
    printf("Integer83:  %5s s/b false\n", (as > ds) ? "true" : "false");
    printf("Integer84:  %5s s/b false\n", (bs > as) ? "true" : "false");
    printf("Integer85:  %5s s/b true\n", (as <= ds) ? "true" : "false");
    printf("Integer86:  %5s s/b true\n", (bs <= as) ? "true" : "false");
    printf("Integer87:  %5s s/b true\n", (as <= cs) ? "true" : "false");
    printf("Integer88:  %5s s/b false\n", (ds <= as) ? "true" : "false");
    printf("Integer89:  %5s s/b false\n", (as <= bs) ? "true" : "false");
    printf("Integer90:  %5s s/b true\n", (ds >= as) ? "true" : "false");
    printf("Integer91:  %5s s/b true\n", (as >= bs) ? "true" : "false");
    printf("Integer92:  %5s s/b true\n", (as >= cs) ? "true" : "false");
    printf("Integer93:  %5s s/b false\n", (as >= ds) ? "true" : "false");
    printf("Integer94:  %5s s/b false\n", (bs >= as) ? "true" : "false");
    printf("Integer95:  %ld s/b 14\n", (long)(labs(as)));
    printf("Integer96:  %ld s/b 0\n", (long)(gs + hs));
    printf("Integer97:  %ld s/b 0\n", (long)(gs - 9223372036854775807));
    printf("Integer98:  %ld s/b 0\n", (long)(gs + vnum));

    printf("Integer99:  %ld s/b 15\n", (long)(45 + (- 30)));
    printf("Integer100:  %ld s/b 45\n", (long)(- 25 + 70));
    printf("Integer101: %ld s/b -39\n", (long)(- 62 + 23));
    printf("Integer102: %ld s/b -35\n", (long)(- 20 + (- 15)));
    printf("Integer103: %ld s/b 34\n", (long)(20 - (- 14)));
    printf("Integer104: %ld s/b -48\n", (long)(- 34 - 14));
    printf("Integer105: %ld s/b -44\n", (long)(- 56 - (- 12)));
    printf("Integer106: %ld s/b -20\n", (long)(5 * (- 4)));
    printf("Integer107: %ld s/b -126\n", (long)((- 18) * 7));
    printf("Integer108: %ld s/b 520\n", (long)((- 40) * (- 13)));
    printf("Integer109: %ld s/b -6\n", (long)(30 / (- 5)));
    printf("Integer110: %ld s/b -25\n", (long)((- 50) / 2));
    printf("Integer111: %ld s/b 5\n", (long)((- 20) / (- 4)));
    printf("Integer112: %ld s/b -9\n", (long)((- 10 + 1)));
    printf("Integer113: %ld s/b 0\n", (long)((- 1 + 1)));
    printf("Integer114: %ld s/b -2\n", (long)((- 1 - 1)));
    printf("Integer115: %ld s/b 64\n", (long)(((- 8)*(- 8))));
    printf("Integer116: %ld s/b -55\n", (long)((- 54 - 1)));
    printf("Integer117: %5s s/b false\n", (((- 20) & 1)) ? "true" : "false");
    printf("Integer118: %5s s/b true\n", (((- 15) & 1)) ? "true" : "false");
    printf("Integer119: %5s s/b true\n", (- 5 == - 5) ? "true" : "false");
    printf("Integer120: %5s s/b false\n", (- 5 == 5) ? "true" : "false");
    printf("Integer121: %5s s/b true\n", (- 21 != - 40) ? "true" : "false");
    printf("Integer122: %5s s/b false\n", (- 21 != - 21) ? "true" : "false");
    printf("Integer123: %5s s/b true\n", (- 3 < 5) ? "true" : "false");
    printf("Integer124: %5s s/b true\n", (- 32 < - 20) ? "true" : "false");
    printf("Integer125: %5s s/b false\n", (20 < - 20) ? "true" : "false");
    printf("Integer126: %5s s/b false\n", (- 15 < - 40) ? "true" : "false");
    printf("Integer127: %5s s/b true\n", (70 > - 4) ? "true" : "false");
    printf("Integer128: %5s s/b true\n", (- 23 > - 34) ? "true" : "false");
    printf("Integer129: %5s s/b false\n", (- 5 > 5) ? "true" : "false");
    printf("Integer130: %5s s/b false\n", (- 60 > - 59) ? "true" : "false");
    printf("Integer131: %5s s/b true\n", (- 12 <= 4) ? "true" : "false");
    printf("Integer132: %5s s/b true\n", (- 14 <= - 5) ? "true" : "false");
    printf("Integer133: %5s s/b true\n", (- 7 <= - 7) ? "true" : "false");
    printf("Integer134: %5s s/b false\n", (5 <= - 5) ? "true" : "false");
    printf("Integer135: %5s s/b false\n", (- 10 <= - 20) ? "true" : "false");
    printf("Integer136: %5s s/b true\n", (9 >= - 3) ? "true" : "false");
    printf("Integer137: %5s s/b true\n", (- 4 >= - 10) ? "true" : "false");
    printf("Integer138: %5s s/b true\n", (- 13 >= - 13) ? "true" : "false");
    printf("Integer139: %5s s/b false\n", (- 6 >= 6) ? "true" : "false");
    printf("Integer140: %5s s/b false\n", (- 20 >= - 10) ? "true" : "false");
    printf("Integer141: %ld s/b 6\n", (long)(labs(- 6)));
    printf("Integer142: %ld s/b -52\n", (long)(-52));
    printf("Integer143: %ld s/b 52\n", (long)(- -52));
    printf("Integer144: %ld s/b -768\n", (long)(-768));
    printf("Integer145: %ld s/b 52\n", (long)(52));
    printf("Integer146: %ld s/b 0\n", (long)(9223372036854775807 + -9223372036854775807));

    myowninteger = 42;
    printf("Integer147: %ld s/b 42\n", (long)(myowninteger));
    myvar = 1;
    myvarmyvar = 2;
    myvarmyvarmyvar = 3;
    myvarmyvarmyvarmyvar = 4;
    myvarmyvarmyvarmyvarmyvar = 5;
    myvarmyvarmyvarmyvarmyvarmyvar = 6;
    myvarmyvarmyvarmyvarmyvarmyvarmyvar = 7;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar = 8;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar = 9;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar = 10;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar = 11;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar = 12;
    myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar = 13;
    printf("Integer148: %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld s/b 1 2 3 4 5 6 7 8 9 10 11 12 13\n", (long)(myvar), (long)(myvarmyvar), (long)(myvarmyvarmyvar), (long)(myvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar), (long)(myvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvarmyvar));

    printf("\n");
    printf("******************* Subranges *******************\n");
    printf("\n");

    srx = 43;
    sry = 78;
    srz = sry;
    printf("Subrange1:   %ld s/b 121\n", (long)(srx + sry));
    printf("Subrange2:   %ld s/b 35\n", (long)(sry - srx));
    printf("Subrange3:   %ld s/b 3354\n", (long)(srx * sry));
    printf("Subrange4:   %ld s/b 1\n", (long)(sry / srx));
    printf("Subrange5:   %ld s/b 35\n", (long)(sry % srx));
    printf("Subrange6:   %ld s/b 44\n", (long)((srx + 1)));
    printf("Subrange7:   %ld s/b 42\n", (long)((srx - 1)));
    printf("Subrange8:   %c s/b N\n", (int)(sry));
    printf("Subrange9:   %ld s/b 43\n", (long)(srx));
    printf("Subrange10:  %5s s/b true\n", (((srx) & 1)) ? "true" : "false");
    printf("Subrange11:  %5s s/b false\n", (((sry) & 1)) ? "true" : "false");
    printf("Subrange12:  %5s s/b true\n", (srz == sry) ? "true" : "false");
    printf("Subrange13:  %5s s/b false\n", (srx == sry) ? "true" : "false");
    printf("Subrange14:  %5s s/b true\n", (srx < sry) ? "true" : "false");
    printf("Subrange15:  %5s s/b false\n", (sry < srx) ? "true" : "false");
    printf("Subrange16:  %5s s/b true\n", (sry > srx) ? "true" : "false");
    printf("Subrange17:  %5s s/b false\n", (srx > sry) ? "true" : "false");
    printf("Subrange18:  %5s s/b true\n", (srx != sry) ? "true" : "false");
    printf("Subrange19:  %5s s/b false\n", (sry != srz) ? "true" : "false");
    printf("Subrange20:  %5s s/b true\n", (srx <= sry) ? "true" : "false");
    printf("Subrange21:  %5s s/b true\n", (srz <= sry) ? "true" : "false");
    printf("Subrange22:  %5s s/b false\n", (sry <= srx) ? "true" : "false");
    printf("Subrange23:  %5s s/b true\n", (sry >= srx) ? "true" : "false");
    printf("Subrange24:  %5s s/b true\n", (sry >= srz) ? "true" : "false");
    printf("Subrange25:  %5s s/b false\n", (srx >= sry) ? "true" : "false");

    sras = - 14;
    srbs = - 32;
    srcs = - 14;
    srds = 20;
    sres = - 15;
    printf("Subrange26:  %ld s/b 6\n", (long)(sras + srds));
    printf("Subrange27:  %ld s/b 6\n", (long)(srds + sras));
    printf("Subrange28:  %ld s/b -12\n", (long)(srbs + srds));
    printf("Subrange29:  %ld s/b -46\n", (long)(sras + srbs));
    printf("Subrange30:  %ld s/b 34\n", (long)(srds - sras));
    printf("Subrange31:  %ld s/b -52\n", (long)(srbs - srds));
    printf("Subrange32:  %ld s/b -18\n", (long)(srbs - sras));
    printf("Subrange33:  %ld s/b -280\n", (long)(srds * sras));
    printf("Subrange34:  %ld s/b -280\n", (long)(sras * srds));
    printf("Subrange35:  %ld s/b 448\n", (long)(sras * srbs));
    printf("Subrange36:  %ld s/b -1\n", (long)(srds / sras));
    printf("Subrange37:  %ld s/b -1\n", (long)(srbs / srds));
    printf("Subrange38:  %ld s/b 2\n", (long)(srbs / sras));
    printf("Subrange39:  %ld s/b -13\n", (long)((sras + 1)));
    printf("Subrange40:  %ld s/b -33\n", (long)((srbs - 1)));
    printf("Subrange41:  %5s s/b false\n", (((sras) & 1)) ? "true" : "false");
    printf("Subrange42:  %5s s/b true\n", (((sres) & 1)) ? "true" : "false");
    printf("Subrange43:  %5s s/b true\n", (sras == srcs) ? "true" : "false");
    printf("Subrange44:  %5s s/b false\n", (sras == srbs) ? "true" : "false");
    printf("Subrange45:  %5s s/b true\n", (sras != srbs) ? "true" : "false");
    printf("Subrange46:  %5s s/b false\n", (sras != srcs) ? "true" : "false");
    printf("Subrange47:  %5s s/b true\n", (sras < srds) ? "true" : "false");
    printf("Subrange48:  %5s s/b true\n", (srbs < sras) ? "true" : "false");
    printf("Subrange49:  %5s s/b false\n", (srds < sras) ? "true" : "false");
    printf("Subrange50:  %5s s/b false\n", (sras < srbs) ? "true" : "false");
    printf("Subrange51:  %5s s/b true\n", (srds > sras) ? "true" : "false");
    printf("Subrange52:  %5s s/b true\n", (sras > srbs) ? "true" : "false");
    printf("Subrange53:  %5s s/b false\n", (sras > srds) ? "true" : "false");
    printf("Subrange54:  %5s s/b false\n", (srbs > sras) ? "true" : "false");
    printf("Subrange55:  %5s s/b true\n", (sras <= srds) ? "true" : "false");
    printf("Subrange56:  %5s s/b true\n", (srbs <= sras) ? "true" : "false");
    printf("Subrange57:  %5s s/b true\n", (sras <= srcs) ? "true" : "false");
    printf("Subrange58:  %5s s/b false\n", (srds <= sras) ? "true" : "false");
    printf("Subrange59:  %5s s/b false\n", (sras <= srbs) ? "true" : "false");
    printf("Subrange60:  %5s s/b true\n", (srds >= sras) ? "true" : "false");
    printf("Subrange61:  %5s s/b true\n", (sras >= srbs) ? "true" : "false");
    printf("Subrange62:  %5s s/b true\n", (sras >= srcs) ? "true" : "false");
    printf("Subrange63:  %5s s/b false\n", (sras >= srds) ? "true" : "false");
    printf("Subrange64:  %5s s/b false\n", (srbs >= sras) ? "true" : "false");
    printf("Subrange65:  %ld s/b 14\n", (long)(labs(sras)));

    printf("\n");
    printf("******************* Characters*******************\n");
    printf("\n");

    ca = 'g';
    cb = 'g';
    cc = 'u';
    printf("Character1:   %c %c %c s/b g g u\n", (int)(ca), (int)(cb), (int)(cc));
    printf("Character2:   %c s/b h\n", (int)((ca + 1)));
    printf("Character3:   %c s/b f\n", (int)((cb - 1)));
    printf("Character4:   %ld s/b 103\n", (long)(ca));
    printf("Character5:   %c s/b u\n", (int)(cc));
    printf("Character6:   %5s s/b true\n", (ca == cb) ? "true" : "false");
    printf("Character7:   %5s s/b false\n", (ca == cc) ? "true" : "false");
    printf("Character8:   %5s s/b true\n", (ca < cc) ? "true" : "false");
    printf("Character9:   %5s s/b false\n", (cc < ca) ? "true" : "false");
    printf("Character10:  %5s s/b true\n", (cc > ca) ? "true" : "false");
    printf("Character11:  %5s s/b false\n", (ca > cc) ? "true" : "false");
    printf("Character12:  %5s s/b true\n", (ca != cc) ? "true" : "false");
    printf("Character13:  %5s s/b false\n", (ca != cb) ? "true" : "false");
    printf("Character14:  %5s s/b true\n", (ca <= cc) ? "true" : "false");
    printf("Character15:  %5s s/b true\n", (ca <= cb) ? "true" : "false");
    printf("Character16:  %5s s/b false\n", (cc <= ca) ? "true" : "false");
    printf("Character17:  %5s s/b true\n", (cc >= cb) ? "true" : "false");
    printf("Character18:  %5s s/b true\n", (cb >= ca) ? "true" : "false");
    printf("Character19:  %5s s/b false\n", (cb >= cc) ? "true" : "false");
    memmove(&sa[1],"porker    ", 10);
    memmove(&sb[1],"porker    ", 10);
    memmove(&sc[1],"parker    ", 10);
    printf("Character20:  %10s%10s%10s s/b porker    porker    parker\n", &sa[1], &sb[1], &sc[1]);
    printf("Character21:  %5s s/b true\n", (strncmp(&sa[1], &sb[1], 10) == 0) ? "true" : "false");
    printf("Character22:  %5s s/b false\n", (strncmp(&sa[1], &sc[1], 10) == 0) ? "true" : "false");
    printf("Character23:  %5s s/b true\n", (strncmp(&sc[1], &sa[1], 10) < 0) ? "true" : "false");
    printf("Character24:  %5s s/b false\n", (strncmp(&sa[1], &sc[1], 10) < 0) ? "true" : "false");
    printf("Character25:  %5s s/b true\n", (strncmp(&sa[1], &sc[1], 10) > 0) ? "true" : "false");
    printf("Character26:  %5s s/b false\n", (strncmp(&sc[1], &sa[1], 10) > 0) ? "true" : "false");
    printf("Character27:  %5s s/b true\n", (strncmp(&sa[1], &sc[1], 10) != 0) ? "true" : "false");
    printf("Character28:  %5s s/b false\n", (strncmp(&sa[1], &sb[1], 10) != 0) ? "true" : "false");
    printf("Character29:  %5s s/b true\n", (strncmp(&sc[1], &sa[1], 10) <= 0) ? "true" : "false");
    printf("Character30:  %5s s/b true\n", (strncmp(&sa[1], &sb[1], 10) <= 0) ? "true" : "false");
    printf("Character40:  %5s s/b false\n", (strncmp(&sa[1], &sc[1], 10) <= 0) ? "true" : "false");
    printf("Character41:  %5s s/b true\n", (strncmp(&sa[1], &sc[1], 10) >= 0) ? "true" : "false");
    printf("Character42:  %5s s/b true\n", (strncmp(&sa[1], &sb[1], 10) >= 0) ? "true" : "false");
    printf("Character43:  %5s s/b false\n", (strncmp(&sc[1], &sa[1], 10) >= 0) ? "true" : "false");
    printf("Character44:  ");
    { long _forlim = 'z';
    for (ca = 'a'; ca <= _forlim; ca++) {

        printf("%c", (int)(ca));
        if (ca == _forlim) break;

    }
    }
    printf(" s/b abcdefghijklmnopqrstuvwxyz\n");
    printf("Character45:  ");
    { long _forlim = 'a';
    for (ca = 'z'; ca >= _forlim; ca--) {

        printf("%c", (int)(ca));
        if (ca == _forlim) break;

    }
    }
    printf(" s/b zyxwvutsrqponmlkjihgfedcba\n");
    printf("Character46:  ");
    x = 0;
    { long _forlim = 'z';
    for (ca = 'a'; ca <= _forlim; ca++) {

        car[(unsigned char)ca] = x;
        x = x + 1;
        if (ca == _forlim) break;

    }
    }
    { long _forlim = 'a';
    for (ca = 'z'; ca >= _forlim; ca--) {

        printf("%ld ", (long)(car[(unsigned char)ca]));
        if (ca == _forlim) break;

    }
    }
    printf("\n");
    printf("Character46: s/b 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0\n");
    r.rc = 'n';
    printf("Character47: %c s/b n\n", (int)(r.rc));
    memmove(&r.rs[1],"junky01234", 10);
    printf("Character48: %10s s/b junky01234\n", &r.rs[1]);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        memmove(&sar[i][1],"0123456789", 10);

    }
    }
    memmove(&sar[1][1],"trash     ", 10);
    memmove(&sar[2][1],"finnork   ", 10);
    memmove(&sar[10][1],"crapola   ", 10);
    printf("Character49:  \n");
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%10s\n", &sar[i][1]);

    }
    }
    printf("Character49: s/b\n");
    printf("crapola\n");
    printf("0123456789\n");
    printf("0123456789\n");
    printf("0123456789\n");
    printf("0123456789\n");
    printf("0123456789\n");
    printf("0123456789\n");
    printf("0123456789\n");
    printf("finnork\n");
    printf("trash\n");
    printf("Character50:  \n");
    { long _forlim = '9';
    for (ca = '0'; ca <= _forlim; ca++) {

        switch (ca) {

        case '5':
            printf("five ");
            break;

        case '3':
            printf("three ");
            break;

        case '6':
            printf("six ");
            break;

        case '8':
            printf("eight ");
            break;

        case '0':
            printf("zero ");
            break;

        case '9':
            printf("nine ");
            break;

        case '7':
            printf("seven ");
            break;

        case '4':
            printf("four ");
            break;

        case '1':
            printf("one ");
            break;

        case '2':
            printf("two ");
            break;

        }
        if (ca == _forlim) break;

    }
    }
    printf("\n");
    printf(" s/b zero one two three four five six seven eight nine\n");

    printf("Character51:  a s/b a\n");
    printf("Character52:  %c s/b b\n", (int)(('a' + 1)));
    printf("Character53:  %c s/b y\n", (int)(('z' - 1)));
    printf("Character54:  %ld s/b 99\n", (long)('c'));
    printf("Character55:  %c s/b g\n", (int)('g'));
    printf("Character56:  %5s s/b true\n", ('q' == 'q') ? "true" : "false");
    printf("Character57:  %5s s/b false\n", ('r' == 'q') ? "true" : "false");
    printf("Character58:  %5s s/b true\n", ('b' < 't') ? "true" : "false");
    printf("Character59:  %5s s/b false\n", ('g' < 'c') ? "true" : "false");
    printf("Character60:  %5s s/b true\n", ('f' > 'e') ? "true" : "false");
    printf("Character61:  %5s s/b false\n", ('f' > 'g') ? "true" : "false");
    printf("Character62:  %5s s/b true\n", ('h' != 'l') ? "true" : "false");
    printf("Character63:  %5s s/b false\n", ('i' != 'i') ? "true" : "false");
    printf("Character64:  %5s s/b true\n", ('v' <= 'y') ? "true" : "false");
    printf("Character65:  %5s s/b true\n", ('y' <= 'y') ? "true" : "false");
    printf("Character66:  %5s s/b false\n", ('z' <= 'y') ? "true" : "false");
    printf("Character67:  %5s s/b true\n", ('l' >= 'b') ? "true" : "false");
    printf("Character68:  %5s s/b true\n", ('l' >= 'l') ? "true" : "false");
    printf("Character69:  %5s s/b false\n", ('l' >= 'm') ? "true" : "false");
    printf("Character70:  %5s s/b true\n", (strncmp("finnork", "finnork", 7) == 0) ? "true" : "false");
    printf("Character71:  %5s s/b false\n", (strncmp("finoork", "finnork", 7) == 0) ? "true" : "false");
    printf("Character72:  %5s s/b true\n", (strncmp("oliab", "olibb", 5) < 0) ? "true" : "false");
    printf("Character73:  %5s s/b false\n", (strncmp("olibb", "oliab", 5) < 0) ? "true" : "false");
    printf("Character74:  %5s s/b true\n", (strncmp("olibb", "oliab", 5) > 0) ? "true" : "false");
    printf("Character75:  %5s s/b false\n", (strncmp("oliab", "olibb", 5) > 0) ? "true" : "false");
    printf("Character76:  %5s s/b true\n", (strncmp("fark ", "farks", 5) != 0) ? "true" : "false");
    printf("Character77:  %5s s/b false\n", (strncmp("farks", "farks", 5) != 0) ? "true" : "false");
    printf("Character78:  %5s s/b true\n", (strncmp("farka", "farkz", 5) <= 0) ? "true" : "false");
    printf("Character79:  %5s s/b true\n", (strncmp("farks", "farks", 5) <= 0) ? "true" : "false");
    printf("Character80:  %5s s/b false\n", (strncmp("farkz", "farks", 5) <= 0) ? "true" : "false");
    printf("Character81:  %5s s/b true\n", (strncmp("topnat", "topcat", 6) >= 0) ? "true" : "false");
    printf("Character82:  %5s s/b true\n", (strncmp("topcat", "topcat", 6) >= 0) ? "true" : "false");
    printf("Character83:  %5s s/b false\n", (strncmp("topcat", "topzat", 6) >= 0) ? "true" : "false");
    printf("Character84:  this is a string s/b this is a string\n");
    printf("Character85:  v s/b v\n");
    printf("Character86:  \n");
    { long _forlim = 1;
    for (i = 15; i >= _forlim; i--) {

        printf("hello, world\n");

    }
    }
    printf("Character87:  s/b:\n");
    printf("   hello, world\n");
    printf("  hello, world\n");
    printf(" hello, world \n");
    printf("hello, world\n");
    printf("hello, worl\n");
    printf("hello, wor\n");
    printf("hello, wo\n");
    printf("hello, w\n");
    printf("hello, \n");
    printf("hello,\n");
    printf("hello\n");
    printf("hell\n");
    printf("hel\n");
    printf("he\n");
    printf("h\n");

    printf("Character88: \n");
    printf("%5s ", (('0' + 1) == '1') ? "true" : "false");
    printf("%5s ", (('1' + 1) == '2') ? "true" : "false");
    printf("%5s ", (('2' + 1) == '3') ? "true" : "false");
    printf("%5s ", (('3' + 1) == '4') ? "true" : "false");
    printf("%5s ", (('4' + 1) == '5') ? "true" : "false");
    printf("%5s ", (('5' + 1) == '6') ? "true" : "false");
    printf("%5s ", (('6' + 1) == '7') ? "true" : "false");
    printf("%5s ", (('7' + 1) == '8') ? "true" : "false");
    printf("%5s \n", (('8' + 1) == '9') ? "true" : "false");
    printf("s/b\n");
    printf(" true  true  true  true  true  true  true  true  true\n");

    printf("Character89:\n");
    printf("%5s ", ('a' < 'b') ? "true" : "false");
    printf("%5s ", ('b' < 'c') ? "true" : "false");
    printf("%5s ", ('c' < 'd') ? "true" : "false");
    printf("%5s ", ('d' < 'e') ? "true" : "false");
    printf("%5s ", ('e' < 'f') ? "true" : "false");
    printf("%5s ", ('f' < 'g') ? "true" : "false");
    printf("%5s ", ('g' < 'h') ? "true" : "false");
    printf("%5s ", ('h' < 'i') ? "true" : "false");
    printf("%5s ", ('i' < 'j') ? "true" : "false");
    printf("%5s \n", ('j' < 'k') ? "true" : "false");
    printf("%5s ", ('k' < 'l') ? "true" : "false");
    printf("%5s ", ('l' < 'm') ? "true" : "false");
    printf("%5s ", ('m' < 'n') ? "true" : "false");
    printf("%5s ", ('n' < 'o') ? "true" : "false");
    printf("%5s ", ('o' < 'p') ? "true" : "false");
    printf("%5s ", ('p' < 'q') ? "true" : "false");
    printf("%5s ", ('q' < 'r') ? "true" : "false");
    printf("%5s ", ('r' < 's') ? "true" : "false");
    printf("%5s ", ('s' < 't') ? "true" : "false");
    printf("%5s \n", ('t' < 'u') ? "true" : "false");
    printf("%5s ", ('u' < 'v') ? "true" : "false");
    printf("%5s ", ('v' < 'w') ? "true" : "false");
    printf("%5s ", ('w' < 'x') ? "true" : "false");
    printf("%5s ", ('x' < 'y') ? "true" : "false");
    printf("%5s \n", ('y' < 'z') ? "true" : "false");
    printf("s/b\n");
    printf(" true  true  true  true  true  true  true  true  true  true\n");
    printf(" true  true  true  true  true  true  true  true  true  true\n");
    printf(" true  true  true  true  true\n");
    printf("Character90:\n");
    printf("%5s ", ('A' < 'B') ? "true" : "false");
    printf("%5s ", ('B' < 'C') ? "true" : "false");
    printf("%5s ", ('C' < 'D') ? "true" : "false");
    printf("%5s ", ('D' < 'E') ? "true" : "false");
    printf("%5s ", ('E' < 'F') ? "true" : "false");
    printf("%5s ", ('F' < 'G') ? "true" : "false");
    printf("%5s ", ('G' < 'H') ? "true" : "false");
    printf("%5s ", ('H' < 'I') ? "true" : "false");
    printf("%5s ", ('I' < 'J') ? "true" : "false");
    printf("%5s \n", ('J' < 'K') ? "true" : "false");
    printf("%5s ", ('K' < 'L') ? "true" : "false");
    printf("%5s ", ('L' < 'M') ? "true" : "false");
    printf("%5s ", ('M' < 'N') ? "true" : "false");
    printf("%5s ", ('N' < 'O') ? "true" : "false");
    printf("%5s ", ('O' < 'P') ? "true" : "false");
    printf("%5s ", ('P' < 'Q') ? "true" : "false");
    printf("%5s ", ('Q' < 'R') ? "true" : "false");
    printf("%5s ", ('R' < 'S') ? "true" : "false");
    printf("%5s ", ('S' < 'T') ? "true" : "false");
    printf("%5s \n", ('T' < 'U') ? "true" : "false");
    printf("%5s ", ('U' < 'V') ? "true" : "false");
    printf("%5s ", ('V' < 'W') ? "true" : "false");
    printf("%5s ", ('W' < 'X') ? "true" : "false");
    printf("%5s ", ('X' < 'Y') ? "true" : "false");
    printf("%5s \n", ('Y' < 'Z') ? "true" : "false");
    printf("s/b\n");
    printf(" true  true  true  true  true  true  true  true  true  true\n");
    printf(" true  true  true  true  true  true  true  true  true  true\n");
    printf(" true  true  true  true  true\n");

    printf("\n");
    printf("******************* Booleans *******************\n");
    printf("\n");

    ba = true;
    bb = false;
    bc = true;
    printf("Boolean1:   %5s %5s s/b true false\n", (ba) ? "true" : "false", (bb) ? "true" : "false");
    printf("Boolean2:   %5s s/b true\n", ((bb + 1)) ? "true" : "false");
    printf("Boolean3:   %5s s/b false\n", ((ba - 1)) ? "true" : "false");
    printf("Boolean4:   %ld s/b 0\n", (long)(bb));
    printf("Boolean5:   %ld s/b 1\n", (long)(ba));
    printf("Boolean6:   %5s s/b true\n", (ba == bc) ? "true" : "false");
    printf("Boolean7:   %5s s/b true\n", (bb == bb) ? "true" : "false");
    printf("Boolean8:   %5s s/b false\n", (ba == bb) ? "true" : "false");
    printf("Boolean9:   %5s s/b true\n", (bb < ba) ? "true" : "false");
    printf("Boolean10:  %5s s/b false\n", (ba < bb) ? "true" : "false");
    printf("Boolean11:  %5s s/b true\n", (ba > bb) ? "true" : "false");
    printf("Boolean12:  %5s s/b false\n", (bb > ba) ? "true" : "false");
    printf("Boolean13:  %5s s/b true\n", (ba != bb) ? "true" : "false");
    printf("Boolean14:  %5s s/b false\n", (ba != bc) ? "true" : "false");
    printf("Boolean15:  %5s s/b true\n", (bb <= ba) ? "true" : "false");
    printf("Boolean16:  %5s s/b true\n", (ba <= bc) ? "true" : "false");
    printf("Boolean17:  %5s s/b false\n", (ba <= bb) ? "true" : "false");
    printf("Boolean18:  %5s s/b true\n", (ba >= bb) ? "true" : "false");
    printf("Boolean19:  %5s s/b true\n", (bb >= bb) ? "true" : "false");
    printf("Boolean20:  %5s s/b false\n", (bb >= ba) ? "true" : "false");
    printf("Boolean21:  ");
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        printf("%5s ", (ba) ? "true" : "false");
        if (ba == _forlim) break;

    }
    }
    printf("s/b false true\n");
    printf("Boolean22:  ");
    { long _forlim = false;
    for (bb = true; bb >= _forlim; bb--) {

        printf("%5s ", (bb) ? "true" : "false");
        if (bb == _forlim) break;

    }
    }
    printf("s/b true false\n");
    printf("Boolean23:  ");
    ba = 1 > 0;
    printf("%5s s/b true\n", (ba) ? "true" : "false");
    printf("Boolean24:  ");
    ba = 1 < 0;
    printf("%5s s/b false\n", (ba) ? "true" : "false");

    printf("Boolean25:  %5s %5s s/b true false\n", (true) ? "true" : "false", (false) ? "true" : "false");
    printf("Boolean26:  %5s s/b true\n", ((false + 1)) ? "true" : "false");
    printf("Boolean27:  %5s s/b false\n", ((true - 1)) ? "true" : "false");
    printf("Boolean28:  %ld s/b 0\n", (long)(false));
    printf("Boolean29:  %ld s/b 1\n", (long)(true));
    printf("Boolean30:  %5s s/b true\n", (true == true) ? "true" : "false");
    printf("Boolean31:  %5s s/b true\n", (false == false) ? "true" : "false");
    printf("Boolean32:  %5s s/b false\n", (true == false) ? "true" : "false");
    printf("Boolean33:  %5s s/b true\n", (false < true) ? "true" : "false");
    printf("Boolean34:  %5s s/b false\n", (true < false) ? "true" : "false");
    printf("Boolean35:  %5s s/b true\n", (true > false) ? "true" : "false");
    printf("Boolean36:  %5s s/b false\n", (false > true) ? "true" : "false");
    printf("Boolean37:  %5s s/b true\n", (true != false) ? "true" : "false");
    printf("Boolean38:  %5s s/b false\n", (true != true) ? "true" : "false");
    printf("Boolean39:  %5s s/b true\n", (false <= true) ? "true" : "false");
    printf("Boolean40:  %5s s/b true\n", (true <= true) ? "true" : "false");
    printf("Boolean41:  %5s s/b false\n", (true <= false) ? "true" : "false");
    printf("Boolean42:  %5s s/b true\n", (true >= false) ? "true" : "false");
    printf("Boolean43:  %5s s/b true\n", (false >= false) ? "true" : "false");
    printf("Boolean44:  %5s s/b false\n", (false >= true) ? "true" : "false");
    printf("Boolean45:\n");
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%*s\n", (int)(i), (false) ? "true" : "false");

    }
    }
    printf("Boolean45: s/b:\n");
    printf("     false\n");
    printf("    false\n");
    printf("   false\n");
    printf("  false\n");
    printf(" false\n");
    printf("false\n");
    printf("fals\n");
    printf("fal\n");
    printf("fa\n");
    printf("f\n");
    printf("Boolean46:\n");
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%*s\n", (int)(i), (true) ? "true" : "false");

    }
    }
    printf("Boolean46: s/b:\n");
    printf("      true\n");
    printf("     true\n");
    printf("    true\n");
    printf("   true\n");
    printf("  true\n");
    printf(" true\n");
    printf("true\n");
    printf("tru\n");
    printf("tr\n");
    printf("t\n");

    printf("\n");
    printf("******************* Scalar *******************\n");
    printf("\n");

    sva = wed;
    svb = mon;
    svc = wed;
    printf("Scalar1:   %5s s/b true\n", ((svb + 1) == tue) ? "true" : "false");
    printf("Scalar2:   %5s s/b true\n", ((sva - 1) == tue) ? "true" : "false");
    printf("Scalar3:   %ld s/b 0\n", (long)(svb));
    printf("Scalar4:   %ld s/b 2\n", (long)(sva));
    printf("Scalar5:   %5s s/b true\n", (sva == svc) ? "true" : "false");
    printf("Scalar6:   %5s s/b true\n", (svb == svb) ? "true" : "false");
    printf("Scalar7:   %5s s/b false\n", (sva == svb) ? "true" : "false");
    printf("Scalar8:   %5s s/b true\n", (svb < sva) ? "true" : "false");
    printf("Scalar9:   %5s s/b false\n", (sva < svb) ? "true" : "false");
    printf("Scalar10:  %5s s/b true\n", (sva > svb) ? "true" : "false");
    printf("Scalar11:  %5s s/b false\n", (svb > sva) ? "true" : "false");
    printf("Scalar12:  %5s s/b true\n", (sva != svb) ? "true" : "false");
    printf("Scalar13:  %5s s/b false\n", (sva != svc) ? "true" : "false");
    printf("Scalar14:  %5s s/b true\n", (svb <= sva) ? "true" : "false");
    printf("Scalar15:  %5s s/b true\n", (sva <= svc) ? "true" : "false");
    printf("Scalar16:  %5s s/b false\n", (sva <= svb) ? "true" : "false");
    printf("Scalar17:  %5s s/b true\n", (sva >= svb) ? "true" : "false");
    printf("Scalar18:  %5s s/b true\n", (svb >= svb) ? "true" : "false");
    printf("Scalar19:  %5s s/b false\n", (svb >= sva) ? "true" : "false");
    printf("Scalar20:  ");
    { long _forlim = sun;
    for (sva = mon; sva <= _forlim; sva++) {

        printf("%ld ", (long)(sva));

    }
    }
    printf("s/b 0 1 2 3 4 5 6\n");
    printf("Scalar21:  ");
    { long _forlim = mon;
    for (svb = sun; svb >= _forlim; svb--) {

        printf("%ld ", (long)(svb));

    }
    }
    printf("s/b 6 5 4 3 2 1 0\n");

    printf("Scalar20:   %5s s/b true\n", ((mon + 1) == tue) ? "true" : "false");
    printf("Scalar21:   %5s s/b true\n", ((fri - 1) == thur) ? "true" : "false");
    printf("Scalar22:   %ld s/b 2\n", (long)(wed));
    printf("Scalar23:   %ld s/b 6\n", (long)(sun));
    printf("Scalar24:   %5s s/b true\n", (thur == thur) ? "true" : "false");
    printf("Scalar25:   %5s s/b true\n", (fri == fri) ? "true" : "false");
    printf("Scalar26:   %5s s/b false\n", (tue == wed) ? "true" : "false");
    printf("Scalar27:   %5s s/b true\n", (mon < wed) ? "true" : "false");
    printf("Scalar28:   %5s s/b false\n", (fri < fri) ? "true" : "false");
    printf("Scalar29:  %5s s/b true\n", (sun > sat) ? "true" : "false");
    printf("Scalar30:  %5s s/b false\n", (fri > sun) ? "true" : "false");
    printf("Scalar31:  %5s s/b true\n", (thur != tue) ? "true" : "false");
    printf("Scalar32:  %5s s/b false\n", (wed != wed) ? "true" : "false");
    printf("Scalar33:  %5s s/b true\n", (mon <= fri) ? "true" : "false");
    printf("Scalar34:  %5s s/b true\n", (fri <= fri) ? "true" : "false");
    printf("Scalar35:  %5s s/b false\n", (sat <= fri) ? "true" : "false");
    printf("Scalar36:  %5s s/b true\n", (fri >= tue) ? "true" : "false");
    printf("Scalar37:  %5s s/b true\n", (tue >= tue) ? "true" : "false");
    printf("Scalar38:  %5s s/b false\n", (tue >= sat) ? "true" : "false");

    printf("\n");
    printf("******************* Reals ******************************\n");
    printf("\n");

    printf("Real1:   %15g s/b  1.554000e+00\n", 1.554);
    printf("Real2:   %15g s/b  3.340000e-03\n", 3.339999999999999e-3);
    printf("Real3:   %15g s/b  3.340000e-24\n", 3.34e-24);
    printf("Real4:   %15g s/b  4.000000e-45\n", 4.0e-45);
    printf("Real5:   %15g s/b -5.565000e+00\n", - 5.565);
    printf("Real6:   %15g s/b -9.440000e-03\n", - 9.440000000000001e-3);
    printf("Real7:   %15g s/b -6.364000e+29\n", - 6.363999999999998e+29);
    printf("Real8:   %15g s/b -2.000000e-14\n", - 2.0e-14);
    printf("Real9:\n");
    printf("         11111111112222222222333333333344444444445\n");
    printf("12345678901234567890123456789012345678901234567890\n");
    { long _forlim = 14;
    for (i = 1; i <= _forlim; i++) {

        printf("%*g\n", (int)(i), 1.234567890123456);

    }
    }
    printf("s/b (note precision dropoff at right):\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.2e+000\n");
    printf(" 1.23e+000\n");
    printf(" 1.234e+000\n");
    printf(" 1.2345e+000\n");
    printf(" 1.23456e+000\n");
    printf("Real10:\n");
    printf("         11111111112222222222333333333344444444445\n");
    printf("12345678901234567890123456789012345678901234567890\n");
    { long _forlim = 14;
    for (i = 1; i <= _forlim; i++) {

        printf("%.*f\n", (int)(i), i + 2.345678901234568e-1);

    }
    }
    printf("s/b (note precision dropoff at right):\n");
    printf("1.2\n");
    printf("2.23\n");
    printf("3.234\n");
    printf("4.2345\n");
    printf("5.23456\n");
    printf("6.234567\n");
    printf("7.2345678\n");
    printf("8.23456789\n");
    printf("9.234567890\n");
    printf("10.2345678901\n");
    printf("11.23456789012\n");
    printf("12.234567890123\n");
    printf("13.2345678901234\n");
    printf("14.23456789012345\n");

    ra = 4.3523e+2;
    rb = 9.836699999999998e+2;
    rc = rb;
    rd = 3.443e-1;
    printf("Real11:  %15g s/b  1.418900e+03\n", ra + rb);
    printf("Rea112:  %15g s/b  5.484399e+02\n", rb - ra);
    printf("Real13:  %15g s/b  4.281227e+05\n", ra * rb);
    printf("Real14:  %15g s/b  2.260115e+00\n", rb / ra);
    printf("Real15:  %5s s/b true\n", (rc == rb) ? "true" : "false");
    printf("Real16:  %5s s/b false\n", (ra == rb) ? "true" : "false");
    printf("Real17:  %5s s/b true\n", (ra < rb) ? "true" : "false");
    printf("Real18:  %5s s/b false\n", (rb < ra) ? "true" : "false");
    printf("Real19:  %5s s/b true\n", (rb > ra) ? "true" : "false");
    printf("Real20:  %5s s/b false\n", (ra > rb) ? "true" : "false");
    printf("Real21:  %5s s/b true\n", (ra != rb) ? "true" : "false");
    printf("Real22:  %5s s/b false\n", (rb != rc) ? "true" : "false");
    printf("Real23:  %5s s/b true\n", (ra <= rb) ? "true" : "false");
    printf("Real24:  %5s s/b true\n", (rc <= rb) ? "true" : "false");
    printf("Real25:  %5s s/b false\n", (rb <= ra) ? "true" : "false");
    printf("Real26:  %5s s/b true\n", (rb >= ra) ? "true" : "false");
    printf("Real27:  %5s s/b true\n", (rb >= rc) ? "true" : "false");
    printf("Real28:  %5s s/b false\n", (ra >= rb) ? "true" : "false");
    printf("Real29:  %15g s/b  4.35230e+02\n", fabs(ra));
    printf("Real30:  %15g s/b  1.89425e+05\n", ((ra)*(ra)));
    printf("Real31:  %15g s/b  3.13635e+01\n", sqrt(rb));
    printf("Real32:  %15g s/b -3.44290e-01\n", sin(rb));
    printf("Real33:  %15g s/b  1.56850e+00\n", atan(ra));
    printf("Real34:  %15g s/b  1.41100e+00\n", exp(rd));
    printf("Real35:  %15g s/b  6.07587e+00\n", log(ra));
    printf("Real36:  %ld s/b 435\n", (long)((long)(ra)));
    printf("Real37:  %ld s/b 984\n", (long)(lround(rb)));
    printf("Real38:  %ld s/b 435\n", (long)(lround(ra)));

    printf("Real39:  %15g s/b  1.278052e+03\n", 3.44939e+2 + 9.331129999999999e+2);
    printf("Real40:  %15g s/b  2.389460e+02\n", 8.838849999999998e+2 - 6.449389999999999e+2);
    printf("Real41:  %15g s/b  1.047202e+05\n", 7.5474e+2 * 1.387499999999999e+2);
    printf("Real42:  %15g s/b  7.259598e-03\n", 6.342999999999999e+2 / 8.737399000000001e+4);
    printf("Real43:  %5s s/b true\n", (7.743999999999999e+1 == 7.743999999999999e+1) ? "true" : "false");
    printf("Real44:  %5s s/b false\n", (7.339e+2 == 9.592e+2) ? "true" : "false");
    printf("Real45:  %5s s/b true\n", (8.8322e+2 < 8.383329999999999e+3) ? "true" : "false");
    printf("Real46:  %5s s/b false\n", (4.75322e+2 < 2.3493e+2) ? "true" : "false");
    printf("Real47:  %5s s/b true\n", (7.3743e+3 > 6.442339999999999e+3) ? "true" : "false");
    printf("Real48:  %5s s/b false\n", (9.85562e+2 > 1.00195e+3) ? "true" : "false");
    printf("Real49:  %5s s/b true\n", (3.011e+1 != 9.384400000000001e+2) ? "true" : "false");
    printf("Real50:  %5s s/b false\n", (1.233 != 1.233) ? "true" : "false");
    printf("Real51:  %5s s/b true\n", (8.484002e+3 <= 9.344003e+3) ? "true" : "false");
    printf("Real52:  %5s s/b true\n", (9.109999999999999 <= 9.109999999999999) ? "true" : "false");
    printf("Real53:  %5s s/b false\n", (9.3323e+1 <= 9.032299999999999e+1) ? "true" : "false");
    printf("Real54:  %5s s/b true\n", (6.543439999999999e+3 >= 5.883329999999999e+3) ? "true" : "false");
    printf("Real55:  %5s s/b true\n", (3.24703e+3 >= 3.24703e+3) ? "true" : "false");
    printf("Real56:  %5s s/b false\n", (2.834322e+4 >= 3.004445e+4) ? "true" : "false");
    printf("Real57:  %15g s/b  3.493000e+01\n", fabs(3.492999999999999e+1));
    printf("Real58:  %15g s/b  5.475600e+00\n", ((2.339999999999999)*(2.339999999999999)));
    printf("Real59:  %15g s/b  9.723333e+01\n", sqrt(9.454319999999999e+3));
    printf("Real60:  %15g s/b  3.311461e-01\n", sin(3.421999999999999e+1));
    printf("Real61:  %15g s/b  1.567883e+00\n", atan(3.431999999999999e+2));
    printf("Real62:  %15g s/b  1.393753e+00\n", exp(3.32e-1));
    printf("Real63:  %15g s/b  4.421488e+00\n", log(8.321999999999999e+1));
    printf("Real64:  %ld s/b 24\n", (long)((long)(2.4344e+1)));
    printf("Real65:  %ld s/b 75\n", (long)(lround(7.456e+1)));
    printf("Real66:  %ld s/b 83\n", (long)(lround(8.323999999999999e+1)));
    printf("Real67:  %15g s/b  4.333000e+01\n", rcnst);

    ra = - 7.342e+2;
    rb = - 7.63452e+3;
    rc = ra;
    rd = 1.03454e+3;
    re = - 3.8483e-1;
    printf("Real68:  %15g s/b  3.003400e+02\n", ra + rd);
    printf("Real69:  %15g s/b  3.003400e+02\n", rd + ra);
    printf("Real70:  %15g s/b -6.599980e+03\n", rb + rd);
    printf("Real71:  %15g s/b -8.368720e+03\n", ra + rb);
    printf("Real72:  %15g s/b  1.768740e+03\n", rd - ra);
    printf("Real73:  %15g s/b -8.669061e+03\n", rb - rd);
    printf("Real74:  %15g s/b -6.900320e+03\n", rb - ra);
    printf("Real75:  %15g s/b -7.595593e+05\n", rd * ra);
    printf("Real76:  %15g s/b -7.595593e+05\n", ra * rd);
    printf("Real77:  %15g s/b  5.605265e+06\n", ra * rb);
    printf("Real78:  %15g s/b -1.409071e+00\n", rd / ra);
    printf("Real79:  %15g s/b -7.379627e+00\n", rb / rd);
    printf("Real80:  %15g s/b  1.039842e+01\n", rb / ra);
    printf("Real81:  %5s s/b true\n", (ra == rc) ? "true" : "false");
    printf("Real82:  %5s s/b false\n", (ra == rb) ? "true" : "false");
    printf("Real83:  %5s s/b true\n", (ra != rb) ? "true" : "false");
    printf("Real84:  %5s s/b false\n", (ra != rc) ? "true" : "false");
    printf("Real85:  %5s s/b true\n", (ra < rd) ? "true" : "false");
    printf("Real86:  %5s s/b true\n", (rb < ra) ? "true" : "false");
    printf("Real87:  %5s s/b false\n", (rd < ra) ? "true" : "false");
    printf("Real88:  %5s s/b false\n", (ra < rb) ? "true" : "false");
    printf("Real89:  %5s s/b true\n", (rd > ra) ? "true" : "false");
    printf("Real90:  %5s s/b true\n", (ra > rb) ? "true" : "false");
    printf("Real91:  %5s s/b false\n", (ra > rd) ? "true" : "false");
    printf("Real92:  %5s s/b false\n", (rb > ra) ? "true" : "false");
    printf("Real93:  %5s s/b true\n", (ra <= rd) ? "true" : "false");
    printf("Real94:  %5s s/b true\n", (rb <= ra) ? "true" : "false");
    printf("Real95:  %5s s/b true\n", (ra <= rc) ? "true" : "false");
    printf("Real96:  %5s s/b false\n", (rd <= ra) ? "true" : "false");
    printf("Real97:  %5s s/b false\n", (ra <= rb) ? "true" : "false");
    printf("Real98:  %5s s/b true\n", (rd >= ra) ? "true" : "false");
    printf("Real99:  %5s s/b true\n", (ra >= rb) ? "true" : "false");
    printf("Real100: %5s s/b true\n", (ra >= rc) ? "true" : "false");
    printf("Real101: %5s s/b false\n", (ra >= rd) ? "true" : "false");
    printf("Real102: %5s s/b false\n", (rb >= ra) ? "true" : "false");
    printf("Real103: %15g s/b  7.34200e+02\n", fabs(ra));
    printf("Real104: %15g s/b  5.39050e+05\n", ((ra)*(ra)));
    printf("Real105: %15g s/b -4.34850e-01\n", sin(rb));
    printf("Real106: %15g s/b -1.56943e+00\n", atan(ra));
    printf("Real107: %15g s/b  6.80566e-01\n", exp(re));
    printf("Real108: %15ld s/b -734\n", (long)((long)(ra)));
    printf("Real109: %15ld s/b -7635\n", (long)(lround(rb)));
    printf("Real110: %15ld s/b -734\n", (long)(lround(ra)));

    printf("Real111: %15g s/b  1.510000e+01\n", 4.593399999999999e+1 + (- 3.0834e+1));
    printf("Real112: %15g s/b  4.513300e+01\n", - 2.573699999999999e+1 + 7.087e+1);
    printf("Real113: %15g s/b -3.864000e+01\n", - 6.262999999999999e+1 + 2.399e+1);
    printf("Real114: %15g s/b -3.658100e+01\n", - 2.0733e+1 + (- 1.584799999999999e+1));
    printf("Real115: %15g s/b  3.554800e+01\n", 2.077399999999999e+1 - (- 1.477399999999999e+1));
    printf("Real116: %15g s/b -4.939840e+01\n", - 3.4523e+1 - 1.48754e+1);
    printf("Real117: %15g s/b -4.400100e+01\n", - 5.6664e+1 - (- 1.266299999999999e+1));
    printf("Real118: %15g s/b -2.641223e+01\n", 5.663 * (- 4.663999999999999));
    printf("Real119: %15g s/b -1.489041e+02\n", (- 1.862e+1) * 7.996999999999999);
    printf("Real120: %15g s/b  5.585632e+02\n", (- 4.0552e+1) * (- 1.377399999999999e+1));
    printf("Real121: %15g s/b -5.220157e+00\n", 3.06632e+1 / (- 5.873999999999999));
    printf("Real122: %15g s/b -1.772163e+01\n", (- 5.0636e+1) / 2.8573);
    printf("Real123: %15g s/b  4.274582e+00\n", (- 2.07631e+1) / (- 4.857339999999999));
    printf("Real124: %5s s/b true\n", (- 5.775 == - 5.775) ? "true" : "false");
    printf("Real125: %5s s/b false\n", (- 5.6364 == 5.857499999999999) ? "true" : "false");
    printf("Real126: %5s s/b true\n", (- 2.16385e+1 != - 4.0764e+1) ? "true" : "false");
    printf("Real127: %5s s/b false\n", (- 2.1772e+1 != - 2.1772e+1) ? "true" : "false");
    printf("Real128: %5s s/b true\n", (- 3.512 < 5.8467) ? "true" : "false");
    printf("Real129: %5s s/b true\n", (- 3.264399999999999e+1 < - 2.090739999999999e+1) ? "true" : "false");
    printf("Real130: %5s s/b false\n", (2.0763e+1 < - 2.0743e+1) ? "true" : "false");
    printf("Real131: %5s s/b false\n", (- 1.5663e+1 < - 4.0784e+1) ? "true" : "false");
    printf("Real132: %5s s/b true\n", (7.0766e+1 > - 4.974) ? "true" : "false");
    printf("Real133: %5s s/b true\n", (- 2.365319999999999e+1 > - 3.4774e+1) ? "true" : "false");
    printf("Real134: %5s s/b false\n", (- 5.772999999999999 > 5.9874) ? "true" : "false");
    printf("Real135: %5s s/b false\n", (- 6.0663e+1 > - 5.977999999999999e+1) ? "true" : "false");
    printf("Real136: %5s s/b true\n", (- 1.254199999999999e+1 <= 4.0848) ? "true" : "false");
    printf("Real137: %5s s/b true\n", (- 1.48763e+1 <= - 5.084699999999999) ? "true" : "false");
    printf("Real138: %5s s/b true\n", (- 7.837299999999999 <= - 7.837299999999999) ? "true" : "false");
    printf("Real139: %5s s/b false\n", (5.4564 <= - 5.4564) ? "true" : "false");
    printf("Real140: %5s s/b false\n", (- 1.072633e+1 <= - 2.0984e+1) ? "true" : "false");
    printf("Real141: %5s s/b true\n", (9.833999999999999 >= - 3.938299999999999) ? "true" : "false");
    printf("Real142: %5s s/b true\n", (- 4.562 >= - 1.074e+1) ? "true" : "false");
    printf("Real143: %5s s/b true\n", (- 1.362999999999999e+1 >= - 1.362999999999999e+1) ? "true" : "false");
    printf("Real144: %5s s/b false\n", (- 6.74 >= 6.74) ? "true" : "false");
    printf("Real145: %5s s/b false\n", (- 2.076229999999999e+1 >= - 1.057399999999999e+1) ? "true" : "false");
    printf("Real146: %15g s/b  6.823000e+00\n", fabs(- 6.823));
    printf("Real147  %15g s/b  1.212572e+05\n", ((- 3.4822e+2)*(- 3.4822e+2)));
    printf("Real148: %15g s/b  9.421146e-01\n", sin(- 7.3322e+2));
    printf("Real149: %15g s/b -1.570677e+00\n", atan(- 8.387219999999999e+3));
    printf("Real150: %15g s/b  4.171539e-01\n", exp(- 8.743e-1));
    printf("Real151: %ld s/b -33\n", (long)((long)(- 3.342199999999999e+1)));
    printf("Real152: %ld s/b -843\n", (long)(lround(- 8.432199999999999e+2)));
    printf("Real153: %ld s/b -6244\n", (long)(lround(- 6.243759999999999e+3)));
    printf("Real154: %15g s/b -8.422000e+01\n", rscst);
    printf("Real155: %15g s/b  8.422000e+01\n", - rscst);
    printf("Real156:  %15g s/b -4.333000e+01\n", rscst2);
    printf("Real157: %15g s/b  8.422000e+01\n", rscst3);

    printf("\n");
    printf("******************* sets ******************************\n");
    printf("\n");

    printf("Set1:  ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_sclr(_stmp2), _stmp2)); }
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        if (((i) & 1)) {

            { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_scpy(_stmp1, sta), p2c_suni(_stmp1, (p2c_sclr(_stmp2), p2c_sadd(_stmp2, i), p2c_sadd(_stmp2, i + 10), _stmp2)), _stmp1)); }

        }

    }
    }
    { long _forlim = 20;
    for (i = 1; i <= _forlim; i++) {

        if (p2c_sisin(i, sta)) {

            printf("1");

        } else {

            printf("0");

        }

    }
    }
    printf(" s/b ");
    printf("10101010101010101010\n");
    printf("Set2:  ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 4), p2c_sadd(_stmp2, 5), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 2), p2c_sadd(_stmp2, 6), p2c_sadd(_stmp2, 10), _stmp2)); }
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(i, (p2c_scpy(_stmp1, sta), p2c_suni(_stmp1, stb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }

    }
    }
    printf(" s/b ");
    printf("1101110001\n");
    printf("Set3:  ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 2), p2c_sadd(_stmp2, 6), p2c_sadd(_stmp2, 5), p2c_sadd(_stmp2, 7), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 2), p2c_sadd(_stmp2, 6), p2c_sadd(_stmp2, 10), _stmp2)); }
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(i, (p2c_scpy(_stmp1, sta), p2c_sint(_stmp1, stb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }

    }
    }
    printf(" s/b ");
    printf("0100010000\n");
    printf("Set4:  ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 2), p2c_sadd(_stmp2, 4), p2c_sadd(_stmp2, 7), p2c_sadd(_stmp2, 8), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 3), p2c_sadd(_stmp2, 4), p2c_sadd(_stmp2, 8), p2c_sadd(_stmp2, 10), _stmp2)); }
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(i, (p2c_scpy(_stmp1, sta), p2c_sdif(_stmp1, stb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }

    }
    }
    printf(" s/b ");
    printf("0100001000\n");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 4), p2c_sadd(_stmp2, 6), p2c_sadd(_stmp2, 8), p2c_sadd(_stmp2, 9), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 4), p2c_sadd(_stmp2, 5), p2c_sadd(_stmp2, 9), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 4), p2c_sadd(_stmp2, 6), p2c_sadd(_stmp2, 8), p2c_sadd(_stmp2, 9), _stmp2)); }
    printf("Set5:  %5s s/b false\n", (p2c_sequ(sta, stb)) ? "true" : "false");
    printf("Set6:  %5s s/b true\n", (p2c_sequ(sta, stc)) ? "true" : "false");
    printf("Set7:  %5s s/b true\n", (!p2c_sequ(sta, stb)) ? "true" : "false");
    printf("Set8:  %5s s/b false\n", (!p2c_sequ(sta, stc)) ? "true" : "false");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 2), p2c_sadd(_stmp2, 5), p2c_sadd(_stmp2, 7), p2c_sadd(_stmp2, 10), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 5), p2c_sadd(_stmp2, 10), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 5), p2c_sadd(_stmp2, 10), p2c_sadd(_stmp2, 6), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(std,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 2), p2c_sadd(_stmp2, 5), p2c_sadd(_stmp2, 7), p2c_sadd(_stmp2, 10), _stmp2)); }
    printf("Set9:  %5s s/b true\n", (p2c_sinc(sta, stb)) ? "true" : "false");
    printf("Set10: %5s s/b true\n", (p2c_sinc(std, stb)) ? "true" : "false");
    printf("Set11: %5s s/b false\n", (p2c_sinc(sta, stc)) ? "true" : "false");
    printf("Set12: %5s s/b true\n", (p2c_sinc(sta, stb)) ? "true" : "false");
    printf("Set13: %5s s/b true\n", (p2c_sinc(std, stb)) ? "true" : "false");
    printf("Set14: %5s s/b false\n", (p2c_sinc(sta, stc)) ? "true" : "false");
    printf("Set15: ");
    i = 2;
    x = 4;
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, i), p2c_sadd(_stmp2, x), p2c_sadd(_stmp2, i + x), _stmp2)); }
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        if (p2c_sisin(i, sta)) {

            printf("1");

        } else {

            printf("0");

        }

    }
    }
    printf(" s/b ");
    printf("0101010000\n");

    p2c_scpy(ste,std);
    { p2c_settype _stmp1, _stmp2; p2c_scpy(stf,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 1), p2c_sadd(_stmp2, 2), p2c_sadd(_stmp2, 5), p2c_sadd(_stmp2, 7), _stmp2)); }
    p2c_scpy(stg,stf);
    i = 10;
    printf("Set16: %5s s/b true\n", (p2c_sisin(5, (p2c_sclr(_stmp2), p2c_radd(_stmp2, 1, i), _stmp2))) ? "true" : "false");

    printf("Set17: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_sclr(_stmp2), _stmp2)); }
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (((ci) & 1)) {

            { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_scpy(_stmp1, csta), p2c_suni(_stmp1, (p2c_sclr(_stmp2), p2c_sadd(_stmp2, ci), p2c_sadd(_stmp2, ci + 10), _stmp2)), _stmp1)); }

        }
        if (ci == _forlim) break;

    }
    }
    { long _forlim = 't';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (p2c_sisin(ci, csta)) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("a_c_e_g_i_k_m_o_q_s_\n");
    printf("Set18: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'c'), p2c_sadd(_stmp2, 'f'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'c'), p2c_sadd(_stmp2, 'd'), p2c_sadd(_stmp2, 'g'), _stmp2)); }
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ci, (p2c_scpy(_stmp1, csta), p2c_suni(_stmp1, cstb), _stmp1))) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        }
        if (ci == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("a_cd_fg___\n");
    printf("Set19: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'd'), p2c_sadd(_stmp2, 'f'), p2c_sadd(_stmp2, 'h'), p2c_sadd(_stmp2, 'a'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'i'), p2c_sadd(_stmp2, 'h'), _stmp2)); }
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ci, (p2c_scpy(_stmp1, csta), p2c_sint(_stmp1, cstb), _stmp1))) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        }
        if (ci == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("a______h__\n");
    printf("Set20: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'd'), p2c_sadd(_stmp2, 'i'), p2c_sadd(_stmp2, 'j'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'i'), p2c_sadd(_stmp2, 'h'), p2c_sadd(_stmp2, 'd'), p2c_sadd(_stmp2, 'e'), _stmp2)); }
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ci, (p2c_scpy(_stmp1, csta), p2c_sdif(_stmp1, cstb), _stmp1))) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        }
        if (ci == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("_b_______j\n");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'd'), p2c_sadd(_stmp2, 'h'), p2c_sadd(_stmp2, 'j'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'd'), p2c_sadd(_stmp2, 'h'), p2c_sadd(_stmp2, 'c'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'd'), p2c_sadd(_stmp2, 'h'), p2c_sadd(_stmp2, 'j'), _stmp2)); }
    printf("Set21: %5s s/b false\n", (p2c_sequ(csta, cstb)) ? "true" : "false");
    printf("Set22: %5s s/b true\n", (p2c_sequ(csta, cstc)) ? "true" : "false");
    printf("Set23: %5s s/b true\n", (!p2c_sequ(csta, cstb)) ? "true" : "false");
    printf("Set24: %5s s/b false\n", (!p2c_sequ(csta, cstc)) ? "true" : "false");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'f'), p2c_sadd(_stmp2, 'g'), p2c_sadd(_stmp2, 'j'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'f'), p2c_sadd(_stmp2, 'g'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'f'), p2c_sadd(_stmp2, 'g'), p2c_sadd(_stmp2, 'h'), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstd,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'f'), p2c_sadd(_stmp2, 'g'), p2c_sadd(_stmp2, 'j'), _stmp2)); }
    printf("Set25: %5s s/b true\n", (p2c_sinc(csta, cstb)) ? "true" : "false");
    printf("Set26: %5s s/b true\n", (p2c_sinc(cstd, cstb)) ? "true" : "false");
    printf("Set27: %5s s/b false\n", (p2c_sinc(csta, cstc)) ? "true" : "false");
    printf("Set28: %5s s/b true\n", (p2c_sinc(csta, cstb)) ? "true" : "false");
    printf("Set29: %5s s/b true\n", (p2c_sinc(cstd, cstb)) ? "true" : "false");
    printf("Set30: %5s s/b false\n", (p2c_sinc(csta, cstc)) ? "true" : "false");
    printf("Set31: ");
    ci = 'a';
    i = 4;
    { p2c_settype _stmp1, _stmp2; p2c_scpy(csta,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, ci), p2c_sadd(_stmp2, ci + i), _stmp2)); }
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (p2c_sisin(ci, csta)) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("a___e_____\n");

    p2c_scpy(cste,cstd);
    { p2c_settype _stmp1, _stmp2; p2c_scpy(cstf,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'a'), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'e'), p2c_sadd(_stmp2, 'f'), _stmp2)); }
    p2c_scpy(cstg,cstf);

    printf("Set32: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_sclr(_stmp2), _stmp2)); }
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        if (((ei) & 1)) {

            { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_scpy(_stmp1, sena), p2c_suni(_stmp1, (p2c_sclr(_stmp2), p2c_sadd(_stmp2, ei), _stmp2)), _stmp1)); }

        }

    }
    }
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        if (p2c_sisin(ei, sena)) {

            printf("1");

        } else {

            printf("0");

        }

    }
    }
    printf(" s/b ");
    printf("0101010101\n");
    printf("Set33: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, four), p2c_sadd(_stmp2, five), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, six), p2c_sadd(_stmp2, ten), _stmp2)); }
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ei, (p2c_scpy(_stmp1, sena), p2c_suni(_stmp1, senb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }

    }
    }
    printf(" s/b ");
    printf("1101110001\n");
    printf("Set34: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, six), p2c_sadd(_stmp2, five), p2c_sadd(_stmp2, seven), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, six), p2c_sadd(_stmp2, ten), _stmp2)); }
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ei, (p2c_scpy(_stmp1, sena), p2c_sint(_stmp1, senb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }

    }
    }
    printf(" s/b ");
    printf("0100010000\n");
    printf("Set35: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, four), p2c_sadd(_stmp2, seven), p2c_sadd(_stmp2, eight), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, three), p2c_sadd(_stmp2, four), p2c_sadd(_stmp2, eight), p2c_sadd(_stmp2, ten), _stmp2)); }
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ei, (p2c_scpy(_stmp1, sena), p2c_sdif(_stmp1, senb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }

    }
    }
    printf(" s/b ");
    printf("0100001000\n");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, four), p2c_sadd(_stmp2, six), p2c_sadd(_stmp2, eight), p2c_sadd(_stmp2, nine), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, four), p2c_sadd(_stmp2, five), p2c_sadd(_stmp2, nine), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, four), p2c_sadd(_stmp2, six), p2c_sadd(_stmp2, eight), p2c_sadd(_stmp2, nine), _stmp2)); }
    printf("Set36: %5s s/b false\n", (p2c_sequ(sena, senb)) ? "true" : "false");
    printf("Set37: %5s s/b true\n", (p2c_sequ(sena, senc)) ? "true" : "false");
    printf("Set38: %5s s/b true\n", (!p2c_sequ(sena, senb)) ? "true" : "false");
    printf("Set39: %5s s/b false\n", (!p2c_sequ(sena, senc)) ? "true" : "false");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, five), p2c_sadd(_stmp2, seven), p2c_sadd(_stmp2, ten), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, five), p2c_sadd(_stmp2, ten), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, five), p2c_sadd(_stmp2, ten), p2c_sadd(_stmp2, six), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(send,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, five), p2c_sadd(_stmp2, seven), p2c_sadd(_stmp2, ten), _stmp2)); }
    printf("Set40: %5s s/b true\n", (p2c_sinc(sena, senb)) ? "true" : "false");
    printf("Set41: %5s s/b true\n", (p2c_sinc(send, senb)) ? "true" : "false");
    printf("Set42: %5s s/b false\n", (p2c_sinc(sena, senc)) ? "true" : "false");
    printf("Set43: %5s s/b true\n", (p2c_sinc(sena, senb)) ? "true" : "false");
    printf("Set44: %5s s/b true\n", (p2c_sinc(send, senb)) ? "true" : "false");
    printf("Set45: %5s s/b false\n", (p2c_sinc(sena, senc)) ? "true" : "false");
    printf("Set46: ");
    ei = two;
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sena,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, ei), p2c_sadd(_stmp2, (ei + 1)), _stmp2)); }
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        if (p2c_sisin(ei, sena)) {

            printf("1");

        } else {

            printf("0");

        }

    }
    }
    printf(" s/b ");
    printf("0110000000\n");

    { p2c_settype _stmp1, _stmp2; p2c_scpy(send,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, five), _stmp2)); }
    p2c_scpy(sene,send);
    { p2c_settype _stmp1, _stmp2; p2c_scpy(senf,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, one), p2c_sadd(_stmp2, two), p2c_sadd(_stmp2, five), p2c_sadd(_stmp2, seven), _stmp2)); }
    p2c_scpy(seng,senf);

    printf("Set47: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_sclr(_stmp2), _stmp2)); }
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        if (((ba) & 1)) {

            { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_scpy(_stmp1, sba), p2c_suni(_stmp1, (p2c_sclr(_stmp2), p2c_sadd(_stmp2, ba), _stmp2)), _stmp1)); }

        }
        if (ba == _forlim) break;

    }
    }
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        if (p2c_sisin(ba, sba)) {

            printf("1");

        } else {

            printf("0");

        }
        if (ba == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("01\n");
    printf("Set48: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, false), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), _stmp2)); }
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ba, (p2c_scpy(_stmp1, sba), p2c_suni(_stmp1, sbb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }
        if (ba == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("11\n");
    printf("Set49: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, false), p2c_sadd(_stmp2, true), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, false), _stmp2)); }
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ba, (p2c_scpy(_stmp1, sba), p2c_sint(_stmp1, sbb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }
        if (ba == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("10\n");
    printf("Set50: ");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), p2c_sadd(_stmp2, false), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), _stmp2)); }
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(ba, (p2c_scpy(_stmp1, sba), p2c_sdif(_stmp1, sbb), _stmp1))) {

            printf("1");

        } else {

            printf("0");

        }
        }
        if (ba == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("10\n");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, false), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), _stmp2)); }
    printf("Set51: %5s s/b false\n", (p2c_sequ(sba, sbb)) ? "true" : "false");
    printf("Set52: %5s s/b true\n", (p2c_sequ(sba, sbc)) ? "true" : "false");
    printf("Set53: %5s s/b true\n", (!p2c_sequ(sba, sbb)) ? "true" : "false");
    printf("Set54: %5s s/b false\n", (!p2c_sequ(sba, sbc)) ? "true" : "false");
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), p2c_sadd(_stmp2, false), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbb,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, false), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbc,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), _stmp2)); }
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbd,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, false), _stmp2)); }
    printf("Set55: %5s s/b true\n", (p2c_sinc(sba, sbb)) ? "true" : "false");
    printf("Set56: %5s s/b true\n", (p2c_sinc(sbd, sbb)) ? "true" : "false");
    printf("Set57: %5s s/b false\n", (p2c_sinc(sbb, sbc)) ? "true" : "false");
    printf("Set58: %5s s/b true\n", (p2c_sinc(sba, sbb)) ? "true" : "false");
    printf("Set59: %5s s/b true\n", (p2c_sinc(sbd, sbb)) ? "true" : "false");
    printf("Set60: %5s s/b false\n", (p2c_sinc(sbb, sbc)) ? "true" : "false");
    printf("Set61: ");
    ba = false;
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sba,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, ba), p2c_sadd(_stmp2, (ba + 1)), _stmp2)); }
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        if (p2c_sisin(ba, sba)) {

            printf("1");

        } else {

            printf("0");

        }
        if (ba == _forlim) break;

    }
    }
    printf(" s/b ");
    printf("11\n");

    p2c_scpy(sbe,sbd);
    { p2c_settype _stmp1, _stmp2; p2c_scpy(sbf,(p2c_sclr(_stmp2), p2c_sadd(_stmp2, true), _stmp2)); }
    p2c_scpy(sbg,sbf);
    printf("set62: ");
    pi1 = malloc(sizeof(long));
    pi2 = malloc(sizeof(long));
    *pi1 = 3;
    *pi2 = 5;
    printf("%5s", (p2c_sequ((p2c_sclr(_stmp2), p2c_radd(_stmp2, *pi1, *pi2), _stmp2), (p2c_sclr(_stmp2), p2c_radd(_stmp2, 3, 5), _stmp2))) ? "true" : "false");
    printf(" s/b true\n");
    printf("set63: ");
    srx = 1;
    sry = 10;
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        { p2c_settype _stmp1, _stmp2;
        if (p2c_sisin(i, (p2c_sclr(_stmp2), p2c_sadd(_stmp2, srx), p2c_sadd(_stmp2, sry), _stmp2))) {

            printf("1");

        } else {

            printf("0");

        }
        }

    }
    }
    printf(" s/b 1000000001\n");

    printf("\n");
    printf("******************* Pointers ******************************\n");
    printf("\n");

    printf("Pointer1:   ");
    pti = malloc(sizeof(long));
    *pti = 4594;
    printf("%ld s/b 4594\n", (long)(*pti));
    printf("Pointer2:   ");
    ptb = malloc(sizeof(bool));
    *ptb = true;
    printf("%5s s/b  true\n", (*ptb) ? "true" : "false");
    printf("Pointer3:   ");
    ptb = malloc(sizeof(bool));
    *ptb = false;
    printf("%5s s/b false\n", (*ptb) ? "true" : "false");
    printf("Pointer4:   ");
    ptc = malloc(sizeof(char));
    *ptc = 'p';
    printf("%c s/b p\n", (int)(*ptc));
    printf("Pointer5:   ");
    pte = malloc(sizeof(int));
    *pte = six;
    printf("%ld s/b 5\n", (long)(*pte));
    printf("Pointer6:   ");
    ptes = malloc(sizeof(unsigned char));
    *ptes = four;
    printf("%ld s/b 3\n", (long)(*ptes));
    printf("Pointer7:   ");
    pts = malloc(sizeof(unsigned char));
    *pts = 17;
    printf("%ld s/b 17\n", (long)(*pts));
    printf("Pointer8:   ");
    ptr = malloc(sizeof(double));
    *ptr = 1.2345678e+3;
    printf("%.4f s/b 1234.5678\n", *ptr);
    printf("Pointer9:   ");
    ptst = malloc(sizeof(string10));
    memmove(&(*ptst)[1],"my word is", 10);
    printf("%10s s/b my word is\n", &(*ptst)[1]);
    printf("Pointer10:  ");
    pta = malloc(sizeof(arri));
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        (*pta)[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)((*pta)[i]));

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Pointer11:   ");
    ptrc = malloc(sizeof(recs));
    ptrc->a = 7234;
    ptrc->b = 'y';
    printf("%ld %c s/b 7234 y\n", (long)(ptrc->a), (int)(ptrc->b));
    printf("Pointer12:   ");
    ptstc = malloc(sizeof(p2c_settype));
    { p2c_settype _stmp1, _stmp2; p2c_scpy((*ptstc),(p2c_sclr(_stmp2), p2c_sadd(_stmp2, 'b'), p2c_sadd(_stmp2, 'd'), p2c_radd(_stmp2, 'i', 'j'), _stmp2)); }
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (p2c_sisin(ci, (*ptstc))) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf(" s/b _b_d____ij\n");
    printf("Pointer13:  ");
    ptp = malloc(sizeof(iptr));
    *ptp = malloc(sizeof(long));
    **ptp = 3732;
    printf("%ld s/b 3732\n", (long)(**ptp));

    printf("Pointer14:  ");
    pti = NULL;
    printf("%5s s/b  true\n", (pti == NULL) ? "true" : "false");
    printf("Pointer15:  ");
    pti = malloc(sizeof(long));
    printf("%5s s/b false\n", (pti == NULL) ? "true" : "false");
    printf("Pointer16:  ");
    pti1 = pti;
    printf("%5s s/b true\n", (pti == pti1) ? "true" : "false");
    printf("Pointer17:  ");
    pti1 = pti;
    printf("%5s s/b false\n", (pti != pti1) ? "true" : "false");
    printf("Pointer18:  ");
    pti1 = malloc(sizeof(long));
    printf("%5s s/b false\n", (pti == pti1) ? "true" : "false");
    printf("Pointer19:  ");
    printf("%5s s/b  true\n", (pti != pti1) ? "true" : "false");

    pti2 = malloc(sizeof(long));
    free(frp());

    printf("Pointer20:  ");
    ipa = malloc(sizeof(long));
    ipb = malloc(sizeof(long));
    ipc = malloc(sizeof(long));
    free(ipa);
    free(ipb);
    free(ipc);
    printf("done s/b done\n");

    printf("Pointer21:  ");
    ipa = malloc(sizeof(long));
    ipb = malloc(sizeof(long));
    ipc = malloc(sizeof(long));
    free(ipc);
    free(ipb);
    free(ipa);

    printf("Pointer22:  ");
    ipa = malloc(sizeof(long));
    ipb = malloc(sizeof(long));
    ipc = malloc(sizeof(long));
    ipd = malloc(sizeof(long));
    free(ipb);
    free(ipc);
    free(ipa);
    free(ipd);
    printf("done s/b done\n");

    printf("Pointer23:  ");
    ipa = malloc(sizeof(long));
    ipb = malloc(sizeof(long));
    ipc = malloc(sizeof(long));
    ipd = malloc(sizeof(long));
    ipe = malloc(sizeof(long));
    free(ipb);
    free(ipd);
    free(ipc);
    free(ipa);
    free(ipe);
    printf("done s/b done\n");
    if (false) {

        printf("Pointer24:  \n");
        { long _forlim = 100;
        for (cnt = 1; cnt <= _forlim; cnt++) {

            printf("%3ld ", (long)(cnt));
            if ((cnt % 10) == 0) {

                printf("\n");

            }
            { long _forlim = 100;
            for (i = 1; i <= _forlim; i++) {

                iap[i] = NULL;

            }
            }
            { long _forlim = 100;
            for (i = 1; i <= _forlim; i++) {

                iap[i] = malloc(sizeof(long));
                *iap[i] = i;

            }
            }
            { long _forlim = 100;
            for (i = 1; i <= _forlim; i++) {

                if (iap[i] == NULL) {

                    printf("*** bad allocation of block\n");

                }

            }
            }
            { long _forlim = 1;
            for (i = 100; i >= _forlim; i--) {

                if (*iap[i] != i) {

                    printf("*** bad block content\n");

                }

            }
            }
            { long _forlim = 100;
            for (i = 1; i <= _forlim; i++) {

                free(iap[i]);
                iap[i] = NULL;
                { long _forlim = 100;
                for (x = 1; x <= _forlim; x++) {

                    if (iap[x] != NULL) {

                        if (*iap[x] != x) {

                            printf("*** bad block content\n");

                        }

                    }

                }
                }

            }
            }
            { long _forlim = 100;
            for (i = 1; i <= _forlim; i++) {

                iap[i] = NULL;

            }
            }
            { long _forlim = 100;
            for (i = 1; i <= _forlim; i++) {

                iap[i] = malloc(sizeof(long));
                *iap[i] = i;

            }
            }
            { long _forlim = 100;
            for (i = 1; i <= _forlim; i++) {

                if (iap[i] == NULL) {

                    printf("*** bad allocation of block\n");

                }

            }
            }
            { long _forlim = 1;
            for (i = 100; i >= _forlim; i--) {

                if (*iap[i] != i) {

                    printf("*** bad block content\n");

                }

            }
            }
            { long _forlim = 1;
            for (i = 100; i >= _forlim; i--) {

                free(iap[i]);
                iap[i] = NULL;
                { long _forlim = 100;
                for (x = 1; x <= _forlim; x++) {

                    if (iap[x] != NULL) {

                        if (*iap[x] != x) {

                            printf("*** bad block content\n");

                        }

                    }

                }
                }

            }
            }

        }
        }
        printf("\n");
        printf("s/b\n");
        printf("\n");
        printf("  1   2   3   4   5   6   7   8   9  10\n");
        printf(" 11  12  13  14  15  16  17  18  19  20\n");
        printf(" 21  22  23  24  25  26  27  28  29  30\n");
        printf(" 31  32  33  34  35  36  37  38  39  40\n");
        printf(" 41  42  43  44  45  46  47  48  49  50\n");
        printf(" 51  52  53  54  55  56  57  58  59  60\n");
        printf(" 61  62  63  64  65  66  67  68  69  70\n");
        printf(" 71  72  73  74  75  76  77  78  79  80\n");
        printf(" 81  82  83  84  85  86  87  88  89  90\n");
        printf(" 91  92  93  94  95  96  97  98  99  100\n");

    } else {

        printf("Pointer24:  \n");
        printf("  1   2   3   4   5   6   7   8   9  10 \n");
        printf(" 11  12  13  14  15  16  17  18  19  20 \n");
        printf(" 21  22  23  24  25  26  27  28  29  30 \n");
        printf(" 31  32  33  34  35  36  37  38  39  40 \n");
        printf(" 41  42  43  44  45  46  47  48  49  50 \n");
        printf(" 51  52  53  54  55  56  57  58  59  60 \n");
        printf(" 61  62  63  64  65  66  67  68  69  70 \n");
        printf(" 71  72  73  74  75  76  77  78  79  80 \n");
        printf(" 81  82  83  84  85  86  87  88  89  90 \n");
        printf(" 91  92  93  94  95  96  97  98  99 100 \n");
        printf("\n");
        printf("s/b\n");
        printf("\n");
        printf("  1   2   3   4   5   6   7   8   9  10\n");
        printf(" 11  12  13  14  15  16  17  18  19  20\n");
        printf(" 21  22  23  24  25  26  27  28  29  30\n");
        printf(" 31  32  33  34  35  36  37  38  39  40\n");
        printf(" 41  42  43  44  45  46  47  48  49  50\n");
        printf(" 51  52  53  54  55  56  57  58  59  60\n");
        printf(" 61  62  63  64  65  66  67  68  69  70\n");
        printf(" 71  72  73  74  75  76  77  78  79  80\n");
        printf(" 81  82  83  84  85  86  87  88  89  90\n");
        printf(" 91  92  93  94  95  96  97  98  99  100\n");

    }
    if (false) {

        rndseq = 1;

        printf("Pointer25:  \n");
        { long _forlim = 100;
        for (i = 1; i <= _forlim; i++) {

            iap[i] = NULL;

        }
        }
        { long _forlim = 100;
        for (cnt2 = 1; cnt2 <= _forlim; cnt2++) {

            printf("%3ld ", (long)(cnt2));
            if ((cnt2 % 10) == 0) {

                printf("\n");

            }
            { long _forlim = 100;
            for (cnt = 1; cnt <= _forlim; cnt++) {

                rn = random_(1, 100);
                iap[rn] = malloc(sizeof(long));

                *iap[rn] = rn;
                { long _forlim = 100;
                for (i = 1; i <= _forlim; i++) {

                    if (iap[i] != NULL) {

                        if (*iap[i] != i) {

                            printf("*** bad block content\n");

                        }

                    }

                }
                }

                rn = random_(1, 100);
                if (iap[rn] != NULL) {

                    free(iap[rn]);

                }

                iap[rn] = NULL;
                { long _forlim = 100;
                for (i = 1; i <= _forlim; i++) {

                    if (iap[i] != NULL) {

                        if (*iap[i] != i) {

                            printf("*** bad block content\n");

                        }

                    }

                }
                }

            }
            }

        }
        }
        printf("\n");
        printf("s/b\n");
        printf("\n");
        printf("  1   2   3   4   5   6   7   8   9  10\n");
        printf(" 11  12  13  14  15  16  17  18  19  20\n");
        printf(" 21  22  23  24  25  26  27  28  29  30\n");
        printf(" 31  32  33  34  35  36  37  38  39  40\n");
        printf(" 41  42  43  44  45  46  47  48  49  50\n");
        printf(" 51  52  53  54  55  56  57  58  59  60\n");
        printf(" 61  62  63  64  65  66  67  68  69  70\n");
        printf(" 71  72  73  74  75  76  77  78  79  80\n");
        printf(" 81  82  83  84  85  86  87  88  89  90\n");
        printf(" 91  92  93  94  95  96  97  98  99  100\n");

    } else {

        printf("Pointer25:  \n");
        printf("  1   2   3   4   5   6   7   8   9  10 \n");
        printf(" 11  12  13  14  15  16  17  18  19  20 \n");
        printf(" 21  22  23  24  25  26  27  28  29  30 \n");
        printf(" 31  32  33  34  35  36  37  38  39  40 \n");
        printf(" 41  42  43  44  45  46  47  48  49  50 \n");
        printf(" 51  52  53  54  55  56  57  58  59  60 \n");
        printf(" 61  62  63  64  65  66  67  68  69  70 \n");
        printf(" 71  72  73  74  75  76  77  78  79  80 \n");
        printf(" 81  82  83  84  85  86  87  88  89  90 \n");
        printf(" 91  92  93  94  95  96  97  98  99 100 \n");
        printf("\n");
        printf("s/b\n");
        printf("\n");
        printf("  1   2   3   4   5   6   7   8   9  10\n");
        printf(" 11  12  13  14  15  16  17  18  19  20\n");
        printf(" 21  22  23  24  25  26  27  28  29  30\n");
        printf(" 31  32  33  34  35  36  37  38  39  40\n");
        printf(" 41  42  43  44  45  46  47  48  49  50\n");
        printf(" 51  52  53  54  55  56  57  58  59  60\n");
        printf(" 61  62  63  64  65  66  67  68  69  70\n");
        printf(" 71  72  73  74  75  76  77  78  79  80\n");
        printf(" 81  82  83  84  85  86  87  88  89  90\n");
        printf(" 91  92  93  94  95  96  97  98  99  100\n");

    }

    printf("\n");
    printf("******************* arrays ******************************\n");
    printf("\n");

    printf("Array1:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avi[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(avi[i]));

    }
    }
    printf(" s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array2:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavi[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(pavi[i]));

    }
    }
    printf(" s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array3:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avis[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(avis[i]));

    }
    }
    printf(" s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array4:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavis[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(pavis[i]));

    }
    }
    printf(" s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array5:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avb[i] = ((i) & 1);

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%5s ", (avb[i]) ? "true" : "false");

    }
    }
    printf("\n");
    printf("    s/b:   false  true false  true false  true false  true false  true\n");
    printf("Array6:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavb[i] = ((i) & 1);

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%5s ", (pavb[i]) ? "true" : "false");

    }
    }
    printf("\n");
    printf("    s/b:   false  true false  true false  true false  true false  true\n");
    printf("Array7:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avr[i] = i + 10 + 1.199999999999999e-1;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%.2f ", avr[i]);

    }
    }
    printf("\n");
    printf("    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 13.12 12.12 11.12\n");
    printf("Array8:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavr[i] = i + 10 + 1.199999999999999e-1;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%.2f ", pavr[i]);

    }
    }
    printf("\n");
    printf("    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 13.12 12.12 11.12\n");
    printf("Array9:   ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avc[i] = i + 'a';

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%c ", (int)(avc[i]));

    }
    }
    printf("s/b k j i h g f e d c b\n");
    printf("Array10:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavc[i] = i + 'a';

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%c ", (int)(pavc[i]));

    }
    }
    printf("s/b k j i h g f e d c b\n");
    printf("Array11:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avcs[i] = i + 'f';

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%c ", (int)(avcs[i]));

    }
    }
    printf("s/b p o n m l k j i h g\n");
    printf("Array12:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavcs[i] = i + 'f';

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%c ", (int)(pavcs[i]));

    }
    }
    printf("s/b p o n m l k j i h g\n");
    printf("Array13:  ");
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        ave[ei + 1] = ei;

    }
    }
    { long _forlim = one;
    for (ei = ten; ei >= _forlim; ei--) {

        printf("%ld ", (long)(ave[ei + 1]));

    }
    }
    printf("s/b 9 8 7 6 5 4 3 2 1 0\n");
    printf("Array14:  ");
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        pave[ei + 1] = ei;

    }
    }
    { long _forlim = one;
    for (ei = ten; ei >= _forlim; ei--) {

        printf("%ld ", (long)(ave[ei + 1]));

    }
    }
    printf("s/b 9 8 7 6 5 4 3 2 1 0\n");
    printf("Array15:  ");
    { long _forlim = six;
    for (ei = three; ei <= _forlim; ei++) {

        aves[ei + 1] = ei;

    }
    }
    { long _forlim = three;
    for (ei = six; ei >= _forlim; ei--) {

        printf("%ld ", (long)(aves[ei + 1]));

    }
    }
    printf("s/b 5 4 3 2\n");
    printf("Array16:  ");
    { long _forlim = six;
    for (ei = three; ei <= _forlim; ei++) {

        paves[ei + 1] = ei;

    }
    }
    { long _forlim = three;
    for (ei = six; ei >= _forlim; ei--) {

        printf("%ld ", (long)(paves[ei + 1]));

    }
    }
    printf("s/b 5 4 3 2\n");
    printf("Array17:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        { p2c_settype _stmp1, _stmp2; p2c_scpy(avs[i],(p2c_sclr(_stmp2), p2c_sadd(_stmp2, i + 'a'), _stmp2)); }

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        { long _forlim = 'z';
        for (ci = 'a'; ci <= _forlim; ci++) {

            if (p2c_sisin(ci, avs[i])) {

                printf("%c ", (int)(ci));

            }
            if (ci == _forlim) break;

        }
        }

    }
    }
    printf("s/b k j i h g f e d c b\n");
    printf("Array18:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        { p2c_settype _stmp1, _stmp2; p2c_scpy(pavs[i],(p2c_sclr(_stmp2), p2c_sadd(_stmp2, i + 'a'), _stmp2)); }

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        { long _forlim = 'z';
        for (ci = 'a'; ci <= _forlim; ci++) {

            if (p2c_sisin(ci, pavs[i])) {

                printf("%c ", (int)(ci));

            }
            if (ci == _forlim) break;

        }
        }

    }
    }
    printf("s/b k j i h g f e d c b\n");
    printf("Array19:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avrc[i].a = i + 10;
        avrc[i].b = i + 'a';

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld %c ", (long)(avrc[i].a), (int)(avrc[i].b));

    }
    }
    printf("\n");
    printf("     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b\n");
    printf("Array20:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavrc[i].a = i + 10;
        pavrc[i].b = i + 'a';

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld %c ", (long)(pavrc[i].a), (int)(pavrc[i].b));

    }
    }
    printf("\n");
    printf("     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b\n");
    printf("Array21:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avf[i] = tmpfile();
        printf("%11ld\n", (long)(i + 10));

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        rewind(avf[i]);
        fscanf(avf[i], "%ld", &x);
        fscanf(avf[i], "%*[^\n]"); fscanf(avf[i], "%*c");
        printf("%ld ", (long)(x));

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array22:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavf[i] = tmpfile();
        printf("%11ld\n", (long)(i + 10));

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        rewind(pavf[i]);
        fscanf(pavf[i], "%ld", &x);
        fscanf(pavf[i], "%*[^\n]"); fscanf(pavf[i], "%*c");
        printf("%ld ", (long)(x));

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array23:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avp[i] = malloc(sizeof(long));
        *avp[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(*avp[i]));

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array24:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavp[i] = malloc(sizeof(long));
        *pavp[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(*pavp[i]));

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");

    printf("Array25:  ");
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        bia[ba] = ba + 10;
        if (ba == _forlim) break;

    }
    }
    { long _forlim = false;
    for (ba = true; ba >= _forlim; ba--) {

        printf("%ld ", (long)(bia[ba]));
        if (ba == _forlim) break;

    }
    }
    printf(" s/b 11 10\n");
    printf("Array26:  ");
    { long _forlim = true;
    for (ba = false; ba <= _forlim; ba++) {

        pbia[ba] = ba + 10;
        if (ba == _forlim) break;

    }
    }
    { long _forlim = false;
    for (ba = true; ba >= _forlim; ba--) {

        printf("%ld ", (long)(pbia[ba]));
        if (ba == _forlim) break;

    }
    }
    printf(" s/b 11 10\n");
    printf("Array27:  ");
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        cia[(unsigned char)ci] = ci;
        if (ci == _forlim) break;

    }
    }
    { long _forlim = 'a';
    for (ci = 'j'; ci >= _forlim; ci--) {

        printf("%c ", (int)(cia[(unsigned char)ci]));
        if (ci == _forlim) break;

    }
    }
    printf(" s/b  j i h g f e d c b a\n");
    printf("Array28:  ");
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        pcia[(unsigned char)ci] = ci;
        if (ci == _forlim) break;

    }
    }
    { long _forlim = 'a';
    for (ci = 'j'; ci >= _forlim; ci--) {

        printf("%c ", (int)(pcia[(unsigned char)ci]));
        if (ci == _forlim) break;

    }
    }
    printf(" s/b  j i h g f e d c b a\n");
    printf("Array29:  ");
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        csia[(unsigned char)ci] = ci;
        if (ci == _forlim) break;

    }
    }
    { long _forlim = 'a';
    for (ci = 'j'; ci >= _forlim; ci--) {

        printf("%c ", (int)(csia[(unsigned char)ci]));
        if (ci == _forlim) break;

    }
    }
    printf(" s/b  j i h g f e d c b a\n");
    printf("Array30:  ");
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        pcsia[(unsigned char)ci] = ci;
        if (ci == _forlim) break;

    }
    }
    { long _forlim = 'a';
    for (ci = 'j'; ci >= _forlim; ci--) {

        printf("%c ", (int)(pcsia[(unsigned char)ci]));
        if (ci == _forlim) break;

    }
    }
    printf(" s/b  j i h g f e d c b a\n");
    printf("Array31:  ");
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        eia[ei] = ei;

    }
    }
    { long _forlim = one;
    for (ei = ten; ei >= _forlim; ei--) {

        printf("%ld ", (long)(eia[ei]));

    }
    }
    printf(" s/b  9 8 7 6 5 4 3 2 1 0\n");
    printf("Array32:  ");
    { long _forlim = ten;
    for (ei = one; ei <= _forlim; ei++) {

        peia[ei] = ei;

    }
    }
    { long _forlim = one;
    for (ei = ten; ei >= _forlim; ei--) {

        printf("%ld ", (long)(peia[ei]));

    }
    }
    printf(" s/b  9 8 7 6 5 4 3 2 1 0\n");
    printf("Array33:  ");
    { long _forlim = six;
    for (ei = two; ei <= _forlim; ei++) {

        eia[ei] = ei;

    }
    }
    { long _forlim = two;
    for (ei = six; ei >= _forlim; ei--) {

        printf("%ld ", (long)(eia[ei]));

    }
    }
    printf(" s/b  5 4 3 2 1\n");
    printf("Array34:  ");
    { long _forlim = six;
    for (ei = two; ei <= _forlim; ei++) {

        peia[ei] = ei;

    }
    }
    { long _forlim = two;
    for (ei = six; ei >= _forlim; ei--) {

        printf("%ld ", (long)(peia[ei]));

    }
    }
    printf(" s/b  5 4 3 2 1\n");

    printf("Array35:\n");
    z = 0;
    { long _forlim = 10;
    for (x = 1; x <= _forlim; x++) {

        { long _forlim = 10;
        for (y = 1; y <= _forlim; y++) {

            da[y][x] = z;
            z = z + 1;

        }
        }

    }
    }
    { long _forlim = 10;
    for (x = 1; x <= _forlim; x++) {

        { long _forlim = 10;
        for (y = 1; y <= _forlim; y++) {

            printf("%2ld ", (long)(da[x][y]));

        }
        }
        printf("\n");

    }
    }
    printf("s/b\n");
    printf("0 10 20 30 40 50 60 70 80 90\n");
    printf("1 11 21 31 41 51 61 71 81 91\n");
    printf("2 12 22 32 42 52 62 72 82 92\n");
    printf("3 13 23 33 43 53 63 73 83 93\n");
    printf("4 14 24 34 44 54 64 74 84 94\n");
    printf("5 15 25 35 45 55 65 75 85 95\n");
    printf("6 16 26 36 46 56 66 76 86 96\n");
    printf("7 17 27 37 47 57 67 77 87 97\n");
    printf("8 18 28 38 48 58 68 78 88 98\n");
    printf("9 19 29 39 49 59 69 79 89 99\n");
    printf("Array36: \n");
    t = 0;
    { long _forlim = 2;
    for (i = 1; i <= _forlim; i++) {

        { long _forlim = 2;
        for (x = 1; x <= _forlim; x++) {

            { long _forlim = 2;
            for (y = 1; y <= _forlim; y++) {

                { long _forlim = 2;
                for (z = 1; z <= _forlim; z++) {

                    { long _forlim = 2;
                    for (q = 1; q <= _forlim; q++) {

                        { long _forlim = 2;
                        for (n = 1; n <= _forlim; n++) {

                            mdar[i][x][y][z][q][n] = t;
                            t = t + 1;

                        }
                        }

                    }
                    }

                }
                }

            }
            }

        }
        }

    }
    }
    { long _forlim = 1;
    for (i = 2; i >= _forlim; i--) {

        { long _forlim = 1;
        for (x = 2; x >= _forlim; x--) {

            { long _forlim = 1;
            for (y = 2; y >= _forlim; y--) {

                { long _forlim = 1;
                for (z = 2; z >= _forlim; z--) {

                    { long _forlim = 1;
                    for (q = 2; q >= _forlim; q--) {

                        { long _forlim = 1;
                        for (n = 2; n >= _forlim; n--) {

                            printf("%2ld ", (long)(mdar[i][x][y][z][q][n]));

                        }
                        }

                    }
                    }

                }
                }
                printf("\n");

            }
            }

        }
        }

    }
    }
    printf("s/b:\n");
    printf("63 62 61 60 59 58 57 56\n");
    printf("55 54 53 52 51 50 49 48\n");
    printf("47 46 45 44 43 42 41 40\n");
    printf("39 38 37 36 35 34 33 32\n");
    printf("31 30 29 28 27 26 25 24\n");
    printf("23 22 21 20 19 18 17 16\n");
    printf("15 14 13 12 11 10  9  8\n");
    printf(" 7  6  5  4  3  2  1  0\n");

    printf("Array37: \n");
    memmove(&pavc[1],"hello, guy", 10);
    printf("%10s s/b hello, guy\n", &pavc[1]);
    printf("Array38: \n");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avi[i] = i + 10;

    }
    }
    memmove(avi2,avi, 80);
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(avi2[i]));

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array39: \n");
    t = 0;
    { long _forlim = 2;
    for (i = 1; i <= _forlim; i++) {

        { long _forlim = 2;
        for (x = 1; x <= _forlim; x++) {

            { long _forlim = 2;
            for (y = 1; y <= _forlim; y++) {

                { long _forlim = 2;
                for (z = 1; z <= _forlim; z++) {

                    { long _forlim = 2;
                    for (q = 1; q <= _forlim; q++) {

                        { long _forlim = 2;
                        for (n = 1; n <= _forlim; n++) {

                            mdar[i][x][y][z][q][n] = t;
                            t = t + 1;

                        }
                        }

                    }
                    }

                }
                }

            }
            }

        }
        }

    }
    }
    memmove(mdar2,mdar, 512);
    { long _forlim = 1;
    for (i = 2; i >= _forlim; i--) {

        { long _forlim = 1;
        for (x = 2; x >= _forlim; x--) {

            { long _forlim = 1;
            for (y = 2; y >= _forlim; y--) {

                { long _forlim = 1;
                for (z = 2; z >= _forlim; z--) {

                    { long _forlim = 1;
                    for (q = 2; q >= _forlim; q--) {

                        { long _forlim = 1;
                        for (n = 2; n >= _forlim; n--) {

                            printf("%2ld ", (long)(mdar2[i][x][y][z][q][n]));

                        }
                        }

                    }
                    }

                }
                }
                printf("\n");

            }
            }

        }
        }

    }
    }
    printf("s/b:\n");
    printf("63 62 61 60 59 58 57 56\n");
    printf("55 54 53 52 51 50 49 48\n");
    printf("47 46 45 44 43 42 41 40\n");
    printf("39 38 37 36 35 34 33 32\n");
    printf("31 30 29 28 27 26 25 24\n");
    printf("23 22 21 20 19 18 17 16\n");
    printf("15 14 13 12 11 10  9  8\n");
    printf(" 7  6  5  4  3  2  1  0\n");

    printf("Array40: \n");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavi[i] = i + 10;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(avi[i]));

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Array41: \n");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        avi[i] = i + 20;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(pavi[i]));

    }
    }
    printf("s/b 30 29 28 27 26 25 24 23 22 21\n");
    printf("Array42: \n");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        pavi[i] = i + 30;

    }
    }
    { long _forlim = 'g';
    for (ci = 'p'; ci >= _forlim; ci--) {

        printf("%ld ", (long)(cia[(unsigned char)ci]));
        if (ci == _forlim) break;

    }
    }
    printf("s/b 40 39 38 37 36 35 34 33 32 31\n");
    printf("Array43: \n");
    x = 1;
    { long _forlim = 'z';
    for (ci = 'a'; ci <= _forlim; ci++) {

        cia[(unsigned char)ci] = x;
        x = x + 1;
        if (ci == _forlim) break;

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(pavi[i]));

    }
    }
    printf("s/b 22 21 20 19 18 17 16 15 14 13\n");

    printf("\n");
    printf("******************* records ******************************\n");
    printf("\n");

    printf("Record1:   \n");
    arec.i = 64;
    arec.b = false;
    arec.c = 'j';
    arec.e = two;
    arec.es = four;
    arec.s = 12;
    arec.r = 4.545119999999999e-29;
    memmove(&arec.st[1],"what ? who", 10);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        arec.a[i] = i + 20;

    }
    }
    arec.rc.a = 2324;
    arec.rc.b = 'y';
    { p2c_settype _stmp1, _stmp2; p2c_scpy(arec.stc,(p2c_sclr(_stmp2), p2c_radd(_stmp2, 'b', 'e'), p2c_sadd(_stmp2, 'i'), _stmp2)); }
    arec.p = malloc(sizeof(long));
    *arec.p = 8454;
    printf("%ld %5s %c %ld %ld %ld %15g %10s\n", (long)(arec.i), (arec.b) ? "true" : "false", (int)(arec.c), (long)(arec.e), (long)(arec.es), (long)(arec.s), arec.r, &arec.st[1]);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        printf("%ld ", (long)(arec.a[i]));

    }
    }
    printf("\n");
    printf("%ld %c\n", (long)(arec.rc.a), (int)(arec.rc.b));
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (p2c_sisin(ci, arec.stc)) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf("\n");
    printf("%ld\n", (long)(*arec.p));
    printf("s/b:\n");
    printf("64 false j 1 3 12  4.54512000e-29 what ? who\n");
    printf("21 22 23 24 25 26 27 28 29 30\n");
    printf("2324 y\n");
    printf("_bcde___i_\n");
    printf("8454\n");
    printf("Record2:   \n");
    parec.i = 64;
    parec.b = false;
    parec.c = 'j';
    parec.e = two;
    parec.es = four;
    parec.s = 12;
    parec.r = 4.545119999999999e-29;
    memmove(&parec.st[1],"what ? who", 10);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        parec.a[i] = i + 20;

    }
    }
    parec.rc.a = 2324;
    parec.rc.b = 'y';
    { p2c_settype _stmp1, _stmp2; p2c_scpy(parec.stc,(p2c_sclr(_stmp2), p2c_radd(_stmp2, 'b', 'e'), p2c_sadd(_stmp2, 'i'), _stmp2)); }
    parec.p = malloc(sizeof(long));
    *parec.p = 8454;
    printf("%ld %5s %c %ld %ld %ld %15g %10s\n", (long)(parec.i), (parec.b) ? "true" : "false", (int)(parec.c), (long)(parec.e), (long)(parec.es), (long)(parec.s), parec.r, &parec.st[1]);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        printf("%ld ", (long)(parec.a[i]));

    }
    }
    printf("\n");
    printf("%ld %c\n", (long)(parec.rc.a), (int)(parec.rc.b));
    { long _forlim = 'j';
    for (ci = 'a'; ci <= _forlim; ci++) {

        if (p2c_sisin(ci, parec.stc)) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf("\n");
    printf("%ld\n", (long)(*parec.p));
    printf("s/b:\n");
    printf("64 false j 1 3 12  4.54512000e-29 what ? who\n");
    printf("21 22 23 24 25 26 27 28 29 30\n");
    printf("2324 y\n");
    printf("_bcde___i_\n");
    printf("8454\n");

    printf("Record3:   ");
    vra.i = 873;
    vra.vt = vti;
    vra.a = 427;
    vra.vdi = 235;
    printf("%ld %ld %ld %ld", (long)(vra.i), (long)(vra.vt), (long)(vra.vdi), (long)(vra.a));
    printf(" s/b 873 0 235 427\n");
    printf("Record4:   ");
    vra.i = 873;
    vra.vt = vtb;
    vra.b = 427;
    vra.vdb = true;
    printf("%ld %ld %5s %ld", (long)(vra.i), (long)(vra.vt), (vra.vdb) ? "true" : "false", (long)(vra.b));
    printf(" s/b 873 1  true 427\n");
    printf("Record5:   ");
    vra.i = 873;
    vra.vt = vtc;
    vra.c = 427;
    vra.vdc = 'f';
    printf("%ld %ld %c %ld", (long)(vra.i), (long)(vra.vt), (int)(vra.vdc), (long)(vra.c));
    printf(" s/b 873 2 f 427\n");
    printf("Record6:   ");
    vra.i = 873;
    vra.vt = vte;
    vra.d = 427;
    vra.vde = nine;
    printf("%ld %ld %ld %ld", (long)(vra.i), (long)(vra.vt), (long)(vra.vde), (long)(vra.d));
    printf(" s/b 873 3 8 427\n");
    printf("Record7:   ");
    vra.i = 873;
    vra.vt = vtes;
    vra.e = 427;
    vra.vdes = four;
    printf("%ld %ld %ld %ld", (long)(vra.i), (long)(vra.vt), (long)(vra.vdes), (long)(vra.e));
    printf(" s/b 873 4 3 427\n");
    printf("Record8:   ");
    vra.i = 873;
    vra.vt = vts;
    vra.f = 427;
    vra.vds = 12;
    printf("%ld %ld %ld %ld", (long)(vra.i), (long)(vra.vt), (long)(vra.vds), (long)(vra.f));
    printf(" s/b 873 5 12 427\n");
    printf("Record9:   ");
    vra.i = 873;
    vra.vt = vtr;
    vra.g = 427;
    vra.vdr = 8.734838900000001e+3;
    printf("%ld %ld %.4f %ld", (long)(vra.i), (long)(vra.vt), vra.vdr, (long)(vra.g));
    printf(" s/b 873 6 8734.8389 427\n");
    printf("Record10:  ");
    vra.i = 873;
    vra.vt = vtst;
    vra.h = 427;
    memmove(&vra.vdst[1],"this one ?", 10);
    printf("%ld %ld %10s %ld", (long)(vra.i), (long)(vra.vt), &vra.vdst[1], (long)(vra.h));
    printf(" s/b 873 7 this one ? 427\n");
    printf("Record11:  ");
    vra.i = 873;
    vra.vt = vta;
    vra.j = 427;
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        vra.vda[i] = i + 10;

    }
    }
    printf("%ld %ld ", (long)(vra.i), (long)(vra.vt));
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        printf("%ld ", (long)(vra.vda[i]));

    }
    }
    printf("%ld\n", (long)(vra.j));
    printf("      s/b:  873 8 20 19 18 17 16 15 14 13 12 11 427\n");
    printf("Record12:  ");
    vra.i = 873;
    vra.vt = vtrc;
    vra.k = 427;
    vra.vdrc.a = 2387;
    vra.vdrc.b = 't';
    printf("%ld %ld %ld %c %ld", (long)(vra.i), (long)(vra.vt), (long)(vra.vdrc.a), (int)(vra.vdrc.b), (long)(vra.k));
    printf(" s/b:  873 9 2387 t 427\n");
    printf("Record13:  ");
    vra.i = 873;
    vra.vt = vtstc;
    vra.l = 427;
    { p2c_settype _stmp1, _stmp2; p2c_scpy(vra.vdstc,(p2c_sclr(_stmp2), p2c_radd(_stmp2, 'b', 'g'), p2c_sadd(_stmp2, 'i'), _stmp2)); }
    printf("%ld %ld ", (long)(vra.i), (long)(vra.vt));
    { long _forlim = 'a';
    for (ci = 'j'; ci >= _forlim; ci--) {

        if (p2c_sisin(ci, vra.vdstc)) {

            printf("%c", (int)(ci));

        } else {

            printf("_");

        }
        if (ci == _forlim) break;

    }
    }
    printf(" %ld\n", (long)(vra.l));
    printf("      s/b:  873 10 _i_gfedcb_ 427\n");
    printf("Record14:  ");
    vra.i = 873;
    vra.vt = vtp;
    vra.m = 427;
    vra.vdp = malloc(sizeof(long));
    *vra.vdp = 2394;
    printf("%ld %ld %ld %ld", (long)(vra.i), (long)(vra.vt), (long)(*vra.vdp), (long)(vra.m));
    printf(" s/b 873 11 2394 427\n");

    printf("Record15:  ");
    vvrs.vt = 10;
    vvrs.vi = 2343;
    printf("%ld %ld", (long)(vvrs.vt), (long)(vvrs.vi));
    printf(" s/b 10 2343\n");
    printf("Record16:  ");
    vvrs.vt = 19;
    vvrs.vb = true;
    printf("%ld %5s", (long)(vvrs.vt), (vvrs.vb) ? "true" : "false");
    printf(" s/b 19  true\n");
    printf("Record17:  ");
    vvrb.vt = true;
    vvrb.vi = 2343;
    printf("%5s %ld", (vvrb.vt) ? "true" : "false", (long)(vvrb.vi));
    printf(" s/b  true 2343\n");
    printf("Record18:  ");
    vvrb.vt = false;
    vvrb.vb = true;
    printf("%5s %5s", (vvrb.vt) ? "true" : "false", (vvrb.vb) ? "true" : "false");
    printf(" s/b false  true\n");
    printf("Record19:  ");
    vvre.vt = three;
    vvre.vi = 2343;
    printf("%ld %ld", (long)(vvre.vt), (long)(vvre.vi));
    printf(" s/b 2 2343\n");
    printf("Record20:  ");
    vvre.vt = eight;
    vvre.vb = true;
    printf("%ld %5s", (long)(vvre.vt), (vvre.vb) ? "true" : "false");
    printf(" s/b 7  true\n");
    printf("Record21:  ");
    vvres.vt = four;
    vvres.vi = 2343;
    printf("%ld %ld", (long)(vvres.vt), (long)(vvres.vi));
    printf(" s/b 3 2343\n");
    printf("Record22:  ");
    vvres.vt = five;
    vvres.vb = true;
    printf("%ld %5s", (long)(vvres.vt), (vvres.vb) ? "true" : "false");
    printf(" s/b 4  true\n");

    printf("Record23:  ");
    vvrs.vt = 10;
    vvrs.vi = 42;
    i = vvrs.vi;
    vvrs.vt = 11;
    i = vvrs.vi;
    printf("%ld s/b 42\n", (long)(i));

    printf("Record24:  ");
    nvr.i = 1;
    nvr.r.i = 2;
    nvr.r.r.i = 3;
    nvr.r.r.r.i = 4;
    nvr.r.r.r.r.i = 5;
    nvr.r.r.r.r.r.i = 6;
    nvr.r.r.r.r.r.r.i = 7;
    nvr.r.r.r.r.r.r.r.i = 8;
    nvr.r.r.r.r.r.r.r.r.i = 9;
    nvr.r.r.r.r.r.r.r.r.r.i = 10;
    printf("%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld s/b 1 2 3 4 5 6 7 8 9 10\n", (long)(nvr.i), (long)(nvr.r.i), (long)(nvr.r.r.i), (long)(nvr.r.r.r.i), (long)(nvr.r.r.r.r.i), (long)(nvr.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.r.r.i));

    printf("Record25:  ");
    { nvr_t *_with1 = &(nvr);
    _with1->i = 10;
    { _rec_9 *_with2 = &(_with1->r);
    _with2->i = 9;
    { _rec_8 *_with3 = &(_with2->r);
    _with3->i = 8;
    { _rec_7 *_with4 = &(_with3->r);
    _with4->i = 7;
    { _rec_6 *_with5 = &(_with4->r);
    _with5->i = 6;
    { _rec_5 *_with6 = &(_with5->r);
    _with6->i = 5;
    { _rec_4 *_with7 = &(_with6->r);
    _with7->i = 4;
    { _rec_3 *_with8 = &(_with7->r);
    _with8->i = 3;
    { _rec_2 *_with9 = &(_with8->r);
    _with9->i = 2;
    { _rec_1 *_with10 = &(_with9->r);
    _with10->i = 2;
    { _rec_1 *_with11 = &(_with9->r);
    _with11->i = 1;
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    printf("%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld s/b 10 9 8 7 6 5 4 3 2 1\n", (long)(nvr.i), (long)(nvr.r.i), (long)(nvr.r.r.i), (long)(nvr.r.r.r.i), (long)(nvr.r.r.r.r.i), (long)(nvr.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.r.r.i));
    printf("Record26:  ");
    { nvr_t *_with1 = &(nvr);
    { _rec_9 *_with2 = &(_with1->r);
    { _rec_8 *_with3 = &(_with2->r);
    { _rec_7 *_with4 = &(_with3->r);
    { _rec_6 *_with5 = &(_with4->r);
    { _rec_5 *_with6 = &(_with5->r);
    { _rec_4 *_with7 = &(_with6->r);
    { _rec_3 *_with8 = &(_with7->r);
    { _rec_2 *_with9 = &(_with8->r);
    { _rec_1 *_with10 = &(_with9->r);
    _with10->i = 76;
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    printf("%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld s/b 10 9 8 7 6 5 4 3 2 76\n", (long)(nvr.i), (long)(nvr.r.i), (long)(nvr.r.r.i), (long)(nvr.r.r.r.i), (long)(nvr.r.r.r.r.i), (long)(nvr.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.r.i), (long)(nvr.r.r.r.r.r.r.r.r.r.i));
    printf("Record27:  ");
    rpa = malloc(sizeof(rec));
    { rec *_with1 = &(*rpa);
    _with1->i = 1;
    { recs *_with2 = &(_with1->rc);
    _with2->b = 'g';
    }
    }
    printf("%ld %c s/b 1 g\n", (long)(rpa->i), (int)(rpa->rc.b));
    printf("Record28:  ");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        { recs *_with1 = &(ara[i]);
        _with1->a = i + 10;
        }

    }
    }
    { long _forlim = 1;
    for (i = 10; i >= _forlim; i--) {

        { recs *_with1 = &(ara[i]);
        printf("%ld ", (long)(_with1->a));
        }

    }
    }
    printf("s/b 20 19 18 17 16 15 14 13 12 11\n");
    printf("Record29: ");
    rpb = malloc(sizeof(recvb));
    rpb->i = 42;
    rpb->b = false;
    rpb->q = true;
    rpb->r = 1.233999999999999e+1;
    printf("%ld %5s %5s %22g", (long)(rpb->i), (rpb->b) ? "true" : "false", (rpb->q) ? "true" : "false", rpb->r);
    printf(" s/b 42 False True 1.234000000000000e+01\n");
    free(rpb);
    printf("Record30: ");
    rpc = malloc(sizeof(recvc));
    rpc->vt = 10;
    rpc->vi = 185;
    rpc->vt = 14;
    printf("%ld", (long)(rpc->vi));
    printf(" s/b 185\n");
    free(rpc);

    if (true) {

        printf("\n");
        printf("******************* files ******************************\n");
        printf("\n");

        printf("File1:   ");
        p2c_frewrite(&fi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(long*)fi.buf) = i + 10;
            p2c_fput(&fi);

        }
        }
        p2c_freset(&fi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            x = (*(long*)fi.buf);
            p2c_fget(&fi);
            printf("%ld ", (long)(x));

        }
        }
        printf("s/b 11 12 13 14 15 16 17 18 19 20\n");
        printf("File2:   ");
        p2c_frewrite(&pfi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(long*)pfi.buf) = i + 10;
            p2c_fput(&pfi);

        }
        }
        p2c_freset(&pfi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            x = (*(long*)pfi.buf);
            p2c_fget(&pfi);
            printf("%ld ", (long)(x));

        }
        }
        printf("s/b 11 12 13 14 15 16 17 18 19 20\n");
        printf("File3:   ");
        p2c_frewrite(&fb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(bool*)fb.buf) = ((i) & 1);
            p2c_fput(&fb);

        }
        }
        p2c_freset(&fb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            ba = (*(bool*)fb.buf);
            p2c_fget(&fb);
            printf("%5s ", (ba) ? "true" : "false");

        }
        }
        printf("\n");
        printf("   s/b:    true false  true false  true false  true false  true false\n");
        printf("File4:   ");
        p2c_frewrite(&pfb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(bool*)pfb.buf) = ((i) & 1);
            p2c_fput(&pfb);

        }
        }
        p2c_freset(&pfb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            ba = (*(bool*)pfb.buf);
            p2c_fget(&pfb);
            printf("%5s ", (ba) ? "true" : "false");

        }
        }
        printf("\n");
        printf("   s/b:    true false  true false  true false  true false  true false\n");
        printf("File5:   ");
        p2c_frewrite(&fc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            (*(char*)fc.buf) = ci;
            p2c_fput(&fc);
            if (ci == _forlim) break;

        }
        }
        p2c_freset(&fc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            ca = (*(char*)fc.buf);
            p2c_fget(&fc);
            printf("%c ", (int)(ca));
            if (ci == _forlim) break;

        }
        }
        printf("s/b a b c d e f g h i j\n");
        printf("File6:   ");
        p2c_frewrite(&pfc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            (*(char*)pfc.buf) = ci;
            p2c_fput(&pfc);
            if (ci == _forlim) break;

        }
        }
        p2c_freset(&pfc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            ca = (*(char*)pfc.buf);
            p2c_fget(&pfc);
            printf("%c ", (int)(ca));
            if (ci == _forlim) break;

        }
        }
        printf("s/b a b c d e f g h i j\n");
        printf("File7:   ");
        p2c_frewrite(&fe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            (*(int*)fe.buf) = ei;
            p2c_fput(&fe);

        }
        }
        p2c_freset(&fe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            ea = (*(int*)fe.buf);
            p2c_fget(&fe);
            printf("%ld ", (long)(ea));

        }
        }
        printf("s/b 0 1 2 3 4 5 6 7 8 9\n");
        printf("File8:   ");
        p2c_frewrite(&pfe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            (*(int*)pfe.buf) = ei;
            p2c_fput(&pfe);

        }
        }
        p2c_freset(&pfe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            ea = (*(int*)pfe.buf);
            p2c_fget(&pfe);
            printf("%ld ", (long)(ea));

        }
        }
        printf("s/b 0 1 2 3 4 5 6 7 8 9\n");

        printf("File9:\n");
        ft = tmpfile();
        x = 7384;
        printf("%ld\n", (long)(x));
        printf("%ld\n", (long)(8342));
        ba = true;
        printf("%5s\n", (ba) ? "true" : "false");
        printf("%5s\n", (false) ? "true" : "false");
        ca = 'm';
        printf("%c\n", (int)(ca));
        printf("q\n");
        ra = 1.234567799999999;
        printf("%15g\n", ra);
        printf("%.7f\n", ra);
        printf("%15g\n", 5.689432099999999e+1);
        printf("%.8f\n", 9.3837632e-1);
        memmove(&s[1],"hi there !", 10);
        printf("%10s\n", &s[1]);
        printf("%10s\n", &s[1]);
        printf("%10s\n", &s[1]);
        rewind(ft);
        fgetc(ft);
        cc = ({int _c=getc(ft);ungetc(_c,ft);_c;});
        rewind(ft);
        while (!feof(ft)) {

            if (p2c_eoln(ft)) {

                fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
                printf("\n");

            } else {

                p2c_readc(ft, ci);
                printf("%c", (int)(ci));

            }

        }
        printf("s/b:\n");
        printf("7384\n");
        printf("8342\n");
        printf(" true\n");
        printf("false\n");
        printf("m\n");
        printf("q\n");
        printf(" 1.2345678000e+00\n");
        printf("1.2345678\n");
        printf(" 5.6894321000e+01\n");
        printf("0.93837632\n");
        printf("hi there !\n");
        printf("hi th\n");
        printf("     hi there !\n");

        printf("file10:\n");
        rewind(ft);
        fscanf(ft, "%ld", &y);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%ld\n", (long)(y));
        fscanf(ft, "%ld", &y);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%ld\n", (long)(y));
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        p2c_readc(ft, ci);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%c\n", (int)(ci));
        p2c_readc(ft, ci);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%c\n", (int)(ci));
        fscanf(ft, "%lg", &rb);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%15g\n", rb);
        fscanf(ft, "%lg", &rb);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%15g\n", rb);
        fscanf(ft, "%lg", &rb);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%15g\n", rb);
        fscanf(ft, "%lg", &rb);
        fscanf(ft, "%*[^\n]"); fscanf(ft, "%*c");
        printf("%15g\n", rb);
        printf("s/b:\n");
        printf("7384\n");
        printf("8342\n");
        printf("m\n");
        printf("q\n");
        printf(" 1.2345678000e+00\n");
        printf(" 1.2345678000e+00\n");
        printf(" 5.6894321000e+01\n");
        printf(" 9.3837632000e-01\n");

        printf("file11:\n");
        ft = tmpfile();
        printf("how now\n");
        printf("brown cow\n");
        rewind(ft);
        printf("'");
        while (!feof(ft)) {

            if (p2c_eoln(ft)) {

                printf("<eoln>");

            }
            p2c_readc(ft, ca);
            printf("%c", (int)(ca));

        }
        printf("'");
        printf(" s/b 'how now<eoln> brown cow<eoln> '\n");
        printf("file12:\n");
        ft = tmpfile();
        printf("too much\n");
        printf("too soon");
        rewind(ft);
        printf("'");
        while (!feof(ft)) {

            if (p2c_eoln(ft)) {

                printf("<eoln>");

            }
            p2c_readc(ft, ca);
            printf("%c", (int)(ca));

        }
        printf("'");
        printf(" s/b 'too much<eoln> too soon<eoln> '\n");

        printf("File13:   ");
        p2c_frewrite(&fi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(long*)fi.buf) = i + 10;
            p2c_fput(&fi);

        }
        }
        p2c_freset(&fi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            x = (*(long*)fi.buf);
            p2c_fget(&fi);
            printf("%ld ", (long)(x));

        }
        }
        printf("s/b 11 12 13 14 15 16 17 18 19 20\n");
        printf("File14:   ");
        p2c_frewrite(&pfi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(long*)pfi.buf) = i + 10;
            p2c_fput(&pfi);

        }
        }
        p2c_freset(&pfi, sizeof(long));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            x = (*(long*)pfi.buf);
            p2c_fget(&pfi);
            printf("%ld ", (long)(x));

        }
        }
        printf("s/b 11 12 13 14 15 16 17 18 19 20\n");
        printf("File15:   ");
        p2c_frewrite(&fb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(unsigned char*)fb.buf) = ((i) & 1);
            p2c_fput(&fb);

        }
        }
        p2c_freset(&fb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            ba = (*(unsigned char*)fb.buf);
            p2c_fget(&fb);
            printf("%5s ", (ba) ? "true" : "false");

        }
        }
        printf("\n");
        printf("   s/b:    true false  true false  true false  true false  true false\n");
        printf("File16:   ");
        p2c_frewrite(&pfb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            (*(unsigned char*)pfb.buf) = ((i) & 1);
            p2c_fput(&pfb);

        }
        }
        p2c_freset(&pfb, sizeof(bool));
        { long _forlim = 10;
        for (i = 1; i <= _forlim; i++) {

            ba = (*(unsigned char*)pfb.buf);
            p2c_fget(&pfb);
            printf("%5s ", (ba) ? "true" : "false");

        }
        }
        printf("\n");
        printf("   s/b:    true false  true false  true false  true false  true false\n");
        printf("File17:   ");
        p2c_frewrite(&fc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            (*(char*)fc.buf) = ci;
            p2c_fput(&fc);
            if (ci == _forlim) break;

        }
        }
        p2c_freset(&fc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            ca = (*(char*)fc.buf);
            p2c_fget(&fc);
            printf("%c ", (int)(ca));
            if (ci == _forlim) break;

        }
        }
        printf("s/b a b c d e f g h i j\n");
        printf("File18:   ");
        p2c_frewrite(&pfc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            (*(char*)pfc.buf) = ci;
            p2c_fput(&pfc);
            if (ci == _forlim) break;

        }
        }
        p2c_freset(&pfc, sizeof(char));
        { long _forlim = 'j';
        for (ci = 'a'; ci <= _forlim; ci++) {

            ca = (*(char*)pfc.buf);
            p2c_fget(&pfc);
            printf("%c ", (int)(ca));
            if (ci == _forlim) break;

        }
        }
        printf("s/b a b c d e f g h i j\n");
        printf("File19:   ");
        p2c_frewrite(&fe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            (*(int*)fe.buf) = ei;
            p2c_fput(&fe);

        }
        }
        p2c_freset(&fe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            ea = (*(int*)fe.buf);
            p2c_fget(&fe);
            printf("%ld ", (long)(ea));

        }
        }
        printf("s/b 0 1 2 3 4 5 6 7 8 9\n");
        printf("File20:   ");
        p2c_frewrite(&pfe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            (*(int*)pfe.buf) = ei;
            p2c_fput(&pfe);

        }
        }
        p2c_freset(&pfe, sizeof(int));
        { long _forlim = ten;
        for (ei = one; ei <= _forlim; ei++) {

            ea = (*(int*)pfe.buf);
            p2c_fget(&pfe);
            printf("%ld ", (long)(ea));

        }
        }
        printf("s/b 0 1 2 3 4 5 6 7 8 9\n");
        printf("File21:   ");
        ft = tmpfile();
        printf("50\n");
        rewind(ft);
        fscanf(ft, "%hhu", &srx);
        printf("%ld", (long)(srx));
        printf(" s/b %ld\n", (long)(50));
        printf("File22:   ");
        ft = tmpfile();
        printf("%5s s/b true\n", (feof(ft)) ? "true" : "false");

    }

    printf("\n");
    printf("************ Procedures and functions ******************\n");
    printf("\n");
    printf("ProcedureFunction1:   ");
    x = 45;
    y = 89;
    junk1(x, y);
    printf(" s/b 45 89\n");
    printf("ProcedureFunction2:   ");
    x = 45;
    junk2(&x);
    printf("%ld s/b 46\n", (long)(x));
    printf("ProcedureFunction3:   ");
    memmove(&s[1],"total junk", 10);
    junk3(s);
    printf(" s/b total junk\n");
    printf("ProcedureFunction4:   ");
    memmove(&s[1],"total junk", 10);
    junk4(s);
    printf(" s/b tota? junk\n");
    printf("                      %10s s/b total junk\n", &s[1]);
    printf("ProcedureFunction5:   ");
    printf("%ld s/b 35\n", (long)(junk5(34)));
    printf("ProcedureFunction6:   ");
    i = junk7(10, 9, 8);
    printf(" %ld\n", (long)(i));
    printf("s/b:   10 9 8 6 5 4 3 2 1 78\n");
    printf("ProcedureFunction7:\n");
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        ai[i] = i + 10;

    }
    }
    arec.i = 64;
    arec.b = false;
    arec.c = 'j';
    arec.e = two;
    arec.es = four;
    arec.s = 12;
    arec.r = 4.545119999999999e-29;
    memmove(&arec.st[1],"what ? who", 10);
    { long _forlim = 10;
    for (i = 1; i <= _forlim; i++) {

        arec.a[i] = i + 20;

    }
    }
    arec.rc.a = 2324;
    arec.rc.b = 'y';
    { p2c_settype _stmp1, _stmp2; p2c_scpy(arec.stc,(p2c_sclr(_stmp2), p2c_radd(_stmp2, 'b', 'e'), p2c_sadd(_stmp2, 'i'), _stmp2)); }
    arec.p = malloc(sizeof(long));
    *arec.p = 8454;
    vrec.a = 23487;
    vrec.b = 'n';
    vrec.c = false;
    memmove(&vrec.d[1],"help me123", 10);
    ip = malloc(sizeof(long));
    *ip = 734;
    { p2c_settype _stmp1, _stmp2; junk8(93, true, 'k', eight, five, 10, 3.141399999999999, "\0hello, guy", ai, arec, vrec, (p2c_sclr(_stmp2), p2c_radd(_stmp2, 'a', 'd'), p2c_sadd(_stmp2, 'h'), _stmp2), ip); }
    printf("s/b:\n");
    printf("93  true k 7 4 10  3.14140000e+00 hello, guy\n");
    printf("11 12 13 14 15 16 17 18 19 20\n");
    printf("64 false j 1 3 12  4.54512000e-29 what ? who\n");
    printf("21 22 23 24 25 26 27 28 29 30\n");
    printf("2324 y\n");
    printf("_bcde___i_\n");
    printf("8454\n");
    printf("23487 n false\n");
    printf("help me123\n");
    printf("abcd___h__\n");
    printf("734\n");
    printf("ProcedureFunction8:   ");
    junk9(junk10, junk11);
    printf(" s/b 9834 8383 j 744\n");
    printf("ProcedureFunction9:   ");
    junk12(junk13, junk11);
    printf(" s/b 942\n");
    printf("ProcedureFunction10:   ");
    junk14();
    printf(" s/b 62 76\n");
    printf("ProcedureFunction11:   ");
    junk17(junk16, 52);
    printf(" s/b 52\n");
    printf("ProcedureFunction12:   ");
    junk19();
    printf(" s/b a\n");
    printf("ProcedureFunction13:   ");
    printf("%ld s/b 37\n", (long)(junk20()));
    printf("ProcedureFunction14:   ");
    printf("%ld s/b 35\n", (long)(junk21()));

}
