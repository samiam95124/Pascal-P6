/*
 * pas2csup.h - Runtime support for p2c translated programs
 *
 * Set operations: Pascaline sets are 256 bits (32 bytes). Each element 0..255
 * is represented by a single bit: byte index = element / 8, bit index = element % 8.
 *
 * Typed file I/O: binary file support using fread/fwrite with tmpfile().
 */

#ifndef PAS2CSUP_H
#define PAS2CSUP_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BIT(x) (1UL << x)
#define P2C_SETSIZE 32
typedef unsigned char p2c_settype[P2C_SETSIZE];

/* construction */
void p2c_sclr(p2c_settype s);                              /* empty set */
void p2c_sset(p2c_settype s, int b);                       /* singleton {b} */
void p2c_rset(p2c_settype s, int b1, int b2);              /* range {b1..b2} */
void p2c_sadd(p2c_settype s, int b);                       /* add element to existing set */
void p2c_radd(p2c_settype s, int b1, int b2);              /* add range without clearing */

/* binary operations (in-place: s1 = s1 op s2) */
void p2c_suni(p2c_settype s1, const p2c_settype s2);      /* union (+) */
void p2c_sint(p2c_settype s1, const p2c_settype s2);      /* intersection (*) */
void p2c_sdif(p2c_settype s1, const p2c_settype s2);      /* difference (-) */

/* assignment */
void p2c_scpy(p2c_settype dst, const p2c_settype src);    /* copy */

/* tests */
int p2c_sisin(int i, const p2c_settype s);                 /* i in s */
int p2c_sequ(const p2c_settype s1, const p2c_settype s2); /* s1 = s2 */
int p2c_sinc(const p2c_settype s1, const p2c_settype s2); /* s2 <= s1 (s2 subset of s1) */

/* Typed (binary) file support */
typedef struct {
    FILE *f;         /* underlying tmpfile handle */
    void *buf;       /* element buffer (malloc'd, size = elsize) */
    size_t elsize;   /* element size in bytes */
    int eof;         /* end-of-file flag */
} p2c_file;

void p2c_frewrite(p2c_file *f, size_t elsize);
void p2c_freset(p2c_file *f, size_t elsize);
void p2c_fput(p2c_file *f);
void p2c_fget(p2c_file *f);
void p2c_fwrite(p2c_file *f, const void *val, size_t sz);
void p2c_fread(p2c_file *f, void *val, size_t sz);
int  p2c_feof(p2c_file *f);

#endif /* PAS2CSUP_H */
