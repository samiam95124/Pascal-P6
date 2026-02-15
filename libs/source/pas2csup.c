/*
 * pas2csup.c - Runtime support for p2c translated programs
 *
 * Set operations based on routines from source/cmach/cmach.c.
 * Pascaline sets are 256 bits stored as 32 unsigned char bytes.
 *
 * Typed file I/O using binary fread/fwrite with tmpfile().
 */

#include "../pas2csup.h"

/* clear set to empty */
void p2c_sclr(p2c_settype s)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++) s[i] = 0;
}

/* set single element */
void p2c_sset(p2c_settype s, int b)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++) s[i] = 0;
    s[b / 8] |= 1 << b % 8;
}

/* set range of elements */
void p2c_rset(p2c_settype s, int b1, int b2)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++) s[i] = 0;
    if (b1 > b2) { i = b1; b1 = b2; b2 = i; }
    for (i = b1; i <= b2; i++) s[i / 8] |= 1 << i % 8;
}

/* add element to existing set (does not clear first) */
void p2c_sadd(p2c_settype s, int b)
{
    s[b / 8] |= 1 << b % 8;
}

/* add range of elements without clearing first */
void p2c_radd(p2c_settype s, int b1, int b2)
{
    int i;

    if (b1 > b2) { i = b1; b1 = b2; b2 = i; }
    for (i = b1; i <= b2; i++) s[i / 8] |= 1 << i % 8;
}

/* set union: s1 = s1 | s2 */
void p2c_suni(p2c_settype s1, const p2c_settype s2)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++) s1[i] = s1[i] | s2[i];
}

/* set intersection: s1 = s1 & s2 */
void p2c_sint(p2c_settype s1, const p2c_settype s2)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++) s1[i] = s1[i] & s2[i];
}

/* set difference: s1 = s1 & ~s2 */
void p2c_sdif(p2c_settype s1, const p2c_settype s2)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++) s1[i] = s1[i] & ~s2[i];
}

/* copy set */
void p2c_scpy(p2c_settype dst, const p2c_settype src)
{
    memmove(dst, src, P2C_SETSIZE);
}

/* test set membership */
int p2c_sisin(int i, const p2c_settype s)
{
    return !!(s[i / 8] & (1 << i % 8));
}

/* test set equality */
int p2c_sequ(const p2c_settype s1, const p2c_settype s2)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++)
        if (s1[i] != s2[i]) return 0;
    return 1;
}

/* test set inclusion: s2 is subset of s1 */
int p2c_sinc(const p2c_settype s1, const p2c_settype s2)
{
    int i;

    for (i = 0; i < P2C_SETSIZE; i++)
        if ((s1[i] & s2[i]) != s2[i]) return 0;
    return 1;
}

/*
 * Typed (binary) file support
 */

/* open typed file for writing */
void p2c_frewrite(p2c_file *f, size_t elsize)
{
    if (f->f) fclose(f->f);
    f->f = tmpfile();
    f->elsize = elsize;
    if (!f->buf) f->buf = malloc(elsize);
    memset(f->buf, 0, elsize);
    f->eof = 1; /* empty file, eof is true */
}

/* open typed file for reading (rewind + prime buffer) */
void p2c_freset(p2c_file *f, size_t elsize)
{
    f->elsize = elsize;
    if (!f->buf) f->buf = malloc(elsize);
    rewind(f->f);
    /* prime buffer with first element (Pascal reset semantics) */
    if (fread(f->buf, elsize, 1, f->f) < 1)
        f->eof = 1;
    else
        f->eof = 0;
}

/* write buffer to file and advance (put) */
void p2c_fput(p2c_file *f)
{
    fwrite(f->buf, f->elsize, 1, f->f);
}

/* read next element into buffer (get) */
void p2c_fget(p2c_file *f)
{
    if (fread(f->buf, f->elsize, 1, f->f) < 1)
        f->eof = 1;
}

/* write value to file: copy to buffer then put */
void p2c_fwrite(p2c_file *f, const void *val, size_t sz)
{
    memcpy(f->buf, val, sz);
    p2c_fput(f);
}

/* read value from file: copy from buffer then get */
void p2c_fread(p2c_file *f, void *val, size_t sz)
{
    memcpy(val, f->buf, sz);
    p2c_fget(f);
}

/* test end of file */
int p2c_feof(p2c_file *f)
{
    return f->eof;
}
