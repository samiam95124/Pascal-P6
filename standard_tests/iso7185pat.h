/*
 * Header for iso7185pat
 */

#ifndef iso7185pat_H
#define iso7185pat_H

#include <stdbool.h>

typedef struct { void (*fn)(); void *ctx; } p2c_procptr;

typedef char string10[12];
typedef long arri[11];typedef struct rec rec;typedef struct recv recv;typedef long* iptr;
void junk1(long z, long q);void junk2(long* z);void junk3(string10 p);void junk4(string10 p_);long junk5(long x);void junk6(void);long junk7(long a, long b, long c);void junk8(long a, bool b, char c, int e, unsigned char es, unsigned char s, double r, string10 st_, arri ar_, rec rc, recv rv, p2c_settype stc, iptr p);void junk9(p2c_procptr junk9, p2c_procptr y);void junk10(long x, long y, char junk10);long junk11(long x);void junk12(p2c_procptr xq, p2c_procptr q);void junk13(p2c_procptr xz);void junk14(void);void junk16(void);void junk17(p2c_procptr x, long i);void junk19(void);long junk20(void);iptr frp(void);long random_(long low, long hi);long junk21(void);
#endif /* iso7185pat_H */
