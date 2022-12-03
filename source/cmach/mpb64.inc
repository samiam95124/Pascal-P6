/*******************************************************************************

64 bit Machine Parameter block (MPB)

*******************************************************************************/

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
