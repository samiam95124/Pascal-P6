/******************************************************************************

16 bit Machine Parameter block (MPB)

******************************************************************************/

#ifdef LENDIAN
#define LENDIAN 1 /* endian mode */
#else
#define LENDIAN 0 /* endian mode */
#endif
#define INTSIZE             2   /* size of integer */
#define INTAL               2   /* alignment of integer */
#define INTDIG              6  /* number of decimal digits in integer */
#define INTHEX              4   /* number of hex digits of integer */
#define REALSIZE            4   /* size of real */
#define REALAL              4   /* alignment of real */
#define CHARSIZE            1   /* size of char */
#define CHARAL              1   /* alignment of char */
#define CHARMAX             1
#define BOOLSIZE            1   /* size of boolean */
#define BOOLAL              1   /* alignment of boolean */
#define PTRSIZE             2   /* size of pointer */
#define ADRSIZE             2   /* size of address */
#define ADRAL               2   /* alignment of address */
#define SETSIZE            32   /* size of set */
#define SETAL               1   /* alignment of set */
#define FILESIZE            1   /* required runtime space for file (lfn) */
#define FILEIDSIZE          1   /* size of the lfn only */
#define EXCEPTSIZE          1   /* size of exception variable */
#define EXCEPTAL            1
#define STACKAL             2   /* alignment of stack */
#define STACKELSIZE         2   /* stack element size */
#define MAXSIZE       SETSIZE   /* this is the largest type that can be on the
                                   stack */
/* Heap alignment should be either the natural word alignment of the
  machine, or the largest object needing alignment that will be allocated.
  It can also be used to enforce minimum block allocation policy. */
#define HEAPAL              2   /* alignment for each heap arena */
#define GBSAL               2   /* globals area alignment */
#define SETHIGH           255   /* Sets are 256 values */
#define SETLOW              0
#define ORDMAXCHAR        255   /* Characters are 8 bit ISO/IEC 8859-1 */
#define ORDMINCHAR          0
#define MAXRESULT    REALSIZE   /* maximum size of function result */
#define MARKSIZE           12   /* maxresult+6*ptrsize */
#define UJPLEN              3   /* length of ujp instruction (used for case
                                   jumps) */

/* Value of nil is 1 because this allows checks for pointers that were
  initialized, which would be zero (since we clear all space to zero).
  In the new unified code/data space scheme, 0 and 1 are always invalid
  addresses, since the startup code is at least that long. */
#define NILVAL              1  /* value of 'nil' */

/* Mark element offsets

  Mark format is:

  -2: Static link.
  -4: Dynamic link.
  -6: Saved EP from previous frame.
  -8: Stack bottom after locals allocate. Used for interprocdural gotos.
  -10: EP from current frame. Used for interprocedural gotos.
  -12: Return address

*/
#define MARKSL              -2  /* static link */
#define MARKDL              -4  /* dynamic link */
#define MARKEP              -6 /* (old) maximum frame size */
#define MARKSB              -8 /* stack bottom */
#define MARKET              -10 /* current ep */
#define MARKRA              -12 /* return address */