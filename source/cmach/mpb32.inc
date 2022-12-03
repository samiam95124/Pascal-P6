/*******************************************************************************

32 bit Machine Parameter block (MPB)

*******************************************************************************/

#ifdef LENDIAN
#define LENDIAN 1 /* endian mode */
#else
#define LENDIAN 0 /* endian mode */
#endif
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

  -4: Static link.
  -8: Dynamic link.
  -12: Saved EP from previous frame.
  -16: Stack bottom after locals allocate. Used for interprocdural gotos.
  -20: EP from current frame. Used for interprocedural gotos.
  -24: Return address

*/
#define MARKSL              -4  /* static link */
#define MARKDL              -8  /* dynamic link */
#define MARKEP              -12 /* (old) maximum frame size */
#define MARKSB              -16 /* stack bottom */
#define MARKET              -20 /* current ep */
#define MARKRA              -24 /* return address */