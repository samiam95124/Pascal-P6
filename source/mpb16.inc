{******************************************************************************

16 bit Machine Parameter block (MPB)

******************************************************************************}

#ifdef LENDIAN
      lendian     =     true;   { endian mode }
#else
      lendian     =    false;   { endian mode }
#endif
      intsize     =        2;   { size of integer }
      intal       =        2;   { alignment of integer }
      intdig      =        6;   { number of decimal digits in integer }
      inthex      =        4;   { number of hex digits of integer }
      realsize    =        4;   { size of real }
      realal      =        4;   { alignment of real }
      charsize    =        1;   { size of char }
      charal      =        1;   { alignment of char }
      charmax     =        1;
      boolsize    =        1;   { size of boolean }
      boolal      =        1;   { alignment of boolean }
      ptrsize     =        2;   { size of pointer }
      adrsize     =        2;   { size of address }
      adral       =        2;   { alignment of address }
      setsize     =       32;   { size of set }
      setal       =        1;   { alignment of set }
      filesize    =        1;   { required runtime space for file (lfn) }
      fileidsize  =        1;   { size of the lfn only }
      exceptsize  =        1;   { size of exception variable }
      exceptal    =        1;
      stackal     =        2;   { alignment of stack }
      stackelsize =        2;   { stack element size }
      maxsize     =  setsize;   { this is the largest type that can be on the
                                  stack, set }
      { Heap alignment should be either the natural word alignment of the
        machine, or the largest object needing alignment that will be allocated.
        It can also be used to enforce minimum block allocation policy. }
      heapal      =        2;   { alignment for each heap arena }
      gbsal       =        2;   { globals area alignment }
      sethigh     =      255;   { Sets are 256 values }
      setlow      =        0;
      ordmaxchar  =      255;   { Characters are 8 bit ISO/IEC 8859-1 }
      ordminchar  =        0;
      marksize    =       12;   { 6*ptrsize }
      maxexp      =      126;   { maximum exponent of real }
      ujplen      =       3;    { length of ujp instruction (used for case
                                  jumps) }
      { Value of nil is 1 because this allows checks for pointers that were
        initialized, which would be zero (since we clear all space to zero).
        In the new unified code/data space scheme, 0 and 1 are always invalid
        addresses, since the startup code is at least that long. }
      nilval      =        1;   { value of 'nil' }
      pmmaxint = 32767; { value of maxint on target machine }

      { Mark element offsets

        Mark format is:

        -2:  Static link.
        -4: Dynamic link.
        -6: Saved EP from previous frame.
        -8: Stack bottom after locals allocate. Used for interprocdural gotos.
        -10: EP from current frame. Used for interprocedural gotos.
        -12: Return address

      }
      marksl      =        -2; { static link }
      markdl      =        -4; { dynamic link }
      markep      =        -6; { (old) maximum frame size }
      marksb      =        -8; { stack bottom }
      market      =        -10; { current ep }
      markra      =        -12; { return address }
