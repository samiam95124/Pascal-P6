
      { 64 bit Machine Parameter block (MPB) }
      intsize     =        8;   { size of integer }
      intal       =        4;   { alignment of integer }
      intdig      =        20;  { number of decimal digits in integer }
      inthex      =        16;  { number of hex digits of integer }
      realsize    =        8;   { size of real }
      realal      =        4;   { alignment of real }
      charsize    =        1;   { size of char }
      charal      =        1;   { alignment of char }
      charmax     =        1;
      boolsize    =        1;   { size of boolean }
      boolal      =        1;   { alignment of boolean }
      ptrsize     =        8;   { size of pointer }
      adrsize     =        8;   { size of address }
      adral       =        4;   { alignment of address }
      setsize     =       32;   { size of set }
      setal       =        1;   { alignment of set }
      filesize    =        1;   { required runtime space for file (lfn) }
      fileidsize  =        1;   { size of the lfn only }
      exceptsize  =        1;   { size of exception variable }
      exceptal    =        1;
      stackal     =        8;   { alignment of stack }
      stackelsize =        8;   { stack element size }
      maxsize     =       32;   { this is the largest type that can be on
                                  the stack }
      { Heap alignment should be either the natural word alignment of the
        machine, or the largest object needing alignment that will be allocated.
        It can also be used to enforce minimum block allocation policy. }
      heapal      =        4;   { alignment for each heap arena }
      gbsal       =        4;   { globals area alignment }
      sethigh     =      255;   { Sets are 256 values }
      setlow      =        0;
      ordmaxchar  =      255;   { Characters are 8 bit ISO/IEC 8859-1 }
      ordminchar  =        0;
      maxresult   = realsize;   { maximum size of function result }
      marksize    =       56;   { maxresult+6*ptrsize }
      ujplen      =       9;    { length of ujp instruction (used for case
                                  jumps) }
      { Value of nil is 1 because this allows checks for pointers that were
        initialized, which would be zero (since we clear all space to zero).
        In the new unified code/data space scheme, 0 and 1 are always invalid
        addresses, since the startup code is at least that long. }
      nilval      =        1;  { value of 'nil' }

      { Mark element offsets

        Mark format is:

        -8:  Function return value, 64 bits, enables a full real result.
        -12:  Static link.
        -16: Dynamic link.
        -20: Saved EP from previous frame.
        -24: Stack bottom after locals allocate. Used for interprocdural gotos.
        -28: EP from current frame. Used for interprocedural gotos.
        -32: Return address

      }
      markfv      =        -8;  { function value }
      marksl      =        -16; { static link }
      markdl      =        -24; { dynamic link }
      markep      =        -32; { (old) maximum frame size }
      marksb      =        -40; { stack bottom }
      market      =        -48; { current ep }
      markra      =        -56; { return address }