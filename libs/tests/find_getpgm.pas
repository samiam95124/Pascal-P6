{******************************************************************************
*                                                                             *
*                      GETPGM LOCATION TEST HELPER                            *
*                                                                             *
* Prints the basename of its own program directory (getpgm with the path      *
* removed). It is installed in bin (bin/build copies every libs/tests program *
* there) and is not relocated by the parallel test harness, so it always      *
* reports "bin". services_test sub-executes it to test getpgm deterministically*
* -- services_test's own binary is moved into per-model directories by the     *
* harness, so its own getpgm would not be stable.                             *
*                                                                             *
******************************************************************************}

program find_getpgm(output);

uses services;

var s:       packed array [1..256] of char;
    p, n, e: packed array [1..256] of char;

begin

   getpgm(s);            { the program's directory }
   brknam(s, p, n, e);   { last path component -> n }
   writeln(n:*)

end.
