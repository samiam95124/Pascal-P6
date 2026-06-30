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

var sp:      pstring;
    s:       packed array [1..256] of char;
    p, n, e: packed array [1..256] of char;
    i, ln:   integer;

begin

   sp := getpgm;                     { the program's directory, e.g. ".../bin/" }
   for i := 1 to 256 do s[i] := ' '; { clear the work buffer }
   ln := max(sp^);                   { length of the getpgm string }
   for i := 1 to ln do s[i] := sp^[i];
   { getpgm returns the directory with a trailing '/', so drop it before
     taking the last path component }
   if ln > 0 then if s[ln] = '/' then s[ln] := ' ';
   brknam(s, p, n, e);
   writeln(n:*)

end.
