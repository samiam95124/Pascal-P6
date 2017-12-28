@echo off
rem
rem Test a single program run
rem
rem Tests the compile and run of a single program.
rem
rem To do this, there must be the files:
rem
rem <file>.inp - Contains all input to the program
rem <file>.cmp - Used to compare the <file>.lst program to, should
rem              contain an older, fully checked version of <file>.lst.
rem
rem <file>.dif will contain the differences in output of the run.
rem

rem
rem Check there is a parameter
rem
if not "%1"=="" goto paramok
echo *** Error: Missing parameter
goto stop
:paramok

rem
rem Check the source file exists
rem
if exist %1.pas goto :sourcefileexist
echo *** Error: Source file %1.pas does not exist
goto stop
:sourcefileexist

rem
rem Check the input file exists
rem
if exist %1.inp goto :inputfileexist
echo *** Error: Input file %1.inp does not exist
goto stop
:inputfileexist

rem
rem Check the result compile file exists
rem
if exist %1.cmp goto :comparefileexist
echo *** Error: Compare file %1.cmp does not exist
goto stop
:comparefileexist

rem
rem Compile and run the program
rem
echo Compile and run %1
call compile %1
rem echo Error return after compile: %errorlevel%
rem
rem Proceed to run and compare only if compile suceeded
rem
if not errorlevel 1 (

    call run %1

    rem
    rem Check output matches the compare file
    rem
    call diffnole %1.lst %1.cmp > %1.dif
    dir %1.dif > %1.tmp
    grep ".dif" %1.tmp
    rm -f %1.tmp

)

rem
rem Terminate program
rem
:stop
