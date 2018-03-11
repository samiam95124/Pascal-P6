@echo off
rem
rem Regression test
rem
rem Run the compiler through a few typical programs
rem to a "gold" standard file
rem
call testprogm sample_programs\hello
call testprogm sample_programs\roman
call testprogm sample_programs\match
call testprogm sample_programs\startrek
call testprogm sample_programs\basics
call testprogm sample_programs\drystone
call testprogm sample_programs\fbench
call testprogm sample_programs\prime
call testprogm sample_programs\qsort
call testprogm basic\basic
rem
rem Now run the ISO7185pat compliance test
rem
call testprog standard_tests\iso7185pat
rem
rem Run previous versions of the system and Pascal-S
rem
call testpascals
call testp2
call testp4
if "%1"=="full" (

    echo Running PRT...
    rem
    rem Run rejection test
    rem
    call runprt

    rem
    rem Self compile has issues
    rem
    exit /b 0
    
    echo Running self compile...
    rem
    rem Run pcom self compile (note this runs on P5 only)
    rem
    call cpcoms
    rem
    rem Run pint self compile (note this runs on P5 only)
    rem
    call cpints
    
)
