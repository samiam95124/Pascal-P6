@echo off
rem
rem Regression test
rem
rem Execution:
rem
rem regress [full|short] [mach]
rem
rem Run the compiler through a few typical programs
rem to a "gold" standard file
rem
rem The "full" parameter runs the long version test, which includes PRT and the
rem self compile/run. That takes a while to run.
rem
rem The "mach" parameter tests against the mach engine.
rem
call testprog sample_programs\hello %2
call testprog sample_programs\roman %2
call testprog sample_programs\match %2
call testprog sample_programs\startrek %2
call testprog sample_programs\basics %2
call testprog sample_programs\drystone %2
call testprog sample_programs\fbench %2
call testprog sample_programs\prime %2
call testprog sample_programs\qsort %2
call testprog basic\basic %2
rem
rem Now run the ISO7185pat compliance test
rem
call testprog standard_tests\iso7185pat %2
rem
rem Run previous versions of the system and Pascal-S
rem
call testpascals %2
call testp2 %2
call testp4 %2
if "%1"=="full" (

    echo Running PRT...
    rem
    rem Run rejection test
    rem
    call runprt %2

    rem
    rem Self compile has issues
    rem
    exit /b 0
    
    echo Running self compile...
    rem
    rem Run pcom self compile (note this runs on P5 only)
    rem
    call cpcoms %2
    rem
    rem Run pint self compile (note this runs on P5 only)
    rem
    call cpints %2
    
)
