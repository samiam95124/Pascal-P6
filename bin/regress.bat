@echo off
rem
rem Regression test
rem
rem Execution:
rem
rem regress [--full|--short|--pmach|--cmach]...
rem
rem Run the compiler through a few typical programs
rem to a "gold" standard file. Each mode is cycled through in sequence.
rem
rem The flags are one of:
rem
rem --full  Run full test sequence.
rem --short Run short test sequence.
rem
set pmach=0
set pmachoption=
set cmach=0
set cmachoption=
set full=0
for %%x in (%*) do (

   	if "%%~x"=="--full" (
   	
   		set full=1
   			
   	) else if "%%~x"=="--short" (
   	
   		set full=0
   		
   	) else if "%%~x"=="--help" (
   	
   		echo.
   		echo Regression test
		echo.
		echo Execution:
		echo.
		echo regress [--full^|--short^|--pmach^|--cmach]...
		echo.
		echo Run the compiler through a few typical programs
		echo to a "gold" standard file. Each mode is cycled through in sequence.
		echo.
		echo The flags are one of:
		echo.
		echo --pmach Generate mach code and run the result through pmach.
		echo --cmach Generate mach code and run the result through cmach.
		echo --full  Run full test sequence.
		echo --short Run short test sequence.
		echo.
		goto stop
   		
    ) else if not "%%~x"=="" (
    
    	echo.
    	echo *** Option not recognized
    	echo.
		echo Execution:
		echo.
		echo regress [--full|--short|--pmach|--cmach]...
		echo.
		goto stop
		
	)

)
echo Regression Summary > temp1
echo Line counts should be 0 for pass >> temp1
set option=
echo pint run >> temp1
call :do_regress
set option=--pmach
echo pmach run >> temp1
call :do_regress
set option=--cmach
echo cmach run >> temp1
call :do_regress

rem
rem Print collected status
rem
echo.
echo Copy report to commits
echo Cut here 
echo -----------------------------------------------------------
date /t
time /t
cat temp1
rm temp1
call chkfiles
echo -----------------------------------------------------------
echo Cut here
echo.

goto stop

:do_regress
call testprog %option% sample_programs\hello
wc -l sample_programs\hello.dif >> temp1
call testprog %option% sample_programs\roman
wc -l sample_programs\roman.dif >> temp1
call testprog %option% sample_programs\match
wc -l sample_programs\match.dif >> temp1
call testprog %option% sample_programs\startrek
wc -l sample_programs\startrek.dif >> temp1
call testprog %option% sample_programs\basics
wc -l sample_programs\basics.dif >> temp1
call testprog %option% sample_programs\drystone
wc -l sample_programs\drystone.dif >> temp1
call testprog %option% sample_programs\fbench
wc -l sample_programs\fbench.dif >> temp1
call testprog %option% sample_programs\prime
wc -l sample_programs\prime.dif >> temp1
call testprog %option% sample_programs\qsort
wc -l sample_programs\qsort.dif >> temp1
call testprog %option% basic\basic
wc -l sample_programs\basics.dif >> temp1
rem
rem Now run the ISO7185pat compliance test
rem
if "%option%"=="--cmach" (

	call testprog %option% --cmpfile standard_tests\iso7185patc standard_tests\iso7185pat
	
) else (

	call testprog %option% standard_tests\iso7185pat
	
)
cat standard_tests\iso7185pat.dif >> temp1
rem
rem Run previous versions of the system and Pascal-S
rem
call testpascals %option%
wc -l sample_programs\pascals.dif >> temp1
call testp2 %option%
wc -l p2\roman.dif >> temp1
call testp4 %option%
wc -l p4\standardp.dif >> temp1
if "%full%"=="1" (

    echo Running PRT...
    echo PRT run >> temp1
    rem
    rem Run rejection test
    rem
    call runprt %option%
    wc -l standard_tests/iso7185prt.dif >> temp1

    echo Running self compile...
    rem
    rem Run pcom self compile (note this runs on P5/P6 only)
    rem
    echo pcom self compile >> temp1
    call cpcoms %option%
    wc -l pcomm.dif >> temp1
    
    rem
    rem Run pint self compile (note this runs on P5/P6 only)
    rem
    echo pint self compile >> temp1
    call cpints %option%
    wc -l standard_tests/iso7185pats.dif >> temp1
    
)
exit /b

rem
rem Terminate program
rem
:stop
