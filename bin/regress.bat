@echo off
rem
rem Regression test
rem
rem Execution:
rem
rem regress [--full|--short|--pmach|--cmach]...
rem
rem Run the compiler through a few typical programs
rem to a "gold" standard file
rem
rem The flags are one of:
rem
rem --pmach	Generate mach code and run the result through pmach.
rem --cmach	Generate mach code and run the result through cmach.
rem --full  Run full test sequence.
rem --short Run short test sequence.
rem
set pmach=0
set pmachoption=
set cmach=0
set cmachoption=
set full=0
for %%x in (%*) do (

    if "%%~x"=="--pmach" (
    
    	set pmach=1
    	set pmachoption=--pmach
    	
   	) else if "%%~x"=="--cmach" (
   	
   		set cmach=1
   		set cmachoption=--cmach
   	
   	) else if "%%~x"=="--full" (
   	
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
		echo to a "gold" standard file
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
call testprog !pmachoption! !cmachoption! sample_programs\hello
wc -l sample_programs\hello.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\roman
wc -l sample_programs\roman.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\match
wc -l sample_programs\match.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\startrek
wc -l sample_programs\startrek.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\basics
wc -l sample_programs\basics.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\drystone
wc -l sample_programs\drystone.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\fbench
wc -l sample_programs\fbench.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\prime
wc -l sample_programs\prime.dif >> temp1
call testprog !pmachoption! !cmachoption! sample_programs\qsort
wc -l sample_programs\qsort.dif >> temp1
call testprog !pmachoption! !cmachoption! basic\basic
wc -l sample_programs\basics.dif >> temp1
rem
rem Now run the ISO7185pat compliance test
rem
if "%cmach%"=="1" (

	call testprog !pmachoption! !cmachoption! --cmpfile standard_tests\iso7185patc standard_tests\iso7185pat
	
) else (

	call testprog !pmachoption! !cmachoption! standard_tests\iso7185pat
	
)
cat standard_tests\iso7185pat.dif >> temp1
rem
rem Run previous versions of the system and Pascal-S
rem
call testpascals %pmachoption% %cmachoption%
wc -l sample_programs\pascals.dif >> temp1
call testp2 %2
wc -l p2\roman.dif >> temp1
call testp4 %2
wc -l p4\standardp.dif >> temp1
if "%full%"=="1" (

    echo Running PRT...
    rem
    rem Run rejection test
    rem
    call runprt %2

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

rem
rem Print collected status
rem
echo.
echo Copy report to commits
echo Cut here -----------------------------------------------------------
date /t
time /t
cat temp1
rm temp1
call chkfiles
echo Cut here -----------------------------------------------------------
echo.

rem
rem Terminate program
rem
:stop
