@echo off
rem
rem Run a Pascal file in batch mode using GPC Pascal
rem
rem Runs a Pascal intermediate in batch mode.
rem
rem Execution:
rem
rem run <file> [--pmach|--cmach] [<inpfile>] <file>
rem
rem <file> is the filename without extention.
rem
rem The files are:
rem
rem <file>.p6  - The intermediate file
rem <file>.out - The prr file produced
rem <file>.inp - The input file to the program
rem <file>.lst - The output file from the program
rem <file>.p6o - The object deck for mach, if selected
rem
rem The flags are one of:
rem
rem --pmach	Generate mach code and run the result through pmach.
rem --cmach	Generate mach code and run the result through cmach.
rem
rem If the <inpfile> appears, it will be concatenated to the intermediate or
rem code file to be read by the target program.
rem
setlocal EnableDelayedExpansion
set pmach=0
set cmach=0
set progfile=
set inpfile=
for %%x in (%*) do (

    if "%%~x"=="--pmach" (
    
    	set pmach=1
    	
   	) else if "%%~x"=="--cmach" (
   	
   		set cmach=1
   		
   	) else if "%%~x"=="--help" (

		echo Run a Pascal file in batch mode using GPC Pascal
		echo.
		echo Runs a Pascal intermediate in batch mode.
		echo.
		echo Execution:
		echo.
		echo run ^<file^> [mach^|pint] [inp]
		echo.
		echo ^<file^> is the filename without extention.
		echo.
		echo The files are:
		echo.
		echo ^<file^>.p6  - The intermediate file
		echo ^<file^>.out - The prr file produced
		echo ^<file^>.inp - The input file to the program
		echo ^<file^>.lst - The output file from the program
		echo ^<file^>.p6o - The object deck for mach, if selected
		echo.
		echo The flags are one of:
		echo.
		echo --pmach	Generate mach code and run the result through pmach.
		echo --cmach	Generate mach code and run the result through cmach.
		echo.
		goto stop
		
    ) else if not "%%~x"=="" (
    
    	set inpfile=!progfile!
    	set progfile=%%~x
    					
   	)
   	 
)

if "%progfile%"=="" (

    echo *** Error: No compile file specified
    exit /b 1

)


if not exist "%progfile%.p6" (

    echo *** Error: Missing %progfile%.p6 file
    exit /b 1

)

if not exist "%progfile%.inp" (

    echo *** Error: Missing %progfile%.inp file
    exit /b 1

)

if "%pmach%"=="1" (

    echo Running with pmach
    cp %progfile%.p6 prd
    pint
    mv prr %progfile%.p6o
    cat %progfile%.p6o %inpfile% > prd
    pmach < %progfile%.inp > %progfile%.lst 2>&1
    
) else if "%cmach%"=="1" (

    echo Running with cmach 
    cp %progfile%.p6 prd
    pint
    mv prr %progfile%.p6o
    cat %progfile%.p6o %inpfile% > prd
    cmach < %progfile%.inp > %progfile%.lst 2>&1
    
) else (

	echo Running with pint
    cat %progfile%.p6 %inpfile% > prd
    pint < %progfile%.inp > %progfile%.lst 2>&1
    
)
if exist "%progfile%" rm %progfile%.out
mv prr %progfile%.out
chmod +w %progfile%.out
rem
rem Terminate program
rem
:stop
