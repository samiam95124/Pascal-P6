@echo off
rem
rem Compile file in batch mode using GPC Pascal.
rem
rem Runs a compile with the input and output coming from/going to files.
rem
rem Execution:
rem
rem Compile [--pmach|--cmach] <file>
rem
rem <file> is the filename without extention.
rem
rem The files are:
rem
rem <file>.pas - The Pascal source file
rem <file>.p6  - The intermediate file produced
rem <file>.err - The errors output from the compiler
rem
rem Note that the l+ option must be specified to get a full
rem listing in the .err file (or just a lack of l-).
rem
rem The flags are one of:
rem
rem --pmach	Generate mach code and run the result through pmach.
rem --cmach	Generate mach code and run the result through cmach.
rem
setlocal EnableDelayedExpansion
set pmach=0
set cmach=0
set progfile=
for %%x in (%*) do (

    if "%%~x"=="--pmach" (
    
    	set pmach=1
    	
   	) else if "%%~x"=="--cmach" (
   	
   		set cmach=1
   		
   	) else if "%%~x"=="--help" (
   	
		echo.
		echo Compile file in batch mode using GPC Pascal.
		echo.
		echo Runs a compile with the input and output coming from/going to files.
		echo.
		echo Execution:
		echo.
		echo Compile [--pmach^|--cmach] ^<file^>
		echo.
		echo ^<file^> is the filename without extention.
		echo.
		echo The files are:
		echo.
		echo ^<file^>.pas - The Pascal source file
		echo ^<file^>.p6  - The intermediate file produced
		echo ^<file^>.err - The errors output from the compiler
		echo.
		echo Note that the l+ option must be specified to get a full
		echo listing in the .err file ^(or just a lack of l-^).
		echo.
		echo The flags are one of:
		echo.
		echo --pmach	Generate mach code and run the result through pmach.
		echo --cmach	Generate mach code and run the result through cmach.
		echo.   	
		goto stop
   		
    ) else if not "%%~x"=="" (
    
    	if not exist "%%~x.pas" (
  
        	echo %%~x.pas does not exist
        	goto stop
      
    	)
    	set progfile=%%~x
    	
    ) 

)

if "%progfile%"=="" (

    echo *** Error: No compile file specified
    exit /b 1

)

if "%pmach%"=="1" (

    echo|set /p="{$e+}" > temp
    cat temp %progfile%.pas > prd
    rm temp
    
) else if "%cmach%"=="1" (

    echo|set /p="{$e+}" > temp
    cat temp %progfile%.pas > prd
    rm temp

) else (

    cp %progfile%.pas prd
    
)
pcom > %progfile%.err
rem
rem The status of the compile is not returned, so convert a non-zero
rem error message to fail status
rem
grep -q "Errors in program: 0" %progfile%.err
if errorlevel 1 exit /b 1
rem
rem Move the prr file to <file.p6>
rem
if exist "%progfile%.p6" del %progfile%.p6
mv prr %progfile%.p6
chmod +w %progfile%.p6
rem
rem Terminate program
rem
:stop
