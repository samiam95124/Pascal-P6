@echo off
rem
rem Compile and run with P6 using GPC
rem
rem Execute with:
rem
rem p6 <sourcefile> [flags] [<sourcefile>]...
rem
rem Where <sourcefile> is the name of the source file without
rem extention. The Pascal input file(s) are compiled and run
rem as a group. Any compiler errors are output to the screen.
rem Input and output to and from the running program are from
rem and to the console.
rem
rem The flags are one of:
rem
rem --pmach	Generate mach code and run the result through pmach.
rem --cmach	Generate mach code and run the result through cmach.
rem
rem The intermediate code is placed in <file>.p6.
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
		echo Compile with P6 using GPC
		echo.
		echo Execute with:
		echo.
		echo p6 ^<sourcefile^> [--pmach^|--cmach] [^<sourcefile^>]...
		echo.
		echo where ^<sourcefile^> is the name of the source file without
		echo extention. The Pascal input file^(s^) are compiled and run
		echo as a group. Any compiler errors are output to the screen.
		echo Input and output to and from the running program are from
		echo and to the console.
		echo.
		echo The flags are one of:
		echo.
		echo --pmach	Generate mach code and run the result through pmach.
		echo --cmach	Generate mach code and run the result through cmach.
		echo.
		echo The intermediate code is placed in ^<file^>.p6.
		echo.
		goto stop
		
    ) else if not "%%~x"=="" (
    
		if not exist "%%~x.pas" (
  
        	echo %%~x.pas does not exist
        	goto stop
      
    	)
    	echo Compiling %%~x...
    	if "!pmach!"=="1" (
    	
    		echo|set /p="{$e+}" > temp
    		cat temp %%~x.pas > prd
    		rm temp
    		
    	) else if "!cmach!"=="1" (

    		echo|set /p="{$e+}" > temp
    		cat temp %%~x.pas > prd
    		rm temp
    	
    	) else (
    	
    		cp %%~x.pas prd
    		
    	)
    	pcom
    	mv prr %%~x.p6
    	cat %%~x.p6 >> temp.p6
    	set progfile=%%~x
    					
   	) 
)

if "%progfile%"=="" (

    echo *** Error: No compile file specified
    exit /b 1

)

rem
rem Run combined file
rem
mv temp.p6 prd
pint
if "%pmach%"=="1" (

	mv prr prd
	pmach
	
) else if "%cmach%"=="1" (

	mv prr prd
	cmach
	
)

rem
rem Terminate program
rem
:stop
rm -f prd
rm -f prr