@echo off
rem
rem Generate a packaged application
rem
rem Execute with:
rem
rem pg [<sourcefile>]...
rem
rem Where <sourcefile> is the name of the source file without
rem extention. The Pascal input file(s) are compiled and run
rem as a group. Any compiler errors are output to the screen.
rem Input and output to and from the running program are from
rem and to the console.
rem
setlocal EnableDelayedExpansion
set pmach=0
set cmach=0
set progfile=
set package=0
for %%x in (%*) do (

   	if "%%~x"=="--help" (
   	
		echo.
		echo Compile with P6 using GPC
		echo.
		echo Execute with:
		echo.
		echo pg [^<sourcefile^>]...
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
   		echo|set /p="{$e+}" > temp
   		cat temp %%~x.pas > prd
   		rm temp
    	pcom
    	mv prr %%~x.p6
    	cat %%~x.p6 >> temp.p6
    	set progfile=%%~x
    					
   	) 
)

if "%progfile%"=="" (

    echo *** Error: No target file specified
    exit /b 1

)

rem
rem Run combined file
rem
mv temp.p6 prd
pint
cp prr %progfile%.p6o
mv %progfile%.p6o prd
genobj
cp prr program_code.c
gcc -DPACKAGE -DGPC=0 -I. -o %progfile% source/cmach.c
	
rem
rem Terminate program
rem
:stop
rm -f prd
rm -f prr