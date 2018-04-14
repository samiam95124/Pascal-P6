@echo off
rem
rem Test a single program run
rem
rem Execution:
rem
rem testprog [--pmach|--cmach]... <file>
rem
rem Tests the compile and run of a single program.
rem
rem To do this, there must be the files:
rem
rem <file>.inp - Contains all input to the program
rem <file>.cmp - Used to compare the <file>.lst program to, should
rem              contain an older, fully checked version of <file>.lst.
rem <file>.dif will contain the differences in output of the run.
rem
rem --pmach          Generate mach code and run the result through pmach.
rem --cmach	         Generate mach code and run the result through cmach.
rem --cmpfile <file> Use filename following option for compare file
rem 
setlocal EnableDelayedExpansion
set pmach=0
set pmachoption=
set cmach=0
set cmachoption=
set progfile=
set cmpnext=0
set cmpfile=
for %%x in (%*) do (

    if "%%~x"=="--pmach" (
    
    	set pmach=1
    	set pmachoption=--pmach
    	
   	) else if "%%~x"=="--cmach" (
   	
   		set cmach=1
   		set cmachoption=--cmach
   		
   	) else if "%%~x"=="--cmpfile" (
   	
   		set cmpnext=1
   		
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
    
        if "!cmpnext!"=="1" (
        
        	set cmpfile=%%~x
        	set cmpnext=0
        	
        ) else (
         
			if not exist "%%~x.pas" (
  
            	echo %%~x.pas does not exist
            	goto stop
        
    		)
    		echo Compiling %%~x...
    		call compile !pmachoption! !cmachoption! %%~x
    		if errorlevel 1 (
    		
    		    echo *** Compile file %%~x failed
    			goto stop
    		
    		) else (
    		
    			rem
    			rem Collect module sections to single intermediate
    			rem
    			cat %%~x.p6 >> temp.p6
    			
    		)
    		rem
    		rem Set the main run program as the last one
    		rem
    		set progfile=%%~x
    		
    	)
    					
   	) 
   	
)

rem
rem Compile and run the program
rem
if "%progfile%"=="" (

	echo *** No program file was specified
	goto stop
	
)

rem
rem set default compare file
rem
if "%cmpfile%"=="" (

	set cmpfile=%progfile%
	
)

rem
rem Move final collected intermediate to target.
rem Note that even the main file will have its intermediate added to the last.
rem
mv temp.p6 %progfile%.p6
if not exist "%progfile%.inp" (

	echo %progfile%.inp does not exist
	goto stop

)
if not exist "%cmpfile%.cmp" (

	echo %cmpfile%.cmp does not exist
	goto stop

)
echo Running %progfile%...
call run %pmachoption% %cmachoption% %progfile%
if not errorlevel 1 (

    rem
    rem Check output matches the compare file
    rem
    call diffnole %progfile%.lst %cmpfile%.cmp > %progfile%.dif
    dir %progfile%.dif > %progfile%.tmp
    grep ".dif" %progfile%.tmp
    rm -f %progfile%.tmp

)

rem
rem Terminate program
rem
:stop
