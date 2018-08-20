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
rem --noerrmsg       Do not output failure message on compile, the .err file is
rem                  sufficient. 
rem 
setlocal EnableDelayedExpansion
set pmach=0
set pmachoption=
set cmach=0
set cmachoption=
set progfile=
set cmpnext=0
set cmpfile=
set noerrmsg=0
for %%x in (%*) do (

    if "%%~x"=="--pmach" (
    
    	set pmach=1
    	set pmachoption=--pmach
    	
   	) else if "%%~x"=="--cmach" (
   	
   		set cmach=1
   		set cmachoption=--cmach
   		
   	) else if "%%~x"=="--cmpfile" (
   	
   		set cmpnext=1
   		
   	) else if "%%~x"=="--noerrmsg" (
   	
   		set noerrmsg=1
   		
   	) else if "%%~x"=="--help" (
   	
		echo.
		echo Test a single program run
		echo.
		echo Execution:
		echo.
		echo testprog [--pmach^|--cmach]... ^<file^>
		echo.
		echo Tests the compile and run of a single program.
		echo.
		echo To do this, there must be the files:
		echo.
		echo ^<file^>.inp - Contains all input to the program
		echo ^<file^>.cmp - Used to compare the ^<file^>.lst program to, should
		echo              contain an older, fully checked version of ^<file^>.lst.
		echo ^<file^>.dif - Will contain the differences in output of the run.
		echo.
		echo --pmach            Generate mach code and run the result through pmach.
		echo --cmach	        Generate mach code and run the result through cmach.
		echo --cmpfile ^<file^> Use filename following option for compare file.
		echo --noerrmsg         Do not output failure message on compile, the .err file is
		echo                    sufficient. 
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
    		
    		    if "!noerrmsg!"=="0" (
    		    
    		    	echo *** Compile file %%~x failed
    				
    			)
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
