@echo off
rem
rem Compile with P5 using GPC
rem
rem Execute with:
rem
rem p6 <sourcefile> [flags] [<sourcefile>]...
rem
rem where <sourcefile> is the name of the source file without
rem extention. The Pascal input file(s) are compiled and run
rem as a group. Any compiler errors are output to the screen.
rem Input and output to and from the running program are from
rem and to the console.
rem
rem The flags are one of:
rem
rem -pmach	Generate mach code and run the result through pmach.
rem -cmach	Generate mach code and run the result through cmach.
rem
rem The intermediate code is placed in <file>.p6.
rem
setlocal EnableDelayedExpansion

rem
rem Check any input files exist
rem
if "%1"=="" (

    echo *** Must specify at least one file
    goto stop
    
)

rem
rem Compile all input files into intermediate form
rem
set pmach=0
set cmach=0
rm -f temp.p6
for %%x in (%*) do (

	echo Processing %%~x
    if "%%~x"=="-pmach" (
    
    	set pmach=1
    	
   	) else if "%%~x"=="-cmach" (
   	
   		set cmach=1
   		
    ) else (
    
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

    		echo {$e+} > temp
    		cat temp %%~x.pas > prd
    		rm temp
    	
    	) else (
    	
    		cp %%~x.pas prd
    		
    	)
    	pcom
    	mv prr %%~x.p6
    	cat %%~x.p6 >> temp.p6
    	
    ) 

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
:stop
rem rm -f prd
rem rm -f prr