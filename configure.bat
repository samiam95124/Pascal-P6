@echo off
rem ################################################################################
rem #
rem # Configure scipt for Pascal-P5
rem #
rem # Sets up the complete Pascal-P5 project.
rem #
rem ################################################################################

rem
rem Set default variables
rem
set compiler=gpc
set bits=32
set host=windows

rem
rem Determine if needed programs exist. The only fatal one is grep, because we
rem need that to run this script. The rest will impact the running of various
rem test and build scripts.
rem

where /q grep
if %errorlevel% neq 0 (

    echo *** No grep was found
    echo Terminating
    exit /b 1
    
)

where /q diff || echo *** No diff was found
where /q sed || echo *** No sed was found
where /q rm || echo *** No rm was found
where /q cp || echo *** No cp was found
where /q mv || echo *** No mv was found
where /q flip || echo *** No flip was found
where /q ls || echo *** No ls was found
where /q gzip || echo *** No zip was found
where /q tar || echo *** No tar was found

rem
rem Check flip (Unix/dos end of line changer) exists, and make it if not.
rem
where /q flip
if %errorlevel% neq 0 (

    echo Making flip.exe
    call bin\make_flip
    dir bin\flip.exe > temp
    grep "flip.exe" temp > temp2
    grep "flip.exe" temp > temp2 || echo *** Unable to make flip
    
)

rem
rem Detect and prioritize IP Pascal. The reason for this is, if IP Pascal
rem exists on the system, that is the preferable compiler.
rem pc.exe is the IP Pascal command shell. You might have to fix this up
rem if there is another pc.exe in the system.
rem
where /q pc
if %errorlevel% equ 0 (

    set compiler=ip_pascal
    goto :compiler_check_done
    
)

rem
rem Now check for GPC. Output scary message for no compiler found, but 
rem otherwise do nothing. rem Its up to the user to find a compiler.
rem 
where /q gpc
if %errorlevel% neq 0 (

    goto :check_fpc

)

set compiler=gpc

rem
rem Evaluate GPC compiler version and word size. This is required with GPC
rem because not all versions work as ISO 7185 Compilers. Also, we do the
rem 32 or 64 bit detection here.
rem
gpc -v 2> temp
grep "gpc version 20070904" temp > temp2
if %errorlevel% neq 0 (

    echo *** Warning, Pascal-P5 is only validated to work with gpc version 20070904

)
rm temp2
rem check 32/64 bit mode
set bits=64
grep "build=x86_64" temp > temp2
if %errorlevel% neq 0 (

    set bits=32

)

goto :compiler_check_done

:check_fpc
rem
rem Check for FPC. Output scary message for no compiler found, but 
rem otherwise do nothing. Its up to the user to find a compiler.
rem 
where /q fpc
if %errorlevel% neq 0 (

    echo *** No gpc, fpc or pc was found, there is no ISO 7185 Pascal compiler installed
    goto :compiler_check_done

)

set compiler=fpc

rem
rem Evaluate FPC compiler version. This is required with FPC
rem because older versions didn't support ISO 1785 mode. Also, versions
rem prior to 3.0.2 (?) didn't support header file association correctly.
rem
rem We need a method to validate > 3.0.4 here.
rem
fpc -iV 2> temp
grep "3.0.4" temp > temp2
if %errorlevel% neq 0 (

    echo *** Warning, Pascal-P5 is only validated to work with FPC version 3.0.4

)
    
:compiler_check_done

rem
rem Check all arguments. Note that we don't attempt to check or fix bad choices
rem on the users part. We figure they know what they are doing.
rem

for %%x in (%*) do (

	if "%%x" == "--help" (

        echo "Configure program for Pascal-P5"
        echo
        echo "--gpc:       Select GPC as target compiler"
        echo "--ip_pascal: Select IP Pascal as target compiler"
        echo "--fpc:       Select FPC as target compiler"
        echo "--32:        Select 32 bit target"
        echo "--64:        Select 64 bit target"
        echo
        exit /b 1

	) else if "%%x" == "--gpc" (

    	set compiler=gpc

    ) else if "%%x" == "--ip_pascal" (

		set compiler=ip_pascal

    ) else if "%%x" == "--fpc" (

		set compiler=fpc

    ) else if "%%x" == "--32" (

		set bits=32

    ) else if "%%x" == "--64" (

		set bits=64

    ) else (
    
        echo *** Option not recognized
        echo Terminating
        exit /b 1
        
    )

)

rem
rem Set up compiler to use.
rem
rem Presently implements:
rem
rem IP Pascal, named "ip_pascal"
rem
rem GPC Pascal, named "gpc"
rem
rem FPC Pascal, named "fpc"
rem

if "%compiler%" == "ip_pascal" (

    rem
    rem Set up for IP Pascal
    rem
    cp ip_pascal\p5.bat      bin\p5.bat
    cp ip_pascal\compile.bat bin\compile.bat
    cp ip_pascal\run.bat     bin\run.bat

    cp ip_pascal\p5          bin\p5
    cp ip_pascal\compile     bin\compile
    cp ip_pascal\run         bin\run

    cp ip_pascal\Makefile    .

    cp ip_pascal/standard_tests/bit%bits%/iso7185pat.cmp standard_tests
    cp ip_pascal/standard_tests/bit%bits%/iso7185patc.cmp standard_tests
    cp ip_pascal/standard_tests/bit%bits%/iso7185pats.cmp standard_tests

    rem
    rem IP Pascal does not care about line endings, but returning to DOS mode
    rem line endings normalizes the files for checkin.
    rem
    rem doseol

    echo Compiler set to IP Pascal
    
)

if "%compiler%" == "gpc" (

    rem
    rem Set up for GPC Pascal
    rem
    cp gpc\p5.bat      bin\p5.bat
    cp gpc\compile.bat bin\compile.bat
    cp gpc\run.bat     bin\run.bat

    cp gpc\p5          bin\p5
    cp gpc\compile     bin\compile
    cp gpc\run         bin\run

    cp gpc/Makefile    .

    cp gpc/standard_tests/bit%bits%/iso7185pat.cmp standard_tests
    cp gpc/standard_tests/bit%bits%/iso7185patc.cmp standard_tests
    cp gpc/standard_tests/bit%bits%/iso7185pats.cmp standard_tests
    
    cp gpc/p4/bit%bits%/standardp.cmp p4
    cp gpc/p4/bit%bits%/standardpc.cmp p4

    rem
    rem GPC needs Unix line endings in both the Unix and cygwin
    rem versions.
    rem
    rem doseol

    echo Compiler set to GPC Pascal

)

if "%compiler%" == "fpc" (

    rem
    rem Set up for FPC Pascal
    rem
    cp fpc\p5.bat      bin\p5.bat
    cp fpc\compile.bat bin\compile.bat
    cp fpc\run.bat     bin\run.bat

    cp fpc\p5          bin\p5
    cp fpc\compile     bin\compile
    cp fpc\run         bin\run

    cp fpc\Makefile    .

    cp fpc\standard_tests/iso7185pat.cmp standard_tests
    cp fpc\standard_tests/iso7185pats.cmp standard_tests

    rem
    rem IP Pascal does not care about line endings, but returning to DOS mode
    rem line endings normalizes the files for checkin.
    rem
    rem doseol

    echo Compiler set to FPC Pascal

)

rem
rem Set bit length
rem
:setbits
if "%bits%" == "32" (

    echo Setting for 32 bit target
    set32

)
if "%bits%" == "64" (

    echo Setting for 64 bit target
    set64

)

echo Configure completed!
