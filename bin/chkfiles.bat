@echo off
rem
rem Find check value for all source files
rem

echo Source files
cat source/*.pas | md5sum

echo batch files
cat bin/chgver.bat bin/chgvers.bat bin/chkfiles.bat bin/cmach.exe bin/compile bin/compile.bat bin/cpcom bin/cpcom.bat bin/cpcoms bin/cpcoms.bat bin/cpint bin/cpint.bat bin/cpints bin/cpints.bat bin/diffnole bin/diffnole.bat bin/doseol bin/doseol.bat bin/fixeol bin/fixeol.bat bin/make_flip bin/make_flip.bat bin/p6 bin/p6.bat bin/regress bin/regress.bat bin/repo_ready bin/repo_ready.bat bin/run bin/run.bat bin/runprt bin/runprt.bat bin/set32 bin/set32.bat bin/set64 bin/set64.bat bin/setgpc.bat bin/setiso7185.bat bin/setok.bat bin/setpascaline.bat bin/testp2 bin/testp2.bat bin/testp4 bin/testp4.bat bin/testpascals bin/testpascals.bat bin/testprog bin/testprog.bat bin/unixeol bin/unixeol.bat bin/zipp5 bin/zipp5.bat | md5sum

echo P2 files
cat p2/*.pas | md5sum

echo p4 files
cat p4/*.pas | md5sum

echo Sample program test files
cat sample_programs/*.pas | md5sum

echo standard test files
cat standard_tests/*.pas | md5sum

echo other files
cat setpath setpath.bat Makefile configure configure.bat | md5sum