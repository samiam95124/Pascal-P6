#
# Makefile for Pascal-p5
#
# Makes the main compiler interpreter set.
#
PC=fpc
CFLAGS=-Miso

all: pcom pint

pcom: source/pcom.pas
	$(PC) $(CFLAGS) source/pcom.pas
	mv source/pcom.exe bin
	
pint: source/pint.pas
	$(PC) $(CFLAGS) source/pint.pas
	mv source/pint.exe bin
	
clean:
	rm -f bin/pcom bin/pint 
	find . -name "*.p5" -type f -delete
	find . -name "*.out" -type f -delete
	find . -name "*.lst" -type f -delete
	find . -name "*.obj" -type f -delete
	find . -name "*.sym" -type f -delete
	find . -name "*.int" -type f -delete
	find . -name "*.dif" -type f -delete
	find . -name "*.err" -type f -delete
	find . -name "*.ecd" -type f -delete
	find . -name "*.tmp" -type f -delete
	find . -name "prd" -type f -delete
	find . -name "prr" -type f -delete
	find . -name "temp" -type f -delete
	find . -name "tmp" -type f -delete
	find . -name "*~" -type f -delete
	find . -name "*.diflst" -type f -delete
	find . -name "*.ecdlst" -type f -delete
	find . -name "*.nocerr" -type f -delete
	find . -name "*.noerr" -type f -delete
	find . -name "*.norerr" -type f -delete
	find . -name "*.p2" -type f -delete
	find . -name "*.p4" -type f -delete

