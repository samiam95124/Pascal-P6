#
# Makefile for Pascal-p6
#
# Makes the main compiler interpreter set.
#
PC=gpc
PFLAGS=--classic-pascal-level-0 --no-warnings --transparent-file-names
CFLAGS=
CPPFLAGS=-DWRDSIZ64

all: pcom pint pmach cmach genobj spew

pcom: source/pcom.pas
	pascpp source/pcom $(CPPFLAGS) -DGNU_PASCAL
	$(PC) $(PFLAGS) -o bin/pcom source/pcom.mpp.pas
	
pint: source/pint.pas 
	pascpp source/pint $(CPPFLAGS) -DGNU_PASCAL
	$(PC) $(PFLAGS) -o bin/pint source/pint.mpp.pas

pmach: source/pmach.pas
	pascpp source/pmach $(CPPFLAGS) -DGNU_PASCAL
	$(PC) $(PFLAGS) -o bin/pmach source/pmach.mpp.pas

cmach: source/cmach.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -o bin/cmach source/cmach.c -lm

genobj: source/genobj.pas
	$(PC) $(PFLAGS) -o bin/genobj source/genobj.pas

spew: source/spew.c
	$(CC) $(CFLAGS) -o bin/spew source/spew.c
	
clean:
	find . -name "*.pint" -type f -delete
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
	find . -name "*.p5" -type f -delete
	find . -name "*.p6" -type f -delete
	find . -name "*.p6o" -type f -delete
	find . -name "*.mpp.pas" -type f -delete
	
