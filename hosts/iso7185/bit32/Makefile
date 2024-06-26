#
# Makefile for Pascal-p6
#
# Makes the main compiler interpreter set.
#
# The generated executables are named according to:
#
# bin16le
#
# where 16 is the bit size of the target, and le is the endian mode.
# The current bit lengths are:
# 
# 16
# 32
# 32
#
# Note that 16 bits actually covers both 8 bit and 16 bit processors, since 
# 8 bit processors usually have 16 bit addressing, regardless of their basic
# word size.
#
# The endian modes are:
#
# le - Little endian.
# be - Big endian.
#
# The make process will create all of the combinations that are possible given
# the current host processor. Not all combinations make sense. For example, 
# pcom, the compiler front end, has no endian mode, and its output decks are
# universal for all endian targets.
#
# After all possible bit and endian modes are generated, the versions  of the
# executable that match the current host are copied to their executable names,
# but without the bit or endian endings. Thus it is not necessary to 
# specifically state the characteristics of the host.
#
PC=gpc
PFLAGS=--classic-pascal-level-0 --no-warnings --transparent-file-names -lpthread
CFLAGS=
CPPFLAGS32LE=-DWRDSIZ32 -DLENDIAN -DGNU_PASCAL
CPPFLAGS16LE=-DWRDSIZ16 -DLENDIAN -DGNU_PASCAL
CPPFLAGS32BE=-DWRDSIZ32 -DBENDIAN -DGNU_PASCAL
CPPFLAGS16BE=-DWRDSIZ16 -DBENDIAN -DGNU_PASCAL

all: pcom pint pmach cmach genobj spew

pcom: source/pcom.pas
	pascpp --linemacro source/pcom $(CPPFLAGS32LE)
	$(PC) $(PFLAGS) -o bin/pcom32 source/pcom.mpp.pas
	pascpp --linemacro source/pcom $(CPPFLAGS16LE)
	$(PC) $(PFLAGS) -o bin/pcom16 source/pcom.mpp.pas
	cp bin/pcom32 bin/pcom
	
pcom_immerr: source/pcom.pas
	pascpp --linemacro source/pcom $(CPPFLAGS32LE) -DIMM_ERR
	$(PC) $(PFLAGS) -o bin/pcom32 source/pcom.mpp.pas
	pascpp --linemacro source/pcom $(CPPFLAGS16LE) -DIMM_ERR
	$(PC) $(PFLAGS) -o bin/pcom16 source/pcom.mpp.pas
	cp bin/pcom32 bin/pcom
	
pint: source/pint.pas source/externals.c
	gcc -o source/externals.o -c source/externals.c
	pascpp --linemacro source/pint $(CPPFLAGS32LE) -DGNU_PASCAL -DEXTERNALS
	$(PC) $(PFLAGS) -o bin/pint32le source/pint.mpp.pas source/externals.o
	pascpp --linemacro source/pint $(CPPFLAGS32BE) -DGNU_PASCAL -DEXTERNALS
	$(PC) $(PFLAGS) -o bin/pint32be source/pint.mpp.pas source/externals.o
	pascpp --linemacro source/pint $(CPPFLAGS16LE) -DGNU_PASCAL -DEXTERNALS
	$(PC) $(PFLAGS) -o bin/pint16le source/pint.mpp.pas source/externals.o
	pascpp --linemacro source/pint $(CPPFLAGS16BE) -DGNU_PASCAL -DEXTERNALS
	$(PC) $(PFLAGS) -o bin/pint16be source/pint.mpp.pas source/externals.o
	cp bin/pint32le bin/pint

pmach: source/pmach.pas
	pascpp --linemacro source/pmach $(CPPFLAGS) -DGNU_PASCAL
	$(PC) $(PFLAGS) -o bin/pmach32le source/pmach.mpp.pas
	cp bin/pmach32le bin/pmach

cmach: source/cmach/cmach.c
	$(CC) $(CFLAGS) $(CPPFLAGS32LE) -o bin/cmach32le source/cmach/cmach.c -lm -lpthread
	cp bin/cmach32le bin/cmach

genobj: source/genobj.pas
	$(PC) $(PFLAGS) -o bin/genobj source/genobj.pas

spew: source/spew.c
	$(CC) $(CFLAGS) -o bin/spew source/spew.c
	
clean:
	rm -f bin/pcom bin/pint 
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
	
help:
	@echo
	@echo Make targets:
	@echo
	@echo All	Make all binaries
	@echo
	@echo pcom	Make pcom, the Pascal compiler.
	@echo
	@echo pcom_immerr	Make pcom with print error immediate option. This causes
	@echo               errors to be printed immediately instead of waiting to
	@echo               collect an entire line. This is for debugging.
	@echo
	@echo pint          Make pint, the interpreter/debugger.
	@echo
	@echo pmach         Make pmach, the stand-alone interpreter.
	@echo
	@echo cmach         Make cmach, the stand-alone interpreter written in C.
	@echo
	@echo genobj        Make genobj, the binary deck to C file generator.
	@echo
	@echo spew          Make spew, a fault generator test program.
	@echo
	@echo clean         Clean intermediate/temp files from tree.
	@echo
    