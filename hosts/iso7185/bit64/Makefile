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
# 64
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

#
# Use of pgen_wrap is a temporary fix to accomodate the (rather odd) GPC 
# treatment of header files as being connected to a file of the same name.
# pgen uses the same method to stay compatible with GPC, at least for now.
#
CFLAGS=-g3 -DWRDSIZ64
SOURCE=$(PASCALP6)/source
BUILD=$(PASCALP6)/build
CPPFLAGS=-P -nostdinc -traditional-cpp
CPPFLAGS64LE=-DWRDSIZ64 -DLENDIAN -DPASCALINE -DNOPRDPRR -DGPC=0
CPPFLAGS16LE=-DWRDSIZ16 -DLENDIAN -DPASCALINE -DNOPRDPRR -DGPC=0
CPPFLAGS64BE=-DWRDSIZ64 -DBENDIAN -DPASCALINE -DNOPRDPRR -DGPC=0
CPPFLAGS16BE=-DWRDSIZ16 -DBENDIAN -DPASCALINE -DNOPRDPRR -DGPC=0
EXTERNAL=libs
EXTERNALLIBS=$(EXTERNAL)/services.o

all: bin/pcom bin/pint bin/pmach bin/cmach bin/pgen bin/genobj bin/spew

pcom: bin/pcom
bin/pcom: source/pcom.pas
	@echo
	@echo "Building pcom..."
	@echo
	mkdir -p $(BUILD)
	cpp $(CPPFLAGS) $(CPPFLAGS64LE) $(SOURCE)/pcom.pas $(BUILD)/pcom.mpp.pas
	pcom $(BUILD)/pcom.mpp.pas $(BUILD)/pcom.p6 --reference-
	pgen $(BUILD)/pcom.p6 $(BUILD)/pcom.s
	gcc $(CFLAGS) $(SOURCE)/AMD64/gcc/psystem.c \
		$(BUILD)/pcom.s -x assembler $(SOURCE)/AMD64/gcc/psystem_asm.asm \
		-o $(BUILD)/pcom -lm -lpthread
	cp $(BUILD)/pcom $(PASCALP6)/bin

bin/pcom_immerr: source/pcom.pas
	@echo
	@echo "Building pcom with immediate error..."
	@echo
	mkdir -p $(BUILD)
	cpp $(CPPFLAGS) $(CPPFLAGS64LE) $(SOURCE)/pcom.pas $(BUILD)/pcom.mpp.pas \
		-DIMM_ERR
	pcom $(BUILD)/pcom.mpp.pas $(BUILD)/pcom.p6 --reference-
	pgen $(BUILD)/pcom.p6 $(BUILD)/pcom.s
	gcc $(CFLAGS) $(SOURCE)/AMD64/gcc/psystem.c \
		$(BUILD)/pcom.s -x assembler $(SOURCE)/AMD64/gcc/psystem_asm.asm \
		-o $(PASCALP6)/bin/pcom -lm -lpthread
	cp $(BUILD)/pcom $(PASCALP6)/bin
	
pint: bin/pint
bin/pint: source/pint.pas 
	@echo
	@echo "Building pint..."
	@echo
	mkdir -p $(BUILD)
	gcc -g -o $(BUILD)/externals.o -c $(SOURCE)/externals.c
	cpp $(CPPFLAGS) $(CPPFLAGS64LE) $(SOURCE)/pint.pas $(BUILD)/pint.mpp.pas \
		-DEXTERNALS
	pcom $(BUILD)/pint.mpp.pas $(BUILD)/pint.p6 --reference-
	pgen $(BUILD)/pint.p6 $(BUILD)/pint.s
	gcc $(CFLAGS) $(SOURCE)/AMD64/gcc/psystem.c \
		$(BUILD)/pint.s -x assembler $(SOURCE)/AMD64/gcc/psystem_asm.asm \
		-o $(BUILD)/pint -lm -lpthread
	cp $(BUILD)/pint $(PASCALP6)/bin

pmach: bin/pmach
bin/pmach: source/pmach.pas
	@echo
	@echo "Building pmach..."
	@echo
	mkdir -p $(BUILD)
	cpp $(CPPFLAGS) $(CPPFLAGS64LE) $(SOURCE)/pmach.pas $(BUILD)/pmach.mpp.pas
	pcom $(BUILD)/pmach.mpp.pas $(BUILD)/pmach.p6 --reference-
	pgen $(BUILD)/pmach.p6 $(BUILD)/pmach.s
	gcc $(CFLAGS) $(SOURCE)/AMD64/gcc/psystem.c \
		$(BUILD)/pmach.s -x assembler $(SOURCE)/AMD64/gcc/psystem_asm.asm \
		-o $(BUILD)/pmach -lm -lpthread
	cp $(BUILD)/pmach $(PASCALP6)/bin

cmach: bin/cmach
bin/cmach: source/cmach/cmach.c
	@echo
	@echo "Building cmach..."
	@echo
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/cmach64le $(SOURCE)/cmach/cmach.c -lm
	cp $(BUILD)/cmach64le $(PASCALP6)/bin/cmach

pgen: bin/pgen
bin/pgen: source/AMD64/gcc/pgen.pas
	@echo
	@echo "Building pgen..."
	@echo
	mkdir -p $(BUILD)
	mkdir -p $(BUILD)/AMD64/gcc
	cpp $(CPPFLAGS) $(CPPFLAGS64LE) $(SOURCE)/AMD64/gcc/pgen.pas \
		-I$(PASCALP6)/source $(BUILD)/AMD64/gcc/pgen.mpp.pas
	pcom $(BUILD)/AMD64/gcc/pgen.mpp.pas $(BUILD)/pgen.p6 --reference-
	pgen $(BUILD)/pgen.p6 $(BUILD)/pgen.s
	gcc $(CFLAGS) $(SOURCE)/AMD64/gcc/psystem.c \
		$(BUILD)/pgen.s -x assembler $(SOURCE)/AMD64/gcc/psystem_asm.asm \
		-o $(BUILD)/pgen -lm -lpthread
	cp $(BUILD)/pgen $(PASCALP6)/bin/pgen

genobj: bin/genobj
bin/genobj: source/genobj.pas
	@echo
	@echo "Building genobj..."
	@echo
	mkdir -p $(BUILD)
	pcom $(SOURCE)/genobj.pas $(BUILD)/genobj.p6 --reference-
	pgen $(BUILD)/genobj.p6 $(BUILD)/genobj.s
	gcc $(CFLAGS) $(SOURCE)/AMD64/gcc/psystem.c \
		$(BUILD)/genobj.s -x assembler $(SOURCE)/AMD64/gcc/psystem_asm.asm \
		-o $(BUILD)/genobj -lm -lpthread
	cp $(BUILD)/genobj $(PASCALP6)/bin/genobj

spew: bin/spew
bin/spew: source/spew.c
	@echo
	@echo "Building spew..."
	@echo
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -o $(BUILD)/spew source/spew.c
	cp $(BUILD)/spew $(PASCALP6)/bin/spew
	
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
	find . -name "*.s" -type f -delete
	
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
    
