################################################################################
#
# Makefile for Pascal-P6
#
# Makes the main compiler/interpreter set.
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
# Note the convention used here is that .asm files are assembly files that were
# manually generated and not to be erased, and .s files are assembly files that
# were generated and can be erased.
#
################################################################################

CC=gcc
CFLAGS=-static -g3 -DWRDSIZ64
SOURCE=$(PASCALP6)/source
BUILD=$(PASCALP6)/build
CPPFLAGS=-P -nostdinc -traditional-cpp
CPPFLAGS64LE=-DWRDSIZ64 -DLENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS16LE=-DWRDSIZ16 -DLENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS64BE=-DWRDSIZ64 -DBENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS16BE=-DWRDSIZ16 -DBENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
EXTERNAL=libs
EXTERNALLIBS=$(EXTERNAL)/services.o

all: bin/pcom bin/pint bin/pmach bin/cmach bin/pgen bin/genobj bin/spew

################################################################################
#
# Build components
#

#
# Build psystem, the Pascaline support library in C.
#
psystem: $(SOURCE)/AMD64/gcc/psystem.c
$(BUILD)/AMD64/gcc/psystem.o: $(SOURCE)/AMD64/gcc/psystem.c
	@echo
	@echo "Building psystem..."
	@echo
	mkdir -p $(BUILD)/AMD64/gcc
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/AMD64/gcc/psystem.o \
		-c $(SOURCE)/AMD64/gcc/psystem.c

#
# Build psystem_asm, the Pascaline support library in assembly.
#
psystem_asm: $(SOURCE)/AMD64/gcc/psystem.asm
$(BUILD)/AMD64/gcc/psystem_asm.o: $(SOURCE)/AMD64/gcc/psystem.asm
	@echo
	@echo "Building psystem_asm..."
	@echo
	mkdir -p $(BUILD)/AMD64/gcc
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/AMD64/gcc/psystem_asm.o \
		-c -x assembler $(SOURCE)/AMD64/gcc/psystem.asm

#
# Build main, the program stack startup shim.
#
main: $(SOURCE)/AMD64/gcc/main.asm
$(BUILD)/AMD64/gcc/main.o: $(SOURCE)/AMD64/gcc/main.asm
	@echo
	@echo "Building main..."
	@echo
	mkdir -p $(BUILD)/AMD64/gcc
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/AMD64/gcc/main.o \
		-c -x assembler $(SOURCE)/AMD64/gcc/main.asm

#
# Build command line parser
#
parcmd: $(SOURCE)/parcmd.pas
$(BUILD)/parcmd.o: $(SOURCE)/parcmd.pas
	@echo
	@echo "Building parcmd..."
	@echo
	pcom $(SOURCE)/parcmd.pas $(BUILD)/parcmd.p6 --list- --reference-
	pgen $(BUILD)/parcmd.p6 $(BUILD)/parcmd.s
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/parcmd.o -c $(BUILD)/parcmd.s

#
# Build simulator externals link module
#
extlink: $(SOURCE)/extlink.pas
$(BUILD)/extlink.o: $(SOURCE)/extlink.pas
	@echo
	@echo "Building externals link..."
	@echo
	pcom $(SOURCE)/extlink.pas $(BUILD)/extlink.p6 --list- --reference-
	pgen $(BUILD)/extlink.p6 $(BUILD)/extlink.s
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/extlink.o -c $(BUILD)/extlink.s

#
# Build simulator extensions module
#
extend: $(SOURCE)/extend.pas
$(BUILD)/extend.o: $(SOURCE)/extend.pas
	@echo
	@echo "Building extensions..."
	@echo
	pcom $(SOURCE)/extend.pas $(BUILD)/extend.p6 --list- --reference-
	pgen $(BUILD)/extend.p6 $(BUILD)/extend.s
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/extend.o -c $(BUILD)/extend.s

################################################################################
#
# Build programs
#

#
# Build pcom, the source to intermediate compiler.
#
pcom: bin/pcom
bin/pcom: $(SOURCE)/pcom.pas $(BUILD)/AMD64/gcc/psystem.o \
	$(BUILD)/AMD64/gcc/psystem_asm.o $(BUILD)/AMD64/gcc/main.o $(BUILD)/parcmd.o \
	$(SOURCE)/lendian/endian.pas $(SOURCE)/mpb64/mpb.pas $(BUILD)/extlink.o \
 	$(BUILD)/extend.o
	@echo
	@echo "Building pcom..."
	@echo
	mkdir -p $(BUILD)
	pcom $(SOURCE)/pcom.pas $(BUILD)/pcom.p6 \
		--modules=$(SOURCE) --modules=$(SOURCE)/lendian --modules=$(SOURCE)/mpb64 \
		--list- --reference-
	pgen $(BUILD)/pcom.p6 $(BUILD)/pcom.s
	gcc $(CFLAGS) $(BUILD)/AMD64/gcc/psystem.o \
		$(BUILD)/AMD64/gcc/psystem_asm.o \
		$(BUILD)/AMD64/gcc/main.o \
		$(BUILD)/parcmd.o \
		$(BUILD)/extlink.o \
		$(BUILD)/extend.o \
		$(BUILD)/pcom.s \
		-o $(BUILD)/pcom -lm -lpthread
	cp $(BUILD)/pcom $(PASCALP6)/bin
	
#
# Build pint, the intermediate interpreter and debug facillity.
#
pint: bin/pint
bin/pint: $(SOURCE)/pint.pas $(BUILD)/AMD64/gcc/psystem.o \
	$(BUILD)/AMD64/gcc/psystem_asm.o $(BUILD)/AMD64/gcc/main.o $(BUILD)/parcmd.o \
	$(SOURCE)/lendian/endian.pas $(SOURCE)/mpb64/mpb.pas $(BUILD)/extlink.o \
    $(BUILD)/extend.o
	@echo
	@echo "Building pint..."
	@echo
	mkdir -p $(BUILD)
	gcc -g -o $(BUILD)/externals.o -c $(SOURCE)/externals.c
	pcom $(SOURCE)/pint.pas $(BUILD)/pint.p6 \
		--modules=$(SOURCE) --modules=$(SOURCE)/lendian --modules=$(SOURCE)/mpb64 \
		--chk- --list- --reference-
	pgen $(BUILD)/pint.p6 $(BUILD)/pint.s
	gcc $(CFLAGS) $(BUILD)/AMD64/gcc/psystem.o \
		$(BUILD)/AMD64/gcc/psystem_asm.o \
		$(BUILD)/AMD64/gcc/main.o \
		$(BUILD)/parcmd.o \
		$(BUILD)/extlink.o \
		$(BUILD)/extend.o \
		$(BUILD)/pint.s \
		-o $(BUILD)/pint -lm -lpthread
	cp $(BUILD)/pint $(PASCALP6)/bin

#
# Build pmach, an intermediate interpreter without assembler.
#
pmach: bin/pmach
bin/pmach: $(SOURCE)/pmach.pas $(BUILD)/AMD64/gcc/psystem.o \
	$(BUILD)/AMD64/gcc/psystem_asm.o $(BUILD)/AMD64/gcc/main.o $(BUILD)/parcmd.o \
	$(SOURCE)/lendian/endian.pas $(SOURCE)/mpb64/mpb.pas $(BUILD)/extlink.o
	@echo
	@echo "Building pmach..."
	@echo
	mkdir -p $(BUILD)
	pcom $(SOURCE)/pmach.pas $(BUILD)/pmach.p6 \
		--modules=$(SOURCE) --modules=$(SOURCE)/lendian --modules=$(SOURCE)/mpb64 \
		--chk- --list- --reference-
	pgen $(BUILD)/pmach.p6 $(BUILD)/pmach.s
	gcc $(CFLAGS) $(BUILD)/AMD64/gcc/psystem.o \
		$(BUILD)/AMD64/gcc/psystem_asm.o \
		$(BUILD)/AMD64/gcc/main.o \
		$(BUILD)/parcmd.o \
		$(BUILD)/extlink.o \
		$(BUILD)/pmach.s \
		-o $(BUILD)/pmach -lm -lpthread
	cp $(BUILD)/pmach $(PASCALP6)/bin

#
# Build cmach, an intermediate interpreter written in C.
#
cmach: bin/cmach
bin/cmach: $(SOURCE)/cmach/cmach.c
	@echo
	@echo "Building cmach..."
	@echo
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/cmach64le $(SOURCE)/cmach/cmach.c -lm
	cp $(BUILD)/cmach64le $(PASCALP6)/bin/cmach

#
# Build pgen, the intermediate to assembly code generator for AMD64 mode 
# processors.
#
# This is always 64 bit little endian.
#
pgen: bin/pgen
bin/pgen: $(SOURCE)/AMD64/gcc/pgen.pas $(BUILD)/AMD64/gcc/psystem.o \
	$(BUILD)/AMD64/gcc/psystem_asm.o $(BUILD)/AMD64/gcc/main.o $(BUILD)/parcmd.o \
	$(SOURCE)/lendian/endian.pas $(SOURCE)/mpb64/mpb.pas
	@echo
	@echo "Building pgen..."
	@echo
	mkdir -p $(BUILD)/AMD64/gcc
	pcom $(SOURCE)/AMD64/gcc/pgen.pas $(BUILD)/AMD64/gcc/pgen.p6 \
		--modules=$(SOURCE) --modules=$(SOURCE)/lendian --modules=$(SOURCE)/mpb64 --list- --reference-
	pgen $(BUILD)/AMD64/gcc/pgen.p6 $(BUILD)/AMD64/gcc/pgen.s
	gcc $(CFLAGS) $(BUILD)/AMD64/gcc/psystem.o \
		$(BUILD)/AMD64/gcc/psystem_asm.o \
		$(BUILD)/AMD64/gcc/main.o \
        $(BUILD)/parcmd.o \
		$(BUILD)/AMD64/gcc/pgen.s \
		-o $(BUILD)/AMD64/gcc/pgen -lm -lpthread
	cp $(BUILD)/AMD64/gcc/pgen $(PASCALP6)/bin/pgen

#
# Build genobj, an object code to C array converter.
#
genobj: bin/genobj
bin/genobj: $(SOURCE)/genobj.pas $(BUILD)/AMD64/gcc/psystem.o \
	$(BUILD)/AMD64/gcc/psystem_asm.o $(BUILD)/AMD64/gcc/main.o
	@echo
	@echo "Building genobj..."
	@echo
	mkdir -p $(BUILD)
	pcom $(SOURCE)/genobj.pas $(BUILD)/genobj.p6 --list- --reference-
	pgen $(BUILD)/genobj.p6 $(BUILD)/genobj.s
	gcc $(CFLAGS) $(BUILD)/AMD64/gcc/psystem.o \
		$(BUILD)/AMD64/gcc/psystem_asm.o \
		$(BUILD)/AMD64/gcc/main.o \
		$(BUILD)/genobj.s \
		-o $(BUILD)/genobj -lm -lpthread
	cp $(BUILD)/genobj $(PASCALP6)/bin/genobj

#
# Build spew, an automated test facillity.
#
spew: bin/spew
bin/spew: $(SOURCE)/spew.c
	@echo
	@echo "Building spew..."
	@echo
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -o $(BUILD)/spew $(SOURCE)/spew.c
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
	find . -name "*.o" -type f -delete
	
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
	@echo pgen			Make pgen, a intermediate to AMD64 assembly language generator.
	@echo
	@echo genobj        Make genobj, the binary deck to C file generator.
	@echo
	@echo spew          Make spew, a fault generator test program.
	@echo
	@echo clean         Clean intermediate/temp files from tree.
	@echo