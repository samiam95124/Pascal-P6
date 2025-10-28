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
LIBS=$(PASCALP6)/libs
AMI=$(PASCALP6)/petit_ami/linux
AMIINC=$(PASCALP6)/petit_ami/include
CPPFLAGS=-P -nostdinc -traditional-cpp
CPPFLAGS64LE=-DWRDSIZ64 -DLENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS16LE=-DWRDSIZ16 -DLENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS64BE=-DWRDSIZ64 -DBENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS16BE=-DWRDSIZ16 -DBENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
EXTERNAL=libs
EXTERNALLIBS=$(EXTERNAL)/services.o

all: bin/cmach bin/pgen bin/spew \
	$(LIBS)/psystem.a $(LIBS)/psystem_asm.o main $(BUILD)/AMD64/gcc/main.o \
	parcmd $(BUILD)/parcmd.o libs/strings.o $(LIBS)/strings.o \
	libs/services.a $(LIBS)/services.a libs/services_test \
	libs/parse.o $(LIBS)/parse.o

################################################################################
#
# Build components
#

################################################################################
#
# Components for compiler and support modules
#

#
# Build psystem, the Pascaline support library in C.
#
$(LIBS)/psystem.a $(LIBS)/psystem_asm.o: $(SOURCE)/AMD64/gcc/psystem.c \
	$(SOURCE)/AMD64/gcc/psystem.asm
	@echo
	@echo "Building psystem..."
	@echo
	mkdir -p $(BUILD)/AMD64/gcc
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/AMD64/gcc/psystem.o \
		-c $(SOURCE)/AMD64/gcc/psystem.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/AMD64/gcc/psystem_asm.o \
		-c -x assembler $(SOURCE)/AMD64/gcc/psystem.asm
	ar r $(LIBS)/psystem.a $(BUILD)/AMD64/gcc/psystem.o \
		$(BUILD)/AMD64/gcc/psystem_asm.o

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
	cp $(BUILD)/AMD64/gcc/main.o $(LIBS)

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

################################################################################
#
# User accessable libraries
#

#
# Build strings, the Pascaline string support library in Pascaline.
#
libs/strings.o $(LIBS)/strings.o: $(LIBS)/source/strings.pas
	@echo
	@echo "Building strings..."
	@echo
	mkdir -p $(BUILD)/libs
	pcom $(LIBS)/source/strings.pas $(BUILD)/libs/strings.p6 --list- --reference-
	pgen $(BUILD)/libs/strings.p6 $(BUILD)/libs/strings.s
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/strings.o \
		-c $(BUILD)/libs/strings.s
	cp $(BUILD)/libs/strings.o $(LIBS)

#
# Build parse, the Pascaline parsre library in Pascaline.
#
libs/parse.o $(LIBS)/parse.o: $(LIBS)/source/parse.pas
	@echo
	@echo "Building parse..."
	@echo
	mkdir -p $(BUILD)/libs
	pcom $(LIBS)/source/parse.pas $(BUILD)/libs/parse.p6 --modules=$(LIBS) \
		--list- --reference-
	pgen $(BUILD)/libs/parse.p6 $(BUILD)/libs/parse.s
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/parse.o \
		-c $(BUILD)/libs/parse.s
	cp $(BUILD)/libs/parse.o $(LIBS)

#
# Services
#
libs/services.a $(LIBS)/services.a: $(AMI)/services.c \
	$(LIBS)/source/services_wrapper.asm
	@echo
	@echo "Building services..."
	@echo
	mkdir -p $(BUILD)/libs
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -I$(AMIINC) -I$(LIBS)/source\
		-o $(BUILD)/libs/services_support.o -c $(LIBS)/source/services_support.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/services_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/services_wrapper.asm
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -I$(AMIINC) -I$(LIBS)/source \
		-o $(BUILD)/libs/services_wrapper.o -c $(LIBS)/source/services_wrapper.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -I$(AMIINC) -I$(LIBS)/source \
		-o $(BUILD)/libs/services.o -c $(AMI)/services.c
	ar r $(LIBS)/services.a $(BUILD)/libs/services_wrapper_asm.o \
		$(BUILD)/libs/services_wrapper.o $(BUILD)/libs/services.o \
		$(BUILD)/libs/services_support.o

################################################################################
#
# Build programs
#

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

################################################################################
#
# Build library tests
#

#
# Build services test.
#
libs/services_test: $(LIBS)/source/services_test.pas $(LIBS)/source/services_test1.pas \
	$(LIBS)/services.a libs/strings.o $(LIBS)/psystem.a
	@echo
	@echo "Building services test..."
	@echo
	mkdir -p $(BUILD)/libs
	pcom --modules=libs/source $(LIBS)/source/services_test.pas \
		$(BUILD)/libs/services_test.p6 --list- --reference-
	pcom --modules=libs/source $(LIBS)/source/services_test1.pas \
		$(BUILD)/libs/services_test1.p6 --list- --reference-
	pgen $(BUILD)/libs/services_test.p6 $(BUILD)/libs/services_test.s
	pgen $(BUILD)/libs/services_test1.p6 $(BUILD)/libs/services_test1.s
	$(CC) $(CFLAGS) \
		$(BUILD)/AMD64/gcc/main.o \
		$(BUILD)/parcmd.o \
		$(LIBS)/strings.o \
		$(BUILD)/libs/services_test.s \
        $(LIBS)/services.a \
		$(LIBS)/psystem.a \
		-o $(LIBS)/services_test -lm -lpthread
	cp $(LIBS)/services_test bin
	$(CC) $(CFLAGS) \
		$(BUILD)/AMD64/gcc/main.o \
		$(BUILD)/parcmd.o \
		$(LIBS)/strings.o \
		$(BUILD)/libs/services_test1.s \
		$(LIBS)/services.a \
		$(LIBS)/psystem.a \
		-o $(LIBS)/services_test1 -lm -lpthread
	cp $(LIBS)/services_test1 bin

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
