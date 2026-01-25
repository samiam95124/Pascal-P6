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
	$(LIBS)/psystem.a $(LIBS)/psystem_asm.o main $(BUILD)/pgen/main.o \
	libs/services.a $(LIBS)/services.a

################################################################################
#
# Build components
#

################################################################################
#
# Components for compiler and support modules in C
#

#
# Build psystem for AMD64, the Pascaline support library in C.
#
$(LIBS)/psystem.a $(LIBS)/psystem_asm.o: $(SOURCE)/pgen/psystem.c \
	$(SOURCE)/pgen/amd64/psystem.asm
	@echo
	@echo "Building psystem..."
	@echo
	mkdir -p $(BUILD)/pgen
	mkdir -p $(BUILD)/pgen/amd64
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/pgen/psystem.o \
		-c $(SOURCE)/pgen/psystem.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/pgen/amd64/psystem_asm.o \
		-c -x assembler $(SOURCE)/pgen/amd64/psystem.asm
	ar r $(LIBS)/psystem.a $(BUILD)/pgen/psystem.o \
		$(BUILD)/pgen/amd64/psystem_asm.o

#
# Build main for AMD64, the program stack startup shim.
#
main: $(SOURCE)/pgen/amd64/main.asm
$(BUILD)/pgen/main.o: $(SOURCE)/pgen/amd64/main.asm
	@echo
	@echo "Building main..."
	@echo
	mkdir -p $(BUILD)/pgen
	mkdir -p $(BUILD)/pgen/amd64
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/pgen/amd64/main.o \
		-c -x assembler $(SOURCE)/pgen/amd64/main.asm
	cp $(BUILD)/pgen/amd64/main.o $(LIBS)

################################################################################
#
# User accessable libraries in C
#

#
# Services
#
# Services is built from components since it is an external C library in
# Petit-Ami. The result is an archive services.a.
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
# Build programs in C
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
