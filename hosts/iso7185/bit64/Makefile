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
CPPFLAGS64LE=-DWRDSIZ64 -DLENDIAN -DPASCALINE -DBIG_STORE -DNOPRDPRR -DGPC=0
CPPFLAGS16LE=-DWRDSIZ16 -DLENDIAN -DPASCALINE -DBIG_STORE -DNOPRDPRR -DGPC=0
CPPFLAGS64BE=-DWRDSIZ64 -DBENDIAN -DPASCALINE -DBIG_STORE -DNOPRDPRR -DGPC=0
CPPFLAGS16BE=-DWRDSIZ16 -DBENDIAN -DPASCALINE -DBIG_STORE -DNOPRDPRR -DGPC=0
EXTERNAL=libs
EXTERNALLIBS=$(EXTERNAL)/services.o

all: bin/pcom bin/pint bin/pmach bin/cmach bin/pgen bin/genobj bin/spew

bin/pcom: source/pcom.pas
	@echo
	@echo "Building pcom..."
	@echo
	pascpp $(PASCALP6)/source/pcom $(CPPFLAGS64LE)
	pcom $(PASCALP6)/source/pcom.mpp.pas $(PASCALP6)/source/pcom.p6 --reference-
	pgen $(PASCALP6)/source/pcom.p6 $(PASCALP6)/source/pcom.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s $(PASCALP6)/source/pcom.s \
		-o $(PASCALP6)/bin/pcom -lm -lpthread

bin/pcom_immerr: source/pcom.pas
	@echo
	@echo "Building pcom with immediate error..."
	@echo
	pascpp $(PASCALP6)/source/pcom $(CPPFLAGS64LE) -DIMM_ERR
	pcom $(PASCALP6)/source/pcom.mpp.pas $(PASCALP6)/source/pcom.p6 --reference-
	pgen $(PASCALP6)/source/pcom.p6 $(PASCALP6)/source/pcom.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s $(PASCALP6)/source/pcom.s \
		-o $(PASCALP6)/bin/pcom -lm -lpthread
	
bin/pint: source/pint.pas 
	@echo
	@echo "Building pint..."
	@echo
	gcc -g -o $(PASCALP6)/source/externals.o -c $(PASCALP6)/source/externals.c
	pascpp $(PASCALP6)/source/pint $(CPPFLAGS64LE) -DEXTERNALS
	pcom $(PASCALP6)/source/pint.mpp.pas $(PASCALP6)/source/pint.p6 --reference-
	pgen $(PASCALP6)/source/pint.p6 $(PASCALP6)/source/pint.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s $(PASCALP6)/source/pint.s \
		-o $(PASCALP6)/bin/pint -lm -lpthread

bin/pmach: source/pmach.pas
	@echo
	@echo "Building pmach..."
	@echo
	pascpp $(PASCALP6)/source/pmach $(CPPFLAGS64LE)
	pcom $(PASCALP6)/source/pmach.mpp.pas $(PASCALP6)/source/pmach.p6 --reference-
	pgen $(PASCALP6)/source/pmach.p6 $(PASCALP6)/source/pmach.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s $(PASCALP6)/source/pmach.s \
		-o $(PASCALP6)/bin/pmach -lm -lpthread

bin/cmach: source/cmach/cmach.c
	@echo
	@echo "Building cmach..."
	@echo
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(PASCALP6)/bin/cmach64le $(PASCALP6)/source/cmach/cmach.c -lm
	cp $(PASCALP6)/bin/cmach64le $(PASCALP6)/bin/cmach

bin/pgen: source/AMD64/gcc/pgen.pas
	@echo
	@echo "Building pgen..."
	@echo
	pascpp $(PASCALP6)/source/AMD64/gcc/pgen -Isource $(CPPFLAGS64LE) -DGNU_PASCAL
	pcom $(PASCALP6)/source/AMD64/gcc/pgen.mpp.pas $(PASCALP6)/source/pgen.p6 --reference-
	pgen $(PASCALP6)/source/pgen.p6 $(PASCALP6)/source/pgen.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s source/pgen.s \
		-o $(PASCALP6)/bin/pgen -lm -lpthread

bin/genobj: source/genobj.pas
	@echo
	@echo "Building genobj..."
	@echo
	pcom $(PASCALP6)/source/genobj.pas $(PASCALP6)/source/genobj.p6 --reference-
	pgen $(PASCALP6)/source/genobj.p6 $(PASCALP6)/source/genobj.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s $(PASCALP6)/source/genobj.s \
		-o $(PASCALP6)/bin/genobj -lm -lpthread

bin/spew: source/spew.c
	@echo
	@echo "Building spew..."
	@echo
	$(CC) $(CFLAGS) -o $(PASCALP6)/bin/spew source/spew.c
	
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
    
