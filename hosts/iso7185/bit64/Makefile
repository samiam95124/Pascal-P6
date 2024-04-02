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
CPPFLAGS64LE=-DWRDSIZ64 -DLENDIAN -DISO7185_PASCAL -DSELF_COMPILE -DBIG_STORE
CPPFLAGS16LE=-DWRDSIZ16 -DLENDIAN -DISO7185_PASCAL -DSELF_COMPILE -DBIG_STORE
CPPFLAGS64BE=-DWRDSIZ64 -DBENDIAN -DISO7185_PASCAL -DSELF_COMPILE -DBIG_STORE
CPPFLAGS16BE=-DWRDSIZ16 -DBENDIAN -DISO7185_PASCAL -DSELF_COMPILE -DBIG_STORE
EXTERNAL=libs
EXTERNALLIBS=$(EXTERNAL)/services.o

all: bin/pcom bin/pint bin/pmach bin/cmach bin/pgen bin/genobj bin/spew

bin/pcom: source/pcom.pas
	pascpp source/pcom $(CPPFLAGS64LE)
	pcom_wrap source/pcom.mpp.pas source/pcom.p6
	pgen_wrap source/pcom.p6 source/pcom.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s source/pcom.s -o bin/pcom -lm -lpthread

bin/pcom_immerr: source/pcom.pas
	pascpp source/pcom $(CPPFLAGS64LE) -DIMM_ERR
	pcom_wrap source/pcom.mpp.pas source/pcom.p6
	pgen_wrap source/pcom.p6 source/pcom.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s source/pcom.s -o bin/pcom -lm -lpthread
	
bin/pint: source/pint.pas 
	gcc -g -o source/externals.o -c source/externals.c
	pascpp source/pint  $(CPPFLAGS64LE) -DEXTERNALS
	pcom_wrap source/pint.mpp.pas source/pint.p6
	pgen_wrap source/pint.p6 source/pint.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s source/pint.s -o bin/pint -lm -lpthread

bin/pmach: source/pmach.pas
	pascpp source/pmach $(CPPFLAGS64LE)
	pcom_wrap source/pmach.mpp.pas source/pmach.p6
	pgen_wrap source/pmach.p6 source/pmach.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s source/pmach.s -o bin/pmach -lm -lpthread

bin/cmach: source/cmach/cmach.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o bin/cmach64le source/cmach/cmach.c -lm
	cp bin/cmach64le bin/cmach

bin/pgen: source/AMD64/gcc/pgen.pas
	pascpp source/AMD64/gcc/pgen -Isource $(CPPFLAGS64LE) -DGNU_PASCAL
	pcom_wrap source/AMD64/gcc/pgen.mpp.pas source/pgen.p6
	pgen_wrap source/pgen.p6 source/pgen.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s source/pgen.s -o bin/pgen -lm -lpthread

bin/genobj: source/genobj.pas
	pcom_wrap source/genobj.pas source/genobj.p6
	pgen_wrap source/genobj.p6 source/genobj.s
	gcc $(CFLAGS) $(PASCALP6)/source/AMD64/gcc/psystem.c \
		$(PASCALP6)/source/AMD64/gcc/psystem_asm.s source/genobj.s -o bin/genobj -lm -lpthread

bin/spew: source/spew.c
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
    
