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
AMI=$(PASCALP6)/amitk/linux
AMIINC=$(PASCALP6)/amitk/include
AMILIBC=$(PASCALP6)/amitk/libc
CPPFLAGS=-P -nostdinc -traditional-cpp
CPPFLAGS64LE=-DWRDSIZ64 -DLENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS16LE=-DWRDSIZ16 -DLENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS64BE=-DWRDSIZ64 -DBENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
CPPFLAGS16BE=-DWRDSIZ16 -DBENDIAN -DPASCALINE -DNOPRDPRR -DNOHEADER
EXTERNAL=libs

#
# I/O bypass mode.
#
# When STDIO_BYPASS is true (the normal mode), psystem is built to route its
# stdio through the Ami stdio implementation (amitk/libc/stdio.c,
# compiled with -DSTDIO_BYPASS). Because psystem is the single I/O point for all
# Pascaline programs, this makes every write/read pass through the Ami I/O
# override layer (ovr_*/vt_*), so the terminal, graphics and other Ami
# models can hook the program's console I/O. With no model installed the
# override defaults to the real system I/O, so non-model programs are unaffected.
#
# When false, psystem uses the system stdio directly and I/O hooks do not work.
# Both modes are expected to function identically except for I/O hooking.
#
STDIO_BYPASS=true

ifeq ($(STDIO_BYPASS),true)
# Ami's stdio.h does not export the fseek origin constants (its own stdio.c
# gets them from a system header); supply the standard values for psystem.
PSYSTEM_BYPASS=-DSTDIO_BYPASS -I$(AMILIBC) -I$(AMIINC) \
	-DSEEK_SET=0 -DSEEK_CUR=1 -DSEEK_END=2
PSYSTEM_STDIO=$(BUILD)/pgen/psystem_stdio.o
else
PSYSTEM_BYPASS=
PSYSTEM_STDIO=
endif

all: bin/cmach bin/spew \
	$(LIBS)/psystem.a main $(BUILD)/pgen/amd64/main.o $(LIBS)/services.a \
	$(LIBS)/terminal.a $(LIBS)/graphics.a $(LIBS)/gnome_widgets.o \
	$(LIBS)/sound.a $(LIBS)/network.a

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
$(LIBS)/psystem.a: $(SOURCE)/pgen/psystem.c \
	$(SOURCE)/pgen/amd64/psystem.asm
	@echo
	@echo "Building psystem..."
	@echo
	mkdir -p $(BUILD)/pgen
	mkdir -p $(BUILD)/pgen/amd64
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) $(PSYSTEM_BYPASS) -o $(BUILD)/pgen/psystem.o \
		-c $(SOURCE)/pgen/psystem.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/pgen/amd64/psystem_asm.o \
		-c -x assembler $(SOURCE)/pgen/amd64/psystem.asm
	if [ -n "$(PSYSTEM_STDIO)" ]; then \
		$(CC) $(CFLAGS) $(CPPFLAGS64LE) -DSTDIO_BYPASS -I$(AMILIBC) -I$(AMIINC) \
			-o $(PSYSTEM_STDIO) -c $(AMILIBC)/stdio.c; \
	fi
	ar rc $(LIBS)/psystem.a $(BUILD)/pgen/psystem.o \
		$(BUILD)/pgen/amd64/psystem_asm.o $(PSYSTEM_STDIO)

#
# Build main for AMD64, the program stack startup shim.
#
main $(BUILD)/pgen/amd64/main.o: $(SOURCE)/pgen/amd64/main.asm
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
# Common Pascaline/C support, shared by services, terminal and graphics. Holds
# the Pascaline<->C string conversions and the Pascaline call/event-thunk
# machinery that do not depend on any one binding's module header. Built once
# and bundled into each binding archive. It uses no FILE/Ami internals, so
# a single object serves all three (bypass and non-bypass) archives.
#
$(BUILD)/libs/support.o: $(LIBS)/source/support.c \
	$(LIBS)/source/support.h
	mkdir -p $(BUILD)/libs
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -I$(LIBS)/source \
		-o $(BUILD)/libs/support.o -c $(LIBS)/source/support.c

#
# Services is built from components since it is an external C library in
# Ami. The result is an archive services.a.
#
# Built with -DSTDIO_BYPASS and the Ami libc include path, the same as terminal
# and graphics: psystem (the I/O point) runs in bypass mode, so the FILE* it
# hands to a wrapper is an Ami-stdio FILE. The services file routines (writetime,
# writedate, ...) must use the matching Ami stdio, or they would dereference that
# FILE as a system FILE and crash.
#
SERVCPP=$(CPPFLAGS64LE) -DSTDIO_BYPASS -I$(AMILIBC) -I$(AMIINC) -I$(LIBS)/source
$(LIBS)/services.a: $(AMI)/services.c \
	$(LIBS)/source/services_wrapper.asm \
	$(LIBS)/source/services_wrapper.c \
	$(LIBS)/source/services_support.c \
	$(LIBS)/source/services_wrapper.h \
	$(LIBS)/source/support.h \
	$(BUILD)/libs/support.o
	@echo
	@echo "Building services..."
	@echo
	mkdir -p $(BUILD)/libs
	$(CC) $(CFLAGS) $(SERVCPP) \
		-o $(BUILD)/libs/services_support.o -c $(LIBS)/source/services_support.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/services_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/services_wrapper.asm
	$(CC) $(CFLAGS) $(SERVCPP) \
		-o $(BUILD)/libs/services_wrapper.o -c $(LIBS)/source/services_wrapper.c
	$(CC) $(CFLAGS) $(SERVCPP) \
		-o $(BUILD)/libs/services.o -c $(AMI)/services.c
	rm -f $(LIBS)/services.a
	ar rc $(LIBS)/services.a $(BUILD)/libs/services_wrapper_asm.o \
		$(BUILD)/libs/services_wrapper.o $(BUILD)/libs/services.o \
		$(BUILD)/libs/services_support.o $(BUILD)/libs/support.o

#
# Terminal
#
# Terminal is built from components since it is an external C library in
# Ami. The result is an archive terminal.a.
#
# The Ami terminal model intercepts console I/O via the libc override
# vectors (ovr_*) supplied by Ami's own stdio (libc/stdio.c). All the
# Ami sources and the wrappers are therefore built with -DSTDIO_BYPASS
# and the Ami libc include path, so stdio calls route through that
# implementation rather than the system libc, and no patched glibc is needed.
# The bundled base modules (services, config, system_event, option, stdio) are
# the console-model dependencies pulled in by terminal.c.
#
TERMCPP=$(CPPFLAGS64LE) -DSTDIO_BYPASS -I$(AMILIBC) -I$(AMIINC) -I$(LIBS)/source
$(LIBS)/terminal.a: $(AMI)/terminal.c \
	$(LIBS)/source/terminal_wrapper.asm \
	$(LIBS)/source/terminal_wrapper.c \
	$(LIBS)/source/terminal_support.c \
	$(LIBS)/source/support.h \
	$(BUILD)/libs/support.o
	@echo
	@echo "Building terminal..."
	@echo
	mkdir -p $(BUILD)/libs
	$(CC) $(CFLAGS) $(TERMCPP) \
		-o $(BUILD)/libs/terminal_support.o -c $(LIBS)/source/terminal_support.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/terminal_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/terminal_wrapper.asm
	$(CC) $(CFLAGS) $(TERMCPP) \
		-o $(BUILD)/libs/terminal_wrapper.o -c $(LIBS)/source/terminal_wrapper.c
	$(CC) $(CFLAGS) $(TERMCPP) \
		-o $(BUILD)/libs/terminal.o -c $(AMI)/terminal.c
	$(CC) $(CFLAGS) $(TERMCPP) \
		-o $(BUILD)/libs/term_services.o -c $(AMI)/services.c
	$(CC) $(CFLAGS) $(TERMCPP) \
		-o $(BUILD)/libs/system_event.o -c $(AMI)/system_event.c
	$(CC) $(CFLAGS) $(TERMCPP) \
		-o $(BUILD)/libs/config.o -c $(PASCALP6)/amitk/utils/config.c
	rm -f $(LIBS)/terminal.a
	ar rc $(LIBS)/terminal.a $(BUILD)/libs/terminal_wrapper_asm.o \
		$(BUILD)/libs/terminal_wrapper.o $(BUILD)/libs/terminal_support.o \
		$(BUILD)/libs/terminal.o $(BUILD)/libs/term_services.o \
		$(BUILD)/libs/system_event.o $(BUILD)/libs/config.o \
		$(BUILD)/libs/support.o

#
# Graphics
#
# Graphics is a superset of terminal (text surface + graphical surface +
# windowing + widgets). It is built from components into an archive graphics.a,
# the same way as terminal.a but with graphics.c and its font dependencies.
# graphics.c renders through X11/FreeType/FontConfig, so it is compiled with
# those include paths and programs that use graphics.a must link
# -lX11 -lfreetype -lfontconfig (in addition to the usual -lm -lpthread).
#
GRAPHCFG=$(shell pkg-config --cflags freetype2 fontconfig)
GRAPHCPP=$(CPPFLAGS64LE) -DSTDIO_BYPASS -I$(AMILIBC) -I$(AMIINC) -I$(LIBS)/source \
	$(GRAPHCFG)
$(LIBS)/graphics.a: $(AMI)/graphics.c \
	$(LIBS)/source/graphics_wrapper.asm \
	$(LIBS)/source/graphics_wrapper.c \
	$(LIBS)/source/graphics_support.c \
	$(LIBS)/source/support.h \
	$(BUILD)/libs/support.o
	@echo
	@echo "Building graphics..."
	@echo
	mkdir -p $(BUILD)/libs
	$(CC) $(CFLAGS) $(GRAPHCPP) \
		-o $(BUILD)/libs/graphics_support.o -c $(LIBS)/source/graphics_support.c
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/graphics_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/graphics_wrapper.asm
	$(CC) $(CFLAGS) $(GRAPHCPP) \
		-o $(BUILD)/libs/graphics_wrapper.o -c $(LIBS)/source/graphics_wrapper.c
	$(CC) $(CFLAGS) $(GRAPHCPP) \
		-o $(BUILD)/libs/graphics.o -c $(AMI)/graphics.c
	$(CC) $(CFLAGS) $(GRAPHCPP) \
		-o $(BUILD)/libs/graph_services.o -c $(AMI)/services.c
	$(CC) $(CFLAGS) $(GRAPHCPP) \
		-o $(BUILD)/libs/graph_system_event.o -c $(AMI)/system_event.c
	$(CC) $(CFLAGS) $(GRAPHCPP) \
		-o $(BUILD)/libs/graph_config.o -c $(PASCALP6)/amitk/utils/config.c
	rm -f $(LIBS)/graphics.a
	ar rc $(LIBS)/graphics.a $(BUILD)/libs/graphics_wrapper_asm.o \
		$(BUILD)/libs/graphics_wrapper.o $(BUILD)/libs/graphics_support.o \
		$(BUILD)/libs/graphics.o $(BUILD)/libs/graph_services.o \
		$(BUILD)/libs/graph_system_event.o $(BUILD)/libs/graph_config.o \
		$(BUILD)/libs/support.o

#
# Gnome widgets, the portable widget set drawn with the graphics API. It
# overrides the graphics widget stubs from a constructor and exports no
# symbols, so it cannot be pulled from an archive by an undefined reference;
# pc links it as an explicit object in windowed programs. Built with the same
# flags as graphics.o (STDIO_BYPASS: it prints to Ami-stdio FILEs).
#
$(LIBS)/gnome_widgets.o: $(PASCALP6)/amitk/portable/gnome_widgets.c
	$(CC) $(CFLAGS) $(GRAPHCPP) \
		-o $(LIBS)/gnome_widgets.o -c $(PASCALP6)/amitk/portable/gnome_widgets.c

#
# Sound and network bindings. Unlike the I/O models, these live in the plain
# glibc stdio world (no STDIO_BYPASS): sound's API carries no files at all,
# and network's connection files are bridged into the Pascaline runtime at
# the descriptor level (network_support.c). Both archives bundle support.o
# for the string conversions, which are stdio-free and safe in either world.
# Sound bundles both synthesizer plugins (dump and fluidsynth). The static
# libasound and libfluidsynth are locally built and installed in
# /usr/local/lib by tools/staticdeps/build.sh (the distribution carries only
# their shared libraries); network's libssl/libcrypto ship with the
# distribution. pc adds the per-library link closures.
#
SNDNETCPP=$(CPPFLAGS64LE) -I$(AMIINC) -I$(LIBS)/source

$(LIBS)/sound.a: $(AMI)/sound.c $(AMI)/dumpsynthplug.c $(AMI)/fluidsynthplug.c \
	$(LIBS)/source/sound_wrapper.asm \
	$(LIBS)/source/sound_wrapper.c \
	$(BUILD)/libs/support.o
	@echo
	@echo "Building sound..."
	@echo
	mkdir -p $(BUILD)/libs
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/sound_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/sound_wrapper.asm
	$(CC) $(CFLAGS) $(SNDNETCPP) \
		-o $(BUILD)/libs/sound_wrapper.o -c $(LIBS)/source/sound_wrapper.c
	$(CC) $(CFLAGS) $(SNDNETCPP) \
		-o $(BUILD)/libs/sound.o -c $(AMI)/sound.c
	$(CC) $(CFLAGS) $(SNDNETCPP) \
		-o $(BUILD)/libs/dumpsynthplug.o -c $(AMI)/dumpsynthplug.c
	$(CC) $(CFLAGS) $(SNDNETCPP) \
		-o $(BUILD)/libs/fluidsynthplug.o -c $(AMI)/fluidsynthplug.c
	rm -f $(LIBS)/sound.a
	ar rc $(LIBS)/sound.a $(BUILD)/libs/sound_wrapper_asm.o \
		$(BUILD)/libs/sound_wrapper.o $(BUILD)/libs/sound.o \
		$(BUILD)/libs/dumpsynthplug.o $(BUILD)/libs/fluidsynthplug.o \
		$(BUILD)/libs/support.o

$(LIBS)/network.a: $(AMI)/network.c \
	$(LIBS)/source/network_wrapper.asm \
	$(LIBS)/source/network_wrapper.c \
	$(LIBS)/source/network_support.c \
	$(BUILD)/libs/support.o
	@echo
	@echo "Building network..."
	@echo
	mkdir -p $(BUILD)/libs
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/libs/network_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/network_wrapper.asm
	$(CC) $(CFLAGS) $(SNDNETCPP) \
		-o $(BUILD)/libs/network_wrapper.o -c $(LIBS)/source/network_wrapper.c
	$(CC) $(CFLAGS) $(SNDNETCPP) \
		-o $(BUILD)/libs/network_support.o -c $(LIBS)/source/network_support.c
	$(CC) $(CFLAGS) $(SNDNETCPP) \
		-o $(BUILD)/libs/network.o -c $(AMI)/network.c
	rm -f $(LIBS)/network.a
	ar rc $(LIBS)/network.a $(BUILD)/libs/network_wrapper_asm.o \
		$(BUILD)/libs/network_wrapper.o $(BUILD)/libs/network_support.o \
		$(BUILD)/libs/network.o $(BUILD)/libs/support.o

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
	
help:
	@echo
	@echo Make targets:
	@echo
	@echo All	Make all binaries
	@echo
	@echo cmach         Make cmach, the stand-alone interpreter written in C.
	@echo
	@echo spew          Make spew, a fault generator test program.
	@echo
	@echo clean         Clean intermediate/temp files from tree.
	@echo
