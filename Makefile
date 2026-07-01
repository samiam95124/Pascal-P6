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
	$(LIBS)/terminal.a $(LIBS)/graphics.a source/graph/graphics.a \
	$(LIBS)/gnome_widgets.o \
	$(LIBS)/sound.a $(LIBS)/network.a \
	$(BUILD)/cmach/cmach_package.o $(BUILD)/cmach/cmach_package_min.o

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
	$(SOURCE)/pgen/amd64/psystem.asm \
	$(AMILIBC)/stdio.c
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
# The "blonde" graphics archive for the graphics-hosted interpreter (pintg):
# graphics.c compiled with NOSTDWIN, which skips binding stdin/stdout to an
# automatic main window. The model does nothing until an openwin call, so the
# interpreter keeps its own console and gives the interpreted program a
# window. All other members are shared with the standard archive. Placed in
# source/graph so the interpreter's flavor module path selects it ahead of
# libs/graphics.a.
#
source/graph/graphics.a: $(LIBS)/graphics.a
	mkdir -p source/graph
	$(CC) $(CFLAGS) $(GRAPHCPP) -DNOSTDWIN \
		-o $(BUILD)/libs/graphics_blonde.o -c $(AMI)/graphics.c
	rm -f source/graph/graphics.a
	ar rc source/graph/graphics.a $(BUILD)/libs/graphics_wrapper_asm.o \
		$(BUILD)/libs/graphics_wrapper.o $(BUILD)/libs/graphics_support.o \
		$(BUILD)/libs/graphics_blonde.o $(BUILD)/libs/graph_services.o \
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
# cmach is built with -DEXTERNALS so it hosts the Ami external models (services,
# sound and network -- the same external executor pint and pmach carry, emitted
# as C in source/cmach/extern.inc by tools/extgen/gencexec.py). Machine decks
# that call the external libraries therefore run under cmach. The native
# archives and their system-library closure are linked in; services.a routes its
# file I/O through the STDIO_BYPASS bridge cmach implements (psystem_libcrdfil/
# psystem_libcatcfil), backed by the ami stdio in psystem_stdio.o (produced by
# the psystem.a build).
#
# cmach is compiled through the Ami bypass stdio (-DSTDIO_BYPASS -I$(AMILIBC), so
# <stdio.h> resolves to amitk/libc/stdio.h), the same stdio world pmach's psystem
# is built in. This makes cmach's file table hold Ami-stdio files, so the model
# bindings -- which open connections as Ami files (network_support.c stdio_fdopen)
# and write through the Ami stdio -- interoperate with cmach exactly as they do
# with pmach. SEEK_* are supplied because the Ami stdio.h does not export them.
CMACHEXT=-DEXTERNALS -DSTDIO_BYPASS -I$(AMILIBC) -I$(AMIINC) \
	-DSEEK_SET=0 -DSEEK_CUR=1 -DSEEK_END=2
# The fluidsynth and dump synth backends self-register from constructors in
# sound.a's fluidsynthplug.o / dumpsynthplug.o. Nothing references their symbols,
# so an archive link drops both members (pc links the sound module's objects
# directly, so its constructors always run). Without them opensynthout finds no
# software synth and falls back to the raw ALSA sequencer -> no audio. Force both
# members in with -u (whole-archiving sound.a would instead duplicate the
# support.o that services.a already pulls).
CMACHSYNTH=-Wl,-u,getparamfluid -Wl,-u,getparamdump
# libasound links whole-archive (mirroring pc/pc.pas): ALSA's device plugins
# resolve through a registry populated by per-member constructors, so a member
# nothing references statically (e.g. the virtual rawmidi plugin) would be left
# out of the link and lose its registration -> "_snd_rawmidi_virtual_open is not
# defined inside [builtin]" at runtime.
CMACHEXTLIBS=$(CMACHSYNTH) $(LIBS)/services.a $(LIBS)/sound.a $(LIBS)/network.a $(PSYSTEM_STDIO) \
	-lssl -lcrypto -Wl,--whole-archive -lasound -Wl,--no-whole-archive -L/usr/local/lib -lfluidsynth -lglib-2.0 -lpcre -lpthread -ldl -lm

cmach: bin/cmach
bin/cmach: $(SOURCE)/cmach/cmach.c $(SOURCE)/cmach/extern.inc \
		$(LIBS)/services.a $(LIBS)/sound.a $(LIBS)/network.a $(LIBS)/psystem.a
	@echo
	@echo "Building cmach..."
	@echo
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) $(CMACHEXT) -o $(BUILD)/cmach64le \
		$(SOURCE)/cmach/cmach.c $(CMACHEXTLIBS)
	cp $(BUILD)/cmach64le $(PASCALP6)/bin/cmach

# Package-mode cmach objects: cmach.c compiled -DPACKAGE. The per-program deck is
# now a separate program_code.o that pc links against (rather than #included into
# cmach's store[]), so these are prebuilt once and shipped -- package mode needs
# no cmach source. Two builds mirror pc's two package paths: cmach_package.o hosts
# the Ami externals like the standalone cmach; cmach_package_min.o is the minimal
# glibc build for programs that use no externals.
$(BUILD)/cmach/cmach_package.o: $(SOURCE)/cmach/cmach.c $(SOURCE)/cmach/extern.inc
	mkdir -p $(BUILD)/cmach
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) $(CMACHEXT) -DPACKAGE -DGPC=0 \
		-o $(BUILD)/cmach/cmach_package.o -c $(SOURCE)/cmach/cmach.c

$(BUILD)/cmach/cmach_package_min.o: $(SOURCE)/cmach/cmach.c
	mkdir -p $(BUILD)/cmach
	$(CC) $(CFLAGS) -DPACKAGE -DGPC=0 \
		-o $(BUILD)/cmach/cmach_package_min.o -c $(SOURCE)/cmach/cmach.c

# cmacht and cmachg are the terminal and graphics flavors of cmach, mirroring
# pmacht/pmachg: the same cmach.c built with -DTERMINAL / -DGRAPHICS so it hosts
# the terminal / graphics model too (the flavor selects extern_term.inc /
# extern_graph.inc). The flavor's native archive (terminal.a / graphics.a)
# replaces nothing -- it is added; graphics.a is the "blonde" archive that
# provides the terminal core too, so the graphics flavor links graphics.a (not
# terminal.a -- they share the ami_* core and cannot co-link).
cmacht: bin/cmacht
bin/cmacht: $(SOURCE)/cmach/cmach.c $(SOURCE)/cmach/extern_term.inc \
		$(LIBS)/services.a $(LIBS)/terminal.a $(LIBS)/sound.a $(LIBS)/network.a \
		$(LIBS)/psystem.a
	@echo
	@echo "Building cmacht (terminal flavor)..."
	@echo
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) $(CMACHEXT) -DTERMINAL -o $(BUILD)/cmacht64le \
		$(SOURCE)/cmach/cmach.c $(CMACHSYNTH) \
		$(LIBS)/services.a $(LIBS)/terminal.a $(LIBS)/sound.a $(LIBS)/network.a \
		$(PSYSTEM_STDIO) -lssl -lcrypto -Wl,--whole-archive -lasound -Wl,--no-whole-archive -L/usr/local/lib -lfluidsynth -lglib-2.0 -lpcre -lpthread -ldl -lm
	cp $(BUILD)/cmacht64le $(PASCALP6)/bin/cmacht

cmachg: bin/cmachg
# cmachg hosts a graphics window over the interpreted program like pmachg/pintg,
# so it links the "blonde" graphics archive (source/graph/graphics.a, graphics.c
# built -DNOSTDWIN). That archive skips binding stdin/stdout to an automatic main
# window at startup; the interpreter keeps its console and vmhost opens the
# program's window via openwin. The standard libs/graphics.a auto-creates a main
# window at init, which collides with vmhost's openwin and hangs.
bin/cmachg: $(SOURCE)/cmach/cmach.c $(SOURCE)/cmach/extern_graph.inc \
		$(LIBS)/services.a source/graph/graphics.a $(LIBS)/gnome_widgets.o \
		$(LIBS)/sound.a $(LIBS)/network.a $(LIBS)/psystem.a
	@echo
	@echo "Building cmachg (graphics flavor)..."
	@echo
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $(CPPFLAGS64LE) $(CMACHEXT) -DGRAPHICS -o $(BUILD)/cmachg64le \
		$(SOURCE)/cmach/cmach.c $(CMACHSYNTH) \
		-Wl,--start-group \
		$(LIBS)/services.a source/graph/graphics.a $(LIBS)/gnome_widgets.o \
		$(LIBS)/sound.a $(LIBS)/network.a $(PSYSTEM_STDIO) \
		-lssl -lcrypto -Wl,--whole-archive -lasound -Wl,--no-whole-archive -L/usr/local/lib -lfluidsynth -lglib-2.0 -lpcre -lpthread -ldl -lm \
		-lfontconfig -lfreetype -lXext -lX11 -lpng -lz -lbz2 \
		-lexpat -luuid -lxcb -lXau -lXdmcp \
		-Wl,--end-group
	cp $(BUILD)/cmachg64le $(PASCALP6)/bin/cmachg

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
