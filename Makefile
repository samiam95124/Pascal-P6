################################################################################
#
# Makefile for Pascal-P6
#
# Makes the C-built components: the runtime support libraries, the C
# interpreter (cmach and its flavors) and the test tools. The Pascal-built
# tools (pcom, pgen, pint, pmach, ...) are built with pc; the hostinstall
# target snapshots them, along with everything this makefile builds, into the
# hosts tree.
#
# This is the single makefile for all hosts. It determines the host it is
# running on, the architecture and the bit length, and places each product it
# builds into the matching directory of the hosts tree:
#
#     hosts/<host>/<arch>/<bits>/bin
#     hosts/<host>/<arch>/<bits>/lib
#
# where <host> is linux, bsd, windows or mac, <arch> is x86, arm or riscv,
# and <bits> is bit32 or bit64. Products for the running host are also copied
# to the top of tree bin and libs directories, so it is not necessary to state
# the characteristics of the host. Cross compiled products (the win64 and
# arm64 runtime libraries when built on another host) are placed in the
# directory of the host they run on.
#
# Note the convention used here is that .asm files are assembly files that were
# manually generated and not to be erased, and .s files are assembly files that
# were generated and can be erased.
#
################################################################################

#
# Determine the running host, architecture and bit length. If the OS
# environment variable is set we are on Windows; otherwise uname names a Unix
# variant. uname -m gives the architecture and implies the bit length.
#
ifeq ($(OS),Windows_NT)
    OSTYPE=Windows_NT
    HOST=windows
else
    OSTYPE=$(shell uname)
    ifeq ($(OSTYPE),Linux)
        HOST=linux
    else ifeq ($(OSTYPE),Darwin)
        HOST=mac
    else ifneq ($(findstring BSD,$(OSTYPE)),)
        HOST=bsd
    else
        HOST=linux
    endif
endif

MACHINE=$(shell uname -m)
ifneq ($(filter x86_64 amd64,$(MACHINE)),)
    ARCH=x86
    BITS=bit64
else ifneq ($(filter i386 i486 i586 i686,$(MACHINE)),)
    ARCH=x86
    BITS=bit32
else ifeq ($(MACHINE),aarch64)
    ARCH=arm
    BITS=bit64
else ifneq ($(findstring armv,$(MACHINE)),)
    ARCH=arm
    BITS=bit32
else ifeq ($(MACHINE),riscv64)
    ARCH=riscv
    BITS=bit64
else ifeq ($(MACHINE),riscv32)
    ARCH=riscv
    BITS=bit32
else
    ARCH=x86
    BITS=bit64
endif

#
# The hosts tree directory for the running host, and the fixed directories
# for the cross compiled runtimes: the win64 runtime runs on windows/x86 and
# the arm64 runtime on linux/arm, wherever they are built.
#
HOSTCELL=hosts/$(HOST)/$(ARCH)/$(BITS)
WINCELL=hosts/windows/x86/bit64
ARMCELL=hosts/linux/arm/bit64

#
# The default build target for each host.
#
ifeq ($(HOST),windows)
    HOSTTARGET=win64
else
    HOSTTARGET=all
endif

CC=gcc
CFLAGS=-static -g3 -DWRDSIZ64

#
# Windows x64 toolchain. On a Windows host the native gcc is used; on any
# other host the mingw-w64 cross toolchain is used (Ubuntu package
# gcc-mingw-w64-x86-64). The products run on Windows, or under Wine.
#
ifeq ($(OSTYPE),Windows_NT)
WINCC=gcc
WINAR=ar
else
WINCC=x86_64-w64-mingw32-gcc
WINAR=x86_64-w64-mingw32-ar
endif
# __USE_MINGW_ANSI_STDIO selects mingw's own C99 compliant printf family
# over the Microsoft C runtime's (2 digit float exponents like glibc, so
# real number output matches the linux build)
WINCFLAGS=-static -g3 -DWRDSIZ64 -D__USE_MINGW_ANSI_STDIO=1

#
# arm64 (aarch64 linux) toolchain. On an arm64 host the native gcc is used;
# on any other host the aarch64 cross toolchain is used (Ubuntu package
# gcc-aarch64-linux-gnu). The products run on arm64 linux, or under
# qemu-aarch64 emulation.
#
ifeq ($(shell uname -m),aarch64)
ARMCC=gcc
ARMAR=ar
else
ARMCC=aarch64-linux-gnu-gcc
ARMAR=aarch64-linux-gnu-ar
endif
ARMCFLAGS=-static -g3 -DWRDSIZ64

SOURCE=$(PASCALP6)/source
BUILD=$(PASCALP6)/build
LIBS=$(PASCALP6)/libs
AMI=$(PASCALP6)/amitk/linux
AMIWIN=$(PASCALP6)/amitk/windows
AMIINC=$(PASCALP6)/amitk/include
AMILIBC=$(PASCALP6)/amitk/libc
# The amitk (Petit-Ami) submodule carries the C sources for the I/O model
# bindings. Release source archives do not include submodules, so the standard
# build must work without it: the built products (the libs archives, the
# widget object and the bypass stdio object) are committed to this repo, and
# when the amitk sources are absent the rules that compile them are disabled
# and the committed products are used as they are.
AMITK=$(wildcard $(AMI)/services.c)
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

#
# The default build is for the current host. all is the standard native
# build; win64 builds the Windows x64 runtime components (cross compiled via
# mingw-w64 on non-Windows hosts) and arm64 the aarch64 linux runtime
# components (cross compiled via the aarch64 toolchain on other hosts).
#
default: $(HOSTTARGET)

linux64: all

win64: $(LIBS)/win64/psystem.a $(LIBS)/win64/main.o $(LIBS)/win64/services.a \
	$(LIBS)/win64/terminal.a $(LIBS)/win64/graphics.a \
	source/graph/win64/graphics.a $(LIBS)/win64/gnome_widgets.o \
	$(LIBS)/win64/sound.a $(LIBS)/win64/network.a

arm64: $(LIBS)/arm64/psystem.a $(LIBS)/arm64/main.o $(LIBS)/arm64/services.a

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
ifneq ($(AMITK),)
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
	mkdir -p $(HOSTCELL)/lib
	cp $(LIBS)/psystem.a $(HOSTCELL)/lib
endif

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
	mkdir -p $(HOSTCELL)/lib
	cp $(BUILD)/pgen/amd64/main.o $(HOSTCELL)/lib

################################################################################
#
# Build the Windows x64 runtime components
#
# These are the components required to link programs generated by pgen in
# Windows calling convention mode (--win64). The hand written assembly
# sources are shared with the linux build; the win64 assembly defines the
# WINDOWS symbol so that ELF only directives are skipped (see main.asm).
#
# psystem uses the C runtime stdio directly (no Ami stdio override), so plain
# batch programs -- including the file-heavy conformance tests -- run under
# Wine exactly as before. Routing psystem's stdio through the Ami Windows stdio
# so the terminal and graphics models can hook the program's console I/O is
# done when the windowed flavors are wired up (it needs runtime reconciliation
# of the file model), not here. The Ami Windows model implementations
# (amitk/windows/*.c) are cross compiled into the terminal/graphics/sound/
# network archives below regardless.
#

#
# Build psystem for win64, the Pascaline support library in C.
#
$(LIBS)/win64/psystem.a: $(SOURCE)/pgen/psystem.c \
	$(SOURCE)/pgen/amd64/psystem.asm
	@echo
	@echo "Building psystem for win64..."
	@echo
	mkdir -p $(BUILD)/win64
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/win64/psystem.o \
		-c $(SOURCE)/pgen/psystem.c
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -Wa,--defsym,WINDOWS=1 \
		-o $(BUILD)/win64/psystem_asm.o \
		-c -x assembler $(SOURCE)/pgen/amd64/psystem.asm
	rm -f $(LIBS)/win64/psystem.a
	$(WINAR) rc $(LIBS)/win64/psystem.a $(BUILD)/win64/psystem.o \
		$(BUILD)/win64/psystem_asm.o
	mkdir -p $(WINCELL)/lib
	cp $(LIBS)/win64/psystem.a $(WINCELL)/lib

#
# Build main for win64, the program stack startup shim.
#
$(LIBS)/win64/main.o: $(SOURCE)/pgen/amd64/main.asm
	@echo
	@echo "Building main for win64..."
	@echo
	mkdir -p $(BUILD)/win64
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -Wa,--defsym,WINDOWS=1 \
		-o $(BUILD)/win64/main.o \
		-c -x assembler $(SOURCE)/pgen/amd64/main.asm
	cp $(BUILD)/win64/main.o $(LIBS)/win64
	mkdir -p $(WINCELL)/lib
	cp $(BUILD)/win64/main.o $(WINCELL)/lib

#
# Build services for win64. The Ami services implementation for Windows is
# cross compiled with mingw, along with the wrappers and support (the x86
# name-coining wrapper assembly is convention neutral and assembles for COFF
# as is). Built against the C runtime stdio, matching the win64 psystem's
# stdio world.
#
WINSERVCPP=$(CPPFLAGS64LE) -I$(AMIINC) -I$(LIBS)/source
ifneq ($(AMITK),)
$(LIBS)/win64/services.a: $(PASCALP6)/amitk/windows/services.c \
	$(LIBS)/source/services_wrapper.asm \
	$(LIBS)/source/services_wrapper.c \
	$(LIBS)/source/services_support.c \
	$(LIBS)/source/support.c
	@echo
	@echo "Building services for win64..."
	@echo
	mkdir -p $(BUILD)/win64
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(WINSERVCPP) \
		-o $(BUILD)/win64/support.o -c $(LIBS)/source/support.c
	$(WINCC) $(WINCFLAGS) $(WINSERVCPP) \
		-o $(BUILD)/win64/services_support.o -c $(LIBS)/source/services_support.c
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/win64/services_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/services_wrapper.asm
	$(WINCC) $(WINCFLAGS) $(WINSERVCPP) \
		-o $(BUILD)/win64/services_wrapper.o -c $(LIBS)/source/services_wrapper.c
	$(WINCC) $(WINCFLAGS) $(WINSERVCPP) \
		-o $(BUILD)/win64/services.o -c $(PASCALP6)/amitk/windows/services.c
	rm -f $(LIBS)/win64/services.a
	$(WINAR) rc $(LIBS)/win64/services.a $(BUILD)/win64/services_wrapper_asm.o \
		$(BUILD)/win64/services_wrapper.o $(BUILD)/win64/services.o \
		$(BUILD)/win64/services_support.o $(BUILD)/win64/support.o
	mkdir -p $(WINCELL)/lib
	cp $(LIBS)/win64/services.a $(WINCELL)/lib
endif

#
# Build terminal for win64. The Ami terminal model for Windows
# (amitk/windows/terminal.c) drives the Win32 console directly, so unlike the
# linux build it needs no system_event or X11 support object -- the archive is
# the wrappers, the model, its services and the shared support object. Built
# with the stdio bypass so the model hooks the program's console I/O through
# the same Ami stdio psystem uses. Programs that link terminal.a pull in the
# Win32 console/multimedia import libraries (-lgdi32 -lwinmm), added by pc.
#
WINTERMCPP=$(CPPFLAGS64LE) -I$(AMILIBC) -I$(AMIINC) -I$(LIBS)/source
ifneq ($(AMITK),)
$(LIBS)/win64/terminal.a: $(AMIWIN)/terminal.c \
	$(LIBS)/source/terminal_wrapper.asm \
	$(LIBS)/source/terminal_wrapper.c \
	$(LIBS)/source/terminal_support.c \
	$(LIBS)/source/support.c
	@echo
	@echo "Building terminal for win64..."
	@echo
	mkdir -p $(BUILD)/win64
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(WINTERMCPP) \
		-o $(BUILD)/win64/support.o -c $(LIBS)/source/support.c
	$(WINCC) $(WINCFLAGS) $(WINTERMCPP) \
		-o $(BUILD)/win64/terminal_support.o -c $(LIBS)/source/terminal_support.c
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/win64/terminal_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/terminal_wrapper.asm
	$(WINCC) $(WINCFLAGS) $(WINTERMCPP) \
		-o $(BUILD)/win64/terminal_wrapper.o -c $(LIBS)/source/terminal_wrapper.c
	$(WINCC) $(WINCFLAGS) $(WINTERMCPP) \
		-o $(BUILD)/win64/terminal.o -c $(AMIWIN)/terminal.c
	$(WINCC) $(WINCFLAGS) $(WINTERMCPP) \
		-o $(BUILD)/win64/term_services.o -c $(AMIWIN)/services.c
	rm -f $(LIBS)/win64/terminal.a
	$(WINAR) rc $(LIBS)/win64/terminal.a $(BUILD)/win64/terminal_wrapper_asm.o \
		$(BUILD)/win64/terminal_wrapper.o $(BUILD)/win64/terminal_support.o \
		$(BUILD)/win64/terminal.o $(BUILD)/win64/term_services.o \
		$(BUILD)/win64/support.o
	mkdir -p $(WINCELL)/lib
	cp $(LIBS)/win64/terminal.a $(WINCELL)/lib
endif

#
# Build graphics for win64. The Ami graphics model for Windows
# (amitk/windows/graphics.c) renders through GDI and the common controls, so
# it needs none of the linux build's X11/FreeType/FontConfig include paths or
# link libraries; programs that link graphics.a pull in the Win32 GDI, common
# dialog and multimedia import libraries (-lgdi32 -lcomdlg32 -lwinmm), added by
# pc. Built with the stdio bypass, mirroring terminal.
#
WINGRAPHCPP=$(CPPFLAGS64LE) -I$(AMILIBC) -I$(AMIINC) -I$(LIBS)/source
ifneq ($(AMITK),)
$(LIBS)/win64/graphics.a: $(AMIWIN)/graphics.c \
	$(LIBS)/source/graphics_wrapper.asm \
	$(LIBS)/source/graphics_wrapper.c \
	$(LIBS)/source/graphics_support.c \
	$(LIBS)/source/support.c
	@echo
	@echo "Building graphics for win64..."
	@echo
	mkdir -p $(BUILD)/win64
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(WINGRAPHCPP) \
		-o $(BUILD)/win64/support.o -c $(LIBS)/source/support.c
	$(WINCC) $(WINCFLAGS) $(WINGRAPHCPP) \
		-o $(BUILD)/win64/graphics_support.o -c $(LIBS)/source/graphics_support.c
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/win64/graphics_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/graphics_wrapper.asm
	$(WINCC) $(WINCFLAGS) $(WINGRAPHCPP) \
		-o $(BUILD)/win64/graphics_wrapper.o -c $(LIBS)/source/graphics_wrapper.c
	$(WINCC) $(WINCFLAGS) $(WINGRAPHCPP) \
		-o $(BUILD)/win64/graphics.o -c $(AMIWIN)/graphics.c
	$(WINCC) $(WINCFLAGS) $(WINGRAPHCPP) \
		-o $(BUILD)/win64/graph_services.o -c $(AMIWIN)/services.c
	rm -f $(LIBS)/win64/graphics.a
	$(WINAR) rc $(LIBS)/win64/graphics.a $(BUILD)/win64/graphics_wrapper_asm.o \
		$(BUILD)/win64/graphics_wrapper.o $(BUILD)/win64/graphics_support.o \
		$(BUILD)/win64/graphics.o $(BUILD)/win64/graph_services.o \
		$(BUILD)/win64/support.o
	mkdir -p $(WINCELL)/lib
	cp $(LIBS)/win64/graphics.a $(WINCELL)/lib

#
# The "blonde" win64 graphics archive for the graphics-hosted interpreter
# (pintg/pmachg/cmachg): graphics.c compiled with NOSTDWIN so it does not bind
# stdin/stdout to an automatic main window. All other members are shared with
# the standard archive. Placed in source/graph/win64 so a win64 flavor build's
# module path selects it ahead of libs/win64/graphics.a.
#
source/graph/win64/graphics.a: $(LIBS)/win64/graphics.a
	mkdir -p source/graph/win64
	$(WINCC) $(WINCFLAGS) $(WINGRAPHCPP) -DNOSTDWIN \
		-o $(BUILD)/win64/graphics_blonde.o -c $(AMIWIN)/graphics.c
	rm -f source/graph/win64/graphics.a
	$(WINAR) rc source/graph/win64/graphics.a $(BUILD)/win64/graphics_wrapper_asm.o \
		$(BUILD)/win64/graphics_wrapper.o $(BUILD)/win64/graphics_support.o \
		$(BUILD)/win64/graphics_blonde.o $(BUILD)/win64/graph_services.o \
		$(BUILD)/win64/support.o

#
# Gnome widgets for win64, the portable widget set drawn with the graphics API.
# Same role as the native gnome_widgets.o: pc links it as an explicit object in
# windowed programs.
#
$(LIBS)/win64/gnome_widgets.o: $(PASCALP6)/amitk/portable/gnome_widgets.c
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(WINGRAPHCPP) \
		-o $(LIBS)/win64/gnome_widgets.o -c $(PASCALP6)/amitk/portable/gnome_widgets.c
	mkdir -p $(WINCELL)/lib
	cp $(LIBS)/win64/gnome_widgets.o $(WINCELL)/lib
endif

#
# Build sound and network for win64. Like the linux build these live in the
# plain C runtime stdio world (no STDIO_BYPASS): the Windows sound model
# (amitk/windows/sound.c) drives the Win32 multimedia MIDI/wave API directly
# and carries no separate synthesizer plugins, and network's connection files
# are bridged at the descriptor level. Programs link the Win32 multimedia and
# Winsock import libraries (-lwinmm -lwsock32), added by pc.
#
WINSNDNETCPP=$(CPPFLAGS64LE) -I$(AMIINC) -I$(LIBS)/source
ifneq ($(AMITK),)
$(LIBS)/win64/sound.a: $(AMIWIN)/sound.c \
	$(LIBS)/source/sound_wrapper.asm \
	$(LIBS)/source/sound_wrapper.c \
	$(LIBS)/source/support.c
	@echo
	@echo "Building sound for win64..."
	@echo
	mkdir -p $(BUILD)/win64
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(WINSNDNETCPP) \
		-o $(BUILD)/win64/snd_support.o -c $(LIBS)/source/support.c
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/win64/sound_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/sound_wrapper.asm
	$(WINCC) $(WINCFLAGS) $(WINSNDNETCPP) \
		-o $(BUILD)/win64/sound_wrapper.o -c $(LIBS)/source/sound_wrapper.c
	$(WINCC) $(WINCFLAGS) $(WINSNDNETCPP) \
		-o $(BUILD)/win64/sound.o -c $(AMIWIN)/sound.c
	rm -f $(LIBS)/win64/sound.a
	$(WINAR) rc $(LIBS)/win64/sound.a $(BUILD)/win64/sound_wrapper_asm.o \
		$(BUILD)/win64/sound_wrapper.o $(BUILD)/win64/sound.o \
		$(BUILD)/win64/snd_support.o
	mkdir -p $(WINCELL)/lib
	cp $(LIBS)/win64/sound.a $(WINCELL)/lib

$(LIBS)/win64/network.a: $(AMIWIN)/network.c \
	$(LIBS)/source/network_wrapper.asm \
	$(LIBS)/source/network_wrapper.c \
	$(LIBS)/source/network_support.c \
	$(LIBS)/source/support.c
	@echo
	@echo "Building network for win64..."
	@echo
	mkdir -p $(BUILD)/win64
	mkdir -p $(LIBS)/win64
	$(WINCC) $(WINCFLAGS) $(WINSNDNETCPP) \
		-o $(BUILD)/win64/net_support.o -c $(LIBS)/source/support.c
	$(WINCC) $(WINCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/win64/network_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/network_wrapper.asm
	$(WINCC) $(WINCFLAGS) $(WINSNDNETCPP) \
		-o $(BUILD)/win64/network_wrapper.o -c $(LIBS)/source/network_wrapper.c
	$(WINCC) $(WINCFLAGS) $(WINSNDNETCPP) \
		-o $(BUILD)/win64/network_support.o -c $(LIBS)/source/network_support.c
	$(WINCC) $(WINCFLAGS) $(WINSNDNETCPP) \
		-o $(BUILD)/win64/network.o -c $(AMIWIN)/network.c
	rm -f $(LIBS)/win64/network.a
	$(WINAR) rc $(LIBS)/win64/network.a $(BUILD)/win64/network_wrapper_asm.o \
		$(BUILD)/win64/network_wrapper.o $(BUILD)/win64/network_support.o \
		$(BUILD)/win64/network.o $(BUILD)/win64/net_support.o
	mkdir -p $(WINCELL)/lib
	cp $(LIBS)/win64/network.a $(WINCELL)/lib
endif

################################################################################
#
# Build the arm64 (aarch64 linux) runtime components
#
# These are the components required to link programs generated by the arm64
# pgen (plain stack model parameter layout). They are cross compiled with the
# aarch64 toolchain on other hosts, and run on arm64 linux or under
# qemu-aarch64 emulation. The assembly shims are the aarch64 ports in
# source/pgen/arm64.
#

#
# Build psystem for arm64, the Pascaline support library in C.
#
$(LIBS)/arm64/psystem.a: $(SOURCE)/pgen/psystem.c \
	$(SOURCE)/pgen/arm64/psystem.asm
	@echo
	@echo "Building psystem for arm64..."
	@echo
	mkdir -p $(BUILD)/arm64
	mkdir -p $(LIBS)/arm64
	$(ARMCC) $(ARMCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/arm64/psystem.o \
		-c $(SOURCE)/pgen/psystem.c
	$(ARMCC) $(ARMCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/arm64/psystem_asm.o \
		-c -x assembler $(SOURCE)/pgen/arm64/psystem.asm
	$(ARMAR) rc $(LIBS)/arm64/psystem.a $(BUILD)/arm64/psystem.o \
		$(BUILD)/arm64/psystem_asm.o
	mkdir -p $(ARMCELL)/lib
	cp $(LIBS)/arm64/psystem.a $(ARMCELL)/lib

#
# Build main for arm64, the program stack startup shim.
#
$(LIBS)/arm64/main.o: $(SOURCE)/pgen/arm64/main.asm
	@echo
	@echo "Building main for arm64..."
	@echo
	mkdir -p $(BUILD)/arm64
	mkdir -p $(LIBS)/arm64
	$(ARMCC) $(ARMCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/arm64/main.o \
		-c -x assembler $(SOURCE)/pgen/arm64/main.asm
	cp $(BUILD)/arm64/main.o $(LIBS)/arm64
	mkdir -p $(ARMCELL)/lib
	cp $(BUILD)/arm64/main.o $(ARMCELL)/lib

#
# Build services for arm64. The Ami services implementation for linux is
# cross compiled with the aarch64 toolchain, along with the wrappers and
# support; the name-coining wrapper assembly is the aarch64 twin
# (services_wrapper_arm64.asm). Built without the Ami stdio bypass, matching
# the arm64 psystem's stdio world.
#
ARMSERVCPP=$(CPPFLAGS64LE) -I$(AMIINC) -I$(LIBS)/source
ifneq ($(AMITK),)
$(LIBS)/arm64/services.a: $(AMI)/services.c \
	$(LIBS)/source/services_wrapper_arm64.asm \
	$(LIBS)/source/services_wrapper.c \
	$(LIBS)/source/services_support.c \
	$(LIBS)/source/support.c
	@echo
	@echo "Building services for arm64..."
	@echo
	mkdir -p $(BUILD)/arm64
	mkdir -p $(LIBS)/arm64
	$(ARMCC) $(ARMCFLAGS) $(ARMSERVCPP) \
		-o $(BUILD)/arm64/support.o -c $(LIBS)/source/support.c
	$(ARMCC) $(ARMCFLAGS) $(ARMSERVCPP) \
		-o $(BUILD)/arm64/services_support.o -c $(LIBS)/source/services_support.c
	$(ARMCC) $(ARMCFLAGS) $(CPPFLAGS64LE) -o $(BUILD)/arm64/services_wrapper_asm.o \
		-c -x assembler $(LIBS)/source/services_wrapper_arm64.asm
	$(ARMCC) $(ARMCFLAGS) $(ARMSERVCPP) \
		-o $(BUILD)/arm64/services_wrapper.o -c $(LIBS)/source/services_wrapper.c
	$(ARMCC) $(ARMCFLAGS) $(ARMSERVCPP) \
		-o $(BUILD)/arm64/services.o -c $(AMI)/services.c
	rm -f $(LIBS)/arm64/services.a
	$(ARMAR) rc $(LIBS)/arm64/services.a $(BUILD)/arm64/services_wrapper_asm.o \
		$(BUILD)/arm64/services_wrapper.o $(BUILD)/arm64/services.o \
		$(BUILD)/arm64/services_support.o $(BUILD)/arm64/support.o
	mkdir -p $(ARMCELL)/lib
	cp $(LIBS)/arm64/services.a $(ARMCELL)/lib
endif

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
ifneq ($(AMITK),)
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
	mkdir -p $(HOSTCELL)/lib
	cp $(LIBS)/services.a $(HOSTCELL)/lib
endif

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
ifneq ($(AMITK),)
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
	mkdir -p $(HOSTCELL)/lib
	cp $(LIBS)/terminal.a $(HOSTCELL)/lib
endif

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
ifneq ($(AMITK),)
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
	mkdir -p $(HOSTCELL)/lib
	cp $(LIBS)/graphics.a $(HOSTCELL)/lib

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
	mkdir -p $(HOSTCELL)/lib
	cp $(LIBS)/gnome_widgets.o $(HOSTCELL)/lib
endif

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

ifneq ($(AMITK),)
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
	mkdir -p $(HOSTCELL)/lib
	cp $(LIBS)/sound.a $(HOSTCELL)/lib

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
	mkdir -p $(HOSTCELL)/lib
	cp $(LIBS)/network.a $(HOSTCELL)/lib
endif

#
# Without the amitk sources (release/source archives), the archives and
# objects above are used as committed. A missing one means the tree is
# incomplete (a git checkout without the products, or a truncated archive).
#
ifeq ($(AMITK),)
$(LIBS)/psystem.a $(LIBS)/services.a $(LIBS)/terminal.a $(LIBS)/graphics.a \
source/graph/graphics.a $(LIBS)/gnome_widgets.o $(LIBS)/sound.a $(LIBS)/network.a:
	@test -f "$@" || { \
	  echo "*** Error: $@ is missing and the amitk (Petit-Ami) sources are not"; \
	  echo "*** present to build it. Releases ship these prebuilt; a git checkout"; \
	  echo "*** needs the amitk submodule (git submodule update --init)."; \
	  exit 1; }
endif

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
	mkdir -p $(HOSTCELL)/bin
	cp $(BUILD)/cmach64le $(HOSTCELL)/bin/cmach

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
	mkdir -p $(HOSTCELL)/bin
	cp $(BUILD)/cmacht64le $(HOSTCELL)/bin/cmacht

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
	mkdir -p $(HOSTCELL)/bin
	cp $(BUILD)/cmachg64le $(HOSTCELL)/bin/cmachg

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
	mkdir -p $(HOSTCELL)/bin
	cp $(BUILD)/spew $(HOSTCELL)/bin/spew

#
# Snapshot the working binaries and libraries into the running host's
# directory in the hosts tree. The Pascal-built tools (pcom, pgen, pint and
# friends) are built with pc, not this makefile, so this is how they enter
# the hosts tree; the C-built products are copied as well, giving a complete
# restorable set. The configure script performs the reverse restore. Run
# after a clean regression, before committing.
#
HOSTBINS=cmach cmacht cmachg genobj pc pcom pgen pgen_arm64 pint pintt \
	pintg pmach pmacht pmachg spew
HOSTLIBS=main.o parse.o psystem.a services.a strings.o terminal.a graphics.a \
	sound.a network.a gnome_widgets.o

hostinstall:
	mkdir -p $(HOSTCELL)/bin $(HOSTCELL)/lib
	for f in $(HOSTBINS); do cp bin/$$f $(HOSTCELL)/bin; done
	for f in $(HOSTLIBS); do cp libs/$$f $(HOSTCELL)/lib; done

#
# Report the detected host characteristics.
#
whathost:
	@echo "host: $(HOST) arch: $(ARCH) bits: $(BITS) -> $(HOSTCELL)"

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
	@echo "all           Make all native C-built components (default on unix hosts)."
	@echo
	@echo "win64         Make the Windows x64 runtime components (cross compiled"
	@echo "              via mingw-w64 on non-Windows hosts)."
	@echo
	@echo "arm64         Make the arm64 linux runtime components (cross compiled"
	@echo "              via the aarch64 toolchain on other hosts)."
	@echo
	@echo "cmach         Make cmach, the stand-alone interpreter written in C."
	@echo
	@echo "spew          Make spew, a fault generator test program."
	@echo
	@echo "hostinstall   Snapshot the working bin and libs into the hosts tree"
	@echo "              directory for this host (run after a clean regression)."
	@echo
	@echo "whathost      Report the detected host, architecture and bit length."
	@echo
	@echo "clean         Clean intermediate/temp files from tree."
	@echo
