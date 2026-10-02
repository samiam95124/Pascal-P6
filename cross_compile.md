# Reorganization for cross compile

## Hosts directory

The hosts directory changes to the form:

hosts
    linux
    bsd
    windows
    mac

Each host subdirectory has the form:

x86
arm
riscv

And each of those directories has the form:

bit32
    bin
    libs
bit64
    bin
    libs

This new structure replaces the old structure.

## Make file

There is only one make file. It will determine:

1. What host is running.
2. What bit length is that host.

The new make will have all products possible in it, ie, linux, bsd, windows, 
mac. For each product and each bit length, the resuling objects, executables
or other files are placed in their appropriate directory in the hosts tree.
If that host happens to be the active host, the files are also copied to the
bin and lib directories (in the top of tree).

## pc

pc has flags, which are passed down to pcom, as to which calling convention/host
is in use:

calling conventions:

amd64_sysv
win64
arm64_sysv

hosts:

linux
bsd
windows
mac

pc passes the calling convention flags to pcom. It uses the host and calling
convention flags as follows.

The .ins file can now have a construct:

begin <tag>

end

The statements between begin and end apply only if the tag given is set
(begin is the command, the tag its argument). The tags are:

amd64_sysv
win64
arm64_sysv
linux
bsd
windows
mac

Ie, the same as the calling convention and hosts option flags. pc will gain the
ability to determine:

1. The host it is running on.
2. The calling convention in use.
3. The bit length in use.

This will be done in psystem.c by routine described below. pc will use this
information to determine a default set of host and calling convention options.

Besides that, each of the .ins files for toolset and library products will
contain special instructions for calling conventions and hosts. Usually this
will consist of copying the binary or other products into the hosts tree.

## psystem host and calling convention query routines

psystem will get new routines that give the host and calling conventions. These
are typicalling determined by defines or other means.

psystem_host()

Gives the number corresponding to the current host:

0 - Unknown
1 - Linux
2 - BSD
3 - Windows
4 - Mac

psystem_callcon()

Gives the number corresponding to the current calling convention:

0 - Unknown
1 - i8386 (not used at present)
2 - AMD64 SYSV
3 - Win32
4 - Win64
5 - ARM32
6 - ARM64
7 - RISCV32
8 - RISCV64