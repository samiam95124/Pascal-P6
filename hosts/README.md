# Hosts tree

Build products for each host, architecture and bit length, in the form:

    hosts/<host>/<arch>/<bits>/bin      executables
    hosts/<host>/<arch>/<bits>/lib      libraries and link objects

Where `<host>` is one of:

    linux
    bsd
    windows
    mac

`<arch>` is one of:

    x86
    arm
    riscv

And `<bits>` is one of:

    bit32
    bit64

The makefile determines the running host, architecture and bit length, places
each product it builds into the matching directory here, and, when the product
is for the running host, also copies it to the top of tree bin and lib
directories. Cross compiled products (for example the win64 and arm64 runtime
libraries built on a linux/x86 host) are placed in the directory of the host
they run on.

The `hostinstall` make target snapshots the current working binaries and
libraries (including the Pascal-built tools, which the makefile does not build
itself) into the running host's directory. The `configure` script performs the
reverse: it restores the working bin and libs directories from the running
host's directory here.
