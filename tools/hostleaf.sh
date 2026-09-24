#!/bin/bash
################################################################################
#
# Hosts tree leaf selection for glibc hosts.
#
# On Linux the hosts tree cell (hosts/linux/<arch>/bit<bits>) forks into one
# leaf per minimum glibc requirement, named glibc<major>.<minor>, each with
# its own bin and libs. glibc is backward compatible only: a product built
# against an older glibc runs on every newer one, never the reverse. So a
# leaf serves every host whose glibc is at or above the leaf's version, and
# a host takes the newest leaf that is not above its own glibc. Keying on
# the glibc version rather than the distribution release is what lets one
# leaf serve Ubuntu, Debian, Fedora and the rest alike.
#
# Two uses:
#
#   hostleaf.sh <product>...
#
#     Print the leaf name for a set of built products: the newest glibc
#     symbol version any of them references (their real minimum
#     requirement, read from the dynamic symbol table). A set with no such
#     reference (fully static products) is labelled with the building host's
#     glibc, the only version it is known to run on. On a non-Linux host
#     nothing is printed, and the cell is used flat.
#
#   hostleaf.sh --select <cell> [<glibc version>]
#
#     Print the directory to restore from: the newest glibc leaf of <cell>
#     not above the given (default: running) glibc version, or <cell> itself
#     when it has no glibc leaves (non-glibc hosts, or the flat layout).
#     Exits 1 when leaves exist but none fits, naming what is there.
#
################################################################################

hostglibc() {

    local v
    v=$(getconf GNU_LIBC_VERSION 2>/dev/null | awk '{print $2}')
    [ -z "$v" ] && v=$(ldd --version 2>/dev/null | head -1 | grep -o '[0-9][0-9.]*$')
    echo "$v"

}

# true when version $1 <= version $2
verle() {

    [ "$(printf '%s\n%s\n' "$1" "$2" | sort -V | head -1)" = "$1" ]

}

if [ "$1" = "--select" ] ; then

    cell="$2"
    want="$3"
    leaves=$(ls -d "$cell"/glibc* 2>/dev/null | sort -V)
    if [ -z "$leaves" ] ; then

        echo "$cell"
        exit 0

    fi
    [ -z "$want" ] && want=$(hostglibc)
    if [ -z "$want" ] ; then

        echo "*** Cannot determine the host glibc version" >&2
        exit 1

    fi
    pick=
    for d in $leaves ; do

        v=${d##*/glibc}
        if verle "$v" "$want" ; then pick="$d" ; fi

    done
    if [ -z "$pick" ] ; then

        echo "*** No hosts tree leaf for glibc $want in $cell; have:" \
             $(for d in $leaves ; do basename $d ; done) >&2
        exit 1

    fi
    echo "$pick"
    exit 0

fi

[ "$(uname)" = "Linux" ] || exit 0

max=
for f in "$@" ; do

    [ -f "$f" ] || continue
    for s in $(objdump -T "$f" 2>/dev/null | grep -o 'GLIBC_[0-9][0-9.]*' | sort -u) ; do

        v=${s#GLIBC_}
        if [ -z "$max" ] || verle "$max" "$v" ; then max="$v" ; fi

    done

done
[ -z "$max" ] && max=$(hostglibc)
[ -n "$max" ] && echo "glibc$max"
