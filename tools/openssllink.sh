#!/bin/bash
#
# Print the linker arguments that link OpenSSL statically on this host.
#
# Programs link OpenSSL statically so that they do not depend on the OpenSSL
# soname of the host they run on (1.1 on Ubuntu 20.04, 3 on later releases,
# #661). The static archives need a dependency set that differs between
# OpenSSL builds (e.g. 20.04's 1.1 needs -ldl -pthread, 26.04's 3.5 adds
# jitterentropy, zlib and zstd), so it comes from pkg-config --static. The
# OpenSSL archives themselves are named exactly (-l:libssl.a), so that the
# linker cannot take the shared objects that sit beside them; the rest of the
# dependency set links however pkg-config gives it.
#
# The result is checked by linking a probe. If the static link cannot be made
# (no static archives, or a missing dependency archive), the shared libraries
# are printed instead, with a warning on stderr, so that builds still link.
#
# Used by configure and bin/build, which write the result to libs/openssl.link
# for pc, and by the Makefile for the cmach links.
#

dynamic="-lssl -lcrypto"

libs=$(pkg-config --static --libs libssl libcrypto 2>/dev/null)
if [ -z "$libs" ]; then

    echo "*** Warning: pkg-config does not know OpenSSL; linking it shared" >&2
    echo "$dynamic"
    exit 0

fi

static=$(echo " $libs " | sed -e 's/ -lssl / -l:libssl.a /' \
                              -e 's/ -lcrypto / -l:libcrypto.a /' \
                              -e 's/^ *//' -e 's/ *$//')

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cat > "$tmp/probe.c" << EOF
int OPENSSL_init_ssl(unsigned long long opts, const void *settings);
int main(void) { return OPENSSL_init_ssl(0, 0); }
EOF
if gcc -o "$tmp/probe" "$tmp/probe.c" $static > "$tmp/probe.log" 2>&1; then

    echo "$static"

else

    echo "*** Warning: OpenSSL cannot be linked statically on this host, so" \
         "programs link it shared and depend on its soname. The static link" \
         "($static) failed with:" >&2
    grep -m 3 -i "cannot find\|undefined" "$tmp/probe.log" >&2
    echo "*** Install the missing static archives (see configure) and rerun" \
         "configure." >&2
    echo "$dynamic"

fi
