#!/bin/bash
################################################################################
#
# Build and install the static external libraries for the Ami bindings.
#
# Pascaline links fully static. The sound binding needs ALSA and fluidsynth,
# whose static libraries the distribution does not carry (the network
# binding's OpenSSL static libraries ship with libssl-dev and need no work).
# This script builds both from source and installs the archives to
# /usr/local/lib, where pc's link lines find them:
#
#   libasound.a     ALSA, version-matched to the installed runtime, with the
#                   static patch (see below)
#   libfluidsynth.a fluidsynth, version-matched, minimal feature set
#
# fluidsynth is built without libsndfile (-Denable-libsndfile=off): the Ami
# fluidsynth plugin only loads SoundFonts (.sf2), which fluidsynth reads
# itself; libsndfile would only add support for other sample-file formats and
# brings a large static closure of its own. The remaining fluidsynth closure
# is glib and pcre, whose static libraries the distribution carries.
#
# The ALSA static patch (alsa-static.patch) makes two changes a static link
# requires:
#
# 1. The static-build dlsym registry is populated by constructors; they get
#    priority 102 so they run before the Ami sound initializer at priority
#    103 (prioritized constructors run before all default-priority ones, so
#    without this the registry is empty when Ami enumerates devices).
#
# 2. A config hook whose module cannot be loaded is tolerated when the hook
#    sets 'errors false' (snd_config_hooks_call). The distribution's
#    99-pulse.conf hook loads a shared module, which a static binary cannot;
#    without this the failure poisons the whole ALSA configuration and no
#    device opens.
#
# Run with sudo available for the installs. Rerunnable; builds in /tmp.
#
################################################################################

set -e

HERE=$(cd "$(dirname "$0")" && pwd)
WORK=/tmp/staticdeps-build
mkdir -p "$WORK"

#
# ALSA: version-match the installed runtime so the static library agrees with
# the system's configuration files in /usr/share/alsa.
#
ALSAVER=$(dpkg -l libasound2 2>/dev/null | tail -1 | awk '{print $3}' | cut -d- -f1)
ALSAVER=${ALSAVER:-1.2.2}
echo "Building static ALSA $ALSAVER..."
cd "$WORK"
if [ ! -f "alsa-lib-$ALSAVER.tar.bz2" ]; then
    wget -q "https://www.alsa-project.org/files/pub/lib/alsa-lib-$ALSAVER.tar.bz2"
fi
rm -rf "alsa-lib-$ALSAVER"
tar xjf "alsa-lib-$ALSAVER.tar.bz2"
cd "alsa-lib-$ALSAVER"
patch -p1 < "$HERE/alsa-static.patch"
./configure --enable-static --disable-shared --without-versioned --quiet
make -j"$(nproc)" > /dev/null
sudo cp src/.libs/libasound.a /usr/local/lib/
echo "installed /usr/local/lib/libasound.a"

#
# fluidsynth: version-match the installed runtime. Minimal features; the
# audio path is ALSA (found via the system alsa.pc; the static link uses our
# libasound.a above).
#
FLUIDVER=$(pkg-config --modversion fluidsynth 2>/dev/null || echo 2.1.1)
echo "Building static fluidsynth $FLUIDVER..."
cd "$WORK"
if [ ! -f "v$FLUIDVER.tar.gz" ]; then
    wget -q "https://github.com/FluidSynth/fluidsynth/archive/refs/tags/v$FLUIDVER.tar.gz"
fi
rm -rf "fluidsynth-$FLUIDVER"
tar xzf "v$FLUIDVER.tar.gz"
cd "fluidsynth-$FLUIDVER"
mkdir -p build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=off \
    -Denable-libsndfile=off -Denable-jack=off -Denable-pulseaudio=off \
    -Denable-dbus=off -Denable-ladspa=off -Denable-readline=off \
    -Denable-network=off -Denable-sdl2=off -Denable-oss=off \
    -Denable-aufile=off -Denable-ipv6=off > /dev/null
make -j"$(nproc)" libfluidsynth > /dev/null
sudo cp src/libfluidsynth.a /usr/local/lib/
echo "installed /usr/local/lib/libfluidsynth.a"

echo
echo "Static dependency closure for pc link lines:"
echo "  sound:   -L/usr/local/lib -lfluidsynth -lglib-2.0 -lpcre -lasound -lm -lpthread -ldl"
echo "  network: -lssl -lcrypto -lpthread -ldl"
