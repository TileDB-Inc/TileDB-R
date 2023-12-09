#!/bin/bash

set -eu

if [ $# -lt 1 ]; then
    echo "Usage: buildTileDB.sh (default|url_to_tarball)"
    echo "where 'default' leads to use of default version, which a given url can override"
    exit 1
fi
url="${1}"

if [ ! -d src ]; then
    echo "No src/ directory. Script should be invoked from top-level."
    exit 1
fi

## CRAN wants us permit different R binaries via different PATHs
: ${R_HOME=`R RHOME`}

cd src

if [ ! -f tiledb.tar.gz ]; then
    echo -n "Downloading ${url}: "
    ${R_HOME}/bin/Rscript ../tools/fetchTileDBSrc.R ${url}
fi

if [ ! -d tiledb-src ]; then
    uname=`uname`
    if test x"${uname}" = x"Darwin" -o x"${uname}" = x"SunOS"; then
        gunzip tiledb.tar.gz
        tar -xf tiledb.tar
        mv Tile* tiledb-src
        rm tiledb.tar
    else
        mkdir tiledb-src
        tar xaf tiledb.tar.gz -C tiledb-src --strip-components 1
        rm tiledb.tar.gz
    fi
fi

## Clean-up just in case
if [ -d build ]; then
    rm -rf build
fi

## Build
mkdir build
cd build
../tiledb-src/bootstrap --force-build-all-deps --enable-s3 --enable-serialization --linkage=shared
make -j 2
make -C tiledb install
cd ..

## Install
mkdir -p ../inst/tiledb
cp -ax build/dist/* ../inst/tiledb/
mkdir -p ../tiledb
cp -ax build/dist/* ../tiledb/

if [ ! -f .keep_build_dirs ]; then
    rm -rf build tiledb-src
fi
