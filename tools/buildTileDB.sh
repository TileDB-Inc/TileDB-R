#!/bin/bash

if [ ! -d src ]; then
    echo "No src/ directory. Script should be invoked from top-level."
    exit 1
fi

cd src

if [ ! -f tiledb.tar.gz ]; then
    #url=$(curl -Ls https://github.com/TileDB-Inc/TileDB/releases/latest | sed -n -e 's/.*href=\"\(.*tar.gz\)".*/\1/p' | grep archive)
    echo "Downloading ${url} as tiledb.tar.gz ..."
    #curl -s -k -L -o tiledb.tar.gz "https://github.com/${url}"
    ${R_HOME}/bin/Rscript ../tools/fetchTileDBSrc.R
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
../tiledb-src/bootstrap --force-build-all-deps --enable-serialization
## NB: temporarily disabling and s3
#../tiledb-src/bootstrap --force-build-all-deps
make -j 2
make -C tiledb install
cd ..

## Install
mkdir -p ../inst/tiledb
cp -ax tiledb-src/dist/* ../inst/tiledb/
mkdir -p ../tiledb
cp -ax tiledb-src/dist/* ../tiledb/

if [ ! -f .keep_build_dirs ]; then
    rm -rf build tiledb-src
fi
