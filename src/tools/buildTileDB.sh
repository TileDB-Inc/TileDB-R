#!/bin/sh

if [ ! -d src ]; then
    echo "No src/ directory. Script should be invoked from top-level."
    exit 1
fi

cd src

if [ ! -f tiledb.tar.gz ]; then
    echo "Downloading...."
    curl -s -k -L -o tiledb.tar.gz https://github.com/TileDB-Inc/TileDB/archive/2.0.1.tar.gz
fi

if [ ! -d tiledb-src ]; then
    #if test $(uname) == "Darwin"; then
    #    tar -xf tiledb.tar.gz
    #    mv Tile* tiledb-src
    #else
    mkdir tiledb-src
    tar xaf tiledb.tar.gz -C tiledb-src --strip-components 1
    #fi
fi

## Clean-up just in case
if [ -d build ]; then
    rm -rf build
fi

## Build
mkdir build
cd build
#../tiledb-src/bootstrap --force-build-all-deps --enable-s3 --enable-serialization
## NB: temporarily disabling serialization and s3
../tiledb-src/bootstrap --force-build-all-deps 
make -j 4
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

