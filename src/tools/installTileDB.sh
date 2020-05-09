#!/bin/sh

if [ ! -f tiledb.tar.gz ]; then
    curl -s -k -L -o tiledb.tar.gz https://github.com/TileDB-Inc/TileDB/archive/2.0.0.tar.gz
fi

if [ ! -d tiledb-src ]; then
    mkdir tiledb-src
    tar xaf tiledb.tar.gz -C tiledb-src --strip-components 1
fi

if [ ! -d tiledb-inst ]; then
    mkdir tiledb-inst
fi

mkdir build
cd build
../tiledb-src/bootstrap --prefix=../tiledb-inst --enable-static-tiledb
make -j 8
make -C tiledb install
