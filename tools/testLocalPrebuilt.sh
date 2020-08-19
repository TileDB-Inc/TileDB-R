#!/bin/bash

ver="2.0.8"

if [ $# -lt 2 ]; then
    echo "Usage: testDownloadPrebuilt.sh loc tgt"
    echo ""
    echo "where "
    echo "   'loc' is the location of the TileDB Embedded build"
    echo "   'tgt' is a TileDB R tar.gz"
    exit 1
fi

loc="${1}"
tgt="${2}"

if [ ! -d ${loc} ]; then
    echo "Error: No directory '${loc}'."
    exit 1
fi

if [ ! -f ${tgt} ]; then
    echo "Error: No source release tar.gz '${tgt}'."
    exit 1
fi

## fetch and expand
curl -qsL https://github.com/TileDB-Inc/TileDB/archive/${ver}.tar.gz tiledb.tar.gz
tar xaf tiledb.tar.gz

## create target directory
mkdir tiledb

## build and install
cd TileDB-${ver}
mkdir build
cd build
../bootstrap --prefix=../../tiledb --disable-tbb
make -j4
make -C tiledb install

## cleanup
cd ../..
rm -rf tiledb.tar.gz TileDB-${ver}

## put pieces together
R CMD INSTALL --configure-args="--with-tiledb=$(pwd)/${loc}" ${tgt}
