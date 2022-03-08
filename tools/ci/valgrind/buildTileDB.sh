#!/bin/bash

set -u

## check for argument
if [ $# -ne 1 ]; then
    echo "Need version argument. Exiting."
    exit 1
fi

## cmdline arg gives us desired release version, most recent could be read off GitHub Releases tag too
version="$1"


echo "::group::Setup sources"
## standard build off source, enabling s3 and serialization
if [ ${version} = "dev" ]; then
    wget https://github.com/TileDB-Inc/TileDB/archive/refs/heads/${version}.zip
    unzip ${version}.zip
    rm ${version}.zip
else
    wget https://github.com/TileDB-Inc/TileDB/archive/${version}.tar.gz
    tar xaf ${version}.tar.gz
    rm ${version}.tar.gz
fi
mv TileDB-${version} tiledb
cd tiledb
echo "::endgroup::"


echo "::group::Build from source"
mkdir build
cd build
export AWSSDK_ROOT_DIR=/usr
../bootstrap --prefix=/usr/local --enable-s3 --enable-serialization
make -j 8
make -C tiledb install
ldconfig
cd ../..
rm -rf tiledb
echo "::endgroup::"
