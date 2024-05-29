#!/bin/bash

set -u

## check for argument
if [ $# -ne 1 ]; then
    echo "Need version argument. Exiting."
    exit 1
fi

## cmdline arg gives us desired release version, most recent could be read off GitHub Releases tag too
version="$1"

## check if version is of the form release-*; use grep here to not use bash [[ ]] notation
echo "${version}" | grep -q "^release-" -
isrelease=$?

## fetch appropriate sources
echo "::group::Setup sources"
if [ ${isrelease} -eq 0 ]; then
    git clone --single-branch --branch ${version} https://github.com/TileDB-Inc/TileDB.git TileDB-${version}
    git log --graph --pretty=format:'%h - %d %s (%cr) <%an>' --abbrev-commit | head
elif [ ${version} = "dev" ]; then
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


## standard build off source, enabling s3 and serialization
echo "::group::Build from source"
mkdir build
cd build
export AWSSDK_ROOT_DIR=/usr
../bootstrap --prefix=/usr/local --enable-s3 --enable-serialization --linkage=shared
make -j 8 tiledb install-tiledb
ldconfig
cd ../..
rm -rf tiledb
echo "::endgroup::"
