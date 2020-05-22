#!/bin/sh

if [ $# -lt 1 ]; then
    echo "Usage: fetchTileDB.sh tarfile"
    exit 1
fi

repo=https://tiledb-inc.github.io/tiledb-linux-library
tarball="$1"

#test -f /etc/os-release && echo "** On " && cat /etc/os-release

if [ ! -f NEWS.md ]; then
    echo "This script should run from the inst/ directory."
    echo "No NEWS.md present, so likely not inst/. Exiting."
    exit 1
fi

## Download if need be
if [ ! -f ${tarball} ]; then
    curl -s -k -L -O ${repo}/${tarball}
fi

## Clean-up just in case
if [ -d tiledb ]; then
    rm -rf tiledb
fi

## Expand tarball
if [ ! -d tiledb ]; then
    mkdir tiledb
    tar xzf ${tarball} -C tiledb
    rm ${tarball}
fi

## Copy tiledb/lib/ so that rpath relative path also works from
## source i.e. before the inst/ directory tree is installed
if [ ! -d ../tiledb/lib ]; then
    mkdir -p ../tiledb/lib/
    cp -a tiledb/lib/* ../tiledb/lib/
fi
