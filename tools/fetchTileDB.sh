#!/bin/sh

set -eu

if [ $# -lt 1 ] || { [ "$1" != "linux" ] && [ "$1" != "macos" ] && [ "$1" != "url" ]; }; then
    echo "Usage: fetchTileDB.sh (linux|macos|url) (givenurl|machine)"
    echo "where 'givenurl' and 'machine' are optional"
    exit 1
fi
os="${1}"

if [ $# -ge 2 ]; then
    url="${2}"
    ## url="" automatic in case of 'else'
fi

if [ ! -d tinytest ]; then
    echo "This script should run from the inst/ directory."
    echo "No tinytest/ directory present, so likely not inst/. Exiting."
    exit 1
fi

## CRAN wants us permit different R binaries via different PATHs
: ${R_HOME=`R RHOME`}

tarball="tiledb.tar.gz"

## Download if need be
if [ ! -f "${tarball}" ]; then
    ##echo "downloading '${tarball}'"
    ${R_HOME}/bin/Rscript ../tools/fetchTileDBLib.R ${os} ${url}
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
    test -d tiledb/lib64 && mv tiledb/lib64 tiledb/lib
fi

## Copy tiledb/lib/ so that rpath relative path also works from
## source i.e. before the inst/ directory tree is installed
if [ ! -d ../tiledb/lib ]; then
    mkdir -p ../tiledb/lib/
    cp -a tiledb/lib/* ../tiledb/lib/
fi
