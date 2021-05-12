#!/bin/sh

if [ $# -lt 1 ] || { [ "$1" != "linux" ] && [ "$1" != "macos" ] && [ "$1" != "url" ]; }; then
    echo "Usage: fetchTileDB.sh (linux|macos|url)"
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

tarball="tiledb.tar.gz"

## Download if need be
if [ ! -f "${tarball}" ]; then
    ##echo "downloading '${tarball}'"
    ## CRAN wants us permit different R binaries via different PATHs
    if [ x"${R_HOME}" = x ]; then
        R_HOME=`R RHOME`
    fi
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
fi

## Copy tiledb/lib/ so that rpath relative path also works from
## source i.e. before the inst/ directory tree is installed
if [ ! -d ../tiledb/lib ]; then
    mkdir -p ../tiledb/lib/
    cp -a tiledb/lib/* ../tiledb/lib/
fi
