#!/bin/sh

if [ $# -ne 1 ] || { [ "$1" != "linux" ] && [ "$1" != "macos" ]; }; then
    echo "Usage: fetchTileDB.sh (linux|macos)"
    exit 1
fi
os="${1}"

if [ ! -f NEWS.md ]; then
    echo "This script should run from the inst/ directory."
    echo "No NEWS.md present, so likely not inst/. Exiting."
    exit 1
fi

## source repo: the latest TileDB release
## which redirects to latest tagged release
#repo=https://github.com/TileDB-Inc/TileDB/releases/latest
tarball="tiledb.tar.gz"

## use curl to download the release, follow redirects, and scan
## the sed expression should result in two tarballs such as
##   tiledb-linux-2.0.3-cf03c60.tar.gz
##   tiledb-macos-2.0.3-cf03c60.tar.gz
## where version and sha will vary
#sourcetgz=$(curl -Ls ${repo} | sed -n -e 's/.*<span.*>\(tiledb-.*tar.gz\).*/\1/p' | grep "${os}")

## need to also extract release version ... because github
#ver=$(echo "${sourcetgz}" | cut -d- -f3)

## construct actual url
#downloadurl="https://github.com/TileDB-Inc/TileDB/releases/download/${ver}/${sourcetgz}"

## Download if need be
if [ ! -f "${tarball}" ]; then
    echo "downloading '${tarball}'"
    #curl -s -k -L -o ${tarball} ${downloadurl}
    ${R_HOME}/bin/Rscript ../tools/fetchTileDBLib.R ${os}
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
