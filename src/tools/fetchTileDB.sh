#!/bin/sh

repo=https://tiledb-inc.github.io/tiledb-linux-library
tarball=tiledb-ubuntu-16.04-shared.tar.gz

#test -f /etc/os-release && echo "** On " && cat /etc/os-release

if [ ! -f ${tarball} ]; then
    curl -s -k -L -O ${repo}/${tarball}
fi

if [ -d tiledb ]; then
    rm -rf tiledb
fi

if [ ! -d tiledb ]; then
    mkdir tiledb
    tar xaf ${tarball} -C tiledb
    rm ${tarball}
fi
