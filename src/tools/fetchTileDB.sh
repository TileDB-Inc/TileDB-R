#!/bin/sh

repo=https://tiledb-inc.github.io/tiledb-linux-library
tarball=tiledb-ubuntu-16.04-shared.tar.gz

#test -f /etc/os-release && echo "** On " && cat /etc/os-release

if [ ! -f ${tarball} ]; then
    curl -s -k -L -O ${repo}/${tarball}
fi

if [ -d tiledb ]; then
    rm -rfv tiledb
fi

if [ ! -d tiledb ]; then
    mkdir tiledb
    tar xaf ${tarball} -C tiledb
    ## making a full copy is inelegant but apparently needed
    cd tiledb/lib && cp -vax libtiledb.so.?.? libtiledb.so && cd -
    #ls -lR tiledb/
fi
