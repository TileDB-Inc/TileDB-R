#!/bin/sh

tarball=tiledb-ubuntu-16.04-shared.tar.gz

test -f /etc/os-release && echo "** On " && cat /etc/os-release

if [ ! -f tiledb_build.tar.gz ]; then
    curl -s -k -L -O http://dirk.eddelbuettel.com/tmp/${tarball}
fi

if [ ! -f tiledb ]; then
    mkdir tiledb
    tar xaf ${tarball} -C tiledb
    cd tiledb/lib && mv -v libtiledb.so.?.? libtiledb.so
    ls -lR tiledb/
fi
