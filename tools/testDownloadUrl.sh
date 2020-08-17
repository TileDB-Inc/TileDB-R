#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: testDownloadUrl.sh tgt"
    exit 1
fi
tgt="${1}"
url="https://github.com/TileDB-Inc/TileDB/releases/download/2.0.7/tiledb-linux-2.0.7-2058d3d.tar.gz"
R CMD INSTALL --configure-args="--with-download=${url}" ${tgt}
