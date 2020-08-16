#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: testBuildFromSource.sh tgt"
    exit 1
fi
tgt="${1}"
R CMD INSTALL --configure-args="--enable-building" ${tgt}
