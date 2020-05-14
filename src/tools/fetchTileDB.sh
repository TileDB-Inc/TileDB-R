#!/bin/sh

repo=https://tiledb-inc.github.io/tiledb-linux-library
tarball=tiledb-ubuntu-16.04-shared.tar.gz

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
    tar xaf ${tarball} -C tiledb
    rm ${tarball}
fi

# ## Adjust DESCRIPTION
# descfile="../DESCRIPTION"
# si=$(grep "^StagedInstall: no" ${descfile})
# echo "Seeing '${si}' in ${descfile}"

# if test x"${si}" = x""; then
#     echo "StagedInstall: no" >> ${descfile}
#     echo "...added"
# else
#     echo "...already present"
# fi
