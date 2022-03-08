#!/bin/bash

echo "::group::Build TileDB R package"
## build package
R CMD build --no-build-vignettes --no-manual .
echo "::endgroup::"


echo "::group::Check TileDB R package"
## check package
R CMD check --use-valgrind --as-cran --no-manual --ignore-vignettes tiledb_*.tar.gz
echo "::endgroup::"
