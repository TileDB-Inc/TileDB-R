#!/usr/bin/Rscript

## by default we download the source from a given release
url <- "https://github.com/TileDB-Inc/TileDB/archive/2.2.1.tar.gz"

cat("Downloading ", url, "\n")
op <- options()
options(timeout=60)
download.file(url, "tiledb.tar.gz", quiet=TRUE)
options(op)
