#!/usr/bin/Rscript

os <- "src"
#base <- "https://dirk.eddelbuettel.com/tldbdl/default/"
base <- "https://eddelbuettel.github.io/tldbdl/default"

url <- file.path(base, os, paste0("tiledb-", os, ".tar.gz"))
download.file(url, "tiledb.tar.gz", quiet=TRUE)
