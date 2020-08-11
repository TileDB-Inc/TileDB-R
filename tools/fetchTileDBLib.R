#!/usr/bin/Rscript

argv <- commandArgs(trailingOnly=TRUE)
if (length(argv) == 0) {
  message("Requires one argument: macos|linux")
  q()
}

if (!argv[1] %in% c("macos", "linux")) {
  message("Requires one argument: macos|linux")
  q()
}

os <- argv[1]
#base <- "https://dirk.eddelbuettel.com/tldbdl/default/"
base <- "https://eddelbuettel.github.io/tldbdl/default"

url <- file.path(base, os, paste0("tiledb-", os, ".tar.gz"))
download.file(url, "tiledb.tar.gz", quiet=TRUE)
