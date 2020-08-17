#!/usr/bin/Rscript

argv <- commandArgs(trailingOnly=TRUE)
if (length(argv) == 0) {
  message("Requires either one argument (macos|linux) or two (url downloadurl)")
  q()
}

osarg <- argv[1]
if (!osarg %in% c("macos", "linux", "url")) {
  message("Requires first argument: macos|linux|url")
  q()
}
if (osarg == "url" && length(argv) <= 1) {
  message("First argument 'url' requires second argument with actual url")
  q()
}
urlarg <- argv[2]

baseurl <- "https://github.com/TileDB-Inc/TileDB/releases/download"
dlurl <- switch(osarg,
                linux = file.path(baseurl,"2.0.8/tiledb-linux-2.0.8-db41376-without_tbb.tar.gz"),
                macos = file.path(baseurl,"2.0.8/tiledb-macos-2.0.8-db41376-without_tbb.tar.gz"),
                url = urlarg)
cat("downloading", dlurl, "\n")
download.file(dlurl, "tiledb.tar.gz", quiet=TRUE)
