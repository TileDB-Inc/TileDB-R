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

dcffile <- "../tools/tiledbVersion.txt"
if (!file.exists(dcffile)) {
    message("TileDB Version file not found.")
    q()
}
dcf <- read.dcf(dcffile)
ver <- dcf[[1, "version"]]
sha <- dcf[[1, "sha"]]

baseurl <- "https://github.com/TileDB-Inc/TileDB/releases/download"
dlurl <- switch(osarg,
                linux = file.path(baseurl,sprintf("%s/tiledb-linux-%s-%s-full.tar.gz", ver, ver, sha)),
                macos = file.path(baseurl,sprintf("%s/tiledb-macos-%s-%s-full.tar.gz", ver, ver, sha)),
                url = urlarg)
cat("downloading", dlurl, "\n")
op <- options()
options(timeout=60)
download.file(dlurl, "tiledb.tar.gz", quiet=TRUE)
options(op)
