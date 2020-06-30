#!/usr/bin/Rscript

if (requireNamespace("jsonlite", quietly=TRUE) == FALSE) {
  message("Need 'jsonlite' package to download sources.")
  q()
}

res <- jsonlite::fromJSON("https://api.github.com/repos/TileDB-Inc/TileDB/releases/latest")
url <- res$tarball_url
download.file(url, "tiledb.tar.gz", quiet=TRUE)
