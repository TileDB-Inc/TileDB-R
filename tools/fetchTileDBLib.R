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

if (requireNamespace("jsonlite", quietly=TRUE) == FALSE) {
  message("Need 'jsonlite' package to download library for ", argv[1])
  q()
}

res <- jsonlite::fromJSON("https://api.github.com/repos/TileDB-Inc/TileDB/releases/latest")
urls <- res$assets$browser_download_url
tgt <- paste0(argv[1], ".*without_tbb")

ind <- grep(tgt, urls)
if (length(ind) == 0) {
  message("No matching file for OS ", argv[1])
  q()
}
if (length(ind) > 1) {
  message("More than one matching file for ", argv[1])
  q()
}

download.file(urls[ind], "tiledb.tar.gz", quiet=TRUE)
