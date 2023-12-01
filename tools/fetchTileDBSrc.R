#!/usr/bin/Rscript

## Require one command arg
args <- commandArgs(TRUE)
if (length(args) != 1) {
    message("Usage: fetchTileDBSrc.R (default|url)")
    message("where 'default' select the given default URL")
    message("      'url' provides an alternative URL to download from")
    q()
}
arg <- args[1]

if (arg == "default") {
    ## Determin the 'default' download from the pin

    ## This is an R script to make it easy to read the dcf format file
    dcffile <- "../tools/tiledbVersion.txt"
    if (!file.exists(dcffile)) {
        message("TileDB Version file not found.")
        q()
    }
    dcf <- read.dcf(dcffile)
    ver <- dcf[[1, "version"]]

    ## by default we download the source from the given release
    url <- paste0("https://github.com/TileDB-Inc/TileDB/archive/", ver, ".tar.gz")

} else {
    ## use the given url
    url <- arg
}

if (!file.exists("tiledb.tar.gz")) {
    op <- options()
    options(timeout=180)

    cat(url, " ...\n")
    download.file(url, "tiledb.tar.gz", quiet=TRUE)

    ## special case of nightly download from GitHub which comes only as a zipfile
    ## so we first unzip the file now downloaded to tiledb.tar.gz, and then create the tar
    if (grepl("zip$", url)) {
        cat("Converting zip to tar.gz ...\n")
        unzip("tiledb.tar.gz")
        unlink("tiledb.tar.gz")
        Sys.chmod("TileDB-dev/bootstrap", mode="0755")
        options(warn=-1)
        tar("tiledb.tar.gz", "TileDB-dev", compression="gzip")
    }

    options(op)
}
q()
