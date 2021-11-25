#!/usr/bin/Rscript

argv <- commandArgs(trailingOnly=TRUE)
if (length(argv) == 0) {
  message("Requires either argument pairing [(macos|linux) machine] or [url downloadurl]")
  q()
}

osarg <- argv[1]
arch <- urlarg <- ""                    # placeholder, used for macos as arm64 arch, or for url
if (!osarg %in% c("macos", "linux", "url")) {
  message("Requires first argument: macos|linux|url")
  q()
}
if (osarg == "url" && length(argv) <= 1) {
  message("First argument 'url' requires second argument with actual url")
  q()
}
if (length(argv) >= 2) {                # if download url, or for macOS arm64, given
  urlarg <- argv[2]                     # used if 'osarg' argument value was 'url'
  arch <- argv[2]                       # otherwise can also be 'arm64' in case of 'macos'
}

dcffile <- "../tools/tiledbVersion.txt"
if (!file.exists(dcffile)) {
    message("TileDB Version file not found.")
    q()
}
dcf <- read.dcf(dcffile)
ver <- dcf[[1, "version"]]
sha <- dcf[[1, "sha"]]

## on linux, we need to consider AVX2 vs non-AVX2 capabilities on the build machine
avx2 <- if (arch == "linux" && any(grepl("avx2", readLines("/proc/cpuinfo")))) "" else "-noavx2"

## downloads are from GitHub releases
baseurl <- "https://github.com/TileDB-Inc/TileDB/releases/download"
## now switch based on macOS or Linux, using version, architecture, avx2 if needed, version and sha
dlurl <- switch(osarg,
                linux = file.path(baseurl,sprintf("%s/tiledb-linux-%s%s-%s-%s.tar.gz", ver, arch, avx2, ver, sha)),
                macos = file.path(baseurl,sprintf("%s/tiledb-macos-%s-%s-%s.tar.gz", ver, arch, ver, sha)),
                url = urlarg)
cat("downloading", dlurl, "\n")
op <- options()
options(timeout=60)
download.file(dlurl, "tiledb.tar.gz", quiet=TRUE)
options(op)
