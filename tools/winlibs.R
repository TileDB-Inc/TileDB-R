# Build against static libraries from rwinlib or rwinlib-tiledb

dcffile <- "../tools/tiledbVersion.txt"
if (!file.exists(dcffile)) stop("TileDB Version file not found.")
dcf <- read.dcf(dcffile)
ver <- dcf[[1, "version"]]

ver <- "test-2.30.0mavx2"
if (!file.exists("../inst/tiledb/include/tiledb/tiledb.h") || !dir.exists("../inst/tiledb/lib/")) {
  if (getRversion() < "4") stop("This package requires Rtools40 or newer")
  if (dir.exists("../inst/tiledb")) unlink("../inst/tiledb", recursive = TRUE, force = TRUE)
  op <- options(timeout = 180) # CRAN request to have patient download settings
  zipfile <- tempfile(tmpdir = tempdir(check = TRUE), fileext = '.zip')
  download.file(
    sprintf("https://github.com/TileDB-Inc/rwinlib-tiledb/archive/v%s.zip", ver),
    destfile = zipfile,
    quiet = !isTRUE(getOption('verbose', default = FALSE))
  )
  options(op)
  unzip(zipfile, exdir = "../inst")
  file.rename(sprintf("../inst/rwinlib-tiledb-%s", ver), "../inst/tiledb")
  unlink(zipfile, force = TRUE)
}
