# Build against static libraries from rwinlib or rwinlib-tiledb

dcffile <- "../tools/tiledbVersion.txt"
if (!file.exists(dcffile)) stop("TileDB Version file not found.")
dcf <- read.dcf(dcffile)
ver <- dcf[[1, "version"]]

# if (!file.exists("../windows/rwinlib-tiledb/include/tiledb/tiledb.h")) {
if (!file.exists("../inst/tiledb/include/tiledb/tiledb.h")) {
    if (getRversion() < "4") stop("This package requires Rtools40 or newer")
    op <- options(timeout = 180) # CRAN request to have patient download settings
    zipfile <- tempfile(tmpdir = tempdir(check = TRUE), fileext = '.zip')
    download.file(
      sprintf("https://github.com/TileDB-Inc/rwinlib-tiledb/archive/v%s.zip", ver),
      destfile = zipfile,
      quiet = TRUE
    )
    options(op)
    # dir.create("../windows", showWarnings = FALSE)
    # unzip("lib.zip", exdir = "../windows")
    unzip(zipfile, exdir = "../inst")
    # file.rename(sprintf("../windows/rwinlib-tiledb-%s", ver), "../windows/rwinlib-tiledb")
    file.rename(sprintf("../inst/rwinlib-tiledb-%s", ver), "../inst/tiledb")
    unlink(zipfile, force = TRUE)
}
