# Build against static libraries from rwinlib
VERSION <- commandArgs(TRUE)
if (!file.exists(sprintf("../windows/tiledb-%s/include/tiledb/tiledb.h", VERSION))) {
  if (getRversion() < "4") stop("This package requires Rtools40 or newer")
  op <- options()
  options(timeout=60)
  ## download.file(sprintf("https://github.com/rwinlib/tiledb/archive/v%s.zip", VERSION), "lib.zip", quiet = TRUE)
  download.file(sprintf("https://github.com/TileDB-Inc/rwinlib-tiledb/archive/v%s.zip", VERSION), "lib.zip", quiet = TRUE)
  options(op)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
