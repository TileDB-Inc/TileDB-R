# Build against static libraries from rwinlib
VERSION <- commandArgs(TRUE)
if(!file.exists(sprintf("../windows/tiledb-%s/include/tiledb/tiledb.h", VERSION))){
  if(getRversion() < "4") stop("This packge requires R-4.0 or newer")
  download.file(sprintf("https://github.com/rwinlib/tiledb/archive/v%s.zip", VERSION), "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
