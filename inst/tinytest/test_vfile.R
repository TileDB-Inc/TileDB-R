library(tinytest)
if (FALSE) {
tmp <- tempfile()
ref <- as.raw(1:255)
writeBin(ref, tiledb:::vfile(tmp, verbosity = 0))
tst <- readBin(tiledb:::vfile(tmp, verbosity = 0),  raw(), 1000)
expect_identical(tst, ref)

tmp <- tempfile()
ref <- as.character(mtcars)
writeLines(ref, tiledb:::vfile(tmp, verbosity = 0))
tst <- readLines(tiledb:::vfile(tmp, verbosity = 0))
expect_identical(tst, ref)
}
