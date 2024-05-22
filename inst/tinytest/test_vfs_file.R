library(tinytest)

tmp <- tempfile()
ref <- as.raw(1:255)
writeBin(ref, tiledb:::vfs_file(tmp))
tst <- readBin(tiledb:::vfs_file(tmp),  raw(), 1000)
expect_identical(tst, ref)

tmp <- tempfile()
ref <- as.character(mtcars)
writeLines(ref, tiledb:::vfs_file(tmp))
tst <- readLines(tiledb:::vfs_file(tmp))
expect_identical(tst, ref)
