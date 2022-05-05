library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_filter default constructor", {
flt <- tiledb_filter()
expect_true(is(flt, "tiledb_filter"))
#})

#test_that("tiledb_filter defaults to no filter", {
flt <- tiledb_filter()
expect_equal(tiledb_filter_type(flt), "NONE")
#})

#test_that("tiledb_filter name is correct", {
name_list <- c("NONE",
               "GZIP",
               "ZSTD",
               "LZ4",
               "RLE",
               "BZIP2",
               "DOUBLE_DELTA",
               "BIT_WIDTH_REDUCTION",
               "BITSHUFFLE",
               "BYTESHUFFLE",
               "POSITIVE_DELTA",
               "CHECKSUM_MD5",
               "CHECKSUM_SHA256")
for (name in name_list) {
  flt <- tiledb_filter(name)
  expect_equal(tiledb_filter_type(flt), name)

}
name_list <- c("DICTIONARY_ENCODING")
for (name in name_list) {
    if (tiledb_version(TRUE) >= "2.9.0") {
        flt <- tiledb_filter(name)
        expect_equal(tiledb_filter_type(flt), name)
    } else {
        expect_error(tiledb_filter(name))
    }
}

expect_error(tiledb_filter("UNKNOWN"))
#})

#test_that("tiledb_filter set compression level", {
name_list <- c("GZIP",
               "ZSTD",
               "LZ4",
               "RLE",
               "BZIP2")
for (name in name_list) {
  flt <- tiledb_filter(name)
  expect_equal(tiledb_filter_type(flt), name)
  tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 10)
  expect_equal(tiledb_filter_get_option(flt, "COMPRESSION_LEVEL"), 10)
}
#})

#test_that("tiledb_filter set bit width max window", {
flt <- tiledb_filter("BIT_WIDTH_REDUCTION")
tiledb_filter_set_option(flt, "BIT_WIDTH_MAX_WINDOW", 10)
expect_equal(tiledb_filter_get_option(flt, "BIT_WIDTH_MAX_WINDOW"), 10)
#})

#test_that("tiledb_filter positive delta max window", {
flt <- tiledb_filter("POSITIVE_DELTA")
tiledb_filter_set_option(flt, "POSITIVE_DELTA_MAX_WINDOW", 10)
expect_equal(tiledb_filter_get_option(flt, "POSITIVE_DELTA_MAX_WINDOW"), 10)
#})

## add some bulk checking for filters
name_list <- c("NONE",
               "GZIP",
               "ZSTD",
               "LZ4",
               "RLE",
               "BZIP2",
               "DOUBLE_DELTA",
               "BIT_WIDTH_REDUCTION",
               "BITSHUFFLE",
               "BYTESHUFFLE",
               "CHECKSUM_MD5",
               "CHECKSUM_SHA256",
               "DICTIONARY_ENCODING")

dat <- readRDS(system.file("sampledata", "bankSample.rds", package="tiledb"))

for (name in name_list) {
    dat2 <- dat
    if (name == "DICTIONARY_ENCODING") {
        if (tiledb_version(TRUE) < "2.9.0") next             # skip if not 2.9.0 or later
        dat2 <- dat2[, sapply(dat2, class) == "character"]
    }

    uri <- file.path(tempdir(), name)
    fromDataFrame(dat2, uri, filter=name)

    chk <- tiledb_array(uri, return_as="data.frame")[]
    expect_equal(dat2, chk[, -1])

    if (is.na(match(name, c("NONE", "BITSHUFFLE", "BYTESHUFFLE", "CHECKSUM_MD5", "CHECKSUM_SHA256")))) {
        size_none <- tiledb_vfs_dir_size(file.path(tempdir(), "NONE"))
        size_curr <- tiledb_vfs_dir_size(uri)
        expect_true(size_curr < size_none)
    }
}
