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
               "POSITIVE_DELTA")
for (name in name_list) {
  flt <- tiledb_filter(name)
  expect_equal(tiledb_filter_type(flt), name)

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
