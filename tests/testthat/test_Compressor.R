library(tiledb)
context("tiledb_compressor")

test_that("tiledb_compressor default constructor", {
  com <- tiledb_compressor()  
  expect_is(com, "tiledb_compressor")
})

test_that("tiledb_compressor defaults to no compression", {
  com <- tiledb_compressor() 
  expect_equal(tiledb::compressor_type(com), "NO_COMPRESSION")
  expect_equal(tiledb::compressor_level(com), -1)
})

test_that("tiledb_compressor type is correct", {
  type_list <- c("NO_COMPRESSION",
                 "GZIP",
                 "ZSTD",
                 "LZ4",
                 "BLOSC_LZ",
                 "BLOSC_LZ4",
                 "BLOSC_LZ4HC",
                 "BLOSC_SNAPPY",
                 "BLOSC_ZLIB",
                 "BLOSC_ZSTD",
                 "RLE",
                 "BZIP2",
                 "DOUBLE_DELTA") 
  for (type in type_list) {
    com <- tiledb_compressor(type = type, level = 10) 
    expect_equal(tiledb::compressor_type(com), type)
    expect_equal(tiledb::compressor_level(com), 10)
  }
  expect_error(tiledb_compressor("UNKNOWN", 10))
})