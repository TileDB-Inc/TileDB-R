library(tiledb)
context("tiledb::Compressor")

test_that("tiledb::Compressor default constructor", {
  com <- tiledb::Compressor()  
  expect_is(com, "Compressor")
})

test_that("tiledb::Compressor defaults to no compression", {
  com <- tiledb::Compressor() 
  expect_equal(tiledb::compressor_type(com), "NO_COMPRESSION")
  expect_equal(tiledb::compressor_level(com), -1)
})

test_that("tiledb::Compressor type is correct", {
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
    com <- tiledb::Compressor(type = type, level = 10) 
    expect_equal(tiledb::compressor_type(com), type)
    expect_equal(tiledb::compressor_level(com), 10)
  }
  expect_error(tiledb::Compressor("UNKNOWN", 10))
})