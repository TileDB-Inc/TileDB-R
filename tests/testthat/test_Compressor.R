library(tiledb)
context("tiledb_compressor")

test_that("tiledb_compressor default constructor", {
  com <- tiledb_compressor()  
  expect_is(com, "tiledb_compressor")
})

test_that("tiledb_compressor defaults to no compression", {
  com <- tiledb_compressor() 
  expect_equal(tiledb_compressor_name(com), "NO_COMPRESSION")
  expect_equal(tiledb_compressor_level(com), -1)
})

test_that("tiledb_compressor name is correct", {
  name_list <- c("NO_COMPRESSION",
                 "GZIP",
                 "ZSTD",
                 "LZ4",
                 "RLE",
                 "BZIP2",
                 "DOUBLE_DELTA") 
  for (name in name_list) {
    com <- tiledb_compressor(name, level = 10) 
    expect_equal(tiledb_compressor_name(com), name)
    expect_equal(tiledb_compressor_level(com), 10)
  }
  expect_error(tiledb_compressor("UNKNOWN", 10))
})