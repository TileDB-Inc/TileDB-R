library(tiledb)
context("tiledb_filter")

test_that("tiledb_filter default constructor", {
  ctx <- tiledb_ctx()
  flt <- tiledb_filter(ctx)  
  expect_is(flt, "tiledb_filter")
})

test_that("tiledb_filter defaults to no filter", {
  ctx <- tiledb_ctx()
  flt <- tiledb_filter(ctx) 
  expect_equal(tiledb_filter_type(flt), "NONE")
})

test_that("tiledb_filter name is correct", {
  ctx <- tiledb_ctx()
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
    flt <- tiledb_filter(ctx, name) 
    expect_equal(tiledb_filter_type(flt), name)
    
  }
  expect_error(tiledb_filter("UNKNOWN"))
})

test_that("tiledb_filter set compression level", {
  ctx <- tiledb_ctx()
  name_list <- c("GZIP",
                 "ZSTD",
                 "LZ4",
                 "RLE",
                 "BZIP2")
  for (name in name_list) {
    flt <- tiledb_filter(ctx, name) 
    expect_equal(tiledb_filter_type(flt), name)
    tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 10)
    expect_equal(tiledb_filter_get_option(flt, "COMPRESSION_LEVEL"), 10)
  }
})

test_that("tiledb_filter set bit width max window", {
  ctx <- tiledb_ctx()
  flt <- tiledb_filter(ctx, "BIT_WIDTH_REDUCTION") 
  tiledb_filter_set_option(flt, "BIT_WIDTH_MAX_WINDOW", 10)
  expect_equal(tiledb_filter_get_option(flt, "BIT_WIDTH_MAX_WINDOW"), 10)
})

test_that("tiledb_filter positive delta max window", {
  ctx <- tiledb_ctx()
  flt <- tiledb_filter(ctx, "POSITIVE_DELTA") 
  tiledb_filter_set_option(flt, "POSITIVE_DELTA_MAX_WINDOW", 10)
  expect_equal(tiledb_filter_get_option(flt, "POSITIVE_DELTA_MAX_WINDOW"), 10)
})