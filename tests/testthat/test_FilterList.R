library(tiledb)
context("tiledb_filter_list")

test_that("tiledb_filter_list default constructor", {
  ctx <- tiledb_ctx()
  flt <- tiledb_filter_list(ctx)  
  expect_is(flt, "tiledb_filter_list")
})

test_that("tiledb_filter_list accepts filters in constructor", {
  ctx <- tiledb_ctx()
  flt <- tiledb_filter(ctx)
  filter_list = tiledb_filter_list(ctx, c(flt))
  expect_equal(nfilters(filter_list), 1)
})

test_that("tiledb_filter_list can set and get max_chunk_size", {
  ctx <- tiledb_ctx()
  flt <- tiledb_filter(ctx)
  filter_list = tiledb_filter_list(ctx, c(flt))
  expect_equal(nfilters(filter_list), 1)
  set_max_chunk_size(filter_list, 10)
  expect_equal(max_chunk_size(filter_list), 10)
})

