library(tiledb)
context("tiledb_attr")

test_that("tiledb_attr constructor works", {
  ctx <- tiledb_ctx()
  a1 <- tiledb_attr(ctx)
  expect_is(a1, "tiledb_attr")
})

test_that("tiledb_attr constructor defaults are correct", {
  ctx <- tiledb_ctx()
  a1 <- tiledb_attr(ctx)
  expect_equal(tiledb::name(a1), "")
  expect_true(is.anonymous(a1))
  expect_equal(tiledb::datatype(a1), "FLOAT64")
  expect_equal(tiledb::ncells(a1), 1)
})

test_that("tiledb_attr is.anonymous is correct", {
  ctx <- tiledb_ctx()
  a1  <- tiledb_attr(ctx, "")
  expect_true(is.anonymous(a1))
  a2  <- tiledb_attr(ctx, "foo") 
  expect_false(is.anonymous(a2))
}) 

test_that("tiledb_attr with compression", {
  ctx <- tiledb_ctx()
  a1 <- tiledb_attr(ctx, "foo", compressor = tiledb_compressor("GZIP", 10))
  com <- tiledb::compressor(a1)
  expect_is(com, "tiledb_compressor")
  expect_equal(tiledb_compressor_name(com), "GZIP")
  expect_equal(tiledb_compressor_level(com), 10)
  
  expect_error(tiledb_attr(ctx, "foo", compressor = tiledb_compressor("UNKNOWN", -1)))
})

test_that("tiledb_attr throws an error with invalid ncells argument", {
  ctx <- tiledb_ctx() 
  a1 <- tiledb_attr(ctx, "foo", ncells = 1)
  expect_equal(tiledb::ncells(a1), 1) 
  expect_error(tiledb_attr(ctx, "foo", ncells = 0))
})