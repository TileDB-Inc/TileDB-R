library(tiledb)
context("tiledb::Attr")

test_that("tiledb::Attr constructor works", {
  ctx <- tiledb::Ctx()
  a1 <- tiledb::Attr(ctx)
  expect_is(a1, "Attr")
})

test_that("tiledb::Attr constructor defaults are correct", {
  ctx <- tiledb::Ctx()
  a1 <- tiledb::Attr(ctx)
  expect_equal(tiledb::name(a1), "")
  expect_true(is.anonymous(a1))
  expect_equal(tiledb::datatype(a1), "FLOAT64")
  expect_equal(tiledb::ncells(a1), 1)
})

test_that("tiledb::Attr is.anonymous is correct", {
  ctx <- tiledb::Ctx()
  a1  <- tiledb::Attr(ctx, "")
  expect_true(is.anonymous(a1))
  a2  <- tiledb::Attr(ctx, "foo") 
  expect_false(is.anonymous(a2))
}) 

test_that("tiledb::Attr with compression", {
  ctx <- tiledb::Ctx()
  a1 <- tiledb::Attr(ctx, "foo", compressor = tiledb::Compressor("GZIP", 10))
  com <- tiledb::compressor(a1)
  expect_is(com, "Compressor")
  expect_equal(tiledb::compressor_type(com), "GZIP")
  expect_equal(tiledb::compressor_level(com), 10)
  
  expect_error(tiledb::Attr(ctx, "foo", compressor = tiledb::Compressor("UNKNOWN", -1)))
})

test_that("tiledb::Attr throws an error with invalid ncells argument", {
  ctx <- tiledb::Ctx() 
  a1 <- tiledb::Attr(ctx, "foo", ncells = 1)
  expect_equal(tiledb::ncells(a1), 1) 
  expect_error(tiledb::Attr(ctx, "foo", ncells = 0))
})