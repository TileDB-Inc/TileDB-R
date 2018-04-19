library(tiledb)
context("tiledb::Attr")

test_that("tiledb::Attr constructor works", {
  ctx <- tiledb::Ctx()
  a1 <- tiledb::Attr(ctx, "foo")
  expect_is(a1, "Attr")
})

test_that("tiledb::Attr constructor defaults are correct", {
  ctx <- tiledb::Ctx()
  a1 <- tiledb::Attr(ctx, "foo")
  expect_equal(tiledb::name(a1), "foo")
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