library(tiledb)
context("tiledb::Domain")

test_that("tiledb::Domain basic constructor", {
  ctx <- tiledb::Ctx()
  d1  <- tiledb::Dim(ctx, "d1", c(1L, 100L))
  dom <- tiledb::Domain(ctx, list(d1))
  expect_is(dom, "Domain")
})

test_that("tiledb::Domain constructor works for multiple tiledb::Dim's", {
  ctx <- tiledb::Ctx()
  d1  <- tiledb::Dim(ctx, "d1", c(1L, 100L))
  d2  <- tiledb::Dim(ctx, "d2", c(1L, 100L))
  d3  <- tiledb::Dim(ctx, "d3", c(1L, 100L))
  dom <- tiledb::Domain(ctx, list(d1, d2, d3))
  expect_is(dom, "Domain") 
})

test_that("tiledb::Domain constructor fails with no tiledb::Dim", {
  ctx <- tiledb::Ctx()
  expect_error(tiledb::Domain(ctx, list()))
})

test_that("tiledb::Domain constructor failes with tiledb::Dim of different dtypes", {
  ctx <- tiledb::Ctx()
  d1  <- tiledb::Dim(ctx, "d1", c(1L, 100L))
  d2  <- tiledb::Dim(ctx, "d2", c(1.3, 2.8))
  expect_error(tiledb::Domain(ctx, list(d1, d2)))
})

test_that("tiledb::Domain dimensions works", {
  ctx <- tiledb::Ctx()
  d1  <- tiledb::Dim(ctx, "d1", c(1L, 100L))
  d2  <- tiledb::Dim(ctx, "d2", c(1L, 100L))
  d3  <- tiledb::Dim(ctx, "d3", c(1L, 100L))
  dom <- tiledb::Domain(ctx, list(d1, d2, d3))
  dims <- tiledb::dimensions(dom)
  expect_equal(length(dims), 3L)
  expect_true(all(as.logical(lapply(dims, function(o) is(o, "Dim")))))
})

test_that("is.integer works for Dimension objects", {
  ctx <- tiledb::Ctx()
  d1 <- tiledb::Dim(ctx, "d1", c(1L, 100L), type = "INT32") 
  int.dom <- tiledb::Domain(ctx, c(d1))
  expect_equal(tiledb::datatype(int.dom), "INT32")
  expect_true(tiledb::is.integral(int.dom)) 
  
  d2 <- tiledb::Dim(ctx, "d2", c(1, 100), type = "FLOAT64")
  dbl.dom <- tiledb::Domain(ctx, c(d2))
  expect_equal(tiledb::datatype(dbl.dom), "FLOAT64")
  expect_false(tiledb::is.integral(dbl.dom))
})