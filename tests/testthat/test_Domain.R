library(tiledb)
context("tiledb::Domain")

test_that("tiledb::Domain basic constructor", {
  ctx <- tiledb_ctx()
  d1  <- tiledb_dim(ctx, "d1", c(1L, 100L))
  dom <- tiledb::Domain(ctx, list(d1))
  expect_is(dom, "Domain")
})

test_that("tiledb::Domain constructor works for multiple tiledb_dim's", {
  ctx <- tiledb_ctx()
  d1  <- tiledb_dim(ctx, "d1", c(1L, 100L))
  d2  <- tiledb_dim(ctx, "d2", c(1L, 100L))
  d3  <- tiledb_dim(ctx, "d3", c(1L, 100L))
  dom <- tiledb::Domain(ctx, list(d1, d2, d3))
  expect_is(dom, "Domain") 
})

test_that("tiledb::Domain constructor fails with no tiledb_dim", {
  ctx <- tiledb_ctx()
  expect_error(tiledb::Domain(ctx, list()))
})

test_that("tiledb::Domain constructor failes with tiledb_dim of different dtypes", {
  ctx <- tiledb_ctx()
  d1  <- tiledb_dim(ctx, "d1", c(1L, 100L))
  d2  <- tiledb_dim(ctx, "d2", c(1.3, 2.8))
  expect_error(tiledb::Domain(ctx, list(d1, d2)))
})

test_that("tiledb::Domain dimensions works", {
  ctx <- tiledb_ctx()
  d1  <- tiledb_dim(ctx, "d1", c(1L, 100L))
  d2  <- tiledb_dim(ctx, "d2", c(1L, 100L))
  d3  <- tiledb_dim(ctx, "d3", c(1L, 100L))
  dom <- tiledb::Domain(ctx, list(d1, d2, d3))
  dims <- tiledb::dimensions(dom)
  expect_equal(length(dims), 3L)
  expect_true(all(as.logical(lapply(dims, function(o) is(o, "tiledb_dim")))))
})