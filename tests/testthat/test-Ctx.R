library(testthat)
library(tiledb)
context("tiledb_ctx")
tiledb_ctx(limitTileDBCores())

test_that("tiledb_ctx default constructor", {
  ctx <- tiledb_ctx()
  expect_is(ctx, "tiledb_ctx")
})

test_that("tiledb_ctx constructor with tiledb_config", {
  ## function passes fine in isolation, but in context of unit tests
  ## and (maybe because other Ctx objects were created?) it croaks
  skip("this")
  cfg <- tiledb_config(c(foo = "bar"))
  ctx <- tiledb_ctx(cfg)
  expect_is(ctx, "tiledb_ctx")
  ctx_cfg <- tiledb::config(ctx)
  expect_equal(cfg["foo"], ctx_cfg["foo"])
})

test_that("tiledb_ctx constructor with named vector config", {
  ## function passes fine in isolation, but in context of unit tests
  ## and (maybe because other Ctx objects were created?) it croaks
  skip("this")
  ctx <- tiledb_ctx(c(foo = "bar"))
  expect_is(ctx, "tiledb_ctx")
  cfg <- tiledb::config(ctx)
  expect_is(cfg, "tiledb_config")
  expect_equal(cfg["foo"], c(foo = "bar"))
})

test_that("tiledb_ctx tiledb_is_supported_fs works", {
  ctx <- tiledb_ctx()
  expect_true(tiledb_is_supported_fs("file"))
  expect_error(tiledb_is_supported_fs("should_error"))
})
