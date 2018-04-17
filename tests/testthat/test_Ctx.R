library(tiledb)
context("tiledb::Ctx")

test_that("tiledb::Ctx default constructor", {
  ctx <- tiledb::Ctx()
  expect_is(ctx, "Ctx")
})

test_that("tiledb::Ctx constructor with tiledb::Config", {
  cfg <- tiledb::Config(c(foo = "bar"))
  ctx <- tiledb::Ctx(cfg)
  expect_is(ctx, "Ctx")
  ctx_cfg <- tiledb::config(ctx)
  expect_equal(cfg["foo"], ctx_cfg["foo"])
})

test_that("tiledb::Ctx constructor with named vector config", {
  ctx <- tiledb::Ctx(c(foo = "bar"))
  expect_is(ctx, "Ctx")
  cfg <- tiledb::config(ctx)
  expect_is(cfg, "Config")
  expect_equal(cfg["foo"], c(foo = "bar"))
})

test_that("tiledb::Ctx is_supported_fs works", {
  ctx <- tiledb::Ctx()
  expect_true(tiledb::is_supported_fs(ctx, "file"))
  expect_error(tiledb::is_supported_fs(ctx, "should_error"))
})