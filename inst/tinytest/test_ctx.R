library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

tiledb_ctx(limitTileDBCores())

#test_that("tiledb_ctx default constructor", {
ctx <- tiledb_ctx()
expect_true(is(ctx, "tiledb_ctx"))
#})

#test_that("tiledb_ctx constructor with tiledb_config", {
cfg <- tiledb_config(c(foo = "bar"))
ctx <- tiledb_ctx(cfg)
expect_true(is(ctx, "tiledb_ctx"))
ctx_cfg <- tiledb::config(ctx)
expect_equal(cfg["foo"], ctx_cfg["foo"])
# })

#test_that("tiledb_ctx constructor with named vector config", {
ctx <- tiledb_ctx(c(foo = "bar"))
expect_true(is(ctx, "tiledb_ctx"))
cfg <- tiledb::config(ctx)
expect_true(is(cfg, "tiledb_config"))
expect_equal(cfg["foo"], c(foo = "bar"))
# })

#test_that("tiledb_ctx tiledb_is_supported_fs works", {
ctx <- tiledb_ctx()
expect_true(tiledb_is_supported_fs("file"))
expect_error(tiledb_is_supported_fs("should_error"))
#})
