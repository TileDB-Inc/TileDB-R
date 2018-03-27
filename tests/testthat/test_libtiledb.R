library(tiledb)
context("libtiledb")

test_that("version is valid", {
  ver <- tiledb_version()
  expect_equal(length(ver), 3)
  expect_equal(ver[0], 1)
  expect_gte(ver[1], 0)
  expect_gte(ver[2], 0)
})

test_that("default tiledb_config constructor", {
  config <- tiledb_config()
  config <- tiledb_config_set(config, "foo", "10")
  expect_equal(tiledb_config_get(config, "foo"), c("foo"="10"))
})

test_that("construct tiledb_config with vector of parameters", {
  params = c("foo"="bar") 
  config <- tiledb_config(params)
  expect_equal(tiledb_config_get(config, "foo"), c("foo"="bar"))
})

test_that("construct tiledb_config with an empty vector of paramters", {
  params = c()
  default_config <- tiledb_config()
  params_config <- tiledb_config(params) 
  expect_equal(tiledb_config_get(default_config, "sm.tile_cache_size"), 
               tiledb_config_get(params_config, "sm.tile_cache_size"))
})

test_that("tiledb_config can be converted to an R vector", {
  config <- tiledb_config()
  config_vec <- tiledb_config_vector(config)
  expect_is(config_vec, "character")
  check <- c()
  for(n in names(config_vec)) {
    expect_equal(tiledb_config_get(config, n), config_vec[n]) 
  }
})

test_that("can create a tiledb_ctx", {
  ctx <- tiledb_ctx()
  expect_is(ctx, "externalptr")
})

test_that("default tiledb_ctx config is default config", {
  ctx <- tiledb_ctx()
  ctx_config <- tiledb_ctx_config(ctx)
  default_config <- tiledb_config()
  expect_equal(tiledb_config_vector(ctx_config),
               tiledb_config_vector(default_config))
})