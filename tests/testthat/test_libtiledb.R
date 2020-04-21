library(testthat)
library(tiledb)
context("tiledb::libtiledb")

test_that("version is valid", {
  ver <- tiledb_version()
  expect_equal(length(ver), 3)
  expect_gte(ver[1], c(major = 1))
  expect_gte(ver[2], c(minor = 0))
  expect_gte(ver[3], c(patch = 0))
})

test_that("default libtiledb_config constructor", {
  config <- tiledb:::libtiledb_config()
  config <- tiledb:::libtiledb_config_set(config, "foo", "10")
  expect_equal(tiledb:::libtiledb_config_get(config, "foo"), c("foo" = "10"))
})

test_that("construct libtiledb_config with vector of parameters", {
  params = c("foo" = "bar")
  config <- tiledb:::libtiledb_config(params)
  expect_equal(tiledb:::libtiledb_config_get(config, "foo"), c("foo" = "bar"))
})

test_that("libtiledb_config_get throws an error if paramter does not exist", {
  config <- tiledb:::libtiledb_config()
  expect_equal(unname(tiledb:::libtiledb_config_get(config, "don't exist")), NA_character_)
})

test_that("construct libtiledb_config with an empty vector of paramters", {
  params = c()
  default_config <- tiledb:::libtiledb_config()
  params_config <- tiledb:::libtiledb_config(params)
  expect_equal(
    tiledb:::libtiledb_config_get(default_config, "sm.tile_cache_size"),
    tiledb:::libtiledb_config_get(params_config, "sm.tile_cache_size")
  )
})

test_that("tiledb_config can be converted to an R vector", {
  config <- tiledb:::libtiledb_config()
  config_vec <- tiledb:::libtiledb_config_vector(config)
  expect_is(config_vec, "character")
  check <- c()
  for (n in names(config_vec)) {
    expect_equal(tiledb:::libtiledb_config_get(config, n), config_vec[n])
  }
})

test_that("can create a libtiledb_ctx", {
  ctx <- tiledb:::libtiledb_ctx()
  expect_is(ctx, "externalptr")
})

test_that("default libtiledb_ctx config is the default config", {
  ctx <- tiledb:::libtiledb_ctx()
  ctx_config <- tiledb:::libtiledb_ctx_config(ctx)
  default_config <- tiledb:::libtiledb_config()
  expect_equal(tiledb:::libtiledb_config_vector(ctx_config),
               tiledb:::libtiledb_config_vector(default_config))
})

test_that("libtiledb_ctx with config", {
  config <- tiledb:::libtiledb_config(c(foo = "bar"))
  ctx <- tiledb:::libtiledb_ctx(config)
  expect_equal(tiledb:::libtiledb_config_get(tiledb:::libtiledb_ctx_config(ctx), "foo"),
               c(foo = "bar"))
})

test_that("libtiledb_ctx fs support", {
  ctx <- tiledb:::libtiledb_ctx()
  expect_true(tiledb:::libtiledb_ctx_is_supported_fs(ctx, "file"))
  expect_is(tiledb:::libtiledb_ctx_is_supported_fs(ctx, "s3"), "logical")
  expect_is(tiledb:::libtiledb_ctx_is_supported_fs(ctx, "hdfs"), "logical")
  expect_error(tiledb:::libtiledb_ctx_is_supported_fs(ctx, "should error"))
})

test_that("basic int32 libtiledb_dim constructor works", {
  ctx <- tiledb:::libtiledb_ctx()
  dim <- tiledb:::libtiledb_dim(ctx, "d1", "INT32", c(1L, 100L), 10L)
  expect_is(dim, "externalptr")
})

test_that("basic float64 libtiledb_dim constructor works", {
  ctx <- tiledb:::libtiledb_ctx()
  dim <- tiledb:::libtiledb_dim(ctx, "d1", "FLOAT64", c(1.0, 100.0), 10.0)
  expect_is(dim, "externalptr")
})

test_that("basic libtiledb_domain constructor works", {
  ctx <- tiledb:::libtiledb_ctx()
  d1 <- tiledb:::libtiledb_dim(ctx, "d1", "INT32", c(1L, 100L), 10L)
  d2 <- tiledb:::libtiledb_dim(ctx, "d2", "INT32", c(1L, 100L), 10L)
  dom <- tiledb:::libtiledb_domain(ctx, c(d1, d2))
  expect_is(dom, "externalptr")
})

test_that("libtiledb_domain throws an error when dimensions are different dtypes", {
  ctx <- tiledb:::libtiledb_ctx()
  d1 <- tiledb:::libtiledb_dim(ctx, "d1", "INT32", c(1L, 100L), 10L)
  d2 <- tiledb:::libtiledb_dim(ctx, "d2", "FLOAT64", c(1, 100), 10)
  if (tiledb_version(compact=TRUE) < as.package_version("1.8.0"))
    expect_error(tiledb:::libtiledb_domain(ctx, c(d1, d2)))
  else {
    dom <- tiledb:::libtiledb_domain(ctx, c(d1, d2))
    expect_is(dom, "externalptr")
  }
})

test_that("basic integer libtiledb_attr constructor works", {
  ctx <- tiledb:::libtiledb_ctx()
  filter <- tiledb:::libtiledb_filter(ctx, "NONE")
  filter_list <- tiledb:::libtiledb_filter_list(ctx, c(filter))
  attr <- tiledb:::libtiledb_attribute(ctx, "a1", "INT32", filter_list, 1)
  expect_is(attr, "externalptr")
})

test_that("basic float64 libtiledb_attr constructor works", {
  ctx <- tiledb:::libtiledb_ctx()
  filter <- tiledb:::libtiledb_filter(ctx, "NONE")
  filter_list <- tiledb:::libtiledb_filter_list(ctx, c(filter))
  attr <- tiledb:::libtiledb_attribute(ctx, "a1", "FLOAT64", filter_list, 1)
  expect_is(attr, "externalptr")
})

test_that("basic libtiledb_array_schema constructor works", {
  ctx <- tiledb:::libtiledb_ctx()
  dim <- tiledb:::libtiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb:::libtiledb_domain(ctx, c(dim))
  filter <- tiledb:::libtiledb_filter(ctx, "GZIP")
  tiledb:::libtiledb_filter_set_option(filter, "COMPRESSION_LEVEL", 5)
  filter_list <- tiledb:::libtiledb_filter_list(ctx, c(filter))
  att <- tiledb:::libtiledb_attribute(ctx, "a1", "FLOAT64", filter_list, 1)
  sch <- tiledb:::libtiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  expect_is(sch, "externalptr")
})

test_that("basic dense vector libtiledb_array creation works", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
   }
   dir.create(tmp)
  })

  ctx <- tiledb:::libtiledb_ctx()
  dim <- tiledb:::libtiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb:::libtiledb_domain(ctx, c(dim))
  filter <- tiledb:::libtiledb_filter(ctx, "NONE")
  filter_list <- tiledb:::libtiledb_filter_list(ctx, c(filter))
  att <- tiledb:::libtiledb_attribute(ctx, "a1", "FLOAT64", filter_list, 1)
  sch <- tiledb:::libtiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  pth <- paste(tmp, "test_array", sep = "/")
  uri <- tiledb:::libtiledb_array_create(pth, sch)
  expect_true(dir.exists(pth))
  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("basic dense vector writes / reads works", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
   }
   dir.create(tmp)
  })

  ctx <- tiledb:::libtiledb_ctx()
  dim <- tiledb:::libtiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb:::libtiledb_domain(ctx, c(dim))
  filter <- tiledb:::libtiledb_filter(ctx, "NONE")
  filter_list <- tiledb:::libtiledb_filter_list(ctx, c(filter))
  att <- tiledb:::libtiledb_attribute(ctx, "a1", "FLOAT64", filter_list, 1)
  sch <- tiledb:::libtiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  pth <- paste(tmp, "test_dense_read_write", sep = "/")
  uri <- tiledb:::libtiledb_array_create(pth, sch)

  dat <- c(3, 2, 1)
  arr <- tiledb:::libtiledb_array_open(ctx, uri, "WRITE")
  qry <- tiledb:::libtiledb_query(ctx, arr, "WRITE")
  qry <- tiledb:::libtiledb_query_set_buffer(qry, "a1", dat)
  qry <- tiledb:::libtiledb_query_submit(qry)
  tiledb:::libtiledb_array_close(arr)
  expect_is(qry, "externalptr")

  res <- c(0, 0, 0)
  arr <- tiledb:::libtiledb_array_open(ctx, uri, "READ")
  qry2 <- tiledb:::libtiledb_query(ctx, arr, "READ")
  qry2 <- tiledb:::libtiledb_query_set_buffer(qry2, "a1", res)
##  qry2 <- libtiledb_query_submit(qry2)
  tiledb:::libtiledb_array_close(arr)
##  expect_equal(res, dat)
  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("basic dense vector read subarray works", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
   }
   dir.create(tmp)
  })
  ctx <- tiledb:::libtiledb_ctx()
  dim <- tiledb:::libtiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb:::libtiledb_domain(ctx, c(dim))
  filter <- tiledb:::libtiledb_filter(ctx, "NONE")
  filter_list <- tiledb:::libtiledb_filter_list(ctx, c(filter))
  att <- tiledb:::libtiledb_attribute(ctx, "a1", "FLOAT64", filter_list, 1)
  sch <- tiledb:::libtiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  pth <- paste(tmp, "test_dense_read_write", sep = "/")
  uri <- tiledb:::libtiledb_array_create(pth, sch)

  dat <- c(3, 2, 1)
  arr <- tiledb:::libtiledb_array_open(ctx, uri, "WRITE")
  qry <- tiledb:::libtiledb_query(ctx, arr, "WRITE")
  qry <- tiledb:::libtiledb_query_set_buffer(qry, "a1", dat)
  qry <- tiledb:::libtiledb_query_submit(qry)
  tiledb:::libtiledb_array_close(arr)
  expect_is(qry, "externalptr")

  res <- c(0, 0)
  sub <- c(1L, 2L)
  arr <- tiledb:::libtiledb_array_open(ctx, uri, "READ")
  qry2 <- tiledb:::libtiledb_query(ctx, arr, "READ")
  qry2 <- tiledb:::libtiledb_query_set_subarray(qry2, sub)
  qry2 <- tiledb:::libtiledb_query_set_buffer(qry2, "a1", res)
  qry2 <- tiledb:::libtiledb_query_submit(qry2)
  tiledb:::libtiledb_array_close(arr)
  expect_equal(res, dat[sub])
  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("basic tiledb vfs constructor works", {
  ctx <- tiledb:::libtiledb_ctx()
  vfs <- tiledb:::tiledb_vfs(ctx)
  expect_is(vfs, "externalptr")

  config <- tiledb:::libtiledb_config(c(foo="bar"))
  vfs <- tiledb:::tiledb_vfs(ctx, config)
  expect_is(vfs, "externalptr")
})

test_that("basic vfs is_dir, is_file functionality works", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
   }
   dir.create(tmp)
  })

  ctx <- tiledb:::libtiledb_ctx()
  vfs <- tiledb:::tiledb_vfs(ctx)

  # test dir
  expect_true(tiledb:::tiledb_vfs_is_dir(vfs, tmp))
  expect_false(tiledb:::tiledb_vfs_is_dir(vfs, "i don't exist"))

  test_file_path <- paste("file:/", tmp, "test_file", sep = "/")
  test_file <- file(test_file_path, "wb")
  writeChar(c("foo", "bar", "baz"), test_file)
  close(test_file)

  # test file
  expect_true(tiledb:::tiledb_vfs_is_file(vfs, test_file_path))
  expect_false(tiledb:::tiledb_vfs_is_file(vfs, tmp))
  teardown({
    unlink(tmp, recursive = TRUE)
  })
})
