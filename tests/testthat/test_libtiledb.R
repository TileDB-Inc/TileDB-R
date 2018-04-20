library(tiledb)
context("libtiledb")

test_that("version is valid", {
  ver <- libtiledb_version()
  expect_equal(length(ver), 3)
  expect_equal(ver[1], c(major = 1))
  expect_gte(ver[2], c(minor = 0))
  expect_gte(ver[3], c(patch = 0))
})

test_that("default tiledb_config constructor", {
  config <- tiledb_config()
  config <- tiledb_config_set(config, "foo", "10")
  expect_equal(tiledb_config_get(config, "foo"), c("foo" = "10"))
})

test_that("construct tiledb_config with vector of parameters", {
  params = c("foo" = "bar")
  config <- tiledb_config(params)
  expect_equal(tiledb_config_get(config, "foo"), c("foo" = "bar"))
})

test_that("tiledb_config_get throws an error if paramter does not exist", {
  config <- tiledb_config()
  expect_equal(unname(tiledb_config_get(config, "don't exist")), NA_character_)
})

test_that("construct tiledb_config with an empty vector of paramters", {
  params = c()
  default_config <- tiledb_config()
  params_config <- tiledb_config(params)
  expect_equal(
    tiledb_config_get(default_config, "sm.tile_cache_size"),
    tiledb_config_get(params_config, "sm.tile_cache_size")
  )
})

test_that("tiledb_config can be converted to an R vector", {
  config <- tiledb_config()
  config_vec <- tiledb_config_vector(config)
  expect_is(config_vec, "character")
  check <- c()
  for (n in names(config_vec)) {
    expect_equal(tiledb_config_get(config, n), config_vec[n])
  }
})

test_that("can create a tiledb_ctx", {
  ctx <- tiledb_ctx()
  expect_is(ctx, "externalptr")
})

test_that("default tiledb_ctx config is the default config", {
  ctx <- tiledb_ctx()
  ctx_config <- tiledb_ctx_config(ctx)
  default_config <- tiledb_config()
  expect_equal(tiledb_config_vector(ctx_config),
               tiledb_config_vector(default_config))
})

test_that("tiledb_ctx with config", {
  config <- tiledb_config(c(foo = "bar"))
  ctx <- tiledb_ctx(config)
  expect_equal(tiledb_config_get(tiledb_ctx_config(ctx), "foo"),
               c(foo = "bar"))
})

test_that("tiledb_ctx fs support", {
  ctx <- tiledb_ctx()
  expect_true(tiledb_ctx_is_supported_fs(ctx, "file"))
  expect_is(tiledb_ctx_is_supported_fs(ctx, "s3"), "logical")
  expect_is(tiledb_ctx_is_supported_fs(ctx, "hdfs"), "logical")
  expect_error(tiledb_ctx_is_supported_fs(ctx, "should error"))
})

test_that("basic int32 tiledb_dim constructor works", {
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "d1", "INT32", c(1L, 100L), 10L)
  expect_is(dim, "externalptr")
})

test_that("basic float64 tiledb_dim constructor works", {
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "d1", "FLOAT64", c(1.0, 100.0), 10.0)
  expect_is(dim, "externalptr")
})

test_that("basic tiledb_domain constructor works", {
  ctx <- tiledb_ctx()
  d1 <- tiledb_dim(ctx, "d1", "INT32", c(1L, 100L), 10L)
  d2 <- tiledb_dim(ctx, "d2", "INT32", c(1L, 100L), 10L)
  dom <- tiledb_domain(ctx, c(d1, d2))
  expect_is(dom, "externalptr")
})

test_that("tiledb_domain throws an error when dimensions are different dtypes", {
  ctx <- tiledb_ctx()
  d1 <- tiledb_dim(ctx, "d1", "INT32", c(1L, 100L), 10L)
  d2 <- tiledb_dim(ctx, "d2", "FLOAT64", c(1, 100), 10)
  expect_error(tiledb_domain(ctx, c(d1, d2)))
})

test_that("basic integer tiledb_attr constructor works", {
  ctx <- tiledb_ctx()
  com <- tiledb_compressor("NO_COMPRESSION", -1)
  attr <- tiledb_attr(ctx, "a1", "INT32", com, 1)
  expect_is(attr, "externalptr")
})

test_that("basic float64 tiledb_attr constructor works", {
  ctx <- tiledb_ctx()
  com <- tiledb_compressor("NO_COMPRESSION", -1)
  attr <- tiledb_attr(ctx, "a1", "FLOAT64", com, 1)
  expect_is(attr, "externalptr")
})

test_that("basic tiledb_array_schema constructor works", {
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb_domain(ctx, c(dim))
  com <- tiledb_compressor("GZIP", 5)
  att <- tiledb_attr(ctx, "a1", "FLOAT64", com, 1)
  sch <- tiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  expect_is(sch, "externalptr")
})

test_that("basic dense vector tiledb_array creation works", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
   }
   dir.create(tmp)
  })
  
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb_domain(ctx, c(dim))
  com <- tiledb_compressor("NO_COMPRESSION", -1)
  att <- tiledb_attr(ctx, "a1", "FLOAT64", com, 1)
  sch <- tiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  pth <- paste(tmp, "test_array", sep = "/")
  uri <- tiledb_array_create(sch, pth)
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
  
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb_domain(ctx, c(dim))
  com <- tiledb_compressor("NO_COMPRESSION", -1)
  att <- tiledb_attr(ctx, "a1", "FLOAT64", com, 1)
  sch <- tiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  pth <- paste(tmp, "test_dense_read_write", sep = "/")
  uri <- tiledb_array_create(sch, pth)
  
  dat <- c(3, 2, 1) 
  qry <- tiledb_query(ctx, uri, "WRITE")
  qry <- tiledb_query_set_buffer(qry, "a1", dat)
  qry <- tiledb_query_submit(qry)
  expect_is(qry, "externalptr")
  
  res <- c(0, 0, 0)
  qry2 <- tiledb_query(ctx, uri, "READ")
  qry2 <- tiledb_query_set_buffer(qry2, "a1", res)
  qry2 <- tiledb_query_submit(qry2)
  expect_equal(res, dat)
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
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "d1", "INT32", c(1L, 3L), 3L)
  dom <- tiledb_domain(ctx, c(dim))
  com <- tiledb_compressor("NO_COMPRESSION", -1)
  att <- tiledb_attr(ctx, "a1", "FLOAT64", com, 1)
  sch <- tiledb_array_schema(ctx, dom, c(att), cell_order = "COL_MAJOR", tile_order = "COL_MAJOR", sparse = FALSE)
  pth <- paste(tmp, "test_dense_read_write", sep = "/")
  uri <- tiledb_array_create(sch, pth)
  
  dat <- c(3, 2, 1) 
  qry <- tiledb_query(ctx, uri, "WRITE")
  qry <- tiledb_query_set_buffer(qry, "a1", dat)
  qry <- tiledb_query_submit(qry)
  expect_is(qry, "externalptr")
  
  res <- c(0, 0)
  sub <- c(1, 2)
  qry2 <- tiledb_query(ctx, uri, "READ")
  qry2 <- tiledb_query_set_buffer(qry2, "a1", res)
  qry2 <- tiledb_query_submit(qry2)
  expect_equal(res, dat[sub])
  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("basic tiledb vfs constructor works", {
  ctx <- tiledb_ctx()
  vfs <- tiledb_vfs(ctx)
  expect_is(vfs, "externalptr")
  
  config <- tiledb_config(c(foo="bar"))
  vfs <- tiledb_vfs(ctx, config)
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
  
  ctx <- tiledb_ctx()
  vfs <- tiledb_vfs(ctx)
  
  # test dir 
  expect_true(tiledb_vfs_is_dir(vfs, tmp))
  expect_false(tiledb_vfs_is_dir(vfs, "i don't exist"))
 
  test_file_path <- paste("file:/", tmp, "test_file", sep = "/")
  test_file = file(test_file_path, "wb")
  writeChar(c("foo", "bar", "baz"), test_file)
  close(test_file)
  
  # test file
  expect_true(tiledb_vfs_is_file(vfs, test_file_path))
  expect_false(tiledb_vfs_is_file(vfs, tmp))
  teardown({
    unlink(tmp, recursive = TRUE)
  })
})