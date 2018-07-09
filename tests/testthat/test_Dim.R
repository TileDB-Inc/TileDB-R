library(tiledb)
context("tiledb_dim")

test_that("tiledb_dim default constructor", {
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "foo", c(1, 100))
  expect_is(dim, "tiledb_dim")
})

test_that("tiledb_dim throws an error on missing constructor argument", {
  ctx <- tiledb_ctx()
  expect_error(tiledb_dim(ctx, "foo"))
})

test_that("tiledb_dim throws an error on invalid domain", {
  ctx <- tiledb_ctx()
  expect_error(tiledb_dim(ctx, "foo", c(100L, 1L), type = "INT32"))
})

test_that("tiledb_dim throws an error on invalid type", {
  ctx <- tiledb_ctx()
  expect_error(tiledb_dim(ctx, "foo", c(1, 100), type = "INVALID"))
})

test_that("tiledb_dim default type is double", {
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "foo", c(1, 100))
  expect_equal(tiledb::datatype(dim), "FLOAT64")
})

test_that("tiledb_dim default type is the domain type", {
  ctx <- tiledb_ctx()
  
  dim <- tiledb_dim(ctx, "foo", c(1.0, 100.0))
  expect_equal(tiledb::datatype(dim), "FLOAT64")
  
  dim <- tiledb_dim(ctx, "foo", c(1L, 100L))
  expect_equal(tiledb::datatype(dim), "INT32")
})

test_that("tiledb_dim name", {
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "foo", c(1L, 100L))
  expect_equal(tiledb::name(dim), "foo")
  
  dim <- tiledb_dim(ctx, "", c(1L, 100L))
  expect_equal(tiledb::name(dim), "")
})

test_that("tiledb_dim tile should equal constructor", {
  ctx <- tiledb_ctx()
  dim <- tiledb_dim(ctx, "foo", c(1L, 100L), tile=10L, type="INT32")
  expect_equal(tiledb::tile(dim), 10L)
})

test_that("tiledb_dim default tile extent should span the whole domain", {
  ctx <- tiledb_ctx() 
  
  dim <- tiledb_dim(ctx, "foo", c(1L, 100L), type = "INT32")
  expect_equal(tiledb::tile(dim), 100L)
  
  dim <- tiledb_dim(ctx, "foo", c(1L, 1L), type = "INT32")
  expect_equal(tiledb::tile(dim), 1L)
  
  dim <- tiledb_dim(ctx, "foo", c(1.1, 11.9), type = "FLOAT64") 
  expect_equal(tiledb::tile(dim), 11.9 - 1.1)
})

test_that("tiledb_dim empty name is anonymous", {
  ctx <- tiledb_ctx() 
  dim <- tiledb_dim(ctx, "", c(1L, 100L))
  expect_true(is.anonymous(dim)) 
  
  dim <- tiledb_dim(ctx, "foo", c(1L, 100L))
  expect_false(is.anonymous(dim))
})

test_that("tiledb_dim tiledb::datatype()", {
  ctx <- tiledb_ctx() 
  dim <- tiledb_dim(ctx,"", c(1L, 100L), type = "INT32")
  expect_equal(tiledb::datatype(dim), "INT32")
  
  dim <- tiledb_dim(ctx, "", c(1, 100), type = "FLOAT64")
  expect_equal(tiledb::datatype(dim), "FLOAT64")
})

test_that("tiledb_dim dim() method", {
  ctx <- tiledb_ctx()
  d <- tiledb_dim(ctx, "", c(-1L, 100L))
  expect_equal(dim(d), 102L)
  
  d <- tiledb_dim(ctx, "", c(1, 100))
  expect_error(dim(d))
})