library(tiledb)
context("tiledb::Dim")

test_that("tiledb::Dim default constructor", {
  ctx <- tiledb::Ctx()
  dim <- tiledb::Dim(ctx, "foo", c(1, 100))
  expect_is(dim, "Dim")
})

test_that("tiledb::Dim throws an error on missing constructor argument", {
  ctx <- tiledb::Ctx()
  expect_error(tiledb::Dim(ctx, "foo"))
})

test_that("tiledb::Dim throws an error on invalid domain", {
  ctx <- tiledb::Ctx()
  expect_error(tiledb::Dim(ctx, "foo", c(100L, 1L), type = "INT32"))
})

test_that("tiledb::Dim throws an error on invalid type", {
  ctx <- tiledb::Ctx()
  expect_error(tiledb::Dim(ctx, "foo", c(1, 100), type = "INVALID"))
})

test_that("tiledb::Dim default type is double", {
  ctx <- tiledb::Ctx()
  dim <- tiledb::Dim(ctx, "foo", c(1, 100))
  expect_equal(tiledb::datatype(dim), "FLOAT64")
})

test_that("tiledb::Dim default type is the domain type", {
  ctx <- tiledb::Ctx()
  
  dim <- tiledb::Dim(ctx, "foo", c(1.0, 100.0))
  expect_equal(tiledb::datatype(dim), "FLOAT64")
  
  dim <- tiledb::Dim(ctx, "foo", c(1L, 100L))
  expect_equal(tiledb::datatype(dim), "INT32")
})

test_that("tiledb::Dim name", {
  ctx <- tiledb::Ctx()
  dim <- tiledb::Dim(ctx, "foo", c(1L, 100L))
  expect_equal(tiledb::name(dim), "foo")
  
  dim <- tiledb::Dim(ctx, "", c(1L, 100L))
  expect_equal(tiledb::name(dim), "")
})

test_that("tiledb::Dim tile should equal constructor", {
  ctx <- tiledb::Ctx()
  dim <- tiledb::Dim(ctx, "foo", c(1L, 100L), tile=10L, type="INT32")
  expect_equal(tiledb::tile(dim), 10L)
})

test_that("tiledb::Dim default tile extent should span the whole domain", {
  ctx <- tiledb::Ctx() 
  
  dim <- tiledb::Dim(ctx, "foo", c(1L, 100L), type = "INT32")
  expect_equal(tiledb::tile(dim), 100L)
  
  dim <- tiledb::Dim(ctx, "foo", c(1L, 1L), type = "INT32")
  expect_equal(tiledb::tile(dim), 1L)
  
  dim <- tiledb::Dim(ctx, "foo", c(1.1, 11.9), type = "FLOAT64") 
  expect_equal(tiledb::tile(dim), 11.9 - 1.1)
})

test_that("tiledb::Dim empty name is anonymous", {
  ctx <- tiledb::Ctx() 
  dim <- tiledb::Dim(ctx, "", c(1L, 100L))
  expect_true(is.anonymous(dim)) 
  
  dim <- tiledb::Dim(ctx, "foo", c(1L, 100L))
  expect_false(is.anonymous(dim))
})

test_that("tiledb::Dim datatype is correct", {
  ctx <- tiledb::Ctx() 
  dim <- tiledb::Dim(ctx,"", c(1L, 100L), type = "INT32")
  expect_equal(tiledb::datatype(dim), "INT32")
  
  dim <- tiledb::Dim(ctx, "", c(1, 100), type = "FLOAT64")
  expect_equal(tiledb::datatype(dim), "FLOAT64")
})

test_that("tiledb::Dim dim S3 method is correct", {
  ctx <- tiledb::Ctx()
  d <- tiledb::Dim(ctx, "", c(-1L, 100L))
  expect_equal(dim(d), 102L)
  
  d <- tiledb::Dim(ctx, "", c(1, 100))
  expect_error(dim(d))
})