
library(testthat)
library(tiledb)
context("tiledb_date_time")

test_that("Can read / write a simple Date vector", {
  uri <- tempfile()

  op <- options()
  options("tiledb.useRDatetimeType"=FALSE,
          "tiledb.castTime"=TRUE)

  dates <- Sys.Date() + 0:9
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)

  dim <- tiledb_dim("dim", domain = c(1L, 10L))
  dom <- tiledb_domain(dim)
  val <- tiledb_attr("dat", type = "DATETIME_DAY")
  sch <- tiledb_array_schema(dom, val)
  tiledb_array_create(uri, sch)

  arr <- tiledb_dense(uri)
  arr[] <- dates

  arr2 <- tiledb_dense(uri)
  expect_equal(dates, arr2[])

  options(op)
  unlink(uri, recursive=TRUE)
})

test_that("Can read / write simple DATETIME_SEC vectors", {
  uri <- tempfile()

  op <- options()
  options("tiledb.useRDatetimeType"=FALSE,
          "tiledb.castTime"=TRUE)

  datetimes <- Sys.time() + 0:59
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)

  dim <- tiledb_dim("dim", domain = c(1L, 60L))
  dom <- tiledb_domain(dim)
  val <- tiledb_attr("dat", type = "DATETIME_SEC")
  sch <- tiledb_array_schema(dom, val)
  tiledb_array_create(uri, sch)

  arr <- tiledb_dense(uri)
  arr[] <- datetimes

  arr2 <- tiledb_dense(uri)
  expect_equal(trunc(datetimes), arr2[])

  options(op)
  unlink(uri, recursive=TRUE)

})

test_that("Can read / write simple DATETIME_MS vectors", {
  uri <- tempfile()

  op <- options()
  options("tiledb.useRDatetimeType"=FALSE,
          "tiledb.castTime"=TRUE)

  datetimes <- Sys.time() + 0:59
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)

  dim <- tiledb_dim("dim", domain = c(1L, 60L))
  dom <- tiledb_domain(dim)
  val <- tiledb_attr("dat", type = "DATETIME_MS")
  sch <- tiledb_array_schema(dom, val)
  tiledb_array_create(uri, sch)

  arr <- tiledb_dense(uri)
  arr[] <- datetimes

  arr2 <- tiledb_dense(uri)
  expect_equal(trunc(1e3*as.numeric(datetimes))/1e3, as.numeric(arr2[]))

  options(op)
  unlink(uri, recursive=TRUE)

})

test_that("Can read / write simple DATETIME_US vectors", {
  uri <- tempfile()

  op <- options()
  options("tiledb.useRDatetimeType"=FALSE,
          "tiledb.castTime"=TRUE)

  datetimes <- Sys.time() + 0:59
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)

  dim <- tiledb_dim("dim", domain = c(1L, 60L))
  dom <- tiledb_domain(dim)
  val <- tiledb_attr("dat", type = "DATETIME_US")
  sch <- tiledb_array_schema(dom, val)
  tiledb_array_create(uri, sch)

  arr <- tiledb_dense(uri)
  arr[] <- datetimes

  arr2 <- tiledb_dense(uri)
  expect_equal(datetimes, arr2[])

  options(op)
  unlink(uri, recursive=TRUE)

})
