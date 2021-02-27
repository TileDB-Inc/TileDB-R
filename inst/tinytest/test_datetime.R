library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

isMacOS <- (Sys.info()['sysname'] == "Darwin")

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("Can read / write a simple Date dense vector", {
uri <- tempfile()

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

unlink(uri, recursive=TRUE)
#})

#test_that("Can read / write simple DATETIME_SEC dense vectors", {
uri <- tempfile()

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
## different tzone behavior between r-release and r-devel so comparing numerically
if (!isMacOS) expect_equal(as.numeric(trunc(datetimes)), as.numeric(arr2[]))

unlink(uri, recursive=TRUE)

#})

#test_that("Can read / write simple DATETIME_MS dense vectors", {
uri <- tempfile()

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

unlink(uri, recursive=TRUE)

#})

#test_that("Can read / write simple DATETIME_US dense vectors", {
uri <- tempfile()

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
expect_equal(datetimes, arr2[], tol=1e-5)

unlink(uri, recursive=TRUE)

#})

#test_that("Can read / write simple DATETIME_NS dense vectors", {

if (requireNamespace("nanotime", quietly = TRUE)) {
  library(nanotime)
  uri <- tempfile()

  datetimes <- nanotime(Sys.time() + 0:59)
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)

  dim <- tiledb_dim("dim", domain = c(1L, 60L))
  dom <- tiledb_domain(dim)
  val <- tiledb_attr("dat", type = "DATETIME_NS")
  sch <- tiledb_array_schema(dom, val)
  tiledb_array_create(uri, sch)

  arr <- tiledb_dense(uri)
  arr[] <- datetimes

  arr2 <- tiledb_dense(uri)
  expect_equal(datetimes, arr2[])

  unlink(uri, recursive=TRUE)
}

#})

## Sparse

#test_that("Can read / write a simple Date sparse vector", {
uri <- tempfile()

dates <- Sys.Date() + 0:9
if (dir.exists(uri)) unlink(uri, recursive=TRUE)

dim <- tiledb_dim("dim", domain = c(1L, 10L))
dom <- tiledb_domain(dim)
val <- tiledb_attr("dat", type = "DATETIME_DAY")
sch <- tiledb_array_schema(dom, val, sparse = TRUE)
tiledb_array_create(uri, sch)

arr <- tiledb_sparse(uri)
arr[1:10] <- dates

arr2 <- tiledb_sparse(uri)
expect_equal(dates, arr2[]$dat)

unlink(uri, recursive=TRUE)
#})

#test_that("Can read / write simple DATETIME_SEC sparse vectors", {
uri <- tempfile()

datetimes <- Sys.time() + 0:59
if (dir.exists(uri)) unlink(uri, recursive=TRUE)

dim <- tiledb_dim("dim", domain = c(1L, 60L))
dom <- tiledb_domain(dim)
val <- tiledb_attr("dat", type = "DATETIME_SEC")
sch <- tiledb_array_schema(dom, val, sparse = TRUE)
tiledb_array_create(uri, sch)

arr <- tiledb_sparse(uri)
arr[1:60] <- datetimes

arr2 <- tiledb_sparse(uri)
## different tzone behavior between r-release and r-devel so comparing numerically
if (!isMacOS) expect_equal(as.numeric(trunc(datetimes)), as.numeric(arr2[]$dat))

unlink(uri, recursive=TRUE)

#})

#test_that("Can read / write simple DATETIME_MS sparse vectors", {
uri <- tempfile()

datetimes <- Sys.time() + 0:59
if (dir.exists(uri)) unlink(uri, recursive=TRUE)

dim <- tiledb_dim("dim", domain = c(1L, 60L))
dom <- tiledb_domain(dim)
val <- tiledb_attr("dat", type = "DATETIME_MS")
sch <- tiledb_array_schema(dom, val, sparse = TRUE)
tiledb_array_create(uri, sch)

arr <- tiledb_sparse(uri)
arr[1:60] <- datetimes

arr2 <- tiledb_sparse(uri)
expect_equal(trunc(1e3*as.numeric(datetimes))/1e3, as.numeric(arr2[]$dat))

unlink(uri, recursive=TRUE)

#})

#test_that("Can read / write simple DATETIME_US sparse vectors", {
uri <- tempfile()

datetimes <- Sys.time() + 0:59
if (dir.exists(uri)) unlink(uri, recursive=TRUE)

dim <- tiledb_dim("dim", domain = c(1L, 60L))
dom <- tiledb_domain(dim)
val <- tiledb_attr("dat", type = "DATETIME_US")
sch <- tiledb_array_schema(dom, val, sparse = TRUE)
tiledb_array_create(uri, sch)

arr <- tiledb_sparse(uri)
arr[1:60] <- datetimes

arr2 <- tiledb_sparse(uri)
expect_equal(datetimes, arr2[]$dat, tol=1e-5)

unlink(uri, recursive=TRUE)

#})

#test_that("Can read / write simple DATETIME_NS sparse vectors", {

if (requireNamespace("nanotime", quietly = TRUE)) {
  library(nanotime)
  uri <- tempfile()

  datetimes <- nanotime(Sys.time() + 0:59)
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)

  dim <- tiledb_dim("dim", domain = c(1L, 60L))
  dom <- tiledb_domain(dim)
  val <- tiledb_attr("dat", type = "DATETIME_NS")
  sch <- tiledb_array_schema(dom, val, sparse = TRUE)
  tiledb_array_create(uri, sch)

  arr <- tiledb_sparse(uri)
  arr[1:60] <- datetimes

  arr2 <- tiledb_sparse(uri)
  expect_equal(datetimes, arr2[]$dat)

  unlink(uri, recursive=TRUE)
}

#})
