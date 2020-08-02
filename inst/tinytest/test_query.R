library(tinytest)
library(tiledb)

tiledb_ctx(limitTileDBCores())

.createArray <- function(tmp) {
  dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L,4L), 4L, "INT32"),
                                tiledb_dim("d2", c(1L,4L), 4L, "INT32")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)
  arr <- tiledb_array(uri = tmp)
}

#test_that("tiledb_query constructor", {
tmp <- tempfile()
dir.create(tmp)
arr <- .createArray(tmp)

query <- tiledb_query(arr)
expect_true(is(query, "tiledb_query"))

expect_error(tiledb_query(arr, "NotReadOrWrite"))
unlink(tmp, recursive=TRUE)
#})

#test_that("tiledb_query type", {
tmp <- tempfile()
dir.create(tmp)
arr <- .createArray(tmp)

query <- tiledb_query(arr)
expect_equal(tiledb_query_type(query), "READ")

warr <- tiledb_array(uri = tmp)
wquery <- tiledb_query(warr, "WRITE")
expect_true(is(wquery, "tiledb_query"))
expect_equal(tiledb_query_type(wquery), "WRITE")

unlink(tmp, recursive=TRUE)
#})

#test_that("tiledb_query layout", {
tmp <- tempfile()
dir.create(tmp)
arr <- .createArray(tmp)

query <- tiledb_query(arr)
expect_equal(tiledb_query_get_layout(tiledb_query_set_layout(query, "ROW_MAJOR")), "ROW_MAJOR")
expect_equal(tiledb_query_get_layout(tiledb_query_set_layout(query, "COL_MAJOR")), "COL_MAJOR")
expect_equal(tiledb_query_get_layout(tiledb_query_set_layout(query, "GLOBAL_ORDER")), "GLOBAL_ORDER")
expect_equal(tiledb_query_get_layout(tiledb_query_set_layout(query, "UNORDERED")), "UNORDERED")

unlink(tmp, recursive=TRUE)
#})

if (tiledb_version(TRUE) < "2.0.0") exit_file("Remaining tests require TileDB 2.0.* or later")

#test_that("tiledb_query basic query", {
tmp <- tempfile()
dir.create(tmp)
arr <- .createArray(tmp)
qry <- tiledb_query(arr, "WRITE")

d1 <- 1:4
d2 <- 1:4
a <- 101:104
dat <- data.frame(d1=d1,d2=d2,a=a)
arr[] <- dat

rarr <- tiledb_array(uri = tmp)
qry <- tiledb_query(rarr, "READ")

tiledb_query_set_buffer(qry, "a", a)
tiledb_query_set_buffer(qry, "d1", d1)
tiledb_query_set_buffer(qry, "d2", d2)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
expect_equal(tiledb_query_status(qry), "COMPLETE")

expect_equal(dat$a, a)
expect_equal(dat$d1, d1)
expect_equal(dat$d2, d2)

unlink(tmp, recursive=TRUE)
#})

#test_that("tiledb_query alloc and range", {
if (requireNamespace("nanotime", quietly=TRUE)) {
  suppressMessages({
    library(bit64)
    library(nanotime)
  })

  tmp <- tempfile()
  dir.create(tmp)

  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(0, 1e12), 1, type = "DATETIME_NS")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
                                               tiledb_attr("d1", type = "DATETIME_US")),
                                sparse=TRUE)
  tiledb_array_create(tmp, schema)

  arr <- tiledb_array(tmp)
  qry <- tiledb_query(arr, "WRITE")
  qry <- tiledb_query_set_layout(qry, "UNORDERED")

  rows <- nanotime(1) + 0:9
  rowptr <- tiledb_query_create_buffer_ptr(qry, "DATETIME_NS", rows)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowptr)

  a1data <- seq(1,10)
  a1ptr <- tiledb_query_create_buffer_ptr(qry, "INT32", a1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "a1", a1ptr)

  d1data <- as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123456
  d1ptr <- tiledb_query_create_buffer_ptr(qry, "DATETIME_US", d1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", d1ptr)

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  expect_equal(tiledb_query_status(qry), "COMPLETE")


  rarr <- tiledb_array(tmp)
  qry <- tiledb_query(rarr, "READ")

  rowptr <- tiledb_query_buffer_alloc_ptr(qry, "DATETIME_NS", 6)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowptr)

  a1ptr <- tiledb_query_buffer_alloc_ptr(qry, "INT32", 6)
  qry <- tiledb_query_set_buffer_ptr(qry, "a1", a1ptr)

  d1ptr <- tiledb_query_buffer_alloc_ptr(qry, "DATETIME_US", 6)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", d1ptr)

  qry <- tiledb_query_add_range(qry, schema(arr), "rows", as.integer64(4), as.integer64(7))

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  n <- tiledb_query_result_buffer_elements(qry, "rows")

  dat <- data.frame(rows=tiledb_query_get_buffer_ptr(rowptr),
                    a1=tiledb_query_get_buffer_ptr(a1ptr),
                    d1=tiledb_query_get_buffer_ptr(d1ptr))[1:n,]

  expect_equal(dat$rows, rows[4:7])
  expect_equal(dat$a1, a1data[4:7])
  expect_equal(dat$d1, d1data[4:7])
}

#test_that("tiledb_query subarray", {
tmp <- tempfile()
dir.create(tmp)

dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 10L), 1L, type = "INT32")))
schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("vals", type = "INT32")), sparse=FALSE)
tiledb_array_create(tmp, schema)

arr <- tiledb_array(tmp)
qry <- tiledb_query(arr, "WRITE")
qry <- tiledb_query_set_layout(qry, "ROW_MAJOR")

rows <- 1:10
qry <- tiledb_query_set_buffer(qry, "rows", rows)

vals <- seq(101,110)
qry <- tiledb_query_set_buffer(qry, "vals", vals)

tiledb_query_submit(qry)
tiledb_query_finalize(qry)
expect_equal(tiledb_query_status(qry), "COMPLETE")
tiledb_array_close(arr)

arr <- tiledb_array(tmp)
qry <- tiledb_query(arr, "READ")

rowdat <- integer(10)
qry <- tiledb_query_set_buffer(qry, "rows", rowdat)
valdat <- integer(10)
qry <- tiledb_query_set_buffer(qry, "vals", valdat)

qry <- tiledb_query_set_subarray(qry, c(4L,7L))
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
expect_equal(tiledb_query_status(qry), "COMPLETE")

n <- tiledb_query_result_buffer_elements(qry, "rows")
expect_equal(n, 4L)
expect_equal(rowdat[1:n], rows[4:7])
expect_equal(valdat[1:n], vals[4:7])
#})
