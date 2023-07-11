library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

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

  qry <- tiledb_query_add_range(qry, tiledb::schema(arr), "rows", as.integer64(4), as.integer64(7))

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

dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, 10L), 1L, type = "INT32"))
schema <- tiledb_array_schema(dom,
                              attrs = c(tiledb_attr("vals", type = "INT32"),
                                        tiledb_attr("keys", type = "INT32", nullable = TRUE)),
                              sparse=TRUE)
tiledb_array_create(tmp, schema)
arr <- tiledb_array(tmp)
qry <- tiledb_query(arr, "WRITE")
if (tiledb_version(TRUE) < "2.12.0") qry <- tiledb_query_set_layout(qry, "ROW_MAJOR")

rows <- 1:10
qry <- tiledb_query_set_buffer(qry, "rows", rows)

vals <- seq(101,110)
qry <- tiledb_query_set_buffer(qry, "vals", vals)

keys <- c(201:204, NA_integer_, 206L, NA_integer_, 208:210)
buf <- tiledb:::libtiledb_query_buffer_alloc_ptr("INT32", 10, TRUE, 1)
buf <- tiledb:::libtiledb_query_buffer_assign_ptr(buf, "INT32", keys, FALSE)
qry@ptr <- tiledb:::libtiledb_query_set_buffer_ptr(qry@ptr, "keys", buf)

tiledb_query_set_layout(qry, "UNORDERED")
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

buf <- tiledb:::libtiledb_query_buffer_alloc_ptr("INT32", 10, TRUE, 1)
buf <- tiledb:::libtiledb_query_buffer_assign_ptr(buf, "INT32", keys, FALSE)
qry@ptr <- tiledb:::libtiledb_query_set_buffer_ptr(qry@ptr, "keys", buf)

qry <- tiledb_query_set_subarray(qry, c(4L,7L))
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
expect_equal(tiledb_query_status(qry), "COMPLETE")

keydat <- tiledb:::libtiledb_query_get_buffer_ptr(buf, FALSE)

if (tiledb_version(TRUE) < "2.7.0") exit_file("Needs TileDB 2.7.* or later")

n <- tiledb_query_result_buffer_elements(qry, "rows")
expect_equal(n, 4L)
expect_equal(rowdat[1:n], rows[4:7])
expect_equal(valdat[1:n], vals[4:7])
expect_equal(keydat[1:n], keys[4:7])
n2 <- tiledb:::libtiledb_query_result_buffer_elements(qry@ptr, "rows", 0)
expect_equal(n2, 0)                     # first element can be requested, is zero for fixed-sized

if (tiledb_version(TRUE) < "2.2.0") exit_file("Remaining tests require TileDB 2.2.* or later")

## not as streamlined as it could, may need a wrapper for schema-from-query
arrschptr <- tiledb:::libtiledb_query_get_schema(qry@ptr, tiledb_get_context()@ptr)
sch <- tiledb:::tiledb_array_schema.from_ptr(arrschptr)

attrs <- attrs(sch)
isnullable <- tiledb:::libtiledb_attribute_get_nullable( attrs[["vals"]]@ptr )

nv <- tiledb_query_result_buffer_elements_vec(qry, "vals", isnullable)
expect_equal(length(nv), 2)             # vector accessors have two elements (or three if nullable)
expect_equal(nv[1], 0)                  # first is zero for fixed-sized attributes
expect_equal(nv[2], n)                  # second is what tiledb_query_result_buffer_elements has

attrs <- attrs(sch)
isnullable <- tiledb:::libtiledb_attribute_get_nullable( attrs[["keys"]]@ptr )

nv <- tiledb_query_result_buffer_elements_vec(qry, "keys", isnullable)
expect_equal(length(nv), 3)             # vector accessors have two elements (or three if nullable)
expect_equal(nv[1], 0)                  # first is zero for fixed-sized attributes
expect_equal(nv[2], n)                  # second is what tiledb_query_result_buffer_elements has
expect_equal(nv[3], n)                  # third is length of validity buffer (if nullable)

#})

## check for warning in insufficient memory
oldcfg <- tiledb_config()
cfg <- tiledb_config()
cfg["sm.memory_budget"] <- "16"
cfg["sm.memory_budget_var"] <- "32"
ctx <- tiledb_ctx(cfg)
array <- tiledb_array(tmp, return_as="data.frame")

if (packageVersion("tiledb") <= "0.11.0") expect_warning(res <- array[]) # newer versions loop, no warning

## check for query stats
if (tiledb_version(TRUE) < "2.4.0") exit_file("TileDB Query + Ctx stats requires TileDB 2.4.* or greater")
res <- tiledb_query_stats(qry)
expect_true(is.character(res))
expect_true(nchar(res) > 1000)  		# safe lower boundary

res <- tiledb_ctx_stats()               # test here rather than in test_ctx to have real query
expect_true(is.character(res))
expect_true(nchar(res) > 1000)  		# safe lower boundary

ctx <- tiledb_ctx(oldcfg)               # reset config


## check deletes
if (tiledb_version(TRUE) < "2.12.0") exit_file("TileDB deletes requires TileDB 2.12.* or greater")
if (!requireNamespace("palmerpenguins", quietly=TRUE)) exit_file("remainder needs 'palmerpenguins'")
uri <- tempfile()
pp <- palmerpenguins::penguins
fromDataFrame(pp, uri, sparse = TRUE, col_index = c("species", "year"))

qc <- parse_query_condition(body_mass_g > 4000 || island == "Biscoe" || sex == "male")
arr <- tiledb_array(uri)
qry <- tiledb_query(arr, "DELETE")
qry <- tiledb_query_set_condition(qry, qc)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)

oo <- tiledb_array(uri, return_as="data.frame", strings_as_factors=TRUE)[]

expect_equal(nrow(oo), 84)             # instead of 344 pre-deletion


## for #537 #538: allocate char buffer and normal buffer with nullable
## quick data frame with NAs
sdf <- data.frame(rows=1:5,
                  keys=c("ABC", NA, "GHI", "JKL", "MNO"),
                  vals=c(NA,sqrt(2:3),NA,sqrt(5)))
uri <- tempfile()
fromDataFrame(sdf, uri, col_index=1)

arr <- tiledb_array(uri)
qry <- tiledb_query(arr, "READ")
N <- 10
rows <- integer(N)
keysbuf <- tiledb_query_alloc_buffer_ptr_char(N, N*8, TRUE)
valsbuf <- tiledb_query_buffer_alloc_ptr(qry, "FLOAT64", N, TRUE)

expect_silent(tiledb_query_set_buffer(qry, "rows", rows))
expect_silent(tiledb_query_set_buffer_ptr_char(qry, "keys", keysbuf))
expect_silent(tiledb_query_set_buffer_ptr(qry, "vals", valsbuf))
expect_silent(tiledb_query_set_subarray(qry, c(1L,5L), "INT32"))
expect_silent(tiledb_query_submit(qry))
expect_silent(tiledb_query_finalize(qry))
expect_equal(tiledb_query_status(qry), "COMPLETE")
