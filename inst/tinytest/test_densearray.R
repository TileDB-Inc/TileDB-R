library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

unlink_and_create <- function(tmp) {
  if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE, force = TRUE)
    dir.create(tmp)
  } else {
    dir.create(tmp)
  }
  return(tmp)
}

# test_that("1D Domain subarray subscripting works", {
#   ctx <- tiledb_ctx()
#   dim1 <- tiledb_dim(ctx, domain = c(1L, 10L))
#
#   expect_equal(tiledb::subset_dense_subarray(dom, 1), list(c(1, 1)))
#   expect_equal(tiledb::subset_dense_subarray(dom, 8), list(c(8, 8)))
#   expect_equal(tiledb::subset_dense_subarray(dom, 10), list(c(10, 10)))
#
#   expect_equal(tiledb::subset_dense_subarray(dom, c(1, 5, 10)), list(c(1, 1, 5, 5, 10, 10)))
#   expect_equal(tiledb::subset_dense_subarray(dom, c(5, 1, 10)), list(c(5, 5, 1, 1, 10, 10)))
#   expect_equal(tiledb::subset_dense_subarray(dom, c(1:3, 5:7, 10)), list(c(1, 3, 5, 7, 10, 10)))
#   expect_equal(tiledb::subset_dense_subarray(dom, c(5:2)), list(c(5, 5, 4, 4, 3, 3, 2, 2)))
#   expect_equal(tiledb::subset_dense_subarray(dom, 1:10), list(c(1, 10)))
# })
#
# test_that("2D Domain subarray subscripting works", {
#   ctx <- tiledb_ctx()
#   dim1 <- tiledb_dim(ctx, domain = c(1L, 10L))
#   dim2 <- tiledb_dim(ctx, domain = c(1L, 10L))
#   dom <- tiledb_domain(ctx, c(dim1, dim2))
#
#   expect_equal(tiledb::subset_dense_subarray(dom, 1, 1), list(c(1, 1), c(1, 1)))
# })
#
# test_that("3D Domain subarray subscripting works", {
#   ctx <- tiledb_ctx()
#   dim1 <- tiledb_dim(ctx, domain = c(1L, 10L))
#   dim2 <- tiledb_dim(ctx, domain = c(1L, 10L))
#   dim3 <- tiledb_dim(ctx, domain = c(1L, 10L))
#   dom <- tiledb_domain(ctx, c(dim1, dim2, dim3))
#
#   expect_equal(tiledb::subset_dense_subarray(dom, 1, 1, 1,  , 1), list(c(1, 1), c(1, 10), c(1, 1)))
# })

#test_that("Can read / write a simple 1D vector", {
tmp <- tempfile()
unlink_and_create(tmp)

dim <- tiledb_dim("dim", domain = c(1L, 10L))
dom <- tiledb_domain(c(dim))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val))
tiledb_array_create(tmp, sch)

arr <- tiledb_dense(tmp, as.data.frame=FALSE)
dat <- as.array(as.double(1:10))
arr[] <- dat

arr <- tiledb_dense(tmp, as.data.frame=FALSE)
expect_equal(arr[], dat)

## explicit range enumeration
expect_equal(arr[c(3,4,5,6,7)], dat[c(3,4,5,6,7)])

## vector range syntax
expect_equal(arr[3:7], dat[3:7])

## vector range syntax (reversed)
## TODO: find a way to efficiently do this
## expect_equal(arr[7:3], dat[7:3])

## scalar indexing
expect_equal(arr[8], dat[8])

arr[6] <- 1000
expect_equal(arr[6], 1000)

arr[7:10] <- c(97, 98, 99, 100)
expect_equal(arr[6:10], as.array(c(1000, 97, 98, 99, 100)))

unlink(tmp, recursive = TRUE)
#})

#test_that("Can read / write a simple 2D matrix", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 5L))
d2  <- tiledb_dim("d2", domain = c(1L, 5L))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val))
tiledb_array_create(tmp, sch)

dat <- matrix(rnorm(25), 5, 5)
arr <- tiledb_dense(tmp, as.data.frame=FALSE)

arr[] <- dat
expect_equal(arr[], dat)

## explicit range enumeration
expect_equal(arr[c(3,4,5), c(3,4,5)],
             dat[c(3,4,5), c(3,4,5)])

## vector range syntax
expect_equal(arr[1:3, 1:3], dat[1:3, 1:3])

## missing index range
expect_equal(arr[1:3,], dat[1:3,])

## scalar indexing
expect_equal(arr[3, 3], dat[3, 3])

unlink(tmp, recursive = TRUE)
#})

#test_that("Can read / write a simple 3D matrix", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 5L))
d2  <- tiledb_dim("d2", domain = c(1L, 5L))
d3  <- tiledb_dim("d3", domain = c(1L, 5L))
dom <- tiledb_domain(c(d1, d2, d3))
val <- tiledb_attr(name="val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val))
tiledb_array_create(tmp, sch)

dat <- array(rnorm(125), dim = c(5, 5, 5))
arr <- tiledb_dense(tmp, as.data.frame=FALSE)

arr[] <- dat
expect_equal(arr[], dat)

## explicit range enumeration
expect_equal(arr[c(3, 4, 5), c(3, 4, 5), c(1, 2)],
             dat[c(3, 4, 5), c(3, 4, 5), c(1, 2)])

## vector range syntax
expect_equal(arr[1:3, 1:3, 1:2], dat[1:3, 1:3, 1:2])

## scalar indexing
expect_equal(arr[3, 3, 3], dat[3, 3, 3])

unlink(tmp, recursive = TRUE)
#})

#test_that("Can read / write 1D multi-attribute array", {
tmp <- tempfile()
unlink_and_create(tmp)

dim <- tiledb_dim("dim", domain = c(1L, 10L))
dom <- tiledb_domain(c(dim))
a1  <- tiledb_attr("a1", type = "FLOAT64")
a2  <- tiledb_attr("a2", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(a1, a2))
tiledb_array_create(tmp, sch)

arr <- tiledb_dense(tmp, as.data.frame=FALSE)

a1_dat <- as.array(as.double(1:10))
a2_dat <- as.array(as.double(11:20))

dat <- list(a1 = a1_dat,
            a2 = a2_dat)
arr[] <- dat

expect_equal(arr[], dat)
expect_equal(names(arr[]), names(dat))
expect_equal(arr[1:10], dat)

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can read / write 2D multi-attribute array", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 10L))
d2  <- tiledb_dim("d2", domain = c(1L, 10L))
dom <- tiledb_domain(c(d1, d2))
a1  <- tiledb_attr("a1", type = "FLOAT64")
a2  <- tiledb_attr("a2", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(a1, a2))
tiledb_array_create(tmp, sch)

arr <- tiledb_dense(tmp, as.data.frame=FALSE)

a1_dat <- array(rnorm(100), dim = c(10, 10))
a2_dat <- array(rnorm(100), dim = c(10, 10))

dat <- list(a1 = a1_dat, a2 = a2_dat)
arr[] <- dat

expect_equal(arr[], dat)
expect_equal(names(arr[]), names(dat))
expect_equal(arr[1:10, 1:10], dat)
expect_equal(arr[2, 2][["a2"]], dat[["a2"]][2, 2])
expect_equal(arr[1:5,][["a1"]], dat[["a1"]][1:5,])
expect_equal(arr[,1:3][["a2"]], dat[["a2"]][,1:3])

arr[1:3, 1:3] <- list(a1 = array(1.0, c(3, 3)), a2 = array(2.0, c(3, 3)))
expect_true(all(arr[1:3, 1:3][["a1"]] == 1.0))
expect_true(all(arr[1:3, 1:3][["a2"]] == 2.0))

dat[["a1"]][1:3, 1:3] <- array(1.0, c(3, 3))
expect_equal(arr[][["a1"]], dat[["a1"]][])

unlink(tmp, recursive = TRUE)
#})

#test_that("as.array() conversion method", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 10L))
dom <- tiledb_domain(c(d1))
a1  <- tiledb_attr("a1", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(a1))
tiledb_array_create(tmp, sch)

arr <- tiledb_dense(tmp, as.data.frame=FALSE)
dat <- as.double(1:10)
arr[] <- dat
expect_equal(as.array(arr), as.array(dat))

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("as.data.frame() conversion method", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 10L))
dom <- tiledb_domain(c(d1))
a1  <- tiledb_attr("a1", type = "FLOAT64")
a2  <- tiledb_attr("a2", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(a1, a2))
tiledb_array_create(tmp, sch)

arr <- tiledb_dense(tmp, as.data.frame=FALSE)

dat <- list(a1 = array(as.double(1:10)),
            a2 = array(as.double(1:10)))
arr[] <- dat

expect_equal(as.data.frame(arr),
             as.data.frame(dat))

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("test tiledb_subarray read for dense array", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 5L))
d2  <- tiledb_dim("d2", domain = c(1L, 5L))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr(name="val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val))
tiledb_array_create(tmp, sch)

dat <- matrix(rnorm(25), 5, 5)
arr <- tiledb_dense(tmp, as.data.frame=FALSE)

arr[] <- dat
expect_equal(arr[], dat)

## explicit range enumeration
expect_equal(tiledb_subarray(arr, list(3,5, 3,5)),
             dat[c(3,4,5), c(3,4,5)])

## vector range syntax
expect_equal(tiledb_subarray(arr, list(1,3,1,3)), dat[1:3, 1:3])

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("test tiledb_subarray read for dense array with select attributes", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 5L))
d2  <- tiledb_dim("d2", domain = c(1L, 5L))
dom <- tiledb_domain(c(d1, d2))
val1 <- tiledb_attr("val1", type = "FLOAT64")
val2 <- tiledb_attr("val2", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val1, val2))
tiledb_array_create(tmp, sch)

dat1 <- matrix(rnorm(25), 5, 5)
dat2 <- matrix(rnorm(25), 5, 5)
arr <- tiledb_dense(tmp, as.data.frame=FALSE)

arr[] <- list(val1=dat1, val2=dat2)
expect_equal(arr[]$val1, dat1)
expect_equal(arr[]$val2, dat2)

## explicit range enumeration
expect_equal(tiledb_subarray(arr, list(3,5, 3,5), attrs=c("val1")),
             dat1[c(3,4,5), c(3,4,5)])

## vector range syntax
expect_equal(tiledb_subarray(arr, list(1,3,1,3), attrs=c("val2")), dat2[1:3, 1:3])

unlink(tmp, recursive = TRUE, force = TRUE)
#})


#test_that("test tiledb_subarray read for dense array as dataframe", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 5L))
d2  <- tiledb_dim("d2", domain = c(1L, 5L))
dom <- tiledb_domain(c(d1, d2))
val1 <- tiledb_attr("val1", type = "FLOAT64")
val2 <- tiledb_attr("val2", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val1, val2))
tiledb_array_create(tmp, sch)

dat1 <- matrix(rnorm(25), 5, 5)
dat2 <- matrix(rnorm(25), 5, 5)
arr <- tiledb_dense(tmp, as.data.frame=TRUE)

arr[] <- list(val1=dat1, val2=dat2)
expect_equal(arr[]$val1, unlist(as.list(dat1)))
expect_equal(arr[]$val2, unlist(as.list(dat2)))

## explicit range enumeration
expect_equal(tiledb_subarray(arr, list(3,5, 3,5), attrs=c("val1"))$val1,
             unlist(as.list(dat1[c(3,4,5), c(3,4,5)])))

## vector range syntax
expect_equal(tiledb_subarray(arr, list(1,3,1,3), attrs=c("val2"))$val2, unlist(as.list(dat2[1:3, 1:3])))

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can read / write a simple 2D matrix with list of coordinates", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 5L))
d2  <- tiledb_dim("d2", domain = c(1L, 5L))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val))
tiledb_array_create(tmp, sch)

dat <- matrix(rnorm(25), 5, 5)
arr <- tiledb_dense(tmp, as.data.frame=FALSE)

arr[] <- dat
expect_equal(arr[], dat)

## explicit range enumeration
expect_equal(arr[list(c(3,4,5), c(3,4,5))],
             dat[c(3,4,5), c(3,4,5)])

## vector range syntax
expect_equal(arr[list(c(1:3), c(1:3))], dat[1:3, 1:3])

## scalar indexing
expect_equal(arr[list(c(3), c(3))], dat[3, 3])

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("treating logical as INT32 works", {
tmp <- tempfile()
unlink_and_create(tmp)
#  })

dat <- matrix(rnorm(25) > 0, 5, 5)

d1  <- tiledb_dim("d1", domain = c(1L, 5L))
d2  <- tiledb_dim("d2", domain = c(1L, 5L))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = r_to_tiledb_type(dat))
sch <- tiledb_array_schema(dom, c(val))
tiledb_array_create(tmp, sch)

arr <- tiledb_dense(tmp, as.data.frame=FALSE)

arr[] <- dat
int_dat = dat
storage.mode(int_dat) = "integer"
expect_equal(arr[], int_dat)
#})


#test_that("low-level write and read works", {
tmp <- tempfile()
unlink_and_create(tmp)

## data: simple (integer sequence) of 1:16 times 10
vec <- 1:16 * 10L

d1  <- tiledb_dim("d1", domain = c(1L, 4L))
d2  <- tiledb_dim("d2", domain = c(1L, 4L))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("a", type = r_to_tiledb_type(vec))
sch <- tiledb_array_schema(dom, c(val))
tiledb_array_create(tmp, sch)

ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, tmp, "WRITE")

subarr <- c(1L,4L, 1L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", vec)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
res <- tiledb:::libtiledb_array_close(arrptr)


arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, tmp, "READ")
## subarray of rows 1,2 and cols 2,3,4
subarr <- c(1L,2L, 2L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
v <- integer(6)
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", v)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
##print(v)         # unformed array, no coordinates
expect_equal(v, c(20L, 30L, 40L, 60L, 70L, 80L))
res <- tiledb:::libtiledb_array_close(arrptr)


arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, tmp, "READ")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "COL_MAJOR")
v <- integer(6)
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", v)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
##print(v)         # unformed array, no coordinates
expect_equal(v, c(20L, 60L, 30L, 70L, 40L, 80L))
res <- tiledb:::libtiledb_array_close(arrptr)

#})


#test_that("low-level encrypted array write and read works", {
tmp <- tempfile()
unlink_and_create(tmp)

## data: simple (integer sequence) of 1:16 times 10
vec <- 1:16 * 10L

encryption_key <- "0123456789abcdeF0123456789abcdeF"

d1  <- tiledb_dim("d1", domain = c(1L, 4L))
d2  <- tiledb_dim("d2", domain = c(1L, 4L))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("a", type = r_to_tiledb_type(vec))
sch <- tiledb_array_schema(dom, c(val))
tiledb:::libtiledb_array_create_with_key(tmp, sch@ptr, encryption_key)

ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open_with_key(ctx@ptr, tmp, "WRITE", encryption_key)

subarr <- c(1L,4L, 1L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", vec)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
res <- tiledb:::libtiledb_array_close(arrptr)


arrptr <- tiledb:::libtiledb_array_open_with_key(ctx@ptr, tmp, "READ", encryption_key)
## subarray of rows 1,2 and cols 2,3,4
subarr <- c(1L,2L, 2L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
v <- integer(6)
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", v)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
##print(v)         # unformed array, no coordinates
expect_equal(v, c(20L, 30L, 40L, 60L, 70L, 80L))
res <- tiledb:::libtiledb_array_close(arrptr)


arrptr <- tiledb:::libtiledb_array_open_with_key(ctx@ptr, tmp, "READ", encryption_key)
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "COL_MAJOR")
v <- integer(6)
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", v)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
##print(v)         # unformed array, no coordinates
expect_equal(v, c(20L, 60L, 30L, 70L, 40L, 80L))
res <- tiledb:::libtiledb_array_close(arrptr)
#})

#test_that("low-level fixed-length write and read works", {
tmp <- tempfile()
unlink_and_create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 4L))
d2  <- tiledb_dim("d2", domain = c(1L, 4L))
dom <- tiledb_domain(c(d1, d2))

vec <- 1:32 * 10L
attr <- tiledb_attr("a", type = r_to_tiledb_type(vec))

## set to two values per cell
tiledb:::libtiledb_attribute_set_cell_val_num(attr@ptr, 2)

sch <- tiledb_array_schema(dom, c(attr))

tiledb_array_create(tmp, sch)

ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, tmp, "WRITE")

subarr <- c(1L,4L, 1L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", vec)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
res <- tiledb:::libtiledb_array_close(arrptr)

## written

arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, tmp, "READ")
## subarray of rows 1,2 and cols 2,3,4
subarr <- c(1L,2L, 2L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
v <- integer(12) ## == (2 x 3) x 2
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", v)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
##print(v)         # unformed array, no coordinates

expect_equal(v, c(30L, 40L, 50L, 60L, 70L, 80L, 110L, 120L, 130L, 140L, 150L, 160L))
res <- tiledb:::libtiledb_array_close(arrptr)
#})

#test_that("low-level variable-length character array write and read works", {
array_name <- tempfile()
unlink_and_create(array_name)

## Define array
## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

attr <- tiledb_attr("a1", type = "CHAR")
## set to variable length
tiledb:::libtiledb_attribute_set_cell_val_num(attr@ptr, NA)

## now set the schema
ctx <- tiledb_ctx()
schptr <- tiledb:::libtiledb_array_schema_create(ctx@ptr, "DENSE")
tiledb:::libtiledb_array_schema_set_domain(schptr, dom@ptr)
tiledb:::libtiledb_array_schema_set_cell_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_set_tile_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_add_attribute(schptr, attr@ptr)

## Create the (empty) array on disk.
tiledb:::libtiledb_array_create(array_name, schptr)

data <- "abbcccddeeefghhhijjjkklmnoop";
offsets <- c(0L, 1L, 3L, 6L, 8L, 11L, 12L, 13L, 16L, 17L, 20L, 22L, 23L, 24L, 25L, 27L)

ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

bufptr <- tiledb:::libtiledb_query_buffer_var_char_create(offsets, data)
qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

## Read and test
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")

subarr <- c(1L,4L, 1L,4L)
bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc(arrptr, subarr, "a1")

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

mat <- tiledb:::libtiledb_query_get_buffer_var_char(bufptr)
expect_equal(mat,  matrix(c("a",   "eee",  "i",    "m",
                            "bb",  "f",    "jjj",  "n",
                            "ccc", "g",    "kk",   "oo",
                            "dd",  "hhh",  "l",    "p"), 4, 4, byrow=TRUE))

## overwrite subarray
data <- "KLLLMMN";
offsets <- c(0L, 1L, 4L, 6L)

subarr <- c(2L,3L, 2L,3L)

ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

bufptr <- tiledb:::libtiledb_query_buffer_var_char_create(offsets, data)
qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)

qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

## Read and test
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")

subarr <- c(1L,4L, 1L,4L)
bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc(arrptr, subarr, "a1")

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

mat <- tiledb:::libtiledb_query_get_buffer_var_char(bufptr)
expect_equal(mat,  matrix(c("a",   "eee",  "i",    "m",
                            "bb",  "K",    "MM",   "n",
                            "ccc", "LLL",  "N",    "oo",
                            "dd",  "hhh",  "l",    "p"), 4, 4, byrow=TRUE))

## Read and test subarry
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")

subarr <- c(2L,3L, 2L,3L)
bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc(arrptr, subarr, "a1")

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
tiledb:::libtiledb_array_close(arrptr)

mat <- tiledb:::libtiledb_query_get_buffer_var_char(bufptr)
expect_equal(mat,  matrix(c("K",    "MM",
                            "LLL",  "N"), 2, 2, byrow=TRUE))

## Read and test allocation sizes
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")

subarr <- c(1L,4L, 1L,4L)
bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc(arrptr, subarr, "a1", 16, 100)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

mat <- tiledb:::libtiledb_query_get_buffer_var_char(bufptr)
expect_equal(mat,  matrix(c("a",   "eee",  "i",    "m",
                            "bb",  "K",    "MM",   "n",
                            "ccc", "LLL",  "N",    "oo",
                            "dd",  "hhh",  "l",    "p"), 4, 4, byrow=TRUE))

## Read and test allocation sizes failure with too little size
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
subarr <- c(1L,4L, 1L,4L)
bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc(arrptr, subarr, "a1", 8)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "INCOMPLETE")
tiledb:::libtiledb_array_close(arrptr)

arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
subarr <- c(1L,4L, 1L,4L)
bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc(arrptr, subarr, "a1", 16, 10)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "a1", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "INCOMPLETE")
tiledb:::libtiledb_array_close(arrptr)
#})


#test_that("low-level variable-length int32 array write and read works", {
array_name <- tempfile()
unlink_and_create(array_name)

## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))


attr <- tiledb_attr("a", type = "INT32")
## set to variable length
tiledb:::libtiledb_attribute_set_cell_val_num(attr@ptr, NA)

## now set the schema
ctx <- tiledb_ctx()
schptr <- tiledb:::libtiledb_array_schema_create(ctx@ptr, "DENSE")
tiledb:::libtiledb_array_schema_set_domain(schptr, dom@ptr)
tiledb:::libtiledb_array_schema_set_cell_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_set_tile_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_add_attribute(schptr, attr@ptr)

## Create the (empty) array on disk.
tiledb:::libtiledb_array_create(array_name, schptr)


## write the data
data <- c(1L, 1L, 2L, 2L, 3L, 4L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 10L,
          11L, 12L, 12L, 13L, 14L, 14L, 14L, 15L, 16L)
offsets <- c(0L, 2L, 4L, 5L, 6L, 7L, 9L, 11L, 14L, 16L, 17L, 18L, 20L, 21L, 24L, 25L)
offsets <- offsets * 4 # known fixed size of integer


##ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

bufptr <- tiledb:::libtiledb_query_buffer_var_vec_create(offsets, data)
qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

## read and test
##ctx <- tiledb_ctx()
subarr <- NULL
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
if (is.null(subarr)) {
  schptr <- tiledb:::libtiledb_array_get_schema(arrptr)
  domptr <- tiledb:::libtiledb_array_schema_get_domain(schptr)
  lst <- tiledb:::libtiledb_domain_get_dimensions(domptr)
  subarr <- c(tiledb:::libtiledb_dim_get_domain(lst[[1]]),
              tiledb:::libtiledb_dim_get_domain(lst[[2]]))
}
bufptr <- tiledb:::libtiledb_query_buffer_var_vec_alloc(arrptr, subarr, "a")

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

rl <- tiledb:::libtiledb_query_get_buffer_var_vec(qryptr, "a", bufptr)
expect_equal(rl[[1]], offsets)
expect_equal(rl[[2]], data)


## write subset
data <- c(11L, 11L, 22L, 22L, 33L, 44L)
offsets <- c(0L, 2L, 4L, 5L)
offsets <- offsets * 4 # known fixed size of integer

subarr <- c(2L,3L, 2L,3L)

##ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

bufptr <- tiledb:::libtiledb_query_buffer_var_vec_create(offsets, data)
qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)


## read and test again
##ctx <- tiledb_ctx()
subarr <- c(2L,3L, 2L,3L)
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
if (is.null(subarr)) {
  schptr <- tiledb:::libtiledb_array_get_schema(arrptr)
  domptr <- tiledb:::libtiledb_array_schema_get_domain(schptr)
  lst <- tiledb:::libtiledb_domain_get_dimensions(domptr)
  subarr <- c(tiledb:::libtiledb_dim_get_domain(lst[[1]]),
              tiledb:::libtiledb_dim_get_domain(lst[[2]]))
}
bufptr <- tiledb:::libtiledb_query_buffer_var_vec_alloc(arrptr, subarr, "a")

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

rl <- tiledb:::libtiledb_query_get_buffer_var_vec(qryptr, "a", bufptr)
expect_equal(rl[[1]], offsets)
expect_equal(rl[[2]], data)

#})

#test_that("low-level variable-length double array write and read works", {
array_name <- tempfile()
unlink_and_create(array_name)

## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))


attr <- tiledb_attr("a", type = "FLOAT64")
## set to variable length
tiledb:::libtiledb_attribute_set_cell_val_num(attr@ptr, NA)

## now set the schema
ctx <- tiledb_ctx()
schptr <- tiledb:::libtiledb_array_schema_create(ctx@ptr, "DENSE")
tiledb:::libtiledb_array_schema_set_domain(schptr, dom@ptr)
tiledb:::libtiledb_array_schema_set_cell_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_set_tile_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_add_attribute(schptr, attr@ptr)

## Create the (empty) array on disk.
tiledb:::libtiledb_array_create(array_name, schptr)


## write the data
data <- c(1.1, 1.1, 2.2, 2.2, 3.3, 4.4, 5.5, 6.6, 6.6, 7.7, 7.7, 8.8, 8.8, 8.8, 9.9, 9.0, 10.0,
          11.1, 12.2, 12.2, 13.3, 14.4, 14.4, 14.4, 15.5, 16.6)
offsets <- c(0L, 2L, 4L, 5L, 6L, 7L, 9L, 11L, 14L, 16L, 17L, 18L, 20L, 21L, 24L, 25L)
offsets <- offsets * 8 # known and fixed size of double

##ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

bufptr <- tiledb:::libtiledb_query_buffer_var_vec_create(offsets, data)
qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)


## read and test
##ctx <- tiledb_ctx()
subarr <- NULL
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
if (is.null(subarr)) {
  schptr <- tiledb:::libtiledb_array_get_schema(arrptr)
  domptr <- tiledb:::libtiledb_array_schema_get_domain(schptr)
  lst <- tiledb:::libtiledb_domain_get_dimensions(domptr)
  subarr <- c(tiledb:::libtiledb_dim_get_domain(lst[[1]]),
              tiledb:::libtiledb_dim_get_domain(lst[[2]]))
}
bufptr <- tiledb:::libtiledb_query_buffer_var_vec_alloc(arrptr, subarr, "a")

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

rl <- tiledb:::libtiledb_query_get_buffer_var_vec(qryptr, "a", bufptr)
expect_equal(rl[[1]], offsets)
expect_equal(rl[[2]], data)


## write subset
data <- c(11.1, 11.1, 22.2, 22.2, 33.3, 44.4)
offsets <- c(0L, 2L, 4L, 5L)
offsets <- offsets * 4 # known fixed size of integer

subarr <- c(2L,3L, 2L,3L)

##ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

bufptr <- tiledb:::libtiledb_query_buffer_var_vec_create(offsets, data)
qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)


## read and test again
##ctx <- tiledb_ctx()
subarr <- c(2L,3L, 2L,3L)
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
if (is.null(subarr)) {
  schptr <- tiledb:::libtiledb_array_get_schema(arrptr)
  domptr <- tiledb:::libtiledb_array_schema_get_domain(schptr)
  lst <- tiledb:::libtiledb_domain_get_dimensions(domptr)
  subarr <- c(tiledb:::libtiledb_dim_get_domain(lst[[1]]),
              tiledb:::libtiledb_dim_get_domain(lst[[2]]))
}
bufptr <- tiledb:::libtiledb_query_buffer_var_vec_alloc(arrptr, subarr, "a")

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

rl <- tiledb:::libtiledb_query_get_buffer_var_vec(qryptr, "a", bufptr)
expect_equal(rl[[1]], offsets)
expect_equal(rl[[2]], data)

#})


#test_that("low-level multi-range subarray read works", {
array_name <- tempfile()
unlink_and_create(array_name)

## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

## one attribute, set schema, create array
attr <- tiledb_attr("a", type = "INT32")
ctx <- tiledb_ctx()
schptr <- tiledb:::libtiledb_array_schema_create(ctx@ptr, "DENSE")
tiledb:::libtiledb_array_schema_set_domain(schptr, dom@ptr)
tiledb:::libtiledb_array_schema_set_cell_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_set_tile_order(schptr, "ROW_MAJOR")
tiledb:::libtiledb_array_schema_add_attribute(schptr, attr@ptr)
tiledb:::libtiledb_array_create(array_name, schptr)

data <- 1:16
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", data)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)

## ## read and test
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")

## range of rows 1 and 2, and 4 for dim 1, all rows for dim 2
qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 0, 1L, 2L)
qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 0, 4L, 4L)
qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 1, 1L, 4L)

v <- integer(12)
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", v)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
expect_equal(tiledb:::libtiledb_query_status(qryptr), "COMPLETE")
tiledb:::libtiledb_array_close(arrptr)
expect_equal(data[c(1:8,13:16)], v)
#})
