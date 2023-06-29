library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

tmp <- tempfile()

unlink_and_create_simple <- function(tmp) {
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  dir.create(tmp, recursive = TRUE)

  dim <- tiledb_dim("dim", domain = c(1L, 4L))
  dom <- tiledb_domain(c(dim))
  a1  <- tiledb_attr("a1", type = "INT32")
  a2  <- tiledb_attr("a2", type = "INT32")
  sch <- tiledb_array_schema(dom, c(a1, a2), sparse=TRUE)
  tiledb_array_create(tmp, sch)

  #arr <- tiledb_sparse(tmp, as.data.frame=FALSE)
  arr <- tiledb_array(tmp, as.data.frame=FALSE)

  if (tiledb:::libtiledb_array_is_open(arr@ptr)) {
      tiledb_array_close(arr)
  }
  tiledb_array_open(arr, "WRITE")

  ## write one record directly to (text) URI
  tiledb_put_metadata(arr, "vec", c(1.1, 2.2, 3.3))

  arr
}

unlink_and_create_ptr <- function(tmp) {
  arr <- unlink_and_create_simple(tmp)
  tiledb_array_close(arr)

  ## write one record via ptr
  arr <- tiledb_array_open(arr, "WRITE")
  tiledb_put_metadata(arr, "txt", "the quick brown fox")
  tiledb_array_close(arr)

  arr <- tiledb_array_open(arr, "READ")
  ##return(arrR)
  #arr <- tiledb_sparse(tmp, as.data.frame=FALSE)
  arr <- tiledb_array(tmp, as.data.frame=FALSE)
}

close_and_reopen <- function(arr, txt) {
  res <- tiledb_array_close(arr)
  res <- tiledb_array_open(arr, txt)
}

## infrastructure
#setup({
#  # empty
#})

#teardown({
#  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
#})


#test_that("Can check presence of metadata", {
arr <- unlink_and_create_ptr(tmp)

arr <- tiledb_array_open(arr, "READ")

expect_error(tiledb_has_metadata(NULL, ""))
expect_false(tiledb_has_metadata(arr, ""))
expect_true(tiledb_has_metadata(arr, "vec"))
expect_true(tiledb_has_metadata(arr, "txt"))

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can retrieve count of metadata", {
arr <- unlink_and_create_ptr(tmp)

arr <- tiledb_array_open(arr, "READ")

expect_error(tiledb_num_metadata(NULL))
expect_equal(tiledb_num_metadata(arr), 2)
unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can get metadata", {
arr <- unlink_and_create_ptr(tmp)

arr <- tiledb_array_open(arr, "READ")

expect_error(tiledb_get_metadata(NULL, ""))
expect_equal(tiledb_get_metadata(arr, ""), NULL)
expect_equal(tiledb_get_metadata(arr, "vec"), c(1.1, 2.2, 3.3))

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can put metadata", {
arr <- unlink_and_create_ptr(tmp)

if (tiledb:::libtiledb_array_is_open(arr@ptr)) tiledb_array_close(arr)
arr <- tiledb_array_open(arr, "WRITE")

expect_true(tiledb_put_metadata(arr, "foo", "the quick brown fox"))
expect_error(tiledb_put_metadata(arr, "foo", list(a=c(1,2,3), b=c("a", "b"))))

tiledb_array_close(arr)

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can do round trip", {
arr <- unlink_and_create_simple(tmp)

vec <- c(1.1, 2.2, 3.3)
expect_true(tiledb_put_metadata(arr, "dvec", vec))
close_and_reopen(arr, "READ")
expect_equal(tiledb_get_metadata(arr, "dvec"), vec)

vec <- c(1L, 2L, 3L)
close_and_reopen(arr, "WRITE")
expect_true(tiledb_put_metadata(arr, "ivec", vec))
close_and_reopen(arr, "READ")
expect_equal(tiledb_get_metadata(arr, "ivec"), vec)

vec <- "the quick brown fox"
close_and_reopen(arr, "WRITE")
expect_true(tiledb_put_metadata(arr, "char", vec))
close_and_reopen(arr, "READ")
expect_equal(tiledb_get_metadata(arr, "char"), vec)

vec <- c(TRUE, FALSE, TRUE)
close_and_reopen(arr, "WRITE")
expect_true(tiledb_put_metadata(arr, "lvec", vec))
close_and_reopen(arr, "READ")
expect_equal(tiledb_get_metadata(arr, "lvec"), vec)

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can get by index", {
arr <- unlink_and_create_ptr(tmp)

arr <- tiledb_array_open(arr, "READ")

expect_error(tiledb:::libtiledb_get_metadata_from_index(NULL, ""))
expect_error(tiledb:::libtiledb_get_metadata_from_index(arr@ptr, -1))
expect_equal(tiledb:::libtiledb_array_get_metadata_from_index(arr@ptr, 0),
             c(txt="the quick brown fox"))
expect_equal(unname(tiledb:::libtiledb_array_get_metadata_from_index(arr@ptr, 1)), c(1.1, 2.2, 3.3))

unlink(tmp, recursive = TRUE, force = TRUE)
#})

#test_that("Can get all", {
arr <- unlink_and_create_ptr(tmp)

arr <- tiledb_array_open(arr, "READ")

res <- tiledb_get_all_metadata(arr)
expect_equal(length(res), 2)
expect_true("vec" %in% names(res))
expect_true("txt" %in% names(res))
#})

#test_that("Can delete by key", {
arr <- unlink_and_create_ptr(tmp)

arrR <- tiledb_array_open(arr, "READ")

## should be two before we add
expect_equal(tiledb_num_metadata(arr), 2)

close_and_reopen(arr, "WRITE")
expect_true(tiledb_put_metadata(arr, "foo", "the quick brown fox"))

close_and_reopen(arr, "READ")
## should be three after we add
expect_equal(tiledb_num_metadata(arr), 3)

close_and_reopen(arr, "WRITE")
expect_true(tiledb_delete_metadata(arr, "foo"))

close_and_reopen(arr, "READ")

## should be two after we delete
expect_equal(tiledb_num_metadata(arr), 2)
#})

vals <- bit64::as.integer64(c(10,20,30))
close_and_reopen(arr, "WRITE")
expect_true(tiledb_put_metadata(arr, "int64", vals))
close_and_reopen(arr, "READ")
expect_equal(tiledb_get_metadata(arr, "int64"), vals)


if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
