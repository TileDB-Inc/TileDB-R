library(testthat)
library(tiledb)
context("tiledb_metadata")

unlink_and_create <- function(tmp) {
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  dir.create(tmp)

  dim <- tiledb_dim(domain = c(1L, 4L))
  dom <- tiledb_domain(c(dim))
  a1  <- tiledb_attr("a1", type = "INT32")
  a2  <- tiledb_attr("a2", type = "INT32")
  sch <- tiledb_array_schema(dom, c(a1, a2), sparse=TRUE)
  tiledb_array_create(tmp, sch)
  arr <- tiledb_sparse(tmp, as.data.frame=FALSE)

  arrW <- tiledb:::libtiledb_array_open(arr@ptr, "WRITE")
  tiledb:::put_metadata(arrW, "vec", c(1.1, 2.2, 3.3))
  tiledb:::libtiledb_array_close(arrW)

  arrR <- tiledb:::libtiledb_array_open(arr@ptr, "READ")
  return(arrR)
}

test_that("Can check presence", {
  tmp <- tempfile()
  setup({
    arr <<- unlink_and_create(tmp)
  })

  expect_error(tiledb:::has_metadata(NULL, ""))
  expect_false(tiledb:::has_metadata(arr, ""))
  expect_true(tiledb:::has_metadata(arr, "vec"))
  expect_true(TRUE)
})
