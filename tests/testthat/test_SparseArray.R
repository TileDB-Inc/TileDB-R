library(tiledb)
context("tiledb::SparseArray")

unlink_and_create <- function(tmp) {
  if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
    dir.create(tmp)
  } else {
    dir.create(tmp)
  }
  return(tmp)
}
#
# test_that("Can read / write simple 1D sparse vector", {
#   tmp <- tempfile()
#   setup({
#     unlink_and_create(tmp)
#   })
#
#   ctx <- tiledb_ctx()
#   d1  <- tiledb_dim(ctx, domain = c(1L, 10L))
#   dom <- tiledb_domain(ctx, c(d1))
#   atr <- tiledb_attr(ctx)
#   sch <- tiledb_array_schema(ctx, dom, c(atr), sparse = TRUE)
#
#   arr <- tiledb::SparseArray(ctx, sch, tmp)
#   expect_true(is.sparse(arr))
#
#   arr <- tiledb::SparseArray.load(ctx, tmp)
#   expect_true(is.sparse(arr))
#
#   teardown(
#     unlink(tmp, recursive = TRUE)
#   )
# })

test_that("test tiledb_subarray read for sparse array", {
  tmp <- tempfile()
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr("val")
  sch <- tiledb_array_schema(dom, c(val), sparse=TRUE)
  tiledb_array_create(tmp, sch)

  dat <- matrix(rnorm(25), 5, 5)
  arr <- tiledb_sparse(tmp, as.data.frame=FALSE)
  I <- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
  J <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)

  arr[I, J] <- dat
  expect_equal(arr[]$val, unlist(as.list(dat)))

  # describe array
  expect_equal(dim(arr), c(5,5))

  # explicit range enumeration
  res <- tiledb_subarray(arr, list(3,5, 3,5))
  expect_equal(res$val,
               unlist(as.list(dat[c(3,4,5), c(3,4,5)])))

  # vector range syntax
  expect_equal(tiledb_subarray(arr, list(1,3,1,3))$val, unlist(as.list(dat[1:3, 1:3])))

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("test tiledb_subarray read for sparse array with attribute list", {
  tmp <- tempfile()
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr("val")
  val2 <- tiledb_attr("val2")

  sch <- tiledb_array_schema(dom, c(val, val2), sparse=TRUE)
  tiledb_array_create(tmp, sch)

  dat1 <- matrix(rnorm(25), 5, 5)
  dat2 <- matrix(rnorm(25), 5, 5)

  arr <- tiledb_sparse(tmp, as.data.frame=FALSE)
  I <- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
  J <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)

  arr[I, J] <-  list(val=dat1, val2=dat2)
  expect_equal(arr[]$val, unlist(as.list(dat1)))

  # explicit range enumeration
  res <- tiledb_subarray(arr, list(3,5, 3,5), attrs=c("val"))
  expect_equal(res$val,
               unlist(as.list(dat1[c(3,4,5), c(3,4,5)])))

  # vector range syntax
  expect_equal(tiledb_subarray(arr, list(1,3,1,3), attrs=c("val2"))$val2, unlist(as.list(dat2[1:3, 1:3])))

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("test tiledb_subarray read for sparse array as dataframe", {
  tmp <- tempfile()
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim("d1", domain = c(1L, 5L))
  d2  <- tiledb_dim("d2", domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr("val")
  val2 <- tiledb_attr("val2")

  sch <- tiledb_array_schema(dom, c(val, val2), sparse=TRUE)
  tiledb_array_create(tmp, sch)

  dat1 <- matrix(rnorm(25), 5, 5)
  dat2 <- matrix(rnorm(25), 5, 5)

  arr <- tiledb_sparse(tmp, as.data.frame=TRUE)
  I <- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
  J <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)

  arr[I, J] <- list(val=dat1, val2=dat2)
  expect_equal(arr[]$val, unlist(as.list(dat1)))

  # explicit range enumeration
  res <- tiledb_subarray(arr, list(3,5, 3,5), attrs=c("val"))
  expect_is(res, "data.frame")
  expect_equal(res$val,
               unlist(as.list(dat1[c(3,4,5), c(3,4,5)])))

  # vector range syntax
  expect_equal(tiledb_subarray(arr, list(1,3,1,3), attrs=c("val2"))$val2, unlist(as.list(dat2[1:3, 1:3])))

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})


test_that("test tiledb_subarray read/write for sparse array with list of coordinates", {
  tmp <- tempfile()
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr("val")
  sch <- tiledb_array_schema(dom, c(val), sparse=TRUE)
  tiledb_array_create(tmp, sch)

  dat <- matrix(rnorm(25), 5, 5)
  arr <- tiledb_sparse(tmp, as.data.frame=FALSE)
  I <- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
  J <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)

  coords = list(I, J)
  arr[coords] <- dat
  expect_equal(arr[]$val, unlist(as.list(dat)))

  # explicit range enumeration
  expect_equal(arr[list(c(3,4,5), c(3,4,5))]$val,
               unlist(as.list(dat[c(3,4,5), c(3,4,5)])))

  # vector range syntax
  expect_equal(arr[list(c(1:3), c(1:3))]$val, unlist(as.list(dat[1:3, 1:3])))

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})
