library(testthat)
library(tiledb)
context("tiledb_dense")

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

test_that("Can read / write a simple 1D vector", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  dim <- tiledb_dim(domain = c(1L, 10L))
  dom <- tiledb_domain(c(dim))
  val <- tiledb_attr("val")
  sch <- tiledb_array_schema(dom, c(val))
  tiledb_array_create(tmp, sch)

  arr <- tiledb_dense(tmp, as.data.frame=FALSE)
  dat <- as.array(as.double(1:10))
  arr[] <- dat

  arr <- tiledb_dense(tmp, as.data.frame=FALSE)
  expect_equal(arr[], dat)

  # explicit range enumeration
  expect_equal(arr[c(3,4,5,6,7)], dat[c(3,4,5,6,7)])

  # vector range syntax
  expect_equal(arr[3:7], dat[3:7])

  # vector range syntax (reversed)
  # TODO: find a way to efficiently do this
  # expect_equal(arr[7:3], dat[7:3])

  # scalar indexing
  expect_equal(arr[8], dat[8])

  arr[6] <- 1000
  expect_equal(arr[6], 1000)

  arr[7:10] <- as.array(c(97, 98, 99, 100))
  expect_equal(arr[6:10], as.array(c(1000, 97, 98, 99, 100)))

  # attribute indexing
  expect_s4_class( arr[[1]], "tiledb_attr")
  expect_s4_class( arr[["val"]], "tiledb_attr")

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("Can read / write a simple 2D matrix", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr("val")
  sch <- tiledb_array_schema(dom, c(val))
  tiledb_array_create(tmp, sch)

  dat <- matrix(rnorm(25), 5, 5)
  arr <- tiledb_dense(tmp, as.data.frame=FALSE)

  arr[] <- dat
  expect_equal(arr[], dat)

  # explicit range enumeration
  expect_equal(arr[c(3,4,5), c(3,4,5)],
               dat[c(3,4,5), c(3,4,5)])

  # vector range syntax
  expect_equal(arr[1:3, 1:3], dat[1:3, 1:3])

  # missing index range
  expect_equal(arr[1:3,], dat[1:3,])

  # scalar indexing
  expect_equal(arr[3, 3], dat[3, 3])

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("Can read / write a simple 3D matrix", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  d3  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2, d3))
  val <- tiledb_attr(name="val")
  sch <- tiledb_array_schema(dom, c(val))
  tiledb_array_create(tmp, sch)

  dat <- array(rnorm(125), dim = c(5, 5, 5))
  arr <- tiledb_dense(tmp, as.data.frame=FALSE)

  arr[] <- dat
  expect_equal(arr[], dat)

  # explicit range enumeration
  expect_equal(arr[c(3, 4, 5), c(3, 4, 5), c(1, 2)],
               dat[c(3, 4, 5), c(3, 4, 5), c(1, 2)])

  # vector range syntax
  expect_equal(arr[1:3, 1:3, 1:2], dat[1:3, 1:3, 1:2])

  # scalar indexing
  expect_equal(arr[3, 3, 3], dat[3, 3, 3])

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})


test_that("Can read / write 1D multi-attribute array", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
   unlink_and_create(tmp)
  })

  dim <- tiledb_dim(domain = c(1L, 10L))
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

  teardown({
    unlink(tmp, recursive = TRUE, force = TRUE)
  })
})

test_that("Can read / write 2D multi-attribute array", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
   unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 10L))
  d2  <- tiledb_dim(domain = c(1L, 10L))
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

  # attribute indexing
  expect_s4_class( arr[["a1"]], "tiledb_attr")
  expect_s4_class( arr[[1]], "tiledb_attr")
  expect_s4_class( arr[["a2"]], "tiledb_attr")
  expect_s4_class( arr[[2]], "tiledb_attr")
  expect_error( arr[[2]] <- dat[[2]] )

  teardown({
    unlink(tmp, recursive = TRUE)
  })
})

test_that("as.array() conversion method", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
   unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 10L))
  dom <- tiledb_domain(c(d1))
  a1  <- tiledb_attr("a1", type = "FLOAT64")
  sch <- tiledb_array_schema(dom, c(a1))
  tiledb_array_create(tmp, sch)

  arr <- tiledb_dense(tmp, as.data.frame=FALSE)
  dat <- as.double(1:10)
  arr[] <- dat
  expect_equal(as.array(arr), as.array(dat))

  teardown({
      unlink(tmp, recursive = TRUE, force = TRUE)
  })
})

test_that("as.data.frame() conversion method", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
   unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 10L))
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

  teardown({
    unlink(tmp, recursive = TRUE, force = TRUE)
  })
})

test_that("test tiledb_subarray read for dense array", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr(name="val")
  sch <- tiledb_array_schema(dom, c(val))
  tiledb_array_create(tmp, sch)

  dat <- matrix(rnorm(25), 5, 5)
  arr <- tiledb_dense(tmp, as.data.frame=FALSE)

  arr[] <- dat
  expect_equal(arr[], dat)

  # explicit range enumeration
  expect_equal(tiledb_subarray(arr, list(3,5, 3,5)),
               dat[c(3,4,5), c(3,4,5)])

  # vector range syntax
  expect_equal(tiledb_subarray(arr, list(1,3,1,3)), dat[1:3, 1:3])

  teardown({
    unlink(tmp, recursive = TRUE, force = TRUE)
  })
})

test_that("test tiledb_subarray read for dense array with select attributes", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val1 <- tiledb_attr("val1")
  val2 <- tiledb_attr("val2")
  sch <- tiledb_array_schema(dom, c(val1, val2))
  tiledb_array_create(tmp, sch)

  dat1 <- matrix(rnorm(25), 5, 5)
  dat2 <- matrix(rnorm(25), 5, 5)
  arr <- tiledb_dense(tmp, as.data.frame=FALSE)

  arr[] <- list(val1=dat1, val2=dat2)
  expect_equal(arr[]$val1, dat1)
  expect_equal(arr[]$val2, dat2)

  # explicit range enumeration
  expect_equal(tiledb_subarray(arr, list(3,5, 3,5), attrs=c("val1")),
               dat1[c(3,4,5), c(3,4,5)])

  # vector range syntax
  expect_equal(tiledb_subarray(arr, list(1,3,1,3), attrs=c("val2")), dat2[1:3, 1:3])

  teardown({
    unlink(tmp, recursive = TRUE, force = TRUE)
  })
})

test_that("test tiledb_subarray read for dense array as dataframe", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val1 <- tiledb_attr("val1")
  val2 <- tiledb_attr("val2")
  sch <- tiledb_array_schema(dom, c(val1, val2))
  tiledb_array_create(tmp, sch)

  dat1 <- matrix(rnorm(25), 5, 5)
  dat2 <- matrix(rnorm(25), 5, 5)
  arr <- tiledb_dense(tmp, as.data.frame=TRUE)

  arr[] <- list(val1=dat1, val2=dat2)
  expect_equal(arr[]$val1, unlist(as.list(dat1)))
  expect_equal(arr[]$val2, unlist(as.list(dat2)))

  # explicit range enumeration
  expect_equal(tiledb_subarray(arr, list(3,5, 3,5), attrs=c("val1"))$val1,
               unlist(as.list(dat1[c(3,4,5), c(3,4,5)])))

  # vector range syntax
  expect_equal(tiledb_subarray(arr, list(1,3,1,3), attrs=c("val2"))$val2, unlist(as.list(dat2[1:3, 1:3])))

  teardown({
    unlink(tmp, recursive = TRUE, force = TRUE)
  })
})

test_that("Can read / write a simple 2D matrix with list of coordinates", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr("val")
  sch <- tiledb_array_schema(dom, c(val))
  tiledb_array_create(tmp, sch)

  dat <- matrix(rnorm(25), 5, 5)
  arr <- tiledb_dense(tmp, as.data.frame=FALSE)

  arr[] <- dat
  expect_equal(arr[], dat)

  # explicit range enumeration
  expect_equal(arr[list(c(3,4,5), c(3,4,5))],
               dat[c(3,4,5), c(3,4,5)])

  # vector range syntax
  expect_equal(arr[list(c(1:3), c(1:3))], dat[1:3, 1:3])

  # scalar indexing
  expect_equal(arr[list(c(3), c(3))], dat[3, 3])

  teardown({
    unlink(tmp, recursive = TRUE, force = TRUE)
  })
})

test_that( "treating logical as INT32 works", {
  tmp <- tempfile(tmpdir = tempdir(check = TRUE))
  setup({
    unlink_and_create(tmp)
  })

  dat <- matrix(rnorm(25) > 0, 5, 5)

  d1  <- tiledb_dim(domain = c(1L, 5L))
  d2  <- tiledb_dim(domain = c(1L, 5L))
  dom <- tiledb_domain(c(d1, d2))
  val <- tiledb_attr("val", type = r_to_tiledb_type(dat))
  sch <- tiledb_array_schema(dom, c(val))
  tiledb_array_create(tmp, sch)

  arr <- tiledb_dense(tmp, as.data.frame=FALSE)

  arr[] <- dat
  int_dat = dat
  storage.mode(int_dat) = "integer"
  expect_equal(arr[], int_dat)

})

test_that( "We can verify the shape and type of replacement values", {
    expect_true( check_replacement_value(1:4, c(4)) )
    expect_true( check_replacement_value(matrix(1:12, ncol = 3), c(4,3)) )
    expect_true( check_replacement_value(list(a = 1:4, b = 2:3, c = 9), c(3)) )
    expect_true( check_replacement_value(matrix(list(a = 1:4, b = 2:3, c = 9, d = 4:5), ncol = 2), c(2,2)) )

    expect_error( check_replacement_value(1:4, c(3)) )
    expect_error( check_replacement_value(matrix(1:12, ncol = 3), c(4,1)) )
    expect_error( check_replacement_value(list(1:4, 2:3, 9), c(9)) )
    expect_error( suppressWarnings(check_replacement_value(matrix(list(1:4, 2:3, 9, 4:5), ncol = 2), c(2,2,12))) )

    expect_error( check_replacement_value( new.env(), 99))
})