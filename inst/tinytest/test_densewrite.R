library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

## Writes to dense arrays via `[<-`, see issue #877

#' Create an empty dense array
#'
#' @param ndim Number of dimensions, named \code{d1}, \code{d2}, ...
#' @param type Datatype of the dimensions
#' @param nattr Number of \code{FLOAT64} attributes, named \code{x}, \code{y}
#'
#' @return The URI of the new array
#'
#' @noRd
#'
make_dense <- function(ndim = 2L, type = "INT32", nattr = 1L) {
  uri <- tempfile()
  ext <- c(4L, 4L, 2L, 2L)[seq_len(ndim)]
  dims <- lapply(seq_len(ndim), function(m) {
    tiledb_dim(paste0("d", m), c(1L, ext[m]), ext[m], type = type)
  })
  attrs <- lapply(c("x", "y")[seq_len(nattr)], tiledb_attr, type = "FLOAT64")
  sch <- tiledb_array_schema(tiledb_domain(dims = dims), attrs = attrs,
                             sparse = FALSE)
  tiledb_array_create(uri, sch)
  return(uri)
}

#' Read a dense array back as a full-domain R array
#'
#' Cells that were never written are \code{NA}; a plain read would only
#' return the non-empty domain
#'
#' @param uri URI of an array created by \code{make_dense()}
#' @param attr Name of the attribute to return
#'
#' @return An R array with one element per cell of the full domain
#'
#' @noRd
#'
read_dense <- function(uri, attr = "x") {
  df <- tiledb_array(uri, return_as = "data.frame")[]
  dn <- grep("^d[0-9]$", names(df), value = TRUE)
  out <- array(NA_real_, c(4L, 4L, 2L, 2L)[seq_along(dn)])
  idx <- vapply(df[dn], as.integer, integer(nrow(df)))
  out[matrix(idx, ncol = length(dn))] <- df[[attr]]
  return(out)
}

## 2-d, every block width: [i, j] and selected_ranges()
for (nc in 1:4) {
  m <- matrix(as.numeric(seq_len(4 * nc)), 4, nc)

  uri <- make_dense()
  arr <- tiledb_array(uri)
  arr[1:4, seq_len(nc)] <- m
  expect_equal(read_dense(uri)[, seq_len(nc), drop = FALSE], m,
               info = sprintf("[i, j] with %d columns", nc))

  uri <- make_dense()
  arr <- tiledb_array(uri)
  selected_ranges(arr) <- list(cbind(1, 4), cbind(1, nc))
  arr[] <- m
  expect_equal(read_dense(uri)[, seq_len(nc), drop = FALSE], m,
               info = sprintf("selected_ranges() with %d columns", nc))
}

## 2-d, offset block leaves the remaining cells empty
uri <- make_dense()
arr <- tiledb_array(uri)
arr[2:3, 3] <- matrix(c(1, 2), 2, 1)
res <- read_dense(uri)
expect_equal(res[2:3, 3], c(1, 2))
expect_equal(sum(!is.na(res)), 2L)

## 3-d, [i, j, k] and [i, , k]
uri <- make_dense(3L)
arr <- tiledb_array(uri)
arr[1:4, 2:4, 1] <- matrix(101:112, 4, 3)
arr[1:4, 2:4, 2] <- matrix(201:212, 4, 3)
res <- read_dense(uri)
expect_equal(res[, 2:4, 1], matrix(as.numeric(101:112), 4, 3))
expect_equal(res[, 2:4, 2], matrix(as.numeric(201:212), 4, 3))
expect_true(all(is.na(res[, 1, ])))

uri <- make_dense(3L)
arr <- tiledb_array(uri)
arr[2:3, , 2] <- matrix(1:8, 2, 4)
res <- read_dense(uri)
expect_equal(res[2:3, , 2], matrix(as.numeric(1:8), 2, 4))
expect_equal(sum(!is.na(res)), 8L)

## 3-d, an array right-hand side
uri <- make_dense(3L)
arr <- tiledb_array(uri)
arr[1:2, 1:4, 1:2] <- array(1:16, c(2, 4, 2))
expect_equal(read_dense(uri)[1:2, , ], array(as.numeric(1:16), c(2, 4, 2)))

## 4-d via named selected_ranges(), given out of order
uri <- make_dense(4L)
arr <- tiledb_array(uri)
selected_ranges(arr) <- list(d4 = cbind(2, 2), d3 = cbind(1, 1))
arr[] <- matrix(1:16, 4, 4)
res <- read_dense(uri)
expect_equal(res[, , 1, 2], matrix(as.numeric(1:16), 4, 4))
expect_equal(sum(!is.na(res)), 16L)

## 1-d
uri <- make_dense(1L)
arr <- tiledb_array(uri)
arr[2:3] <- c(7, 8)
expect_equal(as.vector(read_dense(uri)), c(NA, 7, 8, NA))

## INT64 dimensions
if (requireNamespace("bit64", quietly = TRUE)) {
  uri <- make_dense(2L, "INT64")
  arr <- tiledb_array(uri)
  arr[bit64::as.integer64(2:3), bit64::as.integer64(1:3)] <- matrix(1:6, 2, 3)
  expect_equal(read_dense(uri)[2:3, 1:3], matrix(as.numeric(1:6), 2, 3))
}

## multiple attributes, [i, j] with a list right-hand side
uri <- make_dense(2L, nattr = 2L)
arr <- tiledb_array(uri)
arr[2:3, 1:2] <- list(x = matrix(1:4, 2, 2), y = matrix(5:8, 2, 2))
expect_equal(read_dense(uri, "x")[2:3, 1:2], matrix(as.numeric(1:4), 2, 2))
expect_equal(read_dense(uri, "y")[2:3, 1:2], matrix(as.numeric(5:8), 2, 2))
expect_equal(sum(!is.na(read_dense(uri, "x"))), 4L)
## previously a silent no-op
expect_error(arr[] <- data.frame(a = 1, b = 1, c = 1), "does not match")

## errors
uri <- make_dense()
arr <- tiledb_array(uri)
expect_error(arr[1:4, 1:2, 1] <- matrix(1, 4, 2), "at least 3 dimensions")
expect_error(arr[c(1, 3), 1:2] <- matrix(1, 2, 2), "contiguous")
expect_error(arr[4:1, 1:2] <- matrix(1, 4, 2), "contiguous")
selected_ranges(arr) <- list(cbind(1, 4), NULL)
expect_error(arr[1:4, 1:2] <- matrix(1, 4, 2), "Cannot set both 'i'")
selected_ranges(arr) <- list(cbind(c(1, 3), c(1, 4)), NULL)
expect_error(arr[] <- matrix(1, 3, 4), "single range")
selected_ranges(arr) <- list()
selected_points(arr) <- list(c(1, 2), NULL)
expect_error(arr[] <- matrix(1, 2, 4), "selected_points")

uri <- make_dense(4L)
arr <- tiledb_array(uri)
expect_error(arr[1:4, 1:4, 1, 1] <- matrix(1, 4, 4), "third dimension")

## nothing was written by the failing assignments
expect_equal(tiledb_fragment_info_get_num(tiledb_fragment_info(uri)), 0)
