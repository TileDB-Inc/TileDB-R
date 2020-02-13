#!/usr/bin/Rscript

library(tiledb)

uri <- tempfile()   ## replace with path + filename to keep the data


unlink_and_create_single_attribute <- function(tmp, attr) {
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  dir.create(tmp, recursive = TRUE)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = attr, is_var = TRUE)))

  ## Create the (empty) array on disk.
  tiledb_array_create(tmp, schema)
}

stringExample <- function(uri) {
  unlink_and_create_single_attribute(uri, "CHAR")
  arr <- tiledb_dense(uri)
  dat <- data.table::data.table(V1=list("a", "eee", "i", "m"),
                                V2=list("bb", "f", "jjj", "n"),
                                V3=list("ccc", "g", "kk", "oo"),
                                V4=list("dd", "hhh", "l", "qqqq"))
  arr[] <- dat
  print(arr[])

  val <- "tictoc"
  arr[1,2] <- val
  print(arr[])

  val <- matrix(c("the", "quick", "brown", "fox"), 2,2)
  arr[2:3, 2:3] <- val
  print(arr[])
  invisible(NULL)
}

int32Example <- function(uri) {
  unlink_and_create_single_attribute(uri, "INT32")
  arr <- tiledb_dense(uri)
  dat <- data.table::data.table(V1=list(c(1L, 1L), 5L, c(9L, 9L), 13L),
                                V2=list(c(2L,2L), c(6L,6L), 10L, c(14L,14L,14L)),
                                V3=list(3L, c(7L,7L), 11L, 15L),
                                V4=list(4L, c(8L,8L,8L), c(12L,12L), 16L))
  arr[] <- dat
  print(arr[])

  val <- 55L
  arr[1,2] <- val
  print(arr[])

  val <- array(c(111L,112L),c(1,2))
  arr[3,3] <- val
  print(arr[])
  invisible(NULL)
}

doubleExample <- function(uri) {
  unlink_and_create_single_attribute(uri, "FLOAT64")
  arr <- tiledb_dense(uri)

  dat <- data.table::data.table(V1=list(c(1.0, 1.1), 5,        c(9, 9),  13),
                                V2=list(c(2,2),      c(6,6),   10,       c(14.1,14.2,14.3)),
                                V3=list(3,           c(7,7),   11,       15),
                                V4=list(4,           c(8,8,8), c(12,12), 16.75))
  arr[] <- dat
  print(arr[])

  val <- 5.1
  arr[1,2] <- val
  print(arr[])

  val <- array(c(11.1,11.2),c(1,2))
  arr[3,3] <- val
  print(arr[])
  invisible(NULL)
}

stringExample(uri)
int32Example(uri)
doubleExample(uri)
