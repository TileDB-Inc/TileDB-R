#!/usr/bin/env Rscript
##
## cf writing_dense_sparse.cc
##
## When run, this script will create a simple 2D dense array, write some sparse
## cells to it in a way that some space is empty, and read the entire array data
## back.

library(tiledb)

## Name of the array to create.
array_name <- "writing_dense_sparse_array"

## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  ## and space tiles 2x2
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 2L, "INT32"),
                                tiledb_dim("cols", c(1L,4L), 2L, "INT32")))

  ## Dense scheme, adding a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")))

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
  invisible(NULL)
}

write_array <- function(uri) {
  data <- c(1L, 2L, 3L, 4L)
  coords_rows <- c(1L, 2L, 4L, 1L)
  coords_cols <- c(2L, 1L, 3L, 4L)

  arr <- tiledb_array(uri, query_type="WRITE", is.sparse=FALSE)

  arr[] <- data.frame(rows=coords_rows, cols=coords_cols, a=data)
  invisible(NULL)
}

read_array <- function(uri) {
  ## opening in data.frame mode
  ## returning three vectors
  arr <- tiledb_array(uri, return_as="data.frame")
  dat <- arr[]
  print(dat)
}

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_array(uri)
read_array(uri)
