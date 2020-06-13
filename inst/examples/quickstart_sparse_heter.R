#!/usr/bin/env Rscript
##
## cf quickstart_sparse_heter.cc
##
## When run, this program will create a 2D sparse array with each dimension
## having separate datatypes, similar to a dataframe. It will write some data to
## it, and read a slice of the data back.

library(tiledb)

## Name of the array to create.
array_name <- "quickstart_sparse_heter_array"

## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## The array will be 4x4 with dimensions "rows" and "cols"
  ## "rows" is a string dimension type so domain and extend are NULL
  ## and space tiles 2x2
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1,4), 4, "FLOAT64")))

  ## Sparse schema
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")),
                                sparse = TRUE)

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
  invisible(NULL)
}

write_array <- function(uri) {
  ## Write some simple data to cells (1, 1.1), (2, 1.2) and (2, 1.3).
  data <- c(1L, 2L, 3L)
  coords_rows <- c(1L, 2L, 2L)
  coords_cols <- c(1.1, 1.2, 1.3)

  arr <- tiledb_array(uri, query_type="WRITE", is.sparse=TRUE)

  arr[] <- data.frame(rows=coords_rows, cols=coords_cols, a=data)
  invisible(NULL)
}

read_array <- function(uri) {
  ## opening in data.frame mode; could also open with as.data.frame=FALSE
  ## returning three vectors
  arr <- tiledb_array(uri, as.data.frame=TRUE)
  dat <- arr[]
  print(dat)
}

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_array(uri)
read_array(uri)
