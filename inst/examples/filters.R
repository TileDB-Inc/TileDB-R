#!/usr/bin/env Rscript
##
## cf filters.cc
##
## When run, this program will create a 2D sparse array with each dimension
## having separate datatypes, similar to a dataframe. It will write some data to
## it, and read a slice of the data back.

library(tiledb)

## Name of the array to create.
array_name <- "filters_array"

## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## The array will be 4x4 with dimensions "rows" and "cols"
  ## "rows" is a string dimension type so domain and extend are NULL
  ## and space tiles 2x2
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT32")))

  ## Filters
  fbwd <- tiledb_filter("BIT_WIDTH_REDUCTION")
  fzstd <- tiledb_filter("ZSTD")
  fla1 <- tiledb_filter_list(c(fbwd,fzstd))
  fla2 <- tiledb_filter_list(tiledb_filter("GZIP"))

  ## Sparse schema
  schema <- tiledb_array_schema(dom,
                                attrs = c(tiledb_attr("a1", type = "INT32", filter_list=fla1),
                                          tiledb_attr("a2", type = "INT32", filter_list=fla2)),
                                sparse = TRUE)

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
  invisible(NULL)
}

write_array <- function(uri) {
  ## Write some simple data to cells (1, 1), (2, 4) and (2, 3).
  data_a1 <- c(1L, 2L, 3L)
  data_a2 <- c(-1L, -2L, -3L)
  coords_rows <- c(1L, 2L, 2L)
  coords_cols <- c(1L, 4L, 3L)

  arr <- tiledb_array(uri, query_type="WRITE", is.sparse=TRUE)

  arr[] <- data.frame(rows=coords_rows, cols=coords_cols, a1=data_a1, a2=data_a2)
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
