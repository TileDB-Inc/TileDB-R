
# quickstart_dense.R
#
# LICENSE
#
# The MIT License
#
# Copyright (c) 2018-2022 TileDB, Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
# DESCRIPTION
#
# This is a part of the TileDB quickstart tutorial:
#   https://docs.tiledb.io/en/latest/quickstart.html
#
# When run, this program will create a simple 2D dense array, write some data
# to it, and read a slice of the data back.
#

library(tiledb)

# Name of the array to create using the in-memory filesystem
uri <- "mem://quickstart_dense"

create_array <- function(uri) {
    # Check if the array already exists.
    if (tiledb_object_type(uri) == "ARRAY") {
        message("Array already exists, removing to create new one.")
        tiledb_vfs_remove_dir(uri)
    }

    # The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
    dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                                  tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

    # The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
    schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")))

    # Create the (empty) array on disk.
    invisible(tiledb_array_create(uri, schema))
}

write_array <- function(uri) {
    ## equivalent to matrix(1:16, 4, 4, byrow=TRUE)
    data <- array(c(c(1L, 5L, 9L, 13L),
                    c(2L, 6L, 10L, 14L),
                    c(3L, 7L, 11L, 15L),
                    c(4L, 8L, 12L, 16L)), dim = c(4,4))
    # Open the array and write to it.
    A <- tiledb_array(uri = uri)
    A[] <- data
}

write_array_via_query <- function(uri) {
    data <- 1:16
    arr <- tiledb_array(uri = uri)
    qry <- tiledb_query(arr, "WRITE")
    #qry <- tiledb_query_set_layout(qry, "ROW_MAJOR") # also default, transpose if COL_MAJOR
    qry <- tiledb_query_set_buffer(qry, "a", data)
    qry <- tiledb_query_submit(qry)
    qry <- tiledb_query_finalize(qry)
    stopifnot(tiledb_query_status(qry)=="COMPLETE")
}

write_array_via_query_piped <- function(uri) {
    data <- 1:16
    arr <- tiledb_array(uri = uri)
    qry <- tiledb_query(arr, "WRITE")
    qry |>
        tiledb_query_set_layout("ROW_MAJOR") |>  # also default, transpose if COL_MAJOR
        tiledb_query_set_buffer("a", data) |>
        tiledb_query_submit() |>
        tiledb_query_finalize()
    stopifnot(tiledb_query_status(qry)=="COMPLETE")
}

read_array <- function(uri) {
    # Open the array and read from it.
    A <- tiledb_array(uri = uri)
    data <- A[] #A[1:2, 2:4]
    show(data)
}

read_via_query_object <- function(array_name) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "READ")

  rows <- integer(8)
  cols <- integer(8)
  values <- integer(8)
  tiledb_query_set_buffer(qry, "rows", rows)
  tiledb_query_set_buffer(qry, "cols", cols)
  tiledb_query_set_buffer(qry, "a", values)
  tiledb_query_set_subarray(qry, c(1L, 2L, 2L, 4L))

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  stopifnot(tiledb_query_status(qry)=="COMPLETE")

  n <- tiledb_query_result_buffer_elements(qry, "a")
  print(data.frame(rows=rows,cols=cols,a=values)[1:n,])
}

read_via_query_piped <- function(array_name) {
  arr <- tiledb_array(uri)

  rows <- integer(16)
  cols <- integer(16)
  values <- integer(16)
  qry <- tiledb_query(arr, "READ") |>
    tiledb_query_set_buffer("rows", rows) |>
    tiledb_query_set_buffer("cols", cols) |>
    tiledb_query_set_buffer("a", values) |>
    tiledb_query_set_subarray(c(1L, 4L, 1L, 4L)) |>
    tiledb_query_submit() |>
    tiledb_query_finalize()
  stopifnot(tiledb_query_status(qry) =="COMPLETE")

  n <- tiledb_query_result_buffer_elements(qry, "a")
  print(data.frame(rows=rows,cols=cols,a=values)[1:n,])
}

create_array(uri)
write_array(uri)
write_array_via_query(uri)
write_array_via_query_piped(uri)
read_array(uri)
read_via_query_object(uri)
read_via_query_piped(uri)
