# quickstart_sparse.R
#
# LICENSE
#
# The MIT License
#
# Copyright (c) 2018-2020 TileDB, Inc.
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
# When run, this program will create a simple 2D sparse array, write some data
# to it, and read a slice of the data back.
#

library(tiledb)

# Name of the array to create.
array_name <- "quickstart_sparse"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(array_name) {
    # Check if the array already exists.
    if (tiledb_object_type(array_name) == "ARRAY") {
        message("Array already exists, removing to create new one.")
        tiledb_vfs_remove_dir(array_name)
    }

    # The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
    dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                                  tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

   # The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
    schema = tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "INT32")), sparse = TRUE)

    # Create the (empty) array on disk.
    invisible( tiledb_array_create(array_name, schema) )
}

write_array <- function(array_name) {
    I <- c(1, 2, 2)
    J <- c(1, 4, 3)
    data <- c(1L, 2L, 3L)
    # Open the array and write to it.
    A <- tiledb_array(uri = array_name)
    A[I, J] <- data
}

read_array <- function(array_name) {
    # Open the array and read as a data.frame from it.
    A <- tiledb_array(uri = array_name, as.data.frame=TRUE)
    # Slice rows 1 and 2, and cols 2, 3 and 4
    A[1:2, 2:4]
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

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  stopifnot(tiledb_query_status(qry)=="COMPLETE")

  n <- tiledb_query_result_buffer_elements(qry, "a")
  print(data.frame(rows=rows,cols=cols,a=values)[1:n,])
}

create_array(uri)
write_array(uri)
read_array(uri)
read_via_query_object(uri)
