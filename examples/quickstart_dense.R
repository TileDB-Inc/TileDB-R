# quickstart_dense.R
#
# LICENSE
#
# The MIT License
#
# Copyright (c) 2018 TileDB, Inc.
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

# Name of the array to create.
array_name = "quickstart_dense"

create_array <- function() {
    # Create a TileDB context
    ctx <- tiledb_ctx()

    # Check if the array already exists.
    if (tiledb_object_type(array_name, ctx=ctx) == "ARRAY") {
        message("Array already exists.")
        return(invisible(NULL))
    }

    # The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
    dom <- tiledb_domain(
               dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32", ctx=ctx),
		                    tiledb_dim("cols", c(1L, 4L), 4L, "INT32", ctx=ctx)), ctx=ctx)

    # The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
    schema <- tiledb_array_schema(
                  dom, attrs = c(tiledb_attr("a", type = "INT32", ctx=ctx)), ctx=ctx)

    # Create the (empty) array on disk.
    tiledb_array_create(array_name, schema)
}

write_array <- function() {
    data <- array(c(c(1L, 5L, 9L, 13L), 
                    c(2L, 6L, 10L, 14L),
                    c(3L, 7L, 11L, 15L), 
                    c(4L, 8L, 12L, 16L)), dim = c(4,4))
    # Open the array and write to it.
    ctx <- tiledb_ctx()
    A <- tiledb_dense(uri = array_name, ctx=ctx)
    A[] <- data
}

read_array <- function() {
    ctx = tiledb_ctx()
    # Open the array and read from it.
    A <- tiledb_dense(uri = array_name, ctx=ctx)
    data <- A[1:2, 2:4]
    show(data)
}

create_array()
write_array()
read_array()
