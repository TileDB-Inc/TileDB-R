# quickstart_sparse.R
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
# When run, this program will create a simple 2D sparse array, write some data
# to it, and read a slice of the data back.
#

library(tiledb)

# Name of the array to create.
array_name = "quickstart_sparse"

create_array <- function() {
    # Create a TileDB context
    ctx <- tiledb_ctx()

    # Check if the array already exists.
    if tiledb_object_type(ctx, array_name) == "TILEDB_ARRAY":
        stop("Array already exists.")
	quit(0)

    # The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
    dom <- tiledb_domain(ctx, 
		dims = c(tiledb_dim(ctx, "rows", c(1L, 4L), 4L, "TILEDB_INT32"),
			 tiledb_dim(ctx, "cols", c(1L, 4L), 4L, "TILEDB_INT32")))

   # The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
    schema = tiledb_array_schema(ctx,
		dom, attrs=c(tiledb_attr(ctx, "a", type = "TILEDB_INT32")),
		sparse = TRUE)

    # Create the (empty) array on disk.
    tiledb_array_create(array_name, uri = schema)
}

write_array <- function() {
    I <- c(1, 2, 2)
    J <- c(1, 4, 3)
    data <- c(1L, 2L, 3L)
    # Open the array and write to it.
    ctx <- tiledb_ctx()
    A <- tiledb_sparse(ctx, uri = array_name)
    A[I, J] <- data
}

read_array <- function() {
    ctx = tiledb_ctx()
    # Open the array and read from it.
    A <- tiledb_dense(ctx, uri = array_name)
    data <- A[1:2, 2:4]

    coords <- data[["coords"]] 
    a_vals <- data[["a"]]
    for (i in seq_along(a_vals)) {
	i <- coords[((i - 1) * 2) + 1]
        j <- coords[((i - 1) * 2) + 2]
        cat(sprintf("Cell (%d,%d) has data %d\n", i, j, a_vals[i]))
    }
}

create_array()
write_array()
read_array()
