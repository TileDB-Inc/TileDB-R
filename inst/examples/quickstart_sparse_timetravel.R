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
array_name <- "quickstart_sparse_timetravel"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(array_name) {
    # Check if the array already exists.
    if (tiledb_object_type(array_name) == "ARRAY") {
        message("Array already exists, removing to create new one.")
        tiledb_vfs_remove_dir(array_name)
    }

    # The array will be 10x10 with dimensions "rows" and "cols", with domain [1,10].
    dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 10L), 5L, "INT32"),
                                  tiledb_dim("cols", c(1L, 10L), 5L, "INT32")))

   # The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
    schema <- tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "INT32")), sparse = TRUE)

    # Create the (empty) array on disk.
    invisible( tiledb_array_create(array_name, schema) )
}

write_array <- function(array_name) {
    I <- c(1, 2, 2)
    J <- c(1, 4, 3)
    data <- c(1L, 2L, 3L)
    # Open the array and write to it.
    now1 <- Sys.time()
    cat("Writing first record at", format(now1), "\n")
    A <- tiledb_array(uri = array_name, timestamp=now1)
    A[I, J] <- data

    cat("(...sleeping 1s ...)\n")
    Sys.sleep(1)
    now2 <- Sys.time()
    I <- c(8, 6, 9)
    J <- c(5, 7, 8)
    data <- c(11L, 22L, 33L)
    cat("Writing second record at", format(now2), "\n")
    A <- tiledb_array(uri = array_name, timestamp=now2)
    A[I, J] <- data

    c(now1, now2)
}

read_array <- function(array_name) {
    cat("\nReading everything:\n")
    # Open the array and read as a data.frame from it.
    A <- tiledb_array(uri = array_name, as.data.frame=TRUE)
    A[]
}

read_array_at <- function(array_name, tstamps) {
    ## Read before tstamp[1]
    cat("\nOpening / reading 0.5s before first tstamp\n")
    A <- tiledb_array(uri = array_name, as.data.frame=TRUE, timestamp=tstamps[1] - 0.5)
    print(A[])

    cat("Opening / reading 0.5s after first tstamp\n")
    A <- tiledb_array(uri = array_name, as.data.frame=TRUE, timestamp=tstamps[1] + 0.5)
    print(A[])

    cat("Opening / reading 0.5s before second tstamp\n")
    A <- tiledb_array(uri = array_name, as.data.frame=TRUE, timestamp=tstamps[2] - 0.5)
    print(A[])

    cat("Opening / reading 0.5s after second tstamp\n")
    A <- tiledb_array(uri = array_name, as.data.frame=TRUE, timestamp=tstamps[2] + 0.5)
    print(A[])

}


create_array(uri)
tstamps <- write_array(uri)
read_array(uri)
read_array_at(uri, tstamps)
