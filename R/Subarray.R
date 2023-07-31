#  MIT License
#
#  Copyright (c) 2017-2023 TileDB Inc.
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

#' An S4 class for a TileDB Subarray
#'
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_subarray
setClass("tiledb_subarray",
         slots = list(ptr = "externalptr"))

tiledb_subarray.from_ptr <- function(ptr) {
    stopifnot("ptr must be a non-NULL externalptr to a tiledb_subarray" =
                  !missing(ptr) && is(ptr, "externalptr") && !is.null(ptr))
    return(new("tiledb_subarray", ptr = ptr))
}

#' Constructs a `tiledb_subarray` object from a TileDB Query
#'
#' @param query A TileDB Query Object
#' @return tiledb_subarray object
#' @export
tiledb_subarray <- function(query) {
    stopifnot("Argument 'query' must be a tiledb_query object" = is(query, "tiledb_query"))
    ptr <- libtiledb_subarray(query@ptr)
    return(new("tiledb_subarray", ptr = ptr))
}

#' Apply a Subarray to a Query
#'
#' @param query A TileDB Query Object
#' @param subarray A TileDB Subarray Object
#' @return tiledb_query object
#' @export
tiledb_subarray_to_query <- function(query, subarray) {
    stopifnot("Argument 'query' must be a tiledb_query object" = is(query, "tiledb_query"),
              "Argument 'subarray' must be a tiledb_subarray" = is(subarray, "tiledb_subarray"))
    query@ptr <- libtiledb_query_set_subarray_object(query@ptr, subarray@ptr)
    query
}
