#  MIT License
#
#  Copyright (c) 2017-2020 TileDB Inc.
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

#' An S4 class for a TileDB Query object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_query
setClass("tiledb_query",
         slots = list(ptr = "externalptr"))

#' Creates a 'tiledb_query' object
#'
#' @param array A TileDB Array object
#' @param type A character value that must be one of 'READ' or 'WRITE'
#' @param ctx (optonal) A TileDB Ctx object
#' @return 'tiledb_query' object
#' @export tiledb_query
tiledb_query <- function(array, type = c("READ", "WRITE"), ctx = tiledb_get_context()) {
  type <- match.arg(type)
  stopifnot(valid_array=is(array, "tiledb_array") ||
              is(array, "tiledb_dense") || is(array, "tiledb_sparse"))
  arrary <- tiledb_array_open(array, type)
  ptr <- libtiledb_query(ctx@ptr, array@ptr, type)
  query <- new("tiledb_query", ptr = ptr)
  invisible(query)
}

#' Return TileDB Query type
#'
#' @param query A TileDB Query object
#' @return A character value, either 'READ' or 'WRITE'
#' @export
tiledb_query_type <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_type(query@ptr)
}

#' Set TileDB Query layout
#'
#' @param query A TileDB Query object
#' @param layout A character variable with the layout; must be one of
#'   "ROW_MAJOR", "COL_MAJOR", "GLOBAL_ORDER", "UNORDERED")
#' @return The modified query object, insivisibly
#' @export
tiledb_set_layout_type <- function(query, layout=c("ROW_MAJOR", "COL_MAJOR",
                                                   "GLOBAL_ORDER", "UNORDERED")) {
  layout <- match.arg(layout)
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_set_layout(query@ptr, layout)
  invisible(query)
}

#' Set TileDB Query buffer
#'
#' This function allocations query buffers directly from R vectors in case the types
#' match: `integer`, `double`, `logical`. For more general types see ...
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @param buffer A vector providing the query buffer
#' @return The modified query object, insivisibly
#' @export
tiledb_query_set_buffer <- function(query, attr, buffer) {
  stopifnot(query_object=is(query, "tiledb_query"),
            attr_variable=is.character(attr))
  libtiledb_query_set_buffer(query@ptr, attr, buffer)
  invisible(query)
}

#' Submit TileDB Query
#'
#' @param query A TileDB Query object
#' @return The modified query object, insivisibly
#' @export
#'
tiledb_query_submit <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_submit(query@ptr)
  invisible(query)
}

#' Finalize TileDB Query
#'
#' @param query A TileDB Query object
#' @return A character value, either 'READ' or 'WRITE'
#' @export
tiledb_query_finalize <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_finalize(query@ptr)
  invisible(query)
}

#' Get TileDB Query status
#'
#' @param query A TileDB Query object
#' @return A character value describing the query status
#' @export
tiledb_query_status <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_status(query@ptr)
}

#' Get TileDB Query result buffer elements
#'
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @return A integer with the number of elements in the results buffer
#' for the given attribute
#' @export
tiledb_query_result_buffer_elements <- function(query, attr) {
  stopifnot(query_object=is(query, "tiledb_query"),
            attr_variable=is.character(attr))
  libtiledb_query_result_buffer_elements(query@ptr, attr)
}
