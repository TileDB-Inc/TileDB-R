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
  array <- tiledb_array_open(array, type)
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
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_layout <- function(query, layout=c("ROW_MAJOR", "COL_MAJOR",
                                                    "GLOBAL_ORDER", "UNORDERED")) {
  layout <- match.arg(layout)
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_set_layout(query@ptr, layout)
  invisible(query)
}

#' Get TileDB Query layout
#'
#' @param query A TileDB Query object
#' @return The TileDB Query layout as a string
#' @export
tiledb_query_get_layout <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_layout(query@ptr)
}

#' Set subarray for TileDB Query object
#'
#' @param query A TileDB Query object
#' @param subarray A subarry vector object
#' @param type An optional type as a character, if missing type is
#'   inferred from the vector.
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_subarray <- function(query, subarray, type) {
  stopifnot(query_object=is(query, "tiledb_query"))
  if (missing(type)) {
    libtiledb_query_set_subarray(query@ptr, subarray)
  } else {
    libtiledb_query_set_subarray_with_type(query@ptr, subarray, type)
  }
  invisible(query)
}

#' Set TileDB Query buffer
#'
#' This function allocates query buffers directly from R vectors in
#' case the types match: `integer`, `double`, `logical`. For more
#' general types see \code{tiledb_query_buffer_alloc_ptr}.
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @param buffer A vector providing the query buffer
#' @return The modified query object, invisisibly
#' @export
tiledb_query_set_buffer <- function(query, attr, buffer) {
  stopifnot(query_object=is(query, "tiledb_query"),
            attr_variable=is.character(attr))
  libtiledb_query_set_buffer(query@ptr, attr, buffer)
  invisible(query)
}

## TODO var char support
##
## There are four function for vlc_buf_t -- variable length char buffer type
##
##   XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_alloc_direct(int szoffsets,
##                                                                int szdata)
##    - allocates directly for reads given offset and data vector sizes
##
##   XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_create(IntegerVector intoffsets,
##                                                          std::string data)
##    - creates / allocates for writes from given offsets and data vecroe
##
##   XPtr<tiledb::Query> libtiledb_query_set_buffer_var_char(XPtr<tiledb::Query> query,
##                                                           std::string attr,
##                                                           XPtr<vlc_buf_t> bufptr)
##     - sets a buffer to the query
##
##   CharacterMatrix libtiledb_query_get_buffer_var_char(XPtr<vlc_buf_t> bufptr,
##                                                       int32_t len=0, int32_t nchar=0)
##     - retrieves a query buffer's content as Char matrix



#' Allocate a Query buffer for a given type
#'
#' This function allocates a query buffer for the given data type.
#' @param query A TileDB Query object
#' @param datatype A character value containing the data type
#' @param ncells A number of elements (not bytes)
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_buffer_alloc_ptr <- function(query, datatype, ncells) {
  stopifnot(query_object=is(query, "tiledb_query"),
            datatype_string=is.character(datatype))
  bufptr <- libtiledb_query_buffer_alloc_ptr(query@ptr, datatype, ncells)
}

#' Allocate, populate and set a Query buffer for a given object of a given data type.
#'
#' This function allocates a query buffer for the given data object of the given
#' type and assigns the object content to the buffer.
#' @param query A TileDB Query object
#' @param datatype A character value containing the data type
#' @param object A vector object of the given type
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_create_buffer_ptr <- function(query, datatype, object) {
  stopifnot(query_object=is(query, "tiledb_query"),
            #is_vector=is.vector(object),
            datatype_string=is.character(datatype))
  ncells <- length(object)
  bufptr <- libtiledb_query_buffer_alloc_ptr(query@ptr, datatype, ncells)
  bufptr <- libtiledb_query_buffer_assign_ptr(bufptr, datatype, object)
  bufptr
}

#' Assigns to a Query buffer for a given attribute
#'
#' This function assigns a given query buffer to a query.
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @param bufptr An external pointer with a query buffer
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_buffer_ptr <- function(query, attr, bufptr) {
  stopifnot(query_object=is(query, "tiledb_query"),
            attribute_string=is.character(attr),
            bufptr=is(bufptr, "externalptr"))
  libtiledb_query_set_buffer_ptr(query@ptr, attr, bufptr)
  invisible(query)
}

#' Retrieve content from a Query buffer
#'
#' This function uses a query buffer and returns its content.
#' @param bufptr An external pointer with a query buffer
#' @return An R object as resulting from the query
#' @export
tiledb_query_get_buffer_ptr <- function(bufptr) {
  stopifnot(bufptr=is(bufptr, "externalptr"))
  libtiledb_query_get_buffer_ptr(bufptr)
}


#' Submit TileDB Query
#'
#' @param query A TileDB Query object
#' @return The modified query object, invisibly
#' @export
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

#' Set a range for a given query
#'
#' @param query A TileDB Query object
#' @param schema A TileDB Schema object
#' @param attr An character variable with a dimension name for which the range is set
#' @param lowval The lower value of the range to be set
#' @param highval The higher value of the range to be set
#' @param stride An optional stride value for the range to be set
#' @return The query object, invisibly
#' @export
tiledb_query_add_range <- function(query, schema, attr, lowval, highval, stride=NULL) {
  stopifnot(schema_object=is(schema, "tiledb_array_schema"),
            query_object=is(query, "tiledb_query"),
            datatype_variable=is.character(attr))
  names <- tiledb_schema_get_names(schema)
  types <- tiledb_schema_get_types(schema)
  idx <- which(names == attr)
  query <- tiledb_query_add_range_with_type(query, idx-1, types[idx], lowval, highval, stride)
  invisible(query)
}

#' Set a range for a given query
#'
#' @param query A TileDB Query object
#' @param idx An integer index, zero based, of the dimensions
#' @param datatype A character value containing the data type
#' @param lowval The lower value of the range to be set
#' @param highval The highre value of the range to be set
#' @param stride An optional stride value for the range to be set
#' @return The query object, invisibly
#' @export
tiledb_query_add_range_with_type <- function(query, idx, datatype, lowval, highval, stride=NULL) {
  stopifnot(query_object=is(query, "tiledb_query"),
            datatype_variable=is.character(datatype))
  query@ptr <- libtiledb_query_add_range_with_type(query@ptr, idx, datatype, lowval, highval, stride)
  invisible(query)
}
