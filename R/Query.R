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
#' @param ctx (optional) A TileDB Ctx object
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
#'   "COL_MAJOR", "ROW_MAJOR", "GLOBAL_ORDER", "UNORDERED")
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_layout <- function(query, layout=c("COL_MAJOR", "ROW_MAJOR",
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
#' general types see \code{tiledb_query_buffer_alloc_ptr} and
#' \code{tiledb_query_buffer_assign_ptr}
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

#' Allocate and populate a Query buffer for writing the given char vector
#'
#' @param query A TileDB Query object
#' @param varvec A vector of strings
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_create_buffer_ptr_char <- function(query, varvec) {
  stopifnot(query_object=is(query, "tiledb_query"),
            is_vector=is.vector(varvec))
  n <- length(varvec)
  offsets <- integer(n)
  data <- convertStringVectorIntoOffsetsAndString(varvec, offsets)
  bufptr <- libtiledb_query_buffer_var_char_create(offsets, data)
  bufptr
}

#' Allocate a Query buffer for reading a character attribute
#'
#' @param sizeoffsets An optional value of the size of the offsets vector
#' @param sizedata An optional value of the size of the data string
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_alloc_buffer_ptr_char <- function(sizeoffsets, sizedata) {
  bufptr <- libtiledb_query_buffer_var_char_alloc_direct(sizeoffsets, sizedata)
  bufptr
}

#' Allocate a Query buffer for reading a character attribute using a subarray
#'
#' Note that this uses an API part that may be deprecated in the future.
#' @param array A TileDB Array object
#' @param attr A character value containing the attribute
#' @param subarray A vector of length four describing the subarray required for dense arrays
#' @param sizeoffsets An optional value of the size of the offsets vector
#' @param sizedata An optional value of the size of the data string
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_alloc_buffer_ptr_char_subarray <- function(array, attr, subarray=NULL,
                                                        sizeoffsets=0, sizedata=0) {
  stopifnot(array_ptr=is(array@ptr, "externalptr"),
            is_vector=is.vector(subarray),
            attribute_string=is.character(attr))
  bufptr <- libtiledb_query_buffer_var_char_alloc(array@ptr, subarray, attr, sizeoffsets, sizedata)
  bufptr
}

#' Assign a buffer to a Query attribute
#'
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @param bufptr An external pointer with a query buffer
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_buffer_ptr_char <- function(query, attr, bufptr) {
  stopifnot(query_object=is(query, "tiledb_query"),
            attribute_string=is.character(attr),
            bufptr=is(bufptr, "externalptr"))
  libtiledb_query_set_buffer_var_char(query@ptr, attr, bufptr)
  invisible(query)

}

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

#' Allocate and populate a Query buffer for a given object of a given data type.
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

#' Retrieve content from a Query character buffer
#'
#' This function uses a query buffer for a character attribute
#' or dimension and returns its content.
#' @param bufptr An external pointer with a query buffer
#' @param sizeoffsets An optional argument for the length of the internal offsets vector
#' @param sizestring An optional argument for the length of the internal string
#' @return An R object as resulting from the query
#' @export
tiledb_query_get_buffer_char <- function(bufptr, sizeoffsets=0, sizestring=0) {
  stopifnot(bufptr=is(bufptr, "externalptr"))
  libtiledb_query_get_buffer_var_char(bufptr, sizeoffsets, sizestring)
}


#' Submit TileDB Query
#'
#' Note that the query object may need to be finalized
#' via \code{tiledb_query_finalize}.
#' @param query A TileDB Query object
#' @return The modified query object, invisibly
#' @export
tiledb_query_submit <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_submit(query@ptr)
  invisible(query)
}

#' Submit TileDB Query asynchronously without a callback returning immediately
#'
#' Note that the query object may need to be finalized
#' via \code{tiledb_query_finalize}.
#' @param query A TileDB Query object
#' @return The modified query object, invisibly
#' @export
tiledb_query_submit_async <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_submit_async(query@ptr)
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

#' Get TileDB Query result buffer element size
#'
#' The underlying library functions returns a pair of values as a vector
#' of length two. The first number is the number of element offsets for variable
#' size attributes (and always zero for fixed-sized attributes and coordinates).
#' The second is the number of elements in the data buffer. For variable-sized
#' attributes the first number is the number of cells read (and hence the number
#' of offsets), the second number is the number of elements in the data buffer.
#'
#' As this function was first made available when only a scalar (corresponding to
#' the second result) was returned, we still return that value.
#'
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @return A integer with the number of elements in the results buffer
#' for the given attribute
#' @seealso tiledb_query_result_buffer_elements_vec
#' @export
tiledb_query_result_buffer_elements <- function(query, attr) {
  stopifnot(query_object=is(query, "tiledb_query"),
            attr_variable=is.character(attr))
  libtiledb_query_result_buffer_elements(query@ptr, attr, 1) # request 2nd el in pair
}

#' Get TileDB Query result buffer element size pair as vector
#'
#' The underlying library functions returns a pair of values as a vector
#' of length two. The first number is the number of element offsets for variable
#' size attributes (and always zero for fixed-sized attributes and coordinates).
#' The second is the number of elements in the data buffer. For variable-sized
#' attributes the first number is the number of cells read (and hence the number
#' of offsets), the second number is the number of elements in the data buffer.
#' In the case of a nullable attribute, a third element is returned with the size of
#' the validity buffer.
#'
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @param nullable A logical variable that is \sQuote{TRUE} to signal that the attribute
#' is nullable, and \sQuote{FALSE} otherwise
#' @return A vector with the number of elements in the offsets buffer (and zero
#' for fixed-size attribute or dimensions), the number elements in the results
#' buffer for the given attribute, and (if nullable) a third element with the validity
#' buffer size.
#' @seealso tiledb_query_result_buffer_elements
#' @export
tiledb_query_result_buffer_elements_vec <- function(query, attr, nullable = FALSE) {
  stopifnot(`query must be a query_object` = is(query, "tiledb_query"),
            `attr must be a string` = is.character(attr),
            `nullable must be a logical` = is.logical(nullable))
  libtiledb_query_result_buffer_elements_vec(query@ptr, attr, nullable)
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

#' Set a range for a given query, also supplying type
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


#' Retrieve the Number of Fragments for Query
#'
#' This function is only applicable to \sQuote{WRITE} queries.
#' @param query A TileDB Query object
#' @return An integer with the number of fragments for the given query
#' @export
tiledb_query_get_fragment_num <- function(query) {
  stopifnot(query_object=is(query, "tiledb_query"))
  libtiledb_query_get_fragment_num(query@ptr)
}

#' Retrieve the URI for a given Query Fragment
#'
#' This function is only applicable to \sQuote{WRITE} queries.
#' @param query A TileDB Query object
#' @param idx An integer (or numeric) index ranging from zero to the number of fragments minus 1
#' @return An character value with the fragment URI
#' @export
tiledb_query_get_fragment_uri <- function(query, idx) {
  stopifnot(query_object=is(query, "tiledb_query"),
            idx_numeric=is.numeric(idx))
  libtiledb_query_get_fragment_uri(query@ptr, idx)
}

#' Retrieve the timestamp range for a given Query Fragment
#'
#' This function is only applicable to \sQuote{WRITE} queries. The time resolution in
#' TileDB is millseconds since the epoch so an R \code{Datetime} vector is returned.
#' @param query A TileDB Query object
#' @param idx An integer (or numeric) index ranging from zero to the number of fragments minus 1
#' @return A two-element datetime vector with the start and end time of the fragment write.
#' @export
tiledb_query_get_fragment_timestamp_range <- function(query, idx) {
  stopifnot(query_object=is(query, "tiledb_query"),
            idx_numeric=is.numeric(idx))
  libtiledb_query_get_fragment_uri(query@ptr, idx)
}


#' Retrieve the estimated result size for a query and attribute
#'
#' When reading from sparse arrays, one cannot know beforehand how big the
#' result will be (unless one actually executes the query). This function
#' offers a way to get the estimated result size for the given attribute.
#' As TileDB does not actually execute the query, getting the estimated
#' result is very fast.
#' @param query A TileDB Query object
#' @param name A variable with an attribute name
#' @return An estimate of the query result size
#' @export
tiledb_query_get_est_result_size <- function(query, name) {
  stopifnot(query_object=is(query, "tiledb_query"),
            name_argument=is.character(name))
  libtiledb_query_get_est_result_size(query@ptr, name)
}

#' Retrieve the estimated result size for a query and variable-sized attribute
#'
#' When reading variable-length attributes from either dense or sparse arrays,
#' one cannot know beforehand how big the result will be (unless one actually
#' executes the query). This function offers a way to get the estimated result
#' size for the given attribute. As TileDB does not actually execute the query,
#' getting the estimated result is very fast.
#' @param query A TileDB Query object
#' @param name A variable with an attribute name
#' @return An estimate of the query result size
#' @export
tiledb_query_get_est_result_size_var <- function(query, name) {
  stopifnot(query_object=is(query, "tiledb_query"),
            name_argument=is.character(name))
  libtiledb_query_get_est_result_size_var(query@ptr, name)
}

#' Retrieve the number of ranges for a query dimension
#'
#' @param query A TileDB Query object
#' @param idx An integer index selecting the dimension
#' @return An integer with the number of query range for the
#' given dimensions
#' @export
tiledb_query_get_range_num <- function(query, idx) {
  stopifnot(query_object=is(query, "tiledb_query"),
            idx_argument=is.numeric(idx))
  libtiledb_query_get_range_num(query@ptr, idx-1)
}

#' Retrieve the query range for a query dimension and range index
#'
#' @param query A TileDB Query object
#' @param dimidx An integer index selecting the dimension
#' @param rngidx An integer index selection the given range for the dimension
#' @return An integer vector with elements start, end and stride for the query
#' range for the given dimension and range index
#' @export
tiledb_query_get_range <- function(query, dimidx, rngidx) {
  stopifnot(query_object=is(query, "tiledb_query"),
            dimidx_argument=is.numeric(dimidx),
            rngidx_argument=is.numeric(rngidx))
  libtiledb_query_get_range(query@ptr, dimidx-1, rngidx-1)
}

#' Retrieve the query range for a variable-sized query dimension and range index
#'
#' @param query A TileDB Query object
#' @param dimidx An integer index selecting the dimension
#' @param rngidx An integer index selection the given range for the dimension
#' @return An string vector with elements start and end for the query
#' range for the given dimension and range index
#' @export
tiledb_query_get_range <- function(query, dimidx, rngidx) {
  stopifnot(query_object=is(query, "tiledb_query"),
            dimidx_argument=is.numeric(dimidx),
            rngidx_argument=is.numeric(rngidx))
  libtiledb_query_get_range_var(query@ptr, dimidx-1, rngidx-1)
}

#' Set a query combination object for a query
#'
#' @param query A TileDB Query object
#' @param qc A TileDB Query Combination object
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_condition <- function(query, qc) {
    stopifnot(`needs query object`=is(query, "tiledb_query"),
              `needs query condition object`=is(qc, "tiledb_query_condition"))
    query@ptr <- libtiledb_query_set_condition(query@ptr, qc@ptr)
    invisible(query)
}
