#  MIT License
#
#  Copyright (c) 2017-2022 TileDB Inc.
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
         ##           arr = "ANY"))
         ## could add arr of type 'ANY' (using shortcut to not have to deal with collate order)
         ## if array was needed for query object

#' Creates a 'tiledb_query' object
#'
#' @param array A TileDB Array object
#' @param type A character value that must be one of 'READ', 'WRITE', or
#' 'DELETE' (for TileDB >= 2.12.0)
#' @param ctx (optional) A TileDB Ctx object
#' @return 'tiledb_query' object
#' @export tiledb_query
tiledb_query <- function(array,
                         type = if (tiledb_version(TRUE) >= "2.12.0")
                                    c("READ", "WRITE", "DELETE", "MODIFY_EXCLUSIVE")
                                else
                                    c("READ", "WRITE"),
                         ctx = tiledb_get_context()) {
  stopifnot(`Argument 'arr' must be a tiledb_array object` = .isArray(array))
  type <- match.arg(type)
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
  layout <- match.arg(layout)
  libtiledb_query_set_layout(query@ptr, layout)
  invisible(query)
}

#' Get TileDB Query layout
#'
#' @param query A TileDB Query object
#' @return The TileDB Query layout as a string
#' @export
tiledb_query_get_layout <- function(query) {
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'attr' must be character_variable` = is.character(attr),
            `Argument 'buffer' must be integer, numeric or logical` = is.numeric(buffer) || is.logical(buffer))
  if (is.numeric(buffer) || tiledb_version(TRUE) < "2.10.0") {
      libtiledb_query_set_buffer(query@ptr, attr, buffer)
  } else {              # logical now maps to BOOL which is a uint8_t, we need a different approach
      nr <- NROW(buffer)
      bufptr <- libtiledb_query_buffer_alloc_ptr("BOOL", nr, FALSE, 1)
      bufptr <- libtiledb_query_buffer_assign_ptr(bufptr, "BOOL", buffer, FALSE)
      query@ptr <- libtiledb_query_set_buffer_ptr(query@ptr, attr, bufptr)
  }
  invisible(query)
}

#' Allocate and populate a Query buffer for writing the given char vector
#'
#' @param query A TileDB Query object
#' @param varvec A vector of strings
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_create_buffer_ptr_char <- function(query, varvec) {
  stopifnot("Argument 'query' must be a tiledb_query object" = is(query, "tiledb_query"),
            "Argument 'varvec' must be a character vector" = is.vector(varvec) && is.character(varvec))
  bufptr <- libtiledb_query_buffer_var_char_create(varvec, TRUE)
  bufptr
}

#' Allocate a Query buffer for reading a character attribute
#'
#' @param sizeoffsets A numeric value with the size of the offsets vector
#' @param sizedata A numeric value of the size of the data string
#' @param nullable An optional boolean indicating whether the column can have NULLs
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_alloc_buffer_ptr_char <- function(sizeoffsets, sizedata, nullable=FALSE) {
  stopifnot(`Argument 'sizeoffset' must be numeric` = is.numeric(sizeoffsets),
            `Argument 'sizedata' must be numeric` = is.numeric(sizedata))
  bufptr <- libtiledb_query_buffer_var_char_alloc_direct(sizeoffsets, sizedata, nullable)
  bufptr
}

# ' Allocate a Query buffer for reading a character attribute using a subarray
# '
# ' Note that this uses an API part that may be deprecated in the future.
# ' @param array A TileDB Array object
# ' @param attr A character value containing the attribute
# ' @param subarray A vector of length four describing the subarray required for dense arrays
# ' @param sizeoffsets An optional value of the size of the offsets vector
# ' @param sizedata An optional value of the size of the data string
# ' @return An external pointer to the allocated buffer object
# ' @export
## tiledb_query_alloc_buffer_ptr_char_subarray <- function(array, attr, subarray=NULL,
##                                                         sizeoffsets=0, sizedata=0) {
##   stopifnot(array_ptr=is(array@ptr, "externalptr"),
##             is_vector=is.vector(subarray),
##             attribute_string=is.character(attr))
##   bufptr <- libtiledb_query_buffer_var_char_alloc(array@ptr, subarray, attr, sizeoffsets, sizedata)
##   bufptr
## }

#' Assign a buffer to a Query attribute
#'
#' @param query A TileDB Query object
#' @param attr A character value containing the attribute
#' @param bufptr An external pointer with a query buffer
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_buffer_ptr_char <- function(query, attr, bufptr) {
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'attr' must be a character object` = is.character(attr),
            `Argument 'bufptr' must be an external pointer` = is(bufptr, "externalptr"))
  libtiledb_query_set_buffer_var_char(query@ptr, attr, bufptr)
  invisible(query)

}

#' Allocate a Query buffer for a given type
#'
#' This function allocates a query buffer for the given data type.
#' @param query A TileDB Query object
#' @param datatype A character value containing the data type
#' @param ncells A number of elements (not bytes)
#' @param nullable Optional boolean parameter indicating whether missing values
#' are allowed (for which another column is allocated), default is FALSE
#' @param varnum Option intgeter parameter for the number of elemements per variable,
#' default is one
#' @return An external pointer to the allocated buffer object
#' @export
tiledb_query_buffer_alloc_ptr <- function(query, datatype, ncells, nullable=FALSE, varnum=1) {
  stopifnot("Argument 'query' must be a tiledb_query object" = is(query, "tiledb_query"),
            "Argument 'datatype' must be a character object" = is.character(datatype),
            "Argument 'ncells' must be numeric" = is.numeric(ncells),
            "Argument 'nullable' must be logical" = is.logical(nullable),
            "Argument 'varnum' must be integer or numeric" = is.integer(varnum) || is.numeric(varnum))
  bufptr <- libtiledb_query_buffer_alloc_ptr(datatype, ncells, nullable, varnum)
  bufptr
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            #`Argument 'object' must be a vector` = is.vector(object),
            `Argument 'datatype' must be a character object` = is.character(datatype))
  ncells <- length(object)
  bufptr <- libtiledb_query_buffer_alloc_ptr(datatype, ncells)
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'attr' must be a character object` = is.character(attr),
            `Argument 'bufptr' must be an external pointer` = is(bufptr, "externalptr"))
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
  stopifnot(`Argument 'bufptr' must be an external pointer` = is(bufptr, "externalptr"))
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
  stopifnot(`Argument 'bufptr' must be an external pointer` = is(bufptr, "externalptr"))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
  libtiledb_query_submit_async(query@ptr)
  invisible(query)
}

#' Finalize TileDB Query
#'
#' @param query A TileDB Query object
#' @return A character value, either 'READ' or 'WRITE'
#' @export
tiledb_query_finalize <- function(query) {
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
  libtiledb_query_finalize(query@ptr)
  .pkgenv[["query_status"]] <- libtiledb_query_status(query@ptr)
  invisible(query)
}

#' Get TileDB Query status
#'
#' @param query A TileDB Query object
#' @return A character value describing the query status
#' @export
tiledb_query_status <- function(query) {
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'attr' must be a character object` = is.character(attr))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'attr' must be a character object` = is.character(attr),
            `Argument 'nullable' must be a logical` = is.logical(nullable))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'schema' must be a tiledb_array_schema object` = is(schema, "tiledb_array_schema"),
            `Argument 'attr' must be a character object` = is.character(attr),
            `Argument 'lowval' must be numeric` = is.numeric(lowval),
            `Argument 'highval' must be numeric` = is.numeric(highval),
            `Argument 'stride' must be numeric (or NULL)` = is.null(stride) || is.numeric(lowval))
  names <- tiledb_schema_get_names(schema)
  types <- tiledb_schema_get_types(schema)
  idx <- which(names == attr)
  query <- tiledb_query_add_range_with_type(query, idx-1L, types[idx], lowval, highval, stride)
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'idx' must be integer` = is.integer(idx),
            `Argument 'datatype' must be character` = is.character(datatype),
            `Argument 'lowval' must be numeric` = is.numeric(lowval),
            `Argument 'highval' must be numeric` = is.numeric(highval),
            `Argument 'stride' must be numeric (or NULL)` = is.null(stride) || is.numeric(lowval))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'idx' must be numeric` = is.numeric(idx))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'idx' must be numeric` = is.numeric(idx))
  libtiledb_query_get_fragment_timestamp_range(query@ptr, idx)
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'name' must be character` = is.character(name))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'name' must be character` = is.character(name))
  libtiledb_query_get_est_result_size_var(query@ptr, name)
}

#' Retrieve the number of ranges for a query dimension
#'
#' @param query A TileDB Query object
#' @param idx An integer or numeric index selecting the dimension
#' @return An integer with the number of query range for the
#' given dimensions
#' @export
tiledb_query_get_range_num <- function(query, idx) {
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'idx' must be numeric` = is.numeric(idx))
  libtiledb_query_get_range_num(query@ptr, idx-1)
}

#' Retrieve the query range for a query dimension and range index
#'
#' @param query A TileDB Query object
#' @param dimidx An integer or numeric index selecting the dimension
#' @param rngidx An integer or numeric index selection the given range for the dimension
#' @return An integer vector with elements start, end and stride for the query
#' range for the given dimension and range index
#' @export
tiledb_query_get_range <- function(query, dimidx, rngidx) {
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'dimidx' must be numeric` = is.numeric(dimidx),
            `Argument 'rngidx' must be numeric` = is.numeric(rngidx))
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
  stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
            `Argument 'dimidx' must be numeric` = is.numeric(dimidx),
            `Argument 'rngidx' must be numeric` = is.numeric(rngidx))
  libtiledb_query_get_range_var(query@ptr, dimidx-1, rngidx-1)
}

#' Set a query combination object for a query
#'
#' @param query A TileDB Query object
#' @param qc A TileDB Query Combination object
#' @return The modified query object, invisibly
#' @export
tiledb_query_set_condition <- function(query, qc) {
    stopifnot(`Argument 'query' must be a tiledb_query object` = is(query, "tiledb_query"),
              `Argument 'qc' must be a query_condition object` = is(qc, "tiledb_query_condition"))
    query@ptr <- libtiledb_query_set_condition(query@ptr, qc@ptr)
    invisible(query)
}

#' Retrieve the cached status of the last finalized query
#'
#' This function accesses the status of the last query without
#' requiring the query object.
#'
#' @return The status of the last query
#' @export
tiledb_get_query_status <- function() {
    .pkgenv[["query_status"]]
}

#' Return query statistics as a JSON string
#'
#' @param query A TileDB Query object
#' @return A JSON-formatted string with context statistics
#' @export
tiledb_query_stats <- function(query) {
    stopifnot(`The 'query' argument must be a TileDB Query object` = is(query, "tiledb_query"))
    libtiledb_query_stats(query@ptr)
}

#' Return query context object
#'
#' @param query A TileDB Query object
#' @return A TileDB Context object retrieved from the query
#' @export
tiledb_query_ctx <- function(query) {
    stopifnot(`The 'query' argument must be a TileDB Query object` = is(query, "tiledb_query"))
    new("tiledb_ctx", ptr = libtiledb_query_get_ctx(query@ptr))
}

## The next function could be used to extract an 'tiledb_array' object from a query object
## if we added it, but the only use so far as been in one test so there was not a strong enough
## case to extend the tiledb_query object with
# ' Return query array object
# '
# ' @param query A TileDB Query object
# ' @return A TileDB Array object retrieved from the query
# ' @ export
#tiledb_query_array <- function(query) {
#    stopifnot(`The 'query' must be a TileDB Query object` = is(query, "tiledb_query"))
#    query@arr
#}
