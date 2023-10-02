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

#' Creates a new TileDB array given an input schema.
#'
#' @param uri URI specifying path to create the TileDB array object
#' @param schema tiledb_array_schema object
#' @param encryption_key optional A character value with an AES-256 encryption key
#' in case the array should be encryption.
#'
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' \dontrun{
#' pth <- tempdir()
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
#' tiledb_array_create(pth, sch)
#' tiledb_object_type(pth)
#' }
#'
#' @export
tiledb_array_create <- function(uri, schema, encryption_key) {
    stopifnot(`The 'uri' argument must be a string scalar` = !missing(uri) && is.scalar(uri, "character"),
              `The 'schema' argument must a tiledb_array_schema object` = !missing(schema) && is(schema, "tiledb_array_schema"))
    if (missing(encryption_key)) {
        invisible(libtiledb_array_create(uri, schema@ptr))
    } else {
        invisible(libtiledb_array_create_with_key(uri, schema@ptr, encryption_key))
    }
}

##' Open a TileDB Array
##'
##' @param arr A TileDB Array object as for example returned by `tiledb_array()`
##' @param type A character value that must be either \sQuote{READ}, \sQuote{WRITE}
##' or (for TileDB 2.12.0 or later) \sQuote{DELETE} or \sQuote{MODIFY_EXCLUSIVE}
##' @return The TileDB Array object but opened for reading or writing
##' @importFrom methods .hasSlot
##' @export
tiledb_array_open <- function(arr,
                              type = if (tiledb_version(TRUE) >= "2.12.0")
                                         c("READ", "WRITE", "DELETE", "MODIFY_EXCLUSIVE")
                                     else
                                         c("READ", "WRITE")) {
  stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr))
  type <- match.arg(type)

  if (.hasSlot(arr, "encryption_key") && length(arr@encryption_key) > 0) {
    ctx <- tiledb_get_context()
    arr@ptr <- libtiledb_array_open_with_key(ctx@ptr, arr@uri, type, arr@encryption_key)
  } else {
    arr@ptr <- libtiledb_array_open_with_ptr(arr@ptr, type)
  }
  arr
}

##' Open a TileDB Array at Timestamp
##'
##' @param arr A TileDB Array object as for example returned by \code{tiledb_array()}
##' @param type A character value that must be either \sQuote{READ} or \sQuote{WRITE}
##' @param timestamp A Datetime object that will be converted to millisecond granularity
##' @return The TileDB Array object but opened for reading or writing
##' @export
tiledb_array_open_at <- function(arr, type=c("READ","WRITE"), timestamp) {
  stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr),
            "The 'timestamp' argument must a time object" = inherits(timestamp, "POSIXct"))
  type <- match.arg(type)
  ctx <- tiledb_get_context()
  if (.hasSlot(arr, "encryption_key") && length(arr@encryption_key) > 0) {
    arr@ptr <- libtiledb_array_open_at_with_key(ctx@ptr, arr@uri, type, arr@encryption_key, timestamp)
  } else {
    arr@ptr <- libtiledb_array_open_at(ctx@ptr, arr@uri, type, timestamp)
  }
  arr
}


##' Close a TileDB Array
##'
##' @param arr A TileDB Array object as for example returned by `tiledb_array()`
##' @return The TileDB Array object but closed
##' @export
tiledb_array_close <- function(arr) {
  stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr))
  libtiledb_array_close(arr@ptr)
  arr
}

##' Test if TileDB Array is open
##'
##' @param arr A TileDB Array object as for example returned by `tiledb_array()`
##' @return A boolean indicating whether the TileDB Array object is open
##' @export
tiledb_array_is_open <- function(arr) {
    stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr))
    libtiledb_array_is_open(arr@ptr)
}

##' Check for Homogeneous Domain
##'
##' @param arr A TileDB Array object
##' @return A boolean indicating if the array has homogeneous domains
##' @export
tiledb_array_is_homogeneous <- function(arr) {
  stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr))
  ## there is a non-exported call at the C level we could use instead
  sch <- schema(arr)
  dom <- domain(sch)
  domaintype <- sapply(libtiledb_domain_get_dimensions(dom@ptr),
                       libtiledb_dim_get_datatype)
  n <- length(unique(domaintype))
  n == 1
}

##' Check for Heterogeneous Domain
##'
##' @param arr A TileDB Array object
##' @return A boolean indicating if the array has heterogenous domains
##' @export
tiledb_array_is_heterogeneous <- function(arr) {
  stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr))
  ## there is a non-exported call at the C level we could use instead
  sch <- schema(arr)
  dom <- domain(sch)
  domaintype <- sapply(libtiledb_domain_get_dimensions(dom@ptr),
                       libtiledb_dim_get_datatype)
  n <- length(unique(domaintype))
  n > 1
}

##' Delete fragments written between the start and end times given
##'
##' @param arr A TileDB Array object as for example returned by \code{tiledb_array()}
##' @param ts_start A Datetime object that will be converted to millisecond granularity
##' @param ts_end A Datetime object that will be converted to millisecond granularity
##' @param ctx A tiledb_ctx object (optional)
##' @return A boolean indicating success
##' @export
tiledb_array_delete_fragments <- function(arr, ts_start, ts_end, ctx = tiledb_get_context()) {
    stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr),
              "The 'ts_start' argument must a time object" = inherits(ts_start, "POSIXct"),
              "The 'ts_end' argument must a time object" = inherits(ts_end, "POSIXct"))
    libtiledb_array_delete_fragments(ctx@ptr, arr@ptr, ts_start, ts_end)
    invisible(TRUE)
}

##' Check for Enumeration (aka Factor aka Dictionary)
##'
##' @param arr A TileDB Array object
##' @return A boolean indicating if the array has homogeneous domains
##' @export
tiledb_array_has_enumeration <- function(arr) {
    stopifnot("The 'arr' argument must be a tiledb_array object" = .isArray(arr))
    ctx <- tiledb_get_context()
    if (!tiledb_array_is_open(arr)) {
        arr <- tiledb_array_open(arr, "READ")
        on.exit(tiledb_array_close(arr))
    }
    return(libtiledb_array_has_enumeration_vector(ctx@ptr, arr@ptr))
}
