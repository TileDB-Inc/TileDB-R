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

#' Creates a new TileDB array given an input schema.
#'
#' @param uri URI specifying path to create the TileDB array object
#' @param schema tiledb_array_schema object
#'
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' pth <- tempdir()
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
#' tiledb_array_create(pth, sch)
#' tiledb_object_type(pth)
#'
#' @export
tiledb_array_create <- function(uri, schema) {
  if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  } else if (missing(schema) || !is(schema, "tiledb_array_schema")) {
    stop("argument schema must a tiledb_array_schema")
  }
  return(libtiledb_array_create(uri, schema@ptr))
}

##' Open a TileDB Array
##'
##' @param arr A TileDB Array object as for example returned by `tiledb_dense()`
##' @param type A character value that must be either \sQuote{READ} or \sQuote{WRITE}
##' @return The TileDB Array object but opened for reading or writing
##' @export
tiledb_array_open <- function(arr, type=c("READ","WRITE")) {
  type <- match.arg(type)

  arr@ptr <- libtiledb_array_open_with_ptr(arr@ptr, type)
  arr
}

##' Close a TileDB Array
##'
##' @param arr A TileDB Array object as for example returned by `tiledb_dense()`
##' @return The TileDB Array object but closed
##' @export
tiledb_array_close <- function(arr) {
  libtiledb_array_close(arr@ptr)
  arr
}

##' Check for Homogeneous Domain
##'
##' @param arr A TileDB Array object
##' @return A boolean indicating if the array has homogeneous domains
##' @export
tiledb_array_is_homogeneous <- function(arr) {
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
  ## there is a non-exported call at the C level we could use instead
  sch <- schema(arr)
  dom <- domain(sch)
  domaintype <- sapply(libtiledb_domain_get_dimensions(dom@ptr),
                       libtiledb_dim_get_datatype)
  n <- length(unique(domaintype))
  n > 1
}
