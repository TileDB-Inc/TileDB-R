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

#' An S4 class for a TileDB attribute
#'
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_attr
setClass("tiledb_attr",
         slots = list(ptr = "externalptr"))

tiledb_attr.from_ptr <- function(ptr) {
   if (typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb::Attribute instance")
  }
  new("tiledb_attr", ptr = ptr)
}

#' Contructs a `tiledb_attr` object
#'
#' @param name The dimension name / label string; if missing default `""` is used.
#' @param type The tiledb_attr TileDB datatype string; if missing the user is alerted
#' that this is a _required_ parameter.
#' @param filter_list (default filter_list("NONE")) The tiledb_attr filter_list
#' @param ncells (default 1) The number of cells, use \code{NA} to signal variable length
#' @param ctx tiledb_ctx object (optional)
#' @return `tiledb_dim` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' flt <- tiledb_filter_list(list(tiledb_filter("GZIP")))
#' attr <- tiledb_attr(name = "a1", type = "INT32",
#'                     filter_list = flt)
#' attr
#'
#' @importFrom methods new
#' @export
tiledb_attr <- function(name,
                        type,
                        filter_list = tiledb_filter_list(),
                        ncells = 1,
                        ctx = tiledb_get_context()
                        ) {
    if (missing(name)) {
        name <- ""
    }
    if (missing(type)) {
        stop("The 'type' argument for tiledb_attr() is mandatory")
    }
    if (!is(ctx, "tiledb_ctx")) {
        stop("ctx argument must be a tiledb_ctx")
    } else if (!is.scalar(name, "character")) {
        stop("name argument must be a scalar string")
    } else if(!is(filter_list, "tiledb_filter_list")) {
        stop("filter_list argument must be a tiledb_filter_list instance")
    }
    ptr <- libtiledb_attribute(ctx@ptr, name, type, filter_list@ptr, ncells)
    new("tiledb_attr", ptr = ptr)
}

#' Prints an attribute object
#'
#' @param object An attribute object
#' @export
setMethod("show", "tiledb_attr",
          function(object) {
            libtiledb_attribute_dump(object@ptr)
          })


#' @rdname generics
#' @export
setGeneric("name", function(object) standardGeneric("name"))

#' Return the `tiledb_attr` name
#'
#' @param object `tiledb_attr` object
#' @return string name, empty string if the attribute is anonymous
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' a1 <- tiledb_attr("a1", type = "INT32")
#' name(a1)
#'
#' a2 <- tiledb_attr(type = "INT32")
#' name(a2)
#'
#' @export
setMethod("name", signature(object = "tiledb_attr"),
          function(object) {
            libtiledb_attribute_get_name(object@ptr)
          })

#' @rdname generics
#' @export
setGeneric("datatype", function(object) standardGeneric("datatype"))

#' Return the `tiledb_attr` datatype
#'
#' @param object `tiledb_attr` object
#' @return tiledb datatype string
#' @examples
#' a1 <- tiledb_attr("a1", type = "INT32")
#' datatype(a1)
#'
#' a2 <- tiledb_attr("a1", type = "FLOAT64")
#' datatype(a2)
#'
#' @export
setMethod("datatype", signature(object = "tiledb_attr"),
          function(object) {
            libtiledb_attribute_get_type(object@ptr)
          })

#' Returns the `tiledb_filter_list` object associated with the given `tiledb_attr`
#'
#' @param object tiledb_attr
#' @return a tiledb_filter_list object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' attr <- tiledb_attr(type = "INT32", filter_list=tiledb_filter_list(list(tiledb_filter("ZSTD"))))
#' filter_list(attr)
#'
#' @export
setMethod("filter_list", "tiledb_attr",
          function(object) {
            ptr <- libtiledb_attribute_get_filter_list(object@ptr)
            return(tiledb_filter_list.from_ptr(ptr))
          })

#' @rdname generics
#' @export
setGeneric("cell_val_num", function(object) standardGeneric("cell_val_num"))

#' Return the number of scalar values per attribute cell
#'
#' @param object `tiledb_attr` object
#' @return integer number of cells
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' a1 <- tiledb_attr("a1", type = "FLOAT64", ncells = 1)
#' cell_val_num(a1)
#'
#' @export
setMethod("cell_val_num", signature(object = "tiledb_attr"),
          function(object) {
            libtiledb_attribute_get_cell_val_num(object@ptr)
          })

#' Returns TRUE if the tiledb_dim is anonymous
#'
#' A TileDB attribute is anonymous if no name/label is defined
#'
#' @param object `tiledb_attr` object
#' @return TRUE or FALSE
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' a1 <- tiledb_attr("a1", type = "FLOAT64")
#' is.anonymous(a1)
#'
#' a2 <- tiledb_attr("", type = "FLOAT64")
#' is.anonymous(a2)
#'
#' @export
is.anonymous <- function(object) UseMethod("is.anonymous", object)

#' @rdname is.anonymous
#' @export
is.anonymous.tiledb_attr <- function(object) {
  name <- libtiledb_attribute_get_name(object@ptr)
  nchar(name) == 0
}
