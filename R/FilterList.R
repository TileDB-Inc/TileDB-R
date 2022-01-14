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

#' An S4 class for a TileDB filter list
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_filter_list
setClass("tiledb_filter_list",
         slots = list(ptr = "externalptr"))

tiledb_filter_list.from_ptr <- function(ptr) {
  stopifnot(`ptr must be a non-NULL externalptr to a tiledb_filter_list` = !missing(ptr) && is(ptr, "externalptr") && !is.null(ptr))
  return(new("tiledb_filter_list", ptr = ptr))
}

#' Constructs a `tiledb_filter_list` object
#'
#'
#' @param filters an optional list of one or more tiledb_filter_list objects
#' @param ctx tiledb_ctx object (optional)
#' @return tiledb_filter_list object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' filter_list
#'
#' @export tiledb_filter_list
tiledb_filter_list <- function(filters = c(), ctx = tiledb_get_context()) {
  stopifnot(`Argument 'ctx' must be a tiledb_ctx object` = is(ctx, "tiledb_ctx"))
  is_filter <- function(obj) is(obj, "tiledb_filter")
  if (is_filter(filters)) {             # if a filters object given:
    filters <- list(filters)            # make it a list so that lapply works below
  }
  filter_ptrs = c()
  if (length(filters) > 0) {
    if (!all(vapply(filters, is_filter, logical(1)))) {
      stop("filters argument must be a list of one or tiledb_filter objects")
    }
    filter_ptrs <- lapply(filters, function(obj) slot(obj, "ptr"))
  }
  ptr <- libtiledb_filter_list(ctx@ptr, filter_ptrs)
  return(new("tiledb_filter_list", ptr = ptr))
}

#' Prints a filter_list object
#'
#' @param object A filter_list object
#' @export
setMethod("show", signature(object = "tiledb_filter_list"),
          definition = function(object) {
    ## This is necessary as these are 0-up indexed (unusually for R, a leftover from older code here)
    sapply(seq_len(nfilters(object)), function(i) show(object[i-1]))
    invisible()
})

#' @rdname tiledb_filter_list_set_max_chunk_size
#' @export
setGeneric("set_max_chunk_size", function(object, value) standardGeneric("set_max_chunk_size"))

#' @rdname tiledb_filter_list_set_max_chunk_size
#' @export
setMethod("set_max_chunk_size",
          signature(object = "tiledb_filter_list", value = "numeric"),
          function(object, value) {
  libtiledb_filter_list_set_max_chunk_size(object@ptr, value)
})

#' Set the filter_list's max_chunk_size
#'
#' @param object tiledb_filter_list
#' @param value A numeric value
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' set_max_chunk_size(filter_list, 10)
#' @export
tiledb_filter_list_set_max_chunk_size <- function(object, value) {
  stopifnot(`The 'object' argument must be a tiledb_filter_list` = is(object, "tiledb_filter_list"),
            `The 'value' argument must be numeric` = is.numeric(value))
  libtiledb_filter_list_set_max_chunk_size(object@ptr, value)
}

#' @rdname tiledb_filter_list_get_max_chunk_size
#' @export
setGeneric("max_chunk_size", function(object) standardGeneric("max_chunk_size"))

#' @rdname tiledb_filter_list_get_max_chunk_size
#' @export
setMethod("max_chunk_size", signature(object = "tiledb_filter_list"), function(object) {
  libtiledb_filter_list_get_max_chunk_size(object@ptr)
})

#' Returns the filter_list's max_chunk_size
#'
#' @param object tiledb_filter_list
#' @return integer max_chunk_size
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' max_chunk_size(filter_list)
#'
#' @export
tiledb_filter_list_get_max_chunk_size <- function(object) {
  stopifnot(`The 'object' argument must be a tiledb_filter_list` = is(object, "tiledb_filter_list"))
  libtiledb_filter_list_get_max_chunk_size(object@ptr)
}


#' @rdname generics
#' @export
setGeneric("nfilters", function(object) standardGeneric("nfilters"))

#' Returns the filter_list's number of filters
#'
#' @param object tiledb_filter_list
#' @return integer number of filters
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' nfilters(filter_list)
#'
#' @export
setMethod("nfilters", signature(object = "tiledb_filter_list"),
          function(object) {
            libtiledb_filter_list_get_nfilters(object@ptr)
          })

#' Returns the filter at given index


#' @param x `tiledb_config` object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param drop Optional logical switch to drop dimensions, default false.
#' @return object tiledb_filter
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' filter_list[0]
#'
#' @export
#' @aliases [,tiledb_filter_list
#' @aliases [,tiledb_filter_list-method
#' @aliases [,tiledb_filter_list,ANY,tiledb_filter_list-method
#' @aliases [,tiledb_filter_list,ANY,ANY,tiledb_filter_list-method
setMethod("[", "tiledb_filter_list",
          function(x, i, j, ..., drop = FALSE) {
            tiledb_filter.from_ptr(libtiledb_filter_list_get_filter_from_index(x@ptr, i))
          })
