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

#' An S4 class for a TileDB filter
#'
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_filter
setClass("tiledb_filter",
         slots = list(ptr = "externalptr"))

tiledb_filter.from_ptr <- function(ptr) {
  stopifnot("ptr must be a non-NULL externalptr to a tiledb_filter" = !missing(ptr) && is(ptr, "externalptr") && !is.null(ptr))
  return(new("tiledb_filter", ptr = ptr))
}

#' Constructs a `tiledb_filter` object
#'
#' Available filters:
#'   - "NONE"
#'   - "GZIP"
#'   - "ZSTD"
#'   - "LZ4"
#'   - "RLE"
#'   - "BZIP2"
#'   - "DOUBLE_DELTA"
#'   - "BIT_WIDTH_REDUCTION"
#'   - "BITSHUFFLE"
#'   - "BYTESHUFFLE"
#'   - "POSITIVE_DELTA"
#'   - "CHECKSUM_MD5"
#'   - "CHECKSUM_SHA256"
#'   - "DICTIONARY"
#'
#' Valid compression options vary depending on the filter used,
#' consult the TileDB docs for more information.
#'
#' @param name (default "NONE") TileDB filter name string
#' @param ctx tiledb_ctx object (optional)
#' @return tiledb_filter object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' tiledb_filter("ZSTD")
#'
#' @export tiledb_filter
tiledb_filter <- function(name = "NONE", ctx = tiledb_get_context()) {
  stopifnot("Argument 'ctx' must be a tiledb_ctx object" = is(ctx, "tiledb_ctx"),
            "Argument 'filter' must be scalar string" = is.scalar(name, "character"))
  ptr <- libtiledb_filter(ctx@ptr, name)
  return(new("tiledb_filter", ptr = ptr))
}

# internal function returning text use here and in other higher-level show() methods
.as_text_filter <- function(object) {
    flt <- tiledb_filter_type(object)
    opt <- .getFilterOption(object)
    if (opt == "NA") {
        txt <- paste0("tiledb_filter(\"", flt, "\")")
    } else {
        prt <- strsplit(opt, "=")[[1]]
        txt <- paste0("tiledb_filter_set_option(tiledb_filter(\"",
                      flt, "\"),\"", prt[1], "\",", prt[2], ")")
    }
    txt
}

#' Prints a filter object
#'
#' @param object A filter object
#' @export
setMethod("show",
          signature(object = "tiledb_filter"),
          definition = function(object) {
    cat(.as_text_filter(object), "\n")
})

#' Returns the type of the filter used
#'
#' @param object tiledb_filter
#' @return TileDB filter type string
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' c <- tiledb_filter("ZSTD")
#' tiledb_filter_type(c)
#'
#' @export
tiledb_filter_type <- function(object) {
  stopifnot("The 'object' argument must be a tiledb_filter" = is(object, "tiledb_filter"))
  return(libtiledb_filter_get_type(object@ptr))
}

#' Set the option for a filter
#'
#' @param object tiledb_filter
#' @param option string
#' @param value int
#' @return The modified filter object is returned.
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' c <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(c,"COMPRESSION_LEVEL", 5)
#' tiledb_filter_get_option(c, "COMPRESSION_LEVEL")
#' @export
tiledb_filter_set_option <- function(object, option, value) {
  stopifnot("The 'object' argument must be a tiledb_filter" = is(object, "tiledb_filter"),
            "The 'option' argument must be character" = is.character(option),
            "The 'value' argument must be numeric or character or logical" = is.numeric(value) || is.character(value) || is.logical(value))

  object@ptr <- libtiledb_filter_set_option(object@ptr, option, value)
  return(object)
}

#' Returns the filter's option
#'
#' @param object tiledb_filter
#' @param option string
#' @return Integer value
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' c <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(c,"COMPRESSION_LEVEL", 5)
#' tiledb_filter_get_option(c, "COMPRESSION_LEVEL")
#'
#' @export
tiledb_filter_get_option <- function(object, option) {
  stopifnot("The 'object' argument must be a tiledb_filter" = is(object, "tiledb_filter"),
            "The 'option' argument must be character" = is.character(option))
  return(libtiledb_filter_get_option(object@ptr, option))
}
