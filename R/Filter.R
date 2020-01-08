#' An S4 class for a TileDB filter
#'
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_filter
setClass("tiledb_filter",
         slots = list(ptr = "externalptr"))

tiledb_filter.from_ptr <- function(ptr) {
  stopifnot(is(ptr, "externalptr"))
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
#'
#' Valid compression options vary depending on the filter used,
#' consult the TileDB docs for more information.
#'
#' @param name (default "NONE") TileDB filter name string
#' @param ctx tiledb_ctx object (optional)
#' @return tiledb_filter object
#' @examples
#' tiledb_filter("ZSTD")
#'
#' @export tiledb_filter
tiledb_filter <- function(name = "NONE", ctx = tiledb:::ctx) {
  if (!is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  } else if (!is.scalar(name, "character")) {
    stop("filter argument must be scalar string")
  }
  ptr <- libtiledb_filter(ctx@ptr, name)
  return(new("tiledb_filter", ptr = ptr))
}

#' Returns the type of the filter used
#'
#' @param object tiledb_filter
#' @return TileDB filter type string
#' @examples
#' c <- tiledb_filter("ZSTD")
#' tiledb_filter_type(c)
#'
#' @export
tiledb_filter_type <- function(object) {
  stopifnot(is(object, "tiledb_filter"))
  return(libtiledb_filter_type(object@ptr))
}

#' Set the filter's option
#'
#' @param object tiledb_filter
#' @param option string
#' @param value int
#' @examples
#' c <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(c,"COMPRESSION_LEVEL", 5)
#' tiledb_filter_get_option(c, "COMPRESSION_LEVEL")
#' @export
tiledb_filter_set_option <- function(object, option, value) {
  stopifnot(is(object, "tiledb_filter"))
  return(libtiledb_filter_set_option(object@ptr, option, value))
}

#' Returns the filter's option
#'
#' @param object tiledb_filter
#' @param option string
#' @return Integer value
#' @examples
#' c <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(c,"COMPRESSION_LEVEL", 5)
#' tiledb_filter_get_option(c, "COMPRESSION_LEVEL")
#'
#' @export
tiledb_filter_get_option <- function(object, option) {
  stopifnot(is(object, "tiledb_filter"))
  return(libtiledb_filter_get_option(object@ptr, option))
}
