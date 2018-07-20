#' @exportClass tiledb_compressor
setClass("tiledb_compressor",
         slots = list(ptr = "externalptr"))

tiledb_compressor.from_ptr <- function(ptr) {
  stopifnot(is(ptr, "externalptr"))
  return(new("tiledb_compressor", ptr = ptr))
}

#' Constructs a `tiledb_compressor` object
#'
#' Available compressors:
#'   - "GZIP"
#'   - "ZSTD"
#'   - "LZ4"
#'   - "BLOSC_LZ"
#'   - "BLOSC_LZ4"
#'   - "BLOSC_LZ4HC"
#'   - "BLOSC_SNAPPY"
#'   - "BLOSC_ZSTD"
#'   - "RLE"
#'   - "BZIP2"
#'   - "DOUBLE_DELTA"
#'
#' Valid compression levels vary depending on the compressor used,
#' consult the TileDB docs for more information.
#'
#' @param name (default "NO_COMPRESSION") TileDB compressor name string
#' @param level (default -1) compression level, -1 will fallback to the compression algorithm's default
#' @return tiledb_compressor object
#' @examples
#' c <- tiledb_compressor("ZSTD", 3)
#' c
#'
#' @export tiledb_compressor
tiledb_compressor <- function(name = "NO_COMPRESSION", level = -1L) {
  if (!is.scalar(name, "character")) {
    stop("compressor argument must be scalar string")
  }
  if (!is.scalar(level, "double") && ! is.scalar(level, "integer")) {
    stop("level argument must be a integer or double scalar value")
  } else {
    level <- as.integer(level)
  }
  ptr <- libtiledb_compressor(name, level)
  return(new("tiledb_compressor", ptr = ptr))
}

#' Returns the name of the compression library used
#'
#' @param object tiledb_compressor
#' @return TileDB compression name string
#' @examples
#' c <- tiledb_compressor("ZSTD", 3)
#' tiledb_compressor_name(c)
#'
#' @export
tiledb_compressor_name <- function(object) {
  stopifnot(is(object, "tiledb_compressor"))
  return(libtiledb_compressor_type(object@ptr))
}

#' Returns the compressor's compression level
#'
#' @param object tiledb_compressor
#' @return integer level
#' @examples
#' c <- tiledb_compressor("ZSTD", 3)
#' compressor_level(c)
#'
#' @export
tiledb_compressor_level <- function(object) {
  stopifnot(is(object, "tiledb_compressor"))
  return(libtiledb_compressor_level(object@ptr))
}

setMethod("show", "tiledb_compressor",
          function(object) {
            name <- tiledb_compressor_name(object)
            level <- compressor_level(object)
            cat("tiledb_compressor(\"", name, "\", level = ", level, ")", sep="")
          })
