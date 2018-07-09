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
#' @param type (default "NO_COMPRESSION") TileDB compression type string
#' @param level (default -1) compression level, -1 will fallback to the compression algorithm's default
#' @return tiledb_compressor object
#' @examples 
#' c <- tiledb_compressor("ZSTD", 3)
#' c
#' 
#' @export tiledb_compressor
tiledb_compressor <- function(type = "NO_COMPRESSION", level = -1L) {
  if (!is.scalar(type, "character")) {
    stop("compressor argument must be scalar string")
  }
  if (!is.scalar(level, "double") && ! is.scalar(level, "integer")) {
    stop("level argument must be a integer or double scalar value")    
  } else {
    level <- as.integer(level)
  }
  ptr <- libtiledb_compressor(type, level)
  return(new("tiledb_compressor", ptr = ptr))
}

#' @export
setGeneric("compressor_type", function(object, ...) standardGeneric("compressor_type"))

#' Returns the compressor's compression type
#'
#' Compression library used
#'  
#' @param object tiledb_compressor
#' @return TileDB compression type string
#' @examples 
#' c <- tiledb_compressor("ZSTD", 3)
#' compressor_type(c)
#'
#' @export
setMethod("compressor_type", "tiledb_compressor",
          function(object) {
            return(libtiledb_compressor_type(object@ptr))  
          })

#' @export
setGeneric("compressor_level", function(object, ...) standardGeneric("compressor_level"))

#' Returns the compressor's compression level
#' 
#' @param object tiledb_compressor
#' @return integer level
#' @examples 
#' c <- tiledb_compressor("ZSTD", 3)
#' compressor_level(c)
#' 
#' @export
setMethod("compressor_level", "tiledb_compressor",
          function(object) {
            return(libtiledb_compressor_level(object@ptr));
          })

setMethod("show", "tiledb_compressor",
          function(object) {
            type <- compressor_type(object)
            level <- compressor_level(object)
            cat("tiledb_compressor(\"", type, "\", level = ", level, ")", sep="")
          })