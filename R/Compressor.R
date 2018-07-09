#' @exportClass tiledb_compressor
setClass("tiledb_compressor",
         slots = list(ptr = "externalptr"))

tiledb_compressor.from_ptr <- function(ptr) {
  stopifnot(is(ptr, "externalptr"))
  return(new("tiledb_compressor", ptr = ptr))
}

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

#' @export
setMethod("compressor_type", "tiledb_compressor",
          function(object) {
            return(libtiledb_compressor_type(object@ptr))  
          })

#' @export
setGeneric("compressor_level", function(object, ...) standardGeneric("compressor_level"))

#' @export
setMethod("compressor_level", "tiledb_compressor",
          function(object) {
            return(libtiledb_compressor_level(object@ptr));
          })

setMethod("show", "tiledb_compressor",
          function(object) {
            type <- compressor_type(object)
            level <- compression_level(object)
            cat("tiledb_compressor(\"", type, "\", level = ", level, ")", sep="")
          })