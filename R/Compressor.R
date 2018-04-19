#' @exportClass Compressor
setClass("Compressor",
         slots = list(ptr = "externalptr"))

Compressor.from_ptr <- function(ptr) {
  stopifnot(is(ptr, "externalptr"))
  label <- tiledb_compressor_label(ptr)
  level <- tiledb_compressor_level(ptr)
  ptr <- tiledb_compressor(label, level)
  return(new("Compressor", ptr= ptr))
}

#' @export Compressor
Compressor <- function(type = "NO_COMPRESSION", level = -1L) {
  if (!is.scalar(type, "character")) {
    stop("compressor argument must be scalar string")
  }
  if (!is.scalar(level, "double") && ! is.scalar(level, "integer")) {
    stop("level argument must be a integer or double scalar value")    
  } else {
    level <- as.integer(level)
  }
  ptr <- tiledb_compressor(type, level)
  return(new("Compressor", ptr = ptr))
}

#' @export
setGeneric("compressor_type", function(object, ...) standardGeneric("compressor_type"))

#' @export
setMethod("compressor_type", "Compressor",
          function(object) {
            return(tiledb_compressor_type(object@ptr))  
          })

#' @export
setGeneric("compressor_level", function(object, ...) standardGeneric("compressor_level"))

#' @export
setMethod("compressor_level", "Compressor",
          function(object) {
            return(tiledb_compressor_level(object@ptr));
          })

setMethod("show", "Compressor",
          function(object) {
            type <- compressor_type(object)
            level <- compression_level(object)
            cat("tiledb::Compressor(\"", type, "\", level = ", level, ")", sep="")
          })