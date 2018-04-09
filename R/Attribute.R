#' @exportClass Attr
setClass("Attr",
         slots = list(ptr = "externalptr"))

Attr.from_ptr <- function(ptr) {
   if (typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb::Attribute instance")
  }
  new("Attr", ptr = ptr)
}

#' @export 
Attr <- function(ctx, name="", 
                      type="FLOAT64", 
                      compressor="NO_COMPRESSOR", 
                      level=-1L,
                      ncells=1) {
  if (missing(ctx) || !is(ctx, "Ctx")) {
    stop("ctx argument must be a tiledb::Ctx")
  } else if (!is.scalar(name, "character")) {
    stop("name argument must be a scalar string")
  } else if (type != "INT32" && type != "FLOAT64") {
    stop("type argument must be \"INT32\" or \"FLOAT64\"")
  } else if (!is.scalar(level, "double") && !is.scalar(level, "integer")) {
    stop("level argument must be a integer or double scalar value")    
  } else if (ncells != 1) {
    stop("only single cell attributes are supported")
  } 
  ptr <- tiledb_attr(ctx@ptr, name, type)
  new("Attr", ptr = ptr)
}

setMethod("show", "Attr", 
          function(object) {
            tiledb_attr_dump(object@ptr)
          })

#' @export
setGeneric("name", function(object) standardGeneric("name"))

#' @export
setMethod("name", signature(object = "Attr"),
          function(object) {
            tiledb_attr_name(object@ptr) 
          })

#' @export
setGeneric("datatype", function(object) standardGeneric("datatype"))

#' @export
setMethod("datatype", signature(object = "Attr"),
          function(object) {
            tiledb_attr_datatype(object@ptr)
          })

#' @export
is.anonymous <- function(object) UseMethod("is.anonymous", object)

#' @export
is.anonymous.Attr <- function(object) {
  name <- tiledb_attr_name(object@ptr)
  nchar(name) == 0
}