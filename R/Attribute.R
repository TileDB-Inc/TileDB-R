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
Attr <- function(ctx, 
                 name="", 
                 type="FLOAT64", 
                 compressor=tiledb::Compressor(),
                 ncells=1) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  } else if (!is.scalar(name, "character")) {
    stop("name argument must be a scalar string")
  } else if (!is.scalar(type, "character") || (type != "INT32" && type != "FLOAT64")) {
    stop("type argument must be \"INT32\" or \"FLOAT64\"")
  } else if(!is(compressor, "Compressor")) {
    stop("compressor argument must be a tiledb::Compressor instance") 
  } else if (ncells != 1) {
    stop("only single cell attributes are supported")
  }
  ptr <- tiledb_attr(ctx@ptr, name, type, compressor@ptr, ncells)
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
setMethod("compressor", "Attr",
          function(object) {
            ptr <- tiledb_attr_compressor(object@ptr)
            return(Compressor.from_ptr(ptr))
          })

#' @export
setGeneric("ncells", function(object) standardGeneric("ncells"))

#' @export
setMethod("ncells", signature(object = "Attr"),
          function(object) {
            tiledb_attr_ncells(object@ptr)
          })

#' @export
setGeneric("ncells", function(object) standardGeneric("ncells"))

#' @export
setMethod("ncells", signature(object = "Attr"),
          function(object) {
            tiledb_attr_ncells(object@ptr)
          })

#' @export
is.anonymous <- function(object) UseMethod("is.anonymous", object)

#' @export
is.anonymous.Attr <- function(object) {
  name <- tiledb_attr_name(object@ptr)
  nchar(name) == 0
}