#' @exportClass tiledb_dim
setClass("tiledb_dim", 
         slots = list(ptr = "externalptr"))

#' @importFrom methods new
tiledb_dim.from_ptr <- function(ptr) {
  if (missing(ptr) || typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb::Dim instance")
  }
  return(new("tiledb_dim", ptr = ptr))
}

#' @export tiledb_dim
tiledb_dim <- function(ctx, name="", domain, tile, type) {
  if (!is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  } else if (!is.scalar(name, "character")) {
    stop("name argument must be a scalar string")
  } else if ((typeof(domain) != "integer" && typeof(domain) != "double") 
             || (length(domain) != 2)) {
    stop("domain must be an integer or double vector of length 2")   
  } 
  # by default, tile extent should span the whole domain
  if (missing(tile)) {
    if (is.integer(domain)) {
      tile <- (domain[2L] - domain[1L]) + 1L
    } else {
      tile <- (domain[2L] - domain[1L])
    }
  }
  if (missing(type)) {
    type <- ifelse(is.integer(domain), "INT32", "FLOAT64")
  } else if (type != "INT32" && type != "FLOAT64") {
    stop("type argument must be \"INT32\" or \"FLOAT64\"")
  }
  ptr <- libtiledb_dim(ctx@ptr, name, type, domain, tile)
  return(new("tiledb_dim", ptr = ptr))
}

#' @export
setMethod("name", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_name(object@ptr))
          })

#' @export
setMethod("domain", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_domain(object@ptr))
          })

#' @export
setGeneric("tile", function(object) standardGeneric("tile"))

#' @export
setMethod("tile", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_tile_extent(object@ptr))
          })

#' @export
setMethod("datatype", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_datatype(object@ptr))
          })

#' @export
setMethod("ndim", "tiledb_dim",
          function(object) {
            return(1)            
          })

#' @export
is.anonymous.tiledb_dim <- function(object) {
  name <- libtiledb_dim_name(object@ptr)
  return(nchar(name) == 0)
}

#' @export
dim.tiledb_dim <- function(x) {
  dtype <- datatype(x) 
  if (dtype == "FLOAT32" || dtype == "FLOAT64") {
    stop("dim() is only defined for integral domains") 
  }
  dom <- domain(x)
  return(dom[2L] - dom[1L] + 1L)
}