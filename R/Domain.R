#' @exportClass tiledb_domain
setClass("tiledb_domain", 
         slots = list(ptr = "externalptr"))

tiledb_domain.from_ptr <- function(ptr) {
  if (missing(ptr) || typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb_domain instance")
  }
  return(new("tiledb_domain", ptr = ptr))
}

#' @importFrom methods slot
#' @importFrom methods new
#' @export tiledb_domain
tiledb_domain <- function(ctx, dims) {
  if (!is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  }
  is_dim <- function(obj) is(obj, "tiledb_dim")
  if (missing(dims) || length(dims) == 0 || !all(sapply(dims, is_dim))) {
    stop("argument dims must be a list of one or more tileb_dim")
  }
  dims_ptrs <- lapply(dims, function(obj) slot(obj, "ptr"))
  ptr <- libtiledb_domain(ctx@ptr, dims_ptrs)
  return(new("tiledb_domain", ptr = ptr))
}

setMethod("show", "tiledb_domain",
          function(object) {
            return(libtiledb_domain_dump(object@ptr))
          })

#' @export
setMethod("dimensions", "tiledb_domain", 
          function(object) {
            dim_ptrs <- libtiledb_domain_dimensions(object@ptr)
            return(lapply(dim_ptrs, tiledb_dim.from_ptr))
          })

#' @export
setMethod("datatype", "tiledb_domain",
          function(object) {
            return(libtiledb_domain_datatype(object@ptr))
          })

#' @export
setMethod("tiledb_ndim", "tiledb_domain",
          function(object) {
            return(libtiledb_domain_ndim(object@ptr))          
          })

#' @export
setGeneric("is.integral", function(object) standardGeneric("is.integral"))
 
#' @export
setMethod("is.integral", "tiledb_domain",
          function(object) {
            dt <- datatype(object) 
            return(ifelse(dt == "FLOAT32" || dt == "FLOAT64", FALSE, TRUE))
          })

#' @export
dim.tiledb_domain <- function(x) {
  dtype <- datatype(x)
  if (dtype == "FLOAT32" || dtype == "FLOAT64")  {
   stop("dim() is only defined for integral domains") 
  }
  return(vapply(dimensions(x), dim, integer(1L)))
}