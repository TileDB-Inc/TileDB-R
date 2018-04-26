#' @exportClass Domain
setClass("Domain", 
         slots = list(ptr = "externalptr"))

Domain.from_ptr <- function(ptr) {
  if (missing(ptr) || typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb::Domain instance")
  }
  return(new("Domain", ptr = ptr))
}

#' @export Domain
Domain <- function(ctx, dims) {
  if (!is(ctx, "Ctx")) {
    stop("argument ctx must be a tiledb::Ctx")
  }
  is_dim <- function(obj) is(obj, "Dim")
  if (missing(dims) || length(dims) == 0 || !all(sapply(dims, is_dim))) {
    stop("argument dims must be a list of one or more tileb::Dim")
  }
  dims_ptrs <- lapply(dims, function(obj) slot(obj, "ptr"))
  ptr <- tiledb_domain(ctx@ptr, dims_ptrs)
  return(new("Domain", ptr = ptr))
}

setMethod("show", "Domain",
          function(object) {
            return(tiledb_domain_dump(object@ptr))
          })

#' @export
setMethod("dimensions", "Domain", 
          function(object) {
            dim_ptrs <- tiledb_domain_dimensions(object@ptr)
            return(lapply(dim_ptrs, Dim.from_ptr))
          })

#' @export
setMethod("datatype", "Domain",
          function(object) {
            return(tiledb_domain_datatype(object@ptr))
          })

#' @export
setMethod("ndim", "Domain",
          function(object) {
            return(tiledb_domain_rank(object@ptr))          
          })

#' @export
setGeneric("is.integral", function(object) standardGeneric("is.integral"))
 
#' @export
setMethod("is.integral", "Domain",
          function(object) {
            dt <- tiledb::datatype(object) 
            if (dt == "FLOAT32" || dt == "FLOAT64") {
              return(FALSE) 
            }
            return(TRUE) 
          })

#' @export
dim.Domain <- function(x) {
  dtype <- datatype(x)
  if (dtype == "FLOAT32" || dtype == "FLOAT64")  {
   stop("dim() is only defined for integral domains") 
  }
  return(vapply(dimensions(x), dim, integer(1L)))
}