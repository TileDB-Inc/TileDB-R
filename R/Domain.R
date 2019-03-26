#' @exportClass tiledb_domain
setClass("tiledb_domain",
         slots = list(ptr = "externalptr"))

tiledb_domain.from_ptr <- function(ptr) {
  if (missing(ptr) || typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb_domain instance")
  }
  return(new("tiledb_domain", ptr = ptr))
}

#' Constructs a `tiledb_domain` object
#'
#' All `tiledb_dim` must be of the same TileDB type.
#'
#' @param ctx tiledb_ctx
#' @param dims list() of tiledb_dim objects
#' @return tiledb_domain
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32"),
#'                               tiledb_dim("d2", c(1L, 50L), type = "INT32")))
#' @importFrom methods slot
#' @importFrom methods new
#' @export tiledb_domain
tiledb_domain <- function(dims, ctx = tiledb:::ctx) {
  if (!is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  }
  is_dim <- function(obj) is(obj, "tiledb_dim")
  if (missing(dims) || length(dims) == 0 || !all(vapply(dims, is_dim, logical(1)))) {
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

#' Returns a list of the tiledb_domain dimension objects
#'
#'
#' @param object tiledb_domain
#' @return a list of tiledb_dim
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32"),
#'                               tiledb_dim("d2", c(1L, 50L), type = "INT32")))
#' dimensions(dom)
#'
#' lapply(dimensions(dom), name)
#'
#' @export
setMethod("dimensions", "tiledb_domain",
          function(object) {
            dim_ptrs <- libtiledb_domain_dimensions(object@ptr)
            return(lapply(dim_ptrs, tiledb_dim.from_ptr))
          })

#' Returns tiledb_domain TileDB type string
#'
#' @param object tiledb_domain
#' @return tiledb_domain type string
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32")))
#' datatype(dom)
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64")))
#' datatype(dom)
#'
#' @export
setMethod("datatype", "tiledb_domain",
          function(object) {
            return(libtiledb_domain_datatype(object@ptr))
          })

#' Returns the number of dimensions of the `tiledb_domain`
#'
#' @param object tiledb_domain
#' @return integer number of dimensions
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64")))
#' tiledb_ndim(dom)
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64"),
#'                                    tiledb_dim("d2", c(0.5, 100.0), type = "FLOAT64")))
#' tiledb_ndim(dom)
#'
#' @export
setMethod("tiledb_ndim", "tiledb_domain",
          function(object) {
            return(libtiledb_domain_ndim(object@ptr))
          })

#' @export
setGeneric("is.integral", function(object) standardGeneric("is.integral"))

#' Returns TRUE is tiledb_domain is an integral (integer) domain
#'
#' @param object tiledb_domain
#' @return TRUE if the domain is an integral domain, else FALSE
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32")))
#' is.integral(dom)
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64")))
#' is.integral(dom)
#'
#' @export
setMethod("is.integral", "tiledb_domain",
          function(object) {
            dt <- datatype(object)
            return(ifelse(dt == "FLOAT32" || dt == "FLOAT64", FALSE, TRUE))
          })

#' Retrieve the dimension (domain extent) of the domain
#'
#' Only valid for integral (integer) domains
#'
#' @param x tiledb_domain
#' @return dimension vector
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32"),
#'                               tiledb_dim("d2", c(1L, 100L), type = "INT32")))
#' dim(dom)
#'
#' @export
dim.tiledb_domain <- function(x) {
  dtype <- datatype(x)
  if (dtype == "FLOAT32" || dtype == "FLOAT64")  {
   stop("dim() is only defined for integral domains")
  }
  return(vapply(dimensions(x), dim, integer(1L)))
}
