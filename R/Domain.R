#' @exportClass Domain
setClass("Domain", 
         slots = list(ptr = "externalptr"))

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
  new("Domain", ptr = ptr)
}

setMethod("show", "Domain",
          function(object) {
            tiledb_domain_dump(object@ptr)
          })

#' @export
setGeneric("dimensions", function(object, ...) {
  standardGeneric("dimensions")
})

#' @export
setMethod("dimensions", "Domain", 
          function(object) {
            dim_ptrs <- tiledb_domain_dimensions(object@ptr)
            lapply(dim_ptrs, Dim.from_ptr)
          })

#' @export
dim.Domain <- function(x) {
  stopifnot(is(x, "Domain"))
  tiledb_domain_rank(x@ptr)
}