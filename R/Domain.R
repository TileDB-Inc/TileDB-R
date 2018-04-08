#' @export Domain
#' @exportClass Domain
Domain <- setClass("Domain", 
                   representation(ptr = "externalptr"))

setMethod("initialize", "Domain",
          function(.Object, ctx, dims) {
            if (!is(ctx, "Ctx")) {
              stop("argument ctx must be a tiledb::Ctx")
            }
            is_dim <- function(obj) is(obj, "Dim")
            if (missing(dims) || !all(sapply(dims, is_dim))) {
              stop("argument dims must be a list of tileb::Dim")
            }
            dims_ptrs <- lapply(dims, function(obj) slot(obj, "ptr"))
            .Object@ptr <- tiledb_domain(ctx@ptr, dims_ptr)
            .Object 
          })

#' @export
setGeneric("dump", function(object, ...) {
  standardGeneric("dump")
})


#' @export
setMethod("dump", "Domain",
          function(object) {
            tiledb_domain_dump(object@ptr) 
          })

#' @export
setGeneric("rank", function(object, ...) {
  standardGeneric("rank") 
})

#' @export
setMethod("rank", "Domain",
          function(object) {
            tiledb_domain_rank(object@ptr)
          })

#' @export
setGeneric("dimensions", function(object, ...) {
  standardGeneric("dimensions")
})

#' @export
setMethod("dimensions", "Domain", 
          function(object) {
            print("not implemented error") 
          })