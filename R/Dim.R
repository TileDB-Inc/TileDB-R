#' @export

Dim <- setClass("Dim", representation(ptr = "externalptr"))

is.scalar <- function(x, typestr) (typeof(x) == typestr) && is.atomic(x) && length(x) == 1L

setMethod("initialize",  "Dim",
          function(.Object, ctx, name, domain, tile, type) {
              if (!is(ctx, "Ctx")) {
                stop("ctx argument must be a tiledb::Ctx")
              } else if (!is.scalar(name, "character")) {
                stop("name argument must be a scalar string")
              } else if ((typeof(domain) != "integer" && typeof(domain) != "double") 
                         || (length(domain) != 2)) {
                stop("domain must be an integer or double vector of length 2")   
              } 
              if (missing(tile)) {
                if (is.integer(domain)) {
                  tile <- 0L
                } else {
                  tile <- 0.0
                }
              }
              if (missing(type)) {
                type <- "Float64"  
              } else if (type != "INT32" && type != "FLOAT64") {
                stop("type argument must be \"INT32\" or \"FLOAT64\"")
              }
              .Object@ptr <- tiledb_dim(ctx@ptr, name, type, domain, tile)
              .Object
          })

#' @export
setGeneric("domain", function(object, ...) {
  standardGeneric("domain")
})

#' @export
setMethod("domain", "Dim",
          function(object) {
            tiledb_dim_domain(object@ptr)
          })