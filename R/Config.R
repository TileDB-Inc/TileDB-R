#' @export
Config <- setClass("Config", 
                   representation(ptr = "externalptr"))

setMethod("initialize", "Config",
          function(.Object) {
            .Object@ptr = tiledb_config()
            .Object
          })

setMethod("[", "Config",
          function(x, i, j, ...) {
            if (!is.character(i)) {
              stop("Config subscript must be of type 'character'")
            }
            if (!missing(j)) {
              stop("incorrect number of subscripts")
            }
            tryCatch(tiledb_config_get(x@ptr, i),
                     error = function(e) NA)
          })

setMethod("[<-", "Config",
          function(x, i, j, value) {
            if (!is.character(i)) {
              stop("Config subscript must be of type 'character'")
            } else if (!missing(j)) {
              stop("incorrect number of subscripts")
            } else if (!is.character(value)) {
              if (is.double(value) || is.integer(value)) {
                value <- as.character(value)    
              } else if (is.logical(value)) {
                value <- if (isTRUE(value)) "true" else "false"
              } else {
                stop("Config parameter value must be a 'character', 'double', 'integer' or 'logical'")
              }
            }
            tiledb_config_set(x@ptr, i, value)
            x
          })

setMethod("show", signature(object = "Config"),
          function(object) {
            tiledb_config_dump(object@ptr)
          })

as.vector.Config <- function(x, mode="any") {
  tiledb_config_vector(x@ptr)
}