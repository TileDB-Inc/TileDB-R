#' @exportClass tiledb_ctx
setClass("tiledb_ctx",
         slots = list(ptr = "externalptr"))

#' @export tiledb_ctx
tiledb_ctx <- function(config = NULL) {
  if (is.null(config)) {
    ptr <- libtiledb_ctx()
  } else if (typeof(config) == "character") {
    config <- tiledb_config(config)
    ptr <- libtiledb_ctx(config@ptr)
  } else if (is(config, "tiledb_config")) {
    ptr <- libtiledb_ctx(config@ptr)
  } else {
    stop("invalid tiledb_ctx config argument type")
  }
  return(new("tiledb_ctx", ptr = ptr))
}

#' @export
setGeneric("config", function(object, ...) {
  standardGeneric("config")
})

#' @export
setMethod("config", signature(object = "tiledb_ctx"),
          function(object) {
            ptr <- libtiledb_ctx_config(object@ptr)
            tiledb_config.from_ptr(ptr)
          })

#' @export
setGeneric("tiledb_is_supported_fs", function(object, scheme, ...) {
  standardGeneric("tiledb_is_supported_fs")
})

#' @export
setMethod("tiledb_is_supported_fs", signature(object = "tiledb_ctx", scheme = "character"),
          function(object, scheme) {
            libtiledb_ctx_is_supported_fs(object@ptr, scheme)
          })