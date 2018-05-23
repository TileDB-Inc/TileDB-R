#' @exportClass Ctx
setClass("Ctx",
         slots = list(ptr = "externalptr"))

#' @export Ctx
Ctx <- function(config = NULL) {
  if (is.null(config)) {
    ptr <- tiledb_ctx()
  } else if (typeof(config) == "character") {
    config <- tiledb::Config(config)
    ptr <- tiledb_ctx(config@ptr)
  } else if (is(config, "Config")) {
    ptr <- tiledb_ctx(config@ptr)
  } else {
    stop("invalid tiledb::Ctx config argument type")
  }
  return(new("Ctx", ptr = ptr))
}

#' @export
setGeneric("config", function(object, ...) {
  standardGeneric("config")
})

#' @export
setMethod("config", signature(object = "Ctx"),
          function(object) {
            ptr <- tiledb_ctx_config(object@ptr)
            Config.from_ptr(ptr)
          })

#' @export
setGeneric("is_supported_fs", function(object, scheme, ...) {
  standardGeneric("is_supported_fs")
})

#' @export
setMethod("is_supported_fs", signature(object = "Ctx", scheme = "character"),
          function(object, scheme) {
            tiledb_ctx_is_supported_fs(object@ptr, scheme)
          })