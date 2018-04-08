#' @export Ctx
#' @exportClass Ctx
Ctx <- setClass("Ctx",
                representation(ptr = "externalptr"))

setMethod("initialize", "Ctx",
          function(.Object, config = NULL) {
            if (is.null(config)) {
              .Object@ptr = tiledb_ctx()
            } else if (typeof(config) == "character") {
               config <- tiledb::Config(config)
              .Object@ptr <- tiledb_ctx(config@ptr)
            } else if (is(config, "Config")) {
              .Object@ptr <- tiledb_ctx(config@ptr)
            } else {
              stop("invalid tiledb::Ctx config argument type")
            }
            .Object
          })

#' @export
setGeneric("config", function(object, ...) {
  standardGeneric("config")
})

#' @export
setMethod("config", "Ctx",
          function(object) {
            ptr <- tiledb_ctx_config(object@ptr)
            new("Config", ptr = ptr)
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