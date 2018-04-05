#' @export
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
            tiledb::Config(ptr = ptr)
          })