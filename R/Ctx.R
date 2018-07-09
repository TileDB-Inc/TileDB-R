#' @exportClass tiledb_ctx
setClass("tiledb_ctx",
         slots = list(ptr = "externalptr"))

#' Creates a `tiledb_ctx` object
#' 
#' @param config (optonal) character vector of config parameter names, values
#' @return `tiledb_ctx` object 
#' @examples
#' # default configuration
#' ctx <- tiledb_ctx()
#'
#' # optionally set config parameters
#' ctx <- tiledb_ctx(c("sm.tile_cache_size" = "100")) 
#' 
#' @importFrom methods new
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

#' Retrieve the `tiledb_config` object from the `tiledb_ctx`
#' 
#' @param `tiledb_ctx` object
#' @return `tiledb_config` object associated with the `tiledb_ctx` instance
#' @examples 
#' ctx <- tieldb_ctx(c("sm.tile_cache_size" = "10")) 
#' cfg <- config(ctx)
#' cfg["sm.tile_cache_size"]
#' 
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

#' Query if a TileDB backend is supported
#' 
#' The scheme corresponds to the URI scheme for TileDB resouces.
#'  
#' Ex:
#'  * `{file}:///path/to/file`
#'  * `{hdfs}:///path/to/file`
#'  * `{s3}://hostname:port/path/to/file`
#' 
#' @param object `tiledb_ctx` object 
#' @param schema URI string scheme ("file", "hdfs", "s3")
#' @return TRUE if tiledb backend is supported, FALSE otherwise
#' @examples
#' ctx <- tiledb_ctx()
#' tiledb_is_supported_fs(ctx, "file") 
#' 
#' tiledb_is_supported_fs(ctx, "s3")
#' 
#' @export
setMethod("tiledb_is_supported_fs", signature(object = "tiledb_ctx", scheme = "character"),
          function(object, scheme) {
            libtiledb_ctx_is_supported_fs(object@ptr, scheme)
          })