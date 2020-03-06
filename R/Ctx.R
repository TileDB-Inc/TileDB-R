#' An S4 class for a TileDB context
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_ctx
setClass("tiledb_ctx",
         slots = list(ptr = "externalptr"))

#' Retrieve a TileDB context object from the package cache
#'
#' @return A TileDB context object
tiledb_get_context <- function() {
  ## return the ctx entry from the package environment (a lightweight hash)
  ctx <- .pkgenv[["ctx"]]

  ## if null, create a new context (which caches it too) and return it
  if (is.null(ctx)) {
    ctx <- tiledb_ctx(cached=FALSE)
  }

  ctx
}

# provided old renamed context for continuity/compatibility
getContext <- function() tiledb_get_context()

#' Store a TileDB context object in the package cache
#'
#' @param ctx A TileDB context object
#' @return A TileDB context object
#' @export
tiledb_set_context <- function(ctx) {
  ## set the ctx entry from the package environment (a lightweight hash)
  .pkgenv[["ctx"]] <- ctx
}

# provided old renamed context for continuity/compatibility
setContext <- function(ctx) tiledb_set_context(ctx)

#' Creates a `tiledb_ctx` object
#'
#' @param config (optonal) character vector of config parameter names, values
#' @param cached (optional) logical switch to force new creation
#' @return `tiledb_ctx` object
#' @examples
#' # default configuration
#' ctx <- tiledb_ctx()
#'
#' # optionally set config parameters
#' ctx <- tiledb_ctx(c("sm.tile_cache_size" = "100"))
#'
#' @importFrom methods new
#' @importFrom methods is
#' @export tiledb_ctx
tiledb_ctx <- function(config = NULL, cached = TRUE) {

  ctx <- .pkgenv[["ctx"]]

  ## if not-NULL and no (new) config and cache use is requested, return it
  if (!is.null(ctx) && is.null(config) && cached) {
    return(ctx)
  }

  ## otherwise create a new ctx and cache it
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
  ctx <- new("tiledb_ctx", ptr = ptr)

  tiledb_ctx_set_default_tags(ctx)

  tiledb_set_context(ctx)

  return(ctx)
}

#' @rdname generics
#' @export
setGeneric("config", function(object, ...) {
  standardGeneric("config")
})

#' Retrieve the `tiledb_config` object from the `tiledb_ctx`
#'
#' @param object tiledb_ctx object
#' @return `tiledb_config` object associated with the `tiledb_ctx` instance
#' @examples
#' ctx <- tiledb_ctx(c("sm.tile_cache_size" = "10"))
#' cfg <- config(ctx)
#' cfg["sm.tile_cache_size"]
#'
#' @export
setMethod("config", signature(object = "tiledb_ctx"),
          function(object = tiledb_get_context()) {
            ptr <- libtiledb_ctx_config(object@ptr)
            tiledb_config.from_ptr(ptr)
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
#' @param scheme URI string scheme ("file", "hdfs", "s3")
#' @return TRUE if tiledb backend is supported, FALSE otherwise
#' @examples
#' tiledb_is_supported_fs("file")
#' tiledb_is_supported_fs("s3")
#'
#' @export
tiledb_is_supported_fs <- function(scheme, object = tiledb_get_context()) {
            libtiledb_ctx_is_supported_fs(object@ptr, scheme)
}

#' Sets a string:string "tag" on the Ctx
#'
#' @param object `tiledb_ctx` object
#' @param key string
#' @param value string
#' @examples
#' ctx <- tiledb_ctx(c("sm.tile_cache_size" = "10"))
#' cfg <- tiledb_ctx_set_tag(ctx, "tag", "value")
#'
#' @export
tiledb_ctx_set_tag <- function(object, key, value) {
  stopifnot(is(object, "tiledb_ctx"))
  return(libtiledb_ctx_set_tag(object@ptr, key, value))
}

#' Sets default context tags
#'
#' @param object `tiledb_ctx` object
#' @importFrom utils packageVersion
tiledb_ctx_set_default_tags <- function(object) {
  stopifnot(is(object, "tiledb_ctx"))

  tiledb_ctx_set_tag(object, "x-tiledb-api-language", "r")
  tiledb_ctx_set_tag(object, "x-tiledb-api-language-version", as.character(packageVersion("tiledb")))
  info <- Sys.info()
  tiledb_ctx_set_tag(object, "x-tiledb-api-sys-platform", paste(info["sysname"], info["release"], info["machine"], collapse=""))
}
