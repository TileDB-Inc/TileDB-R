#  MIT License
#
#  Copyright (c) 2017-2021 TileDB Inc.
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

#' An S4 class for a TileDB configuration
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_config
setClass("tiledb_config",
         slots = list(ptr = "externalptr"))

#' @importFrom methods new
tiledb_config.from_ptr <- function(ptr) {
    stopifnot(`ptr must be a non-NULL externalptr to a tiledb_config instance` = !missing(ptr) && is(ptr, "externalptr") && !is.null(ptr))
    new("tiledb_config", ptr = ptr)
}

#' Creates a `tiledb_config` object
#'
#' Note that for actually setting persistent values, the (altered) config
#' object needs to used to create (or update) the \code{tiledb_ctx} object. Similarly,
#' to check whether values are set, one should use the \code{config} method
#' of the of the \code{tiledb_ctx} object. Examples for this are
#' \code{ctx <- tiledb_ctx(limitTileDBCores())} to use updated configuration values to
#' create a context object, and \code{cfg <- config(ctx)} to retrieve it.
#' @param config (optional) character vector of config parameter names, values
#' @return `tiledb_config` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' cfg <- tiledb_config()
#' cfg["sm.tile_cache_size"]
#'
#' # set tile cache size to custom value
#' cfg <- tiledb_config(c("sm.tile_cache_size" = "100"))
#' cfg["sm.tile_cache_size"]
#'
#' @importFrom methods new
#' @export tiledb_config
tiledb_config <- function(config = NA_character_) {
  config <- config[!is.na(x = config)]
  if (length(x = config)) {
    stopifnot(`If given, the 'config' argument must be a name, value character vector` = is.character(config) && !is.null(names(config)))
    ptr <- libtiledb_config(config)
  } else {
    ptr <- libtiledb_config()
  }
  new("tiledb_config", ptr = ptr)
}

#' Gets a config parameter value
#'
#' @param x `tiledb_config` object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param drop Optional logical switch to drop dimensions, default FALSE, currently unused.
#' @return a config string value if parameter exists, else NA
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' cfg <- tiledb_config()
#' cfg["sm.tile_cache_size"]
#' cfg["does_not_exist"]
#' @aliases [,tiledb_config
#' @aliases [,tiledb_config-method
#' @aliases [,tiledb_config,ANY,tiledb_config-method
#' @aliases [,tiledb_config,ANY,ANY,tiledb_config-method
setMethod("[", "tiledb_config", function(x, i, j, ..., drop=FALSE) {
  stopifnot(`The first subscript in tiledb_config subscript must be of type 'character'` = is.character(i),
            `The second subscript is currently unused` = missing(j))
  tryCatch(libtiledb_config_get(x@ptr, i), error = function(e) NA)
})

#' Sets a config parameter value
#'
#' @param x `tiledb_config` object
#' @param i parameter key string
#' @param j parameter key string
#' @param value value to set, will be converted into a stringa
#' @return updated `tiledb_config` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' cfg <- tiledb_config()
#' cfg["sm.tile_cache_size"]
#'
#' # set tile cache size to custom value
#' cfg["sm.tile_cache_size"] <- 100
#' cfg["sm.tile_cache_size"]
#'
#' @aliases [<-,tiledb_config
#' @aliases [<-,tiledb_config-method
#' @aliases [<-,tiledb_config,ANY,tiledb_config-method
#' @aliases [<-,tiledb_config,ANY,ANY,tiledb_config-method
setMethod("[<-", "tiledb_config", function(x, i, j, value) {
  stopifnot(`The first subscript in tiledb_config subscript must be of type 'character'` = is.character(i),
            `The second subscript is currently unused` = missing(j),
            `The value argument must be be int, numeric, character or logical` = is.logical(value) || is.character(value) || is.numeric(value))
  if (is.logical(value)) {
    value <- if (isTRUE(value)) "true" else "false"
  } else {
    value <- as.character(value)
  }
  libtiledb_config_set(x@ptr, i, value)
  x
})

#' Prints the config object to STDOUT
#'
#' @param object `tiledb_config` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' cfg <- tiledb_config()
#' show(cfg)
#' @export
setMethod("show", signature(object = "tiledb_config"), function(object) {
  libtiledb_config_dump(object@ptr)
})

#' Save a `tiledb_config` object ot a local text file
#'
#' @param config The `tiledb_config` object
#' @param path The path to config file to be created
#' @return path to created config file
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' tmp <- tempfile()
#' cfg <- tiledb_config(c("sm.tile_cache_size" = "10"))
#' pth <- tiledb_config_save(cfg, tmp)
#'
#' cat(readLines(pth), sep = "\n")
#'
#' @export
tiledb_config_save <- function(config, path) {
  stopifnot(`The 'config' argument must be a tiledb_config object` = is(config, "tiledb_config"),
            `The 'path' argument must be of type character` = is.character(path))
  libtiledb_config_save_to_file(config@ptr, path)
}

#' Load a saved `tiledb_config` file from disk
#'
#' @param path path to the config file
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' tmp <- tempfile()
#' cfg <- tiledb_config(c("sm.tile_cache_size" = "10"))
#' pth <- tiledb_config_save(cfg, tmp)
#' cfg <- tiledb_config_load(pth)
#' cfg["sm.tile_cache_size"]
#'
#' @export
tiledb_config_load <- function(path) {
  stopifnot(`The 'path' argument must be of type character` = is.character(path))
  ptr <- libtiledb_config_load_from_file(path)
  tiledb_config.from_ptr(ptr)
}

#' Convert a `tiledb_config` object to a R vector
#'
#' @param x `tiledb_config` object
#' @param mode Character value `"any"`, currently unused
#' @return a character vector of config parameter names, values
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' cfg <- tiledb_config()
#' as.vector(cfg)
#'
#' @export
as.vector.tiledb_config <- function(x, mode="any") {
  stopifnot(`The 'x' argument must be a tiledb_config object` = is(x, "tiledb_config"))
  libtiledb_config_vector(x@ptr)
}

#' Convert a `tiledb_config` object to a R data.frame
#' @param x `tiledb_config` object
#' @param ... Extra parameter for method signature, currently unused.
#' @return a data.frame wth parameter, value columns
#' @examples
#' cfg <- tiledb_config()
#' as.data.frame(cfg)
#'
#' @export
as.data.frame.tiledb_config <- function(x, ...) {
  stopifnot(`The 'x' argument must be a tiledb_config object` = is(x, "tiledb_config"))
  v <- libtiledb_config_vector(x@ptr)
  params <- names(v)
  values <- as.vector(v)
  data.frame("parameter" = params, "value" = values, stringsAsFactors = FALSE)
}

#' Limit TileDB core use to a given number of cores
#'
#' By default, TileDB will use all available cores on a given machine. In multi-user or
#' multi-process settings, one may want to reduce the number of core. This function will
#' take a given number, or default to smaller of the \sQuote{Ncpus} options value or the
#' \sQuote{"OMP_THREAD_LIMIT"} enviroment variable (or two as hard fallback).
#'
#' As this function returns a config object, its intended use is as argument to the context
#' creating functions: \code{ctx <- tiledb_ctx(limitTileDBCores())}. To check that the values
#' are set (or at a later point, still set) the config object should be retrieved via the
#' corresponding method and this \code{ctx} object: \code{cfg <- config(ctx)}.
#' @param ncores Value of CPUs used, if missing the smaller of a fallback of two, the value of
#' \sQuote{Ncpus} (if set) and the value of environment variable \sQuote{"OMP_THREAD_LIMIT"} is
#' used.
#' @param verbose Optional logical toggle; if set, a short message is displayed informing the
#' user about the value set.
#' @return The modified configuration object is returned invisibly.
#' @importFrom stats na.omit
#' @export
limitTileDBCores <- function(ncores, verbose=FALSE) {
  if (missing(ncores)) {
    ## start with a simple fallback: 'Ncpus' (if set) or else 2
    ncores <- getOption("Ncpus", 2L)
    ## also consider OMP_THREAD_LIMIT (cf Writing R Extensions), gets NA if envvar unset
    ompcores <- as.integer(Sys.getenv("OMP_THREAD_LIMIT"))
    ## and then keep the smaller
    ncores <- min(na.omit(c(ncores, ompcores)))
  }
  stopifnot(`The 'ncores' argument must be numeric or character` = is.numeric(ncores) || is.character(ncores))
  cfg <- tiledb_config()
  if (tiledb_version(TRUE) >= "2.1.0") {
    cfg["sm.compute_concurrency_level"] <- ncores
    cfg["sm.io_concurrency_level"] <- ncores
  } else {
    cfg["sm.num_reader_threads"] <- ncores
    cfg["sm.num_writer_threads"] <- ncores
    cfg["vfs.file.max_parallel_ops"] <- ncores
    cfg["vfs.num_threads"] <- ncores
  }
  if (verbose) message("Limiting TileDB to ",ncores," cores. See ?limitTileDBCores.")
  invisible(cfg)
}

#' Unset a TileDB Config parameter to its default value
#'
#' @param config A TileDB Config object
#' @param param A character variable with the parameter name
#' @return The modified TileDB Config object
#' @export
tiledb_config_unset <- function(config, param) {
  stopifnot(`The 'config' argument must be a tiledb_config object` = is(config, "tiledb_config"),
            `The 'param' argument must be of type character` = is.character(param))
  libtiledb_config_unset(config@ptr, param)
}
