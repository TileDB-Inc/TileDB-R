#  MIT License
#
#  Copyright (c) 2017-2020 TileDB Inc.
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
  if (typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr must be a non NULL externalptr to a tiledb_config instance")
  }
  new("tiledb_config", ptr = ptr)
}

#' Creates a `tiledb_config` object
#'
#' @param config (optonal) character vector of config parameter names, values
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
  if (!is.na(config)) {
    if (typeof(config) != "character" || is.null(names(config))) {
      stop("config argument must be a name, value character vector")
    }
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
setMethod("[", "tiledb_config",
          function(x, i, j, ..., drop=FALSE) {
            if (!is.character(i)) {
              stop("tiledb_config subscript must be of type 'character'")
            }
            if (!missing(j)) {
              stop("incorrect number of subscripts")
            }
            tryCatch(libtiledb_config_get(x@ptr, i),
                     error = function(e) NA)
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
setMethod("[<-", "tiledb_config",
          function(x, i, j, value) {
            if (!is.character(i)) {
              stop("tiledb_config subscript must be of type 'character'")
            } else if (!missing(j)) {
              stop("incorrect number of subscripts")
            } else if (!is.character(value)) {
              if (is.double(value) || is.integer(value)) {
                value <- as.character(value)
              } else if (is.logical(value)) {
                value <- if (isTRUE(value)) "true" else "false"
              } else {
                stop("tiledb_config parameter value must be a 'character', 'double', 'integer' or 'logical'")
              }
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
setMethod("show", signature(object = "tiledb_config"),
          function(object) {
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
  stopifnot(class(config) == "tiledb_config")
  stopifnot(typeof(path) == "character")
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
  stopifnot(typeof(path) == "character")
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
  v <- libtiledb_config_vector(x@ptr)
  params <- names(v)
  values <- as.vector(v)
  return(data.frame("parameter" = params, "value" = values, stringsAsFactors = FALSE))
}

#' Limit TileDB core use to a given number of cores
#'
#' By default, TileDB will use all available cores on a given machine. In multi-user or
#' multi-process settings, one may want to reduce the number of core. This function will
#' take a given number, or default to smaller of the \sQuote{Ncpus} options value or the
#' \sQuote{"OMP_THREAD_LIMIT"} enviroment variable (or two as hard fallback).
#' @param ncores Value of CPUs used, if missing the smaller of a fallback of two, the value of
#' \sQuote{Ncpus} (if set) and the value of environment variable \sQuote{"OMP_THREAD_LIMIT"} is
#' used.
#' @param verbose Optional logical toggle; if set, a short message is displayed informing the
#' user about the value set.
#' @return The modified configuration object is returned invisibly. As a side-effect the
#' updated config is also used to set the global context object.
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
  cfg <- tiledb_config()
  cfg["sm.num_reader_threads"] <- ncores
  cfg["sm.num_tbb_threads"] <- ncores
  cfg["sm.num_writer_threads"] <- ncores
  cfg["vfs.file.max_parallel_ops"] <- ncores
  cfg["vfs.num_threads"] <- ncores
  if (verbose) message("Limiting TileDB to ",ncores," cores. See ?limitTileDBCores.")
  invisible(cfg)
}
