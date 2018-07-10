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
      stop("config arugment must be a name, value character vector") 
    }
    ptr <- libtiledb_config(config)
  } else {
    ptr <- libtiledb_config()
  }
  new("tiledb_config", ptr = ptr)
}

#' Gets a config parameter value
#' 
#' @param x `tiledb_config`object
#' @param i parameter key string
#' @return a config string value if parameter exists, else NA
#' @examples
#' cfg <- tiledb_config()
#' cfg["sm.tile_cache_size"]
#' 
#' cfg["does_not_exist"]
#' 
setMethod("[", "tiledb_config",
          function(x, i, j, ...) {
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
#' @param x `tiledb_config`object
#' @param i parameter key string
#' @param value value to set, will be converted into a stringa
#' @return updated `tiledb_config` object
#' @examples
#' cfg <- tiledb_config()
#' cfg["sm.tile_cache_size"]
#' 
#' # set tile cache size to custom value
#' cfg["sm.tile_cache_size"] <- 100
#' cfg["sm.tile_cache_size"]
#' 
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
#' cfg <- tiledb_config()
#' show(cfg)
#' 
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
#' @return a character vector of config parameter names, values
#' @examples
#' cfg <- tiledb_config()
#' as.vector(cfg)
#' 
#' @export
as.vector.tiledb_config <- function(x, mode="any") {
  libtiledb_config_vector(x@ptr)
}

#' Convert a `tiledb_config` object to a R data.frame
#' @param x `tiledb_config` object
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