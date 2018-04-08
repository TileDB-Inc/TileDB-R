#' @exportClass Config
setClass("Config", 
         slots = list(ptr = "externalptr"))

Config.from_ptr <- function(ptr) {
  if (typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr must be a non NULL externalptr to a tiledb::Config instance")
  }
  new("Config", ptr = ptr)
}

#' @export Config
Config <- function(config = NA_character_) {
  if (!is.na(config)) {
    if (typeof(config) != "character" || is.null(names(config))) {
      stop("config arugment must be a name, value character vector") 
    }
    ptr <- tiledb_config(config)
  } else {
    ptr <- tiledb_config()
  }
  new("Config", ptr = ptr)
}

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

#' @export
Config.from_file <- function(path) {
  stopifnot(typeof(path) == "character")
  ptr <- tiledb_config_load_from_file(path)
  Config.from_ptr(ptr)
}

#' @export
as.vector.Config <- function(x, mode="any") {
  tiledb_config_vector(x@ptr)
}