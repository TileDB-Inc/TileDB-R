#' @exportClass tiledb_filter_list
setClass("tiledb_filter_list",
         slots = list(ptr = "externalptr"))

tiledb_filter_list.from_ptr <- function(ptr) {
  stopifnot(is(ptr, "externalptr"))
  return(new("tiledb_filter_list", ptr = ptr))
}

#' Constructs a `tiledb_filter_list` object
#'
#'
#' @param ctx tiledb_ctx object
#' @param filter an optional list of one or more tiledb_filter_list objects
#' @return tiledb_filter_list object
#' @examples
#' ctx = tiledb_ctx()
#' flt = tiledb_filter(ctx, "ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' flt.set
#' filter_list <- tiledb_filter_list(ctx, c(flt))
#' filter_list
#'
#' @export tiledb_filter_list
tiledb_filter_list <- function(ctx = tiledb::ctx, filters = c()) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  }
  is_filter <- function(obj) is(obj, "tiledb_filter")
  filter_ptrs = c()
  if (length(filters) > 0) {
    if (!all(sapply(filters, is_filter))) {
      stop("filters argument must be a list of one or tiled_filter objects")
    }
    filter_ptrs <- lapply(filters, function(obj) slot(obj, "ptr"))
  }
  ptr <- libtiledb_filter_list(ctx@ptr, filter_ptrs)
  return(new("tiledb_filter_list", ptr = ptr))
}

#' @export
setGeneric("set_max_chunk_size", function(object, value) standardGeneric("set_max_chunk_size"))

#' Set the filter_list's max_chunk_size
#'
#' @param object tiledb_filter_list
#' @param string option
#' @param int value
#' @examples
#' flt = tiledb_filter(ctx, "ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' flt.set
#' filter_list <- tiledb_filter_list(ctx, c(flt))
#' set_max_chunk_size(filter_list, value)
#'
#'
#' @export
setMethod("set_max_chunk_size", signature(object = "tiledb_filter_list", value = "numeric"),
          function(object, value) {
            libtiledb_filter_list_set_max_chunk_size(object@ptr, value)
          })

#' @export
setGeneric("max_chunk_size", function(object) standardGeneric("max_chunk_size"))

#' Returns the filter_list's max_chunk_size
#'
#' @param object tiledb_filter_list
#' @return integer max_chunk_size
#' @examples
#' flt = tiledb_filter(ctx, "ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' flt.set
#' filter_list <- tiledb_filter_list(ctx, c(flt))
#' max_chunk_size(filter_list)
#'
#' @export
setMethod("max_chunk_size", signature(object = "tiledb_filter_list"),
          function(object) {
            libtiledb_filter_list_max_chunk_size(object@ptr)
          })

#' @export
setGeneric("nfilters", function(object) standardGeneric("nfilters"))

#' Returns the filter_list's number of filters
#'
#' @param object tiledb_filter_list
#' @return integer number of filters
#' @examples
#' flt = tiledb_filter(ctx, "ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' flt.set
#' filter_list <- tiledb_filter_list(ctx, c(flt))
#' nfilters(filter_list)
#'
#' @export
setMethod("nfilters", signature(object = "tiledb_filter_list"),
          function(object) {
            libtiledb_filter_list_nfilters(object@ptr)
          })

#' Returns the filter at given index
#'
#' @return object tiledb_filter
#' @examples
#' flt = tiledb_filter(ctx, "ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' flt.set
#' filter_list <- tiledb_filter_list(ctx, c(flt))
#' filter_list[0]
#'
#' @export
setMethod("[", "tiledb_filter_list",
          function(x, i, ..., drop = FALSE) {
            tiledb_filter.from_ptr(libtiledb_filter_list_filter(x@ptr, i))
          })
