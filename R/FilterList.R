#' An S4 class for a TileDB filter list
#'
#' @slot ptr An external pointer to the underlying implementation
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
#' @param filters an optional list of one or more tiledb_filter_list objects
#' @param ctx tiledb_ctx object (optional)
#' @return tiledb_filter_list object
#' @examples
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' filter_list
#'
#' @export tiledb_filter_list
tiledb_filter_list <- function(filters = c(), ctx = tiledb_get_context()) {
  if (!is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  }
  is_filter <- function(obj) is(obj, "tiledb_filter")
  filter_ptrs = c()
  if (length(filters) > 0) {
    if (!all(vapply(filters, is_filter, logical(1)))) {
      stop("filters argument must be a list of one or tiledb_filter objects")
    }
    filter_ptrs <- lapply(filters, function(obj) slot(obj, "ptr"))
  }
  ptr <- libtiledb_filter_list(ctx@ptr, filter_ptrs)
  return(new("tiledb_filter_list", ptr = ptr))
}

#' @rdname generics
#' @export
setGeneric("set_max_chunk_size", function(object, value) standardGeneric("set_max_chunk_size"))

#' Set the filter_list's max_chunk_size
#'
#' @param object tiledb_filter_list
#' @param value string
#' @examples
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' set_max_chunk_size(filter_list, 10)
#' @export
setMethod("set_max_chunk_size", signature(object = "tiledb_filter_list", value = "numeric"),
          function(object, value) {
            libtiledb_filter_list_set_max_chunk_size(object@ptr, value)
          })

#' @rdname generics
#' @export
setGeneric("max_chunk_size", function(object) standardGeneric("max_chunk_size"))

#' Returns the filter_list's max_chunk_size
#'
#' @param object tiledb_filter_list
#' @return integer max_chunk_size
#' @examples
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' max_chunk_size(filter_list)
#'
#' @export
setMethod("max_chunk_size", signature(object = "tiledb_filter_list"),
          function(object) {
            libtiledb_filter_list_max_chunk_size(object@ptr)
          })

#' @rdname generics
#' @export
setGeneric("nfilters", function(object) standardGeneric("nfilters"))

#' Returns the filter_list's number of filters
#'
#' @param object tiledb_filter_list
#' @return integer number of filters
#' @examples
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' nfilters(filter_list)
#'
#' @export
setMethod("nfilters", signature(object = "tiledb_filter_list"),
          function(object) {
            libtiledb_filter_list_nfilters(object@ptr)
          })

#' Returns the filter at given index


#' @param x `tiledb_config` object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param drop Optional logical switch to drop dimensions, default false.
#' @return object tiledb_filter
#' @examples
#' flt <- tiledb_filter("ZSTD")
#' tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#' filter_list <- tiledb_filter_list(c(flt))
#' filter_list[0]
#'
#' @export
setMethod("[", "tiledb_filter_list",
          function(x, i, j, ..., drop = FALSE) {
            tiledb_filter.from_ptr(libtiledb_filter_list_filter(x@ptr, i))
          })
