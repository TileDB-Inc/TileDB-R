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

#' An S4 class for a TileDB dimension object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_dim
setClass("tiledb_dim",
         slots = list(ptr = "externalptr"))

#' @importFrom methods new
tiledb_dim.from_ptr <- function(ptr) {
  if (missing(ptr) || typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb::Dim instance")
  }
  return(new("tiledb_dim", ptr = ptr))
}

#' Contructs a `tiledb_dim` object
#'
#' @param name The dimension name / label string.  This argument is required.
#' @param domain The dimension (inclusive) domain. The dimension’s domain is
#' defined by a (lower bound, upper bound) vector, and is usually either of
#' type \code{integer} or \code{double} (i.e. \code{numeric}). For type,
#' \code{ASCII} \code{NULL} is expected.
#' @param tile The tile dimension tile extent. For type,
#' \code{ASCII} \code{NULL} is expected.
#' @param type The dimension TileDB datatype string
#' @param ctx tiledb_ctx object (optional)
#' @return `tiledb_dim` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' tiledb_dim(name = "d1", domain = c(1L, 10L), tile = 5L, type = "INT32")
#'
#' @importFrom methods new
#' @export tiledb_dim
tiledb_dim <- function(name, domain, tile, type, ctx = tiledb_get_context()) {
  if (missing(name)) {
    stop("'name' argument must be supplied when creating a dimension object.")
  }
  if (!is.scalar(name, "character")) {
    stop("'name' argument must be a scalar string when creating a dimension object.")
  }
  if (missing(type)) {
    type <- ifelse(is.integer(domain), "INT32", "FLOAT64")
  } else if (!type %in% c("INT8", "INT16", "INT32", "INT64",
                          "UINT8", "UINT16", "UINT32", "UINT64",
                          "FLOAT32", "FLOAT64",
                          "DATETIME_YEAR","DATETIME_MONTH","DATETIME_WEEK","DATETIME_DAY",
                          "DATETIME_HR", "DATETIME_MIN", "DATETIME_SEC",
                          "DATETIME_MS", "DATETIME_US", "DATETIME_NS",
                          "DATETIME_PS", "DATETIME_FS", "DATETIME_AS",
                          "ASCII")) {
    stop("type argument must be '(U)INT{8,16,32,64}', 'FLOAT{32,64}', 'ASCII', or a supported 'DATETIME_*' type.", call.=FALSE)
  }
  if (!type %in% c("ASCII")) {
    if ((typeof(domain) != "integer" && typeof(domain) != "double") || (length(domain) != 2)) {
      stop("domain must be an integer or double vector of length 2")
    }
  }
  if (inherits(domain, "nanotime") ||   # not integer64 as we want the conversion only for datetimes
      type %in% c("DATETIME_PS",        # but also for high precision times we cannot fit into NS
                  "DATETIME_FS",
                  "DATETIME_AS")) {
      w <- getOption("warn")            # store warning levels
      options("warn" = -1)              # suppress warnings
      domain <- as.numeric(domain)      # for this lossy conversion
      options("warn" = w)               # restore warning levels
  }
  if (!is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  }
  # by default, tile extent should span the whole domain
  if (missing(tile)) {
    if (is.integer(domain)) {
      tile <- (domain[2L] - domain[1L]) + 1L
    } else {
      tile <- (domain[2L] - domain[1L])
    }
  }
  ptr <- libtiledb_dim(ctx@ptr, name, type, domain, tile)
  return(new("tiledb_dim", ptr = ptr))
}

#' Return the `tiledb_dim` name
#'
#' @param object `tiledb_dim` object
#' @return string name, empty string if the dimension is anonymous
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' d1 <- tiledb_dim("d1", c(1L, 10L))
#' name(d1)
#'
#' d2 <- tiledb_dim("", c(1L, 10L))
#' name(d2)
#'
#' @export
setMethod("name", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_get_name(object@ptr))
          })

#' Return the `tiledb_dim` domain
#'
#' @param object `tiledb_dim` object
#' @return a vector of (lb, ub) inclusive domain of the dimension
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' d1 <- tiledb_dim("d1", domain = c(5L, 10L))
#' domain(d1)
#'
#' @export
setMethod("domain", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_get_domain(object@ptr))
          })

#' @rdname generics
#' @export
setGeneric("tile", function(object) standardGeneric("tile"))

#' Return the `tiledb_dim` tile extent
#'
#' @param object `tiledb_dim` object
#' @return a scalar tile extent
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' d1 <- tiledb_dim("d1", domain = c(5L, 10L), tile = 2L)
#' tile(d1)
#'
#' @export
setMethod("tile", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_get_tile_extent(object@ptr))
          })

#' Return the `tiledb_dim` datatype
#'
#' @param object tiledb_dim object
#' @return tiledb datatype string
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' d1 <- tiledb_dim("d1", domain = c(5L, 10L), tile = 2L, type = "INT32")
#' datatype(d1)
#'
#' @export
setMethod("datatype", signature(object = "tiledb_dim"),
          function(object) {
            return(libtiledb_dim_get_datatype(object@ptr))
          })

#' Returns the number of dimensions for a tiledb domain object
#'
#' @param object tiledb_ndim object
#' @return 1L
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' d1 <- tiledb_dim("d1", c(1L, 10L), 10L)
#' tiledb_ndim(d1)
#'
#' @export
setMethod("tiledb_ndim", "tiledb_dim",
          function(object) {
            return(1L)
          })

#' Returns TRUE if the tiledb_dim is anonymous
#'
#' A TileDB dimension is anonymous if no name/label is defined
#'
#' @param object `tiledb_dim` object
#' @return TRUE or FALSE
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' d1 <- tiledb_dim("d1", c(1L, 10L), 10L)
#' is.anonymous(d1)
#'
#' d2 <- tiledb_dim("", c(1L, 10L), 10L)
#' is.anonymous(d2)
#'
#' @export
is.anonymous.tiledb_dim <- function(object) {
  name <- libtiledb_dim_get_name(object@ptr)
  return(nchar(name) == 0)
}

#' Retrieves the dimension of the tiledb_dim domain
#'
#' @param x `tiledb_dim` object
#' @return a vector of the tile_dim domain type, of the dim domain dimension (extent)
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' d1 <- tiledb_dim("d1", c(1L, 10L), 5L)
#' dim(d1)
#'
#' @export
dim.tiledb_dim <- function(x) {
  dtype <- datatype(x)
  if (isTRUE(any(sapply(dtype, match, c("FLOAT32","FLOAT32"))))) {
    stop("dim() is only defined for integral domains")
  }
  dom <- domain(x)
  return(dom[2L] - dom[1L] + 1L)
}


## Generic in ArraySchema.R

#' Returns the TileDB Filter List object associated with the given TileDB Dimension
#'
#' @param object TileDB_Dimension
#' @return A TileDB_filter_list object
#' @export
setMethod("filter_list", "tiledb_dim", function(object) {
  ptr <- libtiledb_dimension_get_filter_list(object@ptr)
  return(tiledb_filter_list.from_ptr(ptr))
})

## Generic in ArraySchema.R

#' Sets the TileDB Filter List for the TileDB Dimension object
#'
#' @param x TileDB Dimension
#' @param value TileDB Filter List
#' @return The modified TileDB Dimension object
#' @export
setReplaceMethod("filter_list", "tiledb_dim", function(x, value) {
  x@ptr <- libtiledb_dimension_set_filter_list(x@ptr, value@ptr)
  x
})
