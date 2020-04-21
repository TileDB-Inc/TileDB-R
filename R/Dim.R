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
#' @param domain The dimension (inclusive) domain. The dimension’s domain is defined by a (lower bound, upper bound) vector
#' @param tile The tile dimension tile extent
#' @param type The dimension TileDB datatype string
#' @param ctx tiledb_ctx object (optional)
#' @return `tiledb_dim` object
#' @examples
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
  if ((typeof(domain) != "integer" && typeof(domain) != "double") || (length(domain) != 2)) {
    stop("domain must be an integer or double vector of length 2")
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
  if (missing(type)) {
    type <- ifelse(is.integer(domain), "INT32", "FLOAT64")
  } else if (!type %in% c("INT32", "FLOAT64", "DATETIME_DAY", "DATETIME_SEC",
                          "DATETIME_MS", "DATETIME_US", "DATETIME_NS", "ASCII")) {
    stop("type argument must be 'INT32' or 'FLOAT64' or a supported 'DATETIME_*' type.", call.=FALSE)
  }
  ptr <- libtiledb_dim(ctx@ptr, name, type, domain, tile)
  return(new("tiledb_dim", ptr = ptr))
}

#' Return the `tiledb_dim` name
#'
#' @param object `tiledb_dim` object
#' @return string name, empty string if the dimension is anonymous
#' @examples
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
#' d1 <- tiledb_dim("d1", c(1L, 10L), 5L)
#' dim(d1)
#'
#' @export
dim.tiledb_dim <- function(x) {
  dtype <- datatype(x)
  if (dtype == "FLOAT32" || dtype == "FLOAT64") {
    stop("dim() is only defined for integral domains")
  }
  dom <- domain(x)
  return(dom[2L] - dom[1L] + 1L)
}
