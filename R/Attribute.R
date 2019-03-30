#' @exportClass tiledb_attr
setClass("tiledb_attr",
         slots = list(ptr = "externalptr"))

tiledb_attr.from_ptr <- function(ptr) {
   if (typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb::Attribute instance")
  }
  new("tiledb_attr", ptr = ptr)
}

#' Contructs a `tiledb_attr` object
#'
#' @param name (default "") The dimension name / label string.
#' @param type (default "FLOAT64") The tiledb_attr TileDB datatype string
#' @param filter_list (default filter_list("NONE")) The tiledb_attr filter_list
#' @param ncells The number of cells
#' @return `tiledb_dim` object
#' @examples
#' flt <- tiledb_filter_list(list(tiledb_filter("GZIP")))
#' attr <- tiledb_attr(name = "a1", type = "INT32",
#'                     filter_list = flt)
#' attr
#'
#' @importFrom methods new
#' @export
tiledb_attr <- function(name="",
                        type=c("FLOAT64", "INT32", "UTF8"),
                        filter_list=tiledb_filter_list(),
                        ncells = 1,
                        ctx = tiledb:::ctx
                        ) {
  type = match.arg(type)
  if (!is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  } else if (!is.scalar(name, "character")) {
    stop("name argument must be a scalar string")
  } else if(!is(filter_list, "tiledb_filter_list")) {
    stop("filter_list argument must be a tiledb_filter_list instance")
  } else if (ncells != 1 && ncells != 1) {
      stop("ncells argument must be 1 for fixed-length attributes or -1 for variable-length attributes")
  }
  ptr <- libtiledb_attr(ctx@ptr, name, type, filter_list@ptr, ncells)
  new("tiledb_attr", ptr = ptr)
}

setMethod("show", "tiledb_attr",
          function(object) {
            libtiledb_attr_dump(object@ptr)
          })


#' @export
setGeneric("name", function(object) standardGeneric("name"))

#' Return the `tiledb_attr` name
#'
#' @param `tiledb_attr` object
#' @return string name, empty string if the attribute is anonymous
#' @examples
#' a1 <- tiledb_attr("a1")
#' name(a1)
#'
#' a2 <- tiledb_attr()
#' name(a2)
#'
#' @export
setMethod("name", signature(object = "tiledb_attr"),
          function(object) {
            libtiledb_attr_name(object@ptr)
          })

#' @export
setGeneric("datatype", function(object) standardGeneric("datatype"))

#' Return the `tiledb_attr` datatype
#'
#' @param `tiledb_attr` object
#' @param tiledb datatype string
#' @examples
#' a1 <- tiledb_attr("a1", type = "INT32")
#' datatype(a1)
#'
#' a2 <- tiledb_attr("a1", type = "FLOAT64")
#' datatype(a2)
#'
#' a2 <- tiledb_attr("a1", type = "UTF8")
#' datatype(a2)
#' @export
setMethod("datatype", signature(object = "tiledb_attr"),
          function(object) {
            libtiledb_attr_datatype(object@ptr)
          })

#' Returns the `tiledb_filter_list` object associated with the given `tiledb_attr`
#'
#' @param object tiledb_attr
#' @return a tiledb_filter_list object
#' @examples
#' attr <- tiledb_attr(filter_list=tiledb_filter_list(list(tiledb_filter("ZSTD"))))
#' filter_list(attr)
#'
#' @export
setMethod("filter_list", "tiledb_attr",
          function(object) {
            ptr <- libtiledb_attr_filter_list(object@ptr)
            return(tiledb_filter_list.from_ptr(ptr))
          })

#' @export
setGeneric("ncells", function(object) standardGeneric("ncells"))

#' Return the number of scalar values per attribute cell
#'
#' @param `tiledb_attr` object
#' @return integer number of cells
#' @examples
#' a1 <- tiledb_attr("a1", type = "FLOAT64", ncells = 1)
#' ncells(a1)
#'
#' @export
setMethod("ncells", signature(object = "tiledb_attr"),
          function(object) {
            libtiledb_attr_ncells(object@ptr)
          })

#' @export
is.anonymous <- function(object) UseMethod("is.anonymous", object)

#' Returns TRUE if the tiledb_dim is anonymous
#'
#' A TileDB attribute is anonymous if no name/label is defined
#'
#' @param `tiledb_attr` object
#' @return TRUE or FALSE
#' @examples
#' a1 <- tiledb_attr("a1", type = "FLOAT64")
#' is.anonymous(a1)
#'
#' a2 <- tiledb_attr("", type = "FLOAT64")
#' is.anonymous(a2)
#'
#' @export
is.anonymous.tiledb_attr <- function(object) {
  name <- libtiledb_attr_name(object@ptr)
  nchar(name) == 0
}
