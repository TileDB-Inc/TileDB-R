#' An S4 class for the TileDB array schema
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_array_schema
setClass("tiledb_array_schema",
         slots = list(ptr = "externalptr"))

tiledb_array_schema.from_ptr <- function(ptr) {
   if (missing(ptr) || typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb_array_schema instance")
  }
  new("tiledb_array_schema", ptr = ptr)
}

#' Constructs a `tiledb_array_schema` object
#'
#' @param domain tiledb_domain object
#' @param attrs a list of one or more tiledb_attr objects
#' @param cell_order (default "COL_MAJOR")
#' @param tile_order (default "COL_MAJOR")
#' @param sparse (default FALSE)
#' @param coords_filter_list (optional)
#' @param offsets_filter_list (optional)
#' @param ctx tiledb_ctx object (optional)
#' @examples
#' schema <- tiledb_array_schema(
#'               dom = tiledb_domain(
#'                         dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
#'                                  tiledb_dim("cols", c(1L, 4L), 4L, "INT32"))),
#'               attrs = c(tiledb_attr("a", type = "INT32")),
#'               cell_order = "COL_MAJOR",
#'               tile_order = "COL_MAJOR",
#'               sparse = FALSE)
#' schema
#'
#' @export
tiledb_array_schema <- function(
                        domain,
                        attrs,
                        cell_order = "COL_MAJOR",
                        tile_order = "COL_MAJOR",
                        sparse = FALSE,
                        coords_filter_list = NULL,
                        offsets_filter_list = NULL,
                        ctx = tiledb:::ctx
                        ) {
  if (!is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  }
  if (missing(domain) || !is(domain, "tiledb_domain")) {
    stop("domain argument must be a tiledb::Domain")
  }
  is_attr <- function(obj) is(obj, "tiledb_attr")
  if (missing(attrs) || length(attrs) == 0 || !all(vapply(attrs, is_attr, logical(1)))) {
    stop("attrs argument must be a list of one or tiled_attr objects")
  }
  if (!is.scalar(cell_order, "character")) {
     stop("cell_order argument must be a scalar string")
  }
  if (!is.scalar(tile_order, "character")) {
    stop("tile_order argument must be a scalar string")
  }
  if (!is.null(coords_filter_list) && !is(coords_filter_list, "tiledb_filter_list")) {
    stop("coords_filter_list argument must be a tiledb_filter_list instance")
  }
  if (!is.null(offsets_filter_list) && !is(offsets_filter_list, "tiledb_filter_list")) {
    stop("offsets_filter_list argument must be a tiledb_filter_list instance")
  }
  if (!is.logical(sparse)) {
    stop("sparse argument must be a logical TRUE or FALSE")
  }
  attr_ptrs <- lapply(attrs, function(obj) slot(obj, "ptr"))
  coords_filter_list_ptr <- NULL
  if (!is.null(coords_filter_list)) {
    coords_filter_list_ptr <- coords_filter_list@ptr
  }
  offsets_filter_list_ptr <- NULL
  if (!is.null(offsets_filter_list)) {
    offsets_filter_list_ptr <- offsets_filter_list@ptr
  }
  ptr <- libtiledb_array_schema(ctx@ptr, domain@ptr, attr_ptrs, cell_order, tile_order,
                             coords_filter_list_ptr, offsets_filter_list_ptr, sparse)
  return(new("tiledb_array_schema", ptr = ptr))
}

tiledb_array_schema.from_array <- function(x, ctx = tiledb:::ctx) {
  if (!is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  } else if (missing(x) || !is.array(x)) {
    stop("x argument must be a valid array object")
  }
  xdim <- dim(x)
  dims <- lapply(seq_len(xdim), function(i) {
    tiledb_dim(c(1L, xdim[i]), type = "INT32", ctx)
  })
  dom <- tiledb_domain(dims, ctx)
  #TODO: better datatype checking
  if (is.double(x)) {
    typestr <- "FLOAT64"
  } else if (is.integer(x)) {
    typestr <- "INT32"
  } else {
    stop(paste("invalid array type \"", typeof(x), "\""))
  }
  val <- tiledb_attr("", type = typestr, ctx)
  return(tiledb_array_schema(dom, c(val), ctx))
}

setMethod("show", signature(object = "tiledb_array_schema"),
          function(object) {
            libtiledb_array_schema_dump(object@ptr)
          })

#' @rdname generics
#' @export
setGeneric("domain", function(object, ...) standardGeneric("domain"))

#' Returns the `tiledb_domain` object associated with a given `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
#' domain(sch)
#'
#' @export
setMethod("domain", "tiledb_array_schema",
          function(object) {
            ptr <- libtiledb_array_schema_domain(object@ptr)
            tiledb_domain.from_ptr(ptr)
          })

#' @rdname generics
#' @export
setGeneric("dimensions", function(object, ...) standardGeneric("dimensions"))

#' Returns a list of `tiledb_dim` objects associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @param ... Extra parameter for method signature, currently unused.
#' @return a list of tiledb_dim objects
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32"),
#'                                    tiledb_dim("d2", c(1L, 50L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
#' dimensions(dom)
#'
#' lapply(dimensions(dom), name)
#'
#' @export
setMethod("dimensions", "tiledb_array_schema",
          function(object) dimensions(domain(object)))

#' @rdname generics
#' @export
setGeneric("attrs", function(object, idx, ...) standardGeneric("attrs"))

#' Returns a list of all `tiledb_attr` objects associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @param idx index argument, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @return a list of tiledb_attr objects
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                                tiledb_attr("a2", type = "FLOAT64")))
#' attrs(sch)
#'
#' lapply(attrs(sch), datatype)
#'
#' @export
setMethod("attrs", signature("tiledb_array_schema"),
          function (object, ...) {
            attr_ptrs <- libtiledb_array_schema_attributes(object@ptr)
            attrs <- lapply(attr_ptrs, function(ptr) tiledb_attr.from_ptr(ptr))
            names(attrs) <- vapply(attrs, function(attr) {
              n <- tiledb::name(attr)
              return(ifelse(n == "__attr", "", n))
            }, character(1))
            return(attrs)
          })

#' Returns a `tiledb_attr` object associated with the `tiledb_array_schema` with a given name.
#'
#' @param object tiledb_array_schema
#' @param idx attribute name string
#' @param ... Extra parameter for method signature, currently unused.
#' @return a `tiledb_attr` object
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                                tiledb_attr("a2", type = "FLOAT64")))
#' attrs(sch, "a2")
#'
#' @export
setMethod("attrs", signature("tiledb_array_schema", "character"),
          function(object, idx, ...) {
            attrs <- tiledb::attrs(object)
            return(attrs[[idx]])
          })

#' Returns a `tiledb_attr` object associated with the `tiledb_array_schema` with a given index
#'
#' The attribute index is defined by the order the attributes were defined in the schema
#'
#' @param object tiledb_array_schema
#' @param idx attribute index
#' @param ... Extra parameter for method signature, currently unused.
#' @return a `tiledb_attr` object
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                                tiledb_attr("a2", type = "FLOAT64")))
#' attrs(sch, 2)
#'
#' @export
setMethod("attrs", signature("tiledb_array_schema", "numeric"),
          function(object, idx, ...) {
            attrs <- tiledb::attrs(object)
            return(attrs[[idx]])
          })

#' @rdname generics
#' @export
setGeneric("cell_order", function(object, ...) standardGeneric("cell_order"))

#' Returns the cell layout string associated with the `tiledb_array_schema`
#' @param object tiledb object
#' @export
setMethod("cell_order", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_cell_order(object@ptr)
          })

#' @rdname generics
#' @export
setGeneric("tile_order", function(object, ...) standardGeneric("tile_order"))

#' Returns the tile layout string associated with the `tiledb_array_schema`
#' @param object tiledb object
#' @export
setMethod("tile_order", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_tile_order(object@ptr)
          })

# ' @ export
#tiledb_filter_list.tiledb_array_schema <- function(object) {
#            coords_ptr <- libtiledb_array_schema_coords_filter_list(object@ptr)
#            offsets_ptr <- libtiledb_array_schema_offsets_filter_list(object@ptr)
#            return(c(coords = tiledb_filter_list.from_ptr(coords_ptr),
#                     offsets = tiledb_filter_list.from_ptr(offsets_ptr)))
#}

#' @rdname generics
#' @export
setGeneric("filter_list", function(object, ...) standardGeneric("filter_list"))

#' Returns the offsets and coordinate filter_lists associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return a list of tiledb_filter_list objects
#' @export
setMethod("filter_list", "tiledb_array_schema",
          function(object) {
            coords_ptr <- libtiledb_array_schema_coords_filter_list(object@ptr)
            offsets_ptr <- libtiledb_array_schema_offsets_filter_list(object@ptr)
            return(c(coords = tiledb_filter_list.from_ptr(coords_ptr),
                     offsets = tiledb_filter_list.from_ptr(offsets_ptr)))
          })

#' @rdname generics
#' @export
setGeneric("is.sparse", function(object, ...) standardGeneric("is.sparse"))

#' Returns TRUE if the tiledb_array_schema is sparse, else FALSE
#'
#' @param object tiledb_array_schema
#' @return TRUE if tiledb_array_schema is sparse
#' @export
setMethod("is.sparse", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_sparse(object@ptr)
          })

#' @rdname generics
#' @export
setGeneric("tiledb_ndim", function(object, ...) standardGeneric("tiledb_ndim"))

#' Return the number of dimensions associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return integer number of dimensions
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                                tiledb_attr("a2", type = "FLOAT64")))
#' tiledb_ndim(sch)
#'
#' @export
setMethod("tiledb_ndim", "tiledb_array_schema",
          function(object) {
            dom <- tiledb::domain(object)
           return(tiledb_ndim(dom))
          })

#' Retrieve the dimension (domain extent) of the domain
#'
#' Only valid for integral (integer) domains
#'
#' @param x tiledb_array_schema
#' @return a dimension vector
#' @examples
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                                tiledb_attr("a2", type = "FLOAT64")))
#' dim(sch)
#'
#' @export
dim.tiledb_array_schema <- function(x) dim(tiledb::domain(x))
