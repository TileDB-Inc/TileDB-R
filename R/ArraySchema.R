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
#' @param ctx tiledb_ctx object
#' @param domain tiledb_domain object
#' @param attrs a list of one or more tiledb_attr objects
#' @param cell_order (default "COL_MAJOR")
#' @param tile_order (default "COL_MAJOR")
#' @param sparse (default FALSE)
#' @param coords_compressor (optional)
#' @param offsets_compressor (optional)
#' @examples 
#' ctx <- tiledb_ctx()
#' schema <- tiledb_array_schema(ctx,
#'               dom = tiledb_domain(ctx, 
#'                         dims = c(tiledb_dim(ctx, "rows", c(1L, 4L), 4L, "INT32"),
#'                                  tiledb_dim(ctx, "cols", c(1L, 4L), 4L, "INT32"))),
#'               attrs = c(tiledb_attr(ctx, "a", type = "INT32")), 
#'               cell_order = "COL_MAJOR",
#'               tile_order = "COL_MAJOR",
#'               sparse = FALSE)
#' schema 
#' 
#' @export 
tiledb_array_schema <- function(ctx,
                        domain, 
                        attrs, 
                        cell_order = "COL_MAJOR",
                        tile_order = "COL_MAJOR",
                        sparse = FALSE,
                        coords_compressor = NULL,
                        offsets_compressor = NULL) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  }
  if (missing(domain) || !is(domain, "tiledb_domain")) {
    stop("domain argument must be a tiledb::Domain") 
  }
  is_attr <- function(obj) is(obj, "tiledb_attr") 
  if (missing(attrs) || length(attrs) == 0 || !all(sapply(attrs, is_attr))) {
    stop("attrs argument must be a list of one or tiled_attr objects")    
  }
  if (!is.scalar(cell_order, "character")) {
     stop("cell_order argument must be a scalar string")
  }
  if (!is.scalar(tile_order, "character")) {
    stop("tile_order argument must be a scalar string")
  }
  if (!is.null(coords_compressor) && !is(coords_compressor, "tiledb_compressor")) {
    stop("coords_compressor argument must be a tiledb_compressor instance") 
  }
  if (!is.null(offsets_compressor) && !is(offsets_compressor, "tiledb_compressor")) {
    stop("offsets_compressor argument must be a tiledb_compressor instance") 
  }
  if (!is.logical(sparse)) {
    stop("sparse argument must be a logical TRUE or FALSE")
  }
  attr_ptrs <- lapply(attrs, function(obj) slot(obj, "ptr"))
  coords_compressor_ptr <- NULL
  if (!is.null(coords_compressor)) {
    coords_compressor_ptr <- coords_compressor@ptr
  }
  offsets_compressor_ptr <- NULL
  if (!is.null(offsets_compressor)) {
    offsets_compressor_ptr <- offsets_compressor@ptr 
  }
  ptr <- libtiledb_array_schema(ctx@ptr, domain@ptr, attr_ptrs, cell_order, tile_order, 
                             coords_compressor_ptr, offsets_compressor_ptr, sparse) 
  return(new("tiledb_array_schema", ptr = ptr))
}

tiledb_array_schema.from_array <- function(ctx, x) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  } else if (missing(x) || !is.array(x)) {
    stop("x argument must be a valid array object") 
  }
  xdim <- dim(x)
  dims <- lapply(seq_len(xdim), function(i) {
    tiledb::Dim(ctx, c(1L, xdim[i]), type = "INT32")
  })
  dom <- tiledb_domain(ctx, dims)
  #TODO: better datatype checking
  if (is.double(x)) {
    typestr <- "FLOAT64"   
  } else if (is.integer(x)) {
    typestr <- "INT32"
  } else {
    stop(paste("invalid array type \"", typeof(x), "\""))
  }
  val <- tiledb_attr(ctx, "", type = typestr)
  return(tiledb_array_schema(ctx, dom, c(val)))
}

setMethod("show", signature(object = "tiledb_array_schema"),
          function(object) {
            libtiledb_array_schema_dump(object@ptr)
          })

#' @export
setGeneric("domain", function(object, ...) standardGeneric("domain"))

#' Returns the `tiledb_domain` object associated with a given `tiledb_array_schema`
#' 
#' @param object tiledb_array_schema
#' @param a tiledb_domain object
#' @examples 
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1")))
#' domain(sch)
#' 
#' @export
setMethod("domain", "tiledb_array_schema",
          function(object) {
            ptr <- libtiledb_array_schema_domain(object@ptr)
            tiledb_domain.from_ptr(ptr)
          })

#' @export
setGeneric("dimensions", function(object, ...) standardGeneric("dimensions"))

#' Returns a list of `tiledb_dim` objects associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return a list of tiledb_dim objects
#' @examples
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 100L), type = "INT32"),
#'                                    tiledb_dim(ctx, "d2", c(1L, 50L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1")))
#' dimensions(dom)
#' 
#' lapply(dimensions(dom), name)
#' 
#' @export
setMethod("dimensions", "tiledb_array_schema",
          function(object) dimensions(domain(object)))

#' @export
setGeneric("attrs", function(object, idx, ...) standardGeneric("attrs"))

#' Returns a list of all `tiledb_attr` objects associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return a list of tiledb_attr objects
#' @examples
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1", type = "INT32"), 
#'                                                tiledb_attr(ctx, "a2", type = "FLOAT64")))
#' attrs(sch)
#' 
#' lapply(attrs(sch), datatype)
#' 
#' @export
setMethod("attrs", signature("tiledb_array_schema"),
          function (object, ...) {
            attr_ptrs <- libtiledb_array_schema_attributes(object@ptr)
            attrs <- lapply(attr_ptrs, function(ptr) tiledb_attr.from_ptr(ptr))
            names(attrs) <- sapply(attrs, function(attr) {
              n <- tiledb::name(attr)
              return(ifelse(n == "__attr", "", n))
            })
            return(attrs)
          })

#' Returns a `tiledb_attr` object associated with the `tiledb_array_schema` with a given name.
#'
#' @param object tiledb_array_schema
#' @param idx attribute name string
#' @return a `tiledb_attr` object
#' @examples
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1", type = "INT32"), 
#'                                                tiledb_attr(ctx, "a2", type = "FLOAT64")))
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
#' @return a `tiledb_attr` object
#' @examples
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1", type = "INT32"), 
#'                                                tiledb_attr(ctx, "a2", type = "FLOAT64")))
#' attrs(sch, 2)
#' 
#' @export
setMethod("attrs", signature("tiledb_array_schema", "numeric"),
          function(object, idx, ...) {
            attrs <- tiledb::attrs(object) 
            return(attrs[[idx]])
          })

#' @export
setGeneric("cell_order", function(object, ...) standardGeneric("cell_order"))

#' Returns the cell layout string associated with the `tiledb_array_schema`
#' @export
setMethod("cell_order", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_cell_order(object@ptr) 
          })

#' @export
setGeneric("tile_order", function(object, ...) standardGeneric("tile_order"))

#' Returns the tile layout string associated with the `tiledb_array_schema`
#' @export
setMethod("tile_order", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_tile_order(object@ptr) 
          })

#' @export
setGeneric("compressor", function(object, ...) standardGeneric("compressor"))

#' Returns the offsets and coordinate compressors associated with the `tiledb_array_schema`
#' 
#' @param object tiledb_array_schema
#' @return a list of tiledb_compressor objects
#' @export
setMethod("compressor", "tiledb_array_schema",
          function(object) {
            coords_ptr <- libtiledb_array_schema_coords_compressor(object@ptr)
            offsets_ptr <- libtiledb_array_schema_offsets_compressor(object@ptr)
            return(c(coords = tiledb_compressor.from_ptr(coords_ptr), 
                     offsets = tiledb_compressor.from_ptr(offsets_ptr)))
          })

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
 
#' @export
setGeneric("tiledb_ndim", function(object, ...) standardGeneric("tiledb_ndim"))

#' Return the number of dimensions associated with the `tieldb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return integer number of dimensions
#' @examples 
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1", type = "INT32"), 
#'                                                tiledb_attr(ctx, "a2", type = "FLOAT64")))
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
#' @param object tiledb_array_schema
#' @return a dimension vector
#' @examples
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1", type = "INT32"), 
#'                                                tiledb_attr(ctx, "a2", type = "FLOAT64")))
#' dim(sch)
#' 
#' @export
dim.tiledb_array_schema <- function(x) dim(tiledb::domain(x))