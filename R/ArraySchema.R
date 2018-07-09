#' @exportClass ArraySchema
setClass("ArraySchema",
         slots = list(ptr = "externalptr"))

ArraySchema.from_ptr <- function(ptr) {
   if (missing(ptr) || typeof(ptr) != "externalptr" || is.null(ptr)) {
    stop("ptr argument must be a non NULL externalptr to a tiledb::ArraySchema instance")
  }
  new("ArraySchema", ptr = ptr) 
}

#' @export
ArraySchema <- function(ctx,
                        domain, 
                        attrs, 
                        cell_order = "COL_MAJOR",
                        tile_order = "COL_MAJOR",
                        coords_compressor = NULL,
                        offsets_compressor = NULL,
                        sparse = FALSE) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  }
  if (missing(domain) || !is(domain, "Domain")) {
    stop("domain argument must be a tiledb::Domain") 
  }
  is_attr <- function(obj) is(obj, "Attr") 
  if (missing(attrs) || length(attrs) == 0 || !all(sapply(attrs, is_attr))) {
    stop("attrs argument must be a list of one or tiledb::Attr's")    
  }
  if (!is.scalar(cell_order, "character")) {
     stop("cell_order argument must be a scalar string")
  }
  if (!is.scalar(tile_order, "character")) {
    stop("tile_order argument must be a scalar string")
  }
  if (!is.null(coords_compressor) && !is(coords_compressor, "Compressor")) {
    stop("coords_compressor argument must be a tiledb::Compressor instance") 
  }
  if (!is.null(offsets_compressor) && !is(offsets_compressor, "Compressor")) {
    stop("offsets_compressor argument must be a tiledb::Compressor instance") 
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
  ptr <- tiledb_array_schema(ctx@ptr, domain@ptr, attr_ptrs, cell_order, tile_order, 
                             coords_compressor_ptr, offsets_compressor_ptr, sparse) 
  return(new("ArraySchema", ptr = ptr))
}

ArraySchema.from_array <- function(ctx, x) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("ctx argument must be a tiledb_ctx")
  } else if (missing(x) || !is.array(x)) {
    stop("x argument must be a valid array object") 
  }
  xdim <- dim(x)
  dims <- lapply(seq_len(xdim), function(i) {
    tiledb::Dim(ctx, c(1L, xdim[i]), type = "INT32")
  })
  dom <- tiledb::Domain(ctx, dims)
  #TODO: better datatype checking
  if (is.double(x)) {
    typestr <- "FLOAT64"   
  } else if (is.integer(x)) {
    typestr <- "INT32"
  } else {
    stop(paste("invalid array type \"", typeof(x), "\""))
  }
  val <- tiledb::Attr(ctx, "", type = typestr)
  return(tiledb::ArraySchema(ctx, dom, c(val)))
}

setMethod("show", signature(object = "ArraySchema"),
          function(object) {
            tiledb_array_schema_dump(object@ptr)
          })

#' @export
setGeneric("domain", function(object, ...) standardGeneric("domain"))

#' @export
setMethod("domain", "ArraySchema",
          function(object) {
            ptr <- tiledb_array_schema_domain(object@ptr)
            Domain.from_ptr(ptr)
          })

#' @export
setGeneric("dimensions", function(object, ...) standardGeneric("dimensions"))

#' @export
setMethod("dimensions", "ArraySchema",
          function(object) dimensions(domain(object)))

#' @export
setGeneric("attrs", function(object, idx, ...) standardGeneric("attrs"))

#' @export
setMethod("attrs", signature("ArraySchema"),
          function (object, ...) {
            attr_ptrs <- tiledb_array_schema_attributes(object@ptr)
            attrs <- lapply(attr_ptrs, function(ptr) Attr.from_ptr(ptr))
            names(attrs) <- sapply(attrs, function(attr) {
              n <- tiledb::name(attr)
              return(ifelse(n == "__attr", "", n))
            })
            return(attrs)
          })

#' @export
setMethod("attrs", signature("ArraySchema", "character"),
          function(object, idx, ...) {
            attrs <- tiledb::attrs(object)
            return(attrs)
          })

#' @export
setMethod("attrs", signature("ArraySchema", "numeric"),
          function(object, idx, ...) {
            attrs <- tiledb::attrs(object) 
            return(attrs[idx])
          })

#' @export
setGeneric("cell_order", function(object, ...) standardGeneric("cell_order"))

#' @export
setMethod("cell_order", "ArraySchema",
          function(object) {
            tiledb_array_schema_cell_order(object@ptr) 
          })

#' @export
setGeneric("tile_order", function(object, ...) standardGeneric("tile_order"))

#' @export
setMethod("tile_order", "ArraySchema",
          function(object) {
            tiledb_array_schema_tile_order(object@ptr) 
          })

#' @export
setGeneric("compressor", function(object, ...) standardGeneric("compressor"))

#' @export
setMethod("compressor", "ArraySchema",
          function(object) {
            coords_ptr <- tiledb_array_schema_coords_compressor(object@ptr)
            offsets_ptr <- tiledb_array_schema_offsets_compressor(object@ptr)
            return(c(coords = Compressor.from_ptr(coords_ptr), 
                     offsets = Compressor.from_ptr(offsets_ptr)))
          })

#' @export
setGeneric("is.sparse", function(object, ...) standardGeneric("is.sparse"))

#' @export
setMethod("is.sparse", "ArraySchema",
          function(object) {
            tiledb_array_schema_sparse(object@ptr) 
          })
 
#' @export
setGeneric("ndim", function(object, ...) standardGeneric("ndim"))

#' @export
setMethod("ndim", "ArraySchema",
          function(object) {
            dom <- tiledb::domain(object)
           return(tiledb::ndim(dom)) 
          })

#' @export
dim.ArraySchema <- function(x) dim(tiledb::domain(x))