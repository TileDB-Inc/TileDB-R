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
                        sparse = FALSE) {
  if (missing(ctx) || !is(ctx, "Ctx")) {
    stop("ctx argument must be a tiledb::Ctx")
  }
  if (missing(domain) || !is(domain, "Domain")) {
    stop("domain argument must be a tiledb::Domain") 
  }
  is_attr <- function(obj) is(obj, "Attr") 
  if (missing(attrs) || length(attrs) == 0 || !all(sapply(attrs, is_attr))) {
    stop("attrs argument must be a list of one or tiledb::Attr's")    
  }
  if (!is.logical(sparse)) {
    stop("sparse argument must be a logical TRUE or FALSE")
  }
  attr_ptrs <- lapply(attrs, function(obj) slot(obj, "ptr"))
  ptr <- tiledb_array_schema(ctx@ptr, domain@ptr, attr_ptrs, cell_order, tile_order, sparse)
  new("ArraySchema", ptr = ptr)
}

setMethod("show", signature(object = "ArraySchema"),
          function(object) {
            tiledb_array_schema_dump(object@ptr)
          })

#' @export
dim.ArraySchema <- function(x) {
  stopifnot(is(x, "ArraySchema"))
  ptr <- tiledb_array_schema_domain(x@ptr)
  dom <- Domain.from_ptr(ptr)
  dim(dom)
}