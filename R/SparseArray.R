setClass("SparseArray",
         slots = list(ctx = "Ctx", schema = "ArraySchema", uri = "character"))

#' @export
SparseArray <- function(ctx, schema, uri) {
  if (missing(ctx) || !is(ctx, "Ctx")) {
    stop("argument ctx must be a tiledb::Ctx")
  }
  if (missing(schema) || !is(schema, "ArraySchema")) {
    if (!is.sparse(schema)) {
      stop("schema must be a sparse array schema") 
    }
    stop("argument schema must a tiledb::ArraySchema") 
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }
  uri <- tiledb_array_create(schema@ptr, uri) 
  new("SparseArray", ctx = ctx, schema = schema, uri = uri) 
}

SparseArray.load <- function(ctx, uri) {
  if (missing(ctx) || !is(ctx, "Ctx")) {
    stop("argument ctx must be a tiledb::Ctx")
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }
  schema_ptr <- tiledb_array_load(ctx@ptr, uri)
  schema <- ArraySchema.from_ptr(schema_ptr)
  if (!is.sparse(schema)) {
    stop("array URI must be a sparse array") 
  }
  return(new("SparseArray", ctx = ctx, schema = schema, uri = uri))
}

#' @export
setMethod("is.sparse", "SparseArray", function(object) TRUE)