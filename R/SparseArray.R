setClass("SparseArray",
         slots = list(ctx = "tiledb_ctx", schema = "tiledb_array_schema", uri = "character"))

#' @export
SparseArray <- function(ctx, schema, uri) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  }
  if (missing(schema) || !is(schema, "tiledb_array_schema")) {
    if (!is.sparse(schema)) {
      stop("schema must be a sparse array schema") 
    }
    stop("argument schema must a tiledb_array_schema") 
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }
  uri <- tiledb_array_create(schema@ptr, uri) 
  new("SparseArray", ctx = ctx, schema = schema, uri = uri) 
}

SparseArray.load <- function(ctx, uri) {
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }
  schema_ptr <- tiledb_array_load(ctx@ptr, uri)
  schema <- tiledb_array_schema.from_ptr(schema_ptr)
  if (!is.sparse(schema)) {
    stop("array URI must be a sparse array") 
  }
  return(new("SparseArray", ctx = ctx, schema = schema, uri = uri))
}

#' @export
setMethod("is.sparse", "SparseArray", function(object) TRUE)