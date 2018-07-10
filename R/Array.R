#' Creates a new TileDB array given an input schema.
#' 
#' @param uri URI specifying path to create the TileDB array object
#' @param schema tiledb_array_schema object
#' 
#' @examples 
#' pth <- tempdir()
#' ctx <- tiledb_ctx()
#' dom <- tiledb_domain(ctx, dims = c(tiledb_dim(ctx, "d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(ctx, dom, attrs = c(tiledb_attr(ctx, "a1")))
#' tiledb_array_create(pth, sch)
#' 
#' tiledb_object_type(ctx, pth)
#'
#' @export
tiledb_array_create <- function(uri, schema) {
  if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  } else if (missing(schema) || !is(schema, "tiledb_array_schema")) {
    stop("argument schema must a tiledb_array_schema") 
  }
  return(libtiledb_array_create(uri, schema@ptr))
}