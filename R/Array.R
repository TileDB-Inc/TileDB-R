#' Creates a new TileDB array given an input schema.
#'
#' @param uri URI specifying path to create the TileDB array object
#' @param schema tiledb_array_schema object
#'
#' @examples
#' pth <- tempdir()
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1")))
#' tiledb_array_create(pth, sch)
#' tiledb_object_type(pth)
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
