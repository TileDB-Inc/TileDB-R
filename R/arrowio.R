
##' Export Query Buffer to Pair of Arrow IO Pointers
##'
##' This function exports the name buffer from \sQuote{READ} query
##' to two Arrow C pointers.
##' @param queryxp An external pointer object to TileDB Query object
##' @param name A character variable identifying the buffer
##' @return A two-element numeric vector where the two elements are
##' pointer to the Arrow array and schema
##' @export
tiledb_query_export_buffer <- function(query, name) {
    stopifnot(`query argument`=is(query, "tiledb_query"),
              `name argument`=is.character(name))
    libtiledb_query_export_buffer(query@ptr, name)
}
