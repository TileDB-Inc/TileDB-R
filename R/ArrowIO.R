
##' Export Query Buffer to Pair of Arrow IO Pointers
##'
##' This function exports the named buffer from a \sQuote{READ} query
##' to two Arrow C pointers.
##' @param query A TileDB Query object
##' @param name A character variable identifying the buffer
##' @return A two-element numeric vector where the two elements are
##' pointers to the Arrow array and schema
##' @export
tiledb_query_export_buffer <- function(query, name) {
    stopifnot(`query argument`=is(query, "tiledb_query"),
              `name argument`=is.character(name))
    res <- libtiledb_query_export_buffer(query@ptr, name)
    res
}

##' Import to Query Buffer from Pair of Arrow IO Pointers
##'
##' This function imports to the named buffer for a \sQuote{WRITE} query
##' from two Arrow C pointers.
##' @param query A TileDB Query object
##' @param name A character variable identifying the buffer
##' @param arrowpointers A two-element numeric vector with two pointers
##' to an Arrow Array and Schema, respectively
##' @return The update Query external pointer is returned
##' @export
tiledb_query_import_buffer <- function(query, name, arrowpointers) {
    stopifnot(`query argument` = is(query, "tiledb_query"),
              `name argument` = is.character(name),
              `arrow pointers` = is.numeric(arrowpointers),
              `length of arrow pointers vectors` = length(arrowpointers)==2)
    qry@ptr <- libtiledb_query_import_buffer(query@ptr, name, arrowpointers)
    qry
}
