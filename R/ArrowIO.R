
##' Export Query Buffer to Pair of Arrow IO Pointers
##'
##' This function exports the named buffer from a \sQuote{READ} query
##' to two Arrow C pointers.
##' @param query A TileDB Query object
##' @param name A character variable identifying the buffer
##' @param ctx tiledb_ctx object (optional)
##' @return A two-element numeric vector where the two elements are
##' pointers to the Arrow array and schema
##' @export
tiledb_query_export_buffer <- function(query, name, ctx = tiledb_get_context()) {
    stopifnot(`query argument`=is(query, "tiledb_query"),
              `name argument`=is.character(name))
    res <- libtiledb_query_export_buffer(ctx@ptr, query@ptr, name)
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
##' @param ctx tiledb_ctx object (optional)
##' @return The update Query external pointer is returned
##' @export
tiledb_query_import_buffer <- function(query, name, arrowpointers, ctx = tiledb_get_context()) {
    stopifnot(`query argument` = is(query, "tiledb_query"),
              `name argument` = is.character(name),
              `arrow pointers` = is.numeric(arrowpointers),
              `length of arrow pointers vectors` = length(arrowpointers)==2)
    query@ptr <- libtiledb_query_import_buffer(ctx@ptr, query@ptr, name, arrowpointers)
    query
}

##' Allocate (or Release) Arrow Array and Schema Pointers
##'
##' These functions allocate (and free) appropriate pointer objects
##' for, respectively, Arrow array and schema objects.
##' @param ptr A pointer object previously allocated with these functions
##' @return The allocating functions return the requested pointer
##' @export
tiledb_arrow_array_ptr <- function() {
    res <- .allocate_arrow_array_as_double()
}

##' @rdname tiledb_arrow_array_ptr
##' @export
tiledb_arrow_schema_ptr <- function() {
    res <- .allocate_arrow_schema_as_double()
}

##' @rdname tiledb_arrow_array_ptr
##' @export
tiledb_arrow_array_del <- function(ptr) {
    .delete_arrow_array_from_double(ptr)
}

##' @rdname tiledb_arrow_array_ptr
##' @export
tiledb_arrow_schema_del <- function(ptr) {
    .delete_arrow_schema_from_double(ptr)
}
