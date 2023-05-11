#  MIT License
#
#  Copyright (c) 2017-2023 TileDB Inc.
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

##' Export Query Buffer to Pair of Arrow IO Pointers
##'
##' This function exports the named buffer from a \sQuote{READ} query
##' to two Arrow C pointers.
##' @param query A TileDB Query object
##' @param name A character variable identifying the buffer
##' @param ctx tiledb_ctx object (optional)
##' @return A two-element vector where the two elements are
##' external pointers to the Arrow array and schema
##' @export
tiledb_query_export_buffer <- function(query, name, ctx = tiledb_get_context()) {
    stopifnot(`The 'query' argument must be a tiledb query` = is(query, "tiledb_query"),
              `The 'name' argument must be character` = is.character(name))
    res <- libtiledb_query_export_buffer(ctx@ptr, query@ptr, name)
    res
}

##' Import to Query Buffer from Pair of Arrow IO Pointers
##'
##' This function imports to the named buffer for a \sQuote{WRITE} query
##' from two Arrow exerternal pointers.
##' @param query A TileDB Query object
##' @param name A character variable identifying the buffer
##' @param arrowpointers A two-element list vector with two external pointers
##' to an Arrow Array and Schema, respectively
##' @param ctx tiledb_ctx object (optional)
##' @return The update Query external pointer is returned
##' @export
tiledb_query_import_buffer <- function(query, name, arrowpointers, ctx = tiledb_get_context()) {
    stopifnot(`The 'query' argument must be a tiledb query` = is(query, "tiledb_query"),
              `The 'name' argument must be character` = is.character(name),
              `The 'arrowpointers' argument must be list of length two` = is.list(arrowpointers) && length(arrowpointers)==2)
    query@ptr <- libtiledb_query_import_buffer(ctx@ptr, query@ptr, name, arrowpointers)
    query
}

##' Allocate (or Release) Arrow Array and Schema Pointers
##'
##' These functions allocate (and free) appropriate pointer objects
##' for, respectively, Arrow array and schema objects.
##' @param ptr A external pointer object previously allocated with these functions
##' @return The allocating functions return the requested pointer
##' @export
tiledb_arrow_array_ptr <- function() {
    res <- .allocate_arrow_array_as_xptr()
}

##' @rdname tiledb_arrow_array_ptr
##' @export
tiledb_arrow_schema_ptr <- function() {
    res <- .allocate_arrow_schema_as_xptr()
}

##' @rdname tiledb_arrow_array_ptr
##' @export
tiledb_arrow_array_del <- function(ptr) {
    .delete_arrow_array_from_xptr(ptr)
}

##' @rdname tiledb_arrow_array_ptr
##' @export
tiledb_arrow_schema_del <- function(ptr) {
    .delete_arrow_schema_from_xptr(ptr)
}

##' @noRd
##' @export
.check_arrow_pointers <- function(arrlst) {
    stopifnot("First argument must be an external pointer to ArrowArray" = check_arrow_array_tag(arrlst[[1]]),
              "Second argument must be an external pointer to ArrowSchema" = check_arrow_schema_tag(arrlst[[2]]))
}

##' @noRd
##' @export
.as_arrow_table <- function(arrlst) {
    .check_arrow_pointers(arrlst)
    if (!requireNamespace("arrow", quietly=TRUE)) {
        stop("This functionality requires the 'arrow' package to be installed.", call. = FALSE)
    } else {
        arrow::as_arrow_table(arrow::RecordBatch$import_from_c(arrlst[[1]], arrlst[[2]]))
    }
}

##' @noRd
##' @export
.tiledb_set_arrow_config <- function(ctx = tiledb_get_context()) {
    cfg <- tiledb_config()        # for var-num columns such as char we need these
    cfg["sm.var_offsets.bitsize"] <- "64"
    cfg["sm.var_offsets.mode"] <- "elements"
    cfg["sm.var_offsets.extra_element"] <- "true"
    ctx <- tiledb_ctx(cfg)
}

##' @noRd
##' @export
.tiledb_unset_arrow_config <- function(ctx = tiledb_get_context()) {
    cfg <- tiledb_config()        # for var-num columns such as char we need these
    cfg["sm.var_offsets.bitsize"] <- "64"
    cfg["sm.var_offsets.mode"] <- "bytes"
    cfg["sm.var_offsets.extra_element"] <- "false"
    ctx <- tiledb_ctx(cfg)
}
