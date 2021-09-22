
#' @export
setGeneric("tdb_filter", function(x, ...) standardGeneric("tdb_filter"))

#' @export
setMethod("tdb_filter", signature("tiledb_array"), function(x, ..., strict=TRUE) {
    qc <- parse_query_condition(..., ta=x, debug=FALSE, strict=strict)
    if (is.null(qc))
        return(x)
    if (isTRUE(x@query_condition@init)) {  # if prior qc exists, combine by AND
        x@query_condition <- tiledb_query_condition_combine(x@query_condition, qc, "AND")
    } else {                                     # else just assign
        x@query_condition <- qc
    }
    x
})

#' @export
setGeneric("tdb_select", function(x, ...) standardGeneric("tdb_select"))

#' @export
setMethod("tdb_select", signature("tiledb_array"), function(x, ...) {
    if (length(x@sil) == 0) x@sil <- .fill_schema_info_list(x@uri)
    ## helper with a nod to data.table and its name_dots
    names_from_dots <- function(...) {
        dot_sub <- as.list(substitute(list(...)))[-1L]
        vnames <- character(length(dot_sub))
        notnamed <- vnames == ""
        syms <- sapply(dot_sub, is.symbol)  # save the deparse() in most cases of plain symbol
        for (i in which(notnamed)) {
            tmp <- if (syms[i]) as.character(dot_sub[[i]]) else deparse(dot_sub[[i]])[1L]
            if (tmp == make.names(tmp)) vnames[i] <- tmp
        }
        vnames
    }

    vec <- names_from_dots(...)
    ind <- match(vec, x@sil$names)     		# match against schema names
    ind <- ind[x@sil$status[ind] == 2L]  	# allow only attributes (where status == 2)
    newvec <- na.omit(x@sil$names[ ind ])  	# and create subset (filtering NA for wrong entry)
    x@attrs <- newvec
    x
})

#' @export
setGeneric("tdb_collect", function(x, ...) standardGeneric("tdb_collect"))

#' @export
setMethod("tdb_collect", signature("tiledb_array"), function(x, ...) {
    x[]
})

.fill_schema_info_list <- function(uri) {
    sch <- schema(uri)
    list(names=tiledb_schema_get_names(sch),
         types=tiledb_schema_get_types(sch),
         status=tiledb_schema_get_dim_attr_status(sch))
}
