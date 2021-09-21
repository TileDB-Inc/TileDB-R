
#' @export
setGeneric("tdb_filter", function(x, ...) standardGeneric("tdb_filter"))

#' @export
setMethod("tdb_filter", signature("tiledb_array"), function(x, ..., strict=TRUE) {
    qc <- .parse_query_condition(x, ..., debug=FALSE, strict=strict)
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

#' Create a 'tiledb_query_condition' object for from an expression
#'
#' The grammar for query conditions is at present constraint to six operators
#' and three boolean types.
#'
#' @param ta A \code{tiledb_array} object.
#' @param expr An expression that is understood by the TileDB grammar for
#' query conditions.
#' @param debug A boolean toogle to enable more verbose operations, defaults
#' to 'FALSE'.
#' @param strict A boolean toogle to enable more stringent operations, i.e. stopping
#' on error rather than carrying one, defaults to 'TRUE'.
#' @return A `tiledb_query_condition` object
#' @export
.parse_query_condition <- function(ta, expr, debug=TRUE, strict=TRUE) {
    if (length(ta@sil) == 0) ta@sil <- .fill_schema_info_list(ta@uri)
    .isComparisonOperator <- function(x) as.character(x) %in% c(">", ">=", "<", "<=", "==", "!=")
    .isBooleanOperator <- function(x) as.character(x) %in% c("&&", "||", "!")
    .isAscii <- function(x) grepl("^[[:alnum:]_]+$", x)
    .isInteger <- function(x) grepl("^[[:digit:]]+$", as.character(x))
    .isDouble <- function(x) grepl("^[[:digit:]\\.]+$", as.character(x)) && length(grepRaw(".", as.character(x), fixed = TRUE, all = TRUE)) == 1
    .errorFunction <- if (strict) stop else warning
    .getType <- function(x) {
        if (isTRUE(.isInteger(x))) "INT32"
        else if (isTRUE(.isDouble(x))) "FLOAT64"
        else "ASCII"
    }
    .mapOpToCharacter <- function(x) switch(x,
                                            `>`  = "GT",
                                            `>=` = "GE",
                                            `<`  = "LT",
                                            `<=` = "LE",
                                            `==` = "EQ",
                                            `!=` = "NE")
    .mapBoolToCharacter <- function(x) switch(x,
                                              `&&` = "AND",
                                              `||` = "OR",
                                              `!`  = "NOT")
    .makeExpr <- function(x) {
        if (is.symbol(x)) {
            stop("Unexpected symbol in expression: ", format(x))
        } else if (.isBooleanOperator(x[1])) {
            if (debug) cat("-- [", as.character(x[2]), "]",
                           " ", as.character(x[1]),
                           " [", as.character(x[3]), "]\n", sep="")
            .makeExpr(x[2])
            .makeExpr(x[3])
            tiledb_query_condition_combine(.makeExpr(x[2]),
                                           .makeExpr(x[3]),
                                           .mapBoolToCharacter(as.character(x[1])))

        } else if (.isComparisonOperator(x[1])) {
            op <- as.character(x[1])
            attr <- as.character(x[2])
            ch <- as.character(x[3])
            ind <- match(attr, ta@sil$names)
            if (!is.finite(ind)) {
                .errorFunction("No attibute '", attr, "' present.", call. = FALSE)
                return(NULL)
            }
            if (ta@sil$status[ind] != 2) {
                .errorFunction("Argument '", attr, "' is not an attribute.", call. = FALSE)
                return(NULL)
            }
            dtype <- ta@sil$types[ind]
            if (debug) cat("   [", attr,"] ",
                           op, " (aka ", .mapOpToCharacter(op), ")",
                           " [",ch, "] ", dtype, "\n", sep="")
            tiledb_query_condition_init(attr = attr,
                                        value = if (dtype == "ASCII") ch else as.numeric(ch),
                                        dtype = dtype,
                                        op = .mapOpToCharacter(op))
        } else {
            stop("Unexpected token in expression: ", format(x))
        }
    }
    e <- substitute(expr)
    .makeExpr(e)
}
