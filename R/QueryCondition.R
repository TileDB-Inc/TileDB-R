#  MIT License
#
#  Copyright (c) 2021-2022 TileDB Inc.
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

#' An S4 class for a TileDB QueryCondition object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @slot init A logical variable tracking if the query condition object has been
#' initialized
#' @exportClass tiledb_query_condition
setClass("tiledb_query_condition",
         slots = list(ptr = "externalptr",
                      init = "logical"))

#' Creates a 'tiledb_query_condition' object
#'
#' @param ctx (optional) A TileDB Ctx object; if not supplied the default
#' context object is retrieved
#' @return A 'tiledb_query_condition' object
#' @export
tiledb_query_condition <- function(ctx = tiledb_get_context()) {
    stopifnot("The argument must be a ctx object" = is(ctx, "tiledb_ctx"),
              "This function needs TileDB 2.3.0 or newer" = tiledb_version(TRUE) >= "2.3.0")
    ptr <- libtiledb_query_condition(ctx@ptr)
    query_condition <- new("tiledb_query_condition", ptr = ptr, init = FALSE)
    invisible(query_condition)
}

#' Initialize a 'tiledb_query_condition' object
#'
#' Initializes (and possibly allocates) a query condition object using a triplet of
#' attribute name, comparison value, and operator.  Six types of conditions are supported,
#' they all take a single scalar comparison argument and attribute to compare against.
#' At present only integer or numeric attribute comparisons are implemented.
#' @param attr A character value with the scheme attribute name
#' @param value A scalar value that the attribute is compared against
#' @param dtype A character value with the TileDB data type of the attribute column, for
#' example 'FLOAT64' or 'INT32'
#' @param op A character value with then comparison operation, this must be one of
#' 'LT', 'LE', 'GT', 'GE', 'EQ', 'NE'.
#' @param qc (optional) A 'tiledb_query_condition' object to be initialized by this call,
#' if none is given a new one is allocated.
#' @return The initialized 'tiledb_query_condition' object
#' @export
tiledb_query_condition_init <- function(attr, value, dtype, op, qc = tiledb_query_condition()) {
    stopifnot("Argument 'qc' with query condition object required" = is(qc, "tiledb_query_condition"),
              "Argument 'attr' must be character" = is.character(attr),
              "Argument 'value' must be of length one" = (is.vector(value) || bit64::is.integer64(value)) && all.equal(length(value),1),
              "Argument 'dtype' must be character" = is.character(dtype),
              "Argument 'op' must be character" = is.character(op))
    op <- match.arg(op, c("LT", "LE", "GT", "GE", "EQ", "NE"))
    ## if dtype is INT64 or UINT64 but the class of value does not yet inherit from integer64, cast
    if (grepl("INT64", dtype) && !inherits(value, "integer64")) {
        value <- bit64::as.integer64(value)
        #message("QCI ", attr, ", ", value, ", ", class(value)[1], ", ", dtype, ", ", op)
    }
    libtiledb_query_condition_init(qc@ptr, attr, value, dtype, op)
    qc@init <- TRUE
    invisible(qc)
}

#' Combine two 'tiledb_query_condition' objects
#'
#' Combines two query condition object using a relatiional operator. Support for operator
#' 'AND' is generally available, the 'OR' operator is available if TileDB 2.10 or newer is
#' used.
#' @param lhs A 'tiledb_query_condition' object on the left-hand side of the relation
#' @param rhs A 'tiledb_query_condition' object on the left-hand side of the relation
#' @param op A character value with then relation, this must be one of 'AND', 'OR' or 'NOT'.
#' @return The combined 'tiledb_query_condition' object
#' @export
tiledb_query_condition_combine <- function(lhs, rhs, op) {
    stopifnot("Argument 'lhs' must be a query condition object" = is(lhs, "tiledb_query_condition"),
              "Argument 'rhs' must be a query condition object" = is(rhs, "tiledb_query_condition"),
              "Argument 'op' must be a character" = is.character(op))
    op <- match.arg(op, c("AND", "OR", "NOT"))
    qc <- tiledb_query_condition()
    qc@ptr <- libtiledb_query_condition_combine(lhs@ptr, rhs@ptr, op)
    qc@init <- TRUE
    invisible(qc)
}

#' Create a 'tiledb_query_condition' object from an expression
#'
#' The grammar for query conditions is at present constraint to six operators
#' and three boolean types.
#'
#' @param expr An expression that is understood by the TileDB grammar for
#' query conditions.
#' @param ta An optional tiledb_array object that the query condition is applied to
#' @param debug A boolean toogle to enable more verbose operations, defaults
#' to 'FALSE'.
#' @param strict A boolean toogle to, if set, errors if a non-existing attribute is selected
#' or filtered on, defaults to 'TRUE'; if 'FALSE' a warning is shown by execution proceeds.
#' @param use_int64 A boolean toggle to switch to \code{integer64} if \code{integer} is seen,
#' default is false to remain as a default four-byte \code{int}
#' @return A `tiledb_query_condition` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' \dontrun{
#' uri <- "mem://airquality"    # change to on-disk for persistence
#' fromDataFrame(airquality, uri, col_index=c("Month", "Day"))  # dense array
#' ## query condition on dense array requires extended=FALSE
#' tiledb_array(uri, return_as="data.frame", extended=FALSE,
#'              query_condition=parse_query_condition(Temp > 90))[]
#' }
#' @export
parse_query_condition <- function(expr, ta=NULL, debug=FALSE, strict=TRUE, use_int64=FALSE) {
    .hasArray <- !is.null(ta) && is(ta, "tiledb_array")
    if (.hasArray && length(ta@sil) == 0) ta@sil <- .fill_schema_info_list(ta@uri)
    .isComparisonOperator <- function(x) tolower(as.character(x)) %in% c(">", ">=", "<", "<=", "==", "!=", "%in%")
    .isBooleanOperator <- function(x) as.character(x) %in% c("&&", "||", "!")
    .isAscii <- function(x) grepl("^[[:alnum:]_]+$", x)
    .isInteger <- function(x) grepl("^[[:digit:]]+$", as.character(x))
    .isDouble <- function(x) grepl("^[[:digit:]\\.]+$", as.character(x)) && length(grepRaw(".", as.character(x), fixed = TRUE, all = TRUE)) == 1
    .isInOperator <- function(x) tolower(as.character(x)) == "%in%"
    .errorFunction <- if (strict) stop else warning
    .getType <- function(x, use_int64=FALSE) {
        if (isTRUE(.isInteger(x))) { if (use_int64) "INT64" else "INT32" }
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
    .neweqcond <- function(val, attr) {
        if (debug) cat("   ", attr, "EQ", val, "\n")
        tiledb_query_condition_init(attr = attr, value = val, dtype = "ASCII", op = "EQ")
    }
    .neworcond <- function(op1, op2) tiledb_query_condition_combine(op1, op2, "OR")
    .makeExpr <- function(x, debug=FALSE) {
        if (is.symbol(x)) {
            stop("Unexpected symbol in expression: ", format(x))
        } else if (.isBooleanOperator(x[1])) {
            if (debug) cat("-- [", as.character(x[2]), "]",
                           " ", as.character(x[1]),
                           " [", as.character(x[3]), "]\n", sep="")
            .makeExpr(x[[2]])
            .makeExpr(x[[3]])
            tiledb_query_condition_combine(.makeExpr(x[[2]]),
                                           .makeExpr(x[[3]]),
                                           .mapBoolToCharacter(as.character(x[1])))
        } else if (.isInOperator(x[1])) {
            if (debug) cat("in: [", as.character(x[2]), "]",
                           " ", as.character(x[1]),
                           " [", as.character(x[3]), "]\n", sep="")
            attr <- as.character(x[2])
            vals <- eval(parse(text=as.character(x[3])))
            eqconds <- Map(.neweqcond, vals, attr)
            orcond <- Reduce(.neworcond, eqconds)
        } else if (.isComparisonOperator(x[1])) {
            op <- as.character(x[1])
            attr <- as.character(x[2])
            ch <- as.character(x[3])
            dtype <- .getType(ch, use_int64)
            if (.hasArray) {
                ind <- match(attr, ta@sil$names)
                if (!is.finite(ind)) {
                    .errorFunction("No attribute '", attr, "' present.", call. = FALSE)
                    return(NULL)
                }
                if (ta@sil$status[ind] != 2) {
                    .errorFunction("Argument '", attr, "' is not an attribute.", call. = FALSE)
                    return(NULL)
                }
                dtype <- ta@sil$types[ind]
            }
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
    .makeExpr(e, debug)
}
