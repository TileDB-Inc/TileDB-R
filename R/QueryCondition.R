#  MIT License
#
#  Copyright (c) 2021 TileDB Inc.
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
    stopifnot(`needs ctx object` = is(ctx, "tiledb_ctx"),
              `needs TileDB 2.3.0 or newer` = tiledb_version(TRUE) >= "2.3.0")
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
    stopifnot(`needs query condition object`=is(qc, "tiledb_query_condition"),
              `attr must be character`=is.character(attr),
              `value must be of length one`=is.vector(value) && all.equal(length(value),1),
              `dtype must be character`=is.character(dtype),
              `op must be character`=is.character(op))
    op <- match.arg(op, c("LT", "LE", "GT", "GE", "EQ", "NE"))
    ## maybe check dtype too
    libtiledb_query_condition_init(qc@ptr, attr, value, dtype, op)
    qc@init <- TRUE
    invisible(qc)
}

#' Combine two 'tiledb_query_condition' objects
#'
#' Combines two query condition object using a relatiional operator. Note that at present
#' only 'AND' is supported.
#' @param lhs A 'tiledb_query_condition' object on the left-hand side of the relation
#' @param rhs A 'tiledb_query_condition' object on the left-hand side of the relation
#' @param op A character value with then relation, this must be one of 'AND', 'OR' or 'NOT'.
#' @return The combined 'tiledb_query_condition' object
#' @export
tiledb_query_condition_combine <- function(lhs, rhs, op) {
    stopifnot(`needs query condition object on lhs`=is(lhs, "tiledb_query_condition"),
              `needs query condition object on rhs`=is(rhs, "tiledb_query_condition"),
              `op must be character`=is.character(op))
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
#' @param debug A boolean toogle to enable more verbose operations, defaults
#' to 'FALSE'.
#' @return A `tiledb_query_condition` object
#' @export
parse_query_condition <- function(expr, debug=FALSE) {
    .isComparisonOperator <- function(x) as.character(x) %in% c(">", ">=", "<", "<=", "==", "!=")
    .isBooleanOperator <- function(x) as.character(x) %in% c("&&", "||", "!")
    .isInteger <- function(x) as.character(as.integer(x)) == x
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
            .makeExpr(x[[2]])
            .makeExpr(x[[3]])
            tiledb_query_condition_combine(.makeExpr(x[[2]]),
                                           .makeExpr(x[[3]]),
                                           .mapBoolToCharacter(as.character(x[1])))

        } else if (.isComparisonOperator(x[1])) {
            if (debug) cat("   [",as.character(x[2]),"] ",
                           as.character(x[1]), " (aka ", .mapOpToCharacter(as.character(x[1])), ")",
                           " [",as.character(x[3]), "]",
                           if (.isInteger(as.character(x[3]))) " int" else " float",
                           "\n", sep="")
            tiledb_query_condition_init(attr = as.character(x[2]), # still need to check again schema
                                        value = as.numeric(as.character(x[3])),
                                        dtype = if (.isInteger(as.character(x[3]))) "INT32" else "FLOAT64",
                                        op = .mapOpToCharacter(as.character(x[1])))
        } else {
            stop("Unexpected token in expression: ", format(x))
        }
    }

    e <- substitute(expr)
    .makeExpr(e)
}
