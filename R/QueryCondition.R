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


#' @exportClass tiledb_query_condition
setClass("tiledb_query_condition",
         slots = list(ptr = "externalptr"))

#' @export
tiledb_query_condition <- function(ctx = tiledb_get_context()) {
    stopifnot(`needs ctx object`=is(ctx, "tiledb_ctx"))
    ptr <- libtiledb_query_condition(ctx@ptr)
    query_condition <- new("tiledb_query_condition", ptr = ptr)
    invisible(query_condition)
}

#' @export
tiledb_query_condition_init <- function(attr, value, dtype, op, qc = tiledb_query_condition()) {
    stopifnot(`needs query condition object`=is(qc, "tiledb_query_condition"),
              `attr must be character`=is.character(attr),
              `dtype must be character`=is.character(dtype),
              `op must be character`=is.character(op))
    op <- match.arg(op, c("LT", "LE", "GT", "GE", "EQ", "NE"))
    ## maybe check dtype too
    libtiledb_query_condition_init(qc@ptr, attr, value, dtype, op)
    invisible(qc)
}

#' @export
tiledb_query_condition_combine <- function(lhs, rhs, op) {
    stopifnot(`needs query condition object on lhs`=is(lhs, "tiledb_query_condition"),
              `needs query condition object on rhs`=is(rhs, "tiledb_query_condition"),
              `op must be character`=is.character(op))
    op <- match.arg(op, c("AND", "OR", "NOT"))
    qc <- tiledb_query_condition()
    qc@ptr <- libtiledb_query_condition_combine(lhs@ptr, rhs@ptr, op)
    invisible(qc)
}



## TODO --> Query.R file

#' @export
tiledb_query_set_condition <- function(qry, qc) {
    stopifnot(`needs query object`=is(qry, "tiledb_query"),
              `needs query condition object`=is(qc, "tiledb_query_condition"))
    qry@ptr <- libtiledb_query_set_condition(qry@ptr, qc@ptr)
    invisible(qry)
}
