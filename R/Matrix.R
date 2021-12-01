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


##' Create a TileDB array from an R matrix, or return an R matrix
##'
##' The functions \code{fromMatrix} and \code{toMatrix} help in storing
##' (and retrieving) matrices using a TileDB backend. In particular they help
##' for matrices with explicit rownames.
##' @param obj A sparse matrix object.
##' @param uri A character variable with an Array URI.
##' @param filter A character variable vector, defaults to \sQuote{ZSTD}, for
##' one or more filters to be applied to each attribute;
##' @param capacity A integer value with the schema capacity, default is 10000.
##' @return Null, invisibly.
##'
##' @export
fromMatrix <- function(obj,
                       uri,
                       filter="ZSTD",
                       capacity = 10000L) {

    stopifnot(`Argument 'obj' must be matrix object` = inherits(obj, "matrix"),
              `Argument 'uri' must be character` = is.character(uri))

    dims <- dim(obj)
    dimnm <- dimnames(obj)
    hasnames <- !is.null(dimnm) && !is.null(dimnm[[1]]) && !is.null(dimnm[[2]])
    if (hasnames) {
        dimr <- tiledb_dim(name="rows", type = "ASCII", tile = NULL, domain = c(NULL, NULL))
        dimc <- tiledb_dim(name="cols", type = "ASCII", tile = NULL, domain = c(NULL, NULL))
        dom <- tiledb_domain(dims = c(dimr, dimc))
    } else {
        dimr <- tiledb_dim(name="rows", type = "INT32", tile = dims[1], domain = c(1L, dims[1]))
        dimc <- tiledb_dim(name="cols", type = "INT32", tile = dims[2], domain = c(1L, dims[2]))
        dom <- tiledb_domain(dims = c(dimr, dimc))
    }

    cl <- class(obj[1,1])
    if (cl == "integer")
        tp <- "INT32"
    else if (cl == "numeric")
        tp <- "FLOAT64"
    else
        stop("Currently unsupported type: ", cl)

    filterlist <- tiledb_filter_list(sapply(filter, tiledb_filter))

    attx <- tiledb_attr(name="x", type = tp, ncells = 1, filter_list = filterlist)
    schema <- tiledb_array_schema(dom, attrs=attx, sparse = hasnames, capacity=capacity)
    tiledb_array_create(uri, schema)
    arr <- tiledb_array(uri)
    if (hasnames) {
        df <- data.frame(rows = rep(dimnm[[1]], dims[2]),
                         cols = rep(dimnm[[2]], each=dims[1]),
                         x = as.vector(obj))
        arr[] <- df
    } else {
        arr[] <- obj
    }
    invisible(NULL)
}

##' @rdname fromMatrix
##' @export
toMatrix <- function(uri) {
    stopifnot(`Argument 'uri' must be character` = is.character(uri))
    tiledb_array(uri, return_as="matrix")[]
}
