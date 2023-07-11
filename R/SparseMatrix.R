#  MIT License
#
#  Copyright (c) 2017-2022 TileDB Inc.
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

## sparse matrix helper 'roughly similar' to fromDataFrame()

##' Create (or return) a TileDB sparse array
##'
##' The functions \code{fromSparseMatrix} and \code{toSparseMatrix} help in storing
##' (and retrieving) sparse matrices using a TileDB backend.
##' @param obj A sparse matrix object.
##' @param uri A character variable with an Array URI.
##' @param cell_order A character variable with one of the TileDB cell order values,
##' default is \dQuote{COL_MAJOR}.
##' @param tile_order A character variable with one of the TileDB tile order values,
##' default is \dQuote{COL_MAJOR}.
##' @param filter A character variable vector, defaults to \sQuote{ZSTD}, for
##' one or more filters to be applied to each attribute;
##' @param capacity A integer value with the schema capacity, default is 10000.
##' @return Null, invisibly.
##' @examples
##' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
##' \dontrun{
##' if (requireNamespace("Matrix", quietly=TRUE)) {
##'     library(Matrix)
##'     set.seed(123)      # just to fix it
##'     mat <- matrix(0, nrow=20, ncol=10)
##'     mat[sample(seq_len(200), 20)] <- seq(1, 20)
##'     spmat <- as(mat, "dgTMatrix")  # sparse matrix in dgTMatrix format
##'     uri <- "sparse_matrix"
##'     fromSparseMatrix(spmat, uri)   # now written
##'     chk <- toSparseMatrix(uri)     # and re-read
##'     print(chk)
##'     all.equal(spmat, chk)
##' }
##' }
##' @importFrom methods as
##' @export
fromSparseMatrix <- function(obj,
                             uri,
                             cell_order = "ROW_MAJOR",
                             tile_order = "ROW_MAJOR",
                             filter="ZSTD",
                             capacity = 10000L) {

    stopifnot(`Argument 'obj' must be Matrix object` = inherits(obj, "Matrix"),
              `Argument 'obj' must be sparse` = is(obj, "sparseMatrix"),
              `Argument 'uri' must be character` = is.character(uri))

    dimnm <- dimnames(obj)
    classIn <- "dgTMatrix"
    if (class(obj)[1] != classIn) {
        classIn <- class(obj)[1]
        obj <- as(obj, "TsparseMatrix")
    }

    dimi <- tiledb_dim(name="i", type = "FLOAT64",  # wider range
                       tile = as.numeric(obj@Dim[1]),
                       domain = c(0, obj@Dim[1]-1L))
    dimj <- tiledb_dim(name="j", type = "FLOAT64",  # wider range
                       tile = as.numeric(obj@Dim[2]),
                       domain = c(0, obj@Dim[2]-1L))
    dom <- tiledb_domain(dims = c(dimi, dimj))

    cl <- class(obj@x)[1]
    if (cl == "integer")
        tp <- "INT32"
    else if (cl == "numeric")
        tp <- "FLOAT64"
    else
        stop("Currently unsupported type: ", cl)

    filterlist <- tiledb_filter_list(sapply(filter, tiledb_filter))

    attx <- tiledb_attr(name="x", type = tp, ncells = 1, filter_list = filterlist)
    schema <- tiledb_array_schema(dom,
                                  attrs = attx,
                                  cell_order = cell_order,
                                  tile_order = tile_order,
                                  sparse = TRUE,
                                  capacity=capacity)
    tiledb_array_create(uri, schema)
    arr <- tiledb_array(uri)
    arr[] <- data.frame(i = obj@i, j = obj@j, x = obj@x)

    if (!is.null(dimnm[[1]])) fromDataFrame(data.frame(names=dimnm[[1]]), paste0(uri, "_rows"))
    if (!is.null(dimnm[[2]])) fromDataFrame(data.frame(names=dimnm[[2]]), paste0(uri, "_cols"))

    invisible(NULL)
}

##' @rdname fromSparseMatrix
##' @export
toSparseMatrix <- function(uri) {
    stopifnot(`Argument 'uri' must be character` = is.character(uri))

    arr <- tiledb_array(uri, return_as="data.frame", query_layout="UNORDERED")
    obj <- arr[]

    dimnm <- list(NULL, NULL)        # by default no dimnames
    rowarr <- paste0(uri, "_rows")
    vfs <- tiledb_get_vfs()
    if (dir.exists(rowarr)) { # && tiledb_vfs_is_dir(rowarr, vfs)) {
        arr <- tiledb_array(rowarr, extended=FALSE, return_as="data.frame")[]
        dimnm[[1]] <- arr[,1]
    }
    colarr <- paste0(uri, "_cols")
    if (dir.exists(colarr)) { # && tiledb_vfs_is_dir(colarr, vfs)) {
        arr <- tiledb_array(colarr, extended=FALSE, return_as="data.frame")[]
        dimnm[[2]] <- arr[,1]
    }

    dims <- dimensions(domain(schema(uri)))
    d1 <- domain(dims[[1]])
    d2 <- domain(dims[[2]])
    stopifnot(`No column i in data`=!is.na(match("i", colnames(obj))),
              `No column j in data`=!is.na(match("j", colnames(obj))),
              `No column x in data`=!is.na(match("x", colnames(obj))),
              `Matrix package needed`=requireNamespace("Matrix", quietly=TRUE))

    sp <- Matrix::sparseMatrix(i = obj$i + 1,
                               j = obj$j + 1,
                               x = obj$x,
                               dims = c(d1[2] + 1, d2[2] + 1),
                               dimnames = dimnm,
                               repr = "T")

    sp

}
