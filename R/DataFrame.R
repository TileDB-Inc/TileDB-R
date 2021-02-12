#  MIT License
#
#  Copyright (c) 2017-2020 TileDB Inc.
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


## helper functions for data frame, roughly modeled on what python has

##' Create a TileDB dense or sparse array from a given \code{data.frame} Object
##'
##' The supplied \code{data.frame} object is (currently) limited to integer,
##' numeric, or character. In addition, three datetime columns are supported
##' with the R representations of \code{Date}, \code{POSIXct} and \code{nanotime}.
##'
##' The created (dense or sparse) array will have as many attributes as there
##' are columns in the \code{data.frame}.  Each attribute will be a single column.
##' For a sparse array, one or more columns have to be designated as dimensions.
##'
##' At present, factor variable are converted to character.
##'
##' @param obj A \code{data.frame} object.
##' @param uri A character variable with an Array URI.
##' @param col_index An optional column index, either numeric with a column index,
##' or character with a column name, designating an index column; default is NULL
##' implying an index column is added when the array is created
##' @param sparse A logical switch to select sparse or dense (the default)
##' @param allows_dups A logical switch to select if duplicate values
##' are allowed or not, default is the same value as \sQuote{sparse}.
##' @param cell_order A character variable with one of the TileDB cell order values,
##' default is \dQuote{COL_MAJOR}.
##' @param tile_order A character variable with one of the TileDB tile order values,
##' default is \dQuote{COL_MAJOR}.
##' @param filter A character variable vector, defaults to \sQuote{ZSTD}, for
##' one or more filters to be applied to each attribute;
##' @param capacity A integer value with the schema capacity, default is 10000.
##' @param tile_domain An integer vector of size two specifying the integer domain of the row
##' dimension; if \code{NULL} the row dimension of the \code{obj} is used.
##' @param tile_extent An integer value for the tile extent of the row dimensions;
##' if \code{NULL} the row dimension of the \code{obj} is used. Note that the \code{tile_extent}
##' cannot exceed the tile domain.
##' @param debug Logical flag to select additional output
##' @return Null, invisibly.
##' @examples
##' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
##' \dontrun{
##' uri <- tempfile()
##' ## turn factor into character
##' irisdf <- within(iris, Species <- as.character(Species))
##' fromDataFrame(irisdf, uri)
##' arr <- tiledb_array(uri, as.data.frame=TRUE, sparse=FALSE)
##' newdf <- arr[]
##' all.equal(iris, newdf)
##' }
##' @export
fromDataFrame <- function(obj, uri, col_index=NULL, sparse=FALSE, allows_dups=sparse,
                          cell_order = "ROW_MAJOR", tile_order = "ROW_MAJOR", filter="ZSTD",
                          capacity = 10000L, tile_domain = NULL, tile_extent = NULL, debug = FALSE) {

    if (!is.null(col_index) && is.character(col_index)) col_index <- match(col_index, colnames(obj))
    dims <- dim(obj)

    ## turn factor columns in char columns
    factcols <- grep("factor", sapply(obj, class))
    if (length(factcols) > 0) {
        for (i in factcols) obj[,i] <- as.character(obj[,i])
    }

    if (is.null(col_index)) {
        if (missing(tile_domain)) tile_domain <- c(1L, dims[1])
        if (missing(tile_extent)) tile_extent <- dims[1]

        dom <- tiledb_domain(dims = tiledb_dim(name = "__tiledb_rows",
                                               domain = tile_domain,
                                               tile = tile_extent,
                                               type = "INT32"))
        useobj <- obj

    } else {
        #if (length(col_index) > 1) {
        #    warning("Currently only one index column supported")
        #    col_index <- col_index[1]
        #}

        dimobj <- obj[, col_index, drop=FALSE]
        atrobj <- obj[, -col_index, drop=FALSE]
        useobj <- cbind(dimobj, atrobj)

        makeDim <- function(ind) {
            idxcol <- dimobj[,ind]
            idxnam <- colnames(dimobj)[ind]
            if (debug) cat("Looking at col_index =", ind, ":", idxnam, "\n")
            col_domain <- if (is.null(tile_domain)) c(min(idxcol), max(idxcol)) else tile_domain
            col_extent <- if (is.null(tile_extent)) dims[1] else tile_extent
            dtype <- "INT32"                # default
            if (inherits(idxcol, "POSIXt")) {
                dtype <- "DATETIME_US"
                col_domain <- as.numeric(col_domain) * 1e6 	# int64 used
            } else if (inherits(idxcol, "numeric")) {
                dtype <- "FLOAT64"
                col_extent <- as.numeric(col_extent)
            } else if (inherits(idxcol, "nanotime")) {
                dtype <- "DATETIME_NS"
                col_domain <- c(min(idxcol) - 1e10, max(idxcol) + 1e10)
            } else if (inherits(idxcol, "integer64")) {
                dtype <- "INT64"
                col_extent <- bit64::as.integer64(col_extent)
            } else if (inherits(idxcol, "character")) {
                dtype <- "ASCII"
                col_extent <- NULL
                col_domain <- c(NULL, NULL)
            }

            tiledb_dim(name = idxnam,
                       domain = col_domain,
                       tile = col_extent,
                       type = dtype)
        }
        dimensions <- sapply(seq_len(ncol(dimobj)), makeDim)

        dom <- tiledb_domain(dims = dimensions)
    }

    ## Create filterlist from filter vector, 'NONE' and 'ZSTD' is default
    filterlist <- tiledb_filter_list(sapply(filter, tiledb_filter))

    makeAttr <- function(ind) {
        col <- obj[,ind]
        cl <- class(col)[1]
        if (cl == "integer")
            tp <- "INT32"
        else if (cl == "numeric")
            tp <- "FLOAT64"
        else if (cl == "character")
            tp <- "CHAR"
        else if (cl == "Date")
            tp <- "DATETIME_DAY"
        else if (cl == "POSIXct" || cl == "POSIXlt")
            tp <- "DATETIME_MS"
        else if (cl == "nanotime")
            tp <- "DATETIME_NS"
        else if (cl == "integer64")
            tp <- "INT64"
        else
            stop("Currently unsupported type: ", cl)
        tiledb_attr(colnames(obj)[ind],
                    type = tp,
                    ncells = ifelse(tp=="CHAR",NA_integer_,1),
                    filter_list = filterlist)
    }
    cols <- seq_len(dims[2])
    if (!is.null(col_index)) cols <- cols[-col_index]
    attributes <- sapply(cols, makeAttr)

    schema <- tiledb_array_schema(dom, attrs = attributes,
                                  cell_order = cell_order, tile_order = tile_order,
                                  sparse=sparse, capacity=capacity)
    if (debug) print(schema)
    tiledb_array_create(uri, schema)

    allows_dups(schema) <- allows_dups

    df <- tiledb_array(uri)
    ## when setting an index when likely want 'sparse write to dense array
    if (!is.null(col_index) && !sparse) query_layout(df) <- "UNORDERED"
    if (is.null(col_index) && sparse)
        useobj <- cbind(data.frame(`__tiledb_rows`=seq(1,dims[1]), check.names=FALSE), useobj)
    df[] <- useobj
    invisible(NULL)
}

.testFromDataFrame <- function(obj, uri) {
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)
  fromDataFrame(obj, uri)

  df <- tiledb_array(uri, as.data.frame=TRUE)
  df[]
}

.testWithDate <- function(df, uri) {
  #df <- read.csv("~/git/tiledb-data/csv-pandas/banklist.csv", stringsAsFactors = FALSE)
  bkdf <- within(df, {
    Closing.Date <- as.Date(Closing.Date, "%d-%b-%y")
    Updated.Date <- as.Date(Updated.Date, "%d-%b-%y")
  })

  fromDataFrame(bkdf, uri)
}

.testWithDatetime <- function(df, uri) {
  ## one example data set can be created / read via
  ##   banklist <- read.csv("~/git/tiledb-data/csv-pandas/banklist.csv", stringsAsFactors = FALSE)
  ## pprovided one has those files
  bkdf <- within(df, {
    Closing.Date <- as.POSIXct(as.Date(Closing.Date, "%d-%b-%y"))
    Updated.Date <- as.POSIXct(as.Date(Updated.Date, "%d-%b-%y"))
  })
  if (dir.exists(uri)) {
    message("Removing existing directory ", uri)
    unlink(uri, recursive=TRUE)
  }
  fromDataFrame(bkdf, uri)

  arr <- tiledb_array(uri, as.data.frame = TRUE)
  newdf <- arr[]
  invisible(newdf)
}

.testWithNanotime <- function(df, uri) {
  if (dir.exists(uri)) {
    message("Removing existing directory ", uri)
    unlink(uri, recursive=TRUE)
  }
  fromDataFrame(df, uri)
  cat("Data written\n")

  arr <- tiledb_array(uri, as.data.frame = TRUE)
  newdf <- arr[]
  invisible(newdf)
}
