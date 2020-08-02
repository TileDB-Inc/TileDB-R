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

##' Create a TileDB Dense Array from a given \code{data.frame} Object
##'
##' The supplied \code{data.frame} object is (currently) limited to integer,
##' numeric, or character columns.
##'
##' The create (Dense) Array will have as many attributes as there are columns in
##' the \code{data.frame}.  Each attribute will be a single column.
##'
##' At present, factor variable are converted to character.
##'
##' @param obj A \code{data.frame} object.
##' @param uri A character variable with an Array URI.
##' @return Null, invisibly.
##' @examples
##' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
##' uri <- tempfile()
##' ## turn factor into character
##' irisdf <- within(iris, Species <- as.character(Species))
##' fromDataFrame(irisdf, uri)
##' arr <- tiledb_dense(uri, as.data.frame=TRUE)
##' newdf <- arr[]
##' all.equal(iris, newdf)
##' @export
fromDataFrame <- function(obj, uri) {
  dims <- dim(obj)

  dom <- tiledb_domain(dims = tiledb_dim(name = "rows",
                                         domain = c(1L, dims[1]),
                                         tile = min(10000L, dims[1]),
                                         type = "INT32"))

  ## turn factor columns in char columns
  factcols <- grep("factor", sapply(obj, class))
  if (length(factcols) > 0) {
    for (i in factcols) obj[,i] <- as.character(obj[,i])
  }

  charcols <- grep("character", sapply(obj, class))

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
    else
      stop("Currently unsupported type: ", cl)
    tiledb_attr(colnames(obj)[ind], type=tp, ncells=ifelse(tp=="CHAR",NA_integer_,1))
  }
  attributes <- sapply(seq_len(dims[2]), makeAttr)

  schema <- tiledb_array_schema(dom, attrs = attributes)
  tiledb_array_create(uri, schema)
  #cat("Schema written and array created.\n")

  df <- tiledb_dense(uri)
  df[] <- obj
  invisible(NULL)
}

.testFromDataFrame <- function(obj, uri) {
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)
  fromDataFrame(obj, uri)

  df <- tiledb_dense(uri, as.data.frame=TRUE)
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

  arr <- tiledb_dense(uri, as.data.frame = TRUE)
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

  arr <- tiledb_dense(uri, as.data.frame = TRUE)
  newdf <- arr[]
  invisible(newdf)
}
