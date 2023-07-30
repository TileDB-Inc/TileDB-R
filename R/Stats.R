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

#' Enable internal TileDB statistics counters
#'
#' This function starts the collection of internal statistics.
#' @export
tiledb_stats_enable <- function() {
  libtiledb_stats_enable()
}

#' Disable internal TileDB statistics counters
#'
#' This function ends the collection of internal statistics.
#' @export
tiledb_stats_disable <- function() {
  libtiledb_stats_disable()
}

#' Reset internal TileDB statistics counters
#'
#' This function resets the counters for internal statistics.
#' @export
tiledb_stats_reset <- function() {
  libtiledb_stats_reset()
}

#' Dumps internal TileDB statistics to file or stdout
#'
#' @param path Character variable with path to stats file;
#' if the empty string is passed then the result is displayed on stdout.
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' pth <- tempfile()
#' tiledb_stats_dump(pth)
#' cat(readLines(pth)[1:10], sep = "\n")
#'
#' @export
tiledb_stats_dump <- function(path) {
  stopifnot(`Argument 'path' must be character` = is.character(path))
  libtiledb_stats_dump(path)
}

#' Print internal TileDB statistics
#'
#' This function is a convenience wrapper for \code{tiledb_stats_dump}.
#' @export
tiledb_stats_print <- function() {
  libtiledb_stats_dump("")
}

#' Dumps internal TileDB statistics as JSON to a string
#'
#' This function requires TileDB Embedded 2.0.3 or later.
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' txt <- tiledb_stats_raw_dump()
#' cat(txt, "\n")
#' @export
tiledb_stats_raw_dump <- function() {
    libtiledb_stats_raw_dump()
}

#' Print internal TileDB statistics as JSON
#'
#' This function is a convenience wrapper for \code{tiledb_stats_raw_dump}.
#' It required TileDB Embedded 2.0.3 or later.
#' @export
tiledb_stats_raw_print <- function() {
    cat(libtiledb_stats_raw_dump(), "\n")
}

#' Gets internal TileDB statistics as JSON string
#'
#' This function is a (now deprecated) convenience wrapper for \code{tiledb_stats_raw_dump}
#' and returns the result as a JSON string.
#' It required TileDB Embedded 2.0.3 or later.
#' @export
tiledb_stats_raw_get <- function() {
    .Deprecated(msg="Use 'tiledb_stats_raw_dump' instead of 'tiledb_stats_raw_get'.")
    libtiledb_stats_raw_get()
 }
