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

#' Enable stats counters
#' @concept stats
#' @export
tiledb_stats_enable <- function() {
  libtiledb_stats_enable()
}

#' Disable stats counters
#' @concept stats
#' @export
tiledb_stats_disable <- function() {
  libtiledb_stats_disable()
}

#' Reset stats counters
#' @concept stats
#' @export
tiledb_stats_reset <- function() {
  libtiledb_stats_reset()
}

#' Dump stats to file
#'
#' @param path to stats file
#' @concept stats
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' pth <- tempfile()
#' tiledb_stats_dump(pth)
#' cat(readLines(pth)[1:10], sep = "\n")
#'
#' @export
tiledb_stats_dump <- function(path) {
  libtiledb_stats_dump(path)
}

#' @rdname tiledb_stats_dump
#' @export
tiledb_stats_print <- function() {
  libtiledb_stats_dump("")
}
