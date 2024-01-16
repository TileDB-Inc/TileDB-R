#  MIT License
#
#  Copyright (c) 2017-2021 TileDB Inc.
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

#' tiledb - Interface to the TileDB Storage Manager API
#'
#' The efficient multi-dimensional array management system
#' 'TileDB' introduces a novel on-disk format that can effectively store
#" dense and sparse array data with support for fast updates and
#' reads. It features excellent compression, an efficient parallel I/O
#' system which also scales well, and bindings to multiple languages.

#' @keywords internal
"_PACKAGE"

#' @name tiledb-package
#' @useDynLib tiledb, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods setClass setGeneric setMethod
#' @importFrom utils head
NULL
