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

.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgName) {
  ## create a slot for ctx in the per-package enviroment but do no fill it yet to allow 'lazy load'
  ## this entry is generally accessed with a (non-exported) getter and setter in R/Ctx.R
  .pkgenv[["ctx"]] <- NULL

  ## similarly, use a slot for the vfs object
  .pkgenv[["vfs"]] <- NULL
}

.onAttach <- function(libname, pkgName) {
    if (interactive()) {
        packageStartupMessage("TileDB R ", packageVersion("tiledb"),
                              " with TileDB Embedded ", format(tiledb_version(TRUE)),
                              ". See https://tiledb.com for more information.")
    }
}
