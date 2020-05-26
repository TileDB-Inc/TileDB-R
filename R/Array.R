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

#' Creates a new TileDB array given an input schema.
#'
#' @param uri URI specifying path to create the TileDB array object
#' @param schema tiledb_array_schema object
#'
#' @examples
#' pth <- tempdir()
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
#' tiledb_array_create(pth, sch)
#' tiledb_object_type(pth)
#'
#' @export
tiledb_array_create <- function(uri, schema) {
  if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  } else if (missing(schema) || !is(schema, "tiledb_array_schema")) {
    stop("argument schema must a tiledb_array_schema")
  }
  return(libtiledb_array_create(uri, schema@ptr))
}
