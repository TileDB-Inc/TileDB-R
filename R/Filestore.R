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

## Initial wrapper for the TileDB Filestore functionality currently in tiledb_experimental

##' Create an array schema from a given URI with schema
##'
##' @param uri Character with an TileDB Array Schema URI, if missing or NULL a default schema
##' is returned
##' @param ctx (optional) A TileDB Ctx object; if not supplied the default
##' context object is retrieved
##' @return An ArraySchema object corresponding to the supplied schema, or a default if missing
##' @export
tiledb_filestore_schema_create <- function(uri = NULL, ctx = tiledb_get_context()) {
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'uri' argument must be character" = is.null(uri) || is.character(uri),
              "The 'uri' must providing an existing file" = is.null(uri) || file.exists(uri),
              "This function needs TileDB 2.9.0 or later" = tiledb_version(TRUE) >= "2.9.0")
    arrptr <- libtiledb_filestore_schema_create(ctx@ptr, if (is.null(uri)) "" else uri)
    tiledb_array_schema.from_ptr(arrptr)
}
