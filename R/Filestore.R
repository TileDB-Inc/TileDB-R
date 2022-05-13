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

##' Import a file into a TileDB Filestore
##'
##' @param filestore_uri Character with an TileDB Array Schema URI
##' @param file_uri Character with a file URI
##' @param ctx (optional) A TileDB Ctx object; if not supplied the default
##' context object is retrieved
##' @return A boolean is returned to indicate successful completion
##' @export
tiledb_filestore_uri_import <- function(filestore_uri, file_uri, ctx = tiledb_get_context()) {
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'filestore_uri' argument must be character" = is.character(filestore_uri),
              "The 'file_uri' argument must be character" = is.character(file_uri),
              "The 'file_uri' must providing an existing file" = file.exists(file_uri),
              "This function needs TileDB 2.9.0 or later" = tiledb_version(TRUE) >= "2.9.0")
    libtiledb_filestore_uri_import(ctx@ptr, filestore_uri, file_uri)
}

##' Export a file from a TileDB Filestore
##'
##' @param file_uri Character with a file URI
##' @param filestore_uri Character with an TileDB Array Schema URI
##' @param ctx (optional) A TileDB Ctx object; if not supplied the default
##' context object is retrieved
##' @return A boolean is returned to indicate successful completion
##' @export
tiledb_filestore_uri_export <- function(file_uri, filestore_uri, ctx = tiledb_get_context()) {
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'filestore_uri' argument must be character" = is.character(filestore_uri),
              "The 'file_uri' argument must be character" = is.character(file_uri),
              "This function needs TileDB 2.9.0 or later" = tiledb_version(TRUE) >= "2.9.0")
    libtiledb_filestore_uri_export(ctx@ptr, file_uri, filestore_uri)
}

##' Import size bytes from a string into a TileDB Filestore
##'
##' @param filestore_uri Character with an TileDB Array Schema URI
##' @param buf Character variable with content to be imported
##' @param bytes Number of bytes to be import, defaults to length of \code{buf}
##' @param ctx (optional) A TileDB Ctx object; if not supplied the default
##' context object is retrieved
##' @return A boolean is returned to indicate successful completion
##' @export
tiledb_filestore_buffer_import <- function(filestore_uri, buf, bytes, ctx = tiledb_get_context()) {
    if (missing(bytes)) bytes <- nchar(buf)
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'filestore_uri' argument must be character" = is.character(filestore_uri),
              "The 'buf' argument must be character" = is.character(buf),
              "This function needs TileDB 2.9.0 or later" = tiledb_version(TRUE) >= "2.9.0")
    libtiledb_filestore_buffer_import(ctx@ptr, filestore_uri, buf, bytes)
}

##' Export from a TileDB Filestore to a character variable
##'
##' @param filestore_uri Character with an TileDB Array Schema URI
##' @param offset (optional) Numeric variable with offset from beginnig, default is zero
##' @param bytes (optional) Numeric variable with number of bytes to read, default is zero
##' @param ctx (optional) A TileDB Ctx object; if not supplied the default
##' context object is retrieved
##' @return A character variable containing the filestore content (subject to offset and
##' bytes) is returned
##' @export
tiledb_filestore_buffer_export <- function(filestore_uri, offset, bytes, ctx = tiledb_get_context()) {
    if (missing(offset)) offset <- 0
    if (missing(bytes)) bytes <- tiledb_filestore_size(filestore_uri, ctx=ctx)
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'filestore_uri' argument must be character" = is.character(filestore_uri),
              "The 'offset' argument must be numeric" = is.numeric(offset),
              "The 'bytes' argument must be numeric" = is.numeric(bytes),
              "This function needs TileDB 2.9.0 or later" = tiledb_version(TRUE) >= "2.9.0")
    libtiledb_filestore_buffer_export(ctx@ptr, filestore_uri, offset, bytes)
}

##' Return (uncompressed) TileDB Filestore size
##'
##' @param filestore_uri Character with an TileDB Array Schema URI
##' @param ctx (optional) A TileDB Ctx object; if not supplied the default
##' context object is retrieved
##' @return A numeric with the size is returned
##' @export
tiledb_filestore_size <- function(filestore_uri, ctx = tiledb_get_context()) {
    libtiledb_filestore_size(ctx@ptr, filestore_uri)
}
