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

#' An S4 class for a TileDB VFS object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_vfs
setClass("tiledb_vfs",
         slots = list(ptr = "externalptr"))

#' Creates a `tiledb_vfs` object
#'
#' @param config (optional) character vector of config parameter names, values
#' @param ctx (optional) A TileDB Ctx object
#' @return The `tiledb_vfs` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' # default configuration
#' vfs <- tiledb_vfs()
#'
#' @export
tiledb_vfs <- function(config = NULL, ctx = tiledb_get_context()) {

  ## otherwise create a new ctx and cache it
  if (is.null(config)) {
    ptr <- libtiledb_vfs(ctx@ptr)
  } else if (typeof(config) == "character") {
    config <- tiledb_config(config)
    ptr <- libtiledb_vfs(ctx@ptr, config@ptr)
  } else if (is(config, "tiledb_config")) {
    ptr <- libtiledb_vfs(ctx@ptr, config@ptr)
  } else {
    stop("invalid tiledb_vfs config argument type")
  }
  vfs <- new("tiledb_vfs", ptr = ptr)
  tiledb_set_vfs(vfs)
  invisible(vfs)
}

#' Create a VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value
#' @export
tiledb_vfs_create_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_create_bucket(vfs@ptr, uri)
}

#' Remove a VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value
#' @export
tiledb_vfs_remove_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_remove_bucket(vfs@ptr, uri)
}

#' Check for VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return A boolean value indicating if it is a valid bucket
#' @export
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' \dontrun{
#' cfg <- tiledb_config()
#' cfg["vfs.s3.region"] <- "us-west-1"
#' ctx <- tiledb_ctx(cfg)
#' vfs <- tiledb_vfs()
#' tiledb_vfs_is_bucket(vfs, "s3://tiledb-public-us-west-1/test-array-4x4")
#' }
tiledb_vfs_is_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_is_bucket(vfs@ptr, uri)
}

#' Check for empty VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return A boolean value indicating if it is an empty bucket
#' @export
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' \dontrun{
#' cfg <- tiledb_config()
#' cfg["vfs.s3.region"] <- "us-west-1"
#' ctx <- tiledb_ctx(cfg)
#' vfs <- tiledb_vfs()
#' tiledb_vfs_is_empty_bucket(vfs, "s3://tiledb-public-us-west-1/test-array-4x4")
#' }
tiledb_vfs_is_empty_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_is_empty_bucket(vfs@ptr, uri)
}

#' Empty a VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The URI value that was emptied
#' @export
tiledb_vfs_empty_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_empty_bucket(vfs@ptr, uri)
}

#' Create a VFS Directory
#'
#' @param uri Character variable with a URI describing a diretory path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value of the created directory
#' @export
tiledb_vfs_create_dir <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_create_dir(vfs@ptr, uri)
}

#' Test for VFS Directory
#'
#' @param uri Character variable with a URI describing a diretory path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return A boolean value indicating if it is a directory
#' @export
tiledb_vfs_is_dir <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_is_dir(vfs@ptr, uri)
}

#' Remove a VFS Directory
#'
#' @param uri Character variable with a URI describing a diretory path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value of the removed directory
#' @export
tiledb_vfs_remove_dir <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  invisible(libtiledb_vfs_remove_dir(vfs@ptr, uri))
}

#' Test for VFS File
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return A boolean value indicating if it is a file
#' @export
tiledb_vfs_is_file <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_is_file(vfs@ptr, uri)
}

#' Remove a VFS File
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value of the removed file
#' @export
tiledb_vfs_remove_file <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_remove_file(vfs@ptr, uri)
}

#' Return VFS File Size
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The size of the file
#' @export
tiledb_vfs_file_size <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_file_size(vfs@ptr, uri)
}

#' Move (or rename) a VFS File
#'
#' @param olduri Character variable with an existing URI describing a file path
#' @param newuri Character variable with a new desired URI file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The newuri value of the moved file
#' @export
tiledb_vfs_move_file <- function(olduri, newuri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'olduri' must be character` = is.character(olduri),
            `Argument 'newuri' must be character` = is.character(newuri))
  libtiledb_vfs_move_file(vfs@ptr, olduri, newuri)
}

#' Move (or rename) a VFS Directory
#'
#' @param olduri Character variable with an existing URI describing a directory path
#' @param newuri Character variable with a new desired URI directory path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The newuri value of the moved directory
#' @export
tiledb_vfs_move_dir <- function(olduri, newuri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'olduri' must be character` = is.character(olduri),
            `Argument 'newuri' must be character` = is.character(newuri))
  libtiledb_vfs_move_dir(vfs@ptr, olduri, newuri)
}

#' Touch a VFS URI Resource
#'
#' @param uri Character variable with a URI describing a bucket, file or directory
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value
#' @export
tiledb_vfs_touch <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_touch(vfs@ptr, uri)
}

#' Retrieve a TileDB VFS object from the package environment and cache
#'
#' @return A TileDB VFS object
#' @export
tiledb_get_vfs <- function() {
  ## return the vfs entry from the package environment (a lightweight hash)
  vfs <- .pkgenv[["vfs"]]

  ## if null, create a new context (which caches it too) and return it
  if (is.null(vfs)) {
    vfs <- tiledb_vfs()
  }

  vfs
}

#' Store a TileDB VFS object in the package environment
#'
#' @param vfs A TileDB VFS object
#' @return NULL, invisibly. The function is invoked for the side-effect of
#' storing the VFS object.
#' @export
tiledb_set_vfs <- function(vfs) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"))
  ## set the ctx entry from the package environment (a lightweight hash)
  .pkgenv[["vfs"]] <- vfs
  invisible(NULL)
}

#' Open a TileDB VFS Filehandle for reading or writing
#'
#' @param binfile A character variable describing the (binary) file to be opened
#' @param mode A character variable with value \sQuote{READ}, \sQuote{WRITE}
#' or \sQuote{APPEND}
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @param ctx (optional) A TileDB Ctx object
#' @return A TileDB VFS Filehandle object (as an external pointer)
#' @export
tiledb_vfs_open <- function(binfile, mode = c("READ", "WRITE", "APPEND"),
                            vfs = tiledb_get_vfs(), ctx = tiledb_get_context()) {
  mode <- match.arg(mode)
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'ctx' must a tiledb_ctx object` = is(ctx, "tiledb_ctx"),
            `Argument 'binfile' must be character` = is.character(binfile))
  libtiledb_vfs_open(ctx@ptr, vfs@ptr, binfile, mode)
}

#' Close a TileDB VFS Filehandle
#'
#' @param fh A TileDB VFS Filehandle external pointer as returned from \code{tiledb_vfs_open}
#' @param ctx (optional) A TileDB Ctx object
#' @return The result of the close operation is returned.
#' @export
tiledb_vfs_close <- function(fh, ctx = tiledb_get_context()) {
  stopifnot(`Argument 'fh' must be an external pointer` = is(fh, "externalptr"),
            `Argument 'ctx' must a tiledb_ctx object` = is(ctx, "tiledb_ctx"))
  libtiledb_vfs_close(ctx@ptr, fh)
}

#' Sync a TileDB VFS Filehandle
#'
#' @param fh A TileDB VFS Filehandle external pointer as returned from \code{tiledb_vfs_open}
#' @param ctx (optional) A TileDB Ctx object
#' @return The result of the sync operation is returned.
#' @export
tiledb_vfs_sync <- function(fh, ctx = tiledb_get_context()) {
  stopifnot(`Argument 'fh' must be an external pointer` = is(fh, "externalptr"),
            `Argument 'ctx' must a tiledb_ctx object` = is(ctx, "tiledb_ctx"))
  libtiledb_vfs_sync(ctx@ptr, fh)
}

#' Write to a TileDB VFS Filehandle
#'
#' This interface currently defaults to using an integer vector. This is suitable for R objects
#' as the raw vector result from serialization can be mapped easily to an integer vector. It is
#' also possible to \code{memcpy} to the contiguous memory of an integer vector should other
#' (non-R) data be transferred.
#' @param fh A TileDB VFS Filehandle external pointer as returned from \code{tiledb_vfs_open}
#' @param vec An integer vector of content to be written
#' @param ctx (optional) A TileDB Ctx object
#' @return The result of the write operation is returned.
#' @export
tiledb_vfs_write <- function(fh, vec, ctx = tiledb_get_context()) {
  stopifnot(`Argument 'fh' must be an external pointer` = is(fh, "externalptr"),
            `Argument 'vec' must be integer` = is.integer(vec),
            `Argument 'ctx' must a tiledb_ctx object` = is(ctx, "tiledb_ctx"))
  libtiledb_vfs_write(ctx@ptr, fh, vec)
}

#' Read from a TileDB VFS Filehandle
#'
#' This interface currently defaults to reading an integer vector. This is suitable for R objects
#' as a raw vector used for (de)serialization can be mapped easily to an integer vector. It is
#' also possible to \code{memcpy} to the contiguous memory of an integer vector should other
#' (non-R) data be transferred.
#' @param fh A TileDB VFS Filehandle external pointer as returned from \code{tiledb_vfs_open}
#' @param offset A scalar integer64 value with the byte offset from the beginning of the file
#' with a of zero.
#' @param nbytes A scalar integer64 value with the number of bytes to be read.
#' @param ctx (optional) A TileDB Ctx object
#' @return The binary file content is returned as an integer vector.
#' @export
tiledb_vfs_read <- function(fh, offset, nbytes, ctx = tiledb_get_context()) {
  if (missing(offset)) offset <- bit64::as.integer64(0)
  stopifnot(`Argument 'fh' must be an external pointer` = is(fh, "externalptr"),
            `Argument 'offset' must be integer64` = is(offset, "integer64"),
            `Argument 'nbytes' currently a required parameter` = !missing(nbytes),
            `Argument 'nbytes' must be integer64` = is(nbytes, "integer64"),
            `Argument 'ctx' must a tiledb_ctx object` = is(ctx, "tiledb_ctx"))
  libtiledb_vfs_read(ctx@ptr, fh, offset, nbytes)
}

#' Return VFS Directory Size
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The size of the directory
#' @export
tiledb_vfs_dir_size <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_dir_size(vfs@ptr, uri)
}

#' Return VFS Directory Listing
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The content of the directory, non-recursive
#' @export
tiledb_vfs_ls <- function(uri, vfs = tiledb_get_vfs()) {
  stopifnot(`Argument 'vfs' must a tiledb_vfs object` = is(vfs, "tiledb_vfs"),
            `Argument 'uri' must be character` = is.character(uri))
  libtiledb_vfs_ls(vfs@ptr, uri)
}
