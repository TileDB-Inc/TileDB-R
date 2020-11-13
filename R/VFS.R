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

## version prior to 0.8.3 still had vfs as the first argument
## just like the signature of this internal function -- if called
## this way we just revert argument with a warbubf
.uri_arg_vfs_arg_reversed <- function(vfs, uri) {
    is(vfs, "tiledb_vfs") && is.character(uri)
}

#' Create a VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value
#' @export
tiledb_vfs_create_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_create_bucket(vfs@ptr, uri)
}

#' Remove a VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value
#' @export
tiledb_vfs_remove_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
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
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
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
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_is_empty_bucket(vfs@ptr, uri)
}

#' Empty a VFS Bucket
#'
#' @param uri Character variable with a URI describing a cloud bucket
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The URI value that was emptied
#' @export
tiledb_vfs_empty_bucket <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_empty_bucket(vfs@ptr, uri)
}

#' Create a VFS Directory
#'
#' @param uri Character variable with a URI describing a diretory path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value of the created directory
#' @export
tiledb_vfs_create_dir <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_create_dir(vfs@ptr, uri)
}

#' Test for VFS Directory
#'
#' @param uri Character variable with a URI describing a diretory path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return A boolean value indicating if it is a directory
#' @export
tiledb_vfs_is_dir <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_is_dir(vfs@ptr, uri)
}

#' Remove a VFS Directory
#'
#' @param uri Character variable with a URI describing a diretory path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value of the removed directory
#' @export
tiledb_vfs_remove_dir <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_remove_dir(vfs@ptr, uri)
}

#' Test for VFS File
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return A boolean value indicating if it is a file
#' @export
tiledb_vfs_is_file <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_is_file(vfs@ptr, uri)
}

#' Remove a VFS File
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value of the removed file
#' @export
tiledb_vfs_remove_file <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
  libtiledb_vfs_remove_file(vfs@ptr, uri)
}

#' Return VFS File Size
#'
#' @param uri Character variable with a URI describing a file path
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The size removed file
#' @export
tiledb_vfs_file_size <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
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
  if (.uri_arg_vfs_arg_reversed(olduri, vfs)) {
     warning("Deprecated signature detected, adjusting vfs and (new,old)uri arguments")
     ## was:  v o n
     ## mow:  o n v
     tmp <- vfs
     vfs <- olduri
     olduri <- newuri
     newuri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            olduri_argument=is.character(olduri),
            newuri_argument=is.character(newuri))
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
  if (.uri_arg_vfs_arg_reversed(olduri, vfs)) {
     warning("Deprecated signature detected, adjusting vfs and (new,old)uri arguments")
     tmp <- vfs
     vfs <- olduri
     olduri <- newuri
     newuri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            olduri_argument=is.character(olduri),
            newuri_argument=is.character(newuri))
  libtiledb_vfs_move_dir(vfs@ptr, olduri, newuri)
}

#' Touch a VFS URI Resource
#'
#' @param uri Character variable with a URI describing a bucket, file or directory
#' @param vfs A TileDB VFS object; default is to use a cached value.
#' @return The uri value
#' @export
tiledb_vfs_touch <- function(uri, vfs = tiledb_get_vfs()) {
  if (.uri_arg_vfs_arg_reversed(uri, vfs)) {
     warning("Deprecated signature detected, reversing vfs and uri arguments")
     tmp <- vfs
     vfs <- uri
     uri <- tmp
  }
  stopifnot(vfs_argument=is(vfs, "tiledb_vfs"),
            uri_argument=is.character(uri))
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
  ## set the ctx entry from the package environment (a lightweight hash)
  .pkgenv[["vfs"]] <- vfs
  invisible(NULL)
}
