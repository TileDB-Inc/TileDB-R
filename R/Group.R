#  MIT License
#
#  Copyright (c) 2022 TileDB Inc.
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

#' An S4 class for a TileDB Group object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_group
setClass("tiledb_group",
         slots = list(ptr = "externalptr"))

.tiledb28 <- function() tiledb_version(TRUE) >= "2.8.0"

#' Creates a 'tiledb_group' object
#'
#' @param uri Character variable with the URI of the new group object
#' @param type Character variable with the query type value: one of \dQuote{READ}
#' or \dQuote{WRITE}
#' @param ctx (optional) A TileDB Ctx object; if not supplied the default
#' context object is retrieved
#' @return A 'group' object
#' @export
tiledb_group <- function(uri, type = c("READ", "WRITE"), ctx = tiledb_get_context()) {
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'uri' argument must be character" = is.character(uri),
              "This function needs TileDB 2.8.*" = .tiledb28())
    type <- match.arg(type)
    ptr <- libtiledb_group(ctx@ptr, uri, type)
    group <- new("tiledb_group", ptr = ptr)
    invisible(group)
}

##' Open a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param type A character value that must be either \sQuote{READ} or \sQuote{WRITE}
##' @return The TileDB Group object but opened for reading or writing
##' @export
tiledb_group_open <- function(grp, type=c("READ","WRITE")) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    type <- match.arg(type)
    grp@ptr <- libtiledb_group_open(grp@ptr, type)
    grp
}

##' Set a TileDB Config for a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param cfg A TileDB Config object
##' @return The TileDB Group object with added Config
##' @export
tiledb_group_set_config <- function(grp, cfg) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'cfg' argument must be a tiledb_config object" = is(cfg, "tiledb_config"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    grp@ptr <- libtiledb_group_set_config(grp@ptr, cfg@ptr)
    grp
}

##' Get a TileDB Config from a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return The TileDB Config object of the TileDB Group object
##' @export
tiledb_group_get_config <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    ptr <- libtiledb_group_get_config(grp@ptr)
    cfg <- new("tiledb_config", ptr = ptr)
    cfg
}

##' Close a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return The TileDB Group object but closed for reading or writing
##' @export
tiledb_group_close <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    grp@ptr <- libtiledb_group_close(grp@ptr)
    grp
}

#' Create a TileDB Group at the given path
#'
#' @param uri Character variable with the URI of the new group
#' @param ctx (optional) A TileDB Ctx object; if not supplied the default
#' context object is retrieved
#' @return The uri path, invisibly
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' \dontrun{
#' pth <- tempdir()
#' tiledb_group_create(pth)
#' tiledb_object_type(pth)
#' }
#' @export
tiledb_group_create <- function(uri, ctx = tiledb_get_context()) {
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'uri' argument must be character" = is.character(uri),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_create(ctx@ptr, uri)
    invisible(uri)
}

##' Test if TileDB Group is open
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return A boolean indicating whether the TileDB Group object is open
##' @export
tiledb_group_is_open <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_is_open(grp@ptr)
}

##' Return a TileDB Group URI
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return A character value with the URI
##' @export
tiledb_group_uri <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_uri(grp@ptr)
}

##' Return a TileDB Group query type
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return A character value with the query type i.e. one of \dQuote{READ} or \dQuote{WRITE}.
##' @export
tiledb_group_query_type <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_query_type(grp@ptr)
}

##' Write Metadata to a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param key A character value with they index under which the data will be written
##' @param val An R object (numeric, int, or char vector) that will be stored
##' @return On success boolean \sQuote{TRUE} is returned
##' @export
tiledb_group_put_metadata <- function(grp, key, val) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'key' argument must be character" = is.character(key),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_put_metadata(grp@ptr, key, val)
}

##' Deletes Metadata from a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param key A character value with they index under which the data will be written
##' @return The TileDB Group object, invisibly
##' @export
tiledb_group_delete_metadata <- function(grp, key) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'key' argument must be character" = is.character(key),
              "This function needs TileDB 2.8.*" = .tiledb28())
    grp@ptr <- libtiledb_group_delete_metadata(grp@ptr, key)
    invisible(grp)
}

##' Accesses Metadata from a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param key A character value with the key of the metadata object to be retrieved
##' @return The requested object, or NULL is not found
##' @export
tiledb_group_get_metadata <- function(grp, key) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'key' argument must be character" = is.character(key),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_get_metadata(grp@ptr, key)
}

##' Checks for Metadata in a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param key A character value with they index under which the data will be written
##' @return A boolean value indicating with the object is present
##' @export
tiledb_group_has_metadata <- function(grp, key) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'key' argument must be character" = is.character(key),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_has_metadata(grp@ptr, key)
}

##' Returns Number of Metadata Objects a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return A numeric value with the number of metadata objects
##' @export
tiledb_group_metadata_num <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_metadata_num(grp@ptr)
}


##' Accesses Metadata by Index from a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param idx A numeric value with the index of the metadata object to be retrieved
##' @return The requested object, or NULL is not found
##' @export
tiledb_group_get_metadata_from_index <- function(grp, idx) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'idx' argument must be numeric" = is.numeric(idx),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_get_metadata_from_index(grp@ptr, idx)
}


##' Return all Metadata from a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return A named List with all Metadata objects index
##' @export
tiledb_group_get_all_metadata <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    n <- tiledb_group_metadata_num(grp)
    res <- vector(mode="list", length=n)
    for (i in seq_len(n)) {
        obj <- tiledb_group_get_metadata_from_index(grp, i-1)
        res[[i]] <- obj
        names(res)[i] <- attr(obj, "key")
    }
    res
}


##' Add Member to TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param uri A character value with a new URI
##' @param relative A logical value indicating whether URI is relative to the group
##' @param name An optional character providing a name for the object, defaults to \code{NULL}
##' @return The TileDB Group object, invisibly
##' @export
tiledb_group_add_member <- function(grp, uri, relative, name=NULL) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'uri' argument must be character" = is.character(uri),
              "The 'relative' argument must be logical" = is.logical(relative),
              "The 'name' argument must be NULL or character" = is.null(name) || is.character(name),
              "This function needs TileDB 2.8.*" = .tiledb28())
    grp@ptr <- libtiledb_group_add_member(grp@ptr, uri, relative, name)
    invisible(grp)
}

##' Remove Member from TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param uri A character value with a the URI of the member to be removed, or (if added
##' with a name) the name of the member
##' @param relative A boolean variables describing absolute or relative (to group) uri use
##' @return The TileDB Group object, invisibly
##' @export
tiledb_group_remove_member <- function(grp, uri, relative) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'uri' argument must be character" = is.character(uri),
              "This function needs TileDB 2.8.*" = .tiledb28())
    grp@ptr <- libtiledb_group_remove_member(grp@ptr, uri)
    invisible(grp)
}

##' Get Member Count from TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return The Count of Members in the TileDB Group object
##' @export
tiledb_group_member_count <- function(grp) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_member_count(grp@ptr)
}

##' Get a Member (Description) by Index from TileDB Group
##'
##' This function returns a three-element character vector with the member object translated to
##' character, uri, and optional name.
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param idx A numeric value with the index of the metadata object to be retrieved
##' @return A character vector with three elements: the member type, its uri, and name
##' (or \code{""} if the member is unnamed).
##' @export
tiledb_group_member <- function(grp, idx) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "The 'idx' argument must be numeric" = is.numeric(idx),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_member(grp@ptr, idx)
}

##' Dump the TileDB Group to String
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @param recursive A logical value indicating whether a recursive dump is desired
##' @return A character string
##' @export
tiledb_group_member_dump <- function(grp, recursive) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"),
              "This function needs TileDB 2.8.*" = .tiledb28())
    libtiledb_group_dump(grp@ptr, recursive)
}
