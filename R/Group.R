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

#' Creates a 'tiledb_group' object
#'
#' @param uri Character variable with the URI of the new group object
#' @param type Character variable with the query type value: one of \sQuote{READ}
#' or \sQuote{WRITE}
#' @param ctx (optional) A TileDB Ctx object; if not supplied the default
#' context object is retrieved
#' @return A 'group' object
#' @export
tiledb_group <- function(uri, type = c("READ", "WRITE"), ctx = tiledb_get_context()) {
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'uri' argument must be character" = is.character(uri),
              "This function needs TileDB 2.8.0 or newer" = tiledb_version(TRUE) >= "2.8.0")
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
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"))
    type <- match.arg(type)

    ctx <- tiledb_get_context()
    grp@ptr <- libtiledb_group_open(ctx@ptr, type)
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
              "The 'cfg' argument must be a tiledb_config object" = is(cfg, "tiledb_config"))
    grp@ptr <- libtiledb_group_set_config(grp@ptr, cfg@ptr)
    grp
}

##' Get a TileDB Config from a TileDB Group
##'
##' @param grp A TileDB Group object as for example returned by \code{tiledb_group()}
##' @return The TileDB Config object of the TileDB Group object
##' @export
tiledb_group_set_config <- function(grp, cfg) {
    stopifnot("The 'grp' argument must be a tiledb_group object" = is(grp, "tiledb_group"))
    ptr <- libtiledb_group_set_config(grp@ptr, cfg@ptr)
    cfg <- new("tiledb_config", ptr = ptr)
    cfg
}

## TODO close


#' Create a TileDB Group
#'
#' @param uri Character variable with the URI of the new group
#' @param ctx (optional) A TileDB Ctx object; if not supplied the default
#' context object is retrieved
#' @return The uri path, invisibly
#' @export
tiledb_group_create <- function(uri, ctx = tiledb_get_context()) {
    stopifnot("The 'ctx' argument must be a Context object" = is(ctx, "tiledb_ctx"),
              "The 'uri' argument must be character" = is.character(uri),
              "This function needs TileDB 2.8.0 or newer" = tiledb_version(TRUE) >= "2.8.0")
    libtiledb_group_create_(ctx@ptr, uri)
    invisible(uri)
}
