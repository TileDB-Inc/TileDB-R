#  MIT License
#
#  Copyright (c) 2017-2024 TileDB Inc.
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

#' An S4 class for a TileDB CurrentDomain object
#'
#' @slot ptr An external pointer to the underlying CurrentDomain object
#' @slot datatype An character variable describing the data type of the domain
#' @exportClass tiledb_current_domain
setClass("tiledb_current_domain",
         slots = list(ptr = "externalptr",
                      datatype = "character"))

#' Creates a `tiledb_current_domain` object
#'
#' @param ctx (optional) A TileDB Ctx object
#' @return The `tiledb_current_domain` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' if (tiledb_version(TRUE) >= "2.25.0") {
#'    cd <-tiledb_current_domain()
#' }
#'
#' @export
tiledb_current_domain <- function(ctx = tiledb_get_context()) {
    stopifnot("The first argment must be a TileDB Ctx object" = is(ctx, "tiledb_ctx"),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    ptr <- libtiledb_current_domain_create(ctx@ptr)
    return(new("tiledb_current_domain", ptr = ptr, datatype = NA_character_))
}

#' Get `tiledb_current_domain` data type as string
#'
#' @param cd A TileDB CurrentDomain object
#' @return The datatype (as string) of the `tiledb_current_domain` object
#' @export
tiledb_current_domain_get_type <- function(cd) {
    stopifnot("The first argment must be a TileDB CurrentDomain object" =
                  is(cd, "tiledb_current_domain"),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    libtiledb_current_domain_type(cd@ptr)
}

#' Set a `tiledb_ndrectangle` in a `tiledb_current_domain` object
#'
#' @param cd A TileDB CurrentDomain object
#' @param ndr A TileDB NDRectangle object
#' @return The modifiled TileDB CurrendDomain object
#' @export
tiledb_current_domain_set_ndrectangle <- function(cd, ndr) {
    stopifnot("The first argment must be a TileDB CurrentDomain object" =
                  is(cd, "tiledb_current_domain"),
              "The second argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    cd@ptr <- libtiledb_current_domain_set_ndrectangle(cd@ptr, ndr@ptr)
    cd@datatype <- ndr@datatype
    cd
}

#' Get a `tiledb_ndrectangle` from a `tiledb_current_domain` object
#'
#' @param cd A TileDB CurrentDomain object
#' @return The corresponding TileDB NDRectangle object
#' @export
tiledb_current_domain_get_ndrectangle <- function(cd) {
    stopifnot("The first argment must be a TileDB CurrentDomain object" =
                  is(cd, "tiledb_current_domain"),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    ptr <- libtiledb_current_domain_get_ndrectangle(cd@ptr)
    tpstr <- cd@datatype
    return(new("tiledb_ndrectangle", ptr = ptr, datatype = tpstr))
}

#' Test `tiledb_current_domain` object for being empty
#'
#' @param cd A TileDB CurrentDomain object
#' @return A boolean indicating whether the object is empty or not
#' @export
tiledb_current_domain_is_empty <- function(cd) {
    stopifnot("The first argment must be a TileDB CurrentDomain object" =
                  is(cd, "tiledb_current_domain"),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    libtiledb_current_domain_is_empty(cd@ptr)
}
