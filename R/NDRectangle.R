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

#' An S4 class for a TileDB NDRectangle object
#'
#' @slot ptr An external pointer to the underlying NDRectangle object
#' @slot datatype A character variable with the TileDB type of the corresponding domain
#' @exportClass tiledb_ndrectangle
setClass("tiledb_ndrectangle",
         slots = list(ptr = "externalptr",
                      datatype = "character"))

#' Creates a `tiledb_ndrectangle` object
#'
#' @param domain A TileDB Domain object for which the NDRectangle object is created
#' @param ctx (optional) A TileDB Ctx object
#' @return The `tiledb_ndrectangle` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <-tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#' ndr <- tiledb_ndrectangle(dom)
#'
#' @export
tiledb_ndrectangle <- function(dom, ctx = tiledb_get_context()) {
    stopifnot("The first argument must be a TileDB Domain object" = is(dom, "tiledb_domain"),
              "The second argment must be a TileDB Ctx object" = is(ctx, "tiledb_ctx"),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    typestr <- datatype(dom)
    ptr <- libtiledb_ndrectangle_create(ctx@ptr, dom@ptr)
    return(new("tiledb_ndrectangle", ptr = ptr, datatype = typestr))
}

#' Set a range on a `tiledb_ndrectangle` object
#'
#' @param ndr A TileDB NDRectangle object
#' @param dimname A character variable with the dimension for which to set a range
#' @param start The lower end of the range to be set
#' @param end The upper end of the range to be set
#' @return The modified `tiledb_ndrectangle` object
#'
#' Start and end values have to be of the same data type as the type of the selected
#' dimension. The set of allowed type includes the different integer types as well as
#' string dimensions.
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <-tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#' ndr <- tiledb_ndrectangle(dom)
#' ndr <- tiledb_ndrectangle_set_range(ndr, "d1", 50, 500)
#'
#' @export
tiledb_ndrectangle_set_range <- function(ndr, dimname, start, end) {
    stopifnot("The first argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "The second argument must a single character object" = is.character(dimname) &&
                  length(dimname) == 1,
              "The third argument must be scalar" = length(start) == 1,
              "The fourth argument must be scalar" = length(end) == 1,
              "The fourth and first argument must be of the same class" = class(start) == class(end),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    ndr@ptr <- libtiledb_ndrectangle_set_range(ndr@ptr, ndr@datatype, dimname, start, end)
    invisible(ndr)
}

#' Get a range from a `tiledb_ndrectangle` object
#'
#' @param ndr A TileDB NDRectangle object
#' @param dimname A character variable with the dimension for which to get a range
#' @return The `tiledb_ndrectangle` range as a two-element vector
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#' ndr <- tiledb_ndrectangle(dom)
#' ndr <- tiledb_ndrectangle_set_range(ndr, "d1", 50, 500)
#' tiledb_ndrectangle_get_range(ndr, "d1")
#'
#' @export
tiledb_ndrectangle_get_range <- function(ndr, dimname) {
    stopifnot("The first argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "The second argument must a single character object" = is.character(dimname) &&
                  length(dimname) == 1,
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    rng <- libtiledb_ndrectangle_get_range(ndr@ptr, dimname, ndr@datatype)
    rng
}
