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
#' @exportClass tiledb_ndrectangle
setClass("tiledb_ndrectangle",
         slots = list(ptr = "externalptr"))

#' Creates a `tiledb_ndrectangle` object
#'
#' @param dom A TileDB Domain object for which the NDRectangle object is created
#' @param ctx (optional) A TileDB Ctx object
#' @return The `tiledb_ndrectangle` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' if (tiledb_version(TRUE) >= "2.25.0") {
#'    dom <-tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#'    ndr <- tiledb_ndrectangle(dom)
#' }
#'
#' @export
tiledb_ndrectangle <- function(dom, ctx = tiledb_get_context()) {
    stopifnot("The first argument must be a TileDB Domain object" = is(dom, "tiledb_domain"),
              "The second argment must be a TileDB Ctx object" = is(ctx, "tiledb_ctx"),
              "This function needs TileDB 2.25.0 or later" = tiledb_version(TRUE) >= "2.25.0")
    ptr <- libtiledb_ndrectangle_create(ctx@ptr, dom@ptr)
    return(new("tiledb_ndrectangle", ptr = ptr))
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
#' if (tiledb_version(TRUE) >= "2.26.0") {
#'    dom <-tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#'    ndr <- tiledb_ndrectangle(dom)
#'    ndr <- tiledb_ndrectangle_set_range(ndr, "d1", 50, 500)
#' }
#' @export
tiledb_ndrectangle_set_range <- function(ndr, dimname, start, end) {
    stopifnot("The first argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "The second argument must a single character object" = is.character(dimname) &&
                  length(dimname) == 1,
              "The third argument must be scalar" = length(start) == 1,
              "The fourth argument must be scalar" = length(end) == 1,
              "The fourth and first argument must be of the same class" = class(start) == class(end),
              "This function needs TileDB 2.26.0 or later" = tiledb_version(TRUE) >= "2.26.0")
    dtype <- libtiledb_ndrectangle_datatype(ndr@ptr, dimname)
    ndr@ptr <- libtiledb_ndrectangle_set_range(ndr@ptr, dtype, dimname, start, end)
    invisible(ndr)
}

#' Get a range from a `tiledb_ndrectangle` object
#'
#' @param ndr A TileDB NDRectangle object
#' @param dimname A character variable with the dimension for which to get a range
#' @return The `tiledb_ndrectangle` range as a two-element vector
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' if (tiledb_version(TRUE) >= "2.26.0") {
#'    dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#'    ndr <- tiledb_ndrectangle(dom)
#'    ndr <- tiledb_ndrectangle_set_range(ndr, "d1", 50, 500)
#'    tiledb_ndrectangle_get_range(ndr, "d1")
#' }
#' @export
tiledb_ndrectangle_get_range <- function(ndr, dimname) {
    stopifnot("The first argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "The second argument must a single character object" = is.character(dimname) &&
                  length(dimname) == 1,
              "This function needs TileDB 2.26.0 or later" = tiledb_version(TRUE) >= "2.26.0")
    dtype <- libtiledb_ndrectangle_datatype(ndr@ptr, dimname)
    rng <- libtiledb_ndrectangle_get_range(ndr@ptr, dimname, dtype)
    rng
}

#' Get the number of dimensions for `tiledb_ndrectangle` object
#'
#' @param ndr A TileDB NDRectangle object
#' @return The number of dimentiones for the `tiledb_ndrectangle`
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' if (tiledb_version(TRUE) >= "2.26.0") {
#'    dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#'    ndr <- tiledb_ndrectangle(dom)
#'    tiledb_ndrectangle_dim_num(ndr)
#' }
#' @export
tiledb_ndrectangle_dim_num <- function(ndr) {
    stopifnot("The argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "This function needs TileDB 2.26.0 or later" = tiledb_version(TRUE) >= "2.26.0")
    libtiledb_ndrectangle_dim_num(ndr@ptr)
}

#' Get the datatype of a named `tiledb_ndrectangle` dimension
#'
#' @param ndr A TileDB NDRectangle object
#' @param dimname A character variable with the dimension for which to get a datatype
#' @return The `tiledb_ndrectangle` dimension datatype as a character
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' if (tiledb_version(TRUE) >= "2.26.0") {
#'    dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#'    ndr <- tiledb_ndrectangle(dom)
#'    tiledb_ndrectangle_datatype(ndr, "d1")
#' }
#' @export
tiledb_ndrectangle_datatype <- function(ndr, dimname) {
    stopifnot("The first argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "The second argument must a single character object" = is.character(dimname) &&
                  length(dimname) == 1,
              "This function needs TileDB 2.26.0 or later" = tiledb_version(TRUE) >= "2.26.0")
    libtiledb_ndrectangle_datatype(ndr@ptr, dimname)
}

#' Get the datatype of a `tiledb_ndrectangle` dimension by index
#'
#' @param ndr A TileDB NDRectangle object
#' @param dim Am integer value for the dimension for which to get a datatype
#' @return The `tiledb_ndrectangle` dimension datatype as a character
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' if (tiledb_version(TRUE) >= "2.26.0") {
#'    dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
#'    ndr <- tiledb_ndrectangle(dom)
#'    tiledb_ndrectangle_datatype(ndr, 0)
#' }
#' @export
tiledb_ndrectangle_datatype_by_ind <- function(ndr, dim) {
    stopifnot("The first argument must be a TileDB NDRectangle object" = is(ndr, "tiledb_ndrectangle"),
              "The second argument must a single numeric object" = is.numeric(dim) &&
                  length(dim) == 1,
              "This function needs TileDB 2.26.0 or later" = tiledb_version(TRUE) >= "2.26.0")
    libtiledb_ndrectangle_datatype_by_ind(ndr@ptr, dim)
}
