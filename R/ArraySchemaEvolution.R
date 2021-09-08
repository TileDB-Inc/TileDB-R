#  MIT License
#
#  Copyright (c) 2021 TileDB Inc.
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

#' An S4 class for a TileDB ArraySchemaEvolution object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_array_schema_evolution
setClass("tiledb_array_schema_evolution",
         slots = list(ptr = "externalptr"))

#' Creates a 'tiledb_array_schema_evolution' object
#'
#' @param ctx (optional) A TileDB Ctx object; if not supplied the default
#' context object is retrieved
#' @return A 'array_schema_evolution' object
#' @export
tiledb_array_schema_evolution <- function(ctx = tiledb_get_context()) {
    stopifnot(`The 'ctx' argument must be a Context object` = is(ctx, "tiledb_ctx"),
              `This function needs TileDB 2.4.0 or newer` = tiledb_version(TRUE) >= "2.4.0")
    ptr <- libtiledb_array_schema_evolution(ctx@ptr)
    array_schema_evolution <- new("tiledb_array_schema_evolution", ptr = ptr)
    invisible(array_schema_evolution)
}

#' Add an Attribute to a TileDB Array Schema Evolution object
#'
#' @param object A TileDB 'array_schema_evolution' object
#' @param attr A TileDB attribute
#' @return The modified 'array_schema_evolution' object, invisibly
#' @export
tiledb_array_schema_evolution_add_attribute <- function(object, attr) {
    stopifnot(`The first argument must be a Array Schema Evolution object` =
                  is(object, "tiledb_array_schema_evolution"),
              `The 'attr' argument must be an Attribute object` = is(attr, "tiledb_attr"))
    object@ptr <- libtiledb_array_schema_evolution_add_attribute(object@ptr, attr@ptr)
    invisible(object)
}

#' Drop an attribute given by name from a TileDB Array Schema Evolution object
#'
#' @param object A TileDB 'array_schema_evolution' object
#' @param attrname A character variable with an attribute name
#' @return The modified 'array_schema_evolution' object, invisibly
#' @export
tiledb_array_schema_evolution_drop_attribute <- function(object, attrname) {
    stopifnot(`The first argument must be a Array Schema Evolution object` =
                  is(object, "tiledb_array_schema_evolution"),
              `The 'attrname' argument must be character variable` = is.character(attrname))
    object@ptr <- libtiledb_array_schema_evolution_drop_attribute(object@ptr, attrname)
    invisible(object)
}

#' Evolve an Array Schema
#'
#' @param object A TileDB 'array_schema_evolution' object
#' @param uri A character variable with an URI
#' @return The modified 'array_schema_evolution' object, invisibly
#' @export
tiledb_array_schema_evolution_array_evolve <- function(object, uri) {
    stopifnot(`The first argument must be a Array Schema Evolution object` =
                  is(object, "tiledb_array_schema_evolution"),
              `The 'uri' argument must be character variable` = is.character(uri))
    object@ptr <- libtiledb_array_schema_evolution_array_evolve(object@ptr, uri)
    invisible(object)
}
