#  MIT License
#
#  Copyright (c) 2017-2023 TileDB Inc.
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

##' Test if TileDB Array has Metadata
##'
##' @param arr A TileDB Array object
##' @param key A character value describing a metadata key
##' @return A logical value indicating if the given key exists in the
##'   metdata of the given array
##' @export
tiledb_has_metadata <- function(arr, key) {
  stopifnot(`Argument 'arr' must be a (dense or sparse) TileDB array` = .isArray(arr),
            `Argument 'key' must be a scalar character` = is.scalar(key, "character"),
            `Array must be open for reading to access metadata` = libtiledb_array_is_open_for_reading(arr@ptr))
  res <- libtiledb_array_get_metadata_list(arr@ptr)
  key %in% names(res)
}

##' Return count of TileDB Array Metadata objects
##'
##' @param arr A TileDB Array object, or a character URI describing one
##' @return A integer variable with the number of Metadata objects
##' @export
tiledb_num_metadata <- function(arr) {
  stopifnot(`Argument 'arr' must be a (dense or sparse) TileDB array` = .isArray(arr),
            `Array must be open for reading to access metadata` = libtiledb_array_is_open_for_reading(arr@ptr))
  libtiledb_array_get_metadata_num(arr@ptr)
}

##' Return a TileDB Array Metadata object given by key
##'
##' @param arr A TileDB Array object, or a character URI describing one
##' @param key A character value describing a metadata key
##' @return A object stored in the Metadata under the given key,
##' or \sQuote{NULL} if none found.
##' @export
tiledb_get_metadata <- function(arr, key) {
  stopifnot(`Argument 'arr' must be a (dense or sparse) TileDB array` = .isArray(arr),
            `Array must be open for reading to access metadata` = libtiledb_array_is_open_for_reading(arr@ptr))
  res <- libtiledb_array_get_metadata_list(arr@ptr)
  if (key %in% names(res))
    res[[key]]
  else
    NULL
}

##' Store an object in TileDB Array Metadata under given key
##'
##' @param arr A TileDB Array object, or a character URI describing one
##' @param key A character value describing a metadata key
##' @param val An object to be stored
##' @return A boolean value indicating success
##' @export
tiledb_put_metadata <- function(arr, key, val) {
    stopifnot(`Argument must be a (dense or sparse) TileDB array.` = .isArray(arr),
              `Array is not open for writing.` = libtiledb_array_is_open_for_writing(arr@ptr))
    libtiledb_array_put_metadata(arr@ptr, key, val)
}


##' Return all TileDB Array Metadata objects as a named list
##'
##' @param arr A TileDB Array object, or a character URI describing one
##' @return A named list with all Metadata objects indexed by the given key
##' @export
tiledb_get_all_metadata <- function(arr) {
  stopifnot(`Argument 'arr' must be a (dense or sparse) TileDB array` = .isArray(arr),
            `Array must be open for reading to access metadata` = libtiledb_array_is_open_for_reading(arr@ptr))
  res <- libtiledb_array_get_metadata_list(arr@ptr)
  class(res) <- "tiledb_metadata"
  res
}

##' Print a TileDB Array Metadata object
##'
##' @param x A TileDB array object
##' @param width Optional display width, defaults to NULL
##' @param ... Optional method arguments, currently unused
##' @return The array object, invisibly
##' @export
##' @method print tiledb_metadata
print.tiledb_metadata <- function(x, width=NULL, ...) {
  nm <- names(x)
  for (i in 1:length(nm)) {
    cat(nm[i], ":\t", format(x[i]), "\n", sep="")
  }
  invisible(x)
}

##' Delete a TileDB Array Metadata object given by key
##'
##' @param arr A TileDB Array object
##' @param key A character value describing a metadata key
##' @return A boolean indicating success
##' @export
tiledb_delete_metadata <- function(arr, key) {
  stopifnot(`Argument must be a (dense or sparse) TileDB array.` = .isArray(arr),
            `Array is not open for writing.` = libtiledb_array_is_open_for_writing(arr@ptr))
  libtiledb_array_delete_metadata(arr@ptr, key)
  TRUE                                  # we get NULL from C++
}
