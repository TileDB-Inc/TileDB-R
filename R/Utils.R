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

is.scalar <- function(x, typestr) {
  (typeof(x) == typestr) && is.atomic(x) && length(x) == 1L
}

## Adapted from the DelayedArray package
##' @importFrom utils tail
nd_index_from_syscall <- function(call, env_frame) {
  index <- lapply(seq_len(length(call) - 2L),
                  function(idx){
                    subscript <- call[[2L + idx]]
                    if (missing(subscript))
                      return(NULL)
                    subscript <- eval(subscript, envir = env_frame, enclos = env_frame)
                    return(subscript)
                  })
  argnames <- tail(names(call), n = -2L)
  if (!is.null(argnames))
    index <- index[!(argnames %in% c("drop", "exact", "value"))]
  if (length(index) == 1L && is.null(index[[1L]]))
    index <- list()
  return(index)
}

isNestedList <- function(l) {
  stopifnot(is.list(l))
  for (i in l) {
    if (is.list(i)) return(TRUE)
  }
  return(FALSE)
}

##' Look up TileDB type corresponding to the type of an R object
##'
##' Look up TileDB type corresponding to the type of an R object
##' @param x an R array or list
##' @return single character, e.g. INT32
##' @export
r_to_tiledb_type <- function(x) {
    storage_mode = storage.mode(x)
    if (storage_mode == "list")
        storage_mode = storage.mode(x[[1]])
    if (storage_mode == "integer" || storage_mode == "logical") {
        type = "INT32"
    } else if (storage_mode == "double"){
        type = "FLOAT64"
    } else if (storage_mode == "character"){
        type = "UTF8"
    } else {
        message("Data type ", storage_mode, " not supported for now.")
    }
    type
}
