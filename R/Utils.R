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

packageName <- function() "tiledb"

##' Save (or load) data.frame conversion preference in an optional config file
##'
##' The \code{tiledb_array} object can set a preference for \code{data.frame}
##' conversion for each retrieved object. This preference can also be enconded
##' in configuration file as R (version 4.0.0 or later) allows a user- and
##' package specific configuration file.  These helper functions sets and retrieve
##' the value, respectively, or retrieve the cached value from the package environment
##' where is it set at package load.
##'
##' Note that the value must be one of \dQuote{none} (the default), \dQuote{data.frame},
##' \dQuote{data.table} or \dQuote{tibble}.
##'
##' @note This function requires R version 4.0.0 or later to utilise the per-user
##' config directory accessor function. For older R versions, please set the attribute
##' directly when creating the \code{tiledb_array} object, or via the
##' \code{data.frame_conversion()} method.
##' @title Store data.frame conversion preference
##' @param value A character variable with one of the four permitted values
##' @return For the setter, \code{TRUE} is returned invisibly but the function is invoked for the
##' side effect of storing the value. For either getter, the character value.
##' @export
save_dataframe_conversion_preference <- function(value = c("none", "data.frame",
                                                           "data.table", "tibble")) {
    stopifnot(`This function relies on R version 4.0.0 or later.` = R.version.string >= "4.0.0")
    value <- match.arg(value)

    cfgdir <- tools::R_user_dir(packageName())
    if (!dir.exists(cfgdir)) dir.create(cfgdir)
    fname <- file.path(cfgdir, "config.dcf")
    con <- file(fname, "w+")
    cat("data.frame_conversion:", value, "\n", file=con)
    close(con)
    set_dataframe_conversion_preference(value)
    invisible(TRUE)
}

##' @rdname save_dataframe_conversion_preference
##' @export
load_dataframe_conversion_preference <- function() {
    value <- "none"                     # default, and fallback
    cfgfile <- .defaultConfigFile()
    if (cfgfile != "" && file.exists(cfgfile)) {
        cfg <- read.dcf(cfgfile)
        if ("data.frame_conversion" %in% colnames(cfg))
            value <- cfg[[1, "data.frame_conversion"]]
    }
    set_dataframe_conversion_preference(value)
    value
}

##' @rdname save_dataframe_conversion_preference
##' @export
get_dataframe_conversion_preference <- function() .pkgenv[["data.frame_conversion"]]

##' @rdname save_dataframe_conversion_preference
##' @export
set_dataframe_conversion_preference <- function(value = c("none", "data.frame",
                                                          "data.table", "tibble")) {
    value <- match.arg(value)
    .pkgenv[["data.frame_conversion"]] <- value
}

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
