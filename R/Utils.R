#  MIT License
#
#  Copyright (c) 2017-2022 TileDB Inc.
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

##' Save (or load) \sQuote{return_as} conversion preference in an optional config file
##'
##' The \code{tiledb_array} object can set a preference for conversion for each retrieved
##' object. This preference can also be enconded in a configuration file as R (version
##' 4.0.0 or later) allows a user- and package specific configuration files.  These helper
##' functions sets and retrieve the value, respectively, or retrieve the cached value from
##' the package environment where is it set at package load.
##'
##' Note that the value must be one of \sQuote{asis} (the default), \sQuote{array},
##' \sQuote{matrix}\sQuote{data.frame}, \sQuote{data.table} or \sQuote{tibble}. The latter
##' two require the corresponding package to be installed.
##'
##' @note This function requires R version 4.0.0 or later to utilise the per-user
##' config directory accessor function. For older R versions, please set the attribute
##' directly when creating the \code{tiledb_array} object, or via the
##' \code{return_as()} method.
##' @title Store object conversion preference
##' @param value A character variable with one of the six permitted values
##' @return For the setter, \code{TRUE} is returned invisibly but the function is invoked for the
##' side effect of storing the value. For either getter, the character value.
##' @export
save_return_as_preference <- function(value = c("asis", "array", "matrix", "data.frame",
                                                "data.table", "tibble")) {
    stopifnot(`This function relies on R version 4.0.0 or later.` = R.version.string >= "4.0.0")
    value <- match.arg(value)

    cfgdir <- tools::R_user_dir(packageName())
    if (!dir.exists(cfgdir)) dir.create(cfgdir)
    fname <- file.path(cfgdir, "config.dcf")
    con <- file(fname, "a+")
    cat("return_as:", value, "\n", file=con)
    close(con)
    set_return_as_preference(value)
    invisible(TRUE)
}

##' @rdname save_return_as_preference
##' @export
load_return_as_preference <- function() {
    value <- "asis"                     # default, and fallback
    cfgfile <- .defaultConfigFile()
    if (cfgfile != "" && file.exists(cfgfile)) {
        cfg <- read.dcf(cfgfile)
        if ("return_as" %in% colnames(cfg))
            value <- cfg[[1, "return_as"]]
    }
    set_return_as_preference(value)
    value
}

##' @rdname save_return_as_preference
##' @export
get_return_as_preference <- function() .pkgenv[["return_as"]]

##' @rdname save_return_as_preference
##' @export
set_return_as_preference <- function(value = c("asis", "array", "matrix", "data.frame",
                                               "data.table", "tibble")) {
    value <- match.arg(value)
    .pkgenv[["return_as"]] <- value
}


##' Save (or load) allocation size default preference in an optional
##' config file
##'
##' When retrieving data from sparse arrays, allocation sizes cannot
##' be determined \emph{ex ante} as the degree of sparsity is unknown.
##' A configuration value can aide in providing an allocation size
##' value. These functions let the user store such a value for
##' retrieval by the package code.  The preference will be enconded in
##' a configuration file as R (version 4.0.0 or later) allows a user-
##' and package specific configuration files.  These helper functions
##' sets and retrieve the value, respectively, or retrieve the cached
##' value from the package environment where is it set at package
##' load.
##'
##' The value will be stored as a character value and reparsed so
##' \sQuote{1e6} and \sQuote{1000000} are equivalent, and the fixed
##' (but adjustable) number of digits for numerical precision
##' \emph{use for formatting} will impact the writing. This should
##' have no effect on standard allocation sizes.
##'
##' Note that this memory budget (currently, at least) applies only to
##' character columns.  A fallback value of 50mb is used if no user
##' value is set, and is used for all columns (rather than per column).
##'
##' @note This function requires R version 4.0.0 or later to utilise the per-user
##' config directory accessor function. For older R versions, a fallback from the
##' TileDB configuration object is used.
##' @title Store allocation size preference
##' @param value A numeric value with the desired allocation size (in bytes).
##' @return For the setter, \code{TRUE} is returned invisibly but the function is invoked for the
##' side effect of storing the value. For the getters, the value as a numeric.
##' @export
save_allocation_size_preference <- function(value) {
    stopifnot(`This function relies on R version 4.0.0 or later.` = R.version.string >= "4.0.0",
              `The 'value' has to be numeric` = is.numeric(value))

    cfgdir <- tools::R_user_dir(packageName())
    if (!dir.exists(cfgdir)) dir.create(cfgdir)
    fname <- file.path(cfgdir, "config.dcf")
    con <- file(fname, "a+")
    cat("allocation_size:", value, "\n", file=con)
    close(con)
    set_allocation_size_preference(value)
    invisible(TRUE)
}

##' @rdname save_allocation_size_preference
##' @export
load_allocation_size_preference <- function() {
    value <- 50 * 1024 * 1024           # fallback value is 50mb
    cfgfile <- .defaultConfigFile()     # but check config file
    if (cfgfile != "" && file.exists(cfgfile)) {
        cfg <- read.dcf(cfgfile)
        if ("allocation_size" %in% colnames(cfg))
            value <- as.numeric(cfg[[1, "allocation_size"]])
    }
    set_allocation_size_preference(value)
    value
}

##' @rdname save_allocation_size_preference
##' @export
get_allocation_size_preference <- function() .pkgenv[["allocation_size"]]

##' @rdname save_allocation_size_preference
##' @export
set_allocation_size_preference <- function(value) {
    stopifnot(`The 'value' has to be numeric` = is.numeric(value))
    .pkgenv[["allocation_size"]] <- value
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
    stopifnot(`Argument 'l' must be a list` = is.list(l))
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
    storage_mode <- storage.mode(x)
    if (storage_mode == "list")
        storage_mode <- storage.mode(x[[1]])
    if (storage_mode == "integer" || storage_mode == "logical") {
        type <- "INT32"
    } else if (storage_mode == "double"){
        type <- "FLOAT64"
    } else if (storage_mode == "character"){
        type <- "UTF8"
    } else {
        message("Data type ", storage_mode, " not supported for now.")
    }
    type
}

## next two were in file MetaData.R

.isArray <- function(arr) {
    is(arr, "tiledb_sparse") || is(arr, "tiledb_dense") || is(arr, "tiledb_array")
}

.assertArray <- function(arr) {
    stopifnot(is(arr, "tiledb_sparse") || is(arr, "tiledb_dense") || is(arr, "tiledb_array"))
}
