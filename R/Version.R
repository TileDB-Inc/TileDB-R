#  MIT License
#
#  Copyright (c) 2017-2021 TileDB Inc.
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

#' The version of the libtiledb library
#'
#' @param compact Logical value indicating wheter a compact
#' \code{package_version} object should be returned
#' @return An named int vector c(major, minor, patch), or if select,
#' a \code{package_version} object
#' @examples
#' \dontshow{
#' ctx <- tiledb_ctx(limitTileDBCores())
#' }
#' tiledb_version()
#' tiledb_version(compact = TRUE)
#' @export
tiledb_version <- function(compact = FALSE) {
  stopifnot(`Argument 'compact' must be logical` = is.logical(compact))
  if (compact) {
    as.package_version(paste(unname(tiledb_version()), collapse = "."))
  } else {
    libtiledb_version()
  }
}

#' \code{libtiledb} Information
#'
#' Get version and install information of the core \code{libtiledb} install
#'
#' @section Checking the \code{libtiledb} information in downstream packages:
#' These functions are designed to make it easy to test if the core
#' \code{libtiledb} install has changed. This is accomplished by adding a
#' build-time constant to cache the version of \code{libtiledb} was built with.
#' For example, in \code{zzz.R}, put the following line to cache the
#' \code{libtiledb} information during package build
#' \preformatted{
#' .built_with <- list(libtiledb = tiledb::.core_hash())
#' }
#' Then, in the \link[base:ns-hooks]{load hook}, add the following check
#' \preformatted{
#' .onLoad <- function(libname, pkgname) {
#'   if (.built_with$libtiledb != tiledb::.core_hash()) {
#'     warning("Core libtiledb has changed, please reinstall ", pkgname)
#'   }
#' }
#' }
#' This will throw a warning if \pkg{tiledb}, and therefore \code{libtiledb},
#' has changed between downstream package install and load
#'
#' @return \code{.core_info()}: A named character vector with the following entries:
#' \itemize{
#'  \item \dQuote{\code{version}}: \code{libtiledb} version
#'  \item \dQuote{\code{libtype}}: type of \code{libtiledb} install; will be one
#'  of \dQuote{\code{vendored}}, \dQuote{\code{system}}, or \dQuote{\code{unknown}}
#' }
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' .core_info()
#'
.core_info <- function() {
  info <- c(
    version = as.character(tiledb_version(TRUE)),
    libtype = character(1L)
  )
  lib <- system.file(
    "tiledb",
    package = .pkgenv$pkgname,
    lib.loc = .pkgenv$libname
  )
  if (nzchar(lib)) {
    info['libtype'] <- 'vendored'
    return(info)
  }
  if (nzchar(pkgconfig <- Sys.which("pkg-config"))) {
    if (!system2(pkgconfig, args = c("--exists", "tiledb"))) {
      info['libtype'] <- 'system'
      return(info)
    }
  }
  info['libtype'] <- 'unknown'
  return(info)
}

#' @return \code{.core_hash()}: The \link[tools:md5sum]{MD5 hash} of the core info
#'
#' @rdname dot-core_info
#'
#' @export
#'
#' @examples
#' .core_hash()
#'
.core_hash <- function() {
  tmp <- tempfile()
  on.exit(file.remove(tmp), add = TRUE, after = FALSE)
  info <- .core_info()
  writeLines(paste(names(info), info, sep = ':\t', collapse = '\n'), con = tmp)
  return(unname(tools::md5sum(tmp)))
}

#' Compiler Arguments for Using \code{libtiledb}
#'
#' Get compiler flags for using the core \code{libtiledb} install
#' used by \pkg{tiledb}
#'
#' @param opt A single character value with the package configuration variable
#' to fetch; choose from
#' \itemize{
#'  \item \dQuote{\code{PKG_CXX_FLAGS}}: compiler flags for \code{libtiledb}
#'  \item \dQuote{\code{PKG_CXX_LIBS}}: linking flags for \code{libtiledb}
#' }
#'
#' @return A single string containing either the include directories or linking
#' directories for \code{libtiledb}
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' .pkg_config()
#' .pkg_config("PKG_CXX_LIBS")
#'
.pkg_config <- function(opt = c("PKG_CXX_FLAGS", "PKG_CXX_LIBS")) {
  opt <- match.arg(opt)
  lib <- system.file(
    "tiledb",
    package = .pkgenv$pkgname,
    lib.loc = .pkgenv$libname
  )
  if (nzchar(lib)) {
    pkgdir <- system.file(package = .pkgenv$pkgname, lib.loc = .pkgenv$libname)
    return(switch(
      EXPR = opt,
      PKG_CXX_FLAGS = switch(
        EXPR = .Platform$OS.type,
        # Adapted from Makevars.win, which includes libdir/include/tiledb in
        # addition to libdir/include and pkgdir/include
        windows = sprintf(
          "-I%s/include -I%s/include -I%s/include/tiledb",
          shQuote(pkgdir, type = "cmd"),
          shQuote(lib, type = "cmd"),
          shQuote(lib, type = "cmd")
        ),
        sprintf("-I%s/include -I%s/include", pkgdir, lib)
      ),
      PKG_CXX_LIBS = switch(
        EXPR = .Platform$OS.type,
        # rwinlib-tiledb is structured slightly differently than libtiledb for
        # Unix-alikes; R 4.2 and higher require ucrt
        windows = {
          arch <- .Platform$r_arch
          libs <- as.vector(vapply(
            c(pkgdir, lib),
            FUN = \(x) c(
              sprintf("%s/lib/%s", shQuote(x, type = "cmd"), arch),
              ifelse(
                test = getRversion() > '4.2.0',
                yes = sprintf("%s/lib/%s-ucrt", shQuote(x, type = "cmd"), arch),
                no = ""
              )
            ),
            FUN.VALUE = character(2L),
            USE.NAMES = FALSE
          ))
          paste('-ltiledb', paste0('-L', Filter(dir.exists, libs), collapse = ' '))
        },
        sprintf("-ltiledb -L%s/lib -L%s/lib", pkgdir, lib)
      )
    ))
  }
  if (nzchar(pkgconfig <- Sys.which("pkg-config"))) {
    if (!system2(pkgconfig, args = c("--exists", "tiledb"))) {
      flag <- switch(
        EXPR = opt,
        PKG_CXX_FLAGS = "--cflags",
        PKG_CXX_LIBS = "--libs"
      )
      return(trimws(system2(pkgconfig, args = c(flag, "tiledb"), stdout = TRUE)))
    }
  }
  return(switch(
    EXPR = opt,
    PKG_CXX_FLAGS = "",
    PKG_CXX_LIBS = "-ltiledb"
  ))
}
