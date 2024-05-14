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

.pkgenv <- new.env(parent = emptyenv())

.defaultConfigFile <- function() {
    if (getRversion() >= "4.0.0") {
        ## ~/.local/share/R/ + package
        pkgdir <- tools::R_user_dir(packageName())
        if (dir.exists(pkgdir)) {
            fname <- file.path(pkgdir, "config.dcf")
            if (file.exists(fname)) {
                return(fname)
            }
        }
    }
    return("")
}

.onLoad <- function(libname, pkgname) {
    ## create a slot for ctx in the per-package enviroment but do no fill it yet to allow 'lazy load'
    ## this entry is generally accessed with a (non-exported) getter and setter in R/Ctx.R
    .pkgenv[["ctx"]] <- NULL

    ## similarly, use a slot for the vfs object
    .pkgenv[["vfs"]] <- NULL

    ## cache query status of last finalized query
    .pkgenv[["query_status"]] <- character()

    ## set a preference for data.frame conversion for tiledb_array and [] access
    .pkgenv[["return_as"]] <- load_return_as_preference()

    ## set a preference for allocation size defaults
    .pkgenv[["allocation_size"]] <- load_allocation_size_preference()

    ## call setter for Rcpp plugin support
    .set_compile_link_options()

    lib_path <- system.file("lib", .Platform$r_arch, paste0("libconnection", .Platform$dynlib.ext), package = "tiledb")
    res <- dyn.load(lib_path)
    .Call(`tldb_init_`, res$new_connection$address, res$read_connection$address, PACKAGE="tiledb")
}

.onUnload <- function(libname) {
    lib_path <- system.file("lib", .Platform$r_arch, paste0("libconnection", .Platform$dynlib.ext), package = "tiledb")
    dyn.unload(lib_path)
}

.onAttach <- function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage("TileDB R ", packageVersion("tiledb"),
                              " with TileDB Embedded ", format(tiledb_version(TRUE)),
                              " on ", utils::osVersion,
                              ".\nSee https://tiledb.com for more information about TileDB.")
    }
}

## this uses an interface offered by the Rcpp package which, when seeing 'Rcpp::depends(pkgname)'
## will look for a pkgname::inlineCxxPlugin callback to learn about compile + link options
inlineCxxPlugin <- function(...) {
    txt <- paste("No TileDB system-wide installation found. Consider setting TILEDB_INSTALL_DIR",
                 "if have you an installation.")
    stopifnot(txt = .pkgenv[["tiledb_ldflag"]] != "")
    plugin <- Rcpp::Rcpp.plugin.maker(include.before = "#include <tiledb/tiledb>",
                                      libs = .pkgenv[["tiledb_ldflag"]],
                                      package = "tiledb",
                                      Makevars = NULL,
                                      Makevars.win = NULL)
    settings <- plugin()
    settings$env$PKG_CPPFLAGS <- .pkgenv[["tiledb_cppflag"]]
    settings
}

## find library and header directories from either an env var, or pkg-config
## used only by the Rcpp 'plugin' facilitating quick experimentation with short C++ files
.set_compile_link_options <- function(cppflag, ldflag) {
    if (missing(cppflag) && missing(ldflag)) {
        pkgcfg <- unname(Sys.which("pkg-config"))
        have_tiledb_pkgcfg <- isTRUE(Sys.info()[["sysname"]] != "Windows" &&
                                     pkgcfg != "" &&
                                     system2(pkgcfg, c("tiledb", "--exists")) == 0)
        if ((tiledb <- Sys.getenv("TILEDB_INSTALL_DIR", "")) != "") {
            .pkgenv[["tiledb_cppflag"]] <- sprintf("-I%s/include", tiledb)
            .pkgenv[["tiledb_ldflag"]] <- sprintf("-L%s -ltiledb", tiledb)
        } else if (have_tiledb_pkgcfg) {
            .pkgenv[["tiledb_cppflag"]] <- system2(pkgcfg, c("tiledb", "--cflags"), stdout = TRUE)
            .pkgenv[["tiledb_ldflag"]] <- system2(pkgcfg, c("tiledb", "--libs"), stdout = TRUE)
        } else {
            .pkgenv[["tiledb_cppflag"]] <- ""
            .pkgenv[["tiledb_ldflag"]] <- ""
        }
    } else {
        .pkgenv[["tiledb_cppflag"]] <- cppflag
        .pkgenv[["tiledb_ldflag"]] <- ldflag
    }
}


#' @importFrom utils read.table
.getLinuxFlavor <- function() {
    res <- NA_character_
    osrel <- "/etc/os-release"
    if (isTRUE(file.exists(osrel))) {   # on (at least) Debian, Ubuntu, Fedora
        x <- read.table(osrel, sep="=", row.names=1, col.names=c("","Val"), header = FALSE)
        res <- x["ID", "Val"]
    }
    res
}

.isFedora <- function() isTRUE(.getLinuxFlavor() == "fedora")
