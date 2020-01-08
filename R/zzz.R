
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgName) {
  ## create a slot for ctx in the per-package enviroment but do no fill it yet to allow 'lazy load'
  ## this entry is generally accessed with a (non-exported) getter and setter in R/Ctx.R
  .pkgenv[["ctx"]] <- NULL
}
