.onLoad <- function(libname, pkgName) {
  ns <- asNamespace(pkgName)
  delayedAssign(
      "ctx", tiledb_ctx(),
      eval.env = ns, assign.env = ns
  )
}
