.onLoad <- function(libname, pkgName) {
  ns <- asNamespace(pkgName)
  delayedAssign(
      "ctx", tiledb_ctx(),
      eval.env = ns, assign.env = ns
  )
  #namespaceExport(ns, "ctx") # uncomment to export and access via tiledb::ctx rather than tiledb:::ctx
}
