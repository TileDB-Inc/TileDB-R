#' The version of the libtiledb library
#'
#' @param compact Logical value indicating wheter a compact
#' \code{package_version} object should be returned
#' @return An named int vector c(major, minor, patch), or if select,
#' a \code{package_version} object
#' @examples
#' tiledb_version()
#' tiledb_version(compact = TRUE)
#' @export
tiledb_version <- function(compact = FALSE) {
  if (compact)
    as.package_version(paste(unname(tiledb_version()), collapse="."))
  else
    libtiledb_version()
}
