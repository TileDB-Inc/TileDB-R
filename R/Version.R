#' The version of the libtiledb library
#'
#' @return An named int vector c(major, minor, patch)
#' @examples
#' tiledb_version()
#' @export
tiledb_version <- function() {
  libtiledb_version()
}
