#' The version of the libtiledb library
#'
#' @return An named int vector c(major, minor, patch) 
#' @examples
#' tiledb_version()
#' @export
tiledb_version <- function() {
  libtiledb_version() 
}

#' The version of the R library
#'
#' @return An named int vector c(major, minor, patch) 
#' @examples
#' version()
#' @export
version <- function() {
  c(0,3,0) 
}