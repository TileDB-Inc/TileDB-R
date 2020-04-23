#' Enable stats counters
#' @concept stats
#' @export
tiledb_stats_enable <- function() {
  libtiledb_stats_enable();
}

#' Disable stats counters
#' @concept stats
#' @export
tiledb_stats_disable <- function() {
  libtiledb_stats_disable();
}

#' Dump stats to file
#'
#' @param path to stats file
#' @concept stats
#' @examples
#' pth <- tempfile()
#' tiledb_stats_dump(pth)
#' cat(readLines(pth)[1:10], sep = "\n")
#'
#' @export
tiledb_stats_dump <- function(path) {
  libtiledb_stats_dump(path)
}
