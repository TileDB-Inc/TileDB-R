#' tiledb - Interface to the TileDB Storage Manager API
#'
#' The efficient multi-dimensional array management system
#' 'TileDB' introduces a novel on-disk format that can effectively store
#" dense and sparse array data with support for fast updates and
#' reads. It features excellent compression, an efficient parallel I/O
#' system which also scales well, and bindings to multiple languages.
#'
#' @docType package
#' @name tiledb-package
#' @useDynLib tiledb, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods setClass setGeneric setMethod
NULL
