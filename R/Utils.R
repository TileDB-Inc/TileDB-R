is.scalar <- function(x, typestr) {
  (typeof(x) == typestr) && is.atomic(x) && length(x) == 1L
}

## Adapted from the DelayedArray package
##' @importFrom utils tail
nd_index_from_syscall <- function(call, env_frame) {
  index <- lapply(seq_len(length(call) - 2L),
                  function(idx){
                    subscript <- call[[2L + idx]]
                    if (missing(subscript))
                      return(NULL)
                    subscript <- eval(subscript, envir = env_frame, enclos = env_frame)
                    return(subscript)
                  })
  argnames <- tail(names(call), n = -2L)
  if (!is.null(argnames))
    index <- index[!(argnames %in% c("drop", "exact", "value"))]
  if (length(index) == 1L && is.null(index[[1L]]))
    index <- list()
  return(index)
}

isNestedList <- function(l) {
  stopifnot(is.list(l))
  for (i in l) {
    if (is.list(i)) return(TRUE)
  }
  return(FALSE)
}
##' Look up TileDB type corresponding to the type of an R object
##'
##' Look up TileDB type corresponding to the type of an R object
##' @param x an R array or list
##' @return single character, e.g. INT32
##' @export
r_to_tiledb_type <- function(x) {
    storage_mode = storage.mode(x)
    if (storage_mode == "list")
        storage_mode = storage.mode(x[[1]])
    if (storage_mode == "integer" || storage_mode == "logical") {
        type = "INT32"
    } else if (storage_mode == "double"){
        type = "FLOAT64"
    } else if (storage_mode == "character"){
        type = "UTF8"
    } else {
        message("Data type ", storage_mode, " not supported for now.")
    }
    type
}

##' Open a TileDB Array
##'
##' @param arr A TileDB Array object as for example returned by `tiledb_dense()`
##' @param type A character value that must be either \sQuote{READ} or \sQuote{WRITE}
##' @return The TileDB Array object but opened for reading or writing
##' @export
tiledb_array_open <- function(arr, type=c("READ","WRITE")) {
  type <- match.arg(type)

  arr@ptr <- libtiledb_array_open(arr@ptr, type)
  arr
}

##' Close a TileDB Array
##'
##' @param arr A TileDB Array object as for example returned by `tiledb_dense()`
##' @return The TileDB Array object but closed
##' @export
tiledb_array_close <- function(arr) {
  libtiledb_array_close(arr@ptr)
  arr
}

##' Print an Array Schema
##'
##' Prints the schema of the given array to standard output.
##' @param uri A TileDB Array URI
##' @return Nothing is returned as the function is invoked for its site effect.
##' @export
tiledb_array_schema_dump <- function(uri, ctx = tiledb_get_context()) {
  schema <- tiledb:::libtiledb_array_schema_load(ctx@ptr, uri)
  tiledb:::libtiledb_array_schema_dump(schema)
  invisible(NULL)
}
