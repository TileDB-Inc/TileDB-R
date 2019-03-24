is.scalar <- function(x, typestr) {
  (typeof(x) == typestr) && is.atomic(x) && length(x) == 1L
}

# Adapted from the DelayedArray package
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
##' Get R type of elements in a list
##'
##' TileDB varlen arrays are implemented as R lists of arrays with a uniform storage mode.
##' @param x a list
##' @return single character, e.g. "numeric"
##' @export
varlen_list_eltype <- function(x) {
    stopifnot(is.list(x))
    eltypes = vapply(x, storage.mode, character(1))
    eltype = eltypes[1]
    if (! all(eltypes == eltype)) {
        stop("Variable length arrays must be of a uniform type.")
    }
    eltype
}
