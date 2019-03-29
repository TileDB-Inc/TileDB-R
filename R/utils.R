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
##' Check for unifomity of array dims for a list of arrays
##'
##' Given a list of arrays, make sure they all have the same dimensions,
##' and thus are viable input for creating or updating a multi-attribute
##' TileDB array.
##' @param x a named list of arrays or vectors
##' @return TRUE (or error)
##' @examples
##' assert_uniform_dimensions( list(a = matrix(1:4,ncol=2), b = matrix(1:4,ncol=2)))
##' @export
assert_uniform_dimensions <- function(x) {
    if (!is.list(x) || is.null(names(x))) {
        stop("`x` must be a named list.")
    }
    dims = dim(x[[1]])
    for (el in x) {
        if (!identical(dim(el), dims)) {
            stop("All arrays must be the same size.")
        }
    }
    invisible(TRUE)
}
##' All-in-one function to create a TileDB array
##'
##' Creates a dense or sparse array using sensible defaults.
##' @param array_name single character, tne name of the TileDB array file to be created
##' @param dims integer or numeric, desired dimensions of an attribute, to be repeated for
##' every attribute, e.g. c(2,3) for a 2 row, 3 column matrix
##' @param type character, one value per attribute
##' @param sparse single logical, will the created array be sparse?
##' @param ncells integer, one per attribute. Use 1 for fixed-length arrays and -1
##' for variable length arrays (including character arrays).
##' @return a TileDB array object with the specified properties
##' @examples
##' create_tiledb_array(tempfile(), c(2,3), "FLOAT64")
##' create_tiledb_array(tempfile(), c(2,3), c("FLOAT64", "FLOAT64"))
##' @export
create_tiledb_array <- function(array_name, dims, type, sparse = FALSE, ncells = rep.int(1, length(type))) {
    stopifnot(all(type %in% c("FLOAT64","INT32","UTF8")))
    stopifnot(length(type) == length(ncells))
    dims = as.integer(dims)

    if (tiledb_object_type(array_name) == "ARRAY") {
        stop("Array already exists.")
    }

    ## Make Schema
    attr_names = paste("attr", seq_along(type), sep = "_")
    schema <- tiledb_array_schema(
        tiledb_domain_simple(dims),
        attrs = mapply(
            attr_names, type, ncells,
            FUN = function(n, t, c) {
                tiledb_attr(n, t, ncells = c)
            }, SIMPLIFY = FALSE),
        sparse = sparse
    )

    ## Create the (empty) array on disk.
    tiledb_array_create(array_name, schema)

    if (sparse)
        A <- tiledb_sparse(array_name)
    else
        A <- tiledb_dense(array_name)
    A
}
##' Create a TileDB array
##'
##' Creates and fills a single-attribute, dense TileDB array using sensible defaults.
##' @param x an array to fill a new TileDB array
##' @param array_name single character, name of new TileDB array file
##' @return TileDB array object
##' @examples
##' x = matrix( c(1.3, 1, 2, 4), ncol = 2)
##' tiledb_array( x, tempfile() )
##' @export
tiledb_array <- function(x, array_name) {
    if (is.list(x) || is.character(x))
        ncells = -1
    else
        ncells = 1

    type = r_to_tiledb_type(x)
    A = create_tiledb_array(array_name, dim(x), type, sparse = FALSE, ncells = ncells)
    A[] <- x # sparse [<- does not accept this, which makes sense
    A
}
