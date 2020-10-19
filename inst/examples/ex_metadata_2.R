
## Array Metadata interface from R
##
## Fundamentally we have two access methods, one 'simple' just stating
## a URI (so repeated and/or remote access is more costly) and one
## 'direct' using an external pointer.  The wrappers here switch
## accordingly

library(tiledb)

setup <- function(tmp, verbose=FALSE) {
  if (verbose) cat("Using ", tmp, "\n")
  #if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  dim <- tiledb_dim("dim", domain = c(1L, 4L))
  dom <- tiledb_domain(c(dim))
  a1  <- tiledb_attr("a1", type = "INT32")
  a2  <- tiledb_attr("a2", type = "INT32")
  sch <- tiledb_array_schema(dom, c(a1, a2), sparse=TRUE)
  tiledb_array_create(tmp, sch)
  arr <- tiledb_array(tmp, as.data.frame=FALSE)

  #arrW <- tiledb:::libtiledb_array_open(arr@ptr, tmp, "WRITE")
  #tiledb:::put_metadata(arrW, "vec", c(1.1, 2.2, 3.3))
  #arrW <- tiledb:::libtiledb_array_open(arr@ptr, tmp, "WRITE")
  #tiledb:::put_metadata(arrW, "txt", "the quick brown fox")
  #tiledb:::libtiledb_array_close(arrW)

  arr
}

.isArray <- function(arr) {
    is(arr, "tiledb_sparse") || is(arr, "tiledb_dense") || is(arr, "tiledb_array")
}
.assertArray <- function(arr) stopifnot(.isArray(arr))


.tiledb_array_open <- function(arr, type=c("READ","WRITE")) {
  type <- match.arg(type)
  arr@ptr <- tiledb:::libtiledb_array_open_with_ptr(arr@ptr, type)
  arr
}

.tiledb_array_close <- function(arr) {
  tiledb:::libtiledb_array_close(arr@ptr)
  arr
}

.tiledb_has_metadata <- function(arr, key) {
  if (is.character(arr)) {
    return(tiledb:::has_metadata_simple(arr, key))
  } else if (!.isArray(arr)) {
    message("Neither (text) URI nor Array.")
    return(NULL)
  }
  ## Now deal with (default) case of an array object
  ## Check for 'is it open' ?
  if (!tiledb:::libtiledb_array_is_open(arr@ptr)) {
    stop("Array is not open, cannot access metadata.", call.=FALSE)
  }

  ## Run query
  return(tiledb_has_metadata(arr, key))
}

.tiledb_num_metadata <- function(arr) {
  if (is.character(arr)) {
    return(tiledb:::num_metadata_simple(arr))
  } else if (!.isArray(arr)) {
    message("Neither (text) URI nor Array.")
    return(NULL)
  }

  ## Now deal with (default) case of an array object
  ## Check for 'is it open' ?
  if (!tiledb:::libtiledb_array_is_open(arr@ptr)) {
    stop("Array is not open, cannot access metadata.", call.=FALSE)
  }

  ## Run query
  return(tiledb_num_metadata(arr))
}

.tiledb_get_metadata <- function(arr, key) {
  if (is.character(arr)) {
    return(tiledb:::get_metadata_simple(arr, key))
  } else if (!.isArray(arr)) {
    message("Neither (text) URI nor Array.")
    return(NULL)
  }

  ## Now deal with (default) case of an array object
  ## Check for 'is it open' ?
  if (!tiledb:::libtiledb_array_is_open(arr@ptr)) {
    stop("Array is not open, cannot access metadata.", call.=FALSE)
  }

  ## Run query
  return(tiledb:::get_metadata(arr@ptr, key))
}

.tiledb_put_metadata <- function(arr, key, val) {
  if (is.character(arr)) {
    return(tiledb:::put_metadata_simple(arr, key, val))
  } else if (!.isArray(arr)) {
    message("Neither (text) URI nor Array.")
    return(NULL)
  }

  ## Now deal with (default) case of an array object
  ## Check for 'is it open' ?
  if (!tiledb:::libtiledb_array_is_open(arr@ptr)) {
    stop("Array is not open, cannot access metadata.", call.=FALSE)
  }

  ## Run query
  return(tiledb:::put_metadata(arr@ptr, key, val))
}


tmp <- "/tmp/fooarray" #tempfile()
if (dir.exists(tmp)) unlink(tmp, recursive=TRUE)
arr <- setup(tmp, TRUE)
# arr <- tiledb_array(tmp, as.data.frame=FALSE)

arr <- tiledb_array(tmp)
arr <- .tiledb_array_open(arr, "READ")
cat("Do we have 'arr::vec': ", ifelse(.tiledb_has_metadata(arr, "vec"), "yes", "no"), "\n")
cat("Do we have 'arr::mat': ", ifelse(.tiledb_has_metadata(arr, "mat"), "yes", "no"), "\n")
cat("Do we have 'arr::txt': ", ifelse(.tiledb_has_metadata(arr, "txt"), "yes", "no"), "\n")
cat("Count for 'arr': ", .tiledb_num_metadata(arr), "\n")
cat("Get for 'arr::vec': ", format( tiledb_get_metadata(arr, "vec"), collapse=","), "\n")
arr <- tiledb_array_close(arr)
arr <- .tiledb_array_open(arr, "WRITE")
cat("Adding to 'arr': ", tiledb_put_metadata(arr, "foo", "bar"), "\n")
arr <- tiledb_array_close(arr)
arr <- .tiledb_array_open(arr, "READ")
cat("Count for 'arr': ", tiledb_num_metadata(arr), "\n")
cat("Done\n")
