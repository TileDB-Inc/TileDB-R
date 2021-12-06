#' @export
setGeneric("schema", function(object, ...) standardGeneric("schema"))

domain_subarray <- function(dom, index = NULL) {
  stopifnot(`The 'dom' argument must be a tiledb_domain object` = is(dom, "tiledb_domain"))
  nd <- tiledb_ndim(dom)
  dims <- tiledb::dimensions(dom)
  # return the whole domain
  if (is.null(index) || length(index) == 0L) {
    subarray <- integer(length = 2 * nd)
    for (i in seq_len(nd)) {
      idx <- (i - 1L) * 2L + 1L
      dim_domain <- tiledb::domain(dims[[i]])
      subarray[idx] <- dim_domain[1L]
      subarray[idx + 1L] <- dim_domain[2L]
    }
    return(subarray)
  }
  if (length(index) != nd) {
    stop(paste0("incorrect number of dimensions (given) ", length(index), " != ", nd, " (expected)"))
  }
  dim_subarray <- list()
  for (i in seq_len(nd)) {
    dim_domain <- tiledb::domain(dims[[i]])
    if (is.null(index[[i]])) {
      # replace NULL (missing) indices with explict ranges based on the domain
      dim_subarray[[i]] <- dim_domain
    } else {
      # compute subarray slices along each dimension
      dim_subarray[[i]] <- dim_domain_subarray(dim_domain, index[[i]])
    }
  }

  if (!all(lengths(dim_subarray) == 2L)) {
    stop("non-contiguous subscript ranges are not supported")
  }
  return(unlist(dim_subarray))
}

#' @rdname return.data.frame-tiledb_dense-method
#' @param ... Currently unused
# ' @export
setGeneric("return.data.frame", function(object, ...) standardGeneric("return.data.frame"))


#' @rdname return.data.frame-set-tiledb_dense-method
# ' @export
setGeneric("return.data.frame<-", function(x, value) standardGeneric("return.data.frame<-"))

#' @rdname attrs-set-tiledb_dense-method
#' @export
setGeneric("attrs<-", function(x, value) standardGeneric("attrs<-"))
