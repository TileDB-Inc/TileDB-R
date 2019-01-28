#' @exportClass tiledb_dense
setClass("tiledb_dense",
         slots = list(ctx = "tiledb_ctx", uri = "character", ptr = "externalptr"))

#' Constructs a tiledb_dense object backed by a persisted tiledb array uri
#'
#' @param ctx tiledb_ctx
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @return tiledb_dense array object
#' @export
tiledb_dense <- function(ctx, uri, query_type = c("READ", "WRITE")) {
  query_type = match.arg(query_type)
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }

  array_xptr <- libtiledb_array(ctx@ptr, uri, query_type)
  schema_xptr <- libtiledb_array_get_schema(array_xptr)
  if (libtiledb_array_schema_sparse(schema_xptr)) {
    libtiledb_array_close(array_xptr)
    stop("array URI must be a dense array")
  }
  array_xptr <- libtiledb_array_close(array_xptr)
  new("tiledb_dense", ctx = ctx, uri = uri, ptr = array_xptr)
}

setMethod("show", "tiledb_dense",
          function (object) {
            cat("tiledb_dense(uri = \"", object@uri, "\")")
          })

#' #' Reopens a TileDB array an opened tiledb array
#' #'
#' #' Reopening an array is useful when the array got updated after it got opened
#' #' and the tiledb array object got created. To sync-up with the updates,
#' #' the user must either close the array and open again,
#' #' or just use tiledb_reopen(array) which can be faster because
#' #' only metdata regarding updates has to be loaded.
#' #'
#' #' @param object tileb array object
#' #' @return the reopened array object
#' #' @export
#' setGeneric("reopen", function(object, ...) standardGeneric("reopen"))
#'
#' #' @export
#' setMethod("reopen", "tiledb_dense", function(object) {
#'   libtiledb_array_reopen(object@ptr)
#'   return(object)
#' })
#'
#' #' Closes a tiledb array object
#' #'
#' #' @param conn tiledb array object
#' #' @return returns the closed array object
#' close.tiledb_dense <- function(conn, ...)  {
#'   stopifnot(is(conn, "tiledb_dense"))
#'   libtiledb_array_close(conn@ptr)
#'   return(conn);
#' }

#'Returns true is if the array or array_schema is sparse
#'
#' @param object tiledb_dense
#' @return FALSE
#' @export
setMethod("is.sparse", "tiledb_dense", function(object) FALSE)

#' @export
setGeneric("schema", function(object, ...) standardGeneric("schema"))

#' Returns the `tiledb_dense` array `tiledb_schema` object
#'
#' @param object tiledb_dense array object
#' @return tiledb_schema
#' @export
setMethod("schema", "tiledb_dense", function(object, ...) {
  ctx <- object@ctx
  uri <- object@uri
  schema_xptr <- libtiledb_array_schema_load(ctx@ptr, uri)
  return(tiledb_array_schema.from_ptr(schema_xptr))
})

domain_subarray <- function(dom, index = NULL) {
  stopifnot(is(dom, "tiledb_domain"))
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
    stop("incorrect number of dimensions")
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
  if (!all(sapply(dim_subarray, function(sub) length(sub) == 2L))) {
    stop("non-contiguous subscript ranges are not supported")
  }
  return(unlist(dim_subarray))
}

subarray_dim <- function(sub) {
  len <- length(sub)
  if ((len %% 2) != 0){
    stop("invalid subarray length, must be a multiple of 2")
  }
  nd <- as.integer(len / 2)
  sub_dim <- integer(length = nd)
  for (i in 1L:nd) {
    idx <- (i - 1L) * 2L + 1L
    sub_dim[i] <- sub[idx + 1L] - sub[idx] + 1L
  }
  return(sub_dim)
}

attribute_buffers <- function(sch, dom, sub) {
  stopifnot(is(sch, "tiledb_array_schema"))
  stopifnot(is(dom, "tiledb_domain"))
  sub_dim <- subarray_dim(sub)
  ncells <- prod(sub_dim)
  is_scalar <- all(sub_dim == 1L)
  attrs <- list()
  for(attr in tiledb::attrs(sch)) {
    type <- tiledb_datatype_R_type(tiledb::datatype(attr))
    buff <- vector(mode = type, length = ncells)
    if (!is_scalar) {
      attr(buff, "dim") <- sub_dim
    }
    attrs[[tiledb::name(attr)]] <- buff
  }
  return(attrs)
}

setMethod("[", "tiledb_dense",
          function(x, i, j, ..., drop = FALSE) {
            index <- nd_index_from_syscall(sys.call(), parent.frame())
            ctx <- x@ctx
            uri <- x@uri
            schema <- tiledb::schema(x)
            dom <- tiledb::domain(schema)
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral Domain's")
            }
            libtiledb_array_open(x@ptr, "READ")
            out <- tryCatch(
              {
                subarray <- domain_subarray(dom, index = index)
                buffers <- attribute_buffers(schema, dom, subarray)
                qry <- libtiledb_query(ctx@ptr, x@ptr, "READ")
                qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
                if (is.integral(dom)) {
                  qry <- libtiledb_query_set_subarray(qry, as.integer(subarray))
                } else {
                  qry <- libtiledb_query_set_subarray(qry, as.double(subarray))
                }
                attr_names <- names(buffers)
                for (idx in seq_along(buffers)) {
                  qry <- libtiledb_query_set_buffer(qry, attr_names[[idx]], buffers[[idx]])
                }
                qry <- libtiledb_query_submit(qry)
                if (libtiledb_query_status(qry) != "COMPLETE") {
                  stop("error in read query (not 'COMPLETE')")
                }
                # If true, delete the dimensions of an array which have only one level
                if (drop) {
                  for (i in seq_len(length(buffers))) {
                    buffers[[i]] <- drop(buffers[[i]])
                  }
                }
                # if there is only one buffer, don't return a list of attribute buffers
                if (length(buffers) == 1L) {
                  return(buffers[[1L]])
                }
                return(buffers)
              },
              finally = {
                libtiledb_array_close(x@ptr)
              }
            )
            return(out);
          })

setMethod("[<-", "tiledb_dense",
          function(x, i, j, ..., value) {
             if (!is.list(value)) {
              if (is.array(value) || is.vector(value)) {
                value <- list(value)
              } else {
                stop(paste("cannot assign value of type \"", typeof(value), "\""))
              }
            }
            index <- nd_index_from_syscall(sys.call(), parent.frame())
            ctx <- x@ctx
            schema <- tiledb::schema(x)
            uri <- x@uri
            dom <- tiledb::domain(schema)
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral Domain's")
            }
            subarray <- domain_subarray(dom, index = index)
            attrs <- tiledb::attrs(schema)
            if (length(value) > length(attrs)) {
              stop(paste("invalid number of attribute values (", nvalue, " != ", nattrs, ")"))
            }
            attr_names <- names(attrs)
            value_names <- names(value)
            if (is.null(value_names)) {
              # check the list shape / types against attributes
              nvalue <- length(value)
              nattrs <- length(attrs)
              if (length(value) != length(attrs)) {
                stop(paste("invalid number of attribute values (", nvalue, " != ", nattrs, ")"))
              }
              names(value) <- sapply(attr_names, function(n) ifelse(n == "", "__attr", n))
            } else {
              # check associative assignment
              for (name in value_names)  {
                if (!(name %in%  attr_names)) {
                  stop(paste("invalid array attribute value name: \"", name, "\""))
                }
              }
            }
            # check that value shapes match the subarray shape
            # TODO: R doesn't check this and just assigns values that overlap the domain
            sub_dim <- subarray_dim(subarray)

            for (i in seq_along(value)) {
              val <- value[[i]]
              if (is.vector(val)) {
                if (length(sub_dim) != 1 || sub_dim[1L] != length(val)) {
                  stop("value dim does not match array subscript")
                }
              } else if (is.array(val)) {
                if (!all(sub_dim == dim(val))) {
                  stop("value dim does not match array subscript")
                }
              } else {
                stop(paste("cannot assign value of type \"", typeof(value), "\""))
              }
            }
            libtiledb_array_open(x@ptr, "WRITE")
            out <- tryCatch(
              {
                qry <- libtiledb_query(ctx@ptr, x@ptr, "WRITE")
                qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
                if (is.integral(dom)) {
                  qry <- libtiledb_query_set_subarray(qry, as.integer(subarray))
                } else {
                  qry <- libtiledb_query_set_subarray(qry, as.double(subarray))
                }
                attr_names <- names(value)
                for (idx in seq_along(value)) {
                  qry <- libtiledb_query_set_buffer(qry, attr_names[[idx]], value[[idx]])
                }
                qry <- libtiledb_query_submit(qry)
                if (libtiledb_query_status(qry) != "COMPLETE") {
                  stop("error in write query")
                }
                qry <- libtiledb_query_finalize(qry)
                return(x);
              },
              finally = {
                libtiledb_array_close(x@ptr)
              })
            return(out)
          })


as.array.tiledb_dense <- function(x, ...) {
 return(x[])
}

as.data.frame.tiledb_dense <- function(x, row.names = NULL, optional = FALSE, ...,
                                    cut.names = FALSE, col.names = NULL, fix.empty.names = TRUE,
                                    stringsAsFactors = default.stringsAsFactors()) {
  lst <- x[]
  if (!is(lst, "list")) {
    lst <- list(lst)
  }
  if (is.null(col.names)) {
    schema <- tiledb::schema(x)
    col.names <- sapply(tiledb::attrs(schema), tiledb::name)
  }
  return(as.data.frame(lst, row.names = row.names, optional = optional, ...,
                       cut.names = cut.names, col.names = col.names, fix.empty.names = fix.empty.names,
                       stringsAsFactors = default.stringsAsFactors()))
}
