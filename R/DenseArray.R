#' @exportClass tiledb_dense
setClass("tiledb_dense",
         slots = list(ctx = "tiledb_ctx", uri = "character", as.data.frame = "logical", ptr = "externalptr"))

#' Constructs a tiledb_dense object backed by a persisted tiledb array uri
#'
#' @param ctx tiledb_ctx
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @return tiledb_dense array object
#' @export
tiledb_dense <- function(uri, query_type = c("READ", "WRITE"), as.data.frame=FALSE, ctx = tiledb:::ctx) {
  query_type = match.arg(query_type)
  if (!is(ctx, "tiledb_ctx")) {
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
  new("tiledb_dense", ctx = ctx, uri = uri, as.data.frame = as.data.frame, ptr = array_xptr)
}

setMethod("show", "tiledb_dense",
          function (object) {
            message("tiledb_dense(uri = \"", object@uri, "\")")
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

attribute_buffers <- function(array, sch, dom, sub, filter_attributes=list()) {
  stopifnot(is(sch, "tiledb_array_schema"))
  stopifnot(is(dom, "tiledb_domain"))
  sub_dim <- subarray_dim(sub)
  ncells <- prod(sub_dim)
  is_scalar <- all(sub_dim == 1L)

  attributes <- list()

  # first alloc coordinate buffer if we are returning a data.frame
  if(array@as.data.frame) {
    ncells_coords <- libtiledb_array_max_buffer_elements(array@ptr, sub, libtiledb_coords())
    if (is.integral(dom)) {
      attributes[["coords"]] <- integer(length = ncells_coords)
    } else {
      attributes[["coords"]]  <- numeric(length = ncells_coords)
    }
  }

  attrs <- tiledb::attrs(sch)
  if (length(filter_attributes) > 0) {
    attrs <- Filter(function(a) is.element(name(a), filter_attributes), attrs)
  }
  for(attr in attrs) {
    aname <- tiledb::name(attr)
    type <- tiledb_datatype_R_type(tiledb::datatype(attr))
    # If we are going to get it as a dataframe we need to use max buffer elements to get proper buffer size
    if(array@as.data.frame) {
      ncells <- libtiledb_array_max_buffer_elements(array@ptr, sub, aname)
    }
    buff <- vector(mode = type, length = ncells)
    # If its not scalar and we are not getting it as a data.frame set the dimension attribute
    if (!is_scalar && !array@as.data.frame) {
      attr(buff, "dim") <- sub_dim
    }
    attributes[[aname]] <- buff
  }
  return(attributes)
}

setMethod("[", "tiledb_dense",
          function(x, i, j, ..., drop = FALSE) {
            index <- nd_index_from_syscall(sys.call(), parent.frame())
            # If we have a list of lists of lists we need to remove one layer
            # This happens when a user uses a list of coordinates
            if (isNestedList(index[1])) {
              index <- index[[1]]
            }
            ctx <- x@ctx
            schema <- tiledb::schema(x)
            dom <- tiledb::domain(schema)
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral Domain's")
            }
            libtiledb_array_open(x@ptr, "READ")

            out <- tryCatch(
              {
                subarray <- domain_subarray(dom, index = index)
                buffers <- attribute_buffers(x, schema, dom, subarray)
                qry <- libtiledb_query(ctx@ptr, x@ptr, "READ")
                qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
                if (is.integral(dom)) {
                  qry <- libtiledb_query_set_subarray(qry, as.integer(subarray))
                } else {
                  qry <- libtiledb_query_set_subarray(qry, as.double(subarray))
                }
                attr_names <- names(buffers)
                for (idx in seq_along(buffers)) {
                  aname <- attr_names[[idx]]
                  val = buffers[[idx]]
                  if (aname == "coords") {
                      qry <- libtiledb_query_set_buffer(qry, libtiledb_coords(), val)
                  } else {
                      if (is.character(val))
                          qry <- libtiledb_query_set_buffer_var(qry, aname, enc2utf8(val))
                      else if (is.list(val))
                          qry <- libtiledb_query_set_buffer_var(qry, aname, val)
                      else
                          qry <- libtiledb_query_set_buffer(qry, aname, val)
                  }
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

                # get the actual number of results, instead of realloc
                # just modify the vector length so there is no additional copy
                for (idx in seq_along(attr_names)) {
                  old_buffer <- buffers[[idx]]
                  aname <- attr_names[[idx]]
                  if (aname == "coords") {
                    ncells <- libtiledb_query_result_buffer_elements(qry, libtiledb_coords())
                  } else {
                    ncells <- libtiledb_query_result_buffer_elements(qry, aname)
                  }
                  if (ncells < length(old_buffer)) {
                    buffers[[idx]] <- old_buffer[1:ncells]
                  }
                }

                if (x@as.data.frame) {
                  return(as_data_frame(dom, buffers))
                } else {
                  # if there is only one buffer, don't return a list of attribute buffers
                  if (length(buffers) == 1L) {
                    return(buffers[[1L]])
                  }
                  return(buffers)
                }
              },
              finally = {
                libtiledb_array_close(x@ptr)
              }
            )
            return(out);
          })

setMethod("[<-", "tiledb_dense",
          function(x, i, j, ..., value) {
            index <- nd_index_from_syscall(sys.call(), parent.frame())
            # If we have a list of lists of lists we need to remove one layer
            # This happens when a user uses a list of coordinates
            if (isNestedList(index[1])) {
              index <- index[[1]]
            }
            ctx <- x@ctx
            schema <- tiledb::schema(x)
            dom <- tiledb::domain(schema)
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral domains")
            }
            subarray <- domain_subarray(dom, index = index)
            ## Check the incoming value
            ## single attribute, fixed-length tiledb arrays expect a single vector/array as input
            ## single attribute, variable-length tiledb arrays expect a list with the same ndim as the the tiledb array
            attrs <- tiledb::attrs(schema)
            nattrs <- length(attrs)

            sub_dim <- subarray_dim(subarray)
            attr_names <- names(attrs)

            if (nattrs == 1) {
                value = list(value)
                names(value) = attr_names
            } else {
                # check associative assignment
                value_names <- names(value)
                if (!is.list(value)) {
                    stop("Replacement values for multi-attribute arrays must be a list.")
                }
                if (length(value) != nattrs) {
                    stop(paste("invalid number of attribute values (", nvalue, " != ", nattrs, ")"))
                }
                if (!identical(value_names, attr_names)) {
                    if (setequal(value_names, attr_names)) {
                        value = value[attr_names]
                    } else {
                        bad_names = setdiff(value_names, attr_names)
                        stop(paste("invalid array attribute value name(s):", paste(bad_names, collapse = ",")))
                    }
                }
                assert_uniform_dimensions(value)
            }
            check_replacement_value(value[[1]], sub_dim)
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
                  aname <- attr_names[[idx]]
                  val = value[[idx]]
                  if (is.list(val) || is.character(val))
                      qry <- libtiledb_query_set_buffer_var(qry, aname, val)
                  else
                      qry <- libtiledb_query_set_buffer(qry, aname, val)
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

check_replacement_value <- function(val, sub_dim) {
    # check that value shapes match the subarray shape
    # R doesn't check this and just assigns values that overlap the domain
    # N.B. a list without dimensions is a vector, with dimensions it is an array
    if (is.vector(val)) {
        if (length(sub_dim) != 1 || sub_dim[1L] != length(val)) {
            stop("value dim does not match array subscript")
        }
    } else if (is.array(val)) {
        if (!all(sub_dim == dim(val))) {
            stop("value dim does not match array subscript")
        }
    } else {
        stop(paste("cannot assign value of type \"", typeof(val), "\""))
    }
    invisible(TRUE)
}

#' @export
as.array.tiledb_dense <- function(x, ...) {
 return(x[])
}

#' @export
as.data.frame.tiledb_dense <- function(x, row.names = NULL, optional = FALSE, ...,
                                    cut.names = FALSE, col.names = NULL, fix.empty.names = TRUE,
                                    stringsAsFactors = default.stringsAsFactors()) {
  lst <- x[]
  if (!is(lst, "list")) {
    lst <- list(lst)
  }
  if (is.null(col.names)) {
    schema <- tiledb::schema(x)
    col.names <- vapply(tiledb::attrs(schema), tiledb::name, character(1))
  }
  return(as.data.frame(lst, row.names = row.names, optional = optional, ...,
                       cut.names = cut.names, col.names = col.names, fix.empty.names = fix.empty.names,
                       stringsAsFactors = default.stringsAsFactors()))
}
