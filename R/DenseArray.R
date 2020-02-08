#' An S4 class for a TileDB dense array
#'
#' @slot ctx A TileDB context object
#' @slot uri A character despription
#' @slot as.data.frame A logical value
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_dense
setClass("tiledb_dense",
         slots = list(ctx = "tiledb_ctx", uri = "character",
                      as.data.frame = "logical", ptr = "externalptr"))

#' Constructs a tiledb_dense object backed by a persisted tiledb array uri
#'
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @param as.data.frame optional logical switch, defaults to "FALSE"
#' @param ctx tiledb_ctx (optional)
#' @return tiledb_dense array object
#' @export
tiledb_dense <- function(uri, query_type = c("READ", "WRITE"),
                         as.data.frame=FALSE, ctx = tiledb_get_context()) {
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
            cat("tiledb_dense(uri = \"", object@uri, "\")\n", sep="")
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

#' Returns true is if the array or array_schema is sparse
#'
#' @param object tiledb_dense
#' @param ... Extra parameter for method signature, currently unused.
#' @return FALSE
#' @export
setMethod("is.sparse", "tiledb_dense", function(object) FALSE)

#' @rdname generics
#' @export
setGeneric("schema", function(object, ...) standardGeneric("schema"))

#' Returns the `tiledb_dense` array `tiledb_schema` object
#'
#' @param object tiledb_dense array object
#' @param ... Extra parameter for method signature, currently unused.
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

  anyvarlen <- any(sapply(attrs(sch), function(s) is.na(ncells(s))))

  attributes <- list()
  offsets <- list()

  # first alloc coordinate buffer if we are returning a data.frame
  if (array@as.data.frame) {
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
  for (attr in attrs) {
    aname <- tiledb::name(attr)
    type <- tiledb_datatype_R_type(tiledb::datatype(attr))

    # If we get it as a dataframe we need to use max buffer elements to get proper buffer size
    # or if we have variable length arrays
    if (array@as.data.frame || anyvarlen) {
      ncells <- libtiledb_array_max_buffer_elements(array@ptr, sub, aname)
    }

    if (type == "character") {
      buff <- paste(rep(".", ncells), collapse="") # does not matter what character we repeat
    } else {
      buff <- vector(mode = type, length = ncells)
    }

    # If its not scalar and we are not getting it as a data.frame set the dimension attribute
    if (!is_scalar && !array@as.data.frame && !anyvarlen) {
      attr(buff, "dim") <- sub_dim
    }
    attributes[[aname]] <- buff

    if (is.na(ncells(attr))) {
      ## NB offsets are always uint64 'which we do not have' so proxy with double
      #cat("Ncells in attribute buffers is ", ncells, "\n")
      offsetbuf <- vector(mode = "double", length = ncells)
      offsets[[aname]] <- offsetbuf
    } else {
      offsets[[aname]] <- NULL
    }

  }
  return(list(attributes=attributes, offsets=offsets))
  #return(attributes)
}

#' Gets a dense array value
#'
#' @param x dense array object
#' @param i parameter key string, currently unused
#' @param j parameter key string, currently unused
#' @param ... Extra parameter for method signature, currently unused.
#' @param drop Optional logical switch to drop dimensions, default FALSE, currently unused.
#' @return An element from a dense array
setMethod("[", "tiledb_dense",
          function(x, i, j, ..., drop = FALSE) {
            index <- nd_index_from_syscall(sys.call(), parent.frame())
            # If we have a list of lists of lists we need to remove one layer
            # This happens when a user uses a list of coordinates
            if (isNestedList(index[1])) {
              index <- index[[1]]
            }
            ctx <- x@ctx
            uri <- x@uri
            schema <- tiledb::schema(x)

            anyvarlen <- any(sapply(attrs(schema), function(s) is.na(ncells(s))))

            dom <- tiledb::domain(schema)
            if (!tiledb::is.integral(dom)) { # test is in Domain.R and checks for INT domain
              stop("subscript indexing only valid for integral Domain's")
            }
            libtiledb_array_open(x@ptr, "READ")

            ## query number of cell values for schema, NA indicates variable length
            ncellval <- sapply(attrs(schema), ncells)
            if (x@as.data.frame)        # in the data.frame case first buffer is 'coords' so fill
              ncellval <- c(coords=-1, ncellval)

            storagemode <- vector(mode="character", length=length(ncellval))

            out <- tryCatch(
              {
                subarray <- domain_subarray(dom, index = index)

                if (storage.mode(subarray) == "double") {
                  subarray <- as.integer(subarray)
                }

                ## generalized to return two lists
                bufferlist <- attribute_buffers(x, schema, dom, subarray)
                buffers <- bufferlist[["attributes"]]
                offsets <- bufferlist[["offsets"]]
                #buffers <- attribute_buffers(x, schema, dom, subarray)

                qry <- libtiledb_query(ctx@ptr, x@ptr, "READ")
                qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
                #if (is.integral(dom)) {  ## -- already tested above
                  qry <- libtiledb_query_set_subarray(qry, as.integer(subarray))
                #} else {
                #  qry <- libtiledb_query_set_subarray(qry, as.double(subarray))
                #}
                attr_names <- names(buffers)
                for (idx in seq_along(buffers)) {
                  aname <- attr_names[[idx]]
                  isvarlen <- is.na(unname(ncellval[idx]))  ## NA == variable length
                  val <- buffers[[idx]]  ## could be/should be aname
                  storagemode[idx] <- storage.mode(val)
                  if (aname == "coords") {
                    qry <- libtiledb_query_set_buffer(qry, libtiledb_coords(), val)
                  } else if (isvarlen) {
                    #noff <- libtiledb_array_max_buffer_elements_offsets(x@ptr, subarray, aname)
                    #cat("noff: ", noff, "  aname: ", aname, "  names(offsets):", names(offsets), "\n")
                    qry <- libtiledb_query_set_buffer_var_test(qry, aname, val, offsets[[aname]])
                  } else {
                    #if (is.character(val) || is.list(val)) {
                    # missing function, never written
                    #  qry <- libtiledb_query_set_buffer_var(qry, aname, val)
                    #} else {
                      qry <- libtiledb_query_set_buffer(qry, aname, val)
                    #}
                  }
                }
                qry <- libtiledb_query_submit(qry)
                if (libtiledb_query_status(qry) != "COMPLETE") {
                  cat("Status:", libtiledb_query_status(qry), "\n")
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
                    if (ncells < length(old_buffer)) {
                      buffers[[idx]] <- old_buffer[1:ncells]
                    }

                  } else if (isvarlen) {  ## NA == variable lnegth
                    noffs <- libtiledb_query_result_buffer_elements_offsets(qry, aname)
                    ncells <- libtiledb_query_result_buffer_elements(qry, aname)
                    buffers[[idx]] <- libtiledb_query_result_list_column(qry, storagemode[idx],
                                                                         aname, old_buffer,
                                                                         offsets[[aname]])
                    if (!x@as.data.frame) {
                      subdims <- subarray_dim(subarray)
                      buffers[[idx]] <- matrix(buffers[[idx]], subdims[1], subdims[2])
                      if (requireNamespace("data.table", quietly=TRUE)) {
                        buffers[[idx]] <- data.table::data.table(buffers[[idx]])
                      }
                    }
                  } else {
                    ncells <- libtiledb_query_result_buffer_elements(qry, aname)
                    if (ncells < length(old_buffer)) {
                      buffers[[idx]] <- old_buffer[1:ncells]
                    }
                  }
                }

                if (x@as.data.frame) {
                  ## need extra test for variable length arrays which do not fit
                  if (anyvarlen) {
                    return(as_data_table(dom, buffers))
                  } else {
                    return(as_data_frame(dom, buffers))
                  }
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


#' Sets a dense array value
#'
#' @param x dense array object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param value The value being assigned
#' @return The modified object
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
            # If we have a list of lists of lists we need to remove one layer
            # This happens when a user uses a list of coordinates
            if (isNestedList(index[1])) {
              index <- index[[1]]
            }
            ctx <- x@ctx
            schema <- tiledb::schema(x)
            uri <- x@uri
            dom <- tiledb::domain(schema)
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral Domain's")
            }
            anyvarlen <- any(sapply(attrs(schema), function(s) is.na(ncells(s))))
            subarray <- domain_subarray(dom, index = index)
            attrs <- tiledb::attrs(schema)
            nvalue <- length(value)
            nattrs <- length(attrs)
            if (nvalue > nattrs) {
               stop(paste("invalid number of attribute values (", nvalue, " != ", nattrs, ")"))
            }
            attr_names <- names(attrs)
            value_names <- names(value)
            if (is.null(value_names)) {
              # check the list shape / types against attributes
              if (nvalue != nattrs) {
                stop(paste("invalid number of attribute values (", nvalue, " != ", nattrs, ")"))
              }
              names(value) <- ifelse(attr_names == "", "__attr", attr_names)
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
                  if (!anyvarlen)
                    stop("vector value dim does not match array subscript")
                }
              } else if (is.array(val)) {
                if (!all(sub_dim == dim(val))) {
                  if (!anyvarlen)
                    stop("array value dim does not match array subscript")
                }
              } else {
                stop(paste("cannot assign value of type \"", typeof(value), "\""))
              }
            }
            libtiledb_array_open(x@ptr, "WRITE")

            ## query number of cell values for schema, NA indicates variable length
            ncellval <- sapply(attrs(schema), ncells)
            if (x@as.data.frame)        # in the data.frame case first buffer is 'coords' so fill
              ncellval <- c(coords=-1, ncellval)

            offsets <- double(prod(sub_dim)) # may need to check if this is sufficient space?

            out <- tryCatch(
              {
                qry <- libtiledb_query(ctx@ptr, x@ptr, "WRITE")
                qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
                #if (is.integral(dom)) { ## -- already tested above
                  qry <- libtiledb_query_set_subarray(qry, as.integer(subarray))
                #} else {
                #  qry <- libtiledb_query_set_subarray(qry, as.double(subarray))
                #}
                attr_names <- names(value)
                for (idx in seq_along(value)) {
                  aname <- attr_names[[idx]]
                  val <- value[[idx]]
                  isvarlen <- is.na(unname(ncellval[idx]))  ## NA == variable length

                  if (isvarlen) {
                    #noff <- libtiledb_array_max_buffer_elements_offsets(x@ptr, subarray, aname)
                    #cat("noff: ", noff, "  aname: ", aname, "  names(offsets):", names(offsets), "\n")
                    qry <- libtiledb_query_set_buffer_var_test(qry, aname, val, offsets)
                  } else {
                    #if (is.list(val) || is.character(val))
                    ## missing function, never written
                    #  qry <- libtiledb_query_set_buffer_var(qry, aname, val)
                    #else
                    qry <- libtiledb_query_set_buffer(qry, aname, val)
                  }
                }
                qry <- libtiledb_query_submit(qry)
                if (libtiledb_query_status(qry) != "COMPLETE") {
                  cat("Status:", libtiledb_query_status(qry), "\n")
                  stop("error in write query (not 'COMPLETE')")
                }
                qry <- libtiledb_query_finalize(qry)
                return(x)
              },
              finally = {
                libtiledb_array_close(x@ptr)
              })
            return(out)
          })

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
