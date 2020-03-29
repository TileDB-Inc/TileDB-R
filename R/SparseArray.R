#' An S4 class for a TileDB sparse array
#'
#' @slot ctx A TileDB context object
#' @slot uri A character despription
#' @slot as.data.frame A logical value
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_sparse
setClass("tiledb_sparse",
         slots = list(ctx = "tiledb_ctx", uri = "character",
                      as.data.frame = "logical", ptr = "externalptr"))

#' Constructs a tiledb_sparse object backed by a persisted tiledb array uri
#'
#' tiledb_sparse returns a list of coordinates and attributes vectors for reads
#'
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @param as.data.frame optional logical switch, defaults to "FALSE"
#' @param ctx tiledb_ctx (optional)
#' @return tiledb_sparse array object
#' @export
tiledb_sparse <- function(uri, query_type = c("READ", "WRITE"),
                          as.data.frame=FALSE, ctx = tiledb_get_context()) {
    query_type = match.arg(query_type)
  if (!is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }

  array_xptr <- libtiledb_array_open(ctx@ptr, uri, query_type)
  schema_xptr <- libtiledb_array_get_schema(array_xptr)
  if (!libtiledb_array_schema_sparse(schema_xptr)) {
    libtiledb_array_close(array_xptr)
    stop("array URI must be a sparse array")
  }
  array_xptr <- libtiledb_array_close(array_xptr)
  new("tiledb_sparse", ctx = ctx, uri = uri, as.data.frame = as.data.frame, ptr = array_xptr)
}

#' Return a schema from a sparse array
#'
#' @param object sparse array object
#' @param ... Extra parameter for function signature, currently unused
#' @return The scheme for the object
setMethod("schema", "tiledb_sparse", function(object, ...) {
  ctx <- object@ctx
  uri <- object@uri
  schema_xptr <- libtiledb_array_schema_load(ctx@ptr, uri)
  return(tiledb_array_schema.from_ptr(schema_xptr))
})

sparse_attribute_buffers <- function(array, sch, dom, sub, filter_attributes=list()) {
  stopifnot(is(sch, "tiledb_array_schema"))
  stopifnot(is(dom, "tiledb_domain"))
  domaintype <- libtiledb_domain_get_type(dom@ptr)
  attributes <- list()
  # first alloc coordinate buffer
  ncells <- libtiledb_array_max_buffer_elements_with_type(array@ptr, sub,
                                                          libtiledb_coords(), domaintype)
  attributes[["coords"]] <- libtiledb_query_buffer_alloc_ptr(array@ptr, domaintype, ncells)
  attrs <- tiledb::attrs(sch)
  if (length(filter_attributes) > 0) {
    attrs <- Filter(function(a) is.element(name(a), filter_attributes), attrs)
  }
  # for every attribute, compute the number of cells and allocate vectors
  for(attr in attrs) {
    aname <- tiledb::name(attr)
    type <- tiledb_datatype_R_type(tiledb::datatype(attr))
    ncells <- libtiledb_array_max_buffer_elements_with_type(array@ptr, sub, aname, domaintype)
    buff <- libtiledb_query_buffer_alloc_ptr(array@ptr, tiledb::datatype(attr), ncells)
    attributes[[aname]] <- buff
  }
  return(attributes)
}

#' Construct a data.frame from query results
#'
#' Converts a tiledb object to a data.frame object
#'
#' @param dom tiledb_domain object
#' @param data tiledb object to be converted
#' @param extended optional logical variable selected wider display
#' with coordinates, defaults to false
#' @return data.frame object constructed from `data`
#' @export
as_data_frame <- function(dom, data, extended=FALSE) {
  if (!is(dom, "tiledb_domain")) {
    stop("as_data_frame must be called with a tiledb_domain object")
  }
  # If coordinates are present convert to columns in the data.frame
  if (!is.null(data[["coords"]])) {
    if (extended) {
      ndim <- tiledb_ndim(dom)
      dimensions <- dimensions(dom)
      for (i in seq(1, ndim, 1)) {
        dim_name <- name(dimensions[[i]])
        l <- list()
        l[[dim_name]] = data$coords[seq(i, length(data$coords), ndim)]
        data <- c(data, l)
      }
    }
    data$coords <- NULL
  }
  return(as.data.frame(data))
}

#' Gets a sparse array value
#'
#' @param x sparse array object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param drop Optional logical switch to drop dimensions, default FALSE, currently unused.
#' @return An element from the sparse array
setMethod("[", "tiledb_sparse",
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
            dom <- tiledb::domain(schema)
            domaintype <- libtiledb_domain_get_type(dom@ptr)
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral Domain's")
            }
            libtiledb_array_open_with_ptr(x@ptr, "READ")
            out <- tryCatch(
              {
                subarray <- domain_subarray(dom, index = index)
                if (is.integral(dom)) {
                  subarray <- as.integer(subarray)
                } else {
                  subarray <- as.double(subarray)
                }
                buffers <- sparse_attribute_buffers(x, schema, dom, subarray)
                qry <- libtiledb_query(ctx@ptr, x@ptr, "READ")
                qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
                #qry <- libtiledb_query_set_subarray(qry, subarray)
                qry <- libtiledb_query_set_subarray_with_type(qry, subarray, domaintype)
                attr_names <- names(buffers)
                for (idx in seq_along(buffers)) {
                    aname <- attr_names[[idx]]
                    val <- buffers[[idx]]
                    if (aname == "coords") {
                      qry <- libtiledb_query_set_buffer_ptr(qry, libtiledb_coords(), val)
                    } else {
                      if (is.character(val) || is.list(val)) {
                        qry <- libtiledb_query_set_buffer_var(qry, aname, val)
                      } else {
                        #qry <- libtiledb_query_set_buffer(qry, aname, val)
                        qry <- libtiledb_query_set_buffer_ptr(qry, aname, val)
                      }
                    }
                }
                qry <- libtiledb_query_submit(qry)
                if (libtiledb_query_status(qry) != "COMPLETE") {
                  stop("error in read query (not 'COMPLETE')")
                }
                # get the actual number of results, instead of realloc
                # just modify the vector length so there is no additional copy
                for (idx in seq_along(attr_names)) {
                  ##old_buffer <- buffers[[idx]]
                  old_buffer <- libtiledb_query_get_buffer_ptr(buffers[[idx]], FALSE)
                  aname <- attr_names[[idx]]
                  if (aname == "coords") {
                    ncells <- libtiledb_query_result_buffer_elements(qry, libtiledb_coords())
                  } else {
                    ncells <- libtiledb_query_result_buffer_elements(qry, aname)
                  }
                  if (ncells <= length(old_buffer)) {
                    buffers[[idx]] <- old_buffer[1:ncells]
                  } else {
                    buffers[[idx]] <- old_buffer
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

#' Sets a sparse array value
#'
#' @param x sparse array object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param value The value being assigned
#' @return The modified object
setMethod("[<-", "tiledb_sparse",
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
            attrs <- tiledb::attrs(schema)

            # check that all the indexing and value buffers are the same length
            coord_length <- length(index[[1]])
            for (i in seq_along(index)) {
              if (length(index[[i]]) != coord_length) {
                stop("invalid sparse coordinates, all coordinates must be the same length")
              }
            }

            # check that attributes are correct
            nvalue <- length(value)
            nattrs <- length(attrs)
            if (nvalue > nattrs) {
              stop("number of values to assign does not match the number of array attributes")
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
            # do type conversion based on the domain
            integral_domain <- is.integral(dom)
            coords <- list()
            for (i in seq_along(index)) {
              coord <- index[[i]]
              if (integral_domain) {
                if (!is.integer(coord)) {
                  coords[[i]] <- as.integer(coord)
                } else {
                  coords[[i]] <- coord
                }
              } else {
                if (!is.numeric(coord)) {
                  coords[[i]] = as.numeric(coord)
                } else {
                  coords[[i]] = coord
                }
              }
            }
            # check that we have the right number of index buffers
            if (length(coords) != tiledb_ndim(dom)) {
              stop(paste("number of coordinate vectors does not match the array domain"))
            }
            # zip the coordinates into a single buffer for sparse write
            if (integral_domain) {
              zip_coords <- libtiledb_zip_coords_integer(coords, coord_length)
            } else {
              zip_coords <- libtiledb_zip_coords_numeric(coords, coord_length)
            }
            libtiledb_array_open_with_ptr(x@ptr, "WRITE")
            out <- tryCatch(
              {
                qry <- libtiledb_query(ctx@ptr, x@ptr, "WRITE")
                qry <- libtiledb_query_set_layout(qry, "UNORDERED")
                qry <- libtiledb_query_set_coordinates(qry, zip_coords)
                # set attribute buffers
                attr_names <- names(value)
                for (idx in seq_along(value)) {
                  qry <- libtiledb_query_set_buffer(qry, attr_names[[idx]], value[[idx]])
                }
                qry <- libtiledb_query_submit(qry)
                if (libtiledb_query_status(qry) != "COMPLETE") {
                  stop("error in incomplete sparse write query")
                }
                qry <- libtiledb_query_finalize(qry)
                return(x);
              },
              finally = {
                libtiledb_array_close(x@ptr)
              })
            return(out)
          })

setMethod("show", "tiledb_sparse",
          function (object) {
            cat("tiledb_sparse(uri = \"", object@uri, "\")\n", sep="")
          })

#' Check if object is sparse
#'
#' @param object TileDB object
#' @return A logical value indicating whether the object is sparse
#' @export
setMethod("is.sparse", "tiledb_sparse", function(object) TRUE)


#' Query a array using a subarray vector
#'
#' tiledb_subarray returns a results of query
#'
#' @param A tiledb_sparse or tiledb_dense
#' @param subarray_vector subarray to query
#' @param attrs list of attributes to query
#' @return list of attributes being returned with query results
#' @export
tiledb_subarray <- function(A, subarray_vector, attrs=c()) {
  if (!is(A, "tiledb_sparse") && !is(A, "tiledb_dense")) {
    stop("tiledb_subarray must be called with a tiledb_dense or tiledb_sparse object")
  }
  ctx <- A@ctx
  uri <- A@uri
  schema <- tiledb::schema(A)
  dom <- tiledb::domain(schema)
  if (!tiledb::is.integral(dom)) {
    stop("subscript indexing only valid for integral Domain's")
  }
  libtiledb_array_open_with_ptr(A@ptr, "READ")
  out <- tryCatch(
    {
      if (is.integral(dom)) {
        subarray_vector <- as.integer(subarray_vector)
      } else {
        subarray_vector <- as.double(subarray_vector)
      }
      if (is.sparse(A)) {
        buffers <- sparse_attribute_buffers(A, schema, dom, subarray_vector, attrs)
      } else {
        buffers <- attribute_buffers(A, schema, dom, subarray_vector, attrs)
      }
      qry <- libtiledb_query(ctx@ptr, A@ptr, "READ")
      qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
      qry <- libtiledb_query_set_subarray(qry, subarray_vector)
      attr_names <- names(buffers)
      for (idx in seq_along(buffers)) {
        aname <- attr_names[[idx]]
        if (aname == "coords") {
          if (is(buffers[[idx]], "externalptr")) {
            qry <- libtiledb_query_set_buffer_ptr(qry, libtiledb_coords(), buffers[[idx]])
          } else {
            qry <- libtiledb_query_set_buffer(qry, libtiledb_coords(), buffers[[idx]])
          }
        } else if (is(buffers[[idx]], "externalptr")) {
          qry <- libtiledb_query_set_buffer_ptr(qry, aname, buffers[[idx]])
        } else {
          qry <- libtiledb_query_set_buffer(qry, aname, buffers[[idx]])
        }
      }
      qry <- libtiledb_query_submit(qry)
      if (libtiledb_query_status(qry) != "COMPLETE") {
        stop("error in read query (not 'COMPLETE')")
      }
      # get the actual number of results, instead of realloc
      # just modify the vector length so there is no additional copy
      for (idx in seq_along(attr_names)) {
        if (is(buffers[[idx]], "externalptr")) {
          old_buffer <- libtiledb_query_get_buffer_ptr(buffers[[idx]], FALSE)
        } else {
          old_buffer <- buffers[[idx]]
        }
        aname <- attr_names[[idx]]
        if (aname == "coords") {
          ncells <- libtiledb_query_result_buffer_elements(qry, libtiledb_coords())
        } else {
          ncells <- libtiledb_query_result_buffer_elements(qry, aname)
        }
        if (ncells < length(old_buffer)) {
          buffers[[idx]] <- old_buffer[1:ncells]
        } else {
          buffers[[idx]] <- old_buffer
        }
      }
      for (i in 1:length(buffers)) {
        attr(buffers[[i]], "datatype") <- NULL
      }
      if (A@as.data.frame) {
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
      libtiledb_array_close(A@ptr)
    }
  )
  return(out);
}
