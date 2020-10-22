#  MIT License
#
#  Copyright (c) 2017-2020 TileDB Inc.
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

#' An S4 class for a TileDB sparse array
#'
#' @slot ctx A TileDB context object
#' @slot uri A character despription
#' @slot as.data.frame A logical value
#' @slot attrs A character vector
#' @slot extended A logical value
#' @slot ptr External pointer to the underlying implementation
#' @section Planned Deprecation:
#' We plan to deprecate the \code{tiledb_sparse} array type in a future release. While exact
#' timelines have not been finalised, it is advised to the \code{tiledb_array} for both
#' \emph{dense} and \emph{sparse} arrays going forward.
#' @exportClass tiledb_sparse
setClass("tiledb_sparse",
         slots = list(ctx = "tiledb_ctx",
                      uri = "character",
                      as.data.frame = "logical",
                      attrs = "character",
                      extended = "logical",
                      ptr = "externalptr"))

#' Constructs a tiledb_sparse object backed by a persisted tiledb array uri
#'
#' tiledb_sparse returns a list of coordinates and attributes vectors for reads
#'
#' @section Planned Deprecation:
#' We plan to deprecate the \code{tiledb_sparse} array type in a future release. While exact
#' timelines have not been finalised, it is advised to the \code{tiledb_array} for both
#' \emph{dense} and \emph{sparse} arrays going forward.
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @param as.data.frame optional logical switch, defaults to "FALSE"
#' @param attrs optional character vector to select attributes, default is
#' empty implying all are selected
#' @param extended optional logical switch selecting wide \sQuote{data.frame}
#' format, defaults to "TRUE"
#' @param ctx tiledb_ctx (optional)
#' @return tiledb_sparse array object
#' @export
tiledb_sparse <- function(uri,
                          query_type = c("READ", "WRITE"),
                          as.data.frame = FALSE,
                          attrs = character(),
                          extended = TRUE,
                          ctx = tiledb_get_context()) {
  query_type = match.arg(query_type)
  if (!is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }
  .Deprecated("tiledb_array")
  array_xptr <- libtiledb_array_open(ctx@ptr, uri, query_type)
  schema_xptr <- libtiledb_array_get_schema(array_xptr)
  if (!libtiledb_array_schema_sparse(schema_xptr)) {
    libtiledb_array_close(array_xptr)
    stop("array URI must be a sparse array")
  }
  array_xptr <- libtiledb_array_close(array_xptr)
  new("tiledb_sparse", ctx = ctx, uri = uri,
      as.data.frame = as.data.frame, attrs = attrs,
      extended = extended, ptr = array_xptr)
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

sparse_attribute_buffers <- function(array, sch, dom, sub, selected) {
  stopifnot(is(sch, "tiledb_array_schema"))
  stopifnot(is(dom, "tiledb_domain"))
  #domaintype <- libtiledb_domain_get_type(dom@ptr)
  domaintype <- sapply(libtiledb_domain_get_dimensions(dom@ptr),
                       libtiledb_dim_get_datatype)
  attributes <- list()
  # first alloc coordinate buffer
  #print(domaintype)
  ncells <- libtiledb_array_max_buffer_elements_with_type(array@ptr, sub,
                                                          libtiledb_coords(), domaintype[1])
  attributes[["coords"]] <- libtiledb_query_buffer_alloc_ptr(array@ptr, domaintype[1], ncells)
  attrs <- tiledb::attrs(sch)
  if (length(selected) == 0) {          # no selection given -> use all
    selected <- names(attrs)
  }
  # for every attribute, compute the number of cells and allocate vectors
  for (attr in attrs) {
    aname <- tiledb::name(attr)
    if (! aname %in% selected) {
      next
    }
    dtype <- tiledb::datatype(attr)
    type <- tiledb_datatype_R_type(dtype)
    datatype <- libtiledb_attribute_get_type(attr@ptr)
    #cat("dtype:", dtype, " type:", type, " datatype:", datatype, "\n", sep="")
    ncells <- libtiledb_array_max_buffer_elements_with_type(array@ptr, sub, aname, domaintype[1])
    if (dtype %in% c("CHAR")) {  # TODO: add other char and date types
      buff <- libtiledb_query_buffer_var_char_alloc(array@ptr, sub, aname)
    } else if (datatype %in% c("DATETIME_DAY", "DATETIME_SEC", "DATETIME_MS",
                               "DATETIME_US", "DATETIME_NS")) {
      buff <- libtiledb_query_buffer_alloc_ptr(array@ptr, datatype, ncells)
    } else if (type %in% c("integer", "double")) {
      buff <- vector(mode = type, length = ncells)
    } else {
      stop("Unsupported data type for attribute ", aname)
    }
    attr(buff, "datatype") <- datatype
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
#' @aliases [,tiledb_sparse
#' @aliases [,tiledb_sparse-method
#' @aliases [,tiledb_sparse,ANY,tiledb_sparse-method
#' @aliases [,tiledb_sparse,ANY,ANY,tiledb_sparse-method
setMethod("[", "tiledb_sparse",
          function(x, i, j, ..., drop = FALSE) {
            ## helper function to deal with i and/or j missing
            index <- nd_index_from_syscall(sys.call(), parent.frame())
            #print(str(index))
            #print(object.size(index))
            # If we have a list of lists of lists we need to remove one layer
            # This happens when a user uses a list of coordinates
            if (isNestedList(index[1])) {
              index <- index[[1]]
            }
            ctx <- x@ctx
            uri <- x@uri
            sel <- x@attrs
            schema <- tiledb::schema(x)
            dom <- tiledb::domain(schema)

            nd <- libtiledb_domain_get_ndim(dom@ptr)
            #domaintype <- libtiledb_domain_get_type(dom@ptr)
            domaintype <- sapply(libtiledb_domain_get_dimensions(dom@ptr),
                                 libtiledb_dim_get_datatype)
            #if (!tiledb::is.integral(dom)) {
            #  stop("subscript indexing only valid for integral Domain's")
            #}
            libtiledb_array_open_with_ptr(x@ptr, "READ")
            on.exit(libtiledb_array_close(x@ptr))

            subarray <- domain_subarray(dom, index = index)
            #print(subarray)
            if (is.integral(dom)) {
              subarray <- as.integer(subarray)
            } else {
              subarray <- as.double(subarray)
            }
            buffers <- sparse_attribute_buffers(x, schema, dom, subarray, sel)
            qry <- libtiledb_query(ctx@ptr, x@ptr, "READ")
            qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
            #qry <- libtiledb_query_set_subarray(qry, subarray)
            qry <- libtiledb_query_set_subarray_with_type(qry, subarray, domaintype[1])
            attr_names <- names(buffers)
            for (idx in seq_along(buffers)) {
              aname <- attr_names[[idx]]
              val <- buffers[[idx]]
              if (aname == "coords") {
                qry <- libtiledb_query_set_buffer_ptr(qry, libtiledb_coords(), val)
              } else {
                if (is(val, "externalptr")) {
                  datatype <- attr(val, "datatype")
                  if (datatype == "CHAR") {
                    qry <- libtiledb_query_set_buffer_var_char(qry, aname, val)
                  } else if (datatype %in% c("DATETIME_DAY", "DATETIME_SEC", "DATETIME_MS",
                                             "DATETIME_US", "DATETIME_NS")) {
                    qry <- libtiledb_query_set_buffer_ptr(qry, aname, val)
                  } else {
                    stop("Currently unsupported type: ", datatype)
                  }
                } else {
                  qry <- libtiledb_query_set_buffer(qry, aname, val)
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
              old_buffer <- buffers[[idx]]
              aname <- attr_names[[idx]]

              if (aname == "coords") {
                old_buffer <- libtiledb_query_get_buffer_ptr(buffers[[idx]])
              } else if (is(old_buffer, "externalptr")) {
                ##attribute <- libtiledb_array_schema_get_attribute_from_name(schema@ptr, aname)
                ##attrtype <- libtiledb_attribute_get_type(attribute)
                dtype <- attr(buffers[[idx]], "datatype")
                if (dtype == "CHAR") {
                  old_buffer <- libtiledb_query_get_buffer_var_char(buffers[[idx]])
                } else if (dtype %in% c("DATETIME_DAY", "DATETIME_SEC",
                                        "DATETIME_MS", "DATETIME_US", "DATETIME_NS")) {
                  old_buffer <- libtiledb_query_get_buffer_ptr(buffers[[idx]])
                } else {
                  stop("Unsupported data type for attribute ", aname)
                }
              }

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
              return(as_data_frame(dom, buffers, x@extended))
            } else {
              ## if there is only one buffer, don't return a list of attribute buffers
              if (length(buffers) == 1L) {
                return(buffers[[1L]])
              }
              return(buffers)
            }
})

#' Sets a sparse array value
#'
#' @param x sparse array object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param value The value being assigned
#' @return The modified object
#' @aliases [<-,tiledb_sparse
#' @aliases [<-,tiledb_sparse-method
#' @aliases [<-,tiledb_sparse,ANY,tiledb_sparse-method
#' @aliases [<-,tiledb_sparse,ANY,ANY,tiledb_sparse-method
setMethod("[<-", "tiledb_sparse",
          function(x, i, j, ..., value) {
            if (!is.list(value)) {
              if (is.array(value) || is.vector(value) ||
                  isS4(value) || is(value, "Date") || inherits(value, "POSIXt")) {
                value <- list(value)
              } else {
                stop("Cannot assign value of type '", typeof(value), "'", call.=FALSE)
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
            on.exit(libtiledb_array_close(x@ptr))
            qry <- libtiledb_query(ctx@ptr, x@ptr, "WRITE")
            qry <- libtiledb_query_set_layout(qry, "UNORDERED")
            domaintype <- sapply(libtiledb_domain_get_dimensions(dom@ptr),
                                 libtiledb_dim_get_datatype)
            #cat("Zipped coords\n"); print(zip_coords)
            qry <- libtiledb_query_set_coordinates(qry, zip_coords, domaintype[1])
            ## set attribute buffers
            attr_names <- names(value)
            ## we need to hold on to the allocated buffers til the query fires
            buflst <- vector(mode="list", length=length(attr_names))
            for (idx in seq_along(value)) {
              aname <- attr_names[[idx]]
              val <- value[[idx]]
              attribute <- libtiledb_array_schema_get_attribute_from_name(schema@ptr, aname)
              attrtype <- libtiledb_attribute_get_type(attribute)

              if (inherits(val, "POSIXt") || inherits(val, "nanotime")) {
                buflst[[idx]] <- libtiledb_query_buffer_alloc_ptr(x@ptr, attrtype, length(val))
                buflst[[idx]] <- libtiledb_query_buffer_assign_ptr(buflst[[idx]], attrtype, val)
                qry <- libtiledb_query_set_buffer_ptr(qry, aname, buflst[[idx]])
              } else if (inherits(val, "Date")) {
                buflst[[idx]] <- libtiledb_query_buffer_alloc_ptr(x@ptr, "DATETIME_DAY", length(val))
                buflst[[idx]] <- libtiledb_query_buffer_assign_ptr(buflst[[idx]], "DATETIME_DAY", val)
                qry <- libtiledb_query_set_buffer_ptr(qry, aname, buflst[[idx]])
              } else if (inherits(val, "character")) {
                n <- ifelse(is.vector(val), length(val), prod(dim(val)))
                string <- paste(val[1:n], collapse="")
                ## offsets starts: cumulative sum of all word lengths as provided by nchar
                ## but starting at 0 and then omitting the last
                offs <- cumsum(c(0, head(sapply(val[1:n], nchar, USE.NAMES=FALSE), -1)))
                buflst[[idx]] <- libtiledb_query_buffer_var_char_create(offs, string)
                qry <- libtiledb_query_set_buffer_var_char(qry, aname, buflst[[idx]])
              } else {
                #qry <- libtiledb_query_set_buffer(qry, aname, val)
                buflst[[idx]] <- libtiledb_query_buffer_alloc_ptr(x@ptr, attrtype, length(val))
                buflst[[idx]] <- libtiledb_query_buffer_assign_ptr(buflst[[idx]], attrtype, val)
                qry <- libtiledb_query_set_buffer_ptr(qry, aname, buflst[[idx]])
              }
            }
            qry <- libtiledb_query_submit(qry)
            if (libtiledb_query_status(qry) != "COMPLETE") {
              stop("error in incomplete sparse write query")
            }
            qry <- libtiledb_query_finalize(qry)
            return(x)
          })

#' Prints a tiledb_sparse array object
#'
#' @param object A tiledb_sparse array object
#' @export
setMethod("show",
          signature = "tiledb_sparse",
          definition = function (object) {
  cat("tiledb_sparse array\n"
     ,"  uri           = '", object@uri, "'\n"
     ,"  as.data.frame = ", if (object@as.data.frame) "TRUE" else "FALSE", "\n"
     ,"  attrs         = ", if (length(object@attrs) == 0) "(none)"
                            else paste(object@attrs, collapse=","), "\n"
     ,"  extended      = ", if (object@extended) "TRUE" else "FALSE", "\n"
    , sep="")

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
          old_buffer <- libtiledb_query_get_buffer_ptr(buffers[[idx]]) #, TRUE, FALSE)
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

## -- as.data.frame accessor (generic in DenseArray.R)

#' Retrieve data.frame return toggle
#'
#' A \code{tiledb_sparse} object can be returned as an array (or list of arrays),
#' or, if select, as a \code{data.frame}. This methods returns the selection value.
#' @param object A \code{tiledb_sparse} array object
#' @return A logical value indicating whether \code{data.frame} return is selected
#' @export
setMethod("return.data.frame",
          signature = "tiledb_sparse",
          function(object) object@as.data.frame)


## -- as.data.frame setter (generic in DenseArray.R)

#' Set data.frame return toggle
#'
#' A \code{tiledb_sparse} object can be returned as an array (or list of arrays),
#' or, if select, as a \code{data.frame}. This methods sets the selection value.
#' @param x A \code{tiledb_sparse} array object
#' @param value A logical value with the selection
#' @return The modified \code{tiledb_sparse} array object
#' @export
setReplaceMethod("return.data.frame",
                 signature = "tiledb_sparse",
                 function(x, value) {
  x@as.data.frame <- value
  validObject(x)
  x
})


## -- attrs (generic in Attributes.R and DenseArray.R)

#' Retrieve attributes from \code{tiledb_sparse} object
#'
#' By default, all attributes will be selected. But if a subset of attribute
#' names is assigned to the internal slot \code{attrs}, then only those attributes
#' will be queried.  This methods accesses the slot.
#' @param object A \code{tiledb_sparse} array object
#' @return An empty character vector if no attributes have been selected or else
#' a vector with attributes.
#' @importFrom methods validObject
#' @export
setMethod("attrs",
          signature = "tiledb_sparse",
          function(object) object@attrs)

#' Selects attributes for the given TileDB array
#'
#' @param x A \code{tiledb_sparse} array object
#' @param value A character vector with attributes
#' @return The modified \code{tiledb_sparse} array object
#' @export
setReplaceMethod("attrs",
                 signature = "tiledb_sparse",
                 function(x, value) {
  nm <- names(attrs(schema(x)))
  if (length(nm) == 0) {                # none set so far
    x@attrs <- value
  } else {
    pm <- pmatch(value, nm)
    if (any(is.na(pm))) {
      stop("Multiple partial matches ambiguous: ",
           paste(value[which(is.na(pm))], collapse=","), call.=FALSE)
    }
    x@attrs <- nm[pm]
  }
  validObject(x)
  x
})
