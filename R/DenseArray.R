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

#' An S4 class for a TileDB dense array
#'
#' @slot ctx A TileDB context object
#' @slot uri A character despription
#' @slot as.data.frame A logical value
#' @slot attrs A character vector
#' @slot extended A logical value
#' @slot ptr External pointer to the underlying implementation
#' @section Planned Deprecation:
#' We plan to deprecate the \code{tiledb_dense} array type in a future release. While exact
#' timelines have not been finalised, it is advised to the \code{tiledb_array} for both
#' \emph{dense} and \emph{sparse} arrays going forward.
#' @exportClass tiledb_dense
setClass("tiledb_dense",
         slots = list(ctx = "tiledb_ctx",
                      uri = "character",
                      as.data.frame = "logical",
                      attrs = "character",
                      extended = "logical",
                      ptr = "externalptr"))

#' Constructs a tiledb_dense object backed by a persisted tiledb array uri
#'
#' @section Planned Deprecation:
#' We plan to deprecate the \code{tiledb_dense} array type in a future release. While exact
#' timelines have not been finalised, it is advised to the \code{tiledb_array} for both
#' \emph{dense} and \emph{sparse} arrays going forward.
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @param as.data.frame optional logical switch, defaults to "FALSE"
#' @param attrs optional character vector to select attributes, default is
#' empty implying all are selected
#' @param extended optional logical switch selecting wide \sQuote{data.frame}
#' format, defaults to "FALSE"
#' @param ctx tiledb_ctx (optional)
#' @return tiledb_dense array object
#' @export
tiledb_dense <- function(uri,
                         query_type = c("READ", "WRITE"),
                         as.data.frame = FALSE,
                         attrs = character(),
                         extended = FALSE,
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
  if (libtiledb_array_schema_sparse(schema_xptr)) {
    libtiledb_array_close(array_xptr)
    stop("array URI must be a dense array")
  }
  array_xptr <- libtiledb_array_close(array_xptr)
  new("tiledb_dense", ctx = ctx, uri = uri,
      as.data.frame = as.data.frame, attrs = attrs,
      extended = extended, ptr = array_xptr)
}

#' Prints a tiledb_dense array object
#'
#' @param object A tiledb_dense array object
#' @export
setMethod("show",
          signature = "tiledb_dense",
          definition = function (object) {
  cat("tiledb_dense array\n"
     ,"  uri           = '", object@uri, "'\n"
     ,"  as.data.frame = ", if (object@as.data.frame) "TRUE" else "FALSE", "\n"
     ,"  attrs         = ", if (length(object@attrs) == 0) "(none)"
                            else paste(object@attrs, collapse=","), "\n"
     ,"  extended      = ", if (object@extended) "TRUE" else "FALSE", "\n"
    , sep="")
  })

# # ' Reopens a TileDB array an opened tiledb array
# # '
# # ' Reopening an array is useful when the array got updated after it got opened
# # ' and the tiledb array object got created. To sync-up with the updates,
# # ' the user must either close the array and open again,
# # ' or just use tiledb_reopen(array) which can be faster because
# # ' only metdata regarding updates has to be loaded.
# # '
# # ' @param object tileb array object
# # ' @return the reopened array object
# # ' @export
# setGeneric("reopen", function(object, ...) standardGeneric("reopen"))
#
# # ' @export
# setMethod("reopen", "tiledb_dense", function(object) {
#   libtiledb_array_reopen(object@ptr)
#   return(object)
# })
#
# # ' Closes a tiledb array object
# # '
# # ' @param conn tiledb array object
# # ' @return returns the closed array object
# close.tiledb_dense <- function(conn, ...)  {
#   stopifnot(is(conn, "tiledb_dense"))
#   libtiledb_array_close(conn@ptr)
#   return(conn);
# }

#' Returns true is if the array or array_schema is sparse
#'
#' @param object tiledb_dense
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

attribute_buffers <- function(array, sch, dom, sub, selected) {
  stopifnot(is(sch, "tiledb_array_schema"))
  stopifnot(is(dom, "tiledb_domain"))
  sub_dim <- subarray_dim(sub)
  ncells <- prod(sub_dim)
  is_scalar <- all(sub_dim == 1L)
  domaintype <- libtiledb_domain_get_type(dom@ptr)

  attributes <- list()

  # first alloc coordinate buffer if we are returning a data.frame
  if(array@as.data.frame) {
    ncells_coords <- 2*ncells
    if (is.integral(dom)) {
      attributes[["coords"]] <- integer(length = ncells_coords)
    } else {
      attributes[["coords"]]  <- numeric(length = ncells_coords)
    }
  }
  attrs <- tiledb::attrs(sch)
  if (length(selected) == 0) {          # no selection given -> use all
    selected <- names(attrs)
  }
  for (attr in attrs) {
    aname <- tiledb::name(attr)
    if (! aname %in% selected) {
      next
    }
    dtype <- tiledb::datatype(attr)
    type <- tiledb_datatype_R_type(dtype)
    datatype <- libtiledb_attribute_get_type(attr@ptr)
    #cat("dtype:", dtype, " type:", type, " datatype:", datatype, "\n", sep="")
    if (type %in% c("integer", "double")) {
      buff <- vector(mode = type, length = ncells)
    } else if (dtype %in% c("CHAR")) {  # TODO: add other char and date types
      ##buff <- libtiledb_query_buffer_var_char_alloc(array@ptr, as.integer(sub), aname)
      buff <- libtiledb_query_buffer_var_char_alloc_direct(ncells, ncells*8, FALSE, sub[4]-sub[3]+1)
    } else if (datatype %in% c("DATETIME_DAY", "DATETIME_SEC", "DATETIME_MS",
                               "DATETIME_US", "DATETIME_NS")) {
      buff <- libtiledb_query_buffer_alloc_ptr(array@ptr, datatype, ncells)
    } else {
      stop("Unsupported data type for attribute ", aname)
    }
    # If its not scalar and we are not getting it as a data.frame set the dimension attribute
    if (!is_scalar && !array@as.data.frame &&
        !dtype %in% c("CHAR", "DATETIME_DAY", "DATETIME_SEC", "DATETIME_MS",
                      "DATETIME_US", "DATETIME_NS")) {
      attr(buff, "dim") <- sub_dim
    }
    attr(buff, "datatype") <- datatype
    attributes[[aname]] <- buff
  }
  return(attributes)
}

#' Gets a dense array value
#'
#' @param x dense array object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param drop Optional logical switch to drop dimensions, default FALSE, currently unused.
#' @return An element from a dense array
#' @aliases [,tiledb_dense
#' @aliases [,tiledb_dense-method
#' @aliases [,tiledb_dense,ANY,tiledb_dense-method
#' @aliases [,tiledb_dense,ANY,ANY,tiledb_dense-method
setMethod("[", "tiledb_dense",
          function(x, i, j, ..., drop = FALSE) {
            ## helper function to deal with i and/or j missing
            index <- nd_index_from_syscall(sys.call(), parent.frame())
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
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral Domain types")
            }
            libtiledb_array_open_with_ptr(x@ptr, "READ")
            on.exit(libtiledb_array_close(x@ptr))

            subarray <- domain_subarray(dom, index = index)
            buffers <- attribute_buffers(x, schema, dom, subarray, sel)
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
              val <- buffers[[idx]]
              if (aname == "coords") {
                qry <- libtiledb_query_set_buffer(qry, libtiledb_coords(), val)
              } else {
                #if (is.character(val) || is.list(val)) {
                #  stop("var length never implemented")
                #  #qry <- libtiledb_query_set_buffer_var(qry, aname, val)
                #} else
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
            ## If true, delete the dimensions of an array which have only one level
            if (drop) {
              for (i in seq_len(length(buffers))) {
                buffers[[i]] <- drop(buffers[[i]])
              }
            }

            ## get the actual number of results, instead of realloc
            ## just modify the vector length so there is no additional copy
            for (idx in seq_along(attr_names)) {
              old_buffer <- buffers[[idx]]

              aname <- attr_names[[idx]]
              dtype <- attr(buffers[[idx]], "datatype")
              #cat("---Name: ", aname, " ----Type: ", dtype, "\n")

              if (is(old_buffer, "externalptr")) {
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
              if (ncells < length(old_buffer) || x@as.data.frame) {
                ## for char attributtes ncells is sum of nchar and an overestimate for the indexing
                buffers[[idx]] <- old_buffer[1:min(ncells, length(old_buffer))]
              } else {
                buffers[[idx]] <- old_buffer
              }
            }
            for (i in 1:length(buffers)) {
              attr(buffers[[i]], "datatype") <- NULL
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


#' Sets a dense array value
#'
#' @param x dense array object
#' @param i parameter key string
#' @param j parameter key string, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @param value The value being assigned
#' @return The modified object
#' @importFrom utils head
#' @aliases [<-,tiledb_dense
#' @aliases [<-,tiledb_dense-method
#' @aliases [<-,tiledb_dense,ANY,tiledb_dense-method
#' @aliases [<-,tiledb_dense,ANY,ANY,tiledb_dense-method
setMethod("[<-", "tiledb_dense",
          function(x, i, j, ..., value) {
            if (!is.list(value)) {
              if (is.array(value) || is.vector(value) ||
                  isS4(value) || is(value, "Date") || inherits(value, "POSIXt")) {
                value <- list(value)
              } else {
                stop("Cannot assign initial value of type '", typeof(value), "'", call.=FALSE)
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
            subarray <- domain_subarray(dom, index = index)
            attrs <- tiledb::attrs(schema)
            nvalue <- length(value)
            nattrs <- length(attrs)
            if (nvalue > nattrs) {
               stop("invalid number of attribute values (", nvalue, " != ", nattrs, ")")
            }
            attr_names <- names(attrs)
            value_names <- names(value)
            if (is.null(value_names)) {
              # check the list shape / types against attributes
              if (nvalue != nattrs) {
                stop("invalid number of attribute values (", nvalue, " != ", nattrs, ")")
              }
              names(value) <- ifelse(attr_names == "", "__attr", attr_names)
            } else {
              # check associative assignment
              for (name in value_names)  {
                if (!(name %in%  attr_names)) {
                  stop("invalid array attribute value name: '", name, "'")
                }
              }
            }
            # check that value shapes match the subarray shape
            # TODO: R doesn't check this and just assigns values that overlap the domain
            sub_dim <- subarray_dim(subarray)

            for (i in seq_along(value)) {
              #if (class(value[[i]])=="Date") browser()
              val <- value[[i]]
              if (is.vector(val) ||
                  inherits(val, "Date") ||
                  inherits(val, "POSIXt") ||
                  inherits(val, "nanotime")) {
                if (length(sub_dim) != 1 || sub_dim[1L] != length(val)) {
                  stop("value dim does not match array subscript")
                }
              } else if (is.array(val)) {
                if (!all(sub_dim == dim(val))) {
                  stop("value dim does not match array subscript")
                }
              } else {
                stop("cannot assign column value of type '", typeof(value), "'")
              }
            }
            libtiledb_array_open_with_ptr(x@ptr, "WRITE")
            on.exit(libtiledb_array_close(x@ptr))
            qry <- libtiledb_query(ctx@ptr, x@ptr, "WRITE")
            qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
            if (is.integral(dom)) {
              qry <- libtiledb_query_set_subarray(qry, as.integer(subarray))
            } else {
              qry <- libtiledb_query_set_subarray(qry, as.double(subarray))
            }
            attr_names <- names(value)
            ## we need to hold on to the allocated buffers til the query fires
            buflst <- vector(mode="list", length=length(attr_names))
            for (idx in seq_along(value)) {
              aname <- attr_names[[idx]]
              val <- value[[idx]]

              attribute <- libtiledb_array_schema_get_attribute_from_name(schema@ptr, aname)
              attrtype <- libtiledb_attribute_get_type(attribute)

              if (is.list(val) || is.character(val)) {
                ##qry <- libtiledb_query_set_buffer_var(qry, aname, val)
                n <- ifelse(is.vector(val), length(val), prod(dim(val)))
                string <- paste(val[1:n], collapse="")
                ## offsets starts: cumulative sum of all word lengths as provided by nchar
                ## but starting at 0 and then omitting the last
                offs <- cumsum(c(0, head(sapply(val[1:n], nchar, USE.NAMES=FALSE), -1)))
                buflst[[idx]] <- libtiledb_query_buffer_var_char_create(offs, string)
                qry <- libtiledb_query_set_buffer_var_char(qry, aname, buflst[[idx]])
              } else if (inherits(val, "Date")) {
                buflst[[idx]] <- libtiledb_query_buffer_alloc_ptr(x@ptr, "DATETIME_DAY", length(val))
                buflst[[idx]] <- libtiledb_query_buffer_assign_ptr(buflst[[idx]], "DATETIME_DAY", val)
                qry <- libtiledb_query_set_buffer_ptr(qry, aname, buflst[[idx]])
              } else if (inherits(val, "POSIXt")) {
                #cat("*** POSIXt case\n")
                # could also use DATETIME_SEC here but _MS dominates it with higher resolution
                buflst[[idx]] <- libtiledb_query_buffer_alloc_ptr(x@ptr, attrtype, length(val))
                buflst[[idx]] <- libtiledb_query_buffer_assign_ptr(buflst[[idx]], attrtype, val)
                qry <- libtiledb_query_set_buffer_ptr(qry, aname, buflst[[idx]])
              } else if (inherits(val, "nanotime")) {
                buflst[[idx]] <- libtiledb_query_buffer_alloc_ptr(x@ptr, "DATETIME_NS", length(val))
                buflst[[idx]] <- libtiledb_query_buffer_assign_ptr(buflst[[idx]], "DATETIME_NS", val)
                qry <- libtiledb_query_set_buffer_ptr(qry, aname, buflst[[idx]])
              } else {
                buflst[[idx]] <- libtiledb_query_buffer_alloc_ptr(x@ptr, attrtype, length(val))
                buflst[[idx]] <- libtiledb_query_buffer_assign_ptr(buflst[[idx]], attrtype, val)
                qry <- libtiledb_query_set_buffer_ptr(qry, aname, buflst[[idx]])
              }
            }
            qry <- libtiledb_query_submit(qry)
            if (libtiledb_query_status(qry) != "COMPLETE") {
              stop("error in write query")
            }
            qry <- libtiledb_query_finalize(qry)
            return(x)
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

## -- as.data.frame accessor

#' @rdname return.data.frame-tiledb_dense-method
#' @param ... Currently unused
#' @export
setGeneric("return.data.frame", function(object, ...) standardGeneric("return.data.frame"))

#' Retrieve data.frame return toggle
#'
#' A \code{tiledb_dense} object can be returned as an array (or list of arrays),
#' or, if select, as a \code{data.frame}. This methods returns the selection value.
#' @param object A \code{tiledb_dense} array object
#' @return A logical value indicating whether \code{data.frame} return is selected
#' @export
setMethod("return.data.frame",
          signature = "tiledb_dense",
          function(object) object@as.data.frame)


## -- as.data.frame setter

#' @rdname return.data.frame-set-tiledb_dense-method
#' @export
setGeneric("return.data.frame<-", function(x, value) standardGeneric("return.data.frame<-"))

#' Set data.frame return toggle
#'
#' A \code{tiledb_dense} object can be returned as an array (or list of arrays),
#' or, if select, as a \code{data.frame}. This methods sets the selection value.
#' @param x A \code{tiledb_dense} array object
#' @param value A logical value with the selection
#' @return The modified \code{tiledb_dense} array object
#' @export
setReplaceMethod("return.data.frame",
                 signature = "tiledb_dense",
                 function(x, value) {
  x@as.data.frame <- value
  validObject(x)
  x
})


## -- attrs

#' Retrieve attributes from \code{tiledb_dense} object
#'
#' By default, all attributes will be selected. But if a subset of attribute
#' names is assigned to the internal slot \code{attrs}, then only those attributes
#' will be queried.  This methods accesses the slot.
#' @param object A \code{tiledb_dense} array object
#' @return An empty character vector if no attributes have been selected or else
#' a vector with attributes.
#' @importFrom methods validObject
#' @export
setMethod("attrs",
          signature = "tiledb_dense",
          function(object) object@attrs)

#' @rdname attrs-set-tiledb_dense-method
#' @export
setGeneric("attrs<-", function(x, value) standardGeneric("attrs<-"))

#' Selects attributes for the given TileDB array
#'
#' @param x A \code{tiledb_dense} array object
#' @param value A character vector with attributes
#' @return The modified \code{tiledb_dense} array object
#' @export
setReplaceMethod("attrs",
                 signature = "tiledb_dense",
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
