#' @exportClass Array
setClass("Array",
         slots = list(ctx = "Ctx", schema = "ArraySchema", uri = "character"))

#' @export
Array <- function(ctx, schema, uri) {
  if (missing(ctx) || !is(ctx, "Ctx"))  {
    stop("argument ctx must be a tiledb::Ctx") 
  } else if (missing(schema) || !is(schema, "ArraySchema")) {
    stop("argument schema must a tiledb::ArraySchema") 
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }
  uri <- tiledb_array_create(schema@ptr, uri) 
  new("Array", ctx = ctx, schema = schema, uri = uri) 
}

#' @export
Array.save <- function(ctx, x, uri) {
  if (missing(ctx) || !is(ctx, "Ctx")) {
    stop("argument ctx must be a tiledb::Ctx")  
  } else if (missing(x) || !is.vector(x) || !is.array(x)) {
    stop("x must be an array or vector")
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar") 
  } 
  stop("NOT IMPLEMENTED!")
}

#' @export
Array.load <- function(ctx, uri) {
  if (missing(ctx) || !is(ctx, "Ctx")) {
    stop("argument ctx must be a tiledb::Ctx")  
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar") 
  }
  ptr <- tiledb_array_load(ctx@ptr, uri)
  schema <- ArraySchema.from_ptr(ptr)
  if (is.sparse(schema)) {
    stop("array URI must be a dense array") 
  }
  new("Array", ctx = ctx, schema = schema, uri = uri)
}

setMethod("show", "Array",
          function (object) {
            cat("tiledb::Array object @ ", object@uri, "\n")
            invisible(print(object[]))
          })

#' @export
setGeneric("is.sparse", function(object, ...) standardGeneric("is.sparse"))

#' @export
setMethod("is.sparse", "Array", function(object) FALSE)

domain_subarray <- function(dom, index = NULL) {
  stopifnot(is(dom, "Domain"))
  nd <- tiledb::ndim(dom)
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
  stopifnot(is(sch, "ArraySchema"))
  stopifnot(is(dom, "Domain"))
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

setMethod("[", "Array",
          function(x, i, j, ..., drop = FALSE) {
            index <- nd_index_from_syscall(sys.call(), parent.frame())
            ctx <- x@ctx
            uri <- x@uri
            schema <- x@schema
            dom <- tiledb::domain(schema)
            if (!tiledb::is.integral(dom)) {
              stop("subscript indexing only valid for integral Domain's")  
            }
            subarray <- domain_subarray(dom, index = index)
            buffers <- attribute_buffers(schema, dom, subarray)
            qry <- tiledb_query(ctx@ptr, uri, "READ")
            qry <- tiledb_query_set_layout(qry, "COL_MAJOR")
            if (is.integral(dom)) {
              qry <- tiledb_query_set_subarray(qry, as.integer(subarray))
            } else {
              qry <- tiledb_query_set_subarray(qry, as.double(subarray))
            }
            for (attr_name in names(buffers)) {
              qry <- tiledb_query_set_buffer(qry, attr_name, buffers[[attr_name]])
            }
            qry <- tiledb_query_submit(qry)
            if (tiledb_query_status(qry) != "COMPLETE") {
              stop("error in read query")
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
          })

setMethod("[<-", "Array",
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
            schema <- x@schema
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
            qry <- tiledb_query(ctx@ptr, uri, "WRITE") 
            qry <- tiledb_query_set_layout(qry, "COL_MAJOR")
            if (is.integral(dom)) {
              qry <- tiledb_query_set_subarray(qry, as.integer(subarray))
            } else {
              qry <- tiledb_query_set_subarray(qry, as.double(subarray))
            }
            for (i in seq_along(value)) {
              qry <- tiledb_query_set_buffer(qry, names(value)[[i]], value[[i]])
            }
            qry <- tiledb_query_submit(qry)
            if (tiledb_query_status(qry) != "COMPLETE") {
              stop("error in write query") 
            }
            qry <- tiledb_query_finalize(qry)
            return(x);
          })

as.array.Array <- function(x, ...) {
 return(x[]) 
}

as.data.frame.Array <- function(x, row.names = NULL, optional = FALSE, ...,
                                cut.names = FALSE, col.names = NULL, fix.empty.names = TRUE,
                                stringsAsFactors = default.stringsAsFactors()) {
  lst <- x[]
  if (!is(lst, "list")) {
    lst <- list(lst) 
  }
  if (is.null(col.names)) {
    col.names <- sapply(tiledb::attrs(x@schema), tiledb::name)
  }
  return(as.data.frame(lst, row.names = row.names, optional = optional, ..., 
                       cut.names = cut.names, col.names = col.names, fix.empty.names = fix.empty.names,
                       stringsAsFactors = default.stringsAsFactors()))
}