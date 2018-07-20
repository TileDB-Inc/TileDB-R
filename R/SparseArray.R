#' @exportClass "tiledb_sparse"
setClass("tiledb_sparse",
         slots = list(ctx = "tiledb_ctx", uri = "character", ptr = "externalptr"))

#' Constructs a tiledb_sparse object backed by a persisted tiledb array uri
#'
#' tiledb_sparse returns a list of coordinates and attributes vectors for reads
#'
#' @param ctx tiledb_ctx
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @return tiledb_sparse array object
#' @export
tiledb_sparse <- function(ctx, uri, query_type = c("RW", "READ", "WRITE")) {
    query_type = match.arg(query_type)
  if (missing(ctx) || !is(ctx, "tiledb_ctx")) {
    stop("argument ctx must be a tiledb_ctx")
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }

  array_xptr <- libtiledb_array(ctx@ptr, uri, query_type)
  schema_xptr <- libtiledb_array_get_schema(array_xptr)
  if (!libtiledb_array_schema_sparse(schema_xptr)) {
    libtiledb_array_close(array_xptr)
    stop("array URI must be a sparse array")
  }
  array_xptr <- libtiledb_array_close(array_xptr)
  new("tiledb_sparse", ctx = ctx, uri = uri, ptr = array_xptr)
}


setMethod("schema", "tiledb_sparse", function(object, ...) {
  ctx <- object@ctx
  uri <- object@uri
  schema_xptr <- libtiledb_array_schema_load(ctx@ptr, uri)
  return(tiledb_array_schema.from_ptr(schema_xptr))
})

sparse_attribute_buffers <- function(array, sch, dom, sub) {
  stopifnot(is(sch, "tiledb_array_schema"))
  stopifnot(is(dom, "tiledb_domain"))
  attrs <- list()
  # first alloc coordinate buffer
  ncells <- libtiledb_array_max_buffer_elements(array@ptr, sub, libtiledb_coords())
  if (is.integral(dom)) {
    attrs[["coords"]] <- integer(length = ncells)
  } else {
    attrs[["coords"]]  <- numeric(length = ncells)
  }
  # for every attribute, compute the number of cells and allocate vectors
  for(attr in tiledb::attrs(sch)) {
    aname <- tiledb::name(attr)
    type <- tiledb_datatype_R_type(tiledb::datatype(attr))
    ncells <- libtiledb_array_max_buffer_elements(array@ptr, sub, aname)
    buff <- vector(mode = type, length = ncells)
    attrs[[aname]] <- buff
  }
  return(attrs)
}

setMethod("[", "tiledb_sparse",
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
                if (is.integral(dom)) {
                  subarray <- as.integer(subarray)
                } else {
                  subarray <- as.double(subarray)
                }
                buffers <- sparse_attribute_buffers(x, schema, dom, subarray)
                qry <- libtiledb_query(ctx@ptr, x@ptr, "READ")
                qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
                qry <- libtiledb_query_set_subarray(qry, subarray)
                attr_names <- names(buffers)
                for (idx in seq_along(buffers)) {
                  aname <- attr_names[[idx]]
                  if (aname == "coords") {
                    qry <- libtiledb_query_set_buffer(qry, libtiledb_coords(), buffers[[idx]])
                  } else {
                    qry <-  libtiledb_query_set_buffer(qry, aname, buffers[[idx]])
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
                    ncells <- libtiledb_query_result_buffer_elements(qry, libtiledb_coords())
                  } else {
                    ncells <- libtiledb_query_result_buffer_elements(qry, aname)
                  }
                  if (ncells < length(old_buffer)) {
                    buffers[[idx]] <- old_buffer[1:ncells]
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
            ctx <- x@ctx
            schema <- tiledb::schema(x)
            uri <- x@uri
            dom <- tiledb::domain(schema)
            attrs <- tiledb::attrs(schema)
            # check that we have the right number of index buffers
            if (length(coords) != tiledb_ndim(dom)) {
              stop(paste("number of coordinate vectors does not match the array domain"))
            }
            # check that all the indexing and value buffers are the same length
            coord_length <- length(index[[1]])
            for (i in seq_along(index)) {
              if (length(index[[i]]) != coord_length) {
                stop("invalid sparse coordinates, all coordinates must be the same length")
              }
            }
            # check that attributes are correct
            if (length(value) > length(attrs)) {
              stop("number of values to assign does not match the number of array attributes")
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
            # zip the coordinates into a single buffer for sparse write
            if (integral_domain) {
              zip_coords <- libtiledb_zip_coords_integer(coords, coord_length)
            } else {
              zip_coords <- libtiledb_zip_coords_numeric(coords, coord_length)
            }
            libtiledb_array_open(x@ptr, "WRITE")
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
            cat("tiledb_sparse(uri = \"", object@uri, "\")")
          })

#' @export
setMethod("is.sparse", "tiledb_sparse", function(object) TRUE)
