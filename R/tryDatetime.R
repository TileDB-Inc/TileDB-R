
try_datetime <- function(uri) {
  ##cat("Looking at ", uri, "\n")

  index <- list()
  x <- tiledb_sparse(uri)

  ctx <- x@ctx
  uri <- x@uri
  schema <- tiledb::schema(x)
  dom <- tiledb::domain(schema)
  ## we need the data type of the domain (as a string)
  domaintype <- libtiledb_domain_get_type(dom@ptr)
  if (!tiledb::is.integral(dom)) {
    stop("subscript indexing only valid for integral Domain's")
  }
  libtiledb_array_open_with_ptr(x@ptr, "READ")

  subarray <- domain_subarray(dom, index = index)
  if (is.integral(dom)) {
    subarray <- as.integer(subarray)
  } else {
    subarray <- as.double(subarray)
  }

  ## -- replacing sparse_attribute_buffers
  local_sparse_attribute_buffers <- function(array, sch, dom, sub, filter_attributes=list()) {
    stopifnot(is(sch, "tiledb_array_schema"))
    stopifnot(is(dom, "tiledb_domain"))
    attributes <- list()
    ## first alloc coordinate buffer

    ## FIXME: libtiledb_array_max_buffer_elements
    ## ncells <- libtiledb_array_max_buffer_elements(array@ptr, sub, libtiledb_coords())
    ## ncells <- max(sub) - min(sub) + 1
    #ncells <- libtiledb_array_max_buffer_elements_test(array@ptr, sch@ptr,
    #                                                   dom@ptr, sub, libtiledb_coords())
    ncells <- libtiledb_array_max_buffer_elements_with_type(array@ptr, sub,
                                                            libtiledb_coords(), domaintype)
    #if (is.integral(dom) && !grepl("^DATETIME", domaintype)) {
    #  attributes[["coords"]] <- integer(length = ncells)*8
    #} else {
    #  attributes[["coords"]]  <- numeric(length = ncells)*8
    #}
    attributes[["coords"]] <- libtiledb_query_buffer_alloc_ptr(array@ptr, domaintype, ncells)
    attrs <- tiledb::attrs(sch)
    if (length(filter_attributes) > 0) {
      attrs <- Filter(function(a) is.element(name(a), filter_attributes), attrs)
    }
    ## for every attribute, compute the number of cells and allocate vectors
    for(attr in attrs) {
      aname <- tiledb::name(attr)
      type <- tiledb_datatype_R_type(tiledb::datatype(attr))
      ## FIXME ncells <- libtiledb_array_max_buffer_elements(array@ptr, sub, aname)
      ##       ncells <- max(sub) - min(sub) + 1
      #ncells <- libtiledb_array_max_buffer_elements_test(array@ptr, sch@ptr, dom@ptr, sub, aname)
      ncells <- libtiledb_array_max_buffer_elements_with_type(array@ptr, sub,
                                                              aname, domaintype)
      #buff <- vector(mode = type, length = ncells*8)
      buff <- libtiledb_query_buffer_alloc_ptr(array@ptr, tiledb::datatype(attr), ncells)
      attributes[[aname]] <- buff
      #cat("For buffer '", aname, "' set up size of '", ncells, "' of type '", type, "'\n", sep="")
    }
    return(attributes)
  }
  #buffers <- sparse_attribute_buffers(x, schema, dom, subarray)
  buffers <- local_sparse_attribute_buffers(x, schema, dom, subarray)
  qry <- libtiledb_query(ctx@ptr, x@ptr, "READ")
  qry <- libtiledb_query_set_layout(qry, "COL_MAJOR")
  #FIXME  qry <- libtiledb_query_set_subarray(qry, subarray) -- now '_with_type'
  qry <- libtiledb_query_set_subarray_with_type(qry, subarray, domaintype)
  attr_names <- names(buffers)
  for (idx in seq_along(buffers)) {
    aname <- attr_names[[idx]]
    val <- buffers[[idx]]
    if (aname == "coords") {
      #qry <- libtiledb_query_set_buffer(qry, libtiledb_coords(), val)
      qry <- libtiledb_query_set_buffer_ptr(qry, libtiledb_coords(), val)
    } else {
      if (is.character(val) || is.list(val)) {
        stop("var length TODO")
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
  ## get the actual number of results, instead of realloc
  ## just modify the vector length so there is no additional copy
  for (idx in seq_along(attr_names)) {
    #old_buffer <- buffers[[idx]]
    old_buffer <- libtiledb_query_get_buffer_ptr(buffers[[idx]], TRUE, FALSE)
    #cat("Idx is ", idx, " and name is ", attr_names[idx], ". Retrieved buffer follows\n", sep="")
    #print(str(old_buffer))
    aname <- attr_names[[idx]]
    if (aname == "coords") {
      ncells <- libtiledb_query_result_buffer_elements(qry, libtiledb_coords())
    } else {
      ncells <- libtiledb_query_result_buffer_elements(qry, aname)
    }
    if (ncells <= length(old_buffer)) {
      buffers[[idx]] <- old_buffer[1:ncells]
    }
    #cat("Setting size to '", ncells, "' for '", aname, "', buffer follows\n", sep="")
    #print(buffers[[idx]])
  }
  #fixup_coord_buffer(buffers[[1]]);
  if (x@as.data.frame) {
    return(as_data_frame(dom, buffers))
  } else {
    ## if there is only one buffer, don't return a list of attribute buffers
    if (length(buffers) == 1L) {
      return(buffers[[1L]])
    }
    return(buffers)
  }
}

testDatetime <- function(uri="/tmp/tiledb/datetime-types/ms") {
  rl <- try_datetime(uri)
  res <- data.frame(time=rl$coords, a1=rl$a1)
}
