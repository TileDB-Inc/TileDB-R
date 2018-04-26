#' @exportClass Array
setClass("Array",
         slots = list(ctx = "Ctx", schema = "ArraySchema", uri = "character"))

#' @export
Array <- function(ctx, schema, uri) {
  if (missing(ctx) || !is(ctx, "Ctx"))  {
    stop("argument ctx must be a tiledb::Ctx") 
  } else if (missing(ctx) || !is(schema, "ArraySchema")) {
    stop("argument schema must a tiledb::ArraySchema") 
  } else if (missing(uri) || !is.scalar(uri, "character")) {
    stop("argument uri must be a string scalar")
  }
  uri <- tiledb_array_create(schema@ptr, uri) 
  new("Array", ctx = ctx, schema = schema, uri = uri) 
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
  new("Array", ctx = ctx, schema = schema, uri = uri)
}

setMethod("show", "Array",
          function (object) {
            cat("tiledb::Array object @ ", object@uri, "\n")
            invisible(print(object[]))
          })

domain_subarray <- function(dom, i, j, ...) {
  stopifnot(is(dom, "Domain"))
  if(missing(i)) {
    i <- integer(0)
  }
  if(missing(j)) {
    j <- integer(0)
  }
  indices <- list(i, j, ...)
  indices <- indices[sapply(indices, length) > 0]
  stopifnot(is.list(indices))
  if (any(is.na(unlist(indices)))) {
    stop("NAs are not allowed in element coordinates.")
  }
  if(any(unlist(indices) < 1L)) {
    stop("Elements of parameter elem must be greater or equal than one.")
  }
  if (length(indices) > length(dim(dom))) {
    stop("incorrect number of dimensions")
  }
  # Calculate contiguous index selections
  startidx <- lapply(indices, 
                     function(x) {
                      if(length(x) > 1) 
                        which(c(TRUE, diff(x) > 1)) 
                      else 
                        1
                     })
  #print("DEBUG: startidx: ")
  #print(startidx)
  count <- lapply(1:length(startidx), function(i) 
    diff(c(startidx[[i]], length(indices[[i]]) + 1)))
  
  #print("DEBUG: count: ")
  #print(count)
  
  countsum <- sapply(count, sum)
  #print("DEBUG: countsum: ")
  #print(countsum) 
  
  startidx <- as.matrix(expand.grid(startidx))
  #print("DEBUG: startidx: ")
  #print(startidx)
  
  count <- as.matrix(expand.grid(count))
  #print("DEBUG: count: ")
  #print(count) 
  offset <- do.call(cbind, lapply(1:length(indices), 
                                  function(i) indices[[i]][startidx[, i]]))
  if (length(dim(offset)[[1]]) != 1) {
    stop("non-contiguous indices not supported") 
  }
  #print("DEBUG: offset: ")
  #print(offset)
 
  ndim <- tiledb::ndim(dom)
  dims <- dim(dom) 
  
  #print("Debug: dims: ")
  #print(dims)
  
  # inclusive indexing
  sub <- integer(ndim * 2)
  for (i in 1L:ndim) {
    start <- offset[[i]]
    end <- start + (count[[i]] - 1)
    idx <- (i - 1L) * 2L + 1L
    sub[idx] <- start
    sub[idx + 1L] <- end
  }
  #print("Debug: subarray: ")
  #print(sub)
  return(sub)
}

setMethod("[", "Array",
          function(x, i, j, ...) {
            ctx <- x@ctx
            schema <- x@schema
            uri <- x@uri
            if (missing(i) && missing(j)) {
              result <- array(0, dim(schema))
              attr_name <- name(attrs(schema)[[1L]])
              qry <- tiledb_query(ctx@ptr, uri, "READ")
              qry <- tiledb_query_set_layout(qry, "COL_MAJOR")
              qry <- tiledb_query_set_buffer(qry, attr_name, result)
              qry <- tiledb_query_submit(qry)
              if (tiledb_query_status(qry) != "COMPLETE") {
                stop("error in read query")
              }
              return(result);
            } else {
              dom <- domain(schema)
              if (!tiledb::is.integral(dom)) {
                stop("subscript indexing only valid for integral Domain's")  
              }
              sub <- domain_subarray(dom, i, j, ...)
              nd  <- tiledb::ndim(dom) 
              shp <- integer(nd)
              for (i in 1L:nd) {
                idx <- (i - 1L) * 2L + 1L
                shp[i] <- sub[idx + 1] - sub[idx] + 1L
              }
              if (all(shp == 1L)) {
                # shape a scalar
                result <- c(NA_real_)
              } else {
                # shape indicates an ND array 
                result <- array(NA_real_ , shp)
              }
              attr_name <- name(attrs(schema)[[1L]])
              qry <- tiledb_query(ctx@ptr, uri, "READ")
              qry <- tiledb_query_set_layout(qry, "COL_MAJOR")
              qry <- tiledb_query_set_subarray(qry, as.integer(sub))
              qry <- tiledb_query_set_buffer(qry, attr_name, result)
              qry <- tiledb_query_submit(qry)
              if (tiledb_query_status(qry) != "COMPLETE") {
                stop("error in read query") 
              }
              return(result)
            }           
          })

setMethod("[<-", "Array",
          function(x, i, j, ..., value) {
            ctx <- x@ctx
            schema <- x@schema
            uri <- x@uri
            if (missing(i) && missing(j)) {
              attr_name <- name(attrs(schema)[[1L]])
              qry <- tiledb_query(ctx@ptr, uri, "WRITE") 
              qry <- tiledb_query_set_layout(qry, "COL_MAJOR")
              qry <- tiledb_query_set_buffer(qry, attr_name, value)
              qry <- tiledb_query_submit(qry)
              if (tiledb_query_status(qry) != "COMPLETE") {
                stop("error in write query") 
              }
              return(x);
            } else {
              dom <- tiledb::domain(schema)
              if (!tiledb::is.integral(dom)) {
                stop("subscript indexing only valid for integral Domain's")  
              }
              sub <- domain_subarray(domain(schema), i, j, ...)
              stop("indexing functionality not implemented") 
            }
          })

as.array.Array <- function(x, ...) {
 return(x[]) 
}