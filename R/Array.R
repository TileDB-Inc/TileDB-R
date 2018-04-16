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

DEBUG_INDEXING <- function(i, j, ...) {
  if (missing(i)) {
    print("DEBUG: i missing")
  } else  {
    print(cat("DEBUG i: ", i)) 
  }
  if (missing(j)) {
    print("DEBUG: j missing")
  } else {
    print(cat("DEBUG i: ", j)) 
  }
  if (missing(...)) {
    print("DEBUG: ... missing")
  } else {
    print(cat("DEBUG ...: ", list(...)))
  }
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
              DEBUG_INDEXING(i, j, ...)
              stop("indexing functionality not implemented") 
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
                stop("error in query write") 
              }
              return(x);
            } else {
              DEBUG_INDEXING(i, j, ...)
              stop("indexing functionality not implemented") 
            }
          })