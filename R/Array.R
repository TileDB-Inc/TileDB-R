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
            print(object@schema)
            invisible(object)
          })

setMethod("[", "Array",
          function(x, i, j, value) {
            ctx <- x@ctx
            uri <- x@uri
            if (missing(i) && missing(j)) {
              result <- array(0, c(100, 100))  
              qry <- tiledb_query(ctx@ptr, uri, "READ")
              qry <- tiledb_query_set_layout(qry, "COL_MAJOR")
              qry <- tiledb_query_set_buffer(qry, "foo", result)
              tiledb_query_submit(qry)
              return(result);
            } else {
              stop("indexing functionality not implemented") 
            }           
          })

setMethod("[<-", "Array",
          function(x, i, j, value) {
            ctx <- x@ctx
            uri <- x@uri
            if (missing(i) && missing(j)) {
              qry <- tiledb_query(ctx@ptr, uri, "WRITE") 
              qry <- tiledb_query_set_layout(qry, "COL_MAJOR")
              qry <- tiledb_query_set_buffer(qry, "foo", value)
              #qry <- tiledb_query_set_subarray(qry, c(1, 100, 1, 100))
              tiledb_query_submit(qry)
              return();
            } else {
              stop("indexing functionality not implemented") 
            }
          })

