
## rethink dense vs sparse

#' An S4 class for a TileDB Array
#'
#' This class is experimental.
#'
#' @slot ctx A TileDB context object
#' @slot uri A character despription
#' @slot is.sparse A logical value
#' @slot as.data.frame A logical value
#' @slot attrs A character vector
#' @slot extended A logical value
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_array
setClass("tiledb_array",
         slots = list(ctx = "tiledb_ctx",
                      uri = "character",
                      is.sparse = "logical",
                      as.data.frame = "logical",
                      attrs = "character",
                      extended = "logical",
                      ptr = "externalptr"))

#' Constructs a tiledb_array object backed by a persisted tiledb array uri
#'
#' tiledb_array returns a new object. This class is experimental.
#'
#' @param uri uri path to the tiledb dense array
#' @param query_type optionally loads the array in "READ" or "WRITE" only modes.
#' @param is.sparse optional logical switch, defaults to "NA" letting array determine it
#' @param as.data.frame optional logical switch, defaults to "FALSE"
#' @param attrs optional character vector to select attributes, default is
#' empty implying all are selected
#' @param extended optional logical switch selecting wide \sQuote{data.frame}
#' format, defaults to "TRUE"
#' @param ctx tiledb_ctx (optional)
#' @return tiledb_sparse array object
#' @export
tiledb_array <- function(uri,
                        query_type = c("READ", "WRITE"),
                        is.sparse = NA,
                        as.data.frame = FALSE,
                        attrs = character(),
                        extended = TRUE,
                        ctx = tiledb_get_context()) {
  query_type = match.arg(query_type)
  if (!is(ctx, "tiledb_ctx"))
    stop("argument ctx must be a tiledb_ctx", call. = FALSE)
  if (missing(uri) || !is.scalar(uri, "character"))
    stop("argument uri must be a string scalar", call.=FALSE)

  array_xptr <- libtiledb_array_open(ctx@ptr, uri, query_type)
  schema_xptr <- libtiledb_array_get_schema(array_xptr)
  is_sparse_status <- libtiledb_array_schema_sparse(schema_xptr)
  if (!is.na(is.sparse) && is.sparse != is_sparse_status) {
    libtiledb_array_close(array_xptr)
    stop("sparse array selected but dense array found", call. = FALSE)
  }
  is.sparse <- is_sparse_status
  array_xptr <- libtiledb_array_close(array_xptr)
  new("tiledb_array",
      ctx = ctx,
      uri = uri,
      is.sparse = is.sparse,
      as.data.frame = as.data.frame,
      attrs = attrs,
      extended = extended,
      ptr = array_xptr)
}

#' Return a schema from a tiledb_array object
#'
#' @param object sparse array object
#' @param ... Extra parameter for function signature, currently unused
#' @return The scheme for the object
setMethod("schema", "tiledb_array", function(object, ...) {
  ctx <- object@ctx
  uri <- object@uri
  schema_xptr <- libtiledb_array_schema_load(ctx@ptr, uri)
  return(tiledb_array_schema.from_ptr(schema_xptr))
})

setMethod("show",
          signature = "tiledb_array",
          definition = function (object) {
  cat("tiledb_array\n"
     ,"  uri           = '", object@uri, "'\n"
     ,"  is.sparse     = ", if (object@is.sparse) "TRUE" else "FALSE", "\n"
     ,"  as.data.frame = ", if (object@as.data.frame) "TRUE" else "FALSE", "\n"
     ,"  attrs         = ", if (length(object@attrs) == 0) "(none)"
                            else paste(objects@attrs, sep=","), "\n"
     ,"  extended      = ", if (object@extended) "TRUE" else "FALSE", "\n"
    , sep="")
})


#' Gets a tiledb_array value
#'
#' This function is incomplete.
#'
#' @param x tiledb_array object
#' @param i row index expression
#' @param j column index expression
#' @param ... Extra parameter for method signature
#' @param drop Optional logical switch to drop dimensions, default FALSE, currently unused.
#' @return An element from the sparse array
setMethod("[", "tiledb_array",
          function(x, i, j, ..., drop = FALSE) {
  if (missing(i)) i <- NULL
  if (missing(j)) j <- NULL
  ##if (missing(...)) ... <- NULL

  #cat("\ni:\n"); print(str(i))
  #cat("\nj:\n"); print(str(j))
  #cat("\n...:\n"); print(str(...))
  #print(...length())

  ctx <- x@ctx
  uri <- x@uri
  sel <- x@attrs
  sch <- tiledb::schema(x)
  dom <- tiledb::domain(sch)

  ## d1r <- vector(mode="numeric", length=3)
  ## d2r <- vector(mode="integer", length=3)
  ## ar <- vector(mode="integer", length=3)
  ## tiledb_array_open(x, "READ")
  ## qry <- tiledb:::libtiledb_query(x@ctx@ptr, x@ptr, "READ")
  ## qry <- tiledb:::libtiledb_query_set_buffer(qry, "d1", d1r)
  ## qry <- tiledb:::libtiledb_query_set_buffer(qry, "d2", d2r)
  ## qry <- tiledb:::libtiledb_query_set_buffer(qry, "a", ar)
  ## qry <- tiledb:::libtiledb_query_set_layout(qry, "UNORDERED")
  ## qry <- tiledb:::libtiledb_query_add_range(qry, 0, 1.15, 1.35)
  ## qry <- tiledb:::libtiledb_query_add_range(qry, 1, 2L, 4L)
  ## qry <- tiledb:::libtiledb_query_submit(qry)


  invisible(NULL)
})
