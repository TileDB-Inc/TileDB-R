
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
#' @import nanotime
setMethod("[", "tiledb_array",
          function(x, i, j, ..., drop = FALSE) {
  ## add defaults
  if (missing(i)) i <- NULL
  if (missing(j)) j <- NULL

  ## keep unevaluated substitute expressions
  is <- substitute(i)
  js <- substitute(j)
  #print(is); print(is.null(is)); print(is[[2]])
  #print(js); print(is.null(js)); print(js[[2]])
  ## -- evaluates first, not useful  print(str(enquote(i)))
  ## -- creates char, not useful     print(str(deparse(substitute(i))))

  #cat("\ni:\n"); print(str(i))
  #cat("\nj:\n"); print(str(j))
  #cat("\n...:\n"); print(str(...))
  #print(...length())

  ctx <- x@ctx
  uri <- x@uri
  sel <- x@attrs
  sch <- tiledb::schema(x)
  dom <- tiledb::domain(sch)

  libtiledb_array_open_with_ptr(x@ptr, "READ")
  on.exit(libtiledb_array_close(x@ptr))

  dims <- tiledb::dimensions(dom)
  dimnames <- sapply(dims, function(d) libtiledb_dim_get_name(d@ptr))
  dimtypes <- sapply(dims, function(d) libtiledb_dim_get_datatype(d@ptr))

  attrs <- tiledb::attrs(schema(x))
  attrnames <- unname(sapply(attrs, function(a) libtiledb_attribute_get_name(a@ptr)))
  attrtypes <- unname(sapply(attrs, function(a) libtiledb_attribute_get_type(a@ptr)))
  #print(c(dimnames, attrnames))
  #print(c(dimtypes, attrtypes))

  arrptr <- libtiledb_array_open(ctx@ptr, uri, "READ")

  ## helper function to sweep over names and types of domain
  getDomain <- function(nm, tp) {
    if (tp %in% c("ASCII", "CHAR")) {
      libtiledb_array_get_non_empty_domain_var_from_name(arrptr, nm)
    } else {
      libtiledb_array_get_non_empty_domain_from_name(arrptr, nm, tp)
    }
  }
  nonemptydom <- mapply(getDomain, dimnames, dimtypes, SIMPLIFY=FALSE)
  #print(str(nonemptydom))

  symbol_range <- nonemptydom$symbol
  time_range <- nonemptydom$timestamp

  #symbol_range <- c("A", "ZZZ")
  #symbol_range <- c("TSLA", "TSLA")
  #time_range <- c(nanotime("2020-01-30T08:30:00.000000000+00:00"),
  #                nanotime("2020-01-30T09:00:00.000000000+00:00"))
  #time_range <- c(nanotime("2020-01-30T00:00:00.000000000+00:00"),
  #                nanotime("2020-01-30T23:59:59.999999999+00:00"))


  qryptr <- libtiledb_query(ctx@ptr, arrptr, "READ")

  ## first dimension
  if (is.null(is)) {
    qryptr <- libtiledb_query_add_range_with_type(qryptr, 0, dimtypes[1], time_range[1], time_range[2])
  } else {
    if (!identical(eval(is[[1]]),list)) stop("The row argument must be a list.")
    if (length(is) == 1) stop("No content to parse in row argument.")
    for (i in 2:length(is)) {
      el <- is[[i]]
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 0, dimtypes[1], min(eval(el)), max(eval(el)))
    }
  }

  ## second dimension
  if (is.null(js)) {
    qryptr <- libtiledb_query_add_range(qryptr, 1, symbol_range[1], symbol_range[2])
  } else {
    if (!identical(eval(js[[1]]),list)) stop("The col argument must be a list.")
    if (length(js) == 1) stop("No content to parse in col argument.")
    for (i in 2:length(js)) {
      el <- js[[i]]
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 1, dimtypes[2], min(eval(el)), max(eval(el)))
    }
  }

  tsm <- libtiledb_query_get_est_result_size(qryptr, "timestamp")
  symm <- libtiledb_query_get_est_result_size_var(qryptr, "symbol")
  shm <- libtiledb_query_get_est_result_size(qryptr, "shares")
  prm <- libtiledb_query_get_est_result_size(qryptr, "price")
  resrv <- max(tsm, shm, prm)

  tsbuf <- libtiledb_query_buffer_alloc_ptr(arrptr, dimtypes[1], resrv)
  qryptr <- libtiledb_query_set_buffer_ptr(qryptr, dimnames[1], tsbuf)

  symbuf <- libtiledb_query_buffer_var_char_alloc_direct(resrv, resrv*8)
  qryptr <- libtiledb_query_set_buffer_var_char(qryptr, dimnames[2], symbuf)

  shbuf <- libtiledb_query_buffer_alloc_ptr(arrptr, attrtypes[1], resrv)
  qryptr <- libtiledb_query_set_buffer_ptr(qryptr, attrnames[1], shbuf)

  prbuf <- libtiledb_query_buffer_alloc_ptr(arrptr, attrtypes[2], resrv)
  qryptr <- libtiledb_query_set_buffer_ptr(qryptr, attrnames[2], prbuf)

  qryptr <- libtiledb_query_submit(qryptr)
  libtiledb_array_close(arrptr)

  tsm <- libtiledb_query_result_buffer_elements(qryptr, "timestamp")
  symm <- libtiledb_query_result_buffer_elements(qryptr, "symbol")
  shm <- libtiledb_query_result_buffer_elements(qryptr, "shares")
  prm <- libtiledb_query_result_buffer_elements(qryptr, "price")
  resrv <- max(tsm, shm, prm)

  res <- data.frame(times = libtiledb_query_get_buffer_ptr(tsbuf),
                    symbols = libtiledb_query_get_buffer_var_char(symbuf, resrv, symm)[,1],
                    shares = libtiledb_query_get_buffer_ptr(shbuf),
                    prices = libtiledb_query_get_buffer_ptr(prbuf))[1:resrv,]


  #print(libtiledb_query_get_buffer_var_char_simple(symbuf))
  #print(nsym <- libtiledb_query_result_buffer_elements(qryptr, "symbol"))
  #print(syms <- libtiledb_query_get_buffer_var_char_sized(symbuf, resrv))
#  print(syms <- libtiledb_query_get_buffer_var_char(symbuf))

  ## by dim object name
  ## for (i in seq_along(dimnames)) {
  ##   #cat(sprintf("%d %s %s\n", i, dimnames[i], dimtypes[i]))
  ##   if (dimtypes[i] %in% c("ASCII", "CHAR")) {
  ##     print(libtiledb_array_get_non_empty_domain_var_from_name(arrptr, dimnames[i]))
  ##   } else {
  ##     #print(format(libtiledb_array_non_empty_domain_from_index(arrptr, i-1, dimtypes[i])))
  ##     print(libtiledb_array_non_empty_domain_from_name(arrptr, dimnames[i], dimtypes[i]))
  ##   }
  ## }

  invisible(res)
})
