
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
                            else paste(object@attrs, sep=","), "\n"
     ,"  extended      = ", if (object@extended) "TRUE" else "FALSE", "\n"
    , sep="")
})


#' Returns a TileDB array, allowing for specific subset ranges.
#'
#' Heterogenous domains are supported, including timestamps and characters.
#'
#' This function may still still change; the current implementation should be
#' considered as an initial draft.
#' @param x tiledb_array object
#' @param i optional row index expression which can be a list in which case minimum and maximum
#' of each list element determine a range; multiple list elements can be used to supply multiple
#' ranges.
#' @param j optional column index expression which can be a list in which case minimum and maximum
#' of each list element determine a range; multiple list elements can be used to supply multiple
#' ranges.
#' @param ... Extra parameters for method signature, currently unused.
#' @param drop Optional logical switch to drop dimensions, default FALSE, currently unused.
#' @return An element from the sparse array
#' @import nanotime
#' @aliases [,tiledb_array
#' @aliases [,tiledb_array-method
#' @aliases [,tiledb_array,ANY,tiledb_array-method
#' @aliases [,tiledb_array,ANY,ANY,tiledb_array-method
setMethod("[", "tiledb_array",
          function(x, i, j, ..., drop = FALSE) {
  ## add defaults
  if (missing(i)) i <- NULL
  if (missing(j)) j <- NULL

  ## keep unevaluated substitute expressions, creates a language object we can subset
  is <- substitute(i)
  js <- substitute(j)

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
  dimvarnum <- sapply(dims, function(d) libtiledb_dim_get_cell_val_num(d@ptr))

  attrs <- tiledb::attrs(schema(x))
  attrnames <- unname(sapply(attrs, function(a) libtiledb_attribute_get_name(a@ptr)))
  attrtypes <- unname(sapply(attrs, function(a) libtiledb_attribute_get_type(a@ptr)))
  attrvarnum <- unname(sapply(attrs, function(a) libtiledb_attribute_get_cell_val_num(a@ptr)))

  allnames <- c(dimnames, attrnames)
  alltypes <- c(dimtypes, attrtypes)
  allvarnum <- c(dimvarnum, attrvarnum)

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


  ## open query
  qryptr <- libtiledb_query(ctx@ptr, arrptr, "READ")

  ## set range(s) on first dimension
  if (is.null(is)) {
    qryptr <- libtiledb_query_add_range_with_type(qryptr, 0, dimtypes[1],
                                                  nonemptydom[[1]][1], nonemptydom[[1]][2])
  } else {
    if (!identical(eval(is[[1]]),list)) stop("The row argument must be a list.")
    if (length(is) == 1) stop("No content to parse in row argument.")
    for (i in 2:length(is)) {
      el <- is[[i]]
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 0, dimtypes[1],
                                                    min(eval(el)), max(eval(el)))
    }
  }

  ## set range(s) on  second dimension
  if (is.null(js)) {
    qryptr <- libtiledb_query_add_range_with_type(qryptr, 1, dimtypes[2],
                                                  nonemptydom[[2]][1], nonemptydom[[2]][2])
  } else {
    if (!identical(eval(js[[1]]),list)) stop("The col argument must be a list.")
    if (length(js) == 1) stop("No content to parse in col argument.")
    for (i in 2:length(js)) {
      el <- js[[i]]
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 1, dimtypes[2],
                                                    min(eval(el)), max(eval(el)))
    }
  }

  ## retrieve est_result_size
  getEstimatedSize <- function(name, varnum, qryptr) {
    if (is.na(varnum))
      libtiledb_query_get_est_result_size_var(qryptr, name)[1]
    else
      libtiledb_query_get_est_result_size(qryptr, name)
  }
  ressizes <- mapply(getEstimatedSize, allnames, allvarnum,
                     MoreArgs=list(qryptr=qryptr), SIMPLIFY=TRUE)
  resrv <- max(ressizes)

  ## allocate and set buffers
  getBuffer <- function(name, type, varnum, resrv, qryptr, arrptr) {
    if (is.na(varnum)) {
      buf <- libtiledb_query_buffer_var_char_alloc_direct(resrv, resrv*8)
      qryptr <- libtiledb_query_set_buffer_var_char(qryptr, name, buf)
      buf
    } else {
      buf <- libtiledb_query_buffer_alloc_ptr(arrptr, type, resrv)
      qryptr <- libtiledb_query_set_buffer_ptr(qryptr, name, buf)
      buf
    }
  }
  buflist <- mapply(getBuffer, allnames, alltypes, allvarnum,
                    MoreArgs=list(resrv=resrv, qryptr=qryptr, arrptr=arrptr),
                    SIMPLIFY=FALSE)


  ## fire off query and close array
  qryptr <- libtiledb_query_submit(qryptr)
  libtiledb_array_close(arrptr)

  ## retrieve actual result size (from fixed size element columns)
  getResultSize <- function(name, varnum, qryptr) {
    if (is.na(varnum))                  # symbols come up with higher count
      varnum
    else
      libtiledb_query_result_buffer_elements(qryptr, name)
  }
  estsz <- mapply(getResultSize, allnames, allvarnum, MoreArgs=list(qryptr=qryptr), SIMPLIFY=TRUE)
  resrv <- max(estsz, na.rm=TRUE)

  ## get results
  getResult <- function(buf, name, varnum, resrv, qryptr) {
    if (is.na(varnum)) {
      sz <- libtiledb_query_result_buffer_elements(qryptr, name)
      libtiledb_query_get_buffer_var_char(buf, resrv, sz)[,1]
    } else {
      libtiledb_query_get_buffer_ptr(buf)
    }
  }
  reslist <- mapply(getResult, buflist, allnames, allvarnum,
                    MoreArgs=list(resrv=resrv, qryptr=qryptr), SIMPLIFY=FALSE)

  ## convert list into data.frame (cheaply) and subset
  res <- data.frame(reslist)[1:resrv,]
  colnames(res) <- allnames

  invisible(res)
})


#' Sets a tiledb array value or value range
#'
#' This function assigns a right-hand side object, typically a data.frame or
#' something that can be coerced to a data.frame, to a tiledb array.
#'
#' For sparse matrices, row and column indices can either be supplied
#' as part of the left-hand side object, or as part of the data.frame
#' provided approrpiate column names.
#'
#' This function may still still change; the current implementation should be
#' considered as an initial draft.
#' @param x sparse or dense TileDB array object
#' @param i parameter row index
#' @param j parameter column index
#' @param ... Extra parameter for method signature, currently unused.
#' @param value The value being assigned
#' @return The modified object
#' @examples
#' \dontrun{
#' uri <- "quickstart_sparse"      ## as created by the other example
#' arr <- tiledb_array(uri)        ## open array
#' df <- arr[]                     ## read current content
#' ## First approach: matching data.frame with appriate row and column
#' newdf <- data.frame(rows=c(1,2,2), cols=c(1,3,4), a=df$a+100)
#' ## Second approach: supply indices explicitly
#' arr[c(1,2), c(1,3)] <- c(42,43) ## two values
#' arr[2, 4] <- 88                 ## or just one
#' }
#' @aliases [<-,tiledb_array
#' @aliases [<-,tiledb_array-method
#' @aliases [<-,tiledb_array,ANY,tiledb_array-method
#' @aliases [<-,tiledb_array,ANY,ANY,tiledb_array-method
setMethod("[<-", "tiledb_array",
          function(x, i, j, ..., value) {
  if (!is.data.frame(value)) {
    value <- as.data.frame(value)
  }

  ## add defaults
  if (missing(i)) i <- NULL
  if (missing(j)) j <- NULL

  ctx <- x@ctx
  uri <- x@uri
  sel <- x@attrs
  sch <- tiledb::schema(x)
  dom <- tiledb::domain(sch)

  sparse <- libtiledb_array_schema_sparse(sch@ptr)

  dims <- tiledb::dimensions(dom)
  dimnames <- sapply(dims, function(d) libtiledb_dim_get_name(d@ptr))
  dimtypes <- sapply(dims, function(d) libtiledb_dim_get_datatype(d@ptr))
  dimvarnum <- sapply(dims, function(d) libtiledb_dim_get_cell_val_num(d@ptr))

  attrs <- tiledb::attrs(schema(x))
  attrnames <- unname(sapply(attrs, function(a) libtiledb_attribute_get_name(a@ptr)))
  attrtypes <- unname(sapply(attrs, function(a) libtiledb_attribute_get_type(a@ptr)))
  attrvarnum <- unname(sapply(attrs, function(a) libtiledb_attribute_get_cell_val_num(a@ptr)))

  allnames <- c(dimnames, attrnames)
  alltypes <- c(dimtypes, attrtypes)
  allvarnum <- c(dimvarnum, attrvarnum)

  ## we will recognize two standard cases
  ##  1) arr[]    <- value    where value contains two columns with the dimnames
  ##  2) arr[i,j] <- value    where contains just the attrnames
  ## There is more to do here but it is a start

  ## Case 1
  if (length(colnames(value)) == length(allnames)) {
    ## same length is good
    if (length(intersect(colnames(value), allnames)) == length(allnames)) {
      ## all good, proceed
      #message("Yay all columns found")
    } else {
      stop("Assigned data.frame does not contain all required attribute and dimension columns.")
    }
  }

  ## Case 2
  if (length(colnames(value)) == length(attrnames)) {
    if (is.null(i)) stop("For sparse arrays a row index has to be supplied.")
    if (is.null(j)) stop("For sparse arrays a column index has to be supplied.")
    #if (length(i) != nrow(value)) stop("Row index must have same number of observations as data")
    if (length(j) == 1) j <- rep(j, nrow(value))
    if (length(colnames(value)) == 1 && colnames(value) == "value") colnames(value) <- attrnames
    newvalue <- data.frame(i, j)
    colnames(newvalue) <- dimnames
    value <- cbind(newvalue, value)
  }

  nc <- ncol(value)
  nr <- nrow(value)
  if (all.equal(sort(allnames),sort(colnames(value)))) {
    arrptr <- libtiledb_array_open(ctx@ptr, uri, "WRITE")
    qryptr <- libtiledb_query(ctx@ptr, arrptr, "WRITE")
    qryptr <- libtiledb_query_set_layout(qryptr, "UNORDERED")

    buflist <- vector(mode="list", length=nc)
    for (i in 1:nc) {
      buflist[[i]] <- libtiledb_query_buffer_alloc_ptr(arrptr, alltypes[i], nr)
      buflist[[i]] <- libtiledb_query_buffer_assign_ptr(buflist[[i]], alltypes[i], value[[i]])
      qryptr <- libtiledb_query_set_buffer_ptr(qryptr, allnames[i], buflist[[i]])
    }

    qryptr <- libtiledb_query_submit(qryptr)
    libtiledb_array_close(arrptr)

  }
  invisible(x)
})
