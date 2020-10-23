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


#' An S4 class for a TileDB Array
#'
#' This class aims to eventually replace \code{\link{tiledb_dense}}
#' and \code{\link{tiledb_sparse}} provided equivalent functionality
#' based on refactored implementation utilising newer TileDB features.
#'
#' @slot ctx A TileDB context object
#' @slot uri A character despription
#' @slot is.sparse A logical value
#' @slot as.data.frame A logical value
#' @slot attrs A character vector
#' @slot extended A logical value
#' @slot selected_ranges An optional list with matrices where each matrix i
#' describes the (min,max) pair of ranges for dimension i
#' @slot ptr External pointer to the underlying implementation
#' @exportClass tiledb_array
setClass("tiledb_array",
         slots = list(ctx = "tiledb_ctx",
                      uri = "character",
                      is.sparse = "logical",
                      as.data.frame = "logical",
                      attrs = "character",
                      extended = "logical",
                      selected_ranges = "list",
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
#' @param selected_ranges An optional list with matrices where each matrix i
#' describes the (min,max) pair of ranges for dimension i
#' @param ctx tiledb_ctx (optional)
#' @return tiledb_array object
#' @export
tiledb_array <- function(uri,
                        query_type = c("READ", "WRITE"),
                        is.sparse = NA,
                        as.data.frame = FALSE,
                        attrs = character(),
                        extended = TRUE,
                        selected_ranges = list(),
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
      selected_ranges = selected_ranges,
      ptr = array_xptr)
}

#' Return a schema from a tiledb_array object
#'
#' @param object tiledb array object
#' @param ... Extra parameter for function signature, currently unused
#' @return The scheme for the object
setMethod("schema", "tiledb_array", function(object, ...) {
  ctx <- object@ctx
  uri <- object@uri
  schema_xptr <- libtiledb_array_schema_load(ctx@ptr, uri)
  return(tiledb_array_schema.from_ptr(schema_xptr))
})

#' Prints a tiledb_array object
#'
#' @param object A tiledb array object
#' @export
setMethod("show", signature = "tiledb_array",
          definition = function (object) {
  cat("tiledb_array\n"
     ,"  uri             = '", object@uri, "'\n"
     ,"  is.sparse       = ", if (object@is.sparse) "TRUE" else "FALSE", "\n"
     ,"  as.data.frame   = ", if (object@as.data.frame) "TRUE" else "FALSE", "\n"
     ,"  attrs           = ", if (length(object@attrs) == 0) "(none)"
                               else paste(object@attrs, collapse=","), "\n"
     ,"  selected_ranges = ", if (length(object@selected_ranges) > 0) sprintf("(%d non-null sets)", sum(sapply(object@selected_ranges, function(x) !is.null(x))))
                               else "(none)", "\n"
     ,"  extended        = ", if (object@extended) "TRUE" else "FALSE"
     ,"\n"
     ,sep="")
})

setValidity("tiledb_array", function(object) {
  msg <- NULL
  valid <- TRUE

  if (!is(object@ctx, "tiledb_ctx")) {
    valid <- FALSE
    msg <- c(msg, "The 'ctx' slot does not contain a ctx object.")
  }

  if (!is.character(object@uri)) {
    valid <- FALSE
    msg <- c(msg, "The 'uri' slot does not contain a character value.")
  }

  if (!is.logical(object@is.sparse)) {
    valid <- FALSE
    msg <- c(msg, "The 'is.sparse' slot does not contain a logical value.")
  }

  if (!is.logical(object@as.data.frame)) {
    valid <- FALSE
    msg <- c(msg, "The 'as.data.frame' slot does not contain a logical value.")
  }

  if (!is.character(object@attrs)) {
    valid <- FALSE
    msg <- c(msg, "The 'attrs' slot does not contain a character vector.")
  }

  if (!is.logical(object@extended)) {
    valid <- FALSE
    msg <- c(msg, "The 'extended' slot does not contain a logical value.")
  }

  if (!is.list(object@selected_ranges)) {
    valid <- FALSE
    msg <- c(msg, "The 'selected_ranges' slot does not contain a list.")
  } else {
    for (i in (seq_len(length(object@selected_ranges)))) {
      if (!is.null(object@selected_ranges[[i]])) {
        if (length(dim(object@selected_ranges[[i]])) != 2) {
          valid <- FALSE
          msg <- c(msg, sprintf("Element '%d' of 'selected_ranges' is not 2-d.", i))
        }
        if (ncol(object@selected_ranges[[i]]) != 2) {
          valid <- FALSE
          msg <- c(msg, sprintf("Element '%d' of 'selected_ranges' is not two column.", i))
        }
      }
    }
  }

  if (!is(object@ptr, "externalptr")) {
    valid <- FALSE
    msg <- c(msg, "The 'ptr' slot does not contain an external pointer.")
  }

  if (valid) TRUE else msg

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

  if (length(x@attrs) != 0) {
    ind <- match(x@attrs, attrnames)
    if (length(ind) == 0) {
      stop("Only non-existing columns selected.", call.=FALSE)
    }
    attrnames <- attrnames[ind]
    attrtypes <- attrtypes[ind]
    attrvarnum <- attrvarnum[ind]
  }

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

  ## set default range(s) on first dimension if nothing is specified
  if (is.null(i) &&
      (length(x@selected_ranges) == 0 ||
       (length(x@selected_ranges) >= 1 && is.null(x@selected_ranges[[1]])))) {
    ## domain values can currently be eg (0,0) rather than a flag, so check explicitly
    #domdim <- domain(dimensions(dom)[[1]])
    if (nonemptydom[[1]][1] != nonemptydom[[1]][2]) # || nonemptydom[[1]][1] > domdim[1])
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 0, dimtypes[1],
                                                    nonemptydom[[1]][1], nonemptydom[[1]][2])
  }
  ## if we have is, use it
  if (!is.null(i)) {
    ##if (!identical(eval(is[[1]]),list)) stop("The row argument must be a list.")
    if (length(i) == 0) stop("No content to parse in row argument.")
    for (ii in 1:length(i)) {
      el <- i[[ii]]
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 0, dimtypes[1],
                                                    min(eval(el)), max(eval(el)))
    }
  }

  ## set range(s) on second dimension
  if (is.null(j) &&
      (length(x@selected_ranges) == 0 ||
       (length(x@selected_ranges) >= 2 && is.null(x@selected_ranges[[2]])))) {
    if (length(nonemptydom) == 2) {
      ## domain values can currently be eg (0,0) rather than a flag, so check explicitly
      #domdim <- domain(dimensions(dom)[[2]])
      if (nonemptydom[[2]][1] != nonemptydom[[2]][2]) # || nonemptydom[[2]][1] > domdim[1])
        if (nonemptydom[[2]][1] != nonemptydom[[2]][2])
          qryptr <- libtiledb_query_add_range_with_type(qryptr, 1, dimtypes[2],
                                                        nonemptydom[[2]][1], nonemptydom[[2]][2])
    }
  }
  ## if we have js, use it
  if (!is.null(j)) {
    #if (!identical(eval(js[[1]]),list)) stop("The col argument must be a list.")
    if (length(j) == 0) stop("No content to parse in col argument.")
    for (ii in 1:length(j)) {
      el <- j[[ii]]
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 1, dimtypes[2],
                                                    min(eval(el)), max(eval(el)))
    }
  }

  ## if ranges selected, use those
  for (k in seq_len(length(x@selected_ranges))) {
    if (!is.null(x@selected_ranges[[k]])) {
      m <- x@selected_ranges[[k]]
      for (i in seq_len(nrow(m))) {
        qryptr <- libtiledb_query_add_range_with_type(qryptr, k-1, dimtypes[k], m[i,1], m[i,2])
      }
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
  resrv <- max(1, ressizes) # ensure >0 for correct handling of zero-length outputs

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
  res <- data.frame(reslist)[seq_len(resrv),]
  colnames(res) <- allnames

  ## reduce output if extended is false
  if (!x@extended) {
    res <- res[, attrnames]
  }

  if (!x@as.data.frame) {
    res <- as.list(res)
  }

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
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
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
  if (!is.data.frame(value) && !(is.list(value) && length(value) > 1)) {
    value <- as.data.frame(value)
    if (nrow(value) == 0) {
      message("Cannot assign zero row objects to TileDB Array.")
      return(x)
    }
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
  ndims <- length(dims)
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
  ##  2) arr[i,j] <- value    where value contains just the attribute names
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
  if (sparse && length(colnames(value)) == length(attrnames)) { # FIXME: need to check for array or matrix arg?
    if (is.null(i)) stop("For arrays a row index has to be supplied.")
    if (is.null(j)) stop("For arrays a column index has to be supplied.")
    #if (length(i) != nrow(value)) stop("Row index must have same number of observations as data")
    if (length(j) == 1) j <- rep(j, nrow(value))
    ##if (length(colnames(value)) == 1 && colnames(value) == "value") colnames(value) <- attrnames
    colnames(value) <- attrnames
    newvalue <- data.frame(i, j)
    colnames(newvalue) <- dimnames
    value <- cbind(newvalue, value)
  }

  ## Case 3: dense, length attributes == 1, i and j NULL
  ##         e.g. the quickstart_dense example where the RHS may be a matrix or data.frame
  ##         also need to guard against data.frame object which already have 'rows' and 'cols'
  if (isFALSE(sparse) &&
      ##is.null(i) && is.null(j) &&
      length(attrnames) == 1) {
    d <- dim(value)
    if ((d[2] > 1) &&
        (inherits(value, "data.frame") || inherits(value, "matrix")) &&
        !any(grepl("rows", colnames(value))) &&
        !any(grepl("cols", colnames(value)))    ) {
      ## turn the 2-d RHS in 1-d and align the names for the test that follows
      ## in effect, we just rewrite the query for the user
      value <- data.frame(x=as.matrix(value)[seq(1, d[1]*d[2])])
      colnames(value) <- attrnames
      allnames <- attrnames
    }

  ## Case 4: dense, list on RHS e.g. the ex_1.R example
  } else if (isFALSE(sparse) &&
             ##is.null(i) && is.null(j) &&
             length(value) == length(attrnames)) {
    if (!inherits(value, "data.frame")) {
      nl <- length(value)
      for (i in seq_len(nl)) {
        d <- dim(value[[i]])
        value[[i]] <- as.matrix(value[[i]])[seq(1, d[1]*d[2])]
      }
    }
    names(value) <- attrnames
    allnames <- attrnames
    alltypes <- attrtypes
  }

  nc <- if (is.list(value)) length(value) else ncol(value)
  nm <- if (is.list(value)) names(value) else colnames(value)

  if (all.equal(sort(allnames),sort(nm))) {
    arrptr <- libtiledb_array_open(ctx@ptr, uri, "WRITE")
    qryptr <- libtiledb_query(ctx@ptr, arrptr, "WRITE")
    qryptr <- libtiledb_query_set_layout(qryptr,
                                         if(sparse) "UNORDERED" else "ROW_MAJOR")

    buflist <- vector(mode="list", length=nc)
    for (i in 1:nc) {
      if (alltypes[i] %in% c("CHAR", "ASCII")) { # variable length
        txtvec <- as.character(value[[i]])
        offsets <- c(0L, cumsum(nchar(txtvec[-length(txtvec)])))
        data <- paste(txtvec, collapse="")
        buflist[[i]] <- libtiledb_query_buffer_var_char_create(offsets, data)
        qryptr <- libtiledb_query_set_buffer_var_char(qryptr, allnames[i], buflist[[i]])
      } else {
        nr <- NROW(value[[i]])
        buflist[[i]] <- libtiledb_query_buffer_alloc_ptr(arrptr, alltypes[i], nr)
        buflist[[i]] <- libtiledb_query_buffer_assign_ptr(buflist[[i]], alltypes[i], value[[i]])
        qryptr <- libtiledb_query_set_buffer_ptr(qryptr, allnames[i], buflist[[i]])
      }
    }

    qryptr <- libtiledb_query_submit(qryptr)
    libtiledb_array_close(arrptr)

  }
  invisible(x)
})


## -- as.data.frame accessor (generic in DenseArray.R)

#' Retrieve data.frame return toggle
#'
#' A \code{tiledb_array} object can be returned as an array (or list of arrays),
#' or, if select, as a \code{data.frame}. This methods returns the selection value.
#' @param object A \code{tiledb_array} object
#' @return A logical value indicating whether \code{data.frame} return is selected
#' @export
setMethod("return.data.frame",
          signature = "tiledb_array",
          function(object) object@as.data.frame)


## -- as.data.frame setter (generic in DenseArray.R)

#' Set data.frame return toggle
#'
#' A \code{tiledb_array} object can be returned as an array (or list of arrays),
#' or, if select, as a \code{data.frame}. This methods sets the selection value.
#' @param x A \code{tiledb_array} object
#' @param value A logical value with the selection
#' @return The modified \code{tiledb_array} array object
#' @export
setReplaceMethod("return.data.frame",
                 signature = "tiledb_array",
                 function(x, value) {
  x@as.data.frame <- value
  validObject(x)
  x
})



## -- attrs (generic in Attributes.R and DenseArray.R)

#' Retrieve attributes from \code{tiledb_array} object
#'
#' By default, all attributes will be selected. But if a subset of attribute
#' names is assigned to the internal slot \code{attrs}, then only those attributes
#' will be queried.  This methods accesses the slot.
#' @param object A \code{tiledb_array} object
#' @return An empty character vector if no attributes have been selected or else
#' a vector with attributes.
#' @importFrom methods validObject
#' @export
setMethod("attrs",
          signature = "tiledb_array",
          function(object) object@attrs)

#' Selects attributes for the given TileDB array
#'
#' @param x A \code{tiledb_array} object
#' @param value A character vector with attributes
#' @return The modified \code{tiledb_array} object
#' @export
setReplaceMethod("attrs",
                 signature = "tiledb_array",
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


## -- extended accessor

#' @rdname extended-tiledb_array-method
#' @export
setGeneric("extended", function(object) standardGeneric("extended"))

#' @rdname extended-set-tiledb_array-method
#' @export
setGeneric("extended<-", function(x, value) standardGeneric("extended<-"))

#' Retrieve data.frame extended returns columns toggle
#'
#' A \code{tiledb_array} object can be returned as \code{data.frame}. This methods
#' returns the selection value for \sQuote{extended} format including row (and column,
#' if present) indices.
#' @param object A \code{tiledb_array} object
#' @return A logical value indicating whether an \code{extended} return is selected
#' @export
setMethod("extended",
          signature = "tiledb_array",
          function(object) object@extended)


## -- extended setter (generic in DenseArray.R)

#' Set data.frame extended return columns toggle
#'
#' A \code{tiledb_array} object can be returned as \code{data.frame}. This methods
#' set the selection value for \sQuote{extended} format including row (and column,
#' if present) indices.
#' @param x A \code{tiledb_array} object
#' @param value A logical value with the selection
#' @return The modified \code{tiledb_array} array object
#' @export
setReplaceMethod("extended",
                 signature = "tiledb_array",
                 function(x, value) {
  x@extended <- value
  validObject(x)
  x
})


## -- selected_ranges accessor

#' @rdname selected_ranges-tiledb_array-method
#' @export
setGeneric("selected_ranges", function(object) standardGeneric("selected_ranges"))

#' @rdname selected_ranges-set-tiledb_array-method
#' @export
setGeneric("selected_ranges<-", function(x, value) standardGeneric("selected_ranges<-"))

#' Retrieve selected_ranges values for the array
#'
#' A \code{tiledb_array} object can have a range selection for each dimension
#' attribute. This methods returns the selection value for \sQuote{selected_ranges}
#' and returns a list (with one element per dimension) of two-column matrices where
#' each row describes one pair of minimum and maximum values.
#' @param object A \code{tiledb_array} object
#' @return A list which can contain a matrix for each dimension
#' @export
setMethod("selected_ranges", signature = "tiledb_array",
          function(object) object@selected_ranges)

#' Set selected_ranges return values for the array
#'
#' A \code{tiledb_array} object can have a range selection for each dimension
#' attribute. This methods sets the selection value for \sQuote{selected_ranges}
#' which is a list (with one element per dimension) of two-column matrices where
#' each row describes one pair of minimum and maximum values.
#' @param x A \code{tiledb_array} object
#' @param value A list of two-column matrices where each list element \sQuote{i}
#' corresponds to the dimension attribute \sQuote{i}. The matrices can contain rows
#' where each row contains the minimum and maximum value of a range.
#' @return The modified \code{tiledb_array} array object
#' @export
setReplaceMethod("selected_ranges", signature = "tiledb_array",
                 function(x, value) {
  x@selected_ranges <- value
  validObject(x)
  x
})
