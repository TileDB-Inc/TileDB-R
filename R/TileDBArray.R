#  MIT License
#
#  Copyright (c) 2017-2021 TileDB Inc.
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
#' @slot query_layout An optional character value
#' @slot datetimes_as_int64 A logical value
#' @slot encryption_key A character value
#' @slot timestamp A POSIXct datetime variable
#' @slot as.matrix A logical value
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
                      query_layout = "character",
                      datetimes_as_int64 = "logical",
                      encryption_key = "character",
                      timestamp = "POSIXct",
                      as.matrix = "logical",
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
#' @param selected_ranges optional A list with matrices where each matrix i
#' describes the (min,max) pair of ranges for dimension i
#' @param query_layout optional A value for the TileDB query layout, defaults to
#' an empty character variable indicating no special layout is set
#' @param datetimes_as_int64 optional A logical value selecting date and datetime value
#' representation as \sQuote{raw} \code{integer64} and not as \code{Date},
#' \code{POSIXct} or \code{nanotime} objects.
#' @param encryption_key optional A character value with an AES-256 encryption key
#' in case the array was written with encryption.
#' @param timestamp optional A POSIXct Datetime value determining where in time the array is
#' to be openened.
#' @param as.matrix optional logical switch, defaults to "FALSE"; currently limited to dense
#' matrices; in the case of multiple attributes in query lists of matrices are returned
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
                         query_layout = character(),
                         datetimes_as_int64 = FALSE,
                         encryption_key = character(),
                         timestamp = as.POSIXct(double(), origin="1970-01-01"),
                         as.matrix = FALSE,
                         ctx = tiledb_get_context()) {
  query_type = match.arg(query_type)
  if (!is(ctx, "tiledb_ctx"))
    stop("argument ctx must be a tiledb_ctx", call. = FALSE)
  if (missing(uri) || !is.scalar(uri, "character"))
    stop("argument uri must be a string scalar", call. = FALSE)
  if (as.data.frame && as.matrix)
    stop("arguments as.data.frame and as.matrix cannot be selected togethers", call. = FALSE)
  if (isTRUE(is.sparse) && as.matrix)
    stop("argument as.matrix cannot be selected for sparse arrays", call. = FALSE)

  if (length(encryption_key) > 0) {
    if (!is.character(encryption_key))
      stop("if used, argument aes_key must be character", call. = FALSE)
    if (length(timestamp) > 0) {
      array_xptr <- libtiledb_array_open_at_with_key(ctx@ptr, uri, query_type,
                                                     encryption_key, timestamp)
    } else {
      array_xptr <- libtiledb_array_open_with_key(ctx@ptr, uri, query_type, encryption_key)
    }
  } else {
    if (length(timestamp) > 0) {
      array_xptr <- libtiledb_array_open_at(ctx@ptr, uri, query_type, timestamp)
    } else {
      array_xptr <- libtiledb_array_open(ctx@ptr, uri, query_type)
    }
  }
  schema_xptr <- libtiledb_array_get_schema(array_xptr)
  is_sparse_status <- libtiledb_array_schema_sparse(schema_xptr)
  if (!is.na(is.sparse) && is.sparse != is_sparse_status) {
    libtiledb_array_close(array_xptr)
    if (is_sparse_status) {
      stop("dense array selected but sparse array found", call. = FALSE)
    } else {
      stop("sparse array selected but dense array found", call. = FALSE)
    }
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
      query_layout = query_layout,
      datetimes_as_int64 = datetimes_as_int64,
      encryption_key = encryption_key,
      timestamp = timestamp,
      as.matrix = as.matrix,
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
  enckey <- object@encryption_key
  if (length(enckey) > 0) {
    schema_xptr <- libtiledb_array_schema_load_with_key(ctx@ptr, uri, enckey)
  }  else {
    schema_xptr <- libtiledb_array_schema_load(ctx@ptr, uri)
  }
  return(tiledb_array_schema.from_ptr(schema_xptr))
})

## unexported helper function to deal with ... args / enckey in next method
.array_schema_load <- function(ctxptr, uri, enckey=character()) {
  if (length(enckey) > 0) {
    schema_xptr <- libtiledb_array_schema_load_with_key(ctxptr, uri, enckey)
  }  else {
    schema_xptr <- libtiledb_array_schema_load(ctxptr, uri)
  }
}

#' Return a schema from a URI character value
#'
#' @param object A character variable with a URI
#' @param ... Extra parameters such as \sQuote{enckey}, the encryption key
#' @return The scheme for the object
setMethod("schema", "character", function(object, ...) {
  ctx <- tiledb_get_context()
  schema_xptr <- .array_schema_load(ctx@ptr, object, ...)
  return(tiledb_array_schema.from_ptr(schema_xptr))
})


#' Prints a tiledb_array object
#'
#' @param object A tiledb array object
#' @export
setMethod("show", signature = "tiledb_array",
          definition = function (object) {
  cat("tiledb_array\n"
     ,"  uri                = '", object@uri, "'\n"
     ,"  is.sparse          = ", if (object@is.sparse) "TRUE" else "FALSE", "\n"
     ,"  as.data.frame      = ", if (object@as.data.frame) "TRUE" else "FALSE", "\n"
     ,"  attrs              = ", if (length(object@attrs) == 0) "(none)"
                               else paste(object@attrs, collapse=","), "\n"
     ,"  selected_ranges    = ", if (length(object@selected_ranges) > 0) sprintf("(%d non-null sets)", sum(sapply(object@selected_ranges, function(x) !is.null(x))))
                               else "(none)", "\n"
     ,"  extended           = ", if (object@extended) "TRUE" else "FALSE" ,"\n"
     ,"  query_layout       = ", if (length(object@query_layout) == 0) "(none)" else object@query_layout, "\n"
     ,"  datetimes_as_int64 = ", if (object@datetimes_as_int64) "TRUE" else "FALSE", "\n"
     ,"  encryption_key     = ", if (length(object@encryption_key) == 0) "(none)" else "(set)", "\n"
     ,"  timestamp          = ", if (length(object@timestamp) == 0) "(none)" else format(object@timestamp), "\n"
     ,"  as.matrix          = ", if (object@as.matrix) "TRUE" else "FALSE", "\n"
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

  if (!is.character(object@query_layout)) {
    valid <- FALSE
    msg <- c(msg, "The 'query_layout' slot does not contain a character value.")
  }

  if (!is.logical(object@datetimes_as_int64)) {
    valid <- FALSE
    msg <- c(msg, "The 'datetimes_as_int64' slot does not contain a logical value.")
  }

  if (!is.character(object@encryption_key)) {
    valid <- FALSE
    msg <- c(msg, "The 'encryption_key' slot does not contain a character vector.")
  }

  if (!inherits(object@timestamp, "POSIXct")) {
    valid <- FALSE
    msg <- c(msg, "The 'timestamp' slot does not contain a POSIXct value.")
  }

  if (!is.logical(object@as.matrix)) {
    valid <- FALSE
    msg <- c(msg, "The 'as.matrix' slot does not contain a logical value.")
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
#' @return The resulting elements in the selected format
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
  layout <- x@query_layout
  asint64 <- x@datetimes_as_int64
  enckey <- x@encryption_key
  tstamp <- x@timestamp

  sparse <- libtiledb_array_schema_sparse(sch@ptr)

  if (length(enckey) > 0) {
    if (length(tstamp) > 0) {
      x@ptr <- libtiledb_array_open_at_with_key(ctx@ptr, uri, "READ", enckey, tstamp)
    } else {
      x@ptr <- libtiledb_array_open_with_key(ctx@ptr, uri, "READ", enckey)
    }
  } else {
    if (length(tstamp) > 0) {
      x@ptr <- libtiledb_array_open_at(ctx@ptr, uri, "READ", tstamp)
    } else {
      x@ptr <- libtiledb_array_open(ctx@ptr, uri, "READ")
    }
  }
  on.exit(libtiledb_array_close(x@ptr))

  dims <- tiledb::dimensions(dom)
  dimnames <- sapply(dims, function(d) libtiledb_dim_get_name(d@ptr))
  dimtypes <- sapply(dims, function(d) libtiledb_dim_get_datatype(d@ptr))
  dimvarnum <- sapply(dims, function(d) libtiledb_dim_get_cell_val_num(d@ptr))
  dimnullable <- sapply(dims, function(d) FALSE)

  attrs <- tiledb::attrs(schema(x))
  attrnames <- unname(sapply(attrs, function(a) libtiledb_attribute_get_name(a@ptr)))
  attrtypes <- unname(sapply(attrs, function(a) libtiledb_attribute_get_type(a@ptr)))
  attrvarnum <- unname(sapply(attrs, function(a) libtiledb_attribute_get_cell_val_num(a@ptr)))
  attrnullable <- unname(sapply(attrs, function(a) libtiledb_attribute_get_nullable(a@ptr)))

  if (length(sel) != 0) {
    ind <- match(sel, attrnames)
    if (length(ind) == 0) {
      stop("Only non-existing columns selected.", call.=FALSE)
    }
    attrnames <- attrnames[ind]
    attrtypes <- attrtypes[ind]
    attrvarnum <- attrvarnum[ind]
    attrnullable <- attrnullable[ind]
  }

  allnames <- c(dimnames, attrnames)
  alltypes <- c(dimtypes, attrtypes)
  allvarnum <- c(dimvarnum, attrvarnum)
  allnullable <- c(dimnullable, attrnullable)

  if (length(enckey) > 0) {
    if (length(tstamp) > 0) {
      arrptr <- libtiledb_array_open_at_with_key(ctx@ptr, uri, "READ", enckey, tstamp)
    } else {
      arrptr <- libtiledb_array_open_with_key(ctx@ptr, uri, "READ", enckey)
    }
  } else {
    if (length(tstamp) > 0) {
      arrptr <- libtiledb_array_open_at(ctx@ptr, uri, "READ", tstamp)
    } else {
      arrptr <- libtiledb_array_open(ctx@ptr, uri, "READ")
    }
  }

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
  if (length(layout) > 0) libtiledb_query_set_layout(qryptr, layout)

  ## ranges seem to interfere with the byte/element adjustment below so set up toggle
  rangeunset <- TRUE

  ## set default range(s) on first dimension if nothing is specified
  if (is.null(i) &&
      (length(x@selected_ranges) == 0 ||
       (length(x@selected_ranges) >= 1 && is.null(x@selected_ranges[[1]])))) {
    ## domain values can currently be eg (0,0) rather than a flag, so check explicitly
    #domdim <- domain(dimensions(dom)[[1]])
    if (nonemptydom[[1]][1] != nonemptydom[[1]][2]) # || nonemptydom[[1]][1] > domdim[1])
      qryptr <- libtiledb_query_add_range_with_type(qryptr, 0, dimtypes[1],
                                                    nonemptydom[[1]][1], nonemptydom[[1]][2])
      rangeunset <- FALSE
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
    rangeunset <- FALSE
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
      rangeunset <- FALSE
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
      rangeunset <- FALSE
    }
  }

  ## if ranges selected, use those
  for (k in seq_len(length(x@selected_ranges))) {
    if (!is.null(x@selected_ranges[[k]])) {
      m <- x@selected_ranges[[k]]
      for (i in seq_len(nrow(m))) {
        qryptr <- libtiledb_query_add_range_with_type(qryptr, k-1, dimtypes[k], m[i,1], m[i,2])
      }
      rangeunset <- FALSE
    }
  }

  ## retrieve est_result_size
  getEstimatedSize <- function(name, varnum, nullable, qryptr, datatype) {
    if (is.na(varnum) && !nullable)
      res <- libtiledb_query_get_est_result_size_var(qryptr, name)[1]
    else if (is.na(varnum) && nullable)
      res <- libtiledb_query_get_est_result_size_var_nullable(qryptr, name)[1]
    else if (!is.na(varnum) && !nullable)
      res <- libtiledb_query_get_est_result_size(qryptr, name)
    else if (!is.na(varnum) && nullable)
      res <- libtiledb_query_get_est_result_size_nullable(qryptr, name)[1]
    if (rangeunset && tiledb::tiledb_version(TRUE) >= "2.2.0") {
      sz <- switch(datatype,
                   "UINT8"   = ,
                   "INT8"    = 1,
                   "UINT16"  = ,
                   "INT16"   = 2,
                   "INT32"   = ,
                   "UINT32"  = ,
                   "FLOAT32" = 4,
                   "UINT64"  = ,
                   "INT64"   = ,
                   "FLOAT64" = ,
                   "DATETIME_YEAR" = ,
                   "DATETIME_MONTH" = ,
                   "DATETIME_WEEK" = ,
                   "DATETIME_DAY" = ,
                   "DATETIME_HR" = ,
                   "DATETIME_MIN" = ,
                   "DATETIME_SEC" = ,
                   "DATETIME_MS" = ,
                   "DATETIME_US" = ,
                   "DATETIME_NS" = ,
                   "DATETIME_PS" = ,
                   "DATETIME_FS" = ,
                   "DATETIME_AS" = 8,
                   1)
      res <- res / sz
    }
    res
  }
  ressizes <- mapply(getEstimatedSize, allnames, allvarnum, allnullable, alltypes,
                     MoreArgs=list(qryptr=qryptr), SIMPLIFY=TRUE)
  resrv <- max(1, ressizes) # ensure >0 for correct handling of zero-length outputs

  ## allocate and set buffers
  getBuffer <- function(name, type, varnum, nullable, resrv, qryptr, arrptr) {
      if (is.na(varnum)) {
          if (type %in% c("CHAR", "ASCII", "UTF8")) {
              buf <- libtiledb_query_buffer_var_char_alloc_direct(resrv, resrv*8, nullable)
              qryptr <- libtiledb_query_set_buffer_var_char(qryptr, name, buf)
              buf
          } else {
              message("Non-char var.num columns are not currently supported.")
          }
      } else {
          buf <- libtiledb_query_buffer_alloc_ptr(arrptr, type, resrv, nullable)
          qryptr <- libtiledb_query_set_buffer_ptr(qryptr, name, buf)
          buf
      }
  }
  buflist <- mapply(getBuffer, allnames, alltypes, allvarnum, allnullable,
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
  if (any(!is.na(estsz))) {
      resrv <- max(estsz, na.rm=TRUE)
  } else {
      resrv <- resrv/8                  # character case where bytesize of offset vector was used
  }

  ## get results
  getResult <- function(buf, name, varnum, resrv, qryptr) {
    if (is.na(varnum)) {
      sz <- libtiledb_query_result_buffer_elements(qryptr, name)
      libtiledb_query_get_buffer_var_char(buf, resrv, sz)[,1]
    } else {
      libtiledb_query_get_buffer_ptr(buf, asint64)
    }
  }
  reslist <- mapply(getResult, buflist, allnames, allvarnum,
                    MoreArgs=list(resrv=resrv, qryptr=qryptr), SIMPLIFY=FALSE)

  ## convert list into data.frame (cheaply) and subset
  res <- data.frame(reslist)[seq_len(resrv),]
  colnames(res) <- allnames

  ## reduce output if extended is false, or attrs given
  if (!x@extended || length(sel) > 0) {
      res <- res[, if (sparse) allnames else attrnames]
      k <- match("__tiledb_rows", colnames(res))
      if (is.finite(k)) {
          res <- res[, -k]
      }
  }

  if (!x@as.data.frame && !x@as.matrix) {
    res <- as.list(res)
  }

  if (x@as.matrix) {
    if (ncol(res) < 3) {
      message("ignoring as.matrix argument with insufficient result set")
    } else if (!is.null(i)) {
      message("case of row selection not supported for accessing as.matrix")
    } else if (!is.null(j)) {
      message("case of column selection not supported for accessing as.matrix")
    } else if (ncol(res) == 3) {
      mat <- matrix(, nrow=max(res[,1]), ncol=max(res[,2]))
      mat[ cbind( res[,1], res[,2] ) ] <- res[,3]
      res <- mat
    } else {                            # case of ncol > 3
      k <- ncol(res) - 2
      lst <- vector(mode = "list", length = k)
      for (i in seq_len(k)) {
         mat <- matrix(, nrow=max(res[,1]), ncol=max(res[,2]))
         mat[ cbind( res[,1], res[,2] ) ] <- res[, 2 + i]
         lst[[i]] <- mat
      }
      names(lst) <- tail(colnames(res), k)
      res <- lst
    }
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
  layout <- x@query_layout
  asint64 <- x@datetimes_as_int64
  enckey <- x@encryption_key
  tstamp <- x@timestamp

  sparse <- libtiledb_array_schema_sparse(sch@ptr)

  dims <- tiledb::dimensions(dom)
  ndims <- length(dims)
  dimnames <- sapply(dims, function(d) libtiledb_dim_get_name(d@ptr))
  dimtypes <- sapply(dims, function(d) libtiledb_dim_get_datatype(d@ptr))
  dimvarnum <- sapply(dims, function(d) libtiledb_dim_get_cell_val_num(d@ptr))
  dimnullable <- sapply(dims, function(d) FALSE)

  attrs <- tiledb::attrs(schema(x))
  attrnames <- unname(sapply(attrs, function(a) libtiledb_attribute_get_name(a@ptr)))
  attrtypes <- unname(sapply(attrs, function(a) libtiledb_attribute_get_type(a@ptr)))
  attrvarnum <- unname(sapply(attrs, function(a) libtiledb_attribute_get_cell_val_num(a@ptr)))
  attrnullable <- unname(sapply(attrs, function(a) libtiledb_attribute_get_nullable(a@ptr)))

  allnames <- c(dimnames, attrnames)
  alltypes <- c(dimtypes, attrtypes)
  allvarnum <- c(dimvarnum, attrvarnum)
  allnullable <- c(dimnullable, attrnullable)

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
    allnullable <- attrnullable
  }

  nc <- if (is.list(value)) length(value) else ncol(value)
  nm <- if (is.list(value)) names(value) else colnames(value)

  if (all.equal(sort(allnames),sort(nm))) {

    if (length(enckey) > 0) {
      if (length(tstamp) > 0) {
        arrptr <- libtiledb_array_open_at_with_key(ctx@ptr, uri, "WRITE", enckey, tstamp)
      } else {
        arrptr <- libtiledb_array_open_with_key(ctx@ptr, uri, "WRITE", enckey)
      }
    } else {
      if (length(tstamp) > 0) {
        arrptr <- libtiledb_array_open_at(ctx@ptr, uri, "WRITE", tstamp)
      } else {
        arrptr <- libtiledb_array_open(ctx@ptr, uri, "WRITE")
      }
    }


    qryptr <- libtiledb_query(ctx@ptr, arrptr, "WRITE")
    qryptr <- libtiledb_query_set_layout(qryptr,
                                         if (length(layout) > 0) layout
                                         else { if (sparse) "UNORDERED" else "COL_MAJOR" })

    buflist <- vector(mode="list", length=nc)

    for (colnam in allnames) {
      ## when an index column is use this may be unordered to remap to position in 'nm' names
      i <- match(colnam, nm)
      if (alltypes[i] %in% c("CHAR", "ASCII")) { # variable length
          if (!allnullable[i]) {
              txtvec <- as.character(value[[i]])
              offsets <- c(0L, cumsum(nchar(txtvec[-length(txtvec)])))
              data <- paste(txtvec, collapse="")
              ##cat("Alloc char buffer", i, "for", colnam, ":", alltypes[i], "\n")
              buflist[[i]] <- libtiledb_query_buffer_var_char_create(offsets, data)
              qryptr <- libtiledb_query_set_buffer_var_char(qryptr, colnam, buflist[[i]])
          } else { # variable length and nullable
              txtvec <- as.character(value[[i]])
              navec <- is.na(txtvec)
              newvec <- txtvec
              newvec[navec] <- ".."     # somehow we need two chars for NA as if we passed the char
              offsets <- c(0L, cumsum(nchar(newvec[-length(newvec)])))
              data <- paste(txtvec, collapse="")
              buflist[[i]] <- libtiledb_query_buffer_var_char_create_nullable(offsets, data, allnullable[i], navec)
              qryptr <- libtiledb_query_set_buffer_var_char(qryptr, colnam, buflist[[i]])
          }
      } else {
        nr <- NROW(value[[i]])
        #cat("Alloc buf", i, " ", colnam, ":", alltypes[i], "nr:", nr, "null:", allnullable[i], "\n")
        buflist[[i]] <- libtiledb_query_buffer_alloc_ptr(arrptr, alltypes[i], nr, allnullable[i])
        buflist[[i]] <- libtiledb_query_buffer_assign_ptr(buflist[[i]], alltypes[i], value[[i]], asint64)
        qryptr <- libtiledb_query_set_buffer_ptr(qryptr, colnam, buflist[[i]])
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



## -- query_layout accessor

#' @rdname query_layout-tiledb_array-method
#' @export
setGeneric("query_layout", function(object) standardGeneric("query_layout"))

#' @rdname query_layout-set-tiledb_array-method
#' @export
setGeneric("query_layout<-", function(x, value) standardGeneric("query_layout<-"))

#' Retrieve query_layout values for the array
#'
#' A \code{tiledb_array} object can have a corresponding query with a given layout
#' given layout. This methods returns the selection value for \sQuote{query_layout}
#' as a character value.
#' @param object A \code{tiledb_array} object
#' @return A character value describing the query layout
#' @export
setMethod("query_layout", signature = "tiledb_array", function(object) object@query_layout)

#' Set query_layout return values for the array
#'
#' A \code{tiledb_array} object can have an associated query with a specific layout.
#' This methods sets the selection value for \sQuote{query_layout} from a character
#' value.
#' @param x A \code{tiledb_array} object
#'
#' @param value A character variable for the query layout. Permitted values are
#' \dQuote{ROW_MAJOR}, \dQuote{COL_MAJOR}, \dQuote{GLOBAL_ORDER}, or \dQuote{UNORDERD}.
#' @return The modified \code{tiledb_array} array object
#' @export
setReplaceMethod("query_layout", signature = "tiledb_array", function(x, value) {
  x@query_layout <- value
  validObject(x)
  x
})



## -- datetimes_as_int64 accessor

#' @rdname datetimes_as_int64-tiledb_array-method
#' @export
setGeneric("datetimes_as_int64", function(object) standardGeneric("datetimes_as_int64"))

#' @rdname datetimes_as_int64-set-tiledb_array-method
#' @export
setGeneric("datetimes_as_int64<-", function(x, value) standardGeneric("datetimes_as_int64<-"))

#' Retrieve datetimes_as_int64 toggle
#'
#' A \code{tiledb_array} object may contain date and datetime objects. While their internal
#' representation is generally shielded from the user, it can useful to access them as the
#' \sQuote{native} format which is an \code{integer64}. This function retrieves the current
#' value of the selection variable, which has a default of \code{FALSE}.
#' @param object A \code{tiledb_array} object
#' @return A logical value indicating whether \code{datetimes_as_int64} is selected
#' @export
setMethod("datetimes_as_int64",
          signature = "tiledb_array",
          function(object) object@datetimes_as_int64)


## -- datetimes_as_int64 setter (generic in DenseArray.R)

#' Set datetimes_as_int64 toggle
#'
#' A \code{tiledb_array} object may contain date and datetime objects. While their internal
#' representation is generally shielded from the user, it can useful to access them as the
#' \sQuote{native} format which is an \code{integer64}. This function set the current
#' value of the selection variable, which has a default of \code{FALSE}.
#' @param x A \code{tiledb_array} object
#' @param value A logical value with the selection
#' @return The modified \code{tiledb_array} array object
#' @export
setReplaceMethod("datetimes_as_int64",
                 signature = "tiledb_array",
                 function(x, value) {
  x@datetimes_as_int64 <- value
  validObject(x)
  x
})


## -- consolitate wrapper

#' Consolidate fragments of a TileDB Array
#'
#' This function invokes a consolidation operation. Parameters can be set via
#' an option configuration object.
#' @param uri A character value with the URI of a TileDB Array
#' @param cfg An optional TileDB Configuration object
#' @param ctx An option TileDB Context object
#' @return \code{NULL} is returned invisibly
#' @export
array_consolidate <- function(uri, cfg = NULL, ctx = tiledb_get_context()) {
  libtiledb_array_consolidate(ctx = ctx@ptr, uri = uri,
                              # C++ code has Nullable and can instantiate but needs S4 XPtr
                              cfgptr = if (is.null(cfg)) cfg else cfg@ptr)
}

#' After consolidation, remove consilidated fragments of a TileDB Array
#'
#' This function can remove fragments following a consolidation step. Note that vacuuming
#' should \emph{not} be run if one intends to use the TileDB \emph{time-traveling} feature
#' of opening arrays at particular timestamps
#' @param uri A character value with the URI of a TileDB Array
#' @param cfg An optional TileDB Configuration object
#' @param ctx An option TileDB Context object
#' @return \code{NULL} is returned invisibly
#' @export
array_vacuum <- function(uri, cfg = NULL, ctx = tiledb_get_context()) {
  libtiledb_array_consolidate(ctx = ctx@ptr, uri = uri,
                              # C++ code has Nullable and can instantiate but needs S4 XPtr
                              cfgptr = if (is.null(cfg)) cfg else cfg@ptr)
}

#' Get the non-empty domain from a TileDB Array by index
#'
#' This functions works for both fixed- and variable-sized dimensions and switches
#' internally.
#' @param arr A TileDB Array
#' @param idx An integer index between one the number of dimensions
#' @return A two-element object is returned describing the domain of selected
#' dimension; it will either be a numeric vector in case of a fixed-size
#' fixed-sized dimensions, or a characer vector for a variable-sized one.
#' @export
tiledb_array_get_non_empty_domain_from_index <- function(arr, idx) {
  stopifnot(arr_argument=is(arr, "tiledb_array"),
            idx_argument=is.numeric(idx),
            idx_nonnegative=idx > 0)

  arr <- tiledb_array_close(arr)
  arr <- tiledb_array_open(arr, "READ")
  sch <- schema(arr)
  dom <- domain(sch)
  dims <- dimensions(dom)
  dimtypes <- sapply(dims, function(d) libtiledb_dim_get_datatype(d@ptr))
  dimvarnum <- sapply(dims, function(d) libtiledb_dim_get_cell_val_num(d@ptr))

  if (is.na(dimvarnum[idx]))
    libtiledb_array_get_non_empty_domain_var_from_index(arr@ptr, idx-1)
  else
    libtiledb_array_get_non_empty_domain_from_index(arr@ptr, idx-1, dimtypes[idx])

}

#' Get the non-empty domain from a TileDB Array by name
#'
#' This functions works for both fixed- and variable-sized dimensions and switches
#' internally.
#' @param arr A TileDB Array
#' @param name An character variable with a dimension name
#' @return A two-element object is returned describing the domain of selected
#' dimension; it will either be a numeric vector in case of a fixed-size
#' fixed-sized dimensions, or a characer vector for a variable-sized one.
#' @export
tiledb_array_get_non_empty_domain_from_name <- function(arr, name) {
  stopifnot(arr_argument=is(arr, "tiledb_array"),
            name_argument=is.character(name))

  sch <- schema(arr)
  dom <- domain(sch)
  dims <- dimensions(dom)
  dimnames <- sapply(dims, function(d) libtiledb_dim_get_name(d@ptr))

  idx <- match(name, dimnames)
  if (is.na(idx)) stop("Argument '", name, "' not among domain names for array.", call.=FALSE)

  tiledb_array_get_non_empty_domain_from_index(arr, idx)
}
