
#' Create a \sQuote{batched} query object
#'
#' Batched queries return an initial result set even when it is incomplete. Where
#' the normal retrieval process will loop in place to complete a (potentially large)
#' result set, this function will return a result (which may be part of a larger
#' result set) allowing the user to assemble all part.
#'
#' The \code{tiledb_array} object can be parameterised as usual.
#'
#' @param x A \code{tiledb_array} object
#' @return A \code{batchedquery} object, that is a list containing an external pointer
#' to a TileDB Query object along with other support variables used by \code{fetchBatched}
#' @export
createBatched <- function(x) {
    ## add defaults, shortcut for now
    i <- j <- k <- NULL
    #verbose <- getOption("verbose", FALSE)

    ## ## deal with possible n-dim indexing
    ## ndlist <- nd_index_from_syscall(sys.call(), parent.frame())
    ## if (length(ndlist) >= 0) {
    ##     if (length(ndlist) >= 1 && !is.null(ndlist[[1]])) i <- ndlist[[1]]
    ##     if (length(ndlist) >= 2 && !is.null(ndlist[[2]])) j <- ndlist[[2]]
    ##     if (length(ndlist) >= 3 && !is.null(ndlist[[3]])) k <- ndlist[[3]]
    ##     if (length(ndlist) >= 4) message("Indices beyond the third dimension not supported in [i,j,k] form. Use selected_ranges().")
    ## }

    ctx <- x@ctx
    uri <- x@uri
    sel <- x@attrs
    sch <- tiledb::schema(x)
    dom <- tiledb::domain(sch)
    layout <- x@query_layout
    asint64 <- x@datetimes_as_int64
    enckey <- x@encryption_key
    tstamp <- x@timestamp_end

    sparse <- libtiledb_array_schema_sparse(sch@ptr)

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
    if (length(sel)==1 && is.na(sel[1])) {            # special case of NA selecting no attrs
        attrnames <- character()
        attrtypes <- character()
        attrvarnum <- integer()
        attrnullable <- logical()
    }

    if (length(sel) != 0 && !any(is.na(sel))) {
        ind <- match(sel, attrnames)
        if (length(ind) == 0) {
            stop("Only non-existing columns selected.", call.=FALSE)
        }
        attrnames <- attrnames[ind]
        attrtypes <- attrtypes[ind]
        attrvarnum <- attrvarnum[ind]
        attrnullable <- attrnullable[ind]
    }

    if (x@extended) {                     # if true return dimensions and attributes
        allnames <- c(dimnames, attrnames)
        alltypes <- c(dimtypes, attrtypes)
        allvarnum <- c(dimvarnum, attrvarnum)
        allnullable <- c(dimnullable, attrnullable)
    } else {                              # otherwise only return attributes
        allnames <- attrnames
        alltypes <- attrtypes
        allvarnum <- attrvarnum
        allnullable <- attrnullable
    }

    ## A preference can be set in a local per-user configuration file; if no value
    ## is set a fallback from the TileDB config object is used.
    memory_budget <- get_allocation_size_preference()
    spdl::debug("[createBatched] memory budget is {}", memory_budget)

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
    if (length(x@timestamp_start) > 0) {
        arrptr <- libtiledb_array_set_open_timestamp_start(arrptr, x@timestamp_start)
    }
    if (length(x@timestamp_end) > 0) {
        arrptr <- libtiledb_array_set_open_timestamp_end(arrptr, x@timestamp_end)
    }
    if (length(x@timestamp_start) > 0 || length(x@timestamp_end) > 0) {
        arrptr <- libtiledb_array_reopen(arrptr)
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

    ## ensure selected_ranges, if submitted, is of correct length
    if (length(x@selected_ranges) != 0 &&
        length(x@selected_ranges) != length(dimnames) &&
        is.null(names(x@selected_ranges))) {
        stop(paste0("If ranges are selected by index alone (and not named), ",
                    "one is required for each dimension."), call. = FALSE)
    }

    ## expand a shorter-but-named selected_ranges list
    if (   (length(x@selected_ranges) < length(dimnames))
        && (!is.null(names(x@selected_ranges)))          ) {
        fulllist <- vector(mode="list", length=length(dimnames))
        ind <- match(names(x@selected_ranges), dimnames)
        if (any(is.na(ind))) stop("Name for selected ranges does not match dimension names.")
        for (ii in seq_len(length(ind))) {
            fulllist[[ ind[ii] ]] <- x@selected_ranges[[ii]]
        }
        x@selected_ranges <- fulllist
    }

    ## selected_ranges may be in different order than dimnames, so reorder if need be
    if ((length(x@selected_ranges) == length(dimnames))
        && (!is.null(names(x@selected_ranges)))
        && (!identical(names(x@selected_ranges), dimnames))) {
        x@selected_ranges <- x@selected_ranges[dimnames]
    }

    ## if selected_ranges is still an empty list, make it an explicit one
    if (length(x@selected_ranges) == 0) {
        x@selected_ranges <- vector(mode="list", length=length(dimnames))
    }

    if (!is.null(i)) {
        if (!is.null(x@selected_ranges[[1]])) {
            stop("Cannot set both 'i' and first element of 'selected_ranges'.", call. = FALSE)
        }
        x@selected_ranges[[1]] <- i
    }

    if (!is.null(j)) {
        if (!is.null(x@selected_ranges[[2]])) {
            stop("Cannot set both 'j' and second element of 'selected_ranges'.", call. = FALSE)
        }
        x@selected_ranges[[2]] <- j
    }

    if (!is.null(k)) {
        if (!is.null(x@selected_ranges[[3]])) {
            stop("Cannot set both 'k' and second element of 'selected_ranges'.", call. = FALSE)
        }
        x@selected_ranges[[3]] <- k
    }
    ## (i,j,k) are now done and transferred to x@select_ranges


    ## if ranges selected, use those
    for (k in seq_len(length(x@selected_ranges))) {
        if (is.null(x@selected_ranges[[k]])) {
            ##cat("Adding null dim", k, "on", dimtypes[k], "\n")
            vec <- .map2integer64(nonemptydom[[k]], dimtypes[k])
            if (vec[1] != 0 && vec[2] != 0) { # corner case of A[] on empty array
                qryptr <- libtiledb_query_add_range_with_type(qryptr, k-1, dimtypes[k], vec[1], vec[2])
                rangeunset <- FALSE
            }
        } else if (is.null(nrow(x@selected_ranges[[k]]))) {
            ##cat("Adding nrow null dim", k, "on", dimtypes[k], "\n")
            vec <- x@selected_ranges[[k]]
            vec <- .map2integer64(vec, dimtypes[k])
            qryptr <- libtiledb_query_add_range_with_type(qryptr, k-1, dimtypes[k], min(vec), max(vec))
            rangeunset <- FALSE
        } else {
            ##cat("Adding non-zero dim", k, "on", dimtypes[k], "\n")
            m <- x@selected_ranges[[k]]
            for (i in seq_len(nrow(m))) {
                vec <- .map2integer64(c(m[i,1], m[i,2]), dimtypes[k])
                qryptr <- libtiledb_query_add_range_with_type(qryptr, k-1, dimtypes[k], vec[1], vec[2])
            }
            rangeunset <- FALSE
        }
    }

    buflist <- vector(mode="list", length=length(allnames))

    #if (!qryinit) {
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
            if (rangeunset) {
                sz <- tiledb_datatype_string_to_sizeof(datatype)
                res <- res / sz
            }
            res
        }
        ressizes <- mapply(getEstimatedSize, allnames, allvarnum, allnullable, alltypes,
                           MoreArgs=list(qryptr=qryptr), SIMPLIFY=TRUE)
        ## ensure > 0 for correct handling of zero-length outputs, ensure respecting memory budget
        resrv <- max(1, min(memory_budget/8, ressizes))
        ## allocate and set buffers
        getBuffer <- function(name, type, varnum, nullable, resrv, qryptr, arrptr) {
            if (is.na(varnum)) {
                if (type %in% c("CHAR", "ASCII", "UTF8")) {
                    spdl::debug("[getBuffer] '{}' allocating 'char' {} rows given budget of {}", name, resrv, memory_budget)
                    buf <- libtiledb_query_buffer_var_char_alloc_direct(resrv, memory_budget, nullable)
                    qryptr <- libtiledb_query_set_buffer_var_char(qryptr, name, buf)
                    buf
                } else {
                    message("Non-char var.num columns are not currently supported.")
                }
            } else {
                spdl::debug("[getBuffer] '{}' allocating non-char {} rows given budget of {}", name, resrv, memory_budget)
                buf <- libtiledb_query_buffer_alloc_ptr(type, resrv, nullable, varnum)
                qryptr <- libtiledb_query_set_buffer_ptr(qryptr, name, buf)
                buf
            }
        }
        buflist <- mapply(getBuffer, allnames, alltypes, allvarnum, allnullable,
                          MoreArgs=list(resrv=resrv, qryptr=qryptr, arrptr=arrptr),
                          SIMPLIFY=FALSE)

        ## if we have a query condition, apply it
        if (isTRUE(x@query_condition@init)) {
            qryptr <- libtiledb_query_set_condition(qryptr, x@query_condition@ptr)
        }
    #}


    res <- list(qryptr, allnames, allvarnum, alltypes, allnullable, buflist)
    class(res) <- "batchedquery"
    res
}


#' Run a \sQuote{batched} query
#'
#' Batched queries return an initial result set even when it is incomplete. Where
#' the normal retrieval process will loop in place to complete a (potentially large)
#' result set, this function will return a result (which may be part of a larger
#' result set) allowing the user to assemble all part.
#'
#' The \code{tiledb_array} object can be parameterised as usual.
#'
#' @param x A \code{tiledb_array} object
#' @param obj A \code{batchedquery} object as returned by \code{createBatched}
#' @return A data.frame object with the (potentially partial) result of a
#' batched query
#' @export
fetchBatched <- function(x, obj) {
    stopifnot("The 'x' argument must be a 'tiledb_array'" = is(x, "tiledb_array"),
              "The 'obj' argument must be 'batchedquery' object" = inherits(obj, "batchedquery"))
    qryptr <- obj[[1]]
    allnames <- obj[[2]]
    allvarnum <- obj[[3]]
    alltypes <- obj[[4]]
    allnullable <- obj[[5]]
    buflist <- obj[[6]]

    asint64 <- x@datetimes_as_int64

    #verbose <- getOption("verbose", FALSE)

    ## fire off query
    qryptr <- libtiledb_query_submit(qryptr)

    ## check status
    status <- libtiledb_query_status(qryptr)
    ##if (status != "COMPLETE") warning("Query returned '", status, "'.", call. = FALSE)

    ## close array
    if (status == "COMPLETE") {
        arrptr <- x@ptr
        libtiledb_array_close(arrptr)
        .pkgenv[["query_status"]] <- status
        finished <- TRUE
    }

    ## retrieve actual result size (from fixed size element columns)
    getResultSize <- function(name, varnum, qryptr) {
        if (is.na(varnum))                  # symbols come up with higher count
            libtiledb_query_result_buffer_elements(qryptr, name, 0)
        else
            libtiledb_query_result_buffer_elements(qryptr, name)
    }
    estsz <- mapply(getResultSize, allnames, allvarnum, MoreArgs=list(qryptr=qryptr), SIMPLIFY=TRUE)
    spdl::debug("[fetchBatched] estimated result sizes {}", paste(estsz, collapse=","))
    if (any(!is.na(estsz))) {
        resrv <- max(estsz, na.rm=TRUE)
    } else {
        resrv <- resrv/8                  # character case where bytesize of offset vector was used
    }
    spdl::debug("[fetchBatched] expected size {}", resrv)
    ## Permit one pass to allow zero-row schema read
    #if (resrv == 0 && counter > 1L) {
        #finished <- TRUE
        ##if (verbose) message("Breaking loop at zero length expected")
        #if (status != "COMPLETE") warning("Query returned '", status, "'.", call. = FALSE)
        #.pkgenv[["query_status"]] <- status
                                        #break
    #}
    ## get results
    getResult <- function(buf, name, varnum, estsz, qryptr) {
        has_dumpbuffers <- length(x@dumpbuffers) > 0
        if (is.na(varnum)) {
            vec <- libtiledb_query_result_buffer_elements_vec(qryptr, name)
            if (has_dumpbuffers) {
                vlcbuf_to_shmem(x@dumpbuffers, name, buf, vec)
            }
            libtiledb_query_get_buffer_var_char(buf, vec[1], vec[2])[,1][seq_len(estsz)]
        } else {
            if (has_dumpbuffers) {
                vecbuf_to_shmem(x@dumpbuffers, name, buf, estsz, varnum)
            }
            libtiledb_query_get_buffer_ptr(buf, asint64)[seq_len(estsz)]
        }
    }
    reslist <- mapply(getResult, buflist, allnames, allvarnum, estsz,
                      MoreArgs=list(qryptr=qryptr), SIMPLIFY=FALSE)
    ## convert list into data.frame (possibly dealing with list columns) and subset
    vnum <- 1   # default value of variable number of elements per cell
    if (is.list(allvarnum)) allvarnum <- unlist(allvarnum)
    if (length(allvarnum) > 0 && any(!is.na(allvarnum))) vnum <- max(allvarnum, na.rm=TRUE)
    if (is.finite(vnum) && (vnum > 1)) {
        ## turn to list col if a varnum != 1 (and not NA) seen
        ind <- which(allvarnum != 1 & !is.na(allvarnum))
        for (k in ind) {
            ncells <- allvarnum[k]
            v <- reslist[[k]]
            ## we split a vector v into 'list-columns' which element containing
            ## ncells value (and we get ncells from the Array schema)
            ## see https://stackoverflow.com/a/9547594/143305 for I()
            ## and https://stackoverflow.com/a/3321659/143305 for split()
            reslist[[k]] <- I(unname(split(v, ceiling(seq_along(v)/ncells))))
        }
    }
    res <- data.frame(reslist)[seq_len(resrv),,drop=FALSE]
    colnames(res) <- allnames
    spdl::debug("[fetchBatched] retrieved {}", paste(dim(res), collapse="x"))
    ##overallresults[[counter]] <- res
    ##counter <- counter + 1L

    res
}


#' Return \sQuote{batched} status
#'
#' Batched queries return an initial result set even when it is incomplete. Where
#' the normal retrieval process will loop in place to complete a (potentially large)
#' result set, this function will return a result (which may be part of a larger
#' result set) allowing the user to assemble all part.
#'
#' @param obj A list object as returned by \code{createBatched}
#' @return The Query status as a character variable
#' @export
statusBatched <- function(obj) {
    stopifnot("The 'obj' argument must be 'batchedquery' object" = inherits(obj, "batchedquery"))
    libtiledb_query_status(obj[[1]])
}

#' Check \sQuote{batched} query for completion
#'
#' Batched queries return an initial result set even when it is incomplete. Where
#' the normal retrieval process will loop in place to complete a (potentially large)
#' result set, this function will return a result (which may be part of a larger
#' result set) allowing the user to assemble all part.
#'
#' @param obj A list object as returned by \code{createBatched}
#' @return A logical value to indicated if the query completed
#' @export
completedBatched <- function(obj) {
    stopifnot("The 'obj' argument must be 'batchedquery' object" = inherits(obj, "batchedquery"))
    libtiledb_query_status(obj[[1]]) == "COMPLETE"
}
