#  MIT License
#
#  Copyright (c) 2017-2024 TileDB Inc.
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

#' An S4 class for the TileDB array schema
#'
#' @slot ptr An external pointer to the underlying implementation
#' @slot arrptr An optional external pointer to the underlying array, or NULL if missing
#' @exportClass tiledb_array_schema
setClass("tiledb_array_schema",
         slots = list(ptr = "externalptr",
                      arrptr = "ANY"))

tiledb_array_schema.from_ptr <- function(ptr, arrptr=NULL) {
    stopifnot("The 'ptr' argument must be an external pointer to a tiledb_array_schema instance"
              = !missing(ptr) && is(ptr, "externalptr") && !is.null(ptr))
    new("tiledb_array_schema", ptr = ptr, arrptr = arrptr)
}

#' Constructs a `tiledb_array_schema` object
#'
#' @param domain tiledb_domain object
#' @param attrs a list of one or more tiledb_attr objects
#' @param cell_order (default "COL_MAJOR")
#' @param tile_order (default "COL_MAJOR")
#' @param sparse (default FALSE)
#' @param coords_filter_list (optional)
#' @param offsets_filter_list (optional)
#' @param validity_filter_list (optional)
#' @param capacity (optional)
#' @param allows_dups (optional, requires \sQuote{sparse} to be TRUE)
#' @param enumerations (optional) named list of enumerations
#' @param ctx tiledb_ctx object (optional)
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' schema <- tiledb_array_schema(
#'               dom = tiledb_domain(
#'                         dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
#'                                  tiledb_dim("cols", c(1L, 4L), 4L, "INT32"))),
#'               attrs = c(tiledb_attr("a", type = "INT32")),
#'               cell_order = "COL_MAJOR",
#'               tile_order = "COL_MAJOR",
#'               sparse = FALSE)
#' schema
#'
#' @export
tiledb_array_schema <- function(domain,
                                attrs,
                                cell_order = "COL_MAJOR",
                                tile_order = "COL_MAJOR",
                                sparse = FALSE,
                                coords_filter_list = NULL,
                                offsets_filter_list = NULL,
                                validity_filter_list = NULL,
                                capacity = 10000L,
                                allows_dups = FALSE,
                                enumerations = NULL,
                                ctx = tiledb_get_context()) {
    if (!missing(attrs) && length(attrs) != 0) {
        is_attr <- function(obj) is(obj, "tiledb_attr")
        if (is_attr(attrs))             # if an attrs object given:
            attrs <- list(attrs) 		# make it a list so that lapply works below
        stopifnot("length of 'attrs' cannot be zero" = length(attrs) > 0,
                  "'attrs' must be a list of one or tiled_attr objects" = all(vapply(attrs, is_attr, logical(1))))
    } else {
        attrs <- NULL
    }
    stopifnot("ctx argument must be a tiledb_ctx"           = is(ctx, "tiledb_ctx"),
              "domain argument must be a tiledb::Domain"    = !missing(domain) && is(domain, "tiledb_domain"),
              "cell_order argument must be a scalar string" = is.scalar(cell_order, "character"),
              "tile_order argument must be a scalar string" = is.scalar(tile_order, "character"),
              "coords_filter_list must be a filter list"    = is.null(coords_filter_list) || is(coords_filter_list, "tiledb_filter_list"),
              "offsets_filter_list must be a filter_list"   = is.null(offsets_filter_list) || is(offsets_filter_list, "tiledb_filter_list"),
              "validity_filter_list must be a_filter_list"  = is.null(validity_filter_list) || is(validity_filter_list, "tiledb_filter_list"),
              "'sparse' must be TRUE or FALSE"              = is.logical(sparse),
              "'allows_dups' must be TRUE or FALSE"         = is.logical(allows_dups),
              "'allows_dups' requires 'sparse' TRUE"        = !allows_dups || sparse)
    #if (allows_dups && !sparse) stop("'allows_dups' requires 'sparse' TRUE")

    attr_ptr_list <- if (is.list(attrs)) lapply(attrs, function(obj) slot(obj, "ptr")) else list()
    coords_filter_list_ptr <- if (!is.null(coords_filter_list)) coords_filter_list@ptr else NULL
    offsets_filter_list_ptr <- if (!is.null(offsets_filter_list)) offsets_filter_list@ptr else NULL
    validity_filter_list_ptr <- if (!is.null(validity_filter_list)) validity_filter_list@ptr else NULL

    ptr <- libtiledb_array_schema(ctx@ptr, domain@ptr, attr_ptr_list, cell_order, tile_order,
                                  coords_filter_list_ptr, offsets_filter_list_ptr,
                                  validity_filter_list_ptr, sparse, enumerations)
    libtiledb_array_schema_set_capacity(ptr, capacity)
    if (allows_dups) libtiledb_array_schema_set_allows_dups(ptr, TRUE)
    invisible(new("tiledb_array_schema", ptr = ptr))
}

tiledb_array_schema.from_array <- function(x, ctx = tiledb_get_context()) {
  stopifnot(`The 'ctx' argument must be a tiledb_ctx object` = is(ctx, "tiledb_ctx"),
            `The 'x' argument must be a valid array object` = !missing(x) && is.array(x))
  xdim <- dim(x)
  dims <- lapply(seq_len(xdim), function(i) {
    tiledb_dim(c(1L, xdim[i]), type = "INT32", ctx)
  })
  dom <- tiledb_domain(dims, ctx)
  #TODO: better datatype checking
  if (is.double(x)) {
    typestr <- "FLOAT64"
  } else if (is.integer(x)) {
    typestr <- "INT32"
  } else {
    stop(paste("invalid array type \"", typeof(x), "\""))
  }
  val <- tiledb_attr("", type = typestr, ctx)
  return(tiledb_array_schema(dom, c(val), ctx))
}

#' Raw display of an array schema object
#'
#' This method used the display method provided by the underlying
#' library.
#'
#' @param object An array_schema object
#' @export
setMethod("raw_dump",
          signature(object = "tiledb_array_schema"),
          definition = function(object) libtiledb_array_schema_dump(object@ptr))

#' Prints an array schema object
#'
#' @param object An array_schema object
#' @export
setMethod("show", signature(object = "tiledb_array_schema"),
          definition = function(object) {
    fl <- filter_list(object)
    nfc <- nfilters(fl$coords)
    nfo <- nfilters(fl$offsets)
    nfv <- nfilters(fl$validity)
    cat("tiledb_array_schema(\n    domain=", .as_text_domain(domain(object)), ",\n",
        "    attrs=c(\n        ", paste(sapply(attrs(object), .as_text_attribute, arrptr=object@arrptr), collapse=",\n        "), "\n    ),\n",
        "    cell_order=\"", cell_order(object), "\", ",
        "tile_order=\"", tile_order(object), "\", ",
        "capacity=", capacity(object), ", ",
        "sparse=",if (is.sparse(object)) "TRUE" else "FALSE", ", ",
        "allows_dups=", if (is.sparse(object)) allows_dups(object) else FALSE,
        if (nfc + nfo + nfv > 0) ",\n" else "\n",
        sep="")
    if (nfc > 0) cat("    coords_filter_list=", .as_text_filter_list(fl$coords), if (nfo + nfv > 0) "," else "", "\n", sep="")
    if (nfo > 0) cat("    offsets_filter_list=", .as_text_filter_list(fl$offsets), if (nfv > 0) ",\n" else "", sep="")
    if (nfv > 0) cat("    validity_filter_list=", .as_text_filter_list(fl$validity), "\n", sep="")
    cat(")\n", sep="")
})

#' @rdname generics
#' @export
setGeneric("domain", function(object, ...) standardGeneric("domain"))

#' Returns the `tiledb_domain` object associated with a given `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
#' domain(sch)
#'
#' @export
setMethod("domain", "tiledb_array_schema",
          function(object) {
            ptr <- libtiledb_array_schema_get_domain(object@ptr)
            tiledb_domain.from_ptr(ptr)
          })

#' @rdname generics
#' @export
setGeneric("dimensions", function(object, ...) standardGeneric("dimensions"))

#' Returns a list of `tiledb_dim` objects associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return a list of tiledb_dim objects
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32"),
#'                               tiledb_dim("d2", c(1L, 50L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
#' dimensions(dom)
#'
#' lapply(dimensions(dom), name)
#'
#' @export
setMethod("dimensions", "tiledb_array_schema",
          function(object) dimensions(domain(object)))

#' @rdname generics
#' @export
setGeneric("attrs", function(object, idx, ...) standardGeneric("attrs"))

#' Returns a list of all `tiledb_attr` objects associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @param idx index argument, currently unused.
#' @param ... Extra parameter for method signature, currently unused.
#' @return a list of tiledb_attr objects
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                                tiledb_attr("a2", type = "FLOAT64")))
#' attrs(sch)
#'
#' lapply(attrs(sch), datatype)
#'
#' @export
setMethod("attrs", signature("tiledb_array_schema"),
          function (object, ...) {
            attr_ptrs <- libtiledb_array_schema_attributes(object@ptr)
            attrs <- lapply(attr_ptrs, function(ptr) tiledb_attr.from_ptr(ptr))
            names(attrs) <- vapply(attrs, function(attr) {
              n <- tiledb::name(attr)
              return(ifelse(n == "__attr", "", n))
            }, character(1))
            return(attrs)
          })

#' Returns a `tiledb_attr` object associated with the `tiledb_array_schema` with a given name.
#'
#' @param object tiledb_array_schema
#' @param idx attribute name string
#' @param ... Extra parameter for method signature, currently unused.
#' @return a `tiledb_attr` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                                tiledb_attr("a2", type = "FLOAT64")))
#' attrs(sch, "a2")
#'
#' @export
setMethod("attrs", signature("tiledb_array_schema", "character"),
          function(object, idx, ...) {
            attrs <- tiledb::attrs(object)
            return(attrs[[idx]])
          })

#' Returns a `tiledb_attr` object associated with the `tiledb_array_schema` with a given index
#'
#' The attribute index is defined by the order the attributes were defined in the schema
#'
#' @param object tiledb_array_schema
#' @param idx attribute index
#' @param ... Extra parameter for method signature, currently unused.
#' @return a `tiledb_attr` object
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                           tiledb_attr("a2", type = "FLOAT64")))
#' attrs(sch, 2)
#'
#' @export
setMethod("attrs", signature("tiledb_array_schema", "numeric"),
          function(object, idx, ...) {
            attrs <- tiledb::attrs(object)
            return(attrs[[idx]])
          })

#' @rdname generics
#' @export
setGeneric("cell_order", function(object, ...) standardGeneric("cell_order"))

#' Returns the cell layout string associated with the `tiledb_array_schema`
#' @param object tiledb object
#' @export
setMethod("cell_order", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_get_cell_order(object@ptr)
          })

#' @rdname generics
#' @export
setGeneric("tile_order", function(object, ...) standardGeneric("tile_order"))

#' Returns the tile layout string associated with the `tiledb_array_schema`
#' @param object tiledb object
#' @export
setMethod("tile_order", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_get_tile_order(object@ptr)
          })

# ' @ export
#tiledb_filter_list.tiledb_array_schema <- function(object) {
#            coords_ptr <- libtiledb_array_schema_get_coords_filter_list(object@ptr)
#            offsets_ptr <- libtiledb_array_schema_offsets_filter_list(object@ptr)
#            return(c(coords = tiledb_filter_list.from_ptr(coords_ptr),
#                     offsets = tiledb_filter_list.from_ptr(offsets_ptr)))
#}

#' @rdname generics
#' @export
setGeneric("filter_list", function(object, ...) standardGeneric("filter_list"))

#' @rdname generics
#' @param x A TileDB Object
#' @export
setGeneric("filter_list<-", function(x, value) standardGeneric("filter_list<-"))

#' Returns the offsets and coordinate filter_lists associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return a list of tiledb_filter_list objects
#' @export
setMethod("filter_list", "tiledb_array_schema", function(object) {
  coords_ptr <- libtiledb_array_schema_get_coords_filter_list(object@ptr)
  offsets_ptr <- libtiledb_array_schema_get_offsets_filter_list(object@ptr)
  validity_ptr <- libtiledb_array_schema_get_validity_filter_list(object@ptr)
  return(c(coords = tiledb_filter_list.from_ptr(coords_ptr),
           offsets = tiledb_filter_list.from_ptr(offsets_ptr),
           validity = tiledb_filter_list.from_ptr(validity_ptr)))
})

# ' Set the Filter List for a TileDB Schema
# '
# ' @param x A TileDB Array Scheme
# ' @param value A TileDB Filter List
# ' @return The modified Array Schema object
# ' @ export
#setReplaceMethod("filter_list", "tiledb_array_schema", function(x, value) {
#  x@ptr <- libtiledb_array_schema_set_coords_filter_list(x@ptr, value@ptr)
#  x
#})
## -- need to provide setter for offsets and coords

#' Set a Filter List for Coordinate of a TileDB Schema
#'
#' @param sch A TileDB Array Schema object
#' @param fl A TileDB Filter List object
#' @return The modified Array Schema object
#' @export
tiledb_array_schema_set_coords_filter_list <- function(sch, fl) {
  stopifnot(`The 'sch' argument must be a tiledb_array_schema object` = is(sch, "tiledb_array_schema"),
            `The 'fl' argument must be a tiledb_filter_list object` = is(fl, "tiledb_filter_list"))
  sch@ptr <- libtiledb_array_schema_set_coords_filter_list(sch@ptr, fl@ptr)
  sch
}

#' Set a Filter List for Variable-Sized Offsets of a TileDB Schema
#'
#' @param sch A TileDB Array Schema object
#' @param fl A TileDB Filter List object
#' @return The modified Array Schema object
#' @export
tiledb_array_schema_set_offsets_filter_list <- function(sch, fl) {
  stopifnot(`The 'sch' argument must be a tiledb_array_schema object` = is(sch, "tiledb_array_schema"),
            `The 'fl' argument must be a tiledb_filter_list object` = is(fl, "tiledb_filter_list"))
  sch@ptr <- libtiledb_array_schema_set_offsets_filter_list(sch@ptr, fl@ptr)
  sch
}

#' Set a Filter List for Validity of a TileDB Schema
#'
#' @param sch A TileDB Array Schema object
#' @param fl A TileDB Filter List object
#' @return The modified Array Schema object
#' @export
tiledb_array_schema_set_validity_filter_list <- function(sch, fl) {
  stopifnot(`The 'sch' argument must be a tiledb_array_schema object` = is(sch, "tiledb_array_schema"),
            `The 'fl' argument must be a tiledb_filter_list object` = is(fl, "tiledb_filter_list"))
  sch@ptr <- libtiledb_array_schema_set_validity_filter_list(sch@ptr, fl@ptr)
  sch
}

#' @rdname generics
#' @export
setGeneric("is.sparse", function(object, ...) standardGeneric("is.sparse"))

#' Returns TRUE if the tiledb_array_schema is sparse, else FALSE
#'
#' @param object tiledb_array_schema
#' @return TRUE if tiledb_array_schema is sparse
#' @export
setMethod("is.sparse", "tiledb_array_schema",
          function(object) {
            libtiledb_array_schema_sparse(object@ptr)
          })

#' @rdname generics
#' @export
setGeneric("tiledb_ndim", function(object, ...) standardGeneric("tiledb_ndim"))

#' Return the number of dimensions associated with the `tiledb_array_schema`
#'
#' @param object tiledb_array_schema
#' @return integer number of dimensions
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                           tiledb_attr("a2", type = "FLOAT64")))
#' tiledb_ndim(sch)
#'
#' @export
setMethod("tiledb_ndim", "tiledb_array_schema",
          function(object) {
            dom <- tiledb::domain(object)
           return(tiledb_ndim(dom))
          })

#' Retrieve the dimension (domain extent) of the domain
#'
#' Only valid for integral (integer) domains
#'
#' @param x tiledb_array_schema
#' @return a dimension vector
#' @examples
#' \dontshow{ctx <- tiledb_ctx(limitTileDBCores())}
#' dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
#' sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
#'                                           tiledb_attr("a2", type = "FLOAT64")))
#' dim(sch)
#'
#' @export
dim.tiledb_array_schema <- function(x) dim(tiledb::domain(x))

# ' Sets toggle whether the array schema allows duplicate values or not.
# ' This is only valid for sparse arrays.
# '
# ' @param x tiledb_array_schema
# ' @param allows_dups logical value
# ' @return the logical value, invisibly
# ' @export



#' @rdname tiledb_array_schema_get_allows_dups
#' @export
setGeneric("allows_dups", function(x) standardGeneric("allows_dups"))

#' @rdname tiledb_array_schema_get_allows_dups
#' @export
setMethod("allows_dups", signature = "tiledb_array_schema", function(x) {
  stopifnot(is.sparse(x))
  libtiledb_array_schema_get_allows_dups(x@ptr)
})

#' Returns logical value whether the array schema allows duplicate values or not.
#' This is only valid for sparse arrays.
#'
#' @param x tiledb_array_schema
#' @return the logical value
#' @export
tiledb_array_schema_get_allows_dups <- function(x) {
  stopifnot(`The 'x' argument must be a tiledb_array_schema object` = is(x, "tiledb_array_schema"))
  libtiledb_array_schema_get_allows_dups(x@ptr)
}

#' @rdname tiledb_array_schema_set_allows_dups
#' @export
setGeneric("allows_dups<-", function(x, value) standardGeneric("allows_dups<-"))

#' @rdname tiledb_array_schema_set_allows_dups
#' @export
setMethod("allows_dups<-", signature = "tiledb_array_schema", function(x, value) {
  libtiledb_array_schema_set_allows_dups(x@ptr, value)
  x
})

#' Sets toggle whether the array schema allows duplicate values or not.
#' This is only valid for sparse arrays.
#'
#' @param x tiledb_array_schema
#' @param value logical value
#' @return the tiledb_array_schema object
#' @export
tiledb_array_schema_set_allows_dups <- function(x, value) {
  stopifnot(`The 'x' argument must be a tiledb_array_schema object` = is(x, "tiledb_array_schema"),
            `The 'value' argument must be a boolean` = is.logical(value))
  libtiledb_array_schema_set_allows_dups(x@ptr, value)
}

##' Get all Dimension and Attribute Names
##'
##' @param sch A TileDB Schema object
##' @return A character vector of dimension and attribute names
##' @export
tiledb_schema_get_names <- function(sch) {
  stopifnot(`The 'sch' argument must be a schema` = is(sch, "tiledb_array_schema"))
  dom <- tiledb::domain(sch)
  dims <- tiledb::dimensions(dom)
  ndims <- length(dims)
  dimnames <- sapply(dims, function(d) libtiledb_dim_get_name(d@ptr))

  attrs <- tiledb::attrs(sch)
  attrnames <- unname(sapply(attrs, function(a) libtiledb_attribute_get_name(a@ptr)))

  allnames <- c(dimnames, attrnames)
}

##' Get all Dimension and Attribute Types
##'
##' @param sch A TileDB Schema object
##' @return A character vector of dimension and attribute data types
##' @export
tiledb_schema_get_types <- function(sch) {
  stopifnot(`The 'sch' argument must be a schema` = is(sch, "tiledb_array_schema"))
  dom <- tiledb::domain(sch)
  dims <- tiledb::dimensions(dom)
  ndims <- length(dims)
  dimtypes <- sapply(dims, function(d) libtiledb_dim_get_datatype(d@ptr))

  attrs <- tiledb::attrs(sch)
  attrtypes <- unname(sapply(attrs, function(a) libtiledb_attribute_get_type(a@ptr)))

  alltypes <- c(dimtypes, attrtypes)
}

##' Get Dimension or Attribute Status
##'
##' Note that this function is an unexported internal function that can be called
##' using the colons as in \code{tiledb:::tiledb_schema_get_dim_attr_status(sch)}.
##'
##' @param sch A TileDB Schema object
##' @return An integer vector where each element corresponds to a schema entry,
##' and a value of one signals dimension and a value of two an attribute.
tiledb_schema_get_dim_attr_status <- function(sch) {
  stopifnot(`The 'sch' argument must be a schema` = is(sch, "tiledb_array_schema"))
  dom <- tiledb::domain(sch)
  dims <- tiledb::dimensions(dom)
  attrs <- tiledb::attrs(sch)
  return(c(rep(1L, length(dims)), rep(2L, length(attrs))))
}

##' Get Dimension or Attribute Status
##'
##' Note that this function is an unexported internal function that can be called
##' using the colons as in \code{tiledb:::tiledb_schema_get_enumeration_status(sch)}.
##'
##' @param sch A TileDB Schema object
##' @return An integer vector where each element corresponds to a schema entry,
##' and a value of one signals dimension and a value of two an attribute.
tiledb_schema_get_enumeration_status <- function(sch) {
    stopifnot("The 'sch' argument must be a schema" = is(sch, "tiledb_array_schema"))
    dom <- tiledb::domain(sch)
    dims <- tiledb::dimensions(dom)
    attrs <- tiledb::attrs(sch)
    return(c(rep(FALSE, length(dims)),
             sapply(attrs, tiledb_attribute_has_enumeration)))
}


# -- get and set tile capacity

#' @rdname tiledb_array_schema_get_capacity
#' @export
setGeneric("capacity", function(object) standardGeneric("capacity"))

#' @rdname tiledb_array_schema_set_capacity
#' @export
setGeneric("capacity<-", function(x, value) standardGeneric("capacity<-"))

#' @rdname tiledb_array_schema_get_capacity
#' @export
setMethod("capacity", signature = "tiledb_array_schema", function(object) {
  libtiledb_array_schema_get_capacity(object@ptr)
})

#' @rdname tiledb_array_schema_set_capacity
#' @export
setReplaceMethod("capacity", signature = "tiledb_array_schema", function(x, value) {
  libtiledb_array_schema_set_capacity(x@ptr, value)
  x
})

#' Retrieve schema capacity (for sparse fragments)
#'
#' Returns the \code{tiledb_array} schema tile capacity for sparse fragments.
#' @param object An \code{array_schema} object
#' @return The tile capacity value
#' @export
tiledb_array_schema_get_capacity <- function(object) {
  stopifnot(`The argument must be a tiledb_array_schema object` = is(object, "tiledb_array_schema"))
  libtiledb_array_schema_get_capacity(object@ptr)
}

#' Sets the schema capacity (for sparse fragments)
#'
#' Sets the \code{tiledb_array} schema tile capacity for sparse fragments.
#' @param x An \code{array_schema} object
#' @param value An integer or numeric value for the new tile capacity
#' @return The modified \code{array_schema} object
#' @export
tiledb_array_schema_set_capacity <- function(x, value) {
  stopifnot(`The first argument must be a tiledb_array_schema object` = is(x, "tiledb_array_schema"),
            `The second argumebt must be a int or numeric value` = is.numeric(value))
  libtiledb_array_schema_set_capacity(x@ptr, value)
  x
}




# -- Schema Correctness

#' @rdname tiledb_array_schema_check
#' @export
setGeneric("schema_check", function(object) standardGeneric("schema_check"))

#' @rdname tiledb_array_schema_check
#' @export
setMethod("schema_check", signature = "tiledb_array_schema", function(object) {
  libtiledb_array_schema_check(object@ptr)
})

## -- To be removed by May 2023 or later

#' @rdname tiledb_array_schema_check
#' @export
setGeneric("check", function(object) standardGeneric("check"))
#
#' @rdname tiledb_array_schema_check
#' @export
setMethod("check", signature = "tiledb_array_schema", function(object) {
    .Deprecated(msg="check() is deprecated, please use schema_check() instead.")
    libtiledb_array_schema_check(object@ptr)
})


#' Check the schema for correctness
#'
#' Returns the \code{tiledb_array} schema for correctness
#' @param object An \code{array_schema} object
#' @return The boolean value \code{TRUE} is returned for a correct
#' schema; for an incorrect schema an error condition is triggered.
#' @export
tiledb_array_schema_check <- function(object) {
  stopifnot(`The argument must be a tiledb_array_schema object` = is(object, "tiledb_array_schema"))
  libtiledb_array_schema_check(object@ptr)
}

#' Check the version of the array schema
#'
#' Returns the (internal) version of the \code{tiledb_array} schema
#' @param object An \code{array_schema} object
#' @return An integer value describing the internal schema format version
#' @export
tiledb_array_schema_version <- function(object) {
  stopifnot(`The argument must be a tiledb_array_schema object` = is(object, "tiledb_array_schema"))
  libtiledb_array_schema_version(object@ptr)
}


## -- convenience accessor `has_attribute` -- a little redundant as we already retrieve
##    all names and can check the returned set but a direct caller is a little lighter

#' Check a schema for a given attribute name
#'
#' @param schema A schema for a TileDB Array
#' @param attr A character variable with an attribute name
#' @return A boolean value indicating if the attribute exists in the schema
#' @export
has_attribute <- function(schema, attr) {
  stopifnot(`The 'schema' argument must be an array schema` = is(schema, "tiledb_array_schema"),
            `The 'attr' argument must be a character` = is.character(attr))
  libtiledb_array_schema_has_attribute(schema@ptr, attr)
}

## gather data about a scheme (SC 13273)

## internal helper function
.getFilterOption <- function(fltobj) {
    flt <- tiledb_filter_type(fltobj)
    if (flt %in% c("GZIP", "ZSTD", "LZ4", "BZIP2", "RLE")) {
        paste0("COMPRESSION_LEVEL", "=", tiledb_filter_get_option(fltobj, "COMPRESSION_LEVEL"))
    } else if (flt %in% "BIT_WIDTH_REDUCTION") {
        paste0("BIT_WIDTH_MAX_WINDOW", "=", tiledb_filter_get_option(fltobj, "BIT_WIDTH_MAX_WINDOW"))
    } else if (flt %in% "POSITIVE_DELTA") {
        paste0("POSITIVE_DELTA_MAX_WINDOW", "=", tiledb_filter_get_option(fltobj, "POSITIVE_DELTA_MAX_WINDOW"))
    } else {
        paste0("NA")
    }
}

#' Succinctly describe a TileDB array schema
#'
#' This is an internal function that is not exported.
#'
#' @param array A TileDB Array object
#' @return A list containing two data frames, one describing the overall array as well as one
#' with descriptions about dimensions and attributes in the schema
tiledb_schema_object <- function(array) {
    stopifnot(`Argument must be a 'tiledb_array'` = is(array, "tiledb_array"))

    ctx <- array@ctx
    uri <- array@uri
    sch <- schema(array)
    dom <- domain(sch)
    sparse <- is.sparse(sch)
    cell_order <- cell_order(sch)
    tile_order <- tile_order(sch)
    capacity <- tiledb_array_schema_get_capacity(sch)
    dupes <- if (sparse) allows_dups(sch) else FALSE
    filterlist <- filter_list(sch)
    n_coord <- nfilters(filterlist$coords)
    coords <- sapply(seq_len(n_coord), function(i) tiledb_filter_type(filterlist$coords[i-1]))
    coordopts <- sapply(seq_len(n_coord), function(i) .getFilterOption(filterlist$coords[i-1]))
    n_offsets <- nfilters(filterlist$offsets)
    offsets <- sapply(seq_len(n_offsets), function(i) tiledb_filter_type(filterlist$offsets[i-1]))
    offsetopts <- sapply(seq_len(n_offsets), function(i) .getFilterOption(filterlist$offsets[i-1]))
    n_validity <- nfilters(filterlist$validity)
    validity <- sapply(seq_len(n_validity), function(i) tiledb_filter_type(filterlist$validity[i-1]))
    validityopts <- sapply(seq_len(n_validity), function(i) .getFilterOption(filterlist$validity[i-1]))

    arrdesc <- data.frame(uri = uri,
                          type = if (sparse) "sparse" else "dense",
                          cell_order = cell_order,
                          tile_order = tile_order,
                          capacity = capacity,
                          allow_dupes = dupes,
                          coord_filters = paste0(coords, collapse=","),
                          coord_options = paste0(coordopts, collapse=","),
                          offset_filters = paste0(offsets, collapse=","),
                          offset_options = paste0(offsetopts, collapse=","),
                          validity_filters = paste0(validity, collapse=","),
                          validity_options = paste0(validityopts, collapse=",")
                          )

    dims <- dimensions(dom)
    dimnames <- sapply(dims, name)
    dimtypes <- sapply(dims, datatype)
    dimvarnum <- sapply(dims, cell_val_num)
    dimnullable <- sapply(dims, function(d) FALSE)
    dimdomains <- mapply(function(d, dtype) if (is.na(cell_val_num(d))) "NULL,NULL"
                                            else paste0(paste0(domain(d), if (grepl("INT", dtype)) "L" else ""), collapse=","),
                         dims, dimtypes)
    dimextent <- mapply(function(d, dtype) if (is.na(cell_val_num(d))) "NULL" else paste0(dim(d), if (grepl("INT", dtype)) "L" else ""),
                        dims, dimtypes)
    dimnfilt <- sapply(dims, function(d) nfilters(filter_list(d)))

    dimdesc <- data.frame(names = dimnames,
                          datatype = dimtypes,
                          nullable = dimnullable,
                          varnum = dimvarnum,
                          domain = dimdomains,
                          extent = dimextent,
                          nfilters = dimnfilt)

    attrs <- attrs(sch)
    attrnames <- sapply(attrs, name)
    attrtypes <- sapply(attrs, datatype)
    attrvarnum <- sapply(attrs, cell_val_num)
    attrnullable <- sapply(attrs, tiledb_attribute_get_nullable)
    attrnfilt <- sapply(attrs, function(a) nfilters(filter_list(a)))
    attrfltrs <- unname(sapply(attrs, function(a) {
        fltlst <- filter_list(a)
        if (nfilters(fltlst) == 0) ""
        else sapply(seq_len(nfilters(fltlst)), function(i) tiledb_filter_type(fltlst[i-1]))
    }))
    attrfltropts <- unname(sapply(attrs, function(a) {
        fltlst <- filter_list(a)
        if (nfilters(fltlst) == 0) ""
        else sapply(seq_len(nfilters(fltlst)), function(i) .getFilterOption(fltlst[i-1]))
    }))
    attrfillvals <- sapply(attrs, function(a) if (tiledb_attribute_get_nullable(a)) ""
                                              else format(tiledb_attribute_get_fill_value(a)))

    attrdesc <- data.frame(names = attrnames,
                           datatype = attrtypes,
                           nullable = attrnullable,
                           varnum = attrvarnum,
                           nfilters = attrnfilt,
                           filters = attrfltrs,
                           filtopts = attrfltropts,
                           fillvalue = attrfillvals)

    list(array=arrdesc, dom=dimdesc, attr=attrdesc)
}

## 'describe/create' hence dc. name is work in progress.  not exported yet
.describe_domain <- function(dom) {
    cat("dims <- c(")
    sapply(seq_len(nrow(dom)), function(i) {
        d <- dom[i,,drop=TRUE]
        cat(ifelse(i == 1, "", "          "),
            "tiledb_dim(name=\"", d$name, "\", ",
            "domain=c(", d$domain, "), ",
            "tile=", d$extent, ", ",
            "type=\"", d$datatype, "\")",
            ifelse(i < nrow(dom), ",", ")"),
            "\n",
            sep="")
    })
    cat("dom <- tiledb_domain(dims=dims)\n")
    invisible(NULL)
}

.show_filter_list <- function(filter, filter_options, prefix="") {
    fltrs <- strsplit(filter, ",")[[1]]
    opts <- strsplit(filter_options, ",")[[1]]
    txt <- paste0(prefix, "tiledb_filter_list(c(")
    for (i in seq_along(fltrs)) {
        if (opts[i] == "NA") {
            txt <- paste0(txt, "tiledb_filter(\"", fltrs[i], "\")")
        } else {
            option <- strsplit(opts[i], "=")[[1]]
            txt <- paste0(txt, "tiledb_filter_set_option(tiledb_filter(\"", fltrs[i],
                          "\"),\"", option[1], "\",", option[2], ")")
        }
        txt <- paste0(txt, if (i != length(fltrs)) ", " else ")")
    }
    txt <- paste0(txt, ")")
    txt
}

.describe_attrs <- function(attr) {
    cat("attrs <- c(")
    sapply(seq_len(nrow(attr)), function(i) {
        a <- attr[i,,drop=TRUE]
        cat(ifelse(i == 1, "", "           "),
            "tiledb_attr(name=\"", a$name, "\", ",
            "type=\"", a$datatype, "\", ",
            "ncells=", a$varnum, ", ",
            "nullable=", a$nullable, ", ",
            ifelse(a$filters != "", .show_filter_list(a$filters, a$filtopts), ""),
            ")",
            ifelse(i < nrow(attr), ",", ")"),
            "\n",
            sep="")
    })
    invisible(NULL)
}

.describe_schema <- function(sch) {
    cat("sch <- tiledb_array_schema(domain=dom, attrs=attrs, ",
        "cell_order=\"", sch$cell_order, "\", ",
        "tile_order=\"", sch$tile_order, "\", ",
        "sparse=", if (sch$type=="sparse") "TRUE" else "FALSE", ", ",
        "capacity=", sch$capacity, ", ",
        "allows_dups=", sch$allow_dupes, ", ",
        ifelse(sch$coord_filters != "",
               .show_filter_list(sch$coord_filters, sch$coord_options, "\n\t\t\t   coords_filter_list="),
               "coord_filters=NULL"), ", ",
        ifelse(sch$offset_filters != "",
               .show_filter_list(sch$offset_filters, sch$offset_options, "\n\t\t\t   offsets_filter_list="),
               "offset_filters=NULL"), ", ",
        ifelse(sch$validity_filters != "",
               .show_filter_list(sch$validity_filters, sch$validity_options, "\n\t\t\t   validity_filter_list="),
               "validity_filters=NULL"), ")\n",
        "tiledb_array_create(uri=tempfile(), schema=sch)) # or assign your URI here\n",
        sep="")
}

#' Describe a TileDB array schema via code to create it
#'
#' Note that this function is an unexported internal function that can be called
#' using the colons as in \code{tiledb:::describe(arr)}.
#'
#' @param arr A TileDB Array object
#' @return Nothing is returned as the function is invoked for the side effect
#' of printing the schema via a sequence of R instructions to re-create it.
describe <- function(arr) {
    stopifnot(`Argument must be a 'tiledb_array' object` = is(arr, "tiledb_array"))
    obj <- tiledb_schema_object(arr)
    .describe_domain(obj$dom)
    .describe_attrs(obj$attr)
    .describe_schema(obj$array)
}

#' Add an empty Enumeration to a Schema
#'
#' @param schema An Array Schema
#' @param attr An Attribute for which an empty Enumeration will be added
#' @param enum_name A character value with the Enumeration name
#' @param type_str A character value with the TileDB type, defaults to \sQuote{ASCII}
#' @param cell_val_num An integer with number values per cell, defaults to \code{NA_integer_} to
#' flag the \code{NA} value use for character values
#' @param ordered A logical value indicated standard \code{factor} (when \code{FALSE}, the default)
#' or \code{ordered} (when \code{TRUE})
#' @param ctx Optional tiledb_ctx object
#' @export
tiledb_array_schema_set_enumeration_empty <- function(schema, attr, enum_name,
                                                      type_str = "ASCII", cell_val_num = NA_integer_,
                                                      ordered = FALSE, ctx = tiledb_get_context()) {
    stopifnot("Argument 'schema' must be a 'tiledb_array_schema'" = is(schema, "tiledb_array_schema"),
              "Argument 'attr' must be a 'tiledb_attribute'" = is(attr, "tiledb_attr"),
              "Argument 'enum_name' must be character" = is.character(enum_name),
              "Argument 'type_str' must be character" = is.character(type_str),
              "Argument 'cell_val_num' must be integer" = is.integer(cell_val_num),
              "Argument 'ordered' must be logical" = is.logical(ordered),
              "Argument 'ctx' must be a 'tiledb_ctx'" = is(ctx, "tiledb_ctx"))
    schema@ptr <- libtiledb_array_schema_set_enumeration_empty(ctx@ptr, schema@ptr, attr@ptr,
                                                               enum_name, type_str, cell_val_num,
                                                               ordered)
    schema
}

#' Get the Current Domain of an Array Schema
#'
#' Note that 'CurrendDomain' object may be empty.
#' @param schema An Array Schema
#' @param ctx Optional tiledb_ctx object
#' @return A 'CurrendDomain' object
#' @export
tiledb_array_schema_get_current_domain <- function(schema, ctx = tiledb_get_context()) {
    stopifnot("Argument 'schema' must be a 'tiledb_array_schema'" = is(schema, "tiledb_array_schema"),
              "Argument 'ctx' must be a 'tiledb_ctx'" = is(ctx, "tiledb_ctx"))
    cdptr <- libtiledb_array_schema_get_current_domain(ctx@ptr, schema@ptr)
    new("tiledb_current_domain", ptr=cdptr)
}

#' Set a Current Domain of an Array Schema
#'
#' @param schema An Array Schema
#' @param cd An CurrendDomain object
#' @param ctx Optional tiledb_ctx object
#' @return Nothing is returned from this function (but an error, should it occur is reported)
#' @export
tiledb_array_schema_set_current_domain <- function(schema, cd, ctx = tiledb_get_context()) {
    stopifnot("Argument 'schema' must be a 'tiledb_array_schema'" = is(schema, "tiledb_array_schema"),
              "Argument 'cd' must be a 'tiledb_current_domain'" = is(cd, "tiledb_current_domain"),
              "Argument 'ctx' must be a 'tiledb_ctx'" = is(ctx, "tiledb_ctx"))
    libtiledb_array_schema_set_current_domain(ctx@ptr, schema@ptr, cd@ptr)
    invisible(NULL)
}
