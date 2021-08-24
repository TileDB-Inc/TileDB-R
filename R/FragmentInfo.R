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

#' An S4 class for a TileDB fragment info object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_fragment_info
setClass("tiledb_fragment_info",
         slots = list(ptr = "externalptr"))

tiledb_fragment_info.from_ptr <- function(ptr) {
    stopifnot(is(ptr, "externalptr"))
    return(new("tiledb_fragment_info", ptr = ptr))
}

#' Constructs a `tiledb_fragment_info` object
#'
#' @param uri an character variable with the URI of the
#' array for which fragment info is request
#' @param ctx tiledb_ctx object (optional)
#' @return tiledb_fragment_info object
#' @export tiledb_fragment_info
tiledb_fragment_info <- function(uri, ctx = tiledb_get_context()) {
    stopifnot(`argument ctx must be a tiledb_ctx object` = is(ctx, "tiledb_ctx"),
              `argument uri must be a string scalar` = is.scalar(uri, "character"))

    ptr <- libtiledb_fragment_info(ctx@ptr, uri)
    return(new("tiledb_fragment_info", ptr = ptr))
}

#' Return a fragment info URI given its index
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A character variable with URI
#' @export
tiledb_fragment_info_uri <- function(object, fid) {
    libtiledb_fragment_info_uri(object@ptr, fid)
}

#' Return a fragment info non-empty domain from index
#'
#' TODO: Rework with type information
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @param did A domain index
#' @param typestr An optional character variable describing the data type which will
#' be accessed from the schema if missinh
#' @return A TileDB Domain object
#' @export
tiledb_fragment_info_get_non_empty_domain_index <- function(object, fid, did, typestr) {
    if (missing(typestr)) {
        uri <- dirname(libtiledb_fragment_info_uri(object@ptr, fid))
        typestr <- datatype( dimensions(domain(schema(uri)))[[did+1]] )
    }
    libtiledb_fragment_info_get_non_empty_domain_index(object@ptr, fid, did, typestr)
}

#' Return a fragment info non-empty domain from name
#'
#' TODO: Rework with type information
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @param dim_name A character variable with the dimension name
#' @param typestr An optional character variable describing the data type which will
#' be accessed from the schema if missinh
#' @return A TileDB Domain object
#' @export
tiledb_fragment_info_get_non_empty_domain_name <- function(object, fid, dim_name, typestr) {
    if (missing(typestr)) {
        uri <- dirname(libtiledb_fragment_info_uri(object@ptr, fid))
        names <- sapply(dimensions(domain(schema(uri))), name)
        ind <- which(names == dim_name)
        typestr <- datatype( dimensions(domain(schema(uri)))[[ind]] )
    }
    libtiledb_fragment_info_get_non_empty_domain_name(object@ptr, fid, dim_name, typestr)
}

#' Return a fragment info non-empty domain variable from index
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @param did A domain index
#' @return A character vector with two elements
#' @export
tiledb_fragment_info_get_non_empty_domain_var_index <- function(object, fid, did) {
    libtiledb_fragment_info_get_non_empty_domain_var_index(object@ptr, fid, did)
}

#' Return a fragment info non-empty domain variable from name
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @param dim_name A character variable with the dimension name
#' @return A character vector with two elements
#' @export
tiledb_fragment_info_get_non_empty_domain_var_name <- function(object, fid, dim_name) {
    libtiledb_fragment_info_get_non_empty_domain_var_name(object@ptr, fid, dim_name)
}

#' Return a fragment info number of fragments
#'
#' @param object A TileDB fragment info object
#' @return A numeric variable with the number of fragments
#' @export
tiledb_fragment_info_get_num <- function(object) {
    libtiledb_fragment_info_num(object@ptr)
}

#' Return a fragment info fragment size for a given fragment index
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A numeric variable with the number of fragments
#' @export
tiledb_fragment_info_get_size <- function(object, fid) {
    libtiledb_fragment_info_size(object@ptr, fid)
}

#' Return if a fragment info index is dense
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A logical value indicating if the fragment is dense
#' @export
tiledb_fragment_info_dense <- function(object, fid) {
    libtiledb_fragment_info_dense(object@ptr, fid)
}

#' Return if a fragment info index is sparse
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A logical value indicating if the fragment is sparse
#' @export
tiledb_fragment_info_sparse <- function(object, fid) {
    libtiledb_fragment_info_sparse(object@ptr, fid)
}

#' Return a fragment info timestamp range for a given fragment index
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A Datetime vector with two elements for the range
#' @export
tiledb_fragment_info_get_timestamp_range <- function(object, fid) {
    libtiledb_fragment_info_timestamp_range(object@ptr, fid)
}

#' Return a fragment info number of cells for a given fragment index
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A numeric value with the number of cells
#' @export
tiledb_fragment_info_get_cell_num <- function(object, fid) {
    libtiledb_fragment_info_cell_num(object@ptr, fid)
}

#' Return a fragment info version for a given fragment index
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A integer value value with the version
#' @export
tiledb_fragment_info_get_version <- function(object, fid) {
    libtiledb_fragment_info_version(object@ptr, fid)
}

#' Return if a fragment info index has consolidated metadata
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A logical value indicating consolidated metadata
#' @export
tiledb_fragment_info_has_consolidated_metadata <- function(object, fid) {
    libtiledb_fragment_info_has_consolidated_metadata(object@ptr, fid)
}

#' Return fragment info number of unconsolidated metadata
#'
#' @param object A TileDB fragment info object
#' @return A numeric value with the number of unconsolidated metadata
#' @export
tiledb_fragment_info_get_unconsolidated_metadata_num <- function(object) {
    libtiledb_fragment_info_unconsolidated_metadata_num(object@ptr)
}

#' Return the number of fragment info elements to be vacuumed
#'
#' @param object A TileDB fragment info object
#' @return A numeric value with the number of to be vacuumed fragments
#' @export
tiledb_fragment_info_get_to_vacuum_num <- function(object) {
    libtiledb_fragment_info_to_vacuum_num(object@ptr)
}

#' Return fragment info URI of the to be vacuumed index
#'
#' @param object A TileDB fragment info object
#' @param fid A fragment object index
#' @return A character variable with the URI of the be vacuumed index
#' @export
tiledb_fragment_info_get_to_vacuum_uri <- function(object, fid) {
    libtiledb_fragment_info_to_vacuum_uri(object@ptr, fid)
}

#' Dump the fragment info to console
#'
#' @param object A TileDB fragment info object
#' @return Nothing is returned, as a side effect the fragment info is displayed
#' @export
tiledb_fragment_info_dump <- function(object) {
    libtiledb_fragment_info_dump(object@ptr)
}
