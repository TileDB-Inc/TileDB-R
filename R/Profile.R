#
# Copyright (c) TileDB Inc. under the MIT License
#

#' An S4 class for a TileDB Profile object
#'
#' @slot ptr An external pointer to the underlying implementation
#' @exportClass tiledb_profile
setClass("tiledb_profile",
  slots = list(ptr = "externalptr")
)

#' Raw display of a profile object
#'
#' This method uses the display method provided by the underlying library.
#'
#' @param object A profile object
#' @export
setMethod(
  "raw_dump",
  signature(object = "tiledb_profile"),
  definition = function(object) libtiledb_profile_dump(object@ptr)
)

#' Create a 'tiledb_profile' object
#'
#' @param name (optional) Name for the profile
#' @param dir (optional) Directory to create the profile in
#' @return A new 'tiledb_profile' object
#' @export
tiledb_profile <- function(name = NULL, dir = NULL) {
  stopifnot(`The 'name' for the profile must be null or a character type` = is.null(name) || is.character(name))
  stopifnot(`The 'dir' for the profile must be null or a character type` = is.null(name) || is.character(name))
  ptr <- libtiledb_profile_new(name, dir)
  profile <- new("tiledb_profile", ptr = ptr)
  return(profile)
}

#' Load a saved 'tiledb_profile' object
#'
#' @param name (optional) Name of the profile to load
#' @param dir (optional) Directory where the profile to load is saved
#' @return The loaded 'tiledb_profile' object
#' @export
tiledb_profile_load <- function(name = NULL, dir = NULL) {
  stopifnot(`The 'name' for the profile must be null or a character type` = is.null(name) || is.character(name))
  stopifnot(`The 'dir' for the profile must be null or a character type` = is.null(name) || is.character(name))
  ptr <- libtiledb_profile_load(name, dir)
  profile <- new("tiledb_profile", ptr = ptr)
  return(profile)
}

#' Remove a saved 'tiledb_profile'
#'
#' @param name (optional) Name of the profile to remove
#' @param dir (optional) Directory where the profile to remove is saved
#' @export
tiledb_profile_remove <- function(name = NULL, dir = NULL) {
  stopifnot(`The 'name' for the profile must be null or a character type` = is.null(name) || is.character(name))
  stopifnot(`The 'dir' for the profile must be null or a character type` = is.null(name) || is.character(name))
  libtiledb_profile_remove(name, dir)
  return(invisible(NULL))
}

#' Get the name of a 'tiledb_profile' object
#'
#' @param profile A TileDB profile object
#' @return The name of the 'tiledb_profile' object
#' @export
tiledb_profile_name <- function(profile) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  name <- libtiledb_profile_name(profile@ptr)
  return(name)
}

#' Get the directory of a 'tiledb_profile' object
#'
#' @param profile A TileDB profile object
#' @return The directory of the 'tiledb_profile' object
#' @export
tiledb_profile_dir <- function(profile) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  dir <- libtiledb_profile_dir(profile@ptr)
  return(dir)
}

#' Set a parameter on the 'tiledb_profile' object
#'
#' @param profile A TileDB profile object
#' @param param The key for the new parameter
#' @param value The value for the new parameter
#' @export
tiledb_profile_set_param <- function(profile, param, value) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  stopifnot(`The 'param' arugment must have character type` = is.character(param))
  stopifnot(`The 'value' arugment must have character type` = is.character(value))
  libtiledb_profile_set_param(profile@ptr, param, value)
  return(invisible(NULL))
}

#' Get the value of a parameter set on the 'tiledb_profile' object
#'
#' @param profile A TileDB profile object
#' @param param The key for the parameter to fetch
#' @return The value of the requested parameter or NULL if no such parameter exists
#' @export
tiledb_profile_get_param <- function(profile, param) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  stopifnot(`The 'param' arugment must have character type` = is.character(param))
  value <- libtiledb_profile_get_param(profile@ptr, param)
  return(value)
}

#' Save the 'tiledb_profile' object
#'
#' This will save the 'tiledb_profile' with the name and directory set at creation.
#'
#' @param profile The 'tiledb_profile' object to save
#' @export
tiledb_profile_save <- function(profile) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  libtiledb_profile_save(profile@ptr)
  return(invisible(NULL))
}
