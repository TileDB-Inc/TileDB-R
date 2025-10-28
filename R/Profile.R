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

setMethod(
  "raw_dump",
  signature(object = "tiledb_profile"),
  definition = function(object) libtiledb_profile_dump(object@ptr)
)

#' Create a 'tiledb_profile' object
#'
#' @param name (optional) Name for the profile.
#' @param dir (optional) Directory to create the profile in.
#' @return 'tiledb_profile' object
#' @export tiledb_profile
tiledb_profile <- function(name = NULL, dir = NULL) {
  stopifnot(`The 'name' for the profile must be null or a character type` = is.null(name) || is.character(name))
  stopifnot(`The 'dir' for the profile must be null or a character type` = is.null(name) || is.character(name))
  ptr <- libtiledb_profile_new(name, dir)
  profile <- new("tiledb_profile", ptr = ptr)
  return(profile)
}


tiledb_profile_load <- function(name = NULL, dir = NULL) {
  stopifnot(`The 'name' for the profile must be null or a character type` = is.null(name) || is.character(name))
  stopifnot(`The 'dir' for the profile must be null or a character type` = is.null(name) || is.character(name))
  ptr <- libtiledb_profile_load(name, dir)
  profile <- new("tiledb_profile", ptr = ptr)
  return(profile)
}

tiledb_profile_remove <- function(name = NULL, dir = NULL) {
  stopifnot(`The 'name' for the profile must be null or a character type` = is.null(name) || is.character(name))
  stopifnot(`The 'dir' for the profile must be null or a character type` = is.null(name) || is.character(name))
  libtiledb_profile_remove(name, dir)
  return(invisible(NULL))
}

tiledb_profile_name <- function(profile) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  name <- libtiledb_profile_name(profile@ptr)
  return(name)
}

tiledb_profile_dir <- function(profile) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  dir <- libtiledb_profile_dir(profile@ptr)
  return(dir)
}

tiledb_profile_set_param <- function(profile, param, value) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  stopifnot(`The 'param' arugment must have character type` = is.character(param))
  stopifnot(`The 'value' arugment must have character type` = is.character(value))
  libtiledb_profile_set_param(profile@ptr, param, value)
  return(invisible(NULL))
}

tiledb_profile_get_param <- function(profile, param) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  stopifnot(`The 'param' arugment must have character type` = is.character(param))
  value <- libtiledb_profile_get_param(profile@ptr, param)
  return(value)
}

tiledb_profile_save <- function(profile) {
  stopifnot(`The 'profile' argument must be a tiledb_profile object` = is(profile, "tiledb_profile"))
  libtiledb_profile_save(profile)
  return(invisible(NULL))
}
