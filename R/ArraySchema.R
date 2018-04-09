#' @exportClass ArraySchema
setClass("ArraySchema",
         slots = list(ptr = "externalptr"))

#' @export
ArraySchema <- function() {
  new("ArraySchema", ptr = NULL)
}