#' @rdname schema-tiledb_array-method
#' @export
setGeneric("schema", function(object, ...) standardGeneric("schema"))

#' @rdname return.data.frame-tiledb_array-method
#' @param ... Currently unused
# ' @export
setGeneric("return.data.frame", function(object, ...) standardGeneric("return.data.frame"))

#' @rdname return.data.frame-set-tiledb_array-method
# ' @export
setGeneric("return.data.frame<-", function(x, value) standardGeneric("return.data.frame<-"))

#' @rdname attrs-set-tiledb_array-method
#' @export
setGeneric("attrs<-", function(x, value) standardGeneric("attrs<-"))
