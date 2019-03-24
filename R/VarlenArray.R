#' Calculate offsets
#' @export
##' @param x An array or list (potentially with dimensions) of arrays
varlen_offsets <- function(x) {
    if (is.character(x)) {
        out = cumsum(nchar(x))
    } else if (is.list(x)) {
        lens = lengths(x)
        eltype = varlen_list_eltype(x)
        if (eltype == "integer" || eltype == "logical") {
            out = 4L * lens
        } else if (eltype == "double") {
            out = 8L * lens
        } else {
            stop("Variable length arrays that are not character arrays, must be of the type 'logical', 'integer' or 'numeric'.")
        }
        out = cumsum(as.vector(out))
    } else {
        stop("Support for variable length attributes covers only character arrays and lists of 'logical', 'integer' or 'numeric' types at this time.")
    }
    out = c(0, out[- length(out)])
    out
}
