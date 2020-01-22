
##' @export
variable_length <- function(array, subarray, keys, debug=FALSE) {

  res <- firstTest(array, subarray, keys, debug)

  if (requireNamespace("data.table", quietly=TRUE)) {
    nr <- subarray[2] - subarray[1] + 1
    nc <- subarray[4] - subarray[3] + 1
    s <- seq(1, nr*nc, by=nc)
    n <- length(res)
    ll <- vector(mode="list", length=n)
    for (j in 1:n) {
      dt <- data.table::data.table(res[[j]][s])
      for (i in seq_along(2:nc)) {
        dt <- data.table:::cbind.data.table(dt, data.table::data.table(res[[j]][s + i]))
      }
      ll[[j]] <- dt
    }
    names(ll) <- keys
    return(ll)
  } else {
    return(res)
  }

}
