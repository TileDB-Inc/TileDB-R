
##' @export
variable_length <- function(array, subarray, keys, debug=FALSE) {

  res <- firstTest(array, subarray, keys, debug)

  if (requireNamespace("data.table", quietly=TRUE)) {
    dt1 <- data.table::data.table(res[[1]])
    nr <- subarray[2] - subarray[1] + 1
    nc <- subarray[4] - subarray[3] + 1
    s <- seq(1, nr*nc, by=nc)
    n <- length(res)
    ll <- vector(mode="list", length=n)
    ll[[1]] <- dt1
    for (j in 2:n) {
      dt2 <- data.table::data.table(V1=res[[j]][s])
      for (i in seq_along(2:nc)) {
        dt2 <- data.table:::cbind.data.table(dt2, data.table::data.table(V2=res[[j]][s + i]))
      }
      ll[[j]] <- dt2
    }
    names(ll) <- keys
    return(ll)
  } else {
    return(res)
  }

}
