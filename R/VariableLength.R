
##' @export
variable_length <- function(array, subarray, keys, debug=FALSE) {

  res <- firstTest(array, subarray, keys, debug)

  if (requireNamespace("data.table", quietly=TRUE)) {
    dt1 <- data.table::data.table(res[[1]])

    nr <- subarray[2] - subarray[1] + 1
    nc <- subarray[4] - subarray[3] + 1
    s <- seq(1, nr*nr, by=nc)
    dt2 <- data.table(V1=res[[2]][s],
                      V2=res[[2]][s + 1],
                      V3=res[[2]][s + 2],
                      V4=res[[2]][s + 3])
    ll <- list(dt1, dt2)
    names(ll) <- keys
    return(ll)
  } else {
    return(res)
  }

}
