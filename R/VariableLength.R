
##' @export
read_variable_length <- function(array, key, subarray, debug=FALSE) {

  res <- read_varlength_array(array, key, subarray, debug)

  ## An earlier version returneda list per keys, and the code below still
  ## works that way and is worth keeping.  So we just wrap the per-key
  ## result into a list one length one
  res <- list(key=res)

  if (requireNamespace("data.table", quietly=TRUE)) {
    nr <- subarray[2] - subarray[1] + 1
    nc <- subarray[4] - subarray[3] + 1
    s <- seq(1, nr*nc, by=nc)
    n <- length(res)        # code is general enough for list return
    ll <- vector(mode="list", length=n)
    for (j in 1:n) {
      dt <- data.table::data.table(res[[j]][s])
      for (i in seq_along(2:nc)) {
        dt <- cbind(dt, data.table::data.table(res[[j]][s + i]))
      }
      colnames(dt) <- paste("V", 1:nc, sep="")
      ll[[j]] <- dt
    }
    if (length(ll) == 1) {
      names(ll) <- key
      return(ll[[1]])
    } else {
      return(ll)
    }
  } else {
    return(res)
  }

}

##' @export
write_variable_length <- function(uri, listobject, debug=FALSE) {
  ## we extract names simply because it is easier
  names <- names(listobject)
  #n <- length(listobject)

  ## pass list of objects (and their names) down
  res <- write_varlength_array(uri, listobject, names, debug)

}
