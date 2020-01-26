
##' @export
read_variable_length <- function(array, subarray, keys, debug=FALSE) {

  res <- read_varlength_array(array, subarray, keys, debug)

  if (requireNamespace("data.table", quietly=TRUE)) {
    nr <- subarray[2] - subarray[1] + 1
    nc <- subarray[4] - subarray[3] + 1
    s <- seq(1, nr*nc, by=nc)
    n <- length(res)
    ll <- vector(mode="list", length=n)
    for (j in 1:n) {
      dt <- data.table::data.table(res[[j]][s])
      for (i in seq_along(2:nc)) {
        dt <- cbind(dt, data.table::data.table(res[[j]][s + i]))
      }
      colnames(dt) <- paste("V", 1:nc, sep="")
      ll[[j]] <- dt
    }
    names(ll) <- keys
    return(ll)
  } else {
    return(res)
  }

}

##' @export
write_variable_length <- function(uri, listobject, debug=FALSE) {
  ## we extract names simply because it is easier
  names <- names(listobject)
  n <- length(listobject)

  ## this is very adhoc while we test
  #if (dir.exists(uri)) {
  #  unlink(uri, recursive=TRUE, force=TRUE)
  #}
  #dir.create(uri)
  #create_varlength_array(uri)

  ## pass list of objects (and their names) down
  res <- write_varlength_array(uri, listobject, names, debug)

}
