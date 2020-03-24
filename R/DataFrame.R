
## helper functions for data frame, roughly modeled on what python has

fromDataFrame <- function(obj, uri) {
  dims <- dim(obj)

  dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, dims[1]), dims[1], "INT32"))

  ## turn factor columns in char columns
  factcols <- grep("factor", sapply(obj, class))
  if (length(factcols) > 0) {
    for (i in factcols) obj[,i] <- as.character(obj[,i])
  }

  charcols <- grep("character", sapply(obj, class))

  makeAttr <- function(ind) {
    col <- obj[,ind]
    cl <- class(col)
    if (cl == "integer")
      tp <- "INT32"
    else if (cl == "numeric")
      tp <- "FLOAT64"
    else if (cl == "character")
      tp <- "CHAR"
    else
      stop("Currently unsupported type: ", cl)
    tiledb_attr(colnames(obj)[ind], type=tp, ncells=ifelse(tp=="CHAR",NA_integer_,1))
  }
  attributes <- sapply(seq_len(dims[2]), makeAttr)

  schema <- tiledb_array_schema(dom, attrs = attributes)

  tiledb_array_create(uri, schema)

  df <- tiledb_dense(uri)
  df[] <- obj
  invisble(NULL)
}

.testFromDataFrame <- function(obj, uri) {
  if (dir.exists(uri)) unlink(uri, recursive=TRUE)
  fromDataFrame(obj, uri)

  df <- tiledb_dense(uri, as.data.frame=TRUE)
  df[]
}
