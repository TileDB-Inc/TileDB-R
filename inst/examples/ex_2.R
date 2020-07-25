
library(tiledb)

## Name of the array to create.
array_name <- "ex_2"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  ## The array will be 10x5 with dimensions "rows" and "cols", with domains [1,10] and [1,5].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1, #ISOdatetime(2010,1,1,0,0,0),
                                                     1e15), #ISOdatetime(2029,12,31,23,59,59.999)),
                                           10,
                                           #type = "DATETIME_MS"),
                                           type = "FLOAT64"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT32") ) )
  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32"),
                                               tiledb_attr("b", type = "FLOAT64"),
                                               tiledb_attr("d", type = "DATETIME_DAY"),
                                               tiledb_attr("e", type = "DATETIME_MS")),
                                sparse=TRUE)

  ## Create the (empty) array on disk.
  invisible(tiledb_array_create(uri, schema))
}

write_array <- function(uri) {
  data <- list(seq(1:10),
               as.double(seq(101,110)),
               ##c(letters[1:26], "brown", "fox", LETTERS[1:22])[1:10],
               as.Date("2020-01-01") + cumsum(runif(10)*5),
               ISOdatetime(2020,1,1,6,0,0) + cumsum(rnorm(10) * 1e5))
  ## Open the array and write to it.
  A <- tiledb_array(uri = uri)
  rows <- ISOdatetime(2020,1,1,0,0,0) + (1:10)
  cols <- rep(1L, 10)
  A[rows,cols] <- data
}

read_array <- function(uri) {
  ## Open the array and read from it.
  A <- tiledb_array(uri = uri)
  data <- A[]
  show(data)

}

read_as_df <- function(uri) {
  A <- tiledb_array(uri = uri, as.data.frame = TRUE)
  selected_ranges(A) <- list(cbind(ISOdatetime(2020,1,1,0,0,4),
                                   ISOdatetime(2020,1,1,0,0,6)),
                             cbind(1L,1L))
  data <- A[]
  show(data)
}

read_via_query <- function(uri) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "READ")

  rows <- numeric(10)
  cols <- integer(10)
  tiledb_query_set_buffer(qry, "rows", rows)
  tiledb_query_set_buffer(qry, "cols", cols)

  a <- integer(10)
  b <- numeric(10)
  tiledb_query_set_buffer(qry, "a", a)
  tiledb_query_set_buffer(qry, "b", b)

  dptr <- tiledb_query_buffer_alloc_ptr(qry, "DATETIME_DAY", 10)
  mptr <- tiledb_query_buffer_alloc_ptr(qry, "DATETIME_MS", 10)
  tiledb_query_set_buffer_ptr(qry, "d", dptr)
  tiledb_query_set_buffer_ptr(qry, "e", mptr)

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  stopifnot(tiledb_query_status(qry)=="COMPLETE")

  n <- tiledb_query_result_buffer_elements(qry, "a")
  d <- tiledb_query_get_buffer_ptr(dptr)
  m <- tiledb_query_get_buffer_ptr(mptr)
  print(data.frame(a=a, b=b, d=d, m=m)[1:n,])
}

set.seed(42)

if (dir.exists(uri)) {
  cat("Nuking existing array\n")
  unlink(uri, recursive=TRUE)
}

if (!dir.exists(uri)) {
  create_array(uri)
}

write_array(uri)
read_array(uri)
read_as_df(uri)
read_via_query(uri)

cat("Done.\n")
