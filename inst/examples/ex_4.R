library(tiledb)

## trying to mimick the simple heterogenous_domains_ex2.cpp example
## based the C++ API test

## Name of the array to create.
array_name <- "ex_4"

## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {

  dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1.0, 20.0), 5.0, type="FLOAT64"),
                                tiledb_dim("d2", c(1L, 30L), 5L, type="INT32")))

  myorder <- "COL_MAJOR"
  #cat("Using cell and tile order:", myorder, "\n")
  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom,
                                attrs = c(tiledb_attr("a", type = "INT32")),
                                cell_order = myorder, tile_order = myorder,
                                sparse=TRUE)

  ## Create the (empty) array on disk.
  invisible(tiledb_array_create(uri, schema))
}

write_array <- function(uri) {
  d1 <- c(1.1, 1.2, 1.3, 1.4)
  d2 <- c(1L, 2L, 3L, 4L)
  a <- c(11L, 21L, 31L, 41L)

  x <- tiledb_array(uri = uri)
  qry <- tiledb_query(x, "WRITE")
  qry <- tiledb_query_set_layout(qry, "UNORDERED")

  qry <- tiledb_query_set_buffer(qry, "d1", d1)
  qry <- tiledb_query_set_buffer(qry, "d2", d2)
  qry <- tiledb_query_set_buffer(qry, "a", a)

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  if (tiledb_query_status(qry) != "COMPLETE") {
    stop("error in write query (not 'COMPLETE')", call.=FALSE)
  }
  invisible(NULL)
}

read_array <- function(uri) {
  x <- tiledb_array(uri = uri)
  d1r <- vector(mode="numeric", length=4)
  d2r <- vector(mode="integer", length=4)
  ar <- vector(mode="integer", length=4)
  qry <- tiledb_query(x, "READ")
  qry <- tiledb_query_set_buffer(qry, "d1", d1r)
  qry <- tiledb_query_set_buffer(qry, "d2", d2r)
  qry <- tiledb_query_set_buffer(qry, "a", ar)
  tiledb_query_submit(qry)
  print(df <- data.frame(d1=d1r, d2=d2r, a=ar))
  invisible(NULL)
}

read_array_subset <- function(uri) {
  x <- tiledb_array(uri = uri)
  sch <- schema(x)
  d1r <- vector(mode="numeric", length=5)
  d2r <- vector(mode="integer", length=5)
  ar <- vector(mode="integer", length=5)

  qry <- tiledb_query(x, "READ")
  qry <- tiledb_query_set_buffer(qry, "d1", d1r)
  qry <- tiledb_query_set_buffer(qry, "d2", d2r)
  qry <- tiledb_query_set_buffer(qry, "a", ar)

  qry <- tiledb_query_add_range(qry, sch, "d1", 1.15, 1.35)
  qry <- tiledb_query_add_range(qry, sch, "d2", 2L, 4L)
  qry <- tiledb_query_submit(qry)

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  if (tiledb_query_status(qry) != "COMPLETE") {
    stop("error in write query (not 'COMPLETE')", call.=FALSE)
  }
  n <- tiledb_query_result_buffer_elements(qry, "d1")

  print(df <- data.frame(d1=d1r, d2=d2r, a=ar)[1:n,])

  invisible(NULL)
}


## Check if the array already exists.
if (tiledb_object_type(uri) == "ARRAY") unlink(uri, recursive=TRUE)
message("Creating and writing.")
create_array(uri)
write_array(uri)
read_array(uri)
read_array_subset(uri)
cat("Done\n")
