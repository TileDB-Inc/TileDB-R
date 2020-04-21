suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

## work-in-progress, does not yet work

domrowtype <- "DATETIME_NS"
attrowtype <- "DATETIME_US"

## Name of the array to create.
array_name <- "ex_3"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(0, 1e12), 1, type = domrowtype)))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
                                               tiledb_attr("d1", type = attrowtype)),
                                sparse=TRUE)
  tiledb_array_create(uri, schema)
}

write_array <- function(uri) {
  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "WRITE")
  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
  qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "UNORDERED")

  rows <- 1:10 #ISOdatetime(2020,1,1,0,0,0) + (1:10)*60*60 - as.numeric(ISOdatetime(2020,1,1,0,0,0))
  rows <- nanotime(1) + 0:9
  bufptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domrowtype, 10)
  bufptr <-tiledb:::libtiledb_query_buffer_assign_ptr(bufptr, domrowtype, rows)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "rows", bufptr)

  #data <- list(seq(1:10), ISOdatetime(2020,1,1,0,0,0) + 0:9)
  a1data <- seq(1:10)
  d1data <- switch(attrowtype,
                   DATETIME_DAY = as.Date("2020-01-01") + 0:9,
                   DATETIME_SEC = as.POSIXct("2020-01-01 00:00:00") + 0:9,
                   DATETIME_MS = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123,
                   DATETIME_US = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123456)

  buf1ptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, "INT32", 10)
  buf1ptr <-tiledb:::libtiledb_query_buffer_assign_ptr(buf1ptr, "INT32", a1data)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "a1", buf1ptr)

  buf2ptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, attrowtype, 10)
  buf2ptr <-tiledb:::libtiledb_query_buffer_assign_ptr(buf2ptr, attrowtype, d1data)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "d1", buf2ptr)

  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)
  invisible(NULL)
}

read_array <- function(uri) {
  arr <- tiledb_sparse(uri)

  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "READ")
  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")

  qryptr <- tiledb:::libtiledb_query_add_range_with_type(qryptr, 0, domrowtype, 2, 7)

  bufptr0 <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domrowtype, 10)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "rows", bufptr0)

  d1r <- vector(mode="integer", length=10)

  qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a1", d1r)
  ##qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "d1", d2r)
  bufptr <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, attrowtype, 10)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "d1", bufptr)

  #qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
  qryptr <- tiledb:::libtiledb_query_submit(qryptr)

  tiledb:::libtiledb_array_close(arrptr)

  rows <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr0)
  d2r <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr)
  print(data.frame(rows,d1r,d2r))

}

#read_array_df <- function(uri) {
#  arr <- tiledb_sparse(uri, as.data.frame=TRUE)
#  print(arr[])
#}

set.seed(42)
#if (tiledb_object_type(array_name) != "ARRAY") {
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_array(uri)
read_array(uri)
#read_array_df(uri)
