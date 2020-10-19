suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

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
  invisible(tiledb_array_create(uri, schema))
}

write_array_lowlevel <- function(uri) {
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

write_array_query <- function(uri) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "WRITE")
  qry <- tiledb_query_set_layout(qry, "UNORDERED")

  rows <- nanotime(1) + 0:9
  rowptr <- tiledb_query_create_buffer_ptr(qry, domrowtype, rows)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowptr)

  a1data <- seq(1,10)
  a1ptr <- tiledb_query_create_buffer_ptr(qry, "INT32", a1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "a1", a1ptr)

  d1data <- switch(attrowtype,
                   DATETIME_DAY = as.Date("2020-01-01") + 0:9,
                   DATETIME_SEC = as.POSIXct("2020-01-01 00:00:00") + 0:9,
                   DATETIME_MS = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123,
                   DATETIME_US = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123456)
  d1ptr <- tiledb_query_create_buffer_ptr(qry, attrowtype, d1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", d1ptr)

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)

  invisible(NULL)
}

write_array <- function(uri) {
  rows <- nanotime(1) + 0:9
  a1data <- seq(1:10)
  d1data <- switch(attrowtype,
                   DATETIME_DAY = as.Date("2020-01-01") + 0:9,
                   DATETIME_SEC = as.POSIXct("2020-01-01 00:00:00") + 0:9,
                   DATETIME_MS = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123,
                   DATETIME_US = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123456)

  arr <- tiledb_array(uri, as.data.frame=TRUE)
  arr[] <- data.frame(rows = rows,
                      a1 = a1data,
                      d1 = d1data)
}

read_array_lowlevel <- function(uri) {
  arr <- tiledb_array(uri)

  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "READ")
  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")

  qryptr <- tiledb:::libtiledb_query_add_range_with_type(qryptr, 0, domrowtype, as.integer64(2), as.integer64(7))

  ## allocate and set rows
  bufptr0 <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domrowtype, 10)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "rows", bufptr0)

  ## allocate and set a1, easier as int
  d1r <- vector(mode="integer", length=10)
  qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a1", d1r)

  ## allocate and set d1, easier as int
  bufptr <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, attrowtype, 10)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "d1", bufptr)

  ## fire off query
  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)

  ## fetch data
  rows <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr0)
  d2r <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr)
  print(data.frame(rows,d1r,d2r))
}

read_array_query <- function(uri) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "READ")

  rowptr <- tiledb_query_buffer_alloc_ptr(qry, domrowtype, 6)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowptr)

  a1ptr <- tiledb_query_buffer_alloc_ptr(qry, "INT32", 6)
  qry <- tiledb_query_set_buffer_ptr(qry, "a1", a1ptr)

  d1ptr <- tiledb_query_buffer_alloc_ptr(qry, attrowtype, 6)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", d1ptr)

  qry <- tiledb_query_add_range(qry, schema(arr), "rows", as.integer64(4), as.integer64(7))

  tiledb_query_submit(qry)
  tiledb_query_finalize(qry)
  n <- tiledb_query_result_buffer_elements(qry, "rows")

  dat <- data.frame(rows=tiledb_query_get_buffer_ptr(rowptr),
                    a1=tiledb_query_get_buffer_ptr(a1ptr),
                    d1=tiledb_query_get_buffer_ptr(d1ptr))[1:n,]
  print(dat)
}

read_array <- function(uri) {
  arr <- tiledb_array(uri, as.data.frame=TRUE)
  ## constraint on 1st dim: value from 4 to 7
  selected_ranges(arr) <- list(cbind(as.integer64(4), as.integer64(7)))
  ## fetch data
  dat <- arr[]
  dat
}

set.seed(42)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
#write_array_lowlevel(uri)
write_array_query(uri)
#write_array(uri)

#read_array_lowlevel(uri)
read_array_query(uri)
#ead_array(uri)
