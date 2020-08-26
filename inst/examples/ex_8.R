## quick check writing multiple times
## (which simulates same timestamp different security)

suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

domrowtype <- "DATETIME_NS"
domcoltype <- "ASCII"
attrowtype <- "DATETIME_US"

## Name of the array to create.
array_name <- "ex_8"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  dom <- tiledb_domain(c(tiledb_dim("rows", c(0L,1999999900000000000), 1L, domrowtype),
                         tiledb_dim("cols", NULL, NULL, domcoltype)))

  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
                                               tiledb_attr("d1", type = attrowtype)),
                                sparse=TRUE)
  invisible(tiledb_array_create(uri, schema))
}

write_thrice_array <- function(uri) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "WRITE")
  qry <- tiledb_query_set_layout(qry, "UNORDERED")

  for (i in 1:3) {
    rows <- switch(domrowtype,
                   DATETIME_NS = rep(nanotime(as.POSIXct("2020-01-01 00:00:00")) + (i)*10, 10),
                   INT32 = (i-1)*10 + seq(1,10),
                   default = (i-1)*10 + 1:10)
    rowbufptr <- tiledb_query_create_buffer_ptr(qry, domrowtype, rows)
    qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowbufptr)

    cols <- c("aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj")
    colbufptr <- tiledb_query_create_buffer_ptr_char(qry, cols)
    query <- tiledb_query_set_buffer_ptr_char(qry, "cols", colbufptr)

    a1data <- (i-1)*10 + seq(1:10)
    d1data <- switch(attrowtype,
                     DATETIME_DAY = as.Date("2020-01-01") + (i-1)*10 + 0:9,
                     DATETIME_SEC = as.POSIXct("2020-01-01 00:00:00") + (i-1)*10 + 0:9,
                     DATETIME_MS = as.POSIXct("2020-01-01 00:00:00") + (i-1)*10 + 0:9 + 0.123,
                     DATETIME_US = as.POSIXct("2020-01-01 00:00:00") + (i-1)*10 + 0:9 + 0.123456)

    buf1ptr <- tiledb_query_create_buffer_ptr(qry, "INT32", a1data)
    qry <- tiledb_query_set_buffer_ptr(qry, "a1", buf1ptr)

    buf2ptr <- tiledb_query_create_buffer_ptr(qry, attrowtype, d1data)
    qry <- tiledb_query_set_buffer_ptr(qry, "d1", buf2ptr)

    qry <- tiledb_query_submit(qry)
  }
  invisible(NULL)
}

read_array <- function(uri) {
  arr <- tiledb_sparse(uri)
  sch <- schema(arr)
  qry <- tiledb_query(arr, "READ")

  ## important: we currently cannot retrieve more than nrow
  ## (though this maybe be a constraint we impose)
  #nrow <- 8
  nrow <- 30

  rowptr <- tiledb_query_buffer_alloc_ptr(qry, domrowtype, nrow)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowptr)

  colptr <- tiledb_query_alloc_buffer_ptr_char(nrow, 90)
  qry <- tiledb_query_set_buffer_ptr_char(qry, "cols", colptr)
  qry <- tiledb_query_add_range(qry, sch, "cols", "aa", "zz")

  a1r <- vector(mode="integer", length=nrow) * NA_integer_
  qry <- tiledb_query_set_buffer(qry, "a1", a1r)

  d1ptr <- tiledb_query_buffer_alloc_ptr(qry, attrowtype, nrow)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", d1ptr)

  qry <- tiledb_query_submit(qry)
  tiledb_array_close(arr)

  rows <- tiledb_query_get_buffer_ptr(rowptr)
  cols <- tiledb_query_get_buffer_char(colptr)
  d1r <- tiledb_query_get_buffer_ptr(d1ptr)
  print(data.frame(rows,cols,a1r,d1r))
}

set.seed(42)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_thrice_array(uri)
read_array(uri)
