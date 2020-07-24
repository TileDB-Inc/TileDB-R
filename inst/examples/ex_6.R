## generalizes ex_R to two dims

suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

domrowtype <- "DATETIME_NS"
domcoltype <- "INT32"
attrowtype <- "DATETIME_US"

## Name of the array to create.
array_name <- "ex_6"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(0, 1e12), 1, type = domrowtype),
                                tiledb_dim("cols", c(1L,500L), 1L, type = domcoltype)))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
                                               tiledb_attr("d1", type = attrowtype)),
                                "ROW_MAJOR", "ROW_MAJOR", sparse=TRUE)
  invisible(tiledb_array_create(uri, schema))
}

write_array <- function(uri) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "WRITE")
  qry <- tiledb_query_set_layout(qry, "UNORDERED")

  rows <- switch(domrowtype,
                 DATETIME_NS = nanotime(1) + 0:9,
                 default = 1:10)
  rowbufptr <- tiledb_query_create_buffer_ptr(qry, domrowtype, rows)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowbufptr)

  cols <- seq(101,110)
  colbufptr <- tiledb_query_create_buffer_ptr(qry, domcoltype, cols)
  qry <- tiledb_query_set_buffer_ptr(qry, "cols", colbufptr)

  a1data <- seq(1:10)
  buf1ptr <- tiledb_query_create_buffer_ptr(qry, "INT32", a1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "a1", buf1ptr)

  d1data <- switch(attrowtype,
                   DATETIME_DAY = as.Date("2020-01-01") + 0:9,
                   DATETIME_SEC = as.POSIXct("2020-01-01 00:00:00") + 0:9,
                   DATETIME_MS = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123,
                   DATETIME_US = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123456)
  buf2ptr <- tiledb_query_create_buffer_ptr(qry, attrowtype, d1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", buf2ptr)

  tiledb_query_submit(qry)
  invisible(NULL)
}

read_array <- function(uri) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "READ")
  sch <- schema(arr)

  qry <- tiledb_query_add_range(qry, sch, "rows", as.integer64(2), as.integer64(7))
  qry <- tiledb_query_add_range(qry, sch, "cols", 104L, 109L)

  rowptr <- tiledb_query_buffer_alloc_ptr(qry, domrowtype, 10)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowptr)
  colptr <- tiledb_query_buffer_alloc_ptr(qry, domcoltype, 10)
  qry <- tiledb_query_set_buffer_ptr(qry, "cols", colptr)

  d1r <- vector(mode="integer", length=10) * NA_integer_
  qry <- tiledb_query_set_buffer(qry, "a1", d1r)

  bufptr <- tiledb_query_buffer_alloc_ptr(qry, attrowtype, 10)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", bufptr)

  qry <- tiledb_query_submit(qry)

  rows <- tiledb_query_get_buffer_ptr(rowptr)
  cols <- tiledb_query_get_buffer_ptr(colptr)
  d2r <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr)
  print(na.omit(data.frame(rows,cols,d1r,d2r)))

}

set.seed(42)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_array(uri)
read_array(uri)
