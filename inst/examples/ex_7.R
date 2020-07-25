## generalizes ex_3 and ex_6 to two-dim domain with char

suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

domrowtype <- "DATETIME_NS"
domrowtype <- "INT32"
domcoltype <- "ASCII"
attrowtype <- "DATETIME_US"

## Name of the array to create.
array_name <- "ex_7"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(0L,100000000L), 1L, domrowtype),
                                tiledb_dim("cols", NULL, NULL, domcoltype)))

  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
                                               tiledb_attr("d1", type = attrowtype)),
                                "ROW_MAJOR", "ROW_MAJOR",
                                sparse=TRUE)
  invisible(tiledb_array_create(uri, schema))
}

write_array <- function(uri) {
  arr <- tiledb_array(uri)
  qry <- tiledb_query(arr, "WRITE")
  qry <- tiledb_query_set_layout(qry, "UNORDERED")

  rows <- switch(domrowtype,
                 DATETIME_NS = nanotime(1) + 0:9,
                 INT32 = seq(1,10),
                 default = 1:10)
  rowbufptr <- tiledb_query_create_buffer_ptr(qry, domrowtype, rows)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowbufptr)

  coldata <- "aabbccccddeeffgghhiijj"
  coloffsets <- c(0L, 2L, 4L, 8L, 10L, 12L, 14L, 16L, 18L, 20L)
  colbufptr <- tiledb:::libtiledb_query_buffer_var_char_create(coloffsets, coldata)
  qry@ptr <- tiledb:::libtiledb_query_set_buffer_var_char(qry@ptr, "cols", colbufptr)

  a1data <- seq(1:10)
  d1data <- switch(attrowtype,
                   DATETIME_DAY = as.Date("2020-01-01") + 0:9,
                   DATETIME_SEC = as.POSIXct("2020-01-01 00:00:00") + 0:9,
                   DATETIME_MS = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123,
                   DATETIME_US = as.POSIXct("2020-01-01 00:00:00") + 0:9 + 0.123456)

  buf1ptr <-tiledb_query_create_buffer_ptr(qry, "INT32", a1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "a1", buf1ptr)

  buf2ptr <-tiledb_query_create_buffer_ptr(qry, attrowtype, d1data)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", buf2ptr)

  tiledb_query_submit(qry)
  invisible(NULL)
}

read_array <- function(uri) {
  arr <- tiledb_sparse(uri)
  sch <- schema(arr)
  qry <- tiledb_query(arr, "READ")

  arrptr <- arr@ptr
  qryptr <- qry@ptr

  rowptr <- tiledb_query_buffer_alloc_ptr(qry, domrowtype, 10)
  qry <- tiledb_query_set_buffer_ptr(qry, "rows", rowptr)

  colptr <- tiledb:::libtiledb_query_buffer_var_char_alloc_direct(10, 40)
  qry@ptr <- tiledb:::libtiledb_query_set_buffer_var_char(qry@ptr, "cols", colptr)
  qry <- tiledb_query_add_range(qry, sch, "cols", "a", "z")

  a1r <- vector(mode="integer", length=10) * NA_integer_
  qry <- tiledb_query_set_buffer(qry, "a1", a1r)

  d1ptr <- tiledb_query_buffer_alloc_ptr(qry, attrowtype, 10)
  qry <- tiledb_query_set_buffer_ptr(qry, "d1", d1ptr)

  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)

  rows <- tiledb_query_get_buffer_ptr(rowptr)
  cols <- tiledb:::libtiledb_query_get_buffer_var_char(colptr)
  d1r <- tiledb_query_get_buffer_ptr(d1ptr)
  print(data.frame(rows,cols,a1r,d1r))
}

set.seed(42)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_array(uri)
read_array(uri)
