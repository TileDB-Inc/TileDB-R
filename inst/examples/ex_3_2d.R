suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

## work-in-progress, does not yet work

domrowtype <- "DATETIME_NS"
domcoltype <- "INT32"
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

  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(0, 1e12), 1, type = domrowtype),
                                tiledb_dim("cols", c(1L,500L), 1L, type = domcoltype)))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
                                               tiledb_attr("d1", type = attrowtype)),
                                "ROW_MAJOR", "ROW_MAJOR", sparse=TRUE)
  tiledb_array_create(uri, schema)
}

write_array <- function(uri) {
  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "WRITE")
  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
  qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "UNORDERED")

  rows <- switch(domrowtype,
                 DATETIME_NS = nanotime(1) + 0:9,
                 default = 1:10)
  rowbufptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domrowtype, 10)
  rowbufptr <-tiledb:::libtiledb_query_buffer_assign_ptr(rowbufptr, domrowtype, rows)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "rows", rowbufptr)

  cols <- seq(101,110)
  colbufptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domcoltype, 10L)
  colbufptr <-tiledb:::libtiledb_query_buffer_assign_ptr(colbufptr, domcoltype, cols)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "cols", colbufptr)

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
  qryptr <- tiledb:::libtiledb_query_add_range_with_type(qryptr, 1, domcoltype, 104L, 109L)

  bufptr0 <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domrowtype, 10)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "rows", bufptr0)
  bufptr1 <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domcoltype, 10)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "cols", bufptr1)

  d1r <- vector(mode="integer", length=10) * NA_integer_
  qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a1", d1r)

  bufptr <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, attrowtype, 10)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "d1", bufptr)

  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)

  rows <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr0)
  cols <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr1)
  d2r <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr)
  print(na.omit(data.frame(rows,cols,d1r,d2r)))

}

set.seed(42)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_array(uri)
read_array(uri)
#read_array_df(uri)
