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

  ctx <- tiledb_ctx()
  #dimrows <- new("tiledb_dim", ptr = tiledb:::libtiledb_dim(ctx@ptr, "rows", domrowtype, c(nanotime(as.POSIXct("2000-01-01 00:00:00")),nanotime(as.POSIXct("2022-12-31 23:59:59"))),1000))
  dimrows <- new("tiledb_dim", ptr = tiledb:::libtiledb_dim(ctx@ptr, "rows", domrowtype, c(0L,1999999900000000000),1L))


  dimcols <- new("tiledb_dim", ptr = tiledb:::libtiledb_dim(ctx@ptr, "cols", domcoltype, NULL, NULL))
  dom <- tiledb_domain(c(dimrows,dimcols))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32"),
                                               tiledb_attr("d1", type = attrowtype)),
                                sparse=TRUE)
  tiledb_array_create(uri, schema)
}

write_thrice_array <- function(uri) {
  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "WRITE")
  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
  qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "UNORDERED")

  for (i in 1:3) {
    rows <- switch(domrowtype,
                   DATETIME_NS = rep(nanotime(as.POSIXct("2020-01-01 00:00:00")) + (i)*10, 10),
                   INT32 = (i-1)*10 + seq(1,10),
                   default = (i-1)*10 + 1:10)
    print(rows)
    rowbufptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domrowtype, 10)
    rowbufptr <-tiledb:::libtiledb_query_buffer_assign_ptr(rowbufptr, domrowtype, rows)
    qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "rows", rowbufptr)

    coldata <- "aabbccddeeffgghhiijj"
    coloffsets <- c(0L, 2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L)
    #coldata <- paste0(letters[1:10], collapse="")
    #coloffsets <- seq(0,9)
    colbufptr <- tiledb:::libtiledb_query_buffer_var_char_create(coloffsets, coldata)
    qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "cols", colbufptr)

    ##data <- list(seq(1:10), ISOdatetime(2020,1,1,0,0,0) + 0:9)
    a1data <- (i-1)*10 + seq(1:10)
    d1data <- switch(attrowtype,
                     DATETIME_DAY = as.Date("2020-01-01") + (i-1)*10 + 0:9,
                     DATETIME_SEC = as.POSIXct("2020-01-01 00:00:00") + (i-1)*10 + 0:9,
                     DATETIME_MS = as.POSIXct("2020-01-01 00:00:00") + (i-1)*10 + 0:9 + 0.123,
                     DATETIME_US = as.POSIXct("2020-01-01 00:00:00") + (i-1)*10 + 0:9 + 0.123456)

    buf1ptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, "INT32", 10)
    buf1ptr <-tiledb:::libtiledb_query_buffer_assign_ptr(buf1ptr, "INT32", a1data)
    qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "a1", buf1ptr)

    buf2ptr <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, attrowtype, 10)
    buf2ptr <-tiledb:::libtiledb_query_buffer_assign_ptr(buf2ptr, attrowtype, d1data)
    qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "d1", buf2ptr)

    qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  }

  tiledb:::libtiledb_array_close(arrptr)
  invisible(NULL)
}

read_array <- function(uri) {
  arr <- tiledb_sparse(uri)

  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "READ")
  #print(tiledb:::libtiledb_array_nonempty_domain_var_from_name(arrptr, "cols"))

  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")

  ## important: we currently cannot retrieve more than nrow
  ## (though this maybe be a constraint we impose)
  #nrow <- 8
  nrow <- 30

  bufptr0 <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, domrowtype, nrow)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "rows", bufptr0)

  #qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 0, 1L, 9L)
  qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 1, "aa", "zz")

  bufptr1 <- tiledb:::libtiledb_query_buffer_var_char_alloc_direct(nrow, 90)
  qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "cols", bufptr1)

  d1r <- vector(mode="integer", length=nrow) * NA_integer_
  qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a1", d1r)

  bufptr <- tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, attrowtype, nrow)
  qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "d1", bufptr)

  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)

  rows <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr0)
  cols <- tiledb:::libtiledb_query_get_buffer_var_char(bufptr1)
  #cols <- tiledb:::libtiledb_query_get_buffer_var_char_simple(bufptr1)
  d2r <- tiledb:::libtiledb_query_get_buffer_ptr(bufptr)
  print(data.frame(rows,cols,d1r,d2r))
}

set.seed(42)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
create_array(uri)
write_thrice_array(uri)
read_array(uri)
