library(tiledb)

## Name of the array to create. Will get auto-deleted.
## Replace with filename in existing directory if you want to keep it.
array_name <- tempfile()

create_array <- function() {
  ## Check if the array already exists.
  if (tiledb_object_type(array_name) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))


  attr <- tiledb_attr("a", type = "FLOAT64")
  ## set to variable length
  tiledb:::libtiledb_attribute_set_cell_val_num(attr@ptr, NA)

  ## now set the schema
  ctx <- tiledb_ctx()
  schptr <- tiledb:::libtiledb_array_schema_create(ctx@ptr, "DENSE")
  tiledb:::libtiledb_array_schema_set_domain(schptr, dom@ptr)
  tiledb:::libtiledb_array_schema_set_cell_order(schptr, "ROW_MAJOR")
  tiledb:::libtiledb_array_schema_set_tile_order(schptr, "ROW_MAJOR")
  tiledb:::libtiledb_array_schema_add_attribute(schptr, attr@ptr)

  ## Create the (empty) array on disk.
  tiledb:::libtiledb_array_create(array_name, schptr)
  invisible(NULL)
}

write_array <- function() {
  data <- c(1.1, 1.1, 2.2, 2.2, 3.3, 4.4, 5.5, 6.6, 6.6, 7.7, 7.7, 8.8, 8.8, 8.8, 9.9, 9.0, 10.0,
            11.1, 12.2, 12.2, 13.3, 14.4, 14.4, 14.4, 15.5, 16.6)
  offsets <- c(0L, 2L, 4L, 5L, 6L, 7L, 9L, 11L, 14L, 16L, 17L, 18L, 20L, 21L, 24L, 25L)
  offsets <- offsets * 8 # known and fixed size of double

  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
  qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

  bufptr <- tiledb:::libtiledb_query_buffer_var_vec_create(offsets, data)
  qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)
  invisible(NULL)
}

read_array <- function(txt="", subarr=NULL) {
  cat("\nReading", txt, "\n")
  ctx <- tiledb_ctx()

  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
  if (is.null(subarr)) {
      schptr <- tiledb:::libtiledb_array_get_schema(arrptr)
      domptr <- tiledb:::libtiledb_array_schema_get_domain(schptr)
      lst <- tiledb:::libtiledb_domain_get_dimensions(domptr)
      subarr <- c(tiledb:::libtiledb_dim_get_domain(lst[[1]]),
                  tiledb:::libtiledb_dim_get_domain(lst[[2]]))
  }
  bufptr <- tiledb:::libtiledb_query_buffer_var_vec_alloc(arrptr, subarr, "a")

  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
  qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
  qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

  qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)

  rl <- tiledb:::libtiledb_query_get_buffer_var_vec(qryptr, "a", bufptr)
  invisible(rl)
}

write_subarray <- function() {
  data <- c(11.1, 11.1, 22.2, 22.2, 33.3, 44.4)
  offsets <- c(0L, 2L, 4L, 5L)
  offsets <- offsets * 4 # known fixed size of integer

  subarr <- c(2L,3L, 2L,3L)

  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
  qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
  qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

  bufptr <- tiledb:::libtiledb_query_buffer_var_vec_create(offsets, data)
  qryptr <- tiledb:::libtiledb_query_set_buffer_var_vec(qryptr, "a", bufptr)
  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_array_close(arrptr)
  invisible(NULL)
}

create_array()
write_array()
print(read_array("original"))
write_subarray()
print(read_array("after subarray"))
print(read_array("after subarray, subset", c(2L,3L, 2L,3L)))

cat("Done.\n")
