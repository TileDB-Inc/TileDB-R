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
  dom <- tiledb_domain(c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                         tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))
  sch <- tiledb_array_schema(dom,
                             attrs = c(tiledb_attr("a1", type="CHAR", ncells=NA)),
                             cell_order = "ROW_MAJOR",
                             tile_order = "ROW_MAJOR",
                             sparse = FALSE)
  tiledb_array_create(array_name, sch)
  invisible(NULL)
}

write_array <- function() {
  datavec <- c("a","bb", "ccc", "dd", "eee", "f", "g", "hhh",
               "i", "jjj", "kk", "l", "m", "n", "oo", "p")

  array <- tiledb_array(array_name, "WRITE")
  query <- tiledb_query(array, "WRITE")
  bufptr <- tiledb_query_create_buffer_ptr_char(query, datavec)
  query <- tiledb_query_set_buffer_ptr_char(query, "a1", bufptr)
  query <- tiledb_query_submit(query)
  tiledb_array_close(array)
  invisible(NULL)
}

read_array <- function(txt="", subarr=NULL) {
  cat("\nReading", txt, "\n")
  ctx <- tiledb_ctx()
  array <- tiledb_array(array_name, "READ")
  query <- tiledb_query(array, "READ")
  if (is.null(subarr)) {
    d <- dim(schema(array))
    subarr <- c(1L, d[1], 1L, d[2])
  }
  query <- tiledb_query_set_subarray(query, subarr)
  bufptr <- tiledb_query_alloc_buffer_ptr_char_subarray(array, "a1", subarr)
  query <- tiledb_query_set_buffer_ptr_char(query, "a1", bufptr)
  query <- tiledb_query_submit(query)
  print(tiledb_query_get_buffer_char(bufptr), quote=FALSE)
}

write_subarray <- function() {
  datavec <- c("K", "LLL", "MM", "N")
  subarr <- c(2L,3L, 2L,3L)

  array <- tiledb_array(array_name, "WRITE")
  query <- tiledb_query(array, "WRITE")
  query <- tiledb_query_set_subarray(query, subarr)
  bufptr <- tiledb_query_create_buffer_ptr_char(query, datavec)
  query <- tiledb_query_set_buffer_ptr_char(query, "a1", bufptr)
  query <- tiledb_query_submit(query)
  tiledb_array_close(array)
  invisible(NULL)
}

create_array()
write_array()
read_array("original array")
write_subarray()
read_array("after subarray write")
read_array("after subarray write, subset", c(2L,3L,2L,3L))
