library(tiledb)

# Name of the array to create.
array_name <- "/tmp/tiledb/variable_length_array2"

create_array <- function() {
    # Check if the array already exists.
    if (tiledb_object_type(array_name) == "ARRAY") {
        message("Array already exists.")
        return(invisible(NULL))
    }

    # The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
    dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                                  tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

    # The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
    #schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "CHAR")))

    attr <- tiledb_attr("a1", type = "CHAR")
    ## set to variable length
    tiledb:::libtiledb_attribute_set_cell_val_num(attr@ptr, NA)

    ctx <- tiledb_ctx()
    schptr <- tiledb:::libtiledb_array_schema_create(ctx@ptr, "DENSE")
    schptr <- tiledb:::libtiledb_array_schema_set_domain(schptr, dom@ptr)
    tiledb:::libtiledb_array_schema_set_cell_order(schptr, "ROW_MAJOR")
    tiledb:::libtiledb_array_schema_set_tile_order(schptr, "ROW_MAJOR")
    tiledb:::libtiledb_array_schema_add_attribute(schptr, attr@ptr)

    # Create the (empty) array on disk.
    tiledb:::libtiledb_array_create(array_name, schptr)
}

write_array <- function() {
    #data <- array(letters[1:16], dim = c(4,4))
    # Open the array and write to it.
    #A <- tiledb_dense(uri = array_name)
    #A[] <- data

    data <- "abbcccddeeefghhhijjjkklmnoop";
    offsets <- c(0L, 1L, 3L, 6L, 8L, 11L, 12L, 13L, 16L, 17L, 20L, 22L, 23L, 24L, 25L, 27L)

    ## reserver a numeric vector of the same length
    #dblvec <- double(length(offsets))
    ## and cast each element to uint64_t which is needed internally
    #tiledb:::libtiledb_query_set_buffer_inject_offsets(offsets, dblvec)

    ctx <- tiledb_ctx()

    arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")
    qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
    qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
    ##qryptr <- tiledb:::libtiledb_query_set_buffer_var_string(qryptr, "a1", dblvec, data)
    ##qryptr <- tiledb:::libtiledb_query_submit(qryptr)
    qryptr <- tiledb:::libtiledb_query_set_buffer_var_string_and_submit(qryptr, "a1", offsets, data)
}

read_array <- function() {
  ## Open the array and read from it.
  ##A <- tiledb_dense(uri = array_name)
  ##data <- A[1:2L, 2:4]
  ##show(data)

  ctx <- tiledb_ctx()
  arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
  subarr <- c(1L,4L, 1L,4L)
  #elems <- tiledb:::libtiledb_array_max_buffer_elements_vec(arrptr, subarr, "a1")
  #print(elems)

  bufptr <- tiledb:::libtiledb_query_buffer_var_string_allocate(arrptr, subarr, "a1")

  ## reserve memory
  #offsets <- double(elems[1])
  #string <- paste(rep(' ', elems[2]), collapse="")
  #print(string)

  qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
  qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
  qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
  #qryptr <- tiledb:::libtiledb_query_set_buffer_var_string(qryptr, "a1", offsets, string)

  qryptr <- tiledb:::libtiledb_query_set_buffer_var_string_from_buffer(qryptr, "a1", bufptr)
  qryptr <- tiledb:::libtiledb_query_submit(qryptr)
  tiledb:::libtiledb_query_show_bufptr(bufptr)

}

create_array()
write_array()
read_array()
## if (FALSE) {

##   ##read_array()
##   x <- tiledb_dense(uri = array_name)
##   ##$rint(x)
##   ##rint(str(x))

##   ctx <- x@ctx
##   schema <- tiledb::schema(x)
##   uri <- x@uri
##   dom <- tiledb::domain(schema)
##   ##print(dom)

##   index <- list(c(1:4),c(1:4))
##   subarray <- tiledb:::domain_subarray(dom, index = index)
##   attrs <- tiledb::attrs(schema)

##   sub_dim <- tiledb:::subarray_dim(subarray)

##   tiledb:::libtiledb_array_open_with_ptr(x@ptr, "WRITE")

##   qry <- tiledb:::libtiledb_query(ctx@ptr, x@ptr, "WRITE")
##   qry <- tiledb:::libtiledb_query_set_layout(qry, "COL_MAJOR")
##   qry <- tiledb:::libtiledb_query_set_subarray(qry, as.integer(subarray))

##   qry <- tiledb:::libtiledb_query_set_buffer_var(qry, aname, val)
## }
