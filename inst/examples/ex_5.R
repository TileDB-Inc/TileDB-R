library(tiledb)

## trying to mimick the simple variable length char domain unit test
## based the C++ API test

## Name of the array to create.
array_name <- "ex_5"

## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create <- function(uri) {
    if (dir.exists(uri)) unlink(uri, recursive = TRUE)

    #dim <- new("tiledb_dim", ptr = tiledb:::libtiledb_dim(ctx@ptr, "d", "ASCII", NULL, NULL))
    dim <- tiledb_dim("d", NULL, NULL, "ASCII")
    dom <- tiledb_domain(dim)

    a <- tiledb_attr("a", type = "INT32")
    sch <- tiledb_array_schema(dom, a, sparse = TRUE)

    invisible(tiledb_array_create(uri, sch))
}

write <- function(uri) {
    x <- tiledb_sparse(uri)
    avals <- c(3L, 2L, 1L, 4L)
    data <- "ccbbddddaa"
    offsets <- c(0L, 2L, 4L, 8L)

    arr <- tiledb_array(uri = uri)
    qry <- tiledb_query(arr, "WRITE")
    qry <- tiledb_query_set_layout(qry, "UNORDERED")
    qryptr <- qry@ptr

    ## TODO var_char wrappers
    bufptr <- tiledb:::libtiledb_query_buffer_var_char_create(offsets, data)
    qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "d", bufptr)

    bufptr2 <- tiledb_query_create_buffer_ptr(qry, "INT32", avals)
    qry <- tiledb_query_set_buffer_ptr(qry, "a", bufptr2)

    tiledb_query_submit(qry)
    invisible(NULL)
}

read <- function(uri) {
    arr <- tiledb_sparse(uri)

    arr <- tiledb_array(uri = uri)
    qry <- tiledb_query(arr, "READ")

    qry <- tiledb_query_add_range(qry, schema(arr), "d", "a", "ee")

    bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc_direct(4, 10)
    qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qry@ptr, "d", bufptr)

    qry <- tiledb_query_submit(qry)

    mat <- tiledb:::libtiledb_query_get_buffer_var_char(bufptr)
    print(mat)

}

create(uri)
write(uri)
read(uri)
