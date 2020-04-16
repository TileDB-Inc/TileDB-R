library(tiledb)

## trying to mimick the simple variable length char domain unit tes
## based the C++ API test

## Name of the array to create.
array_name <- "ex_5"

## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create <- function(uri) {
    if (dir.exists(uri)) unlink(uri, recursive = TRUE)

    ctx <- tiledb_ctx()
    dim <- new("tiledb_dim", ptr = tiledb:::libtiledb_dim(ctx@ptr, "d", "ASCII", NULL, NULL))

    dom <- tiledb_domain(dim)
    #dom <- new("tiledb_domain", ptr=dim@ptr)

    a <- tiledb_attr("a", type = "INT32")
    sch <- tiledb_array_schema(dom, a, sparse = TRUE)
    tiledb_array_create(uri, sch)
    invisible(NULL)
}

write <- function(uri) {
    x <- tiledb_sparse(uri)
    avals <- c(3L, 2L, 1L, 4L)
    data <- "ccbbddddaa"
    offsets <- c(0L, 2L, 4L, 8L)

    ctx <- tiledb_ctx()
    arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "WRITE")
    qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
    qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "UNORDERED")
    #qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")

    bufptr <- tiledb:::libtiledb_query_buffer_var_char_create(offsets, data)
    qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "d", bufptr)

    bufptr2 <-tiledb:::libtiledb_query_buffer_alloc_ptr(arrptr, "INT32", 4)
    bufptr2 <-tiledb:::libtiledb_query_buffer_assign_ptr(bufptr2, "INT32", avals)
    qryptr <- tiledb:::libtiledb_query_set_buffer_ptr(qryptr, "a", bufptr2)

    qryptr <- tiledb:::libtiledb_query_submit(qryptr)
    tiledb:::libtiledb_array_close(arrptr)
    invisible(NULL)
}

read <- function(uri) {
    arr <- tiledb_sparse(uri)
    #print(schema(arr))

    ctx <- tiledb_ctx()
    arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "READ")
    print(tiledb:::libtiledb_array_nonempty_domain_var_from_name(arrptr, "d"))

    qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
    qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 0, "a", "ee")

    bufptr <- tiledb:::libtiledb_query_buffer_var_char_alloc_direct(arrptr, "a1", 4, 10)
    qryptr <- tiledb:::libtiledb_query_set_buffer_var_char(qryptr, "d", bufptr)
    qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
    qryptr <- tiledb:::libtiledb_query_submit(qryptr)

    tiledb:::libtiledb_array_close(arrptr)

    mat <- tiledb:::libtiledb_query_get_buffer_var_char(bufptr)
    print(mat)

}

create(uri)
write(uri)
read(uri)
