
library(tiledb)
uri <- tempfile()

dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 5L), 5L, "INT32"),
                              tiledb_dim("cols", c(1L, 5L), 5L, "INT32")))

## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")))

## Create the (empty) array on disk.
tiledb_array_create(uri, schema)

data <- array(1:25, dim = c(5,5))
## Open the array and write to it.
A <- tiledb_array(uri = uri)
A[] <- data


## Open the array and read from it.
A <- tiledb_array(uri = uri)

## All the data
alldata <- A[]
cat("-- All the data\n")
show(alldata)

## Subsetting
data <- A[1:2, 2:4]
cat("\n-- A subset of the data\n")
show(data)

## Or via low-lever API and subarr
ctx <- tiledb_ctx()
subarr <- c(3L,5L, 3L,5L)
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "READ")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "COL_MAJOR")
buf <- integer(9)
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", buf)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
res <- tiledb:::libtiledb_array_close(arrptr)
cat("\n-- A subarray of the data\n")
print(array(buf, dim = c(3,3)))


## Or via range -- non-contiguous
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, uri, "READ")
qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "COL_MAJOR")
qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 0, 3L, 3L)
qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 0, 5L, 5L)
qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 1, 3L, 3L)
qryptr <- tiledb:::libtiledb_query_add_range(qryptr, 1, 5L, 5L)
buf <- integer(4)
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", buf)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
res <- tiledb:::libtiledb_array_close(arrptr)
cat("\n-- Two non-contiguous subranges: c(3,5), c(3,5)\n")
print(matrix(buf,2,2))


## Addendum
#cat("\n-- Running equivalent of 'A[c(3:5), c(3:5)]'\n")
#print(A[c(3,5), c(3,5)])
