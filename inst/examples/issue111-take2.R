
library(tiledb)
uri <- tempfile()

dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 5L), 5L, "INT32"),
                              tiledb_dim("cols", c(1L, 5L), 5L, "INT32")))

## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
schema <- tiledb_array_schema(dom,
                              attrs = c(tiledb_attr("a", type = "INT32")),
                              sparse=TRUE)

## Create the (empty) array on disk.
tiledb_array_create(uri, schema)

data <- array(1:25, dim = c(5,5))
## Open the array and write to it.
#A <- tiledb_sparse(uri = uri)


## Open the array to write to it.
A <- tiledb_array(uri = uri)

A[] <- data.frame(rows=rep(1:5,5), cols=rep(1:5,each=5), a=1:25)

## All the data
alldata <- A[]
cat("-- All the data\n")
show(alldata)

## Subsetting
data <- A[list(1:2), list(2:4)]
cat("\n-- A subset of the data\n")
show(data)

## Or via subarr
data <- A[list(3:5), list(3:5)]
cat("\n-- A subset of the data\n")
show(data)

## Or via range -- non-contiguous
data <- A[list(c(1:2),c(4:5)), list(3:5)]
cat("\n-- A subset of the data\n")
show(data)
