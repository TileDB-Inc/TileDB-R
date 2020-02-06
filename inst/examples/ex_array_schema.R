
library(tiledb)

#uri <- "~/git/tiledb-unit-test-arrays/v1_7_0/DENSE_v1_7_0_INT8/"
uri <- "~/git/tiledb-data/examples/quickstart_dense"
arr <- tiledb_dense(uri)

ctx <- arr@ctx

#arr <- tiledb_array_open(arr, "READ")
#schema_xptr <- tiledb:::libtiledb_array_get_schema(arr@ptr)   ## need higher-level helper hereby
#arrsch <- new("tiledb_array_schema", ptr=schema_xptr)

sch <- schema(arr)

dom <- domain(sch)

dimensions(sch)                           			# returns list  TODO: make nicer
lapply(dimensions(sch), name)                   # gets names    TODO: make nicer

rl <- attrs(sch)

## names(rl)[grep("attribute_INT32", names(rl))] # here over 4k
#int32attr <- names(rl)[grep("attribute_INT32", names(rl))]

subarray <- tiledb:::domain_subarray(dom, index = c(1,2))
buffers <- tiledb:::attribute_buffers(arr, sch, dom, subarray)

tiledb:::libtiledb_array_open(arr@ptr, "READ")
qry <- tiledb:::libtiledb_query(ctx@ptr, arr@ptr, "READ")
qry <- tiledb:::libtiledb_query_set_layout(qry, "COL_MAJOR")

## is.integral is true
#qry <- tiledb:::libtiledb_query_set_subarray(qry, as.integer(subarray))

attr_names <- names(buffers)
idx <- 1 ## only one name here
aname <- attr_names[[idx]]
val <- buffers[[idx]]
qry <- tiledb:::libtiledb_query_set_buffer_var(qry, aname, val)
