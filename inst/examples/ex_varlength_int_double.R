
library(tiledb)

uri <- "/tmp/tiledb/test4"

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(tiledb_dense(uri))
  }

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom,
                                attrs = c(tiledb_attr("a2", type = "INT32", is_var = TRUE),
                                          tiledb_attr("a3", type = "FLOAT64", is_var = TRUE)))

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
}

write_variable_array <- function(uri, debug=FALSE) {
  a2 <- data.table::data.table(v1=list(c(1L, 1L), 5L, c(9L, 9L), 13L),
                               v2=list(c(2L,2L), c(6L,6L), 10L, c(14L,14L,14L)),
                               v3=list(3L, c(7L,7L), 11L, 15L),
                               v4=list(4L, c(8L,8L,8L), c(12L,12L), 16L))
  a3 <- data.table::data.table(v1=list(c(1.0, 1.1), 5,        c(9, 9),  13),
                               v2=list(c(2,2),      c(6,6),   10,       c(14.1,14.2,14.3)),
                               v3=list(3,           c(7,7),   11,       15),
                               v4=list(4,           c(8,8,8), c(12,12), 16.75))
  write_variable_length(uri, list(a2=a2, a3=a3), debug=debug)
}

read_variable_array <- function(uri, debug=FALSE) {
  res <- read_variable_length(uri, "a2", c(1,4,1,4), debug=debug)
  print(res)
  res <- read_variable_length(uri, "a3", c(1,4,1,4), debug=debug)
  print(res)
}

debug <- FALSE
if (!dir.exists(uri)) {
  arr <- create_array(uri)
  if (debug) tiledb_array_schema_dump(uri)
  write_variable_array(uri)
}
#read_variable_array(uri, debug)
#cat("Done\n")

arr1 <- tiledb_dense(uri, as.data.frame=FALSE)
#arr1[2:3,]
arr1[2,2]

arr2 <- tiledb_dense(uri, as.data.frame=TRUE)
#arr2[2:3,3:4]
arr2[2,2]

#arr1[2,2] <- list(array(c(6L,7L), c(1,1)), array(c(6,7), c(1,1)))
#arr1[2,2] <- list(data.table::data.table(list(c(21L,22L))),
#                  data.table::data.table(list(c(21,22))))

## works
arr1[2,2] <- list(array(c(6L,7L)), array(c(6,7)))
## also works
#arr1[2,2] <- list(c(6L,7L), c(6,7))

arr1[]
