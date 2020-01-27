
library(tiledb)

uri <- "/tmp/test2"

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
                                attrs = c(tiledb_attr("a1", type = "CHAR", is_var = TRUE),
                                          tiledb_attr("a2", type = "INT32", is_var = TRUE)))

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
}

write_variable_array <- function(uri) {
  a1 <- data.table::data.table(v1=list("a", "eee", "i", "m"),
                               v2=list("bb", "f", "jjj", "n"),
                               v3=list("ccc", "g", "kk", "oo"),
                               v4=list("dd", "hhh", "l", "p"))
  a2 <- data.table::data.table(v1=list(c(1L, 1L), 5L, c(9L, 9L), 13L),
                               v2=list(c(2L,2L), c(6L,6L), 10L, c(14L,14L,14L)),
                               v3=list(3L, c(7L,7L), 11L, 15L),
                               v4=list(4L, c(8L,8L,8L), c(12L,12L), 16L))
  write_variable_length(uri, list(a1=a1, a2=a2), debug=FALSE)
}

arr <- create_array(uri)
tiledb_array_schema_dump(uri)
write_variable_array(uri)
cat("Done\n")

#debug <- TRUE
#ll <- read_variable_length(tmp, c(1,4,1,4), c("a1", "a2"), debug)
#if (debug) print(ll)
#write_variable_length(tmp, ll, debug)
