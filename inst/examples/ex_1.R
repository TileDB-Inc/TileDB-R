
library(tiledb)

## Name of the array to create.
array_name <- "ex_1"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", ""), array_name)

create_array <- function() {
    # Check if the array already exists.
    if (tiledb_object_type(array_name) == "ARRAY") {
        message("Array already exists.")
        return(invisible(NULL))
    }

  ## The array will be 10x5 with dimensions "rows" and "cols", with domain [1,10].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 10L), 10L, "INT32"),
                                tiledb_dim("cols", c(1L, 5L), 5L, "INT32")))

  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32"),
                                               tiledb_attr("b", type = "FLOAT64")))

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
}


write_array <- function() {
  data <- list(array(seq(1:50), dim = c(10,5)),
               array(as.double(seq(101,150)), dim = c(10,5)))
  ## Open the array and write to it.
  A <- tiledb_dense(uri = uri)
  A[] <- data
}

read_array <- function() {
  ## Open the array and read from it.
  A <- tiledb_dense(uri = uri)
  data <- A[1:3, 2:5]
  show(data)

  a <- data[["a"]]
  show(a)
}

create_array()
write_array()
read_array()
