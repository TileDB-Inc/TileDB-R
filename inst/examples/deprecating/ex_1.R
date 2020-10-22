
library(tiledb)

## Name of the array to create.
array_name <- "ex_1"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  ## The array will be 10x5 with dimensions "rows" and "cols", with domains [1,10] and [1,5].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 10L), 10L, "INT32"),
                                tiledb_dim("cols", c(1L, 5L), 5L, "INT32")))

  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32"),
                                               tiledb_attr("b", type = "FLOAT64"),
                                               tiledb_attr("c", type = "CHAR", ncells=NA_integer_)))

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
}


write_array <- function(uri) {
  data <- list(array(seq(1:50), dim = c(10,5)),
               array(as.double(seq(101,by=0.5,length=50)), dim = c(10,5)),
               array(c(letters[1:26], "brown", "fox", LETTERS[1:22]), dim = c(10,5)))
  ## Open the array and write to it.
  A <- tiledb_dense(uri = uri)
  A[] <- data
}

read_array <- function(uri) {
  ## Open the array and read from it.
  A <- tiledb_dense(uri = uri)
  data <- A[6:9, 2:4]
  show(data)

  a <- data[["a"]]
  show(a)
}

read_as_df <- function(uri) {
  A <- tiledb_dense(uri = uri, as.data.frame = TRUE)
  data <- A[3:7, 2:4]
  show(data)
}

read_array_subset <- function(uri) {
  ## Open the array and read from it.
  A <- tiledb_dense(uri = uri, attrs = c("b","c"))
  data <- A[6:9, 2:4]
  show(data)
}

open_read_change_read <- function(uri) {
  ## Open the array and read from it.
  A <- tiledb_dense(uri = uri)
  data <- A[6:9, 2:4]
  show(data)

  ## now activate data.frame mode
  return.data.frame(A) <- TRUE
  data <- A[6:9, 2:4]
  show(data)

  ## now select subset
  attrs(A) <- c("b", "c")
  data <- A[6:9, 2:4]
  show(data)
}

simple_ex <- function(uri) {
  arr <- tiledb_dense(uri, as.data.frame = TRUE)
  show(arr[7:9, 2:3])
}

create_array(uri)
write_array(uri)
read_array(uri)
read_as_df(uri)
read_array_subset(uri)
open_read_change_read(uri)
simple_ex(uri)
