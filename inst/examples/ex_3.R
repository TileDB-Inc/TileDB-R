library(tiledb)

## work-in-progress, does not yet work

## Name of the array to create.
array_name <- "ex_3"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

create_array <- function(uri) {
  ## Check if the array already exists.
  if (tiledb_object_type(uri) == "ARRAY") {
    message("Array already exists.")
    return(invisible(NULL))
  }

  ## The array will be 10x5 with dimensions "rows" and "cols", with domains [1,10] and [1,5].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(ISOdatetime(2010,1,1,0,0,0),
                                                     ISOdatetime(2029,12,31,23,59,59.999)),
                                           10L, type = "DATETIME_MS"),
                                tiledb_dim("cols", domain=c(1L,5L), tile=1L, type = "INT32")))

  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32"),
                                               tiledb_attr("b", type = "FLOAT64"),
                                               #tiledb_attr("c", type = "CHAR", ncells=NA_integer_),
                                               tiledb_attr("d", type = "DATETIME_DAY"),
                                               tiledb_attr("e", type = "DATETIME_MS")),
                                sparse=TRUE)

  ## Create the (empty) array on disk.
  tiledb_array_create(uri, schema)
}

write_array <- function(uri) {
  data <- list(seq(1:50),
               as.double(seq(101,150)),
               #c(letters[1:26], "brown", "fox", LETTERS[1:22])[1:10],
               as.Date("2020-01-01") + cumsum(runif(50)*5),
               ISOdatetime(2020,1,1,6,0,0) + cumsum(rnorm(50) * 1e5))
  ## Open the array and write to it.
  A <- tiledb_sparse(uri = uri)
  coords <- ISOdatetime(2020,1,1,0,0,0) + (1:50)*60
  A[coords] <- data
}

set.seed(42)
#if (tiledb_object_type(array_name) != "ARRAY") {
if (!dir.exists(array_name)) {
  create_array(uri)
  write_array(uri)
}
