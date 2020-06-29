
library(tiledb)

## Given an account at TileDB Cloud with username and password we can access the data
## Here we access username and password as environment variables
config <- tiledb_config()
config["rest.username"] <- Sys.getenv("TILEDB_REST_USERNAME")
config["rest.password"] <- Sys.getenv("TILEDB_REST_PASSWORD")
ctx <- tiledb_ctx(config)

array_name <- "tiledb://TileDB-Inc/quickstart_sparse"

arr <- tiledb_array(array_name, query_type="READ", as.data.frame=TRUE)
show(arr[])
