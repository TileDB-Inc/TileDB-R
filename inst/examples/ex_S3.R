
library(tiledb)

## a public s3 bucket
array_name <- "s3://tiledb-public-us-west-1/test-array-4x4"

cfg <- tiledb_config()
cfg["vfs.s3.region"] <- "us-west-1"

ctx <- tiledb_ctx(cfg)
tiledb_set_context(ctx)

tiledb_object_type(array_name)
