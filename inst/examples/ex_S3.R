
library(tiledb)

## replace this URI with one for a public s3 bucket,
## or one accessible under your aws access key
array_name <- "s3://tiledb-public-us-west-1/test-array-4x4"

cfg <- tiledb_config()
cfg["vfs.s3.region"] <- "us-west-1"
ctx <- tiledb_ctx(cfg)
## ctx object is now updated globally

## access the s3 bucket, here simply querying the type
tiledb_object_type(array_name)
