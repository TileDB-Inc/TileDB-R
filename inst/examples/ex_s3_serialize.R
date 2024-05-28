
library(tiledb)

uri <- "s3://tiledb-dirk/serializeDemo"
tiledb_vfs_serialize(palmerpenguins::penguins, uri)

newpp <- tiledb_vfs_unserialize(uri)
head(newpp)
