
library(tiledb)

uri <- "s3://tiledb-dirk/tempfolder/penguins.csv"
pp <- read.csv(vfs_file(uri))
summary(pp)

## write.csv also works via vfs_file(...)
