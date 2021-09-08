library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.4.0") exit_file("Needs TileDB 2.4.* or later")

df <- data.frame(key=letters[1:10],
                 val=1:10)
uri <- tempfile()
arr <- fromDataFrame(df, uri)

ase <- tiledb_array_schema_evolution()
attr <- tiledb_attr("foo", "INT32")
ase <- tiledb_array_schema_evolution_add_attribute(ase, attr)
ase <- tiledb_array_schema_evolution_array_evolve(ase, uri)
arr <- tiledb_array(uri, return_as="data.frame", extended=FALSE)
res <- arr[]
expect_equal(dim(res), c(10,3))
expect_equal(colnames(res), c("key", "val", "foo"))

ase <- tiledb_array_schema_evolution()
ase <- tiledb_array_schema_evolution_drop_attribute(ase, "val")
ase <- tiledb_array_schema_evolution_array_evolve(ase, uri)
arr <- tiledb_array(uri, return_as="data.frame", extended=FALSE)
res <- arr[]
expect_equal(dim(res), c(10,2))
expect_equal(colnames(res), c("key", "foo"))
