library(tinytest)
library(tiledb)

if (Sys.info()[["sysname"]] == "Windows") exit_file("Skip on Windows")
if (Sys.getenv("CI", "") == "") exit_file("Skip unextended test run")

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.17.0") exit_file("Needs TileDB 2.17.* or later")

df <- data.frame(key=letters[1:10], val=c(1:5,5:1))
uri <- tempfile()
arr <- fromDataFrame(df, uri)

sch <- schema(uri)
attrs <- attrs(sch)
attr <- attrs$val   # copy of attribute

## First drop existing attribute
ase <- tiledb_array_schema_evolution()
ase <- tiledb_array_schema_evolution_drop_attribute(ase, "val")
tiledb_array_schema_evolution_array_evolve(ase, uri)

## Second add enumeration under a name
ase <- tiledb_array_schema_evolution()
enums <- c("blue", "green", "orange", "pink", "red")
ase <- tiledb_array_schema_evolution_add_enumeration(ase, "frobo", enums)

## Third connect the attribute to the enum and add it back in
attr <- tiledb_attribute_set_enumeration_name(attr, "frobo")
ase <- tiledb_array_schema_evolution_add_attribute(ase, attr)
tiledb_array_schema_evolution_array_evolve(ase, uri)

## check as data.frame
arr <- tiledb_array(uri, return_as="data.frame")
res <- arr[]
expect_true(is.factor(res$val))
expect_equal(levels(res$val), enums)
expect_equal(as.integer(res$val), c(1:5,5:1))

## check as arrow
if (!requireNamespace("arrow", quietly=TRUE)) exit_file("No 'arrow' package.")
arr <- tiledb_array(uri, return_as="arrow")
res <- arr[]
v <- res[["val"]]$as_vector()
expect_true(is.factor(v))
expect_equal(levels(v), enums)
expect_equal(as.integer(v), c(1:5,5:1))
