library(tinytest)
library(tiledb)

#isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (Sys.info()[["sysname"]] == "Windows") exit_file("Skip on Windows")

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.11.0") exit_file("Needs TileDB 2.11.* or later")

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


## -- testing 'create empty following by extending'
if (tiledb_version(TRUE) < "2.17.3") exit_file("Needs TileDB 2.17.3 or later")
uri <- tempfile()
dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, 20L), 10L, "INT32"))
attrs <- c(tiledb_attr("a", type = "INT32"), tiledb_attr("b", type = "INT32"))
schema <- tiledb_array_schema(dom, attrs = attrs, sparse = TRUE)
schema <- tiledb_array_schema_set_enumeration_empty(schema, attrs[[2]], "an_enum")
tiledb_array_create(uri, schema)
df <- data.frame(rows = 1:10, a = 100 + 0:9, b = rep(c(1L, 2L), each=5))
A <- tiledb_array(uri)
A[] <- df
arr <- tiledb_array(uri, return_as="data.frame")[]
expect_true(is.factor(arr[, "b"]))
expect_equal(levels(arr[, "b"]), character()) # empty levels vector
## now alter array
ase <- tiledb_array_schema_evolution()
arr <- tiledb_array(uri)
arr <- tiledb_array_open(arr, "READ")
ase <- tiledb_array_schema_evolution_extend_enumeration(ase, arr, "an_enum", c("red", "green"))
tiledb_array_schema_evolution_array_evolve(ase, uri)
arr <- tiledb_array(uri, return_as="data.frame")[]
expect_equal(levels(arr[, "b"]), c("red", "green"))
## append to array
df <- data.frame(rows = 11:20, a = 100 + 10:19, b = rep(c(3L, 4L), each=5))
A <- tiledb_array(uri)
A[] <- df
## and alter again
ase <- tiledb_array_schema_evolution()
arr <- tiledb_array(uri)
arr <- tiledb_array_open(arr, "READ")
ase <- tiledb_array_schema_evolution_extend_enumeration(ase, arr, "an_enum", c("blue", "orange"))
tiledb_array_schema_evolution_array_evolve(ase, uri)
arr <- tiledb_array(uri, return_as="data.frame")[]
expect_equal(levels(arr[, "b"]), c("red", "green", "blue", "orange"))
