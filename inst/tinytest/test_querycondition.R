library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

#if (Sys.getenv("_RUNNING_UNDER_VALGRIND_", "FALSE") == "TRUE" && Sys.Date() < as.Date("2022-08-06")) exit_file("Skipping under valgrind until Aug 6")

## GitHub Actions had some jobs killed on the larger data portion so we dial mem use down
if (Sys.getenv("CI") != "") set_allocation_size_preference(1024*1024*5)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.3.0") exit_file("TileDB Query Condition requires TileDB 2.3.* or greater")

## simple data.frame to test against
D <- data.frame(a=1:20,
                b=seq(101,120)+0.5)
uri <- tempfile()
fromDataFrame(D, uri, sparse=TRUE)
arr <- tiledb_array(uri)
qry <- tiledb_query(arr, "READ")
rows <- integer(20)
cola <- integer(20)
colb <- numeric(20)
tiledb_query_set_buffer(qry, "__tiledb_rows", rows)
tiledb_query_set_buffer(qry, "a", cola)
tiledb_query_set_buffer(qry, "b", colb)

# check a >= 2 && a < 3
lhs <- tiledb_query_condition_init("a", 2L, "INT32", "GE")
rhs <- tiledb_query_condition_init("a", 3L, "INT32", "LT")
qc <- tiledb_query_condition_combine(lhs, rhs, "AND")
qry <- tiledb_query_set_condition(qry, qc)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
n <- tiledb_query_result_buffer_elements(qry, "a")
ndf <- data.frame(rows=rows,a=cola,b=colb)[1:n,]
expect_equal(nrow(ndf), 1)
expect_equal(ndf[1,"a"], 2L)
tiledb_array_close(arr)
rm(qry)

## check a >= 2
qry <- tiledb_query(arr, "READ")
rows <- integer(20)
cola <- integer(20)
colb <- numeric(20)
tiledb_query_set_buffer(qry, "__tiledb_rows", rows)
tiledb_query_set_buffer(qry, "a", cola)
tiledb_query_set_buffer(qry, "b", colb)
lhs <- tiledb_query_condition_init("a", 2L, "INT32", "GE")
qry <- tiledb_query_set_condition(qry, lhs)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
n <- tiledb_query_result_buffer_elements(qry, "a")
ndf <- data.frame(rows=rows,a=cola,b=colb)[1:n,]
expect_equal(nrow(ndf), 19)
tiledb_array_close(arr)
rm(qry)

## check a != 2 && a != 12
qry <- tiledb_query(arr, "READ")
rows <- integer(20)
cola <- integer(20)
colb <- numeric(20)
tiledb_query_set_buffer(qry, "__tiledb_rows", rows)
tiledb_query_set_buffer(qry, "a", cola)
tiledb_query_set_buffer(qry, "b", colb)
lhs <- tiledb_query_condition_init("a", 2L, "INT32", "NE")
rhs <- tiledb_query_condition_init("a", 12L, "INT32", "NE")
qc <- tiledb_query_condition_combine(lhs, rhs, "AND")
qry <- tiledb_query_set_condition(qry, qc)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
n <- tiledb_query_result_buffer_elements(qry, "a")
ndf <- data.frame(rows=rows,a=cola,b=colb)[1:n,]
expect_equal(nrow(ndf), 18)
tiledb_array_close(arr)
rm(qry)

## check a >=5 && b <= 115
qry <- tiledb_query(arr, "READ")
rows <- integer(20)
cola <- integer(20)
colb <- numeric(20)
tiledb_query_set_buffer(qry, "__tiledb_rows", rows)
tiledb_query_set_buffer(qry, "a", cola)
tiledb_query_set_buffer(qry, "b", colb)
lhs <- tiledb_query_condition_init("a", 5L, "INT32", "GE")
rhs <- tiledb_query_condition_init("b", 115, "FLOAT64", "LE")
qc <- tiledb_query_condition_combine(lhs, rhs, "AND")
qry <- tiledb_query_set_condition(qry, qc)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
n <- tiledb_query_result_buffer_elements(qry, "a")
ndf <- data.frame(rows=rows,a=cola,b=colb)[1:n,]
expect_equal(nrow(ndf), 10)
tiledb_array_close(arr)
rm(qry)

## check b == 115.5 (yes, yes, yes, we know EQ is dicey on floats; can remove this if it croaks)
qry <- tiledb_query(arr, "READ")
rows <- integer(20)
cola <- integer(20)
colb <- numeric(20)
tiledb_query_set_buffer(qry, "__tiledb_rows", rows)
tiledb_query_set_buffer(qry, "a", cola)
tiledb_query_set_buffer(qry, "b", colb)
qc <- tiledb_query_condition_init("b", 115.5, "FLOAT64", "EQ")
qry <- tiledb_query_set_condition(qry, qc)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
n <- tiledb_query_result_buffer_elements(qry, "a")
ndf <- data.frame(rows=rows,a=cola,b=colb)[1:n,]
expect_equal(nrow(ndf), 1)
tiledb_array_close(arr)
rm(qry)

## check b >= 115.4 && b <= 115.6
qry <- tiledb_query(arr, "READ")
rows <- integer(20)
cola <- integer(20)
colb <- numeric(20)
tiledb_query_set_buffer(qry, "__tiledb_rows", rows)
tiledb_query_set_buffer(qry, "a", cola)
tiledb_query_set_buffer(qry, "b", colb)
lhs <- tiledb_query_condition_init("b", 115.4, "FLOAT64", "GE")
rhs <- tiledb_query_condition_init("b", 115.6, "FLOAT64", "LE")
qc <- tiledb_query_condition_combine(lhs, rhs, "AND")
qry <- tiledb_query_set_condition(qry, qc)
tiledb_query_submit(qry)
tiledb_query_finalize(qry)
n <- tiledb_query_result_buffer_elements(qry, "a")
ndf <- data.frame(rows=rows,a=cola,b=colb)[1:n,]
expect_equal(nrow(ndf), 1)
tiledb_array_close(arr)
rm(qry)


if (tiledb_version(TRUE) < "2.7.0") exit_file("Needs TileDB 2.7.* or later")

## tiledb_array support
if (!requireNamespace("palmerpenguins", quietly=TRUE)) exit_file("remainder needs 'palmerpenguins'")
library(palmerpenguins)
uri <- tempfile()
fromDataFrame(penguins, uri, sparse=TRUE)
unconstr <- tiledb_array(uri, as.data.frame=TRUE)
expect_equal(NROW(unconstr[]), 344L)    # no condition -> 344 rows

qc <- tiledb_query_condition_init("year", 2009, "INT32", "EQ")
arrwithqc <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
res <- arrwithqc[]
expect_equal(NROW(res), 120L)    		# year 2009 only -> 120 rows
expect_true(all(res$year == 2009))

arr2 <- tiledb_array(uri, as.data.frame=TRUE)
expect_equal(NROW(arr2[]), 344L)    	# no condition -> 344 rows
query_condition(arr2) <- qc
expect_equal(NROW(arr2[]), 120L)    	# year 2009 only -> 120 rows

qc2 <- tiledb_query_condition_init("bill_length_mm", 40.0, "FLOAT64", "LT")
qc3 <- tiledb_query_condition_combine(qc, qc2, "AND")
query_condition(arr2) <- qc3
res <- arr2[]
expect_equal(NROW(res), 34L)
expect_true(all(res$bill_length_mm < 40))
expect_true(all(res$year == 2009))

unlink(uri, recursive=TRUE)

## parse query condition support
uri <- tempfile()
fromDataFrame(penguins, uri, sparse=TRUE)
qc <- parse_query_condition(year == 2009)
arrwithqc <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
res <- arrwithqc[]
expect_equal(NROW(res), 120L)    # year 2009 only -> 120 rows
expect_true(all(res$year == 2009))

qc2 <- parse_query_condition(year == 2009 && bill_length_mm <= 39.99)
arrwithqc2 <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc2)
res <- arrwithqc2[]
expect_equal(NROW(res), 34L)
expect_true(all(res$bill_length_mm < 40))
expect_true(all(res$year == 2009))

unlink(uri, recursive=TRUE)

## qc and string_ascii
uri <- tempfile()
fromDataFrame(na.omit(penguins), uri, sparse=TRUE)
qc3 <- parse_query_condition(sex == "male")
arrwithqc3 <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc3)
res <- arrwithqc3[]
expect_equal(NROW(res), 168L)
expect_true(all(res$sex == "male"))

qc <- tiledb_query_condition_init("sex", "female", "ASCII", "EQ")
arrwithqc <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
res <- arrwithqc[]
expect_equal(NROW(res), 165L)
expect_true(all(res$sex != "male"))

## check type inference for edge cases
edgecases <- data.frame(x1 = "a1", x2 = 1L, x3 = "_1", x4 = "1.1.1")

uri <- tempfile()
fromDataFrame(edgecases, uri, sparse=TRUE)

qcx1 <- tiledb::parse_query_condition(x1 == "a1")
arrx1 <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qcx1)
res <- arrx1[]
expect_equal(res$x1, "a1")

qcx2 <- tiledb::parse_query_condition(x2 == 1L)
arrx2 <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qcx2)
res <- arrx2[]
expect_equal(res$x2, 1L)

qcx3 <- tiledb::parse_query_condition(x3 == "_1")
arrx3 <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qcx3)
expect_equal(arrx3[]$x3, "_1")

qcx4 <- tiledb::parse_query_condition(x4 == "1.1.1")
arrx4 <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qcx4)
expect_equal(arrx4[]$x4, "1.1.1")


## edge case of text only array
df <- data.frame(abb = state.abb,		# builtin-data
                 region = state.region,	# idem
                 name = state.name)     # idem
uri <- tempfile()
fromDataFrame(df, uri, col_index="abb", sparse=TRUE)
fullarr <- tiledb_array(uri, as.data.frame=TRUE)[]
expect_equal(dim(fullarr), c(50,3))
subarr <- tiledb_array(uri, as.data.frame=TRUE,
                       query_condition=parse_query_condition(region == "Northeast"))[]
expect_equal(dim(subarr), c(9,3))


## -- Testing OR condition

## Test minimal version
if (tiledb_version(TRUE) < "2.10.0") exit_file("Remainder needs 2.10.* or later")

## Re-create penguins
uri <- tempfile()
fromDataFrame(penguins, uri, sparse=TRUE)

## Basics
qc <- tiledb_query_condition_init("year", 2009, "INT32", "EQ")
arrwithqc <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arrwithqc[]), 120L)

lhs <- tiledb_query_condition_init("year", 2008, "INT32", "GE")
rhs <- tiledb_query_condition_init("year", 2008, "INT32", "LE")
qc <- tiledb_query_condition_combine(lhs, rhs, "AND")
arrwithqc <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arrwithqc[]), 114L)  # basically a different way of writing EQ via '<= && >='

lhs <- tiledb_query_condition_init("year", 2008, "INT32", "GE")
rhs <- tiledb_query_condition_init("year", 2008, "INT32", "LE")
qc <- tiledb_query_condition_combine(lhs, rhs, "OR")
arrwithqc <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arrwithqc[]), 344L)  # the OR makes it unconstrained via '<= || >='

## simple OR
qc <- parse_query_condition(species == "Adelie" || species == "Chinstrap")
arr <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
## Note that in R '||' is used for length-1 comparison, and '|' along a vector so '|' here
expect_equal(NROW(arr[]), sum(with(penguins, species == "Adelie" | species == "Chinstrap")))

## three elements works too
qc <- parse_query_condition(species == "Adelie" || species == "Chinstrap" || year >= 2009)
arr <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arr[]),
             sum(with(penguins, species == "Adelie" | species == "Chinstrap" | year >= 2009)))

## three elements works too as does mixing AND and OR
qc <- parse_query_condition(species == "Adelie" || species == "Chinstrap" && year >= 2009)
arr <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arr[]),
             sum(with(penguins, species == "Adelie" | species == "Chinstrap" & year >= 2009)))

## empty sets are fine
qc <- parse_query_condition(year < 2008 || year > 2010)
arr <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arr[]),
             sum(with(penguins, year < 2008 | year > 2010)))

## Overlapping ranges
qc <- parse_query_condition(year < 2009 && year < 2010)
arr <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arr[]),
             sum(with(penguins, year < 2009)))

qc <- parse_query_condition(year <= 2009 && year >= 2009)
arr <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arr[]),
             sum(with(penguins, year == 2009)))

qc <- parse_query_condition(year < 2009 || year < 2010)
arr <- tiledb_array(uri, as.data.frame=TRUE, query_condition=qc)
expect_equal(NROW(arr[]),
             sum(with(penguins, year < 2010)))


## query conditions over different types
suppressMessages(library(bit64))
n <- 20L
dir.create(tmp <- tempfile())
dim <- tiledb_dim("rows", domain=c(1L,n), type="INT32", tile=1L)
dom <- tiledb_domain(dim)
sch <- tiledb_array_schema(dom,
                           attrs = c(tiledb_attr("int8",   type="INT8"),
                                     tiledb_attr("uint8",  type="UINT8"),
                                     tiledb_attr("int16",  type="INT16"),
                                     tiledb_attr("uint16", type="UINT16"),
                                     tiledb_attr("int32",  type="INT32"),
                                     tiledb_attr("uint32", type="UINT32"),
                                     tiledb_attr("int64",  type="INT64"),
                                     tiledb_attr("uint64", type="UINT64"),
                                     tiledb_attr("float32",type="FLOAT32"),
                                     tiledb_attr("float64",type="FLOAT64")),
                           sparse = TRUE)
tiledb_array_create(tmp, sch)
arr <- tiledb_array(tmp)
## given the existing schema these values will be cast appropriately
arr[] <- data.frame(rows = 1:n,
                    int8 = 1:n,
                    uint8 = 1:n,
                    int16 = 1:n,
                    uint16 = 1:n,
                    int32 = 1:n,
                    uint32 = 1:n,
                    int64 = as.integer64(1:n),
                    uint64 = as.integer64(1:n),
                    float32 = 1:n,
                    float64 = 1:n)

for (col in c("int8", "uint8", "int16", "uint16", "int32", "uint32",
              "int64", "uint64", "float32", "float64")) {
    val <- if (grepl("int64", col)) as.integer64(10) else 10
    expect_silent(qc <- tiledb_query_condition_init(col, val, toupper(col), "GT"))
    arr <- tiledb_array(tmp, return_as="data.frame", query_condition = qc)
    expect_equal( NROW(arr[]), 10)      # ten rows if we restrict to 'value' > 10
}

## test on dense array (without dims) and query condition
uri <- tempfile()
fromDataFrame(airquality, uri, col_index=c("Month", "Day"))  # dense array
res <- tiledb_array(uri, return_as="data.frame", extended=FALSE,
                    query_condition=parse_query_condition(Temp > 90))[]
expect_equal(NROW(res), 14)
