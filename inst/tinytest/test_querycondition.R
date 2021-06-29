library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

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

## check b == 115.5 (yes, yes, yes, we know EQ dicey on floats; can remove this if it croaks)
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
