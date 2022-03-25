<!--
%\VignetteIndexEntry{Data Ingestion from SQL}
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteEncoding{UTF-8}
-->
---
title: "TileDB Ingestion from SQL: A Commented Example"
date: "2022-01-25"
css: "water.css"
---

## Introduction

[TileDB](https://www.tiledb.com/) provides the _Universal Data Engine_ that can be accessed in a
variety of ways. Users sometimes wonder how to transfer data from existing databases.  This short
vignettes shows an example relying on the [DBI](https://cran.r-project.org/package=DBI) package for
R. It offers a powerful and convenient abstraction layer on top a number of database backends with
connection packages that adhere to, and utilise, the DBI framework.  Some examples are the packages
(listed in alphabetical order) [duckdb](https://cran.r-project.org/package=duckdb),
[RClickhouse](https://cran.r-project.org/package=RClickhouse),
[RGreenplum](https://cran.r-project.org/package=RGreenplum),
[RJDBC](https://cran.r-project.org/package=RJDBC),
[RMariaDB](https://cran.r-project.org/package=RMariaDB),
[RMySQL](https://cran.r-project.org/package=RMySQL),
[ROracle](https://cran.r-project.org/package=ROracle),
[RPostgres](https://cran.r-project.org/package=RPostgres),
[RPostgreSQL](https://cran.r-project.org/package=RPostgreSQL),
[RPresto](https://cran.r-project.org/package=RPresto),
[RRedshiftSQL](https://cran.r-project.org/package=RRedshiftSQL),
[RSQLite](https://cran.r-project.org/package=RSQLite), and many more as seen via the
[CRAN page](https://cran.r-project.org/package=DBI).

We provide a simple example using
[RPostgreSQL](https://cran.r-project.org/package=RPostgreSQL) and an existing
database of historical stockmarket price data.

## Load Required Packages

The basic setup is straightforward. We load the required package
[RPostgreSQL](https://cran.r-project.org/package=RPostgreSQL) which in turn imports
[DBI](https://cran.r-project.org/package=DBI) as well as
[tiledb](https://cran.r-project.org/package=tiledb). We use
[data.table](https://cran.r-project.org/package=data.table) for its print method, the
[tibble](https://cran.r-project.org/package=tibble) package offers an alternative):

```r
library(RPostgreSQL)
library(data.table)
library(tiledb)
```

## Connect to Database

This step uses the DBI abstraction. A compliant backend driver can be loaded via `dbDriver`, and a
connection can be established via `dbConnect` using appropriate arguments `dbname`, `user`,
`password`, `host`, and `port`, as needed, with proper dispatching the implementation provided by
the driver.  The details depend on the chosen backend, this can be as simple as `con <-
dbConnect(RSQLite::SQLite(), ":memory:")` in the case of
[RSQLite](https://cran.r-project.org/package=RSQLite) and an in-memory (and likely
transient) database.

```r
## a local SQL db we have here -- about 617k rows
dbSetup <- function() {
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv,
                     user="...omitted...",
                     password="...omitted...", # Could use e.g. Sys.getenv("DB_PASSWD")
                     dbname="...omitted...")
    con
}
```

## Fetch Data

In the next step we fetch the data---and for simplicity issue just one `select` statement returning
a single `data.frame` (or here a `data.table` variant). In larger-than-memory settings the SQL query
could easily bucket by symbols, or date range, or ...


```r
getDataFromSQL <- function() {
    con <- dbSetup()
    sql <- "select * from stockprices order by symbol, date;"
    res <- dbGetQuery(con, sql)
    dbDisconnect(con)
    setDT(res)                          # create data.table
    res
}
```

## Writing Data to TileDB

Having read the data into memory we can use the TileDB R function `fromDataFrame`. It has numerous
option to configure, as well as sensible defaults (to for example enable ZSTD compression). Here we
select the first two columns for symbol and data as dimensions. Symbols, being text, do not set a
domain set.  For the date we set two 'safe' outer values for the range.

```r
storeDataTDB <- function(dat, uri) {
    fromDataFrame(dat, uri,
                  col_index=1:2,
                  tile_domain=list(date=c(as.numeric(as.Date("1985-01-01")),
                                          as.numeric(as.Date("2030-12-31")))))
}
```

The `mode="append"` argument of `fromDataFrame` can be used to append to an existing array to
support chunked operation.


### Reading Data Back In

Reading data from TileDB is a very standard operation of opening the URI, possibly specifying the
return type and possibly subsetting by dimension values, or attributes.  Here, for simplicity,
we just read everything.

```r
getDataTDB <- function(uri) {
    set_allocation_size_preference(1e7) # larger than local default value
    arr <- tiledb_array(uri, return_as="data.frame")
    res <- arr[]
    res
}

uri <- "/tmp/tiledb/beancounter"

res <- getDataFromSQL(con)
storeData(dat, uri)
chk <- getDataTDB(uri)
print(dim(chk))
cat("Done!\n")
```

## See Also

The vignette [TileDB MariaDB Examples](tiledb-mariadb-examples.html) shows to
use MariaDB via the MyTile integration of TileDB as a direct backend.

The [TileDB R Tutorial at useR! 2021](https://dirk.eddelbuettel.com/papers/useR2021_tiledb_tutorial.pdf) contained a
worked example of writing _much_ larger data set in chunks.  The process is very similar to the
simple example we showed here -- and in addition requires a suffient domain range for the dimension
along with a (sequential or parallel) loop of reading chunks and writing them to TileDB.

## Summary

This vignette provides a commented walk-through of a worked example of a SQL-to-TileDB data
ingestion.
