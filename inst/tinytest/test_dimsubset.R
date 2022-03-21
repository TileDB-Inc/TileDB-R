
## this test file has an implicit dependency on package 'nycflights13'

library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

if (!requireNamespace("nycflights13", quietly=TRUE)) exit_file("Needed 'nycflights13' package missing")

ctx <- tiledb_ctx(limitTileDBCores())

op <- options()
options(stringsAsFactors=FALSE)       # accomodate R 3.*
dir.create(tmp <- tempfile())

library(nycflights13)

set_allocation_size_preference(1e8)
dom <- tiledb_domain(dims = c(tiledb_dim("carrier", NULL, NULL, "ASCII"),
                              tiledb_dim("origin", NULL, NULL, "ASCII"),
                              tiledb_dim("dest", NULL, NULL, "ASCII"),
                              tiledb_dim("time_hour",
                                         c(as.POSIXct("2012-01-01 00:00:00"),
                                           as.POSIXct("2014-12-31 23:59:99")), 1000, "DATETIME_SEC")))

sch <- tiledb_array_schema(dom,
                           attrs <- c(tiledb_attr("year", type = "INT32"),
                                      tiledb_attr("month", type = "INT32"),
                                      tiledb_attr("day", type = "INT32"),
                                      tiledb_attr("dep_time", type = "INT32", nullable = TRUE),
                                      tiledb_attr("sched_dep_time", type = "INT32"),
                                      tiledb_attr("dep_delay", type = "FLOAT64", nullable = TRUE),
                                      tiledb_attr("arr_time", type = "INT32"),
                                      tiledb_attr("sched_arr_time", type = "INT32"),
                                      tiledb_attr("arr_delay", type = "FLOAT64", nullable = TRUE),
                                      tiledb_attr("flight", type = "INT32", nullable = TRUE),
                                      tiledb_attr("tailnum", type = "ASCII", ncells = NA, nullable = TRUE),
                                      tiledb_attr("air_time", type = "FLOAT64", nullable = TRUE),
                                      tiledb_attr("distance", type = "FLOAT64"),
                                      tiledb_attr("hour", type = "FLOAT64"),
                                      tiledb_attr("minute", type = "FLOAT64")),
                           sparse = TRUE,
                           allows_dups = TRUE)
res <- tiledb_array_create(tmp, sch)

arr <- tiledb_array(res)
## we reorder the data.frame / tibble on the fly, and yes there are a number of ways to do this
arr[] <- list(carrier = flights$carrier,
              origin = flights$origin,
              dest = flights$dest,
              time_hour = flights$time_hour,
              year = flights$year,
              month = flights$month,
              day = flights$day,
              dep_time = flights$dep_time,
              sched_dep_time = flights$sched_dep_time,
              dep_delay = flights$dep_delay,
              arr_time = flights$arr_time,
              sched_arr_time = flights$sched_arr_time,
              arr_delay = flights$arr_delay,
              flight = flights$flight,
              tailnum = flights$tailnum,
              air_time = flights$air_time,
              distance = flights$distance,
              hour = flights$hour,
              minute = flights$minute)

newarr <- tiledb_array(tmp, as.data.frame=TRUE)
dat <- newarr[]
expect_equal(nrow(dat), nrow(flights))
## compare some columns, as we re-order comparing all trickers
expect_equal(sort(dat$carrier), sort(as.character(flights$carrier)))
expect_equal(table(dat$origin), table(flights$origin))

## test list of four with one null
selected_ranges(newarr) <- list(cbind("AA","AA"),
                                cbind("JFK","JFK"),
                                cbind("BOS", "BOS"),
                                NULL)
dat <- newarr[]
expect_equal(unique(dat$carrier), "AA")
expect_equal(unique(dat$origin), "JFK")
expect_equal(unique(dat$dest), "BOS")

## test named lists with one element
selected_ranges(newarr) <- list(carrier = cbind("AA","AA"))
dat <- newarr[]
expect_equal(unique(dat$carrier), "AA")

selected_ranges(newarr) <- list(origin = cbind("JFK","JFK"))
dat <- newarr[]
expect_equal(unique(dat$origin), "JFK")

selected_ranges(newarr) <- list(dest = cbind("BOS", "BOS"))
dat <- newarr[]
expect_equal(unique(dat$dest), "BOS")

daterange <- c(as.POSIXct("2013-01-10 00:00:00"), as.POSIXct("2013-01-19 23:59:99"))
selected_ranges(newarr) <- list(time_hour = cbind(daterange[1], daterange[2]))
dat <- newarr[]
expect_true(all(dat$time_hour >= daterange[1]))
expect_true(all(dat$time_hour <= daterange[2]))


## test named lists of two
selected_ranges(newarr) <- list(dest = cbind("BOS", "BOS"), origin = cbind("LGA", "LGA"))
dat <- newarr[]
expect_equal(unique(dat$dest), "BOS")
expect_equal(unique(dat$origin), "LGA")

selected_ranges(newarr) <- list(origin = cbind("JFK", "JFK"), carrier = cbind("AA", "AA"))
dat <- newarr[]
expect_equal(unique(dat$carrier), "AA")
expect_equal(unique(dat$origin), "JFK")

selected_ranges(newarr) <- list(dest = cbind("BOS", "BOS"), origin = cbind("JFK", "LGA"))
dat <- newarr[]
expect_equal(unique(dat$origin), c("JFK", "LGA"))
expect_equal(unique(dat$dest), "BOS")
