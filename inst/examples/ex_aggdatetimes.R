## quick check of various dimension types

suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

dimtype <- "INT32"
intmax <- .Machine$integer.max         # shorthand

## Name of the array to create.
array_name <- "ex_aggdatetimes"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)
if (tiledb_vfs_is_dir(uri)) res <- tiledb_vfs_remove_dir(uri)

tile <- 1000L
domain <- tiledb_domain(tiledb_dim("row", c(-intmax,intmax), tile, dimtype))
attrib <- c(tiledb_attr("year",  type = "DATETIME_YEAR"),  # year
            tiledb_attr("month", type = "DATETIME_MONTH"), # month
            tiledb_attr("week",  type = "DATETIME_WEEK"),  # week
            tiledb_attr("day",   type = "DATETIME_DAY"),   # date
            tiledb_attr("hr",    type = "DATETIME_HR"),    # hour
            tiledb_attr("min",   type = "DATETIME_MIN"),   # minute
            tiledb_attr("sec",   type = "DATETIME_SEC"),   # second
            tiledb_attr("ms",    type = "DATETIME_MS"),    # millisecond
            tiledb_attr("us",    type = "DATETIME_US"),    # microsecond
            tiledb_attr("ns",    type = "DATETIME_NS"),    # nanosecond
            tiledb_attr("ps",    type = "DATETIME_PS"),    # picosecond
            tiledb_attr("fs",    type = "DATETIME_FS"),    # femtosecond
            tiledb_attr("as",    type = "DATETIME_AS")     # attosecond
            )
schema <- tiledb_array_schema(domain, attrib, sparse=TRUE)
res <- tiledb_array_create(uri, schema)

arr <- tiledb_array(uri, as.data.frame=TRUE)

dvec <- 1:3
data <- data.frame(row = dvec,
                   year  = c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")),
                   month = c(as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01")),
                   week  = c(as.Date("2020-01-01"), as.Date("2020-01-08"), as.Date("2020-01-15")),
                   day   = as.Date("2020-01-01") + 0:2,
                   hr    = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   min   = as.POSIXct("2020-01-01 00:00:00") + (0:2)*60,
                   sec   = as.POSIXct("2020-01-01 00:00:00") + (0:2),
                   ms    = as.POSIXct("1970-01-01 00:00:00") + (0:2)*0.001,
                   us    = as.POSIXct("1970-01-01 00:00:00") + (0:2)*0.000001,
                   ns    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2),
                   ps    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2),
                   fs    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2),
                   as    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2)
                   )

cat("writing ... ")
arr[] <- data

cat("reading ... ")
arr2 <- tiledb_array(uri, as.data.frame=TRUE)
readdata <- arr2[]

cat("checking ... ")
stopifnot(all.equal(data, readdata))

cat("done.\n\n")
print(readdata)

cat("\nAs int64:\n")
datetimes_as_int64(arr2) <- TRUE
readdata2 <- arr2[]
print(readdata2)
