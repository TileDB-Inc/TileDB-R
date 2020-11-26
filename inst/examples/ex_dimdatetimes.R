## quick check of various dimension types

suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

atttype <- "INT32"
intmax <- .Machine$integer.max         # shorthand

## Name of the array to create.
array_name <- "ex_dimdatetimes"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

dimtypes <- c("DATETIME_YEAR",  # year
              "DATETIME_MONTH", # month
              "DATETIME_WEEK",  # week
              "DATETIME_DAY",   # date
              "DATETIME_HR",    # hour
              "DATETIME_MIN",   # minute
              "DATETIME_SEC",   # second
              "DATETIME_MS",    # millisecond
              "DATETIME_US",    # microsecond
              "DATETIME_NS",    # nanosecond
              "DATETIME_PS",    # picosecond
              "DATETIME_FS",    # femtosecond
              "DATETIME_AS"     # attosecond
              )

for (dtype in dimtypes) {
    cat("Creating", dtype, "... ")
    if (tiledb_vfs_is_dir(uri)) {
        #message("Removing existing uri")
        tiledb_vfs_remove_dir(uri)
    }

    dom <- switch(dtype,
                  "DATETIME_YEAR"  =,
                  "DATETIME_MONTH" =,
                  "DATETIME_WEEK"  =,
                  "DATETIME_DAY"   = c(-intmax, intmax),
                  "DATETIME_HR"    =,
                  "DATETIME_MIN"   =,
                  "DATETIME_SEC"   =,
                  "DATETIME_MS"    =,
                  "DATETIME_US"    =, #c(-1e18, 1e18),
                  "DATETIME_NS"    =, #(as.nanotime("1950-01-01T00:00:00.000000000+00:00"),
                                      #as.nanotime("2030-12-31T23:59:59.999999999+00:00")),
                  "DATETIME_PS"    =,
                  "DATETIME_FS"    =,
                  "DATETIME_AS"    = c(-5e18, 5e18)  #c(as.integer64(-1e17), as.integer64(1e17))
                  )
    tile <- 10000
    domain <- tiledb_domain(tiledb_dim("row", dom, tile, dtype))
    attrib <- tiledb_attr("attr", type = "INT32")
    schema <- tiledb_array_schema(domain, attrib, sparse=TRUE)
    tiledb_array_create(uri, schema)


    arr <- tiledb_array(uri, as.data.frame=TRUE)

    dvec <- switch(dtype,
                   "DATETIME_YEAR"  = c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")),
                   "DATETIME_MONTH" = c(as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01")),
                   "DATETIME_WEEK"  = c(as.Date("2020-01-01"), as.Date("2020-01-08"), as.Date("2020-01-15")),
                   "DATETIME_DAY"   = as.Date("2020-01-01") + 0:2,
                   "DATETIME_HR"    = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   "DATETIME_MIN"   = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   "DATETIME_SEC"   = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   "DATETIME_MS"    = as.POSIXct("2000-01-01 00:00:00") + (0:2)*3600 + rep(0.001,3),
                   ## POSIXct can do a bit less than 1 microsec so we set it to 2 on purpose
                   "DATETIME_US"    = as.POSIXct("2000-01-01 00:00:00") + (0:2)*3600 + rep(0.000002,3),
                   "DATETIME_NS"    =,
                   "DATETIME_PS"    =,
                   "DATETIME_FS"    =,
                   "DATETIME_AS"    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2)*1e9
                   )
    avec <- 10^(1:3)
    data <- data.frame(row = dvec, attr = avec)
    cat("writing ... ")
    arr[] <- data

    cat("reading ... ")
    arr2 <- tiledb_array(uri, as.data.frame=TRUE)
    readdata <- arr2[]

    cat("(",format(readdata[1,1]), ",", format(readdata[2,1]), ",", format(readdata[3,1]), ") ", sep="")

    cat("checking ... ")
    stopifnot(all.equal(data, readdata))
    #print(arr2[])
    cat("done.\n")
}
