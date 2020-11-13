## quick check of various dimension types

suppressMessages({
  library(tiledb)
  library(nanotime)
  library(bit64)
})

atttype <- "INT32"

## Name of the array to create.
array_name <- "ex_dimemsions"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)
library(tiledb)

dimtypes <- c("ASCII",  		# Variable length string
              "INT8",   		# 8-bit integer
              "UINT8",  		# 8-bit unsigned integer
              "INT16",  		# 16-bit integer
              "UINT16", 		# 16-bit unsigned integer
              "INT32",  		# 32-bit integer
              "UINT32", 		# 32-bit unsigned integer
              "INT64",  		# 64-bit integer
              "UINT64", 		# 64-bit unsigned integer
              "FLOAT32",		# 32-bit floating point
              "FLOAT64",		# 64-bit floating point
              "DATETIME_YEAR",  # year
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
                  "ASCII"          = NULL,
                  "INT8"           =,
                  "UINT8"          =,
                  "INT16"          =,
                  "UINT16"         =,
                  "UINT32"         =,
                  "INT32"          = c(1L, 100L),
                  "INT64"          =,
                  "UINT64"         = c(as.integer64(1), as.integer64(1000)),
                  "FLOAT32"        =,
                  "FLOAT64"        = c(1, 1000),
                  "DATETIME_YEAR"  = c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_MONTH" = c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_WEEK"  = c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_DAY"   = c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_HR"    =,
                  "DATETIME_MIN"   =,
                  "DATETIME_SEC"   = c(as.POSIXct("2000-01-01 00:00:00"),
                                       as.POSIXct("2030-12-31 23:00:59")),
                  "DATETIME_MS"    =,
                  "DATETIME_US"    =,
                  "DATETIME_NS"    = c(as.nanotime("1970-01-01T00:00:00.000000000+00:00"),
                                       as.nanotime("2030-12-31T23:59:59.999999999+00:00")),
                  "DATETIME_PS"    =,
                  "DATETIME_FS"    =,
                  "DATETIME_AS"    = c(as.integer64(1), as.integer64(1e18))
                  )

    if (dtype %in% c("DATETIME_MS", "DATETIME_US", "DATETIME_NS",
                     "DATETIME_PS", "DATETIME_FS", "DATETIME_AS"))
        tile <- 1000
    else
        tile <- dom[1]                      # fallback

    domain <- tiledb_domain(tiledb_dim("row", dom, tile, dtype))
    attrib <- tiledb_attr("attr", type = "INT32")
    schema <- tiledb_array_schema(domain, attrib, sparse=TRUE)
    tiledb_array_create(uri, schema)


    arr <- tiledb_array(uri, as.data.frame=TRUE)

    dvec <- switch(dtype,
                   "ASCII"   = LETTERS[1:3],
                   "INT8"    =,
                   "UINT8"   =,
                   "INT16"   =,
                   "UINT16"  =,
                   "UINT32"  =,
                   "INT32"   = 1:3,      # sequences are integers
                   "INT64"   =,
                   "UINT64"  = as.integer64(1:3),
                   "FLOAT32" =,
                   "FLOAT64" = as.numeric(1:3),
                   "DATETIME_YEAR" = c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")),
                   "DATETIME_MONTH" = c(as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01")),

                   "DATETIME_WEEK" = c(as.Date("2020-01-01"), as.Date("2020-01-08"), as.Date("2020-01-15")),
                   "DATETIME_DAY" = as.Date("2020-01-01") + 0:2,
                   "DATETIME_HR"  = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   "DATETIME_MIN" = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   "DATETIME_SEC" = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   "DATETIME_MS"  = as.POSIXct("2000-01-01 00:00:00") + (0:2)*3600 + rep(0.001,3),
                   ## POSIXct can do a bit less than 1 microsec so we set it to 2 on purpose
                   "DATETIME_US"  = as.POSIXct("2000-01-01 00:00:00") + (0:2)*3600 + rep(0.000002,3),
                   "DATETIME_NS"  = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2)*1e9,
                   "DATETIME_PS"  =,
                   "DATETIME_FS"  =,
                   "DATETIME_AS"  = as.integer64(1e12 + 0:2)
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
