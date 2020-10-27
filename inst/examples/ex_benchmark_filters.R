
suppressMessages({
    library(tiledb)
    library(rbenchmark)
})

setwd("/tmp")

## NYC Flights data
url <- "https://raw.githubusercontent.com/eddelbuettel/data-examples/master/flights/flights14.csv"
flights <- data.table::fread(url, data.table=FALSE)


createArrays <- function() {
    fromDataFrame(flights, "flightsNONE",  sparse = TRUE, allows_dups = TRUE, filter = "NONE")
    fromDataFrame(flights, "flightsGZIP",  sparse = TRUE, allows_dups = TRUE, filter = "GZIP")
    fromDataFrame(flights, "flightsZSTD",  sparse = TRUE, allows_dups = TRUE, filter = "ZSTD")
    fromDataFrame(flights, "flightsLZ4",   sparse = TRUE, allows_dups = TRUE, filter = "LZ4")
    fromDataFrame(flights, "flightsBZIP2", sparse = TRUE, allows_dups = TRUE, filter = "BZIP2")
    fromDataFrame(flights, "flightsRLE",   sparse = TRUE, allows_dups = TRUE, filter = "RLE")
    fromDataFrame(flights, "flightsDD",    sparse = TRUE, allows_dups = TRUE, filter = "DOUBLE_DELTA")
}

time1 <- function() {
    get1 <- function(uri) {
        arr <- tiledb_array(uri)
        arr[]
    }
    cat("\nTiming reading all data\n")
    res <- benchmark(none=get1("flightsNONE"),
                     gzip=get1("flightsGZIP"),
                     zstd=get1("flightsZSTD"),
                     lz4=get1("flightsLZ4"),
                     bz2=get1("flightsBZIP2"),
                     rle=get1("flightsRLE"),
                     dd=get1("flightsDD"),
                     order="relative")
    print(res[,1:4])
    invisible(NULL)
}

time2 <- function() {
    get <- function(uri) {
        arr <- tiledb_array(uri)
        selected_ranges(arr) <- list(matrix(c(100,200,1300,1400),2,2,byrow=TRUE))
        arr[]
    }

    cat("\nTiming reading slice of data\n")
    res <- benchmark(none=get("flightsNONE"),
                     gzip=get("flightsGZIP"),
                     zstd=get("flightsZSTD"),
                     lz4=get("flightsLZ4"),
                     bz2=get("flightsBZIP2"),
                     rle=get("flightsRLE"),
                     dd=get("flightsDD"),
                     order="relative", replications=1000)
    print(res[,1:4])
    invisible(NULL)
}

createArrays()
time1()
time2()
