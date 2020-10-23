library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_fromdataframe", {
uri <- tempfile()
## turn factor into character
irisdf <- within(iris, Species <- as.character(Species))

fromDataFrame(irisdf, uri)

arr <- tiledb_array(uri, as.data.frame=TRUE)
newdf <- arr[]
if (getRversion() >= '4.0.0') newdf$Species <- as.factor(newdf$Species)
expect_equal(iris, newdf[,-1])
expect_equal(dim(irisdf), dim(newdf[,-1]))

## result comes back as factor by default
newdf <- within(newdf, Species <- as.character(Species))
expect_equal(irisdf, newdf[,-1])


## test attrs subselection
arr <- tiledb_array(uri, as.data.frame=TRUE,
                    attrs = c("Petal.Length", "Petal.Width"))
newdf <- arr[]
expect_equal(iris[, c("Petal.Length", "Petal.Width")], newdf[,-1])


## test list
arr <- tiledb_array(uri)
res <- arr[]
expect_equal(class(res), "list")
expect_equal(length(res), 6)            # plus one for 'rows'
#})

#test_that("tiledb_date_datetime_types", {
uri <- tempfile()

now <- Sys.time()
df <- data.frame(char=c("abc", "def", "g", "hijk"),
                 int=1:4,
                 dbl=sqrt(1:4),
                 date=Sys.Date() + 0:3,
                 #datetime=Sys.time() + 0.3,
                 stringsAsFactors = FALSE)
## eval() trickery to not formally depend on nanotime which we do not currently have as formal
## dependency, or suggestion, of the package, and not installed on the Docker CI images
##eval("if (requireNamespace(\"nanotime\", quietly=TRUE)) df[,\"nanotime\"] <- nanotime::nanotime(now) + 0:3")

fromDataFrame(df, uri)

arr <- tiledb_array(uri, as.data.frame=TRUE)
newdf <- arr[]

## result comes back as factor by default
newdf <- within(newdf, char <- as.character(char))

expect_equal(df, newdf[,-1])
#})
