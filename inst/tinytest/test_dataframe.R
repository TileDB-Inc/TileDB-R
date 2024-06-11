library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_fromdataframe", {
uri <- tempfile()
## turn factor into character
irisdf <- within(iris, Species <- as.character(Species))

expect_error(fromDataFrame(uri, irisdf)) # arguments checked, error in this wrong case

fromDataFrame(irisdf, uri)

arr <- tiledb_array(uri, return_as="data.frame")
newdf <- arr[]
#if (getRversion() >= '4.0.0') newdf$Species <- as.factor(newdf$Species)
if (getRversion() <  '4.0.0') newdf$Species <- as.character(newdf$Species)
expect_equal(irisdf, newdf[,-1])
expect_equal(dim(irisdf), dim(newdf[,-1]))

## result comes back as factor by default
newdf <- within(newdf, Species <- as.character(Species))
expect_equal(irisdf, newdf[,-1])


## test attrs subselection
arr <- tiledb_array(uri, return_as="data.frame", extended=FALSE,
                    attrs = c("Petal.Length", "Petal.Width"))
newdf <- arr[]
expect_equivalent(iris[, c("Petal.Length", "Petal.Width")], newdf) # skip attribute


## test list
arr <- tiledb_array(uri, return_as="asis")
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

arr <- tiledb_array(uri, return_as="data.frame")
newdf <- arr[]

## result comes back as factor by default
newdf <- within(newdf, char <- as.character(char))

expect_equal(df, newdf[,-1])
#})

## n=8
## test dense with non-default index columm
uri <- tempfile()
set.seed(42)
rows <- 50L

df <- data.frame(index=sort(sample(1:1000, rows)),
                 chars=sample(LETTERS, rows, replace=TRUE),
                 vals=rnorm(rows) * 100,
                 stringsAsFactors=FALSE)
fromDataFrame(df, uri)
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() < '4.0.0') chk$chars <- as.character(chk$chars)
expect_equal(df, chk[,-1])              # omit first col which is added

if (tiledb_version(TRUE) < "2.1.0") exit_file("Remaining tests require TileDB 2.1.0 or later")

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(df, uri, col_index=1)
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() < '4.0.0') chk$chars <- as.character(chk$chars)
expect_equal(df[,2], na.omit(chk)[,2])  # compare column by column
expect_equal(df[,3], na.omit(chk)[,3])


if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(df, uri, col_index="index")
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() < '4.0.0') chk$chars <- as.character(chk$chars)
expect_equal(df[,2], na.omit(chk)[,2])  # compare column by column
expect_equal(df[,3], na.omit(chk)[,3])

## n=13
olddf <- df
df <- data.frame(chars=olddf$chars,
                 index=olddf$index,     # index not in first column
                 val=olddf$vals)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(df, uri)
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() < '4.0.0') {
    df$chars <- as.character(df$chars)
    chk$chars <- as.character(chk$chars)
}
expect_equal(df, chk[,-1])              # omit first col which is added

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(df, uri, col_index=2)
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() < '4.0.0') chk$chars <- as.character(chk$chars)
expect_equal(df[,1], na.omit(chk)[,2])  # compare column by column
expect_equal(df[,3], na.omit(chk)[,3])

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(df, uri, col_index="index")
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() < '4.0.0') chk$chars <- as.character(chk$chars)
expect_equal(df[,1], na.omit(chk)[,2])  # compare column by column
expect_equal(df[,3], na.omit(chk)[,3])

## n=18
## test sparse with non-default index columm
uri <- tempfile()
set.seed(42)
nobs <- 50L

## datetimes test (cf ex_aggdatetimes)
suppressMessages({
  library(nanotime)
  library(bit64)
})

prevTZ <- Sys.getenv("TZ")
on.exit(Sys.setenv(TZ=prevTZ))
Sys.setenv(TZ="")
df <- data.frame(time=round(Sys.time(), "secs") + trunc(cumsum(runif(nobs)*3600)),
                 double_range=seq(-1000, 1000, length=nobs),
                 int_vals=sort(as.integer(runif(nobs)*1e9)),
                 int64_vals=sort(as.integer64(runif(nobs)*1e9)),
                 nanotime=as.nanotime(Sys.time() + cumsum(runif(nobs)*3600)),
                 stringsAsFactors=FALSE)

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(df, uri, sparse=TRUE)

chk <- tiledb_array(uri, return_as="data.frame", extended=FALSE)
expect_equivalent(df, chk[])            # skip attribute

for (i in seq_len(dim(df)[2])) {
    if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    fromDataFrame(df, uri, sparse=TRUE, col_index=i)
    chk <- tiledb_array(uri, return_as="data.frame")
    expect_equal(df, chk[][,colnames(df)]) 		# index col comes first so need re-order
}

## n=24
## test sparse with several non-default index columms
uri <- tempfile()
set.seed(42)
nobs <- 50L
df <- data.frame(time = round(Sys.time(), "secs") + trunc(cumsum(runif(nobs)*3600)),
                 double_range = seq(-1000, 1000, length=nobs),
                 int_vals = sort(as.integer(runif(nobs)*1e9)),
                 int64_vals = sort(as.integer64(runif(nobs)*1e9)),
                 nanotime = as.nanotime(Sys.time() + cumsum(runif(nobs)*3600)),
                 txt = sort(sapply(seq_len(nobs), function(i) paste0(sample(LETTERS,1), paste(sample(letters, sample(1:6,1)), collapse="")))),
                 stringsAsFactors=FALSE)


if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(df, uri, sparse=TRUE)

chk <- tiledb_array(uri, return_as="data.frame", extended=FALSE)
newdf <- chk[]
if (getRversion() < '4.0.0') newdf$txt <- as.character(newdf$txt)
expect_equivalent(df, newdf)            # skip attribute


for (i in seq_len(dim(df)[2])) {
    if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    fromDataFrame(df, uri, sparse=TRUE, col_index=i)
    chk <- tiledb_array(uri, return_as="data.frame")
    newdf <- chk[]
    if (getRversion() < '4.0.0') newdf$txt <- as.character(newdf$txt)
    expect_equal(df, newdf[, colnames(df)])
}

combinations <- list(c(1,2), c(1,3), c(2,4), c(3,5), c(4,5), c(2,3,4))
for (comb in combinations) {
    if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    fromDataFrame(df, uri, sparse=TRUE, col_index=comb) # by index
    chk <- tiledb_array(uri, return_as="data.frame")
    newdf <- chk[]
    if (getRversion() < '4.0.0') newdf$txt <- as.character(newdf$txt)
    expect_equal(df, newdf[][, colnames(df)])

    if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    fromDataFrame(df, uri, sparse=TRUE, col_index=colnames(df)[comb]) # by name
    chk <- tiledb_array(uri, return_as="data.frame")
    newdf <- chk[]
    if (getRversion() < '4.0.0') newdf$txt <- as.character(newdf$txt)
    expect_equal(df, newdf[, colnames(df)])
}

## n=44
## simple nullable example, no CHAR support yet C++
uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
dat <- data.frame(A=1:10,
                  B=LETTERS[1:10],
                  C=sqrt(1:10))
dat[3,1] <- NA
dat[4,3] <- NA
fromDataFrame(dat, uri)
chk <- tiledb_array(uri, return_as="data.frame")
val <- chk[][,-1]  # omit added rows
if (getRversion() < '4.0.0') {
    dat$B <- as.character(dat$B)
    val$B <- as.character(val$B)
}
expect_equal(dat, val)

## array with char only columns in dimension and attribute, used to error before #217
if (tiledb_version(TRUE) < "2.2.0") exit_file("Remaining tests require TileDB 2.2.0 or later")
N <- 20
D <- data.frame(sample=paste(LETTERS[1:N], as.character(sort(trunc(runif(N, 100, 200)))), sep=""),
                header=paste(LETTERS[1:N], as.character(sort(trunc(runif(N, 10000, 20000)))), sep=""),
                stringsAsFactors=FALSE)
uri <- tempfile()
fromDataFrame(D, uri, col_index=1, sparse=TRUE)
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() < '4.0.0') {
    chk$sample <- as.character(chk$sample)
    chk$header <- as.character(chk$header)
}
expect_equivalent(D, chk)               # skip attribute

## n=45
## sparse array can have duplicate values in index column
df <- data.frame(
  index = c(1, 1, 3),
  char = c("a", "a", "c"),
  stringsAsFactors = FALSE
)

uri <- tempfile()
expect_error(fromDataFrame(df, uri, col_index=1, sparse=TRUE, allows_dups=FALSE))

uri <- tempfile()
expect_silent(arr <- fromDataFrame(df, uri, col_index=1, sparse=TRUE, allows_dups=TRUE))

arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
if (getRversion() <  '4.0.0') chk$char <- as.character(chk$char)
expect_equivalent(df, chk)              # skip attribute

## explicit dense data.frame
df <- data.frame(aa=1:26,
                 bb=26:1,
                 cc=letters,
                 dd=LETTERS)
uri <- tempfile()
fromDataFrame(df, uri, sparse=FALSE)
arr <- tiledb_array(uri, return_as="data.frame", extended=FALSE)  # skip row index on return
res <- arr[]
if (getRversion() < '4.0.0') {
    res$cc <- as.character(res$cc)
    res$dd <- as.character(res$dd)
}
expect_equivalent(df, res)

## n=49
## test ingest vs schema_only vs append
if (tiledb_version(TRUE) < "2.4.0") exit_file("Neeeds TileDB 2.4.* or later")
if (!requireNamespace("palmerpenguins", quietly=TRUE)) exit_file("remainder needs 'palmerpenguins'")
library(palmerpenguins)
data <- penguins

uri <- tempfile()

fromDataFrame(data, uri, col_index=1:2, mode="schema_only")
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
expect_equal(nrow(chk), 0)              # no data
expect_equal(ncol(chk), ncol(data))     # but all columns

fromDataFrame(data, uri, col_index=1:2, mode="append")
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
expect_equal(nrow(chk), nrow(data))     # all data
expect_equal(ncol(chk), ncol(data))     # all columns

tiledb_vfs_remove_dir(uri)
fromDataFrame(data, uri, col_index=1:2) # default mode
arr <- tiledb_array(uri, return_as="data.frame")
chk <- arr[]
expect_equal(nrow(chk), nrow(data))     # all data
expect_equal(ncol(chk), ncol(data))     # all columns

## n=55
## attribute-less arrays
uri <- tempfile()
D <- data.frame(dim = c(2L, 4L, 6L))
dim <- tiledb_dim(name = "dim", domain = c(0L, 9L), type = "INT32")
sch <- tiledb_array_schema(domain = tiledb_domain(dim), sparse = TRUE)
tiledb_array_create(uri, sch)
arr <- tiledb_array(uri)
arr[] <- D
arr2 <- tiledb_array(uri, return_as="data.frame")
res2 <- arr2[]
attr(res2, "query_status") <- NULL
expect_equal(D, res2)

uri <- tempfile()
fromDataFrame(D, uri, col_index=1)
arr2 <- tiledb_array(uri, return_as="data.frame")
res2 <- arr2[]
attr(res2, "query_status") <- NULL
expect_equal(D, res2)


## list columns
D <- data.frame(a=1:5,
                b=I(split(c(1:4,NA,NA,7:10), ceiling((1:10)/2))),
                c=I(split(c(101:109, NA, NA, NA, 113:115), ceiling((1:15)/3))))
uri <- tempfile()
fromDataFrame(D, uri, col_index=1)
arr <- tiledb_array(uri, return_as="data.frame")
res <- arr[]
expect_equivalent(res, D)


## fromDataFrame with timestamps
if (tiledb_version(TRUE) < "2.15.0") exit_file("Remaining tests require TileDB 2.15.0 or later")
D <- data.frame(key=(1:10)*10, value=letters[1:10])
uri <- tempfile()
now <- Sys.time()
fromDataFrame(D, uri)         # no timestamps
expect_equal(nrow(tiledb_array(uri, return_as="data.frame")[]), 10)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_end=as.POSIXct(100, origin="1970-01-01"))[]),  0)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_start=now + 1)[]), 0)
unlink(uri, recursive=TRUE)

fromDataFrame(D, uri, timestamps=as.POSIXct(100, origin="1970-01-01"))         # end timestamps
expect_equal(nrow(tiledb_array(uri, return_as="data.frame")[]), 10)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_end=as.POSIXct(50, origin="1970-01-01"))[]), 0)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_start=as.POSIXct(50, origin="1970-01-01"))[]), 10)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_start=as.POSIXct(150, origin="1970-01-01"))[]), 0)
unlink(uri, recursive=TRUE)

fromDataFrame(D, uri, timestamps=c(as.POSIXct(100, origin="1970-01-01"), as.POSIXct(100, origin="1970-01-01"))) # start and end
expect_equal(nrow(tiledb_array(uri, return_as="data.frame")[]), 10)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_end=as.POSIXct(50, origin="1970-01-01"))[]), 0)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_start=as.POSIXct(50, origin="1970-01-01"))[]), 10)
expect_equal(nrow(tiledb_array(uri, return_as="data.frame", timestamp_start=as.POSIXct(150, origin="1970-01-01"))[]), 0)
