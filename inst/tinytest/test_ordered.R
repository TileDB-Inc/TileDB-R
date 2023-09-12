
library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.17.0") exit_file("Remainder needs 2.17.* or later")

## A data.frame with an ordered column, taken from package `earth` and its `etitanic` cleaned
## dataset of Titanic survivors (with NAs removed).
##
## et <- earth::etitanic
## et$pclass <- as.ordered(et$pclass)
## set.seed(42)
## et <- et[sort(sample(nrow(et), 100)), ]
## dput(et)
##
## Slightly edited (for code alignment) `dput(et)` output below
et <- structure(list(pclass = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                          1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
                                          3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                          3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                          3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                                        levels = c("1st", "2nd", "3rd"), class = c("ordered", "factor")),
                     survived = c(0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L,
                                  1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L,
                                  0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L,
                                  1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                  0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                  0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                  0L, 0L, 0L),
                     sex = structure(c(1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L,
                                       2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L,
                                       2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
                                       2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L,
                                       2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L,
                                       2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L,
                                       2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                                     levels = c("female", "male"), class = "factor"),
                     age = c(2, 24, 29, 58, 59, 28, 36,
                             27, 39, 27, 48, 24, 19, 22, 48, 35, 38, 16, 65, 28.5, 35, 34,
                             32, 43, 49, 31, 30, 18, 28, 32, 19, 40, 0.833299994, 19, 37,
                             32, 34, 54, 8, 27, 34, 16, 21, 62, 21, 23, 36, 29, 41, 33, 25,
                             25, 18.5, 13, 20, 6, 32, 21, 18, 26, 32, 29, 18.5, 21, 17, 37,
                             35, 30, 22, 47, 26, 21, 28, 25, 28, 43, 22, 30, 20.5, 51, 35,
                             28, 19, 28, 29, 41, 19, 28, 8, 39, 2, 45, 30, 33, 21, 24, 11.5,
                             18, 36, 45.5),
                     sibsp = c(1L, 0L, 0L, 0L, 2L, 0L, 1L, 1L, 1L,
                               1L, 1L, 3L, 3L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L,
                               0L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 1L,
                               0L, 1L, 0L, 2L, 2L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 4L, 1L,
                               0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 2L, 2L,
                               0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 4L,
                               0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L),
                     parch = c(2L, 1L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 2L, 0L, 2L, 2L, 2L, 0L, 0L, 0L, 1L,
                               0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 2L, 0L,
                               0L, 0L, 1L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 2L,
                               0L, 0L, 0L, 2L, 0L, 2L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L,
                               0L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                               1L, 0L, 4L, 5L, 0L, 0L, 1L, 5L, 1L, 4L, 0L, 0L, 0L, 0L, 1L, 0L,
                               0L, 0L)),
                row.names = c("3", "17", "25", "34", "43", "53", "58",
                              "65", "85", "91", "100", "112", "115", "123", "146", "165", "169",
                              "188", "206", "223", "258", "260", "279", "282", "295", "299",
                              "324", "327", "335", "337", "338", "353", "360", "365", "369",
                              "390", "397", "398", "399", "402", "415", "417", "420", "433",
                              "445", "448", "449", "453", "533", "543", "556", "568", "569",
                              "602", "616", "624", "656", "676", "677", "678", "685", "689",
                              "693", "697", "701", "711", "730", "761", "786", "794", "804",
                              "807", "839", "854", "864", "869", "953", "975", "978", "980",
                              "996", "1022", "1051", "1084", "1101", "1107", "1109", "1127",
                              "1146", "1147", "1157", "1212", "1219", "1223", "1225", "1238",
                              "1264", "1289", "1299", "1302"),
                class = "data.frame")

uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(et, uri)

dfval <- tiledb_array(uri, return_as="data.frame", extended=FALSE)[]
expect_true(is.ordered(dfval$pclass))
expect_false(is.ordered(dfval$sex))
expect_true(is.factor(dfval$sex))
expect_equivalent(et, dfval)

dtval <- tiledb_array(uri, return_as="data.table", extended=FALSE)[]
expect_true(is.ordered(dtval$pclass))
expect_false(is.ordered(dtval$sex))
expect_true(is.factor(dtval$sex))
expect_equivalent(et, dtval)

tbval <- tiledb_array(uri, return_as="tibble", extended=FALSE)[]
expect_true(is.ordered(tbval$pclass))
expect_false(is.ordered(tbval$sex))
expect_true(is.factor(tbval$sex))
expect_equivalent(et, tbval)

if (!requireNamespace("arrow", quietly=TRUE)) exit_file("No 'arrow' package.")
arval <- tiledb_array(uri, return_as="arrow", extended=FALSE)[]
tbval <- tibble::as_tibble(arval)
expect_true(is.ordered(tbval$pclass))
expect_false(is.ordered(tbval$sex))
expect_true(is.factor(tbval$sex))
expect_equivalent(et, tbval)
