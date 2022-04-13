<!--
%\VignetteIndexEntry{Introduction to TileDB}
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteEncoding{UTF-8}
-->
---
title: "First Steps with TileDB"
css: "water.css"
---

This document introduces TileDB via several simple examples. A corresponding
document with more complete [API documentation](documentation.html) is also available.

# Getting started

Once the TileDB R package is installed, it can be loaded via `library(tiledb)`. Installation is
supported for Windows, Linux and macOS via the [official CRAN package][cran_tiledb], on Linux and
macOS via the [conda](https://anaconda.org/conda-forge) package as well as from source.

Documentation for the TileDB R package is available via the `help()` function from within R as well
as via the [package documentation][tiledb-r_docs] and an [introductory notebook][tiledb_notebook].
Documentation about TileDB itself is [also available](https://docs.tiledb.com/main/).

Several "quickstart" examples that are discussed on the website are available in the
[examples][tiledb_examples] directory. This vignette discusses similar examples.

In the following examples, the URIs describing arrays point to local file system object. When TileDB
has been built with S3 support, and with proper AWS credentials in the usual environment variables,
URIs such as `s3://some/data/bucket` can be used where a local file would be used. See the script
[ex_S3.R][ex_s3] for an example.

# Dense Arrays

## Preliminaries

These illustrations use the array created by the file [ex_1.R][ex_1] which one can run from within
R, or on the command-line. To follow along with discussion that follows, it helps to run the example
once to create the array after possibly adjusting the array location path from its default value
(using the current directory or, if set as an option, an override).

## Basic Reading of Dense Arrays

The file [ex_1.R][ex_1] in the examples directory is a simple yet complete example extending
[quickstart_dense.R][qs_dense] by adding a second and third attribute. In this as well as the
following examples we will use `tiledb_array()` to access the array; the older variants
`tiledb_dense()` and `tiledb_sparse()` remain supported but are deprecated and may be removed at
some point in the future.

*Read 1-D*

The first example extracts rows 1 to 2 and column 2 from an array. It also limits the selection to
just one attribute (via `attrs`), asks for the return to be a `data.frame` (instead of a simpler list)
and for the (row and column, if present as here) indices to not be printed (via `extended=FALSE`).

```r
> A <- tiledb_array(uri = uri, attrs = "b",
+                   as.data.frame=TRUE, extended=FALSE))
> A[1:2,2]
[1] 101.5 104.0
>
```

Note that the examples create three two-dimensional attributes. The attributes can be selected via
the `attrs` argument, or the `attrs()` method on the array object.  The square-bracket indexing then
selects with in the 2-d attribute object.

If multiple objects are returned (as `list` or `data.frame`), subsetting on the
returned object works via `[[var]]` or `$var`.  A numeric index also works (but needs to account for
`rows` and `cols`).

```r
> A <- tiledb_array(uri = uri, attrs = c("a","b"),
+                   as.data.frame = TRUE)
> A[1:2,2][["a"]]
[1] 2 7
> A[1:2,2]$a
[1] 2 7
>
```

*Read 2-D*

This works analogously.  Note that the results are generally returned as vectors, or as a columns of
a `data.frame` object in case that option was set.

```r
> A[6:9,3:4]
$a
[1] 28 29 33 34 38 39 43 44

$b
[1] 114.5 115.0 117.0 117.5 119.5 120.0 122.0 122.5

$c
[1] "fox" "A"   "E"   "F"   "J"   "K"   "O"   "P"
>
```

## Read 2-D with attribute selection

We can restrict the selection to a subset of attributes when opening the array.

```r
> A <- tiledb_dense(uri = uri, attrs = c("b","c"),
+                   as.data.frame = TRUE, extended=FALSE)
> A[6:9,2:4]
       b     c
1  114.0 brown
2  114.5   fox
3  115.0     A
4  116.5     D
5  117.0     E
6  117.5     F
7  119.0     I
8  119.5     J
9  120.0     K
10 121.5     N
11 122.0     O
12 122.5     P
>
```

This also illustrated the effect of setting `as.data.frame=TRUE` when opening the array.

This scheme can be generalized to variable cells, or cells where N>1, as we can expand each
(atomistic) value over corresponding row and column indices.

The column types correspond to the attribute typed in the array schema, subject to the constraint
mentioned above on R types.  (The char comes in as a factor variable as is still the R 3.6.* default
which is about to change. We can also override, users can too.)

```r
> A <- tiledb_array("/tmp/tiledb/ex_1/", attrs=c("b","c"),
+                   as.data.frame = TRUE, extended=TRUE)
> sapply(A[6:9, 3:4], "class")
       rows        cols           b           c
  "integer"   "integer"   "numeric" "character"
>
```

Consistent with the `data.frame` semantics, *now* requesting a named column *reduces to a vector* as
this happens at the R side:

```
> A[6:9, 3:4]$b
[1] 114.5 115.0 117.0 117.5 119.5 120.0 122.0 122.5
>
```


# Sparse Arrays

## Basic Reading and Writing of Sparse Arrays

*Simple Examples*

Basic reading returns the coordinates and any attributes.  The following examples use the array
created by the [quickstart_sparse][qs_sparse] example.

```r
> A <- tiledb_array(uri = uri, is.sparse = TRUE)
> A[]
$rows
[1] 1 2 2

$cols
[1] 1 3 4

$a
[1] 1 3 2

>
```
We can also request a `data.frame` object, either when opening or by changing this object characteristic on the fly:

```r
> return.data.frame(A) <- TRUE
> A[]
  a rows cols
1 1    1    1
2 3    2    3
3 2    2    4
```

For sparse arrays, the return type is by default ‘extended’ showing rows and column but this can be overridden.

Assignment works similarly:

```r
> A[4,2] <- 42L
> A[]
> A[]
  rows cols  a
1    1    1  1
2    2    3  3
3    2    4  2
4    4    2 42
>
```

Reads can select rows and or columns:

```r
> A[2,]
  rows cols a
1    2    3 3
2    2    4 2
> A[,2]
  rows cols  a
1    4    2 42
>
```

Attributes can be selected similarly.

## Date(time) Attributes

Similar to the dense array case described earlier, the file [ex_2.R][ex_2]
illustrates some basic operations on sparse arrays. It also shows date and datetime types instead of
just integer and double precision floats.

```r
> A <- tiledb_array(uri = uri, as.data.frame = TRUE)
> A[1577858403:1577858408]
        rows cols a   b          d                       e
1 1577858403    1 3 103 2020-01-11 2020-01-02 18:24:33.844
2 1577858404    1 4 104 2020-01-15 2020-01-05 02:28:36.214
3 1577858405    1 5 105 2020-01-19 2020-01-05 00:44:04.805
4 1577858406    1 6 106 2020-01-21 2020-01-06 12:58:51.770
5 1577858407    1 7 107 2020-01-25 2020-01-09 04:29:56.309
6 1577858408    1 8 108 2020-01-26 2020-01-07 13:55:10.240
>
```

The row coordinate is currently a floating point representation of the underlying time type.  We can
both select attributes (here we excluded the “a” column) and select rows by time (as the time stamps
get converted to the required floating point value).

```r
> attrs(A) <- c("b", "d", "e")
> A[as.POSIXct("2020-01-01"):as.POSIXct("2020-01-01 00:00:03")]
        rows cols   b          d                       e
1 1577858401    1 101 2020-01-05 2020-01-01 03:03:07.548
2 1577858402    1 102 2020-01-10 2020-01-02 21:02:19.747
3 1577858403    1 103 2020-01-11 2020-01-02 18:24:33.844
>
```

More extended examples are available showing indexing by date(time) as well as character dimension.

# Additional Information

The TileDB R package is documented via R help functions (_e.g._ `help("tiledb_array")` shows
information for the `tiledb_array()` function) as well as via a [website regrouping all
documentation][tiledb-r_docs].  Extended [API documentation][api_documentation] is available, as is a
[examples/][tiledb_examples] directory.

TileDB itself has extensive [installation](https://docs.tiledb.com/developer/installation),
and [overall documentation](https://docs.tiledb.com/developer/) as well as a [support forum](https://forum.tiledb.com/).


[cran_tiledb]: https://cran.r-project.org/package=tiledb
[tiledb-r_docs]: https://tiledb-inc.github.io/TileDB-R/
[api_documentation]: https://tiledb-inc.github.io/TileDB-R/documentation.html
[tiledb_examples]: https://github.com/TileDB-Inc/TileDB-R/tree/master/inst/examples
[tiledb_notebook]: https://github.com/TileDB-Inc/TileDB-R/tree/master/inst/notebooks

[ex_1]: https://github.com/TileDB-Inc/TileDB-R/blob/master/inst/examples/ex_1.R
[ex_2]: https://github.com/TileDB-Inc/TileDB-R/blob/master/inst/examples/ex_2.R
[ex_s3]: https://github.com/TileDB-Inc/TileDB-R/blob/master/inst/examples/ex_S3.R

[qs_dense]: https://github.com/TileDB-Inc/TileDB-R/blob/master/inst/examples/quickstart_dense.R
[qs_sparse]: https://github.com/TileDB-Inc/TileDB/blob/dev/examples/cpp_api/quickstart_sparse.cc
