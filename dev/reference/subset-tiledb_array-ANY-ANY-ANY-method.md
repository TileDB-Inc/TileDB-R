# Sets a tiledb array value or value range

This function assigns a right-hand side object, typically a data.frame
or something that can be coerced to a data.frame, to a tiledb array.

## Usage

``` r
# S4 method for class 'tiledb_array,ANY,ANY,ANY'
x[i, j, ...] <- value
```

## Arguments

- x:

  sparse or dense TileDB array object

- i:

  parameter row index

- j:

  parameter column index

- ...:

  For dense arrays, an optional third index `k`; further indices are not
  supported.

- value:

  The value being assigned

## Value

The modified object

## Details

For sparse matrices, row and column indices can either be supplied as
part of the left-hand side object, or as part of the data.frame provided
appropriate column names.

For dense arrays, the region written to is given by the indices `i`, `j`
and (for arrays with three or more dimensions) `k`, or by the
[`selected_ranges`](https://tiledb-inc.github.io/TileDB-R/dev/reference/selected_ranges-tiledb_array-method.md)
of the array object. Each index must be a contiguous increasing
sequence, and each selected range a single range. Dimensions without an
index or selected range span their full domain. Arrays with more than
three dimensions can be written to via
[`selected_ranges`](https://tiledb-inc.github.io/TileDB-R/dev/reference/selected_ranges-tiledb_array-method.md).
Note that selected ranges set on an array object, e.g. for an earlier
read, also apply to writes through it.

For dense arrays, an error is raised for an index that is not a
contiguous increasing sequence, an index for a dimension the array does
not have, more than three indices, more than one selected range for a
dimension, or any
[`selected_points`](https://tiledb-inc.github.io/TileDB-R/dev/reference/selected_points-tiledb_array-method.md).
For all arrays, an error is raised if the assigned value does not match
the attributes (and, where given, dimensions) of the array.

This function may still change; the current implementation should be
considered as an initial draft.

## Examples

``` r
if (FALSE) { # \dontrun{
uri <- "quickstart_sparse" ## as created by the other example
arr <- tiledb_array(uri) ## open array
df <- arr[] ## read current content
## First approach: matching data.frame with appriate row and column
newdf <- data.frame(rows = c(1, 2, 2), cols = c(1, 3, 4), a = df$a + 100)
## Second approach: supply indices explicitly
arr[c(1, 2), c(1, 3)] <- c(42, 43) ## two values
arr[2, 4] <- 88 ## or just one
} # }
```
