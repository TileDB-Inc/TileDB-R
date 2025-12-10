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

  Extra parameter for method signature, currently unused.

- value:

  The value being assigned

## Value

The modified object

## Details

For sparse matrices, row and column indices can either be supplied as
part of the left-hand side object, or as part of the data.frame provided
appropriate column names.

This function may still still change; the current implementation should
be considered as an initial draft.

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
