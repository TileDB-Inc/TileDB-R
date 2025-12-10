# Returns a TileDB array, allowing for specific subset ranges.

Heterogenous domains are supported, including timestamps and characters.

## Usage

``` r
# S4 method for class 'tiledb_array,ANY'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  tiledb_array object

- i:

  optional row index expression which can be a list in which case
  minimum and maximum of each list element determine a range; multiple
  list elements can be used to supply multiple ranges.

- j:

  optional column index expression which can be a list in which case
  minimum and maximum of each list element determine a range; multiple
  list elements can be used to supply multiple ranges.

- ...:

  Extra parameters for method signature, currently unused.

- drop:

  Optional logical switch to drop dimensions, default FALSE, currently
  unused.

## Value

The resulting elements in the selected format

## Details

This function may still still change; the current implementation should
be considered as an initial draft.
