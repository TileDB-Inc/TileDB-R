# Sets toggle whether the array schema allows duplicate values or not. This is only valid for sparse arrays.

Sets toggle whether the array schema allows duplicate values or not.
This is only valid for sparse arrays.

## Usage

``` r
allows_dups(x) <- value

# S4 method for class 'tiledb_array_schema'
allows_dups(x) <- value

tiledb_array_schema_set_allows_dups(x, value)
```

## Arguments

- x:

  A TileDB Schema object

- value:

  logical value

## Value

An object of class `tiledb_array_schema`.
