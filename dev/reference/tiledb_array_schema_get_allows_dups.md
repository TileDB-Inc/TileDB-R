# Returns logical value whether the array schema allows duplicate values or not. This is only valid for sparse arrays.

Returns logical value whether the array schema allows duplicate values
or not. This is only valid for sparse arrays.

## Usage

``` r
allows_dups(x)

# S4 method for class 'tiledb_array_schema'
allows_dups(x)

tiledb_array_schema_get_allows_dups(x)
```

## Arguments

- x:

  A TileDB Schema object

## Value

A logical value.
