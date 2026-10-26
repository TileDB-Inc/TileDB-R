# Set strings_as_factors return toggle

A `tiledb_array` object containing character column can have those
converted to factors variables. This methods sets the selection value
for ‘strings_as_factors’.

## Usage

``` r
strings_as_factors(x) <- value

# S4 method for class 'tiledb_array'
strings_as_factors(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A logical value with the selection

## Value

The modified `tiledb_array` array object
