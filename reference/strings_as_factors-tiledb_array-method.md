# Retrieve strings_as_factors conversion toggle

A `tiledb_array` object containing character column can have those
converted to factors variables. This methods returns the selection value
for ‘strings_as_factors’.

## Usage

``` r
strings_as_factors(object)

# S4 method for class 'tiledb_array'
strings_as_factors(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

A logical value indicating whether an `strings_as_factors` return is
selected
