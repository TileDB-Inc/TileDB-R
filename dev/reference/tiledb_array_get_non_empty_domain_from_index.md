# Get the non-empty domain from a TileDB Array by index

This functions works for both fixed- and variable-sized dimensions and
switches internally.

## Usage

``` r
tiledb_array_get_non_empty_domain_from_index(arr, idx)
```

## Arguments

- arr:

  A TileDB Array

- idx:

  An integer index between one and the number of dimensions

## Value

A two-element object is returned describing the domain of selected
dimension; it will either be a numeric vector in case of a fixed-sized
dimension or a character vector for a variable-sized one.
