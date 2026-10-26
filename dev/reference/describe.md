# Describe a TileDB array schema via code to create it

Note that this function is an unexported internal function that can be
called using the colons as in `tiledb:::describe(arr)`.

## Usage

``` r
describe(arr)
```

## Arguments

- arr:

  A TileDB Array object

## Value

Nothing is returned as the function is invoked for the side effect of
printing the schema via a sequence of R instructions to re-create it.
