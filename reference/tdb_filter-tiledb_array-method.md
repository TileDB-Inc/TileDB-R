# Filter from array for query via logical conditions

Filter from array for query via logical conditions

## Usage

``` r
# S4 method for class 'tiledb_array'
tdb_filter(x, ..., strict = TRUE)
```

## Arguments

- x:

  A tiledb_array object as first argument, permitting piping

- ...:

  One or more expressions that are parsed as query_condition objects

- strict:

  A boolean toogle to, if set, errors if a non-existing attribute is
  selected or filtered on, defaults to 'TRUE'; if 'FALSE' a warning is
  shown by execution proceeds.

## Value

The tiledb_array object, permitting piping
