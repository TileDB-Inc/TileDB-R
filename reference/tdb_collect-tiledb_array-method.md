# Collect the query results to finalize piped expression

Collect the query results to finalize piped expression

## Usage

``` r
# S4 method for class 'tiledb_array'
tdb_collect(x, ...)
```

## Arguments

- x:

  A tiledb_array object as first argument, permitting piping

- ...:

  Ignored

## Value

The object returning from a tiledb_array query (the type of which can be
set via the return preference mechanism, see the help for `"["`
accessor)
