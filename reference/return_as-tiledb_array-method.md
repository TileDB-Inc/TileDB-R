# Retrieve return_as conversion preference

A `tiledb_array` object can be returned as a ‘list’ (default), ‘array’,
‘matrix’, ‘data.frame’, ‘data.table’ or ‘tibble’. This method permits to
select a preference for the returned object. The default value of ‘asis’
means that no conversion is performed.

## Usage

``` r
return_as(object, ...)

# S4 method for class 'tiledb_array'
return_as(object)
```

## Arguments

- object:

  A `tiledb_array` object

- ...:

  Currently unused

## Value

A character value indicating the preferred conversion where the value is
one of ‘asis’ (the default), ‘array’, ‘matrix’,‘data.frame’,
‘data.table’, or ‘tibble’.
