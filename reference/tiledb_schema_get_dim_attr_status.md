# Get Dimension or Attribute Status

Note that this function is an unexported internal function that can be
called using the colons as in
`tiledb:::tiledb_schema_get_dim_attr_status(sch)`.

## Usage

``` r
tiledb_schema_get_dim_attr_status(sch)
```

## Arguments

- sch:

  A TileDB Schema object

## Value

An integer vector where each element corresponds to a schema entry, and
a value of one signals dimension and a value of two an attribute.
