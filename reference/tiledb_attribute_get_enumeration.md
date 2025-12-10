# Get the TileDB Attribute Enumeration

Get the TileDB Attribute Enumeration

## Usage

``` r
tiledb_attribute_get_enumeration(attr, arr, ctx = tiledb_get_context())

tiledb_attribute_get_enumeration_ptr(attr, arrptr, ctx = tiledb_get_context())
```

## Arguments

- attr:

  A TileDB Attribute object

- arr:

  A Tiledb Array object

- ctx:

  A Tiledb Context object (optional)

- arrptr:

  A Tiledb Array object pointer

## Value

A character vector with the enumeration (of length zero if none)
