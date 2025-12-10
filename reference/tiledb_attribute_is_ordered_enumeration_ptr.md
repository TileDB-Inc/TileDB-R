# Check if TileDB Attribute Enumeration is Ordered

Check if TileDB Attribute Enumeration is Ordered

## Usage

``` r
tiledb_attribute_is_ordered_enumeration_ptr(
  attr,
  arrptr,
  ctx = tiledb_get_context()
)
```

## Arguments

- attr:

  A Tiledb Array object

- arrptr:

  A Tiledb Array object pointer

- ctx:

  A Tiledb Context object (optional)

## Value

A character vector with the enumeration (of length zero if none)
