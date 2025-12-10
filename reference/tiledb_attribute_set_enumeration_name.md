# Set a TileDB Attribute Enumeration Name

Set a TileDB Attribute Enumeration Name

## Usage

``` r
tiledb_attribute_set_enumeration_name(
  attr,
  enum_name,
  ctx = tiledb_get_context()
)
```

## Arguments

- attr:

  A TileDB Attribute object

- enum_name:

  A character value with the enumeration value

- ctx:

  A Tiledb Context object (optional)

## Value

The modified TileDB Attribute object
