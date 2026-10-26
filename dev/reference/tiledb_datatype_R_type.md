# Map from TileDB type to R datatype

This function maps from the TileDB types to the (fewer) key datatypes in
R. This can be lossy as TileDB integers range from (signed and unsigned)
8 to 64 bit whereas R only has (signed) 32 bit values. Similarly, R only
has 64 bit doubles whereas TileDB has 32 and 64 bit floating point
types. TileDB also has more character encodings, and the full range of
(NumPy) date and time types.

## Usage

``` r
tiledb_datatype_R_type(datatype)
```

## Arguments

- datatype:

  A string describing one TileDB datatype

## Value

A string describing the closest match for an R datatype
