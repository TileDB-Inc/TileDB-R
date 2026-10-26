# (Deprecated) Allocate (or Release) Arrow Array and Schema Pointers

These functions allocate (and free) appropriate pointer objects for,
respectively, Arrow array and schema objects. These functions are
deprecated and will be removed, it is recommended to rely directly on
the `nanoarrow` replacements.

## Usage

``` r
tiledb_arrow_array_ptr()

tiledb_arrow_schema_ptr()

tiledb_arrow_array_del(ptr)

tiledb_arrow_schema_del(ptr)
```

## Arguments

- ptr:

  A external pointer object previously allocated with these functions

## Value

The allocating functions return the requested pointer
