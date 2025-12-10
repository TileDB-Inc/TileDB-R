# Return a fragment info non-empty domain from name

TODO: Rework with type information

## Usage

``` r
tiledb_fragment_info_get_non_empty_domain_name(object, fid, dim_name, typestr)
```

## Arguments

- object:

  A TileDB fragment info object

- fid:

  A fragment object index

- dim_name:

  A character variable with the dimension name

- typestr:

  An optional character variable describing the data type which will be
  accessed from the schema if missing

## Value

A TileDB Domain object
