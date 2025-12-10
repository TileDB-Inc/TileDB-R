# Return a fragment info non-empty domain from index

TODO: Rework with type information

## Usage

``` r
tiledb_fragment_info_get_non_empty_domain_index(object, fid, did, typestr)
```

## Arguments

- object:

  A TileDB fragment info object

- fid:

  A fragment object index

- did:

  A domain index

- typestr:

  An optional character variable describing the data type which will be
  accessed from the schema if missing

## Value

A TileDB Domain object
