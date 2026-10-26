# Generic Methods

Definition of generic methods

## Usage

``` r
schema(object, ...)

return.data.frame(object, ...)

return.data.frame(x) <- value

attrs(x) <- value

raw_dump(object, ...)

domain(object, ...)

dimensions(object, ...)

attrs(object, idx, ...)

cell_order(object, ...)

tile_order(object, ...)

filter_list(object, ...)

filter_list(x) <- value

is.sparse(object, ...)

tiledb_ndim(object, ...)

name(object)

datatype(object)

config(object, ...)

tile(object)

is.integral(object)

nfilters(object)

tdb_filter(x, ...)

tdb_select(x, ...)

tdb_collect(x, ...)
```

## Arguments

- object:

  A TileDB object

- ...:

  Currently unused

- x:

  A TileDB Object

- value:

  A value to be assigned

- idx:

  An index argument
