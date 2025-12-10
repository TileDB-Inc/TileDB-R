# Create a TileDB dense or sparse array from a given `data.frame` Object

The supplied `data.frame` object is (currently) limited to integer,
numeric, or character. In addition, three datetime columns are supported
with the R representations of `Date`, `POSIXct` and `nanotime`.

## Usage

``` r
fromDataFrame(
  obj,
  uri,
  col_index = NULL,
  sparse = TRUE,
  allows_dups = sparse,
  cell_order = "COL_MAJOR",
  tile_order = "COL_MAJOR",
  filter = "ZSTD",
  capacity = 10000L,
  tile_domain = NULL,
  tile_extent = NULL,
  mode = c("ingest", "schema_only", "append"),
  filter_list = NULL,
  coords_filters = "ZSTD",
  offsets_filters = "ZSTD",
  validity_filters = "RLE",
  debug = FALSE,
  timestamps = as.POSIXct(double(), origin = "1970-01-01")
)
```

## Arguments

- obj:

  A `data.frame` object.

- uri:

  A character variable with an Array URI.

- col_index:

  An optional column index, either numeric with a column index, or
  character with a column name, designating an index column; default is
  NULL implying an index column is added when the array is created

- sparse:

  A logical switch to select sparse (the default) or dense

- allows_dups:

  A logical switch to select if duplicate values are allowed or not,
  default is the same value as ‘sparse’.

- cell_order:

  A character variable with one of the TileDB cell order values, default
  is “COL_MAJOR”.

- tile_order:

  A character variable with one of the TileDB tile order values, default
  is “COL_MAJOR”.

- filter:

  A character variable vector, defaults to ‘ZSTD’, for one or more
  filters to be applied to each attribute;

- capacity:

  A integer value with the schema capacity, default is 10000.

- tile_domain:

  An integer vector or list or `NULL`. If an integer vector of size two
  it specifies the integer domain of the row dimension; if a list then a
  named element is used for the dimension of the same name; or if `NULL`
  the row dimension of the `obj` is used.

- tile_extent:

  An integer value for the tile extent of the row dimensions; if `NULL`
  the row dimension of the `obj` is used. Note that the `tile_extent`
  cannot exceed the tile domain.

- mode:

  A character variable with possible values ‘ingest’ (for schema
  creation and data ingestion, the default behavior), ‘schema_only’ (to
  create the array schema without writing to the newly-created array)
  and ‘append’ (to only append to an already existing array).

- filter_list:

  A named list specifying filter choices per column, default is an empty
  `list` object. This argument applies for all named arguments and the
  matchin dimensions or attributes. The `filter` argument still applies
  for all unnamed arguments.

- coords_filters:

  A character vector with filters for coordinates, default is `ZSTD`.

- offsets_filters:

  A character vector with filters for coordinates, default is `ZSTD`.

- validity_filters:

  A character vector with filters for coordinates, default is `RLE`.

- debug:

  Logical flag to select additional output.

- timestamps:

  Vector with up to two `POSIXct` variables denoting open intervals;
  default is length zero where start and end are set (implicitly) to
  current time; in case of one value it is used as the interval end, and
  in case of two values they are taken as start and end. This applies to
  write and append modes only and not to schema creation.

## Value

Null, invisibly.

## Details

The created (dense or sparse) array will have as many attributes as
there are columns in the `data.frame`. Each attribute will be a single
column. For a sparse array, one or more columns have to be designated as
dimensions.

At present, factor variable are converted to character.

## Examples

``` r
uri <- tempfile()
fromDataFrame(iris, uri)
arr <- tiledb_array(uri, return_as="data.frame", extended=FALSE)
newdf <- arr[]
all.equal(iris, newdf, check.attributes=FALSE)  # extra attribute on query in newdf
#> [1] TRUE
all.equal(as.matrix(iris), as.matrix(newdf))  # also strips attribute
#> [1] TRUE
```
