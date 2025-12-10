# An S4 class for a TileDB Array

This class replaces the earlier (and now removed) `tiledb_dense` and
`tiledb_sparse` and provides equivalent functionality based on a
refactored implementation utilising newer TileDB features.

## Slots

- `ctx`:

  A TileDB context object

- `uri`:

  A character despription with the array URI

- `is.sparse`:

  A logical value whether the array is sparse or not

- `attrs`:

  A character vector to select particular column ‘attributes’; default
  is an empty character vector implying ‘all’ columns, the special value
  `NA_character_` has the opposite effect and selects ‘none’.

- `extended`:

  A logical value, defaults to `TRUE`, indicating whether index columns
  are returned as well.

- `selected_ranges`:

  An optional list with matrices where each matrix i describes the
  (min,max) pair of ranges for dimension i

- `selected_points`:

  An optional list with vectors where each vector i describes the
  selected points for dimension i

- `query_layout`:

  An optional character value

- `datetimes_as_int64`:

  A logical value

- `encryption_key`:

  A character value

- `query_condition`:

  A Query Condition object

- `timestamp_start`:

  A POSIXct datetime variable for the inclusive interval start

- `timestamp_end`:

  A POSIXct datetime variable for the inclusive interval start

- `return_as`:

  A character value with the desired `tiledb_array` conversion,
  permitted values are ‘asis’ (default, returning a list of columns),
  ‘array’, ‘matrix’,‘data.frame’, ‘data.table’ ‘tibble’, ‘arrow_table’
  or ‘arrow’ (where the last two are synomyms); note that ‘data.table’,
  ‘tibble’ and ‘arrow’ require the respective packages to be installed.

- `query_statistics`:

  A logical value, defaults to ‘FALSE’; if ‘TRUE’ the query statistics
  are returned (as a JSON string) via the attribute ‘query_statistics’
  of the return object.

- `sil`:

  An optional and internal list object with schema information, used for
  parsing queries.

- `dumpbuffers`:

  An optional character variable with a directory name (relative to
  `/dev/shm`) for writing out results buffers (for internal use /
  testing)

- `buffers`:

  An optional list with full pathnames of shared memory buffers to read
  data from

- `strings_as_factors`:

  An optional logical to convert character columns to factor type

- `keep_open`:

  An optional logical to not close after read or write

- `ptr`:

  External pointer to the underlying implementation
