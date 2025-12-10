# Constructs a tiledb_array object backed by a persisted tiledb array uri

tiledb_array returns a new object. This class is experimental.

## Usage

``` r
tiledb_array(
  uri,
  query_type = c("READ", "WRITE"),
  is.sparse = NA,
  attrs = character(),
  extended = TRUE,
  selected_ranges = list(),
  selected_points = list(),
  query_layout = character(),
  datetimes_as_int64 = FALSE,
  encryption_key = character(),
  query_condition = new("tiledb_query_condition"),
  timestamp_start = as.POSIXct(double(), origin = "1970-01-01"),
  timestamp_end = as.POSIXct(double(), origin = "1970-01-01"),
  return_as = get_return_as_preference(),
  query_statistics = FALSE,
  strings_as_factors = getOption("stringsAsFactors", FALSE),
  keep_open = FALSE,
  sil = list(),
  dumpbuffers = character(),
  buffers = list(),
  ctx = tiledb_get_context(),
  as.data.frame = FALSE
)

tiledb_dense(...)

tiledb_sparse(...)
```

## Arguments

- uri:

  uri path to the tiledb array

- query_type:

  optionally loads the array in "READ" or "WRITE" only modes.

- is.sparse:

  optional logical switch, defaults to "NA" letting array determine it

- attrs:

  optional character vector to select attributes, default is empty
  implying all are selected, the special value `NA_character_` has the
  opposite effect and implies no attributes are returned.

- extended:

  optional logical switch selecting wide ‘data.frame’ format, defaults
  to `TRUE`

- selected_ranges:

  optional A list with matrices where each matrix i describes the
  (min,max) pair of ranges selected for dimension i

- selected_points:

  optional A list with vectors where each vector i describes the points
  selected in dimension i

- query_layout:

  optional A value for the TileDB query layout, defaults to an empty
  character variable indicating no special layout is set

- datetimes_as_int64:

  optional A logical value selecting date and datetime value
  representation as ‘raw’ `integer64` and not as `Date`, `POSIXct` or
  `nanotime` objects.

- encryption_key:

  optional A character value with an AES-256 encryption key in case the
  array was written with encryption.

- query_condition:

  optional `tiledb_query_condition` object, by default uninitialized
  without a condition; this functionality requires TileDB 2.3.0 or later

- timestamp_start:

  optional A POSIXct Datetime value determining the inclusive time point
  at which the array is to be openened. No fragments written earlier
  will be considered.

- timestamp_end:

  optional A POSIXct Datetime value determining the inclusive time point
  until which the array is to be openened. No fragments written earlier
  later be considered.

- return_as:

  optional A character value with the desired `tiledb_array` conversion,
  permitted values are ‘asis’ (default, returning a list of columns),
  ‘array’, ‘matrix’, ‘data.frame’, ‘data.table’, ‘tibble’,
  ‘arrow_table’, or ‘arrow’ (as an alias for ‘arrow_table’; here
  ‘data.table’, ‘tibble’ and ‘arrow’ require the respective packages to
  be installed. The existing `as.*` arguments take precedent over this.

- query_statistics:

  optional A logical value, defaults to ‘FALSE’; if ‘TRUE’ the query
  statistics are returned (as a JSON string) via the attribute
  ‘query_statistics’ of the return object.

- strings_as_factors:

  An optional logical to convert character columns to factor type;
  defaults to the value of `getOption("stringsAsFactors", FALSE)`.

- keep_open:

  An optional logical to not close after read or write

- sil:

  optional A list, by default empty to store schema information when
  query objects are parsed.

- dumpbuffers:

  An optional character variable with a directory name (relative to
  `/dev/shm`) for writing out results buffers (for internal use /
  testing)

- buffers:

  An optional list with full pathnames of shared memory buffers to read
  data from

- ctx:

  optional tiledb_ctx

- as.data.frame:

  An optional deprecated alternative to `return_as="data.frame"` which
  has been deprecated and removed, but is still used in one BioConductor
  package; this argument will be removed once the updated package has
  been released.

- ...:

  Used as a pass-through for `tiledb_dense` and `tiledb_sparse` aliasing

## Value

tiledb_array object
