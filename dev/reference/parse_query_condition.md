# Create a 'tiledb_query_condition' object from an expression

The grammar for query conditions is at present constraint to eight
operators (`">"`, `">="`, `"<"`, `"<="`, `"=="`, `"!="`, `"%in%"`,
`"%nin%"`), and three boolean operators (`"&&"`, also as `"&"`, (`"||"`,
also as `"|"`, and `"!"` for negation. Note that we locally define
`"%nin%"` as [`Negate()`](https://rdrr.io/r/base/funprog.html) call
around `%in%)` which extends R a little for this use case.

## Usage

``` r
parse_query_condition(
  expr,
  ta = NULL,
  debug = FALSE,
  strict = TRUE,
  use_int64 = FALSE
)
```

## Arguments

- expr:

  An expression that is understood by the TileDB grammar for query
  conditions.

- ta:

  A tiledb_array object that the query condition is applied to; this
  argument is optional in some cases but required in some others.

- debug:

  A boolean toggle to enable more verbose operations, defaults to
  'FALSE'.

- strict:

  A boolean toggle to, if set, errors if a non-existing attribute is
  selected or filtered on, defaults to 'TRUE'; if 'FALSE' a warning is
  shown by execution proceeds.

- use_int64:

  A boolean toggle to switch to `integer64` if `integer` is seen,
  default is false to remain as a default four-byte `int`

## Value

A `tiledb_query_condition` object

## Details

Expressions are parsed locally by this function. The `debug=TRUE` option
may help if an issue has to be diagnosed. In most cases of an erroneous
parse, it generally helps to supply the `tiledb_array` providing schema
information. One example are numeric and integer columns where the data
type is difficult to guess. Also, when using the `"%in%"` or `"%nin%"`
operators, the argument is mandatory.

## Examples

``` r
if (FALSE) { # \dontrun{
uri <- "mem://airquality" # change to on-disk for persistence
fromDataFrame(airquality, uri, col_index = c("Month", "Day")) # dense array
## query condition on dense array requires extended=FALSE
tiledb_array(uri,
  return_as = "data.frame", extended = FALSE,
  query_condition = parse_query_condition(Temp > 90)
)[]
} # }
```
