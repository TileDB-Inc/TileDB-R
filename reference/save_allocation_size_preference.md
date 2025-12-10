# Store allocation size preference

Save (or load) allocation size default preference in an optional config
file

## Usage

``` r
save_allocation_size_preference(value)

load_allocation_size_preference()

get_allocation_size_preference()

set_allocation_size_preference(value)
```

## Arguments

- value:

  A numeric value with the desired allocation size (in bytes).

## Value

For the setter, `TRUE` is returned invisibly but the function is invoked
for the side effect of storing the value. For the getters, the value as
a numeric.

## Details

When retrieving data from sparse arrays, allocation sizes cannot be
determined *ex ante* as the degree of sparsity is unknown. A
configuration value can aide in providing an allocation size value.
These functions let the user store such a value for retrieval by their
package or script code. The preference will be encoded in a
configuration file as R (version 4.0.0 or later) allows a user- and
package specific configuration files. These helper functions sets and
retrieve the value, respectively, or retrieve the cached value from the
package environment where is it set at package load.

The value will be stored as a character value and reparsed so ‘1e6’ and
‘1000000’ are equivalent, and the fixed (but adjustable) number of
digits for numerical precision *use for formatting* will impact the
writing. This should have no effect on standard allocation sizes.

The value is used as a limit *per column* so total memory use per query
will a multiple of this value, and increasing in dimension and attribute
count.

A fallback value of 10 mb is used if no user value is set.

## Note

This function requires R version 4.0.0 or later to utilise the per-user
config directory accessor function. For older R versions, a fallback
from the TileDB configuration object is used.
