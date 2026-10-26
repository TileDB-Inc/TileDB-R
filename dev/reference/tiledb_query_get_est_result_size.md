# Retrieve the estimated result size for a query and attribute

When reading from sparse arrays, one cannot know beforehand how big the
result will be (unless one actually executes the query). This function
offers a way to get the estimated result size for the given attribute.
As TileDB does not actually execute the query, getting the estimated
result is very fast.

## Usage

``` r
tiledb_query_get_est_result_size(query, name)
```

## Arguments

- query:

  A TileDB Query object

- name:

  A variable with an attribute name

## Value

An estimate of the query result size
