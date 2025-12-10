# Dumps internal TileDB statistics to file or stdout

Dumps internal TileDB statistics to file or stdout

## Usage

``` r
tiledb_stats_dump(path)
```

## Arguments

- path:

  Character variable with path to stats file; if the empty string is
  passed then the result is displayed on stdout.

## Examples

``` r
pth <- tempfile()
tiledb_stats_dump(pth)
cat(readLines(pth)[1:10], sep = "\n")
#> [
#> 
#> ]
#> NA
#> NA
#> NA
#> NA
#> NA
#> NA
#> NA
```
