# Return the number of scalar values per attribute cell

Return the number of scalar values per attribute cell

## Usage

``` r
cell_val_num(object)

# S4 method for class 'tiledb_attr'
cell_val_num(object)

tiledb_attribute_get_cell_val_num(object)
```

## Arguments

- object:

  `tiledb_attr` object

## Value

integer number of cells

## Examples

``` r
a1 <- tiledb_attr("a1", type = "FLOAT64", ncells = 1)
cell_val_num(a1)
#> [1] 1
```
