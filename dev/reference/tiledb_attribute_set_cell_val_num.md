# Set the number of scalar values per attribute cell

Set the number of scalar values per attribute cell

## Usage

``` r
cell_val_num(x) <- value

# S4 method for class 'tiledb_attr'
cell_val_num(x) <- value

tiledb_attribute_set_cell_val_num(x, value)
```

## Arguments

- x:

  A TileDB Attribute object

- value:

  An integer value of number of cells

## Value

The modified attribute is returned
