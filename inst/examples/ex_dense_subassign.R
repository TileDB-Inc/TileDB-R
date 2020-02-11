
## using quickstart_dense

library(tiledb)

arr <- tiledb_dense("/tmp/tiledb/quickstart_dense/")
##arr[]

## assign 2x2 in the middle of arr
arr[2:3,2:3] <- list(array(seq(42L,45L), c(2,2)))


## similar for single value
arr[3,3] <- list(array(99L, c(1,1)))
arr[1,4] <- array(101L, c(1,1))

## but cannot assign single scalar as it needs dimensionality -- maybe should fix that
#fails:   arr[4,4] <- 200L
