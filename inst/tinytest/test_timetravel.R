library(tinytest)
library(tiledb)

isMacOS <- (Sys.info()['sysname'] == "Darwin")

ctx <- tiledb_ctx(limitTileDBCores())


## tests formerly in test_tiledbarray.R


## timestamp_start and timestamp_end
uri <- tempfile()

data <- data.frame(grp = rep(1:4, each=10), 				# 4 distinct groups
                   val = rep(1:4, each=10) * 100 + 1:10)    # and 4 sets of value (used as index)

times <- Sys.time() + 0:3          			# pre-allocate for four time stamps

deltat <- 0.100                     		# start with this gap of 'delta_t'
success <- FALSE

## the test has seen to be sensitive to the test environment; some CI frameworks were 'randomly'
## failing even with a 'moderately large' timestep. so we now invert this by first finding a suitable
## timestep (still starting from a small value) and then using that value
while (deltat < 30) {
    if (dir.exists(uri)) unlink(uri, TRUE)

    fromDataFrame(subset(data, grp==1), # write array, but only write group one
                  uri, sparse=TRUE,
                  col_index="val", tile_domain=list(val=range(data$val)))

    arr <- tiledb_array(uri)
    for (i in 2:4) {                        # in loop, write groups two to four
        Sys.sleep(deltat)
        arr[] <- subset(data, grp==i)
        times[i] <- Sys.time()              # and note time
    }

    ## we need an 'epsilon' because when we record 'times' is not exactly where the array timestamp is
    epst <- deltat/2

    res1 <- tiledb_array(uri, return_as="data.frame")[]							 		# no limits
    res2 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[1]-epst, timestamp_end=times[2]+epst)[]    # end after 2nd timestamp
    res3 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[4]+epst)[] 	# start after fourth
    res4 <- tiledb_array(uri, return_as="data.frame", timestamp_end=times[3]-epst)[]    # end before 3rd
    res5 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[2]-epst, timestamp_end=times[3]+epst)[]

    if (isTRUE(all.equal(NROW(res1), 40)) &&                    # all four groups
        isTRUE(all.equal(NROW(res2), 20)) &&		            # expect group one + two (20 elements)
        isTRUE(all.equal(NROW(res3), 0))  &&		            # expect zero data
        isTRUE(all.equal(NROW(res4), 20)) &&  					# expect 2 groups, 20 obs
        isTRUE(all.equal(max(res4$grp), 2))  &&		            # with groups being 1 and 2
        isTRUE(all.equal(NROW(res5), 20)) && 		            # expects 2 groups, 2 and 3, with 20 obs
        isTRUE(all.equal(min(res5$grp), 2)) &&
        isTRUE(all.equal(max(res5$grp), 3))) {
           success <- TRUE
           break
    }
    deltat <- deltat * 5
}

if (!success) exit_file("Issue with time traveling")

res1 <- tiledb_array(uri, return_as="data.frame")[] 		# no limits
expect_equal(NROW(res1), 40)                            # all four observations

res2 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[1]-epst, timestamp_end=times[2]+epst)[]    # end before 1st timestamp
expect_equal(NROW(res2), 20)            # expect group one and two (20 elements)

res3 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[4]+epst)[] # start after fourth
expect_equal(NROW(res3), 0)             # expect zero data

res4 <- tiledb_array(uri, return_as="data.frame", timestamp_end=times[3]-epst)[]    # end before 3rd
expect_equal(NROW(res4), 20)            # expect 2 groups, 20 obs
expect_equal(max(res4$grp), 2)          # with groups being 1 and 2

res5 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[2]-epst, timestamp_end=times[3]+epst)[]
expect_equal(NROW(res5), 20)            # expects 2 groups, 2 and 3, with 20 obs
expect_equal(min(res5$grp), 2)
expect_equal(max(res5$grp), 3)

## now given the existing array we can also read its timestamps
## from the fragment info and use with a smaller eps_t
fi <- tiledb_fragment_info(uri)
n <- tiledb_fragment_info_get_num(fi)
expect_equal(n, 4)
times <- do.call(c, lapply(seq_len(n), function(i) tiledb_fragment_info_get_timestamp_range(fi, i-1)[1]))

epstsml <- 0.005
res1 <- tiledb_array(uri, return_as="data.frame")[]						# no limits
res2 <- tiledb_array(uri, return_as="data.frame", timestamp_end=times[1]+epstsml)[]    	# end after 1st timestamp
res3 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[4]+epstsml)[]  	# start after fourth
res4 <- tiledb_array(uri, return_as="data.frame", timestamp_end=times[3]-epstsml)[]    	# end before 3rd
res5 <- tiledb_array(uri, return_as="data.frame", timestamp_start=times[2]-epstsml, timestamp_end=times[3]+epstsml)[]
expect_equal(NROW(res1), 40)
expect_equal(NROW(res2), 10)		            # expect group one (10 elements)
expect_equal(NROW(res3), 0)			            # expect zero data
expect_equal(NROW(res4), 20)  					# expect 2 groups, 20 obs
expect_equal(max(res4$grp), 2)		            # with groups being 1 and 2
expect_equal(NROW(res5), 20) 		            # expects 2 groups, 2 and 3, with 20 obs
expect_equal(min(res5$grp), 2)
expect_equal(max(res5$grp), 3)



## timestamp_start and timestamp_end



## time-travel vaccum test
vfs <- tiledb_vfs()
uridir <- if (tiledb_vfs_is_dir(file.path(uri, "__fragments"))) file.path(uri, "__fragments") else uri
ndirfull <- tiledb_vfs_ls(uridir, vfs=vfs)
array_consolidate(uri, start_time=times[2]-epst, end_time=times[3]+epst)
array_vacuum(uri, start_time=times[2]-epst, end_time=times[3]+epst)
ndircons <- tiledb_vfs_ls(uridir, vfs=vfs)
expect_true(length(ndircons) < length(ndirfull))
array_consolidate(uri, start_time=times[1]-0.5, end_time=times[3])
array_vacuum(uri, start_time=times[1]-0.5, end_time=times[3])
ndircons2 <- tiledb_vfs_ls(uridir, vfs=vfs)
expect_true(length(ndircons2) < length(ndircons))

## time-travel via policy object
if (tiledb_version(TRUE) < "2.15.0") exit_file("Needs TileDB 2.15.* or later")

uri <- tempfile()
ts <- function(x) as.POSIXct(x/1000, tz="UTC", origin="1970-01-01")
D <- data.frame(ind = 1L, val = 1.0)
fromDataFrame(D, uri, col_index = 1, mode = "schema_only")
for (tstamp in 1:3) {
    arr <- tiledb_array(uri, "WRITE", timestamp_end = ts(tstamp))
    arr[] <- data.frame(ind=1, val=tstamp)
}

## Unconstrained, expect all 3
expect_equal(tiledb_array(uri, return_as="data.frame")[]$val, c(1,2,3))

## Interval (1,2), expect 1 to 2
expect_equal(tiledb_array(uri, return_as="data.frame",
                          timestamp_start=ts(1), timestamp_end=ts(2))[]$val, c(1,2))

## Interval (2,3), expect 2 to 3
expect_equal(tiledb_array(uri, return_as="data.frame",
                          timestamp_start=ts(2), timestamp_end=ts(3))[]$val, c(2,3))

## Endpoint 1, expect 1
expect_equal(tiledb_array(uri, return_as="data.frame", timestamp_end=ts(1))[]$val, 1)

## Interval (1, ) expect 1 to 3
expect_equal(tiledb_array(uri, return_as="data.frame", timestamp_start=ts(1))[]$val, c(1,2,3))

## Also check fragment_info
fi <- tiledb_fragment_info(uri)
expect_equal(as.numeric(tiledb_fragment_info_get_timestamp_range(fi,0))*1000, c(1,1))
expect_equal(as.numeric(tiledb_fragment_info_get_timestamp_range(fi,1))*1000, c(2,2))
expect_equal(as.numeric(tiledb_fragment_info_get_timestamp_range(fi,2))*1000, c(3,3))


## tests formerly in test_timetravel_extra.R

## earlier time travel test recast via timestamp_{start,end}
## time travel

tmp <- tempfile()
dir.create(tmp)
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 10L), 5L, "INT32"),
                              tiledb_dim("cols", c(1L, 10L), 5L, "INT32")))
schema <- tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "INT32")), sparse = TRUE)
invisible( tiledb_array_create(tmp, schema) )

I <- c(1, 2, 2)
J <- c(1, 4, 3)
data <- c(1L, 2L, 3L)
now1 <- as.POSIXct(60, tz="UTC") 		# the epoch plus one minute
A <- tiledb_array(uri = tmp, timestamp_start=now1, timestamp_end=now1)
A[I, J] <- data

deltat <- 1 							# delta of 1 second
epst <- deltat/2

I <- c(8, 6, 9)
J <- c(5, 7, 8)
data <- c(11L, 22L, 33L)
now2 <- Sys.time()
A <- tiledb_array(uri = tmp, timestamp_start=now2, timestamp_end=now2)
A[I, J] <- data

A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_end=now1 - epst)
expect_equal(nrow(A[]), 0) 			# no rows before start
A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_start=now1 + epst)
expect_equal(nrow(A[]), 3) 			# three rows in first write at now1
A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_start=now1 - epst, timestamp_end=now2 - epst)
expect_equal(nrow(A[]), 3)			# three rows if end before now2
A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_start=now1 - epst, timestamp_end=now2 + epst)
expect_true(nrow(A[]) >= 3)			# all data from before now1 to after now2
