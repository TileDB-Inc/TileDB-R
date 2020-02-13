#!/usr/bin/python3

import tiledb
import numpy as np

uri = "/tmp/tiledb/test4"

with tiledb.open(uri) as A:
    print(A[1:5,1:5]["a2"])
    print(A[1:5,1:5]["a3"])
