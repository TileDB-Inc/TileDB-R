#include <tiledb/tiledb>
#include "Rcpp.h"

#ifndef __tiledb_h__
#define __tiledb_h__

using namespace Rcpp;

struct var_length_string_buffer {
  std::vector<uint64_t> offsets;  // vector for offset values
  std::string str;              	// string for data values
  int32_t rows, cols;             // dimension from subarray
};
typedef struct var_length_string_buffer vlsbuf_t;

#endif // __tiledb_h__
