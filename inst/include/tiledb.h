#include <tiledb/tiledb>
#include "Rcpp.h"

#ifndef __tiledb_h__
#define __tiledb_h__

using namespace Rcpp;

struct var_length_char_buffer {
  std::vector<uint64_t> offsets;  // vector for offset values
  std::string str;              	// string for data values
  int32_t rows, cols;             // dimension from subarray
};
typedef struct var_length_char_buffer vlc_buf_t;

// template <typename T>
// struct var_length_vec_buffer_initial {
//   std::vector<uint64_t> offsets;  // vector for offset values
//   std::vector<T> data;            // vector for data values
// };
// using vli_buf_t_old = struct var_length_vec_buffer_initial<int32_t>;
// using vld_buf_t_old = struct var_length_vec_buffer_initial<double>;


template <typename T>
struct var_length_vec_buffer {
public:
  std::vector<uint64_t> offsets;  // vector for offset values
  std::vector<T> data;            // vector for data values
};
using vli_buf_t = struct var_length_vec_buffer<int32_t>;
using vld_buf_t = struct var_length_vec_buffer<double>;


#endif // __tiledb_h__
