
// open questions:
//  - simple accessor functions or s4 instances?
//  - base layer here and refined interface in R function?

#include <tiledb.h>

#define STRICT_R_HEADERS
#include <Rcpp.h>

using namespace tiledb;

// forward declaration
const char* _tiledb_datatype_to_string(tiledb_datatype_t dtype);

// [[Rcpp::export]]
SEXP read_array_metadata(const std::string array_name, const std::string key) {
  // Create TileDB context
  Context ctx;

  // Open array for reading
  // TODO error check
  Array array(ctx, array_name, TILEDB_READ);

  // Read with key
  tiledb_datatype_t v_type;
  uint32_t v_num;
  const void* v;
  array.get_metadata(key.c_str(), &v_type, &v_num, &v);
  if (v == NULL) {
    return R_NilValue;
  }

  //Rcpp::Rcout << v_type << std::endl;
  //Rcpp::Rcout << v_num << std::endl;

  // TODO more cases
  if (v_type == TILEDB_INT32) {
    IntegerVector vec(v_num);
    memcpy(vec.begin(), v, v_num*sizeof(int32_t));
    return(vec);
  } else if (v_type == TILEDB_FLOAT64) {
    NumericVector vec(v_num);
    memcpy(vec.begin(), v, v_num*sizeof(double));
    return(vec);
  } else if (v_type == TILEDB_FLOAT32) {
    NumericVector vec(v_num);
    const float *fvec = static_cast<const float*>(v);
    size_t n = static_cast<size_t>(v_num);
    for (size_t i=0; i<n; i++) vec(i) = static_cast<double>(fvec[i]);
    return(vec);
  } else {
    Rcpp::stop("No support yet for %s", _tiledb_datatype_to_string(v_type));
  }
}
