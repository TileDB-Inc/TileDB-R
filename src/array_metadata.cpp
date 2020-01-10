
// open questions:
//  - simple accessor functions or s4 instances?
//      -> s4 instance could cache more easily
//  - base layer here and refined interface in R function?
//      -> add s4 instance on R side

#include <tiledb.h>

#define STRICT_R_HEADERS
#include <Rcpp.h>

using namespace tiledb;

// forward declaration
const char* _tiledb_datatype_to_string(tiledb_datatype_t dtype);

// [[Rcpp::export]]
bool has_array_metadata(const std::string array_name, const std::string key) {
  // Create TileDB context
  Context ctx;

  // Open array for reading
  // TODO error check
  Array array(ctx, array_name, TILEDB_READ);

  tiledb_datatype_t v_type;
  bool res = array.has_metadata(key.c_str(), &v_type);
  return res;
}

// [[Rcpp::export]]
int num_array_metadata(const std::string array_name) {
  // Create TileDB context
  Context ctx;

  // Open array for reading
  // TODO error check
  Array array(ctx, array_name, TILEDB_READ);

  tiledb_datatype_t v_type;
  uint64_t nm = array.metadata_num();
  return static_cast<int>(nm);
}

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
    Rcpp::IntegerVector vec(v_num);
    std::memcpy(vec.begin(), v, v_num*sizeof(int32_t));
    return(vec);
  } else if (v_type == TILEDB_FLOAT64) {
    Rcpp::NumericVector vec(v_num);
    std::memcpy(vec.begin(), v, v_num*sizeof(double));
    return(vec);
  } else if (v_type == TILEDB_FLOAT32) {
    Rcpp::NumericVector vec(v_num);
    const float *fvec = static_cast<const float*>(v);
    size_t n = static_cast<size_t>(v_num);
    for (size_t i=0; i<n; i++) vec(i) = static_cast<double>(fvec[i]);
    return(vec);
  } else if (v_type == TILEDB_STRING_ASCII) {
    // from reading the code I think I am inferring that we do not have vectors of strings but just one
    Rcpp::CharacterVector vec(1);
    std::string s(static_cast<const char*>(v));
    s.resize(v_num);            // incoming char* is not null terminated, so ensure we only consume v_num bytes and terminate
    return(Rcpp::wrap(s));
  } else {
    Rcpp::stop("No support yet for %s", _tiledb_datatype_to_string(v_type));
  }
}

// [[Rcpp::export]]
bool write_array_metadata(const std::string array_name, const std::string key, const SEXP obj) {
  // Create TileDB context
  Context ctx;

  // Open array for writing
  // TODO error check
  Array array(ctx, array_name, TILEDB_WRITE);

  // TODO probably want to add a mapper from SEXP type to tiledb type in libtiledb.cpp
  switch(TYPEOF(obj)) {
    case VECSXP: {
      Rcpp::stop("List objects are not supported.");
      array.close();
      return false;
      break;// not reached
    }
    case REALSXP: {
      Rcpp::NumericVector v(obj);
      array.put_metadata(key.c_str(), TILEDB_FLOAT64, v.size(), v.begin());
     break;
    }
    case INTSXP: {
      Rcpp::IntegerVector v(obj);
      array.put_metadata(key.c_str(), TILEDB_INT32, v.size(), v.begin());
      break;
    }
    case STRSXP: {
      Rcpp::CharacterVector v(obj);
      std::string s(v[0]);
      // TODO: best string type? And what about the other vector elements?
      array.put_metadata(key.c_str(), TILEDB_STRING_ASCII, s.length(), s.c_str());
      break;
    }
    default: {
      Rcpp::stop("No support (yet) for type '%d'.", TYPEOF(obj));
      array.close();
      return false;
      break;// not reached
    }
  }
  // Close array - Important so that the metadata get flushed
  array.close();
  return true;
}
