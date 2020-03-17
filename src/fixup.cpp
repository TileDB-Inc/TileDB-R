
#include <Rcpp.h>

// [[Rcpp::export]]
void fixup_coord_buffer(Rcpp::NumericVector v) {
  int n = v.size();
  std::vector<int64_t> tt(n);
  std::memcpy(tt.data(), &(v[0]), n*8);
  std::vector<double> dd(n);
  for (int i=0; i<n; i++) {
    dd[i] = static_cast<double>(tt[i]);
  }
  std::memcpy(&(v[0]), dd.data(), n*8);
}
