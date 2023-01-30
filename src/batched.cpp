
#include "libtiledb.h"
#include "tiledb_version.h"

class QueryWrapper {
public:
    QueryWrapper(SEXP qp): qryptr(qp), init(true) {};
    inline bool is_intialized(void) { return qryptr != R_NilValue && init; }
private:
    SEXP qryptr = R_NilValue;
    bool init = false;
};

// [[Rcpp::export]]
SEXP makeQueryWrapper(SEXP qp) {
    //alternate form: Rcpp::XPtr<QueryWrapper> makeQueryWrapper(SEXP qp) {
    QueryWrapper* qwp = new QueryWrapper(qp);
    Rcpp::XPtr<QueryWrapper> ptr(qwp);
    return ptr;
}
