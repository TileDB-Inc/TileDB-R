//  MIT License
//
//  Copyright (c) 2020 TileDB Inc.
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include <tiledb.h>
#include "tiledb_version.h"
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
#include <arrowio>
#endif
#include "finalizers.h"


#if TILEDB_VERSION >= TileDB_Version(2,2,0)
// two finalizers used if we use XPtr
extern "C" {

    inline void libtiledb_arrowarray_delete(SEXP sexp) {
        XPtr<ArrowArray> arr(sexp);
        ArrowArray* ptr = arr.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_arrowschema_delete(SEXP sexp) {
        XPtr<ArrowSchema> attr(sexp);
        ArrowSchema* ptr = attr.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

}

// borrowed from arrow R package (licensed under Apache-2.0) and slightly extended
template <typename T>
struct Pointer {
    Pointer() : ptr_(new T()) {}
    explicit Pointer(SEXP x) : ptr_(reinterpret_cast<T*>(static_cast<uintptr_t>(REAL(x)[0]))) {}
    inline operator SEXP() const { return Rf_ScalarReal(static_cast<double>(reinterpret_cast<uintptr_t>(ptr_)));  }
    inline operator T*()   const { return ptr_; }
    inline void finalize()       { delete ptr_; }
    inline T* get()        const { return ptr_; }
    T* ptr_;
};

Pointer<ArrowArray> allocate_arrow_array()   { return {}; }
Pointer<ArrowSchema> allocate_arrow_schema() { return {}; }
void delete_arrow_array(Pointer<ArrowArray> ptr)   { ptr.finalize(); }
void delete_arrow_schema(Pointer<ArrowSchema> ptr) { ptr.finalize(); }


// ' First pass version exports XPtrs
//
// ' @ export
// [ [ Rcpp::export ] ]
Rcpp::List query_export_buffer_xptr(XPtr<tiledb::Query> queryxp, std::string name) {
    std::shared_ptr<tiledb::Query> query(queryxp.get());
    tiledb::arrow::ArrowAdapter adapter(query);
    Rcpp::Rcout << "Name is " << name << std::endl;

    auto arrptr = Rcpp::XPtr<ArrowArray>(new ArrowArray, false);
    registerXptrFinalizer(arrptr, libtiledb_arrowarray_delete);
    auto schptr = Rcpp::XPtr<ArrowSchema>(new ArrowSchema, false);
    registerXptrFinalizer(schptr, libtiledb_arrowarray_delete);

    adapter.export_buffer(name.c_str(), arrptr.get(), schptr.get());

    return Rcpp::List::create(Rcpp::Named("array") = arrptr,
                              Rcpp::Named("schema") = schptr);
}
#endif


//' Export Query Buffer to Pair of Arrow IO Pointers
//'
//' This function exports the name buffer from \sQuote{READ} query
//' to two Arrow C pointers.
//' @param queryxp An external pointer object to TileDB Query object
//' @param name A character variable identifying the buffer
//' @return A two-element numeric vector where the two elements are
//' pointer to the Arrow array and schema
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector tiledb_query_export_buffer(XPtr<tiledb::Query> queryxp, std::string name) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    std::shared_ptr<tiledb::Query> query(queryxp.get());
    tiledb::arrow::ArrowAdapter adapter(query);

    auto arrptr = allocate_arrow_array();
    auto schptr = allocate_arrow_schema();
    adapter.export_buffer(name.c_str(),
                          static_cast<void*>(arrptr.get()),
                          static_cast<void*>(schptr.get()));

    // use same trick as arrow as send the pointer as a double converted to SEXP
    return Rcpp::NumericVector::create(Rcpp::as<double>(arrptr),
                                       Rcpp::as<double>(schptr));
#else
    Rcpp::stop("This function requires TileDB 2.2.0 or greater.");
    return Rcpp::NumericVector::create(0, 0); // not reached
#endif
}
