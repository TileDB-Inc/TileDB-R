//  MIT License
//
//  Copyright (c) 2020-2022 TileDB Inc.
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

#include "libtiledb.h"
#include "tiledb_version.h"
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
#include <tiledb/arrowio>
#endif


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

#if TILEDB_VERSION >= TileDB_Version(2,2,0)
// these functions are local to this compilation unit as is the defintion of Pointer
Pointer<ArrowArray> allocate_arrow_array()   { return {}; }
Pointer<ArrowSchema> allocate_arrow_schema() { return {}; }
void delete_arrow_array(Pointer<ArrowArray> ptr)   { ptr.finalize(); }
void delete_arrow_schema(Pointer<ArrowSchema> ptr) { ptr.finalize(); }
#endif

// [[Rcpp::export(.allocate_arrow_array_as_double)]]
double allocate_arrow_array_as_double() {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    return Rcpp::as<double>(allocate_arrow_array());
#else
    return NA_REAL;
#endif
}

// [[Rcpp::export(.allocate_arrow_schema_as_double)]]
double allocate_arrow_schema_as_double() {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    return Rcpp::as<double>(allocate_arrow_schema());
#else
    return NA_REAL;
#endif
}

// [[Rcpp::export(.delete_arrow_array_from_double)]]
void delete_arrow_array_from_double(double dbl) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    Pointer<ArrowArray> ptr(Rcpp::wrap(dbl));
    delete_arrow_array(ptr);
#endif
}

// [[Rcpp::export(.delete_arrow_schema_from_double)]]
void delete_arrow_schema_from_double(double dbl) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    Pointer<ArrowSchema> ptr(Rcpp::wrap(dbl));
    delete_arrow_schema(ptr);
#endif
}


// [[Rcpp::export]]
Rcpp::NumericVector libtiledb_query_export_buffer(XPtr<tiledb::Context> ctx,
                                                  XPtr<tiledb::Query> query,
                                                  std::string name) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    auto arrptr = allocate_arrow_array(); 	// manage outside and pass in?
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

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_import_buffer(XPtr<tiledb::Context> ctx,
                                                  XPtr<tiledb::Query> query,
                                                  std::string name,
                                                  Rcpp::NumericVector arrowpointers) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    Pointer<ArrowArray> arrptr(Rcpp::wrap(arrowpointers[0]));
    Pointer<ArrowSchema> schptr(Rcpp::wrap(arrowpointers[1]));
    adapter.import_buffer(name.c_str(),
                          static_cast<void*>(arrptr.get()),
                          static_cast<void*>(schptr.get()));
#else
    Rcpp::stop("This function requires TileDB 2.2.0 or greater.");
#endif
    return(query);
}
