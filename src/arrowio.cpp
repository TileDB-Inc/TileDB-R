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

// borrowed from arrow R package (version 7.0.0)  (licensed under Apache-2.0) and slightly extended/adapted
template <typename T>
struct Pointer {
    Pointer() : ptr_(new T()) {}
    explicit Pointer(SEXP x) {
        if (TYPEOF(x) == EXTPTRSXP) {
            ptr_ = (T*)R_ExternalPtrAddr(x);

        } else if (TYPEOF(x) == STRSXP && Rf_length(x) == 1) {
            // User passed a character representation of the pointer address
            SEXP char0 = STRING_ELT(x, 0);
            if (char0 == NA_STRING) {
                Rcpp::stop("Can't convert NA_character_ to pointer");
            }

            const char* input_chars = CHAR(char0);
            char* endptr;
            uint64_t ptr_value = strtoull(input_chars, &endptr, 0);
            if (endptr != (input_chars + strlen(input_chars))) {
                Rcpp::stop("Can't parse '%s' as a 64-bit integer address", input_chars);
            }
            ptr_ = reinterpret_cast<T*>(static_cast<uintptr_t>(ptr_value));

        } else if (Rf_inherits(x, "integer64") && Rf_length(x) == 1) {
            // User passed an integer64(1) of the pointer address an integer64 is a REALSXP
            // under the hood, with the bytes of each double reinterpreted as an int64.
            uint64_t ptr_value;
            memcpy(&ptr_value, REAL(x), sizeof(uint64_t));
            ptr_ = reinterpret_cast<T*>(static_cast<uintptr_t>(ptr_value));

        } else if (TYPEOF(x) == RAWSXP && Rf_length(x) == sizeof(T*)) {
            // User passed a raw(<pointer size>) with the literal bytes of the pointer.
            memcpy(&ptr_, RAW(x), sizeof(T*));

        } else if (TYPEOF(x) == REALSXP && Rf_length(x) == 1) {
            // User passed a double(1) of the static-casted pointer address.
            ptr_ = reinterpret_cast<T*>(static_cast<uintptr_t>(REAL(x)[0]));

        } else {
            Rcpp::stop("Can't convert input object to pointer: %d", TYPEOF(x));
        }
    }

    inline operator SEXP() const { return R_MakeExternalPtr(ptr_, R_NilValue, R_NilValue); }

    inline operator T*() const { return ptr_; }

    inline void finalize() { delete ptr_; }

    inline T* get() const { return ptr_; }

    T* ptr_;
};


#if TILEDB_VERSION >= TileDB_Version(2,2,0)
// these functions are local to this compilation unit as is the defintion of Pointer
Pointer<ArrowArray> allocate_arrow_array()   { return {}; }
Pointer<ArrowSchema> allocate_arrow_schema() { return {}; }
void delete_arrow_array(Pointer<ArrowArray> ptr)   { ptr.finalize(); }
void delete_arrow_schema(Pointer<ArrowSchema> ptr) { ptr.finalize(); }
#endif

// [[Rcpp::export(.allocate_arrow_array_as_xptr)]]
SEXP allocate_arrow_array_as_xptr() {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    return allocate_arrow_array();
#else
    return NA_REAL;
#endif
}

// [[Rcpp::export(.allocate_arrow_schema_as_xptr)]]
SEXP allocate_arrow_schema_as_xptr() {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    return allocate_arrow_schema();
#else
    return NA_REAL;
#endif
}

// [[Rcpp::export(.delete_arrow_array_from_xptr)]]
void delete_arrow_array_from_xptr(SEXP sxp) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    Pointer<ArrowArray> ptr(sxp);
    delete_arrow_array(ptr);
#endif
}

// [[Rcpp::export(.delete_arrow_schema_from_xptr)]]
void delete_arrow_schema_from_xptr(SEXP sxp) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    Pointer<ArrowSchema> ptr(sxp);
    delete_arrow_schema(ptr);
#endif
}

// [[Rcpp::export]]
Rcpp::List libtiledb_query_export_buffer(XPtr<tiledb::Context> ctx,
                                         XPtr<tiledb::Query> query,
                                         std::string name) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    auto arrptr = allocate_arrow_array(); 	// external pointer object
    auto schptr = allocate_arrow_schema();
    adapter.export_buffer(name.c_str(),
                          static_cast<void*>(R_ExternalPtrAddr(arrptr)),
                          static_cast<void*>(R_ExternalPtrAddr(schptr)));
    return Rcpp::List::create(arrptr, schptr);
#else
    Rcpp::stop("This function requires TileDB 2.2.0 or greater.");
    return Rcpp::NumericVector::create(0, 0); // not reached
#endif
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_import_buffer(XPtr<tiledb::Context> ctx,
                                                  XPtr<tiledb::Query> query,
                                                  std::string name,
                                                  Rcpp::List arrowpointers) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    adapter.import_buffer(name.c_str(),
                          R_ExternalPtrAddr(arrowpointers[0]),
                          R_ExternalPtrAddr(arrowpointers[1]));
#else
    Rcpp::stop("This function requires TileDB 2.2.0 or greater.");
#endif
    return(query);
}
