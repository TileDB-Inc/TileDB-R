//  MIT License
//
//  Copyright (c) 2020      TileDB Inc.
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

#ifndef __libtiledb_finalizers_h__
#define __libtiledb_finalizers_h__


// Register a finalizer
//
// See Section 5.13 'External pointers and weak references' in WRE
//
// Some details:
//   - the first arguments, SEXP s, will in general be a templated XPtr<>()
//     instance but because we communicate with R via a specified C interface
//     it is simpler to use SEXP (to which the templated XPtr, just like any
//     other Rcpp expression, can convert)
//   - the second argument is a typedef from one of the R headers and specifies
//     the one and only form for a finalizer function that can be registered,
//     name taking a SEXP and returning void; all our declarations below fit that;
//     also note that these have to declared 'extern "C"' as they use C linkage
//   - the last argument is a plain bool for us which gets map to an Rboolean
//     as defined in R_ext/Boolean.h
//   - the function is inline to provide header-only use
//   - the function is not exported to R, or registered, but "simply" used by C++
//
// Important:
//   - when these finalizers are registered we cannot use the XPtr deleter mechanism
//     so the bool argument in the templated XPtr constructor has to be set to false
//
inline void registerXptrFinalizer(SEXP s, R_CFinalizer_t f, bool onexit = true) {
    R_RegisterCFinalizerEx(s, f, onexit ? TRUE : FALSE);
}

extern "C" {

    inline void libtiledb_arrayschema_delete(SEXP sexp) {
        XPtr<tiledb::ArraySchema> arr(sexp);
        tiledb::ArraySchema* ptr = arr.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_attribute_delete(SEXP sexp) {
        XPtr<tiledb::Attribute> attr(sexp);
        tiledb::Attribute* ptr = attr.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_config_delete(SEXP sexp) {
        XPtr<tiledb::Config> cfg(sexp);
        tiledb::Config* ptr = cfg.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_ctx_delete(SEXP sexp) {
        XPtr<tiledb::Context> ctx(sexp);
        tiledb::Context* ptr = ctx.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_dimension_delete(SEXP sexp) {
        XPtr<tiledb::Dimension> dimension(sexp);
        tiledb::Dimension* ptr = dimension.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_domain_delete(SEXP sexp) {
        XPtr<tiledb::Domain> domain(sexp);
        tiledb::Domain* ptr = domain.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_filter_delete(SEXP sexp) {
        XPtr<tiledb::Filter> filter(sexp);
        tiledb::Filter* ptr = filter.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_filterlist_delete(SEXP sexp) {
        XPtr<tiledb::FilterList> filterlist(sexp);
        tiledb::FilterList* ptr = filterlist.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_vfs_delete(SEXP sexp) {
        XPtr<tiledb::VFS> vfs(sexp);
        tiledb::VFS* ptr = vfs.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_query_buf_delete(SEXP sexp) {
        XPtr<query_buf_t> buf(sexp);
        query_buf_t* ptr = buf.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_vlc_buf_delete(SEXP sexp) {
        XPtr<vlc_buf_t> buf(sexp);
        vlc_buf_t* ptr = buf.get();
        if (ptr != nullptr) {
            delete ptr;
            ptr = nullptr;
        }
    }

    inline void libtiledb_vfs_fh_delete(SEXP sexp) {
        XPtr<vfs_fh_t> buf(sexp);
        vfs_fh_t* ptr = buf.get();
        if (ptr != nullptr) {
            tiledb_vfs_fh_t *fh = static_cast<tiledb_vfs_fh_t*>(ptr->fh);
            tiledb_vfs_fh_free(&fh);
            delete ptr;
            ptr = nullptr;
        }
    }

}

#endif
