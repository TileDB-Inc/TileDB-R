
#include "libtiledb.h"
#include "finalizers.h"
#include "tiledb_version.h"

#include <fstream>
#include <unistd.h>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

tiledb_vfs_mode_t _string_to_tiledb_vfs_mode_t(std::string modestr) {
  if (modestr == "READ") {
    return TILEDB_VFS_READ;
  } else if (modestr == "WRITE") {
    return TILEDB_VFS_WRITE;
  } else if (modestr == "APPEND") {
    return TILEDB_VFS_APPEND;
  } else {
    Rcpp::stop("Unknown TileDB VFS mode type '%s'", modestr.c_str());
  }
}

// [[Rcpp::export]]
XPtr<vfs_fh_t> libtiledb_vfs_open(XPtr<tiledb::Context> ctxxp, XPtr<tiledb::VFS> vfsxp,
                                  std::string uri, std::string mode) {
   std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
   std::shared_ptr<tiledb_vfs_t> vfs = vfsxp.get()->ptr();
   tiledb_vfs_fh_t *fh = nullptr;
   tiledb_vfs_mode_t vfsmode = _string_to_tiledb_vfs_mode_t(mode);
   tiledb_vfs_open(ctx.get(), vfs.get(), uri.c_str(), vfsmode, &fh);
   XPtr<vfs_fh_t> ptr = XPtr<vfs_fh_t>(new vfs_fh_t);
   ptr->fh = static_cast<void*>(fh);
   return ptr;
}

// [[Rcpp::export]]
void libtiledb_vfs_close(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_close(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh));
}

// [[Rcpp::export]]
void libtiledb_vfs_write(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh,
                         Rcpp::IntegerVector vec) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_write(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh),
                   &(vec[0]), vec.size()*sizeof(int));
}

// [[Rcpp::export]]
Rcpp::IntegerVector libtiledb_vfs_read(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh,
                                       double offset, double nbytes) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  std::int64_t offs = makeScalarInteger64(offset);
  std::int64_t nb = makeScalarInteger64(nbytes);
  Rcpp::IntegerVector buf(nb/4);
  tiledb_vfs_read(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh), offs, &(buf[0]), nb);
  return buf;
}

// [[Rcpp::export]]
void libtiledb_vfs_sync(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_sync(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh));
}
