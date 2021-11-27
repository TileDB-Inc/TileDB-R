//  MIT License
//
//  Copyright (c) 2021 TileDB Inc.
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

// The functions in this file are of less general use or interest as they rely on the
// 'handshakes' from the TileDB Cloud backend, which is not published as open source.
// They simply constitute an alternate mechanism of filling result data structures taking
// advantage of an auxiliary parallel query tp TileDB Embedded (that is done solely for
// performance reasons in the context of TileDB Cloud). They are also made conditional
// on building on Linux as the shared memory inter-process communication is only use there.

#include "libtiledb.h"
#include "finalizers.h"
#include "tiledb_version.h"

#ifdef __linux__
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <filesystem>
#include <regex>

static const bool debug = false;

static std::string _datafile(const std::string dir, const std::string name) {
    std::string path = std::string("/dev/shm/") + dir + std::string("/buffers/data/");
    if (!std::filesystem::is_directory(path)) std::filesystem::create_directories(path);
    return path + name;
}

static std::string _offsetsfile(const std::string dir, const std::string name) {
    std::string path = std::string("/dev/shm/") + dir + std::string("/buffers/offsets/");
    if (!std::filesystem::is_directory(path)) std::filesystem::create_directories(path);
    return path + name;
}

static std::string _validityfile(const std::string dir, const std::string name) {
    std::string path = std::string("/dev/shm/") + dir + std::string("/buffers/validity/");
    if (!std::filesystem::is_directory(path)) std::filesystem::create_directories(path);
    return path + name;
}

void write_buffer(std::string bufferpath, int numelem, int elemsize, void *data_ptr) {
    if (debug) Rcpp::Rcout << "Writing " << bufferpath << " ";
    int mode = S_IRWXU | S_IRWXG | S_IRWXO;
    int fd = open(bufferpath.c_str(), O_RDWR | O_CREAT | O_TRUNC, mode);
    int bytes = numelem * elemsize;
    void *dest = mmap(NULL,      				// kernel picks address
                      bytes,  	 		   		// length
                      PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    lseek (fd, bytes-1, SEEK_SET); 				// seek to n, write an empty char to allocate block, then memcpy in
    if (write (fd, "", 1) != 1) Rcpp::stop("write error");
    memcpy (dest, data_ptr, bytes);
    close(fd);
    if (debug) Rcpp::Rcout << " ... done\n";
}

template <class T>
void read_buffer(std::string bufferpath, std::vector<T> & vec) {
    // // open shared memory region, and set up mmap
    int fd = open(bufferpath.c_str(), O_RDONLY);
    if (fd < 0) Rcpp::stop("Cannot open %s for reading", bufferpath.c_str());
    struct stat statbuf;
    if (fstat(fd,&statbuf) < 0) Rcpp::stop("Cannot fstat %s", bufferpath.c_str());
    int sz = statbuf.st_size;
    void *src = mmap (0, sz, PROT_READ, MAP_SHARED, fd, 0);
    if (src == (caddr_t) -1) Rcpp::stop("mmap error");

    vec.resize(sz / sizeof(T));
    memcpy(vec.data(), src, sz);
    close(fd);
}

void read_string(std::string bufferpath, std::string & str) {
    // // open shared memory region, and set up mmap
    int fd = open(bufferpath.c_str(), O_RDONLY);
    if (fd < 0) Rcpp::stop("Cannot open %s for reading", bufferpath.c_str());
    struct stat statbuf;
    if (fstat(fd,&statbuf) < 0) Rcpp::stop("Cannot fstat %s", bufferpath.c_str());
    int sz = statbuf.st_size;
    void *src = mmap (0, sz, PROT_READ, MAP_SHARED, fd, 0);
    if (src == (caddr_t) -1) Rcpp::stop("mmap error");

    str.resize(sz);
    memcpy(str.data(), src, sz);
    close(fd);
}

#endif


// [[Rcpp::export]]
void vecbuf_to_shmem(std::string dir, std::string name, XPtr<query_buf_t> buf, int sz) {
#ifdef __linux__
    std::string bufferpath = _datafile(dir, name);
    write_buffer(bufferpath, sz, buf->size, buf->vec.data());
    if (buf->nullable) {
        std::string validitypath = _validityfile(dir, name);
        write_buffer(validitypath, sz, sizeof(uint8_t), buf->validity_map.data());
    }
#endif
}

// [[Rcpp::export]]
void vlcbuf_to_shmem(std::string dir, std::string name, XPtr<vlc_buf_t> buf, IntegerVector vec) {
#ifdef __linux__
    std::string bufferpath = _datafile(dir, name);
    write_buffer(bufferpath, std::strlen(buf->str.c_str()), 1L, (void*)buf->str.c_str());

    bufferpath = _offsetsfile(dir, name);
    write_buffer(bufferpath, vec[0], sizeof(uint64_t), buf->offsets.data());

    if (buf->nullable) {
        std::string validitypath = _validityfile(dir, name);
        write_buffer(validitypath, vec[0], sizeof(uint8_t), buf->validity_map.data());
    }
#endif
}

// [[Rcpp::export]]
XPtr<query_buf_t> querybuf_from_shmem(std::string path, std::string dtype) {
#ifdef __linux__
    // allocate buffer, register finalizer, then set up buffer
    XPtr<query_buf_t> buf = XPtr<query_buf_t>(new query_buf_t, false);
    registerXptrFinalizer(buf, libtiledb_query_buf_delete);
    buf->dtype = _string_to_tiledb_datatype(dtype);
    buf->size = static_cast<int32_t>(tiledb_datatype_size(_string_to_tiledb_datatype(dtype)));
    buf->nullable = false; // default, overriden if buffer in validity path seen
    read_buffer<int8_t>(path, buf->vec);
    buf->ncells = buf->vec.size() / buf->size;
    if (debug) Rcpp::Rcout << path << " " << " dtype " << dtype << " sizeof:" << buf->size
                           << " ncells:" << buf->ncells << " vecsize:" << buf->size * buf->ncells;

    std::string validitypath = std::regex_replace(path, std::regex("/data/"), "/validity/");
    if (std::filesystem::is_regular_file(validitypath)) {
        if (debug) Rcpp::Rcout << " seeing " << validitypath;
        read_buffer<uint8_t>(validitypath, buf->validity_map);
        buf->nullable = true;
    }
    if (debug) Rcpp::Rcout << std::endl;
    return buf;
#else
    Rcpp::stop("This function is only available under Linux.");
    // not reached
    XPtr<query_buf_t> buf = XPtr<query_buf_t>(new query_buf_t, false);
    registerXptrFinalizer(buf, libtiledb_query_buf_delete);
    return buf;
#endif
}

// [[Rcpp::export]]
XPtr<vlc_buf_t> vlcbuf_from_shmem(std::string datapath, std::string dtype) {
#ifdef __linux__
    // allocate buffer, register finalizer, then set up buffer
    XPtr<vlc_buf_t> buf = XPtr<vlc_buf_t>(new vlc_buf_t, false);
    registerXptrFinalizer(buf, libtiledb_vlc_buf_delete);
    read_string(datapath, buf->str);
    std::string offsetspath = std::regex_replace(datapath, std::regex("/data/"), "/offsets/");
    read_buffer<uint64_t>(offsetspath, buf->offsets);
    buf->rows = buf->offsets.size();
    buf->cols = 2;              // value not used
    buf->nullable = false;      // default, overridden below if validity path used

    if (debug) Rcpp::Rcout << datapath << " " << offsetspath
                           << " data:" << buf->str.size()
                           << " offsets:" << buf->offsets.size();

    std::string validitypath = std::regex_replace(datapath, std::regex("/data/"), "/validity/");
    if (std::filesystem::is_regular_file(validitypath)) {
        if (debug) Rcpp::Rcout << " validity: " << validitypath;
        read_buffer<uint8_t>(validitypath, buf->validity_map);
        buf->nullable = true;
    }
    if (debug) Rcpp::Rcout << std::endl;
    return buf;
#else
    Rcpp::stop("This function is only available under Linux.");
    // not reached
    XPtr<vlc_buf_t> buf = XPtr<vlc_buf_t>(new vlc_buf_t, false);
    registerXptrFinalizer(buf, libtiledb_vlc_buf_delete);
    return buf;
#endif
}
