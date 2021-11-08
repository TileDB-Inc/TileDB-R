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
// performance reasons in the context of TileDB Cloud).

#include "libtiledb.h"
#include "finalizers.h"
#include "tiledb_version.h"

#include <sys/types.h>
#ifndef _WIN32
#include <sys/mman.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <filesystem>
#include <regex>

static const bool debug = false;

using namespace Rcpp;

static std::string _datafile(const std::string dir, const std::string name) {
#ifdef __linux__
    std::string path = std::string("/dev/shm/") + dir + std::string("/buffers/data/");
    if (!std::filesystem::is_directory(path)) std::filesystem::create_directories(path);
    return path + name;
#else
    return std::string();
#endif
}

static std::string _offsetsfile(const std::string dir, const std::string name) {
#ifdef __linux__
    std::string path = std::string("/dev/shm/") + dir + std::string("/buffers/offsets/");
    if (!std::filesystem::is_directory(path)) std::filesystem::create_directories(path);
    return path + name;
#else
    return std::string();
#endif
}

static std::string _validityfile(const std::string dir, const std::string name) {
#ifdef __linux__
    std::string path = std::string("/dev/shm/") + dir + std::string("/buffers/validity/");
    if (!std::filesystem::is_directory(path)) std::filesystem::create_directories(path);
    return path + name;
#else
    return std::string();
#endif
}

// [[Rcpp::export]]
void vecbuf_to_shmem(std::string dir, std::string name, XPtr<query_buf_t> buf, int sz) {
#ifdef __linux__
    std::string bufferpath = _datafile(dir, name);
    if (debug) Rcpp::Rcout << "Writing " << bufferpath << " ";
    int mode = S_IRWXU | S_IRWXG | S_IRWXO;
    int fd = open(bufferpath.c_str(), O_RDWR | O_CREAT | O_TRUNC, mode);
    int n = sz * buf->size;
    void *dest = mmap(NULL,      				// kernel picks address
                      n, 				   		// length
                      PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    lseek (fd, n-1, SEEK_SET); 	 // seek to n, write an empty char to allocate block, then memcpy in
    if (write (fd, "", 1) != 1) Rcpp::stop("write error");
    memcpy (dest, (void*) buf->vec.data(), n);
    close(fd);

    if (buf->nullable) {
        std::string validitypath = _validityfile(dir, name);
        if (debug) Rcpp::Rcout << " writing " << validitypath << " ";
        mode = S_IRWXU | S_IRWXG | S_IRWXO;
        fd = open(validitypath.c_str(), O_RDWR | O_CREAT | O_TRUNC, mode);
        n = sz * sizeof(uint8_t);
        void *dest = mmap(NULL,      				// kernel picks address
                          n, 				   		// length
                          PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
        lseek (fd, n-1, SEEK_SET); 	 // seek to n, write an empty char to allocate block, then memcpy in
        if (write (fd, "", 1) != 1) Rcpp::stop("write error");
        memcpy (dest, (void*) buf->validity_map.data(), n);
        close(fd);
    }
    if (debug) Rcpp::Rcout << " ... done\n";
#endif
}

// [[Rcpp::export]]
void vlcbuf_to_shmem(std::string dir, std::string name, XPtr<vlc_buf_t> buf, IntegerVector vec) {
#ifdef __linux__
    std::string bufferpath = _datafile(dir, name);
    if (debug) Rcpp::Rcout << "Writing char to " << bufferpath << " " << buf->str.length() << " " << buf->offsets.size() << " ";
    //if (debug) Rcpp::Rcout << buf->str << " (" << std::strlen(buf->str.c_str()) << ")\n";
    int mode = S_IRWXU | S_IRWXG | S_IRWXO;
    int fd = open(bufferpath.c_str(), O_RDWR | O_CREAT | O_TRUNC, mode);
    // int n = buf->str.size();
    int n = std::strlen(buf->str.c_str()); 		// NB: only write string length
    void *dest = mmap(NULL,      				// kernel picks address
                      n,	 			   		// length
                      PROT_READ | PROT_WRITE,
                      MAP_SHARED,
                      fd,
                      0);
    lseek (fd, n-1, SEEK_SET); 	 // seek to n, write an empty char to allocate block, then memcpy in
    int res = write (fd, "", 1); if (res != 1) { Rcpp::Rcout << "Res: " << res << std::endl; Rcpp::stop("write error"); }
    memcpy (dest, (void*) buf->str.c_str(), n);
    close(fd);

    bufferpath = _offsetsfile(dir, name);
    fd = open(bufferpath.c_str(), O_RDWR | O_CREAT | O_TRUNC, mode);
    //n = buf->offsets.size() * sizeof(uint64_t);
    n = vec[0] * sizeof(uint64_t);
    if (debug) Rcpp::Rcout << "Offsets (byte) size: " << n << " " << vec[0]*sizeof(uint64_t);
    dest = mmap(NULL,      				// kernel picks address
                n + 1, 			   		// length
                PROT_READ | PROT_WRITE,
                MAP_SHARED,
                fd,
                0);
    lseek (fd, n-1, SEEK_SET); 	 // seek to n, write an empty char to allocate block, then memcpy in
    if (write (fd, "", 1) != 1) Rcpp::stop("write error");
    memcpy (dest, (void*) buf->offsets.data(), n);
    close(fd);

    if (buf->nullable) {
        std::string validitypath = _validityfile(dir, name);
        if (debug) Rcpp::Rcout << " writing " << validitypath << " ";
        mode = S_IRWXU | S_IRWXG | S_IRWXO;
        fd = open(validitypath.c_str(), O_RDWR | O_CREAT | O_TRUNC, mode);
        n = vec[0] * sizeof(uint8_t);
        void *dest = mmap(NULL,      				// kernel picks address
                          n, 				   		// length
                          PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
        lseek (fd, n-1, SEEK_SET); 	 // seek to n, write an empty char to allocate block, then memcpy in
        if (write (fd, "", 1) != 1) Rcpp::stop("write error");
        memcpy (dest, (void*) buf->validity_map.data(), n);
        close(fd);
    }
    if (debug) Rcpp::Rcout << std::endl;
#endif
}

// [[Rcpp::export]]
XPtr<query_buf_t> querybuf_from_shmem(std::string path, std::string dtype, bool nullable=false) {
#ifdef __linux__
    // struct query_buffer {
    //     //void *ptr;                    	// pointer to data as an alternative
    //     std::vector<int8_t> vec;        	// vector of int8_t as a memory container
    //     tiledb_datatype_t dtype;        	// data type
    //     R_xlen_t ncells;                	// extent
    //     size_t size;                    	// element size
    //     std::vector<uint8_t> validity_map;  // for nullable vectors
    //     bool nullable;                      // flag
    // };
    // typedef struct query_buffer query_buf_t;

    // open shared memory region, and set up mmap
    int fd = open(path.c_str(), O_RDONLY);
    if (fd < 0) Rcpp::stop("Cannot open %s for reading", path.c_str());
    struct stat statbuf;
    if (fstat(fd,&statbuf) < 0) Rcpp::stop("Cannot fstat %s", path.c_str());
    int sz = statbuf.st_size;
    void *src = mmap (0, sz, PROT_READ, MAP_SHARED, fd, 0);
    if (src == (caddr_t) -1) Rcpp::stop("mmap error");

    // allocate buffer, register finalizer, then set up buffer
    XPtr<query_buf_t> buf = XPtr<query_buf_t>(new query_buf_t, false);
    registerXptrFinalizer(buf, libtiledb_query_buf_delete);
    buf->dtype = _string_to_tiledb_datatype(dtype);
    buf->size = _tiledb_datatype_to_sizeof(_string_to_tiledb_datatype(dtype));
    buf->ncells = sz / buf->size;
    buf->nullable = false; // default, overriden if buffer in validity path seen
    if (debug) Rcpp::Rcout << path << " "
                           << " dtype " << dtype
                           << " sizeof:" << buf->size
                           << " ncells:" << buf->ncells
                           << " vecsize:" << sz;
    buf->vec.resize(sz);
    memcpy(buf->vec.data(), src, sz);
    close(fd);

    std::string validitypath = std::regex_replace(path, std::regex("/data/"), "/validity/");
    if (std::filesystem::is_regular_file(validitypath)) {
        if (debug) Rcpp::Rcout << " seeing " << validitypath;
        int fdv = open(validitypath.c_str(), O_RDONLY);
        if (fdv < 0) Rcpp::stop("Cannot open %s for reading", validitypath.c_str());
        struct stat statbufv;
        if (fstat(fdv,&statbufv) < 0) Rcpp::stop("Cannot fstat %s", validitypath.c_str());
        int szv = statbufv.st_size;
        void *srcv = mmap (0, szv, PROT_READ, MAP_SHARED, fdv, 0);
        if (srcv == (caddr_t) -1) Rcpp::stop("mmap error");
        if (debug) Rcpp::Rcout << validitypath << " vecsize:" << szv;
        buf->validity_map.resize(szv);
        buf->nullable = true;
        if (szv != buf->ncells) Rcpp::stop("Unexpected length mismatch for validity buffer");
        memcpy(buf->validity_map.data(), srcv, szv);
        close(fdv);
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
XPtr<vlc_buf_t> vlcbuf_from_shmem(std::string datapath, std::string dtype, bool nullable=false) {
#ifdef __linux__
    // struct var_length_char_buffer {
    //     std::vector<uint64_t> offsets;  	// vector for offset values
    //     std::string str;              		// string for data values
    //     int32_t rows, cols;              	// dimension from subarray
    //     bool nullable;                       // flag
    //     std::vector<uint8_t> validity_map;   // for nullable vectors
    // };
    // typedef struct var_length_char_buffer vlc_buf_t;

    // open shared memory region, and set up mmap for data
    int fdd = open(datapath.c_str(), O_RDONLY);
    if (fdd < 0) Rcpp::stop("Cannot open %s for reading", datapath.c_str());
    struct stat statbufd;
    if (fstat(fdd,&statbufd) < 0) Rcpp::stop("Cannot fstat %s", datapath.c_str());
    int szd = statbufd.st_size;
    void *datasrc = mmap (0, szd, PROT_READ, MAP_SHARED, fdd, 0);
    if (datasrc == (caddr_t) -1) Rcpp::stop("mmap error");

    // open shared memory region, and set up mmap for offsets
    std::string offsetspath = std::regex_replace(datapath, std::regex("/data/"), "/offsets/");
    int fdo = open(offsetspath.c_str(), O_RDONLY);
    if (fdo < 0) Rcpp::stop("Cannot open %s for reading", offsetspath.c_str());
    struct stat statbufo;
    if (fstat(fdo,&statbufo) < 0) Rcpp::stop("Cannot fstat %s", offsetspath.c_str());
    int szo = statbufo.st_size;
    void *offsetssrc = mmap (0, szo, PROT_READ, MAP_SHARED, fdo, 0);
    if (datasrc == (caddr_t) -1) Rcpp::stop("mmap error");

    // allocate buffer, register finalizer, then set up buffer
    XPtr<vlc_buf_t> buf = XPtr<vlc_buf_t>(new vlc_buf_t, false);
    registerXptrFinalizer(buf, libtiledb_vlc_buf_delete);
    buf->offsets.resize(szo/sizeof(uint64_t));
    buf->rows = szo/sizeof(uint64_t);
    buf->cols = 2;              // value not used
    memcpy(buf->offsets.data(), offsetssrc, szo);
    buf->str.resize(szd);
    memcpy(buf->str.data(), datasrc, szd);
    buf->nullable = false; // default, overriden if buffer in validity path seen

    if (debug) Rcpp::Rcout << datapath << " " << offsetspath
                           << " data:" << szd
                           << " offsets:" << szo/sizeof(uint64_t);

    std::string validitypath = std::regex_replace(datapath, std::regex("/data/"), "/validity/");
    if (std::filesystem::is_regular_file(validitypath)) {
        if (debug) Rcpp::Rcout << " seeing " << validitypath;
        int fdv = open(validitypath.c_str(), O_RDONLY);
        if (fdv < 0) Rcpp::stop("Cannot open %s for reading", validitypath.c_str());
        struct stat statbufv;
        if (fstat(fdv,&statbufv) < 0) Rcpp::stop("Cannot fstat %s", validitypath.c_str());
        int szv = statbufv.st_size;
        void *srcv = mmap (0, szv, PROT_READ, MAP_SHARED, fdv, 0);
        if (srcv == (caddr_t) -1) Rcpp::stop("mmap error");
        if (debug) Rcpp::Rcout << validitypath << " vecsize:" << szv;
        buf->validity_map.resize(szv);
        buf->nullable = true;
        if (szv != buf->rows) Rcpp::stop("Unexpected length mismatch for validity buffer");
        memcpy(buf->validity_map.data(), srcv, szv);
        close(fdv);
    }
    if (debug) Rcpp::Rcout << std::endl;

    close(fdd);
    close(fdo);
    return buf;
#else
    Rcpp::stop("This function is only available under Linux.");
    // not reached
    XPtr<vlc_buf_t> buf = XPtr<vlc_buf_t>(new vlc_buf_t, false);
    registerXptrFinalizer(buf, libtiledb_vlc_buf_delete);
    return buf;
#endif
}
