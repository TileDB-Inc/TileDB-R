
// This file borrows, with acknowledgements, from the MIT-licensed code in the
// repository at https://github.com/coolbutuseless/rconnection and adapts it (in
// a slightly simplified version) to TileDB. It also uses the approach employed
// in the also MIT-licensed repository https://github.com/r-lib/archive to deploy
// a connection from within R without triggering a warning from package checks.

#include <Rcpp/Lighter>         				// for Rcpp
#include <tinyspdl.h>                           // for spdl logging
#include "connection/connection.h"              // for connections
#include "tiledb.h"

extern "C" SEXP vfile_c_impl_(SEXP, SEXP, SEXP);

//' Create a custom file connection
//'
//' @details
//' This \code{vfs_file()} connection works like the \code{file()} connection in R itself.
//'
//' This connection works with both ASCII and binary data, e.g. using
//' \code{readLines()} and \code{readBin()}.
//'
//' @param description path to a filename; contrary to \code{rconnection} a connection
//' object is not supported.
//' @param open character string. A description of how to open the connection if
//' it is to be opened upon creation e.g. "rb". Default "" (empty string) means
//' to not open the connection on creation - user must still call \code{open()}.
//' Note: If an "open" string is provided, the user must still call \code{close()}
//' otherwise the contents of the file aren't completely flushed until the
//' connection is garbage collected.
//' @param verbosity integer value 0, 1, or 2. Default: 0.
//' Set to \code{0} for no debugging messages, \code{1} for some high-level messages
//' and \code{verbosity = 2} for all debugging messages.
//'
//' @export
//'
//' @examples
//' \dontrun{
//' tmp <- tempfile()
//' dat <- as.raw(1:255)
//' writeBin(dat, vfs_file(tmp))
//' readBin(vfs_file(tmp),  raw(), 1000)
//' }
// [[Rcpp::export]]
SEXP vfs_file(std::string description, std::string mode = "", int verbosity = 0) {
    spdl::debug("[vfile_] entered");
    return vfile_c_impl_(Rcpp::wrap(description), Rcpp::wrap(mode), Rcpp::wrap(verbosity));
}

extern "C" {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Vfile state.
//   - This is user/private data stored with the 'Rconn' struct that gets
//     passed to each callback function
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
typedef struct {
    Rboolean is_file;  // Is this a file or a connection we're writing to?
    FILE *fp;          // FilePointer if accessing a file
    Rconnection inner; // inner connection if accessing a connection
    int verbosity;     // For debugging!
    tiledb::Context *ctx;
    tiledb::VFS *vfs;
    char* uri;
    std::vector<std::byte> buf;
    size_t nread;
} vfile_state;

Rboolean vfile_open(struct Rconn *rconn);
void vfile_close(struct Rconn *rconn);
void vfile_destroy(struct Rconn *rconn);
int vfile_fgetc_internal(struct Rconn *rconn);
double vfile_seek(struct Rconn *rconn, double x, int y, int z);
void vfile_truncate(struct Rconn *rconn);
int vfile_fflush(struct Rconn *rconn);
size_t vfile_read(void *dst, size_t size, size_t nitems, struct Rconn *rconn);
int vfile_fgetc(struct Rconn *rconn);
size_t vfile_write(const void *src, size_t size, size_t nitems, struct Rconn *rconn);
int vfile_vfprintf(struct Rconn *rconn, const char* fmt, va_list ap);

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Initialize a vfile() R connection object to return to the user
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP vfile_c_impl_(SEXP description_, SEXP mode_, SEXP verbosity_) {

    spdl::debug("[vfile_c_impl_] entered");
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Initialize User State
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    vfile_state *vstate = (vfile_state *)calloc(1, sizeof(vfile_state));
    vstate->ctx = new tiledb::Context();
    vstate->vfs = new tiledb::VFS(*vstate->ctx);
    vstate->verbosity = Rf_asInteger(verbosity_);

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Set information regarding file vs connection handling
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    char *description;
    if (TYPEOF(description_) == STRSXP) {
        vstate->is_file = TRUE;
        description = (char *)CHAR(STRING_ELT(description_, 0));
        spdl::debug(tfm::format("[vfile_c_impl_] file %s", description));
        vstate->uri = description;
    } else {
        /* vstate->is_file = FALSE; */
        /* description = "vfile(connection)"; */
        /* vstate->inner = R_GetConnection(description_); */
        /* if (vstate->inner->isopen) { */
        /*   error("vfile_(): inner connection must not already be open"); */
        /* } */
        /* // Ensure we start with EOF not set, as this is not zeroed in all cases */
        /* // within R/connections.c */
        /* vstate->inner->EOF_signalled = FALSE; */
    }


    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // R will alloc for 'con' within R_new_custom_connection() and then
    // I think it takes responsibility for freeing it later.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Rconnection con = NULL;
    //SEXP rc = PROTECT(R_new_custom_connection(description, "rb", "vfile", &con));
    SEXP rc = PROTECT(new_connection(description, "rb", "vfile", &con));

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // text       - true if connection operates on text
    // isopen     - true if connection is open
    // incomplete - used in @code{do_readLines}, @code{do_isincomplete},
    //              and text_vfprintf, From `?connections`: true if last
    //              read was blocked, or for an output text connection whether
    //              there is unflushed output
    // canread    - true if connection is readable
    // canwrite   - true if connection is writable
    // canseek    - true if connection is seekable
    // blocking   - true if connection reads are blocking
    // isGzcon    - true if connection operates on gzip compressed data
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    con->isopen     = FALSE; // not open initially.
    con->incomplete =  TRUE; // NFI. Data write hasn't been completed?
    con->text       = FALSE; // binary connection by default
    con->canread    =  TRUE; // read-only for now
    con->canwrite   =  TRUE; // read-only for now
    con->canseek    = FALSE; // not possible in this implementation
    con->blocking   =  TRUE; // blacking IO
    con->isGzcon    = FALSE; // Not a gzcon

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // In base R's connections, EOF_signalled is set to zero during 'set_iconv()'
    // I'm not setting any text conversion stuff, so
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    con->EOF_signalled = FALSE;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Not sure what this really means, but vfile() is not going to do
    // any character conversion, so let's pretend any text returned in readLines()
    // is utf8.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    con->UTF8out =  TRUE;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // This private data is user data that will be available in each of the
    // following callbacks
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    con->private_ptr = vstate;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Callbacks
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    con->open           = vfile_open;
    con->close          = vfile_close;
    con->destroy        = vfile_destroy;
    con->vfprintf       = vfile_vfprintf;
    con->fgetc          = vfile_fgetc;
    con->fgetc_internal = vfile_fgetc_internal;
    con->seek           = vfile_seek;
    con->truncate       = vfile_truncate;
    con->fflush         = vfile_fflush;
    con->read           = vfile_read;
    con->write          = vfile_write;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Auto open if 'mode' is set to something other than the empty string.
    // An issue is that without the context stuff (not exported from R?),
    // I don't think I can get the context to auto-close!
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    const char *mode = CHAR(STRING_ELT(mode_, 0));
    strncpy(con->mode, mode, 4);
    con->mode[4] = '\0';
    if (strlen(mode) > 0) {
        con->open(con);
    }

    UNPROTECT(1);
    return rc;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// From R source code: 'src/main/connections.c'
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// SEXP   R_new_custom_connection(
//            const char *description,  // the filename related to this particular instance
//            const char *mode,         // read/write/binarymode/textmode
//            const char *class_name,   // 'vfile'
//            Rconnection *ptr          // Rconnection pointer
//        );
//
// The returned value is the R-side instance. To avoid additional call to getConnection()
//  the internal Rconnection pointer will be placed in ptr[0] if ptr is not NULL.
//  It is the responsibility of the caller to customize callbacks in the structure,
//  they are initialized to dummy_ (where available) and null_ (all others) callbacks.
//  Also note that the resulting object has a finalizer, so any clean up (including after
//  errors) is done by garbage collection - the caller may not free anything in the
//  structure explicitly (that includes the con->private_ptr pointer!).


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// From Matthew Shotwell
// https://biostatmatt.com/R/R-conn-ints/C-Structures.html#C-Structures
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// struct Rconn {
//   /** class name (null terminated) **/
//   char* class;
//
//   /** description (null terminated), can be a filename, url, or other
//    identifier, depending on the connection type
//    **/
//   char* description;
//   int enc; /* the encoding of 'description' */
//
//   /** file operation mode (null terminated) **/
//   char mode[5];
//
//   /** text       - true if connection operates on text
//    isopen     - true if connection is open
//    incomplete - used in @code{do_readLines}, @code{do_isincomplete},
//    and text_vfprintf, From `?connections`: true if last
//    read was blocked, or for an output text connection whether
//    there is unflushed output
//    canread    - true if connection is readable
//    canwrite   - true if connection is writable
//    canseek    - true if connection is seekable
//    blocking   - true if connection reads are blocking
//    isGzcon    - true if connection operates on gzip compressed data
//    **/
//   Rboolean text, isopen, incomplete, canread, canwrite, canseek, blocking,
//   isGzcon;
//
//   /** function pointers for I/O operations **/
//
//
//   /** open - called when the connection should be opened
//    args: struct Rconn * - an initialized connection to be opened
//    return: Rboolean - true if connection successfully opened, false otherwise
//    **/
//   Rboolean (*open)(struct Rconn *);
//
//
//   /** close - called when the connection should be closed
//    args: struct Rconn * - a connection to be closed
//    **/
//   void (*close)(struct Rconn *); /* routine closing after auto open */
//
//
//   /** destroy - called after the connection is closed in order to free memory,
//    and other cleanup tasks
//    args: struct Rconn * - a connection to be closed
//    **/
//   void (*destroy)(struct Rconn *); /* when closing connection */
//
//
//   /** vfprintf - variable argument list version of printf for a connection
//    args: struct Rconn * - a connection where items should be printed
//    const char *   - a format string in the style of the printf family
//    va_list        - a variable argument list containing the items
//    referred to in the format string
//    return: int - number of characters printed, negative on failure
//    **/
//   int (*vfprintf)(struct Rconn *, const char *, va_list);
//
//
//
//   /** fgetc - get a (re-encoded) character from the connection
//    args: struct Rconn * - a connection to be read
//    return: int - a (re-encoded) character, or R_EOF
//    **/
//   int (*fgetc)(struct Rconn *);
//
//
//   /** fgetc_internal - get a character from the connection
//    args: struct Rconn * - a connection to be read
//    return: int - a character, or R_EOF
//    **/
//   int (*fgetc_internal)(struct Rconn *);
//
//
//   /** seek - seek to a new position in the connection
//    args: struct Rconn * - a connection to seek
//    double         - offset to seek relative to origin, apparently
//    double is used here to avoid using
//    integer types, i.e. long int, which is
//    the prototype of the corresponding parameter
//    in fseek, as defined in stdio.h
//    int            - the origin of seeking, 1 (and any except 2 and
//    3) if relative to the beginning of the
//    connection, 2 if relative to the current
//    connection read/write position, 3 if relative to
//    the end of the connection
//    int            - currently only used by file_seek to select
//    the read or write position when the offset is NA
//    return: double - the read/write position of the connection before
//    seeking, negative on error double is again used to
//    avoid integer types
//    **/
//   double (*seek)(struct Rconn *, double, int, int);
//
//
//   /** truncate - truncate the connection at the current read/write position.
//    args: struct Rconn * - a connection to be truncated
//    **/
//   void (*truncate)(struct Rconn *);
//
//
//   /** fflush - called when the connection should flush internal read/write buffers
//    args: struct Rconn * - a connection to be flushed
//    return: int - zero on success, non-zero otherwise
//    **/
//   int (*fflush)(struct Rconn *);
//
//
//   /** read - read in the style of fread
//    args: void *         - buffer where data is read into
//    size_t         - size (in bytes) of each item to be read
//    size_t         - number of items to be read
//    struct Rconn * - a connection to be read
//    return: size_t - number of _items_ read
//    **/
//   size_t (*read)(void *, size_t, size_t, struct Rconn *);
//
//
//   /** write - write in the style of fwrite
//    args: void *         - buffer containing data to be written
//    size_t         - size (in bytes) of each item to be written
//    size_t         - number of items to be written
//    struct Rconn * - a connection to be written
//    return: size_t - number of _items_ written
//    **/
//   size_t (*write)(const void *, size_t, size_t, struct Rconn *);
//
//   /** cached and pushBack data
//    nPushBack   - number of lines of cached/pushBack storage
//    posPushBack - read position on current line of storage
//    PushBack    - cached/pushBack data lines ('\n' delimited)
//    save        - used to store the character following a \n, if not \r
//    save2       - used to store a character from Rconn_ungetc
//    **/
//   int nPushBack, posPushBack; /* number of lines, position on top line */
//   char **PushBack;
//   int save, save2;
//
//   /** character re-encoding with iconv
//    encname   - character encoding string (null terminated), this string
//    must be one of the standard encoding strings used by [lib]iconv
//    inconv    - input character encoding context (iconv_t)
//    outconv   - output character encoding context (iconv_t)
//    iconvbuff - input character encoding buffer
//    oconvbuff - output character encoding buffer
//    next      - only used by dummy_fgetc, points to the next re-encoded
//    character for reading
//    init_out  - storage for output iconv initialization sequence
//    navail    - iconv buffer offset
//    inavail   - iconv buffer offset
//    EOF_signalled - true if EOF reached
//    UTF8out   - true if connection writes UTF8 encoded characters
//    **/
//   char encname[101];
//   /* will be iconv_t, which is a pointer. NULL if not in use */
//   void *inconv, *outconv;
//   /* The idea here is that no MBCS char will ever not fit */
//   char iconvbuff[25], oconvbuff[50], *next, init_out[25];
//   short navail, inavail;
//   Rboolean EOF_signalled;
//   Rboolean UTF8out;
//
//   /** finalization pointers
//    id     - unique id, used to "ensure that the finalizer does not
//    try to close connection after it is alread closed"
//    (quoted from source code), but also to identify the
//    connection to be finalized. Using an arbitrary but
//    unique id here is clever, it means the connections
//    internals are further protected from passing references
//    to connection structures.
//    ex_ptr - external pointer, referenced by finalizer code
//    **/
//   void *id;
//   void *ex_ptr;
//
//   /** private user data (i.e. FILE *, offsets etc.) **/
//   void *private;
// };




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Vfile state.
//   - This is user/private data stored with the 'Rconn' struct that gets
//     passed to each callback function
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// typedef struct {
//     Rboolean is_file;  // Is this a file or a connection we're writing to?
//     FILE *fp;          // FilePointer if accessing a file
//     Rconnection inner; // inner connection if accessing a connection
//     int verbosity;     // For debugging!
// } vfile_state;



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// open()
//  - this may be called explicitly by a user call to open(con, mode)
//  - this is also called implicitly by readBin()/writeBin()/readLines()/writeLines();
//
// Possible Modes
//    - "r" or "rt"    Open for reading in text mode.
//    - "w" or "wt"    Open for writing in text mode.
//    - "a" or "at"    Open for appending in text mode.
//    - "rb"           Open for reading in binary mode.
//    - "wb"           Open for writing in binary mode.
//    - "ab"           Open for appending in binary mode.
//    - "r+", "r+b"    Open for reading and writing.
//    - "w+", "w+b"    Open for reading and writing, truncating file initially.
//    - "a+", "a+b"    Open for reading and appending.
//
// Notes:
//   - Supported modes: r, rt, w, wt, rb, wb
//   - unsupported modes: append, simultaneous read/write
//
// @return Rboolean - true if connection successfully opened, false otherwise
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Rboolean vfile_open(struct Rconn *rconn) {

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;
    if (vstate->verbosity > 0)
        Rprintf("vfile_open('%s', mode = '%s')\n", rconn->description, rconn->mode);
    spdl::debug(tfm::format("[vfile_open] entered for '%s' with '%s'",
                            rconn->description, rconn->mode));

    if (rconn->isopen) {
        Rf_error("vfile(): Connection is already open. Cannot open twice");
    }

    if (strchr(rconn->mode, 'a') != NULL) {
        Rf_error("vfile() does not support append.");
    } else if (strchr(rconn->mode, '+') != NULL) {
        Rf_error("vfile() does not support simultaneous r/w.");
    }

    rconn->text   = strchr(rconn->mode, 'b') ? FALSE : TRUE;
    rconn->isopen = TRUE;

    if (strchr(rconn->mode, 'w') == NULL) {
        rconn->canread  =  TRUE;
        rconn->canwrite = FALSE;
    } else {
        rconn->canread  = FALSE;
        rconn->canwrite =  TRUE;
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Setup file pointer
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (rconn->canread) {
        if (vstate->is_file) {
            // vstate->fp = fopen(rconn->description, "rb");
            // if (vstate->fp == NULL) {
            //     Rf_error("vfile_(): Couldn't open input file '%s' with mode '%s'", rconn->description, rconn->mode);
            // }
        } else {
            /* memset(vstate->inner->mode, '\0', 5); */
            /* strcpy(vstate->inner->mode, "rb"); */
            /* int res = vstate->inner->open(vstate->inner); */
            /* if (!res || !vstate->inner->isopen) { */
            /*   Rf_error("vfile_(): Couldn't open connection for reading"); */
            /* } */
        }
    } else {
        if (vstate->is_file) {
            // vstate->fp = fopen(rconn->description, "wb");
            // if (vstate->fp == NULL) {
            //     Rf_error("vfile_(): Couldn't open output file '%s' with mode '%s'", rconn->description, rconn->mode);
            // }
        } else {
            /* memset(vstate->inner->mode, '\0', 5); */
            /* strcpy(vstate->inner->mode, "wb"); */
            /* int res = vstate->inner->open(vstate->inner); */
            /* if (!res || !vstate->inner->isopen) { */
            /*   Rf_error("vfile_(): Couldn't open connection for writing"); */
            /* } */
        }
    }

    if (rconn->text && rconn->canread) {
        tiledb::VFS::filebuf sbuf(*vstate->vfs);
        sbuf.open(vstate->uri, std::ios::in);
        std::istream is(&sbuf);
        if (!is.good()) {
            Rcpp::stop("Error opening uri '%s' in text mode\n", vstate->uri);
        }
        auto file_size = vstate->vfs->file_size(vstate->uri);
        vstate->buf.resize(file_size);
        is.read((char*) vstate->buf.data(), file_size);
        vstate->nread = 0;
        sbuf.close();
    }

    if (rconn->canwrite) {
        if (vstate->vfs->is_file(vstate->uri)) {
            if (vstate->verbosity > 0)
                Rprintf("Uri '%s' exists, removing", vstate->uri);
            vstate->vfs->remove_file(vstate->uri);
        }
    }

    return TRUE;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Close()
//  - should only change state. No resources should be created/destroyed
//  - all actual destruction should happen in 'destroy()' which is called
//    by the garbage collector.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void vfile_close(struct Rconn *rconn) {
    spdl::debug("[vfile_close] entered");

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;
    if (vstate->verbosity > 0)Rprintf("vfile_close('%s')\n", rconn->description);

    rconn->isopen = FALSE;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Close the file
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (vstate->is_file && vstate->fp) {
        fclose(vstate->fp);
        vstate->fp = NULL;
    }
    /* if (!vstate->is_file) { */
    /*   vstate->inner->close(vstate->inner); */
    /* } */

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Destroy()
//   - R will destroy the Rbonn struct (?)
//   - R will destroy the Rconnection object (?)
//   - Only really have to take care of 'rconn->private_ptr' (?)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void vfile_destroy(struct Rconn *rconn) {
    spdl::debug("[vfile_destroy] entered");

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;
    if (vstate->verbosity > 0) Rprintf("vfile_destroy()\n");
    delete vstate->ctx;
    delete vstate->vfs;

    free(vstate);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// get a character from the connection
// This doesn't seem to be called for use cases I've tried.
// @return int - a character, or R_EOF
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int vfile_fgetc_internal(struct Rconn *rconn) {
    spdl::debug("[vfile_fgetc_internal] entered");

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;
    if (vstate->verbosity > 0) Rprintf("vfile_fgetc_internal()\n");

    return rconn->fgetc(rconn);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// seek()
//   - vfile() will not support seeking
// @param double - offset to seek relative to origin, apparently
//        double is used here to avoid using
//        integer types, i.e. long int, which is
//        the prototype of the corresponding parameter
//        in fseek, as defined in stdio.h
// @param int - the origin of seeking, 1 (and any except 2 and
//        3) if relative to the beginning of the
//        connection, 2 if relative to the current
//        connection read/write position, 3 if relative to
//        the end of the connection
// @param int - currently only used by file_seek to select
//        the read or write position when the offset is NA
// @return  double - the read/write position of the connection before
//          seeking, negative on error double is again used to
//          avoid integer types
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
double vfile_seek(struct Rconn *rconn, double x, int y, int z) {
    Rf_error("vfile_seek() - not supported");
    return 0;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// truncate the connection at the current read/write position.
//   - vfile() will not support truncation
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void vfile_truncate(struct Rconn *rconn) {
    Rf_error("vfile_truncate() - not supported");
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// called when the connection should flush internal read/write buffers
//   - vfile will not suport flush()
//
// @return int zero on success. Non-zero otherwise
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int vfile_fflush(struct Rconn *rconn) {
    Rf_error("vfile_fflush() - not supported\n");
    return 1;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// readBin()
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
size_t vfile_read(void *dst, size_t size, size_t nitems, struct Rconn *rconn) {
    spdl::debug("[vfile_read] entered");

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;
    if (vstate->verbosity > 0) Rprintf("vfile_read(size = %zu, nitems = %zu)\n", size, nitems);
    spdl::debug(tfm::format("[vfile_read] reading from '%s' up to size '%zu' times '%zu'",
                            vstate->uri, size, nitems));

#if 0
    if (vstate->is_file && feof(vstate->fp)) {
        return 0;
    }
    /* if (!vstate->is_file && vstate->inner->EOF_signalled) { */
    /*   return 0; */
    /* } */

    /* size_t n = size * nitems; */

    size_t nread;
    if (vstate->is_file) {
        nread = fread(dst, size, nitems, vstate->fp);
    } else {
        /* nread = R_ReadConnection(vstate->inner, dst, n); */
        /* if (nread != n) { */
        /*   vstate->inner->EOF_signalled = TRUE; */
        /* } */
    }
    return nread;
#else
    tiledb::VFS::filebuf sbuf(*vstate->vfs);
    sbuf.open(vstate->uri, std::ios::in);
    std::istream is(&sbuf);
    if (!is.good()) {
        Rcpp::stop("Error opening uri '%s' for reads\n", vstate->uri);
    }
    size_t file_size = static_cast<size_t>(vstate->vfs->file_size(vstate->uri));
    auto nread = std::min(size * nitems, file_size);
    is.read((char*)dst, nread);
    sbuf.close();
    return nread;
#endif
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// readLines()
//   - fgetc() called until '\n'. this counts as 1 line.
//   - when EOF reached, return -1
//
// get a (re-encoded) character from the connection
// @return int - a (re-encoded) character, or R_EOF
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int vfile_fgetc(struct Rconn *rconn) {
    spdl::debug("[vfile_fgetc] entered");

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;
    if (vstate->verbosity > 1) Rprintf("vfile_fgetc()\n");

    int c;

#if 0
    if (vstate->is_file) {
        c = fgetc(vstate->fp);
    } else {
        char cchar;
        size_t nread = rconn->read(&cchar, 1, 1, rconn);
        if (nread == 0) {
            c = -1;
        } else {
            c = (int)cchar;
        }
    }
#endif

    if (vstate->nread == vstate->buf.size()) {
        c = -1;
    } else {
        c = static_cast<int>(vstate->buf[vstate->nread++]);
    }

    return c;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// writeBin()
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
size_t vfile_write(const void *src, size_t size, size_t nitems, struct Rconn *rconn) {
    spdl::debug("[vfile_write] entered");

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;
    if (vstate->verbosity > 0) Rprintf("vfile_write(size = %zu, nitems = %zu)\n", size, nitems);

    size_t wlen = 0;
#if 0
    if (vstate->is_file) {
        wlen = fwrite(src, 1, size * nitems, vstate->fp);
    /* } else { */
        /* wlen = R_WriteConnection(vstate->inner, (void *)src, size * nitems); */
    }
#else
    tiledb::VFS::filebuf sbuf(*vstate->vfs);
    sbuf.open(vstate->uri, std::ios::out);
    std::ostream os(&sbuf);
    if (!os.good()) {
        Rcpp::stop("Error opening uri '%s' for writes\n", vstate->uri);
    }
    wlen = size * nitems;
    os.write((char*)src, wlen);
    os.flush();
    sbuf.close();
#endif
    return wlen;
}


#define BUFSIZE 32768

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Used in writeLines
// @return int - number of characters printed, negative on failure
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int vfile_vfprintf(struct Rconn *rconn, const char* fmt, va_list ap) {
    spdl::debug("[vfile_vfprintf] entered");

    vfile_state *vstate = (vfile_state *)rconn->private_ptr;

    unsigned char str_buf[BUFSIZE + 1];

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // vsnprintf() return value:
    //   The number of characters written if successful or negative value if an
    //   error occurred. If the resulting string gets truncated due to buf_size
    //   limit, function returns the total number of characters (not including the
    //   terminating null-byte) which would have been written, if the limit
    //   was not imposed.
    //
    // So when vsnprintf() overflows the given size, it returns the number of
    // characters it couldn't write.  Tell it the buffer size is '0' and it
    // will just return how long a buffer would be needed to contain the string!
    //
    // Note: need to copy the 'va_list', since you can't (officially) use it twice!
    // ubuntu platform complains
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    va_list apc;
    va_copy(apc, ap);
    // n = vsnprintf(p, size, fmt, apc);
    int slen = vsnprintf((char *)(str_buf), 0, fmt, apc);
    va_end(apc);

    int wlen = slen;
    if (wlen > BUFSIZE) {
        Rf_warning("vfile_vfprintf(): Long string truncated to length = %i\n", BUFSIZE);
        wlen = BUFSIZE;
    }


    slen = vsnprintf((char *)(str_buf), BUFSIZE, fmt, ap);
    if (slen < 0) {
        Rf_error("vfile_vfprintf(): error in 'vsnprintf()");
    }

    unsigned char display_buf[40+1];
    strncpy((char *)display_buf, (char *)str_buf, 40);
    display_buf[40] = '\0';
    if (vstate->verbosity > 0) Rprintf("vfile_vfprintf('%s ...')\n", display_buf);

#if 0
    if (vstate->is_file) {
        fwrite(str_buf, 1, wlen, vstate->fp);
    } else {
        /* R_WriteConnection(vstate->inner, str_buf, wlen);   */
    }
#else
    tiledb::VFS::filebuf sbuf(*vstate->vfs);
    sbuf.open(vstate->uri, std::ios::app);
    std::ostream os(&sbuf);
    if (!os.good()) {
        Rcpp::stop("Error opening uri '%s' for writes\n", vstate->uri);
    }
    os.write((char*)str_buf, wlen);
    os.flush();
    sbuf.close();
#endif
    return wlen;
}

}

// Setup initialization

SEXP new_connection_xptr;

// [[Rcpp::export]]
void tldb_init_(SEXP nc_xptr) {
    new_connection_xptr = nc_xptr;
    R_PreserveObject(nc_xptr);
}

SEXP new_connection(const char* description, const char* mode,
                    const char* class_name, Rconnection* ptr) {
    auto new_connection_ptr =
        reinterpret_cast<SEXP (*)(const char*, const char*,
                                  const char*, Rconnection*)>(R_ExternalPtrAddr(new_connection_xptr));
    return new_connection_ptr(description, mode, class_name, ptr);
}
