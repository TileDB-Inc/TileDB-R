#include <tiledb/tiledb>

#ifndef __tiledb_version_h__
#define __tiledb_version_h__

// create a single 'comparable' number out of version, minor and patch
#define TileDB_Version(v,m,p)	(((v) * 65536) + ((m) * 256) + (p))

// current build is encoded in TILEDB_VERSION
#define TILEDB_VERSION TileDB_Version(TILEDB_VERSION_MAJOR,TILEDB_VERSION_MINOR,TILEDB_VERSION_PATCH)

#endif
