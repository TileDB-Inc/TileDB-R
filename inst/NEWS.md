# Ongoing

* This release of the R package supports [TileDB 1.7.7](https://github.com/TileDB-Inc/TileDB/releases/tag/1.7.7) and [TileDB 2.0.2](https://github.com/TileDB-Inc/TileDB/releases/tag/2.0.2)

## Improvements

- All S4 classes are now consistently documented or aliased (#117)

- If needed, the build system now builds TileDB and its required component (#118)

- Data.frame support has been extended further and made more robust (#119, #123)

- The Description: in `DESCRIPTION` has been refreshed (#120)

- Builds on Linux and macOS can use a pre-built TileDB library (#121, #122, #124)

# 0.6.0

* This release of the R package supports [TileDB 1.7.7](https://github.com/TileDB-Inc/TileDB/releases/tag/1.7.7) and [TileDB 2.0.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.0.0)

## Improvements

- Added support for heterogenous domains

- Added support for string dimensions

- Added support for duplicate dimension values in sparse arrays

- Added support for data.frame object import and conversion to dense and
  sparse arrays

- Added enhanced support for data.frame returns from dense array

- Added support for data.frame column selection (i.e. attributes) from dense array

- Added support for new filter types for md5 and sha256 checksums

- Added support for Date, Datetime (i.e. POSIXct) and nanosecond dense and
  sparse array attributes and domains

- Documentation and examples were enhanced and extended

- `tiledb_stats_dump()` is now simpler (but needs to be enabled first as
  usual)

- Support for nanosecond and integer64 columns was added, this is an optional
  feature for which the nanotime (and bit64) packages need to be installed

## Changes

- Dimension attributes must now be named


# 0.5.0

- This release of the R package builds against the 1.7.5 releases of TileDB.

## Improvements

- Added support for i) multi-range subarrays, ii) incomplete queries,
  iii) result size estimation and 'time travel' at to time-points has been
  added [#105](https://github.com/TileDB-Inc/TileDB-R/pull/105)

- Added additional support for metadata [#106](https://github.com/TileDB-Inc/TileDB-R/pull/105)


# 0.4.0

* This release of the R package builds against the 1.7.* releases of tiledb.

## Improvements

- This release contains increased coverage of the underlying API, additional
  documentation as well as unit tests.
