# Ongoing

## Improvements

* The older implementations `tiledb_dense` and `tiledb_sparse` are now marked as deprecated in favor of `tiledb_array`. No removal date is set or planned yet, but it is recommended to migrate new code. (#180)

* Updated the underlying TileDB library to use TileDB 2.1.2 on macOS and Linux (when no system library is found) (#181)

* There is extended support for array creation directly from DataFrame objects. (#182)

* Internal TileDB performance statistics can now be exported 'raw' in JSON format (for TileDB versions greater than 2.0.3). (#183, #186)

* The vignette was updated with respect to the preferred used of `tiledb_array`. (#184)

* The Hilbert cell layout added recently to TileDB Embbeded is supported. (#185)

* Virtual File System functions now use the default VFS object which allows for a simpler interface (#187)

* TileDB Array Dimension support has been extended to more data types (#188)

* Datetime support for Dimensions and Attributes has been extended (#189)

* The API coverage has been increased alongside an update of the documentation (#190)

# 0.8.2

* This release of the R package builds against [TileDB 2.1.1](https://github.com/TileDB-Inc/TileDB/releases/tag/2.1.1), but has also been tested against previous releases as well as the development version.

## Bug Fixes

* The `tiledb_stats_reset()` function is now exported, and `tiledb_stats_print()` has been re-added as a wrapper to `tiledb_stats_dump()` (#174)

* Configuration options for compute and input/output concurrency set only the new TileDB 2.1 configuration options; documentation on how to checking values has been expanded. (#175)

* The `download.file()` use now (re-)sets the timeout to the standard value to accomodate uses where a lower value may be set such as some CRAN builders (#176)

* Build scripts have been updated for use of TileDB 2.1.1 on Windows, macOS and Linux (when no system library is found) (#178)


# 0.8.1

* This release of the R package supports [TileDB 2.1.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.1.0), but has also been tested against the previous release [TileDB 2.0.8](https://github.com/TileDB-Inc/TileDB/releases/tag/2.0.8).

## Improvements

* R-based metadata accessors have been extended to also support `tiledb_array` arrays (#169)

* `configure` now also checks the hardware platform before attempting a download of a prebuilt library (#170)

* `SystemRequirements:` in the `DESCRIPTION` file has been expanded (#170)

## Bug Fixes

* A typo in the manual page source, copied three more times, has been corrected (#167)


# 0.8.0

* This release of the R package supports [TileDB 2.0.8](https://github.com/TileDB-Inc/TileDB/releases/tag/2.0.8), but has
  also been tested against the previous release [TileDB 1.7.7](https://github.com/TileDB-Inc/TileDB/releases/tag/1.7.7).

## Improvements

- A new function `limitTileDBCores()` controls resource use, it is being used in tests (#139)

- The function `tiledb_get_context()` is now exported (#140)

- A new S4 class `tiledb_vfs` provides access to the virtual file system functionality (#140)

- Functionality of `selected_ranges()` was extended (#142)

- More (signed and unsigned) integer types are supported as dimension types in sparse arrays (#143), as well as in dense arrays (#144) and as attributes (#144)

- A new S4 class `tiledb_query` offers access to the query object functionality in the underlying library (#145, #161)

- Examples are running with a lowered thread count setting per CRAN Policies (#152)

- External pointer objects now use explicitly set finalizers (#149)

- Users can explicitly select a TileDB Embedded shared library built to be used (#151)

- Compile-time configuration was refactored and changes (#158)

- Windows builds are now possible also using TileDB Embedded build 2.0.8 (#159, #164)

- Continuous integration now uses Azure for macOS and Linux (#160) and GitHub Actions for Windows (#162,#165)

## Bug Fixes

- Conda builds no longer call `install_name_tool` (#133, #146)

- Downloading the prebuild library accomodates multitple targets per architecture (#150)

- The number of TBB threads will only be set once (#158)


# 0.7.1

* This release of the R package supports [TileDB 1.7.7](https://github.com/TileDB-Inc/TileDB/releases/tag/1.7.7) and [TileDB 2.0.5](https://github.com/TileDB-Inc/TileDB/releases/tag/2.0.5)

## Improvements

- Range selection for tiledb_array objects can get/set matrices defining range (#132)

- The `show` methods are now consistently exported and documented (#134)

- TileDB is listed as copyright owner in DESCRIPTION as well (#134)

- The `selected_ranges` method for `tiledb_array` types was improved, and more tests were added (#135)

- C++ source code was rearranged slightly with respect to possible API deprecations in the libary (#136)

- A very simple example for using TileDB Cloud from R was added (#136)

- The helper scripts for the package build are now in the `tools/` directory (#137)

- The (optional) library download now relied on suggested R packages 'jsonlite' and 'curl' (#137)

## Bug Fixes

- A character conversion when retrieving array metadata resulting in an out-of-bounds reads has been corrected (#137)


# 0.7.0

* This release of the R package supports [TileDB 1.7.7](https://github.com/TileDB-Inc/TileDB/releases/tag/1.7.7) and [TileDB 2.0.5](https://github.com/TileDB-Inc/TileDB/releases/tag/2.0.5)

## Improvements

- All S4 classes are now consistently documented or aliased (#117)

- If needed, the build system now builds TileDB and its required component (#118)

- Data.frame support has been extended further and made more robust (#119, #123, #128)

- The Description: in `DESCRIPTION` has been refreshed (#120)

- Builds on Linux and macOS can use a pre-built TileDB library (#121, #122, #124, #127)

- Copyright headers were added to source files (#125)

- The pkg-config helper can be used when building from source (#126)

- An introductory vignette was added (#129, #131)


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
