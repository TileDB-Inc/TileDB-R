# tiledb 0.9.4

* This release of the R package builds against [TileDB 2.3.1](https://github.com/TileDB-Inc/TileDB/releases/tag/2.3.1), but has also been tested against previous releases and the development version.

## Improvements

* The build defaults to TileDB Embedded 2.3.* (unless another version is found during build, or explicitly selected) (#258, #264)

* Query condition support is available for TileDB 2.3.0 or later, allowing (possibly multiple) numerical constraints on attributes (#261)

* Multi-dimensional arrays can now be returned from (dense) arrays via a new option (#263)

* The package is now natively supported on Arm64 ("M1") macOS system (#264)

## Bug Fixes

* Dense arrays with more than two dimensions can now be written (#260)


# tiledb 0.9.3

* This release of the R package builds against [TileDB 2.2.9](https://github.com/TileDB-Inc/TileDB/releases/tag/2.2.9), but has also been tested against previous releases as well as the development version.

## Improvements

* Continuous integration at GitHub is now faster as suggested packages are no longer installed (#250)

* Arrays can now be written incrementally via the higher-level replacement function `arr[] <- obj` (#251)

* The default column layout for arrays written via `fromDataFrame` is now column-order (#254)

## Bug Fixes

* The call to vaccum not correctly calls the library function to vaccum instead of the consolidation function (#252)

* When several columns are selected via `selected_ranges`, a potentially necessary reordering is done for a query (#253)

* Dense subarrays can be written for `tiledb_array` (#256)


# tiledb 0.9.2

* This release of the R package builds against [TileDB 2.2.9](https://github.com/TileDB-Inc/TileDB/releases/tag/2.2.9), but has also been tested against previous releases as well as the development version.

## Improvements

* Matrix objects can now be returned under range selections (#247)

* Matrix return get turned on and off with setter / getter functions (#248)

## Bug Fixes

* Unit tests of character columns in data frames accomodate R versions prior to R 4.0.0 in all cases (#243)

* Dimension reduction for attribute-selected columns was incorrect in some cases (#245)

* Attribute-selected columns were using incorrect dimension data types in some cases (#246)


# tiledb 0.9.1

* This release of the R package builds against [TileDB 2.2.9](https://github.com/TileDB-Inc/TileDB/releases/tag/2.2.9), but has also been tested against previous releases as well as the development version.

## Improvements

* A new vignette show use of TileDB array via RMariaDB and the MyTile extension to MariaDB (#221)

* Matrices can now be returned directly from suitable two-dimensional TileDB arrays (#225)

* More data types are supported in the non-empty domain accessor function (#229)

* The DESCRIPTION, README.md and pkgdown site were updated (#230)

* Creation of TileDB arrays from data.frame object has been made more robust (#238)

* On startup, versions numbers of the R package and the TileDB Embedded library are displayed (#239)

* The pkgdown website now shows the ChangeLog derived from this NEWS file (#240)

## Bug Fixes

* Two tests with datetime comparisons which fail only on one macOS system are now conditional (#216)

* Result sets with all-character column now fall back to estimated result sizes (#217)

* Setup of support for duplicate values in sparse arrays has been corrected (#223)

* Error messages concerning an array types and selection mismatch are now clearer (#224)

* Writes from data.frame objects to dense array revert back to column-major order (#226)

* Tests of sparse writes to dense matrices now use UNORDERED layout (#228)

* Data.frame returns of selected columns now coversion dimensions as well (#231)

* Schema creation has been generalized and made more robust (#232)

* Selection of dimension ranges now maps date and datetime values correctly (#233, #241)

* Selection and setting of dimension ranges has been generalized and made more robust (#235, #236)


# tiledb 0.9.0

* This release of the R package builds against [TileDB 2.2.4](https://github.com/TileDB-Inc/TileDB/releases/tag/2.2.4), but has also been tested against two previous release series as well as the development version.

## Improvements

* The older implementations `tiledb_dense` and `tiledb_sparse` are now marked as deprecated in favor of `tiledb_array`. No removal date is set or planned yet, but it is recommended to migrate to new code. (#180)

* Updated the underlying TileDB library to use TileDB 2.1.2 on macOS and Linux (when no system library is found) (#181)

* There is extended support for array creation directly from DataFrame objects. (#182)

* Internal TileDB performance statistics can now be exported 'raw' in JSON format (for TileDB versions greater than 2.0.3). (#183, #186)

* The vignette was updated with respect to the preferred used of `tiledb_array`. (#184)

* The Hilbert cell layout added recently to TileDB Embbeded is supported. (#185)

* Virtual File System functions now use the default VFS object which allows for a simpler interface (#187)

* TileDB Array Dimension support has been extended to more data types (#188)

* Datetime support for Dimensions and Attributes has been extended (#189)

* The API coverage has been increased alongside an update of the documentation (#190)

* The Array schema can now be accessed and printed directly from a URI (#191)

* The accessor for the estimated array result size was updated with the updaded API (#192)

* Initial Arrow support for importing and exporting vector has been added (#193, #199)

* Support for the default TileDB Embedded library was set to 2.1.3 and 2.1.4 (#194, #195)

* The package documentation was updated and extended (#196)

* Support for 'time-traveling' access to arrays was extended (#197)

* Support for the default TileDB Embedded library was set to 2.1.5 and 2.1.6 (#198, #200)

* Support for the default TileDB Embedded library was set to 2.2.0 and 2.2.1 (#201, #202)

* The vignettes were updated and extended (#203)

* The `fromDataFrame()` function was extended (#204)

* Some unit tests were conditioned on having TileDB Embedded 2.1.0 or later (#205)

* Support for the default TileDB Embedded library was set to 2.2.2 and 2.2.3 (#206, #208)

* The default TileDB Embedded library version is now set centrally (#207)

* Support was added to write and read sparse matrices directly via higher-level functions (#209)

* Arrow array and schema allocation and deallocation helper functions were added (#210)

* Support for Nullable vectors has been added (#211)

* Support for the default TileDB Embedded library has been set to 2.2.4 (#212)

* Small tweaks to timezone and factor settings in unit tests, and vignette (#213, #214, #215)


# tiledb 0.8.2

* This release of the R package builds against [TileDB 2.1.1](https://github.com/TileDB-Inc/TileDB/releases/tag/2.1.1), but has also been tested against previous releases as well as the development version.

## Bug Fixes

* The `tiledb_stats_reset()` function is now exported, and `tiledb_stats_print()` has been re-added as a wrapper to `tiledb_stats_dump()` (#174)

* Configuration options for compute and input/output concurrency set only the new TileDB 2.1 configuration options; documentation on how to checking values has been expanded. (#175)

* The `download.file()` use now (re-)sets the timeout to the standard value to accomodate uses where a lower value may be set such as some CRAN builders (#176)

* Build scripts have been updated for use of TileDB 2.1.1 on Windows, macOS and Linux (when no system library is found) (#178)


# tiledb 0.8.1

* This release of the R package supports [TileDB 2.1.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.1.0), but has also been tested against the previous release [TileDB 2.0.8](https://github.com/TileDB-Inc/TileDB/releases/tag/2.0.8).

## Improvements

* R-based metadata accessors have been extended to also support `tiledb_array` arrays (#169)

* `configure` now also checks the hardware platform before attempting a download of a prebuilt library (#170)

* `SystemRequirements:` in the `DESCRIPTION` file has been expanded (#170)

## Bug Fixes

* A typo in the manual page source, copied three more times, has been corrected (#167)


# tiledb 0.8.0

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


# tiledb 0.7.1

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


# tiledb 0.7.0

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


# tiledb 0.6.0

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


# tiledb 0.5.0

- This release of the R package builds against the 1.7.5 releases of TileDB.

## Improvements

- Added support for i) multi-range subarrays, ii) incomplete queries,
  iii) result size estimation and 'time travel' at to time-points has been
  added [#105](https://github.com/TileDB-Inc/TileDB-R/pull/105)

- Added additional support for metadata [#106](https://github.com/TileDB-Inc/TileDB-R/pull/105)


# tiledb 0.4.0

* This release of the R package builds against the 1.7.* releases of tiledb.

## Improvements

- This release contains increased coverage of the underlying API, additional
  documentation as well as unit tests.
