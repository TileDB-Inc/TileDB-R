# Ongoing Development

## Improvements

## Bug Fixes

## Build and Test Systems

* The nighhly valgrind job setup was updated to include two new dependencies (#493)


# tiledb 0.17.0

* This release of the R package builds against [TileDB 2.13.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.13.0), and has also been tested against earlier releases as well as the development version (#492).

## Improvements

* Support for testing group URIs on being relative has been added (#478)

* Logging support at the R and C++ level has been added (#479, #487, #489)

* Use of TileDB Embedded was upgraded to release 2.12.1, and 2.12.2 (#480, #481)

* Sparse array queries via tiledb_array and '[]' access use an UNORDERED query layout (#488)

* Use of TileDB Embedded was upgraded to release 2.13.0 (#490)

* Support for selecting dimensions by discrete points has been added (#491)

## Bug Fixes

* Accomodate possible zero sized allocation estimates for attributes (#482)

* Detect missing columns in a write-attempt with partial data (#483)

## Build and Test Systems

* Update check-out action to version three suppressing a warning (#477)

* Code Coverage reports are now generated and available at codecov.io (#484)

* Small internal changes renaming two files and conditioning tests under two older releases (#485)


# tiledb 0.16.0

* This release of the R package builds against [TileDB 2.12.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.12.0), and has also been tested against earlier releases as well as the development version (#476).

## Improvements

* Several deprecated API entry points of TileDB Embedded are no longer used (#452, #453)

* Support for DELETE queries has been added (requires TileDB Embedded 2.12.0 or later) (#455, #456)

* Use of TileDB Embedded was upgraded to release 2.11.1, 2.11.2, and 2.11.3 (#460, #466, #474)

* Support for XOR filters has been added (#472)

* Support for deletion of fragments has been added (#473)

* Use of TileDB Embedded was upgraded to release 2.12.0 (#475)

## Bug Fixes

* Treatment of character columns with missing values has been corrected (#454)

* Accessing encrypted arrays has been reverted to the older API accessors (#458)

* Int64 domain values in excess of int range are now expressed as integer64 objects (#465)

## Build and Test Systems

* Sparse matrix conversion used mainly in tests have been updated for version 1.4-2 of the Matrix packages (#457)

* Support builds on the riskv64 platform by adding a missing link instruction (#459)

* The test setup was tweaked to not trigger a spurious valgrind report from libcrypto (#461)

* The test setup was tweaked to make a group comparison more resilient to ordering (#462)

* The test setup was refined for two filter tests (#467, #468)

* A parameterized test for the SCALE_FLOAT filter has been added (#469)

* The test setup ensures that the per-session directory remains accessible (#470)

* Continuous integration testing for Linux and macOS has been moved to GitHub Actions (#471)


# tiledb 0.15.0

* This release of the R package builds against [TileDB 2.11.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.11.0), and has also been tested against earlier releases as well as the development version (#451).

## Improvements

* Support for query conditions has been extended to dense arrays (#447)

* Support for filter lists has extended to both the data.frame helper and the dimension object constructor (#448)

* Use of TileDB Embedded was upgraded to release 2.11.0 (#449)

## Bug Fixes

* Small enhancements have been made to the test suite (#450)

## Build and Test Systems

* A small enhancement was made to the test system (#450)


# tiledb 0.14.1

* This release of the R package builds against [TileDB 2.10.2](https://github.com/TileDB-Inc/TileDB/releases/tag/2.10.2), and has also been tested against earlier releases as well as the development version.

## Improvements

* Use of TileDB Embedded was upgraded to release 2.10.2 (#443) following an earlier update to 2.10.1 (#434)

* List columns are now supported in reading and writing of data frames by extending cell variable numbers beyond one (#438, #440)

* Query condition support has been extended to more data types (#441)

* The 'SCALE_FLOAT' filter for compression of floating-point attributes is now supported (with TileDB 2.11 or later) (#445)

## Bug Fixes

* Unit tests were refined with some additional conditioning on envuironment variable `CI` being present (#436)

* An unnessary final argument has been dropped from a 'remove member' method (#437)

## Build and Test Systems

* The nightly `valgrind` check was updated to Ubuntu 22.04 (#435, #439, #442)


# tiledb 0.14.0

* This release of the R package builds against [TileDB 2.10.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.10.0), and has also been tested against earlier releases as well as the development version.

## Improvements

* Use of TileDB Embedded was upgraded to release 2.10.0 (#432) following earlier updates to 2.9.1 (#415), 2.9.2 (#419), 2.9.3 (#422), 2.9.4 (#427) and 2.9.5 (#430)

* The BOOL data type is now supported (#416)

* Query conditions support was extended with support for an OR operator (#417)

* An incomplete query result is now signaled via a warning message (#420)

* A helper function was added to check if an Array is open (#421)

* Batched queries are now supported given the user the possibility to process larger-than-memory result sets in parts (#429)

* Some internal object creation code was refactored (#431)

## Bug Fixes

* The `attr` setter for Attributes was corrected to support NA settings (#425)

## Build and Test Systems

* Filter compression tests are skipped on systems lacking AVX2 support (#418)

* The build system now checks for C++17 support (#424)

* The valgrind test was upgraded to release 2.9.3 and the 2.10 release branch (#426)

* Tests for overlapping ranges have been added (#428)


# tiledb 0.13.0

* This release of the R package builds against [TileDB 2.9.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.9.0), and has also been tested against earlier releases as well as the development version.

## Improvements

* Support for groups has been added for TileDB 2.8 or later (#404)

* The group member name retrieval can now also return the optional group member name (#399)

* Allocation and creation of large string vector buffers was refactored (#400)

* Support for dictionary encoding compression filters has been added for TileDB 2.9 or later (#404)

* Support for Filestore functionality has been added for TileDB 2.9 or later (#410)

* Support for BLOB datatypes has been added for TileDB 2.7 or later (#411)

* Use of TileDB Embedded was upgraded to release 2.9.0 (#413) following earlier updates to 2.8.1 (#401), 2.8.2 (#403), 2.8.3 (#408)

## Bug Fixes

* Tests for filters have been made more robust (#407, #412)

## Deprecations

* The `check()` function is now deprecated and `schema_check()` is provided (#409)

## Build and Test Systems

* Nightly valgrind checks were updated to use current versions (#397, #402)

* Following release of R 4.2.0, only ucrt builds are supported on Windows (#405)


# tiledb 0.12.0

* This release of the R package builds against [TileDB 2.8.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.8.0), and has also been tested against earlier releases as well as the development version.

## Improvements

* A schedule nightly continuous action now checks current and release-candidate branches of TileDB with the R package under valgrind (#387)

* Support for Groups was added (#388, #392, #395)

* All external pointers are now tagged and validated at compile- and run-time (#389)

* A now-redundant group-creation method has been removed (#391)

* Unit tests for group member addition were added and updated (#393)

* Group members can also be added or removed by name (#395)

* Use of TileDB Embedded was upgraded to release 2.8.0 (#396) following an earlier upgrades to 2.7.0 (#372) and 2.7.1 (#384)

## Bug Fixes

* The detection of TileDB headers and library is now more robust for cases where `pkg-config` is present but does not know about TileDB (#385)

* The package documentation website was updated (#386)

* A fallback was added for external pointer creation to support compilation without group support in TileDB Embedded (#390)

* An incorrectly specified function call was corrected (#392)

* The templated initialization for external pointer is now inlined to satisfy all compilers (#394)


# tiledb 0.11.1

* This release of the R package builds against [TileDB 2.6.4](https://github.com/TileDB-Inc/TileDB/releases/tag/2.6.4), but has also been tested against earlier releases, and the development version.

## Improvements

* Use of TileDB Embedded was upgraded to release 2.6.4 (#384) following an earlier upgrade to 2.6.2 (#359)

* Creations of arrays from `data.frame` objects now supports a `mode=` argument with values 'ingest', 'schema_only', and 'append' (#360)

* Some unit test and continuous integration code was refactored (#364, #375)

* Finalizer use is now simplified taking advantage of an [Rcpp](https://cran.r-project.org/package=Rcpp) change (#366)

* A new option `strings\_as\_factors` was added for `data.frame` retrieval (#367)

* The [arrow](https://cran.r-project.org/package=arrow) C-level interface now uses external pointer objects following Arrow 7.0 (#368)

* Support for memory limits has been extended, and partial reads are using with iterations to complete (#371)

* Fragment info reading now account for the `__fragments` object (#373)

* A nightly test under [valgrind](https://valgrind.org/) has been added; results are reported to slack (#382, #383)

* UTF-8 string in metadata are now supported (#377)

* Attribute-less arrays can now be created, written, and read (#378), also via higher-level accessors (#379)

* A plugin for [Rcpp](https://cran.r-project.org/package=Rcpp) has been added (#380)

## Bug Fixes

* Array status is now checked before closing (#362)

* Signed and unsigned `int64` dimensions are now mapped correctly from 'square-bracket indexing', and the third dimension is recognised (#365)

* Domain information could overflow `int64_t` if an unsigned value was used, this now flips to `double` (#370)

* Unit tests for consolidation and vacuuming were update to account for `__fragments` too (#374)

* A unit test was corrected to ensure logical expressions are of length one (#381)

## Documentation

* A new vignette on data ingestion has been added (#357)

* A new vignette on installation options has been added (#358)

* The vignettes are now built using package [simplermarkdown](https://cran.r-project.org/package=simplermarkdown) (#361)

* Help pages were polished (#369)

## Deprecations

* The `tiledb_dense` and `tiledb_sparse` functions which were deprecated in February 2021 have been removed after a twelve-month grace period.


# tiledb 0.11.0

* This release of the R package builds against [TileDB 2.6.1](https://github.com/TileDB-Inc/TileDB/releases/tag/2.6.1), but has also been tested against previous releases, and the development version.

## Improvements

* Use of TileDB Embedded was upgraded to release 2.6.1 (#354) following an earlier upgrade to 2.6.0 (#340)

* A cell value getter for dimension was added (#341)

* Getter and setter functions for validity filter lists have been added (#349)

* Memory budget use has been refined via a configurable budget setting (#346, #350)

* A context getter function was added for query objects (#351)

* The schema display functionality was refactored and extended (#342, #343, #344, #345, #352, #355)

* Use of `TILEDB_CHAR` is deprecated in favor of `TILEDB_STRING_ASCII` (#353)

## Bug Fixes

* A `.nojekyll` file was added to prevent unnecessary GitHub Pages builds (#339)

* A getter for fill values is only called with TileDB 2.1.0 or later (#347)

* GitHub Actions on Windows no longer install `qpdf` which was never used (#348)


# tiledb 0.10.2

* This release of the R package builds against [TileDB 2.5.3](https://github.com/TileDB-Inc/TileDB/releases/tag/2.5.3), but has been tested against previous releases, and the development version.

## Improvements

* The `stopifnot()` assertions now use consistent error messages across all functions (#331)

* A helper function matching TileDB data types to R types is now exported (#336)

## Bug Fixes

* The boolean variable for 'nullable' is now set with a default value (#329)

* A test for accessing shared memory segements is now correctly checking for TileDB 2.6.0 (#332)


# tiledb 0.10.1

* This release of the R package builds against [TileDB 2.5.2](https://github.com/TileDB-Inc/TileDB/releases/tag/2.5.2), but has been tested against previous releases, and the development version.

## Improvements

* An accessor for the most-recent error message string has been added (#327)

## Bug Fixes

* On Linux, if a pre-made TileDB Embedded library is used, lack of AVX2 instructions is now detected and a suitable build is deployed (#328)


# tiledb 0.10.0

* This release of the R package builds against [TileDB 2.5.1](https://github.com/TileDB-Inc/TileDB/releases/tag/2.5.1), but has been tested against previous releases, and the development version.

## Improvements

* CI tests were expanded to also test refactored TileDB Embedded readers (#310), and now deactivated as this is now part of release 2.5.0 (#321)

* The minimal version of TileDB Embedded that can be used with the R package is now release 2.0.0 (#313)

* The package now compiles using the C++17 standard just like TileDB Embedded (#314)

* Shared-memory interprocess communication is used to accelerate operation for TileDB Cloud (#316)

* The long-deprecated `max_element_size` function has been removed from TileDB Embedded, and the R interface was updated accordingly (#317, #319)

* The `extended` toggle and field for `tiledb_array()` can now select dimension-less returns from sparse arrays (#318)

* Use of TileDB Embedded was upgraded to release 2.5.0 (#321) following earlier upgrades to 2.4.2 and 2.4.3 (#308, #312)

* A new quickstart example using the 'memory filesystem' was added, and one another example updated (#323)

## Bug Fixes

* Tests for time-traveling were refactored and now in a separate test file (#311)

* Read-queries no longer call `finalize()` required only on write-queries (#309)

* Some examples were updated with copy/paste corrections (#317)

* Single-column `data.frame` returns work via `drop=FALSE` where needed (#320)


# tiledb 0.9.7

* This release of the R package builds against [TileDB 2.4.0](https://github.com/TileDB-Inc/TileDB/releases/tag/2.4.0), but has been tested against previous releases and the development version.

## Improvements

* Accessors for context and query statistics were added (returning easily parseable JSON strings) (#293).

* Initial support for schema evolution was added to add or drop attributes (#294).

* Use of TileDB Embedded was upgraded to release 2.4.0 (#295)

* Windows builds under GitHub Actions now also include the newer UCRT variant (#296).

* The internal memory allocation has been switched to `set_{data,offset,validity}_buffer` functions (#297).

* A convenience accessor for schema information retrieving 'dimension' or 'attribute' status has been added (#299).

* The default array type `fromDataFrame` has been change to sparse to match some optimizations in TileDB Embedded, some unit tests have been updated accordingly (#300).

* TileDB arrays can now be queried in expression using pipes (for row-wise filtering and colunb-wise selection) (#301).

* When matrices as well as sparse matrices are written to arrays, optional row and column names are now supported as well (#303, #304).

* The configure script was update to the standards of autoconf 2.69 as requested by CRAN (#305).

## Bug Fixes

* Use of `set_{data,offset,validity}_buffer` is made conditional on TileDB 2.4.0 or later to continue builds on older versions (#298).

* Tests of piped expressions have been rewritten to be compatible with R versions earlier than 4.1.0 (#302).

* A dangling documentation link in README.md was corrected, and another removed (#306).


# tiledb 0.9.6

* This release of the R package builds against [TileDB 2.3.4](https://github.com/TileDB-Inc/TileDB/releases/tag/2.3.4), but has been tested against previous releases and the development version.

## Improvements

* When retrieving results via the `[` operator, incomplete queries generate a warning (#283)

* The interface to query element size of queries was extended (#282)

* If query ends as 'incomplete', a warning is now issue (#283)

* The status of the preceding query can now be accessed also when using a higher-level wrapper (#285)

* Fragment Information can be accessed via high-level accessor functions (#286)

* A preference for data type as which TileDB array data is returned can be set (#288)

* Continuous Integration will use increased test coverage by installing more optional package (#289)

* Use of TileDB Embedded was upgraded to release 2.3.4 (#290)

## Bug Fixes

* One cast statement was corrected so a warning is no longer triggered from `clang` (#281)

* Some added unit tests were not conditional on TileDB Embedded 2.2.* or later (#284)

* A time-comparison unit test did not properly respect timezones which was corrected (#287)


# tiledb 0.9.5

* This release of the R package builds against [TileDB 2.3.3](https://github.com/TileDB-Inc/TileDB/releases/tag/2.3.3), but has also been tested against previous releases and the development version.

## Improvements

* A query condition parser was added for use with standard (non-quoted) R expressions (#267)

* Windows UCRT builds at CRAN are now supported (#268)

* Use of TileDB Embedded was upgraded to release 2.3.2 (#270), and again to 2.3.3 (#280)

* The vacuum and consolidation helper functions now use time stamp support (#271)

* The time-travel array opening support was updated to start and end timestamps (#272)

* Tests for both vacuuming and consolidation 'time traveling' were added (#273)

* Nullable string string attribute support was improved (#274)

* Ascii columns attribute support was added (#276)

* The query parser heuristic was improved to cover ascii strings (#277)

* Array opening uses improvemed array state consideration skippingg re-openings for better performance (#279)

## Bug Fixes

* Domain size information gathering has been corrected for uint32, uint64, and int64 attribute domains (#266)

* Timesteps for time-traveling unit tests were adjusted to not trip up macOS continuous integration tests (#275)

* String array buffer size calculation was corrected also allowing for all-string arrays (#278)


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
