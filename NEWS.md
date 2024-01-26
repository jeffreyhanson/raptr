# raptr 1.0.1

- Fix usage sections in documentation.
- Fix missing braces in documentation.
- Fix aliasing for package manual entry (#15).

# raptr 1.0.0

- The package has been overhauled to support modern spatial data structures.
  Specifically, _sf_ package objects (`sf::st_sf()`) are now use for vector
  data and _terra_  package objects (`terra::rast()`) for raster data.
  These updates impact many of the user-facing functions (e.g.,
  `calcBoundaryData()`, `sim.species()`, `sim.space()`,
  `calcSpeciesAverageInPus()`, `rap()`), so users will likely need to update
  their code.
- The `cs_pus`, `cs_spp`, and `cs_spaces` built-in datasets have been removed.
  This is because `terra::rast()` objects cannot be distributed as built-in
  datasets for packages. These datasets can now be manually imported from
  external files distributed with the package (see `?cs_pus` for more details).
- Remove _rgdal_ and _rgeos_ packages as dependencies (#13).
- Support for the _raster_ package has been deprecated.
- The `SpatialPolygons2PolySet` function has been renamed to `convert2PolySet`.
- Update citation format.
- Update package startup message.
- Update C++ specification to C++14.
- Update examples and unit tests.

# raptr 0.2.2.0

- Add `NumericFocus` parameter to `GurobiOpts` to handle numerical issues.
- Update `spacePlot` to avoid throwing a _ggplot2_ deprecation warning.

# raptr 0.2.1

- CRAN release.
- Update Gurobi documentation URLs.

# raptr 0.2.0.0

- The data simulation function has been updated such that the _RandomFields_
   package is no longer a dependency.
- The `sim.space` and `sim.species` functions no longer accept _RandomFileds_
  objects as arguments. To simulate spatially auto-correlated data using
  random fields, a `numeric` value should be supplied to the `model` parameter.
- Small tweaks to case study in vignette.

# raptr 0.1.8.0

- The _RandomFields_ and _RgoogleMaps_ R packages are now optional
  dependencies.
- Minimum supported versions are now specified for most dependencies.
- The `verbose` parameter of `solve()` can now be used to suppress
  output from the Gurobi solver.
- Fix compatibility issues with updates to the _ggplot2_ package.
- Fix note during package checks related to the _rgdal_ package.

# raptr 0.1.7.3

- Update example datasets and `make.RapData` function to improve compatibility
  with updates to coordinate reference system (`sp::CRS`) objects. These
  updates simply mean that warning messages will no longer be displayed.
  The compatibility issues that resulted in warnings did not affect the
  correctness of any results.

# raptr 0.1.7.2

- Fix compatibility issues between the _testthat_ R package and the _gurobi_ R
  package in package tests.

# raptr 0.1.7.1

- Implement GitHub Actions continuous integration (i.e. update tests
  and README).

# raptr 0.1.7

- Remove support for processing data using GDAL.
- Remove `is.gdalInstalled` and `rasterizeGDAL` functions.
- Update Spatial-class objects with updated `sp::CRS()` class definition

# raptr 0.1.6.1

- Fix "Non-file package-anchored link(s) in documentation object" warnings in
  R-devel checks.

# raptr 0.1.6

- Fix errors in R-devel CRAN checks due changes in `is.finite` behaviour for
  `character` class objects.
- Fix WorldClim URLs.

# raptr 0.1.5

- Fix warnings in R-devel CRAN checks related to documentation.
- Fix broken link in vignette.

# raptr 0.1.4

- Retain debugging symbols to conform with CRAN policies.

# raptr 0.1.3

- Fix bug that caused the R session to crash when none of the planning
  units associated with an attribute space were selected in a manually
  specified solution. Now the space-held calculations will return
  a negative infinity value in such cases.
- Permit attribute spaces to have a single demand point.

# raptr 0.1.2

- Fix compatibility issues with demand point examples and _hypervolume_ R
  package (version 2.0.10).
- Update package citation.
- Removed _rgurobi_ R package dependency because the _gurobi_ R package
  (version 8.0.0+) provides the functionality to access solutions from the
  solution pool.
- Multiple solutions can now be generated using the three different search pool
  methods provided Gurobi.
- Permit a zero MIPGAP in argument to `GurobiOpts`.
- The `print.RapResults` function now prints information in a prettier manner.
- The `raptr::solve` function now throws a warning if some species are poorly
  represented in the solution.

# raptr 0.1.1

- Vignette size has been reduced.
- Updated DOI in README.

# raptr 0.1.0

- Unit tests now compatible with _testthat R_ package (version 1.0.2.9000).
- The _assertthat R_ package is now used for validating function arguments.
- Vignette now uses `knitr::rmarkdown_notangle` engine to avoid running the
  code during package checks, and placeholder vignette file has been removed.
- Code has been linted.
- Functions from other packages are now called explicitly using `pkg::function`.
- Messages are now displayed using `message` following best practices.
- Updated readme.
- Added citation for _Methods in Ecology and Evolution_ paper.
- Strip debugging symbols from compiled files to reduce installation size.
- The `is.GurobiInstalled` now does not run _gurobi_ to avoid spurious notes
  during CRAN checks on academic license.

# raptr 0.0.5

- Fix documentation in `make.DemandPoints`.
- Use `\donttest` instead of `\dontrun` for examples.
- Fix broken example code.

# raptr 0.0.4

- Find out which suggested packages are installed using `requireNamespace`.
- Add more links to package documentation.
- Register Rcpp functions.

# raptr 0.0.3

- Reduce memory consumption using external pointers.
- Fix memory leaks.
- Fix bug in tests for `calcSpeciesAverageInPus`.

# raptr 0.0.2

- Reduce installation size.
- Fix typo in README.

# raptr 0.0.1.1

- Fixed authorship typo.
- Updated README with instructions for installing from CRAN.

# raptr 0.0.1

- Initial package version.
