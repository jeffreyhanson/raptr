# raptr 0.1.7 (released)

- remove support for processing data using GDAL.
- remove `is.gdalInstalled` and `rasterizeGDAL` functions.
- update Spatial-class objects with updated sp::CRS class definition

# raptr 0.1.6.1

- fix "Non-file package-anchored link(s) in documentation object" warnings in
  R-devel checks.

# raptr 0.1.6 (released)

- fix errors in R-devel CRAN checks due changes in `is.finite` behaviour for
  `character` class objects.
- fix WorldClim URLs.

# raptr 0.1.5 (released)

- fix warnings in R-devel CRAN checks related to documentation.
- fix broken link in vignette.

# raptr 0.1.4 (released)

- retain debugging symbols to conform with CRAN policies.

# raptr 0.1.3 (released)

- fix bug that caused the R session to crash when none of the planning
  units associated with an attribute space were selected in a manually
  specified solution. Now the space-held calculations will return
  a negative infinity value in such cases.
- permit attribute spaces to have a single demand point.

# raptr 0.1.2 (released)

- fix compatibility issues with demand point examples and _hypervolume_ R
  package (version 2.0.10).
- update package citation.
- removed _rgurobi_ R package dependency because the _gurobi_ R package
  (version 8.0.0+) provides the functionality to access solutions from the
  solution pool.
- multiple solutions can now be generated using the three different search pool
  methods provided Gurobi.
- permit a zero MIPGAP in argument to `GurobiOpts`.
- the `print.RapResults` function now prints information in a prettier manner.
- the `raptr::solve` function now throws a warning if some species are poorly
  represented in the solution.

# raptr 0.1.1 (released)

- vignette size has been reduced.
- updated DOI in README.

# raptr 0.1.0

- unit tests now compatible with _testthat R_ package (version 1.0.2.9000).
- _assertthat R_ package is now used for validating function arguments.
- vignette now uses `knitr::rmarkdown_notangle` engine to avoid running the
  code during package checks, and placeholder vignette file has been removed.
- code has been linted.
- functions from other packages are now called explicitly using `pkg::function`.
- messages are now displayed using `message` following best practices.
- updated readme.
- added citation for _Methods in Ecology and Evolution_ paper.
- strip debugging symbols from compiled files to reduce installation size.
- `is.GurobiInstalled` now does not run _gurobi_ to avoid spurious notes during
  CRAN checks on academic license.

# raptr 0.0.5 (released)

- fix documentation in `make.DemandPoints`.
- `\donttest` instead of `\dontrun` for examples.
- fix broken example code.

# raptr 0.0.4

- find out which suggested packages are installed using `requireNamespace`.
- add more links to package documentation.
- register Rcpp functions.

# raptr 0.0.3 (released)

- reduce memory consumption using external pointers.
- fix memory leaks.
- fix bug in tests for `calcSpeciesAverageInPus`.

# raptr 0.0.2

- reduce installation size.
- fix typo in README.

# raptr 0.0.1.1

- fixed authorship typo.
- updated README with instructions for installing from CRAN.

# raptr 0.0.1 (released)

- Initial package version.
