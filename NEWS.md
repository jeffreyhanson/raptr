# raptr 0.1.1 (released)

- vignette size has been reduced.
- updated DOI in readme.

# raptr 0.1.0 (unreleased)

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
- \donttest instead of \dontrun for examples.
- fix broken example code.

# raptr 0.0.4 (unreleased)

- find out which suggested packages are installed using `requireNamespace`.
- add more links to package documentation.
- register Rcpp functions.

# raptr 0.0.3 (released)

- reduce memory consumption using external pointers.
- fix memory leaks.
- fix bug in tests for calcSpeciesAverageInPus.

# raptr 0.0.2 (unreleased)

- reduce installation size.
- fix typo in README.

# raptr 0.0.1.1 (unreleased)

- fixed authorship typo.
- updated README.md with instructions for installing from CRAN.

# raptr 0.0.1 (unreleased)

- Initial package version.
