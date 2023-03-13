# check if on Fedora
os_name <- utils::sessionInfo()$running
is_fedora <- TRUE
if (
  is.character(os_name) &&
  identical(length(os_name), 1L) &&
  all(!is.na(os_name))
) {
  is_fedora <- any(grepl("fedora", tolower(os_name), fixed = TRUE))
}

# run tests (but not on Fedora systems)
if (isTRUE(is_fedora)) {
  message("skipping tests on Fedora system")
} else {
  ## load packages
  library(testthat)
  library(raptr)

  ## enable parallel testing
  Sys.unsetenv("R_TESTS")

  ## determine reporter
  if (identical(Sys.getenv("CI"), "true")) {
    reporter <- "progress"
  } else {
    reporter <- testthat::check_reporter()
  }

  ## run tests
  test_check("raptr", reporter = reporter)
}
