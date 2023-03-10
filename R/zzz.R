.onAttach <- function(libname, pkgname) {
  # check gurobi package installed
  gurobi_installed <- is.GurobiInstalled(verbose = FALSE)
  # display start up message if needed and not interactive
  if (!gurobi_installed && interactive()) {
    msg <- c(
      "The gurobi software and/or R package is not installed.",
      "Use `is.GurobiInstalled()` to see installation instructions."
    )
    packageStartupMessage(paste(msg, collapse = "\n"))
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("raptr", libpath)
}
