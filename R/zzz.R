.onAttach <- function(libname, pkgname) {
  # check gurobi package installed
  msg <- utils::capture.output({
    gurobi.installed <- is.GurobiInstalled(verbose = TRUE)
  })
  if (!gurobi.installed) {
    packageStartupMessage(paste0(paste(msg, collapse = "\n"),
        "\n\nType the code \"is.GurobiInstalled()\" to check if Gurobi ",
        "is\nsuccesfully installed after following these instructions."))
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("raptr", libpath)
}
