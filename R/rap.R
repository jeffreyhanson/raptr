#' @include RcppExports.R raptr-internal.R misc.R generics.R RapReliableOpts.R RapUnreliableOpts.R GurobiOpts.R RapData.R RapUnsolved.R RapResults.R RapSolved.R
NULL

#' Generate prioritizations using RAP
#'
#' This is a general function to create Rap objects from scratch and solve them
#' to generate solutions.
#'
#' @param pus \code{\link[sp]{SpatialPolygons}} object representing planning
#'   units.
#'
#' @param species \code{\link[raster]{raster}} object with species distribution
#'   data.
#'
#' @param spaces \code{list} of \code{\link[raster]{raster}} objects. Each
#'   elements denotes the spatial distribution for each space. Defaults to
#'   \code{NULL}.
#'
#' @param formulation \code{character} to indicate if the \code{"unreliable"} or
#'   \code{"reliable"} formulation should be used to generate prioritizations.
#'   Defaults to \code{"unreliable"}.
#'
#' @param solve \code{logical} should solutions be generated?
#'
#' @param ... arguments are passed to \code{\link{GurobiOpts}},
#'   \code{\link{make.RapData}}, and \code{\link{RapReliableOpts}} or
#'   \code{\link{RapUnreliableOpts}} functions.
#'
#' @note Type \code{vignette("raptr")} to see the package vignette for a
#'   tutorial.
#'
#' @return \code{\link{RapSolved}} object if \code{solve} is \code{TRUE},
#'  otherwise an \code{\link{RapUnsolved}} is returned.
#'
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RapReliableOpts}},
#'   \code{\link{RapUnreliableOpts}} \code{\link{RapData}},
#'   \code{\link{RapResults}}, \code{\link{RapUnsolved}},
#'   \code{\link{RapSolved}}.
#'
#' @export
rap <- function(pus, species, spaces = NULL,
                formulation = c("unreliable", "reliable")[1], solve = TRUE,
                ...) {
  # set formulation
  match.arg(formulation, c("unreliable", "reliable"))
  if (formulation == "unreliable") {
    opts <- "RapUnreliableOpts"
  } else {
    opts <- "RapReliableOpts"
  }
  # check for unrecognised args
  all.args <- names(list(...))
  valid.args <- c(unlist(lapply(c(opts, "make.RapData", "GurobiOpts"),
                                function(fn) names(formals(fn)))),
                  "b", "verbose")
  invalid.args <- all.args[!all.args %in% valid.args]
  if (length(invalid.args) > 0)
    stop("Unrecognised arguments: ", paste(invalid.args, collapse = ", "))
  # create unsolved object
  x <- RapUnsolved(opts = do.call(opts, parseArgs(opts, object = NULL,
                                                  skip = NULL, ...)),
                  data = do.call(make.RapData,
                                 append(list(pus = pus, species = species,
                                             spaces = spaces),
                                        parseArgs("make.RapData",
                                                  object = NULL,
                                                  skip = NULL, ...))))
  # solve object if specified
  if (solve) {
    x <- do.call("solve", append(append(list(a = x),
                                        parseArgs2(c("b", "verbose"), ...)),
                                 parseArgs("GurobiOpts", object = NULL,
                                           skip = NULL, ...)))
  }
  # return object
  return(x)
}
