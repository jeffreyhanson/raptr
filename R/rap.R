#' @include RcppExports.R raptr-internal.R misc.R generics.R RapReliableOpts.R RapUnreliableOpts.R GurobiOpts.R RapData.R RapUnsolved.R RapResults.R RapSolved.R
NULL

#' Generate prioritizations using RAP
#'
#' This is a general function to create Rap objects from scratch and solve them
#' to generate solutions.
#'
#' @param pus [sf::st_as_sf()] object representing planning
#'   units.
#'
#' @param species [terra::rast()] object with species distribution
#'   data.
#'
#' @param spaces [terra::rast()] or `list` of [terra::rast()] objects. Each
#'   elements denotes the spatial distribution for each space. Defaults to
#'   `NULL` such that spaces are generated automatically.
#'
#' @param formulation `character` to indicate if the `"unreliable"` or
#'   `"reliable"` formulation should be used to generate prioritizations.
#'   Defaults to `"unreliable"`.
#'
#' @param solve `logical` should solutions be generated?
#'
#' @param ... arguments are passed to [GurobiOpts()],
#'   [make.RapData()], and [RapReliableOpts()] or
#'   [RapUnreliableOpts()] functions.
#'
#' @note Type `vignette("raptr")` to see the package vignette for a
#'   tutorial.
#'
#' @return A new [RapSolved()] object if `solve` is `TRUE`,
#'  otherwise an [RapUnsolved()] is returned.
#'
#' @seealso [GurobiOpts()], [RapReliableOpts()],
#'   [RapUnreliableOpts()] [RapData()],
#'   [RapResults()], [RapUnsolved()],
#'   [RapSolved()].
#'
#' @export
rap <- function(pus, species, spaces = NULL,
                formulation = c("unreliable", "reliable")[1],
                solve = TRUE,
                ...) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(pus, "sf"),
    inherits(species, "SpatRaster"),
    inherits(spaces, c("SpatRaster", "list", "NULL")),
    assertthat::is.string(formulation),
    assertthat::noNA(formulation),
    assertthat::is.flag(solve),
    assertthat::noNA(solve)
  )
  # set formulation
  match.arg(formulation, c("unreliable", "reliable"))
  if (formulation == "unreliable") {
    opts <- "RapUnreliableOpts"
  } else {
    opts <- "RapReliableOpts"
  }
  # check for unrecognised args
  all.args <- names(list(...))
  valid.args <- c(
    unlist(
      lapply(
        c(opts, "make.RapData", "GurobiOpts"),
        function(fn) names(formals(fn))
      )
    ),
    "b", "verbose"
  )
  invalid.args <- all.args[!all.args %in% valid.args]
  if (length(invalid.args) > 0)
    stop("Unrecognised arguments: ", paste(invalid.args, collapse = ", "))
  # create unsolved object
  x <- RapUnsolved(
    opts = do.call(
      opts, parseArgs(opts, object = NULL, skip = NULL, ...)
    ),
    data = do.call(
      make.RapData,
      append(
        list(pus = pus, species = species, spaces = spaces),
        parseArgs("make.RapData", object = NULL, skip = NULL, ...)
      )
    )
  )
  # solve object if specified
  if (solve) {
    x <- do.call(
      "solve",
      append(
        append(list(a = x), parseArgs2(c("b", "verbose"), ...)),
        parseArgs("GurobiOpts", object = NULL, skip = NULL, ...)
      )
    )
  }
  # return object
  return(x)
}
