#' @include RcppExports.R raptr-internal.R
NULL

#' Test if Gurobi is installed
#'
#' This function determines if the Gurobi R package is installed on the
#' computer and that it can be used [base::options()].
#'
#' @param verbose `logical` should messages be printed?
#'
#' @return `logical` Is it installed and ready to use?
#'
#' @seealso [base::options()].
#'
#' @examples
#' \dontrun{
#' # check if Gurobi is installed
#' is.GurobiInstalled()
#'
#' # print cached status of installation
#' options()$GurobiInstalled
#' }
#' @export
is.GurobiInstalled <- function(verbose = TRUE) {
  # check if gurobi installed
  software_installed <- suppressWarnings(
    system2("gurobi_cl", "-v", stdout = FALSE, stderr = FALSE)
  ) == 0
  pkg_installed <-
    requireNamespace("gurobi", quietly = TRUE) &&
    utils::packageVersion("gurobi") >= as.package_version("8.0.0")
  # store result
  options(
    GurobiInstalled = list(
      gurobi = isTRUE(software_installed) && isTRUE(pkg_installed)
    )
  )
  # display messages if needed
  if (verbose) {
    if (!isTRUE(software_installed)) {
      message(paste(gurobi_installation_message(), collapse = "\n\n"))
    }
    if (!isTRUE(pkg_installed)) {
      message(paste(package_installation_message(), collapse = "\n\n"))
    }
  }
  # return result
  software_installed && pkg_installed
}

#' Gurobi installation message
#'
#' Create a message to display when the Gurobi suite is not available.
#'
#' @return A `character` value.
#'
#' @noRd
gurobi_installation_message <- function() {
  # define installation instructions
  gurobiInstallationInstructions <- paste(
    "Follow these instructions to download the Gurobi software suite:\n  ",
    c(
      "Linux" = "https://www.gurobi.com/documentation/9.5/quickstart_linux/software_installation_guid.html",
      "Windows" = "https://www.gurobi.com/documentation/9.5/quickstart_windows/software_installation_guid.html",
      "Darwin" = "https://www.gurobi.com/documentation/9.5/quickstart_mac/software_installation_guid.html"
    )[Sys.info()[["sysname"]]])
  rInstallationInstructions1 <- paste(
    "Follow these instructions to install the \"gurobi\" R package:\n  ",
    c(
      "Linux" = "https://www.gurobi.com/documentation/6.5/quickstart_linux/r_installing_the_r_package.html",
      "Windows" = "https://www.gurobi.com/documentation/6.5/quickstart_windows/r_installing_the_r_package.html",
      "Darwin" = "https://www.gurobi.com/documentation/6.5/quickstart_mac/r_installing_the_r_package.html"
    )[Sys.info()[["sysname"]]])
  licenseInstructions <- paste0("The gurobi R package requires a Gurobi ",
    "license to work:\n  visit this web-page for an overview: \n    ",
    "https://www.gurobi.com/products/licensing-options/\n  academics can obtain a license at no cost ",
    "here:\n    https://www.gurobi.com/downloads/end-user-license-agreement-academic/")
  c(
    "The gurobi software is not installed",
    gurobiInstallationInstructions,
    licenseInstructions,
    rInstallationInstructions1
  )
}

#' Gurobi package installation message
#'
#' Create a message to display when the Gurobi package is not available.
#'
#' @return A `character` value.
#'
#' @noRd
package_installation_message <- function() {
  rInstallationInstructions1 <- paste(
    "Follow these instructions to install the \"gurobi\" R package:\n  ",
    c(
      "Linux" = "https://www.gurobi.com/documentation/6.5/quickstart_linux/r_installing_the_r_package.html",
      "Windows" = "https://www.gurobi.com/documentation/6.5/quickstart_windows/r_installing_the_r_package.html",
      "Darwin" = "https://www.gurobi.com/documentation/6.5/quickstart_mac/r_installing_the_r_package.html"
    )[Sys.info()[["sysname"]]])
  c(
    "The gurobi R package (version 8.0.0+) is not installed",
    rInstallationInstructions1
  )
}

#' Blank raster
#'
#' This functions creates a blank `SpatRaster` based on the spatial extent of a
#' `sf` object.
#'
#' @param x [sf::st_sf()] object.
#'
#' @param res `numeric` `vector` specifying resolution of the output raster
#'   in the x and y dimensions. If `vector` is of length one, then the
#'   pixels are assumed to be square.
#'
#' @examples
#' \dontrun{
#' # make sf data
#' polys <- sim.pus(225L)
#'
#' # make raster from sf
#' blank.raster(polys, 1)
#' }
#' @rdname blank.raster
#'
#' @export
blank.raster <- function(x, res) {
  assertthat::assert_that(
    inherits(x, "sf"),
    is.numeric(res),
    assertthat::noNA(res),
    length(res) %in% c(1, 2)
  )
  # initialize resolution inputs
  if (length(res) == 1)
    res <- c(res, res)
  # get bounding box data
  bb <- sf::st_bbox(x)
  # extract coordinates
  if ((bb[["xmax"]] - bb[["xmin"]]) <= res[1]) {
    xpos <- c(bb[["xmin"]], res[1])
  } else {
    xpos <- seq(
      bb[["xmin"]],
      bb[["xmax"]] +
        (res[1] * (((bb[["xmax"]] - bb[["xmin"]]) %% res[1]) != 0)),
      res[1]
    )
  }
  if ((bb[["ymax"]] - bb[["ymin"]]) <= res[2]) {
    ypos <- c(bb[["ymin"]], res[2])
  } else {
    ypos <- seq(
      bb[["ymin"]],
      bb[["ymax"]] +
        (res[2] * (((bb[["ymax"]] - bb[["ymin"]]) %% res[2]) != 0)),
      res[2]
    )
  }
  # generate raster
  rast <- terra::rast(
    xmin = min(xpos),
    xmax = max(xpos),
    ymin = min(ypos),
    ymax = max(ypos),
    nrow = length(ypos) - 1,
    ncol = length(xpos) - 1
  )
  return(terra::setValues(rast, 1))
}

#' PolySet
#'
#' Object contains PolySet data.
#'
#' @seealso [PBSmapping::PolySet()].
#'
#' @name PolySet-class
#'
#' @aliases PolySet
#'
#' @exportClass PolySet
methods::setClass("PolySet")

#' RapOpts class
#'
#' Object is either [RapReliableOpts()] or
#' [RapUnreliableOpts()].
#'
#' @name RapOpts-class
#'
#' @aliases RapOpts
#'
#' @exportClass RapOpts
methods::setClass("RapOpts",
  methods::representation(BLM = "numeric"),
  prototype = list(BLM = 0))

#' SolverOpts class
#'
#' Object stores parameters used to solve problems.
#'
#' @name SolverOpts-class
#'
#' @seealso [GurobiOpts()].
#'
#' @aliases SolverOpts
#'
#' @exportClass SolverOpts
methods::setClass("SolverOpts")

#' Sample random points from a SpatRaster
#'
#' This function generates random points in a [terra::rast()] object.
#'
#' @param mask [terra::rast()] object
#'
#' @param n `integer` number of points to sample
#'
#' @param prob `logical` should the raster values be used as weights?
#'   Defaults to `FALSE`.
#'
#' @return [base::matrix()] with x-coordinates, y-coordinates, and
#'   cell values.
#'
#' @seealso This function is similar to `dismo::randomPoints`.
#'
#' @examples
#' \dontrun{
#' # simulate data
#' sim_pus <- sim.pus(225L)
#' sim_spp <- sim.species(sim_pus, model = "normal", n = 1, res = 0.25)
#'
#' # generate points
#' pts1 <- randomPoints(sim_spp, n = 5)
#' pts2 <- randomPoints(sim_spp, n = 5, prob = TRUE)
#'
#' # plot points
#' plot(sim_spp)
#' points(pts1, col = "red")
#' points(pts2, col = "black")
#' }
#' @export
randomPoints <- function(mask, n, prob = FALSE) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(mask, "SpatRaster"),
    assertthat::is.count(n),
    assertthat::noNA(n),
    assertthat::is.flag(prob),
    assertthat::noNA(prob)
  )
  # extract cells
  d <- terra::as.data.frame(sum(mask), cells = TRUE)
  d <- d[is.finite(d[[2]]), , drop = FALSE]
  if (nrow(d) < n) {
    stop("argument to n is greater than the number of cells with finite values")
  }
  if (prob) {
    randomCells <- sample(
      d[[1]], n, prob = d[[2]], replace = FALSE
    )
  } else {
    randomCells <- sample(d[[1]], n, replace = FALSE)
  }
  # get coordinates of the cell centres
  terra::xyFromCell(mask, randomCells)
}
