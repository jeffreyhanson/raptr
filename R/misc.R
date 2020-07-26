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
#'
#' @export
is.GurobiInstalled <- function(verbose = TRUE) {
  # define installation instructions
  gurobiInstallationInstructions <- paste(
    "Follow these instructions to download the Gurobi software suite:\n  ",
    c("Linux" = "http://bit.ly/1ksXUaQ", "Windows" = "http://bit.ly/1MrjXWc",
      "Darwin" = "http://bit.ly/1N0AlT0")[Sys.info()[["sysname"]]])

  rInstallationInstructions1 <- paste(
    "Follow these instructions to install the \"gurobi\" R package:\n  ",
    c("Linux" = "http://bit.ly/1HLCRoE", "Windows" = "http://bit.ly/1MMSZaH",
      "Darwin" = "http://bit.ly/1Pr2WRG")[Sys.info()[["sysname"]]])

  licenseInstructions <- paste0("The Gurobi R package requires a Gurobi ",
    "license to work:\n  visit this web-page for an overview: ",
    "http://bit.ly/1OHEQCm\n  academics can obtain a license at no cost ",
    "here: http://bit.ly/1iYg3LX")

  # check if gurobi installed
  result <- suppressWarnings(system2("gurobi_cl", "-v", stdout = FALSE,
                                     stderr = FALSE))
  if (result != 0) {
    if (verbose) {
      message("The gorubi software is not installed")
      message(gurobiInstallationInstructions, "\n\n", licenseInstructions,
              "\n\n", rInstallationInstructions1)
    }
    options(GurobiInstalled = list(gurobi = FALSE))
    return(FALSE)
  }

  # check if R packages installed
  pkgs.installed <- list(gurobi =
    requireNamespace("gurobi", quietly = TRUE) &&
    utils::packageVersion("gurobi") >= as.package_version("8.0.0"))
  if (!pkgs.installed[[1]]) {
    if (verbose) {
      message("The gorubi R package (version 8.0.0+) is not installed\n")
      message(rInstallationInstructions1, "\n")
    }
  }
  options(GurobiInstalled = pkgs.installed)
  if (!pkgs.installed[[1]])
    return(FALSE)
  return(TRUE)
}

#' Blank raster
#'
#' This functions creates a blank raster based on the spatial extent of a
#' Spatial object.
#'
#' @param x [sp::Spatial-class] object.
#'
#' @param res `numeric` `vector` specifying resolution of the output raster
#'   in the x and y dimensions. If `vector` is of length one, then the
#'   pixels are assumed to be square.
#'
#' @examples
#' # make SpatialPolygons
#' polys <- sim.pus(225L)
#'
#' # make RasterLayer from SpatialPolygons
#' blank.raster(polys, 1)
#'
#' @rdname blank.raster
#'
#' @export
blank.raster <- function(x, res) {
  assertthat::assert_that(inherits(x, "Spatial"), is.numeric(res),
                          all(is.finite(res)), length(res) %in% c(1, 2))
  # initialize resolution inputs
  if (length(res) == 1)
    res <- c(res, res)
  # extract coordinates
  if ((raster::xmax(x) - raster::xmin(x)) <= res[1]) {
    xpos <- c(raster::xmin(x), res[1])
  } else {
    xpos <- seq(raster::xmin(x),
                raster::xmax(x) + (res[1] * (((raster::xmax(x) -
                  raster::xmin(x)) %% res[1]) != 0)),
                res[1])
  }
  if ((raster::ymax(x) - raster::ymin(x)) <= res[2]) {
    ypos <- c(raster::ymin(x), res[2])
  } else {
    ypos <- seq(raster::ymin(x),
                raster::ymax(x) + (res[2] * (((raster::ymax(x) -
                  raster::ymin(x)) %% res[2]) != 0)),
                res[2])
  }
  # generate raster from sp
  rast <- raster::raster(xmn = min(xpos), xmx = max(xpos), ymn = min(ypos),
                         ymx = max(ypos), nrow = length(ypos) - 1,
                         ncol = length(xpos) - 1)
  return(raster::setValues(rast, 1))
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

#' Sample random points from a RasterLayer
#'
#' This function generates random points in a [raster::raster()]
#' object.
#'
#' @param mask [raster::raster()] object
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
#'
#' @export
randomPoints <- function(mask, n, prob = FALSE) {
  # check that data can be processed in memory
  stopifnot(raster::canProcessInMemory(mask, n = 3))
  # extract cells
  validPos <- which(is.finite(mask[]))
  if (length(validPos) < n)
    stop("argument to n is greater than the number of cells with finite values")
  if (prob) {
    randomCells <- sample(validPos, n, prob = mask[validPos], replace = FALSE)
  } else {
    randomCells <- sample(validPos, n, replace = FALSE)
  }
  # get coordinates of the cell centres
  return(raster::xyFromCell(mask, randomCells))
}
