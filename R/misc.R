#' @include RcppExports.R raptr-internal.R
NULL

#' Test if Gurobi is installed
#'
#' This function determines if the Gurobi R package is installed on the
#' computer and that it can be used \code{\link[base]{options}}.
#'
#' @param verbose \code{logical} should messages be printed?
#'
#' @return \code{logical} Is it installed and ready to use?
#'
#' @seealso \code{\link[base]{options}}.
#'
#' @examples
#' \donttest{
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

#' Test if GDAL is installed on computer
#'
#' This function tests if GDAL is installed on the computer.
#' If not, download it here: \url{http://download.osgeo.org/gdal}.
#'
#' @return \code{logical} is GDAL installed?
#'
#' @seealso \code{\link[gdalUtils]{gdal_setInstallation}}.
#'
#' @examples
#' # check if gdal is installed on system
#' \donttest{
#' is.gdalInstalled()
#' }
#'
#' @export
is.gdalInstalled <- function() {
  suppressWarnings(findGdalInstallationPaths())
  return(!is.null(getOption("gdalUtils_gdalPath")))
}

#' Rasterize polygon data using GDAL
#'
#' This function converts a \code{SpatialPolygonsDataFrame} to a
#' \code{RasterLayer} using GDAL. It is expected to be faster than
#' \code{\link[raster]{rasterize}} for large datasets. However, it will be
#' significantly slower for small datasets because the data will need to be
#' written and read from disk.
#'
#' @param x \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#'
#' @param y \code{\link[raster]{raster}} with dimensions, extent, and
#'   resolution to be used as a template for new raster.
#'
#' @param field \code{character} column name with values to burn into the
#'   output raster. If not supplied, default behaviour is to burn polygon
#'   indices into the \code{\link[raster]{raster}}.
#'
#' @return \code{RasterLayer} object.
#'
#' @seealso \code{\link[raster]{rasterize}}, \code{\link{is.gdalInstalled}}.
#'
#' @examples
#' \donttest{
#' # load dat
#' data(cs_pus,cs_spp)
#'
#' # rasterize spatial polygon data
#' x <- rasterizeGDAL(cs_pus[1:5,], cs_spp[[1]])
#'
#' # plot data
#' par(mfrow = c(1,2))
#' plot(cs_pus[1:5,], main = "original data")
#' plot(x, main = "rasterized data")
#' }
#'
#' @export
rasterizeGDAL <- function(x, y, field = NULL) {
  assertthat::assert_that(inherits(x, "SpatialPolygonsDataFrame"),
                          inherits(y, "RasterLayer"),
                          is.null(field) || (assertthat::is.string(field) &&
                                             field %in% names(x@data)))
  if (is.null(field)) {
    x@data$id <- seq_len(nrow(x@data))
    field <- "id"
  }
  rgdal::writeOGR(x, tempdir(), "polys", driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  raster::writeRaster(raster::setValues(y, NA), file.path(tempdir(),
                      "rast.tif"), NAflag = -9999, overwrite = TRUE)
  return(gdalUtils::gdal_rasterize(file.path(tempdir(), "polys.shp"),
                                  file.path(tempdir(), "rast.tif"),
                                  l = "polys", a = field,
                                  output_Raster = TRUE)[[1]])
}

#' Blank raster
#'
#' This functions creates a blank raster based on the spatial extent of a
#' Spatial object.
#'
#' @param x \code{\link[sp]{Spatial-class}} object.
#'
#' @param res \code{numeric vector} specifying resolution of the output raster
#'   in the x and y dimensions. If \code{vector} is of length one, then the
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
#' @seealso \code{\link[PBSmapping]{PolySet}}.
#'
#' @name PolySet-class
#'
#' @aliases PolySet
#'
#' @exportClass PolySet
methods::setClass("PolySet")

#' RapOpts class
#'
#' Object is either \code{\link{RapReliableOpts}} or
#' \code{\link{RapUnreliableOpts}}.
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
#' @seealso \code{\link{GurobiOpts}}.
#'
#' @aliases SolverOpts
#'
#' @exportClass SolverOpts
methods::setClass("SolverOpts")

#' Sample random points from a RasterLayer
#'
#' This function generates random points in a \code{\link[raster]{raster}}
#' object.
#'
#' @param mask \code{\link[raster]{raster}} object
#'
#' @param n \code{integer} number of points to sample
#'
#' @param prob \code{logical} should the raster values be used as weights?
#'   Defaults to \code{FALSE}.
#'
#' @return \code{\link[base]{matrix}} with x-coordinates, y-coordinates, and
#'   cell values.
#'
#' @seealso This function is similar to \code{dismo::randomPoints}.
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
