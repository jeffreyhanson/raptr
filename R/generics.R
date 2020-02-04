#' @include RcppExports.R raptr-internal.R
NULL

#' Calculate average value for species data in planning units
#'
#' This function calculates the average of species values in each planning unit.
#' Be aware that using polygons with overlaps will result in inaccuracies.
#' By default all polygons will be treated as having separate ids.
#'
#' @param x \code{\link[sp]{SpatialPolygons}} or
#'   \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#'
#' @param y \code{\link[raster]{RasterLayer-class}},
#'   \code{\link[raster]{RasterStack-class}}, or
#'   \code{\link[raster]{RasterBrick-class}} object.
#'
#' @param ids \code{integer} vector of ids. Defaults to indices of layers in
#'   argument to \code{y}.
#'
#' @param ncores \code{integer} number of cores to use for processing. Defaults
#'   to 1.
#'
#' @param gdal \code{logical} Should raster processing be performed using GDAL
#'   libraries? Defaults to \code{FALSE}.
#'
#' @param field \code{integer} index or \code{character} name of column with
#'   planning unit ids. Valid only when \code{x} is a
#'   \code{\link[sp]{SpatialPolygonsDataFrame}} object. Default behavior is to
#'   treat each polygon as a different planning unit.
#'
#' @param ... not used.
#'
#' @return \code{\link[base]{data.frame}} with sum of raster values in each
#'   polygon.
#'
#' @seealso \code{\link{is.gdalInstalled}}, \url{http://www.gdal.org/},
#'   \url{http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries}.
#'
#' @examples
#' # simulate data
#' sim_pus <- sim.pus(225L)
#' sim_spp <- lapply(c("uniform", "normal", "bimodal"), sim.species, n = 1,
#'                   res = 1, x = sim_pus)
#'
#' # calculate average for 1 species
#' puvspr1.dat <- calcSpeciesAverageInPus(sim_pus, sim_spp[[1]])
#'
#' # calculate average for multiple species
#' puvspr2.dat <- calcSpeciesAverageInPus(sim_pus, stack(sim_spp))
#'
#' @export
calcSpeciesAverageInPus <- function(x, ...) UseMethod("calcSpeciesAverageInPus")

#' Simulate species distribution data for RAP
#'
#' This function simulates species distributions for RAP.
#'
#' @param x \code{\link[raster]{RasterLayer-class}} or
#'   \code{\link[sp]{SpatialPolygons}} object delineate the spatial extent to
#'   delineate study area.
#'
#' @param n \code{integer} number of species. Defaults to 1.
#'
#' @param res \code{numeric} resolution to simulate distributions. Only needed
#'   when \code{\link[sp]{SpatialPolygons}} supplied.
#'
#' @param model \code{\link[RandomFields]{RMmodel}} model to simulate species
#'   distributions with. Defaults \code{\link[RandomFields]{RPgauss}}.
#'
#' @param ... parameters passed to \code{\link[RandomFields]{RandomFields}}.
#'
#' @details Distributions are simulated by passing \code{model} to
#'   \code{\link[RandomFields]{RFsimulate}} and converting to logistic values
#'   using \code{\link[boot]{inv.logit}}.
#'
#' @return \code{RasterStack} with layers for each species.
#'
#' @seealso \code{\link[RandomFields]{RFsimulate}}.
#'
#' @examples
#' # make polygons
#' sim_pus <- sim.pus(225L)
#'
#' # simulate 1 uniform species distribution using RasterLayer
#' s1 <- sim.species(blank.raster(sim_pus, 1), n = 1, model = "uniform")
#'
#' # simulate 1 uniform species distribution based on SpatialPolygons
#' s2 <- sim.species(sim_pus, res = 1, n = 1, model = "uniform")
#'
#' # simulate 1 normal species distributions
#' s3 <- sim.species(sim_pus, res = 1, n = 1, model = "normal")
#'
#' # simulate 1 bimodal species distribution
#' s4 <- sim.species(sim_pus, res = 1, n = 1, model = "bimodal")
#'
#' # simulate 1 species distribution using a RModel object from RandomFields
#' s5 <- sim.species(sim_pus, res = 1, n = 1, model = RandomFields::RPgauss())
#'
#' # simulate 5 species distribution using a RModel object from RandomFields
#' s6 <- sim.species(sim_pus, res = 1, n = 5, model = RandomFields::RPgauss())
#'
#' # plot simulations
#' par(mfrow = c(2,2))
#' plot(s2, main = "constant")
#' plot(s3, main = "normal")
#' plot(s4, main = "bimodal")
#' plot(s5, main = "RPgauss()")
#'
#' @export sim.species
sim.species <- function(x, ...) UseMethod("sim.species")

#' Simulate attribute space data for RAP
#'
#' This function simulates attribute space data for RAP.
#'
#' @param x \code{RasterLayer} or \code{\link[sp]{SpatialPolygons}} object
#'   delineate the spatial extent to delineate study area.
#'
#' @param d \code{integer} number of dimensions. Defaults to 2.
#'
#' @param res \code{numeric} resolution to simulate distributions. Only needed
#'   when \code{\link[sp]{SpatialPolygons}} supplied.
#'
#' @param model \code{\link[RandomFields]{RMmodel}} model to simulate species
#'   distributions with. Defaults \code{\link[RandomFields]{RPgauss}}.
#'
#' @param ... parameters passed to \code{\link[RandomFields]{RandomFields}}.
#'
#' @details Distributions are simulated by passing \code{model} to
#'   \code{\link[RandomFields]{RFsimulate}}.
#'
#' @return \code{RasterStack} with layers for each dimension of the space.
#'
#' @seealso \code{\link[RandomFields]{RFsimulate}}.
#'
#' @name sim.space
#'
#' @examples
#' # simulate plannign units
#' sim_pus <- sim.pus(225L)
#'
#' # simulate 1d space using RasterLayer
#' s1 <- sim.space(blank.raster(sim_pus, 1), d = 1)
#'
#' # simulate 1d space using SpatialPolygons
#' s2 <- sim.space(sim_pus, res = 1, d = 1)
#'
#' # simulate 2d space using SpatialPolygons
#' s3 <- sim.space(sim_pus, res = 1, d = 2)
#'
#' # plot simulated spaces
#' par(mfrow = c(2,2))
#' plot(s1, main = "s1")
#' plot(s2, main = "s2")
#' plot(s3[[1]], main = "s3: first dimension")
#' plot(s3[[2]], main = "s3: second dimension")
#'
#' @export sim.space
sim.space <- function(x, ...) UseMethod("sim.space")

#' Solve RAP object
#'
#' This function uses Gurobi to find prioritizations using the input parameter
#' and data stored in a \code{\link{RapUnsolved}} object, and returns a
#' \code{\link{RapSolved}} object with outputs in it.
#'
#' @param a \code{\link{RapUnsolved}} or \code{\link{RapSolved}} object.
#'
#' @param b \code{missing} to generate solutions using Gurobi. Prioritizations
#'   can be specified using  \code{logical}, \code{numeric}, or
#'   \code{\link[base]{matrix}} objects. This may be useful for evaluating the
#'   performance of solutions obtained using other software.
#'
#' @param verbose \code{logical} should messages be printed during creation of
#'   the initial model matrix?.
#'
#' @param ... not used.
#'
#' @return \code{\link{RapSolved}} object
#'
#' @note This function is used to solve a \code{\link{RapUnsolved}} object that
#'   has all of its inputs generated. The rap function (without lower case 'r')
#'   provides a more general interface for generating inputs and outputs.
#'
#' @seealso \code{\link{RapUnsolved}}, \code{\link{RapSolved}}.
#'
#' @examples
#' # load RapUnsolved object
#' data(sim_ru)
#' \donttest{
#' # solve it using Gurobi
#' sim_rs <- solve(sim_ru)
#'
#' # evaluate manually specified solution using planning unit indices
#' sim_rs2 <- solve(sim_ru, seq_len(10))
#'
#' # evaluate manually specifed solution using binary selections
#' sim_rs3 <- solve(sim_ru, c(rep(TRUE, 10), rep(FALSE, 90)))
#'
#' #  evaluate multiple manually specified solutions
#' sim_rs4 <- solve(sim_ru, matrix(sample(c(0, 1), size = 500, replace = TRUE),
#'                  ncol = 100, nrow = 5))
#' }
#'
#' @name solve
#'
#' @rdname solve
#'
#' @importFrom Matrix solve
#'
#' @export solve
NULL

#' Plot object
#'
#' This function plots the solutions contained in \code{\link{RapSolved}}
#' objects. It can be used to show a single solution, or the the selection
#' frequencies of planning units contained in a single \code{\link{RapSolved}}
#' object. Additionally, two \code{\link{RapSolved}} objects can be supplied to
#' plot the differences between them.
#'
#' @param x \code{\link{RapSolved}} object.
#'
#' @param y Available inputs are: \code{NULL} to plot selection frequencies,
#'   \code{numeric} number to plot a specific solution,
#'   \code{0} to plot the best solution, and a \code{\link{RapSolved}}
#'   object to plot differences in solutions between objects. Defaults to
#'   \code{NULL}.
#'
#' @param i Available inputs are: \code{NULL} to plot selection frequencies.
#'   \code{numeric} to plot a specific solution, \code{0} to plot the best
#'   solution. This argument is only used when \code{y} is a
#'   \code{\link{RapSolved}} object. Defaults to \code{NULL}.
#'
#' @param j Available inputs are: \code{NULL} to plot selection frequencies.
#'   \code{numeric} to plot a specific solution, \code{0} to plot the best
#'   solution. This argument is only used when \code{y} is a
#'   \code{\link{RapSolved}} object. Defaults to argument \code{j}.
#'
#' @param basemap \code{character} object indicating the type of basemap to use
#'   (see \code{\link{basemap}}). Valid options include \code{"none"},
#'   \code{"roadmap"}, \code{"mobile"}, \code{"satellite"}, \code{"terrain"},
#'   \code{"hybrid"}, \code{"mapmaker-roadmap"}, \code{"mapmaker-hybrid"}.
#'   Defaults to \code{"none"}.
#'
#' @param pu.color.palette \code{character} name of colors or color palette
#'   (\code{\link[RColorBrewer]{brewer.pal}}) to indicate planning unit
#'   statuses. Defaults to \code{c("grey30", "green", "yellow", "black",
#'   "gray80", "red", "orange")}.
#'
#' @param alpha \code{numeric} value to indicating the transparency level for
#'   coloring the planning units.
#'
#' @param grayscale \code{logical} should the basemap be gray-scaled?
#'
#' @param main \code{character} title for the plot. Defaults to \code{NULL} and
#'   a default title is used.
#'
#' @param force.reset \code{logical} if basemap data has been cached, should it
#'   be re-downloaded?
#'
#' @name plot
#'
#' @seealso \code{\link{RapSolved}}.
#'
#' @examples
#' # load example data set with solutions
#' data(sim_rs)
#'
#' # plot selection frequencies
#' plot(sim_rs)
#'
#' # plot best solution
#' plot(sim_rs, 0)
#'
#' # plot second solution
#' plot(sim_rs, 2)
#'
#' # plot different between best and second solutions
#' plot(sim_rs, sim_rs, 0 ,2)
NULL

#' Names
#'
#' This function sets or returns the species names in an object.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param value new species names.
#'
#' @name names
#'
#' @seealso \code{\link{RapData}}, \code{\link{RapUnsolved}},
#'   \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # show names
#' names(sim_rs)
#'
#' # change names
#' names(sim_rs) <- c('spp1', 'spp2', 'spp3')
#'
#' # show new names
#' names(sim_rs)
#'
NULL

#' Print objects
#'
#' Prints objects.
#'
#' @param x \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}},
#'   \code{\link{RapReliableOpts}}, \code{\link{RapData}},
#'   \code{\link{RapUnsolved}}, \code{\link{RapResults}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param header \code{logical} should object header be included?
#'
#' @param ... not used.
#'
#' @name print
#'
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}},
#'   \code{\link{RapReliableOpts}}, \code{\link{RapData}},
#'   \code{\link{RapUnsolved}}, \code{\link{RapResults}},
#'   \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#'
#' # print GurobiOpts object
#' print(GurobiOpts())
#'
#' # print RapReliableOpts object
#' print(RapReliableOpts())
#'
#' # print RapUnreliableOpts object
#' print(RapUnreliableOpts())
#'
#' # print RapData object
#' print(sim_ru@@data)
#'
#' # print RapUnsolved object
#' print(sim_ru)
#'
#' # print RapResults object
#' print(sim_rs@@results)
#'
#' # print RapSolved object
#' print(sim_rs)
NULL

#' Show objects
#'
#' Shows objects.
#'
#' @param object \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}},
#'   \code{\link{RapReliableOpts}}, \code{\link{RapData}},
#'   \code{\link{RapUnsolved}}, \code{\link{RapResults}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @name show
#'
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}},
#'   \code{\link{RapReliableOpts}}, \code{\link{RapData}},
#'   \code{\link{RapUnsolved}}, \code{\link{RapResults}},
#'   \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#'
#' # show GurobiOpts object
#' GurobiOpts()
#'
#' # show RapReliableOpts object
#' RapReliableOpts()
#'
#' # show RapUnreliableOpts object
#' RapUnreliableOpts()
#'
#' # show RapData object
#' sim_ru@@data
#'
#' # show RapUnsolved object
#' sim_ru
#'
#' # show RapResults object
#' sim_rs@@results
#'
#' # show RapSolved object
#' sim_rs
NULL

#' Convert object to list
#'
#' Convert \code{\link{GurobiOpts}} object to list.
#'
#' @param x \code{\link{GurobiOpts}} object.
#'
#' @param ... not used.
#'
#' @name as.list
#'
#' @return \code{list}
#'
#' @note This function will not include the \code{NumberSolutions} slot, the
#'  \code{MultipleSolutionsMethod} slot, or the \code{TimeLimit} slot if it is
#'  not finite.
#'
#' @seealso \code{GurobiOpts}.
#'
#' @examples
#' # make GuboriOpts object
#' x <- GurobiOpts()
#'
#' # convert to list
#' as.list(x)
NULL

#' Summary of solutions
#'
#' Extracts summary of solutions in a \code{\link{RapResults}} or
#' \code{\link{RapSolved}} object.
#'
#' @param object \code{\link{RapResults}}, or \code{\link{RapSolved}} object.
#'
#' @param ... not used.
#'
#' @details This table follows Marxan conventions ("summary.dat" in
#'   \url{http://marxan.net/downloads/uq_marxan_web_2/module5.html}). The
#'   columns are:
#' \describe{
#' \item{Run_Number}{The index of each solution in the object.}
#' \item{Status}{The status of the solution. The values in this column
#' correspond to outputs from the Gurobi software package (\url{http://www.gurobi.com/documentation/6.5/refman/optimization_status_codes.html}).}
#' \item{Score}{The objective function for the solution.}
#' \item{Cost}{Total cost associated with a solution.}
#' \item{Planning_Units}{Number of planning units selected in a solution.}
#' \item{Connectivity_Total}{The total amount of shared boundary length between
#' all planning units. All solutions in the same object should have equal
#' values for this column.}
#' \item{Connectivity_In}{The amount of shared boundary length among planning
#' units selected in the solution.}
#' \item{Connectivity_Edge}{The amount of exposed boundary length in the
#' solution.}
#' \item{Connectivity_Out}{The number of shared boundary length among planning
#' units not selected in the solution.}
#' \item{Connectivity_Fraction}{The ratio of shared boundary length in the
#' solution (\code{Connectivity_In}) to the total amount of boundary length
#' (\code{Connectivity_Edge}). This ratio is an indicator of solution quality.
#' Solutions with a lower ratio will have less planning units and will be more
#' efficient.}
#' }
#'
#' @name summary
#'
#' @return \code{data.frame}
#'
#' @seealso \code{\link{RapResults}}, \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # show summary
#' summary(sim_rs)
NULL

#' Update object
#'
#' This function updates parameters or data stored in an existing
#' \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}},
#' \code{\link{RapReliableOpts}}, \code{\link{RapData}},
#' \code{\link{RapUnsolved}}, or \code{\link{RapSolved}} object.
#'
#' @param object \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}},
#'   \code{\link{RapReliableOpts}}, \code{\link{RapData}},
#'   \code{\link{RapUnsolved}}, or \code{\link{RapSolved}} object.
#'
#' @param Threads \code{integer} number of cores to use for processing.
#'
#' @param MIPGap \code{numeric} MIP gap specifying minimum solution quality.
#'
#' @param Method \code{integer} Algorithm to use for solving model.
#'
#' @param Presolve \code{integer} code for level of computation in presolve.
#'
#' @param TimeLimit \code{integer} number of seconds to allow for solving.
#'
#' @param NumberSolutions \code{integer} number of solutions to generate.
#'
#' @param MultipleSolutionsMethod \code{integer} name of method to obtain
#'   multiple solutions (used when \code{NumberSolutions} is greater than one).
#'   Available options are \code{"benders.cuts"}, \code{"solution.pool.0"},
#'   \code{"solution.pool.1"}, and \code{"solution.pool.2"}. The
#'   \code{"benders.cuts"} method produces a set of distinct solutions that
#'   are all within the optimality gap. The \code{"solution.pool.0"}
#'   method returns all solutions identified whilst trying to find
#'   a solution that is within the specified optimality gap. The
#'   \code{"solution.pool.1"} method finds one solution within the optimality
#'   gap and a number of additional solutions that are of any level of quality
#'   (such that the total number of solutions is equal to
#'   \code{number_solutions}). The \code{"solution.pool.2"} finds a
#'   specified number of solutions that are nearest to optimality. The
#'   search pool methods correspond to the parameters used by the Gurobi
#'   software suite (see \url{http://www.gurobi.com/documentation/8.0/refman/poolsearchmode.html#parameter:PoolSearchMode}).
#'   Defaults to \code{"benders.cuts"}.
#'
#' @param BLM \code{numeric} boundary length modifier.
#'
#' @param failure.multiplier \code{numeric} multiplier for failure planning
#'   unit.
#'
#' @param max.r.level \code{numeric} maximum R failure level for approximation.
#'
#' @param formulation \code{character} indicating new problem formulation to
#'   use. This can be either "unreliable" or "reliable". The default is
#'   \code{NULL} so that formulation in \code{object} is used.
#'
#' @param species \code{integer} or \code{character} denoting species for which
#'   targets or name should be updated.
#'
#' @param space \code{integer} denoting space for which targets should be
#'   updated.
#'
#' @param name \code{character} to rename species.
#'
#' @param amount.target \code{numeric} vector for new area targets (\%) for the
#'   specified species.
#'
#' @param space.target \code{numeric} vector for new attribute space targets
#'   (\%) for the specified species and attribute spaces.
#'
#' @param pu \code{integer} planning unit indices that need to be updated.
#'
#' @param status \code{integer} new statuses for specified planning units.
#'
#' @param cost \code{numeric} new costs for specified planning units.
#'
#' @param ... parameters passed to \code{\link{update.RapReliableOpts}},
#'   \code{\link{update.RapUnreliableOpts}}, or \code{\link{update.RapData}}.
#'
#' @param solve \code{logical} should the problem be solved? This argument is
#'   only valid for \code{\link{RapUnsolved}} and \code{\link{RapSolved}}
#'   objects. Defaults to \code{TRUE}.
#'
#' @name update
#'
#' @return \code{\link{GurobiOpts-class}},
#'   \code{\link{RapUnreliableOpts-class}},
#'   \code{\link{RapReliableOpts-class}}, \code{\link{RapData-class}},
#'   \code{\link{RapUnsolved-class}}, or \code{\link{RapSolved-class}} object
#'   depending on argument to \code{x}.
#'
#' @seealso \code{\link{GurobiOpts-class}},
#'   \code{\link{RapUnreliableOpts-class}},
#'   \code{\link{RapReliableOpts-class}}, \code{\link{RapData-class}},
#'   \code{\link{RapUnsolved-class}}, \code{\link{RapSolved-class}}.
#'
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#'
#' # GurobiOpts
#' x <- GurobiOpts(MIPGap = 0.7)
#' y <- update(x, MIPGap = 0.1)
#' print(x)
#' print(y)
#'
#' # RapUnreliableOpts
#' x <- RapUnreliableOpts(BLM = 10)
#' y <- update(x, BLM = 2)
#' print(x)
#' print(y)
#'
#' # RapReliableOpts
#' x <- RapReliableOpts(failure.multiplier = 2)
#' y <- update(x, failure.multiplier = 4)
#' print(x)
#' print(y)
#'
#' # RapData
#' x <- sim_ru@@data
#' y <- update(x, space.target = c(0.4, 0.7, 0.1))
#' print(space.target(x))
#' print(space.target(y))
#'
#' ## RapUnsolved
#' x <- sim_ru
#' y <- update(x, amount.target = c(0.1, 0.2, 0.3), BLM = 3, solve = FALSE)
#' print(x@@opts@@BLM); print(amount.target(x))
#' print(y@@opts@@BLM); print(space.target(y))
#'
#' ## RapSolved
#' x <- sim_rs
#' y <- update(x, space.target = c(0.4, 0.6, 0.9), BLM = 100, Presolve = 1L,
#'             solve = FALSE)
#' print(x@@opts@@BLM); print(amount.target(x))
#' print(y@@opts@@BLM); print(space.target(y))
NULL

#' Subset species
#'
#' Subset species from a \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#' \code{\link{RapSolved}} object.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param species \code{integer}, or \code{character} vectors to specify the
#'   index or species names to subset.
#'
#' @return \code{\link{RapData}} or \code{\link{RapUnsolved}} object depending
#'   on input object.
#'
#' @seealso \code{\link{RapData}}, \code{\link{RapUnsolved}},
#'   \code{\link{RapSolved}}.
#'
#' @rdname spp.subset
#'
#' @examples
#' # load data
#' data(sim_ru)
#'
#' # generate new object with only species 1
#' sim_ru2 <- spp.subset(sim_ru, 1)
#'
#' @export
spp.subset <- function(x, species) UseMethod("spp.subset")

#' Subset planning units
#'
#' Subset planning units from a \code{\link{RapData}},
#' \code{\link{RapUnsolved}}, or \code{\link{RapSolved}} object.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param pu \code{integer} vector to specify the index of planning units to
#'  subset.
#'
#' @return \code{\link{RapData}} or \code{\link{RapUnsolved}} object depending
#'   on input object.
#'
#' @seealso \code{\link{RapData}}, \code{\link{RapUnsolved}},
#'   \code{\link{RapSolved}}.
#'
#' @rdname pu.subset
#'
#' @examples
#' # load data
#' data(sim_ru)
#'
#' # generate new object with first 10 planning units
#' sim_ru2 <- pu.subset(sim_ru, seq_len(10))
#'
#' @export
pu.subset <- function(x, pu) UseMethod("pu.subset")

#' Subset demand points
#'
#' Subset demand points from a \code{\link{RapData}},
#' \code{\link{RapUnsolved}}, or \code{\link{RapSolved}} object.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param space \code{integer} vector to specify the index of the space to
#'   subset demand points from.
#'
#' @param species \code{integer} vector to specify the index of the species to
#'   subset demand points from.
#'
#' @param points \code{integer} vector to specify the index of demand points to
#'   subset.
#'
#' @return \code{\link{RapData}} or \code{\link{RapUnsolved}} object depending
#'   on input object.
#'
#' @seealso \code{\link{RapData}}, \code{\link{RapUnsolved}},
#'   \code{\link{RapSolved}}.
#'
#' @rdname dp.subset
#'
#' @examples
#' # load data
#' data(sim_ru)
#'
#' # generate new object with first 10 planning units
#' sim_ru2 <- dp.subset(sim_ru, 1, 1, seq_len(10))
#'
#' @export
dp.subset <- function(x, space, species, points) UseMethod("dp.subset")

#' Subset probabilities above a threshold
#'
#' This function subsets out probabilities assigned to planning units above a
#' threshold. It effectively sets the probability that species inhabit planning
#' units to zero if they are below the threshold.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param species \code{integer} vector specifying the index of the species to
#'   which the threshold should be applied.
#'
#' @param threshold \code{numeric} probability to use a threshold.
#'
#' @return \code{\link{RapData}} or \code{\link{RapUnsolved}} object depending
#'   on input object.
#'
#' @seealso \code{\link{RapData}}, \code{\link{RapUnsolved}},
#'   \code{\link{RapSolved}}.
#'
#' @rdname prob.subset
#'
#' @examples
#' # load data
#' data(sim_ru)
#'
#' # generate new object with first 10 planning units
#' sim_ru2 <- prob.subset(sim_ru, seq_len(3), c(0.1, 0.2, 0.3))
#'
#' @export
prob.subset <- function(x, species, threshold) UseMethod("prob.subset")

#' Solution score
#'
#' Extract solution score from \code{\link{RapResults}} or
#' \code{\link{RapSolved}} object.
#'
#' @param x \code{\link{RapResults}} or \code{\link{RapSolved}} object.
#'
#' @param y Available inputs include: \code{NULL} to return all scores,
#'  \code{integer} number specifying the solution for which the score should
#'  be returned, and \code{0} to return score for the best solution.
#'
#' @return \code{matrix} or \code{numeric} vector with solution score(s)
#'  depending on arguments.
#'
#' @seealso \code{\link{RapResults}}, \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # score for the best solution
#' score(sim_rs, 0)
#'
#' # score for the second solution
#' score(sim_rs, 2)
#'
#' # score for all solutions
#' score(sim_rs, NULL)
#'
#' @export
score <- function(x, y) UseMethod("score")

#' Log file
#'
#' This function returns the Gurobi log file (*.log) associated with solving
#' an optimization problem.
#'
#' @param x \code{\link{RapResults}} or \code{\link{RapSolved}} object.
#'
#' @param y Available inputs include: \code{NULL} to return all values,
#'  \code{integer} number specifying the solution for which the log file should
#'  be returned, and \code{0} to return log file for the best solution.
#'
#' @note The term logging file was used due to collisions with the \code{log}
#'   function.
#'
#' @seealso \code{\link{RapResults}}, \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # log file for the best solution
#' cat(logging.file(sim_rs, 0))
#'
#' # log file for the second solution
#' cat(logging.file(sim_rs, 2))
#'
#' # log files for all solutions
#' cat(logging.file(sim_rs, NULL))
#'
#' @export
logging.file <- function(x, y) UseMethod("logging.file")

#' Extract amount held for a solution
#'
#' This function returns the amount held for each species in a solution.
#'
#' @param x \code{\link{RapResults}} or \code{\link{RapSolved}} object.
#'
#' @param y Available inputs include: \code{NULL} to return all values,
#'  \code{integer} number specifying the solution for which the value should
#'  be returned, and \code{0} to return the value for the best solution.
#'
#' @param species \code{NULL} for all species or \code{integer} indicating
#'   species.
#'
#' @return \code{\link[base]{matrix}} or \code{numeric} vector depending on
#'   arguments.
#'
#' @seealso \code{\link{RapResults}}, \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # amount held (\%) in best solution for each species
#' amount.held(sim_rs, 0)
#'
#' # amount held (\%) in best solution for species 1
#' amount.held(sim_rs, 0, 1)
#'
#' # amount held (\%) in second solution for each species
#' amount.held(sim_rs, 2)
#'
#' # amount held (\%) in each solution for each species
#' amount.held(sim_rs, NULL)
#'
#' @export
amount.held <- function(x, y, species) UseMethod("amount.held")

#' Amount targets
#'
#' This function sets or returns the target amounts for each species.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param species \code{NULL} for all species or \code{integer} indicating
#'   species.
#'
#' @param value \code{numeric} new target.
#'
#' @return code{numeric} vector.
#'
#' @seealso \code{\link{RapData}}, \code{\link{RapResults}},
#'   \code{\link{RapSolved}}.
#'
#' @export
#'
#' @name amount.target
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # extract amount targets for all species
#' amount.target(sim_rs)
#'
#' # set amount targets for all species
#' amount.target(sim_rs) <- 0.1
#'
#' # extract amount targets for first species
#' amount.target(sim_rs, 1)
#'
#' # set amount targets for for first species
#' amount.target(sim_rs, 1) <- 0.5
#'
#' @export
amount.target <- function(x, species) UseMethod("amount.target")

#' @rdname amount.target
#'
#' @export
`amount.target<-` <- function(x, species, value) UseMethod("amount.target<-")

#' Extract attribute space held for a solution
#'
#' This function returns the attribute space held for each species in a
#' solution.
#'
#' @param x \code{\link{RapResults}} or \code{\link{RapSolved}} object.
#'
#' @param y Available inputs include: \code{NULL} to return all values,
#'  \code{integer} number specifying the solution for which the value should
#'  be returned, and \code{0} to return the value for the best solution.
#'
#' @param species \code{NULL} for all species or \code{integer} indicating
#'   species.
#'
#' @param space \code{NULL} for all spaces or \code{integer} indicating a
#'   specific space.
#'
#' @return code{matrix} object.
#'
#' @seealso \code{\link{RapResults}}, \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # space held (\%) for each species in best solution
#' space.held(sim_rs, 0)
#'
#' # space held (\%) for each species in second solution
#' space.held(sim_rs, 2)
#'
#' # space held (\%) for each species in each solution
#' space.held(sim_rs)
#'
#' @export
space.held <- function(x, y, species, space) UseMethod("space.held")

#' Attribute space targets
#'
#' This function sets or returns the attribute space targets for each species.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param species \code{NULL} for all species or \code{integer} indicating
#'   species.
#'
#' @param space \code{NULL} for all spaces or \code{integer} indicating a
#'   specific space.
#'
#' @param value \code{numeric} new target.
#'
#' @return \code{numeric matrix}.
#'
#' @seealso \code{\link{RapData}}, \code{\link{RapResults}},
#'   \code{\link{RapSolved}}.
#'
#' @name space.target
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # extract space targets for all species
#' space.target(sim_rs)
#'
#' # set space targets for all species
#' space.target(sim_rs) <- 0.1
#'
#' # extract target for first species for first space
#' space.target(sim_rs, 1, 1)
#'
#' # set space targets for first species for first space
#' space.target(sim_rs, 1, 1) <- 0.5
#'
#' @export
space.target <- function(x, species, space) UseMethod("space.target")

#' @rdname space.target
#'
#' @export
`space.target<-` <- function(x, species, space, value)
  UseMethod("space.target<-")

#' Extract solution selections
#'
#' Extract selections for a given solution from a \code{\link{RapResults}} or
#' \code{\link{RapSolved}} object.
#'
#' @param x \code{\link{RapResults}} or \code{\link{RapSolved}} object.
#'
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values
#'   for the best solution, \code{integer} value greater than 0 for \code{y}'th
#'   solution value.
#'
#' @return \code{\link[base]{matrix}} or \code{numeric} vector depending on
#'   arguments.
#'
#' @seealso \code{\link{RapResults}}, \code{\link{RapSolved}}.
#'
#' @examples
#' # load data
#' data(sim_rs)
#'
#' # selections for the best solution
#' selections(sim_rs, 0)
#'
#' # selections for the second solution
#' selections(sim_rs, 2)
#'
#' # selections for each solution
#' selections(sim_rs)
#'
#' @export
selections <- function(x, y) UseMethod("selections")

#' Convert SpatialPolygons to PolySet data
#'
#' This function converts spatial \code{\link[sp]{SpatialPolygons}} and
#' \code{\link[sp]{SpatialPolygonsDataFrame}} objects to
#' \code{\link[PBSmapping]{PolySet}} objects.
#'
#' @param x \code{\link[sp]{SpatialPolygons}} or
#'   \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#'
#' @param n_preallocate \code{integer} How much memory should be preallocated
#'   for processing? Ideally, this number should equal the number of vertices
#'   in the \code{\link[sp]{SpatialPolygons}} object. If data processing is
#'   taking too long consider increasing this value.
#'
#' @return \code{\link[PBSmapping]{PolySet}} object.
#'
#' @note Be aware that this function is designed to be as fast as possible, but
#'   as a result it depends on C++ code and if used inappropriately this
#'   function will crash R.
#'
#' @seealso For a slower, more stable equivalent see
#'   \code{maptools::SpatialPolygons2PolySet}.
#'
#' @examples
#' # generate SpatialPolygons object
#' sim_pus <- sim.pus(225L)
#'
#' # convert to PolySet
#' x <- SpatialPolygons2PolySet(sim_pus)
#'
#' @export
SpatialPolygons2PolySet <- function(x, n_preallocate)
  UseMethod("SpatialPolygons2PolySet")

#' Plot species
#'
#' This function plots the distribution of species across the study area.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param species \code{character} name of species, or \code{integer} index for
#'   species.
#'
#' @param y \code{NULL} \code{integer} 0 to return values for the best
#'   solution, \code{integer} value greater than 0 for \code{y}'th solution
#'   value.
#'
#' @param prob.color.palette \code{character} name of color palette to denote
#'   probability of occupancy of the species in planning units (see
#'   \code{\link[RColorBrewer]{brewer.pal}}). Defaults to \code{"YlGnBu"}.
#'
#' @param pu.color.palette \code{character} name of colors or color palette
#'   (\code{\link[RColorBrewer]{brewer.pal}}) to indicate planning unit
#'   statuses. Defaults to \code{c("grey30", "green", "black", "red")} which
#'   indicate non selected, selected, locked in, and locked out (respectively).
#'
#' @param basemap \code{character} object indicating the type of basemap to use
#'   (see \code{\link{basemap}}). Valid options include \code{"none"},
#'   \code{"roadmap"}, \code{"mobile"}, \code{"satellite"}, \code{"terrain"},
#'   \code{"hybrid"}, \code{"mapmaker-roadmap"}, \code{"mapmaker-hybrid"}.
#'   Defaults to \code{"none"}.
#'
#' @param alpha \code{numeric} value to indicating the transparency level for
#'   plotting the planning units.
#'
#' @param grayscale \code{logical} should the basemap be gray-scaled?
#'
#' @param main \code{character} title for the plot. Defaults to \code{NULL} and
#'   a default title is used.
#'
#' @param force.reset \code{logical} if basemap data has been cached, should it
#'   be re-downloaded?
#'
#' @param ... not used.
#'
#' @examples
#' # load RapSolved objects
#' data(sim_ru, sim_rs)
#'
#' # plot first species in sim_ru
#' spp.plot(sim_ru, species = 1)
#'
#' # plot "bimodal" species in sim_rs
#' spp.plot(sim_rs, species = "bimodal")
#'
#' @export
spp.plot <- function(x, species, ...) UseMethod("spp.plot")

#' Plot space
#'
#' This function plots the distribution of planning units and the distribution
#' of demand points for a particular species in an attribute space.
#' Note that this function only works for attribute spaces with one, two, or
#' three dimensions.
#'
#' @param x \code{\link{RapData}}, \code{\link{RapUnsolved}}, or
#'   \code{\link{RapSolved}} object.
#'
#' @param species \code{character} name of species, or \code{integer} index for
#'   species.
#'
#' @param space \code{integer} index of attribute space.
#'
#' @param y \code{integer} number specifying the solution to be plotted. The
#'   value \code{0} can be used to plot the best solution.
#'
#' @param pu.color.palette \code{character} name of colors or color palette
#'   (\code{\link[RColorBrewer]{brewer.pal}}) to indicate planning unit
#'   statuses. Defaults to \code{c("grey30", "green", "black", "red")} which
#'   indicate non selected, selected, locked in, and locked out (respectively).
#'
#' @param main \code{character} title for the plot. Defaults to \code{NULL} and
#'   a default title is used.
#'
#' @param ... not used.
#'
#' @examples
#' # load RapSolved objects
#' data(sim_ru, sim_rs)
#'
#' # plot first species in first attribute space
#' space.plot(sim_ru, 1, 1)
#'
#' # plot distribution of solutions for first species in first attribute space
#' space.plot(sim_rs, 1, 1)
#'
#' @export
space.plot <- function(x, species, space, ...) UseMethod("space.plot")

#' Maximum targets
#'
#' This function accepts a \code{\link{RapUnsolved}} object and returns a
#' \code{data.frame} containing the amount-based and space-based targets for
#' each species and attribute space. These are calculated using a
#' prioritization that contains all the available planning units. Note that the
#' maximum amount-based targets are always 1.
#'
#' @param x \code{\link{RapUnsolved}} or \code{\link{RapSolved}} object.
#'
#' @param verbose \code{logical} should messages be printed during
#'   calculations? Defaults to \code{FALSE}.
#'
#' @return \code{data.frame} object.
#'
#' @examples
#' # load RapSolved objects
#' data(sim_ru)
#'
#' # calculate maximum metrics
#' maximum.targets(sim_ru)
#'
#' @export
maximum.targets <- function(x, verbose) UseMethod("maximum.targets")
