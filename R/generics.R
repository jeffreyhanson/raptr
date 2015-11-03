#' @include RcppExports.R rapr-internal.R
NULL

#' Calculate average value for species data in planning units
#'
#' This function calculates the average of species values in each planning unit.
#' Be aware that using polygons with overlaps will result in inaccuracies.
#' By default all polygons will be treated as having separate ids.
#'
#' @param x "SpatialPolygons" or "SpatialPolygonsDataFrame" object.
#' @param y "RasterLayer", "RasterStack", or "RasterBrick" object.
#' @param ids "integer" vector of ids. Defaults to indices of layers in the \code{y}.
#' @param ncores "integer" Number of cores to use for processing. Defaults to 1.
#' @param gdal "logical" Should raster processing be performed using GDAL libraries? Defaults to \code{FALSE}.
#' @param field "character" "integer" index or "character" name of column with planning unit ids. Valid only for "SpatialPolygonsDataFrame" objects. Default behaviour is to treat each polygon as a different planning unit.
#' @param ... not used.
#' @return data.frame with sum of raster values in each polygon.
#' @seealso \code{\link{is.gdalInstalled}}, \url{http://www.gdal.org/}, \url{http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries}.
#' @export
#' @examples
#' # simulate data
#' sim_pus <- sim.pus(225L)
#' sim_spp <- lapply(c('uniform','normal','bimodal'), sim.species, n=1, res=1, x=sim_pus)
#' # calculate average for 1 species
#' puvspr1.dat <- calcSpeciesAverageInPus(sim_pus, sim_spp[[1]])
#' # calculate average for multiple species
#' puvspr2.dat <- calcSpeciesAverageInPus(sim_pus, stack(sim_spp))
calcSpeciesAverageInPus<-function(x, ...) UseMethod("calcSpeciesAverageInPus")

#' Simulate species distribution data for RASP
#'
#' This function simulates species distributions for RASP.
#'
#' @param x \code{RasterLayer} or \code{SpatialPolygons} object delineate the spatial extent to delineate study area.
#' @param n \code{integer} number of species. Defaults to 1.
#' @param res \code{numeric} resolution to simulate distributions. Only needed when \code{SpatialPolygons} supplied.
#' @param model \code{RMmodel} model to simulate species distributions with. Defaults \code{\link[RandomFields]{RPgauss}}.
#' @param ... parameters passed to \code{\link[RandomFields]{RandomFields}}.
#' @details Distributions are simulated by passing \code{model} to \code{\link[RandomFields]{RFsimulate}} and converting to logistic values using \code{\link[boot]{inv.logit}}.
#' @return \code{RasterStack} with layers for each species.
#' @seealso \code{\link[RandomFields]{RFsimulate}}.
#' @export sim.species
#' @examples
#' # make polygons
#' sim_pus <- sim.pus(225L)
#' # simulate 1 uniform species distribution using RasterLayer
#' s1 <- sim.species(blank.raster(sim_pus, 1), n=1, model='uniform')
#'
#' # simulate 1 uniform species distribution based on SpatialPolygons
#' s2 <- sim.species(sim_pus, res=1, n=1, model='uniform')
#'
#' # simulate 1 normal species distributions
#' s3 <- sim.species(sim_pus, res=1, n=1, model='normal')
#'
#' # simulate 1 bimodal species distribution
#' s4 <- sim.species(sim_pus, res=1, n=1, model='bimodal')
#'
#' # simulate 1 species distribution using a RModel object from RandomFields
#' s5 <- sim.species(sim_pus, res=1, n=1, model=RandomFields::RPgauss())
#'
#' # simulate 5 species distribution using a RModel object from RandomFields
#' s6 <- sim.species(sim_pus, res=1, n=5, model=RandomFields::RPgauss())
#'
#' # plot simulations
#' par(mfrow=c(2,2))
#' plot(s2, main='constant')
#' plot(s3, main='normal')
#' plot(s4, main='bimodal')
#' plot(s5, main='RPgauss()')
sim.species<-function(x, ...) UseMethod('sim.species')

#' Simulate attribute space data for RASP
#'
#' This function simulates attribute space data for RASP.
#'
#' @param x \code{RasterLayer} or \code{SpatialPolygons} object delineate the spatial extent to delineate study area.
#' @param d \code{integer} number of dimensions. Defaults to 2.
#' @param res \code{numeric} resolution to simulate distributions. Only needed when \code{SpatialPolygons} supplied.
#' @param model \code{RMmodel} model to simulate species distributions with. Defaults \code{\link[RandomFields]{RPgauss}}.
#' @param ... parameters passed to \code{\link[RandomFields]{RandomFields}}.
#' @details Distributions are simulated by passing \code{model} to \code{\link[RandomFields]{RFsimulate}}.
#' @return \code{RasterStack} with layers for each dimension of the space.
#' @seealso \code{\link[RandomFields]{RFsimulate}}.
#' @export sim.space
#' @name sim.space
#' @examples
#' # simulate plannign units
#' sim_pus <- sim.pus(225L)
#'
#' # simulate 1d space using RasterLayer
#' s1 <- sim.space(blank.raster(sim_pus, 1), d=1)
#'
#' # simulate 1d space using SpatialPolygons
#' s2 <- sim.space(sim_pus, res=1, d=1)
#'
#' # simulate 2d space using SpatialPolygons
#' s3 <- sim.space(sim_pus, res=1, d=2)
#'
#' # plot simulated spaces
#' par(mfrow=c(2,2))
#' plot(s1, main='s1')
#' plot(s2, main='s2')
#' plot(s3[[1]], main='s3: first dimension')
#' plot(s3[[2]], main='s3: second dimension')
sim.space<-function(x, ...) UseMethod('sim.space')

#' Solve RASP object
#'
#' This function uses Gurobi to find prioritisations using the input parameter and data stored in a \code{RapUsolved} object,
#' and returns a \code{RapSolved} object with outputs in it.
#'
#' @param a \code{RapUnsolved} or \code{RapSolved} object.
#' @param b \code{missing} to generate solutions using Gurobi. Prioritisations can be specified using  \code{logical}, \code{numeric}, or \code{matrix} objects. This may be useful for evaluating the performance of solutions obtained using other software.
#' @param verbose \code{logical} should messages be printed during creation of the initial model matrix?.
#' @param ... not used.
#' @return \code{RapSolved} object
#' @note This function is used to solve a \code{RapUnsolved} object that has all of its inputs generated. The rap function (without lower case 'r') provides a more general interface for generating inputs and outputs.
#' @seealso \code{\link{RapUnsolved}}, \code{\link{RapSolved}}.
#' @examples
#' # load RapUnsolved object
#' data(sim_ru)
#' \dontrun{
#' # solve it using Gurobi
#' sim_rs <- solve(sim_ru)
#' }
#' # evaluate manually specified solution using planning unit indices
#' sim_rs2 <- solve(sim_ru, 1:10)
#' # evaluate manually specifed solution using binary selections
#' sim_rs3 <- solve(sim_ru, c(rep(TRUE,10), rep(FALSE, 90)))
#' #  evaluate multiple manually specified solutions
#' sim_rs4 <- solve(sim_ru, matrix(sample(c(0,1), size=500, replace=TRUE), ncol=100, nrow=5))
setGeneric('solve', function(a, b, ...) standardGeneric('solve'))

#' Plot RASP object
#'
#' This function plots the solutions contained in \code{RapSolved} objects. It can be used to show a single solution, or the the selection frequencies of planning
#' units contained in a single \code{RapSolved} object. Additionally, two \code{RapSolved} objects can be supplied to plot the differences between them.
#'
#' @param x \code{RapSolved} object.
#' @param y \code{NULL} to plot selection frequencies. \code{numeric} to plot the i'th solution, or 0 to plot the best solution. \code{RapSolved} object to plot differences in solutions between objects. Defaults to \code{ULL}.
#' @param i \code{NULL} to plot selection frequencies. \code{numeric} to plot the i'th solution, or 0 to plot the best solution. Only used when \code{y} is a \code{RapSolved} object. Defaults to \code{NULL}.
#' @param j \code{NULL} to plot selection frequencies. \code{numeric} to plot the i'th solution, or 0 to plot the best solution. Only used when \code{y} is a \code{RapSolved} object. Defaults to \code{j}.
#' @param basemap \code{character} object indicating the type of basemap to use (see \code{link{basemap}}). Use either 'none', 'roadmap', 'mobile', 'satellite', 'terrain', 'hybrid', 'mapmaker-roadmap', 'mapmaker-hybrid'. Defaults to 'none'.
#' @param color.palette \code{character} name of colour palette to use for planning units (see \code{\link[RColorBrewer]{brewer.pal}}). Defaults to 'PuBu' when \code{y} is \code{NULL}, 'Greens' when \code{y} is \code{numeric}, and 'RdYlBu' or 'Accent' when \code{y} is \code{RapSolved}.
#' @param locked.in.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#000000FF'.
#' @param locked.out.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#D7D7D7FF'.
#' @param x.locked.in.color \code{character} color to denote locked in planning units in \code{x}. Used when \code{y} is \code{RapSolved}. Defaults to '#000000FF'.
#' @param x.locked.out.color \code{character} color to denote locked out planning units in \code{x}. Used when \code{y} is \code{RapSolved}. Defaults to '#D7D7D7FF'.
#' @param y.locked.in.color \code{character} color to denote locked in planning units in \code{y}. Used when \code{y} is \code{RapSolved}. Defaults to '#FFFFFFFF'.
#' @param y.locked.out.color \code{character} color to denote locked out planning units in \code{y}. Used when \code{y} is \code{RapSolved}. Defaults to '#D7D7D7FF'.
#' @param alpha \code{numeric} value to indicate how transparent the planning unit colors shoud be.
#' @param grayscale \code{logical} should the basemap be gray-scaled?
#' @param force.reset \code{logical} if basemap data has been cached, should it be re-downloaded?
#' @name plot
#' @seealso \code{\link{RapSolved}}.
#' @examples
#' # load data
#' data(sim_rs)
#' ## simulated species examples
#' # plot selection frequencies
#' plot(sim_rs)
#' # plot best solution
#' plot(sim_rs, 0)
#' # plot second solution
#' plot(sim_rs, 2)
#' # plot different between best and second solutions
#' plot(sim_rs, sim_rs, 0 ,2)
NULL

#' Print objects
#'
#' Prints objects.
#'
#' @param x \code{GurobiOpts}, \code{RapUnreliableOpts}, \code{RapReliableOpts}, \code{RapData}, \code{RapUnsolved}, \code{RapResults}, or \code{RapSolved} object.
#' @param header \code{logical} should object header be included?
#' @param ... not used.
#' @name print
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}}, \code{\link{RapReliableOpts}}, \code{\link{RapData}}, \code{\link{RapUnsolved}}, \code{\link{RapResults}}, \code{\link{RapSolved}}.
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#' ## print classes in package
#' # GurobiOpts
#' print(GurobiOpts())
#' # RapReliableOpts
#' print(RapReliableOpts())
#' # RapUnreliableOpts
#' print(RapUnreliableOpts())
#' # RapData
#' print(sim_ru@@data)
#' # RapUnsolved
#' print(sim_ru)
#' # RapResults
#' print(sim_rs@@results)
#' # RapSolved
#' print(sim_rs)
NULL

#' Show objects
#'
#' Shows objects.
#'
#' @param object \code{GurobiOpts}, \code{RapUnreliableOpts}, \code{RapReliableOpts}, \code{RapData}, \code{RapUnsolved}, \code{RapResults}, or \code{RapSolved} object.
#' @name show
#' @seealso \code{\link{GurobiOpts}}, \code{\link{RapUnreliableOpts}}, \code{\link{RapReliableOpts}}, \code{\link{RapData}}, \code{\link{RapUnsolved}}, \code{\link{RapResults}}, \code{\link{RapSolved}}.
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#' ## show methods for classes in package
#' # GurobiOpts
#' GurobiOpts()
#' # RapReliableOpts
#' RapReliableOpts()
#' # RapUnreliableOpts
#' RapUnreliableOpts()
#' # RapData
#' sim_ru@@data
#' # RapUnsolved
#' sim_ru
#' # RapResults
#' sim_rs@@results
#' # RapSolved
#' sim_rs
NULL

#' Convert object to list.
#'
#' Convert \code{GurobiOpts} object to list.
#'
#' @param x \code{GurobiOpts} object.
#' @param ... not used.
#' @name as.list
#' @return \code{list}
#' @note This function will not include the \code{NumberSolutions} slot or the \code{TimeLimit} slot if it is not finite.
#' @seealso \code{GurobiOpts}.
#' @examples
#' # make GuboriOpts object
#' x <- GurobiOpts()
#' # convert to list
#' as.list(x)
NULL

#' Summary of solutions
#'
#' Extracts summary of solutions in a \code{RapResults} or \code{RapSolved} object.
#'
#' @param object \code{RapResults}, or \code{RapSolved} object.
#' @param ... not used.
#' @name summary
#' @return \code{data.frame}
#' @seealso \code{\link{RapResults}}, \code{\link{RapSolved}}.
#' @examples
#' # load data
#' data(sim_rs)
#' # show summary
#' summary(sim_rs)
NULL

#' Update object
#'
#' This function updates parameters or data stored in an existing \code{GurobiOpts}, \code{RapUnreliableOpts}, \code{RapReliableOpts}, \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#'
#' @param object \code{GurobiOpts}, \code{RapUnreliableOpts}, \code{RapReliableOpts}, \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param Threads \code{integer} number of cores to use for processing.
#' @param MIPGap \code{numeric} MIP gap specifying minimum solution quality.
#' @param Presolve \code{integer} code for level of computation in presolve.
#' @param TimeLimit \code{integer} number of seconds to allow for solving.
#' @param NumberSolutions \code{integer} number of solutions to generate.
#' @param BLM \code{numeric} boundary length modifier.
#' @param FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit.
#' @param MAXRLEVEL \code{numeric} maximum R failure level for approximation.
#' @param species \code{integer} or \code{character} denoting species for which targets or name should be updated.
#' @param space \code{integer} denoting space for which targets should be updated.
#' @param name \code{character} to rename species.
#' @param amount.target \code{numeric} vector for new area targets (\%) for the specified species.
#' @param space.target \code{numeric} vector for new attribute space targets (\%) for the specified species and attribute spaces.
#' @param pu \code{integer} planning unit indices that need to be updated.
#' @param status \code{integer} new statuses for specified planning units.
#' @param cost \code{numeric} new costs for spcified planning units.
#' @param ... parameters passed to \code{update.RapReliableOpts}, \code{update.RapUnreliableOpts}, or \code{update.RapData}.
#' @param solve \code{logical} should the problem be solved? This argument is only valid for \code{RapUnsolved} and \code{RapSolved} objects. Defaults to \code{TRUE}.
#' @name update
#' @return \code{\link{GurobiOpts-class}}, \code{\link{RapUnreliableOpts-class}}, \code{\link{RapReliableOpts-class}}, \code{\link{RapData-class}}, \code{\link{RapUnsolved-class}}, or \code{\link{RapSolved-class}} object depending on \code{x}.
#' @seealso \code{\link{GurobiOpts-class}}, \code{\link{RapUnreliableOpts-class}}, \code{\link{RapReliableOpts-class}}, \code{\link{RapData-class}}, \code{\link{RapUnsolved-class}}, \code{\link{RapSolved-class}}.
#' @export
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#'
#' # GurobiOpts
#' x <- GurobiOpts(MIPGap=0.7)
#' y <- update(x, MIPGap=0.1)
#' print(x)
#' print(y)
#'
#' # RapUnreliableOpts
#' x <- RapUnreliableOpts(BLM=10)
#' y <- update(x, BLM=2)
#' print(x)
#' print(y)
#'
#' # RapReliableOpts
#' x <- RapReliableOpts(FAILUREMULTIPLIER=2)
#' y <- update(x, FAILUREMULTIPLIER=4)
#' print(x)
#' print(y)
#'
#' # RapData
#' x <- sim_ru@@data
#' y <- update(x, space.target=c(0.4, 0.7, 0.1))
#' print(space.target(x))
#' print(space.target(y))
#'
#' ## RapUnsolved
#' x <- sim_ru
#' y <- update(x, amount.target=c(0.1, 0.2, 0.3), BLM=3, solve=FALSE)
#' # print x parameters
#' print(x@@opts@@BLM); print(amount.target(x))
#' # print y parameters
#' print(y@@opts@@BLM); print(space.target(y))
#'
#' ## RapSolved
#' x <- sim_rs
#' y <- update(x, space.targets=c(0.4, 0.6, 0.9), BLM=100, Presolve=1L, solve=FALSE)
#' # print x parameters
#' print(x@@opts@@BLM); print(amount.target(x))
#' # print y parameters
#' print(y@@opts@@BLM); print(space.target(y))
NULL

#' Subset species
#'
#' Subset species from a \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param species \code{integer}, or \code{character} vectors to specify the index or species names to subset.
#' @return \code{RapData} or \code{RapUnsolved} object depending on input object.
#' @seealso \code{RapData}, \code{RapUnsolved}, \code{RapSolved}.
#' @rdname spp.subset
#' @export
#' @examples
#' # load data
#' data(sim_ru)
#' # generate new object with only species 1
#' sim_ru2 <- spp.subset(sim_ru, 1)
spp.subset<-function(x, species) UseMethod('spp.subset')

#' Subset planning units
#'
#' Subset planning units from a \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param pu \code{integer} vector to specify the index of planning units to subset.
#' @return \code{RapData} or \code{RapUnsolved} object depending on input object.
#' @seealso \code{RapData}, \code{RapUnsolved}, \code{RapSolved}.
#' @rdname pu.subset
#' @export
#' @examples
#' # load data
#' data(sim_ru)
#' # generate new object with first 10 planning units
#' sim_ru2 <- pu.subset(sim_ru, 1:10)
pu.subset<-function(x, pu) UseMethod('pu.subset')

#' Subset demand points
#'
#' Subset demand points from a \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param space \code{integer} vector to specify the index of the space to subset demand points from.
#' @param species \code{integer} vector to specify the index of the species to subset demand points from.
#' @param points \code{integer} vector to specify the index of demand points to subset.
#' @return \code{RapData} or \code{RapUnsolved} object depending on input object.
#' @seealso \code{RapData}, \code{RapUnsolved}, \code{RapSolved}.
#' @rdname dp.subset
#' @export
#' @examples
#' # load data
#' data(sim_ru)
#' # generate new object with first 10 planning units
#' sim_ru2 <- dp.subset(sim_ru, 1, 1, 1:10)
dp.subset<-function(x, space, species, points) UseMethod('dp.subset')

#' Solution score
#'
#' Extract solution score from \code{RapResults} or \code{RapSolved} object.
#'
#' @param x \code{RapResults} or \code{RapSolved} object.
#' @param y "NULL" to return all scores, "integer" 0 to return score for best solution, "integer" value greater than 0 for \code{y}'th solution score.
#' @return "matrix" or "numeric" vector with solution score(s) depending on arguments.
#' @seealso \code{RapResults}, \code{RapSolved}.
#' @export
#' @examples
#' # load data
#' data(sim_rs)
#' # score for best solution
#' score(sim_rs, 0)
#' # score for second solution
#' score(sim_rs, 2)
#' # score for all solutions
#' score(sim_rs, NULL)
score<-function(x, y) UseMethod('score')

#' Log file
#'
#' This function returns the Gurobi log file (*.log) associated with solving RASP.
#'
#' @param x \code{RapResults} or \code{RapSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return the log file for best solution, \code{integer} value greater than 0 for log file for the \code{y}'th solution.
#' @note The term logging file was used due to collisions with the \code{log} function in base R.
#' @seealso \code{RapResults}, \code{RapSolved}.
#' @export
#' @examples
#' data(sim_rs)
#' # log file for best solution
#' cat(logging.file(sim_rs, 0))
#' # log file for second solution
#' cat(logging.file(sim_rs, 2))
#' # log files for all solutions
#' cat(logging.file(sim_rs, NULL))
logging.file<-function(x,y) UseMethod('logging.file')

#' Extract amount held for a solution
#'
#' This function returns the amount held for each species in a solution.
#'
#' @param x \code{RapResults} or \code{RapSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @seealso \code{RapResults}, \code{RapSolved}.
#' @export
#' @examples
#' data(sim_rs)
#' # amount held (%) for each species in best solution
#' amount.held(sim_rs, 0)
#' # amount held (%) for each species in second solution
#' amount.held(sim_rs, 2)
#' # amount held (%) for each species in each solution
#' amount.held(sim_rs, NULL)
amount.held<-function(x,y) {UseMethod('amount.held')}

#' Amount targets
#'
#' This function sets or returns the target amounts for each species.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param species \code{NULL} for all species or \code{integer} indicating species.
#' @param value \code{numeric} new target.
#' @return code{numeric} vector.
#' @seealso \code{RapData}, \code{RapResults}, \code{RapSolved}.
#' @export
#' @name amount.target
#' @examples
#' # load data
#' data(sim_rs)
#' # extract amount targets for all species
#' amount.target(sim_rs)
#' # set amount targets for all species
#' amount.target(sim_rs) <- 0.1
#' # extract amount targets for first species
#' amount.target(sim_rs, 1)
#' # set amount targets for for first species
#' amount.target(sim_rs, 1) <- 0.5
amount.target<-function(x, species) {UseMethod('amount.target')}

#' @rdname amount.target
#' @export
`amount.target<-`<-function(x, species, value) {UseMethod('amount.target<-')}

#' Extract attribute space held for a solution
#'
#' This function returns the attribute space held for each species in a solution.
#'
#' @param x \code{RapResults} or \code{RapSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @return code{matrix} or code{numeric} vector depending on arguments.
#' @seealso \code{RapResults}, \code{RapSolved}.
#' @export
#' @examples
#' data(sim_rs)
#' # space held (%) for each species in best solution
#' space.held(sim_rs, 0)
#' # space held (%) for each species in second solution
#' space.held(sim_rs, 2)
#' # space held (%) for each species in each solution
#' space.held(sim_rs)
space.held<-function(x,y) {UseMethod('space.held')}

#' Attribute space targets
#'
#' This function sets or returns the attribute space targets for each species.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param species \code{NULL} for all species or \code{integer} indicating species.
#' @param space \code{NULL} for all spaces or \code{integer} indicating a specific space.
#' @param value \code{numeric} new target..
#' @return \code{numeric matrix}.
#' @seealso \code{RapData}, \code{RapResults}, \code{RapSolved}.
#' @export
#' @name space.target
#' @examples
#' # load data
#' data(sim_rs)
#' # extract space targets for all species
#' space.target(sim_rs)
#' # set space targets for all species
#' space.target(sim_rs) <- 0.1
#' # extract target for first species for first space
#' space.target(sim_rs, 1, 1)
#' # set space targets for first species for first space
#' space.target(sim_rs, 1, 1) <- 0.5
space.target<-function(x, species, space) {UseMethod('space.target')}

#' @rdname space.target
#' @export
`space.target<-`<-function(x, species, space, value) {UseMethod('space.target<-')}

#' Extract solution selections
#'
#' Extract selections for a given solution from a \code{RapResults} or \code{RapSolved} object.
#'
#' @param x \code{RapResults} or \code{RapSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @seealso \code{RapResults}, \code{RapSolved}.
#' @export
#' @examples
#' data(sim_rs)
#' # selections for best solution
#' selections(sim_rs, 0)
#' # selections for second solution
#' selections(sim_rs, 2)
#' # selections for each solution
#' selections(sim_rs)
selections<-function(x, y) {UseMethod('selections')}

#' Convert SpatialPolygons to PolySet data
#'
#' This function converts spatial \code{SpatialPolygons} and \code{SpatialPolygonsDataFrame} objects to \code{PolySet} objects.
#'
#' @param x \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object.
#' @param n_preallocate \code{integer} How much memory should be preallocated for processing? Ideally, this number should equal the number of vertices in the \code{SpatialPolygons} object. If data processing is taking too long consider increasing this value.
#' @return \code{PolySet} object.
#' @note Be aware that this function is designed to be as fast as possible, but as a result it depends on C++ code and if used inappropriately this function will crash R.
#' @seealso For a slower, more stable equivalent see \code{maptools::SpatialPolygons2PolySet}.
#' @export
#' @examples
#' # generate SpatialPolygons object
#' sim_pus <- sim.pus(225L)
#' # convert to PolySet
#' x <- SpatialPolygons2PolySet(sim_pus)
SpatialPolygons2PolySet<-function(x, n_preallocate) UseMethod("SpatialPolygons2PolySet")

#' Plot species
#'
#' This function plots the distribution of species across the study area.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param species \code{character} name of species, or \code{integer} index for species.
#' @param y \code{NULL} \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param prob.color.palette \code{character} name of color palette to denote probability of occupancy of the species in planning units (see \code{\link[RColorBrewer]{brewer.pal}}). Defaults to 'YlGnBu'.
#' @param pu.color.palette \code{character} name of color palette to indicate planning unit statuses (see \code{\link[RColorBrewer]{brewer.pal}}). Defaults to 'RdYlGn.
#' @param locked.in.color \code{character} color to denote locked in planning units. Defaults to '#000000FF'.
#' @param locked.out.color \code{character} color to denote locked in planning units. Defaults to '#D7D7D7FF'.
#' @param basemap \code{character} object indicating the type of basemap to use (see \code{link{basemap}}). Use either 'none', 'roadmap', 'mobile', 'satellite', 'terrain', 'hybrid', 'mapmaker-roadmap', 'mapmaker-hybrid'. Defaults to 'none'.
#' @param alpha \code{numeric} value to indicate how transparent the planning unit colors shoud be.
#' @param grayscale \code{logical} should the basemap be gray-scaled?
#' @param force.reset \code{logical} if basemap data has been cached, should it be re-downloaded?
#' @param ... not used.
#' @export
#' @examples
#' # load RapSolved objects
#' data(sim_ru)
#' # plot first species in sim_rs
#' spp.plot(sim_ru, species=1)
#' # plot 'bimodal' species in sim_rs
#' spp.plot(sim_ru, species='bimodal')
spp.plot<-function(x, species, ...) UseMethod('spp.plot')

#' Plot space
#'
#' This function plots the distribution of planning units and the distribution of demand points for a particular species in an attribute space.
#' Note that this function only works for attribute spaces with one, two, or three dimensions.
#'
#' @param x \code{RapData}, \code{RapUnsolved}, or \code{RapSolved} object.
#' @param species \code{character} name of species, or \code{integer} index for species.
#' @param space \code{integer} index of attribute space.
#' @param y \code{NULL} \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param pu.color.palette \code{character} name of color palette to use for planning units (see \code{\link[RColorBrewer]{brewer.pal}}). Defaults to 'RdYlGn'.
#' @param locked.in.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#000000FF'.
#' @param locked.out.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#D7D7D7FF'.
#' @param ... not used.
#' @export
#' @examples
#' # load RapSolved objects
#' data(sim_rs)
#' # plot distribution of first species in first attribute space
#' space.plot(sim_rs, 1, 1)
space.plot<-function(
  x,
	species,
	space,
  ...
) {
  UseMethod('space.plot')
}
