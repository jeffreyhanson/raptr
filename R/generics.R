#' @include RcppExports.R raspr-internal.R
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
#' data(sim_pus, sim_spp)
#' # calculate average for 1 species
#' puvspr1.dat<-calcSpeciesAverageInPus(sim_pus, sim_spp[[1]])
#' # calculate average for multiple species
#' puvspr2.dat<-calcSpeciesAverageInPus(sim_pus, sim_spp)
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
#' data(sim_pus)
#' # simulate 1 constant species distribution using RasterLayer
#' s1 <- sim.species(blank.raster(sim_pus, 1), n=1, model='constant')
#'
#' # simulate 1 constant species distribution based on SpatialPolygons
#' s2 <- sim.species(sim_pus, res=1, n=1, model='constant')
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
#' data(sim_pus)
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
#' This function uses Gurobi to find prioritisations using the input parameter and data stored in a \code{RaspUsolved} object,
#' and returns a \code{RaspSolved} object with outputs in it.
#'
#' @param x \code{RaspUnsolved} or \code{RaspSolved} object.
#' @param verbose \code{logical} should messages be printed during creation of the initial model matrix?.
#' @param force.reset \code{logical} if \code{x} already has solutions, should this be discarded and a new solution(s) obtained?
#' @param ... not used.
#' @return \code{RaspSolved} object
#' @note This function is used to solve a \code{RaspUnsolved} object that has all of its inputs generated. The rasp function (without lower case 'r') provides a more general interface for generating inputs and outputs.
#' @seealso \code{RaspUnsolved}, \code{RaspSolved}.
#' @examples
#' \dontrun{
#' # load RaspUnsolved object
#' data(sim_ru)
#' # solve it
#' sim_rs <- solve(ru)
#' }
setGeneric('solve', function(x, ...) standardGeneric('solve'))

#' Plot RASP object
#'
#' This function plots the solutions contained in \code{RaspSolved} objects. It can be used to show a single solution, or the the selection frequencies of planning
#' units contained in a single \code{RaspSolved} object. Additionally, two \code{RaspSolved} objects can be supplied to plot the differences between them.
#'
#' @param x \code{RaspSolved} object.
#' @param y \code{NULL} to plot selection frequencies. \code{numeric} to plot the i'th solution, or 0 to plot the best solution. \code{RaspSolved} object to plot differences in solutions between objects. Defaults to \code{ULL}.
#' @param i \code{NULL} to plot selection frequencies. \code{numeric} to plot the i'th solution, or 0 to plot the best solution. Only used when \code{y} is a \code{RaspSolved} object. Defaults to \code{NULL}.
#' @param j \code{NULL} to plot selection frequencies. \code{numeric} to plot the i'th solution, or 0 to plot the best solution. Only used when \code{y} is a \code{RaspSolved} object. Defaults to \code{j}.
#' @param basemap \code{character} object indicating the type of basemap to use (see \code{link{basemap}}). Use either 'none', 'roadmap', 'mobile', 'satellite', 'terrain', 'hybrid', 'mapmaker-roadmap', 'mapmaker-hybrid'. Defaults to 'none'.
#' @param color.palette \code{character} name of colour palette to use for planning units (see \code{\link[RColorBrewer]{brewer.pal}}). Defaults to 'PuBu' when \code{y} is \code{NULL}, 'Greens' when \code{y} is \code{numeric}, and 'RdYlBu' or 'Accent' when \code{y} is \code{RaspSolved}.
#' @param locked.in.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#000000FF'.
#' @param locked.out.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#D7D7D7FF'.
#' @param x.locked.in.color \code{character} color to denote locked in planning units in \code{x}. Used when \code{y} is \code{RaspSolved}. Defaults to '#000000FF'.
#' @param x.locked.out.color \code{character} color to denote locked out planning units in \code{x}. Used when \code{y} is \code{RaspSolved}. Defaults to '#D7D7D7FF'.
#' @param y.locked.in.color \code{character} color to denote locked in planning units in \code{y}. Used when \code{y} is \code{RaspSolved}. Defaults to '#FFFFFFFF'.
#' @param y.locked.out.color \code{character} color to denote locked out planning units in \code{y}. Used when \code{y} is \code{RaspSolved}. Defaults to '#D7D7D7FF'.
#' @param alpha \code{numeric} value to indicate how transparent the planning unit colors shoud be.
#' @param grayscale \code{logical} should the basemap be gray-scaled?
#' @param force.reset \code{logical} if basemap data has been cached, should it be re-downloaded?
#' @name plot
#' @seealso \code{RaspSolved}.
#' @examples
#' # load data
#' data(cs_rs_area, cs_rs_space)
#' # plot selection frequencies
#' plot(cs_rs_area)
#' # plot best solution with basemap
#' plot(cs_rs_area, 0, basemap='satellite')
#' # plot second solution
#' plot(cs_rs_area, 2, basemap='satellite', grayscale=TRUE)
#' # plot different between first and second solutions
#' plot(cs_rs_area, 1, 2)
#' # plot difference in selection frequencies between two RaspSolved objects
#' plot(cs_rs_area, cs_rs_space)
#' # plot difference between best solutions in two RaspSolved objects
#' plot(cs_rs_area, cs_rs_space, 0, 0)
NULL

#' Print objects
#'
#' Prints objects.
#'
#' @param x \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, \code{RaspUnsolved}, \code{RaspResults}, or \code{RaspSolved} object.
#' @param header \code{logical} should object header be included?
#' @param ... not used.
#' @name print
#' @seealso \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, \code{RaspUnsolved}, \code{RaspResults}, \code{RaspSolved}.
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#' ## print classes in package
#' # GurobiOpts
#' print(GurobiOpts())
#' # RaspReliableOpts
#' print(RaspReliableOpts())
#' # RaspUnreliableOpts
#' print(RaspUnreliableOpts())
#' # RaspData
#' print(sim_ru@@data)
#' # RaspUnsolved
#' print(sim_ru)
#' # RaspResults
#' print(sim_rs@@results)
#' # RaspSolved
#' print(sim_rs)
NULL

#' Show objects
#'
#' Shows objects.
#'
#' @param object \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, \code{RaspUnsolved}, \code{RaspResults}, or \code{RaspSolved} object.
#' @name show
#' @seealso \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, \code{RaspUnsolved}, \code{RaspResults}, \code{RaspSolved}.
#' @examples
#' # load data
#' data(sim_ru, sim_rs)
#' ## show methods for classes in package
#' # GurobiOpts
#' GurobiOpts()
#' # RaspReliableOpts
#' RaspReliableOpts()
#' # RaspUnreliableOpts
#' RaspUnreliableOpts()
#' # RaspData
#' sim_ru@@data
#' # RaspUnsolved
#' sim_ru
#' # RaspResults
#' sim_rs@@results
#' # RaspSolved
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
#' Extracts summary of solutions in a \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param object \code{RaspResults}, or \code{RaspSolved} object.
#' @param ... not used.
#' @name summary
#' @return \code{data.frame}
#' @seealso \code{RaspResults}, \code{RaspSolved}.
#' @examples
#' # load data
#' load(sim_rs)
#' # show summary
#' summary(sim_rs)
NULL

#' Update object
#'
#' This function updates parameters or data stored in an existing \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#'
#' @param object \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object..
#' @param ... parameters to update.
#' @param ignore.extra \code{logical} should extra invalid arguments be ignored? Defaults to \code{FALSE}.
#' @param solve \code{logical} should the problem be solved? This argument is only valid for \code{RaspUnsolved} and \code{RaspSolved} objects. Defaults to \code{TRUE}.
#' @name update
#' @return \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, or \code{RaspUnsolved} object depending on \code{x}.
#' @seealso \code{GurobiOpts}, \code{RaspUnreliableOpts}, \code{RaspReliableOpts}, \code{RaspData}, or \code{RaspUnsolved}.
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
#' # RaspUnreliableOpts
#' x <- RaspUnreliableOpts(BLM=10)
#' y <- update(x, BLM=2)
#' print(x)
#' print(y)
#'
#' # RaspReliableOpts
#' x <- RaspReliableOpts(FAILUREMULTIPLIER=2)
#' y <- update(x, FAILUREMULTIPLIER=4)
#' print(x)
#' print(y)
#'
#' # RaspData
#' x <- sim_ru@@data
#' y <- update(x, space.targets=c(0.4, 0.7, 0.1))
#' print(x@@data$space.targets)
#' print(y@@data$space.targets)
#'
#' ## RaspUnsolved
#' x <- sim_ru
#' y <- update(x, area.targets=c(0.1, 0.2, 0.3), BLM=3, Presolve=0L)
#' # print x parameters
#' print(x@@opts@@BLM); print(x@@solver@@Presolve); print(x@@data@@species$area.targets)
#' # print y parameters
#' print(y@@opts@@BLM); print(y@@solver@@Presolve); print(y@@data@@species$area.targets)
#'
#' ## RaspSolved
#' x <- sim_rs
#' y <- update(x, space.targets=c(0.4, 0.6, 0.9), BLM=100, Presolve=1L)
#' # print x parameters
#' print(x@@opts@@BLM); print(x@@solver@@Presolve); print(x@@data@@species$area.targets)
#' # print y parameters
#' print(y@@opts@@BLM); print(y@@solver@@Presolve); print(y@@data@@species$area.targets)
NULL

#' Subset species
#'
#' Subset species from a \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @param species \code{integer}, or \code{character} vectors to specify the index or species names to subset.
#' @return \code{RaspData} or \code{RaspUnsolved} object depending on input object.
#' @seealso \code{RaspData}, \code{RaspUnsolved}, \code{RaspSolved}.
#' @rdname spp.subset
#' @export
#' @examples
#' # load data
#' load(sim_ru)
#' # generate new object with only species 1
#' sim_ru2 <- spp.subset(sim_ru, 1)
spp.subset<-function(x, species) UseMethod('spp.subset')

#' Subset planning units
#'
#' Subset planning units from a \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @param pu \code{integer} vector to specify the index of planning units to subset.
#' @return \code{RaspData} or \code{RaspUnsolved} object depending on input object.
#' @seealso \code{RaspData}, \code{RaspUnsolved}, \code{RaspSolved}.
#' @rdname pu.subset
#' @export
#' @examples
#' # load data
#' load(sim_ru)
#' # generate new object with first 10 planning units
#' sim_ru2 <- pu.subset(sim_ru, 1:10)
pu.subset<-function(x, pu) UseMethod('pu.subset')

#' Solution score
#'
#' Extract solution score from \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y "NULL" to return all scores, "integer" 0 to return score for best solution, "integer" value greater than 0 for \code{y}'th solution score.
#' @return "matrix" or "numeric" vector with solution score(s) depending on arguments.
#' @seealso \code{RaspResults}, \code{RaspSolved}.
#' @export
#' @examples
#' # load data
#' load(sim_rs)
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
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return the log file for best solution, \code{integer} value greater than 0 for log file for the \code{y}'th solution.
#' @note The term logging file was used due to collisions with the \code{log} function in base R.
#' @seealso \code{RaspResults}, \code{RaspSolved}.
#' @export
#' @examples
#' load(sim_rs)
#' # log file for best solution
#' logging.file(sim_rs, 0)
#' # log file for second solution
#' logging.file(sim_rs, 2)
#' # log files for all solutions
#' logging.file(sim_rs, NULL)
logging.file<-function(x,y) UseMethod('logging.file')

#' Extract amount held for a solution
#'
#' This function returns the amount held for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @seealso \code{RaspResults}, \code{RaspSolved}.
#' @export
#' @examples
#' load(sim_rs)
#' # amount held (%) for each species in best solution
#' amount.held(sim_rs, 0)
#' # amount held (%) for each species in second solution
#' amount.held(sim_rs, 2)
#' # amount held (%) for each species in each solution
#' amount.held(sim_rs, NULL)
amount.held<-function(x,y) {UseMethod('amount.held')}

#' Extract amount targets
#'
#' This function returns the amount targets for each species.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @return code{numeric} vector.
#' @seealso \code{RaspData}, \code{RaspResults}, \code{RaspSolved}.
#' @export
#' @examples
#' # load data
#' load(sim_rs)
#' # extract amount targets
#' amount.target(sim_rs)
amount.target<-function(x) {UseMethod('amount.target')}

#' Extract attribute space held for a solution
#'
#' This function returns the attribute space held for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @return code{matrix} or code{numeric} vector depending on arguments.
#' @seealso \code{RaspResults}, \code{RaspSolved}.
#' @export
#' @examples
#' load(sim_rs)
#' # space held (%) for each species in best solution
#' space.held(sim_rs, 0)
#' # space held (%) for each species in second solution
#' space.held(sim_rs, 2)
#' # space held (%) for each species in each solution
#' space.held(sim_rs, NULL)
space.held<-function(x,y) {UseMethod('space.held')}

#' Extract attribute space targets
#'
#' This function returns the attribute space targets for each species.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @return code{numeric} vector.
#' @seealso \code{RaspData}, \code{RaspResults}, \code{RaspSolved}.
#' @export
#' @examples
#' # load data
#' load(sim_rs)
#' # extract space targets
#' space.target(sim_rs)
space.target<-function(x) {UseMethod('space.target')}

#' Extract solution selections
#'
#' Extract selections for a given solution from a \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @seealso \code{RaspResults}, \code{RaspSolved}.
#' @export
#' @examples
#' load(sim_rs)
#' # selections for best solution
#' selections(sim_rs, 0)
#' # selections for second solution
#' selections(sim_rs, 2)
#' #selections for each solution
#' selections(sim_rs, NULL)
selections<-function(x, y) {UseMethod('selections')}

#' Convert SpatialPolygons to PolySet data
#'
#' This function converts spatial \code{SpatialPolygons} and \code{SpatialPolygonsDataFrame} objects to \code{PolySet} objects.
#'
#' @param x \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object.
#' @param n_preallocate \code{integer} How much memory should be preallocated for processing? Ideally, this number should equal the number of vertices in the \code{SpatialPolygons} object. If data processing is taking too long consider increasing this value.
#' @return \code{PolySet} object.
#' @note Be aware that this function is designed to be as fast as possible, but as a result it depends on C++ code and if used inappropriately this function will crash R.
#' @seealso For a slower, more stable equivalent see \code{\link[maptools]{SpatialPolygons2PolySet}}.
#' @export
#' @examples
#' # load SpatialPolygons object
#' data(sim_pus)
#' # convert to PolySet
#' x <- SpatialPolygons2PolySet(sim_pus)
SpatialPolygons2PolySet<-function(x, n_preallocate) UseMethod("SpatialPolygons2PolySet")

#' Plot species
#'
#' This function plots the distribution of species across the study area.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @param y \code{character} name of species, or \code{integer} index for species.
#' @param basemap \code{character} object indicating the type of basemap to use (see \code{link{basemap}}). Use either 'none', 'roadmap', 'mobile', 'satellite', 'terrain', 'hybrid', 'mapmaker-roadmap', 'mapmaker-hybrid'. Defaults to 'none'.
#' @param color.palette \code{character} name of colour palette to use for planning units (see \code{\link[RColorBrewer]{brewer.pal}}). Defaults to 'YlGnBu'.
#' @param alpha \code{numeric} value to indicate how transparent the planning unit colors shoud be.
#' @param grayscale \code{logical} should the basemap be gray-scaled?
#' @param force.reset \code{logical} if basemap data has been cached, should it be re-downloaded?
#' @export
#' @examples
#' # load RaspSolved objects
#' data(sim_rs, cs_rs)
#' # plot first species in sim_rs
#' spp.plot(sim_rs)
#' # plot species in cs_rs with basemap
#' spp.plot(cs_rs, basemap='satellite')
spp.plot<-function(x, y, basemap, color.palette, alpha, grayscale, force.reset) UseMethod('spp.plot')

#' Plot space
#'
#' This function plots the distribution of planning units and the distribution of demand points for a particular species in an attribute space.
#' Note that this function only works for attribute spaces with one, two, or three dimensions.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @param y \code{character} name of species, or \code{integer} index for species.
#' @param space \code{integer} index of attribute space.
#' @param pu.color.palette \code{character} name of color palette to use for planning units (see \code{\link[RColorBrewer]{brewer.pal}}). Defaults to 'RdYlGn'.
#' @param locked.in.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#000000FF'.
#' @param locked.out.color \code{character} color to denote locked in planning units. Used when \code{y} is \code{NULL}. Defaults to '#D7D7D7FF'.
#' @export
#' @examples
#' # load RaspSolved objects
#' data(sim_rs)
#' # plot distribution of first species in first attribute space
#' space.plot(sim_rs, 1, 1)
space.plot<-function(
  x,
	y,
	space,
	pu.color.palette,
	locked.in.color,
	locked.out.color
) {
  UseMethod('space.plot')
}
