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
#' data(species, pus)
#' puvspr1.dat<-calcSpeciesAverageInPus(pus, species[[1]])
#' puvspr2.dat<-calcSpeciesAverageInPus(pus, species)
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
sim.space<-function(x, ...) UseMethod('sim.space')

#' Solve RASP object
#'
#' This function uses Gurobi to find prioritisations using the input parameter and data stored in a \code{RaspUsolved} object, 
#' and returns a \code{RaspSolved} object with outputs in it.
#'
#' @param x \code{RaspUnsolved} or \code{RaspSolved} object.
#' @param wd \code{character} file path to a working directory, this is a temporary directory by default to avoid pollution.
#' @param clean \code{logical} delete files once processing completed?
#' @param force.reset \code{logical} should solutions be recalculated even if \code{RaspSolved} object supplied?
#' @param ... not used.
#' @return \code{RaspSolved} object
#' @note This function is used to solve a \code{RaspUnsolved} object that has all of its inputs generated. The rasp function (without lower case 'r') provides a more general interface for generating inputs and outputs.
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
NULL

#' Print objects
#'
#' Prints objects.
#'
#' @param x \code{GurobiOpts}, \code{RaspOpts}, \code{RaspData}, \code{RaspUnsolved}, \code{RaspResults}, or \code{RaspSolved} object.
#' @param header \code{logical} should object header be included?
#' @param ... not used.
#' @name print
NULL

#' Show objects
#'
#' Shows objects.
#'
#' @param object \code{GurobiOpts}, \code{RaspOpts}, \code{RaspData}, \code{RaspUnsolved}, \code{RaspResults}, or \code{RaspSolved} object.
#' @name show
NULL

#' Summary of solutions
#'
#' Extracts summary of solutions in a \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param object \code{RaspResults}, or \code{RaspSolved} object.
#' @param ... not used.
#' @name summary
NULL

#' Solution score
#'
#' Extract solution score from \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y "NULL" to return all scores, "integer" 0 to return score for best solution, "integer" value greater than 0 for \code{y}'th solution score.
#' @return "matrix" or "numeric" vector with solution score(s) depending on arguments.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved-class}}, \code{\link{rasp}}.
#' @export
score<-function(x, y) UseMethod('score')

#' Log file
#'
#' This function returns the Gurobi log file (*.log) associated with solving RASP.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved-class}}, \code{\link{rasp}}.
#' @export
logging.file<-function(x) UseMethod('logging.file')

#' Model file
#'
#' This function returns the Gurobi model file (*.lp) associated with solving RASP.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}.
#' @return \code{character} object.
#' @export model.file
model.file<-function(x) UseMethod('model.file')

#' Solution file
#'
#' This function returns the Gurobi solution file (*.sol) associated with solving RASP.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}.
#' @return \code{character} object.
#' @export solution.file
solution.file<-function(x) UseMethod('solution.file')

#' Extract amount held for a solution
#'
#' This function returns the amount held for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}.
#' @export amount.held
amount.held<-function(x,y) {UseMethod('amount.held')}

#' Extract attribute space held for a solution
#'
#' This function returns the attribute space held for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @return code{matrix} or code{numeric} vector depending on arguments.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}.
#' @export space.held
space.held<-function(x,y) {UseMethod('space.held')}

#' Extract solution selections
#'
#' Extract selections for a given solution from a \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved-class}}, \code{\link{rasp}}.
#' @export
selections<-function(x, y) {UseMethod('selections')}

#' Compare Rasp objects
#'
#' This function checks objects to see if they share the same input data.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @param y \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @return \code{logical} are the objects based on the same data?
#' @export
#' @seealso \code{\link{RaspData-class}}, \code{\link{RaspUnsolved-class}}, \code{\link{RaspSolved-class}}.
setGeneric("is.comparable", function(x, y) standardGeneric("is.comparable"))

#' Basemap 
#'
#' This function retrieves google map data for planning units. The google map data is cached to provide fast plotting capabilities.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, \code{RaspSolved} object.
#' @param basemap \code{character} type of base map to display. Valid names are "roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid".
#' @param grayscale \code{logical} should base map be gray scale?
#' @param force.reset \code{logical} ignore data in cache? Setting this as ignore will make function slower but may avoid bugs in cache system.
#' @return \code{list} with google map data.
#' @export
#' @seealso \code{\link[RgoogleMaps]{GetMap.bbox}}, \code{\link{plot}}.
basemap<-function(x, basemap="hybrid", grayscale=FALSE, force.reset=FALSE) {UseMethod("basemap")}

#' Test if hash is cached in a Rasp object
#'
#' Tests if hash is cached in Rasp object.
#' 
#' @param x \code{RaspData} or \code{RaspResults} object
#' @param name \code{character} hash.
#' @note caches are implemented using environments, the hash is used as the name of the object in the environment.
#' @return \code{logical} Is it cached?
#' @keywords internal
setGeneric("is.cached", function(x,name) standardGeneric("is.cached"))

#' Get and set cache Methods
#'
#' Getter and setter methods for caches in MarxanData and MarxanResults object.
#' 
#' @param x \code{RaspData} or \code{RaspResults} object
#' @param name \code{character} hash.
#' @param y if \code{ANY} this object gets cached with name, else if \code{missing} the object hashed at name gets returned.
#' @note caches are implemented using environments, the hash is used as the name of the object in the environment.
#' @return \code{ANY} or \code{NULL} depends on \code{y} argument.
#' @keywords internal
setGeneric("cache", function(x, name, y) standardGeneric("cache"))

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
#' data(pus)
#' x <- SpatialPolygons2PolySet(pus)
SpatialPolygons2PolySet<-function(x, n_preallocate) UseMethod("SpatialPolygons2PolySet")
