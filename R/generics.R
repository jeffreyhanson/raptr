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
#' @seealso \code{\link[RandomFields]{RFsimulate}}
#' @export
simulate.species<-function(x, ...) UseMethod('simulate.species')

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
#' @seealso \code{\link[RandomFields]{RFsimulate}}
#' @export
simulate.space<-function(x, ...) UseMethod('simulate.space')


#' Solve RASP object
#'
#' This function uses Gurobi to find prioritisations using the input parameter and data stored in a \code{RaspUsolved} object, 
#' and returns a \code{RaspSolved} object with outputs in it.
#'
#' @param x \code{RaspUnsolved} or \code{RaspSolved} object.
#' @param wd \code{character} file path to a working directory, this is a temporary directory by default to avoid pollution.
#' @param clean \code{logical} delete files once processing completed?
#' @param force_reset \code{logical} should solutions be recalculated even if \code{RaspSolved} object supplied?
#' @return \code{RaspSolved} object
#' @note This function is used to solve a \code{RaspUnsolved} object that has all of its inputs generated. The rasp function (without lower case 'r') provides a more general interface for generating inputs and outputs.
#' @name solve
NULL

#' Extract solution score
#'
#' Extract solution score from \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y "NULL" to return all scores, "integer" 0 to return score for best solution, "integer" value greater than 0 for \code{y}'th solution score.
#' @return "matrix" or "numeric" vector with solution score(s) depending on arguments.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved-class}}, \code{\link{rasp}}
#' @export
score<-function(x, ...) UseMethod('score')

#' Extract amount targets
#'
#' This function returns the amount targets (%) for species in a RASP object.
#'
#' @param x any \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @param ... not used.
#' @note This generic method does not work on \code{RaspResults} objects because they do not store this information.
#' @export
#' @seealso \code{\link{RaspOpts-class}}, \code{\link{RaspData-class}}, \code{\link{RaspUnsolved-class}}, \code{\link{RaspSolved-class}}
amount.targets<-function(x, ...) UseMethod('amount.targets')

#' Extract space targets
#'
#' This function returns the space targets (%) for species in a RASP object.
#'
#' @param x any \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} object.
#' @param ... not used.
#' @note This generic method does not work on \code{RaspResults} objects because they do not store this information.
#' @export
#' @seealso \code{\link{RaspOpts-class}}, \code{\link{RaspData-class}}, \code{\link{RaspUnsolved-class}}, \code{\link{RaspSolved-class}}
space.targets<-function(x, ...) UseMethod('space.targets')

#' Log file
#'
#' This function returns the Gurobi log file associated with solving RASP.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param ... not used.
#' @export
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}
log.file<-function(x, ...) UseMethod('log.file')

#' Modelfile
#'
#' This function returns the Gurobi model file associated with solving RASP.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param ... not used.
#' @export
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}
model.file<-function(x, ...) UseMethod('model.file')


#' Extract amount held for a solution
#'
#' This function returns the amount held for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}
#' @export
amount.held<-function(x, ...) {UseMethod('amount.held')}

#' Extract attribute space held for a solution
#'
#' This function returns the attribute space held for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return code{matrix} or code{numeric} vector depending on arguments.
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}
#' @export
space.held<-function(x, ...) {UseMethod('space.held')}

#' Extract occurrence held for a solution
#'
#' This function returns the number of occurrences held for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @export
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}
occ.held<-function(x, ...) {UseMethod('occ.held')}

#' Extract information on whether solutions have met the amount targets
#'
#' This function reports whether a solution has met the amount targets for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @export
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}
amount.targets.met<-function(x, ...) {UseMethod('amount.targets.met')}

#' Extract information on whether solutions have met the space targets
#'
#' This function reports whether a solution has met the amount targets for each species in a solution.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @export
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved}}, \code{\link{rasp}}
space.targets.met<-function(x, ...) {UseMethod('space.targets.met')}

#' Extract solution selections
#'
#' Extract selections for a given solution from a \code{RaspResults} or \code{RaspSolved} object.
#'
#' @param x \code{RaspResults} or \code{RaspSolved} object.
#' @param y \code{NULL} to return all values, \code{integer} 0 to return values for best solution, \code{integer} value greater than 0 for \code{y}'th solution value.
#' @param ... not used.
#' @return \code{matrix} or \code{numeric} vector depending on arguments.
#' @export
#' @seealso \code{\link{RaspResults-class}}, \code{\link{RaspSolved-class}}, \code{\link{rasp}}.
selections<-function(x, ...) {UseMethod('selections')}

#' Compare RASP objects
#'
#' This function checks objects to see if they share the same input data.
#'
#' @param x \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} objects.
#' @param y \code{RaspData}, \code{RaspUnsolved}, or \code{RaspSolved} objects.
#' @return "logical" are the objects based on the same data?
#' @export
#' @seealso \code{\link{RaspData-class}}, \code{\link{RaspUnsolved-class}}, \code{\link{RaspSolved-class}}
setGeneric("is.comparable", function(x, y) standardGeneric("is.comparable"))

