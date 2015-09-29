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
simulate.species<-function(x, ...) UseMethod('simulate.species',x)

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
simulate.space<-function(x, ...) UseMethod('simulate.space',x)

