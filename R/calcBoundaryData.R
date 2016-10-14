#' @include RcppExports.R raptr-internal.R
NULL

#' Calculate boundary data for planning units
#'
#' This function calculates boundary length data for \code{PolySet}, \code{SpatialPolygons}, and \code{SpatialPolygonsDataFrame} objects.
#' Be aware that this function is designed to be as fast as possible, and as a consequence, if this
#' function is used improperly then it may crash R. Furthermore, multipart polygons with touching edges will likely result in inaccuracies.
#' If argument set to \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}, this will be converted to PolySet before processing.
#'
#' @param x \code{PolySet}, \code{SpatialPolygons} or \code{SpatialPolyognsDataFrame} object.
#' @param tol \code{numeric} to specify precision of calculations (that is, how far apart do vertices have to be to be considered different).
#' @param length.factor \code{numeric} to scale boundary lengths.
#' @param edge.factor \code{numeric} to scale boundary lengths for edges that do not have any neighbors, such as those that occur along the margins.
#' @param ... not used.
#' @return \code{data.frame} with 'id1' (\code{integer}), 'id2' (\code{integer}), and 'amount' (\code{numeric}) columns.
#' @seealso This function is based on the algorithm in QMARXAN \url{https://github.com/tsw-apropos/qmarxan} for calculating boundary length.
#' @export
#' @examples
#' # simulate planning units
#' sim_pus <- sim.pus(225L)
#' # calculate boundary data
#' bound.dat <- calcBoundaryData(sim_pus)
#' summary(bound.dat)
calcBoundaryData<-function(x, tol, length.factor, edge.factor) UseMethod("calcBoundaryData")

#' @rdname calcBoundaryData
#' @method calcBoundaryData PolySet
#' @inheritParams calcBoundaryData
#' @export
calcBoundaryData.PolySet<-function(x, tol=0.001, length.factor=1.0, edge.factor=1.0) {
	ret<-rcpp_calcBoundaryDF(x, tolerance=tol, lengthFactor=length.factor, edgeFactor=edge.factor)
	if (length(ret$warnings)!=0) {
		warning("Invalid geometries detected, see \"warnings\" attribute for more information.")
		attr(ret$bldf, "warnings")<-ret$warnings
	}
	return(ret$bldf)
}


#' @rdname calcBoundaryData
#' @method calcBoundaryData SpatialPolygons
#' @inheritParams calcBoundaryData
#' @export
calcBoundaryData.SpatialPolygons<-function(x, tol=0.001, length.factor=1.0, edge.factor=1.0) {
	return(calcBoundaryData(rcpp_Polygons2PolySet(x@polygons), tol, length.factor, edge.factor))
}
