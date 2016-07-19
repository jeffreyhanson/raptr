 #' @include RcppExports.R raptr-internal.R
NULL

#' @rdname SpatialPolygons2PolySet
#' @method SpatialPolygons2PolySet SpatialPolygonsDataFrame
#' @export
SpatialPolygons2PolySet.SpatialPolygonsDataFrame<-function(x, n_preallocate=10000L) {
	return(rcpp_Polygons2PolySet(x@polygons))
}

#' @rdname SpatialPolygons2PolySet
#' @method SpatialPolygons2PolySet SpatialPolygons
#' @export
SpatialPolygons2PolySet.SpatialPolygons<-function(x, n_preallocate=10000L) {
	return(rcpp_Polygons2PolySet(x@polygons))
}