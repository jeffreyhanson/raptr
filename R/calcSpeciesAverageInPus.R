#' @include RcppExports.R
NULL

#' @method calcSpeciesAverageInPus SpatialPolygons
#' @rdname calcSpeciesAverageInPus
#' @export
calcSpeciesAverageInPus.SpatialPolygons<-function(x,y,ids=seq_len(nlayers(y)), ncores=1, gdal=FALSE, ...) {
	# check for invalid inputs
	stopifnot(inherits(y, "Raster"))
	stopifnot(nlayers(y)!=length(ids))
	return(
		calcSpeciesAverageInPus.SpatialPolygonsDataFrame(
			x=SpatialPolygonsDataFrame(x@polygons, data=data.frame(id=seq_len(nrow(x@data)), row.names=laply(x@polygons, slot, name="ID"))),
			y=y,
			ids=ids,
			ncores=ncores,
			gdal=gdal,
			field="id"
		)
	)
}

#' @method calcSpeciesAverageInPus SpatialPolygonsDataFrame
#' @rdname calcSpeciesAverageInPus
#' @export
calcSpeciesAverageInPus.SpatialPolygonsDataFrame<-function(x,y,ids=seq_len(nlayers(y)), ncores=1, gdal=FALSE, field=NULL, ...) {
	# check for invalid inputs
	stopifnot(inherits(y, "Raster"))
	stopifnot(nlayers(y)==length(ids))
	# prepare attribute table
	if (is.null(field)) {
		x@data<-data.frame(id=seq_len(nrow(x@data)), row.names=row.names(x@data))
	} else {
		x@data<-data.frame(id=x@data[[field]], row.names=row.names(x@data))
	}
	# generate raster layer with polygons
	if (gdal & is.gdalInstalled()) {
		x<-rasterizeGDAL(x, y[[1]], "id")
	} else {
		if (gdal & !is.gdalInstalled())
			warning('GDAL is not installed on this computer, using raster::rasterize for processing')
		x<-rasterize(x, y[[1]], method="ngb")
	}
	# main processing
	return(zonalMean(x, y, ids, ncores))
}
